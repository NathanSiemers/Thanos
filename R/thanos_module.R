################################################################
## Thanos cross-filter Shiny module
##
## thanosUI(id):  column picker + an empty anchor div.  Per-variable
##   filter panels are inserted/removed individually (insertUI/removeUI)
##   so changing one filter never tears down the other widgets -- the
##   central performance fix over the renderUI-everything ancestors.
##
## thanosServer(id, backend, ...): drives the module against any backend
##   implementing the contract in thanos_backend.R.  Returns accessors
##   the parent app uses to learn which rows survive filtering:
##     $mask()           logical(n_rows), TRUE = row passes all filters
##     $rows()           integer row IDs passing all filters (the cheap
##                       "pointer" to hand to a parent app)
##     $n_selected()     sum(mask())
##     $selected_vars()  currently selected column names
##     $filters()        named list of current filter values (save/restore)
##     $streams(v, ...)  leave-one-out row streams: rows passing every
##                       OTHER filter, partitioned by v's own filter
##                       (selected/excluded, or selected/below/above/na
##                       with split_range = TRUE on a range filter)
##     $add_vars(cols)   parent -> module: ensure these columns appear in
##                       the filter selection (adds panels for any not
##                       already selected; never removes; unknown column
##                       names are ignored).  The one sanctioned way for
##                       a parent app to drive the filter set.
##
## Reactive architecture (see design.md for the full graph):
##   - each variable owns ONE debounced observer; changing its filter
##     recomputes only that variable's O(n) mask (vector mode) and its
##     canonical filter entry
##   - leave-one-out masks for all k variables come from one reactive
##     using prefix/suffix cumulative ANDs: O(3k) vector ANDs, not O(k^2)
##   - each plot is registered once; where its counts come from is the
##     ONLY thing the two execution modes disagree about, so that choice
##     is a single closure (counts_for) picked at server start
##   - per-variable lifecycle state is one object (cache$var[[v]]),
##     created whole by add_var() and deleted whole by remove_var()
################################################################
#' Thanos cross-filter module UI
#'
#' Places the module's user interface: a column picker plus an anchor
#' `div` into which per-variable filter panels (widget + live
#' cross-filter histogram) are inserted and removed individually as the
#' user selects columns. Pair with [thanosServer()] using the same `id`.
#'
#' @param id Module id; must match the `id` given to [thanosServer()].
#' @param width CSS width of the column picker (default `"100%"`).
#'
#' @return A [shiny::tagList()] of UI elements to place in your layout.
#'
#' @examples
#' \donttest{
#' library(shiny)
#' backend <- backend_memory(mtcars)
#' ui <- fluidPage(thanosUI("thanos"))
#' server <- function(input, output, session) {
#'     th <- thanosServer("thanos", backend)
#' }
#' if (interactive()) shinyApp(ui, server)
#' }
#' @export
thanosUI <- function(id, width = "100%") {
    ns <- NS(id)
    tagList(
        ## loading feedback scoped STRICTLY to this module's own panels
        ## (a subtle pulse while a plot recalculates), so embedding
        ## Thanos never restyles anything in the host app
        tags$style(HTML(sprintf(
            "#%s .recalculating { opacity: 0.4; animation: thanos-pulse 1.2s ease-in-out infinite; }
             @keyframes thanos-pulse { 50%% { opacity: 0.15; } }
             #%s .thanos-controls { display: flex; gap: 16px; align-items: center;
                                    flex-wrap: wrap; font-size: 85%%; color: #555;
                                    margin: -4px 0 2px 0; }
             #%s .thanos-controls .form-group, #%s .thanos-controls .checkbox { margin: 0; }",
            ns("panels"), ns("panels"), ns("panels"), ns("panels")))),
        selectizeInput(ns("vars"), "Filter columns", choices = NULL,
                       multiple = TRUE, width = width),
        ## filled by the server with a one-line note explaining what
        ## removing a column does (wording depends on remember_removed);
        ## in-page text rather than a notification so the module stays
        ## polite when embedded in a host app
        uiOutput(ns("removal_note")),
        div(id = ns("panels"))
    )
}

#' Thanos cross-filter module server
#'
#' Drives the cross-filter module against any backend implementing the
#' Thanos backend contract (see [backend_memory()], [backend_dbi()]).
#' Each selected column gets an auto-chosen filter widget (slider,
#' checkboxes, or selectize) and a live histogram of the rows passing
#' all *other* filters, with this variable's own selection overlaid.
#'
#' @param id Module id; must match the `id` given to [thanosUI()].
#' @param backend A backend object, e.g. from [backend_memory()],
#'   [backend_sqlite()], or [backend_duckdb()].
#' @param default_selected Character vector of columns pre-selected for
#'   filtering at startup (unknown names are ignored).
#' @param bins Number of histogram bins for continuous numeric columns.
#' @param debounce_ms Debounce interval (ms) for slider inputs.
#' @param debounce_checkbox_ms Debounce interval (ms) for checkbox
#'   groups and selectize filters.
#' @param plot_height CSS height of each histogram.
#' @param max_checkbox_levels Categorical columns with at most this many
#'   levels get checkboxes; above it, a selectize input.
#' @param mode `"auto"` (default), `"vector"`, or `"aggregate"`.
#'   Vector mode fetches whole columns once and filters in R; aggregate
#'   mode never fetches columns and asks the backend for binned counts
#'   via SQL. `"auto"` picks aggregate when the backend supports it and
#'   the data exceeds `aggregate_threshold` rows.
#' @param aggregate_threshold Row count above which `"auto"` switches to
#'   aggregate mode.
#' @param remember_removed If `TRUE`, a removed column's filter settings
#'   are kept and restored when it is re-added; if `FALSE` (default)
#'   removing a column clears its filter completely.
#' @param removal_note Show the one-line in-page note explaining what
#'   removing a column does.
#' @param max_discrete_numeric Numeric columns with at most this many
#'   distinct values are treated as discrete (checkboxes with
#'   membership semantics) instead of getting a range slider.
#' @param plot_engine `"base"` (default; identical visual at a fraction
#'   of the rendering cost) or `"ggplot"`.
#'
#' @return A list of accessors for the parent app:
#'   \describe{
#'     \item{`mask()`}{reactive: `logical(n_rows)`, `TRUE` = row passes
#'       all filters}
#'     \item{`rows()`}{reactive: integer row IDs passing all filters}
#'     \item{`n_selected()`}{reactive: number of rows passing}
#'     \item{`selected_vars()`}{reactive: currently selected columns}
#'     \item{`filters()`}{reactive: named list of current filter values
#'       in raw units (save/restore, bookmarking)}
#'     \item{`streams(v, split_range = FALSE, drop_na = FALSE)`}{function:
#'       leave-one-out row streams for column `v` -- the rows passing
#'       every *other* filter, partitioned by what `v`'s own filter did
#'       to them. Returns sorted row-ID vectors: `selected` (also passes
#'       `v`'s filter; identical to `rows()` when `v` is selected) and
#'       `excluded`, or with `split_range = TRUE` on a range filter
#'       `selected`/`below`/`above`/`na` (rejects split by side; a
#'       slider handle at its endpoint leaves that side empty; excluded
#'       NAs, having no side, land in `na`). NA rows follow the
#'       include-NA checkbox like the mask; `drop_na = TRUE` removes
#'       them from every stream. Fetch data as
#'       `backend$get_column(v)[ids]`.}
#'     \item{`add_vars(cols)`}{function: ensure these columns appear in
#'       the filter selection (additive, idempotent)}
#'   }
#'
#' @examples
#' \donttest{
#' library(shiny)
#' backend <- backend_memory(as.data.frame(mtcars))
#' ui <- fluidPage(
#'     thanosUI("thanos"),
#'     verbatimTextOutput("n")
#' )
#' server <- function(input, output, session) {
#'     th <- thanosServer("thanos", backend,
#'                        default_selected = c("mpg", "cyl"))
#'     output$n <- renderText(th$n_selected())
#' }
#' if (interactive()) shinyApp(ui, server)
#' }
#' @export
thanosServer <- function(id, backend,
                         default_selected = NULL,
                         bins = 50,
                         debounce_ms = 300,
                         debounce_checkbox_ms = 300,
                         plot_height = "150px",
                         max_checkbox_levels = 30,
                         mode = c("auto", "vector", "aggregate"),
                         aggregate_threshold = 2e6,
                         remember_removed = FALSE,
                         removal_note = TRUE,
                         max_discrete_numeric = 12,
                         plot_engine = c("base", "ggplot")) {
    ## "base" draws the identical visual with base graphics at a fraction
    ## of ggplot's per-render overhead (see bench/bench_plots.R);
    ## "ggplot" remains available if a host app needs grid graphics
    plot_engine <- match.arg(plot_engine)
    mode <- match.arg(mode)
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        n_rows <- backend$n_rows()
        ## vector mode caches full columns and filters in R; aggregate
        ## mode never fetches columns and asks the backend for binned
        ## counts via SQL instead -- for data too big to hold as vectors
        if (mode == "auto") {
            mode <- if (isTRUE(backend$supports_binned) &&
                        n_rows > aggregate_threshold) "aggregate" else "vector"
        }
        if (mode == "aggregate" && !isTRUE(backend$supports_binned)) {
            stop("this backend does not support aggregate mode")
        }

        ## one non-reactive state object per selected variable:
        ## info, widget kind, bin spec, cached column (vector mode),
        ## slider bounds, observers, seen/can_log flags
        cache <- new.env(parent = emptyenv())
        cache$var <- list()   # one state object per selected variable
        cache$deb <- list()   # debounced filter reactives, reused across
                              # remove/re-add (their observers can't be
                              # destroyed, so never re-create them)

        ## INVALIDATION DISCIPLINE (see design.md): every write below is
        ## equality-gated -- a value that did not change is never written,
        ## so no-op events (initial widget reports, adding an unfiltered
        ## column, re-sending the same value, our own slider echoes)
        ## invalidate nothing downstream.
        maskStore   <- reactiveValues()  # var -> logical(n_rows), vector mode
        filterState <- reactiveValues(filters = list(), includeNA = list())
        logState    <- reactiveValues() # var -> TRUE when log2 display is on
                                        # (one KEY per var, so a toggle
                                        #  invalidates exactly one plot)
        varsNow     <- reactiveVal(character(0))

        ## one-line in-page note (Project.md): tell the user what removing
        ## a column does, without notifications a host app can't control
        output$removal_note <- renderUI({
            if (!removal_note) return(NULL)
            div(style = "font-size: 85%; color: #777; font-style: italic; margin: -8px 0 8px 0;",
                if (remember_removed) {
                    "removing a column stops its filtering; re-adding it restores its last filter"
                } else {
                    "removing a column clears its filter completely"
                })
        })

        all_columns <- backend$get_columns()
        ## the last selection we asked the client for (non-reactive):
        ## add_vars() unions against this so it can never race the
        ## still-round-tripping default_selected update
        cache$requested <- intersect(default_selected %||% character(0),
                                     all_columns)
        updateSelectizeInput(session, "vars", choices = all_columns,
            selected = cache$requested, server = TRUE)

        ## column name -> id-safe fragment for input/output ids and selectors
        vid <- function(v) gsub("[^A-Za-z0-9_]", "_", v)

        plot_label <- function(v) {
            if (isTRUE(logState[[v]])) paste0(v, " (log2+1)") else v
        }

        ## where a plot's counts come from is the ONLY per-mode decision
        ## in the render path; everything downstream of this closure is
        ## shared (see bin_counts / plot_histo_counts in thanos_plot.R)
        counts_for <- if (mode == "vector") {
            function(v) {
                loo <- looStore[[v]]   # per-var gated: only content
                req(!is.null(loo))     # changes invalidate this plot
                ## an absent mask means "no own filter": bin_counts
                ## short-circuits instead of AND-ing an all-TRUE vector
                bin_counts(cache$var[[v]]$bin, loo, maskStore[[v]])
            }
        } else {
            function(v) {
                ## non-reactive existence guard: a removed variable's
                ## stale render is simply stopped, and live plots gain
                ## no dependency on the selection structure itself
                req(!is.null(cache$var[[v]]))
                fl <- filtersNow()   # normalized: no-op filters absent
                own_f <- fl[[v]]     # NULL when v's filter is inactive
                loo_f <- fl[setdiff(names(fl), v)]
                ## one combined query for both count vectors; the global
                ## row count is a reactive shared by all plots.  When v
                ## has no active filter its leave-one-out set IS the full
                ## filter set, so n_shown = n_sel and the per-plot count
                ## query is skipped -- an interaction costs k pair
                ## queries + one loo count per ACTIVELY filtered column
                ## + 1 global, instead of 2k + 1
                pair <- backend$get_binned_pair(v, cache$var[[v]]$bin,
                                                loo_f, own_f)
                list(shown = pair$shown, sel = pair$sel,
                     n_shown = if (is.null(own_f)) nSelected()
                               else backend$get_count(loo_f),
                     n_sel = nSelected())
            }
        }

        build_bin <- function(st, use_log) {
            if (mode == "vector") {
                bin_column(st$col, bins,
                    discrete_values = if (st$discrete) st$info$values,
                    range = if (st$info$is_numeric && !st$discrete)
                                display_range(st$info),
                    log2p1 = use_log)
            } else {
                bin_spec_from_info(st$info, bins, discrete = st$discrete,
                                   log2p1 = use_log)
            }
        }

        add_var <- function(v) {
            id <- vid(v)
            info <- backend$get_column_info(v)
            ## a numeric column with few distinct values ('month') gets
            ## checkboxes with membership semantics instead of a slider
            discrete <- isTRUE(info$is_numeric) && !is.null(info$values) &&
                (info$n_unique %||% Inf) <= max_discrete_numeric
            if (discrete) info$levels <- as.character(info$values)
            widget <- if (info$is_numeric && !discrete) "slider"
                      else if (length(info$levels) <= max_checkbox_levels) "checkbox"
                      else "selectize"
            ## log2(x+1) toggle: non-negative sliders only, and in
            ## aggregate mode only if the SQL engine has log2()
            can_log <- widget == "slider" && isTRUE(info$range[1] >= 0) &&
                (mode == "vector" || isTRUE(backend$supports_log2))

            stored     <- isolate(filterState$filters[[v]])
            stored_na  <- isolate(filterState$includeNA[[v]]) %||% TRUE
            stored_log <- can_log && isTRUE(isolate(logState[[v]]))

            st <- list(info = info, widget = widget, discrete = discrete,
                       can_log = can_log,
                       ## a remembered empty selection (character(0)) must
                       ## survive re-add: the widget HAS spoken before, so
                       ## its fresh NULL report means "none", not "no filter"
                       seen = !is.null(stored),
                       log_active = stored_log,
                       echoes = list(),
                       last_applied = NULL,
                       col = if (mode == "vector") backend$get_column(v),
                       slider = if (widget == "slider")
                                    slider_bounds(info, log2p1 = stored_log))
            st$bin <- build_bin(st, stored_log)
            cache$var[[v]] <- st
            if (mode == "vector") {
                ## seed the leave-one-out entry: a new unfiltered column's
                ## loo set IS the current global mask, so its first render
                ## needn't wait for (or re-trigger) the combiner observer
                looStore[[v]] <- isolate(globalMaskVal()) %||% rep(TRUE, n_rows)
                ## a REMEMBERED filter applies immediately (the rebuilt
                ## widget shows it): seed the mask rather than waiting
                ## for the widget round trip the freeze below defers
                if (!is.null(stored)) {
                    m <- make_mask(st$col, stored, stored_na)
                    if (!all(m)) maskStore[[v]] <- m
                }
            }

            ## the session may still hold this input's value from a
            ## PREVIOUS incarnation of the column (removed and re-added):
            ## processing it would resurrect a filter that removal cleared,
            ## and parents would see wrong rows() until the rebuilt widget
            ## reports.  Defense: screen the observer's FIRST post-rebuild
            ## event against the value the rebuilt widget is KNOWN to
            ## report (st$expect): a match is processed, anything else is
            ## the stale ghost and is skipped once.
            ## (freezeReactiveValue is deliberately NOT used: its thaw
            ## re-delivers the stale value anyway, and freezing an input
            ## consumed through shiny::debounce wedges the debouncer so
            ## later values never emit; freezing an observeEvent's
            ## ignoreInit event expression swallows the first real event.)
            readd <- !is.null(cache$deb[[v]])
            if (readd) {
                cache$var[[v]]$has_expect <- TRUE
                cache$var[[v]]$expect <- if (widget == "slider") {
                    d <- stored %||% c(st$slider$lo, st$slider$hi)
                    fin <- is.finite(d)
                    if (stored_log) d[fin] <- log2(d[fin] + 1)
                    pmin(pmax(d, st$slider$lo), st$slider$hi)
                } else if (widget == "selectize") {
                    stored                     # NULL = empty selectize
                } else {
                    sel_set <- stored %||% info$levels
                    if (length(sel_set) == 0) NULL   # checkbox: none ticked
                    else sel_set
                }
                if (can_log) cache$var[[v]]$log_expect <- stored_log
            }

            insertUI(paste0("#", ns("panels")), where = "beforeEnd",
                     immediate = TRUE,
                     ui = make_var_panel(ns, v, id, info, widget,
                                         stored, stored_na, plot_height,
                                         slider = st$slider,
                                         can_log = can_log,
                                         stored_log = stored_log))

            ## one observer per variable: any change to its filter or
            ## include-NA input recomputes only THIS variable's mask.
            ## Sliders AND checkboxes are debounced: a lazy drag or a
            ## rapid run of clicks coalesces into one recomputation.
            ## The debounced reactive is created ONCE per column name and
            ## reused across remove/re-add cycles: shiny::debounce's
            ## internal observers cannot be destroyed, so re-creating one
            ## per incarnation would leak observer chains.
            if (is.null(cache$deb[[v]])) {
                raw_filter <- reactive(input[[paste0("filter_", id)]])
                db_ms <- if (widget == "slider") debounce_ms
                         else debounce_checkbox_ms
                cache$deb[[v]] <- if (db_ms > 0) debounce(raw_filter, db_ms)
                                  else raw_filter
                ## the include-NA checkbox debounces like other checkboxes
                raw_na <- reactive(input[[paste0("na_", id)]])
                cache$deb[[paste0(v, ".na")]] <-
                    if (debounce_checkbox_ms > 0) {
                        debounce(raw_na, debounce_checkbox_ms)
                    } else raw_na
            }
            filt  <- cache$deb[[v]]
            na_in <- cache$deb[[paste0(v, ".na")]]
            has_na <- info$n_na > 0
            obs_mask <- observe({
                val <- filt()
                keep_na <- if (has_na) (na_in() %||% TRUE) else TRUE
                st0 <- cache$var[[v]]
                ## re-add screening (see add_var): first event must match
                ## the rebuilt widget's known value; the stale ghost of a
                ## previous incarnation is skipped exactly once
                if (isTRUE(st0$has_expect)) {
                    cache$var[[v]]$has_expect <- FALSE
                    matches <- if (widget == "slider") {
                        !is.null(val) && length(val) == 2 &&
                            all(abs(val - st0$expect) <=
                                    st0$slider$step / 2 + 1e-9)
                    } else {
                        identical(val, st0$expect)
                    }
                    if (!matches) return()
                }
                if (!is.null(val)) cache$var[[v]]$seen <- TRUE
                ## a checkboxGroup with everything unchecked reports NULL;
                ## once the widget has spoken, NULL means "none", not "no filter"
                if (is.null(val) && widget == "checkbox" &&
                    isTRUE(cache$var[[v]]$seen)) {
                    val <- character(0)
                }
                if (!is.null(val) && widget == "slider") {
                    ## a log2 toggle repositions the slider to the SAME raw
                    ## filter; those updates echo back here and must not be
                    ## mistaken for user intent.  Pending predictions are a
                    ## LIST (rapid double-toggles queue two echoes); a
                    ## value matching any pending echo (within slider-step
                    ## quantization) is consumed silently, a non-match
                    ## clears the queue and is processed as user input.
                    echoes <- cache$var[[v]]$echoes
                    if (length(echoes)) {
                        sb <- cache$var[[v]]$slider
                        hit <- vapply(echoes, function(e) {
                            length(e) == length(val) &&
                                all(abs(val - e) <= sb$step / 2 + 1e-9)
                        }, NA)
                        if (any(hit)) {
                            cache$var[[v]]$echoes <-
                                echoes[-seq_len(max(which(hit)))]
                            return()
                        }
                        cache$var[[v]]$echoes <- list()
                    }
                    ## a slider handle AT an endpoint means "unbounded on
                    ## that side": the visible range is outlier-robust
                    ## (quantile bounds), so endpoints must not silently
                    ## drop the tails
                    sb <- cache$var[[v]]$slider
                    if (val[1] <= sb$lo + sb$step / 2) val[1] <- -Inf
                    if (val[2] >= sb$hi - sb$step / 2) val[2] <- Inf
                    ## with the log2 transform active the slider lives in
                    ## log space; the FILTER is always stored in raw units.
                    ## The slider's scale is module state (log_active, set
                    ## by obs_log), NOT a reactive read of the checkbox --
                    ## otherwise the toggle would re-run this observer
                    ## against the stale slider value and corrupt the
                    ## filter before the repositioned slider reports.
                    if (isTRUE(cache$var[[v]]$log_active)) {
                        val <- ifelse(is.finite(val), 2^val - 1, val)
                    }
                }
                ## short-circuit anything already applied: no-op re-sends
                ## and the self-echo run cost O(1), not an O(n) make_mask
                applied <- list(val, keep_na)
                if (identical(applied, cache$var[[v]]$last_applied)) return()
                cache$var[[v]]$last_applied <- applied
                ## equality-gated writes: unchanged values propagate
                ## nothing.  The compound sub-assignments below are
                ## wrapped in isolate() because `x$f[[v]] <- val` desugars
                ## to a READ of x$f then a write -- and an unisolated read
                ## here would make this observer depend on the very key it
                ## writes, re-running it once per interaction.
                if (mode == "vector") {
                    new_mask <- make_mask(cache$var[[v]]$col, val, keep_na)
                    old_mask <- isolate(maskStore[[v]])
                    ## an absent entry already means "all pass" downstream
                    if (!(is.null(old_mask) && all(new_mask)) &&
                        !identical(new_mask, old_mask)) {
                        isolate(maskStore[[v]] <- new_mask)
                    }
                }
                if (!identical(val, isolate(filterState$filters[[v]]))) {
                    isolate(filterState$filters[[v]] <- val)
                }
                if (!identical(keep_na,
                               isolate(filterState$includeNA[[v]]) %||% TRUE)) {
                    isolate(filterState$includeNA[[v]] <- keep_na)
                }
            })
            obs_list <- list(obs_mask)

            if (can_log) {
                ## toggling the transform is DISPLAY-only: the column is
                ## rebinned on the new scale and the slider repositioned,
                ## but the filter KEEPS its raw-unit value -- so no other
                ## plot's inputs change and exactly ONE plot re-renders
                ## (with one query in aggregate mode, none in vector mode)
                obs_log <- observeEvent(input[[paste0("log_", id)]], {
                    use_log <- isTRUE(input[[paste0("log_", id)]])
                    ## re-add screening (see add_var): the first event on a
                    ## rebuilt column must match the rebuilt checkbox --
                    ## anything else is the previous incarnation's stale
                    ## value and is skipped once
                    lx <- cache$var[[v]]$log_expect
                    if (!is.null(lx)) {
                        cache$var[[v]]$log_expect <- NULL
                        if (!identical(use_log, isTRUE(lx))) return()
                    }
                    if (identical(isTRUE(cache$var[[v]]$log_active), use_log)) {
                        return()   # no scale change (e.g. widget re-report)
                    }
                    cache$var[[v]]$log_active <- use_log
                    logState[[v]] <- use_log
                    cache$var[[v]]$bin <- build_bin(cache$var[[v]], use_log)
                    sb <- slider_bounds(info, log2p1 = use_log)
                    cache$var[[v]]$slider <- sb
                    ## show the CURRENT raw filter at its position on the
                    ## new scale (endpoints stand for +/-Inf as usual;
                    ## infinite bounds skip the transform rather than
                    ## producing NaN warnings)
                    raw <- isolate(filterState$filters[[v]])
                    disp <- if (is.null(raw)) c(sb$lo, sb$hi) else {
                        d <- raw
                        if (use_log) {
                            f <- is.finite(raw)
                            d[f] <- log2(raw[f] + 1)
                        }
                        c(if (is.finite(d[1])) max(d[1], sb$lo) else sb$lo,
                          if (is.finite(d[2])) min(d[2], sb$hi) else sb$hi)
                    }
                    ## queue our own update's echo for suppression
                    cache$var[[v]]$echoes <-
                        c(cache$var[[v]]$echoes, list(disp))
                    updateSliderInput(session, paste0("filter_", id),
                                      min = sb$lo, max = sb$hi,
                                      value = disp, step = sb$step)
                }, ignoreInit = TRUE)
                obs_list <- c(obs_list, list(obs_log))
            }

            if (widget == "checkbox") {
                obs_an <- observeEvent(input[[paste0("allnone_", id)]], {
                    cur <- input[[paste0("filter_", id)]] %||% character(0)
                    new_sel <- if (setequal(cur, info$levels)) character(0) else info$levels
                    updateCheckboxGroupInput(session, paste0("filter_", id),
                                             selected = new_sel)
                }, ignoreInit = TRUE)
                obs_list <- c(obs_list, list(obs_an))
            } else if (widget == "selectize") {
                ## high-cardinality columns use a selectize where EMPTY means
                ## "no filter"; the link just clears any restriction
                obs_an <- observeEvent(input[[paste0("allnone_", id)]], {
                    updateSelectizeInput(session, paste0("filter_", id),
                                         selected = character(0))
                }, ignoreInit = TRUE)
                obs_list <- c(obs_list, list(obs_an))
            }
            cache$var[[v]]$obs <- obs_list

            ## registered ONCE per variable; O(bins) per render in vector
            ## mode, a couple of memoised SQL queries in aggregate mode.
            ## PARSIMONY CHECK: even when upstream reactives invalidate,
            ## a plot whose content (counts + geometry + label + pixel
            ## size) is unchanged keeps its previous image instead of
            ## repainting -- req(cancelOutput) is Shiny's mechanism for
            ## exactly that.
            plot_id <- paste0("plot_", id)
            output[[plot_id]] <- renderPlot({
                ct <- counts_for(v)
                bin <- cache$var[[v]]$bin
                key <- list(ct, bin[setdiff(names(bin), "idx")], plot_label(v),
                            session$clientData[[paste0("output_", ns(plot_id), "_width")]],
                            session$clientData[[paste0("output_", ns(plot_id), "_height")]])
                if (identical(key, cache$var[[v]]$last_render)) {
                    req(FALSE, cancelOutput = TRUE)
                }
                cache$var[[v]]$last_render <- key
                plot_histo_counts(bin, ct$shown, ct$sel,
                                  ct$n_shown, ct$n_sel, plot_label(v),
                                  engine = plot_engine)
            })
        }

        remove_var <- function(v) {
            removeUI(paste0("#", ns(paste0("panel_", vid(v)))), immediate = TRUE)
            for (o in cache$var[[v]]$obs) o$destroy()
            cache$var[[v]] <- NULL
            maskStore[[v]] <- NULL
            if (mode == "vector") looStore[[v]] <- NULL
            ## deselecting a column removes its filtering COMPLETELY --
            ## no ghost filters (Project.md note).  With remember_removed
            ## = TRUE the settings are kept and restored on re-add; the
            ## restriction is then visible in the rebuilt widget, never
            ## silently applied while the column is deselected.
            if (!remember_removed) {
                filterState$filters[[v]]   <- NULL
                filterState$includeNA[[v]] <- NULL
                logState[[v]]              <- NULL
            }
        }

        observeEvent(input$vars, ignoreNULL = FALSE, {
            new_vars <- input$vars %||% character(0)
            old_vars <- varsNow()
            for (v in setdiff(old_vars, new_vars)) remove_var(v)
            for (v in setdiff(new_vars, old_vars)) add_var(v)
            cache$requested <- new_vars   # the user's word is final
            if (!identical(new_vars, old_vars)) varsNow(new_vars)
        })

        ## all leave-one-out masks + the global mask in one pass
        ## (prefix[i] = m1&..&mi, suffix[i] = mi&..&mk,
        ##  loo[i] = prefix[i-1] & suffix[i+1], global = prefix[k]),
        ## then EQUALITY-GATED per variable into looStore and
        ## globalMaskVal: a structural change (adding an unfiltered
        ## column) recomputes here but invalidates ONLY the entries
        ## whose content changed -- untouched plots are not even
        ## pulsed, and rows()/mask() stay bit-stable for parent apps
        ## (so e.g. the grapher's sampled scatter is not re-drawn).
        looStore <- reactiveValues()       # var -> loo mask (vector mode)
        globalMaskVal <- reactiveVal(NULL) # AND of all masks; NULL is the
                                           # canonical "all pass" so the
                                           # no-filters state never flaps
                                           # between NULL and rep(TRUE, n)
        ## the combiner exists only in vector mode (aggregate mode derives
        ## everything from filtersNow and would waste O(k*n) allocations),
        ## and runs at raised priority so plots never see a half-updated
        ## flush (no cancelled first renders, no phantom frames)
        if (mode == "vector") observe(priority = 10, x = {
            vs <- varsNow()
            k <- length(vs)
            if (k == 0) {
                if (!is.null(isolate(globalMaskVal()))) globalMaskVal(NULL)
                return()
            }
            ms <- lapply(vs, function(v) maskStore[[v]] %||% rep(TRUE, n_rows))
            prefix <- vector("list", k)
            suffix <- vector("list", k)
            acc <- ms[[1]]
            prefix[[1]] <- acc
            for (i in seq_len(k)[-1]) { acc <- acc & ms[[i]]; prefix[[i]] <- acc }
            acc <- ms[[k]]
            suffix[[k]] <- acc
            for (i in rev(seq_len(k)[-k])) { acc <- acc & ms[[i]]; suffix[[i]] <- acc }
            for (i in seq_len(k)) {
                left  <- if (i > 1) prefix[[i - 1]] else NULL
                right <- if (i < k) suffix[[i + 1]] else NULL
                loo <- if (is.null(left) && is.null(right)) rep(TRUE, n_rows)
                       else if (is.null(left)) right
                       else if (is.null(right)) left
                       else left & right
                if (!identical(loo, isolate(looStore[[vs[i]]]))) {
                    looStore[[vs[i]]] <- loo
                }
            }
            g <- prefix[[k]]
            if (all(g)) g <- NULL   # canonical all-pass
            if (!identical(g, isolate(globalMaskVal()))) {
                globalMaskVal(g)
            }
        })

        ## current filter settings of every active variable, in the shape
        ## the DBI backends' filter_clauses() expects, NORMALIZED (no-op
        ## entries dropped so they can neither add SQL clauses nor perturb
        ## query cache keys) and EQUALITY-GATED: a reactiveVal updated
        ## only when the canonical content actually changes, so events
        ## that leave the effective filter state alone (initial widget
        ## reports, adding an unfiltered column, slider echoes) invalidate
        ## none of the plots reading it
        filtersNow <- reactiveVal(list())
        observe(priority = 10, x = {
            vs <- varsNow()
            fs <- filterState$filters
            na <- filterState$includeNA
            out <- lapply(vs, function(v) {
                list(is_numeric = isTRUE(cache$var[[v]]$info$is_numeric),
                     val = fs[[v]],
                     include_na = na[[v]] %||% TRUE)
            })
            names(out) <- vs
            out <- normalize_filters(out, lapply(cache$var, `[[`, "info"))
            if (!identical(out, isolate(filtersNow()))) filtersNow(out)
        })

        globalMask <- reactive({
            if (mode == "aggregate") {
                backend$get_row_mask(filtersNow())
            } else {
                globalMaskVal() %||% rep(TRUE, n_rows)
            }
        })
        nSelected <- reactive({
            if (mode == "aggregate") backend$get_count(filtersNow())
            else sum(globalMask())
        })

        list(
            mask          = globalMask,
            rows          = reactive(which(globalMask())),
            n_selected    = nSelected,
            selected_vars = reactive(varsNow()),
            ## equality-gated like filtersNow, so structural changes with
            ## unchanged filter content don't re-run parent consumers
            filters       = local({
                out <- reactiveVal(list())
                observe(priority = 10, x = {
                    fs <- filterState$filters
                    now <- fs[intersect(names(fs), varsNow())]
                    if (!identical(now, isolate(out()))) out(now)
                })
                out
            }),
            ## Leave-one-out row streams for one column: partition the
            ## rows passing every OTHER filter by what this column's own
            ## filter did to them.  selected == rows() whenever v is a
            ## selected column; with split_range = TRUE a range filter's
            ## rejects are split into below/above (see stream_partition
            ## in thanos_plot.R for the exact semantics).  Row IDs only,
            ## like rows(): fetch data as backend$get_column(v)[ids].
            ## Reads the module's reactives, so parent reactives using
            ## it auto-update.  Works for ANY backend column, selected
            ## in Thanos or not.
            streams       = function(v, split_range = FALSE, drop_na = FALSE) {
                if (!v %in% all_columns) stop("unknown column: ", v)
                st <- cache$var[[v]]
                if (mode == "vector") {
                    universe <- if (!is.null(st)) {
                        looStore[[v]] %||% rep(TRUE, n_rows)
                    } else globalMask()
                    x <- if (!is.null(st)) st$col else backend$get_column(v)
                    ## v's own filter from the NORMALIZED, equality-gated
                    ## store (like the aggregate path) -- reading the raw
                    ## filterState here would subscribe the caller to
                    ## every no-op widget report of every column
                    own <- filtersNow()[[v]]
                    return(stream_partition(x, own$val,
                                            own$include_na %||% TRUE,
                                            universe, split_range, drop_na))
                }
                ## aggregate mode: composed from the existing memoised
                ## mask queries -- no new SQL.  Strict below/above come
                ## from complements of closed one-sided ranges, so
                ## boundaries match the vector path exactly.
                fl <- filtersNow()
                loo_f <- fl[setdiff(names(fl), v)]
                own_f <- fl[[v]]
                uni_m <- backend$get_row_mask(loo_f)
                sel_m <- backend$get_row_mask(fl) & uni_m
                with_v <- function(f) {
                    loo_f[[v]] <- f
                    loo_f
                }
                pres_m <- backend$get_row_mask(with_v(list(
                    is_numeric = TRUE, val = NULL, include_na = FALSE)))
                if (drop_na) {
                    uni_m <- uni_m & pres_m
                    sel_m <- sel_m & pres_m
                }
                ranged <- split_range && !is.null(own_f) &&
                    !is.character(own_f$val)
                if (!ranged) {
                    return(list(selected = which(sel_m),
                                excluded = which(uni_m & !sel_m)))
                }
                lo <- own_f$val[1]; hi <- own_f$val[2]
                ge_lo <- if (is.finite(lo)) {
                    backend$get_row_mask(with_v(list(
                        is_numeric = TRUE, val = c(lo, Inf),
                        include_na = FALSE)))
                } else pres_m
                le_hi <- if (is.finite(hi)) {
                    backend$get_row_mask(with_v(list(
                        is_numeric = TRUE, val = c(-Inf, hi),
                        include_na = FALSE)))
                } else pres_m
                keep_na <- isTRUE(own_f$include_na)
                list(selected = which(sel_m),
                     below = which(uni_m & pres_m & !ge_lo),
                     above = which(uni_m & pres_m & !le_hi),
                     na = if (keep_na || drop_na) integer(0)
                          else which(uni_m & !pres_m))
            },
            ## parent -> module: add columns to the filter selection.
            ## Purely additive and idempotent; the update round-trips
            ## through the selectize, so panels appear via the normal
            ## add_var path and the user can still remove them by hand.
            add_vars      = function(cols) {
                cols <- intersect(cols, all_columns)
                ## union against the last REQUESTED selection, not the
                ## reactive varsNow(): at startup the default_selected
                ## update is still round-tripping and varsNow() is empty,
                ## so a parent's early add_vars() (e.g. the grapher's
                ## axis bridge firing on init) must not clobber it
                want <- union(cache$requested, cols)
                if (!setequal(want, cache$requested)) {
                    cache$requested <- want
                    ## selected only: choices were registered at init and
                    ## never change (a full choices re-send would reload
                    ## the widget client-side)
                    updateSelectizeInput(session, "vars", selected = want)
                }
                invisible(want)
            }
        )
    })
}

## Drop filter entries that impose no restriction: NULL values, fully
## unbounded slider ranges, or a categorical/discrete selection covering
## every level -- all with include-NA on.  They contribute no SQL clause,
## and keeping them would only perturb query cache keys.
## An entry with include_na = FALSE always restricts (it drops NA rows)
## and is always kept.
normalize_filters <- function(fl, infos) {
    keep <- vapply(names(fl), function(v) {
        f <- fl[[v]]
        if (!isTRUE(f$include_na)) return(TRUE)
        if (is.null(f$val)) return(FALSE)
        if (f$is_numeric && !is.character(f$val)) {
            return(any(is.finite(f$val)))
        }
        levs <- infos[[v]]$levels
        !(!is.null(levs) && setequal(f$val, levs))
    }, NA)
    fl[keep]
}

## Slider geometry for a numeric variable, over the outlier-robust
## display range (see display_range).  Kept separate from the UI builder
## because the server also needs the bounds to translate endpoint
## positions into "unbounded".
slider_bounds <- function(info, log2p1 = FALSE) {
    rng <- display_range(info)
    if (log2p1 && all(is.finite(rng))) rng <- log2(rng + 1)
    if (!all(is.finite(rng))) rng <- c(0, 1)
    span <- rng[2] - rng[1]
    step <- signif(span / 100, digits = 1)
    if (!is.finite(step) || step == 0) step <- 0.01
    ## integer steps make no sense in log space
    if (!log2p1 && isTRUE(info$is_integerish) && span >= 1) {
        step <- max(1, round(step))
    }
    list(lo = floor(rng[1] / step) * step,
         hi = ceiling(rng[2] / step) * step,
         step = step)
}

## Build the inserted panel for one variable: filter widget, optional
## all/none link and include-NA checkbox, then the histogram.
make_var_panel <- function(ns, v, id, info, widget, stored, stored_na,
                           plot_height, slider = NULL,
                           can_log = FALSE, stored_log = FALSE) {
    filter_id <- ns(paste0("filter_", id))
    extra <- NULL
    if (widget == "slider") {
        sb <- slider %||% slider_bounds(info, log2p1 = stored_log)
        ## stored filters are raw units; the slider may live in log space,
        ## and stored values may contain +/-Inf (endpoint = unbounded)
        val <- stored %||% c(sb$lo, sb$hi)
        if (stored_log) val <- log2(val + 1)
        val <- pmin(pmax(val, sb$lo), sb$hi)
        filter_ui <- sliderInput(filter_id, v, min = sb$lo, max = sb$hi,
                                 value = val, step = sb$step, width = "100%")
    } else if (widget == "checkbox") {
        filter_ui <- checkboxGroupInput(filter_id, v, choices = info$levels,
                                        selected = stored %||% info$levels,
                                        inline = TRUE, width = "100%")
        extra <- actionLink(ns(paste0("allnone_", id)), "all/none")
    } else {
        filter_ui <- selectizeInput(filter_id, v, choices = info$levels,
                                    selected = stored, multiple = TRUE,
                                    width = "100%",
                                    options = list(
                                        plugins = list("remove_button"),
                                        placeholder = "no filter (all values)"))
        extra <- actionLink(ns(paste0("allnone_", id)), "clear")
    }
    na_ui <- if (info$n_na > 0) {
        checkboxInput(ns(paste0("na_", id)),
                      sprintf("include NA (%s)", format(info$n_na, big.mark = ",")),
                      value = stored_na)
    }
    log_ui <- if (can_log) {
        checkboxInput(ns(paste0("log_", id)), "log2 scale", value = stored_log)
    }
    ## secondary controls share one compact muted row (styled by the
    ## .thanos-controls CSS in thanosUI) so the widget + plot stay primary
    controls <- if (!is.null(extra) || !is.null(na_ui) || !is.null(log_ui)) {
        div(class = "thanos-controls", extra, na_ui, log_ui)
    }
    div(id = ns(paste0("panel_", id)), class = "thanos-panel",
        filter_ui, controls,
        plotOutput(ns(paste0("plot_", id)), height = plot_height, width = "100%"))
}
