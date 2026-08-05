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
        cache$var <- list()

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
        updateSelectizeInput(session, "vars", choices = all_columns,
            selected = intersect(default_selected %||% character(0), all_columns),
            server = TRUE)

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
                loo <- looMasks()[[v]]
                req(!is.null(loo))
                own <- maskStore[[v]] %||% rep(TRUE, n_rows)
                bin_counts(cache$var[[v]]$bin, loo, own)
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
                       can_log = can_log, seen = FALSE,
                       log_active = stored_log,
                       col = if (mode == "vector") backend$get_column(v),
                       slider = if (widget == "slider")
                                    slider_bounds(info, log2p1 = stored_log))
            st$bin <- build_bin(st, stored_log)
            cache$var[[v]] <- st

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
            raw_filter <- reactive(input[[paste0("filter_", id)]])
            db_ms <- if (widget == "slider") debounce_ms else debounce_checkbox_ms
            filt <- if (db_ms > 0) debounce(raw_filter, db_ms) else raw_filter
            has_na <- info$n_na > 0
            obs_mask <- observe({
                val <- filt()
                keep_na <- if (has_na) (input[[paste0("na_", id)]] %||% TRUE) else TRUE
                if (!is.null(val)) cache$var[[v]]$seen <- TRUE
                ## a checkboxGroup with everything unchecked reports NULL;
                ## once the widget has spoken, NULL means "none", not "no filter"
                if (is.null(val) && widget == "checkbox" &&
                    isTRUE(cache$var[[v]]$seen)) {
                    val <- character(0)
                }
                if (!is.null(val) && widget == "slider") {
                    ## a log2 toggle repositions the slider to the SAME raw
                    ## filter; that update echoes back here and must not be
                    ## mistaken for user intent (echo = the value we
                    ## predicted, within slider-step quantization)
                    echo <- cache$var[[v]]$echo
                    if (!is.null(echo)) {
                        cache$var[[v]]$echo <- NULL
                        sb <- cache$var[[v]]$slider
                        if (all(abs(val - echo) <= sb$step / 2 + 1e-9)) return()
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
                ## equality-gated writes: unchanged values propagate nothing
                if (mode == "vector") {
                    new_mask <- make_mask(cache$var[[v]]$col, val, keep_na)
                    old_mask <- isolate(maskStore[[v]])
                    ## an absent entry already means "all pass" downstream
                    if (!(is.null(old_mask) && all(new_mask)) &&
                        !identical(new_mask, old_mask)) {
                        maskStore[[v]] <- new_mask
                    }
                }
                if (!identical(val, isolate(filterState$filters[[v]]))) {
                    filterState$filters[[v]] <- val
                }
                if (!identical(keep_na,
                               isolate(filterState$includeNA[[v]]) %||% TRUE)) {
                    filterState$includeNA[[v]] <- keep_na
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
                    if (identical(isTRUE(cache$var[[v]]$log_active), use_log)) {
                        return()   # no scale change (e.g. widget re-report)
                    }
                    cache$var[[v]]$log_active <- use_log
                    logState[[v]] <- use_log
                    cache$var[[v]]$bin <- build_bin(cache$var[[v]], use_log)
                    sb <- slider_bounds(info, log2p1 = use_log)
                    cache$var[[v]]$slider <- sb
                    ## show the CURRENT raw filter at its position on the
                    ## new scale (endpoints stand for +/-Inf as usual)
                    raw <- isolate(filterState$filters[[v]])
                    disp <- if (is.null(raw)) c(sb$lo, sb$hi) else {
                        d <- if (use_log) log2(raw + 1) else raw
                        c(if (is.finite(d[1])) max(d[1], sb$lo) else sb$lo,
                          if (is.finite(d[2])) min(d[2], sb$hi) else sb$hi)
                    }
                    cache$var[[v]]$echo <- disp  # our own update: suppress it
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
            if (!identical(new_vars, old_vars)) varsNow(new_vars)
        })

        ## all leave-one-out masks + the global mask in one pass:
        ## prefix[i] = m1&..&mi, suffix[i] = mi&..&mk,
        ## loo[i] = prefix[i-1] & suffix[i+1], global = prefix[k]
        looMasks <- reactive({
            vs <- varsNow()
            k <- length(vs)
            if (k == 0) return(structure(list(), global = NULL))
            ms <- lapply(vs, function(v) maskStore[[v]] %||% rep(TRUE, n_rows))
            prefix <- vector("list", k)
            suffix <- vector("list", k)
            acc <- ms[[1]]
            prefix[[1]] <- acc
            for (i in seq_len(k)[-1]) { acc <- acc & ms[[i]]; prefix[[i]] <- acc }
            acc <- ms[[k]]
            suffix[[k]] <- acc
            for (i in rev(seq_len(k)[-k])) { acc <- acc & ms[[i]]; suffix[[i]] <- acc }
            loo <- vector("list", k)
            for (i in seq_len(k)) {
                left  <- if (i > 1) prefix[[i - 1]] else NULL
                right <- if (i < k) suffix[[i + 1]] else NULL
                loo[[i]] <- if (is.null(left) && is.null(right)) rep(TRUE, n_rows)
                            else if (is.null(left)) right
                            else if (is.null(right)) left
                            else left & right
            }
            names(loo) <- vs
            structure(loo, global = prefix[[k]])
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
        observe({
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
                attr(looMasks(), "global") %||% rep(TRUE, n_rows)
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
            filters       = reactive({
                fs <- filterState$filters
                fs[intersect(names(fs), varsNow())]
            }),
            ## parent -> module: add columns to the filter selection.
            ## Purely additive and idempotent; the update round-trips
            ## through the selectize, so panels appear via the normal
            ## add_var path and the user can still remove them by hand.
            add_vars      = function(cols) {
                cols <- intersect(cols, all_columns)
                want <- union(isolate(varsNow()), cols)
                if (!setequal(want, isolate(varsNow()))) {
                    updateSelectizeInput(session, "vars",
                                         choices = all_columns,
                                         selected = want, server = TRUE)
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
