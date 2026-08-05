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
##                       a parent app to drive the filter set -- e.g. a
##                       grapher adding its plotted axes so their NAs and
##                       ranges become user-controllable.
##
## Reactive architecture (why it is fast):
##   - each variable owns ONE debounced mask reactive; moving a slider
##     recomputes only that variable's O(n) mask
##   - leave-one-out masks for all k variables come from one reactive
##     using prefix/suffix cumulative ANDs: O(3k) vector ANDs, not O(k^2)
##   - each plot is registered once and reads pre-binned indices, so a
##     render is O(bins), not O(rows)
################################################################
library(shiny)

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

        ## non-reactive per-session caches, keyed by column name
        cache <- new.env(parent = emptyenv())
        cache$col    <- list()  # full column vectors (vector mode only)
        cache$bin    <- list()  # bin_column() / bin_spec_from_info() results
        cache$info   <- list()  # get_column_info() results
        cache$obs    <- list()  # observers to destroy when a var is removed
        cache$seen   <- list()  # TRUE once a checkbox filter has sent a value
        cache$widget <- list()  # "slider" | "checkbox" | "selectize"
        cache$slider <- list()  # slider bounds (lo/hi/step) per numeric var

        maskStore   <- reactiveValues()  # var -> logical(n_rows)
        filterState <- reactiveValues(filters = list(), includeNA = list(),
                                      log = list())
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

        add_var <- function(v) {
            info <- backend$get_column_info(v)
            id <- vid(v)
            ## a numeric column with few distinct values ('month') gets
            ## checkboxes with membership semantics instead of a slider
            discrete <- isTRUE(info$is_numeric) && !is.null(info$values) &&
                (info$n_unique %||% Inf) <= max_discrete_numeric
            if (discrete) info$levels <- as.character(info$values)
            cache$info[[v]] <- info
            stored_log0 <- isTRUE(isolate(filterState$log[[v]]))
            if (mode == "vector") {
                x <- backend$get_column(v)
                cache$col[[v]] <- x
                cache$bin[[v]] <- bin_column(x, bins,
                    discrete_values = if (discrete) info$values,
                    range = if (info$is_numeric && !discrete) display_range(info),
                    log2p1 = stored_log0)
            } else {
                cache$bin[[v]] <- bin_spec_from_info(info, bins,
                                                     discrete = discrete,
                                                     log2p1 = stored_log0)
            }
            widget <- if (info$is_numeric && !discrete) "slider"
                      else if (length(info$levels) <= max_checkbox_levels) "checkbox"
                      else "selectize"
            cache$widget[[v]] <- widget
            stored     <- isolate(filterState$filters[[v]])
            stored_na  <- isolate(filterState$includeNA[[v]]) %||% TRUE
            stored_log <- isTRUE(isolate(filterState$log[[v]]))
            ## log2(x+1) toggle: non-negative sliders only, and in
            ## aggregate mode only if the SQL engine has log2()
            can_log <- widget == "slider" && isTRUE(info$range[1] >= 0) &&
                (mode == "vector" || isTRUE(backend$supports_log2))
            if (!can_log) stored_log <- FALSE
            if (widget == "slider") {
                cache$slider[[v]] <- slider_bounds(info, log2p1 = stored_log)
            }
            insertUI(paste0("#", ns("panels")), where = "beforeEnd",
                     immediate = TRUE,
                     ui = make_var_panel(ns, v, id, info, widget,
                                         stored, stored_na, plot_height,
                                         slider = cache$slider[[v]],
                                         can_log = can_log,
                                         stored_log = stored_log))

            ## one mask observer per variable: any change to its filter or
            ## include-NA input recomputes only THIS variable's mask
            ## debounce sliders AND checkboxes: a lazy drag or a rapid
            ## run of clicks coalesces into one recomputation
            raw_filter <- reactive(input[[paste0("filter_", id)]])
            db_ms <- if (widget == "slider") debounce_ms else debounce_checkbox_ms
            filt <- if (db_ms > 0) debounce(raw_filter, db_ms) else raw_filter
            has_na <- info$n_na > 0
            obs_mask <- observe({
                val <- filt()
                keep_na <- if (has_na) (input[[paste0("na_", id)]] %||% TRUE) else TRUE
                if (!is.null(val)) cache$seen[[v]] <- TRUE
                ## a checkboxGroup with everything unchecked reports NULL;
                ## once the widget has spoken, NULL means "none", not "no filter"
                if (is.null(val) && widget == "checkbox" && isTRUE(cache$seen[[v]])) {
                    val <- character(0)
                }
                ## a slider handle AT an endpoint means "unbounded on that
                ## side": the visible range is outlier-robust (quantile
                ## bounds), so endpoints must not silently drop the tails
                if (!is.null(val) && widget == "slider") {
                    sb <- cache$slider[[v]]
                    if (val[1] <= sb$lo + sb$step / 2) val[1] <- -Inf
                    if (val[2] >= sb$hi - sb$step / 2) val[2] <- Inf
                    ## with the log2 transform active the slider lives in
                    ## log space; the FILTER is always stored in raw units
                    if (can_log && isTRUE(input[[paste0("log_", id)]])) {
                        val <- ifelse(is.finite(val), 2^val - 1, val)
                    }
                }
                if (mode == "vector") {
                    maskStore[[v]] <- make_mask(cache$col[[v]], val, keep_na)
                }
                filterState$filters[[v]]   <- val
                filterState$includeNA[[v]] <- keep_na
            })
            obs_list <- list(obs_mask)

            if (can_log) {
                ## toggling the transform rebins the column and rescales
                ## the slider; the filter resets to "no restriction"
                obs_log <- observeEvent(input[[paste0("log_", id)]], {
                    use_log <- isTRUE(input[[paste0("log_", id)]])
                    filterState$log[[v]] <- use_log
                    if (mode == "vector") {
                        cache$bin[[v]] <- bin_column(cache$col[[v]], bins,
                            range = display_range(info), log2p1 = use_log)
                    } else {
                        cache$bin[[v]] <- bin_spec_from_info(info, bins,
                                                             log2p1 = use_log)
                    }
                    sb <- slider_bounds(info, log2p1 = use_log)
                    cache$slider[[v]] <- sb
                    updateSliderInput(session, paste0("filter_", id),
                                      min = sb$lo, max = sb$hi,
                                      value = c(sb$lo, sb$hi), step = sb$step)
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
            cache$obs[[v]] <- obs_list

            ## registered ONCE; O(bins) per render thanks to bin_column(),
            ## or four SQL aggregate queries in aggregate mode
            plot_label <- function() {
                if (isTRUE(filterState$log[[v]])) paste0(v, " (log2+1)") else v
            }
            ## both engines draw the same visual; "base" skips the
            ## ggplot/grid pipeline (a ggplot object must be returned for
            ## renderPlot to print, base draws directly and returns NULL)
            draw <- function(spec, shown, sel, n_shown, n_sel, label) {
                if (plot_engine == "base") {
                    plot_histo_counts_base(spec, shown, sel, n_shown, n_sel,
                                           label)
                } else {
                    plot_histo_counts(spec, shown, sel, n_shown, n_sel, label)
                }
            }
            output[[paste0("plot_", id)]] <- if (mode == "vector") {
                renderPlot({
                    loo <- looMasks()[[v]]
                    req(!is.null(loo))
                    own <- maskStore[[v]] %||% rep(TRUE, n_rows)
                    bin <- cache$bin[[v]]
                    if (bin$nbins == 0) {
                        draw(bin, integer(0), integer(0),
                             sum(loo), sum(own & loo), plot_label())
                    } else {
                        draw(bin,
                             shown = tabulate(bin$idx[loo], nbins = bin$nbins),
                             sel   = tabulate(bin$idx[own & loo],
                                              nbins = bin$nbins),
                             n_shown = sum(loo), n_sel = sum(own & loo),
                             plot_label())
                    }
                })
            } else {
                renderPlot({
                    fl <- filtersNow()   # normalized: no-op filters absent
                    req(v %in% varsNow())
                    spec <- cache$bin[[v]]
                    own_f <- fl[[v]]     # NULL when v's filter is inactive
                    loo_f <- fl[setdiff(names(fl), v)]
                    ## one combined query for both count vectors; the
                    ## global row count is a reactive shared by all plots.
                    ## When v has no active filter, its leave-one-out set
                    ## IS the full filter set, so n_shown = n_sel and the
                    ## per-plot count query is skipped -- with normalized
                    ## filters an interaction costs k pair queries + one
                    ## loo count per ACTIVELY filtered column + 1 global,
                    ## instead of 2k + 1
                    pair <- backend$get_binned_pair(v, spec, loo_f, own_f)
                    draw(spec, shown = pair$shown, sel = pair$sel,
                         n_shown = if (is.null(own_f)) nSelected()
                                   else backend$get_count(loo_f),
                         n_sel   = nSelected(),
                         plot_label())
                })
            }
        }

        remove_var <- function(v) {
            removeUI(paste0("#", ns(paste0("panel_", vid(v)))), immediate = TRUE)
            for (o in cache$obs[[v]]) o$destroy()
            cache$obs[[v]]    <- NULL
            cache$col[[v]]    <- NULL
            cache$bin[[v]]    <- NULL
            cache$info[[v]]   <- NULL
            cache$seen[[v]]   <- NULL
            cache$widget[[v]] <- NULL
            cache$slider[[v]] <- NULL
            maskStore[[v]] <- NULL
            ## deselecting a column removes its filtering COMPLETELY --
            ## no ghost filters (Project.md note).  With remember_removed
            ## = TRUE the settings are kept and restored on re-add; the
            ## restriction is then visible in the rebuilt widget, never
            ## silently applied while the column is deselected.
            if (!remember_removed) {
                filterState$filters[[v]]   <- NULL
                filterState$includeNA[[v]] <- NULL
                filterState$log[[v]]       <- NULL
            }
        }

        observeEvent(input$vars, ignoreNULL = FALSE, {
            new_vars <- input$vars %||% character(0)
            old_vars <- varsNow()
            for (v in setdiff(old_vars, new_vars)) remove_var(v)
            for (v in setdiff(new_vars, old_vars)) add_var(v)
            varsNow(new_vars)
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
        ## the DBI backends' filter_clauses() expects (aggregate mode),
        ## NORMALIZED: entries that impose no restriction are dropped.
        ## An untouched widget still reports a value (full-range slider,
        ## every box ticked) which adds no SQL clause -- but if it stayed
        ## in this list it would change every plot's cache key, so merely
        ## ADDING a column used to cold-recompute all existing plots
        ## (the demo_big replot-on-add / queue pile-up bug).
        filtersNow <- reactive({
            vs <- varsNow()
            fs <- filterState$filters
            na <- filterState$includeNA
            out <- lapply(vs, function(v) {
                list(is_numeric = isTRUE(cache$info[[v]]$is_numeric),
                     val = fs[[v]],
                     include_na = na[[v]] %||% TRUE)
            })
            names(out) <- vs
            normalize_filters(out, cache$info)
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
