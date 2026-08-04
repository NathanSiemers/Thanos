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
        selectizeInput(ns("vars"), "Filter columns", choices = NULL,
                       multiple = TRUE, width = width),
        div(id = ns("panels"))
    )
}

thanosServer <- function(id, backend,
                         default_selected = NULL,
                         bins = 50,
                         debounce_ms = 300,
                         plot_height = "150px",
                         max_checkbox_levels = 30) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        n_rows <- backend$n_rows()

        ## non-reactive per-session caches, keyed by column name
        cache <- new.env(parent = emptyenv())
        cache$col    <- list()  # full column vectors (fetched once per selection)
        cache$bin    <- list()  # bin_column() results
        cache$obs    <- list()  # observers to destroy when a var is removed
        cache$seen   <- list()  # TRUE once a checkbox filter has sent a value
        cache$widget <- list()  # "slider" | "checkbox" | "selectize"

        maskStore   <- reactiveValues()  # var -> logical(n_rows)
        filterState <- reactiveValues(filters = list(), includeNA = list())
        varsNow     <- reactiveVal(character(0))

        all_columns <- backend$get_columns()
        updateSelectizeInput(session, "vars", choices = all_columns,
            selected = intersect(default_selected %||% character(0), all_columns),
            server = TRUE)

        ## column name -> id-safe fragment for input/output ids and selectors
        vid <- function(v) gsub("[^A-Za-z0-9_]", "_", v)

        add_var <- function(v) {
            x <- backend$get_column(v)
            info <- backend$get_column_info(v)
            id <- vid(v)
            cache$col[[v]] <- x
            cache$bin[[v]] <- bin_column(x, bins)
            widget <- if (info$is_numeric) "slider"
                      else if (length(info$levels) <= max_checkbox_levels) "checkbox"
                      else "selectize"
            cache$widget[[v]] <- widget
            stored    <- isolate(filterState$filters[[v]])
            stored_na <- isolate(filterState$includeNA[[v]]) %||% TRUE
            insertUI(paste0("#", ns("panels")), where = "beforeEnd",
                     immediate = TRUE,
                     ui = make_var_panel(ns, v, id, info, widget,
                                         stored, stored_na, plot_height))

            ## one mask observer per variable: any change to its filter or
            ## include-NA input recomputes only THIS variable's mask
            raw_filter <- reactive(input[[paste0("filter_", id)]])
            filt <- if (info$is_numeric && debounce_ms > 0) {
                debounce(raw_filter, debounce_ms)
            } else raw_filter
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
                maskStore[[v]] <- make_mask(cache$col[[v]], val, keep_na)
                filterState$filters[[v]]   <- val
                filterState$includeNA[[v]] <- keep_na
            })
            obs_list <- list(obs_mask)

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

            ## registered ONCE; O(bins) per render thanks to bin_column()
            output[[paste0("plot_", id)]] <- renderPlot({
                loo <- looMasks()[[v]]
                req(!is.null(loo))
                own <- maskStore[[v]] %||% rep(TRUE, n_rows)
                plot_histo(cache$bin[[v]], loo, own, v)
            })
        }

        remove_var <- function(v) {
            removeUI(paste0("#", ns(paste0("panel_", vid(v)))), immediate = TRUE)
            for (o in cache$obs[[v]]) o$destroy()
            cache$obs[[v]]    <- NULL
            cache$col[[v]]    <- NULL
            cache$bin[[v]]    <- NULL
            cache$seen[[v]]   <- NULL
            cache$widget[[v]] <- NULL
            maskStore[[v]] <- NULL
            ## filterState deliberately kept: re-adding the variable
            ## restores its previous filter settings
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

        globalMask <- reactive({
            attr(looMasks(), "global") %||% rep(TRUE, n_rows)
        })

        list(
            mask          = globalMask,
            rows          = reactive(which(globalMask())),
            n_selected    = reactive(sum(globalMask())),
            selected_vars = reactive(varsNow()),
            filters       = reactive({
                fs <- filterState$filters
                fs[intersect(names(fs), varsNow())]
            })
        )
    })
}

## Build the inserted panel for one variable: filter widget, optional
## all/none link and include-NA checkbox, then the histogram.
make_var_panel <- function(ns, v, id, info, widget, stored, stored_na,
                           plot_height) {
    filter_id <- ns(paste0("filter_", id))
    extra <- NULL
    if (widget == "slider") {
        rng <- info$range
        if (!all(is.finite(rng))) rng <- c(0, 1)
        span <- rng[2] - rng[1]
        step <- signif(span / 100, digits = 1)
        if (!is.finite(step) || step == 0) step <- 0.01
        if (isTRUE(info$is_integerish) && span >= 1) step <- max(1, round(step))
        lo <- floor(rng[1] / step) * step
        hi <- ceiling(rng[2] / step) * step
        filter_ui <- sliderInput(filter_id, v, min = lo, max = hi,
                                 value = stored %||% c(lo, hi),
                                 step = step, width = "100%")
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
    div(id = ns(paste0("panel_", id)), class = "thanos-panel",
        filter_ui, extra, na_ui,
        plotOutput(ns(paste0("plot_", id)), height = plot_height, width = "100%"))
}
