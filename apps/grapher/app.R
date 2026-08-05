################################################################
## GRAPHER: an independent Shiny app with Thanos plugged in.
##
## This app is the INTEGRATION REFERENCE.  It is a self-standing
## scatter-plot application (pick x, y, color, size, draw points) that
## acquired filtering by adding Thanos at FOUR required points plus one
## optional one, each marked with a "PLUG-IN POINT" banner below:
##
##   1. load Thanos (package or loader)       (once, at startup)
##   2. create a backend around your data     (once, at startup)
##   3. thanosUI("<id>")   somewhere in ui    (one line)
##   4. thanosServer("<id>", backend, ...) in server(), keeping its
##      return value                          (one line)
##   5. OPTIONAL, parent -> module: th$add_vars(cols) to push columns
##      into the filter selection (here: the plotted axes, so their
##      NAs and ranges are always user-controllable)
##
## HOW THE TWO APPS INTERACT after that -- the whole contract:
##
##   parent app  -- add_vars(cols) only, optional -->  Thanos
##   parent app  <-------- reactives ---------------- Thanos
##
##   Thanos never calls into your app, never modifies your data, and
##   never hands you a filtered copy of anything.  The single sanctioned
##   parent->module call is th$add_vars(cols) (see PLUG-IN POINT 5).
##   Everything else flows the other way: Thanos returns a list of
##   REACTIVES, and you read the ones you want inside your own
##   reactive()/renderPlot()/renderTable() code:
##
##     th$rows()          integer row IDs passing all current filters --
##                        the cheap "pointer"; subset your own data with
##                        it (df[th$rows(), ] or a WHERE row_id IN ...)
##     th$mask()          the same thing as a logical(n_rows) vector
##     th$n_selected()    how many rows survive (cheap; use for captions)
##     th$selected_vars() which columns the user is filtering on
##     th$filters()       the current filter settings, in raw units
##                        (save/restore, bookmarking, audit)
##     th$add_vars(cols)  parent -> module: ensure these columns have
##                        filter panels (additive, idempotent, unknown
##                        names ignored)
##
##   Because these are reactives, Shiny does the wiring: any output of
##   yours that reads th$rows() re-renders automatically when the user
##   touches a Thanos filter.  There are no events to subscribe to, no
##   observers to write, no invalidation to manage.
##
##   Values are debounced inside the module (slider drags and rapid
##   checkbox runs coalesce), so your outputs see settled states, not
##   every intermediate wiggle.  All Thanos input/output IDs live under
##   its module namespace ("thanos-..."), so they cannot collide with
##   yours; its CSS is scoped to its own panels, so your styling is
##   untouched.  You can embed several independent Thanos instances by
##   using different ids (thanosUI("f1") + thanosServer("f1", ...)).
##
## Run with:  shiny::runApp("apps/grapher")
################################################################
library(shiny)
library(ggplot2)
library(nycflights13)

################################################################
## PLUG-IN POINT 1 of 4: load Thanos.  Two equivalent ways:
##   a) install the thanos package once (pak::pak("NathanSiemers/Thanos")
##      or remotes::install_github), then library(thanos), or
##   b) no install at all: copy this repo (or add it as a submodule)
##      and source the ONE loader file, thanos.R, at its root.
## Either way exactly six functions are defined here (thanosUI,
## thanosServer, backend_memory, backend_dbi, backend_sqlite,
## backend_duckdb); the source() route additionally provides `thanos`,
## a handle to the private namespace.  All internals stay inside a
## namespace in both cases, so nothing Thanos uses can collide with
## functions your app defines, and your same-named functions cannot
## break the module.  The shim below prefers the package and falls
## back to the loader, so this app runs in both worlds.
################################################################
if (requireNamespace("thanos", quietly = TRUE)) {
    library(thanos)
} else {
    thanos_loader <- Filter(file.exists,
                            file.path(c(".", "..", "../.."), "thanos.R"))[1]
    if (is.na(thanos_loader)) {
        stop("install the thanos package or run from the repo checkout")
    }
    source(thanos_loader)
}

################################################################
## PLUG-IN POINT 2 of 4: wrap your data in a backend.
## The backend decides where the data lives; the module code never
## changes.  Pick ONE:
##   backend_memory(your_data_frame)          in-memory data frame
##   backend_sqlite("path/to/db.sqlite")      tall/skinny SQLite
##   backend_duckdb("path/to/db.duckdb")      tall/skinny DuckDB
##                                            (the choice at many
##                                             millions of rows)
## IMPORTANT: share ONE backend object between Thanos and your own
## code, as done here.  Then the columns the user can filter on and
## the columns your app plots are guaranteed to be the same set, and
## row IDs from th$rows() index the same row order you fetch.
################################################################
backend <- backend_memory(as.data.frame(nycflights13::flights))

## Everything from here to the server function is ordinary app code
## that existed "before Thanos": column choices for the plot controls,
## a cap so geom_point stays responsive.
PLOT_CAP <- 50000
numeric_cols <- Filter(function(cn) backend$get_column_info(cn)$is_numeric,
                       backend$get_columns())

ui <- fluidPage(
    titlePanel("Grapher, powered by Thanos"),
    sidebarLayout(
        sidebarPanel(width = 5,
            h4("Filters"),
            ############################################################
            ## PLUG-IN POINT 3 of 4: place the module UI.
            ## One call, anywhere in your layout -- a sidebar, a tab, a
            ## modal.  The id ("thanos") just has to match point 4.
            ## Everything inside (column picker, widgets, histograms,
            ## the explanatory note) is generated and managed by the
            ## module; you never reference its internals.
            ############################################################
            thanosUI("thanos")
        ),
        mainPanel(width = 7,
            ## the grapher's own controls -- plain app code, note it can
            ## reuse the shared backend for metadata (column names/types)
            fluidRow(
                column(3, selectInput("x", "x", choices = numeric_cols,
                                      selected = "dep_delay")),
                column(3, selectInput("y", "y", choices = numeric_cols,
                                      selected = "arr_delay")),
                column(3, selectInput("color", "color",
                                      choices = c("(none)", backend$get_columns()),
                                      selected = "origin")),
                column(3, selectInput("size", "size",
                                      choices = c("(none)", numeric_cols),
                                      selected = "(none)"))
            ),
            fluidRow(
                column(6, checkboxInput("show_excluded",
                    "show points excluded by the x column's filter",
                    value = FALSE)),
                column(6, checkboxInput("fit_slopes",
                    "fit linear slopes", value = TRUE))
            ),
            plotOutput("scatter", height = "500px"),
            textOutput("counts"),
            ## slope estimates + the selected-vs-excluded slope test
            verbatimTextOutput("slopes")
        )
    )
)

server <- function(input, output, session) {
    ############################################################
    ## PLUG-IN POINT 4 of 4: start the module server; KEEP the
    ## return value.  `th` is your only handle on Thanos.
    ##   - id must match thanosUI() above
    ##   - backend is the shared object from point 2
    ##   - everything else is optional tuning (defaults are sane):
    ##       default_selected     columns pre-selected for filtering
    ##       debounce_ms          slider settle time (default 300)
    ##       debounce_checkbox_ms checkbox settle time (default 300)
    ##       bins                 histogram bins (default 50)
    ##       max_discrete_numeric numerics with <= this many distinct
    ##                            values get checkboxes (default 12)
    ##       remember_removed     re-adding a column restores its old
    ##                            filter (default FALSE = forget)
    ##       mode                 "auto": in-R filtering normally, SQL
    ##                            aggregation on huge DB backends
    ############################################################
    th <- thanosServer("thanos", backend,
                       default_selected = c("carrier", "origin",
                                            "dep_delay", "distance"))

    ############################################################
    ## PLUG-IN POINT 5 (OPTIONAL): parent -> module, the ONLY
    ## message that ever flows in that direction.
    ##
    ## th$add_vars(cols) asks Thanos to include those columns in its
    ## "Filter columns" selection, exactly as if the user had picked
    ## them: panels appear through the module's normal path, with
    ## their include-NA checkboxes, sliders/checkboxes, and
    ## histograms.
    ##
    ## WHY the grapher does this: the columns this app PLOTS are not
    ## automatically columns Thanos FILTERS.  Without this, a y axis
    ## like arr_delay can carry NAs (cancelled flights) that pass the
    ## filters untouched -- correct, but invisible until ggplot drops
    ## them.  By adding the plotted axes to the filter set, the user
    ## always has that column's include-NA checkbox and range slider
    ## in front of them, so 'why are rows missing from my plot?' has
    ## a visible, adjustable answer.
    ##
    ## Behavior notes an integrator should know:
    ##   - add_vars() only ADDS, and is idempotent: nothing happens
    ##     if the columns are already selected.
    ##   - the user keeps full control: they can remove the panel
    ##     again; it will only come back if the axis CHANGES (this
    ##     observer fires on input$x/input$y changes, not on the
    ##     module's own selection changes -- no tug-of-war loop).
    ##   - unknown column names are ignored, so it is safe to pass
    ##     UI state through directly.
    ############################################################
    observeEvent(c(input$x, input$y), {
        th$add_vars(c(input$x, input$y))
    })

    ############################################################
    ## INTERACTION, part 1: consume the row pointer.
    ## This reactive is the ENTIRE data hand-off.  Reading th$rows()
    ## inside it subscribes it to the filters: whenever the user moves
    ## a slider or ticks a box, plot_rows() -- and everything built on
    ## it -- recomputes automatically.  No other glue exists.
    ############################################################
    plot_rows <- reactive({
        r <- th$rows()                              # <- the hand-off
        if (length(r) > PLOT_CAP) sort(sample(r, PLOT_CAP)) else r
    })

    ## INTERACTION, part 2: the parent fetches ITS OWN data for the
    ## surviving rows -- Thanos never ships data, only the pointer.
    ## With a DB backend this same code would fetch just the plotted
    ## columns for just the surviving rows.
    ##
    ## NA NOTE -- read this if you see ggplot 'Removed N rows' warnings:
    ## th$rows() rightly includes rows with NAs (each filter column has
    ## an 'include NA' checkbox, default ON, and columns you never
    ## filtered aren't checked at all). Whether those rows are USABLE
    ## depends on the columns YOUR app consumes -- here, the plotted
    ## x/y/color/size. That is the parent's call, not the module's:
    ## drop them explicitly (as below), or plot them your own way.
    plot_data <- reactive({
        cols <- unique(c(input$x, input$y,
                         setdiff(c(input$color, input$size), "(none)")))
        r <- plot_rows()
        ## with a DB backend these per-interaction reads are free after
        ## first touch: backends cache fetched columns (cache = TRUE
        ## default), so only the first read of a column hits the database
        df <- as.data.frame(lapply(setNames(cols, cols),
                                   function(cn) backend$get_column(cn)[r]))
        keep <- complete.cases(df)
        attr(df, "n_na_dropped") <- sum(!keep)
        df[keep, , drop = FALSE]
    })

    ############################################################
    ## INTERACTION, part 2b (optional): STREAMS -- the leave-one-out
    ## partition.  th$streams(v) hands back row IDs partitioned by what
    ## v's OWN filter did to them, while every other filter still
    ## applies to all streams:
    ##   $selected  passed v's filter too (identical to th$rows())
    ##   $excluded  rejected ONLY by v's filter
    ## (split_range = TRUE further splits a range filter's rejects into
    ##  $below / $above; drop_na = TRUE strips NA-in-v rows.)
    ## Here: when the toggle is on, the points the x column's filter
    ## rejected are drawn as an equally-valid second population in the
    ## module's own sel/unsel plasma pair (magenta), under the
    ## survivors.  Empty until x has an active filter in the Thanos
    ## panel.
    ############################################################
    excluded_data <- reactive({
        if (!isTRUE(input$show_excluded)) return(NULL)
        ex <- th$streams(input$x, drop_na = TRUE)$excluded
        if (length(ex) == 0) return(NULL)
        if (length(ex) > PLOT_CAP) ex <- sort(sample(ex, PLOT_CAP))
        df <- data.frame(x = backend$get_column(input$x)[ex],
                         y = backend$get_column(input$y)[ex])
        df[complete.cases(df), , drop = FALSE]
    })

    ## From here down: 100% ordinary grapher code.  It has no idea
    ## Thanos exists; it just renders whatever plot_data() holds.
    output$scatter <- renderPlot({
        req(input$x, input$y)
        df <- plot_data()
        aes_args <- list(x = as.name(input$x), y = as.name(input$y))
        if (input$color != "(none)") aes_args$colour <- as.name(input$color)
        if (input$size  != "(none)") aes_args$size   <- as.name(input$size)
        p <- ggplot(df, do.call(aes, aes_args))
        ed <- excluded_data()
        ## the two populations wear the module's plasma sel/unsel pair
        sel_col <- "#0D0887"; exc_col <- "#9C179E"
        if (!is.null(ed) && nrow(ed) > 0) {
            ## excluded population first, so survivors draw on top.
            ## Redundant encodings keep the two sets unmistakable even
            ## when the survivors are color-mapped by a variable:
            ## excluded = hollow magenta circles, selected = solid.
            p <- p + geom_point(data = ed, aes(x = x, y = y),
                                colour = exc_col, shape = 1, alpha = 0.4,
                                inherit.aes = FALSE)
        }
        p <- p + if (input$color == "(none)") {
            ## no color mapping: survivors wear the plasma blue so they
            ## pair with their slope line and contrast the magenta
            geom_point(alpha = 0.4, colour = sel_col)
        } else {
            geom_point(alpha = 0.4)
        }
        ## slopes: ONE line per population (not per color group), so the
        ## smooths get their own fixed aes and ignore the point layers'
        if (isTRUE(input$fit_slopes) && nrow(df) >= 3) {
            p <- p + geom_smooth(
                data = df,
                aes(x = .data[[input$x]], y = .data[[input$y]]),
                method = "lm", formula = y ~ x, se = FALSE,
                colour = sel_col, linewidth = 1, inherit.aes = FALSE)
        }
        if (isTRUE(input$fit_slopes) && !is.null(ed) && nrow(ed) >= 3) {
            p <- p + geom_smooth(
                data = ed, aes(x = x, y = y),
                method = "lm", formula = y ~ x, se = FALSE,
                colour = exc_col, linetype = "dashed", linewidth = 1,
                inherit.aes = FALSE)
        }
        if (input$color != "(none)") {
            p <- p + if (backend$get_column_info(input$color)$is_numeric) {
                scale_color_viridis_c(option = "plasma", end = 0.85)
            } else {
                scale_color_viridis_d(option = "plasma", end = 0.85)
            }
        }
        p + theme_minimal(base_size = 14)
    })

    ## INTERACTION, part 3 (optional niceties): cheap summary reactives
    ## for captions, and th$filters() if you want to persist state.
    output$counts <- renderText({
        n_sel <- th$n_selected()
        capped <- if (n_sel > PLOT_CAP) {
            sprintf(" (plotting a %s-row sample)", format(PLOT_CAP, big.mark = ","))
        } else ""
        ## be transparent about rows dropped for NA in the PLOTTED
        ## columns -- these passed the filters; they just can't be drawn
        n_na <- attr(plot_data(), "n_na_dropped") %||% 0
        na_note <- if (n_na > 0) {
            sprintf("; %s not drawn (NA in a plotted column)",
                    format(n_na, big.mark = ","))
        } else ""
        ed <- excluded_data()
        ex_note <- if (!is.null(ed) && nrow(ed) > 0) {
            sprintf("; %s excluded by the %s filter shown as hollow magenta points",
                    format(nrow(ed), big.mark = ","), input$x)
        } else ""
        sprintf("%s of %s rows pass filters%s%s%s",
                format(n_sel, big.mark = ","),
                format(backend$n_rows(), big.mark = ","), capped, na_note,
                ex_note)
    })

    ## Rudimentary slope comparison: y ~ x fit within each population,
    ## then an interaction model y ~ x * population whose x:population
    ## coefficient IS the slope difference (its t test answers "do the
    ## two populations have different slopes?").  Fits use the plotted
    ## (possibly sampled) points.
    output$slopes <- renderText({
        if (!isTRUE(input$fit_slopes)) return("")
        req(input$x, input$y)
        df <- plot_data()
        xs <- df[[input$x]]; ys <- df[[input$y]]
        if (length(xs) < 3) return("too few selected points for a linear fit")
        cs <- summary(lm(ys ~ xs))$coefficients
        line_sel <- sprintf("selected:   slope = %.4g ± %.2g   (n = %s)",
                            cs["xs", 1], cs["xs", 2],
                            format(length(xs), big.mark = ","))
        ed <- excluded_data()
        if (is.null(ed) || nrow(ed) < 3) {
            return(paste0(line_sel, "\n(turn on the excluded-points toggle,",
                          " with an active filter on ", input$x,
                          ", to compare slopes)"))
        }
        ce <- summary(lm(y ~ x, data = ed))$coefficients
        line_exc <- sprintf("excluded:   slope = %.4g ± %.2g   (n = %s)",
                            ce["x", 1], ce["x", 2],
                            format(nrow(ed), big.mark = ","))
        both <- data.frame(
            x = c(xs, ed$x), y = c(ys, ed$y),
            grp = factor(rep(c("selected", "excluded"),
                             c(length(xs), nrow(ed))),
                         levels = c("excluded", "selected")))
        ci <- summary(lm(y ~ x * grp, data = both))$coefficients
        est <- ci["x:grpselected", ]
        pv <- format.pval(est[[4]], digits = 3, eps = 1e-16)
        pv <- if (grepl("^<", pv)) paste("p", pv) else paste("p =", pv)
        verdict <- if (est[[4]] < 0.05) "the slopes differ" else
                   "no significant slope difference"
        line_diff <- sprintf(
            "difference: %.4g (selected − excluded), t = %.3g, %s  →  %s",
            est[[1]], est[[3]], pv, verdict)
        paste(line_sel, line_exc, line_diff, sep = "\n")
    })
}

shinyApp(ui, server)
