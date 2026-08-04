################################################################
## GRAPHER: an independent Shiny app with Thanos plugged in.
##
## This app is the INTEGRATION REFERENCE.  It is a self-standing
## scatter-plot application (pick x, y, color, size, draw points) that
## acquired filtering by adding Thanos at exactly FOUR points, each
## marked with a "PLUG-IN POINT" banner below:
##
##   1. source the Thanos R/ files            (once, at startup)
##   2. create a backend around your data     (once, at startup)
##   3. thanosUI("<id>")   somewhere in ui    (one line)
##   4. thanosServer("<id>", backend, ...) in server(), keeping its
##      return value                          (one line)
##
## HOW THE TWO APPS INTERACT after that -- the whole contract:
##
##   parent app  ------ nothing ------>  Thanos
##   parent app  <---- reactives ----- Thanos
##
##   Thanos never calls into your app, never modifies your data, and
##   never hands you a filtered copy of anything.  It returns a list of
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
## PLUG-IN POINT 1 of 4: source the Thanos loader -- ONE file.
## An established app copies the R/ directory (or adds this repo as a
## submodule) and sources R/thanos.R.  That defines exactly six
## functions here (thanosUI, thanosServer, backend_memory, backend_dbi,
## backend_sqlite, backend_duckdb) plus `thanos`, a handle to the
## private namespace.  All internals stay inside that namespace, so
## nothing Thanos uses can collide with functions your app defines,
## and your same-named functions cannot break the module.
################################################################
thanos_r_dir <- Filter(function(p) file.exists(file.path(p, "thanos.R")),
                       c("R", "../R", "../../R"))[1]
if (is.na(thanos_r_dir)) stop("cannot locate the Thanos R/ directory")
source(file.path(thanos_r_dir, "thanos.R"))

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
            plotOutput("scatter", height = "500px"),
            textOutput("counts")
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
    plot_data <- reactive({
        cols <- unique(c(input$x, input$y,
                         setdiff(c(input$color, input$size), "(none)")))
        r <- plot_rows()
        as.data.frame(lapply(setNames(cols, cols),
                             function(cn) backend$get_column(cn)[r]))
    })

    ## From here down: 100% ordinary grapher code.  It has no idea
    ## Thanos exists; it just renders whatever plot_data() holds.
    output$scatter <- renderPlot({
        req(input$x, input$y)
        df <- plot_data()
        aes_args <- list(x = as.name(input$x), y = as.name(input$y))
        if (input$color != "(none)") aes_args$colour <- as.name(input$color)
        if (input$size  != "(none)") aes_args$size   <- as.name(input$size)
        p <- ggplot(df, do.call(aes, aes_args)) +
            geom_point(alpha = 0.4)
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
        sprintf("%s of %s rows pass filters%s",
                format(n_sel, big.mark = ","),
                format(backend$n_rows(), big.mark = ","), capped)
    })
}

shinyApp(ui, server)
