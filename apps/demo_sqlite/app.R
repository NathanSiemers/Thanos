################################################################
## Thanos demo: the SAME flights app as apps/demo_flights, but served
## from the tall/skinny SQLite database instead of memory.  The only
## meaningful difference is the backend constructor line -- which is
## the point of the backend abstraction.
##
## First build the database:  Rscript db/setup_demos.R
## Then:                      shiny::runApp("apps/demo_sqlite")
################################################################
library(shiny)
library(ggplot2)

## load Thanos: prefer the installed package; fall back to sourcing the
## repo-root loader (publics land here, internals stay private either way)
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

## the demo database lives in the repo checkout: locate the repo root
## by probing for its db/ directory
thanos_db_dir <- Filter(dir.exists, file.path(c(".", "..", "../.."), "db"))[1]
if (is.na(thanos_db_dir)) stop("cannot locate the repo's db/ directory")
db_path <- file.path(thanos_db_dir, "data", "flights.sqlite")
if (!file.exists(db_path)) {
    stop("no database at ", db_path,
         " - run:  Rscript db/build_flights_sqlite.R")
}
backend <- backend_sqlite(db_path)

ui <- fluidPage(
    titlePanel("Thanos: flights (tall/skinny SQLite backend)"),
    sidebarLayout(
        sidebarPanel(width = 5,
            thanosUI("thanos")
        ),
        mainPanel(width = 7,
            h4(textOutput("count")),
            tableOutput("head")
        )
    )
)

server <- function(input, output, session) {
    th <- thanosServer("thanos", backend,
                       default_selected = c("carrier", "origin", "dest",
                                            "dep_delay", "arr_delay",
                                            "distance", "month", "hour"))
    output$count <- renderText({
        sprintf("%s of %s rows pass filters",
                format(th$n_selected(), big.mark = ","),
                format(backend$n_rows(), big.mark = ","))
    })
    ## the parent app subsets its own copy of whatever it displays;
    ## here we just fetch a few columns through the same backend
    show_cols <- c("month", "day", "carrier", "origin", "dest",
                   "dep_delay", "arr_delay", "distance")
    show_df <- as.data.frame(lapply(setNames(show_cols, show_cols),
                                    backend$get_column))
    output$head <- renderTable(show_df[head(th$rows(), 12), ])
}

shinyApp(ui, server)
