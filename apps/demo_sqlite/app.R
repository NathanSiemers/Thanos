################################################################
## Thanos demo: the SAME flights app as apps/demo_flights, but served
## from the tall/skinny SQLite database instead of memory.  The only
## meaningful difference is the backend constructor line -- which is
## the point of the backend abstraction.
##
## First build the database:  Rscript db/build_flights_sqlite.R
## Then:                      shiny::runApp("apps/demo_sqlite")
################################################################
library(shiny)
library(ggplot2)

thanos_r_dir <- Filter(function(p) file.exists(file.path(p, "thanos_module.R")),
                       c("R", "../R", "../../R"))[1]
if (is.na(thanos_r_dir)) stop("cannot locate the Thanos R/ directory")
invisible(lapply(list.files(thanos_r_dir, pattern = "[.]R$", full.names = TRUE),
                 source))

db_path <- file.path(dirname(thanos_r_dir), "db", "data", "flights.sqlite")
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
    output$head <- renderTable(head(show_df[th$rows(), ], 12))
}

shinyApp(ui, server)
