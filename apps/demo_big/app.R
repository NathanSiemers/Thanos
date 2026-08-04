################################################################
## Thanos demo: NYC TLC yellow taxi 2023, ~38M rows, aggregate mode.
## The module never fetches a column; every histogram is a SQL GROUP BY
## pushed to the backend, chosen automatically because n_rows exceeds
## the aggregate threshold.
##
## Build a database first (duckdb strongly recommended at this scale):
##   Rscript db/build_big_duckdb.R      # db/data/taxi.duckdb
##   Rscript db/build_big_sqlite.R      # db/data/taxi.sqlite (slower)
## Then:  shiny::runApp("apps/demo_big")
################################################################
library(shiny)
library(ggplot2)

thanos_r_dir <- Filter(function(p) file.exists(file.path(p, "thanos_module.R")),
                       c("R", "../R", "../../R"))[1]
if (is.na(thanos_r_dir)) stop("cannot locate the Thanos R/ directory")
invisible(lapply(list.files(thanos_r_dir, pattern = "[.]R$", full.names = TRUE),
                 source))

data_dir <- file.path(dirname(thanos_r_dir), "db", "data")
duck_path <- file.path(data_dir, "taxi.duckdb")
sqlite_path <- file.path(data_dir, "taxi.sqlite")
backend <- if (file.exists(duck_path) &&
               requireNamespace("duckdb", quietly = TRUE)) {
    message("using DuckDB backend: ", duck_path)
    backend_duckdb(duck_path)
} else if (file.exists(sqlite_path)) {
    message("using SQLite backend (build taxi.duckdb for speed): ", sqlite_path)
    backend_sqlite(sqlite_path)
} else {
    stop("no taxi database - run:  Rscript db/build_big_duckdb.R")
}

ui <- fluidPage(
    titlePanel(sprintf("Thanos: NYC taxi 2023 (%s rows, aggregate mode)",
                       format(backend$n_rows(), big.mark = ","))),
    sidebarLayout(
        sidebarPanel(width = 5,
            thanosUI("thanos")
        ),
        mainPanel(width = 7,
            h4(textOutput("count")),
            p(paste("Histograms are SQL GROUP BY aggregates - no column",
                    "vector for this data ever enters R."))
        )
    )
)

server <- function(input, output, session) {
    th <- thanosServer("thanos", backend,
                       default_selected = c("trip_distance", "fare_amount",
                                            "tip_amount", "payment_type"),
                       debounce_ms = 500)
    output$count <- renderText({
        sprintf("%s of %s rows pass filters",
                format(th$n_selected(), big.mark = ","),
                format(backend$n_rows(), big.mark = ","))
    })
}

shinyApp(ui, server)
