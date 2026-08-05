################################################################
## Thanos demo: NYC TLC yellow taxi 2023, ~38M rows, aggregate mode.
## The module never fetches a column; every histogram is a SQL GROUP BY
## pushed to the backend, chosen automatically because n_rows exceeds
## the aggregate threshold.
##
## One-command setup (downloads taxi parquet months, builds the db):
##   Rscript db/setup_demos.R big
## Then:  shiny::runApp("apps/demo_big")
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

## the demo databases live in the repo checkout: locate the repo root
## by probing for its db/ directory
thanos_db_dir <- Filter(dir.exists, file.path(c(".", "..", "../.."), "db"))[1]
if (is.na(thanos_db_dir)) stop("cannot locate the repo's db/ directory")
data_dir <- file.path(thanos_db_dir, "data")
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
