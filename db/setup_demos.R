#!/usr/bin/env Rscript
################################################################
## One-command demo setup for a fresh checkout.
##
##   Rscript db/setup_demos.R            install demo deps + build
##                                       db/data/flights.sqlite (~1 min)
##   Rscript db/setup_demos.R big        additionally download NYC taxi
##                                       parquet months and build
##                                       db/data/taxi.duckdb for demo_big
##   Rscript db/setup_demos.R big 12     ...with 12 months (default 3;
##                                       ~50 MB download per month)
##
## Idempotent: existing downloads and databases are skipped -- delete
## files under db/data/ to force a rebuild.  After it finishes:
##
##   shiny::runApp("apps/demo_storms")    no database needed
##   shiny::runApp("apps/demo_flights")   no database needed
##   shiny::runApp("apps/grapher")        no database needed
##   shiny::runApp("apps/demo_sqlite")    uses flights.sqlite
##   shiny::runApp("apps/demo_big")       uses taxi.duckdb ('big' setup)
################################################################
args <- commandArgs(trailingOnly = TRUE)
big <- length(args) >= 1 && args[1] == "big"
months <- if (big && length(args) >= 2) max(1L, as.integer(args[2])) else 3L

root <- Filter(function(p) dir.exists(file.path(p, "db")) &&
                           file.exists(file.path(p, "thanos.R")),
               c(".", ".."))[1]
if (is.na(root)) stop("run from the repository root:  Rscript db/setup_demos.R")

## ---- 1. demo dependencies --------------------------------------
repos <- getOption("repos")
if (!is.character(repos) || is.na(repos[1]) || repos[1] == "@CRAN@") {
    repos <- c(CRAN = "https://cloud.r-project.org")
}
need <- c("shiny", "ggplot2", "jsonlite", "viridisLite",   # thanos core
          "DBI", "RSQLite", "nycflights13", "dplyr")       # demo apps
if (big) need <- c(need, "duckdb", "arrow", "data.table")  # demo_big build
missing <- need[!vapply(need, requireNamespace, NA, quietly = TRUE)]
if (length(missing)) {
    message("installing missing demo dependencies: ",
            paste(missing, collapse = ", "))
    install.packages(missing, repos = repos)
} else {
    message("all demo dependencies present")
}

## ---- 2. flights.sqlite (demo_sqlite) ---------------------------
fl_db <- file.path(root, "db", "data", "flights.sqlite")
if (file.exists(fl_db)) {
    message("db/data/flights.sqlite exists -- skipping (delete to rebuild)")
} else {
    message("building db/data/flights.sqlite ...")
    source(file.path(root, "db", "build_flights_sqlite.R"))
    build_tall_skinny(nycflights13::flights, fl_db)
}

## ---- 3. taxi data + taxi.duckdb (demo_big, optional) -----------
if (big) {
    tlc <- file.path(root, "db", "data", "tlc")
    dir.create(tlc, recursive = TRUE, showWarnings = FALSE)
    for (m in sprintf("%02d", seq_len(months))) {
        f <- file.path(tlc, sprintf("yellow_tripdata_2023-%s.parquet", m))
        if (file.exists(f)) {
            message(basename(f), " exists -- skipping")
            next
        }
        message("downloading ", basename(f), " (~50 MB) ...")
        download.file(sprintf(
            "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2023-%s.parquet",
            m), f, mode = "wb", quiet = TRUE)
    }
    duck <- file.path(root, "db", "data", "taxi.duckdb")
    if (file.exists(duck)) {
        message("db/data/taxi.duckdb exists -- skipping (delete to rebuild)")
    } else {
        message("building db/data/taxi.duckdb from ", months, " month(s) ...")
        status <- system2("Rscript", file.path(root, "db", "build_big_duckdb.R"))
        if (status != 0) stop("taxi.duckdb build failed")
    }
}

message("\ndemo setup complete. Run the apps with e.g.:")
message('  R -e \'shiny::runApp("apps/demo_flights")\'')
if (big) message('  R -e \'shiny::runApp("apps/demo_big")\'')
