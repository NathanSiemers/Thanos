################################################################
## Phase D benchmarks: per-interaction aggregate queries on the ~38M-row
## taxi data, SQLite vs DuckDB over the identical tall/skinny schema.
## Prereq: db/data/taxi.duckdb and/or db/data/taxi.sqlite
## Run:    Rscript bench/bench_big.R
################################################################
suppressPackageStartupMessages(library(ggplot2))
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_backend.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "[.]R$",
                            full.names = TRUE), source))

timeit <- function(label, expr, reps = 3) {
    expr <- substitute(expr)
    eval(expr, parent.frame())  # warm-up
    t <- system.time(for (i in seq_len(reps)) eval(expr, parent.frame()))
    cat(sprintf("%-52s %9.0f ms\n", label, 1000 * t[["elapsed"]] / reps))
}

## a realistic interaction state: two active filters
FILTERS <- list(
    fare_amount  = list(is_numeric = TRUE, val = c(5, 60), include_na = TRUE),
    payment_type = list(is_numeric = FALSE, val = c("1", "2"),
                        include_na = TRUE)
)

bench_backend <- function(be, engine) {
    n <- be$n_rows()
    cat(sprintf("\n== %s: %s rows ==\n", engine, format(n, big.mark = ",")))
    spec <- bin_spec_from_info(be$get_column_info("trip_distance"), bins = 50)
    loo <- FILTERS  # trip_distance not filtered, so loo = all filters

    timeit("get_binned numeric, 2 filters active",
           be$get_binned("trip_distance", spec, loo))
    spec_cat <- bin_spec_from_info(be$get_column_info("payment_type"))
    timeit("get_binned categorical, 1 loo filter",
           be$get_binned("payment_type", spec_cat, FILTERS["fare_amount"]))
    timeit("get_count, 2 filters", be$get_count(FILTERS))
    timeit("get_count, no filters", be$get_count(list()))
    cat("   (one full interaction ~= 2x get_binned + 2x get_count per plot)\n")
    timeit("get_column('payment_type') full fetch",
           be$get_column("payment_type"), reps = 1)
    timeit("get_row_mask, 2 filters", be$get_row_mask(FILTERS), reps = 1)
}

duck_path   <- file.path(root, "db", "data", "taxi.duckdb")
sqlite_path <- file.path(root, "db", "data", "taxi.sqlite")

if (file.exists(duck_path) && requireNamespace("duckdb", quietly = TRUE)) {
    be <- backend_duckdb(duck_path)
    bench_backend(be, "DuckDB")
    be$disconnect()
}
if (file.exists(sqlite_path)) {
    be <- backend_sqlite(sqlite_path)
    bench_backend(be, "SQLite")
    be$disconnect()
}
