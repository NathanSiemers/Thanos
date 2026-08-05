################################################################
## Storage-layout benchmark: the SAME Thanos-style queries against
## three physical layouts of the same 38M taxi rows, all in DuckDB, so
## the only variable is the layout:
##
##   melt     long_data(row_id, column_name, value_num, value_txt) --
##            what backend_dbi runs today.  Filters are row_id
##            semi-joins against the same long table; column fetches
##            ship (row_id, value) pairs and reassemble in R.
##   wide     one real column per variable (the 'wide' table the duckdb
##            build already materializes).  Filters are plain WHERE
##            clauses; fetches ship just the ordered column.
##   parquet  the same expressions over read_parquet() directly -- no
##            database build at all.  (No stable row_id, so the
##            rows()/mask() pointer would need materializing; counts
##            and histograms are exact.)
##
## Prereq: db/data/taxi.duckdb (build_big_duckdb.R) + db/data/tlc/*.parquet
## Run:    Rscript bench/bench_layouts.R
################################################################
suppressPackageStartupMessages({
    library(DBI)
    library(duckdb)
    library(ggplot2)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_backend.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
                            full.names = TRUE), source))

db_path <- file.path(root, "db", "data", "taxi.duckdb")
tlc_glob <- file.path(root, "db", "data", "tlc", "yellow_tripdata_*.parquet")
if (!file.exists(db_path)) stop("run:  Rscript db/build_big_duckdb.R")

timeit <- function(label, expr, reps = 3) {
    expr <- substitute(expr)
    eval(expr, parent.frame())  # warm-up
    t <- system.time(for (i in seq_len(reps)) eval(expr, parent.frame()))
    cat(sprintf("  %-46s %9.0f ms\n", label, 1000 * t[["elapsed"]] / reps))
}

con <- dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
melt <- backend_dbi(con)   # the production melt-backed backend

## identical interaction state to bench_big.R: two active filters,
## include-NA on for both
FILTERS <- list(
    fare_amount  = list(is_numeric = TRUE, val = c(5, 60), include_na = TRUE),
    payment_type = list(is_numeric = FALSE, val = c("1", "2"),
                        include_na = TRUE)
)
## bin geometry from the shared registry (outlier-robust range)
spec <- bin_spec_from_info(melt$get_column_info("trip_distance"), bins = 50)

## wide/parquet WHERE translation of the same filters (include_na TRUE:
## NULL passes, mirroring make_mask / the melt backend's NOT IN form)
WIDE_WHERE <- paste(
    "(fare_amount IS NULL OR fare_amount BETWEEN 5 AND 60)",
    "AND (payment_type IS NULL OR payment_type IN ('1', '2'))")
bin_expr <- sprintf(
    "CASE WHEN CAST(floor((trip_distance - %.17g) / %.17g) AS INTEGER) + 1 > %d
          THEN %d
          WHEN CAST(floor((trip_distance - %.17g) / %.17g) AS INTEGER) + 1 < 1
          THEN 1
          ELSE CAST(floor((trip_distance - %.17g) / %.17g) AS INTEGER) + 1 END",
    spec$origin, spec$binwidth, spec$nbins, spec$nbins,
    spec$origin, spec$binwidth, spec$origin, spec$binwidth)

## parquet-direct: a view with the same derived columns the wide build
## materialized, straight off the files
dbExecute(con, sprintf(
    "CREATE OR REPLACE TEMP VIEW pq AS
     SELECT trip_distance, fare_amount,
            CAST(payment_type AS VARCHAR) AS payment_type
     FROM read_parquet('%s')", tlc_glob))

run_layout <- function(label, from, has_rowid) {
    cat(sprintf("\n== %s ==\n", label))
    timeit("get_binned numeric, 2 filters active",
           dbGetQuery(con, sprintf(
               "SELECT %s AS bin, COUNT(*) AS n FROM %s
                WHERE trip_distance IS NOT NULL AND %s GROUP BY bin",
               bin_expr, from, WIDE_WHERE)))
    timeit("get_binned categorical, 1 loo filter",
           dbGetQuery(con, sprintf(
               "SELECT payment_type, COUNT(*) AS n FROM %s
                WHERE (fare_amount IS NULL OR fare_amount BETWEEN 5 AND 60)
                GROUP BY payment_type", from)))
    timeit("get_count, 2 filters",
           dbGetQuery(con, sprintf(
               "SELECT COUNT(*) AS n FROM %s WHERE %s", from, WIDE_WHERE)))
    timeit("get_column full fetch (payment_type)",
           dbGetQuery(con, sprintf(
               "SELECT payment_type FROM %s%s", from,
               if (has_rowid) " ORDER BY row_id" else "")), reps = 1)
    if (has_rowid) {
        timeit("get_row_mask, 2 filters",
               dbGetQuery(con, sprintf(
                   "SELECT row_id FROM %s WHERE %s", from, WIDE_WHERE)),
               reps = 1)
    } else {
        cat("  get_row_mask                                   n/a (no stable row_id)\n")
    }
}

cat(sprintf("taxi rows: %s\n", format(melt$n_rows(), big.mark = ",")))

## -- melt: the production backend functions themselves --
cat("\n== melt (long_data via backend_dbi -- production path) ==\n")
loo <- FILTERS
timeit("get_binned numeric, 2 filters active",
       melt$get_binned("trip_distance", spec, loo))
spec_cat <- bin_spec_from_info(melt$get_column_info("payment_type"))
timeit("get_binned categorical, 1 loo filter",
       melt$get_binned("payment_type", spec_cat, FILTERS["fare_amount"]))
timeit("get_count, 2 filters", melt$get_count(FILTERS))
timeit("get_column full fetch (payment_type)",
       melt$get_column("payment_type"), reps = 1)
timeit("get_row_mask, 2 filters", melt$get_row_mask(FILTERS), reps = 1)

run_layout("wide (materialized columns, same duckdb file)", "wide", TRUE)
run_layout("parquet-direct (read_parquet view, no build)", "pq", FALSE)

dbDisconnect(con)
