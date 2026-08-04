################################################################
## Memory vs tall/skinny SQLite backend on flights.
## Prereq:  Rscript db/build_flights_sqlite.R
## Run:     Rscript bench/bench_backends.R
################################################################
suppressPackageStartupMessages({
    library(nycflights13)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_backend.R")),
               c(".", ".."))[1]
source(file.path(root, "R", "thanos_backend.R"))
source(file.path(root, "R", "thanos_backend_sqlite.R"))
source(file.path(root, "R", "thanos_plot.R"))
suppressPackageStartupMessages(library(ggplot2))
source(file.path(root, "R", "thanos_theme.R"))

db_path <- file.path(root, "db", "data", "flights.sqlite")
if (!file.exists(db_path)) stop("run:  Rscript db/build_flights_sqlite.R")

timeit <- function(label, expr, reps = 5) {
    expr <- substitute(expr)
    t <- system.time(for (i in seq_len(reps)) eval(expr, parent.frame()))
    ms <- 1000 * t[["elapsed"]] / reps
    cat(sprintf("%-58s %9.2f ms\n", label, ms))
    invisible(ms)
}

mem <- backend_memory(as.data.frame(nycflights13::flights))
cat("opening sqlite backend (reads registry once)...\n")
t0 <- system.time(sql <- backend_sqlite(db_path))
cat(sprintf("%-58s %9.2f ms\n", "backend_sqlite() open + registry", 1000 * t0[["elapsed"]]))
cat("\n")

## widget-building metadata: registry lookup vs in-memory scan
timeit("get_column_info x19 cols, memory (first, computes)",
       { m <- backend_memory(as.data.frame(nycflights13::flights))
         for (col in m$get_columns()) m$get_column_info(col) }, reps = 3)
timeit("get_column_info x19 cols, sqlite (registry, cached)",
       for (col in sql$get_columns()) sql$get_column_info(col))

cat("\n## full-column fetch (the only per-selection cost)\n")
for (col in c("dep_delay", "distance", "carrier", "dest")) {
    timeit(sprintf("get_column('%s'), memory", col), mem$get_column(col))
    timeit(sprintf("get_column('%s'), sqlite", col), sql$get_column(col))
}

cat("\n## everything AFTER the fetch is backend-independent:\n")
x_mem <- mem$get_column("dep_delay")
x_sql <- sql$get_column("dep_delay")
stopifnot(identical(x_mem, x_sql))
timeit("make_mask on the fetched column (either backend)",
       make_mask(x_sql, c(-10, 60)), reps = 20)
cat("\nconclusion: backends differ ONLY in one-time column fetch latency;\n")
cat("per-interaction filtering/plotting cost is identical by construction.\n")

sql$disconnect()
