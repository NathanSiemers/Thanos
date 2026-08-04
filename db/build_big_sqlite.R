################################################################
## Build the tall/skinny SQLite database for the Phase D large-data
## test: NYC TLC yellow taxi trips, 2023 (~38M rows wide, > 2 GB).
##
## Prereq: parquet files in db/data/tlc/ (yellow_tripdata_2023-MM.parquet),
##   e.g.  curl -O https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2023-01.parquet
##
## Streams month by month with arrow (never holds the year in memory),
## melts each chunk with data.table, appends inside one transaction per
## month, and accumulates registry stats incrementally.
##
## Run from the repo root:
##   Rscript db/build_big_sqlite.R           # all months found
##   Rscript db/build_big_sqlite.R 3         # first 3 months only
################################################################
suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(arrow)
    library(data.table)
})

root <- Filter(function(p) file.exists(file.path(p, "db")), c(".", ".."))[1]
tlc_dir <- file.path(root, "db", "data", "tlc")
db_path <- file.path(root, "db", "data", "taxi.sqlite")

## keep an informative subset of columns; location IDs stay categorical
KEEP <- c(
    tpep_pickup_datetime  = "numeric",   # POSIXct -> epoch seconds
    passenger_count       = "numeric",
    trip_distance         = "numeric",
    RatecodeID            = "character",
    PULocationID          = "character",
    DOLocationID          = "character",
    payment_type          = "character",
    fare_amount           = "numeric",
    tip_amount            = "numeric",
    tolls_amount          = "numeric",
    total_amount          = "numeric",
    airport_fee           = "numeric",
    trip_minutes          = "numeric"    # derived: dropoff - pickup
)

months <- sort(list.files(tlc_dir, pattern = "^yellow_tripdata_.*[.]parquet$",
                          full.names = TRUE))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) months <- head(months, as.integer(args[1]))
if (length(months) == 0) stop("no parquet files in ", tlc_dir)

if (file.exists(db_path)) file.remove(db_path)
con <- dbConnect(RSQLite::SQLite(), db_path)
dbExecute(con, "PRAGMA journal_mode = WAL")
dbExecute(con, "PRAGMA synchronous = OFF")   # bulk build; rebuildable artifact
dbExecute(con,
    "CREATE TABLE long_data (
         row_id      INTEGER NOT NULL,
         column_name TEXT    NOT NULL,
         value_num   REAL,
         value_txt   TEXT
     )")

## incremental registry accumulators
acc <- new.env(parent = emptyenv())
acc$n <- 0L
acc$stats <- list()
bump <- function(col, x, type) {
    s <- acc$stats[[col]] %||% list(type = type, n_na = 0, min = Inf,
                                    max = -Inf, levels = character(0),
                                    vals = numeric(0), too_many = FALSE)
    s$n_na <- s$n_na + sum(is.na(x))
    if (type == "numeric") {
        if (any(!is.na(x))) {
            s$min <- min(s$min, min(x, na.rm = TRUE))
            s$max <- max(s$max, max(x, na.rm = TRUE))
        }
        ## track distinct values only until there are too many for a
        ## checkbox widget to make sense
        if (!s$too_many) {
            s$vals <- union(s$vals, unique(x[!is.na(x)]))
            if (length(s$vals) > 100) s$too_many <- TRUE
        }
        ## keep a bounded random sample per chunk for quantile estimates
        ## (exact quantiles would need the whole 38M-value column)
        good <- x[!is.na(x)]
        if (length(good) > 20000) good <- sample(good, 20000)
        s$samp <- c(s$samp %||% numeric(0), good)
    } else {
        s$levels <- union(s$levels, unique(x[!is.na(x)]))
    }
    acc$stats[[col]] <- s
}

t_all <- system.time(for (f in months) {
    t_m <- system.time({
        dt <- as.data.table(read_parquet(f))
        ## some 2023 months capitalize this column ('Airport_fee')
        setnames(dt, "Airport_fee", "airport_fee", skip_absent = TRUE)
        dt[, trip_minutes := as.numeric(
              difftime(tpep_dropoff_datetime, tpep_pickup_datetime,
                       units = "mins"))]
        offset <- acc$n
        n_m <- nrow(dt)
        dbExecute(con, "BEGIN")
        for (col in names(KEEP)) {
            type <- KEEP[[col]]
            x <- dt[[col]]
            if (inherits(x, c("POSIXct", "Date"))) x <- as.numeric(x)
            x <- if (type == "numeric") as.numeric(x) else as.character(x)
            bump(col, x, type)
            ok <- which(!is.na(x))
            if (length(ok) == 0) next
            chunk <- if (type == "numeric") {
                data.table(row_id = offset + ok, column_name = col,
                           value_num = x[ok], value_txt = NA_character_)
            } else {
                data.table(row_id = offset + ok, column_name = col,
                           value_num = NA_real_, value_txt = x[ok])
            }
            dbWriteTable(con, "long_data", chunk, append = TRUE)
        }
        dbExecute(con, "COMMIT")
        acc$n <- acc$n + n_m
    })
    cat(sprintf("%s: %s rows (%.0f s)\n", basename(f),
                format(acc$n, big.mark = ","), t_m[["elapsed"]]))
})

cat("building registry...\n")
dbExecute(con,
    "CREATE TABLE column_registry (
         column_name   TEXT PRIMARY KEY,
         type          TEXT NOT NULL,
         n_rows        INTEGER NOT NULL,
         n_na          INTEGER NOT NULL,
         min_val       REAL,
         max_val       REAL,
         is_integerish INTEGER,
         n_unique      INTEGER,
         q_low         REAL,
         q_high        REAL,
         levels_json   TEXT
     )")
for (col in names(acc$stats)) {
    s <- acc$stats[[col]]
    reg <- if (s$type == "numeric") {
        q <- if (length(s$samp %||% numeric(0)) > 0) {
            unname(quantile(s$samp, c(0.001, 0.999)))
        } else c(NA_real_, NA_real_)
        data.frame(column_name = col, type = "numeric", n_rows = acc$n,
                   n_na = s$n_na, min_val = s$min, max_val = s$max,
                   is_integerish = NA_integer_,
                   n_unique = if (s$too_many) NA_integer_ else length(s$vals),
                   q_low = q[1], q_high = q[2],
                   levels_json = if (!s$too_many && length(s$vals) > 0) {
                       as.character(jsonlite::toJSON(sort(s$vals)))
                   } else NA_character_)
    } else {
        data.frame(column_name = col, type = "character", n_rows = acc$n,
                   n_na = s$n_na, min_val = NA_real_, max_val = NA_real_,
                   is_integerish = NA_integer_, n_unique = length(s$levels),
                   q_low = NA_real_, q_high = NA_real_,
                   levels_json = as.character(jsonlite::toJSON(sort(s$levels))))
    }
    dbWriteTable(con, "column_registry", reg, append = TRUE)
}

cat("indexing (column_name, row_id)... (this is the slow part)\n")
t_idx <- system.time(
    invisible(dbExecute(con,
        "CREATE INDEX idx_long_col ON long_data (column_name, row_id)")))
invisible(dbExecute(con, "ANALYZE"))
total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM long_data")$n
dbDisconnect(con)
cat(sprintf("done: %s wide rows -> %s long rows in %.0f s (+%.0f s index)\n%s (%.1f GB)\n",
            format(acc$n, big.mark = ","), format(total, big.mark = ","),
            t_all[["elapsed"]], t_idx[["elapsed"]], db_path,
            file.size(db_path) / 1e9))
