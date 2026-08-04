################################################################
## Build the SAME tall/skinny schema as build_big_sqlite.R, but in a
## DuckDB database, entirely in SQL straight from the parquet files --
## no R-side melting, DuckDB's parallel reader does the work.
##
## Prereq: parquet files in db/data/tlc/
## Run from the repo root:  Rscript db/build_big_duckdb.R
## Writes db/data/taxi.duckdb.
##
## Note: row_ids here are NOT guaranteed to match taxi.sqlite's
## (DuckDB's parallel parquet scan interleaves months); each database
## is internally consistent, which is all the module needs.
################################################################
suppressPackageStartupMessages({
    library(DBI)
    library(duckdb)
})

root <- Filter(function(p) file.exists(file.path(p, "db")), c(".", ".."))[1]
tlc_glob <- file.path(root, "db", "data", "tlc", "yellow_tripdata_*.parquet")
db_path  <- file.path(root, "db", "data", "taxi.duckdb")

NUM_COLS <- c("tpep_pickup_datetime", "passenger_count", "trip_distance",
              "fare_amount", "tip_amount", "tolls_amount", "total_amount",
              "airport_fee", "trip_minutes")
TXT_COLS <- c("RatecodeID", "PULocationID", "DOLocationID", "payment_type")

if (file.exists(db_path)) file.remove(db_path)
con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
run <- function(sql) invisible(dbExecute(con, sql))

cat("materializing wide table with row_ids from parquet...\n")
t1 <- system.time(run(sprintf(
    "CREATE TABLE wide AS
     SELECT row_number() OVER () AS row_id,
            epoch(tpep_pickup_datetime) AS tpep_pickup_datetime,
            CAST(passenger_count AS DOUBLE) AS passenger_count,
            trip_distance,
            CAST(RatecodeID AS VARCHAR)   AS RatecodeID,
            CAST(PULocationID AS VARCHAR) AS PULocationID,
            CAST(DOLocationID AS VARCHAR) AS DOLocationID,
            CAST(payment_type AS VARCHAR) AS payment_type,
            fare_amount, tip_amount, tolls_amount, total_amount,
            CAST(airport_fee AS DOUBLE) AS airport_fee,
            (epoch(tpep_dropoff_datetime) - epoch(tpep_pickup_datetime)) / 60.0
                AS trip_minutes
     FROM read_parquet('%s')", tlc_glob)))
n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM wide")$n
cat(sprintf("  %s wide rows (%.0f s)\n", format(n, big.mark = ","),
            t1[["elapsed"]]))

cat("melting to long_data in SQL...\n")
selects <- c(
    vapply(NUM_COLS, function(col) sprintf(
        "SELECT row_id, '%s' AS column_name, %s AS value_num,
                CAST(NULL AS VARCHAR) AS value_txt
         FROM wide WHERE %s IS NOT NULL", col, col, col), character(1)),
    vapply(TXT_COLS, function(col) sprintf(
        "SELECT row_id, '%s' AS column_name, CAST(NULL AS DOUBLE) AS value_num,
                %s AS value_txt
         FROM wide WHERE %s IS NOT NULL", col, col, col), character(1))
)
t2 <- system.time(run(paste("CREATE TABLE long_data AS",
                            paste(selects, collapse = " UNION ALL "))))
total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM long_data")$n
cat(sprintf("  %s long rows (%.0f s)\n", format(total, big.mark = ","),
            t2[["elapsed"]]))

cat("registry, row_universe, index...\n")
run("CREATE TABLE column_registry (
        column_name TEXT PRIMARY KEY, type TEXT NOT NULL,
        n_rows INTEGER NOT NULL, n_na INTEGER NOT NULL,
        min_val REAL, max_val REAL, is_integerish INTEGER,
        levels_json TEXT)")
for (col in NUM_COLS) {
    run(sprintf(
        "INSERT INTO column_registry
         SELECT '%s', 'numeric', %d, %d - COUNT(*),
                MIN(value_num), MAX(value_num), NULL, NULL
         FROM long_data WHERE column_name = '%s'", col, n, n, col))
}
for (col in TXT_COLS) {
    ## levels_json built in R: duckdb's to_json needs the json extension,
    ## which may not be installable offline
    levs <- dbGetQuery(con, sprintf(
        "SELECT DISTINCT value_txt AS v FROM long_data
         WHERE column_name = '%s' ORDER BY v", col))$v
    n_present <- dbGetQuery(con, sprintf(
        "SELECT COUNT(*) AS n FROM long_data WHERE column_name = '%s'",
        col))$n
    dbWriteTable(con, "column_registry", append = TRUE,
        data.frame(column_name = col, type = "character", n_rows = n,
                   n_na = n - n_present, min_val = NA_real_,
                   max_val = NA_real_, is_integerish = NA_integer_,
                   levels_json = as.character(jsonlite::toJSON(levs))))
}
run(sprintf("CREATE TABLE row_universe AS
             SELECT CAST(range AS INTEGER) AS row_id FROM range(1, %d)", n + 1))
t3 <- system.time(
    run("CREATE INDEX idx_long_col ON long_data (column_name, row_id)"))

dbDisconnect(con)
cat(sprintf("done in %.0f s build + %.0f s index: %s (%.1f GB)\n",
            t1[["elapsed"]] + t2[["elapsed"]], t3[["elapsed"]],
            db_path, file.size(db_path) / 1e9))
