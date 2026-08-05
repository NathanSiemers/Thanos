################################################################
## Build the tall/skinny SQLite version of nycflights13::flights.
##
##   long_data(row_id, column_name, value_num, value_txt)
##     one row per non-NA cell; exactly one of value_num/value_txt is
##     set.  NA cells are simply absent (NA = absence, reconstructed on
##     fetch), the standard tall/skinny convention.
##   column_registry(column_name, type, n_rows, n_na, min_val, max_val,
##     is_integerish, levels_json)
##     precomputed per-column metadata so filter widgets can be built
##     without ever scanning the long table.
##
## Run from the repo root:  Rscript db/build_flights_sqlite.R
## Writes db/data/flights.sqlite (gitignored).
################################################################
suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
    library(nycflights13)
})
## coercion + column statistics shared with backend_memory, so the two
## data paths agree by construction
.thanos_root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_columns.R")),
                       c(".", "..", "../.."))[1]
source(file.path(.thanos_root, "R", "thanos_columns.R"))

build_tall_skinny <- function(df, db_path,
                              table = "long_data",
                              registry = "column_registry") {
    df <- thanos_coerce_columns(df)
    n <- nrow(df)

    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(db_path)) file.remove(db_path)
    con <- dbConnect(RSQLite::SQLite(), db_path)
    on.exit(dbDisconnect(con))
    dbExecute(con, "PRAGMA journal_mode = WAL")

    dbExecute(con, sprintf(
        "CREATE TABLE %s (
             row_id      INTEGER NOT NULL,
             column_name TEXT    NOT NULL,
             value_num   REAL,
             value_txt   TEXT
         )", table))
    dbExecute(con, sprintf(
        "CREATE TABLE %s (
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
         )", registry))

    dbExecute(con, "BEGIN")
    for (col in names(df)) {
        x <- df[[col]]
        ok <- !is.na(x)
        s <- thanos_column_stats(x)
        if (s$is_numeric) {
            chunk <- data.frame(row_id = which(ok),
                                column_name = rep(col, sum(ok)),
                                value_num = x[ok],
                                value_txt = rep(NA_character_, sum(ok)))
            reg <- data.frame(
                column_name = col, type = "numeric", n_rows = n,
                n_na = s$n_na,
                min_val = s$range[1], max_val = s$range[2],
                is_integerish = as.integer(s$is_integerish),
                n_unique = s$n_unique,
                q_low = s$q_low, q_high = s$q_high,
                ## values kept when few enough to drive checkboxes
                levels_json = if (!is.null(s$values) && length(s$values) > 0) {
                    as.character(jsonlite::toJSON(s$values))
                } else NA_character_)
        } else {
            chunk <- data.frame(row_id = which(ok),
                                column_name = rep(col, sum(ok)),
                                value_num = rep(NA_real_, sum(ok)),
                                value_txt = x[ok])
            reg <- data.frame(
                column_name = col, type = "character", n_rows = n,
                n_na = s$n_na, min_val = NA_real_, max_val = NA_real_,
                is_integerish = NA_integer_, n_unique = length(s$levels),
                q_low = NA_real_, q_high = NA_real_,
                levels_json = as.character(jsonlite::toJSON(s$levels)))
        }
        dbWriteTable(con, table, chunk, append = TRUE)
        dbWriteTable(con, registry, reg, append = TRUE)
        cat(sprintf("  %-12s %s cells\n", col, format(sum(ok), big.mark = ",")))
    }
    dbExecute(con, "COMMIT")

    cat("indexing (column_name, row_id)...\n")
    dbExecute(con, sprintf(
        "CREATE INDEX idx_%s_col ON %s (column_name, row_id)", table, table))
    dbExecute(con, "ANALYZE")

    total <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", table))$n
    cat(sprintf("done: %s long rows, %s\n", format(total, big.mark = ","),
                db_path))
    invisible(db_path)
}

if (sys.nframe() == 0) {
    root <- Filter(function(p) file.exists(file.path(p, "db")), c(".", ".."))[1]
    build_tall_skinny(nycflights13::flights,
                      file.path(root, "db", "data", "flights.sqlite"))
}
