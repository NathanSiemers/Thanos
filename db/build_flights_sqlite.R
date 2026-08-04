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

build_tall_skinny <- function(df, db_path,
                              table = "long_data",
                              registry = "column_registry") {
    ## same column coercions as backend_memory, so the two backends
    ## serve identical data
    supported <- vapply(df, function(x) {
        is.numeric(x) || is.character(x) || is.factor(x) || is.logical(x) ||
            inherits(x, c("POSIXct", "Date"))
    }, NA)
    df <- as.data.frame(df)[supported]
    df[] <- lapply(df, function(x) {
        if (inherits(x, c("POSIXct", "Date"))) as.numeric(x)
        else if (is.factor(x) || is.logical(x)) as.character(x)
        else if (is.integer(x)) as.numeric(x)  # match SQLite REAL storage
        else x
    })
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
             levels_json   TEXT
         )", registry))

    dbExecute(con, "BEGIN")
    for (col in names(df)) {
        x <- df[[col]]
        ok <- !is.na(x)
        if (is.numeric(x)) {
            chunk <- data.frame(row_id = which(ok),
                                column_name = rep(col, sum(ok)),
                                value_num = x[ok],
                                value_txt = rep(NA_character_, sum(ok)))
            vals <- sort(unique(x[ok]))
            reg <- data.frame(
                column_name = col, type = "numeric", n_rows = n,
                n_na = sum(!ok),
                min_val = suppressWarnings(min(x, na.rm = TRUE)),
                max_val = suppressWarnings(max(x, na.rm = TRUE)),
                is_integerish = as.integer(is.integer(x) ||
                    isTRUE(all(x == round(x), na.rm = TRUE))),
                n_unique = length(vals),
                ## values kept when few enough to drive checkboxes
                levels_json = if (length(vals) <= 100 && length(vals) > 0) {
                    as.character(jsonlite::toJSON(vals))
                } else NA_character_)
        } else {
            chunk <- data.frame(row_id = which(ok),
                                column_name = rep(col, sum(ok)),
                                value_num = rep(NA_real_, sum(ok)),
                                value_txt = x[ok])
            levs <- sort(unique(x[ok]))
            reg <- data.frame(
                column_name = col, type = "character", n_rows = n,
                n_na = sum(!ok), min_val = NA_real_, max_val = NA_real_,
                is_integerish = NA_integer_, n_unique = length(levs),
                levels_json = as.character(jsonlite::toJSON(levs)))
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
