################################################################
## SQLite tall/skinny backend implementing the contract in
## thanos_backend.R against the schema built by db/build_flights_sqlite.R:
##   long_data(row_id, column_name, value_num, value_txt)
##   column_registry(column_name, type, n_rows, n_na, min_val, max_val,
##                   is_integerish, levels_json)
##
## The database is used as a COLUMN STORE: the module fetches a column
## once when it is selected (one indexed query) and all filtering then
## happens in R on the cached vector.  Filtering in SQL instead would
## need k leave-one-out round trips per interaction and could not feed
## the histograms, which need full-column vectors.
##
## The connection is held in the closure for the app's lifetime; for a
## multi-session production deployment use a pool instead.
################################################################

backend_sqlite <- function(db_path,
                           table = "long_data",
                           registry = "column_registry") {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("backend_sqlite needs the DBI and RSQLite packages")
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    reg <- DBI::dbGetQuery(con, sprintf(
        "SELECT column_name, type, n_rows, n_na, min_val, max_val,
                is_integerish, levels_json FROM %s", registry))
    n <- reg$n_rows[1]

    infos <- lapply(seq_len(nrow(reg)), function(i) {
        r <- reg[i, ]
        if (r$type == "numeric") {
            list(name = r$column_name, is_numeric = TRUE, n_na = r$n_na,
                 range = c(r$min_val, r$max_val),
                 is_integerish = isTRUE(r$is_integerish == 1))
        } else {
            list(name = r$column_name, is_numeric = FALSE, n_na = r$n_na,
                 levels = as.character(jsonlite::fromJSON(r$levels_json)))
        }
    })
    names(infos) <- reg$column_name

    list(
        get_columns = function() reg$column_name,
        n_rows = function() n,
        get_column = function(name) {
            info <- infos[[name]]
            if (is.null(info)) stop("unknown column: ", name)
            value_col <- if (info$is_numeric) "value_num" else "value_txt"
            res <- DBI::dbGetQuery(con, sprintf(
                "SELECT row_id, %s AS value FROM %s WHERE column_name = ?",
                value_col, table), params = list(name))
            out <- if (info$is_numeric) rep(NA_real_, n) else rep(NA_character_, n)
            out[res$row_id] <- res$value
            out
        },
        get_column_info = function(name) infos[[name]],
        disconnect = function() DBI::dbDisconnect(con)
    )
}
