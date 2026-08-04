################################################################
## DBI backends over the tall/skinny schema
##   long_data(row_id, column_name, value_num, value_txt)
##   column_registry(column_name, type, n_rows, n_na, min_val, max_val,
##                   is_integerish, levels_json)
##
## backend_dbi(con) implements the contract in thanos_backend.R for any
## DBI connection holding that schema; backend_sqlite() / backend_duckdb()
## are thin wrappers.  Two modes of use:
##
## 1. COLUMN STORE (flights scale): the module fetches whole columns
##    once per selection and filters in R.  One indexed query each.
##
## 2. AGGREGATE (Phase D scale, tens of millions of rows): fetching
##    whole columns is too heavy, so these backends also expose
##      $supports_binned          TRUE
##      $get_binned(name, spec, filters)  histogram counts via SQL GROUP BY
##      $get_count(filters)               rows passing a filter set
##      $get_row_mask(filters)            logical(n) for the parent app
##    'filters' is a named list: filters[[var]] = list(is_numeric, val,
##    include_na), exactly the module's filter state.
##
## NA semantics in SQL: NA cells are ABSENT from long_data, so
##   include_na = TRUE   row passes unless it appears with a failing value
##                       -> row_id NOT IN (rows failing the predicate)
##   include_na = FALSE  row must appear with a passing value
##                       -> row_id IN (rows passing the predicate)
## Counting "all rows" needs a row_universe(row_id) helper table; build
## scripts create it, and backend_dbi() creates it lazily if missing.
################################################################

backend_dbi <- function(con,
                        table = "long_data",
                        registry = "column_registry") {
    ## n_unique + numeric levels_json are newer registry columns; older
    ## databases still work (discrete-numeric detection just stays off)
    reg <- tryCatch(
        DBI::dbGetQuery(con, sprintf(
            "SELECT column_name, type, n_rows, n_na, min_val, max_val,
                    is_integerish, n_unique, levels_json FROM %s", registry)),
        error = function(e) {
            r <- DBI::dbGetQuery(con, sprintf(
                "SELECT column_name, type, n_rows, n_na, min_val, max_val,
                        is_integerish, levels_json FROM %s", registry))
            r$n_unique <- NA_integer_
            r
        })
    n <- reg$n_rows[1]

    infos <- lapply(seq_len(nrow(reg)), function(i) {
        r <- reg[i, ]
        if (r$type == "numeric") {
            list(name = r$column_name, is_numeric = TRUE, n_na = r$n_na,
                 range = c(r$min_val, r$max_val),
                 is_integerish = isTRUE(r$is_integerish == 1),
                 n_unique = if (!is.na(r$n_unique)) r$n_unique,
                 values = if (!is.na(r$levels_json))
                     as.numeric(jsonlite::fromJSON(r$levels_json)))
        } else {
            list(name = r$column_name, is_numeric = FALSE, n_na = r$n_na,
                 levels = as.character(jsonlite::fromJSON(r$levels_json)))
        }
    })
    names(infos) <- reg$column_name

    universe_ready <- FALSE
    ensure_universe <- function() {
        if (universe_ready) return(invisible())
        have <- DBI::dbExistsTable(con, "row_universe")
        if (!have) {
            DBI::dbExecute(con,
                "CREATE TABLE row_universe (row_id INTEGER PRIMARY KEY)")
            filled <- tryCatch({  # duckdb fast path
                DBI::dbExecute(con, sprintf(
                    "INSERT INTO row_universe SELECT * FROM range(1, %d)",
                    n + 1))
                TRUE
            }, error = function(e) FALSE)
            if (!filled) {        # portable recursive CTE (SQLite)
                DBI::dbExecute(con, sprintf(
                    "INSERT INTO row_universe
                     WITH RECURSIVE c(x) AS
                       (SELECT 1 UNION ALL SELECT x + 1 FROM c WHERE x < %d)
                     SELECT x FROM c", n))
            }
        }
        universe_ready <<- TRUE
        invisible()
    }

    qs <- function(s) as.character(DBI::dbQuoteString(con, s))
    num <- function(x) sprintf("%.17g", x)

    ## one SQL condition per filtered variable, per the NA semantics above
    filter_clauses <- function(filters) {
        out <- character(0)
        for (v in names(filters)) {
            f <- filters[[v]]
            keep_na <- isTRUE(f$include_na %||% TRUE)
            vq <- qs(v)
            clause <- if (is.null(f$val)) {
                if (keep_na) next
                sprintf("row_id IN (SELECT row_id FROM %s WHERE column_name = %s)",
                        table, vq)
            } else if (f$is_numeric && is.character(f$val)) {
                ## discrete numeric (checkbox widget): membership on value_num
                if (length(f$val) == 0) {
                    if (keep_na) {
                        sprintf("row_id NOT IN (SELECT row_id FROM %s WHERE column_name = %s)",
                                table, vq)
                    } else "1 = 0"
                } else {
                    set <- paste(vapply(as.numeric(f$val), num, character(1)),
                                 collapse = ", ")
                    if (keep_na) {
                        sprintf(paste("row_id NOT IN (SELECT row_id FROM %s",
                                      "WHERE column_name = %s AND value_num NOT IN (%s))"),
                                table, vq, set)
                    } else {
                        sprintf(paste("row_id IN (SELECT row_id FROM %s",
                                      "WHERE column_name = %s AND value_num IN (%s))"),
                                table, vq, set)
                    }
                }
            } else if (f$is_numeric) {
                if (keep_na) {
                    sprintf(paste("row_id NOT IN (SELECT row_id FROM %s",
                                  "WHERE column_name = %s AND",
                                  "(value_num < %s OR value_num > %s))"),
                            table, vq, num(f$val[1]), num(f$val[2]))
                } else {
                    sprintf(paste("row_id IN (SELECT row_id FROM %s",
                                  "WHERE column_name = %s AND",
                                  "value_num BETWEEN %s AND %s)"),
                            table, vq, num(f$val[1]), num(f$val[2]))
                }
            } else if (length(f$val) == 0) {
                if (keep_na) {
                    sprintf("row_id NOT IN (SELECT row_id FROM %s WHERE column_name = %s)",
                            table, vq)
                } else "1 = 0"
            } else {
                set <- paste(vapply(f$val, qs, character(1)), collapse = ", ")
                if (keep_na) {
                    sprintf(paste("row_id NOT IN (SELECT row_id FROM %s",
                                  "WHERE column_name = %s AND value_txt NOT IN (%s))"),
                            table, vq, set)
                } else {
                    sprintf(paste("row_id IN (SELECT row_id FROM %s",
                                  "WHERE column_name = %s AND value_txt IN (%s))"),
                            table, vq, set)
                }
            }
            out <- c(out, clause)
        }
        out
    }

    where_sql <- function(clauses) {
        if (length(clauses) == 0) "" else paste(" AND", paste(clauses, collapse = " AND "))
    }

    list(
        get_columns = function() reg$column_name,
        n_rows = function() n,
        get_column_info = function(name) infos[[name]],

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

        supports_binned = TRUE,

        ## histogram counts for `name` over rows passing `filters`,
        ## binned per `spec` (a bin_spec_from_info()/bin_column() result)
        get_binned = function(name, spec, filters) {
            info <- infos[[name]]
            clauses <- filter_clauses(filters)
            vq <- qs(name)
            if (spec$kind == "cat" && info$is_numeric) {
                ## discrete numeric: one bar per distinct value
                res <- DBI::dbGetQuery(con, sprintf(
                    "SELECT value_num AS lev, COUNT(*) AS cnt
                     FROM %s WHERE column_name = %s%s
                     GROUP BY value_num",
                    table, vq, where_sql(clauses)))
                counts <- rep(0, spec$nbins)
                hit <- match(as.character(res$lev), spec$labels)
                counts[hit[!is.na(hit)]] <- res$cnt[!is.na(hit)]
                counts
            } else if (info$is_numeric) {
                bin_expr <- sprintf(
                    "CAST((value_num - %s) / %s AS INTEGER) + 1",
                    num(spec$origin), num(spec$binwidth))
                res <- DBI::dbGetQuery(con, sprintf(
                    "SELECT CASE WHEN %s > %d THEN %d
                                 WHEN %s < 1 THEN 1 ELSE %s END AS bin,
                            COUNT(*) AS cnt
                     FROM %s WHERE column_name = %s AND value_num IS NOT NULL%s
                     GROUP BY bin",
                    bin_expr, spec$nbins, spec$nbins, bin_expr, bin_expr,
                    table, vq, where_sql(clauses)))
                counts <- rep(0, spec$nbins)
                counts[res$bin] <- res$cnt
            } else {
                res <- DBI::dbGetQuery(con, sprintf(
                    "SELECT value_txt AS lev, COUNT(*) AS cnt
                     FROM %s WHERE column_name = %s%s
                     GROUP BY value_txt",
                    table, vq, where_sql(clauses)))
                counts <- rep(0, spec$nbins)
                hit <- match(res$lev, spec$labels)
                counts[hit[!is.na(hit)]] <- res$cnt[!is.na(hit)]
            }
            counts
        },

        get_count = function(filters) {
            clauses <- filter_clauses(filters)
            if (length(clauses) == 0) return(n)
            ensure_universe()
            DBI::dbGetQuery(con, sprintf(
                "SELECT COUNT(*) AS cnt FROM row_universe WHERE %s",
                paste(clauses, collapse = " AND ")))$cnt
        },

        get_row_mask = function(filters) {
            clauses <- filter_clauses(filters)
            if (length(clauses) == 0) return(rep(TRUE, n))
            ensure_universe()
            ids <- DBI::dbGetQuery(con, sprintf(
                "SELECT row_id FROM row_universe WHERE %s",
                paste(clauses, collapse = " AND ")))$row_id
            out <- rep(FALSE, n)
            out[ids] <- TRUE
            out
        },

        disconnect = function() DBI::dbDisconnect(con)
    )
}

backend_sqlite <- function(db_path,
                           table = "long_data",
                           registry = "column_registry") {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("backend_sqlite needs the DBI and RSQLite packages")
    }
    backend_dbi(DBI::dbConnect(RSQLite::SQLite(), db_path), table, registry)
}

backend_duckdb <- function(db_path,
                           table = "long_data",
                           registry = "column_registry") {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("duckdb", quietly = TRUE)) {
        stop("backend_duckdb needs the DBI and duckdb packages")
    }
    backend_dbi(DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                               read_only = FALSE),
                table, registry)
}
