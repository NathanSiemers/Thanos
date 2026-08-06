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
##
## CACHING (cache = TRUE by default): fetched columns are kept for the
## backend's lifetime, and aggregate query results are memoised (bounded
## at cache_max_entries, oldest evicted).  Both assume the database is
## immutable while open -- call $clear_cache() if the data changes;
## $cache_stats() reports hits/misses/entries/approximate bytes.  Pass
## cache = FALSE for mutable data or to cap memory at very large scale.
################################################################

#' DBI backend over the tall/skinny Thanos schema
#'
#' Implements the Thanos backend contract for any DBI connection
#' holding the tall/skinny schema `long_data(row_id, column_name,
#' value_num, value_txt)` plus a `column_registry` metadata table (see
#' the build scripts under `db/` in the Thanos repository). Beyond the
#' column-store contract it advertises the aggregate capability
#' (`supports_binned`): histogram counts, row counts, and row masks are
#' computed by SQL `GROUP BY` queries, so tens of millions of rows can
#' be filtered without a column vector ever entering R.
#'
#' Fetched columns and aggregate query results are cached by default;
#' the cache assumes the database is immutable while the backend is
#' open. Call `$clear_cache()` after a data change and
#' `$cache_stats()` for hits/misses/bytes.
#'
#' @param con A `DBI` connection to a database holding the tall/skinny
#'   schema.
#' @param table Name of the tall/skinny data table.
#' @param registry Name of the column-registry metadata table.
#' @param cache Cache fetched columns and memoise aggregate query
#'   results (assumes the database is immutable while open).
#' @param cache_max_entries Maximum number of memoised aggregate query
#'   results (oldest evicted first).
#'
#' @return A backend implementing the Thanos backend contract, with the
#'   aggregate-mode extensions (`supports_binned`, `get_binned`,
#'   `get_binned_pair`, `get_count`, `get_row_mask`), caching accessors
#'   (`clear_cache`, `cache_stats`), and `disconnect()`.
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), "db/data/flights.sqlite")
#' backend <- backend_dbi(con)
#' backend$get_columns()
#' backend$disconnect()
#' }
#' @export
backend_dbi <- function(con,
                        table = "long_data",
                        registry = "column_registry",
                        cache = TRUE,
                        cache_max_entries = 256) {
    ## registries from older builds may lack newer columns (n_unique,
    ## q_low/q_high); read what exists and fill the rest with NA so old
    ## databases keep working (the associated features just stay off)
    reg <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", registry))
    for (col in c("n_unique", "q_low", "q_high")) {
        if (is.null(reg[[col]])) reg[[col]] <- NA_real_
    }
    n <- reg$n_rows[1]

    infos <- lapply(seq_len(nrow(reg)), function(i) {
        r <- reg[i, ]
        if (r$type == "numeric") {
            list(name = r$column_name, is_numeric = TRUE, n_na = r$n_na,
                 range = c(r$min_val, r$max_val),
                 is_integerish = isTRUE(r$is_integerish == 1),
                 n_unique = if (!is.na(r$n_unique)) r$n_unique,
                 values = if (!is.na(r$levels_json))
                     as.numeric(jsonlite::fromJSON(r$levels_json)),
                 q_low = r$q_low, q_high = r$q_high)
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
            ## a TEMP table so this works on READ-ONLY connections
            ## (temp storage is separate from the database file); build
            ## scripts create a permanent one, which is found above
            DBI::dbExecute(con,
                "CREATE TEMP TABLE row_universe (row_id INTEGER PRIMARY KEY)")
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

    ## the log2(x+1) display transform needs log2() in SQL; SQLite may be
    ## compiled without math functions, so probe once
    log2_ok <- tryCatch({
        DBI::dbGetQuery(con, "SELECT log2(2.0) AS x")$x == 1
    }, error = function(e) FALSE)
    ## engines disagree on CAST(double AS INTEGER): SQLite truncates
    ## (= floor for our non-negative offsets), DuckDB ROUNDS, which would
    ## shift every histogram by half a bin -- use floor() where available
    floor_ok <- tryCatch({
        DBI::dbGetQuery(con, "SELECT floor(1.7) AS x")$x == 1
    }, error = function(e) FALSE)

    ## the binning expression for a numeric spec, honoring the transform
    bin_value_expr <- function(spec) {
        v <- if (isTRUE(spec$log2p1)) "log2(value_num + 1)" else "value_num"
        ratio <- sprintf("(%s - %s) / %s", v, num(spec$origin), num(spec$binwidth))
        if (floor_ok) sprintf("CAST(floor(%s) AS INTEGER) + 1", ratio)
        else sprintf("CAST(%s AS INTEGER) + 1", ratio)
    }

    ## THE single translator from a filter entry to SQL conditions on the
    ## value column.  Returns:
    ##   $pass  condition a value must meet (NULL = unrestricted)
    ##   $fail  condition marking a value as failing (NULL = none fail)
    ##   $none  TRUE when the selection passes no present value at all
    ##          (an empty checkbox set): only NA rows can survive
    ## Handles the three filter shapes: numeric range with possibly
    ## infinite bounds (slider endpoint = unbounded contributes nothing),
    ## discrete-numeric membership, and categorical level sets.
    value_predicate <- function(f) {
        if (is.null(f$val)) return(list(pass = NULL, fail = NULL, none = FALSE))
        if (f$is_numeric && is.character(f$val)) {          # discrete numeric
            if (length(f$val) == 0) return(list(pass = NULL, fail = NULL,
                                                none = TRUE))
            set <- paste(vapply(as.numeric(f$val), num, character(1)),
                         collapse = ", ")
            list(pass = sprintf("value_num IN (%s)", set),
                 fail = sprintf("value_num NOT IN (%s)", set), none = FALSE)
        } else if (f$is_numeric) {                          # range
            lo <- f$val[1]; hi <- f$val[2]
            pass <- c(if (is.finite(lo)) sprintf("value_num >= %s", num(lo)),
                      if (is.finite(hi)) sprintf("value_num <= %s", num(hi)))
            fail <- c(if (is.finite(lo)) sprintf("value_num < %s", num(lo)),
                      if (is.finite(hi)) sprintf("value_num > %s", num(hi)))
            list(pass = if (length(pass)) paste(pass, collapse = " AND "),
                 fail = if (length(fail)) paste(fail, collapse = " OR "),
                 none = FALSE)
        } else {                                            # categorical
            if (length(f$val) == 0) return(list(pass = NULL, fail = NULL,
                                                none = TRUE))
            set <- paste(vapply(f$val, qs, character(1)), collapse = ", ")
            list(pass = sprintf("value_txt IN (%s)", set),
                 fail = sprintf("value_txt NOT IN (%s)", set), none = FALSE)
        }
    }

    ## one row_id condition per filtered variable, wrapping the value
    ## predicates in the NA-by-absence algebra described above
    filter_clauses <- function(filters) {
        out <- character(0)
        for (v in names(filters)) {
            f <- filters[[v]]
            keep_na <- isTRUE(f$include_na %||% TRUE)
            vq <- qs(v)
            pr <- value_predicate(f)
            presence <- sprintf(
                "(SELECT row_id FROM %s WHERE column_name = %s)", table, vq)
            clause <- if (keep_na) {
                if (pr$none) {
                    paste("row_id NOT IN", presence)
                } else if (is.null(pr$fail)) {
                    next   # unrestricted: no clause at all
                } else {
                    sprintf("row_id NOT IN (SELECT row_id FROM %s WHERE column_name = %s AND (%s))",
                            table, vq, pr$fail)
                }
            } else {
                if (pr$none) {
                    "1 = 0"
                } else if (is.null(pr$pass)) {
                    paste("row_id IN", presence)
                } else {
                    sprintf("row_id IN (SELECT row_id FROM %s WHERE column_name = %s AND %s)",
                            table, vq, pr$pass)
                }
            }
            out <- c(out, clause)
        }
        out
    }

    where_sql <- function(clauses) {
        if (length(clauses) == 0) "" else paste(" AND", paste(clauses, collapse = " AND "))
    }

    ## ---- caching (cache = TRUE by default) --------------------------
    ## Two caches, both assuming the database is IMMUTABLE while the
    ## backend is open (call $clear_cache() if the data changes):
    ##   col_cache  full column vectors, keyed by column name.  Few keys
    ##              but real memory (a 38M-row numeric column is ~300 MB;
    ##              pass cache = FALSE at that scale if columns are ever
    ##              fetched -- aggregate mode doesn't fetch them at all).
    ##   memo       aggregate query results (binned counts, row counts,
    ##              row masks), keyed by the deparsed query arguments and
    ##              bounded at cache_max_entries with oldest-first
    ##              eviction.  Makes revisited filter states (a checkbox
    ##              toggled off and back on) instant.
    col_cache <- new.env(parent = emptyenv())
    memo      <- new.env(parent = emptyenv())
    stats <- new.env(parent = emptyenv())
    stats$hits <- 0L; stats$misses <- 0L; stats$tick <- 0L

    memo_key <- function(...) {
        paste(vapply(list(...),
                     function(x) paste(deparse(x), collapse = " "),
                     character(1)),
              collapse = " | ")
    }
    memo_get <- function(key, compute) {
        if (!cache) return(compute())
        hit <- memo[[key]]
        if (!is.null(hit)) {
            stats$hits <- stats$hits + 1L
            return(hit$value)
        }
        stats$misses <- stats$misses + 1L
        val <- compute()
        keys <- ls(memo)
        if (length(keys) >= cache_max_entries) {
            ticks <- vapply(keys, function(k) memo[[k]]$tick, integer(1))
            rm(list = keys[which.min(ticks)], envir = memo)
        }
        stats$tick <- stats$tick + 1L
        memo[[key]] <- list(value = val, tick = stats$tick)
        val
    }
    ## bin_column() results carry a row-length idx vector; never let it
    ## into a deparse()d key
    spec_key <- function(spec) spec[setdiff(names(spec), "idx")]

    ## shown and sel counts in ONE query: shown = rows passing the
    ## leave-one-out filters, sel = of those, rows also passing this
    ## variable's own filter (a CASE on the value itself -- cheap).
    ## get_binned() is this with no own filter, so it delegates.
    get_binned_pair <- function(name, spec, loo_filters, own) {
      memo_get(memo_key("pair", name, spec_key(spec), loo_filters, own),
               function() {
        info <- infos[[name]]
        clauses <- filter_clauses(loo_filters)
        vq <- qs(name)
        value_col <- if (info$is_numeric) "value_num" else "value_txt"
        own_pred <- if (is.null(own)) NULL else {
            own$is_numeric <- info$is_numeric   # the column decides its type
            pr <- value_predicate(own)
            if (pr$none) "1 = 0" else pr$pass
        }
        sel_expr <- if (is.null(own_pred)) "COUNT(*)" else
            sprintf("SUM(CASE WHEN %s THEN 1 ELSE 0 END)", own_pred)
        if (spec$kind == "num") {
            bin_expr <- bin_value_expr(spec)
            res <- DBI::dbGetQuery(con, sprintf(
                "SELECT CASE WHEN %s > %d THEN %d
                             WHEN %s < 1 THEN 1 ELSE %s END AS bin,
                        COUNT(*) AS shown, %s AS sel
                 FROM %s WHERE column_name = %s AND value_num IS NOT NULL%s
                 GROUP BY bin",
                bin_expr, spec$nbins, spec$nbins, bin_expr, bin_expr,
                sel_expr, table, vq, where_sql(clauses)))
            shown <- rep(0, spec$nbins); sel <- rep(0, spec$nbins)
            shown[res$bin] <- res$shown
            sel[res$bin]   <- res$sel
        } else {
            res <- DBI::dbGetQuery(con, sprintf(
                "SELECT %s AS lev, COUNT(*) AS shown, %s AS sel
                 FROM %s WHERE column_name = %s%s
                 GROUP BY %s",
                value_col, sel_expr, table, vq, where_sql(clauses),
                value_col))
            shown <- rep(0, spec$nbins); sel <- rep(0, spec$nbins)
            hit <- match(as.character(res$lev), spec$labels)
            shown[hit[!is.na(hit)]] <- res$shown[!is.na(hit)]
            sel[hit[!is.na(hit)]]   <- res$sel[!is.na(hit)]
        }
        list(shown = shown, sel = sel)
      })
    }

    list(
        get_columns = function() reg$column_name,
        n_rows = function() n,
        get_column_info = function(name) infos[[name]],

        get_column = function(name) {
            if (cache) {
                got <- col_cache[[name]]
                if (!is.null(got)) {
                    stats$hits <- stats$hits + 1L
                    return(got)
                }
                stats$misses <- stats$misses + 1L
            }
            info <- infos[[name]]
            if (is.null(info)) stop("unknown column: ", name)
            value_col <- if (info$is_numeric) "value_num" else "value_txt"
            res <- DBI::dbGetQuery(con, sprintf(
                "SELECT row_id, %s AS value FROM %s WHERE column_name = ?",
                value_col, table), params = list(name))
            out <- if (info$is_numeric) rep(NA_real_, n) else rep(NA_character_, n)
            out[res$row_id] <- res$value
            if (cache) col_cache[[name]] <- out
            out
        },

        supports_binned = TRUE,
        supports_log2 = log2_ok,

        get_binned_pair = get_binned_pair,

        ## histogram counts over rows passing `filters`: the pair query
        ## with no own filter, so shown IS the answer
        get_binned = function(name, spec, filters) {
            get_binned_pair(name, spec, filters, NULL)$shown
        },

        get_count = function(filters) {
          ## the no-filter state needs no query and no memo entry
          if (length(filters) == 0) return(n)
          memo_get(memo_key("count", filters), function() {
            clauses <- filter_clauses(filters)
            if (length(clauses) == 0) return(n)
            ensure_universe()
            DBI::dbGetQuery(con, sprintf(
                "SELECT COUNT(*) AS cnt FROM row_universe WHERE %s",
                paste(clauses, collapse = " AND ")))$cnt
          })
        },

        get_row_mask = function(filters) {
          if (length(filters) == 0) return(rep(TRUE, n))
          memo_get(memo_key("mask", filters), function() {
            clauses <- filter_clauses(filters)
            if (length(clauses) == 0) return(rep(TRUE, n))
            ensure_universe()
            ids <- DBI::dbGetQuery(con, sprintf(
                "SELECT row_id FROM row_universe WHERE %s",
                paste(clauses, collapse = " AND ")))$row_id
            out <- rep(FALSE, n)
            out[ids] <- TRUE
            out
          })
        },

        clear_cache = function() {
            rm(list = ls(col_cache), envir = col_cache)
            rm(list = ls(memo), envir = memo)
            stats$hits <- 0L; stats$misses <- 0L; stats$tick <- 0L
            invisible()
        },

        cache_stats = function() {
            approx_bytes <- sum(
                vapply(ls(col_cache),
                       function(k) as.numeric(object.size(col_cache[[k]])), 0),
                vapply(ls(memo),
                       function(k) as.numeric(object.size(memo[[k]]$value)), 0))
            list(enabled = cache,
                 hits = stats$hits, misses = stats$misses,
                 columns_cached = length(ls(col_cache)),
                 memo_entries = length(ls(memo)),
                 approx_bytes = approx_bytes)
        },

        disconnect = function() DBI::dbDisconnect(con)
    )
}

## Both wrappers open READ-ONLY by default so any number of processes
## (multiple app instances, benchmarks, an interactive session) can
## share one database file; the backends never need write access -- the
## row_universe helper falls back to a TEMP table when absent.
#' SQLite backend over the tall/skinny Thanos schema
#'
#' Thin wrapper around [backend_dbi()] that opens a SQLite database
#' file, read-only by default so any number of processes can share it.
#'
#' @param db_path Path to the SQLite database file (tall/skinny schema;
#'   see the build scripts under `db/` in the Thanos repository).
#' @param read_only Open the database read-only (default `TRUE`).
#' @inheritParams backend_dbi
#'
#' @return A backend as described in [backend_dbi()].
#'
#' @examples
#' \dontrun{
#' backend <- backend_sqlite("db/data/flights.sqlite")
#' backend$n_rows()
#' backend$disconnect()
#' }
#' @export
backend_sqlite <- function(db_path,
                           table = "long_data",
                           registry = "column_registry",
                           cache = TRUE,
                           cache_max_entries = 256,
                           read_only = TRUE) {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("backend_sqlite needs the DBI and RSQLite packages")
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path,
                          flags = if (read_only) RSQLite::SQLITE_RO
                                  else RSQLite::SQLITE_RW)
    backend_dbi(con, table, registry,
                cache = cache, cache_max_entries = cache_max_entries)
}

#' DuckDB backend over the tall/skinny Thanos schema
#'
#' Thin wrapper around [backend_dbi()] that opens a DuckDB database
#' file, read-only by default. The recommended backend at many millions
#' of rows (aggregate mode).
#'
#' @param db_path Path to the DuckDB database file (tall/skinny schema;
#'   see the build scripts under `db/` in the Thanos repository).
#' @param read_only Open the database read-only (default `TRUE`).
#' @inheritParams backend_dbi
#'
#' @return A backend as described in [backend_dbi()].
#'
#' @examples
#' \dontrun{
#' backend <- backend_duckdb("db/data/taxi.duckdb")
#' backend$n_rows()
#' backend$disconnect()
#' }
#' @export
backend_duckdb <- function(db_path,
                           table = "long_data",
                           registry = "column_registry",
                           cache = TRUE,
                           cache_max_entries = 256,
                           read_only = TRUE) {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("duckdb", quietly = TRUE)) {
        stop("backend_duckdb needs the DBI and duckdb packages")
    }
    backend_dbi(DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                               read_only = read_only),
                table, registry,
                cache = cache, cache_max_entries = cache_max_entries)
}
