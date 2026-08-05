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
                ## infinite bounds (slider handle at an endpoint =
                ## "unbounded on that side") contribute no condition
                lo <- f$val[1]; hi <- f$val[2]
                if (keep_na) {
                    fail <- c(if (is.finite(lo)) sprintf("value_num < %s", num(lo)),
                              if (is.finite(hi)) sprintf("value_num > %s", num(hi)))
                    if (length(fail) == 0) next  # fully unbounded: no filter
                    sprintf("row_id NOT IN (SELECT row_id FROM %s WHERE column_name = %s AND (%s))",
                            table, vq, paste(fail, collapse = " OR "))
                } else {
                    pass <- c(if (is.finite(lo)) sprintf("value_num >= %s", num(lo)),
                              if (is.finite(hi)) sprintf("value_num <= %s", num(hi)))
                    if (length(pass) == 0) {
                        sprintf("row_id IN (SELECT row_id FROM %s WHERE column_name = %s)",
                                table, vq)
                    } else {
                        sprintf("row_id IN (SELECT row_id FROM %s WHERE column_name = %s AND %s)",
                                table, vq, paste(pass, collapse = " AND "))
                    }
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

        ## histogram counts for `name` over rows passing `filters`,
        ## binned per `spec` (a bin_spec_from_info()/bin_column() result)
        get_binned = function(name, spec, filters) {
          memo_get(memo_key("binned", name, spec_key(spec), filters), function() {
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
                bin_expr <- bin_value_expr(spec)
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
          })
        },

        ## shown and sel counts in ONE query: shown = rows passing the
        ## leave-one-out filters, sel = of those, rows also passing this
        ## variable's own filter (a CASE on the value itself -- cheap).
        ## Halves the per-plot query load vs two get_binned() calls.
        get_binned_pair = function(name, spec, loo_filters, own) {
          memo_get(memo_key("pair", name, spec_key(spec), loo_filters, own),
                   function() {
            info <- infos[[name]]
            clauses <- filter_clauses(loo_filters)
            vq <- qs(name)
            value_col <- if (info$is_numeric) "value_num" else "value_txt"
            own_pred <- if (is.null(own) || is.null(own$val)) {
                NULL                                   # no own filter: sel = shown
            } else if (info$is_numeric && is.character(own$val)) {
                if (length(own$val) == 0) "1 = 0"
                else sprintf("value_num IN (%s)",
                             paste(vapply(as.numeric(own$val), num,
                                          character(1)), collapse = ", "))
            } else if (info$is_numeric) {
                pass <- c(if (is.finite(own$val[1]))
                              sprintf("value_num >= %s", num(own$val[1])),
                          if (is.finite(own$val[2]))
                              sprintf("value_num <= %s", num(own$val[2])))
                if (length(pass) == 0) NULL
                else paste(pass, collapse = " AND ")
            } else {
                if (length(own$val) == 0) "1 = 0"
                else sprintf("value_txt IN (%s)",
                             paste(vapply(own$val, qs, character(1)),
                                   collapse = ", "))
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
        },

        get_count = function(filters) {
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

backend_sqlite <- function(db_path,
                           table = "long_data",
                           registry = "column_registry",
                           cache = TRUE,
                           cache_max_entries = 256) {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("backend_sqlite needs the DBI and RSQLite packages")
    }
    backend_dbi(DBI::dbConnect(RSQLite::SQLite(), db_path), table, registry,
                cache = cache, cache_max_entries = cache_max_entries)
}

backend_duckdb <- function(db_path,
                           table = "long_data",
                           registry = "column_registry",
                           cache = TRUE,
                           cache_max_entries = 256) {
    if (!requireNamespace("DBI", quietly = TRUE) ||
        !requireNamespace("duckdb", quietly = TRUE)) {
        stop("backend_duckdb needs the DBI and duckdb packages")
    }
    backend_dbi(DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                               read_only = FALSE),
                table, registry,
                cache = cache, cache_max_entries = cache_max_entries)
}
