################################################################
## Thanos backend contract
##
## A backend is a plain named list of functions.  The module treats
## the data as a column store: it fetches a column once when the user
## selects it and never again while it stays selected, so a backend can
## be an in-memory data frame or a database without the module changing.
##
##   $get_columns()          -> character vector of available column names
##   $n_rows()               -> integer; row IDs are 1..n_rows
##   $get_column(name)       -> full-length vector, NAs preserved, in
##                              row-ID order; numeric or character
##   $get_column_info(name)  -> list(name, is_numeric, n_na, and
##                              range + is_integerish + n_unique +
##                              values + q_low/q_high   (numeric) or
##                              levels                  (categorical))
##                              cheap column metadata for building widgets
##                              without shipping the whole column
##
## Optional capabilities (see thanos_backend_sqlite.R): supports_binned
## + the aggregate query functions, supports_log2, caching accessors.
################################################################

## In-memory backend wrapping a data frame.  Column type handling and
## statistics live in thanos_columns.R, shared with the database build
## scripts so all data paths agree by construction.
#' In-memory backend wrapping a data frame
#'
#' Wraps a data frame in the Thanos backend contract: the module treats
#' it as a column store and fetches each column at most once while it
#' stays selected. Factors and logicals are coerced to character,
#' Date/POSIXct to numeric (epoch values), integers to double;
#' non-atomic (e.g. list) columns are dropped.
#'
#' @param df A data frame (or anything `as.data.frame()` accepts).
#'
#' @return A backend: a named list of functions (`get_columns`,
#'   `n_rows`, `get_column`, `get_column_info`) implementing the Thanos
#'   backend contract, suitable for [thanosServer()].
#'
#' @examples
#' backend <- backend_memory(mtcars)
#' backend$n_rows()
#' backend$get_columns()
#' backend$get_column_info("mpg")$is_numeric
#' @export
backend_memory <- function(df) {
    df <- thanos_coerce_columns(df)
    info_cache <- new.env(parent = emptyenv())
    list(
        get_columns = function() names(df),
        n_rows = function() nrow(df),
        get_column = function(name) df[[name]],
        get_column_info = function(name) {
            cached <- info_cache[[name]]
            if (!is.null(cached)) return(cached)
            info <- c(list(name = name), thanos_column_stats(df[[name]]))
            assign(name, info, envir = info_cache)
            info
        }
    )
}
