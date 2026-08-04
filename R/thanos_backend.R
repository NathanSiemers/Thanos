################################################################
## Thanos backend contract
##
## A backend is a plain named list of four functions.  The module treats
## the data as a column store: it fetches a column once when the user
## selects it and never again while it stays selected, so a backend can
## be an in-memory data frame or a database without the module changing.
##
##   $get_columns()          -> character vector of available column names
##   $n_rows()               -> integer; row IDs are 1..n_rows
##   $get_column(name)       -> full-length vector, NAs preserved, in
##                              row-ID order; numeric or character
##   $get_column_info(name)  -> list(name, is_numeric, n_na, and
##                              range + is_integerish  (numeric) or
##                              levels                 (categorical))
##                              cheap column metadata for building widgets
##                              without shipping the whole column
################################################################

## In-memory backend wrapping a data frame.
## Column handling: factors and logicals become character (categorical
## widgets), Date/POSIXct become numeric (sliders over epoch values),
## anything else non-atomic (list columns etc.) is dropped.
backend_memory <- function(df) {
    df <- as.data.frame(df)
    supported <- vapply(df, function(x) {
        is.numeric(x) || is.character(x) || is.factor(x) || is.logical(x) ||
            inherits(x, c("POSIXct", "Date"))
    }, NA)
    df <- df[supported]
    df[] <- lapply(df, function(x) {
        if (inherits(x, c("POSIXct", "Date"))) as.numeric(x)
        else if (is.factor(x) || is.logical(x)) as.character(x)
        else x
    })

    info_cache <- new.env(parent = emptyenv())
    list(
        get_columns = function() names(df),
        n_rows = function() nrow(df),
        get_column = function(name) df[[name]],
        get_column_info = function(name) {
            cached <- info_cache[[name]]
            if (!is.null(cached)) return(cached)
            x <- df[[name]]
            info <- if (is.numeric(x)) {
                list(name = name, is_numeric = TRUE, n_na = sum(is.na(x)),
                     range = suppressWarnings(range(x, na.rm = TRUE)),
                     is_integerish = is.integer(x) ||
                         isTRUE(all(x == round(x), na.rm = TRUE)))
            } else {
                list(name = name, is_numeric = FALSE, n_na = sum(is.na(x)),
                     levels = sort(unique(x[!is.na(x)])))
            }
            assign(name, info, envir = info_cache)
            info
        }
    )
}
