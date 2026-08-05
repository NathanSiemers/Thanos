################################################################
## Shared column handling -- the ONE place that defines which column
## types Thanos supports, how they are coerced, and which statistics
## describe a column.  Used by backend_memory() AND the database build
## scripts, so the in-memory and tall/skinny data paths agree BY
## CONSTRUCTION (the backend-equivalence tests then prove it).
################################################################

## Keep supported columns and coerce them to Thanos' two value types:
## factors and logicals become character (categorical widgets),
## Date/POSIXct become numeric (sliders over epoch values), integers
## become double (SQLite REAL storage has no integer type, and the two
## backends must return identical() vectors).  Anything non-atomic
## (list columns etc.) is dropped.
thanos_coerce_columns <- function(df) {
    df <- as.data.frame(df)
    supported <- vapply(df, function(x) {
        is.numeric(x) || is.character(x) || is.factor(x) || is.logical(x) ||
            inherits(x, c("POSIXct", "Date"))
    }, NA)
    df <- df[supported]
    df[] <- lapply(df, function(x) {
        if (inherits(x, c("POSIXct", "Date"))) as.numeric(x)
        else if (is.factor(x) || is.logical(x)) as.character(x)
        else if (is.integer(x)) as.numeric(x)
        else x
    })
    df
}

## Everything the module needs to know about one (coerced) column to
## build its widget and bin geometry: type, NA count, range,
## integerish-ness, distinct-value count (and the values themselves
## when few enough for a checkbox widget), outlier-robust quantile
## bounds, or the sorted level set for categoricals.
thanos_column_stats <- function(x) {
    if (is.numeric(x)) {
        vals <- sort(unique(x[!is.na(x)]))
        q <- if (length(vals) > 0) {
            unname(quantile(x, c(0.001, 0.999), na.rm = TRUE))
        } else c(NA_real_, NA_real_)
        list(is_numeric = TRUE, n_na = sum(is.na(x)),
             range = suppressWarnings(range(x, na.rm = TRUE)),
             is_integerish = is.integer(x) ||
                 isTRUE(all(x == round(x), na.rm = TRUE)),
             n_unique = length(vals),
             values = if (length(vals) <= 100) vals,
             q_low = q[1], q_high = q[2])
    } else {
        levs <- sort(unique(x[!is.na(x)]))
        list(is_numeric = FALSE, n_na = sum(is.na(x)), levels = levs)
    }
}
