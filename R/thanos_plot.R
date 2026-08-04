################################################################
## Pure (Shiny-free) helpers for masks, binning, and histograms.
## Everything here is unit-testable from a plain R session.
################################################################
library(ggplot2)

## Logical mask for one variable's filter setting.
##   x          full column vector (numeric or character)
##   val        slider range c(lo, hi), a character vector of kept levels,
##              or NULL meaning "no filter set" (all rows pass)
##   include_na whether rows with NA in x survive the filter
make_mask <- function(x, val, include_na = TRUE) {
    if (is.null(val)) {
        ok <- rep(TRUE, length(x))
        return(if (include_na) ok else ok & !is.na(x))
    }
    if (is.numeric(x)) {
        ok <- !is.na(x) & x >= val[1] & x <= val[2]
    } else {
        ok <- !is.na(x) & x %in% val
    }
    if (include_na) ok | is.na(x) else ok
}

## Bin a column once, up front, so every subsequent histogram is O(bins)
## instead of O(rows): plots only ever tabulate() the precomputed indices.
## Fixed breaks over the full-data range also keep the x axis stable while
## filtering (the original geom_histogram re-derived breaks per render).
bin_column <- function(x, bins = 50) {
    if (is.numeric(x)) {
        finite <- x[is.finite(x)]
        if (length(finite) == 0) {
            return(list(kind = "num", idx = rep(NA_integer_, length(x)),
                        mids = 0, width = 1, nbins = 1))
        }
        rng <- range(finite)
        if (rng[1] == rng[2]) rng <- rng + c(-0.5, 0.5)
        breaks <- seq(rng[1], rng[2], length.out = bins + 1)
        idx <- findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
        idx[!is.finite(x)] <- NA_integer_
        list(kind = "num", idx = idx,
             mids = (breaks[-1] + breaks[-(bins + 1)]) / 2,
             width = breaks[2] - breaks[1], nbins = bins)
    } else {
        x <- as.character(x)
        levs <- sort(unique(x[!is.na(x)]))
        if (length(levs) == 0) {
            return(list(kind = "cat", idx = rep(NA_integer_, length(x)),
                        labels = character(0), nbins = 0))
        }
        list(kind = "cat", idx = match(x, levs), labels = levs,
             nbins = length(levs))
    }
}

## The signature Thanos histogram: rows passing all OTHER filters ("loo",
## leave-one-out), stacked as this variable's own selected vs unselected.
##   bin  result of bin_column() for this variable
##   loo  logical mask: rows surviving every other variable's filter
##   own  logical mask: rows surviving this variable's own filter
plot_histo <- function(bin, loo, own, var) {
    n_shown <- sum(loo)
    n_sel   <- sum(own & loo)
    title <- paste(var, ":", format(n_sel, big.mark = ","),
                   "/", format(n_shown, big.mark = ","))
    if (bin$nbins == 0) {
        return(ggplot() + ggtitle(title) + theme_thanos)
    }
    shown <- tabulate(bin$idx[loo], nbins = bin$nbins)
    sel   <- tabulate(bin$idx[own & loo], nbins = bin$nbins)
    fills <- factor(rep(c("sel", "unsel"), each = bin$nbins),
                    levels = c("sel", "unsel"))
    if (bin$kind == "num") {
        df <- data.frame(pos = rep(bin$mids, 2),
                         count = c(sel, shown - sel), fill = fills)
        p <- ggplot(df, aes(pos, count, fill = fill)) +
            geom_col(width = bin$width)
    } else {
        df <- data.frame(pos = factor(rep(bin$labels, 2), levels = bin$labels),
                         count = c(sel, shown - sel), fill = fills)
        p <- ggplot(df, aes(pos, count, fill = fill)) +
            geom_col() +
            scale_x_discrete(labels = abbreviate)
    }
    p + ggtitle(title) + scale_fill_thanos() + theme_thanos
}
