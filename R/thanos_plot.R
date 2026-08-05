################################################################
## Pure (Shiny-free) helpers for masks, binning, and histograms.
## Everything here is unit-testable from a plain R session.
################################################################

## Logical mask for one variable's filter setting.
##   x          full column vector (numeric or character)
##   val        slider range c(lo, hi); a character vector of kept levels
##              (for categorical columns OR discrete numerics rendered as
##              checkboxes -- a character val on a numeric x means
##              membership, not a range); or NULL = "no filter set"
##   include_na whether rows with NA in x survive the filter
make_mask <- function(x, val, include_na = TRUE) {
    if (is.null(val)) {
        ok <- rep(TRUE, length(x))
        return(if (include_na) ok else ok & !is.na(x))
    }
    if (is.numeric(x) && !is.character(val)) {
        ok <- !is.na(x) & x >= val[1] & x <= val[2]
    } else if (is.numeric(x)) {
        ok <- !is.na(x) & as.character(x) %in% val
    } else {
        ok <- !is.na(x) & x %in% val
    }
    if (include_na) ok | is.na(x) else ok
}

## Bin a column once, up front, so every subsequent histogram is O(bins)
## instead of O(rows): plots only ever tabulate() the precomputed indices.
## Fixed breaks over the full-data range also keep the x axis stable while
## filtering (the original geom_histogram re-derived breaks per render).
bin_column <- function(x, bins = 50, discrete_values = NULL, range = NULL,
                       log2p1 = FALSE) {
    ## log2(x+1) display transform for skewed non-negative columns:
    ## bin in log space (breaks, mids and width are log2 units)
    if (log2p1 && is.numeric(x) && is.null(discrete_values)) {
        out <- bin_column(log2(x + 1), bins,
                          range = if (!is.null(range)) log2(range + 1))
        out$log2p1 <- TRUE
        return(out)
    }
    ## a numeric column treated as discrete (few unique values, checkbox
    ## widget) bins like a categorical: one bar per value, in value order
    if (is.numeric(x) && !is.null(discrete_values)) {
        labels <- as.character(discrete_values)
        return(list(kind = "cat", idx = match(as.character(x), labels),
                    labels = labels, nbins = length(labels)))
    }
    if (is.numeric(x)) {
        finite <- x[is.finite(x)]
        if (length(finite) == 0) {
            return(list(kind = "num", idx = rep(NA_integer_, length(x)),
                        mids = 0, width = 1, nbins = 1))
        }
        ## an explicit range (e.g. outlier-robust quantile bounds) wins;
        ## values outside it clamp into the edge bins via all.inside
        rng <- if (!is.null(range) && all(is.finite(range))) range
               else base::range(finite)
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

## Histogram counts for one variable from cached bin indices (vector
## mode): rows passing all OTHER filters ("loo"), the subset also
## passing this variable's own filter, and the row totals for the
## title.  The single tabulation point -- the module and plot_histo()
## both use it.
bin_counts <- function(bin, loo, own) {
    if (bin$nbins == 0) {
        return(list(shown = integer(0), sel = integer(0),
                    n_shown = sum(loo), n_sel = sum(own & loo)))
    }
    list(shown = tabulate(bin$idx[loo], nbins = bin$nbins),
         sel   = tabulate(bin$idx[own & loo], nbins = bin$nbins),
         n_shown = sum(loo), n_sel = sum(own & loo))
}

## Render a histogram from pre-computed bin counts -- the one entry
## point both execution modes and both engines share.
##   spec    a bin spec: kind/nbins plus mids+width (num) or labels (cat);
##           bin_column() and bin_spec_from_info() results qualify
##   shown   per-bin counts of rows passing all OTHER filters
##   sel     per-bin counts of rows passing ALL filters
##   n_shown/n_sel  row totals for the title (may exceed sum(counts)
##           because NA-in-this-var rows are counted but not binned)
##   engine  "ggplot" returns a ggplot object; "base" draws directly to
##           the current device (an order of magnitude faster, see
##           bench/bench_plots.R) and returns NULL
plot_histo_counts <- function(spec, shown, sel, n_shown, n_sel, var,
                              engine = c("ggplot", "base")) {
    if (match.arg(engine) == "base") {
        return(plot_histo_counts_base(spec, shown, sel, n_shown, n_sel, var))
    }
    title <- paste(var, ":", format(n_sel, big.mark = ","),
                   "/", format(n_shown, big.mark = ","))
    if (spec$nbins == 0) {
        return(ggplot() + ggtitle(title) + theme_thanos)
    }
    fills <- factor(rep(c("sel", "unsel"), each = spec$nbins),
                    levels = c("sel", "unsel"))
    if (spec$kind == "num") {
        df <- data.frame(pos = rep(spec$mids, 2),
                         count = c(sel, shown - sel), fill = fills)
        p <- ggplot(df, aes(pos, count, fill = fill)) +
            geom_col(width = spec$width)
    } else {
        df <- data.frame(pos = factor(rep(spec$labels, 2), levels = spec$labels),
                         count = c(sel, shown - sel), fill = fills)
        p <- ggplot(df, aes(pos, count, fill = fill)) +
            geom_col() +
            scale_x_discrete(labels = abbreviate)
    }
    p + ggtitle(title) + scale_fill_thanos() + theme_thanos
}

## Base-graphics twin of plot_histo_counts: same visual (stacked
## sel/unsel bars in the plasma pair, count title, compact axes) drawn
## with rect()/axis() instead of ggplot -- an order of magnitude less
## rendering overhead, for thanosServer(plot_engine = "base").
plot_histo_counts_base <- function(spec, shown, sel, n_shown, n_sel, var) {
    cols <- viridisLite::plasma(2, begin = 0, end = 0.4)  # sel, unsel
    title <- paste(var, ":", format(n_sel, big.mark = ","),
                   "/", format(n_shown, big.mark = ","))
    op <- par(mar = c(2.2, 3.2, 1.6, 0.4), mgp = c(2, 0.6, 0), tcl = -0.3)
    on.exit(par(op))
    if (spec$nbins == 0 || sum(shown) == 0) {
        plot.new()
        title(main = title, adj = 0, cex.main = 1, font.main = 1)
        return(invisible())
    }
    unsel <- shown - sel
    if (spec$kind == "num") {
        half <- spec$width / 2
        xlim <- c(spec$mids[1] - half, spec$mids[spec$nbins] + half)
        plot.new()
        plot.window(xlim = xlim, ylim = c(0, max(shown)), xaxs = "i", yaxs = "i")
        x0 <- spec$mids - half
        x1 <- spec$mids + half
        ## unselected on top of selected, exactly like the ggplot stack
        rect(x0, 0, x1, sel, col = cols[1], border = NA)
        rect(x0, sel, x1, shown, col = cols[2], border = NA)
        axis(1, cex.axis = 1)
        axis(2, cex.axis = 0.75, las = 1)
    } else {
        k <- spec$nbins
        plot.new()
        plot.window(xlim = c(0, k), ylim = c(0, max(shown)), xaxs = "i", yaxs = "i")
        x0 <- seq_len(k) - 0.9
        x1 <- seq_len(k) - 0.1
        rect(x0, 0, x1, sel, col = cols[1], border = NA)
        rect(x0, sel, x1, shown, col = cols[2], border = NA)
        ## draw labels ourselves: axis() silently drops labels that would
        ## overlap, ggplot's scale_x_discrete does not.  Above ~30 levels
        ## no label set is readable, so thin to at most 30 evenly spaced
        labs <- abbreviate(spec$labels, minlength = 4)
        at <- seq_len(k) - 0.5
        if (k > 30) {
            keep <- seq(1, k, by = ceiling(k / 30))
            labs <- labs[keep]
            at <- at[keep]
        }
        cex_lab <- min(1, max(0.55, 18 / length(labs)))
        mtext(labs, side = 1, at = at, line = 0.4, cex = cex_lab)
        axis(2, cex.axis = 0.75, las = 1)
    }
    title(main = title, adj = 0, cex.main = 1, font.main = 1)
    invisible()
}

## The signature Thanos histogram: rows passing all OTHER filters ("loo",
## leave-one-out), stacked as this variable's own selected vs unselected.
##   bin  result of bin_column() for this variable
##   loo  logical mask: rows surviving every other variable's filter
##   own  logical mask: rows surviving this variable's own filter
plot_histo <- function(bin, loo, own, var, engine = c("ggplot", "base")) {
    ct <- bin_counts(bin, loo, own)
    plot_histo_counts(bin, ct$shown, ct$sel, ct$n_shown, ct$n_sel, var,
                      engine = engine)
}

## Outlier-robust display range for a numeric column: the quantile
## bounds when the backend provides them (q_low/q_high, typically 0.1%
## and 99.9%), else the true range.  Sliders and histogram breaks use
## this so a handful of absurd outliers (300,000-mile taxi trips) can't
## crush the real distribution into one bin; outliers clamp into the
## edge bins and a slider handle AT an endpoint means "unbounded".
display_range <- function(info) {
    rng <- info$range
    q <- c(info$q_low %||% NA_real_, info$q_high %||% NA_real_)
    if (all(is.finite(q)) && q[2] > q[1]) {
        rng <- q
        if (isTRUE(info$is_integerish)) rng <- c(floor(rng[1]), ceiling(rng[2]))
    }
    rng
}

## Fixed-break bin spec from registry metadata alone (no column vector),
## for backends that aggregate in SQL.  Mirrors bin_column()'s geometry.
bin_spec_from_info <- function(info, bins = 50, discrete = FALSE,
                               log2p1 = FALSE) {
    if (discrete && info$is_numeric) {
        labels <- as.character(info$values)
        return(list(kind = "cat", labels = labels, nbins = length(labels)))
    }
    if (info$is_numeric) {
        rng <- display_range(info)
        if (log2p1 && all(is.finite(rng))) rng <- log2(rng + 1)
        if (!all(is.finite(rng))) {
            return(list(kind = "num", mids = 0, width = 1, nbins = 1,
                        origin = 0, binwidth = 1, log2p1 = log2p1))
        }
        if (rng[1] == rng[2]) rng <- rng + c(-0.5, 0.5)
        breaks <- seq(rng[1], rng[2], length.out = bins + 1)
        list(kind = "num",
             mids = (breaks[-1] + breaks[-(bins + 1)]) / 2,
             width = breaks[2] - breaks[1], nbins = bins,
             origin = rng[1], binwidth = breaks[2] - breaks[1],
             log2p1 = log2p1)
    } else {
        list(kind = "cat", labels = info$levels, nbins = length(info$levels))
    }
}
