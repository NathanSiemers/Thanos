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
bin_counts <- function(bin, loo, own = NULL) {
    n_shown <- sum(loo)
    if (is.null(own)) {
        ## no own filter: the selected set IS the shown set
        shown <- if (bin$nbins == 0) integer(0)
                 else tabulate(bin$idx[loo], nbins = bin$nbins)
        return(list(shown = shown, sel = shown,
                    n_shown = n_shown, n_sel = n_shown))
    }
    both <- own & loo
    if (bin$nbins == 0) {
        return(list(shown = integer(0), sel = integer(0),
                    n_shown = n_shown, n_sel = sum(both)))
    }
    list(shown = tabulate(bin$idx[loo], nbins = bin$nbins),
         sel   = tabulate(bin$idx[both], nbins = bin$nbins),
         n_shown = n_shown, n_sel = sum(both))
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
        ## a ggplot object does not know its device yet, so the label
        ## layout is decided for a nominal panel (5in wide, 2in tall, 12pt
        ## text ~ 0.6 em per character) -- same rules as the base engine
        font_in <- 12 / 72
        lay <- cat_label_layout(
            spec$labels, slot = 5 / spec$nbins,
            width_of = function(l, cex) nchar(l) * 0.6 * font_in * cex,
            line_h = font_in * 0.9, max_depth = 0.7)
        p <- ggplot(df, aes(pos, count, fill = fill)) +
            geom_col() +
            scale_x_discrete(breaks = spec$labels[lay$keep],
                             labels = lay$labels[lay$keep])
        return(p + ggtitle(title) + scale_fill_thanos() + theme_thanos +
               theme(axis.text.x = element_text(
                   size = 12 * lay$cex, angle = lay$angle,
                   hjust = if (lay$angle == 0) 0.5 else 1,
                   vjust = if (lay$angle == 90) 0.5 else 1)))
    }
    p + ggtitle(title) + scale_fill_thanos() + theme_thanos
}

## How to label k equal-width bars so the labels never run into each
## other -- pure geometry, shared by both plot engines.
##   labels     the full category labels, one per bar
##   slot       width available to one bar's label (inches)
##   width_of   function(labels, cex) -> each label's width in inches
##   line_h     room one line of text needs at cex 1 (inches)
##   max_depth  how far rotated labels may reach below the axis (inches)
##   min_cex    smallest font scale considered readable
## Strategy, in order of preference:
##   1. horizontal: full labels, else abbreviated, side by side (the
##      shortest abbreviations may shrink to 75% to stay horizontal)
##   2. rotated: 60 degrees when adjacent slanted lines clear each
##      other, else vertical (the tightest packing)
##   3. still too dense: shrink the font to the size at which lines just
##      clear each other
##   4. below min_cex nothing is readable: keep that size and label only
##      every n-th bar
## Rotated labels are abbreviated only as far as max_depth demands.
## Returns list(labels, keep = which bars get a label, angle = 0/60/90,
##              cex, depth = inches the labels need below the axis).
cat_label_layout <- function(labels, slot, width_of, line_h, max_depth,
                             min_cex = 0.5) {
    k <- length(labels)
    abbr <- function(n) {
        if (is.finite(n)) abbreviate(labels, minlength = n, named = FALSE)
        else labels
    }
    widest <- function(l, cex) if (length(l)) max(width_of(l, cex)) else 0
    room <- slot * 0.92          # leave a sliver between neighbours
    for (n in c(Inf, 8, 4)) {
        labs <- abbr(n)
        if (widest(labs, 1) <= room) {
            return(list(labels = labs, keep = seq_len(k), angle = 0,
                        cex = 1, depth = line_h))
        }
    }
    ## nearly fits: a slightly smaller font keeps short abbreviations
    ## horizontal, which costs the bars no height at all
    w4 <- widest(labs, 1)
    if (w4 * 0.75 <= room) {
        return(list(labels = labs, keep = seq_len(k), angle = 0,
                    cex = room / w4, depth = line_h))
    }
    ## rotated: adjacent baselines are slot * sin(angle) apart
    angle <- if (slot * sin(pi / 3) >= line_h) 60 else 90
    s <- sin(angle * pi / 180)
    cex <- min(1, slot * s / line_h)
    step <- 1
    if (cex < min_cex) {
        cex <- min_cex
        step <- ceiling(line_h * min_cex / (slot * s))
    }
    for (n in c(Inf, 16, 12, 8, 6, 4)) {
        labs <- abbr(n)
        if (widest(labs, cex) * s <= max_depth) break
    }
    ## abbreviations stay unique, so they can still be too long: shrink
    ## the font towards min_cex, and only then cut the text itself
    depth <- widest(labs, cex) * s
    if (depth > max_depth) {
        cex <- max(min_cex, cex * max_depth / depth)
        while (widest(labs, cex) * s > max_depth && max(nchar(labs)) > 1) {
            labs <- substr(labs, 1, max(nchar(labs)) - 1)
        }
    }
    list(labels = labs, keep = seq(1, k, by = step), angle = angle,
         cex = cex, depth = widest(labs, cex) * s)
}

## Base-graphics twin of plot_histo_counts: same visual (stacked
## sel/unsel bars in the plasma pair, count title, compact axes) drawn
## with rect()/axis() instead of ggplot -- an order of magnitude less
## rendering overhead, for thanosServer(plot_engine = "base").
plot_histo_counts_base <- function(spec, shown, sel, n_shown, n_sel, var) {
    cols <- viridisLite::plasma(2, begin = 0, end = 0.4)  # sel, unsel
    title <- paste(var, ":", format(n_sel, big.mark = ","),
                   "/", format(n_shown, big.mark = ","))
    op <- par(mar = c(2.2, 3.2, 1.6, 0.4), mgp = c(2, 0.6, 0), tcl = -0.3,
              xpd = FALSE)
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
        ## draw labels ourselves (axis() silently drops labels that would
        ## overlap), laid out from the REAL geometry of this device: the
        ## width one bar gets, and the measured width of each label.
        ## Dense bars get rotated labels, then a smaller font, then only
        ## every n-th label (see cat_label_layout).
        pad <- 0.06                                   # axis-to-label gap, in
        lay <- cat_label_layout(
            spec$labels, slot = par("pin")[1] / k,
            width_of = function(l, cex) strwidth(l, units = "inches", cex = cex),
            line_h = strheight("M", units = "inches", cex = 1) * 1.25,
            max_depth = par("fin")[2] * 0.33)
        if (lay$angle != 0) {
            ## rotated labels need a deeper bottom margin than one line
            mai <- par("mai")
            mai[1] <- max(mai[1], lay$depth + 2 * pad)
            par(mai = mai)
        }
        plot.new()
        plot.window(xlim = c(0, k), ylim = c(0, max(shown)), xaxs = "i", yaxs = "i")
        x0 <- seq_len(k) - 0.9
        x1 <- seq_len(k) - 0.1
        rect(x0, 0, x1, sel, col = cols[1], border = NA)
        rect(x0, sel, x1, shown, col = cols[2], border = NA)
        at <- (seq_len(k) - 0.5)[lay$keep]
        labs <- lay$labels[lay$keep]
        if (lay$angle == 0) {
            mtext(labs, side = 1, at = at, line = 0.4, cex = lay$cex)
        } else {
            ## anchored at the axis, reading up towards their bar
            text(at, -pad * diff(par("usr")[3:4]) / par("pin")[2], labs,
                 srt = lay$angle, cex = lay$cex, xpd = NA,
                 adj = if (lay$angle == 90) c(1, 0.5) else c(1, 1))
        }
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

## Partition rows by ONE variable's filter outcome, within a universe
## of rows (the module passes the leave-one-out set: rows passing every
## OTHER filter).  Returns sorted row-ID vectors:
##   merged (default):    selected / excluded  -- excluded mirrors the
##                        mask exactly (NAs land per keep_na, like the
##                        include-NA checkbox)
##   split_range = TRUE:  selected / below / above / na  -- range
##                        filters only (ignored for membership and
##                        categorical filters); below/above are the
##                        strict outsides (an infinite bound, i.e. a
##                        slider handle at its endpoint, leaves that
##                        side empty); excluded NAs go in `na` because
##                        they have no side (empty when keep_na keeps
##                        them inside `selected`)
## drop_na = TRUE removes rows with NA in x from every stream.
## Invariants: streams are disjoint and their union is the universe
## (minus NA rows when drop_na).
stream_partition <- function(x, val, keep_na = TRUE, universe = NULL,
                             split_range = FALSE, drop_na = FALSE) {
    if (is.null(universe)) universe <- rep(TRUE, length(x))
    if (drop_na) universe <- universe & !is.na(x)
    sel <- universe & make_mask(x, val, keep_na)
    ranged <- split_range && !is.null(val) && !is.character(val)
    if (!ranged) {
        return(list(selected = which(sel), excluded = which(universe & !sel)))
    }
    nn <- universe & !is.na(x)
    list(selected = which(sel),
         below = which(nn & x < val[1]),   # x < -Inf is all-FALSE
         above = which(nn & x > val[2]),
         na = if (keep_na) integer(0) else which(universe & is.na(x)))
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
