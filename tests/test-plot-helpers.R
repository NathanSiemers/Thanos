################################################################
## Unit tests for the pure helpers in R/thanos_plot.R.
## Run from the repo root:  Rscript tests/test-plot-helpers.R
################################################################
suppressPackageStartupMessages(library(ggplot2))
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_plot.R")),
               c(".", ".."))[1]
source(file.path(root, "R", "thanos_theme.R"))
source(file.path(root, "R", "thanos_plot.R"))

check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

## ---- make_mask: numeric ----
x <- c(1, 5, NA, 10, 3)
check("numeric range keeps in-range + NA by default",
      identical(make_mask(x, c(2, 9)), c(FALSE, TRUE, TRUE, FALSE, TRUE)))
check("numeric range drops NA when include_na = FALSE",
      identical(make_mask(x, c(2, 9), include_na = FALSE),
                c(FALSE, TRUE, FALSE, FALSE, TRUE)))
check("NULL filter passes everything",
      identical(make_mask(x, NULL), rep(TRUE, 5)))
check("NULL filter + exclude NA drops only NA rows",
      identical(make_mask(x, NULL, include_na = FALSE),
                c(TRUE, TRUE, FALSE, TRUE, TRUE)))

## ---- make_mask: categorical ----
y <- c("a", "b", NA, "c", "a")
check("categorical keeps chosen levels + NA by default",
      identical(make_mask(y, c("a")), c(TRUE, FALSE, TRUE, FALSE, TRUE)))
check("categorical empty selection keeps only NA (include_na = TRUE)",
      identical(make_mask(y, character(0)), c(FALSE, FALSE, TRUE, FALSE, FALSE)))
check("categorical empty selection + exclude NA keeps nothing",
      identical(make_mask(y, character(0), include_na = FALSE), rep(FALSE, 5)))

## ---- make_mask: discrete numeric (character val = membership) ----
xm <- c(1, 2, 3, NA, 2)
check("character val on numeric column means membership, NA kept",
      identical(make_mask(xm, c("2", "3")), c(FALSE, TRUE, TRUE, TRUE, TRUE)))
check("membership + exclude NA",
      identical(make_mask(xm, c("2", "3"), include_na = FALSE),
                c(FALSE, TRUE, TRUE, FALSE, TRUE)))

## ---- bin_column: discrete numeric ----
bd <- bin_column(c(2, 1, NA, 2), discrete_values = c(1, 2))
check("discrete numeric bins one bar per value, NA unbinned",
      identical(bd$labels, c("1", "2")) &&
      identical(bd$idx, c(2L, 1L, NA_integer_, 2L)) && bd$kind == "cat")

## ---- bin_column: numeric ----
b <- bin_column(c(0, 25, 50, 75, 100), bins = 4)
check("numeric binning spans the range", b$nbins == 4)
check("numeric bin indices are within 1..nbins",
      all(b$idx >= 1 & b$idx <= 4))
check("min lands in first bin, max in last",
      b$idx[1] == 1 && b$idx[5] == 4)

b_na <- bin_column(c(NA_real_, NA_real_), bins = 10)
check("all-NA numeric column yields all-NA indices",
      all(is.na(b_na$idx)) && b_na$nbins >= 1)

b_const <- bin_column(rep(7, 5), bins = 10)
check("constant column bins without error",
      all(!is.na(b_const$idx)) && b_const$nbins == 10)

b_inf <- bin_column(c(1, 2, Inf, NA), bins = 5)
check("Inf and NA excluded from histogram indices",
      is.na(b_inf$idx[3]) && is.na(b_inf$idx[4]) && !any(is.na(b_inf$idx[1:2])))

## ---- bin_column: categorical ----
bc <- bin_column(c("b", "a", NA, "b"))
check("categorical levels sorted, NA unbinned",
      identical(bc$labels, c("a", "b")) &&
      identical(bc$idx, c(2L, 1L, NA_integer_, 2L)))

## ---- tabulate counts match a direct computation ----
set.seed(42)
z <- c(rnorm(1000), NA, NA)
bz <- bin_column(z, bins = 20)
loo <- rep(TRUE, length(z))
own <- make_mask(z, c(-1, 1))
check("binned counts total the non-NA rows passing loo",
      sum(tabulate(bz$idx[loo], nbins = bz$nbins)) == 1000)
check("selected binned counts total non-NA rows passing both masks",
      sum(tabulate(bz$idx[own & loo], nbins = bz$nbins)) ==
          sum(!is.na(z) & z >= -1 & z <= 1))

## ---- plot_histo builds without error ----
p1 <- plot_histo(bz, loo, own, "z")
check("numeric plot is a ggplot", inherits(p1, "ggplot"))
p2 <- plot_histo(bc, rep(TRUE, 4), c(TRUE, FALSE, TRUE, TRUE), "cat")
check("categorical plot is a ggplot", inherits(p2, "ggplot"))
p3 <- plot_histo(bin_column(character(0)), logical(0), logical(0), "empty")
check("empty column plot is a ggplot", inherits(p3, "ggplot"))

## ---- base-graphics engine draws without error ----
tmp_png <- tempfile(fileext = ".png")
png(tmp_png, width = 400, height = 150)
plot_histo_counts_base(bz, tabulate(bz$idx[loo], bz$nbins),
                       tabulate(bz$idx[own & loo], bz$nbins),
                       sum(loo), sum(own & loo), "z")
plot_histo_counts_base(bc, c(2L, 2L), c(1L, 2L), 4, 3, "cat")
plot_histo_counts_base(bin_column(character(0)), integer(0), integer(0),
                       0, 0, "empty")
dev.off()
check("base engine renders numeric, categorical, and empty specs",
      file.exists(tmp_png))
unlink(tmp_png)

## ---- category label layout: labels must never run into each other ----
## a fixed-pitch "device": every character is 0.1in wide, a line 0.15in
w_of <- function(l, cex) nchar(l) * 0.1 * cex
lay <- function(labels, slot, max_depth = 0.7) {
    cat_label_layout(labels, slot = slot, width_of = w_of, line_h = 0.15,
                     max_depth = max_depth)
}
fits <- function(L, slot) {        # no two drawn labels can collide
    step <- if (length(L$keep) > 1) diff(L$keep)[1] else 1
    if (L$angle == 0) max(w_of(L$labels[L$keep], L$cex)) <= slot * step
    else 0.15 * L$cex <= slot * step * sin(L$angle * pi / 180) + 1e-9
}
few <- c("breast", "colon", "lung", "pancreas", "skin")
L <- lay(few, slot = 1)
check("roomy bars: full labels, horizontal, full size",
      L$angle == 0 && L$cex == 1 && identical(L$labels, few) &&
      identical(L$keep, 1:5))
long <- c("Additional - New Primary", "Metastatic", "Primary Tumor",
          "Solid Tissue Normal")
L <- lay(long, slot = 0.9)
check("long labels that fit abbreviated stay horizontal",
      L$angle == 0 && all(nchar(L$labels) <= 9) && fits(L, 0.9))
codes <- sprintf("C%03d", 1:34)                    # 34 cohort-like codes
L <- lay(codes, slot = 0.16)
check("dense bars: labels are rotated, all kept, none collide",
      L$angle %in% c(60, 90) && length(L$keep) == 34 && fits(L, 0.16))
L60 <- lay(codes, slot = 0.25)
check("moderately dense: a 60-degree slant when slanted lines clear",
      L60$angle == 60 && L60$cex == 1 && fits(L60, 0.25))
L <- lay(sprintf("C%03d", 1:80), slot = 0.10)
check("very dense: vertical with a font shrunk to just clear",
      L$angle == 90 && L$cex < 1 && L$cex >= 0.5 && length(L$keep) == 80 &&
      fits(L, 0.10))
L <- lay(sprintf("C%03d", 1:400), slot = 0.02)
check("extreme: font floor reached, only every n-th bar labelled",
      L$angle == 90 && L$cex == 0.5 && length(L$keep) < 400 &&
      L$keep[1] == 1 && fits(L, 0.02))
wordy <- sprintf("a rather long category name number %d", 1:40)
L <- lay(wordy, slot = 0.16, max_depth = 0.6)
check("rotated labels respect the depth budget (abbreviate, then shrink)",
      L$angle == 90 && L$depth <= 0.6 + 1e-9 && L$cex >= 0.5 &&
      !anyDuplicated(L$labels) && fits(L, 0.16))
L <- lay(wordy, slot = 0.16, max_depth = 0.2)
check("...and are cut as a last resort, never overflowing",
      L$depth <= 0.2 + 1e-9 && L$cex == 0.5 && all(nchar(L$labels) >= 1))
check("no labels at all is handled",
      length(lay(character(0), slot = 1)$keep) == 0)

## both engines draw dense and extreme category sets without error
tmp_png <- tempfile(fileext = ".png")
png(tmp_png, width = 400, height = 150)
for (k in c(34, 126, 400)) {
    sp <- list(kind = "cat", labels = sprintf("level-%03d", seq_len(k)), nbins = k)
    plot_histo_counts_base(sp, rep(3L, k), rep(1L, k), 3 * k, k, "dense")
    stopifnot(inherits(plot_histo_counts(sp, rep(3L, k), rep(1L, k), 3 * k, k,
                                         "dense"), "ggplot"))
}
dev.off()
unlink(tmp_png)
check("dense category histograms render in both engines", TRUE)

cat("\nall plot-helper tests passed\n")
