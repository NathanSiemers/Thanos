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

cat("\nall plot-helper tests passed\n")
