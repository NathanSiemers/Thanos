################################################################
## Micro-benchmarks for the data path behind one slider drag:
## mask recompute -> leave-one-out combination -> binned counts -> ggplot.
## Run from the repo root:  Rscript bench/bench_masks.R
################################################################
suppressPackageStartupMessages({
    library(ggplot2)
    library(nycflights13)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_plot.R")),
               c(".", ".."))[1]
source(file.path(root, "R", "thanos_theme.R"))
source(file.path(root, "bench", "bench_common.R"))
source(file.path(root, "R", "thanos_plot.R"))


fl <- as.data.frame(nycflights13::flights)
n <- nrow(fl)
cat(sprintf("flights: %s rows\n\n", format(n, big.mark = ",")))

dep_delay <- fl$dep_delay
carrier   <- fl$carrier

timeit(reps = 20, "make_mask numeric (dep_delay in [-10, 60])",
       make_mask(dep_delay, c(-10, 60)))
timeit("make_mask categorical (3 carriers)",
       make_mask(carrier, c("UA", "AA", "DL")))

## leave-one-out combination via prefix/suffix cumulative ANDs
set.seed(1)
masks_for <- function(k) replicate(k, sample(c(TRUE, TRUE, TRUE, FALSE), n,
                                             replace = TRUE), simplify = FALSE)
for (k in c(3, 6, 10)) {
    ms <- masks_for(k)
    timeit(sprintf("leave-one-out combine, k = %2d vars", k), loo_combine(ms))
}

## per-plot work: tabulate on pre-binned indices vs full ggplot build
bin_num <- bin_column(dep_delay, bins = 50)
bin_cat <- bin_column(carrier)
loo <- make_mask(dep_delay, c(-10, 60))
own <- make_mask(carrier, c("UA", "AA", "DL"))

timeit("tabulate counts, numeric 50 bins", {
    tabulate(bin_num$idx[loo], nbins = bin_num$nbins)
    tabulate(bin_num$idx[own & loo], nbins = bin_num$nbins)
})
timeit("plot_histo build (ggplot object), numeric",
       plot_histo(bin_num, loo, own, "dep_delay"))
timeit("plot_histo build (ggplot object), categorical",
       plot_histo(bin_cat, loo, own, "carrier"))
timeit("plot_histo build+render to png, numeric", {
    png(tempfile(fileext = ".png"), width = 600, height = 150)
    print(plot_histo(bin_num, loo, own, "dep_delay"))
    dev.off()
}, reps = 10)

## the old way, for contrast: geom_histogram over all filtered rows
timeit("OLD: geom_histogram over 337k raw rows (build+render)", {
    png(tempfile(fileext = ".png"), width = 600, height = 150)
    fcolor <- factor(ifelse(own[loo], "sel", "unsel"), levels = c("sel", "unsel"))
    print(ggplot() +
          geom_histogram(aes(x = dep_delay[loo], fill = fcolor), bins = 50) +
          scale_fill_thanos() + theme_thanos)
    dev.off()
}, reps = 5)
