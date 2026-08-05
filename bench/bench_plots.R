################################################################
## Whole-interaction plot benchmark: the FULL cost of one slider drag
## with k columns selected -- recompute one mask, re-combine the
## leave-one-out masks, and re-render ALL k histograms to a raster
## device (which is what Shiny's renderPlot does per plot).
##
## Compares the two plot engines x two raster devices:
##   engine ggplot  plot_histo_counts()      (ggplot2/grid pipeline)
##   engine base    plot_histo_counts_base() (direct base graphics)
##   device png     grDevices::png
##   device ragg    ragg::agg_png (if installed)
##
## Run from the repo root:  Rscript bench/bench_plots.R
################################################################
suppressPackageStartupMessages({
    library(ggplot2)
    library(nycflights13)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_plot.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
                            full.names = TRUE), source))
source(file.path(root, "bench", "bench_common.R"))

fl <- as.data.frame(nycflights13::flights)
n <- nrow(fl)

## the demo_flights default selection: 8 columns, mixed types
VARS <- c("carrier", "origin", "dest", "dep_delay", "arr_delay",
          "distance", "month", "hour")
be <- backend_memory(fl)
cols <- lapply(setNames(VARS, VARS), be$get_column)
bins <- lapply(VARS, function(v) {
    info <- be$get_column_info(v)
    discrete <- info$is_numeric && !is.null(info$values) &&
        (info$n_unique %||% Inf) <= 12
    bin_column(cols[[v]], 50,
               discrete_values = if (discrete) info$values,
               range = if (info$is_numeric && !discrete) display_range(info))
})
names(bins) <- VARS

## steady-state masks: everything passes except the two filters below
masks <- lapply(setNames(VARS, VARS), function(v) rep(TRUE, n))
masks$distance <- make_mask(cols$distance, c(200, 1500))


## one full interaction: dep_delay slider moved -> its mask recomputes,
## loo masks recombine, all k plots re-render to `device`
interaction_once <- function(engine, device) {
    masks$dep_delay <<- make_mask(cols$dep_delay, c(-10, 30))
    loo <- loo_combine(masks)
    for (v in VARS) {
        f <- tempfile(fileext = ".png")
        device(f, width = 600, height = 150)
        bin <- bins[[v]]
        shown <- tabulate(bin$idx[loo[[v]]], nbins = bin$nbins)
        sel   <- tabulate(bin$idx[masks[[v]] & loo[[v]]], nbins = bin$nbins)
        if (engine == "base") {
            plot_histo_counts_base(bin, shown, sel,
                                   sum(loo[[v]]), sum(masks[[v]] & loo[[v]]), v)
        } else {
            print(plot_histo_counts(bin, shown, sel,
                                    sum(loo[[v]]), sum(masks[[v]] & loo[[v]]), v))
        }
        dev.off()
        unlink(f)
    }
}

devices <- list(png = function(f, width, height) {
    grDevices::png(f, width = width, height = height)
})
if (requireNamespace("ragg", quietly = TRUE)) {
    devices$ragg <- function(f, width, height) {
        ragg::agg_png(f, width = width, height = height)
    }
}

cat(sprintf("flights: %s rows, %d columns selected -> one interaction = 1 mask + loo + %d rendered plots\n\n",
            format(n, big.mark = ","), length(VARS), length(VARS)))
for (engine in c("ggplot", "base")) {
    for (dname in names(devices)) {
        interaction_once(engine, devices[[dname]])  # warm-up
        reps <- 5
        t <- system.time(for (i in seq_len(reps)) {
            interaction_once(engine, devices[[dname]])
        })
        ms <- 1000 * t[["elapsed"]] / reps
        cat(sprintf("engine %-7s device %-5s  %8.0f ms / interaction  (%.0f ms per plot)\n",
                    engine, dname, ms, ms / length(VARS)))
    }
}
