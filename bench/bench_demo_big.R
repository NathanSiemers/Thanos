################################################################
## Step-by-step diagnosis of demo_big (taxi.duckdb, aggregate mode):
## reproduce the EXACT query cascade thanosServer issues for each user
## action and time every step, cold and warm.
##
## The module's per-interaction work with k plots:
##   for each var v:  get_binned_pair(v, spec_v, loo_f = filters minus v,
##                                    own = filters[v])   [1 query]
##                    get_count(loo_f)                    [1 query]
##   plus one shared  get_count(all filters)              [1 query]
##   => 2k + 1 queries, then k base-engine plot renders.
##
## Crucially, filtersNow() includes an entry for EVERY selected var,
## even a no-op one (full-range slider = c(-Inf, Inf), all boxes
## ticked).  No-ops add no SQL clause -- but they DO change the memo
## key, so adding an unfiltered column cold-recomputes every plot.
## The 'normalized' rows below show what canonical keys would give.
##
## Run from the repo root:  Rscript bench/bench_demo_big.R
################################################################
suppressPackageStartupMessages(library(ggplot2))
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_backend.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
                            full.names = TRUE), source))
## the app may hold a write lock on the canonical file; allow overriding
## the path:  Rscript bench/bench_demo_big.R /path/to/copy.duckdb
db_path <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(db_path)) db_path <- file.path(root, "db", "data", "taxi.duckdb")
be <- backend_duckdb(db_path)

VARS <- c("trip_distance", "fare_amount", "tip_amount", "payment_type")

specs <- lapply(setNames(VARS, VARS), function(v) {
    info <- be$get_column_info(v)
    discrete <- info$is_numeric && !is.null(info$values) &&
        (info$n_unique %||% Inf) <= 12
    bin_spec_from_info(info, 50, discrete = discrete)
})

## a filter entry exactly as filtersNow() holds it after the widget
## reports: full ranges arrive endpoint-expanded to +/-Inf, categoricals
## arrive as the full level set
noop_entry <- function(v) {
    info <- be$get_column_info(v)
    if (info$is_numeric && is.null(specs[[v]]$labels)) {
        list(is_numeric = TRUE, val = c(-Inf, Inf), include_na = TRUE)
    } else if (info$is_numeric) {
        list(is_numeric = TRUE, val = as.character(info$values),
             include_na = TRUE)
    } else {
        list(is_numeric = FALSE, val = info$levels, include_na = TRUE)
    }
}
state_for <- function(vars, overrides = list()) {
    st <- lapply(setNames(vars, vars), noop_entry)
    for (v in names(overrides)) st[[v]] <- overrides[[v]]
    st
}

## strip entries that contribute no SQL clause (the proposed fix)
normalize <- function(st) {
    keep <- vapply(names(st), function(v) {
        f <- st[[v]]
        if (!isTRUE(f$include_na)) return(TRUE)
        if (is.null(f$val)) return(FALSE)
        if (f$is_numeric && !is.character(f$val)) return(any(is.finite(f$val)))
        info <- be$get_column_info(v)
        all_levs <- if (f$is_numeric) as.character(info$values) else info$levels
        !setequal(f$val, all_levs)
    }, NA)
    st[keep]
}

## one full module interaction over filter state `st` for plots `vars`.
## canonical = FALSE replays the PRE-FIX module (every entry in every
## key, a loo count per plot); canonical = TRUE replays the module
## SINCE the fix: normalized keys, and the loo count skipped for plots
## whose own filter is inactive (their n_shown is the shared global)
cascade <- function(vars, st, canonical = FALSE) {
    if (canonical) st <- normalize(st)
    for (v in vars) {
        loo <- st[setdiff(names(st), v)]
        be$get_binned_pair(v, specs[[v]], loo, st[[v]])
        if (!canonical || !is.null(st[[v]])) be$get_count(loo)
    }
    be$get_count(st)
    invisible()
}

step <- function(label, expr) {
    t <- system.time(expr)[["elapsed"]]
    cat(sprintf("%-58s %8.0f ms\n", label, t * 1000))
}

cat(sprintf("taxi.duckdb: %s rows; demo_big defaults: %s\n\n",
            format(be$n_rows(), big.mark = ","),
            paste(VARS, collapse = ", ")))

## ---- (startup) first render of 4 panels, everything cold ----
s0 <- state_for(VARS)
step("startup: first render cascade, 4 plots, cold", cascade(VARS, s0))

## ---- (a) one slider move: fare_amount -> [5, 60] ----
s1 <- state_for(VARS, list(fare_amount = list(
    is_numeric = TRUE, val = c(5, 60), include_na = TRUE)))
step("slider move: cascade, 4 plots, cold", cascade(VARS, s1))
step("slider move again (same state): warm memo", cascade(VARS, s1))
be$clear_cache()
step("slider move, NORMALIZED keys, cold", cascade(VARS, s1, canonical = TRUE))
step("slider move, NORMALIZED keys, warm", cascade(VARS, s1, canonical = TRUE))

## ---- (b) add an unfiltered 5th column on top of s1 ----
VARS5 <- c(VARS, "tolls_amount")
specs$tolls_amount <- bin_spec_from_info(be$get_column_info("tolls_amount"), 50)
s2 <- state_for(VARS5, list(fare_amount = list(
    is_numeric = TRUE, val = c(5, 60), include_na = TRUE)))
step("add unfiltered column: cascade, 5 plots, AS TODAY",
     cascade(VARS5, s2))
be$clear_cache()
cascade(VARS, s1, canonical = TRUE)          # prior state, canonical, warm
step("add unfiltered column: cascade, NORMALIZED keys",
     cascade(VARS5, s2, canonical = TRUE))

## ---- (c) rapid additions: 5 -> 8 columns, each triggering a cascade ----
MORE <- c("total_amount", "trip_minutes", "passenger_count")
for (m in MORE) {
    specs[[m]] <- {
        info <- be$get_column_info(m)
        discrete <- info$is_numeric && !is.null(info$values) &&
            (info$n_unique %||% Inf) <= 12
        bin_spec_from_info(info, 50, discrete = discrete)
    }
}
be$clear_cache()
t <- system.time({
    vs <- VARS5
    for (m in MORE) {
        vs <- c(vs, m)
        cascade(vs, state_for(vs, list(fare_amount = list(
            is_numeric = TRUE, val = c(5, 60), include_na = TRUE))))
    }
})[["elapsed"]]
cat(sprintf("%-58s %8.0f ms\n", "3 rapid additions (6,7,8 cols), AS TODAY", t * 1000))
be$clear_cache()
vs <- VARS5
cascade(vs, s2, canonical = TRUE)            # warm the canonical state
t <- system.time({
    for (m in MORE) {
        vs <- c(vs, m)
        cascade(vs, state_for(vs, list(fare_amount = list(
            is_numeric = TRUE, val = c(5, 60), include_na = TRUE))),
            canonical = TRUE)
    }
})[["elapsed"]]
cat(sprintf("%-58s %8.0f ms\n", "3 rapid additions, NORMALIZED keys", t * 1000))

## ---- plot rendering share (base engine, 4 panels) ----
pair <- be$get_binned_pair("trip_distance", specs$trip_distance,
                           normalize(s1)["fare_amount"], s1$trip_distance)
t <- system.time(for (i in 1:5) {
    f <- tempfile(fileext = ".png")
    ragg::agg_png(f, width = 600, height = 150)
    for (j in 1:4) {
        plot_histo_counts_base(specs$trip_distance, pair$shown, pair$sel,
                               1e6, 5e5, "trip_distance")
    }
    dev.off(); unlink(f)
})[["elapsed"]]
cat(sprintf("%-58s %8.0f ms\n", "render 4 plots, base engine + ragg", t * 1000 / 5))

be$disconnect()
