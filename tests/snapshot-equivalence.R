################################################################
## Behavioral snapshot for refactoring safely: captures the outputs of
## every layer (pure helpers, backends, whole module, plot data) on
## fixed inputs, and compares a later run against a saved snapshot.
##
##   Rscript tests/snapshot-equivalence.R capture  path.rds
##   Rscript tests/snapshot-equivalence.R compare  path.rds
##
## compare exits non-zero and names the first differing element if the
## refactored code produces ANY different result.
################################################################
suppressPackageStartupMessages({
    library(shiny)
    library(ggplot2)
    library(DBI)
    library(RSQLite)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_module.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
                            full.names = TRUE), source))
source(file.path(root, "db", "build_flights_sqlite.R"))

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]
path <- args[2]
stopifnot(mode %in% c("capture", "compare"), !is.na(path))

snap <- list()

## ---------- layer 1: pure helpers ----------
x_num <- c(1, 5, NA, 10, 3, -2, 0, 7.5, NA, 100)
x_chr <- c("a", "b", NA, "c", "a", "b", NA, "a", "c", "b")
snap$mask <- list(
    rng      = make_mask(x_num, c(2, 9)),
    rng_noNA = make_mask(x_num, c(2, 9), include_na = FALSE),
    open     = make_mask(x_num, c(-Inf, 8)),
    none     = make_mask(x_num, NULL),
    none_no  = make_mask(x_num, NULL, include_na = FALSE),
    member   = make_mask(x_num, c("5", "10")),
    chr      = make_mask(x_chr, c("a", "c")),
    chr_empty = make_mask(x_chr, character(0)),
    chr_noNA = make_mask(x_chr, c("a"), include_na = FALSE)
)
snap$bin <- list(
    num      = bin_column(x_num, 5),
    ranged   = bin_column(x_num, 5, range = c(0, 10)),
    log      = bin_column(abs(x_num), 5, range = c(0, 100), log2p1 = TRUE),
    discrete = bin_column(c(2, 1, NA, 2, 3), discrete_values = c(1, 2, 3)),
    cat      = bin_column(x_chr),
    all_na   = bin_column(c(NA_real_, NA_real_)),
    const    = bin_column(rep(7, 4), 10)
)
info_n <- list(name = "n", is_numeric = TRUE, n_na = 2, range = c(-2, 100),
               is_integerish = FALSE, n_unique = 8, values = NULL,
               q_low = 0, q_high = 50)
info_c <- list(name = "c", is_numeric = FALSE, n_na = 2,
               levels = c("a", "b", "c"))
snap$spec <- list(
    num  = bin_spec_from_info(info_n, 10),
    log  = bin_spec_from_info(info_n, 10, log2p1 = TRUE),
    cat  = bin_spec_from_info(info_c),
    disc = bin_spec_from_info(list(is_numeric = TRUE, values = c(1, 5, 9)),
                              discrete = TRUE)
)
snap$display_range <- list(display_range(info_n),
                           display_range(list(is_numeric = TRUE,
                                              range = c(1, 12),
                                              is_integerish = TRUE,
                                              q_low = 1.02, q_high = 11.9)))
snap$slider <- list(slider_bounds(info_n),
                    slider_bounds(info_n, log2p1 = TRUE))
infos_nf <- list(a = list(is_numeric = TRUE),
                 g = list(is_numeric = FALSE, levels = c("x", "y")))
snap$normalize <- list(
    normalize_filters(list(
        a = list(is_numeric = TRUE, val = c(-Inf, Inf), include_na = TRUE),
        g = list(is_numeric = FALSE, val = c("x", "y"), include_na = TRUE)),
        infos_nf),
    normalize_filters(list(
        a = list(is_numeric = TRUE, val = c(0, Inf), include_na = TRUE),
        g = list(is_numeric = FALSE, val = c("x", "y"), include_na = FALSE)),
        infos_nf)
)

## ---------- layer 2: backends on a fixed fixture ----------
set.seed(42)
df <- data.frame(
    a = c(runif(497, 0, 100), 5000, -50, NA),          # outliers + NA
    b = ifelse(runif(500) < 0.15, NA, rnorm(500)),
    g = sample(c("p", "q", "r", NA), 500, TRUE, prob = c(.4, .3, .2, .1)),
    m = ifelse(runif(500) < 0.05, NA, sample(1:6, 500, TRUE)),
    f = factor(sample(c("lo", "hi"), 500, TRUE)),
    l = sample(c(TRUE, FALSE, NA), 500, TRUE),
    d = as.Date("2024-01-01") + sample(c(0:30, NA), 500, TRUE),
    p = ifelse(runif(500) < 0.1, NA, runif(500, 0, 50)),  # non-negative, for log2
    stringsAsFactors = FALSE
)
db_path <- tempfile(fileext = ".sqlite")
build_tall_skinny(df, db_path)
mem <- backend_memory(df)
sql <- backend_sqlite(db_path)

snap$columns <- lapply(setNames(mem$get_columns(), mem$get_columns()),
                       mem$get_column)
snap$columns_sql <- lapply(setNames(sql$get_columns(), sql$get_columns()),
                           sql$get_column)
snap$infos <- lapply(setNames(mem$get_columns(), mem$get_columns()),
                     mem$get_column_info)
snap$infos_sql <- lapply(setNames(sql$get_columns(), sql$get_columns()),
                         sql$get_column_info)

FILTERS <- list(
    one  = list(a = list(is_numeric = TRUE, val = c(20, 80),
                         include_na = TRUE)),
    mix  = list(a = list(is_numeric = TRUE, val = c(20, Inf),
                         include_na = TRUE),
                g = list(is_numeric = FALSE, val = c("p", "q"),
                         include_na = FALSE),
                m = list(is_numeric = TRUE, val = c("2", "5"),
                         include_na = TRUE)),
    noNA = list(b = list(is_numeric = TRUE, val = NULL, include_na = FALSE)),
    none = list()
)
spec_a <- bin_spec_from_info(sql$get_column_info("a"), bins = 12)
spec_g <- bin_spec_from_info(sql$get_column_info("g"))
snap$agg <- lapply(FILTERS, function(fl) list(
    count  = sql$get_count(fl),
    mask   = sql$get_row_mask(fl),
    bin_a  = sql$get_binned("a", spec_a, fl),
    bin_g  = sql$get_binned("g", spec_g, fl),
    pair_a = sql$get_binned_pair("a", spec_a, fl,
                                 list(is_numeric = TRUE, val = c(30, 60),
                                      include_na = TRUE)),
    pair_g = sql$get_binned_pair("g", spec_g, fl,
                                 list(is_numeric = FALSE, val = "p",
                                      include_na = TRUE))
))
if (requireNamespace("duckdb", quietly = TRUE)) {
    scon <- dbConnect(RSQLite::SQLite(), db_path)
    dcon <- dbConnect(duckdb::duckdb())
    dbWriteTable(dcon, "long_data", dbReadTable(scon, "long_data"))
    dbWriteTable(dcon, "column_registry", dbReadTable(scon, "column_registry"))
    dbDisconnect(scon)
    duck <- backend_dbi(dcon)
    snap$agg_duck <- lapply(FILTERS, function(fl) list(
        count = duck$get_count(fl),
        bin_a = duck$get_binned("a", spec_a, fl),
        log_p = if (isTRUE(duck$supports_log2)) {
            duck$get_binned("p",
                bin_spec_from_info(duck$get_column_info("p"), 12,
                                   log2p1 = TRUE), fl)
        }
    ))
    duck$disconnect()
}

## ---------- layer 3: the whole module, scripted interactions ----------
run_module <- function(backend, mode) {
    out <- list()
    testServer(thanosServer,
               args = list(backend = backend, debounce_ms = 0,
                           debounce_checkbox_ms = 0, mode = mode,
                           max_discrete_numeric = 12), {
        grab <- function(tag) {
            out[[tag]] <<- list(n = session$returned$n_selected(),
                                rows = session$returned$rows(),
                                filters = session$returned$filters(),
                                vars = session$returned$selected_vars())
        }
        session$setInputs(vars = c("a", "g", "m"))
        grab("initial")
        session$setInputs(filter_a = c(20, 80));            grab("slider")
        session$setInputs(filter_g = c("p", "q"));          grab("checkbox")
        session$setInputs(na_g = FALSE);                    grab("na_off")
        session$setInputs(filter_m = c("2", "5"));          grab("discrete")
        session$setInputs(filter_g = NULL);                 grab("uncheck_all")
        session$setInputs(filter_g = c("p", "q", "r"), na_g = TRUE)
        grab("restore")
        session$setInputs(vars = c("a", "m"));              grab("remove_g")
        session$setInputs(vars = c("a", "m", "g"))
        session$setInputs(filter_g = c("p", "q", "r"));     grab("readd_g")
        session$setInputs(log_a = TRUE)
        session$setInputs(filter_a = c(2, 5));              grab("log_range")
    })
    out
}
snap$module_vec_mem <- run_module(mem, "vector")
snap$module_vec_sql <- run_module(sql, "vector")
snap$module_agg_sql <- run_module(backend_sqlite(db_path), "aggregate")

## ---------- layer 4: plot outputs ----------
bin_a4 <- bin_column(df$a, 12, range = display_range(mem$get_column_info("a")))
loo4 <- make_mask(df$g, c("p", "q"))
own4 <- make_mask(df$a, c(20, 80))
shown4 <- tabulate(bin_a4$idx[loo4], bin_a4$nbins)
sel4   <- tabulate(bin_a4$idx[own4 & loo4], bin_a4$nbins)
gg <- plot_histo_counts(bin_a4, shown4, sel4, sum(loo4), sum(own4 & loo4), "a")
snap$plot_gg_data <- ggplot_build(gg)$data
snap$plot_gg_labs <- gg$labels
png_path <- tempfile(fileext = ".png")
ragg::agg_png(png_path, width = 600, height = 150)
plot_histo_counts_base(bin_a4, shown4, sel4, sum(loo4), sum(own4 & loo4), "a")
dev.off()
snap$plot_base_png <- readBin(png_path, "raw", file.size(png_path))
unlink(png_path)

sql$disconnect()

## ---------- capture or compare ----------
if (mode == "capture") {
    saveRDS(snap, path)
    cat(sprintf("captured %d top-level elements -> %s\n", length(snap), path))
} else {
    ref <- readRDS(path)
    fail <- 0
    walk <- function(a, b, where) {
        if (is.list(a) && is.list(b) && !is.data.frame(a)) {
            keys <- union(names(a) %||% seq_along(a), names(b) %||% seq_along(b))
            for (k in keys) walk(a[[k]], b[[k]], paste0(where, "$", k))
        } else {
            same <- isTRUE(all.equal(a, b, check.attributes = TRUE)) ||
                identical(a, b)
            if (!same) {
                fail <<- fail + 1
                cat("DIFFERS:", where, "\n")
            }
        }
    }
    walk(ref, snap, "snap")
    if (fail == 0) {
        cat("EQUIVALENT: all", length(snap), "element trees identical\n")
    } else {
        cat(fail, "differences found\n")
        quit(status = 1)
    }
}
