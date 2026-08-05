################################################################
## Aggregate mode: SQL-side binned counts, row counts, and masks must
## equal what vector mode computes in R on the same data.
## Run from the repo root:  Rscript tests/test-aggregate.R
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

check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

set.seed(99)
df <- data.frame(
    a = runif(2000, 0, 100),
    b = ifelse(runif(2000) < 0.1, NA, rnorm(2000)),
    g = sample(c("p", "q", "r", NA), 2000, replace = TRUE,
               prob = c(.4, .3, .2, .1)),
    m = ifelse(runif(2000) < 0.05, NA, sample(1:6, 2000, replace = TRUE)),
    stringsAsFactors = FALSE
)
db_path <- tempfile(fileext = ".sqlite")
build_tall_skinny(df, db_path)
be <- backend_sqlite(db_path)

filters <- list(
    a = list(is_numeric = TRUE, val = c(20, 80), include_na = TRUE),
    b = list(is_numeric = TRUE, val = c(-1, 1), include_na = FALSE),
    g = list(is_numeric = FALSE, val = c("p", "q"), include_na = TRUE)
)
## the same masks in R
m_a <- make_mask(df$a, c(20, 80), TRUE)
m_b <- make_mask(df$b, c(-1, 1), FALSE)
m_g <- make_mask(df$g, c("p", "q"), TRUE)
all_m <- m_a & m_b & m_g

check("get_count matches R mask arithmetic",
      be$get_count(filters) == sum(all_m))
check("get_row_mask matches R mask exactly",
      identical(be$get_row_mask(filters), unname(all_m)))
check("empty filter list counts everything",
      be$get_count(list()) == nrow(df))
check("NULL-val filter with include_na=FALSE drops only NAs",
      be$get_count(list(b = list(is_numeric = TRUE, val = NULL,
                                 include_na = FALSE))) == sum(!is.na(df$b)))
check("empty categorical selection keeps only NA rows",
      be$get_count(list(g = list(is_numeric = FALSE, val = character(0),
                                 include_na = TRUE))) == sum(is.na(df$g)))

## binned counts: SQL GROUP BY vs R tabulate over the same fixed breaks
## (both sides use the outlier-robust display range, like the module)
info_a <- be$get_column_info("a")
spec_a <- bin_spec_from_info(info_a, bins = 25)
bin_a  <- bin_column(df$a, bins = 25, range = display_range(info_a))
loo_f  <- filters[c("b", "g")]
loo_m  <- m_b & m_g
check("numeric binned counts match tabulate (leave-one-out set)",
      isTRUE(all.equal(be$get_binned("a", spec_a, loo_f),
                       as.numeric(tabulate(bin_a$idx[loo_m], nbins = 25)))))
check("numeric binned counts match tabulate (all filters)",
      isTRUE(all.equal(be$get_binned("a", spec_a, filters),
                       as.numeric(tabulate(bin_a$idx[all_m], nbins = 25)))))

spec_g <- bin_spec_from_info(be$get_column_info("g"))
bin_g  <- bin_column(df$g)
loo_g_f <- filters[c("a", "b")]
loo_g_m <- m_a & m_b
check("categorical binned counts match tabulate",
      isTRUE(all.equal(be$get_binned("g", spec_g, loo_g_f),
                       as.numeric(tabulate(bin_g$idx[loo_g_m],
                                           nbins = bin_g$nbins)))))

## discrete numeric ('m', 6 unique values): membership semantics in SQL
info_m <- be$get_column_info("m")
check("registry carries n_unique + values for discrete numerics",
      info_m$n_unique == 6 && identical(info_m$values, as.numeric(1:6)))
filters_m <- list(m = list(is_numeric = TRUE, val = c("2", "5"),
                           include_na = TRUE))
m_m <- make_mask(df$m, c("2", "5"), TRUE)
check("discrete numeric membership count matches R",
      be$get_count(filters_m) == sum(m_m))
spec_m <- bin_spec_from_info(info_m, discrete = TRUE)
bin_m  <- bin_column(df$m, discrete_values = info_m$values)
check("discrete numeric binned counts match tabulate",
      isTRUE(all.equal(be$get_binned("m", spec_m, filters["a"]),
                       as.numeric(tabulate(bin_m$idx[m_a],
                                           nbins = bin_m$nbins)))))

## combined shown+sel query equals two independent computations
pair <- be$get_binned_pair("a", spec_a, loo_f,
                           list(val = c(20, 80), include_na = TRUE))
check("get_binned_pair shown matches loo tabulate",
      isTRUE(all.equal(pair$shown,
                       as.numeric(tabulate(bin_a$idx[loo_m], nbins = 25)))))
check("get_binned_pair sel matches all-filters tabulate",
      isTRUE(all.equal(pair$sel,
                       as.numeric(tabulate(bin_a$idx[all_m], nbins = 25)))))

## slider endpoints mean unbounded: infinite bounds drop the condition
check("infinite bounds impose no numeric filter",
      be$get_count(list(a = list(is_numeric = TRUE, val = c(-Inf, Inf),
                                 include_na = TRUE))) == nrow(df))
check("half-open range works (a >= 50, no upper bound)",
      be$get_count(list(a = list(is_numeric = TRUE, val = c(50, Inf),
                                 include_na = TRUE))) == sum(df$a >= 50))

## whole-module equivalence: aggregate vs vector mode, same inputs
run_module <- function(backend, mode) {
    out <- new.env()
    testServer(thanosServer,
               args = list(backend = backend, debounce_ms = 0, debounce_checkbox_ms = 0, mode = mode), {
        session$setInputs(vars = c("a", "g"))
        session$setInputs(filter_a = c(20, 80), filter_g = c("p", "q"))
        out$n <- session$returned$n_selected()
        out$rows <- session$returned$rows()
    })
    out
}
agg <- run_module(be, "aggregate")
vec <- run_module(backend_memory(df), "vector")
check("module n_selected identical in aggregate and vector modes",
      agg$n == vec$n)
check("module rows() identical in aggregate and vector modes",
      identical(agg$rows, vec$rows))

## normalization end to end: full-range / full-set inputs behave as
## "no filter" -- n_selected is the whole table, and rows() needs no
## clauses (would equal a filterless mask)
testServer(thanosServer,
           args = list(backend = be, debounce_ms = 0,
                       debounce_checkbox_ms = 0, mode = "aggregate"), {
    session$setInputs(vars = c("a", "g"))
    session$setInputs(filter_a = c(-Inf, Inf), filter_g = c("p", "q", "r"))
    check("no-op filters normalize to 'everything passes' (aggregate)",
          session$returned$n_selected() == nrow(df) &&
          all(session$returned$mask()))
})

## log2(x+1) SQL binning: RSQLite lacks log2(), so this path is verified
## against DuckDB over the same tall/skinny fixture
check("sqlite backend reports missing log2 support",
      isFALSE(be$supports_log2))
if (requireNamespace("duckdb", quietly = TRUE)) {
    scon <- dbConnect(RSQLite::SQLite(), db_path)
    dcon <- dbConnect(duckdb::duckdb())
    dbWriteTable(dcon, "long_data", dbReadTable(scon, "long_data"))
    dbWriteTable(dcon, "column_registry", dbReadTable(scon, "column_registry"))
    dbDisconnect(scon)
    bd <- backend_dbi(dcon)
    check("duckdb backend reports log2 support", isTRUE(bd$supports_log2))
    spec_log <- bin_spec_from_info(info_a, bins = 25, log2p1 = TRUE)
    bin_log  <- bin_column(df$a, bins = 25,
                           range = display_range(info_a), log2p1 = TRUE)
    check("log2-transformed binned counts match tabulate (duckdb)",
          isTRUE(all.equal(bd$get_binned("a", spec_log, loo_f),
                           as.numeric(tabulate(bin_log$idx[loo_m],
                                               nbins = 25)))))
    bd$disconnect()
}
be$disconnect()
cat("\nall aggregate-mode tests passed\n")
