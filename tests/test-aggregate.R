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
invisible(lapply(list.files(file.path(root, "R"), pattern = "[.]R$",
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
spec_a <- bin_spec_from_info(be$get_column_info("a"), bins = 25)
bin_a  <- bin_column(df$a, bins = 25)
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

## whole-module equivalence: aggregate vs vector mode, same inputs
run_module <- function(backend, mode) {
    out <- new.env()
    testServer(thanosServer,
               args = list(backend = backend, debounce_ms = 0, mode = mode), {
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

be$disconnect()
cat("\nall aggregate-mode tests passed\n")
