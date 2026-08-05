################################################################
## Backend caching: correctness, hit/miss accounting, eviction,
## clear_cache, and the config-off path.
## Run from the repo root:  Rscript tests/test-cache.R
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

set.seed(7)
df <- data.frame(
    a = runif(2000, 0, 100),
    g = sample(c("p", "q", "r", NA), 2000, replace = TRUE),
    stringsAsFactors = FALSE
)
db_path <- tempfile(fileext = ".sqlite")
build_tall_skinny(df, db_path)

be   <- backend_sqlite(db_path)                 # cache ON (default)
raw  <- backend_sqlite(db_path, cache = FALSE)  # reference, no cache

## ---- column cache ----
x1 <- be$get_column("a")
s <- be$cache_stats()
check("first fetch is a miss, column cached",
      s$misses == 1 && s$hits == 0 && s$columns_cached == 1)
x2 <- be$get_column("a")
s <- be$cache_stats()
check("second fetch is a hit, no extra miss",
      s$hits == 1 && s$misses == 1)
check("cached column identical to a fresh uncached fetch",
      identical(x2, raw$get_column("a")) && identical(x1, x2))

## ---- aggregate memo ----
FL <- list(a = list(is_numeric = TRUE, val = c(20, 80), include_na = TRUE))
c1 <- be$get_count(FL)
c2 <- be$get_count(FL)
s <- be$cache_stats()
check("repeated count with same filters hits the memo",
      s$hits == 2 && c1 == c2)
check("count agrees with the uncached backend", c1 == raw$get_count(FL))

FL2 <- list(a = list(is_numeric = TRUE, val = c(10, 90), include_na = TRUE))
check("different filters miss and compute correctly",
      be$get_count(FL2) == raw$get_count(FL2))

info_a <- be$get_column_info("a")
spec <- bin_spec_from_info(info_a, bins = 20)
p1 <- be$get_binned_pair("a", spec, list(), FL$a)
p2 <- be$get_binned_pair("a", spec, list(), FL$a)
check("binned pair memoised and identical across calls",
      identical(p1, p2) &&
      identical(p1, raw$get_binned_pair("a", spec, list(), FL$a)))
m1 <- be$get_row_mask(FL)
check("row mask memoised and correct",
      identical(m1, be$get_row_mask(FL)) &&
      identical(m1, raw$get_row_mask(FL)))

## ---- cache = FALSE never caches ----
s_raw <- raw$cache_stats()
check("uncached backend reports cache disabled and no entries",
      isFALSE(s_raw$enabled) && s_raw$memo_entries == 0 &&
      s_raw$columns_cached == 0 && s_raw$hits == 0)

## ---- eviction keeps the memo bounded and correct ----
tiny <- backend_sqlite(db_path, cache_max_entries = 3)
counts <- vapply(1:5, function(i) {
    tiny$get_count(list(a = list(is_numeric = TRUE, val = c(i, 100 - i),
                                 include_na = TRUE)))
}, numeric(1))
s <- tiny$cache_stats()
check("memo never exceeds cache_max_entries", s$memo_entries <= 3)
check("evicted states still recompute correctly",
      tiny$get_count(list(a = list(is_numeric = TRUE, val = c(1, 99),
                                   include_na = TRUE))) == counts[1])
tiny$disconnect()

## ---- clear_cache ----
be$clear_cache()
s <- be$cache_stats()
check("clear_cache empties everything",
      s$columns_cached == 0 && s$memo_entries == 0 &&
      s$hits == 0 && s$misses == 0)
check("results remain correct after clearing",
      identical(be$get_column("a"), raw$get_column("a")) &&
      be$get_count(FL) == c1)

## ---- module level: deselect -> re-add rides the column cache ----
testServer(thanosServer, args = list(backend = be, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = "a")
    session$setInputs(filter_a = c(20, 80))
    n1 <- session$returned$n_selected()
    session$setInputs(vars = character(0))   # deselect
    session$setInputs(vars = "a")            # re-add: hits column cache
    session$setInputs(filter_a = c(20, 80))
    check("re-added variable filters correctly through the cache",
          session$returned$n_selected() == n1)
})
s <- be$cache_stats()
check("module re-add produced cache hits", s$hits > 0)

be$disconnect()
raw$disconnect()
cat("\nall cache tests passed\n")
