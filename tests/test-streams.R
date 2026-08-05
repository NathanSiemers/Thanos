################################################################
## th$streams(): leave-one-out row streams.  Pure partition semantics,
## module behavior in vector mode, and vector/aggregate equivalence.
## Run from the repo root:  Rscript tests/test-streams.R
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

## ---- stream_partition: pure semantics ----
x <- c(1, 5, NA, 10, 3, 20, NA, 7)
uni <- c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)  # row 5 fails "other" filters

s <- stream_partition(x, c(4, 9), keep_na = TRUE, universe = uni)
check("merged: selected has in-range + NAs (checkbox on), within universe",
      identical(s$selected, c(2L, 3L, 7L, 8L)) &&
      identical(s$excluded, c(1L, 4L, 6L)))
check("merged streams partition the universe",
      setequal(c(s$selected, s$excluded), which(uni)) &&
      length(intersect(s$selected, s$excluded)) == 0)

s <- stream_partition(x, c(4, 9), keep_na = FALSE, universe = uni)
check("merged: checkbox off moves NAs into excluded",
      identical(s$selected, c(2L, 8L)) &&
      setequal(s$excluded, c(1L, 3L, 4L, 6L, 7L)))

s <- stream_partition(x, c(4, 9), keep_na = TRUE, universe = uni,
                      split_range = TRUE)
check("split: below/above are the strict sides, NAs in selected",
      identical(s$below, 1L) && identical(s$above, c(4L, 6L)) &&
      identical(s$na, integer(0)) && identical(s$selected, c(2L, 3L, 7L, 8L)))

s <- stream_partition(x, c(4, 9), keep_na = FALSE, universe = uni,
                      split_range = TRUE)
check("split: checkbox off puts excluded NAs in the na stream",
      identical(s$na, c(3L, 7L)) && identical(s$selected, c(2L, 8L)))
check("split streams partition the universe",
      setequal(c(s$selected, s$below, s$above, s$na), which(uni)))

s <- stream_partition(x, c(-Inf, 9), keep_na = TRUE, universe = uni,
                      split_range = TRUE)
check("one-sided filter: unbounded side is empty",
      identical(s$below, integer(0)) && identical(s$above, c(4L, 6L)))

s <- stream_partition(x, c(4, 9), keep_na = TRUE, universe = uni,
                      split_range = TRUE, drop_na = TRUE)
check("drop_na strips NA rows from every stream",
      identical(s$selected, c(2L, 8L)) && identical(s$na, integer(0)))

y <- c("a", "b", NA, "c", "a", "b")
s <- stream_partition(y, c("a", "b"), keep_na = TRUE, split_range = TRUE)
check("membership filter ignores split_range (merged form)",
      setequal(names(s), c("selected", "excluded")) &&
      identical(s$selected, c(1L, 2L, 3L, 5L, 6L)) && identical(s$excluded, 4L))

s <- stream_partition(x, NULL, keep_na = TRUE, universe = uni)
check("no filter: everything in universe is selected",
      identical(s$selected, which(uni)) && length(s$excluded) == 0)

## ---- module, vector mode: the taxi-style scenario ----
set.seed(31)
df <- data.frame(
    dist = c(runif(300, 0, 30), rep(NA, 20)),
    fare = runif(320, 0, 100),
    pay  = sample(c("card", "cash"), 320, TRUE),
    g2   = sample(c("u", "v", NA), 320, TRUE),
    stringsAsFactors = FALSE
)
backend <- backend_memory(df)

testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = c("dist", "fare", "pay"))
    session$setInputs(filter_dist = c(5, 20), filter_fare = c(10, 90),
                      filter_pay = "card")
    s <- session$returned$streams("dist", split_range = TRUE)
    other <- make_mask(df$fare, c(10, 90)) & make_mask(df$pay, "card")
    check("selected identical to rows()",
          identical(s$selected, session$returned$rows()))
    check("below = other-filter survivors with dist < 5",
          identical(s$below, which(other & !is.na(df$dist) & df$dist < 5)))
    check("above = other-filter survivors with dist > 20",
          identical(s$above, which(other & !is.na(df$dist) & df$dist > 20)))
    check("NAs inside selected while include-NA is on",
          length(s$na) == 0 && all(which(other & is.na(df$dist)) %in% s$selected))

    session$setInputs(na_dist = FALSE)
    s <- session$returned$streams("dist", split_range = TRUE)
    check("include-NA off moves NAs to the na stream",
          identical(s$na, which(other & is.na(df$dist))) &&
          identical(s$selected, session$returned$rows()))

    ## a column never selected in Thanos: all filters apply, no partition
    s2 <- session$returned$streams("g2")
    check("unselected column: selected == rows(), excluded empty",
          identical(s2$selected, session$returned$rows()) &&
          length(s2$excluded) == 0)
})

## ---- vector vs aggregate equivalence ----
db_path <- tempfile(fileext = ".sqlite")
build_tall_skinny(df, db_path)
be <- backend_sqlite(db_path)

grab <- function(backend, mode) {
    out <- new.env()
    testServer(thanosServer,
               args = list(backend = backend, debounce_ms = 0,
                           debounce_checkbox_ms = 0, mode = mode,
                           max_discrete_numeric = 0), {
        session$setInputs(vars = c("dist", "fare", "pay"))
        session$setInputs(filter_dist = c(5, 20), filter_fare = c(10, 90),
                          filter_pay = "card")
        out$merged <- session$returned$streams("dist")
        out$split  <- session$returned$streams("dist", split_range = TRUE)
        out$drop   <- session$returned$streams("dist", split_range = TRUE,
                                               drop_na = TRUE)
        session$setInputs(na_dist = FALSE)
        out$split_noNA <- session$returned$streams("dist", split_range = TRUE)
        out$cat <- session$returned$streams("pay")
        out$unsel <- session$returned$streams("g2")
    })
    as.list(out)
}
vec <- grab(backend_memory(df), "vector")
agg <- grab(be, "aggregate")
for (nm in names(vec)) {
    check(paste0("aggregate streams identical to vector: ", nm),
          identical(vec[[nm]], agg[[nm]]))
}
be$disconnect()

cat("\nall streams tests passed\n")
