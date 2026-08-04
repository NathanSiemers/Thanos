################################################################
## Backend equivalence: backend_sqlite must serve columns identical()
## to backend_memory's, NA positions included, plus matching metadata.
## Run from the repo root:  Rscript tests/test-backend.R
################################################################
suppressPackageStartupMessages({
    library(DBI)
    library(RSQLite)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_backend.R")),
               c(".", ".."))[1]
source(file.path(root, "R", "thanos_backend.R"))
source(file.path(root, "R", "thanos_backend_sqlite.R"))
source(file.path(root, "db", "build_flights_sqlite.R"))

check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

## fixture: 100 rows, every column type quirk we support
set.seed(7)
fixture <- data.frame(
    num_plain = rnorm(100),
    num_nas   = ifelse(runif(100) < 0.2, NA, rpois(100, 5)),
    int_col   = 1:100,
    chr_col   = sample(c("aa", "bb", "cc", NA), 100, replace = TRUE),
    fct_col   = factor(sample(c("lo", "hi"), 100, replace = TRUE)),
    lgl_col   = sample(c(TRUE, FALSE, NA), 100, replace = TRUE),
    date_col  = as.Date("2024-01-01") + sample(c(0:30, NA), 100, replace = TRUE),
    stringsAsFactors = FALSE
)
fixture$num_all_na <- NA_real_

db_path <- tempfile(fileext = ".sqlite")
build_tall_skinny(fixture, db_path)

mem <- backend_memory(fixture)
sql <- backend_sqlite(db_path)

check("same column set",
      identical(sort(mem$get_columns()), sort(sql$get_columns())))
check("same row count", mem$n_rows() == sql$n_rows())

for (col in mem$get_columns()) {
    check(paste0("column '", col, "' identical through both backends"),
          identical(mem$get_column(col), sql$get_column(col)))
    im <- mem$get_column_info(col)
    is <- sql$get_column_info(col)
    check(paste0("info '", col, "': type and n_na agree"),
          im$is_numeric == is$is_numeric && im$n_na == is$n_na)
    if (im$is_numeric) {
        check(paste0("info '", col, "': range agrees"),
              isTRUE(all.equal(im$range, is$range)) ||
              (all(!is.finite(im$range)) && all(!is.finite(is$range))))
    } else {
        check(paste0("info '", col, "': levels agree"),
              identical(im$levels, is$levels))
    }
}

sql$disconnect()
cat("\nall backend tests passed\n")
