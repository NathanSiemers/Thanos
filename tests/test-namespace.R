################################################################
## Namespace loader (thanos.R at the repo root): publics exported,
## internals hidden, and name collisions with a host app harm neither
## side.  Also checks the loader's export list against the package
## NAMESPACE, so the two distribution routes cannot drift apart.
## Run from the repo root:  Rscript tests/test-namespace.R
################################################################
suppressPackageStartupMessages({
    library(shiny)
    library(ggplot2)
})

check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

## the "host app" defines functions whose names collide with Thanos
## internals BEFORE loading -- these must survive untouched, and the
## module must never call them
make_mask     <- function(...) stop("host make_mask called by Thanos!")
display_range <- function(...) stop("host display_range called by Thanos!")
host_mask     <- make_mask

root <- Filter(function(p) file.exists(file.path(p, "thanos.R")),
               c(".", ".."))[1]
source(file.path(root, "thanos.R"))

check("public API exported",
      all(vapply(c("thanosUI", "thanosServer", "backend_memory",
                   "backend_dbi", "backend_sqlite", "backend_duckdb"),
                 function(nm) is.function(get(nm)), NA)))
check("internals NOT exported (bin_column stays private)",
      !exists("bin_column", inherits = FALSE))
check("host's colliding make_mask untouched by loading",
      identical(make_mask, host_mask))
check("namespace handle exposes internals for power users",
      is.function(thanos$make_mask) && is.function(thanos$bin_column))
check("exported functions resolve helpers in the private namespace",
      identical(environment(thanosServer), thanos))

## the package and the loader must publish the SAME public API: parse
## the NAMESPACE export() lines and compare against the loader's list
loader_api <- c("thanosUI", "thanosServer", "backend_memory",
                "backend_dbi", "backend_sqlite", "backend_duckdb")
ns_lines <- readLines(file.path(root, "NAMESPACE"))
ns_exports <- sub("^export\\(", "",
                  sub("\\)$", "", grep("^export\\(", ns_lines, value = TRUE)))
check("loader and NAMESPACE agree on the public API",
      setequal(ns_exports, loader_api))

## the module must work end to end while the host's poisoned
## make_mask/display_range sit in the global environment
df <- data.frame(num = c(1, 2, 3, 4, 5, NA),
                 cat = c("x", "x", "y", "y", "z", "z"),
                 stringsAsFactors = FALSE)
backend <- backend_memory(df)
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = c("num", "cat"))
    session$setInputs(filter_num = c(2, 4), filter_cat = "y")
    check("module filters correctly despite host-side name collisions",
          identical(session$returned$rows(), 3:4))
})

cat("\nall namespace tests passed\n")
