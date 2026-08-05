################################################################
## Thanos loader -- source THIS ONE FILE (it lives at the repo root)
## to use Thanos without installing anything:
##
##     source("path/to/thanos.R")
##
## (The installable alternative is the R package built from this same
## tree: install it, then library(thanos).)
##
## All Thanos code is defined inside a private environment; only the
## public API is placed where you sourced from:
##
##     thanosUI(), thanosServer()
##     backend_memory(), backend_dbi(), backend_sqlite(), backend_duckdb()
##     thanos   -- the private environment itself, so power users and
##                 tests can reach internals as thanos$make_mask etc.
##                 (if your app already owns a `thanos` object, load the
##                  R/thanos_*.R files directly instead)
##
## Why: internals (make_mask, bin_column, display_range, plot helpers,
## the theme...) have collision-prone names.  Kept private, a host app
## that defines its own make_mask() can neither break the module nor be
## clobbered by it.  Usage is otherwise identical to sourcing the files
## directly -- one line, no package install.
################################################################

## the sourced files carry no library() calls (R CMD check forbids them
## in package code), so the loader attaches Thanos' two hard
## dependencies for source-mode consumers
library(shiny)
library(ggplot2)

local({
    ## locate the directory this file lives in (robust under source()),
    ## then find the thanos_*.R sources in R/ next to it
    this_file <- NULL
    for (fr in rev(sys.frames())) {
        if (!is.null(fr$ofile)) { this_file <- fr$ofile; break }
    }
    dir <- if (!is.null(this_file)) {
        file.path(dirname(normalizePath(this_file)), "R")
    } else {
        Filter(function(p) file.exists(file.path(p, "thanos_module.R")),
               c("R", "../R", "../../R"))[1]
    }
    if (is.null(dir) || is.na(dir) ||
        !file.exists(file.path(dir, "thanos_module.R"))) {
        stop("thanos.R: cannot locate the thanos_*.R files")
    }

    ## private namespace: lookups go ns -> globalenv -> attached packages,
    ## and ns wins for everything defined here
    ns <- new.env(parent = globalenv())
    for (f in sort(list.files(dir, pattern = "^thanos_.*[.]R$",
                              full.names = TRUE))) {
        sys.source(f, envir = ns, keep.source = TRUE)
    }

    exports <- c("thanosUI", "thanosServer",
                 "backend_memory", "backend_dbi",
                 "backend_sqlite", "backend_duckdb")
    dest <- parent.env(environment())   # wherever this file was sourced
    for (nm in exports) assign(nm, get(nm, envir = ns), envir = dest)
    assign("thanos", ns, envir = dest)
    invisible()
})
