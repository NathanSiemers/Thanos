################################################################
## Module logic tests via shiny::testServer.
## Run from the repo root:  Rscript tests/test-module.R
################################################################
suppressPackageStartupMessages({
    library(shiny)
    library(ggplot2)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_module.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "[.]R$",
                            full.names = TRUE), source))

check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

df <- data.frame(
    num = c(1, 2, 3, 4, 5, NA),
    cat = c("x", "x", "y", "y", "z", "z"),
    stringsAsFactors = FALSE
)
backend <- backend_memory(df)

testServer(thanosServer, args = list(backend = backend, debounce_ms = 0), {
    ## select two variables
    session$setInputs(vars = c("num", "cat"))
    m <- session$returned$mask()
    check("mask has one entry per row", length(m) == nrow(df))
    check("no filters yet: everything passes", all(m))
    check("rows() is 1..n", identical(session$returned$rows(), 1:6))

    ## numeric filter: keep num in [2,4]; NA row kept by default
    session$setInputs(filter_num = c(2, 4))
    check("numeric filter applied (2,3,4 + NA row pass)",
          identical(session$returned$mask(),
                    c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)))
    check("n_selected agrees", session$returned$n_selected() == 4)

    ## exclude the NA row too
    session$setInputs(na_num = FALSE)
    check("include-NA off drops the NA row",
          identical(session$returned$mask(),
                    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)))

    ## categorical filter on top: only "y"
    session$setInputs(filter_cat = "y")
    check("combined numeric AND categorical filters",
          identical(session$returned$rows(), 3:4))

    ## checkbox reporting NULL after having spoken means "none selected"
    session$setInputs(filter_cat = NULL)
    check("unchecking every box selects nothing",
          session$returned$n_selected() == 0)

    ## restore
    session$setInputs(filter_cat = c("x", "y", "z"), na_num = TRUE)
    check("filters() reports current settings",
          identical(session$returned$filters()$num, c(2, 4)))

    ## deselect num: its filter must stop applying...
    session$setInputs(vars = "cat")
    check("removed variable no longer filters",
          session$returned$n_selected() == 6)
    check("selected_vars tracks", identical(session$returned$selected_vars(), "cat"))

    ## ...but re-adding it restores the stored filter (persistence)
    session$setInputs(vars = c("cat", "num"))
    check("re-added variable remembers its stored filter",
          identical(session$returned$filters()$num, c(2, 4)))
})

cat("\nall module tests passed\n")
