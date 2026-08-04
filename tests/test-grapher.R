################################################################
## Embedding integration test: a parent app consuming th$rows() gets
## exactly the rows the module says survive.
## Run from the repo root:  Rscript tests/test-grapher.R
################################################################
suppressPackageStartupMessages({
    library(shiny)
    library(ggplot2)
})
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_module.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
                            full.names = TRUE), source))

check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

set.seed(11)
df <- data.frame(
    a = runif(500, 0, 100),
    b = rnorm(500),
    g = sample(c("p", "q", "r"), 500, replace = TRUE),
    stringsAsFactors = FALSE
)
backend <- backend_memory(df)

## a minimal parent server embedding the module, like apps/grapher
parent_server <- function(input, output, session) {
    th <- thanosServer("thanos", backend, debounce_ms = 0,
                       debounce_checkbox_ms = 0)
    plot_data <- reactive({
        r <- th$rows()
        data.frame(a = backend$get_column("a")[r],
                   g = backend$get_column("g")[r])
    })
    th_out <- th          # expose for the test
    plot_data_out <- plot_data
}

testServer(parent_server, {
    session$setInputs(`thanos-vars` = c("a", "g"))
    session$setInputs(`thanos-filter_a` = c(25, 75), `thanos-filter_g` = c("p", "q"))
    expected <- which(df$a >= 25 & df$a <= 75 & df$g %in% c("p", "q"))
    check("parent's plot_data has exactly the surviving rows",
          nrow(plot_data_out()) == length(expected))
    check("module rows() matches a direct computation",
          identical(th_out$rows(), expected))
    check("n_selected agrees with the parent's frame",
          th_out$n_selected() == nrow(plot_data_out()))
    check("values round-trip: parent frame really is the filtered subset",
          isTRUE(all(plot_data_out()$g %in% c("p", "q"))) &&
          isTRUE(all(plot_data_out()$a >= 25 & plot_data_out()$a <= 75)))
})

cat("\nall grapher embedding tests passed\n")
