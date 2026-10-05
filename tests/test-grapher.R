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

## add_vars(): the one parent->module call. In testServer there is no
## client to round-trip the selectize update, so we verify the request
## logic directly and then simulate the client echoing it back.
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0), {
    session$setInputs(vars = "a")
    want <- session$getReturned()$add_vars(c("g", "nonexistent_column"))
    check("add_vars unions with current selection, drops unknown names",
          setequal(want, c("a", "g")))
    session$setInputs(vars = want)   # the client echo
    check("added column participates in filtering after round trip",
          setequal(session$returned$selected_vars(), c("a", "g")))
    check("add_vars is idempotent",
          setequal(session$getReturned()$add_vars("g"), c("a", "g")))
})

## add_vars() re-sends the server-side selectize (so columns outside
## the first loaded page of options can be selected at all); the client
## clears the widget while it reloads.  That ONE transient empty report
## must not tear down the existing panels and their filters.
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0), {
    session$setInputs(vars = "a")
    session$setInputs(filter_a = c(25, 75))
    kept <- session$returned$rows()
    session$getReturned()$add_vars("g")
    session$setInputs(vars = character(0))      # client clears on reload
    check("transient empty report after add_vars keeps panels + filters",
          identical(session$returned$selected_vars(), "a") &&
          identical(session$returned$rows(), kept))
    session$setInputs(vars = c("a", "g"))       # then re-selects
    check("the reloaded selection adds the new column, filters intact",
          setequal(session$returned$selected_vars(), c("a", "g")) &&
          identical(session$returned$rows(), kept))
    session$setInputs(vars = character(0))      # a GENUINE clear-all
    check("a genuine clear-all afterwards is honoured",
          length(session$returned$selected_vars()) == 0 &&
          session$returned$n_selected() == nrow(df))
})
## ...and no report can be swallowed when nothing was selected before
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0), {
    session$getReturned()$add_vars("g")
    session$setInputs(vars = "g")
    session$setInputs(vars = character(0))
    check("clear-all right after an add_vars round trip is honoured",
          length(session$returned$selected_vars()) == 0)
})

## start-up: this module's vars observer runs once before the widget has
## reported anything (input$vars is NULL).  That run must not be taken
## as "the user selected nothing" -- a parent's start-up add_vars() has
## to union with default_selected, not replace it.
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     default_selected = "a"), {
    session$flushReact()
    want <- session$getReturned()$add_vars("g")
    check("start-up add_vars() keeps default_selected",
          setequal(want, c("a", "g")))
})

## the REAL grapher app server, end to end: shadow shinyApp so sourcing
## app.R hands back its server function, then drive a scenario where
## the x filter rejects on BOTH sides -> three populations -> three
## pairwise comparison facets and three stats blocks
grapher_server <- local({
    shinyApp <- function(ui, server) server
    source(file.path(root, "apps", "grapher", "app.R"), local = TRUE)$value
})
testServer(grapher_server, {
    session$setInputs(x = "dep_delay", y = "arr_delay",
                      color = "(none)", size = "(none)",
                      show_excluded = TRUE, fit_slopes = TRUE)
    session$setInputs(`thanos-vars` = "dep_delay")
    session$setInputs(`thanos-filter_dep_delay` = c(-10, 30))
    session$elapse(400)   # module slider debounce
    cmp <- comparison()
    check("both handles rejecting -> three populations, three pairs",
          !is.null(cmp) && length(cmp$pairs) == 3 &&
          setequal(names(cmp$sets), c("below", "selected", "above")))
    txt <- output$slopes
    check("a stats block per pair, with slope test verdicts",
          length(gregexpr("== ", txt, fixed = TRUE)[[1]]) == 3 &&
          grepl("slopes differ|no significant", txt))
    check("caption reports the compared populations",
          grepl("comparing", output$counts) &&
          grepl("below", output$counts))
})

## parsimony in the real app: changing x should cost exactly ONE
## scatter-data recompute (for the new x) -- the arr_time panel
## addition that follows must not re-trigger it
testServer(grapher_server, {
    session$setInputs(x = "dep_delay", y = "arr_delay",
                      color = "(none)", size = "(none)",
                      show_excluded = FALSE, fit_slopes = FALSE)
    session$setInputs(`thanos-vars` = "dep_delay")
    session$setInputs(`thanos-filter_dep_delay` = c(-10, 30))
    session$elapse(400)
    runs <- 0
    observe({ plot_data(); runs <<- runs + 1 })
    session$flushReact()
    r0 <- runs
    ## the user picks a new x; the add_vars observer then adds it to the
    ## module (simulated round trip), whose widget reports a full-range
    ## no-op filter
    session$setInputs(x = "arr_time")
    session$setInputs(`thanos-vars` = c("dep_delay", "arr_time"))
    session$elapse(400)
    check("changing x costs exactly one scatter-data recompute",
          runs == r0 + 1)
})

cat("\nall grapher embedding tests passed\n")
