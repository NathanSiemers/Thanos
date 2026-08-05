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
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
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

## max_discrete_numeric = 0 keeps the 5-unique-value 'num' column on a
## slider so these blocks exercise classic range semantics
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0, debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
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

    ## deselect num: its filtering must be removed COMPLETELY -- both the
    ## active mask and the stored settings (no ghost filters, Project.md)
    session$setInputs(vars = "cat")
    check("removed variable no longer filters",
          session$returned$n_selected() == 6)
    check("selected_vars tracks", identical(session$returned$selected_vars(), "cat"))
    check("removed variable's stored filter is forgotten (default)",
          is.null(isolate(session$getReturned()$filters()$num)))

    ## re-adding comes back unfiltered once the fresh widget reports
    ## (testServer has no client, so we send the full-range value the
    ##  rebuilt slider would report)
    session$setInputs(vars = c("cat", "num"))
    session$setInputs(filter_num = c(1, 5), na_num = TRUE)
    check("re-added variable starts unfiltered",
          session$returned$n_selected() == 6)
})

## opt-in: remember_removed = TRUE restores settings on re-add
testServer(thanosServer,
           args = list(backend = backend, debounce_ms = 0, debounce_checkbox_ms = 0,
                       max_discrete_numeric = 0,
                       remember_removed = TRUE), {
    session$setInputs(vars = c("num", "cat"))
    session$setInputs(filter_num = c(2, 4))
    check("remember mode: filter applies", session$returned$n_selected() == 4)
    session$setInputs(vars = "cat")
    check("remember mode: removed variable still stops filtering",
          session$returned$n_selected() == 6)
    session$setInputs(vars = c("cat", "num"))
    check("remember mode: re-added variable restores its filter",
          identical(session$returned$filters()$num, c(2, 4)) &&
          session$returned$n_selected() == 4)
})

## discrete numeric: 'num' has 5 unique values, so with the default
## max_discrete_numeric = 12 it gets checkboxes and MEMBERSHIP semantics
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0, debounce_checkbox_ms = 0), {
    session$setInputs(vars = "num")
    session$setInputs(filter_num = c("2", "4"))
    check("discrete numeric filters by membership (values 2 and 4 + NA)",
          identical(session$returned$mask(),
                    c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)))
    session$setInputs(na_num = FALSE)
    check("discrete numeric membership + exclude NA",
          identical(session$returned$rows(), c(2L, 4L)))
})

## log2 toggle is display-only: an ACTIVE filter is preserved in raw
## units (the slider is repositioned, the filter itself never changes)
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = "num")
    session$setInputs(filter_num = c(2, 4))
    check("filter active before toggle", session$returned$n_selected() == 4)
    session$setInputs(log_num = TRUE)
    check("log toggle preserves the raw filter value",
          identical(session$returned$filters()$num, c(2, 4)))
    check("log toggle changes no results",
          session$returned$n_selected() == 4)
    session$setInputs(log_num = FALSE)
    check("toggling back still preserves the filter",
          identical(session$returned$filters()$num, c(2, 4)) &&
          session$returned$n_selected() == 4)
})

## log2(x+1) transform: slider moves to log space, filters stay raw
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = "num")
    session$setInputs(log_num = TRUE)
    ## slider space is now log2(x+1); [1.5, 2] maps to raw [~1.83, 3]
    session$setInputs(filter_num = c(1.5, 2))
    check("log2 slider range filters in raw units",
          identical(session$returned$rows(), c(2L, 3L, 6L)))
    check("filters() reports raw units",
          isTRUE(all.equal(session$returned$filters()$num[2], 3)))
    ## toggling back off resets to unfiltered
    session$setInputs(log_num = FALSE)
    session$setInputs(filter_num = c(1, 5))
    check("log toggle off restores linear, unfiltered",
          session$returned$n_selected() == 6)
})

## UI construction rules (white box, via make_var_panel):
##  - ANY column with NAs gets an include-NA checkbox, categorical too
##  - any non-negative continuous column gets the log2 toggle
info_num <- backend$get_column_info("num")   # numeric, has NA, min >= 0
p_slider <- as.character(make_var_panel(
    NS("t"), "num", "num", info_num, "slider", NULL, TRUE, "150px",
    slider = slider_bounds(info_num), can_log = TRUE, stored_log = FALSE))
check("non-negative slider panel offers include-NA AND log2 controls",
      grepl("na_num", p_slider) && grepl("log_num", p_slider))

be2 <- backend_memory(data.frame(catna = c("a", "b", NA),
                                 stringsAsFactors = FALSE))
info_cat <- be2$get_column_info("catna")
p_cat <- as.character(make_var_panel(
    NS("t"), "catna", "catna", info_cat, "checkbox", NULL, TRUE, "150px"))
check("categorical panel with NAs offers include-NA too",
      grepl("na_catna", p_cat) && grepl("include NA", p_cat))

be3 <- backend_memory(data.frame(nona = c("a", "b", "c"),
                                 stringsAsFactors = FALSE))
p_nona <- as.character(make_var_panel(
    NS("t"), "nona", "nona", be3$get_column_info("nona"), "checkbox",
    NULL, TRUE, "150px"))
check("column without NAs gets no include-NA checkbox",
      !grepl("na_nona", p_nona))

## normalize_filters: no-op entries dropped, restrictive ones kept
infos <- list(
    num = list(is_numeric = TRUE),
    cat = list(is_numeric = FALSE, levels = c("x", "y", "z")),
    dsc = list(is_numeric = TRUE, levels = c("1", "2", "3"))
)
fl <- list(
    num = list(is_numeric = TRUE, val = c(-Inf, Inf), include_na = TRUE),
    cat = list(is_numeric = FALSE, val = c("x", "y", "z"), include_na = TRUE),
    dsc = list(is_numeric = TRUE, val = c("1", "2", "3"), include_na = TRUE)
)
check("fully unbounded / full-set filters normalize away",
      length(normalize_filters(fl, infos)) == 0)
fl$num$val <- c(2, Inf)
fl$cat$val <- c("x")
fl$dsc$include_na <- FALSE
check("half-open range, level subset, and NA-exclusion all kept",
      setequal(names(normalize_filters(fl, infos)), c("num", "cat", "dsc")))
check("NULL val with include-NA on normalizes away",
      length(normalize_filters(
          list(num = list(is_numeric = TRUE, val = NULL,
                          include_na = TRUE)), infos)) == 0)

cat("\nall module tests passed\n")
