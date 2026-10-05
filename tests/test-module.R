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

## parsimony: rows() must be INVALIDATION-stable across changes that
## don't alter its content -- adding an unfiltered column re-ran every
## consumer before the looStore/globalMask gating.  We count actual
## re-executions of an observer reading rows().
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = "num")
    runs <- 0
    observe({ session$returned$rows(); runs <<- runs + 1 })
    session$flushReact()
    r0 <- runs
    session$setInputs(vars = c("num", "cat"))       # structural, no content change
    session$setInputs(filter_cat = c("x", "y", "z")) # no-op widget report
    check("adding an unfiltered column re-runs NO rows() consumer",
          runs == r0)
    session$setInputs(filter_num = c(2, 4))          # a real filter change
    check("a genuine filter change re-runs consumers exactly once",
          runs == r0 + 1)
})

## audit regression: a remembered EMPTY selection survives remove/re-add
## (the fresh widget's NULL report must not clobber character(0))
testServer(thanosServer,
           args = list(backend = backend, debounce_ms = 0,
                       debounce_checkbox_ms = 0, max_discrete_numeric = 0,
                       remember_removed = TRUE), {
    session$setInputs(vars = "cat")
    session$setInputs(filter_cat = "y")          # widget has spoken
    session$setInputs(filter_cat = NULL)         # uncheck all = none
    check("empty selection selects nothing", session$returned$n_selected() == 0)
    session$setInputs(vars = character(0))
    session$setInputs(vars = "cat")
    check("remembered empty selection survives re-add",
          session$returned$n_selected() == 0 &&
          identical(session$returned$filters()$cat, character(0)))
})

## audit regression: re-adding a FORGOTTEN column must not resurrect its
## old filter from the session's stale input value (parents saw wrong
## rows() during that window before the input-freeze fix)
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = "num")
    session$setInputs(filter_num = c(2, 4))
    check("filter active", session$returned$n_selected() == 4)
    session$setInputs(vars = character(0))
    session$setInputs(vars = "num")   # stale filter_num = c(2,4) persists
    check("re-add does NOT resurrect the forgotten filter",
          session$returned$n_selected() == 6 &&
          is.null(session$returned$filters()$num))
})

## audit regression: obs_mask runs ONCE per event (the compound
## filterState assignments used to re-trigger it), measured by counting
## O(n) mask computations through a wrapped make_mask
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = "num")
    calls <- 0
    orig <- make_mask
    make_mask <<- function(...) { calls <<- calls + 1; orig(...) }
    on.exit(make_mask <<- orig, add = TRUE)
    session$setInputs(filter_num = c(2, 4))
    check("one filter change = exactly one mask computation", calls == 1)
    session$setInputs(filter_num = c(2, 4))      # identical re-send
    check("identical re-send computes no mask at all", calls == 1)
    make_mask <<- orig
})

## ---------------------------------------------------------------
## base_mask: a parent-imposed universe.  It is ANDed into every
## leave-one-out mask and the global mask, so histograms, counts,
## mask()/rows() and streams() all see only the parent's rows.
## ---------------------------------------------------------------
bm <- reactiveVal(NULL)
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0,
                                     base_mask = bm), {
    session$flushReact()
    check("base_mask NULL, no vars: everything passes",
          all(session$returned$mask()) && session$returned$n_selected() == 6)

    ## no filter columns at all: the universe alone decides
    base <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
    bm(base); session$flushReact()
    check("no vars: mask() is exactly the base mask",
          identical(session$returned$mask(), base))
    check("no vars: rows()/n_selected() follow the base mask",
          identical(session$returned$rows(), 1:4) &&
          session$returned$n_selected() == 4)

    ## unfiltered columns change nothing, and their histograms count
    ## only the universe
    session$setInputs(vars = c("num", "cat"))
    check("unfiltered columns: mask still equals the base mask",
          identical(session$returned$mask(), base))
    ct <- isolate(counts_for("cat"))
    check("histogram of an unfiltered column counts only the universe",
          ct$n_shown == 4 && ct$n_sel == 4 && identical(sum(ct$shown), 4L))

    ## an active filter combines with the universe (row 6 is NA in num:
    ## kept by include-NA, removed by the base mask)
    session$setInputs(filter_num = c(2, 4))
    check("filter AND base mask", identical(session$returned$rows(), 2:4))
    ct <- isolate(counts_for("cat"))
    check("another column's leave-one-out set respects the base mask",
          ct$n_shown == 3 && identical(as.integer(ct$shown), c(1L, 2L, 0L)))
    ct <- isolate(counts_for("num"))
    check("own histogram: shown = universe, selected = universe & filter",
          ct$n_shown == 4 && ct$n_sel == 3)
    st <- isolate(session$returned$streams("num"))
    check("streams() never leaks rows outside the universe",
          identical(st$selected, 2:4) && identical(st$excluded, 1L))

    ## the universe is reactive: changing it re-filters without touching
    ## the user's filter settings
    bm(c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)); session$flushReact()
    check("changing the base mask re-filters, filter settings kept",
          identical(session$returned$rows(), c(2L, 3L, 6L)) &&
          identical(isolate(session$returned$filters())$num, c(2, 4)))

    ## NA in the base mask means "not in the universe"
    bm(c(NA, TRUE, TRUE, TRUE, TRUE, TRUE)); session$flushReact()
    check("NA in base mask counts as FALSE",
          identical(session$returned$rows(), c(2L, 3L, 4L, 6L)))

    ## an empty universe
    bm(rep(FALSE, 6)); session$flushReact()
    check("all-FALSE base mask selects nothing",
          session$returned$n_selected() == 0 &&
          length(session$returned$rows()) == 0)

    ## removing every column leaves exactly the universe
    bm(base); session$flushReact()
    session$setInputs(vars = character(0))
    check("removing all columns returns the mask to the base mask",
          identical(session$returned$mask(), base))

    ## parsimony: NULL and all-TRUE are the SAME universe -- switching
    ## between them must not re-run consumers of rows()
    bm(NULL); session$flushReact()
    runs <- 0
    observe({ session$returned$rows(); runs <<- runs + 1 })
    session$flushReact()
    r0 <- runs
    bm(rep(TRUE, 6)); session$flushReact()
    bm(NULL); session$flushReact()
    check("NULL <-> all-TRUE base mask invalidates nothing", runs == r0)

    ## a malformed mask is a loud error, not a silent mis-filter
    bm(c(TRUE, FALSE))
    bad <- tryCatch({ isolate(base_now()); FALSE }, error = function(e) TRUE)
    bm(NULL)
    check("wrong-length base mask is rejected", bad)
})
check("non-function base_mask is rejected at server start",
      tryCatch({
          testServer(thanosServer,
                     args = list(backend = backend,
                                 base_mask = c(TRUE, FALSE)), { NULL })
          FALSE
      }, error = function(e) grepl("base_mask", conditionMessage(e))))

## ---------------------------------------------------------------
## ids: column names that differ only in punctuation must get
## DIFFERENT input ids (a lossy "replace with _" scheme collided),
## while names that are already id-safe keep their name as the id
## ---------------------------------------------------------------
check("id-safe names map to themselves",
      identical(thanos_vid("dep_delay"), "dep_delay") &&
      identical(thanos_vid("num"), "num"))
check("punctuation variants get distinct ids",
      length(unique(vapply(c("a.b", "a_b", "a-b", "a b", "a..b"),
                           thanos_vid, ""))) == 5 &&
      thanos_vid("HLA-A") != thanos_vid("HLA.A"))
check("ids contain only selector-safe characters",
      !grepl("[^A-Za-z0-9_-]", thanos_vid("TP53.mut / x:y (z)")))
df_ids <- data.frame(c(1, 2, 3, 4), c(10, 20, 30, 40), check.names = FALSE)
names(df_ids) <- c("a.b", "a_b")
testServer(thanosServer, args = list(backend = backend_memory(df_ids),
                                     debounce_ms = 0, debounce_checkbox_ms = 0,
                                     max_discrete_numeric = 0), {
    session$setInputs(vars = c("a.b", "a_b"))
    args <- stats::setNames(list(c(2, 4)), paste0("filter_", thanos_vid("a.b")))
    do.call(session$setInputs, args)
    check("filtering 'a.b' does not touch 'a_b'",
          identical(session$returned$rows(), 2:4) &&
          identical(names(isolate(session$returned$filters())), "a.b"))
    session$setInputs(filter_a_b = c(10, 30))
    check("'a.b' and 'a_b' filter independently",
          identical(session$returned$rows(), 2:3))
})

## the column picker is whitelisted against the backend's columns
testServer(thanosServer, args = list(backend = backend, debounce_ms = 0,
                                     debounce_checkbox_ms = 0), {
    session$setInputs(vars = c("num", "no_such_column"))
    check("unknown names in the column picker are ignored",
          identical(session$returned$selected_vars(), "num"))
})

## max_vars: the number of open columns is limited on the server, whatever the
## browser sends and whatever a parent asks for
many <- as.data.frame(matrix(runif(40 * 12), 40, 12)); names(many) <- paste0("v", 1:12)
testServer(thanosServer, args = list(backend = backend_memory(many), debounce_ms = 0,
                                     debounce_checkbox_ms = 0, max_vars = 3), {
    session$setInputs(vars = paste0("v", 1:12))
    check("the column picker opens at most max_vars columns",
          identical(session$returned$selected_vars(), paste0("v", 1:3)))
    session$setInputs(filter_v2 = c(0.25, 0.75))
    session$setInputs(vars = c("v9", "v2", "v10", "v11", "v12"))
    check("columns already open are kept when too many are asked for",
          "v2" %in% session$returned$selected_vars() && length(session$returned$selected_vars()) == 3 &&
          identical(names(isolate(session$returned$filters())), "v2"))
    before <- session$returned$selected_vars()
    session$returned$add_vars(paste0("v", 4:8))
    check("add_vars() cannot exceed the limit either",
          identical(session$returned$selected_vars(), before))
})
testServer(thanosServer, args = list(backend = backend_memory(many), debounce_ms = 0,
                                     debounce_checkbox_ms = 0, max_vars = NULL), {
    session$setInputs(vars = paste0("v", 1:12))
    check("max_vars = NULL means no limit", length(session$returned$selected_vars()) == 12)
})


cat("\nall module tests passed\n")
