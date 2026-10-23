################################################################
## Malformed client inputs must be ignored, never error.
## Run from the repo root:  Rscript tests/test-module-hardening.R
################################################################
suppressPackageStartupMessages({ library(shiny); library(ggplot2) })
root <- Filter(function(p) file.exists(file.path(p, "R", "thanos_module.R")),
               c(".", ".."))[1]
invisible(lapply(list.files(file.path(root, "R"), pattern = "^thanos_.*[.]R$",
                            full.names = TRUE), source))
check <- function(label, expr) {
    ok <- isTRUE(expr)
    cat(sprintf("%s  %s\n", if (ok) "PASS" else "FAIL", label))
    if (!ok) stop("test failed: ", label, call. = FALSE)
}

## validators: valid input returned unchanged
check("range valid unchanged", identical(thanos_valid_range(c(2, 4), 1, 5), c(2, 4)))
check("range clamped", identical(thanos_valid_range(c(-9, 99), 1, 5), c(1, 5)))
check("range rejects len1/len3/NA/char/inverted/empty",
      all(vapply(list(1, 1:3, c(1, NA), c(NaN, 2), c("a", "b"), c(4, 2), numeric(0),
                      c(1, Inf), list(1, 2), TRUE),
                 function(x) is.null(thanos_valid_range(x, 1, 5)), NA)))
check("flag", isTRUE(thanos_valid_flag(TRUE)) && identical(thanos_valid_flag(FALSE), FALSE) &&
      isTRUE(thanos_valid_flag("no")) && isTRUE(thanos_valid_flag(NA)) &&
      isTRUE(thanos_valid_flag(c(TRUE, FALSE))) && isTRUE(thanos_valid_flag(NULL)))
check("levels subset", identical(thanos_valid_levels(c("x", "bogus"), c("x", "y")), "x") &&
      identical(thanos_valid_levels(c("y", "x"), c("x", "y")), c("y", "x")) &&
      identical(thanos_valid_levels(character(0), "x"), character(0)) &&
      is.na(thanos_valid_levels(5, "x")))

df <- data.frame(num = c(1, 2, 3, 4, 5, NA),
                 cat = c("x", "x", "y", "y", "z", "z"), stringsAsFactors = FALSE)
for (mode in "vector") {
    be <- backend_memory(df)
    testServer(thanosServer, args = list(backend = be, debounce_ms = 0,
                                         debounce_checkbox_ms = 0,
                                         max_discrete_numeric = 0, mode = mode), {
        session$setInputs(vars = c("num", "cat"))
        ok_n <- function() { n <- session$returned$n_selected()
            is.numeric(n) && length(n) == 1 && !is.na(n) && n >= 0 && n <= 6 }
        session$setInputs(filter_num = c(2, 4))
        base <- session$returned$mask()
        bad <- list(5, c(1, 2, 3), c(NaN, 3), c(NA_real_, NA_real_), c("a", "b"),
                    c(4, 2), numeric(0), list(1, 2), TRUE, c(Inf, 3), seq_len(1e5))
        for (b in bad) {
            r <- tryCatch({ suppressMessages(session$setInputs(filter_num = b)); "ok" },
                          error = function(e) conditionMessage(e))
            check("malformed slider value: no error", identical(r, "ok"))
            check("n_selected stays valid", ok_n())
            check("filter keeps last valid state", identical(session$returned$mask(), base))
        }
        for (b in list("no", NA, c(TRUE, FALSE), character(0), 1:3, list())) {
            r <- tryCatch({ suppressMessages(session$setInputs(na_num = b)); "ok" },
                          error = function(e) conditionMessage(e))
            check("malformed NA flag: no error", identical(r, "ok"))
            check("n_selected valid after bad flag", ok_n())
        }
        for (b in list(5, NA, list("x"), c("x", "bogus"), rep("x", 1e4), character(0))) {
            r <- tryCatch({ suppressMessages(session$setInputs(filter_cat = b)); "ok" },
                          error = function(e) conditionMessage(e))
            check("malformed checkbox value: no error", identical(r, "ok"))
            check("n_selected valid after bad levels", ok_n())
        }
        r <- tryCatch({ suppressMessages(session$setInputs(vars = 5)); "ok" },
                      error = function(e) conditionMessage(e))
        check("malformed vars: no error", identical(r, "ok"))
        check("vars unchanged", identical(session$returned$selected_vars(), c("num", "cat")))
        ## valid input still works afterwards, with the same result as in test-module.R
        session$setInputs(na_num = TRUE, filter_num = c(2, 4), filter_cat = c("x", "y", "z"))
        check("valid input after malformed ones",
              identical(session$returned$mask(),
                        c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)))
        check("n_selected regression value", session$returned$n_selected() == 4)
        session$setInputs(na_num = FALSE)
        check("include-NA regression", session$returned$n_selected() == 3)
    })
}
cat("\nall hardening tests passed\n")
