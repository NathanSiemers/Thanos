################################################################
## Real-browser check of the grapher embedding: the selectize round
## trips that testServer cannot simulate (default_selected + start-up
## add_vars(), adding a column without losing filters, manual removal).
## NOT part of the tests/test-*.R suite: it needs shinytest2 and a
## Chrome/Chromium binary.  Run from the repo root:
##   CHROMOTE_CHROME=/path/to/chrome NOT_CRAN=true Rscript tests/browser-grapher.R
## Skips (exit 0) when no browser is available.
################################################################
suppressMessages({ library(shiny); library(shinytest2) })
if (is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) {
    cat("SKIP: no Chrome/Chromium found (set CHROMOTE_CHROME)\n"); quit(status = 0)
}
chromote::set_chrome_args(c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage"))
ok <- function(cond, msg) cat(if (isTRUE(cond)) "  PASS " else "  FAIL ", msg, "\n")
app <- AppDriver$new("apps/grapher", load_timeout = 180000, timeout = 120000, height = 900, width = 1400)
settle <- function(ms = 2000) { Sys.sleep(ms / 1000); app$wait_for_idle(500, timeout = 120000) }
settle(3000)
v <- app$get_value(input = "thanos-vars"); cat("    vars:", paste(v, collapse = ", "), "\n")
ok(setequal(v, c("carrier", "origin", "dep_delay", "distance", "arr_delay")),
   "startup: default_selected + add_vars(x, y) both arrive")
np <- function() app$get_js("document.querySelectorAll('#thanos-panels > .thanos-panel').length")
ok(np() == 5, "five panels")
app$set_inputs(`thanos-filter_origin` = c("JFK", "LGA")); settle()
c1 <- app$get_value(output = "counts"); cat("    ", c1, "\n")
app$set_inputs(x = "air_time"); settle(2500)
v2 <- app$get_value(input = "thanos-vars"); cat("    vars:", paste(v2, collapse = ", "), "\n")
ok("air_time" %in% v2 && np() == 6, "changing x adds its panel")
ok(setequal(app$get_value(input = "thanos-filter_origin"), c("JFK", "LGA")), "existing checkbox filter kept")
c2 <- app$get_value(output = "counts"); cat("    ", c2, "\n")
ok(identical(sub(" rows pass.*", "", c1), sub(" rows pass.*", "", c2)), "row count unchanged by adding a column")
## user removes a column by hand, then removes all
app$run_js("$('#thanos-vars')[0].selectize.removeItem('carrier')"); settle()
ok(!("carrier" %in% app$get_value(input = "thanos-vars")) && np() == 5, "manual removal still works")
app$run_js("$('#thanos-vars')[0].selectize.clear()"); settle()
ok(np() == 0 && grepl("^336,776 of 336,776", app$get_value(output = "counts")), "manual clear-all removes every panel and filter")
logs <- app$get_logs(); errs <- logs[logs$level == "error", ]
ok(nrow(errs) == 0, "no errors logged"); if (nrow(errs)) print(errs)
app$stop()
cat("\nall grapher browser checks passed\n")
