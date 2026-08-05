#' thanos: Reusable Cross-Filtering Module for Shiny
#'
#' Interactive cross-filter histograms as a Shiny module: pick columns,
#' get auto-generated filter widgets, and every variable's histogram
#' updates live to show the rows passing all other filters. Backends
#' are pluggable (in-memory data frame, SQLite, DuckDB), and an
#' aggregate mode pushes histogram counts to SQL for data at the scale
#' of tens of millions of rows.
#'
#' @import shiny
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom utils object.size
#' @importFrom graphics axis mtext par plot.new plot.window rect title
#' @keywords internal
"_PACKAGE"

## data-frame column names used inside ggplot2::aes() in
## plot_histo_counts() -- non-standard evaluation, not real globals
utils::globalVariables(c("pos", "count", "fill"))
