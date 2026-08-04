# Thanos benchmark log

Append a dated section per meaningful commit so regressions are visible.
All numbers from `Rscript bench/bench_masks.R` unless noted.
Box: R 4.5.3, Linux (rstudio container).

## 2026-08-04 — Phase A baseline (merged module, pre-binned plots)

flights: 336,776 rows

| operation | time |
|---|---|
| make_mask numeric (dep_delay in [-10, 60]) | 10.7 ms |
| make_mask categorical (3 carriers) | 22.2 ms |
| leave-one-out combine, k = 3 vars | 15.8 ms |
| leave-one-out combine, k = 6 vars | 44.4 ms |
| leave-one-out combine, k = 10 vars | 57.8 ms |
| tabulate counts, numeric 50 bins | 6.6 ms |
| plot_histo build (ggplot object), numeric | 22.3 ms |
| plot_histo build (ggplot object), categorical | 25.7 ms |
| plot_histo build+render to png, numeric | 245 ms |
| OLD: geom_histogram over 337k raw rows (build+render) | 920 ms |

Reading:

- The per-interaction **data** path is cheap: one slider drag = 1 mask
  recompute (~10–20 ms) + 1 leave-one-out combine (~15–60 ms) + per-plot
  tabulate (~7 ms). Well under the 150 ms target.
- Plot **rendering** now dominates: ~245 ms per plot on this box, ~3.7×
  faster than the old full-data geom_histogram (920 ms). Most of the
  remaining cost is fixed ggplot/grid + png device overhead, not data
  size — the ggplot object build is only ~22 ms.
- Future lever if plots ever feel sluggish with many panels: the ragg
  device, or swapping plot_histo internals to base graphics `barplot`
  (identical look achievable) — revisit only if it matters in practice.
- Architectural win not visible in this table: the old module re-rendered
  **every** widget (renderUI teardown) and re-filtered the full frame per
  plot on every input; the merged module recomputes exactly one mask and
  redraws k plots, nothing else.
