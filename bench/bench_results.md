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

## 2026-08-04 — Phase B: memory vs tall/skinny SQLite (flights)

`Rscript bench/bench_backends.R`, db = 6,352,149 long rows, 282 MB.

| operation | memory | sqlite |
|---|---|---|
| backend open (registry read) | — | 340 ms |
| get_column_info, all 19 cols | 322 ms (first scan) | 2.2 ms (registry) |
| get_column(dep_delay) | ~0 ms | 203 ms |
| get_column(distance) | ~0 ms | 210 ms |
| get_column(carrier) | ~0 ms | 227 ms |
| get_column(dest) | ~0 ms | 231 ms |
| make_mask on fetched column | 8.5 ms | 8.5 ms (identical) |

Reading:

- Project.md's hypothesis confirmed: the backends differ **only** in the
  one-time ~200–230 ms column fetch when a variable is first selected.
  Every per-interaction cost afterwards (masks, leave-one-out, plots) is
  identical by construction, because the module filters cached columns.
- The registry pays off immediately: widget metadata (ranges, levels,
  NA counts) is ~146× faster from the registry than from a first
  in-memory scan, because it was precomputed at build time.
- The `(column_name, row_id)` index is what keeps the fetch at ~200 ms
  over a 6.4M-row long table; without it, fetches are full scans.

## 2026-08-04 — Phase D: NYC taxi 2023, 38,310,226 rows, aggregate mode

Identical tall/skinny schema in both engines. `Rscript bench/bench_big.R`.

Build times (12 parquet months, ~607 MB compressed):

| step | SQLite (R melt via arrow/data.table) | DuckDB (all-SQL) |
|---|---|---|
| build | 559 s | 94 s |
| index | 690 s | 64 s |
| file size | 27.8 GB (460M long rows*) | 5.9 GB (494M long rows) |

*The SQLite build initially missed `airport_fee` for months that name it
`Airport_fee` (case-sensitive R vs case-insensitive DuckDB); the build
script now normalizes the name. Benchmark columns were unaffected.

Per-interaction aggregate queries (2 filters active):

| operation | DuckDB | SQLite |
|---|---|---|
| get_binned numeric (GROUP BY over filtered rows) | 477 ms | 75.5 s |
| get_binned categorical | 229 ms | 50.7 s |
| get_count | 361 ms | 45.1 s |
| get_column full fetch (38M values) | 3.3 s | 28.3 s |
| get_row_mask | 0.9 s | 55.5 s |

Reading:

- **DuckDB is the Phase D answer.** Sub-second aggregate queries make
  the module interactive at 38M rows (with the 500 ms debounce, a
  slider drag settles in roughly a second per histogram); its columnar,
  parallel execution fits the tall/skinny row-set intersections.
- **SQLite does not survive this scale in aggregate mode**: every query
  is a b-tree walk over hundreds of millions of index entries on one
  core; 45–75 s per query is two orders of magnitude off. SQLite
  remains perfectly good as the column-store backend up to roughly
  flights scale (Phase B numbers above).
- The same module code drives both; the only change between demo apps
  is the backend constructor. That was the point of the contract.
