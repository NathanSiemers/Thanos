# Thanos — reusable cross-filtering for R/Shiny

Interactive filtering with live cross-filter histograms: pick columns,
get auto-generated filter widgets (sliders for numerics, checkboxes for
categoricals), and every variable's histogram updates instantly as you
filter — each one shows the rows passing all *other* filters, with this
variable's own selection overlaid in plasma colors (sel/unsel).

See `Project.md` for the project goals and roadmap.

## Layout

- `R/` — the sourceable module code
  - `thanos.R` — **the loader: `source()` this one file.** Defines the
    public API (`thanosUI`, `thanosServer`, `backend_*`) plus `thanos`,
    a handle to the private namespace holding all internals
  - `thanos_module.R` — `thanosUI()` / `thanosServer()` (the module)
  - `thanos_backend.R` — backend contract + `backend_memory(df)`
  - `thanos_backend_sqlite.R` — `backend_dbi()` over the tall/skinny
    schema, with `backend_sqlite()` / `backend_duckdb()` wrappers and
    the SQL aggregation capability for very large data
  - `thanos_plot.R` — pure helpers: `make_mask`, `bin_column`, `plot_histo`
  - `thanos_theme.R` — visual identity (compact theme, plasma fills)
- `apps/` — runnable demos: `demo_flights/`, `demo_storms/`,
  `demo_sqlite/` (DB-backed), `grapher/` (embedding), `demo_big/`
  (~38M-row NYC taxi, aggregate mode)
- `db/` — database build scripts; built `.sqlite` files live in
  `db/data/` (gitignored)
- `bench/` — benchmark scripts and `bench_results.md` (running log)
- `tests/` — plain-Rscript tests
- `attic/` — all historical experiments, preserved untouched

## Run

```r
shiny::runApp("apps/demo_flights")   # nycflights13::flights, ~337k rows
shiny::runApp("apps/demo_storms")    # dplyr::storms, small smoke test
shiny::runApp("apps/demo_sqlite")    # flights from tall/skinny SQLite
shiny::runApp("apps/grapher")        # scatter app embedding Thanos
shiny::runApp("apps/demo_big")       # 38M-row taxi data, aggregate mode
```

Databases for the last two demos are built with:

```sh
Rscript db/build_flights_sqlite.R    # flights -> db/data/flights.sqlite
# download parquet months into db/data/tlc/ first (see script header):
Rscript db/build_big_duckdb.R        # taxi -> db/data/taxi.duckdb (fast)
Rscript db/build_big_sqlite.R        # taxi -> db/data/taxi.sqlite
```

Deselecting a column removes its filtering **completely** — the stored
settings are forgotten too, so there is never a ghost filter from a
column that is no longer visible. Pass `remember_removed = TRUE` to
`thanosServer()` if you want a re-added column to restore its previous
filter (the restriction is then visible in the rebuilt widget). A small
in-page note under the column picker tells users which behavior is
active (`removal_note = FALSE` hides it).

Numeric columns with few distinct values (`month`, `hour`, ratings…)
get **checkboxes with membership semantics** instead of a slider; the
threshold is `thanosServer(max_discrete_numeric = 12)`. Their
histograms show one bar per value, in numeric order.

Sliders and histogram breaks use an **outlier-robust display range**
(the 0.1%–99.9% quantiles, from the backend/registry) so a few absurd
values can't crush the real distribution into one bin. Outliers clamp
into the edge bins, and a slider handle resting **at an endpoint means
"unbounded on that side"** — no rows are silently dropped by the
robust bounds.

Non-negative numeric columns get a small **log2 scale** toggle: the
slider and histogram move to log2(x+1) space (good for skewed data
like fares), while the actual filter — and everything a parent app
sees via `filters()`/`rows()` — stays in raw units. In aggregate mode
the toggle appears only if the SQL engine has `log2()` (DuckDB yes,
stock RSQLite no).

**Plot engine**: histograms are drawn by a base-graphics renderer by
default (`thanosServer(plot_engine = "base")`) — the identical visual
at ~24 ms per plot instead of ggplot's ~280 ms, making a full 8-plot
interaction ~10× faster (see `bench/bench_plots.R`). Pass
`plot_engine = "ggplot"` to use the grid pipeline instead.

**Reactive hygiene** (all handled inside the module, parent apps need
nothing): slider inputs are debounced (`debounce_ms`, default 300 ms)
and so are checkbox groups (`debounce_checkbox_ms`, default 300 ms —
rapidly ticking half a dozen boxes coalesces into one recomputation),
and Shiny reactives pull the *latest* value rather than replaying a
queue, so the module can never fall progressively behind; at most one
stale render can be in flight. While a plot recalculates it pulses
gently — CSS scoped to the module's own panels, so a host app is
never restyled.

**Caching** (DB backends, on by default): fetched columns are kept for
the backend's lifetime and aggregate query results are memoised
(bounded, oldest evicted), so re-selecting a column, a parent app
re-reading columns per interaction, and revisited filter states cost
nothing after first touch (flights: 213 ms cold → ~0 ms warm). The
cache assumes the database is immutable while open — construct with
`cache = FALSE` for mutable data (or to cap memory when fetching
38M-row columns), call `backend$clear_cache()` after a data change,
and inspect `backend$cache_stats()` for hits/misses/bytes.
`backend_memory` needs no cache (the data is already in RAM).

## Two execution modes

`thanosServer()` picks a mode automatically (`mode = "auto"`):

- **vector** (default at small/medium scale): columns are fetched once
  per selection and cached; filtering is boolean vector algebra in R;
  histograms tabulate pre-binned indices. Backend-agnostic.
- **aggregate** (DB backends, > `aggregate_threshold` rows): no column
  vector ever enters R. Histogram counts, row counts, and the row mask
  are SQL queries composed by the backend from the module's filter
  state. Semantics are identical (verified by tests/test-aggregate.R).

## Test / bench

```sh
Rscript tests/test-plot-helpers.R    # pure helpers
Rscript tests/test-module.R          # module logic via shiny::testServer
Rscript tests/test-backend.R         # memory/sqlite backend equivalence
Rscript tests/test-aggregate.R       # SQL aggregation == R vector algebra
Rscript tests/test-grapher.R         # embedding integration
Rscript bench/bench_masks.R          # per-interaction data path
Rscript bench/bench_backends.R       # memory vs sqlite (flights)
Rscript bench/bench_big.R            # sqlite vs duckdb (taxi, 38M rows)
```

## Embedding Thanos in your own app

**The fully annotated integration reference is `apps/grapher/app.R`** —
an independent scatter-plot app that acquired Thanos filtering at four
marked "PLUG-IN POINT" comments (source the code → wrap your data in a
backend → `thanosUI()` in the layout → `thanosServer()` in the server),
with the parent/module interaction contract documented inline. Short
version:

```r
source("R/thanos.R")   # one file; publics only, internals stay in a
                       # private namespace (no name collisions with
                       # your app in either direction)

backend <- backend_memory(my_data_frame)      # or a DB backend

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(thanosUI("thanos")),
        mainPanel(...)
    )
)
server <- function(input, output, session) {
    th <- thanosServer("thanos", backend,
                       default_selected = c("some", "columns"))
    ## th$rows() is an integer vector of row IDs passing all filters —
    ## a cheap pointer, not a copy of the data:
    output$myplot <- renderPlot({
        plot(my_data_frame[th$rows(), c("x", "y")])
    })
}
```

The module never hands your app a filtered copy of the data; it hands
you `rows()` (or `mask()`), and you subset whatever you need yourself.

One optional call flows the other way: `th$add_vars(cols)` asks Thanos
to include columns in its filter selection (additive, idempotent) —
the grapher uses it to keep its plotted axes filterable, so their NAs
and ranges are always user-controllable.
