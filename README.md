# Thanos — reusable cross-filtering for R/Shiny

Interactive filtering with live cross-filter histograms: pick columns,
get auto-generated filter widgets (sliders for numerics, checkboxes for
categoricals), and every variable's histogram updates instantly as you
filter — each one shows the rows passing all *other* filters, with this
variable's own selection overlaid in plasma colors (sel/unsel).

See `Project.md` for the project goals and roadmap.

## Layout

- `R/` — the sourceable module code
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
filter (the restriction is then visible in the rebuilt widget).

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

```r
invisible(lapply(list.files("R", pattern = "[.]R$", full.names = TRUE), source))

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
