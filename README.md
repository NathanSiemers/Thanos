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
  - `thanos_plot.R` — pure helpers: `make_mask`, `bin_column`, `plot_histo`
  - `thanos_theme.R` — visual identity (compact theme, plasma fills)
- `apps/` — runnable demos: `demo_flights/`, `demo_storms/` (more coming:
  `demo_sqlite/`, `grapher/`, `demo_big/`)
- `db/` — database build scripts; built `.sqlite` files live in
  `db/data/` (gitignored)
- `bench/` — benchmark scripts and `bench_results.md` (running log)
- `tests/` — plain-Rscript tests
- `attic/` — all historical experiments, preserved untouched

## Run

```r
shiny::runApp("apps/demo_flights")   # nycflights13::flights, ~337k rows
shiny::runApp("apps/demo_storms")    # dplyr::storms, small smoke test
```

## Test / bench

```sh
Rscript tests/test-plot-helpers.R
Rscript tests/test-module.R
Rscript bench/bench_masks.R
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
