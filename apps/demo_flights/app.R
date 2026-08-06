################################################################
## Thanos demo: nycflights13::flights (~337k rows) via backend_memory.
## Run from the repo root or this directory:
##   shiny::runApp("apps/demo_flights")
################################################################
library(shiny)
library(ggplot2)
library(nycflights13)

## load Thanos: prefer the installed package; fall back to sourcing the
## repo-root loader (publics land here, internals stay private either way)
if (requireNamespace("thanos", quietly = TRUE)) {
    library(thanos)
} else {
    thanos_loader <- Filter(file.exists,
                            file.path(c(".", "..", "../.."), "thanos.R"))[1]
    if (is.na(thanos_loader)) {
        stop("install the thanos package or run from the repo checkout")
    }
    source(thanos_loader)
}

flights_df <- as.data.frame(nycflights13::flights)
backend <- backend_memory(flights_df)

ui <- fluidPage(
    titlePanel("Thanos: flights"),
    sidebarLayout(
        sidebarPanel(width = 5,
            thanosUI("thanos")
        ),
        mainPanel(width = 7,
            h4(textOutput("count")),
            tableOutput("head")
        )
    )
)

server <- function(input, output, session) {
    th <- thanosServer("thanos", backend,
                       default_selected = c("carrier", "origin", "dest",
                                            "dep_delay", "arr_delay",
                                            "distance", "month", "hour"))
    output$count <- renderText({
        sprintf("%s of %s rows pass filters",
                format(th$n_selected(), big.mark = ","),
                format(backend$n_rows(), big.mark = ","))
    })
    output$head <- renderTable({
        flights_df[head(th$rows(), 12),
                   c("year", "month", "day", "carrier", "flight",
                     "origin", "dest", "dep_delay", "arr_delay",
                     "distance")]
    })
}

shinyApp(ui, server)
