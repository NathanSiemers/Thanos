################################################################
## Thanos demo: nycflights13::flights (~337k rows) via backend_memory.
## Run from the repo root or this directory:
##   shiny::runApp("apps/demo_flights")
################################################################
library(shiny)
library(ggplot2)
library(nycflights13)

thanos_r_dir <- Filter(function(p) file.exists(file.path(p, "thanos_module.R")),
                       c("R", "../R", "../../R"))[1]
if (is.na(thanos_r_dir)) stop("cannot locate the Thanos R/ directory")
invisible(lapply(list.files(thanos_r_dir, pattern = "[.]R$", full.names = TRUE),
                 source))

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
        head(flights_df[th$rows(),
                        c("year", "month", "day", "carrier", "flight",
                          "origin", "dest", "dep_delay", "arr_delay",
                          "distance")], 12)
    })
}

shinyApp(ui, server)
