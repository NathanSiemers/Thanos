################################################################
## Thanos demo: dplyr::storms -- small smoke test, and the visual
## parity target against the original root app.R.
##   shiny::runApp("apps/demo_storms")
################################################################
library(shiny)
library(ggplot2)
library(dplyr)

## one line loads Thanos: publics land here, internals stay in a
## private namespace (see R/thanos.R)
thanos_r_dir <- Filter(function(p) file.exists(file.path(p, "thanos.R")),
                       c("R", "../R", "../../R"))[1]
if (is.na(thanos_r_dir)) stop("cannot locate the Thanos R/ directory")
source(file.path(thanos_r_dir, "thanos.R"))

storms_df <- as.data.frame(storms)
backend <- backend_memory(storms_df)

ui <- fluidPage(
    titlePanel("Thanos: storms"),
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
                       default_selected = c("status", "category", "lat", "long"))
    output$count <- renderText({
        sprintf("%s of %s rows pass filters",
                format(th$n_selected(), big.mark = ","),
                format(backend$n_rows(), big.mark = ","))
    })
    output$head <- renderTable(head(storms_df[th$rows(), ], 12))
}

shinyApp(ui, server)
