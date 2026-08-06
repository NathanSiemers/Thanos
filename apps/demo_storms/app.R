################################################################
## Thanos demo: dplyr::storms -- small smoke test, and the visual
## parity target against the original root app.R.
##   shiny::runApp("apps/demo_storms")
################################################################
library(shiny)
library(ggplot2)
library(dplyr)

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
    output$head <- renderTable(storms_df[head(th$rows(), 12), ])
}

shinyApp(ui, server)
