
library(shiny)

ui <- fluidPage(
  thanosModule("thanos1")$ui
)

server <- function(input, output) {
  # Initialize the module with your data
  callModule(thanosModule, "thanos1")
}

shinyApp(ui = ui, server = server)



