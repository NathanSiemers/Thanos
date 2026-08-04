source("thanos.R")

exampleApp <- function() {
  # Our 'database' is just iris, but imagine it is huge or remote
  library(dplyr)
  bigData <- storms
  
  get_columns <- reactive({
    names(storms)  # in a real DB scenario, we might query metadata
  })
  
  get_data <- function(selectedCols) {
    # Return all rows, only the columns we actually need:
    # plus we might add row IDs if needed for indexing
    bigData[, selectedCols, drop = FALSE]
  }
  
  ui <- fluidPage(
    titlePanel("Example: Dynamic Filter Module with T/F Mask"),
    dynamicFilterModuleUI("myFilter"),
    fluidRow(
      column(6, tableOutput("debugFiltered")),
      column(6, verbatimTextOutput("debugMask"))
    )
  )
  
  server <- function(input, output, session) {
    mod <- dynamicFilterModuleServer(
      id          = "myFilter",
      get_data    = get_data,
      get_columns = get_columns,
      default_selected = c("lat")
    )
    
    # (1) retrieve the T/F mask
    # (2) retrieve the filtered subset (just the selected columns)
    output$debugFiltered <- renderTable({
      head(mod$getFilteredData(), 10)
    })
    
    output$debugMask <- renderPrint({
      head(mod$getFilterMask(), 20) # show the first 20 elements of the mask
    })
  }
  
  shinyApp(ui, server)
}
# Run exampleApp() to test

exampleApp()