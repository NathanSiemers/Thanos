# App File: app.R

library(shiny)
source("dynamic_filter_module.R")


server <- function(input, output, session) {
  # Initialize the dynamic filter module
  filter_module <- DynamicFilterModuleServer(
    "filterModule",
    data = storms,
    defaultFilter = c("category", "wind", "lat", "long"),
    showTable = TRUE,  # We'll use a custom table display
    download = TRUE
  )
# Debugging the module return values
  observe({
  ##   print(class(filter_module$get_filtered_data_table))  # Should print "function"
     print(class(filter_module$get_filtered_data))       # Should print "function"
  ##   print(filter_module$get_filtered_data())            # Should print the filtered data as a data frame
   })
  
  
  # Use get_filtered_data to process or display filtered data
  output$custom_filtered_table <- DT::renderDataTable({
    filter_module$get_filtered_data_table()
  })
}

ui <- fluidPage(
  titlePanel("Dynamic Filter Shiny Module"),
  sidebarLayout(
    sidebarPanel(
      DynamicFilterModuleUI("filterModule"),  # Dynamic filter module UI
      ),
    mainPanel(
       ##DT::dataTableOutput("custom_filtered_table")  # Custom filtered data table
    )
  )
)


shinyApp(ui, server)
