library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dynamic Filtering Without reactiveValues"),
  sidebarLayout(
    sidebarPanel(
      # 1) Select columns from the iris data
      selectizeInput("vars", "Select columns:", 
                     choices = names(dplyr::storms), multiple = TRUE),
      
      # 2) A placeholder to dynamically create filter UIs
      uiOutput("dynamicFilters")
    ),
    mainPanel(
      # A single plot that depends on the current filtered data
      plotOutput("plotFiltered"),
      
      # Optionally, show the filtered data table (or just a head)
      tableOutput("filteredTable")
    )
  )
)

server <- function(input, output, session) {
  
  # 1) Render dynamic sliders/checkboxes for each chosen variable
  output$dynamicFilters <- renderUI({
    req(input$vars)  # Must have at least one variable selected
    
    # lapply(...) returns a list of UI elements for each selected column
    lapply(input$vars, function(varName) {
      # We'll check if varName is numeric or categorical
      columnData <- iris[[varName]]
      
      if (is.numeric(columnData)) {
        # For numeric data, create a slider from min to max
        rng <- range(columnData, na.rm = TRUE)
        sliderInput(
          inputId = paste0("filter_", varName),
          label   = paste("Filter", varName),
          min     = rng[1],
          max     = rng[2],
          value   = rng    # start at full range
        )
        
      } else {
        # For factor/character data, create a checkbox group
        levelsVec <- sort(unique(columnData))
        checkboxGroupInput(
          inputId = paste0("filter_", varName),
          label   = paste("Filter", varName),
          choices = levelsVec,
          selected= levelsVec  # by default, select all
        )
      }
    })
  })
  
  # 2) Build a reactive that applies all filters to the iris data
  filteredData <- reactive({
    df <- iris
    
    # For each selected variable, read the corresponding input
    for (varName in input$vars) {
      columnData <- df[[varName]]
      filterInputId <- paste0("filter_", varName)
      filterVal <- input[[filterInputId]]  # could be a range or a set of categories
      
      if (is.numeric(columnData)) {
        # filterVal is something like c(min, max)
        df <- df[columnData >= filterVal[1] & columnData <= filterVal[2], ]
      } else {
        # filterVal is a set of categories
        df <- df[columnData %in% filterVal, ]
      }
    }
    df
  })
  
  # 3) Show a plot of the filtered data. For illustration:
  #    we pick the first numeric variable in input$vars, if any, and make a histogram
  output$plotFiltered <- renderPlot({
    df <- filteredData()
    numericVars <- input$vars[sapply(input$vars, function(v) is.numeric(iris[[v]]))]
    
    if (length(numericVars) == 0) {
      plot.new()
      text(0.5, 0.5, "No numeric columns selected.")
      return()
    }
    # Example: plot the histogram of the *first* numeric variable
    ggplot(df, aes(x = .data[[numericVars[1]]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "white") +
      ggtitle(paste("Histogram of", numericVars[1]))
  })
  
  # Optionally show the filtered data table or partial preview
  output$filteredTable <- renderTable({
    head(filteredData(), 10)
  })
}

shinyApp(ui, server)
