library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dynamic Filtering With Factor All/None (No reactiveValues)"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("vars", "Select columns:",
                     choices = names(iris), multiple = TRUE),
      uiOutput("dynamicFilters")  # placeholder for sliders/checkboxes + all/none
    ),
    mainPanel(
      plotOutput("plotFiltered"),
      tableOutput("filteredTable")
    )
  )
)

server <- function(input, output, session) {
  
  # 1) Dynamic creation of sliders/checkboxes (+ All/None link for factors)
  output$dynamicFilters <- renderUI({
    req(input$vars)  # must pick at least 1 column
    
    # For each chosen variable, build either:
    # - numeric: sliderInput
    # - factor/char: checkboxGroupInput + All/None link
    lapply(input$vars, function(varName) {
      colData <- iris[[varName]]
      
      if (is.numeric(colData)) {
        rng <- range(colData, na.rm = TRUE)
        sliderInput(
          inputId = paste0("filter_", varName),
          label   = paste("Filter", varName),
          min     = rng[1],
          max     = rng[2],
          value   = rng
        )
      } else {
        # factor/char => checkboxGroup + All/None link
        levs <- sort(unique(colData))
        
        # Return these two UI elements together in a small tagList
        tagList(
          checkboxGroupInput(
            inputId = paste0("filter_", varName),
            label   = paste("Filter", varName),
            choices = levs,
            selected = levs  # start with all selected
          ),
          actionLink(
            inputId = paste0("allnone_", varName),
            label   = "All/None"
          )
        )
      }
    })
  })
  
  # 2) Observe the All/None links for factor variables
  #    For each factor var, if user clicks, we either select all or none.
  #    We'll define these observers each time input$vars changes (dynamic).
  observe({
    # For each selected variable
    for (varName in input$vars) {
      # If that var is numeric, skip
      if (is.numeric(iris[[varName]])) next
      
      # Create a local scope so varName doesn't get overwritten in the loop
      local({
        v <- varName
        linkId <- paste0("allnone_", v)
        filterId <- paste0("filter_", v)
        allLevs <- sort(unique(iris[[v]]))
        
        # This observer triggers whenever user clicks the actionLink
        observeEvent(input[[linkId]], {
          currentSel <- input[[filterId]]  # what's currently selected
          
          # If user has everything selected, unselect all
          # Otherwise select them all
          if (setequal(currentSel, allLevs)) {
            updateCheckboxGroupInput(session, filterId, selected = character(0))
          } else {
            updateCheckboxGroupInput(session, filterId, selected = allLevs)
          }
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # 3) Build a reactive that applies all filters to the iris data
  filteredData <- reactive({
    df <- iris
    for (varName in input$vars) {
      colData <- df[[varName]]
      filterId <- paste0("filter_", varName)
      val <- input[[filterId]]
      
      if (is.numeric(colData)) {
        # val is c(min, max)
        df <- df[colData >= val[1] & colData <= val[2], ]
      } else {
        # val is a character vector of chosen factor levels
        df <- df[colData %in% val, ]
      }
    }
    df
  })
  
  # 4) Render a plot (just the first numeric var for demonstration)
  output$plotFiltered <- renderPlot({
    df <- filteredData()
    numericVars <- input$vars[sapply(input$vars, function(v) is.numeric(iris[[v]]))]
    
    if (length(numericVars) == 0) {
      plot.new()
      text(0.5, 0.5, "No numeric columns selected.")
      return()
    }
    ggplot(df, aes(x = .data[[numericVars[1]]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "white") +
      ggtitle(paste("Histogram of", numericVars[1]))
  })
  
  # 5) Show table of filtered data (first 10 rows)
  output$filteredTable <- renderTable({
    head(filteredData(), 10)
  })
}

shinyApp(ui, server)
