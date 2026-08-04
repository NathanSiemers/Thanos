library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dynamic Filtering & Side-by-Side Plots (Preserving State)"),
  
  fluidRow(
    column(12,
           selectizeInput("vars", "Select columns:",
                          choices = names(iris), multiple = TRUE)
    )
  ),
  
  # For each selected variable, we will display:
  #   1) The filter (slider or checkbox),
  #   2) The associated plot (histogram or bar chart).
  uiOutput("varPanels"),
  
  fluidRow(
    column(12, tableOutput("filteredTable"))
  )
)

server <- function(input, output, session) {
  
  # A reactiveValues store where we keep each filter’s current setting
  # e.g. rv$filters[["Sepal.Length"]] = c(4.3, 7.9) if numeric
  #      rv$filters[["Species"]] = c("setosa","virginica") if factor
  rv <- reactiveValues(filters = list())
  
  # Whenever user removes a variable, clear out old stored values
  observeEvent(input$vars, {
    # remove any filter in rv$filters that is NOT in input$vars
    oldVars <- names(rv$filters)
    for (v in oldVars) {
      if (!v %in% input$vars) {
        rv$filters[[v]] <- NULL
      }
    }
  })
  
  # Whenever a filter changes, store its new value in rv$filters
  # e.g., if filter_Sepal.Length is changed, then store that numeric range.
  observe({
    req(input$vars)  # only run if user has at least one var selected
    for (v in input$vars) {
      filterId <- paste0("filter_", v)
      curVal <- input[[filterId]]
      if (!is.null(curVal)) {
        rv$filters[[v]] <- curVal
      }
    }
  })
  
  # Reactive: filter the data according to the stored user selections in rv$filters
  filteredData <- reactive({
    df <- iris
    req(input$vars)
    
    for (varName in input$vars) {
      columnData <- df[[varName]]
      filterVal  <- rv$filters[[varName]]
      
      if (is.numeric(columnData)) {
        # filterVal = c(min, max)
        df <- df[columnData >= filterVal[1] & columnData <= filterVal[2], ]
      } else {
        # filterVal is a set of selected categories
        df <- df[columnData %in% filterVal, ]
      }
    }
    df
  })
  
  # Dynamically create one row of UI per selected variable:
  #   column(3) = the filter,
  #   column(9) = plotOutput
  output$varPanels <- renderUI({
    req(input$vars)
    
    theRows <- lapply(input$vars, function(varName) {
      columnData <- iris[[varName]]
      filterId   <- paste0("filter_", varName)
      plotId     <- paste0("plot_", varName)
      
      # If we have a stored value in rv$filters[[varName]],
      # use that as the default. Otherwise, set the brand-new default.
      if (is.numeric(columnData)) {
        rng <- range(columnData, na.rm = TRUE)
        
        range_diff <- rng[2] - rng[1]
        
        # 1) Pick a step that is ~1/100 of the range, rounded to 1 significant digit.
        #    E.g., if the range is 0 to 500, step = 5; if 3.141 to 3.867, step ~ 0.01.
        step <- signif(range_diff / 100, digits = 1)
        # In case range_diff is tiny or zero, fall back to a small but nonzero step:
        if (step == 0) step <- 0.01
        
        # 2) Round down the minimum and round up the maximum to that step.
        min_val <- floor(rng[1] / step) * step
        max_val <- ceiling(rng[2] / step) * step
            
        valueToUse <- if (!is.null(rv$filters[[varName]])) {
          rv$filters[[varName]]
        } else {
          new_range = c(min_val, max_val)
          new_range
        }
        
        # 3) Use them in sliderInput
        filterInput <- sliderInput(
          inputId = filterId,
          label   = paste("Filter", varName),
          min     = min_val,
          max     = max_val,
          value   = valueToUse,  # Full range initially
          step    = step
        )
      } else {
        levelsVec <- sort(unique(columnData))
        selectedToUse <- if (!is.null(rv$filters[[varName]])) {
          rv$filters[[varName]]
        } else {
          levelsVec
        }
        filterInput <- checkboxGroupInput(
          inputId  = filterId,
          label    = paste("Filter", varName),
          choices  = levelsVec,
          selected = selectedToUse
        )
      }
      
      fluidRow(
        column(3, filterInput),
        column(9, plotOutput(plotId, height = "300px"))
      )
    })
    do.call(tagList, theRows)
  })
  
  # For each var, dynamically create a renderPlot to show histogram or bar chart
  observe({
    req(input$vars)
    
    for (varName in input$vars) {
      local({
        myVar  <- varName
        plotId <- paste0("plot_", myVar)
        
        output[[plotId]] <- renderPlot({
          df <- filteredData()
          if (is.numeric(iris[[myVar]])) {
            ggplot(df, aes(x = .data[[myVar]])) +
              geom_histogram(bins = 30, fill = "skyblue", color = "white") +
              labs(title = paste("Histogram of", myVar))
          } else {
            ggplot(df, aes(x = .data[[myVar]])) +
              geom_bar(fill = "skyblue", color = "white") +
              labs(title = paste("Bar Plot of", myVar))
          }
        })
      })
    }
  })
  
  output$filteredTable <- renderTable({
    head(filteredData(), 10)
  })
}

shinyApp(ui, server)
