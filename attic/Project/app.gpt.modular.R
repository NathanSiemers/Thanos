library(shiny)
library(ggplot2)

###########################################
# 1) The Module UI
###########################################
dynamicFilterModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        12,
        # We'll populate choices in the module server with updateSelectizeInput
        selectizeInput(
          inputId = ns("vars"),
          label   = "Select columns:",
          choices = NULL,  # updated dynamically
          multiple = TRUE
        )
      )
    ),
    
    uiOutput(ns("varPanels")),
    
    fluidRow(
      column(12, tableOutput(ns("filteredTable")))
    )
  )
}

###########################################
# 2) The Module Server
###########################################
dynamicFilterModuleServer <- function(
    id,
    get_data    = reactive(function() iris),
    get_columns = reactive(function() names(iris))
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A reactiveValues store where we keep each filter’s current setting.
    # e.g. rv$filters[["Sepal.Length"]] = c(4.3, 7.9) if numeric
    rv <- reactiveValues(filters = list())
    
    # 2.1) Populate the selectizeInput with columns
    observe({
      # Evaluate get_columns(); should return a vector of colnames
      cols <- get_columns()
      if (length(cols) == 0) {
        updateSelectizeInput(session, "vars", choices = character(0))
      } else {
        # For convenience, you might pick no selection or a default
        updateSelectizeInput(session, "vars", choices = cols, selected = cols[1], server = TRUE)
      }
    })
    
    # 2.2) Whenever user removes a variable from 'vars', clear out old stored values
    observeEvent(input$vars, {
      oldVars <- names(rv$filters)
      for (v in oldVars) {
        if (!v %in% input$vars) {
          rv$filters[[v]] <- NULL
        }
      }
    })
    
    # 2.3) Whenever a filter changes, store its new value in rv$filters
    observe({
      req(input$vars)
      for (v in input$vars) {
        filterId <- paste0("filter_", v)
        curVal   <- input[[filterId]]
        if (!is.null(curVal)) {
          rv$filters[[v]] <- curVal
        }
      }
    })
    
    # 2.4) A reactive that filters the data according to rv$filters
    filteredData <- reactive({
      df <- get_data()  # call the reactive data function
      req(df)
      varsChosen <- input$vars
      req(varsChosen)
      
      if (nrow(df) == 0 || length(varsChosen) == 0) {
        return(df)
      }
      
      for (varName in varsChosen) {
        if (!varName %in% names(df)) next
        
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
    
    # 2.5) Dynamically create one row of UI per selected variable:
    #      column(3) = the filter, column(9) = plotOutput
    output$varPanels <- renderUI({
      req(input$vars)
      df <- get_data()
      req(df)
      
      varsChosen <- input$vars
      theRows <- lapply(varsChosen, function(varName) {
        # skip if data doesn't have this column
        if (!varName %in% names(df)) return(NULL)
        
        columnData <- df[[varName]]
        filterId   <- paste0("filter_", varName)
        plotId     <- paste0("plot_", varName)
        
        # If we have a stored value in rv$filters[[varName]],
        # use that as the default. Otherwise, set the brand-new default.
        if (is.numeric(columnData)) {
          rng <- range(columnData, na.rm = TRUE)
          range_diff <- rng[2] - rng[1]
          step <- signif(range_diff / 100, digits = 1)
          if (step == 0) step <- 0.01
          
          min_val <- floor(rng[1] / step) * step
          max_val <- ceiling(rng[2] / step) * step
          
          valueToUse <- if (!is.null(rv$filters[[varName]])) {
            rv$filters[[varName]]
          } else {
            c(min_val, max_val)
          }
          
          filterInput <- sliderInput(
            inputId = ns(filterId),
            label   = paste("Filter", varName),
            min     = min_val,
            max     = max_val,
            value   = valueToUse,
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
            inputId  = ns(filterId),
            label    = paste("Filter", varName),
            choices  = levelsVec,
            selected = selectedToUse
          )
        }
        
        fluidRow(
          column(3, filterInput),
          column(9, plotOutput(ns(plotId), height = "300px"))
        )
      })
      
      do.call(tagList, theRows)
    })
    
    # 2.6) For each var, dynamically create a renderPlot to show histogram or bar chart
    observe({
      req(input$vars)
      df <- get_data()
      
      for (varName in input$vars) {
        local({
          myVar  <- varName
          plotId <- paste0("plot_", myVar)
          
          output[[plotId]] <- renderPlot({
            req(myVar, df)
            if (!myVar %in% names(df)) {
              plot.new(); text(0.5, 0.5, paste("Missing", myVar)); return()
            }
            # Use the filtered data in the plot
            dsub <- filteredData()
            if (is.numeric(df[[myVar]])) {
              ggplot(dsub, aes(x = .data[[myVar]])) +
                geom_histogram(bins = 30, fill = "skyblue", color = "white") +
                labs(title = paste("Histogram of", myVar))
            } else {
              ggplot(dsub, aes(x = .data[[myVar]])) +
                geom_bar(fill = "skyblue", color = "white") +
                labs(title = paste("Bar Plot of", myVar))
            }
          })
        })
      }
    })
    
    # 2.7) Return the reactive filtered data if the user wants to use it in main app
    return(filteredData)
  })
}

# Example main app that calls the module
exampleApp <- function() {
  ui <- fluidPage(
    titlePanel("Module Demo (Using iris)"),
    dynamicFilterModuleUI("myFilterModule")  # Insert the module's UI
  )
  
  server <- function(input, output, session) {
    # Provide the data and columns as reactive expressions
    dataReactive <- reactive({ iris })
    colsReactive <- reactive({ names(iris) })
    
    # Call the module, passing in get_data() and get_columns()
    filteredIris <- dynamicFilterModuleServer(
      id          = "myFilterModule",
      get_data    = dataReactive,
      get_columns = colsReactive
    )
    
    # If desired, do anything else with filteredIris() here
  }
  
  shinyApp(ui, server)
}

exampleApp()

