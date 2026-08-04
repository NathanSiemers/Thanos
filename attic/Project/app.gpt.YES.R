library(shiny)
library(ggplot2)

###########################################
# 1) The Module UI
###########################################
dynamicFilterModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # A selectize input to pick columns from the dataset
    fluidRow(
      column(
        12,
        selectizeInput(
          inputId = ns("vars"),
          label   = "Select columns:",
          choices = NULL,   # We'll populate these in the server
          multiple = TRUE
        )
      )
    ),
    
    # Dynamically generated filters & plots
    uiOutput(ns("varPanels"))
  )
}

###########################################
# 2) The Module Server
###########################################
dynamicFilterModuleServer <- function(id,
                                      get_data,
                                      get_columns,
                                  default_selected = NULL) {
  # get_data(selectedCols) -> returns a data.frame with *all rows* but *only* the columns in selectedCols
  # get_columns() -> returns a vector of all possible column names
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A reactiveValues store for the user’s filter settings:
    # e.g., rv$filters[["Sepal.Length"]] = c(4.3,7.9) or rv$filters[["Species"]] = c("setosa","virginica")
    rv <- reactiveValues(filters = list())
    
    # 2.1) Populate the selectizeInput with all possible columns
    observe({
      cols <- get_columns()  # user-provided function returning vector of column names
      # Filter default_selected so we only select columns that actually exist
      if (!is.null(default_selected)) {
        default_sel <- intersect(default_selected, cols)
      } else {
        default_sel <- character(0)
      }
      
      updateSelectizeInput(session, "vars",
                           choices  = cols,
                           selected = default_sel,
                           server = TRUE
      )
      
    })
    
    # 2.2) Whenever user *removes* a variable, clear out stored filter values
    observeEvent(input$vars, {
      oldVars <- names(rv$filters)
      for (v in oldVars) {
        if (!v %in% input$vars) {
          rv$filters[[v]] <- NULL
        }
      }
    })
    
    # 2.3) Keep track of user inputs for each filter in rv$filters
    observe({
      req(input$vars)
      for (v in input$vars) {
        filterId <- paste0("filter_", v)
        curVal <- input[[filterId]]
        if (!is.null(curVal)) {
          rv$filters[[v]] <- curVal
        }
      }
    })
    
    # 2.4) Build the dynamic UI for each selected variable:
    #      one row with (filter UI) and (plot).
    output$varPanels <- renderUI({
      req(input$vars)
      selectedVars <- input$vars
      
      # We'll fetch the data for just those columns (plus row index).
      # For an actual DB, get_data(selectedVars) should return all rows but only the columns needed for filtering.
      dfFilter <- get_data(selectedVars)
      validate(
        need(nrow(dfFilter) > 0, "No rows returned. Possibly an empty table?")
      )
      
      # For each chosen variable, create (filter, plot)
      theRows <- lapply(selectedVars, function(varName) {
        columnData <- dfFilter[[varName]]
        filterId   <- paste0("filter_", varName)
        plotId     <- paste0("plot_", varName)
        
        # For numeric columns: slider
        if (is.numeric(columnData)) {
          rng <- range(columnData, na.rm = TRUE)
          range_diff <- rng[2] - rng[1]
          
          step <- signif(range_diff / 100, digits = 1)
          if (is.na(step) || step == 0) step <- 0.01
          
          min_val <- floor(rng[1] / step) * step
          max_val <- ceiling(rng[2] / step) * step
          
          # If we already have a stored value for this var, use it; otherwise full range
          storedVal <- rv$filters[[varName]]
          if (is.null(storedVal)) {
            storedVal <- c(min_val, max_val)
          }
          
          filterUI <- sliderInput(
            inputId = ns(filterId),
            label   = paste("Filter", varName),
            min     = min_val,
            max     = max_val,
            value   = storedVal,
            step    = step
          )
          
        } else {
          # For factor/character columns: checkboxes
          levelsVec <- sort(unique(columnData))
          storedVal <- rv$filters[[varName]]
          if (is.null(storedVal)) {
            storedVal <- levelsVec
          }
          
          filterUI <- checkboxGroupInput(
            inputId  = ns(filterId),
            label    = paste("Filter", varName),
            choices  = levelsVec,
            selected = storedVal
          )
        }
        
        fluidRow(
          column(3, filterUI),
          column(9, plotOutput(ns(plotId), height = "100px"))
        )
      })
      
      do.call(tagList, theRows)
    })
    
    # 2.5) Create a reactive that computes a TRUE/FALSE mask for *all rows* of the data,
    #      using only the selected columns. The length of the mask = nrow(dfFilter).
    #      Also produce a subset with just the selected columns, filtered.
    #      We'll return both as separate reactives.
    filterMask <- reactive({
      selectedVars <- input$vars
      dfFilter <- get_data(selectedVars)
      n <- nrow(dfFilter)
      if (n == 0) {
        return(logical(0))
      }
      
      # Start with all TRUE, then narrow down
      keep <- rep(TRUE, n)
      
      for (varName in selectedVars) {
        colData  <- dfFilter[[varName]]
        filterVal <- rv$filters[[varName]]
        if (is.null(filterVal)) {
          next
        }
        
        if (is.numeric(colData)) {
          keep <- keep & (colData >= filterVal[1] & colData <= filterVal[2])
        } else {
          keep <- keep & (colData %in% filterVal)
        }
      }
      keep
    })
    
    filteredData <- reactive({
      # Return only the selected columns
      selectedVars <- input$vars
      if (length(selectedVars) == 0) {
        return(data.frame())
      }
      dfFilter <- get_data(selectedVars)
      mask <- filterMask()
      if (length(mask) == 0) {
        return(dfFilter[FALSE, , drop = FALSE]) # empty
      }
      dfFilter[mask, , drop = FALSE]
    })
    
    # 2.6) Dynamically render plots for each variable, using the *filteredData()*
    observe({
      req(input$vars)
      # Force reactivity on each filter
      for (v in input$vars) {
        input[[paste0("filter_", v)]]
      }
      
      # for each var, create a renderPlot
      for (varName in input$vars) {
        local({
          myVar <- varName
          plotId <- paste0("plot_", myVar)
          
          output[[plotId]] <- renderPlot({
            df <- filteredData()
            if (nrow(df) == 0) {
              plot.new()
              text(0.5, 0.5, "No data after filtering")
              return()
            }
            
            if (is.numeric(df[[myVar]])) {
              ggplot(df, aes(x = .data[[myVar]])) +
                geom_histogram(bins = 100, fill = "darkblue", color = "white") +
                labs(title = paste("Histogram of", myVar))
            } else {
              print(paste("non-numeric data:", myVar))
              print(table(df[[myVar]]))
              ggplot(df, aes(x = .data[[myVar]])) +
                geom_bar(fill = "darkblue", color = "white") +
                labs(title = paste("Bar Plot of", myVar))
            }
          })
        })
      }
    })
    
    # 2.7) Return a list of functions so the parent app can retrieve the mask and filtered data
    list(
      getFilterMask = function() {
        filterMask()
      },
      getFilteredData = function() {
        filteredData()
      }
    )
  })
}

###########################################
# 3) Example usage in an App
###########################################
# Suppose we have a big data set or DB. We'll simulate with 'iris' as a stand-in.
# We'll define:
#   get_columns() -> all colnames of iris
#   get_data(cols) -> returns a data.frame with all rows but only the requested columns

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