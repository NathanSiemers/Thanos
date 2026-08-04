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
          choices = NULL,  # populated by server
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
#
# Arguments:
#   get_data(selectedCols) -> returns a data.frame with all rows but only the chosen columns
#   get_columns()         -> returns a character vector of all possible columns
#   default_selected      -> optional vector of column names to select at startup
#
# This version includes an 'Include NA?' checkbox for each variable,
# showing the number of NA rows in that column.
dynamicFilterModuleServer <- function(id,
                                      get_data,
                                      get_columns,
                                      default_selected = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # A reactiveValues store for:
    #   rv$filters[[varName]]   -> numeric range or factor levels
    #   rv$includeNA[[varName]] -> TRUE/FALSE for including NA
    rv <- reactiveValues(
      filters   = list(),
      includeNA = list()
    )
    
    ###########################################
    # 2.1) Populate the selectizeInput with all possible columns
    observe({
      cols <- get_columns()
      if (!is.null(default_selected)) {
        default_sel <- intersect(default_selected, cols)
      } else {
        default_sel <- character(0)
      }
      
      updateSelectizeInput(
        session, "vars",
        choices  = cols,
        selected = default_sel,
        server   = TRUE
      )
    })
    
    ###########################################
    # 2.2) Whenever user removes a variable, clear out stored filter values
    observeEvent(input$vars, {
      oldVars1 <- names(rv$filters)
      for (v in oldVars1) {
        if (!v %in% input$vars) {
          rv$filters[[v]] <- NULL
        }
      }
      oldVars2 <- names(rv$includeNA)
      for (v in oldVars2) {
        if (!v %in% input$vars) {
          rv$includeNA[[v]] <- NULL
        }
      }
    })
    
    ###########################################
    # 2.3) Track user-chosen filter input (slider/checkboxgroup) + NA checkboxes
    observe({
      req(input$vars)
      for (v in input$vars) {
        # 1) store numeric/factor filter
        filterId <- paste0("filter_", v)
        curVal   <- input[[filterId]]
        if (!is.null(curVal)) {
          rv$filters[[v]] <- curVal
        }
        
        # 2) store "include NA" checkbox
        includeNAId <- paste0("includeNA_", v)
        includeVal  <- input[[includeNAId]]
        if (!is.null(includeVal)) {
          rv$includeNA[[v]] <- includeVal
        }
      }
    })
    # 2.3b Observe the All/None links for factor variables
    #    For each factor var, if user clicks, we either select all or none.
    #    We'll define these observers each time input$vars changes (dynamic).

    observe({
      # For each selected variable
      for (varName in input$vars) {
        if (is.numeric(colData)) next
        selectedVars <- input$vars
        dfFilter <- get_data(selectedVars)
        colData = dfFilter[[varName]]
        ##cols <- get_columns()
        ##colData <- get_data(varName)
        # If that var is numeric, skip
        # Create a local scope so varName doesn't get overwritten in the loop
        local({
          v <- varName
          linkId <- paste0("allnone_", v)
          print(paste("we are evaluating allnone:", linkId))
          filterId <- paste0("filter_", v)
          allLevs <- sort(unique(get_columns()))
          # This observer triggers whenever user clicks the actionLink
          observeEvent(input[[linkId]], {
            print(paste("observeEvent, we are evaluating allnone:", linkId))
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

    ###########################################
    # 2.4) Build the dynamic UI for each selected variable
    #      one row with (filter UI) and (plot).
    #      Also a checkbox to toggle whether NA is included
    output$varPanels <- renderUI({
      req(input$vars)
      selectedVars <- input$vars
      
      dfFilter <- get_data(selectedVars)
      validate(
        need(nrow(dfFilter) > 0, "No rows returned (empty data).")
      )
      
      rowsForVars <- lapply(selectedVars, function(varName) {
        columnData <- dfFilter[[varName]]
        filterId   <- paste0("filter_", varName)
        plotId     <- paste0("plot_", varName)
        
        # NA info
        nNA <- sum(is.na(columnData))
        includeNAId <- paste0("includeNA_", varName)
        oldIncludeNA <- rv$includeNA[[varName]]
        if (is.null(oldIncludeNA)) {
          # default to FALSE? Or TRUE? You can pick:
          oldIncludeNA <- TRUE  # let's assume we keep NAs by default
        }
        
        # The "Include NA?" checkbox with a small note of how many are NA
        includeNAcheckbox <- checkboxInput(
          inputId = ns(includeNAId),
          label   = paste0("Include NA (N=", nNA, ")?"),
          value   = oldIncludeNA
        )
        
        # Build main filter UI: numeric => slider, factor => checkboxGroup
        if (is.numeric(columnData)) {
          rng <- range(columnData, na.rm = TRUE)
          range_diff <- rng[2] - rng[1]
          
          step <- signif(range_diff / 100, digits = 1)
          if (is.na(step) || step == 0) step <- 0.01
          
          min_val <- floor(rng[1] / step) * step
          max_val <- ceiling(rng[2] / step) * step
          
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
          return(
            fluidRow(
              column(3,
                     filterUI,
                     includeNAcheckbox
                     ),
              column(9, plotOutput(ns(plotId), height = "150px"))
            )
          )
        } else {
          # factor/character
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
          allnoneUI <- actionLink(
            inputId = paste0("allnone_", varName),
            label   = "All/None"
          )
          
        }
        return(
          
          fluidRow(
            column(3,
                   filterUI,
                   includeNAcheckbox,
                   allnoneUI
                   ),
            column(9, plotOutput(ns(plotId), height = "150px"))
          )
        )
      })
      
      do.call(tagList, rowsForVars)
    })
    
    ###########################################
    # 2.5) Create a reactive T/F mask for all rows
    filterMask <- reactive({
      selectedVars <- input$vars
      dfFilter <- get_data(selectedVars)
      n <- nrow(dfFilter)
      
      if (n == 0) return(logical(0))
      
      keep <- rep(TRUE, n)
      for (varName in selectedVars) {
        colData   <- dfFilter[[varName]]
        filterVal <- rv$filters[[varName]]
        incNA     <- rv$includeNA[[varName]]
        if (is.null(filterVal)) next
        
        # If incNA is TRUE, keep rows that have NA or pass the condition
        # If incNA is FALSE, exclude rows with NA
        if (is.numeric(colData)) {
          if (isTRUE(incNA)) {
            keep <- keep & (is.na(colData) | (colData >= filterVal[1] & colData <= filterVal[2]))
          } else {
            keep <- keep & (!is.na(colData) & colData >= filterVal[1] & colData <= filterVal[2])
          }
        } else {
          if (isTRUE(incNA)) {
            keep <- keep & (is.na(colData) | (colData %in% filterVal))
          } else {
            keep <- keep & (!is.na(colData) & colData %in% filterVal)
          }
        }
      }
      
      keep
    })
    
    ###########################################
    # 2.6) Build a reactive for the filtered data (only selected columns)
    filteredData <- reactive({
      selectedVars <- input$vars
      if (length(selectedVars) == 0) {
        return(data.frame())
      }
      dfFilter <- get_data(selectedVars)
      mask <- filterMask()
      if (length(mask) == 0) {
        return(dfFilter[FALSE, , drop = FALSE])
      }
      dfFilter[mask, , drop = FALSE]
    })
    
    ###########################################
    # 2.7) Dynamically render plots for each variable
    observe({
      req(input$vars)
      # Force reactivity on each filter
      for (v in input$vars) {
        input[[paste0("filter_", v)]]
        input[[paste0("includeNA_", v)]]
      }
      
      for (varName in input$vars) {
        local({
          myVar  <- varName
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
                geom_histogram(bins = 30, fill = "darkblue", color = "white") +
                labs(title = paste("Histogram of", myVar))
            } else {
              ggplot(df, aes(x = .data[[myVar]])) +
                geom_bar(fill = "darkblue", color = "white") +
                labs(title = paste("Bar Plot of", myVar))
            }
          })
        })
      }
    })
    
    ###########################################
    # 2.8) Return a list with two functions the parent app can call
    list(
      getFilterMask = function() { filterMask() },
      getFilteredData = function() { filteredData() }
    )
  })
}

###########################################
# Example Usage
###########################################
# You can test this module with a simple application, e.g.:
exampleApp <- function() {
  bigData <- storms  # or iris, or any data
  get_columns <- reactive({ names(bigData) })
  get_data <- function(cols) {
    bigData[, cols, drop = FALSE]
  }
  
  ui <- fluidPage(
    titlePanel("Dynamic Filter Module w/ NA Checkboxes"),
    dynamicFilterModuleUI("myFilter"),
    fluidRow(
      column(6, tableOutput("debugTable")),
      column(6, verbatimTextOutput("debugMask"))
    )
  )
  
  server <- function(input, output, session) {
    mod <- dynamicFilterModuleServer(
      id               = "myFilter",
      get_data         = get_data,
      get_columns      = get_columns,
      default_selected = c("status", "lat")  # example
    )
    
    output$debugTable <- renderTable({
      head(mod$getFilteredData(), 10)
    })
    
    output$debugMask <- renderPrint({
      head(mod$getFilterMask(), 20)
    })
  }
  
  shinyApp(ui, server)
}

# Run exampleApp() to see it in action:
# exampleApp()
