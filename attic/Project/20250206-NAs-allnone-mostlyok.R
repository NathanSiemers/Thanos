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
        # Allows user to pick columns from the dataset
        selectizeInput(
          inputId = ns("vars"),
          label   = "Select columns:",
          choices = NULL,  # Populated by server logic
          multiple = TRUE
        )
      )
    ),
    
    # A placeholder that will contain (filter UI + plot) for each var
    uiOutput(ns("varPanels"))
  )
}

###########################################
# 2) The Module Server
###########################################
#
#  get_data(selectedCols): returns a data.frame with *all rows* but only the chosen columns
#  get_columns(): returns a character vector of all possible columns
#  default_selected: optional vector of column names to select at startup
#
# This version includes:
# - All/None action link for factor variables
# - "Include NA?" checkboxes
# - Ensures toggling to "none" triggers all histograms to re-check the global subset
dynamicFilterModuleServer <- function(id,
                                      get_data,
                                      get_columns,
                                      default_selected = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # We'll store user-chosen filters in two lists:
    #   rv$filters[[varName]] -> numeric range or factor-level selection
    #   rv$includeNA[[varName]] -> TRUE/FALSE for whether to keep NA rows
    rv <- reactiveValues(
      filters   = list(),
      includeNA = list()
    )
    
    ###########################################
    # Step 1) Populate the selectizeInput with column choices
    observe({
      cols <- get_columns()
      if (!is.null(default_selected)) {
        # Only select columns that actually exist
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
    # Step 2) Remove old filters if the user unselects variables
    observeEvent(input$vars, {
      oldFilterVars <- names(rv$filters)
      for (v in oldFilterVars) {
        if (!v %in% input$vars) {
          rv$filters[[v]] <- NULL
        }
      }
      oldIncludeVars <- names(rv$includeNA)
      for (v in oldIncludeVars) {
        if (!v %in% input$vars) {
          rv$includeNA[[v]] <- NULL
        }
      }
    })
    
    ###########################################
    # Step 3) Observe main filter inputs & "Include NA?"
    observe({
      req(input$vars)
      for (v in input$vars) {
        # For numeric/factor filter
        filterId <- paste0("filter_", v)
        val <- input[[filterId]]
        if (!is.null(val)) {
          rv$filters[[v]] <- val
        }
        
        # For the "Include NA?" checkbox
        incId <- paste0("includeNA_", v)
        incVal <- input[[incId]]
        if (!is.null(incVal)) {
          rv$includeNA[[v]] <- incVal
        }
      }
    })
    
    ###########################################
    # Step 3b) Observe the All/None action links for factor variables
    observe({
      req(input$vars)
      dfSmall <- get_data(input$vars)  # so we can see factor vs numeric
      
      for (varName in input$vars) {
        local({
          v <- varName
          colData <- dfSmall[[v]]
          # If numeric, skip
          if (is.numeric(colData)) return(NULL)
          
          # The link and filter IDs
          linkId   <- paste0("allnone_", v)
          filterId <- paste0("filter_", v)
          allLevs  <- sort(unique(colData))
          
          observeEvent(input[[linkId]], {
            # The user clicked "All/None" for a factor var
            currentSel <- input[[filterId]]
            if (setequal(currentSel, allLevs)) {
              # Currently all => unselect all
              updateCheckboxGroupInput(session, filterId, selected = character(0))
              rv$filters[[v]] <- character(0)  # so it doesn't revert
            } else {
              # Not all => select all
              updateCheckboxGroupInput(session, filterId, selected = allLevs)
              rv$filters[[v]] <- allLevs
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    ###########################################
    # Step 4) Build dynamic UI for each selected variable
    output$varPanels <- renderUI({
      req(input$vars)
      dfFilter <- get_data(input$vars)
      validate(need(nrow(dfFilter) > 0, "No rows returned (empty dataset)."))
      
      # We'll produce a small fluidRow with (filter + "Include NA?" + "All/None" if factor) + plot
      lapply(input$vars, function(varName) {
        colData <- dfFilter[[varName]]
        filterId <- paste0("filter_", varName)
        plotId   <- paste0("plot_", varName)
        
        # Include NA checkbox
        nNA <- sum(is.na(colData))
        incId <- paste0("includeNA_", varName)
        oldInc <- rv$includeNA[[varName]]
        if (is.null(oldInc)) oldInc <- TRUE
        
        incCheck <- checkboxInput(
          ns(incId),
          label = paste0("Include NA (", nNA, " NAs)?"),
          value = oldInc
        )
        
        if (is.numeric(colData)) {
          # numeric => slider
          rng <- range(colData, na.rm = TRUE)
          diffVal <- rng[2] - rng[1]
          st <- signif(diffVal / 100, digits = 1)
          if (is.na(st) || st == 0) st <- 0.01
          
          min_val <- floor(rng[1] / st) * st
          max_val <- ceiling(rng[2] / st) * st
          
          storedVal <- rv$filters[[varName]]
          if (is.null(storedVal)) {
            storedVal <- c(min_val, max_val)
          }
          
          filterUI <- sliderInput(
            ns(filterId),
            label   = paste("Filter", varName),
            min     = min_val,
            max     = max_val,
            value   = storedVal,
            step    = st
          )
          
          fluidRow(
            column(3, filterUI, incCheck),
            column(9, plotOutput(ns(plotId), height = "150px"))
          )
        } else {
          # factor => checkboxGroup + all/none link
          allLevs <- sort(unique(colData))
          storedVal <- rv$filters[[varName]]
          if (is.null(storedVal)) {
            storedVal <- allLevs
          }
          
          filterUI <- checkboxGroupInput(
            ns(filterId),
            label    = paste("Filter", varName),
            choices  = allLevs,
            selected = storedVal
          )
          linkId <- paste0("allnone_", varName)
          allnoneUI <- actionLink(
            ns(linkId),
            label = "All/None"
          )
          
          fluidRow(
            column(3,
              filterUI,
              incCheck,
              allnoneUI
            ),
            column(9, plotOutput(ns(plotId), height = "150px"))
          )
        }
      })
    })
    
    ###########################################
    # Step 5) Build a T/F mask from all filters
    filterMask <- reactive({
      req(input$vars)
      df <- get_data(input$vars)
      n <- nrow(df)
      if (n == 0) return(logical(0))
      
      keep <- rep(TRUE, n)
      for (v in input$vars) {
        colData   <- df[[v]]
        filterVal <- rv$filters[[v]]
        incVal    <- rv$includeNA[[v]]
        
        if (is.null(filterVal)) next
        
        if (is.numeric(colData)) {
          # numeric
          if (isTRUE(incVal)) {
            keep <- keep & (is.na(colData) | (colData >= filterVal[1] & colData <= filterVal[2]))
          } else {
            keep <- keep & (!is.na(colData) & (colData >= filterVal[1] & colData <= filterVal[2]))
          }
        } else {
          # factor
          if (isTRUE(incVal)) {
            keep <- keep & (is.na(colData) | (colData %in% filterVal))
          } else {
            keep <- keep & (!is.na(colData) & (colData %in% filterVal))
          }
        }
      }
      keep
    })
    
    ###########################################
    # Step 6) Filtered data
    filteredData <- reactive({
      df <- get_data(input$vars)
      if (nrow(df) == 0) return(df)
      mask <- filterMask()
      if (length(mask) == 0) return(df[FALSE, , drop=FALSE])
      df[mask, , drop=FALSE]
    })
    
    ###########################################
    # Step 7) Render plots. We watch *all* relevant inputs in a loop so that
    #         toggling "none" in *any* factor variable triggers re-renders
    #         of all histograms.
    observe({
      req(input$vars)
      
      # Force reactivity on everything for each var
      for (v in input$vars) {
        input[[paste0("filter_", v)]]
        input[[paste0("includeNA_", v)]]
        input[[paste0("allnone_", v)]]
      }
      
      # Now for each var, produce a histogram or bar chart
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
    # 8) Return helpers
    list(
      getFilterMask   = function() filterMask(),
      getFilteredData = function() filteredData()
    )
  })
}

# app.R
library(shiny)
library(dplyr)   # storms is here
# source("dynamicFilterModule.R")  # if your module is in a separate file

exampleApp <- function() {
  bigData <- storms
  get_columns <- reactive({ names(bigData) })
  get_data <- function(cols) {
    bigData[, cols, drop=FALSE]
  }
  
  ui <- fluidPage(
    titlePanel("Final All/None Toggling Example"),
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
      default_selected = c("status", "lat")
    )
    
    # Show the filtered data or mask
    output$debugTable <- renderTable({
      head(mod$getFilteredData(), 10)
    })
    output$debugMask <- renderPrint({
      head(mod$getFilterMask(), 20)
    })
  }
  
  shinyApp(ui, server)
}

# run exampleApp()

