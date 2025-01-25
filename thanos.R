### Module for dynamic filter functionality
DynamicFilterModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("variable_selector")), # Variable selector
    uiOutput(ns("dynamic_filters")),   # Placeholder for dynamic filters and plots
    conditionalPanel(
      condition = sprintf("input.%s_showTable == true", ns("")),
      DT::dataTableOutput(ns("filtered_table")), # Table to show filtered data
      uiOutput(ns("download_ui")) # Download UI
    )
  )
}

DynamicFilterModuleServer <- function(id, data, plot_width = 600, plot_height = 150, defaultFilter = NULL, showTable = TRUE, download = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Reactive to store the selected variables for filtering
    ##selected_vars <- reactiveVal(if (is.null(defaultFilter)) names(data) else defaultFilter)
    selected_vars <- reactiveVal(if (is.null(defaultFilter)) get_colnames(data) else defaultFilter)
    print("Initial value of selected_vars:")
    observe({
      print("Accessing selected_vars in observe:")
      print(selected_vars())
    })
    # Reactive to fetch column names dynamically
    available_columns <- reactive({
      get_colnames(data)
    })
    # Reactive to store NA inclusion for each variable
    include_na <- reactiveValues()
    output$variable_selector <- renderUI({
      selectizeInput(ns("select_vars"),
                     label = "Select Variables for Filtering",
                     choices = available_columns(),
                     selected = selected_vars(),
                     multiple = TRUE,
                     options = list(placeholder = "Select variables..."))
    })
    # Reactive to fetch selected columns
    filtered_columns <- reactive({
      req(input$select_vars)
      get_variables(data, input$select_vars)
    })
    
    # Reactive to update the selected variables
    observeEvent(input$select_vars, {
      selected_vars(input$select_vars)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Reactive to generate dynamic UI based on the selected variables
    output$dynamic_filters <- renderUI({
      print('output$dynamic_filters <- renderUI({...')
      req(selected_vars())
      vars <- selected_vars()
      print(vars)
      df <- get_variables(data, selected_vars())
      print(dim(df))
      
      # Generate filters, plots, reset buttons, and NA checkboxes dynamically for each selected column
      filter_ui <- lapply(selected_vars(), function(col) {
        print("In filter_ui")
        df = get_variables(data, selected_vars())
        tagList(
          if (is.numeric(df[[col]])) {
            tagList(
              sliderInput(ns(paste0("filter_", col)),
                          label = paste("Filter", col),
                          min = min(df[[col]], na.rm = TRUE),
                          max = max(df[[col]], na.rm = TRUE),
                          value = c(min(df[[col]], na.rm = TRUE), max(df[[col]], na.rm = TRUE))
              ),
              checkboxInput(ns(paste0("na_", col)),
                            label = paste0("Include NAs (", sum(is.na(df[[col]])), ")"), 
                            value = TRUE)
            )
          } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
            tagList(
              checkboxGroupInput(ns(paste0("filter_", col)),
                                 label = paste("Filter", col),
                                 choices = unique(df[[col]]),
                                 selected = unique(df[[col]])),
              checkboxInput(ns(paste0("na_", col)),
                            label = paste0("Include NAs (", sum(is.na(df[[col]])), ")"),
                            value = TRUE),
              actionButton(ns(paste0("action_", col)), label = "All/None")
            )
          },
          print("we are supposed to be plotting with plotOutput"),
          print(col),
          print(ns(paste0("plot_", col))),
          plotOutput(ns(paste0("plot_", col)), height = paste0(plot_height, "px"), width = paste0(plot_width, "px"))
        )
      })
      do.call(tagList, filter_ui)
    })
    
    # Observe reset button actions and NA inclusion changes for categorical variables
    observe({
      req(selected_vars())
      ##df <- data
      vars <- selected_vars()
      print("in reset code")
      print(vars)
      df <- get_variables(data, vars)
      lapply(vars, function(col) {
        observeEvent(input[[paste0("action_", col)]], {
          if (is.factor(df[[col]]) || is.character(df[[col]])) {
            act <- input[[paste0("action_", col)]]
            allchoices <- sort(unique(df[[col]]))
            if (is.null(act)) {
              act <- 0
            }
            if (as.numeric(act) %% 2 == 0) {
              updateCheckboxGroupInput(session, ns(paste0("filter_", col)), label = paste("Filter", col), choices = allchoices, selected = allchoices, inline = TRUE)
            } else {
              updateCheckboxGroupInput(session, ns(paste0("filter_", col)), label = paste("Filter", col), choices = allchoices, selected = character(0), inline = TRUE)
            }
          }
        }, ignoreInit = TRUE)
        
        # Track NA inclusion
        observeEvent(input[[paste0("na_", col)]], {
          include_na[[col]] <- input[[paste0("na_", col)]]
        }, ignoreInit = TRUE)
      })
    })
    
    
    filtered_vector <- reactive({
      req(data, selected_vars())
      df <- get_variables(data, selected_vars())
      vars <- selected_vars()
      filter_vector <- rep(TRUE, nrow(df))  # Start with all rows included
      `%||%` <- function(x, y) {
        if (!is.null(x)) x else y
      }
      for (col in vars) {
        filter_id <- paste0("filter_", col)
        include_na_flag <- input[[paste0("na_", col)]] %||% FALSE  # Default to FALSE
        
        if (is.numeric(df[[col]]) && !is.null(input[[filter_id]])) {
          # Debugging input values
          print(paste("Filter values for", col, ":", input[[filter_id]]))
          
          range_filter <- between(
            df[[col]],
            left = input[[filter_id]][1],
            right = input[[filter_id]][2]
          )
          
          if (include_na_flag) {
            range_filter <- range_filter | is.na(df[[col]])
          }
          filter_vector <- filter_vector & range_filter
        } else if ((is.factor(df[[col]]) || is.character(df[[col]])) && !is.null(input[[filter_id]])) {
          value_filter <- df[[col]] %in% input[[filter_id]]
          if (include_na_flag) {
            value_filter <- value_filter | is.na(df[[col]])
          }
          filter_vector <- filter_vector & value_filter
        }
      }
      
      print("Final filtered vector:")
      print(table(filter_vector))
      filter_vector
    })
    
    ## filtered_vector <- reactive({
    ##     req(data, selected_vars())
    ##     print("Calculating filtered vector...")
    
    ##     # Fetch the selected columns
    ##     df <- get_variables(data, selected_vars())
    ##     vars <- selected_vars()
    
    ##     # Initialize the vector as TRUE for all rows
    ##     filter_vector <- rep(TRUE, nrow(df))
    
    ##     # Apply filters for each selected variable
    ##     for (col in vars) {
    ##         filter_id <- paste0("filter_", col)
    ##         include_na_flag <- input[[paste0("na_", col)]] %||% FALSE  # Default to FALSE if NULL
    
    ##         if (is.numeric(df[[col]])) {
    ##             range_filter <- between(df[[col]], input[[filter_id]][1], input[[filter_id]][2])
    ##             if (include_na_flag) {
    ##                 range_filter <- range_filter | is.na(df[[col]])
    ##             }
    ##             filter_vector <- filter_vector & range_filter
    ##         } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
    ##             value_filter <- df[[col]] %in% input[[filter_id]]
    ##             if (include_na_flag) {
    ##                 value_filter <- value_filter | is.na(df[[col]])
    ##             }
    ##             filter_vector <- filter_vector & value_filter
    ##         }
    ##     }
    
    ##     print("Filtered vector calculated:")
    ##     print(table(filter_vector))
    ##     filter_vector
    ## })
    
    filtered_data <- reactive({
      req(filtered_vector())  # Ensure the logical vector is available
      print("Subsetting data with filtered vector...")
      
      # Fetch all available columns
      df <- get_variables(data, selected_vars())
      print(paste("Original data dimensions:", dim(df)))
      
      # Subset using the logical vector
      df <- df[filtered_vector(), , drop = FALSE]
      print(paste("Filtered data dimensions:", dim(df)))
      
      df
    })
    
    
    
    # Function to return the filtered data as a DT table
    get_filtered_data_table <- function() {
      DT::datatable(filtered_data())
    }
    
    # Provide a download handler for filtered data if enabled
    if (download) {
      output$download_ui <- renderUI({
        downloadButton(ns("download_data"), "Download Filtered Data")
      })
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste("filtered_data", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(filtered_data(), file, row.names = FALSE)
        }
      )
    } else {
      output$download_ui <- renderUI({ NULL })
    }
    
    # Render each plot dynamically
    ##print("selected_vars is NULL or empty?")
    ##print(is.null(selected_vars()))
    ##print(selected_vars())
    ##print(head(filtered_data()))
    
    observeEvent(input$select_vars, {
      print("Updating selected_vars...")
      selected_vars(input$select_vars)
    })
    
    observe({
      req(selected_vars(), filtered_data())
      
      vars <- selected_vars()
      print("Generating plots for variables:")
      print(vars)
      
      lapply(vars, function(col) {
        output[[paste0("plot_", col)]] <- renderPlot({
          req(filtered_data())
          df <- filtered_data()
          
          if (!col %in% names(df)) {
            print(paste("Column", col, "not found in filtered data. Skipping."))
            return(NULL)
          }
          
          print(paste("Plotting column:", col))
          if (is.numeric(df[[col]])) {
            ggplot(df, aes(x = .data[[col]])) +
              geom_histogram(fill = "blue", alpha = 0.8, bins = 30) +
              theme_minimal()
          } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
            ggplot(df, aes(x = .data[[col]])) +
              geom_bar(fill = "blue", alpha = 0.8) +
              theme_minimal()
          }
        })
      })
    })
    
    observeEvent(input$select_vars, {
      print("Updating selected_vars...")
      print("Input$select_vars:")
      print(input$select_vars)
      selected_vars(input$select_vars)  # Update selected_vars
      print("Updated selected_vars:")
      print(selected_vars())
    })
    
    
    # Render filtered data table
    output$filtered_table <- DT::renderDataTable({
      req(filtered_data())
      if (showTable) {
        DT::datatable(filtered_data())
      }
    })
    
    
    # Ensure the correct list of functions is returned
    return(list(
      filtered_data = filtered_data,
      filtered_vector = filtered_vector,
      get_filtered_data_table = get_filtered_data_table
    ))
  })
}
