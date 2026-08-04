
library(shiny)
library(dplyr)
library(ggplot2)

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

# Simulated database functions for large datasets
get_colnames <- function(data) {
    # Fetch column names (simulate external database interaction)
    ## ignore "data
    colnames(storms)
}

get_variables <- function(data, columns) {
    # Fetch specific columns (simulate external database interaction)
    # ignore "data"
    storms[, columns, drop = FALSE]
}

# Module for dynamic filter functionality
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


        initialized_vars <- reactiveValues(
            sliders = list(),  # Track slider inputs
            checkboxes = list(),  # Track checkbox inputs
            plots = list()  # Track rendered plots
        )

        new_vars <- reactive({
            setdiff(selected_vars(), names(initialized_vars$sliders))  # Identify new variables
        })




        output$dynamic_filters <- renderUI({
            req(selected_vars())

            # Generate new UI components for new variables
            ui_list <- lapply(new_vars(), function(col) {
                if (is.numeric(data[[col]])) {
                    tagList(
                        sliderInput(ns(paste0("filter_", col)),
                                    label = paste("Filter", col),
                                    min = min(data[[col]], na.rm = TRUE),
                                    max = max(data[[col]], na.rm = TRUE),
                                    value = c(min(data[[col]], na.rm = TRUE), max(data[[col]], na.rm = TRUE))),
                        plotOutput(ns(paste0("plot_", col)))
                    )
                } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
                    tagList(
                        checkboxGroupInput(ns(paste0("filter_", col)),
                                           label = paste("Filter", col),
                                           choices = unique(data[[col]]),
                                           selected = unique(data[[col]])),
                        plotOutput(ns(paste0("plot_", col)))
                    )
                }
            })

            do.call(tagList, ui_list)
        })


        observeEvent(new_vars(), {
            lapply(new_vars(), function(col) {
                if (is.numeric(data[[col]])) {
                    initialized_vars$sliders[[col]] <- paste0("filter_", col)
                    initialized_vars$plots[[col]] <- paste0("plot_", col)
                } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
                    initialized_vars$checkboxes[[col]] <- paste0("filter_", col)
                    initialized_vars$plots[[col]] <- paste0("plot_", col)
                }
            })
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

        observe({
            lapply(names(initialized_vars$sliders), function(col) {
                updateSliderInput(session, ns(paste0("filter_", col)),
                                  value = input[[paste0("filter_", col)]])
            })

            lapply(names(initialized_vars$checkboxes), function(col) {
                updateCheckboxGroupInput(session, ns(paste0("filter_", col)),
                                         selected = input[[paste0("filter_", col)]])
            })

            lapply(names(initialized_vars$plots), function(col) {
                output[[ns(paste0("plot_", col))]] <- renderPlot({
                    req(filtered_data())
                    df <- filtered_data()

                    if (is.numeric(df[[col]])) {
                        ggplot(df, aes(x = df[[col]])) +
                            geom_histogram(fill = "blue", alpha = 0.8, bins = 30) +
                            theme_minimal() +
                            labs(title = paste("Distribution of", col), x = col, y = "Count")
                    } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
                        ggplot(df, aes(x = df[[col]])) +
                            geom_bar(fill = "blue", alpha = 0.8) +
                            theme_minimal() +
                            labs(title = paste("Distribution of", col), x = col, y = "Count")
                    }
                })
            })
        })

        filtered_vector <- reactive({
            req(data, selected_vars())
            df <- get_variables(data, selected_vars())
            vars <- selected_vars()
            filter_vector <- rep(TRUE, nrow(df))  # Start with all rows included
            
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


server <- function(input, output, session) {
    # Initialize the dynamic filter module
    filter_module <- DynamicFilterModuleServer(
        "filterModule",
        data = storms,
        defaultFilter = c("category", "wind", "lat", "long"),
        showTable = TRUE,  # We'll use a custom table display
        download = TRUE
    )
    # Debugging filter_module structure
    ##  print(class(filter_module))            # Should print "list"
    ##  print(names(filter_module))            # Should include "get_filtered_data" and "get_filtered_data_table"
    # Debugging the module return values
    ## observe({
    ##   print(class(filter_module$get_filtered_data_table))  # Should print "function"
    ##   print(class(filter_module$get_filtered_data))       # Should print "function"
    ## })
    # Use get_filtered_data to process or display filtered data
    output$custom_filtered_table <- DT::renderDataTable({
        filter_module$get_filtered_data_table()
    })
    # Populate X and Y selectInput choices with column names
    observe({
        updateSelectInput(session, "select_x", choices = names(storms),  selected = names(storms)[1])
        updateSelectInput(session, "select_y", choices = names(storms),  selected = names(storms)[2])
    })
    
    # Generate the scatter plot dynamically
    output$scatter_plot <- renderPlot({
        req(input$select_x, input$select_y)  # Ensure X and Y are selected
        # Debugging: Print selected X and Y variables
        print(paste("Selected X:", input$select_x))
        print(paste("Selected Y:", input$select_y))
        
        # Fetch filtered vector and plot data
        filtered_vector <- filter_module$filtered_vector()  # Get the logical vector
        print(paste("Filtered vector length:", length(filtered_vector)))
        
        plot_data <- get_variables(storms, c(input$select_x, input$select_y))  # Fetch only required columns
        print(dim(plot_data))
        print(head(plot_data))  # Debug: Print fetched data
        
        plot_data <- plot_data[filtered_vector, , drop = FALSE]  # Subset using the logical vector
        print(head(plot_data))  # Debug: Print subsetted data
        
        # Check for issues in plot_data
        if (nrow(plot_data) == 0) {
            print("No data to plot!")
            return(NULL)  # Avoid plotting if no data is available
        }
        
        # Generate the plot
        ggplot(plot_data, aes_string(x = input$select_x, y = input$select_y)) +
            geom_point() +
            theme_minimal() +
            labs(
                title = "Scatter Plot of Filtered Data",
                x = input$select_x,
                y = input$select_y
            )
    })

}

ui <- fluidPage(
    titlePanel("Dynamic Filter Shiny Module"),
    sidebarLayout(
        sidebarPanel(
            DynamicFilterModuleUI("filterModule"),  # Dynamic filter module UI
            ),
        mainPanel(
            selectInput("select_x", "Select X Axis:", choices = NULL),
            selectInput("select_y", "Select Y Axis:", choices = NULL),
            plotOutput("scatter_plot"),           # Scatter plot output
            DT::dataTableOutput("custom_filtered_table")  # Custom filtered data table
        )
    )
)

shinyApp(ui, server)
