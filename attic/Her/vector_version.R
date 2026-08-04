DynamicFilterModuleServer <- function(id, data, plot_width = 800, plot_height = 200, defaultFilter = NULL, showTable = TRUE, download = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store the selected variables for filtering
    selected_vars <- reactiveVal(if (is.null(defaultFilter)) get_colnames(data) else defaultFilter)

    # Reactive to fetch column names dynamically
    available_columns <- reactive({
      get_colnames(data)
    })

    # Compute the logical vector for filtering
    filtered_vector <- reactive({
      df <- get_variables(data, selected_vars())  # Fetch only the selected columns
      vars <- selected_vars()
      filter_vector <- rep(TRUE, nrow(df))  # Start with all rows included

      # Apply filtering logic
      for (col in vars) {
        filter_id <- paste0("filter_", col)
        include_na_flag <- ifelse(is.null(input[[paste0("na_", col)]]), FALSE, input[[paste0("na_", col)]])

        if (is.numeric(df[[col]]) && !is.null(input[[filter_id]])) {
          range_filter <- between(df[[col]], input[[filter_id]][1], input[[filter_id]][2])
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
      filter_vector  # Return the logical vector
    })

    # Function to fetch the filtered data
    get_filtered_data <- reactive({
      df <- get_variables(data, available_columns())  # Fetch all available columns
      df[filtered_vector(), , drop = FALSE]  # Subset rows based on the logical vector
    })

    # Function to fetch the filtered vector
    get_filtered_vector <- reactive({
      filtered_vector()  # Return the logical vector
    })

    # Function to return the filtered data as a DT table
    get_filtered_data_table <- function() {
      DT::datatable(get_filtered_data())
    }

    # Return the functions for external use
    return(list(
      get_filtered_data = get_filtered_data,
      get_filtered_vector = get_filtered_vector,
      get_filtered_data_table = get_filtered_data_table
    ))
  })
}


new plotter

server <- function(input, output, session) {
  # Initialize the dynamic filter module
  filter_module <- DynamicFilterModuleServer(
    "filterModule",
    data = storms,  # Simulated database
    defaultFilter = c("category", "wind", "lat", "long"),
    showTable = TRUE,
    download = TRUE
  )

  # Populate X and Y dropdowns with column names from the database
  observe({
    colnames <- get_colnames(storms)  # Replace `storms` with your data source
    updateSelectInput(session, "select_x", choices = colnames, selected = colnames[1])
    updateSelectInput(session, "select_y", choices = colnames, selected = colnames[2])
  })

  # Generate the scatter plot dynamically
  output$scatter_plot <- renderPlot({
    req(input$select_x, input$select_y)  # Ensure X and Y are selected
    filtered_vector <- filter_module$get_filtered_vector()  # Get the logical vector
    plot_data <- get_variables(storms, c(input$select_x, input$select_y))  # Fetch only required columns
    plot_data <- plot_data[filtered_vector, , drop = FALSE]  # Subset using the logical vector

    ggplot(plot_data, aes_string(x = input$select_x, y = input$select_y)) +
      geom_point() +
      theme_minimal() +
      labs(
        title = "Scatter Plot of Filtered Data",
        x = input$select_x,
        y = input$select_y
      )
  })

  # Render the custom filtered table
  output$custom_filtered_table <- DT::renderDataTable({
    filter_module$get_filtered_data_table()
  })
}
