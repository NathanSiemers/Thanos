library(shiny)
library(dplyr)
library(ggplot2)

# Define the Shiny module for dynamic UI
# Module UI
DynamicFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("variable_selector")), # Variable selector
    uiOutput(ns("dynamic_filters")),   # Placeholder for dynamic filters and plots
    DT::dataTableOutput(ns("filtered_table")) # Table to show filtered data
  )
}

# Module Server
DynamicFilterServer <- function(id, data, plot_width = 800, plot_height = 200) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store the selected variables for filtering
    selected_vars <- reactiveVal()

    # UI for selecting variables to include in filtering
    output$variable_selector <- renderUI({
      req(data())
      df <- data()

      selectizeInput(ns("select_vars"),
                     label = "Select Variables for Filtering",
                     choices = names(df),
                     selected = names(df)[1:min(5, ncol(df))],
                     multiple = TRUE,
                     options = list(placeholder = "Select variables..."))
    })

    # Reactive to get the selected variables
    observeEvent(input$select_vars, {
      selected_vars(input$select_vars)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Reactive to generate dynamic UI based on the selected variables
    output$dynamic_filters <- renderUI({
      req(data(), selected_vars())
      df <- data()
      vars <- selected_vars()

      # Generate filters, plots, and reset buttons dynamically for each selected column
      filter_ui <- lapply(vars, function(col) {
        tagList(
          if (is.numeric(df[[col]])) {
            tagList(
              sliderInput(ns(paste0("filter_", col)),
                          label = paste("Filter", col),
                          min = min(df[[col]], na.rm = TRUE),
                          max = max(df[[col]], na.rm = TRUE),
                          value = c(min(df[[col]], na.rm = TRUE), max(df[[col]], na.rm = TRUE))
              ),
              actionButton(ns(paste0("reset_", col)), label = "Reset")
            )
          } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
            tagList(
              selectInput(ns(paste0("filter_", col)),
                          label = paste("Filter", col),
                          choices = unique(df[[col]]),
                          selected = unique(df[[col]]),
                          multiple = TRUE),
              actionButton(ns(paste0("reset_", col)), label = "All/None")
            )
          },
          plotOutput(ns(paste0("plot_", col)), height = paste0(plot_height, "px"), width = paste0(plot_width, "px"))
        )
      })

      do.call(tagList, filter_ui)
    })

    # Observe reset button actions for each variable
    observe({
      req(data(), selected_vars())
      df <- data()
      vars <- selected_vars()

      lapply(vars, function(col) {
        observeEvent(input[[paste0("reset_", col)]], {
          if (is.numeric(df[[col]])) {
            updateSliderInput(session, ns(paste0("filter_", col)),
                              value = c(min(df[[col]], na.rm = TRUE), max(df[[col]], na.rm = TRUE)))
          } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
            all_values <- unique(df[[col]])
            current_selection <- input[[paste0("filter_", col)]]
            if (is.null(current_selection) || length(current_selection) < length(all_values)) {
              updateSelectInput(session, ns(paste0("filter_", col)), selected = all_values)
            } else {
              updateSelectInput(session, ns(paste0("filter_", col)), selected = character(0))
            }
          }
        }, ignoreInit = TRUE)

        # Observe changes in filters and trigger updates
        observeEvent(input[[paste0("filter_", col)]], {
          selected_vars(selected_vars())
        })
      })
    })

    # Reactive to filter the dataset based on user inputs
    filtered_data <- reactive({
      req(data(), selected_vars())
      df <- data()
      vars <- selected_vars()

      for (col in vars) {
        filter_id <- paste0("filter_", col)
        if (is.numeric(df[[col]]) && !is.null(input[[filter_id]])) {
          df <- df %>% filter(between(!!sym(col), input[[filter_id]][1], input[[filter_id]][2]))
        } else if ((is.factor(df[[col]]) || is.character(df[[col]])) && !is.null(input[[filter_id]])) {
          df <- df %>% filter(!!sym(col) %in% input[[filter_id]])
        }
      }
      df
    })

    # Render each plot dynamically
    observe({
      req(data(), selected_vars())
      df <- data()
      filtered_df <- filtered_data()
      vars <- selected_vars()

      lapply(vars, function(col) {
        output[[paste0("plot_", col)]] <- renderPlot({
          if (is.numeric(df[[col]])) {
            ggplot(df, aes(x = !!sym(col))) +
              geom_histogram(data = df, fill = "gray", alpha = 0.5, bins = 30) +
              geom_histogram(data = filtered_df, fill = "blue", alpha = 0.8, bins = 30) +
              theme_minimal() +
              labs(title = paste("Distribution of", col), x = col, y = "Count")
          } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
            ggplot(df, aes(x = !!sym(col))) +
              geom_bar(data = df, fill = "gray", alpha = 0.5) +
              geom_bar(data = filtered_df, fill = "blue", alpha = 0.8) +
              theme_minimal() +
              labs(title = paste("Distribution of", col), x = col, y = "Count")
          }
        })
      })
    })

    # Render filtered data table
    output$filtered_table <- DT::renderDataTable({
      req(filtered_data())
      DT::datatable(filtered_data())
    })
  })
}

# Main app to demonstrate the module
ui <- fluidPage(
  titlePanel("Dynamic Filter Shiny Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Upload CSV", accept = ".csv")
    ),
    mainPanel(
      DynamicFilterUI("filterModule")
    )
  )
)

server <- function(input, output, session) {
  # Reactive to load the uploaded dataset
  dataset <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath)
  })

  # Call the dynamic filter module
  DynamicFilterServer("filterModule", dataset)
}

shinyApp(ui, server)
