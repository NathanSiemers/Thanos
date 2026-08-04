library(shiny)
library(dplyr)
library(ggplot2)

# Define the Shiny module for dynamic UI
# Module UI
DynamicFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("variable_selector")), # Variable selector
    uiOutput(ns("dynamic_filters")),   # Placeholder for dynamic filters
    uiOutput(ns("dynamic_plots")),     # Placeholder for dynamic plots
    DT::dataTableOutput(ns("filtered_table")) # Table to show filtered data
  )
}

# Module Server
DynamicFilterServer <- function(id, data) {
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

      # Generate filters dynamically for each selected column
      filter_ui <- lapply(vars, function(col) {
        if (is.numeric(df[[col]])) {
          sliderInput(ns(paste0("filter_", col)),
                      label = paste("Filter", col),
                      min = min(df[[col]], na.rm = TRUE),
                      max = max(df[[col]], na.rm = TRUE),
                      value = c(min(df[[col]], na.rm = TRUE), max(df[[col]], na.rm = TRUE)))
        } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
          selectInput(ns(paste0("filter_", col)),
                      label = paste("Filter", col),
                      choices = unique(df[[col]]),
                      selected = unique(df[[col]]),
                      multiple = TRUE)
        }
      })

      do.call(tagList, filter_ui)
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

    # Generate dynamic plots for each variable being filtered
    output$dynamic_plots <- renderUI({
      req(data(), selected_vars())
      df <- data()
      vars <- selected_vars()

      plot_ui <- lapply(vars, function(col) {
        plotOutput(ns(paste0("plot_", col)))
      })

      do.call(tagList, plot_ui)
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
