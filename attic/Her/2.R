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
    })

    # Reactive to generate dynamic UI based on the selected variables
    output$dynamic_filters <- renderUI({
      req(data(), selected_vars())
      df <- data()
      vars <- selected_vars()

      # Generate filters dynamically for each selected column
      lapply(vars, function(col) {
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
