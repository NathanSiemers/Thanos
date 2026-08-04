library(shiny)
library(dplyr)
library(ggplot2)

# Module for dynamic filter functionality
DynamicFilterModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("variable_selector")), # Variable selector
    uiOutput(ns("dynamic_filters")),   # Placeholder for dynamic filters and plots
    conditionalPanel(
      condition = sprintf("input.%s_showTable == true", ns("")),
      DT::dataTableOutput(ns("filtered_table")) # Table to show filtered data
    )
  )
}

DynamicFilterModuleServer <- function(id, data, plot_width = 800, plot_height = 200, defaultFilter = NULL, showTable = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store the selected variables for filtering
    selected_vars <- reactiveVal(if (is.null(defaultFilter)) names(data) else defaultFilter)

    # UI for selecting variables to include in filtering
    output$variable_selector <- renderUI({
      req(data)
      df <- data

      selectizeInput(ns("select_vars"),
                     label = "Select Variables for Filtering",
                     choices = names(df),
                     selected = selected_vars(),
                     multiple = TRUE,
                     options = list(placeholder = "Select variables..."))
    })

    # Reactive to update the selected variables
    observeEvent(input$select_vars, {
      selected_vars(input$select_vars)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Reactive to generate dynamic UI based on the selected variables
    output$dynamic_filters <- renderUI({
      req(data, selected_vars())
      df <- data
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
              )
            )
          } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
            tagList(
              checkboxGroupInput(ns(paste0("filter_", col)),
                                 label = paste("Filter", col),
                                 choices = unique(df[[col]]),
                                 selected = unique(df[[col]])),
              actionButton(ns(paste0("action_", col)), label = "All/None")
            )
          },
          plotOutput(ns(paste0("plot_", col)), height = paste0(plot_height, "px"), width = paste0(plot_width, "px"))
        )
      })

      do.call(tagList, filter_ui)
    })

    # Observe reset button actions for categorical variables
    observe({
      req(data, selected_vars())
      df <- data
      vars <- selected_vars()

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

        # Trigger reactive updates when checkbox inputs change
        observeEvent(input[[paste0("filter_", col)]], {
          selected_vars(selected_vars())
        }, ignoreInit = TRUE)
      })
    })

    # Reactive to filter the dataset based on user inputs
    filtered_data <- reactive({
      req(data, selected_vars())
      df <- data
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
      req(data, selected_vars())
      df <- data
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
      if (showTable) {
        DT::datatable(filtered_data())
      }
    })
  })
}

# Main app to demonstrate the module
ui <- fluidPage(
  titlePanel("Dynamic Filter Shiny Module"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      DynamicFilterModuleUI("filterModule")
    )
  )
)

server <- function(input, output, session) {
  # Provide a data frame to the module with default filters and table visibility
  DynamicFilterModuleServer(
    "filterModule",
    data = storms,
    defaultFilter = c('category', 'wind', 'lat', 'long'),
    showTable = TRUE
  )
}

shinyApp(ui, server)
