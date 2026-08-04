DynamicFilterModuleServer <- function(id, data, plot_width = 800, plot_height = 200, defaultFilter = NULL, showTable = TRUE, download = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store the selected variables for filtering
    selected_vars <- reactiveVal(if (is.null(defaultFilter)) get_colnames(data) else defaultFilter)

    # Reactive to fetch column names dynamically
    available_columns <- reactive({
      get_colnames(data)
    })

    # UI for selecting variables to include in filtering
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

    # Reactive to filter the dataset based on user inputs
    filtered_data <- reactive({
      df <- filtered_columns()
      vars <- selected_vars()

      for (col in vars) {
        filter_id <- paste0("filter_", col)
        include_na_flag <- ifelse(is.null(input[[paste0("na_", col)]]), FALSE, input[[paste0("na_", col)]])

        if (is.numeric(df[[col]]) && !is.null(input[[filter_id]])) {
          if (include_na_flag) {
            df <- df %>% filter(is.na(!!sym(col)) | between(!!sym(col), input[[filter_id]][1], input[[filter_id]][2]))
          } else {
            df <- df %>% filter(between(!!sym(col), input[[filter_id]][1], input[[filter_id]][2]))
          }
        } else if ((is.factor(df[[col]]) || is.character(df[[col]])) && !is.null(input[[filter_id]])) {
          if (include_na_flag) {
            df <- df %>% filter(is.na(!!sym(col)) | !!sym(col) %in% input[[filter_id]])
          } else {
            df <- df %>% filter(!!sym(col) %in% input[[filter_id]])
          }
        }
      }
      df
    })

    # Other module functionalities remain unchanged...
  })
}
