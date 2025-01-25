library(shiny)
library(dplyr)
library(ggplot2)

source("thanos.R")  # now (like?) a module with it's own namespace

# Simulated database functions for large datasets
# define a get (all) possible column names of table
# and retrieve one or more specific columns by name
# we are using the storms data set from dplyr as an example
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
        updateSelectInput(session, "select_x", choices = get_colnames(storms),  selected = get_colnames(storms)[1])
        updateSelectInput(session, "select_y", choices = get_colnames(storms),  selected = get_colnames(storms)[2])
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
    titlePanel("Thanos: interactive data filtering in R/shiny"),
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
