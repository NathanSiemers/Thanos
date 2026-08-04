library(shiny)
library(dplyr)     # only needed for storms data set
library(purrr)
library(ggplot2)
library(viridis)

############################################################
## The Thanos Module
############################################################

# 1. The module UI
thanosModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    # A selectize input for picking columns
    selectizeInput(ns("selectthings"), "Select", choices = NULL, multiple = TRUE),
    
    # A place to render the dynamic filters (sliders, checkboxes, histograms)
    uiOutput(ns("renderfilters"))
  )
}

# 2. The module Server
thanosModuleServer <- function(id,
                               data_list,
                               default_selected = c("status", "category", "lat", "long"),
                               width  = "85%",
                               height = 100) {
  moduleServer(id, function(input, output, session) {
    
    ############################
    ## All Thanos functions here
    ############################
    
    theme_thanos <- theme(
      axis.text.x     = element_text(size = 12),
      axis.text.y     = element_text(size = 9),
      legend.position = "none",
      plot.title      = element_text(size = 12, hjust = 0),
      axis.title      = element_text(size = 0, hjust = 0),
      legend.text     = element_text(size = 0, hjust = 0),
      legend.title    = element_text(size = 0, hjust = 0)
    )
    
    # UI builder for dynamic filter controls
    ui_filters <- function(x, selectedvals, var, picket, width = '100%', height = '200px') {
      if (picket == 2) {
        if (is.numeric(x)) {
          rng <- range(x, na.rm = TRUE)
          if (length(selectedvals) == 2) {
            sliderInput(var, var, min = rng[1], max = rng[2],
                        value = selectedvals, width = width)
          } else {
            sliderInput(var, var, min = rng[1], max = rng[2],
                        value = rng, width = width)
          }
        } else {
          levs <- sort(unique(x))
          if (length(selectedvals) < 1) {
            checkboxGroupInput(var, var, choices = levs, selected = levs,
                               inline = TRUE, width = width)
          } else {
            checkboxGroupInput(var, var, choices = levs, selected = selectedvals,
                               inline = TRUE, width = width)
          }
        }
      } else if (picket == 3) {
        # "all/none" action link for categorical variables
        if (!is.numeric(x)) {
          alink <- paste0('action_', var)
          actionLink(alink, 'all/none')
        }
      } else if (picket == 4) {
        # Show histogram
        plot_obj <- paste0('plot_', var)
        plotOutput(plot_obj, height = height, width = width)
      }
      # picket == 1 and 5 do nothing by default
    }
    
    # Filter each data frame in data_list by a single variable
    filter_var <- function(data_list, var, val) {
      l <- lapply(data_list, function(dat)  {
        if (var %in% colnames(dat)) {
          x <- dat[, var]
          if (is.numeric(x)) {
            is.na(x) | (x >= val[1] & x <= val[2])
          } else {
            is.na(x) | x %in% val
          }
        } else {
          rep(TRUE, nrow(dat))
        }
      })
      Reduce("&", l)
    }
    
    # Pull out one named variable from the first data frame that contains it
    get_variable_in_data_list <- function(dl, var) {
      out <- lapply(dl, function(x) {
        if (var %in% colnames(x)) x[, var]
      })
      out[vapply(out, Negate(is.null), NA)][[1]]
    }
    
    # Build a scatter/gather of all columns in data_list
    get_cols_in_data_list <- function(dl) {
      unique(unlist(sapply(dl, colnames)))
    }
    
    # Plot histogram for either numeric or categorical
    plot_it <- function(data_list, selected, x) {
      var <- get_variable_in_data_list(data_list, x)
      # Combine filters for all variables except x
      filter_most <- selected[names(selected) != x]
      reduced <- Reduce("&", filter_most)
      filtered_most <- var[reduced]
      
      # For "x", see which rows pass that filter
      filter_last <- selected[[x]]
      filter_last <- filter_last[reduced]
      
      fcolor <- factor(ifelse(filter_last, 'sel', 'unsel'),
                       levels = c('sel', 'unsel'))
      plot_title <- paste(x, ':', sum(filter_last), '/', length(filtered_most))
      
      if (is.numeric(var)) {
        p <- ggplot() +
          geom_histogram(aes(x = filtered_most, fill = fcolor), bins = 50) +
          ggtitle(plot_title) +
          scale_fill_viridis(end = 0.4, discrete = TRUE, option = "plasma")
      } else {
        p <- ggplot() +
          geom_histogram(aes(x = filtered_most, fill = fcolor), stat = "count") +
          ggtitle(plot_title) +
          scale_x_discrete(labels = abbreviate) +
          scale_fill_viridis(end = 0.4, discrete = TRUE, option = "plasma")
      }
      p + theme_thanos
    }
    
    # 1) Update the selectize input with available columns
    thanos_selectize <- function(input, output, session, data_list, default_selected) {
      updateSelectizeInput(
        session, "selectthings",
        choices  = get_cols_in_data_list(data_list),
        selected = default_selected,
        server   = TRUE
      )
    }
    
    # 2) Build a reactive list of logical vectors (one per selected column)
    thanos_selected <- function(input, output, session, data_list) {
      reactive({
        each_var <- map(input$selectthings, ~ filter_var(data_list, .x, input[[.x]]))
        names(each_var) <- input$selectthings
        each_var
      })
    }
    
    # 3) Generate the histograms for selected columns
    thanos_histos <- function(input, output, session, data_list, selected) {
      cname <- paste0("plot_", input$selectthings)
      Map(function(name) {
        cname <- paste0("plot_", name)
        output[[cname]] <<- renderPlot({
          plot_it(data_list, selected, name)
        })
      }, input$selectthings)
    }
    
    # 4) "All/none" toggles for factor/character columns
    thanos_allnone <- function(input, output, session, data_list, x) {
      act <- input[[paste0('action_', x)]]
      allchoices <- sort(unique(get_variable_in_data_list(data_list, x)))
      if (as.numeric(act) %% 2 == 0) {
        updateCheckboxGroupInput(session, x, x,
                                 choices = allchoices, selected = allchoices,
                                 inline = TRUE)
      } else {
        updateCheckboxGroupInput(session, x, x,
                                 choices = allchoices, selected = character(0),
                                 inline = TRUE)
      }
    }
    
    ############################
    ## Module logic
    ############################
    
    # Initialize the selectize input
    thanos_selectize(input, output, session, data_list, default_selected)
    
    # Reactive expression for the per-column filters
    selected_list <- thanos_selected(input, output, session, data_list)
    
    # Observe "all/none" toggles for each selected variable
    observe({
      map(input$selectthings, ~ observeEvent(input[[paste0("action_", .x)]], {
        thanos_allnone(input, output, session, data_list, .x)
      }))
    })
    
    # Dynamically render the filter UI whenever selectthings changes
    observeEvent(input$selectthings, {
      output$renderfilters <- renderUI({
        dupnumber <- 5
        dupnames  <- unlist(lapply(input$selectthings, function(x) {
          rep(x, dupnumber)
        }))
        dupcounts <- rep(1:dupnumber, length(input$selectthings))
        
        map2(dupnames, dupcounts, ~ ui_filters(
          x            = get_variable_in_data_list(data_list, .x),
          selectedvals = isolate(input[[.x]]),
          var          = .x,
          picket       = .y,
          width        = width,
          height       = height
        ))
      })
    })
    
    # Render histograms each time filters are updated
    observe({
      # Force dependency on the actual slider/checkbox inputs
      map(input$selectthings, ~ input[[.x]])
      thanos_histos(input, output, session, data_list, selected_list())
    })
    
    # Combine all selected columns into a single reactive filter
    combined_filter <- reactive({
      reduce(selected_list(), `&`)
    })
    
    # Return the combined filter to the calling app (if needed)
    return(combined_filter)
  })
}

############################################################
## Example: Using the Module in an App
############################################################

# Example data: storms
thanos_data_list <- list(as.data.frame(storms))

ui <- fluidPage(
  titlePanel("Example: Thanos Filter Module"),
  sidebarLayout(
    sidebarPanel(
      # Place module UI here
      thanosModuleUI("thanos1")
    ),
    mainPanel(
      # Show some filtered data
      tableOutput("data"),
      
      # Show a small summary
      uiOutput("data_summary")
    )
  )
)

server <- function(input, output, session) {
  # Call the Thanos module
  final_filter <- thanosModuleServer(
    id               = "thanos1",
    data_list        = thanos_data_list,
    default_selected = c("status", "category", "lat", "long"),
    width            = "85%",
    height           = 100
  )
  
  # Use the returned filter for anything you like in main server
  output$data <- renderTable({
    storms_df <- thanos_data_list[[1]]
    storms_df[final_filter(), ] %>% head(12)
  })
  
  output$data_summary <- renderUI({
    storms_df <- thanos_data_list[[1]]
    slct <- final_filter()
    tagList(
      tags$p(paste("Original rows:", nrow(storms_df))),
      tags$p(paste("Filtered rows:", sum(slct)))
    )
  })
}

shinyApp(ui, server)
