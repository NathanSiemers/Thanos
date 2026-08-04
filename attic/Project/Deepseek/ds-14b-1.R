# thanos_module.R

library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(shinyWidgets)

thanosModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Input section for filters
    inputSection = div(class = "input-section")({
      inputCheckboxGroupInput(ns("keep"), "Keep observations:", inline = TRUE)
    }),
    
    # Output section for plots and tables
    outputSection = div(class = "output-section")({
      plotOutput(ns("thanos_plot"))
      tableOutput(ns("thanos_table"))
    })
  )
}

thanosModuleServer <- function(id, input, output, session,
                               data_list,
                               selectthings,
                               selected,
                               get_variable_in_data_list) {
  
  # Create a namespace for the module
  ns <- NS(id)
  
  # Update all output bindings with the namespace
  output$thanos_plot <<- renderPlot(ns())
  output$thanos_table <<- renderTable(ns())
  
  # Function to render filters remains the same
  thanos_renderfilters = function(data_list, selectthings, selected, dups = 5) {
    output$thanos_filter <<- renderUI({
      dupnames = unlist(lapply(selectthings, function(x){ rep(x, dups) } ) )
      dupcounts = rep(1:dups, length(selectthings))
      tagList(map2(dupnames, dupcounts, ~ ui_filters( get_variable_in_data_list(data_list, .x), 
                                                      input[[.x]], .x, .y)))
    })
  }
  
  # Function to generate plots remains the same
  plot_it = function(var_name) {
    output$thanos_plot <<- renderPlot(ns(), {
      var = get_variable_in_data_list(data_list, var_name)
      filter_most = selected[names(selected) != var_name]
      reduced = Reduce("&", filter_most)
      
      filtered_most = var[reduced]
      filter_last =  selected[[var_name]]
      filter_last = filter_last[reduced] 
      
      plot_name = paste0('plot_', var_name)
      fcolor = factor(ifelse(filter_last, 'sel', 'unsel'), levels = c('sel', 'unsel'))
      
      if (is.numeric(var)) {
        p = ggplot(data.frame(filtered_most, filter_last), aes(x = filtered_most, color = fcolor)) +
          geom_density() +
          labs(title = plot_name, x = var_name)
      } else {
        p = ggplot(data.frame(filtered_most, filter_last), aes(x = factor(filter_last))) +
          geom_bar(aes(fill = fcolor)) +
          labs(title = plot_name, x = var_name)
      }
      
      return(p)
    })
  }
  


  # Function to generate filtered table
  output$thanos_table <<- renderTable({
    filter_most = selected[names(selected) != "keep"]
    reduced = Reduce("&", filter_most)
    
    data_list %>%
      dplyr::filter(reduced, keep = input$thanos_keep)
  })
  
  # Observe changes in selectthings and re-render
  observeEvent(selectthings, {
    thanos_renderfilters(data_list, selectthings, selected)
  })
  
  # Render filters on initial load
  thanos_renderfilters(data_list, selectthings, selected)
}

# Example Shiny app using the module
##thanosExampleApp <- function() {
##  shinyApp(
    ui = fluidPage(
      titlePanel("Thanos Module Example"),
      mainPanel(
        thanosModuleUI("thanos")
      )
    )
    #,
    
    server = function(input, output, session) {
      data_list = list(mpg = mpg, cyl = cyl, hp = hp)
      selectthings = c("mpg", "cyl", "hp")
      selected = reactiveValues()
      
      thanosModuleServer("thanos",
                         data_list = mtcars
                         selectthings = c('mpg')
                         selected,
                         get_variable_in_data_list)
    }
##  )
##}

runApp(ui,server)


