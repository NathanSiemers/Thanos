library(shiny)
library(dplyr)  ## only needed for storms data set I think..
library(purrr)
################################################################
source('thanos.R')
## thanos default variables to set
## thanos_data_list can be one or more data frames and matrices in register
thanos_data_list = list ( as.data.frame(storms) )
thanos_default_selected = c('status', 'category', 'lat', 'long')
thanos_width = '85%'
thanos_height = 100
################################################################
## ui
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel( ## thanos stuff
            selectizeInput('selectthings', 'Select', choices = NULL, multiple = TRUE),
            uiOutput('renderfilters')
            ),
        mainPanel(
            ## this is just to show actions, it's not a real site!!!
            tableOutput("data"),
            uiOutput("data_summary")
            )
        )
    )
server <- function(input, output, session) {
    ## needed for thanos
    thanos_selectize( input, output, session, data_list = thanos_data_list, default_selected = thanos_default_selected )
    selected = thanos_selected( input, output, session, data_list = thanos_data_list)
    observe({ map( input$selectthings, ~ observeEvent( input[[ paste0( 'action_', .x ) ]], {
        thanos_allnone( input, output, session, data_list = thanos_data_list, .x ) } ) ) })
    observeEvent( input$selectthings,  {
        output$renderfilters = renderUI({
            ## all I'm doing is making duplicated elements of a map2 call that will build the gui elements
            dupnumber = 5; dupnames = unlist(lapply(input$selectthings, function(x){ rep(x, dupnumber) } ) );
            dupcounts = rep(1:dupnumber, length( input$selectthings ) )
            map2(dupnames, dupcounts, ~ ui_filters(
                x = get_variable_in_data_list( thanos_data_list, .x ),
                selectedvals = isolate(input[[.x]]),
                var = .x,
                picket = .y,
                width = thanos_width,
                height = thanos_height
                ) ) }) })
    observe({ map( input$selectthings, ~ input[[.x]] )
              thanos_histos(input, output, session, data_list = thanos_data_list,
                            selected = selected() ) })
    ################################################################
    ## not needed for thanos proper
    ## print out example filtered table, debugging information
    output$data <- renderTable(head(thanos_data_list[[1]][  reduce(selected(), `&`) , ], 12))
    output$data_summary = renderPrint({
        slct = reduce(selected(), `&`)
        print(c( "Original Data:", dim(storms), '</br>', "Thanos Result:", length(which( slct )), '</br>',
                "Selected:", capture.output(str( selected() )), '</br>', table(slct), '</br>',
                capture.output(str(reactiveValuesToList(input)))   ))
    })
}
shinyApp(ui, server)
