################################################################
## Grapher: a scatter-plot app with Thanos embedded as its filter
## engine -- the Project.md embedding demo.
##
## The integration surface is deliberately tiny: this app touches only
##   thanosUI("thanos")            in the ui
##   thanosServer("thanos", ...)   in the server
##   th$rows()                     an integer pointer to surviving rows
## It never sees masks, filters, or module internals, and no filtered
## copy of the data is ever passed around.
##
##   shiny::runApp("apps/grapher")
################################################################
library(shiny)
library(ggplot2)
library(nycflights13)

thanos_r_dir <- Filter(function(p) file.exists(file.path(p, "thanos_module.R")),
                       c("R", "../R", "../../R"))[1]
if (is.na(thanos_r_dir)) stop("cannot locate the Thanos R/ directory")
invisible(lapply(list.files(thanos_r_dir, pattern = "[.]R$", full.names = TRUE),
                 source))

## one backend shared by the module and the grapher, so the filterable
## columns and the plottable columns are always the same set
backend <- backend_memory(as.data.frame(nycflights13::flights))
PLOT_CAP <- 50000  # sample cap so geom_point stays responsive

numeric_cols <- Filter(function(cn) backend$get_column_info(cn)$is_numeric,
                       backend$get_columns())

ui <- fluidPage(
    titlePanel("Grapher, powered by Thanos"),
    sidebarLayout(
        sidebarPanel(width = 5,
            h4("Filters"),
            thanosUI("thanos")
        ),
        mainPanel(width = 7,
            fluidRow(
                column(3, selectInput("x", "x", choices = numeric_cols,
                                      selected = "dep_delay")),
                column(3, selectInput("y", "y", choices = numeric_cols,
                                      selected = "arr_delay")),
                column(3, selectInput("color", "color",
                                      choices = c("(none)", backend$get_columns()),
                                      selected = "origin")),
                column(3, selectInput("size", "size",
                                      choices = c("(none)", numeric_cols),
                                      selected = "(none)"))
            ),
            plotOutput("scatter", height = "500px"),
            textOutput("counts")
        )
    )
)

server <- function(input, output, session) {
    th <- thanosServer("thanos", backend,
                       default_selected = c("carrier", "origin",
                                            "dep_delay", "distance"))

    plot_rows <- reactive({
        r <- th$rows()                    # <- the whole integration
        if (length(r) > PLOT_CAP) sort(sample(r, PLOT_CAP)) else r
    })

    plot_data <- reactive({
        cols <- unique(c(input$x, input$y,
                         setdiff(c(input$color, input$size), "(none)")))
        r <- plot_rows()
        as.data.frame(lapply(setNames(cols, cols),
                             function(cn) backend$get_column(cn)[r]))
    })

    output$scatter <- renderPlot({
        req(input$x, input$y)
        df <- plot_data()
        aes_args <- list(x = as.name(input$x), y = as.name(input$y))
        if (input$color != "(none)") aes_args$colour <- as.name(input$color)
        if (input$size  != "(none)") aes_args$size   <- as.name(input$size)
        p <- ggplot(df, do.call(aes, aes_args)) +
            geom_point(alpha = 0.4)
        if (input$color != "(none)") {
            p <- p + if (backend$get_column_info(input$color)$is_numeric) {
                scale_color_viridis_c(option = "plasma", end = 0.85)
            } else {
                scale_color_viridis_d(option = "plasma", end = 0.85)
            }
        }
        p + theme_minimal(base_size = 14)
    })

    output$counts <- renderText({
        n_sel <- th$n_selected()
        capped <- if (n_sel > PLOT_CAP) {
            sprintf(" (plotting a %s-row sample)", format(PLOT_CAP, big.mark = ","))
        } else ""
        sprintf("%s of %s rows pass filters%s",
                format(n_sel, big.mark = ","),
                format(backend$n_rows(), big.mark = ","), capped)
    })
}

shinyApp(ui, server)
