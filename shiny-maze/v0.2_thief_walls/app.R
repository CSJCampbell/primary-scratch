# Copyright 2018 Chris Campbell

library(shiny)
library(ggplot2)
library(grid)
library(png)

background <- "thief_background.png"
source("is_colour.R")

ht <- 54L
wd <- 72L

# array, ignore alpha transparency layer
bgd <- readPNG(source = background)[, , 1:3]
# sprite pixel length
size <- ceiling(dim(bgd)[1L] * 0.01)

# interface is plot
background_plot <- ggplot(
    data = expand.grid(x = 0:1, y = 0:1), 
    mapping = aes(x = x, y = y)) + 
    geom_blank() +
    annotation_custom(
        rasterGrob(bgd, 
            width = unit(1L, "npc"), 
            height = unit(1L, "npc")),
        xmin = 0L, xmax = 1L, ymin = 0L, ymax = 1L) +
    theme_void() +  
    theme(plot.margin = unit(c(0, 0, -0.2, -0.2), "lines"))

# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("Burglar is not a ghost"),
    # keyboard bindings
    # http://www.foreui.com/articles/Key_Code_Table.htm
    tags$script('$(document).on("keydown",
        function (e) {
            if (e.which == 37) {
                Shiny.onInputChange("LButton", new Date());
            } else if (e.which == 38) {
                Shiny.onInputChange("UButton", new Date());
            } else if (e.which == 39) {
                Shiny.onInputChange("RButton", new Date());
            } else if (e.which == 40) {
                Shiny.onInputChange("DButton", new Date());
            }
        });
        '),
    # Sidebar with buttons
    sidebarLayout(
        sidebarPanel(
            width = 2,
            fluidRow(
                column(
                    width = 12,
                    align = "center", 
                    actionButton(
                        inputId = "Reset", 
                        label = "Reset"),
                    p(),
                    # arrow pad
                    actionButton(
                        inputId = "UButton", 
                        label = "U"),
                    p(),
                        actionButton(
                            inputId = "LButton", 
                            label = "L"),
                        actionButton(
                            inputId = "RButton", 
                            label = "R"),
                    p(),
                    actionButton(
                        inputId = "DButton", 
                        label = "D")
                )
            )
        ),
    # Show stage
    mainPanel(
        plotOutput(outputId = "map"))
    )
)

# Define server
server <- function(session, input, output) {
    values <- reactiveValues(pos = c(wd / 2L, ht / 2L))
    observeEvent(input$Reset,{
        values$pos <- c(wd / 2L, ht / 2L)
    })
    observeEvent(input$UButton,{
        new_pos <- values$pos + c(0L, 1L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
    })
    observeEvent(input$LButton,{
        new_pos <- values$pos + c(-1L, 0L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
    })
    observeEvent(input$RButton,{
        new_pos <- values$pos + c(1L, 0L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
    })
    observeEvent(input$DButton,{
        new_pos <- values$pos + c(0L, -1L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
    })
    output$map <- renderPlot({
        print(background_plot + 
            geom_point(
                x = values$pos[1L] / wd, 
                y = values$pos[2L] / ht, size = 3))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

