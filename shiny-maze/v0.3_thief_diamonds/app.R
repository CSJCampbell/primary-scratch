# Copyright 2018 Chris Campbell

library(shiny)
library(ggplot2)
library(ggimage)
library(grid)
library(png)

background <- "thief_background.png"
source("is_colour.R")
source("not_touched.R")

ht <- 54L
wd <- 72L
diamondsx <- c(0.2, 0.3, 0.4, 0.9)
diamondsy <- c(0.5, 0.6, 0.81, 0.7)

# array, ignore alpha transparency layer
bgd <- readPNG(source = background)[, , 1:3]
# sprite pixel length
sizep <- 0.01
size <- ceiling(dim(bgd)[1L] * sizep)

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
    titlePanel("Burglar loves diamonds"),
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
    # Sidebar with a slider input for number of bins 
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
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput(outputId = "map"))
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    values <- reactiveValues(
        pos = c(wd / 2L, ht / 2L), 
        diamondsx = diamondsx, 
        diamondsy = diamondsy,
        diamondss = rep(TRUE, times = length(diamondsx)))
    observeEvent(input$Reset,{
        values$pos <- c(wd / 2L, ht / 2L)
        values$diamondsx = diamondsx
        values$diamondsy = diamondsy
        values$diamondss = rep(TRUE, times = length(diamondsx))
    })
    observeEvent(input$UButton,{
        new_pos <- values$pos + c(0L, 1L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
        values$diamondss <- not_touched(
            pos = values$pos / c(wd, ht), 
            sizep = sizep, 
            x = values$diamondsx, 
            y = values$diamondsy, 
            s = values$diamondss)
    })
    observeEvent(input$LButton,{
        new_pos <- values$pos + c(-1L, 0L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
        values$diamondss <- not_touched(
            pos = values$pos / c(wd, ht), 
            sizep = sizep, 
            x = values$diamondsx, 
            y = values$diamondsy, 
            s = values$diamondss)
    })
    observeEvent(input$RButton,{
        new_pos <- values$pos + c(1L, 0L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
        values$diamondss <- not_touched(
            pos = values$pos / c(wd, ht), 
            sizep = sizep, 
            x = values$diamondsx, 
            y = values$diamondsy, 
            s = values$diamondss)
    })
    observeEvent(input$DButton,{
        new_pos <- values$pos + c(0L, -1L)
        if (!is_colour(pos = new_pos / c(wd, ht), size = size, bgarray = bgd)) {
            values$pos <- new_pos
        }
        values$diamondss <- not_touched(
            pos = values$pos / c(wd, ht), 
            sizep = sizep, 
            x = values$diamondsx, 
            y = values$diamondsy, 
            s = values$diamondss)
    })
    output$map <- renderPlot({
        print(background_plot + 
            geom_image(
                data = data.frame(
                    x = values$diamondsx, 
                    y = values$diamondsy)[values$diamondss, , drop = FALSE],
                mapping = aes(
                    x = x, 
                    y = y), 
                image = "diamond.png") +
            geom_label(x = 0.1, y = 0.9, label = paste("Diamonds:", 
                length(values$diamondss) - sum(values$diamondss))) + 
            geom_point(
                x = values$pos[1L] / wd, 
                y = values$pos[2L] / ht, 
                size = 3))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

