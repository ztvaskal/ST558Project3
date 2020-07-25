## server.R ##
## Programmed By: Zack Vaskalis ##
## Programmed Date: 07.27.2020 ##
## Programmed For: ST 558 Project 3 ##

## Load Libraries ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(tidyverse)
library(dplyr)
library(DT)



server <- function(input, output, session) {
    # Stock TEMPLATE INFO
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$obs1)]
        hist(data)
    })
    # Picture of UCI Database
    output$UCI_LOGO <-
        renderText({
            c(
                '<img src="',
                "https://archive.ics.uci.edu/ml/assets/logo.gif",
                '">'
            )
        })
}
