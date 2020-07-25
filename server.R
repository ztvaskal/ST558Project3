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
library(plotly)



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
    uciurl <- a("Center for Machine Learning and Intelligent Systems",
                 href="http://cml.ics.uci.edu/")
    output$UCI_URL <- renderUI({
        tagList("URL:", uciurl)
    })
    uciproj <- a("Heart Failure Clinical Records Project Page",
                 href="https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records#")
    output$UCI_PROJ <- renderUI({
        tagList("URL:", uciproj)
    })    
    dataurl <- a("Heart Failure Clinical Records Full Dataset",
                 href="https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv")
    output$UCI_fullDATA <- renderUI({
        tagList("URL:", dataurl)
    })
    
    #Info Table of Dataset variables
    table1 <- 0 
    table1 <- tribble( ~Feature, ~Explanation, ~Measurement, ~Range,
                       "Age", "Age of the patient", "Years", "[40,...,95]")

    #Output Table1 Information
    output$TABLE1 <- renderTable({
        data=table1
    })
    
    
    
    
}
