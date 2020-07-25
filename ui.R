## ui.R ##
## Programmed By: Zack Vaskalis ##
## Programmed Date: 07.27.2020 ##
## Programmed For: ST 558 Project 3 ##

## Load Libraries ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(htmltools)
library(colourpicker)
library(tidyverse)
library(dplyr)
library(DT)
library(plotly)


## UI ##
ui <- dashboardPage(skin = "red", 
    dashboardHeader(title = "ST 558 Project 3 - Zack Vaskalis", titleWidth = 350),
    ## Sidebar content
    dashboardSidebar(
        width = 175,
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Information", tabName = "info", icon = icon("info-circle")),
            menuItem("Data", tabName = "data", icon = icon("database")),
            menuItem("Data Exploration", tabName = "explore", icon = icon("search")),
            menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    h2("Project 3 Dashboard"),
                    fluidRow(
                        box(
                            tags$head(tags$style(HTML("a {color: yellow}"))),
                            background = "navy", width = 5,
                            htmlOutput("UCI_LOGO"),
                            span(uiOutput("UCI_URL"), style = "color:yellow")
                        )
                    ),
                    
                    fluidRow(
                        box(
                            tags$head(tags$style(HTML("a {color: yellow}"))),
                            title = "Heart Failure Clinical Records Dataset", width = 5,
                            background = "navy", 
                            "The data for this project comes from the University of California Irvine (UCI)
                            Machine Learning Repository. The project page can be accessed by clicking the 
                            following link: ", span(uiOutput("UCI_PROJ"), style = "color:yellow"),tags$br(),
                            "The full heart failure clinical records dataset contains de-identified medical 
                            records from 299 patients who had heart failure.  This information was collected 
                            during a follow-up period.  Each patient file is considered as one row of this 
                            dataset, where each patient profile contains 13 clinical features, which will be 
                            described in detail on the Information tab in the left-hand toolbar.  The full
                            dataset can be downloaded directly by clicking the 
                            following link: ", span(uiOutput("UCI_fullDATA"), style = "color:yellow"),tags$br(),
                            
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "explore",
                    h2("Data Exploration tab content"),
                    box(
                        title = "Title 6",width = 5, background = "black",
                        "A box with a solid black background"
                    ),
            ),
            
            # Third tab content
            tabItem(tabName = "info",
                    h2("Information tab content"),
                    tableOutput("TABLE1")
            ),
            # Fourth tab content
            tabItem(tabName = "charts",
                    fluidRow(
                        box(title = "Histogram", background = "red", solidHeader = TRUE,
                            plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls", background = "black",
                            setSliderColor(c("red"),c(1)),
                            sliderInput("obs1", "Size of Points on Graph:",
                                        min = 1, max = 10, value = 5, step = 1)
                        )
                    )
            ),
            
            #Fifth tab content
            tabItem(tabName = "data",
                    h2("Data goes here")
            )
        )
    )
)
