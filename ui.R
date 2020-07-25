## ui.R ##
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


## UI ##
ui <- dashboardPage(skin = "red", 
    dashboardHeader(title = "ST 558 Project 3 - Zack Vaskalis", titleWidth = 350),
    ## Sidebar content
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Information", tabName = "info", icon = icon("info-circle")),
            menuItem("Data", tabName = "data", icon = icon("database")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th")),
            menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    h2("Dashboard content")
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
            ),
            
            # Third tab content
            tabItem(tabName = "info",
                    fluidRow(
                        box(
                            background = "navy",
                            htmlOutput("UCI_LOGO"), 
                        )
                    ),
                    
                    fluidRow(
                        box(
                            title = "Title 5", width = 6, background = "red",
                            "A box with a solid red background"
                        ),
                        box(
                            title = "Title 6",width = 6, background = "black",
                            "A box with a solid black background"
                        )
                    )
            ),
            # Fourth tab content
            tabItem(tabName = "charts",
                    fluidRow(
                        box(title = "Histogram", background = "red", solidHeader = TRUE,
                            plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls", background = "black",
                            setSliderColor(c("red"),c(1)),
                            sliderInput("obs1", "Number of observations:", 1, 100, 50)
                        )
                    )
            ),
            
            #Fifth tab content
            tabItem(tabName = "data",
                    h2("Data Information goes here"))
        )
    )
)
