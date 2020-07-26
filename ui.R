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
library(psych)
library(plotly)


## UI ##
ui <- dashboardPage(skin = "yellow", 
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
                    fluidRow(
                        box(
                            tags$head(tags$style(HTML("a {color: red}"))),
                            background = "navy", width = 13,
                            htmlOutput("UCI_LOGO"),
                            span(uiOutput("UCI_URL"), style = "color:red")
                        )
                    ),
                    
                    fluidRow(
                        box(
                            tags$head(tags$style(HTML("a {color: red}"))),
                            title = "Heart Failure Clinical Records Dataset", width = 13,
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
                            following link: ", span(uiOutput("UCI_fullDATA"), style = "color:red"),tags$br(),
                            tags$b("Data Set Information: "),tags$br(),
                            "A detailed description of the dataset can be found in the following paper: ",tags$br(),
                            "Davide Chicco, Giuseppe Jurman: Machine learning can predict survival of patients 
                            with heart failure from serum creatinine and ejection fraction alone. BMC Medical 
                            Informatics and Decision Making 20, 16 (2020).",
                            span(uiOutput("UCI_ARTICLE"), style = "color:red"),tags$br()
                            
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "explore", h2("Basic Exploratory Data Analysis: Simple Descriptives & Histograms"),
                    fluidRow(
                        box(
                            title = "Title 6",width = 5, background = "black",
                            "A box with a solid black background"
                        )
                    ),
                    fluidRow(
                        box(
                            varSelectInput("variable", "Variable:", hfcrDATA)
                        )
                    )
            ),
            
            # Third tab content
            tabItem(tabName = "data",
                    box(title = "Heart Failure Dataset", width = 13,
                        "Below is a set of tabs containing: [TAB 1] a table of the 13 patient record 
                        features/variables included in the full heart failure clinical records dataset.  
                        The table provides what each feature  is, an explanation of the feature, 
                        how it is measured, and the range of values. [TAB 2] is a data table containing the 
                        full dataset so you can explore and see the variables and de-identified patient data.",
                        tags$br(),
                        ),
                    tabsetPanel(type = "tabs",
                                tabPanel("Variables", tableOutput("TABLE1")),
                                tabPanel("Dataset", DT::dataTableOutput("TABLE2"))
                    )
                    ),
            # Fourth tab content
            tabItem(tabName = "charts",
                    fluidRow(
                        box(title = "Histogram", background = "red", solidHeader = TRUE,
                            plotOutput("plot1", height = 500)),
                        
                        box(
                            title = "Controls", background = "black",
                            setSliderColor(c("red"),c(1)),
                            sliderInput("obs1", "Size of Points on Graph:",
                                        min = 1, max = 10, value = 5, step = 1)
                        )
                    ),
                    fluidRow(
                        box(
                            varSelectInput("variable", "Variable:", hfcrDATA)
                        )
                    )
            ),
            
            #Fifth tab content
            tabItem(tabName = "info",
                    h2("Info page that describes data and app abilities goes here"),
                    box(
                        title = "Data Description", width = 6, background = "navy",
                        "A box with a solid navy background"
                    ),
                    box(
                        title = "App Abilities", width = 6, background = "purple",
                        "A box with a solid white background"
                    ),
            )
        )
    )
)
