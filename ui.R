## ui.R ##

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

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
                    box(
                        title = "Title 5", width = 4, background = "red",
                        "A box with a solid light-blue background"
                    ),
                    box(
                        title = "Title 6",width = 4, background = "black",
                        "A box with a solid maroon background"
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
