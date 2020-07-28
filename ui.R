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
library(pipeR)
library(heatmaply)
library(dendextend)
library(tree)
library(caret)
library(MASS)


## UI ##
ui <- dashboardPage(skin = "yellow", 
    dashboardHeader(title = "ST 558 Project 3 - Zack Vaskalis", titleWidth = 350),
    ## Sidebar content
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("heartbeat")),
            menuItem("Information", tabName = "info", icon = icon("info-circle")),
            menuItem("Data", tabName = "data", icon = icon("database")),
            menuItem("Data Exploration - Numeric Variables", tabName = "explore1", icon = icon("area-chart")),
            menuItem("Data Exploration - Categorical Variables", tabName = "explore2", icon = icon("bar-chart")),
            menuItem("Scatter Plot", tabName = "scatter", icon = icon("line-chart")),
            menuItem("Clustering - Unsupervised", tabName = "cluster", icon = icon("object-group")),
            menuItem("Random Forest - Supervised", tabName = "randomforest", icon = icon("tree")),
            menuItem("Logisitic Regression - Supervised", tabName = "logreg", icon = icon("sitemap"))
        )
    ),
    ## Body content
    dashboardBody(

        tabItems(
            #DASHBOARD tab content
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
                            with heart failure from serum creatinine and ejection fraction alone.",tags$i("BMC Medical 
                            Informatics and Decision Making 20, 16 (2020)."),
                            span(uiOutput("UCI_ARTICLE"), style = "color:red"),tags$br()
                            
                        )
                    )
            ),
            
            #EXPLORE1 tab content
            tabItem(tabName = "explore1",
                    h2("Basic Exploratory Data Analysis: Simple Descriptives & Histograms"),
                    fluidRow(
                        box(width = 5,
                            title = "Histogram", solidHeader = TRUE,
                            plotlyOutput(outputId = "FIG")
                            ),
                        box(
                            "You can use the dropdown menu below to select which numeric variable you
                            would like to investigate.  The histogram plot to the left is created using
                            the R plotly package.  It includes two layers, seperated by color,
                            blue for those who survived, and orange for those patients who died during
                            follow-up.  You can click your mouse and select a rectangular area
                            of the plot to zoom-in on.  Additionally, when your mouse hovers over the plot
                            you will see a toolbar appear at the top. The first option, denoted by a camera
                            icon is to download the plot as a png.",icon("camera"),tags$br(),tags$br(),
                            selectInput("variable", "Variable:",
                                        choices = c("Age","CPK","EF","Platelet","SCr","SNa","Time"),
                                        selected = "Age"),
                            "Additionally, you can download the dataset used to produce the plot by clicking
                            on the download button below.",tags$br(),tags$br(),
                            downloadButton('downloadData', 'Download'),tags$br(),tags$br(),
                            "Also, below is a tabset of summary data for the numerical variables showcasing
                            standard descriptive statistics.  The first tab is for the full dataset, the
                            second is for patients who survived, and the third is for those patients
                            who died during the follow-up period."
                            )
                    ),
                    fluidRow(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Full Dataset", DT::dataTableOutput("TABLE3")),
                                    tabPanel("Survived", DT::dataTableOutput("TABLE4")),
                                    tabPanel("Dead", DT::dataTableOutput("TABLE5"))
                        )
                    )
            ),
            
            #EXPLORE1 tab content
            tabItem(tabName = "explore2",
                    h2("Basic Exploratory Data Analysis: Categorical Frequencies & Bar Charts"),
                    fluidRow(
                        box(width = 5,title = "Categorical Data", solidHeader = TRUE,
                            DT::dataTableOutput("TABLE6")
                            ),
                        box(width = 5,title = "Barchart", solidHeader = TRUE,
                            plotlyOutput(outputId = "FIGC")
                        ),
                        box(width = 5,
                            selectInput("targetVar", "Dataset Type:",
                                        choices = c("Full", "Survived", "Dead"),
                                        selected = "Full")
                            )
                        )
                    ),

            #DATA tab content
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
            #CHARTS tab content
            tabItem(tabName = "scatter",
                    fluidRow(
                        box(title = "Scatterplot",
                            plotlyOutput(outputId = "SPLOT")
                            ),
                        box(
                            "You can use the dropdown menu below to select the numeric variables you
                            would like to investigate to explore the relationship between.  For the x variable,
                            you can choose from any one of the 7 numeric variables in the dataset.  Similarly,
                            for the y variable you can choose from the same set of 7 variables.  Thus, you could
                            plot a variable against itself, which would be a straight line.  Based on the analysis
                            of the dataset, however, the default status is actually a scatterplot of interest, as
                            the relationship between a patient's serum creatinine level and ejection fraction turns
                            out to be of interest, as you can see a clear distinction between the two groups of
                            the target variable when viewing the relationship between SCr and EF.",tags$br(),tags$br(),
                            "The scatterplot to the left is again created using the R plotly package.
                            It includes the two selected variables, x and y, and plots them grouped by color,
                            on the target variable status, blue for those who survived,
                            and red for those patients who died during follow-up.
                            Just as before, you can click your mouse and select a rectangular area
                            of the plot to zoom-in on.  Additionally, when your mouse hovers over the plot
                            you will see a toolbar appear at the top. The first option, denoted by a camera
                            icon is to download the plot as a png.",icon("camera")
                            )
                            ),
                    fluidRow(
                        box(
                            selectInput("varX", "X Variable:",
                                        choices = c("Age","CPK","EF","Platelet","SCr","SNa","Time"),
                                        selected = "SCr")
                            ),
                        box(
                            selectInput("varY", "Y Variable:",
                                        choices = c("Age","CPK","EF","Platelet","SCr","SNa","Time"),
                                        selected = "EF")
                            )
                            )
                    ),
            
            #INFO tab content
            tabItem(tabName = "info",
                    h2("Info page that describes data and app abilities goes here"),
                    box(
                        title = "Data Description", width = 6, background = "navy",
                        "The dashboard page introduced the heart failure clinical records
                        dataset very briefly.  The dataset will be introduced in a bit more
                        detail here, however, for a full in depth look at the attributes and
                        variables contained for each de-identified patient record, please
                        refer to the next tab on the left-hand menu: Data.",tags$br(),tags$br(),
                        "The dataset contains 13 variables for each de-identified patient record.
                        THere are 7 numeric variables, 5 dichotomous categorical variables, and
                        1 dichotmous variable, target, which is the event of interest, either
                        the patient survived or died during the follow-up period.",tags$br(),tags$br(),
                        "The 7 numeric variables of interest are age: age, creatinine phosphokinase (CPK),
                        the ejection fraction: percentage of blood leaving the heart at each contraction,
                        platelets in the blood, serum creatinine, serum sodium, and time or the length
                        of days of the follow-up period.",tags$br(),tags$br(),
                        "The 5 categorical variables are sex (0-woman, 1-man), smoke (0-false, 1-true),
                        anemia (0-false, 1-true), high blood pressure (0-false, 1-true), and
                        diabetes (0-false, 1-true)."
                    ),
                    box(
                        title = "App Abilities", width = 6, background = "green",
                        "A box with a solid white background"
                    )
            ),
            
            #Cluster tab content
            tabItem(tabName = "cluster",
                    h2("Unsupervised Learning - Hierarchical Clustering: Dendrogram"),
                    fluidRow(
                        box(width = 8, title = "Dendrogram for Heart Failure Dataset", solidHeader = TRUE,
                            plotlyOutput(outputId = "DDGRAMHEATMAP")
                        ),
                        box(width = 3,
                            "Please be patient as this plot may take a moment to load",tags$br(),tags$br(),
                            "Please choose from the three dendrogram methods. The default method
                            for this application is - centroid.",tags$br(),tags$br(),
                            selectInput("ddgramMeth", "Dendrogram Method:",
                                        choices = c("centroid", "complete", "average"),
                                        selected = "centroid"),
                            "The dendrogram hierarchical cluster uses the R package heatmaply. This is very similar
                            to the other graphs produced by the plotly package, i.e. you can select an area of
                            interest on the plot and zoom in, allowing you to see the de-identified patient
                            record numbers on the left hand axis.  You can also save this plot as a png, the same
                            way you did previously.  Any time you need to reset, simply click on the
                            Home button.",icon("home")
                        )
                    )
            ),
            
            #LOGREG tab content
            tabItem(tabName = "logreg",
                    fluidRow(
                        box(
                            width = 5,
                            "Logistic Regression is perfect to handle this scenario, where outcome trying to
                            predict is survived or death.  First we use the MASS package to run a forward
                            stepwise logistic regression, using method = glmStepAIC.  This finds the combination
                            of variables that minimizes the AIC score.",
                            verbatimTextOutput("LOGREGFITa")
                        ),
                        box(
                            
                        )
                    ),
                    fluidRow(
                        box(width = 5

                        ),
                        box(
                            
                        )
                    )
            ),
            
            #Random Forest tab content
            tabItem(tabName = "randomforest",
                    fluidRow(
                        box(
                            width = 6,
                            "Training Data Results:",
                            verbatimTextOutput("RFFIT"),
                            verbatimTextOutput("RFFIT2"),
                            "Test Data Results:",
                            verbatimTextOutput("RFFIT3")
                            ),
                        box(
                            width = 6,
                            tags$b("Please be patient, your defaults will also run your code in a few seconds."),
                            tags$br(),tags$br(),
                            "Now it's your turn to try. Your output will appear here. See below to select values.",
                            tags$br(),tags$br(),
                            "Training Data Results:",
                            verbatimTextOutput("RFFIT4"),
                            verbatimTextOutput("RFFIT5"),
                            "Test Data Results:",
                            verbatimTextOutput("RFFIT6")
                            )
                    ),
                    fluidRow(
                        box(
                            width = 6,
                            tags$b("Please be patient, this code may take a full minute to run."),
                            tags$br(),tags$br(),
                            "This random forest model uses 10-fold cross-validation (cv) and
                            was trained on a set of values for mtry, shown above,
                            the number of variables randomly sampled as candidates at each split, where the
                            default values are different for classification and regression.  If there
                            are p variables, the typical values are:",
                            withMathJax(helpText("for classification: $$\\sqrt{p}$$")),
                            withMathJax(helpText("& for regression: $$\\frac{p}{3}$$")),tags$br(),tags$br()
                        ),
                        box(
                            width = 6,
                            "Specify a numeric value for mtry between 1 and 12.",tags$br(),
                            numericInput("mtrys","Specify mtry [1,12]: ", min = 1, max = 12,
                                         value = 1, step = 0.001),
                            "Specify the number of trees, ntree. Default for ntree in the R caret package is 500.
                            Typical values vary depending on which source you consult.  Can be as small as 1
                            and in some cases greater than 2000.  There is tradeoff between number of trees and
                            efficiency. Having ntree values over 1000 may run slower depending on the speed of
                            your processor. For this applet, you are capped at 2500.",tags$br(),
                            numericInput("ntrees","Specify ntree [1,2500]: ", min = 1, max = 2500,
                                         value = 1, step = 1)
                        )
                    )
            )
        )
    )
)
