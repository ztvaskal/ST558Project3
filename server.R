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
library(psych)
library(plotly)

## Code here that you only need to evaluate once. This can include reading in data, 
## creation of functions common to all sessions, and reading of other common r scripts.

## Read in dataset from UCI:
dataurl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv"
hfcrRAW <- read_csv(dataurl)

## Rename Variables
names(hfcrRAW)[1]<-"Age"
names(hfcrRAW)[2]<-"Anemia"
names(hfcrRAW)[3]<-"CPK"
names(hfcrRAW)[4]<-"Diabetes"
names(hfcrRAW)[5]<-"EF"
names(hfcrRAW)[6]<-"HighBP"
names(hfcrRAW)[7]<-"Platlet"
names(hfcrRAW)[8]<-"SCr"
names(hfcrRAW)[9]<-"SNa"
names(hfcrRAW)[10]<-"Sex"
names(hfcrRAW)[11]<-"Smoke"
names(hfcrRAW)[12]<-"Time"
names(hfcrRAW)[13]<-"Target"

## Reorder Variables
hfcrDATA <- hfcrRAW[,c(1,3,5,7,8,9,12,2,4,6,10,11,13)]

    
## Server - Code here that can be reactive. Differs for every instance of your app that runs.
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
        tagList("", uciurl)
    })
    uciproj <- a("Heart Failure Clinical Records Project Page",
                 href="https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records#")
    output$UCI_PROJ <- renderUI({
        tagList("", uciproj)
    })    
    dataurl <- a("Heart Failure Clinical Records Full Dataset",
                 href="https://archive.ics.uci.edu/ml/machine-learning-databases/00519/heart_failure_clinical_records_dataset.csv")
    output$UCI_fullDATA <- renderUI({
        tagList("", dataurl)
    })
    uciarticle <- a("[Web Link]",
                 href="https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-020-1023-5")
    output$UCI_ARTICLE <- renderUI({
        tagList("", uciarticle)
    })  
    
    #Info Table of Dataset variables
    table1 <- 0 
    table1 <- tribble( ~Feature, ~Explanation, ~Measurement, ~Range,
                       "Age", "Age of the patient", "Years", "[40,...,95]",
                       "Creatinine Phosphokinase (CPK)", "Level of the CPK enzyme in the blood", "mcg/L", "[23,...,7861]",
                       "Ejection Fraction (EF)", "Percentage of blood leaving the heart at each contraction", "Percentage", "[14,...80]",
                       "Platelet", "Platelets in the blood", "kilplatelets/mL", "[25.01,...,850.00]",
                       "Serum Creatinine (SCr)", "Level of creatinine in the blood", "mg/dL", "[0.50,...,9.40]",
                       "Serum Sodium (SNa)", "Level of sodium in the blood", "mEq/L", "[114,...,148]",
                       "Time", "Follow-up Period", "Days", "[4,...,285]",
                       "Anemia", "Decrease of Red Blood Cells or Hemoglobin", "Boolean", "0,1",
                       "High Blood Pressure (HighBP)", "If Patient has Hypertension", "Boolean", "0,1",
                       "Diabetes", "If Patient has Diabetes", "Boolean", "0,1",
                       "Sex", "Woman or Man", "Boolean", "0,1",
                       "Smoke", "If Patient Smokes", "Boolean", "0,1",
                       "Target (Death Event)", "If Patient Died during follow-up period", "Boolean", "0,1")

    #Output Table1 Information
    output$TABLE1 <- renderTable({
        data=table1
    })
    
    #Output Table2 Information
    output$TABLE2 <- DT::renderDataTable({
        hfcrDATA
    })

    
}
