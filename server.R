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
library(pipeR)

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
names(hfcrRAW)[7]<-"Platelet"
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
    
    # Info Table of Dataset variables
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

    # Output Table1 Information
    output$TABLE1 <- renderTable({
        data=table1
    })
    
    # Output Table2 Information
    output$TABLE2 <- DT::renderDataTable({
        hfcrDATA
    })

    # Prep for Plotly Histogram
    fig1 <- reactive({filter(hfcrDATA, Target==0)})
    fig2 <- reactive({filter(hfcrDATA, Target==1)})
    varLIST <- select(hfcrDATA, Age:Time)
    
    # Histogram Figure Info
    output$FIG <- renderPlotly({
        
        if (input$variable == "Age"){
            n <- 1
            var1 <- fig1()$Age
            var2 <- fig2()$Age
        }
        if (input$variable == "CPK"){
            n <- 2
            var1 <- fig1()$CPK
            var2 <- fig2()$CPK
        }
        if (input$variable == "EF"){
            n <- 3
            var1 <- fig1()$EF
            var2 <- fig2()$EF
        }
        if (input$variable == "Platelet"){
            n <- 4
            var1 <- fig1()$Platelet
            var2 <- fig2()$Platelet
        }
        if (input$variable == "SCr"){
            n <- 5
            var1 <- fig1()$SCr
            var2 <- fig2()$SCr
        }
        if (input$variable == "SNa"){
            n <- 6
            var1 <- fig1()$SNa
            var2 <- fig2()$SNa
        }
        if (input$variable == "Time"){
            n <- 7
            var1 <- fig1()$Time
            var2 <- fig2()$Time
        }

        x <- list(title = names(fig1)[n]) #replaced 1 with n
        y <- list(title = "Frequency")
        
        fig <- plot_ly(alpha = 0.5)
        fig <- fig %>% add_histogram(x = var1, name = "survived") #replaced fig1$Age with var1
        fig <- fig %>% add_histogram(x = var2, name = "dead") #replaced fig2$Age with var2
        fig <- fig %>% layout(xaxis = x, yaxis = y, barmode = "overlay")
        fig
    })
    
    varLIST <- dplyr::select(hfcrDATA, Age:Time)
    
    # Download subsetted dataset
    output$downloadData <- downloadHandler(
        
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
         content = function(con) {
         write_csv(varLIST, con)
       }
    )
    
    # TABLE3 - FULL Data Table of Selected Variables
    output$TABLE3 <- DT::renderDataTable({
        if (input$variable == "Age"){n <- 1}
        if (input$variable == "CPK"){n <- 2}
        if (input$variable == "EF"){n <- 3}
        if (input$variable == "Platelet"){n <- 4}
        if (input$variable == "SCr"){n <- 5}
        if (input$variable == "SNa"){n <- 6}
        if (input$variable == "Time"){n <- 7}
        
        # Prep Data for FULL Data Table
        df <- describeBy(hfcrDATA)
        df %>% filter(vars<8) %>>% (~ DF0)
        DF <- formatRound(datatable(DF0),columns=c(3:13),digits=2)
        DF %>% formatStyle('vars', target = 'row',
                           backgroundColor = styleEqual(c(n), c('lightgray'))) %>>% (~ cDF)
        cDF
    })
    
    # TABLE 4 - Survived Data Table of Selected Variables
    output$TABLE4 <- DT::renderDataTable({
        if (input$variable == "Age"){n <- 1}
        if (input$variable == "CPK"){n <- 2}
        if (input$variable == "EF"){n <- 3}
        if (input$variable == "Platelet"){n <- 4}
        if (input$variable == "SCr"){n <- 5}
        if (input$variable == "SNa"){n <- 6}
        if (input$variable == "Time"){n <- 7}
        
        # Prep Data for Survived Data Table
        hfcrDATA1 <- filter(hfcrDATA, Target==0)
        df1A <- describeBy(hfcrDATA1)
        df1A %>% filter(vars<8) %>>% (~ DF1B)
        DF1C <- formatRound(datatable(DF1B),columns=c(3:13),digits=2)
        DF1C %>% formatStyle('vars', target = 'row',
                           backgroundColor = styleEqual(c(n), c('lightgray'))) %>>% (~ cDF1C)
        cDF1C
    })
    
    # TABLE 5 - Dead Data Table of Selected Variables
    output$TABLE5 <- DT::renderDataTable({
        if (input$variable == "Age"){n <- 1}
        if (input$variable == "CPK"){n <- 2}
        if (input$variable == "EF"){n <- 3}
        if (input$variable == "Platelet"){n <- 4}
        if (input$variable == "SCr"){n <- 5}
        if (input$variable == "SNa"){n <- 6}
        if (input$variable == "Time"){n <- 7}
        
        # Prep Data for Dead Data Table
        hfcrDATA2 <- filter(hfcrDATA, Target==1)
        df2A <- describeBy(hfcrDATA2)
        df2A %>% filter(vars<8) %>>% (~ DF2B)
        DF2C <- formatRound(datatable(DF2B),columns=c(3:13),digits=2)
        DF2C %>% formatStyle('vars', target = 'row',
                             backgroundColor = styleEqual(c(n), c('lightgray'))) %>>% (~ cDF2C)
        cDF2C
    })    
    
    # TABLE 6 - Full Dataset Categorical
    output$TABLE6 <- DT::renderDataTable({
        
        varLIST2 <- dplyr::select(hfcrDATA, Anemia:Target)
        AnemiaF <- table(varLIST2$Anemia)
        HBPF <- table(varLIST2$HighBP)
        DiabF <- table(varLIST2$Diabetes)
        SexF <- table(varLIST2$Sex)
        SmokeF <- table(varLIST2$Smoke)
        
        AnemiaFPer <- table(varLIST2$Anemia)/299*100
        HBPFPer <- table(varLIST2$HighBP)/299*100
        DiabFPer <- table(varLIST2$Diabetes)/299*100
        SexFPer <- table(varLIST2$Sex)/299*100
        SmokeFPer <- table(varLIST2$Smoke)/299*100
        
        catF <- 0 
        catF <- tribble( ~'Category Feature', ~Count, ~Percent,
                         "Anemia (0-false)", AnemiaF[[1]],AnemiaFPer[[1]],
                         "Anemia (1-true)", AnemiaF[[2]],AnemiaFPer[[2]],
                         "High BP (0-false)", HBPF[[1]], HBPFPer[[1]],
                         "High BP (1-true)", HBPF[[2]],HBPFPer[[2]],
                         "Diabetes (0-false)", DiabF[[1]], DiabFPer[[1]],
                         "Diabetes (1-true)", DiabF[[2]], DiabFPer[[2]],
                         "Smoke (0-false)", SmokeF[[1]], SmokeFPer[[1]],
                         "Smoke (1-true)", SmokeF[[2]], SmokeFPer[[2]],
                         "Sex (0-woman)", SexF[[1]], SexFPer[[1]],
                         "Sex (1-man)", SexF[[2]], SexFPer[[2]])
        catFF <- formatRound(datatable(catF,rownames=FALSE),columns=c(3),digits=2)
        catFF
        


    })
    
    # Barchart Figure Info
    output$FIGC <- renderPlotly({
        varLIST <- dplyr::select(hfcrDATA, Anemia:Target)
        
        if (input$targetVar == "Full")
        {
        xVARSF <- c("Anemia", "High BP", "Diabetes", "Smoke", "Sex")
        ZerosF <- c(AnemiaF[[1]],HBPF[[1]],DiabF[[1]],SmokeF[[1]],SexF[[1]])
        OnesF <- c(AnemiaF[[2]],HBPF[[2]],DiabF[[2]],SmokeF[[2]],SexF[[2]])
        dataF <- tibble(xVARSF,ZerosF,OnesF)
        
        figF <- plot_ly(dataF, x = xVARSF, y = ZerosF, type = 'bar', name = '0')
        figF <- figF %>% add_trace(y = ~OnesF, name = '1')
        figF <- figF %>% layout(title = "Full Dataset", yaxis = list(title = 'Count'), barmode = 'group')
        
        figF
        }
    })
    
    
}
