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
library(heatmaply)
library(dendextend)
library(tree)
library(caret)

## Code here that you only need to evaluate once. This can include reading in data, 
## creation of functions common to all sessions, and reading of other common r scripts.

# Objects in this file are shared across all sessions
source('all_sessions.R', local=FALSE)
    
## Server - Code here that can be reactive. Differs for every instance of your app that runs.
server <- function(input, output, session) {
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
    #varLIST <- select(hfcrDATA, Age:Time)
    
    observe({input$variable})
    
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

        x <- list(title = names(fig1())[n]) #replaced 1 with n
        y <- list(title = "Frequency")
        
        fig <- plot_ly(alpha = 0.5)
        fig <- fig %>% add_histogram(x = var1, name = "survived") #replaced fig1$Age with var1
        fig <- fig %>% add_histogram(x = var2, name = "dead") #replaced fig2$Age with var2
        fig <- fig %>% layout(xaxis = x, yaxis = y, barmode = "overlay")
        fig
    })
    

    
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
    
    varLIST2 <- reactive({filter(hfcrDATA, Target <2)}) #reactive({select(hfcrDATA, Anemia:Target)})
    varLIST3 <- reactive({filter(varLIST2(), Target==0)})
    varLIST4 <- reactive({filter(varLIST2(), Target==1)})
    
    observe({input$targetVar})
    
    # TABLE 6 - Full Dataset Categorical
    output$TABLE6 <- DT::renderDataTable({
        
        AnemiaF <- table(varLIST2()$Anemia)
        HBPF <- table(varLIST2()$HighBP)
        DiabF <- table(varLIST2()$Diabetes)
        SexF <- table(varLIST2()$Sex)
        SmokeF <- table(varLIST2()$Smoke)
        
        AnemiaFPer <- table(varLIST2()$Anemia)/299*100
        HBPFPer <- table(varLIST2()$HighBP)/299*100
        DiabFPer <- table(varLIST2()$Diabetes)/299*100
        SexFPer <- table(varLIST2()$Sex)/299*100
        SmokeFPer <- table(varLIST2()$Smoke)/299*100
        
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
        
        AnemiaS <- table(varLIST3()$Anemia)
        HBPS <- table(varLIST3()$HighBP)
        DiabS <- table(varLIST3()$Diabetes)
        SexS <- table(varLIST3()$Sex)
        SmokeS <- table(varLIST3()$Smoke)
        
        AnemiaSPer <- table(varLIST3()$Anemia)/203*100
        HBPSPer <- table(varLIST3()$HighBP)/203*100
        DiabSPer <- table(varLIST3()$Diabetes)/203*100
        SexSPer <- table(varLIST3()$Sex)/203*100
        SmokeSPer <- table(varLIST3()$Smoke)/203*100
        
        catSu <- 0 
        catSu <- tribble( ~'Category Feature', ~Count, ~Percent,
                          "Anemia (0-false)", AnemiaS[[1]],AnemiaSPer[[1]],
                          "Anemia (1-true)", AnemiaS[[2]],AnemiaSPer[[2]],
                          "High BP (0-false)", HBPS[[1]], HBPSPer[[1]],
                          "High BP (1-true)", HBPS[[2]],HBPSPer[[2]],
                          "Diabetes (0-false)", DiabS[[1]], DiabSPer[[1]],
                          "Diabetes (1-true)", DiabS[[2]], DiabSPer[[2]],
                          "Smoke (0-false)", SmokeS[[1]], SmokeSPer[[1]],
                          "Smoke (1-true)", SmokeS[[2]], SmokeSPer[[2]],
                          "Sex (0-woman)", SexS[[1]], SexSPer[[1]],
                          "Sex (1-man)", SexS[[2]], SexSPer[[2]])
        
        AnemiaD <- table(varLIST4()$Anemia)
        HBPD <- table(varLIST4()$HighBP)
        DiabD <- table(varLIST4()$Diabetes)
        SexD <- table(varLIST4()$Sex)
        SmokeD <- table(varLIST4()$Smoke)
        
        AnemiaDPer <- table(varLIST4()$Anemia)/96*100
        HBPDPer <- table(varLIST4()$HighBP)/96*100
        DiabDPer <- table(varLIST4()$Diabetes)/96*100
        SexDPer <- table(varLIST4()$Sex)/96*100
        SmokeDPer <- table(varLIST4()$Smoke)/96*100
        
        catDu <- 0 
        catDu <- tribble( ~'Category Feature', ~Count, ~Percent,
                          "Anemia (0-false)", AnemiaD[[1]],AnemiaDPer[[1]],
                          "Anemia (1-true)", AnemiaD[[2]],AnemiaDPer[[2]],
                          "High BP (0-false)", HBPD[[1]], HBPDPer[[1]],
                          "High BP (1-true)", HBPD[[2]],HBPDPer[[2]],
                          "Diabetes (0-false)", DiabD[[1]], DiabDPer[[1]],
                          "Diabetes (1-true)", DiabD[[2]], DiabDPer[[2]],
                          "Smoke (0-false)", SmokeD[[1]], SmokeDPer[[1]],
                          "Smoke (1-true)", SmokeD[[2]], SmokeDPer[[2]],
                          "Sex (0-woman)", SexD[[1]], SexDPer[[1]],
                          "Sex (1-man)", SexD[[2]], SexDPer[[2]])
        
        
        
        
        
        if (input$targetVar=="Full"){
        catFF <- formatRound(datatable(catF,rownames=FALSE),columns=c(3),digits=2)
        }
        
        else if (input$targetVar=="Survived")
        {
        catSuS <- formatRound(datatable(catSu,rownames=FALSE),columns=c(3),digits=2)
        }
        
        else 
        {
        catDuD <- formatRound(datatable(catDu,rownames=FALSE),columns=c(3),digits=2)
        }



    })
    
    # Barchart Figure Info
    output$FIGC <- renderPlotly({

        AnemiaF <- table(varLIST2()$Anemia)
        HBPF <- table(varLIST2()$HighBP)
        DiabF <- table(varLIST2()$Diabetes)
        SexF <- table(varLIST2()$Sex)
        SmokeF <- table(varLIST2()$Smoke)
        
        AnemiaFPer <- table(varLIST2()$Anemia)/299*100
        HBPFPer <- table(varLIST2()$HighBP)/299*100
        DiabFPer <- table(varLIST2()$Diabetes)/299*100
        SexFPer <- table(varLIST2()$Sex)/299*100
        SmokeFPer <- table(varLIST2()$Smoke)/299*100
        
        AnemiaS <- table(varLIST3()$Anemia)
        HBPS <- table(varLIST3()$HighBP)
        DiabS <- table(varLIST3()$Diabetes)
        SexS <- table(varLIST3()$Sex)
        SmokeS <- table(varLIST3()$Smoke)
        
        AnemiaSPer <- table(varLIST3()$Anemia)/203*100
        HBPSPer <- table(varLIST3()$HighBP)/203*100
        DiabSPer <- table(varLIST3()$Diabetes)/203*100
        SexSPer <- table(varLIST3()$Sex)/203*100
        SmokeSPer <- table(varLIST3()$Smoke)/203*100
        
        AnemiaD <- table(varLIST4()$Anemia)
        HBPD <- table(varLIST4()$HighBP)
        DiabD <- table(varLIST4()$Diabetes)
        SexD <- table(varLIST4()$Sex)
        SmokeD <- table(varLIST4()$Smoke)
        
        AnemiaDPer <- table(varLIST4()$Anemia)/96*100
        HBPDPer <- table(varLIST4()$HighBP)/96*100
        DiabDPer <- table(varLIST4()$Diabetes)/96*100
        SexDPer <- table(varLIST4()$Sex)/96*100
        SmokeDPer <- table(varLIST4()$Smoke)/96*100
        
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
        
        else if (input$targetVar == "Survived")
        {
            xVARSS <- c("Anemia", "High BP", "Diabetes", "Smoke", "Sex")
            ZerosS <- c(AnemiaS[[1]],HBPS[[1]],DiabS[[1]],SmokeS[[1]],SexS[[1]])
            OnesS <- c(AnemiaS[[2]],HBPS[[2]],DiabS[[2]],SmokeS[[2]],SexS[[2]])
            dataS <- tibble(xVARSS,ZerosS,OnesS)
            
            figS <- plot_ly(dataS, x = xVARSS, y = ZerosS, type = 'bar', name = '0')
            figS <- figS %>% add_trace(y = ~OnesS, name = '1')
            figS <- figS %>% layout(title = "Survived", yaxis = list(title = 'Count'), barmode = 'group')
            
            figS
        }
        
        else 
        {
            xVARSD <- c("Anemia", "High BP", "Diabetes", "Smoke", "Sex")
            ZerosD <- c(AnemiaD[[1]],HBPD[[1]],DiabD[[1]],SmokeD[[1]],SexD[[1]])
            OnesD <- c(AnemiaD[[2]],HBPD[[2]],DiabD[[2]],SmokeD[[2]],SexD[[2]])
            dataD <- tibble(xVARSD,ZerosD,OnesD)
            
            figD <- plot_ly(dataD, x = xVARSD, y = ZerosD, type = 'bar', name = '0')
            figD <- figD %>% add_trace(y = ~OnesD, name = '1')
            figD <- figD %>% layout(title = "Dead", yaxis = list(title = 'Count'), barmode = 'group')
            
            figD
        }
    })
    
    vLX <- reactive({filter(hfcrDATA, Target<2)})
    vLY <- reactive({filter(hfcrDATA, Target<2)})
    
    observe({input$varX})
    observe({input$varY})
    
    # Scatterplot Figure Info
    output$SPLOT <- renderPlotly({
        
        if (input$varX == "Age"){vX <- vLX()$Age}
        if (input$varX == "CPK"){vX <- vLX()$CPK}
        if (input$varX == "EF"){vX <- vLX()$EF}
        if (input$varX == "Platelet"){vX <- vLX()$Platelet}
        if (input$varX == "SCr"){vX <- vLX()$SCr}
        if (input$varX == "SNa"){vX <- vLX()$SNa}
        if (input$varX == "Time"){vX <- vLX()$Time}
        
        if (input$varY == "Age"){vY <- vLY()$Age}
        if (input$varY == "CPK"){vY <- vLY()$CPK}
        if (input$varY == "EF"){vY <- vLY()$EF}
        if (input$varY == "Platelet"){vY <- vLY()$Platelet}
        if (input$varY == "SCr"){vY <- vLY()$SCr}
        if (input$varY == "SNa"){vY <- vLY()$SNa}
        if (input$varY == "Time"){vY <- vLY()$Time}
        
        varLISTP <- select(hfcrDATA, Age:Time,Target)
        varLISTP$TargetC <- ifelse((varLISTP$Target==0),"Survived","Dead")
        scatrPlot <- plot_ly(data = varLISTP, type = "scatter",x = vX, y = vY,
                             color = ~TargetC, colors = "Set1", mode = "markers")
        scatrPlot <- scatrPlot %>% layout(xaxis = list(title = "X Variable"),
                                          yaxis = list(title = "Y Variable"))
        scatrPlot
    })

    # Scatterplot Figure Info
    output$DDGRAMHEATMAP <- renderPlotly({
        if (input$ddgramMeth == "centroid")
        {
        HC <- hclust(dist(data.frame(hfcrDATA)), method = "centroid")
        heatmaply(scale(hfcrDATA), k_row = 2, k_col = 2)
        }
        
        else if (input$ddgramMeth == "complete"){
        HC <- hclust(dist(data.frame(hfcrDATA)), method = "complete")
        heatmaply(scale(hfcrDATA), k_row = 2, k_col = 2)
        }
        
        else {
        HC <- hclust(dist(data.frame(hfcrDATA)), method = "average")
        heatmaply(scale(hfcrDATA), k_row = 2, k_col = 2)
        }
        
    })
    
    ## Logistic Regression
    
    
    
    
    
    
    
    
    
    
    
    

    ## Random Forest
    set.seed(1)
    alpha     <- 0.7 # percentage of training set
    inTrain   <- sample(1:nrow(hfcrDATA), alpha * nrow(hfcrDATA))
    trainSet <- hfcrDATA[inTrain,]
    testSet  <- hfcrDATA[-inTrain,]
    
    mtry <- c(2,3,sqrt(dim(trainSet)[2]-1),4,5,6)
    trCtrl1 <- trainControl(method = "cv", number = 10)
    fitRF <- reactive({train(Target ~ ., data = trainSet, method = "rf",
                   trControl = trCtrl1, preProcess = c("center", "scale"),
                   ntree = 500, tuneGrid = expand.grid(.mtry = mtry))})
    
    output$RFFIT<- renderPrint({
        fitRF()
    })
    
    output$RFFIT2<- renderPrint({
        fitRF()$finalModel 
    })
    
    output$RFFIT3<- renderPrint({
        predRF <- predict(fitRF(), newdata = testSet)
        predRF_RMSE <- sqrt(mean((predRF - testSet$Target)^2))
        paste0("Predicted RMSE = ",predRF_RMSE)
    })
    
    observe({input$mtrys})
    observe({input$ntrees})

    # User Results:
    
    set.seed(1)
    alpha1     <- 0.7 # percentage of training set
    inTrain1   <- sample(1:nrow(hfcrDATA), alpha1 * nrow(hfcrDATA))
    trainSet1  <- hfcrDATA[inTrain1,]
    testSet1   <- hfcrDATA[-inTrain1,]
    trCtrl2 <- trainControl(method = "cv", number = 10)
    
    mtry1  <- reactive({input$mtrys})
    nt1    <- reactive({input$ntrees})
    fitRF1 <- reactive({train(Target ~ ., data = trainSet1, method = "rf",
                    trControl = trCtrl2, preProcess = c("center", "scale"),
                    ntree = nt1(), tuneGrid = expand.grid(.mtry = mtry1()))})
    
    output$RFFIT4<- renderPrint({
        fitRF1()
    })
    
    output$RFFIT5<- renderPrint({
        fitRF1()$finalModel 
    })
    
    output$RFFIT6<- renderPrint({
        predRF1 <- predict(fitRF1(), newdata = testSet1)
        predRF1_RMSE <- sqrt(mean((predRF1 - testSet1$Target)^2))
        paste0("Predicted RMSE = ",predRF1_RMSE)
    })
}
