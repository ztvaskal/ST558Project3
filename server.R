## app.R ##

library(shiny)
library(shinydashboard)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$obs1)]
        hist(data)
    })
}