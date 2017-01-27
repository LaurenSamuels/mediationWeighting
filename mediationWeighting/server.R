#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    pA <- reactive(input$probA)
    pMA1 <- reactive(input$probM_A1)
    pMA0 <- reactive(input$probM_A0)
    
    dat <- reactive({
        
    })
    
    output$distPlot <- renderPlot({
    
        # generate bins based on input$bins from ui.R
        x    <- rnorm(1000)
        hist(x)
    
    })
  
})
