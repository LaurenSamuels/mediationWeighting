#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
    titlePanel("Section 4.5"),
  
    sidebarLayout(
        sidebarPanel(
            sliderInput("probA",
                "P(A)",
                min = 0,
                max = 1,
                value = 0.5),
            sliderInput("probM_A1",
                "P(M | A = 1)",
                min = 0,
                max = 1,
                value = 0.5),
            sliderInput("probM_A0",
                "P(M | A = 0)",
                min = 0,
                max = 1,
                value = 0.5)
            ), # end sidebarPanel

        mainPanel(
            plotOutput("distPlot")
        )
    )
))
