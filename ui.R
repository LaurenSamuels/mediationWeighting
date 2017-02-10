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
            sliderInput("probC",
                "P(C)",
                min = 0,
                max = 1,
                value = 0.5),
            tags$hr(),
            sliderInput("probA_C0",
                "P(A | C = 0)",
                min = 0,
                max = 1,
                value = 0.5),
            sliderInput("probA_C1",
                "P(A | C = 1)",
                min = 0,
                max = 1,
                value = 0.5),
            tags$hr(),
            sliderInput("probM_A0C0",
                "P(M | A = 0, C = 0)",
                min = 0,
                max = 1,
                value = 0.5),
            sliderInput("probM_A0C1",
                "P(M | A = 0, C = 1)",
                min = 0,
                max = 1,
                value = 0.5),
            sliderInput("probM_A1C0",
                "P(M | A = 1, C = 0)",
                min = 0,
                max = 1,
                value = 0.5),
            sliderInput("probM_A1C1",
                "P(M | A = 1, C = 1)",
                min = 0,
                max = 1,
                value = 0.5)
            ), # end sidebarPanel

        mainPanel(
            plotOutput("wtPlot"),
            tags$hr(),
            h4("Original sample, shown by A"),
            verbatimTextOutput("showtabOrig"),
            h4("Fully weighted sample, shown by A"),
            verbatimTextOutput("showtabWtdW"),
            tags$hr(),
            h4("W_A-weighted sample, shown by A"),
            verbatimTextOutput("showtabWtdWa"),
            tags$hr(),
            h4("W_M-weighted sample, shown by A"),
            verbatimTextOutput("showtabWtdWm"),
            tags$hr()
        )
    )
))
