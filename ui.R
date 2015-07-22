
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Random data heatmap tree and pca"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("bins",
                "Set random seed:",
                min = 1,
                max = 10,
                value = 5),
    sliderInput("nsamples",
                "Samples number:",
                min = 5,
                max = 25,
                value = 15),
    sliderInput("nfeatures",
                "Features number:",
                min = 5,
                max = 25,
                value = 15)
  ),
 
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("heatmap"),
    plotOutput("tree"),
    plotOutput("pca")
  )
))
