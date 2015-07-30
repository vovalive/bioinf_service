
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
rm(list = ls(all=T))

##libs
library(ggplot2)
library(gplots)
library(RColorBrewer)
library(ggvis)
##

##data generation

##
brewercol=rev(brewer.pal(n = 9,name = 'Spectral'))

### file input
options(shiny.maxRequestSize = 9*1024^2)
###

shinyServer(function(input, output) {
  
  output$heatmap <- renderPlot({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      m=read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    heatmap.2(data.matrix(m),trace='none',col = brewercol,xlab = 'Samples',ylab = 'Features',key=T,main='Heatmap')
      })
  
  output$tree <- renderPlot({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      m=read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    plot(hclust(dist(m)),hang = -1,xlab = 'Samples',main='Classification tree')
  })
  
  output$pca <- renderPlot({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      m=read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    pca=prcomp(m)
    summary(pca)
    plot(pca$x[,1],pca$x[,2],pch=19,cex=3,main='Principal complnent analysis',xlab='PC 1',ylab='PC 2')
  })
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    m=read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    m
  })
  
  output$pcatable <- renderTable({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      m=read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    pca=prcomp(m)
    summary(pca)
  })
})


