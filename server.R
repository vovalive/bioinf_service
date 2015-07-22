
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
##

##data generation

##
brewercol=rev(brewer.pal(n = 9,name = 'Spectral'))


shinyServer(function(input, output) {
  
  output$heatmap <- renderPlot({
    set.seed(input$bins)
    m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1),ncol=input$nsamples,nrow=input$nfeatures)
    heatmap.2(m,trace='none',col = brewercol,xlab = 'Samples',ylab = 'Features',key=T,main='Heatmap')
      })
  
  output$tree <- renderPlot({
    set.seed(input$bins)
    m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    plot(hclust(dist(m)),hang = -1,xlab = 'Samples',main='Classification tree')
  })
  
  output$pca <- renderPlot({
    set.seed(input$bins)
    m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    pca=prcomp(m)
    summary(pca)
    plot(pca$x[,1],pca$x[,2],pch=19,cex=3,main='Principal complnent analysis',xlab='PC 1',ylab='PC 2')
  })
})


