
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
rm(list = ls(all=T))
# dev.off()
##libs
library(ggplot2)
library(gplots)
library(RColorBrewer)
#library(ggvis)

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
      {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
      row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
      m$name=NULL
      m=data.matrix(m)}
    heatmap.2(m,trace='none',col = brewercol,xlab = 'Samples',ylab = 'Features',key=T,main='Heatmap')
      })
  output$downloadHeatmap.png <- downloadHandler(
    filename = "heatmap.png",
    content = function(file) {
      png(file)
      heatmap.2(x=data.matrix(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote)),trace='none',col = brewercol,xlab = 'Samples',ylab = 'Features',key=T,main='Heatmap')
      dev.off()
    })  
  output$downloadHeatmap.pdf <- downloadHandler(
    filename = "heatmap.png",
    content = function(file) {
      pdf(file)
      heatmap.2(x=data.matrix(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote)),trace='none',col = brewercol,xlab = 'Samples',ylab = 'Features',key=T,main='Heatmap')
      dev.off()
    })   
    output$tree <- renderPlot({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
       row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
       m$name=NULL
       m=data.matrix(m)}
    plot(hclust(dist(m)),hang = -1,xlab = 'Samples',main='Classification tree')
  })
  
    output$downloadTree.png <- downloadHandler(
      filename = "tree.png",
      content = function(file) {
        png(file)
        plot(hclust(dist(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))),hang = -1,xlab = 'Samples',main='Classification tree')
        dev.off()
      })  
    output$downloadTree.pdf <- downloadHandler(
      filename = "tree.png",
      content = function(file) {
        pdf(file)
        plot(hclust(dist(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))),hang = -1,xlab = 'Samples',main='Classification tree')
        dev.off()
      })   
    
  output$pca <- renderPlot({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
       row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
       m$name=NULL
       m=data.matrix(m)}
    pca=prcomp(m)
    summary(pca)
    plot(pca$x[,1],pca$x[,2],pch=19,cex=3,main='Principal complnent analysis',xlab='PC 1',ylab='PC 2')
  })
  
  output$downloadPca.png <- downloadHandler(
    filename = "pca.png",
    content = function(file) {
      png(file)
      plot(prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,1],prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,2],pch=19,cex=3,main='Principal complnent analysis',xlab='PC 1',ylab='PC 2')
      dev.off()
    })  
  output$downloadPca.pdf <- downloadHandler(
    filename = "pca.png",
    content = function(file) {
      pdf(file)
      plot(prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,1],prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,2],pch=19,cex=3,main='Principal complnent analysis',xlab='PC 1',ylab='PC 2')
      dev.off()
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
    
    {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
     row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
     m$name=NULL
     m=data.matrix(m)}
    m
  })
  
  output$pcatable <- renderTable({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile))
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)
    else
      {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
       row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
       m$name=NULL
       m=data.matrix(m)}
    pca=prcomp(m)
    summary(pca)
  })
})


#####тестируем гит