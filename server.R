
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
require(ROCR)
library(ROCR)
library(xtable)
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

shinyServer(function(input, output){
  
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
    heatmap.2(m,trace='none',col = brewercol,xlab = 'Образцы',ylab = 'Признаки',key=T,main='Тепловая карта')
      })#close output$heatmap
  
  output$downloadHeatmap.png <- downloadHandler(
    filename = "heatmap.png",
    content = function(file) {
      png(file)
      if (is.null(input$file1)){
        set.seed(input$bins)
        heatmap.2(x=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures),trace='none',col = brewercol,xlab = 'Образцы',ylab = 'Признаки',key=T,main='Тепловая карта') 
        }else{
      heatmap.2(x=data.matrix(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote)),trace='none',col = brewercol,xlab = 'Образцы',ylab = 'Признаки',key=T,main='Тепловая карта')}
      dev.off()
    }) #close downloadHeatmap.png 
  output$downloadHeatmap.pdf <- downloadHandler(
    filename = "heatmap.png",
    content = function(file) {
      pdf(file)
      if (is.null(input$file1)){
        set.seed(input$bins)
        heatmap.2(x=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures),trace='none',col = brewercol,xlab = 'Образцы',ylab = 'Признаки',key=T,main='Тепловая карта')}
        else{
      heatmap.2(x=data.matrix(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote)),trace='none',col = brewercol,xlab = 'Образцы',ylab = 'Признаки',key=T,main='Тепловая карта')}
      dev.off()
    }) #close downloadHeatmap.pdf  
    output$tree <- renderPlot({
    set.seed(input$bins)
    inFile <- input$file1
    if (is.null(inFile)){
      m=matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures)}
    else
      {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
       row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
       m$name=NULL
       m=data.matrix(m)}
    plot(hclust(dist(m)),hang = -1,ylab="Расстояние",xlab = 'Образцы',main='Дерево классификации')
  })#close output$tree
  
    output$downloadTree.png <- downloadHandler(
      filename = "tree.png",
      content = function(file) {
        png(file)
        if (is.null(input$file1)){
          set.seed(input$bins)
          plot(hclust(dist(matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures))),hang = -1,ylab="Расстояние",xlab = 'Образцы',main='Дерево классификации')}
          else{
        plot(hclust(dist(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))),hang = -1,ylab="Расстояние",xlab = 'Образцы',main='Дерево классификации')}
        dev.off()
      })  #close downloadTree.png
    output$downloadTree.pdf <- downloadHandler(
      filename = "tree.png",
      content = function(file) {
        pdf(file)
        if (is.null(input$file1)){
          set.seed(input$bins)
          plot(hclust(dist(matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures))),hang = -1,ylab="Расстояние",xlab = 'Образцы',main='Дерево классификации')}
          else{
        plot(hclust(dist(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))),hang = -1,ylab="Расстояние",xlab = 'Образцы',main='Дерево классификации')}
        dev.off()
      })  #close downloadTree.pdf 
    
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
    plot(pca$x[,1],pca$x[,2],pch=19,cex=3,main='Анализ главных компонент',xlab='Главная компонента 1',ylab='Главная компонента 2')
  })#close output$pca
  
  output$downloadPca.png <- downloadHandler(
    filename = "pca.png",
    content = function(file) {
      png(file)
      if (is.null(input$file1)){
        set.seed(input$bins)
        m<-prcomp(matrix(rnorm(n = input$nsamples*20,mean = 5,sd = .1), ncol=input$nsamples,nrow=input$nfeatures))
        plot(m$x[,1],m$x[,2],pch=19,cex=3,main='Анализ главных компонент',xlab='Главная компонента 1',ylab='Главная компонента 2')}
        else{
      plot(m$x[,1],m$x[,2],pch=19,cex=3,main='Анализ главных компонент',xlab='Главная компонента 1',ylab='Главная компонента 2')}
      dev.off()
    })#close downloadPca.png  
  output$downloadPca.pdf <- downloadHandler(
    filename = "pca.png",
    content = function(file) {
      pdf(file)
      if (is.null(input$file1)){
        set.seed(input$bins)
        plot(prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,1],prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,2],pch=19,cex=3,main='Анализ главных компонент',xlab='Главная компонента 1',ylab='Главная компонента 2')}
        else{
      plot(prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,1],prcomp(read.table(input$file1$datapath, header = input$header,sep = input$sep, quote = input$quote))$x[,2],pch=19,cex=3,main='Анализ главных компонент',xlab='Главная компонента 1',ylab='Главная компонента 2')}
      dev.off()
    })#close downloadPca.pdf  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    else
    {m=read.table(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
     row.names(m)=paste(m$name,seq(1:nrow(m)),sep='.')
     m$name=NULL
     m=data.matrix(m)}
    m
  })#close output$contents
  
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
  })#close output$pcatable
##

output$crosstable<-renderTable({
  m<-data.frame(matrix(data=c(a<-input$"++",b<-input$"+-",a+b,c<-input$"-+",d<-input$"--",c+d,a+c,b+d,a+b+c+d),nrow = 3,ncol=3,byrow = F,dimnames = list(c("Тест положительный","Тест отрицательный","Всего"),c("Признак положительный","Признак отрицательный","Всего"))))
  xtable(m)
  })
output$sens<-renderText({ ##здесь и далее дов интервалы клоппера-пирсона
  a<-input$"++"
  b<-input$"-+"
  c<-input$"+-"
  d<-input$"--"
  rateTrue<-a/(a+c)
  ###надо допилить, пока работает с ошибками, используется встроенная функция binom.test
  if (input$diagclevel=="95%")  conf<-0.05
  if (input$diagclevel=="90%")  conf<-0.1
  if (input$diagclevel=="99%")  conf<-0.01
  if (input$methodCI=="Клоппера-Пирсона") method<-"exact"
  if (input$methodCI=="Агрести-Коула") method<-"agresti-coull"
  if (input$methodCI=="Уилсона")    method<-"wilson"
  if (input$methodCI=="Асимптотика") method<-"asymptotic"
  if (input$methodCI=="Logit") method<-"logit"
  if (input$methodCI=="Probit") method<-"probit"
  if (input$methodCI=="cloglog") method<-"cloglog"
#   E<-z*sqrt(rateTrue*(1-rateTrue)/(a+c))
#   CIsens<-paste("Доверительный интервал", (rateTrue-E)*100, " - ", (rateTrue+E)*100)
  CIsens<-NA
  if (is.finite(rateTrue)){
  binom<-binom.confint(x = a, n=a+c,conf.level = conf,methods = method)
  CIsens<-paste("Доверительный интервал",round(binom$lower*100,digits=4)," - ",round(binom$upper*100,digits=4),"%")
  }else{
  CIsens<-paste("Доверительный интервал"," - ","%")}  
  if (input$diagci==T) {paste("Чувствительность теста равна",round(rateTrue*100),"%;",CIsens)}
  else {paste("Чувствительность теста равна",round(rateTrue*100),"%")}
})
output$spec<-renderText({
  a<-input$"++"
  b<-input$"-+"
  c<-input$"+-"
  d<-input$"--"
  if (input$diagclevel=="95%")  conf<-0.05
  if (input$diagclevel=="90%")  conf<-0.1
  if (input$diagclevel=="99%")  conf<-0.01
  if (input$methodCI=="Клоппера-Пирсона") method<-"exact"
  if (input$methodCI=="Агрести-Коула") method<-"agresti-coull"
  if (input$methodCI=="Уилсона")    method<-"wilson"
  if (input$methodCI=="Асимптотика") method<-"asymptotic"
  if (input$methodCI=="Logit") method<-"logit"
  if (input$methodCI=="Probit") method<-"probit"
  if (input$methodCI=="cloglog") method<-"cloglog"
  rateTrue<-d/(b+d)
  CIsens<-NA
  if (is.finite(rateTrue)){
    binom<-binom.confint(x = d, n=b+d,conf.level = conf,methods = method)
    CIsens<-paste("Доверительный интервал",round(binom$lower*100,digits=4)," - ",round(binom$upper*100,digits=4),"%")
  }else{
    CIsens<-paste("Доверительный интервал"," - ","%")}  
  if (input$diagci==T) {paste("Специфичность теста равна",round(rateTrue*100),"%;",CIsens)}
  else {paste("Специфичность теста равна",round(rateTrue*100),"%")}
})
output$progpos<-renderText({
    a<-input$"++"
    b<-input$"-+"
    c<-input$"+-"
    d<-input$"--"
    if (input$diagclevel=="95%")  conf<-0.05
    if (input$diagclevel=="90%")  conf<-0.1
    if (input$diagclevel=="99%")  conf<-0.01
    if (input$methodCI=="Клоппера-Пирсона") method<-"exact"
    if (input$methodCI=="Агрести-Коула") method<-"agresti-coull"
    if (input$methodCI=="Уилсона")    method<-"wilson"
    if (input$methodCI=="Асимптотика") method<-"asymptotic"
    if (input$methodCI=="Logit") method<-"logit"
    if (input$methodCI=="Probit") method<-"probit"
    if (input$methodCI=="cloglog") method<-"cloglog"
    rateTrue<-a/(a+b)
    CIsens<-NA
    if (is.finite(rateTrue)){
      binom<-binom.confint(x = a, n=a+b,conf.level = conf,methods = method)
      CIsens<-paste("Доверительный интервал",round(binom$lower*100,digits=4)," - ",round(binom$upper*100,digits=4),"%")
    }else{
      CIsens<-paste("Дов. инт."," - ","%")}  
    if (input$diagci==T) {paste("Прог. ценность положительного результата теста равна",round(rateTrue*100),"%;",CIsens)}
    else {paste("Прог. ценность положительного результата теста равна",round(rateTrue*100),"%")}
})
output$progneg<-renderText({
a<-input$"++"
b<-input$"-+"
c<-input$"+-"
d<-input$"--"
if (input$diagclevel=="95%")  conf<-0.05
if (input$diagclevel=="90%")  conf<-0.1
if (input$diagclevel=="99%")  conf<-0.01
if (input$methodCI=="Клоппера-Пирсона") method<-"exact"
if (input$methodCI=="Агрести-Коула") method<-"agresti-coull"
if (input$methodCI=="Уилсона")    method<-"wilson"
if (input$methodCI=="Асимптотика") method<-"asymptotic"
if (input$methodCI=="Logit") method<-"logit"
if (input$methodCI=="Probit") method<-"probit"
if (input$methodCI=="cloglog") method<-"cloglog"
rateTrue<-d/(c+d)
CIsens<-NA
if (is.finite(rateTrue)){
  binom<-binom.confint(x = d, n=d+c,conf.level = conf,methods = method)
  CIsens<-paste("Доверительный интервал",round(binom$lower*100,digits=4)," - ",round(binom$upper*100,digits=4),"%")
}else{
  CIsens<-paste("Дов. инт."," - ","%")}  
if (input$diagci==T) {paste("Прог. ценность отрицательного результата теста равна",round(rateTrue*100),"%;",CIsens)}
else {paste("Прог. ценность отрицательного результата теста равна",round(rateTrue*100),"%")}
})
output$diagOR<-renderText({ ##диаг отношение шансов с дов интервалом вальда
  a<-input$"++"
  b<-input$"-+"
  c<-input$"+-"
  d<-input$"--"
  if (input$diagclevel=="95%")  {conf<-0.05; z<-qnorm(1-(0.05/2))}
  if (input$diagclevel=="90%")  {conf<-0.1; z<-qnorm(1-0.(05/2))}
  if (input$diagclevel=="99%")  {conf<-0.01; z<-qnorm(1-0.(05/2))}
  OR<-(a*d)/(b*c)
  s<-sqrt((1/a)+(1/b)+(1/c)+(1/d))
  logOR<-log(OR)
  up<-exp(logOR + z*s)
  lo<-exp(logOR - z*s)
  print(paste("Диаг. отношение шансов",round(OR,digits=4),";","Дов. инт.",round(lo,digits = 4),"-",round(up,digits=4),";"))
})
output$likelihoodpos<-renderText({ ##Koopman 1984 http://www.jstor.org/stable/2531405?seq=1#page_scan_tab_contents
  a<-input$"++"
  b<-input$"-+"
  c<-input$"+-"
  d<-input$"--"
  if (input$diagclevel=="95%")  {conf<-0.05; z<-qnorm(1-(0.05/2))}
  if (input$diagclevel=="90%")  {conf<-0.1; z<-qnorm(1-(0.05/2))}
  if (input$diagclevel=="99%")  {conf<-0.01; z<-qnorm(1-(0.05/2))}
  spec<-d/(b+d)
  sens<-a/(a+c)
  lr<-sens/(1-spec)
  if (a!=0 & b!=0){
    s<-sqrt((1/a)-(1/(a+c))+(1/b)-(1/(b+d)))
    lo<-lr*exp(-z*s)
    up<-lr*exp(z*s)
  } else if (a==0 & b==0){
      lo<-0
      up<-Inf
  } else if ( a == 0 & b != 0 ){
    a.temp <- (1/2)
    
    spec.temp <- d/(b+d)
    sens.temp <- a.temp/(a+c)
    
    lr0.5 <- sens.temp/(1 - spec.temp)  
    
    lo <- 0
    
    s <- sqrt((1/a.temp) - (1/(a.temp+c)) + (1/b) - (1/(b+d)))
    up <-lr0.5*exp(z*s)
    } else if ( a != 0 & b == 0 ) {
      b.temp <- (1/2)
      spec.temp <- d/(b.temp+d)
      sens.temp <- a/(a+c)
      lr0.5 <- sens.temp/(1 - spec.temp) 
      s <- sqrt((1/a) - (1/(a+c)) + (1/b.temp) - (1/(b.temp+d)))
      lo<- lr0.5 * exp(-z*s)
      up<- Inf  
    }  else if ( (a == (a+c)) & (b == (b+d)) ) {
      a.temp <- a - (1/2)
      b.temp <- b - (1/2)
      
      spec.temp <- d/(b.temp+d)
      sens.temp <- a.temp/(a+c)
      
      lr0.5 <- sens.temp/(1 - spec.temp) 
      
      s <- sqrt((1/a.temp) - (1/(a.temp+c)) + (1/b.temp) - (1/(b.temp+d)))
      
      lo <- lr0.5 * exp(-z*s)
      
      up<- lr0.5 * exp(z*s)
    }
  print(paste("Отношение правдоподобия для пол. результата",round(lr,digits = 4),";","Дов. инт.",round(lo,digits = 4),"-",round(up,digits=4),";"))
})
output$likelihoodneg<-renderText({ ##Koopman 1984 http://www.jstor.org/stable/2531405?seq=1#page_scan_tab_contents
  a<-input$"++"
  b<-input$"-+"
  c<-input$"+-"
  d<-input$"--"
  if (input$diagclevel=="95%")  {conf<-0.05; z<-qnorm(1-(0.05/2))}
  if (input$diagclevel=="90%")  {conf<-0.1; z<-qnorm(1-(0.05/2))}
  if (input$diagclevel=="99%")  {conf<-0.01; z<-qnorm(1-(0.05/2))}
  spec<-d/(b+d)
  sens<-a/(a+c)
  lrneg<-(1-sens)/spec
  if (c!=0 & d!=0){
    s<-sqrt((1/c)-(1/(a+c))+(1/d)-(1/(b+d)))
    lo<-lrneg*exp(-z*s)
    up<-lrneg*exp(z*s)
  } else if (c==0 & d==0){
    lo<-0
    up<-Inf
  } else if ( c == 0 & d != 0 ){
    c.temp <- (1/2)
    
    spec.temp <- d/(b+d)
    sens.temp <- a/(a+c.temp)
    
    lr0.5 <- (1-sens.temp)/spec.temp  
    
    lo <- 0
    
    s <- sqrt((1/c.temp) - (1/(a+c)) + (1/d) - (1/(b+d)))
    up <-lr0.5*exp(z*s)
  } else if ( c != 0 & d == 0 ) {
    d.temp <- (1/2)
    spec.temp <- d.temp/(b+d)
    sens.temp <- a/(a+c)
    lr0.5 <- (1-sens.temp)/spec.temp
    s <- sqrt((1/c) - (1/(a+c)) + (1/d.temp) - (1/(b+d)))
    lo<- lr0.5 * exp(-z*s)
    up<- Inf  
  }  else if ( (a == (a+c)) & (b == (b+d)) ) {
    c.temp <- a - (1/2)
    d.temp <- b - (1/2)
    
    spec.temp <- d.temp/(b+d.temp)
    sens.temp <- a/(a+c.temp)
    
    lr0.5 <- (1-sens.temp)/spec.temp 
    
    s <- sqrt((1/c.temp) - (1/(a+c)) + (1/d.temp) - (1/(b+d.temp)))
    
    lo <- lr0.5 * exp(-z*sqrt(s))
    
    up<- lr0.5 * exp(z*sqrt(s))
  }
  print(paste("Отношение правдоподобия для отр. результата",round(lrneg,digits = 4),";","Дов. инт.",round(lo,digits = 4),"-",round(up,digits=4),";"))
})
output$accuracy<-renderText({
  a<-input$"++"
  b<-input$"-+"
  c<-input$"+-"
  d<-input$"--"
  if (input$diagclevel=="95%")  conf<-0.05
  if (input$diagclevel=="90%")  conf<-0.1
  if (input$diagclevel=="99%")  conf<-0.01
  if (input$methodCI=="Клоппера-Пирсона") method<-"exact"
  if (input$methodCI=="Агрести-Коула")    method<-"agresti-coull"
  if (input$methodCI=="Уилсона")          method<-"wilson"
  if (input$methodCI=="Асимптотика")      method<-"asymptotic"
  if (input$methodCI=="Logit")            method<-"logit"
  if (input$methodCI=="Probit")           method<-"probit"
  if (input$methodCI=="cloglog")          method<-"cloglog"
  rateTrue<-(a+d)/(a+b+c+d)
  CIsens<-NA
  if (is.finite(rateTrue)){
    binom<-binom.confint(x = a+d, n=a+b+d+c,conf.level = conf,methods = method)
    CIsens<-paste("Доверительный интервал",round(binom$lower*100,digits=4)," - ",round(binom$upper*100,digits=4),"%")
  }else{
    CIsens<-paste("Дов. инт."," - ","%")}  
  if (input$diagci==T) {paste("Точность теста равна",round(rateTrue*100),"%;",CIsens)}
  else {paste("Точность теста равна",round(rateTrue*100),"%")}
})
output$roc<-renderPlot({
  inf<-input$file2
  if (is.null(inf)) {
    set.seed(1234)
    num<-rnorm(n=100,mean = 0,sd = 1)
    bin<-rep(c(0,1),50)
    matr<-data.frame(num,bin)
    matr[,1]<-as.numeric(matr[,1])
    matr[,2]<-as.factor(matr[,2])
    pred<-prediction(matr[,1],matr[,2])
    perf<-performance(pred,"tpr","fpr")
    plot(perf,colorize=T, print.cutoff.at = seq(0,1,0.5),ylab="Чувствительность",xlab="1-специфичность")}
    else{
  m=read.table(inf$datapath, header = input$header2,sep = input$sep2)
  matr<-data.frame(m)
  matr[,1]<-as.numeric(matr[,1])
  matr[,2]<-as.factor(matr[,2])
  pred<-prediction(matr[,1],matr[,2])
  perf<-performance(pred,"tpr","fpr")
  plot(perf,colorize=T, print.cutoff.at = seq(0,1,0.5),ylab="Чувствительность",xlab="1-специфичность")
  abline(a=1,b=0)
    }
  })
output$auc<-renderText({
  inf<-input$file2
if (is.null(inf)) {
  set.seed(1234)
  num<-rnorm(n=100,mean = 0,sd = 1)
  bin<-rep(c(0,1),50)
  matr<-data.frame(num,bin)
  matr[,1]<-as.numeric(matr[,1])
  matr[,2]<-as.factor(matr[,2])
  pred<-prediction(matr[,1],matr[,2])
  auctemp<-performance(pred,"auc")
  auc <- as.numeric(auctemp@y.values)
  print(paste("Рассчитанное значение AUC равно",auc))}
else{
  m=read.table(inf$datapath, header = input$header2,sep = input$sep2)
  matr<-data.frame(m)
  matr[,1]<-as.numeric(matr[,1])
  matr[,2]<-as.factor(matr[,2])
  pred<-prediction(matr[,1],matr[,2])
  auctemp<-performance(pred,"auc")
  auc <- as.numeric(auctemp@y.values)
  print(paste("Рассчитанное значение AUC равно",auc))
}
  })
output$cutoff<-renderPlot({
  inf<-input$file2
  if (is.null(inf)) {
  set.seed(1234)
  num<-rnorm(n=100,mean = 0,sd = 1)
  bin<-rep(c(0,1),50)
  matr<-data.frame(num,bin)
  matr[,1]<-as.numeric(matr[,1])
  matr[,2]<-as.factor(matr[,2])
  pred<-prediction(matr[,1],matr[,2])
  plot(performance(pred,"sens","cutoff"),col="red",lwd=1.5,ylab="",xlab="Порог отсечки")
  plot((performance(pred,"spec","cutoff")),add=T,col="green",lwd=1.5)
  plot((performance(pred,"acc","cutoff")),add=T,col="blue",lwd=1.5)
  abline(v = input$cutoffline)
  if (input$legcut==T){
  legend(x=1,y=1,legend=c("Чувствительность","Специфичность","Точность"),col = c("red","green","blue"),lty = 1,lwd = 2,bty = "n")
  } 
  }
  else{
    m=read.table(inf$datapath, header = input$header2,sep = input$sep2)
    matr<-data.frame(m)
    matr[,1]<-as.numeric(matr[,1])
    matr[,2]<-as.factor(matr[,2])
    pred<-prediction(matr[,1],matr[,2])
    plot(performance(pred,"sens","cutoff"),col="red",lwd=1.5,ylab="",xlab="Порог отсечки")
    plot((performance(pred,"spec","cutoff")),add=T,col="green",lwd=1.5)
    plot((performance(pred,"acc","cutoff")),add=T,col="blue",lwd=1.5)
    abline(v = input$cutoffline)
      if (input$legcut==T){
    legend(x=1,y=1,legend=c("Чувствительность","Специфичность","Точность"),col = c("red","green","blue"),lty = 1,lwd = 2,bty = "n")
      }
    }
})
})#close server function


