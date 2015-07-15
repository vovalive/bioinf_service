rm(list = ls(all=T))

##libs
library(ggplot2)
library(gplots)
library(RColorBrewer)
##

##data generation
set.seed(5)
m=matrix(rnorm(n = 1000,mean = 5,sd = .1), 50)
head(m)
##

dev.off()
brewercol=rev(brewer.pal(n = 9,name = 'Spectral'))
heatmap.2(m,trace='none',col = brewercol,xlab = 'Samples',ylab = 'Features',key=T,main='Heatmap')

plot(hclust(dist(m)),hang = -1,xlab = 'Samples',main='Classification tree')

pca=prcomp(m)
summary(pca)
plot(pca$x[,1],pca$x[,2],pch=19,cex=3,main='Principal complnent analysis',xlab='PC 1',ylab='PC 2')
