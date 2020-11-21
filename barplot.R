setwd("D:/renji/ILD")
library(ggplot2)
library(openxlsx)
library(ggpubr)

barplot<-read.xlsx("all_1198.xlsx",sheet=3)#
ggbarplot(barplot,x="Feature",y="HR",fill="Feature",rotate=T,ylab="Hazard Ratio",sort.val="asc",sort.by.groups=F)

