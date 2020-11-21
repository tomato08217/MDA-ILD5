setwd("D:/renji/ILD")
library(survival)
library(glmnet)
library(Hmisc)
library(openxlsx)
data1<-read.xlsx("all_1198.xlsx",sheet=4)#
for (i in c(1,2,5,10,11,13,16)){
  data1[,i] <- as.factor(data1[,i])
}
for (i in c(3,4,6:9,15,17:19)){
  data1[,i] <- as.numeric(as.character(data1[,i]))
}
age.group<-ifelse(data1$age>55,'high','low')
data1<-cbind(data1,age.group)
train<-subset(data1,train==1)
test<-subset(data1,train==0)
external<-subset(data1,train==2)
internal<-subset(data1,train==3)
#read the data
d<-train
d<-test
d<-external
d<-internal

#we decided we are interested in calibration at 36 mo and 60 mo,
#so censor after 24
#already done in excel

#calculate predicted probability at pre-specified time (adm.cens)
survcox_d<-coxph(data=train, Surv(Survival,CustomLabel)~rad_score+age.group+FVC.rate+LDH_rate)
summary(survcox_d)
survfit_d=survfit(survcox_d, newdata=d, se.fit=FALSE)


survcox_e<-coxph(data=train, Surv(Survival,CustomLabel)~rad_score)
summary(survcox_e)
survfit_e=survfit(survcox_e, newdata=d, se.fit=FALSE)


survcox_f<-coxph(data=train, Surv(Survival,CustomLabel)~HRCT_score)
summary(survcox_f)
survfit_f=survfit(survcox_f, newdata=d, se.fit=FALSE)

survpr24=survfit_d$surv[20,]
estsurv24=survpr24
estinc24=1-survpr24



#split into deciles
d$dec24=as.numeric(cut2(estinc24, g=3))


#check that there are 5 or more events in each group
#if not then collapse groups
table(d$dec24, d$CustomLabel)

source("GND_test.v2.r")
#calculate the GND test
GND.result1=GND.calib(pred=estinc24, tvar=d$Survival, out=d$CustomLabel, 
                     cens.t=24, groups=d$dec24, adm.cens=24)

GND.result1



