setwd("D:/renji/ILD")
library(survival)
library(glmnet)
library(openxlsx)
data1<-read.xlsx("all_1198.xlsx")
for (i in c(1,2,5,10,11,13,16)){
  data1[,i] <- as.factor(data1[,i])
}
for (i in c(3,4,6:9,15,17:19)){
  data1[,i] <- as.numeric(as.character(data1[,i]))
}

train<-subset(data1,train==1)

library(rms) 
library(prodlim)
library(lava)
library(riskRegression)

CRmodel<-cph(Surv(Survival,CustomLabel)~rad_score+OI.200+FVC.rate,x=T,y=T,data=train,surv=T,time.inc=24)#define predicion time point

Rmodel<-cph(Surv(Survival,CustomLabel)~rad_score,x=T,y=T,data=train,surv=T,time.inc=24)

Vmodel<-cph(Surv(Survival,CustomLabel)~HRCT_score,x=T,y=T,data=train,surv=T,time.inc=24)#define predicion time point


cal_cr<- calibrate(CRmodel,cmethod='KM', method='boot', u=24, m=25,B=500)
plot(cal_cr,lwd = 2,lty = 1,errbar.col = c("#D70033"),xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("#D70033"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6,main="Calibration Curve for Combined Model")
abline(0,1,lty= 3, lwd = 2, col =c("#224444"))

cal_r<- calibrate(Rmodel,cmethod='KM', method='boot', u=24, m=25,B=500)
plot(cal_r,lwd = 2,lty = 1,errbar.col = c("#D70033"),xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("black"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6,main="Calibration Curve for Radiomics Model")
abline(0,1,lty= 3, lwd = 2, col =c("#D70033"))

cal_v<- calibrate(Vmodel,cmethod='KM', method='boot', u=24, m=25,B=500)
plot(cal_v,lwd = 2,lty = 1,errbar.col = c("#0000CD"),xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("blue"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6,main="Calibration Curve for Clinical Model")
abline(0,1,lty= 3, lwd = 2, col =c("#224444"))

#test
cf=calPlot(list("CRmodel"=CRmodel,"Rmodel"=Rmodel,"Vmodel"=Vmodel),
            time=24,type="risk",data=test)
print(cf)
plot(cf)

cf1=calPlot(list("CRmodel"=CRmodel,"Rmodel"=Rmodel,"Vmodel"=Vmodel),
           time=24,type="risk",data=internal)
print(cf1)
plot(cf1)

cf2=calPlot(list("CRmodel"=CRmodel,"Rmodel"=Rmodel,"Vmodel"=Vmodel),
            time=24,type="risk",data=external)
print(cf2)
plot(cf2)
