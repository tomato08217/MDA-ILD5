setwd("D:/renji/ILD")
library(survival)
library(glmnet)
library(openxlsx)

data1<-read.xlsx("clinicaltable.xlsx",sheet=1)

for (i in c(2:4,8,14:28,30,36,37,41,44,48,53,57:61)){
  data1[,i] <- as.factor(data1[,i])
}
for (i in c(5:7,9:13,29,31:35,38:40,42,43,45:47,49:52,54,56)){
  data1[,i] <- as.numeric(as.character(data1[,i]))
}
age.group<-ifelse(data1$age>55,'high','low')
data1<-cbind(data1,age.group)
data1$result<-as.numeric(as.character(data1$result))
train<-subset(data1,train==1)

y_train<- Surv(train$survival.time,train$result) 

source("stdca.R")
library(survival)

CRmodel<- coxph(Surv(survival.time,result,type = "right")~radscore+age.group+FVC.rate, data=train)
Rmodel<- coxph(Surv(survival.time,result,type = "right")~radscore, data=train)
Vmodel<- coxph(Surv(survival.time,result,type = "right")~HRCTscore, data=train)

data2<-na.omit(data1)
##
data2$CRfail = c(1- (summary(survfit(CRmodel, newdata=data2), times=24)$surv))
data2$Rfail= c(1- (summary(survfit(Rmodel, newdata=data2), times=24)$surv))
data2$Vfail= c(1- (summary(survfit(Vmodel, newdata=data2), times=24)$surv))

CR=stdca(data=data2, outcome="result", ttoutcome="survival.time", predictors= c("CRfail"),timepoint=4,smooth=TRUE)
R=stdca(data=data2, outcome="result", ttoutcome="survival.time", predictors= c("Rfail"),timepoint=24,smooth=TRUE)
V=stdca(data=data2, outcome="result", ttoutcome="survival.time", predictors= c("Vfail"),timepoint=24,smooth=TRUE)

plot(CR$net.benefit.threshold, CR$net.benefit.none, type = "l", lwd=2, xlim=c(0,1.0), ylim=c(-0.05,0.5), xlab = "Threshold Probability",ylab = "Net Benefit")
lines(CR$net.benefit$threshold, CR$net.benefit$all, type="l", col=8, lwd=0.5)
lines(CR$net.benefit$threshold, CR$net.benefit$none, type="l", col=8, lwd=0.5,lty=2)
lines(CR$net.benefit$threshold, CR$net.benefit$CRfail, type="l", col="red",lwd=2)
lines(R$net.benefit$threshold, R$net.benefit$Rfail, type="l", col = "blue",lwd=2)
lines(V$net.benefit$threshold, C$net.benefit$Vfail, type="l", col = "black",lwd=2)
legend("topright", inset=.05, c("All", "None","clinical model","radiomics model","clinical plus radiomics model"),lty=c(1,2,1,1,1),cex=0.3, col=c("grey","grey","blue","black", "red"),lwd=c(1,1,2,2,2),bty="o")

##
library(ggDCA)
library(rms)
CRmodel<- cph(Surv(survival.time,result)~radscore+age.group+FVC.rate, data=train)
Rmodel<- cph(Surv(survival.time,result)~radscore, data=train)
visualmodel<- cph(Surv(survival.time,result)~HRCTscore, data=train)

#on new dataset
d_all<-dca(CRmodel,Rmodel,visualmodel,times=24,new.data=data2)
ggplot(d_all)
