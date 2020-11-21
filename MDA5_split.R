setwd("D:/renji/ILD")
library(survival)
library(glmnet)
library(openxlsx)
#table1
data1<-read.xlsx("clinicaltable.xlsx",sheet=1)

for (i in c(2:4,8,14:28,30,36,37,41,44,48,53,57:61)){
  data1[,i] <- as.factor(data1[,i])
}
for (i in c(5:7,9:13,29,31:35,38:40,42,43,45:47,49:52,54,56)){
  data1[,i] <- as.numeric(as.character(data1[,i]))
}
library(compareGroups)
restab<-descrTable(group ~ ., data = data1,method=NA,show.all=TRUE)
export2csv(restab, file='tables1.csv')
#train/test
table_small<-subset(data1,group=="de")
restab2<-descrTable(train~ ., data = table_small,method=NA,show.all=TRUE)
export2csv(restab2, file='tables2.csv')
restab3<-descrTable(result~ ., data = table_small,method=NA,show.all=TRUE)
export2csv(restab3, file='tables3.csv')

library(mice)
data_missing<-md.pattern(data1)
write.csv(data_missing,"miss.csv")

#check status of data
str(data1)
data1$FVC.rate<-factor(data1$FVC.rate,order=FALSE,levels=c('0','1','2'))
data1$OI.200<-factor(data1$OI.200,order=FALSE,levels=c('0','1'))
data1$result<-as.numeric(as.character(data1$result))
age.group<-ifelse(data1$age>55,'high','low')
risk.group<-ifelse(data1$radscore>1,'high','low')
data1<-cbind(data1,age.group,risk.group)
data1$age.group<-factor(data1$age.group,order=FALSE,levels=c('low','high'))

train<-subset(data1,train==1)
test<-subset(data1,train==0)
external<-subset(data1,train==2)
internal<-subset(data1,train==3)
y_train<- Surv(train$survival.time,train$result) 
y_test<- Surv(test$survival.time,test$result) 
y_internal<- Surv(internal$survival.time,internal$result) 
y_external<- Surv(external$survival.time,external$result) 

#median and comparison
#comparison
library(survminer)
survdiff(Surv(Survival,CustomLabel)~train,data=data1)
#median
fit1<-survfit(Surv(Survival,CustomLabel)~train,data=data1)
surv_median(fit1)
#Rmodel
Rmodel <- coxph(y_train~radscore, data = train)
summary(Rmodel)
method<- survConcordance(Surv(train$survival.time,train$result) ~ predict(Rmodel, train))
cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
method<- survConcordance(Surv(test$survival.time,test$result) ~ predict(Rmodel, test))
cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
method<- survConcordance(Surv(internal$survival.time,internal$result) ~ predict(Rmodel, internal))
cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
method<- survConcordance(Surv(external$survival.time,external$result) ~ predict(Rmodel, external))
cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)


#build clinical+radiomics model
CRmodel<-coxph(y_train~radscore+age.group+FVC.rate,train)
summary(CRmodel)

#build visual model
Vmodel<-coxph(y_train~HRCTscore,train)
#AIC
CRmodel<- step(CRmodel, direction = "backward")
#BIC
CRmodel = step(CRmodel, direction = "backward", trace = TRUE,  k = log(nrow(train)))

#forest map
library(survminer)
ggforest(CRmodel,main="hazard ratio of Rad-clinical model",cpositions=c(0.02,0.22,0.4),fontsize=0.8,refLabel="reference",noDigits=2,data=train)
#nomogram
library(rms)
dd<-datadist(train)
options(datadist="dd")
#build with cph
crmodel2<-cph(y_train~radscore+age.group+FVC.rate,x=T,y=T,data=train,surv=T,time.inc=24)#define predicion time point
surv<- Survival(crmodel2)
surv1<-function(x)surv(24,lp=x)
nom<-nomogram(crmodel2, fun=surv1,funlabel=c("6 months Survival Probability"),lp=F,fun.at=c('0.90','0.70','0.5','0.3','0.1'),maxscale=10)
plot(nom,xfrac = 0.6)
#DCA
library(ggDCA)
library(rms)
CRmodel<- cph(Surv(survival.time,result)~radscore+age.group+FVC.rate, data=train)
Rmodel<- cph(Surv(survival.time,result)~radscore, data=train)
visualmodel<- cph(Surv(survival.time,result)~HRCTscore, data=train)

#on new dataset
d_all<-dca(CRmodel,Rmodel,visualmodel,times=24,new.data=data1)

#NRI IRI
library(survIDINRI)
t0=24
indata0=as.matrix(subset(train,select=c(Survival,CustomLabel,FVC_rate,OI_rate)))
indata1=as.matrix(subset(train,select=c(Survival,CustomLabel,rad_score,FVC_rate,OI_rate)))
covs1<-as.matrix(indata1[,c(-1,-2)])
covs0<-as.matrix(indata0[,c(-1,-2)])
x<-IDI.INF(indata1[,1:2], covs0, covs1, t0, npert=100)
IDI.INF.OUT(x) 
#m1 Result of IDI. Point and corresponding (1-alpha/2) confidence interval are given
#m2 Result of continuous-NRI. Point and corresponding (1-alpha/2) confidence interval are given.
#m3 Result of median improvement in risk score
#Likelihood ratio test
anova(CRmodel,Rmodel)

# Integrated Brier score and prediction error curve
library(pec)
library(riskRegression)
library(rms)
library(Hmisc)
library(survival)
library(survminer)
library(survcomp)
dd=datadist(train)
options(datadist="dd")
Models <- list("CRmodel"= coxph(Surv(survival.time,result)~radscore+age.group+FVC.rate+LDH.rate.400, data=train,x=TRUE,y=TRUE),
               "Rmodel" = coxph(Surv(survival.time,result)~ radscore, data=train,x=TRUE,y=TRUE),
               "visualmodel" = coxph(Surv(survival.time,result)~HRCTscore, data=train,x=TRUE,y=TRUE))
p <- pec(object = Models,cens.model = "cox", data=train, splitMethod="Boot632plus", B=100,reference = FALSE)
print(p)
par(mai=c(1,1,1,1))
plot(p,type="l",smooth=TRUE,legend = FALSE,xlim=c(0,24),axis1.at=seq(0,24,2), xlab="Survival weeks", ylab="Prediction error",col = c("red", "blue","black"),lwd = c(3,3,3),lty = c(1,1,1))
#KM
library(survival)
library(survminer)
forKM_FVC_rate<-survfit(Surv(survival.time,result)~ FVC.rate, data = subset(data1,group=="de"))
ggsurvplot(forKM_FVC_rate, data = subset(data1,group=="de"), risk.table = TRUE,pval=T,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("FVC rate=0","FVC rate=1","FVC rate=2")) 
forKM_FVC_rate<-survfit(Surv(survival.time,result)~ FVC.rate, data = subset(data1,group=="ex"))
ggsurvplot(forKM_FVC_rate, data = subset(data1,group=="ex"), risk.table = TRUE,pval=T,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("FVC rate=0","FVC rate=1","FVC rate=2")) 
forKM_FVC_rate<-survfit(Surv(survival.time,result)~ FVC.rate, data = subset(data1,group=="in"))
ggsurvplot(forKM_FVC_rate, data = subset(data1,group=="in"), risk.table = TRUE,pval=T,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("FVC rate=0","FVC rate=1","FVC rate=2")) 

forKM_age_rate<-survfit(Surv(survival.time,result)~ age.group, data = subset(data1,group=="de"))
ggsurvplot(forKM_age_rate, data = subset(data1,group=="de"),risk.table = TRUE,pval = TRUE,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("age>55 years old","age<55 years old"))
forKM_age_rate<-survfit(Surv(survival.time,result)~ age.group, data = subset(data1,group=="ex"))
ggsurvplot(forKM_age_rate, data = subset(data1,group=="ex"),risk.table = TRUE,pval = TRUE,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("age>55 years old","age<55 years old"))
forKM_age_rate<-survfit(Surv(survival.time,result)~ age.group, data = subset(data1,group=="in"))
ggsurvplot(forKM_age_rate, data = subset(data1,group=="in"),risk.table = TRUE,pval = TRUE,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("age>55 years old","age<55 years old"))

forKM_radscore_rate<-survfit(Surv(survival.time,result)~ risk.group, data =subset(data1,group=="de"))
ggsurvplot(forKM_radscore_rate, data = subset(data1,group=="de"), risk.table = TRUE,pval=T,pval.method=F,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("Rad score high risk","Rad score low risk")) 
forKM_radscore_rate<-survfit(Surv(survival.time,result)~ risk.group, data =subset(data1,group=="ex"))
ggsurvplot(forKM_radscore_rate, data = subset(data1,group=="ex"), risk.table = TRUE,pval=T,pval.method=F,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("Rad score high risk","Rad score low risk")) 
forKM_radscore_rate<-survfit(Surv(survival.time,result)~ risk.group, data =subset(data1,group=="in"))
ggsurvplot(forKM_radscore_rate, data = subset(data1,group=="in"), risk.table = TRUE,pval=T,pval.method=F,conf.int = F,xlab='Follow Up',legend = "bottom",legend.title = "risk",legend.labs = c("Rad score high risk","Rad score low risk")) 

