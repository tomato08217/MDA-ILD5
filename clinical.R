library(openxlsx)
data1<-read.xlsx("clinicaltable.xlsx",sheet=2)


for (i in c(2:4,8,14:28,30,36,37,41,44,48,53,57:61)){
  data1[,i] <- as.factor(data1[,i])
}
for (i in c(5:7,9:13,29,31:35,38:40,42,43,45:47,49:52,54,56)){
  data1[,i] <- as.numeric(as.character(data1[,i]))
}

data1$result<-as.numeric(as.character(data1$result))
train<-subset(data1,train==1)
a<-md.pattern(train)

library(survival)
y_train<- Surv(train$survival.time,train$result) 
train_unicox<-train[,8:61]
UniCox<-function(x){
FML<-as.formula(paste0('y_train~',x))
Gcox<-coxph(FML,data=data_unicox)
Gsum<-summary(Gcox)
HR<-round(Gsum$coefficients[,2],3)
Pvalue<-round(Gsum$coefficients[,5],3)
CI<-paste0(round(Gsum$conf.int[,3:4],3),collapse='-')
Unicox<-data.frame('Characteristic'=x,'Hazard Ratio'=HR,'CI95'=CI,'P value'=Pvalue)
return(Unicox)
}
UniCox('sex.female.0')
VarNames<-colnames(data_unicox)
Univar<-lapply(VarNames,UniCox)
library(plyr)
Univar<-ldply(Univar,data.frame)
Univar$Characteristic[Univar$P.value<0.05]
write.csv(Univar,"unicox1.csv")

p<-0
for (i in 1:41) p[i]<-wilcox.test(data[,i]~CustomLabel,data)$p.value
data_p<-rbind(data,p)
data_sig<-data_p[which (p<0.05)]
write.csv(data_sig,"top.csv",sep=",")

library(glmulti)
glmulti.coxph.out <- glmulti(y_train ~ radscore+age
+course.month+OI.200+FVC.rate+arthralgia+CEA
+Lac+SF.rate1000+Lymphocyte+LDH.rate.400+Tbil+BUN, data = train,  level = 1, method = "h", crit = "aic", confsetsize = 5, plotty = F, report = F,  fitfunction = "coxph")  
glmulti.coxph.out@formulas 
summary(glmulti.coxph.out@objects[[1]])

data2<-read.xlsx("clinicaltable.xlsx",sheet=2)
data3<-data2[,4:16]
data4<-na.omit(data3)
y_data4<- Surv(data4$survival.time,data4$result) 
CRmodel1<-coxph(y_data4 ~ radscore+age.55+course.month+OI.200+FVC.rate+arthralgia+CEA+CRP+SF.rate1500+Lymphocyte+LDH.rate.400,data4)

CRmodel<- step(CRmodel1, direction = "backward")

#bic
CRmodel = step(CRmodel1, direction = "backward", trace = TRUE,  k = log(nrow(train)))

x<- data.matrix(train[c(6:61)])
cvfit <- cv.glmnet(y = y_data, x = x, family="cox", alpha=1, nfolds = 10)#
sink("clinical.txt")
print(cvfit) #every lambda and corresponding feature numbers
res = cvfit$glmnet.fit$beta[, 7]#
which(res != 0)
plot(cvfit)
sink()
