setwd("D:/renji/ILD")
library(survival)

library(glmnet)
library(dplyr)
library(openxlsx)
data1<-read.xlsx("all_1198.xlsx",sheet=2)

for (i in c(1,2,5,10,11,13,16)){
  data1[,i] <- as.factor(data1[,i])
}
for (i in c(3,4,6:9,15,17:19)){
  data1[,i] <- as.numeric(as.character(data1[,i]))
}
#check status of data
str(data1[,1:22])
data2<-subset(data1,train==0|train==1)
a<-data2[20:844] %>% summarise_all("mean")
b<-data2[20:844]%>% summarise_all("sd")
external1<-subset(data1,train==2)
internal1<-subset(data1,train==3)
internal<-internal1[,20:844]
external<-external1[,20:844]
data3<-data.frame(data2[,1:19],scale(data2[,20:844],center=T,scale=T))
for (i in 1:dim(internal1)[1]){
  internal[i,] <- (internal[i,] - a) / b
}
for (i in 1:dim(external1)[1]){
  external[i,] <- (external[i,] - a) / b
}
internal<-data.frame(internal1[,1:19],internal)
external<-data.frame(external1[,1:19],external)
sink("lasso1.txt")

traincindex <- NULL
testcindex <- NULL
internalcindex <- NULL
externalcindex <- NULL
group <- NULL
counter <- 0
for (groupindex in c(1:1000)){
  counter <- counter + 1
  group[counter] <- groupindex
  print(groupindex)
  set.seed(groupindex)
  
  #set.seed(1198)
  indexes<-sample(152,0.8*152,replace=FALSE)
  train<-data3[indexes,]
  test<-data3[-indexes,]
  
  y_train<- Surv(train$Survival,train$CustomLabel) #bundle y
  x<- as.matrix(train[c(20:844)]) #define predictors
  cvfit <- cv.glmnet(y = y_train, x = x, family="cox", alpha=1, nfolds = 10)#look for the best lambda 
  #features5 <- which(cvfit[["glmnet.fit"]]$df > 9)[1]
  coefficients<-coef(cvfit,s=cvfit$lambda.min)
  Active.Index<-which(coefficients!=0)  #Index of Non-zero Coefficients
  Active.coefficients<-coefficients[Active.Index]  #Actual Coefficients
  Active.features<-colnames(train[,20:844])[Active.Index]
  dataframenew <- data.frame(y_train,
                             train[,Active.features])
  
  print(cvfit) #every lambda and corresponding feature numbers
  cvfit[["glmnet.fit"]]
  #cox again
  Rmodel <- coxph(y_train~., data = dataframenew)
  method<- survConcordance(Surv(train$Survival,train$CustomLabel) ~ predict(Rmodel, train))
  cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
  traincindex[counter] <- method$concordance
  
  method<- survConcordance(Surv(test$Survival,test$CustomLabel) ~ predict(Rmodel, test))
  cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
  testcindex[counter] <- method$concordance
  
  method<- survConcordance(Surv(external$Survival,external$CustomLabel) ~ predict(Rmodel, external))
  cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
  externalcindex[counter] <- method$concordance
  
  method<- survConcordance(Surv(internal$Survival,internal$CustomLabel) ~ predict(Rmodel, internal))
  cat(method$concordance, "95%CI:", method$concordance-method$std.err*1.96, method$concordance+method$std.err*1.96)
  internalcindex[counter] <- method$concordance
}

bestcombo <- data.frame(group, traincindex, testcindex, internalcindex, externalcindex)
bestcombo %>% filter(externalcindex > 0.71) %>% filter(abs(traincindex-testcindex)<0.02)



#cvfit 
plot(cvfit)
res = cvfit$glmnet.fit$beta[, 34]#choose number of features, in results.txt[]
which(res != 0)
sink()
#lasso path
plot(cvfit$glmnet.fit,xvar ="lambda", label = T) 
abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty=2)
#cox 
#Rmodel1<-coxph(y_train~original_firstorder_RobustMeanAbsoluteDeviation +original_glszm_ZoneEntropy +original_shape_Flatness + original_shape_SphericalDisproportion + wavelet.HLL_glszm_LargeAreaHighGrayLevelEmphasis +wavelet.LLH_glcm_Idmn +wavelet.LLL_firstorder_90Percentile +wavelet.LLL_firstorder_Skewness +wavelet.LLL_glcm_MCC +wavelet.LLL_glszm_ZoneEntropy,train )
#AIC backward
#Rmodel<- step(Rmodel1, direction = "backward")

#split
train<-subset(data1,train==1)
test<-subset(data1,train==0)
external<-subset(data1,train==2)
internal<-subset(data1,train==3)
y_train<- Surv(train$Survival,train$CustomLabel) 
y_test<- Surv(test$Survival,test$CustomLabel) 
y_internal<- Surv(internal$Survival,internal$CustomLabel) 
y_external<- Surv(external$Survival,external$CustomLabel) 
#cox
Rmodel<-coxph(y_train~wavelet.LLL_glcm_MCC+wavelet.LLH_glcm_Idmn+wavelet.HLL_glszm_LargeAreaHighGrayLevelEmphasis+original_shape_Flatness+wavelet.LLL_firstorder_Skewness,train)

#rad_score calculation
rad_sig<-train[c('wavelet.LLL_glcm_MCC','wavelet.LLH_glcm_Idmn','wavelet.HLL_glszm_LargeAreaHighGrayLevelEmphasis','original_shape_Flatness','wavelet.LLL_firstorder_Skewness')] 
rad_sig_matrix <- as.matrix(rad_sig)
coef_matrix <- Rmodel$coefficients
rad_score<- rad_sig_matrix %*% coef_matrix
plus_rad_score<-cbind(train,rad_score)
colnames(plus_rad_score)[845] <- "rad_score"
write.csv(plus_rad_score,"plus.rad_score.csv",sep=",")
