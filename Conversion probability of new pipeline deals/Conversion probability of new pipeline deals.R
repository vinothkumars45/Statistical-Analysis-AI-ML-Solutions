library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(caTools)
library(caret)
library(ROCR)
library(broom)
library(data.table)
library(smotefamily)
library(bootStepAIC)
library(car)
library(data.table)
library(MASS)
library(openxlsx)
library(readxl)
library(Metrics)
library(MLmetrics)
library(AICcmodavg)
library(glmnet)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(caTools)
library(caret)
library(ROCR)
library(broom)
library(data.table)
library(smotefamily)
library(bootStepAIC)
library(car)
library(data.table)
library(data.table)

#Setting the Library
setwd("C:/Users/vinotsek/OneDrive - Cisco/Desktop/BCS Deal Prioritization model")

##%######################################################%##
#                                                          #
####          Raw data treatment                        ####
#                                                          #
##%######################################################%##

#Reading the file
Raw<-read_excel("Inter_file.xlsx")
Raw<-Raw[,c(1,3,5,7,9:12,14,15,16,17)]
#Raw<-Raw %>% filter(`Forecast Status`=="Upside")

#Checking for missing values

glimpse(Raw)
table(is.na(Raw))
colSums(is.na(Raw))
Raw$Age[is.na(Raw$Age)]<-median(Raw$Age,na.rm = TRUE)
Raw$`Primary Partner` <-ifelse(Raw$`Primary Partner`=="0","N","Y")

#Treating Categorical & numerical variable

#Categorical

Categorical<-c(2,3,5,6,7,8,9)
Raw[,Categorical] <- lapply(Raw[,Categorical] , factor)
glimpse(Raw)

#Numerical

Raw<-Raw %>% filter(between(Age,quantile(Age,0.00015,na.rm = TRUE),quantile(Age,0.99985,na.rm=TRUE)))
Raw<-Raw %>% filter(between(Amount,quantile(Amount,0.00015,na.rm = TRUE),quantile(Amount,0.99985,na.rm=TRUE)))
Raw<-Raw %>% filter(between(`Stage Duration`,quantile(`Stage Duration`,0.00015,na.rm = TRUE),quantile(`Stage Duration`,0.99985,na.rm=TRUE)))
Raw<-Raw %>% filter(between(`Days passed EBD`,quantile(`Days passed EBD`,0.00015,na.rm = TRUE),quantile(`Days passed EBD`,0.99985,na.rm=TRUE)))


#Feature Visualization
#Categorical
ggplot(Raw, aes( `Primary Partner`,`Y/N`)) + geom_bar(stat = "identity", color = "purple")+
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
ggtitle("BCS Purchase vs Primary Partner") + theme_bw()

#Numerical
ggplot(Raw, aes(x= `Age`, y = `Y/N`)) + geom_jitter(size = 2.5, color="navy") +
xlab("Age") + ylab("BCS Purchase") + ggtitle("BCS Purchase vs Age") + theme_bw()


#Creating dummies for categorical variable

Raw1<-fastDummies::dummy_cols(Raw[,Categorical])
Raw1<-Raw1[,-c(1:7)]
Raw2<-Raw[,-Categorical]
Raw<-cbind(Raw2,Raw1)
rm(Raw1,Raw2)

##%######################################################%##
#                                                          #
####         Feature Significance                        ####
#                                                          #
##%######################################################%##


#Splitting the data set between train and test

set.seed(88)

split <- sample.int(n = nrow(Raw), size = floor(.75*nrow(Raw)), replace = F)

Train<-Raw[split,]
Test<-Raw[-split,]

print(prop.table(table(Train$`Y/N`)))

Train_Balanced <- SMOTE(Train[,-1],Train[,1], K = 5, dup_size = 0)

Train_Balanced<-Train_Balanced[1]

Train_Balanced<-as.data.frame(Train_Balanced)

print(prop.table(table(Train_Balanced$data.class)))

Train_Balanced$data.class<-as.numeric(Train_Balanced$data.class)

#logistic regression model

model <- glm (`data.class`~ ., data = Train_Balanced, family = binomial)

summary(model)

############################################
###########################################
##########################################

set.seed(123)
x=model.matrix(Train_Balanced$`data.class`~.,Train_Balanced)[,-1]
y=Train_Balanced$`data.class`
cv_model <- cv.glmnet(x, y, family="binomial",alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
best_model <- glmnet(x, y, family="binomial",alpha = 1, lambda = best_lambda)
coef(best_model)
tmp_coeffs <- coef(best_model)
coeff_lasso=data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
Coef<-coeff_lasso[1]
Coef<-Coef[-1,]
Coef1<-paste(Coef,collapse = "+")

model2 <- glm(as.formula(paste("data.class ~",Coef1,sep="")),data=Train_Balanced,family = binomial)

summary(model2)

colnames(Test)<-c("data.class","data.Amount","data.Age","data.Stage.Duration","data.Days.passed.EBD","data.CX.Category_BCS.2.x","data.CX.Category_BCS.3.x","data.New.Renew_NEW","data.New.Renew_RENEW","data.Level.01.Territory_Americas","data.Level.01.Territory_APJC","data.Level.01.Territory_EMEAR.REGION","data.Level.02.Territory_AMERICAS_SP","data.Level.02.Territory_ANZ.AREA","data.Level.02.Territory_APJ_SP","data.Level.02.Territory_ASEAN_AREA","data.Level.02.Territory_CANADA","data.Level.02.Territory_EMEAR.CENTRAL","data.Level.02.Territory_EMEAR.NORTH","data.Level.02.Territory_EMEAR.SOUTH","data.Level.02.Territory_EMEAR.UKI","data.Level.02.Territory_EMEAR_GERMANY","data.Level.02.Territory_EMEAR_MEA","data.Level.02.Territory_EMEAR_SP","data.Level.02.Territory_GLOBAL.ENTERPRISE.SEGMENT","data.Level.02.Territory_GREATER_CHINA","data.Level.02.Territory_INDIA_AREA","data.Level.02.Territory_JAPAN__","data.Level.02.Territory_LATIN.AMERICA","data.Level.02.Territory_ROK_AREA","data.Level.02.Territory_US.COMMERCIAL","data.Level.02.Territory_US.PS.Market.Segment","data.Primary.Partner_N","data.Primary.Partner_Y","data.SCMS_COMMERCIAL","data.SCMS_ENTERPRISE","data.SCMS_PUBLIC.SECTOR","data.SCMS_SERVICE.PROVIDER","data.SCMS_SMALL","data.Tier_KEY","data.Tier_MAJOR","data.Tier_MIDSIZE","data.Tier_OTHER","data.Tier_PREMIER","data.Tier_SELECT","data.Tier_SMALL"
)


#knn

library(class)
Train_Balanced = Train_Balanced %>% dplyr::select("data.class", 
                                                  everything())
train.x=Train_Balanced[,-1]
test.x=Test[,-1]
train.classlabels=Train_Balanced[,1]
str(train.classlabels)
set.seed(123)
knn.pred=knn(train.x,test.x,train.classlabels,k=12)
knn.pred
cm_knn=table(knn.pred,Test$data.class)
confusionMatrix(cm_knn,mode="everything",positive="1")

#decision trees

library(tree)
Train_Balanced$data.class=as.factor(Train_Balanced$data.class)
tree.model=tree(Train_Balanced$data.class~.,Train_Balanced)
summary(tree.model)
plot(tree.model)
text(tree.model,pretty=0)
#predict for test
tree.pred=predict(tree.model,Test,type="class")
cm=table(tree.pred,Test$data.class)
cm
mean(tree.pred==Test$data.class)
accuracy_Test <- sum(diag(cm)) / sum(cm)
accuracy_Test
confusionMatrix(cm,mode="everything",positive="1")

#cross validation & pruning
set.seed(123)
cv.model=cv.tree(tree.model,FUN=prune.misclass)
names(cv.model)
cv.model
par(mfrow=c(1,2))
plot(cv.model$size,cv.model$dev,type="b")
plot(cv.model$k,cv.model$dev,type="b")
#11 node tree
prune.model=prune.misclass(tree.model,best=10)
plot(prune.model)
text(prune.model,pretty=0)
tree.pred=predict(prune.model,Test,type="class")
tree.pred
conf_m=table(tree.pred,Test$data.class)
confusionMatrix(conf_m,mode="everything",positive="1")
accuracy_Test <- sum(diag(conf_m)) / sum(conf_m)
print(accuracy_Test)

#bagging and random forest
library(randomForest)
# Create random forest
# For classification
Train_Balanced$data.class=as.factor(Train_Balanced$data.class)
model.rf <- randomForest(Train_Balanced$data.class~ .,
                         data = Train_Balanced,
                         importance = TRUE,
                         proximity = TRUE)
model.rf
yhat.rf=predict(model.rf,newdata = Test,type="class")
yhat.rf.1=predict(model.rf,newdata = Train_Balanced,type="class")
cm_rf=table(yhat.rf,Test$data.class)
#cm_rf.1=table(yhat.rf.1,Train_Balanced$data.class)
cm_rf
confusionMatrix(cm_rf,mode="everything",positive="1")
confusionMatrix(cm_rf.1,mode="everything",positive="1")

##%######################################################%##
#                                                          #
####         Prediction                                 ####
#                                                          #
##%######################################################%##


#Reading the file to predict

Predict<-read_excel("To_Predict.xlsx")

Predict1<-Predict[,c(3,1,5)]

Predict2<-Predict[,c(2,4,6,8,9,10,11,13,14,15,16)]

Predict2$Age[is.na(Predict$Age)]<-median(Predict2$Age,na.rm = TRUE)

Predict2$`Primary Partner` <-ifelse(Predict2$`Primary Partner`== 0,"N","Y")

Categorical<-c(1,2,4,5,6,7,8)

Predict2[,Categorical] <- lapply(Predict2[,Categorical] , factor)

Predict21<-fastDummies::dummy_cols(Predict2[,Categorical])

Predict21<-Predict21[,-c(1:7)]

Predict22<-Predict2[,-Categorical]

Predict2<-cbind(Predict22,Predict21)

rm(Predict21,Predict22)

Predict2$Tier_Other<-0

colnames(Predict2)<-c("data.Amount","data.Age","data.Stage.Duration","data.Days.passed.EBD","data.CX.Category_BCS.2.x","data.CX.Category_BCS.3.x","data.New.Renew_NEW","data.New.Renew_RENEW","data.Level.01.Territory_Americas","data.Level.01.Territory_APJC","data.Level.01.Territory_EMEAR.REGION","data.Level.02.Territory_AMERICAS_SP","data.Level.02.Territory_ANZ.AREA","data.Level.02.Territory_APJ_SP","data.Level.02.Territory_ASEAN_AREA","data.Level.02.Territory_CANADA","data.Level.02.Territory_EMEAR.CENTRAL","data.Level.02.Territory_EMEAR.NORTH","data.Level.02.Territory_EMEAR.SOUTH","data.Level.02.Territory_EMEAR.UKI","data.Level.02.Territory_EMEAR_GERMANY","data.Level.02.Territory_EMEAR_MEA","data.Level.02.Territory_EMEAR_SP","data.Level.02.Territory_GLOBAL.ENTERPRISE.SEGMENT","data.Level.02.Territory_GREATER_CHINA","data.Level.02.Territory_INDIA_AREA","data.Level.02.Territory_JAPAN__","data.Level.02.Territory_LATIN.AMERICA","data.Level.02.Territory_ROK_AREA","data.Level.02.Territory_US.COMMERCIAL","data.Level.02.Territory_US.PS.Market.Segment","data.Primary.Partner_N","data.Primary.Partner_Y","data.SCMS_COMMERCIAL","data.SCMS_ENTERPRISE","data.SCMS_PUBLIC.SECTOR","data.SCMS_SERVICE.PROVIDER","data.SCMS_SMALL","data.Tier_KEY","data.Tier_MAJOR","data.Tier_MIDSIZE","data.Tier_PREMIER","data.Tier_SELECT","data.Tier_SMALL","data.Tier_OTHER")

Predict2$`data.New.Renew_RENEW`<-0

Output=predict(model.rf,newdata = Predict2,type="prob")

Output<-cbind(Predict,Output)

Output<-Output[,-17]

colnames(Output)[17]<-".fitted"

Output<-as.data.table(Output)

Output[between(.fitted,0,0.5),Probability_filter := "Less than 50% Probability"][
  between(.fitted,0.5,0.75),Probability_filter := "Between 50% to 75% Probability"][
    between(.fitted,0.75,1e10), Probability_filter := "Greater than 75% Probability"]

Output[between(Amount,0,100000),Deal_Size_filter := "Less than 100k"][
  between(Amount,100000,1000000),Deal_Size_filter := "Between 100k to 1M"][
    between(Amount,1000000,1e10), Deal_Size_filter := "Greater than 1M"]

Output$`Primary Partner` <-ifelse(Output$`Primary Partner`=="0","N","Y")

write.xlsx(Output,"Final2.xlsx")

