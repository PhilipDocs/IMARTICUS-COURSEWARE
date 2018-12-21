#########Remove all
rm(list=ls(all=TRUE))
##########Load random  numbers
y<-21:40
x<-51:70
#########Create data frame
da<-data.frame(y,x)
######View data
da
########Plot data
plot(y~x,da)
###########Insert a mean line
abline(h=mean(da$y),col="blue")
#########Mean of Y as first model
abline(v=mean(da$x,col="green"))
##########Mean of x
abline(reg=lm(y~x,da),col="red")
#########Regression line 
###############Insert Column names
colnames(da)<-c("salary","Age")
da
##################Convert Salary to a factor variable
da$salary_class<-as.factor(ifelse(da$salary>=35,"High","Low"))
da
###########View the characteristic
str(da)
da$salary_class<-ifelse(da$salary>=35,1,0)
plot(salary_class~x,da)
#########Now we have converted our data to 0 and 1's
###############Null model is a straiht horizontal line through 0.4
##############Pass another linear regression in top of this
abline(h=mean(da$salary_class),col="blue")
################Mean of salary as first line
abline(v=mean(da$Age,col="green"))
############Mean of age
abline(reg=lm(da$salary_class~da$Age),col="red")
##############We have created a mess over here for sure
#################There are only 2 forms of data we have only 0 and 1
##########let me use regression line a
#################For value of 60 we get salary _class as 0.3 which is not possible
######similiarly for age of 50 we will get a negative value of salary class which is not possible
###########Similiarly if i extend to age of 75 i will get positive value greate rthan 1 whoch is not possible

#########The errors are going to have a pattern 
###########increasing from x 55 to 65 which violates normal distribution assumption
############Logistic regression will, be used for the rescue
sal<-seq(from=21000, to=40000,by=1000)
exp<-seq(from =2,to=21,by=1)
sal
exp
da<-data.frame(sal,exp)
###########What would be salary of a person given his experience
############
plot(sal~exp,da)
summary(da$sal)
hist(da$sal)
da$sal_class<-ifelse(da$sal>=35250,1,0)
da
plot(sal_class~exp,da)
#############Linear reression wont work out here 
#############Can we check with experience and salary by probablity 
#############Probablity cannot be below 0 and above 1
###############incidentally most of the events we are tracking in analytics are rare events 
#########what is the proobablity that a specific employee has a high salary
###########Can a relate to log(odds
###########Change the prob value in such a way that the relationship becomes linear
###log(p/1-p)=b0+b1x1+b2x2+err
###p/1-p=exp(b0+b1x1+b2x2)
###find p
########################first build linear model
model<-lm(sal_class~exp,da)
da$predict_lm<-predict.lm(model,newdata=da)
da
da$exp_predict<-exp(da$predict_lm)
da$exp_predict<-exp(da$predict_lm)/(1+exp(da$predict_lm))
da$y_prob<-da$exp_predict
da
###########Load bostin data 
require("MASS")
data(Boston)
help(Boston)
summary(Boston)
hist(Boston$medv)
summary(Boston$medv)
###################75% of data are less than 3rd quartile
###########
Boston$medv_class<-ifelse(Boston$medv >=25,1,0)
summary(Boston)
hist(Boston$medv_class)
plot(Boston$medv_class,Boston$lstat)
plot(Boston$medv,Boston$lstat)
plot(Boston$medv_class~Boston$lstat)
###########Create a sample model
model=lm(medv_class~lstat,data=Boston)
summary(model)
##################
Boston$predict<-predict.lm(model,Boston)
Boston$prob_class_1<-exp(Boston$predict)/(1+exp(Boston$predict))
View(Boston)
names(Boston)
plot(Boston$prob_class_1,Boston$lstat)
#################Expected probablity of a high value property
####value of lstat vs probablity of housing greater than 25k
###########Now build a logistic regression model
colnames(Boston)
model<-glm(formula=medv_class~.-medv,data=Boston,family=binomial("logit"))
summary(Boston$medv_class)
class(Boston$medv_class)
#############this is because it is not stored as factor
Boston$medv_class<-as.factor(Boston$medv_class)
summary(Boston$medv_class)
model<-glm(formula=medv_class~.-medv,data=Boston,family=binomial("logit"))
summary(model)
###################################Stepwise regression
model_optim<-step(model)
summary(model_optim)
##############there is a z value other then t value in lr
dim(Boston)
###null deviance - empty model no variable
#########Subset data
colnames(Boston)
Boston1<-subset(Boston,select=-c(medv,predict,prob_class_1))
colnames(Boston1)
model<-glm(formula=medv_class~.,data=Boston1,family=binomial("logit"))
summary(model)
####################Residual deviance lower so adding variables is good
###############AIC is  like sum of sqaured errors in linear reg
#########################AIC is lower is good
############Dont use complicated model .For every new variable AIC penalizes 
###############−
###-2log-likelihood+  knpar AIC
#############
optim_model<-step(model)
##########AIC decreases for each iteration
summary(optim_model)
require("magrittr")
######Choose a model by AIC
step.model <- optim_model %>% stepAIC(trace = FALSE)

summary(step.model)
####################################3

step.model <- optim_model %>% stepAIC(trace = FALSE,direction="backward")
summary(step.model)
step.model1 <- optim_model %>% stepAIC(trace = FALSE,direction="forward")
summary(step.model1)
#####################predict for new data 
Boston$predict<-predict(optim_model,newdata=Boston,type="response")
View(Boston)
max(Boston$predict)
min(Boston$predict)
#########Cannnot ve less than 0 and more than 1
#######Predictioj that medv will take a class 1 or 0 
#######For most of the classes we will get proba between 0 and 1 but need not always be clsoe to 1sometime
###can be max of 0.8 based on kind of samples
table(Boston$medv_class,Boston$predict)
colnames(Boston)
class(Boston)
########This is not good
Boston$predict<-predict.glm(optim_model,newdata=Boston,type="terms")
class(Boston$predict)
colnames(Boston$predict)
summary(Boston$predict)
###############33
Boston$medv_class<-as.numeric(Boston$medv_class)
Boston$medv_class1<-ifelse(Boston$medv_class==1,"High","Low")
###########No model is perfectly wrong or perfectly corrcte
###there are certain statistical parameters to validate the model 
####just based on significance
###A student scoring 40% in tenth can highlught that probablity to pass in 12 th is very low 
###but in relaity he can become a great business man  
#####so just stats giuves based on history and significance
Boston$predict<-NULL
########Build model 
colnames(Boston)
model<-glm(medv_class~.,data=Boston[,-ncol(Boston)])
summary(model)
model_optim<-step(model)
summary(model_optim)
###
summary(Boston$medv_class)
Boston$medv_class<-as.factor(Boston$medv_class)
model_fin<-glm(formula=medv_class~crim+zn+indus+rm+dis+lstat+medv+prob_class_1,family=binomial("logit"),data=Boston)
class(Boston$medv_class)
Boston$medv_class<-as.factor(Boston$medv_class)
model_fin<-glm(formula=medv_class~crim+zn+indus+rm+dis+lstat,family=binomial("logit"),data=Boston)
summary(model_fin)
#########No test data so use in the same data itseld
Boston$predict<-predict.glm(model_fin,newdata=Boston)
colnames(Boston)
Boston$predict<-predict.glm(model_fin,newdata=Boston,type="terms")
colnames(Boston$predict)
Boston$predict<-predict.glm(model_fin,newdata=Boston,type="response")
######Probablity of event beigh High
View(Boston)
nnn<-predict.glm(model_fin,newdata=Boston)
class(nnn)
#######default prob function 
#########Link function for odds ratio
Boston$predict<-predict.glm(model_fin,newdata=Boston,type="link")

Boston$predict1<-predict.glm(model_fin,newdata=Boston,type="response")
#########Response makes sense for logistic regression 
###########terms and response does not 
#######first relate the predictions
#####Assume 0.5 is best cutoff
Boston$flag<-as.factor(ifelse(Boston$predict1<=0.5,"High","Low"))
table(Actual=Boston$medv_class1,Predicted=Boston$flag)
#######Model is 100% accurate 
#####Calculate sensitivity,specificty
#install.packages("caret")
library(caret)
conf_matrix<-table(Actual=Boston$medv_class1,Predicted=Boston$flag)
sensitivity(conf_matrix)
specificity(conf_matrix)
#########Changing threshold
Boston$flag<-as.factor(ifelse(Boston$predict1<=0.00001,"High","Low"))
conf_matrix<-table(Actual=Boston$medv_class1,Predicted=Boston$flag)
conf_matrix
sensitivity(conf_matrix)
specificity(conf_matrix)
########Model performance depends on the data 
############Now we will do the model using real time data set
##########https://archive.ics.uci.edu/ml/machine-learning-databases/00222/
#####################

###############Read the data 
setwd("C:\\Users\\128564\\Desktop\\121\\")
bank=read.csv("./Day2/bank.csv")
str(bank)
#number of numeric variables
x<-sapply(bank,is.numeric)
x
bank_numeric<-bank[,x]
head(bank_numeric)
#7 numeric variables

#number of factor variables
y<-sapply(bank, is.factor)
bank_factor<-bank[,y]
head(bank_factor)
#missing values treatment
#install.packages("Amelia")
library(Amelia)
missmap(bank, y.at = 1,y.labels = "",col=c("red","black"),legend = FALSE)
sum(is.na(bank))
#no missing values

#feature engineering

#age outliers
boxplot(bank$age)
upper_side_outliers_age <- quantile(bank$age, 0.75) + 1.5*IQR(bank$age)
max(bank$age)
bank[bank$age > round(upper_side_outliers_age), "age"] <- round(upper_side_outliers_age)
max(bank$age)
min(bank$age)
bank$age<-ifelse(bank$age<=18 & bank$age<40,"adult",ifelse(bank$age>=40 & bank$age<58,
                                                           "middle age","older"))
table(bank$age)
bank$age<-as.factor(bank$age)
#duration outliers
colnames(bank)
bank$duration<-(bank$duration/60)
boxplot(bank$duration)
##################33
#duration outlier capping
upper_side_outliers_duration <- quantile(bank$duration, 0.75) + 1.5*IQR(bank$duration)
bank[bank$duration > round(upper_side_outliers_duration), "duration"] <- round(upper_side_outliers_duration)
min(bank$duration)
max(bank$duration)
boxplot(bank$duration)
bank$duration<-ifelse(bank$duration<5 ,"1",ifelse(bank$duration>=5 & bank$duration<10,"2",
                                                  ifelse(bank$duration>=10 & bank$duration<15,"3","4")))
bank$duration<-as.factor(bank$duration)
#EDA
library(ggplot2)
table(bank$age)
ggplot(bank,aes(age,fill=deposit))+geom_bar()+
  ggtitle("Age vs Deposit")->p1
p1
table(bank$age)
#older people have deposit rate

ggplot(bank,aes(job,fill=deposit))+geom_bar()+
  ggtitle("Job vs Deposit")->p2
p2
#people who are at the management deposit more followed by technicians and bluecollars
ggplot(bank,aes(marital,fill=deposit))+geom_bar()+
  ggtitle("Marital vs Depsoit")->p3
p3
#married people deposit more than others

ggplot(bank,aes(education,fill=deposit))+geom_bar()+
  ggtitle("Education vs Deposit")->p4
p4
#people who have secondary level education deposit more followed by 
#people who have tertiary level education

ggplot(bank,aes(default,fill=deposit))+geom_bar()+
  ggtitle("Default vs Deposit")->p5
p5
#people who have no credit default deposit more and people who have credit default
#almost have no deposit

ggplot(bank,aes(balance))+geom_histogram(aes(fill=deposit),color="black")+
  ggtitle("Balance vs Deposit")->p6
p6
#people who have below 500 deposit more

ggplot(bank,aes(housing,fill=deposit))+geom_bar()+
  ggtitle("Housing vs Deposit")->p7
p7
#people who dont have house loan deposit more than people who have home loans.

ggplot(bank,aes(loan,fill=deposit))+geom_bar()+
  ggtitle("Loan vs Deposit")->p8
p8
#people who dont have personal loan deposit more than people who have personal loans

ggplot(bank,aes(contact,fill=deposit))+geom_bar()+
  ggtitle("Contact vs Deposit")->p9
p9
#people who are contacted by cellular deposit more

ggplot(bank,aes(month,fill=deposit))+geom_bar()+
  ggtitle("Month vs Deposit")->p10
p10
#people who are contacted in the month of may deposit more followed by august,july and april

ggplot(bank,aes(campaign))+geom_histogram(aes(fill=deposit),color="black",binwidth =5)+
  ggtitle("Campaign vs Deposit")->p11
p11
#people who are contacted for less than 5 times deposit more

ggplot(bank,aes(duration,fill=deposit))+geom_bar()+
  ggtitle("Duration vs Deposit")->p12
p12
#people whose duration of contact is less than 5 minutes deposit more 
#but people whose duration of contact exceeded 5 minutes have higher deposit rate
library(gridExtra)
grid.arrange(p1,p2,p3)->g1
g1
grid.arrange(p4,p5,p6)->g2
g2
grid.arrange(p7,p8,p9)->g3
g3
grid.arrange(p10,p11,p12)->g4
g4

grid.arrange(p10,p11,p12,p4,p5)->g4
g4
#correaltion matrix
library(corrplot)
library(psych)
bank_cor <- bank

for(i in 1:ncol(bank_cor)){
  
  bank_cor[,i]<- as.integer(bank_cor[,i])
}
corrplot(cor(bank_cor))
# pdays, previous and poutcome are highly correlated variables
#outliers
boxplot(bank$balance)
upper_side_outliers_balance <- quantile(bank$balance, 0.75) + 1.5*IQR(bank$balance)
IQR(bank$balance)
quantile(bank$balance, 0.75) 
boxplot(bank$balance)
#base accuracy
prop.table(table(bank$deposit))
#47 percent accuracy
bank$deposit<-ifelse(bank$deposit=="yes",1,0)
#splitting the data
#install.packages("caTools")
library(caTools)
set.seed(1234)
split <- sample.split(bank$deposit, SplitRatio = 0.7)
train <- subset(bank, split == TRUE)
test <- subset(bank, split == FALSE)
#model bulding without removing correlated variables
log.model1 <- glm(deposit ~ ., data=train, family = binomial(link='logit'))
summary(log.model1)
# aic value 6795.9
## coefficients
exp(coef(log.model1))
summary(log.model1)
#Validating model 1

#prediction
pred1 <- predict(log.model1, newdata=test, type = "response")
table(test$deposit, pred1>= 0.5)


# Area under the curve
#install.packages("ROCR")
library(ROCR)
predic1<-prediction(pred1, test$deposit)
# creating ROC curve
roc1<-performance(predic1,"tpr","fpr")
plot(roc1)
title("ROC Curve")
auc1<- performance(predic1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1
#cross validation
ctrl1 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
# bank$deposit<-as.factor(bank$deposit)
# class(bank$deposit)
#install.packages("e1071")
require("e1071")
bank$deposit<-as.factor(bank$deposit)
mod_fit1 <- train(deposit ~.,  data=bank, method="glm", family="binomial",
                  trControl = ctrl1)
summary(mod_fit1)
pred_cross1 = predict(mod_fit1, newdata=test)
unique(pred_cross1)
pred_cross2<-as.numeric(pred_cross1)
summary(pred_cross2)
pred_cross3<-pred_cross2-1
summary(pred_cross3)
pred_cross1<-pred_cross3
max(pred_cross1)

unique(pred_cross1)
pred_cross1<-as.numeric(pred_cross1)
summary(pred_cross1)
pred_cross1<-ifelse(pred_cross1>=0.5, 1,0)
test$deposit<-as.factor(test$deposit)

unique(test$deposit)
unique(pred_cross1)
pred_cross1<-as.factor(pred_cross1)
confusionMatrix(pred_cross1, test$deposit)

#variable importance
varImp(log.model1,scale=FALSE)
bank_cor <- train

for(i in 1:ncol(bank_cor)){
  
  bank_cor[,i]<- as.integer(bank_cor[,i])
}
cor(bank_cor)
#building model without correlated variables
colnames(train)
log.model2<-glm(deposit~age+job+marital+education+default+balance+housing+ 
                  loan+contact+day+ month + duration + campaign,   
                data=train, family = binomial(link = 'logit'))

summary(log.model2)
# aic value 7271.2

## coefficients
exp(coef(log.model2))
#Validating model 2

#prediction
pred2 <- predict(log.model2, newdata=test, type = "response")
table(test$deposit, pred2 >= 0.5)  
#confusion matrix
pred_threshold2<-ifelse(pred2>=0.5,1,0)
pred_threshold2<-as.factor(pred_threshold2)
confusionMatrix(pred_threshold2,test$deposit)
# Area under the curve
predic2<-prediction(pred2, test$deposit)
# creating ROC curve
roc2<-performance(predic2,"tpr","fpr")
plot(roc2)
title("ROC Curve")
auc2 <- performance(predic2, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2
#cross validation
ctrl2 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit2 <- train(deposit~age+job+education+default+balance+housing+ 
                    loan+contact+day+ month + duration + campaign,  data=bank, method="glm", family="binomial",
                  trControl = ctrl2, tuneLength = 5)
summary(mod_fit2)

pred_cross2 = predict(mod_fit2, newdata=test)
unique(pred_cross2)
pred_cross3<-as.numeric(pred_cross2)
summary(pred_cross3)
pred_cross2<-pred_cross3-1
summary(pred_cross2)
pred_cross2<-ifelse(pred_cross2>=0.5, 1,0)
pred_cross2<-as.factor(pred_cross2)
confusionMatrix(pred_cross2, test$deposit)
#accuracy 80

#variable importance
varImp(log.model2,scale=FALSE)
nrow(bank)

#model1 has more acccuracy  than model2
#model 1 has has higher auc vlaue than model 2
#model 1 has more accuracy than model 2 in cross validation
#model 1 has lower aic value than model 2 in cross validation
#################Go to bank marketiing data set and understahd in detail
bank.additional.full<-bank.additional
View(bank.additional.full)
str(bank.additional.full)
summary(bank.additional.full)


#########Explain about this in full in summary outputs
bank.additional.full$default_transformed<-ifelse(bank.additional.full$default=="yes","unknown",bank.additional.full$default)
unique(bank.additional.full$default_transformed)
unique(bank.additional.full$default)

bank.additional.full$default_transformed<-as.factor(bank.additional.full$default_transformed)
colnames(bank.additional.full)
summary(bank.additional.full)[,c(5,22)]
summary(bank.additional.full[,c("default","default_transformed")])
unique( bank.additional.full$default)
bank.additional.full$default_transformed<-as.factor(ifelse(bank.additional.full$default=="yes","unknown",as.character(bank.additional.full$default)))
summary(bank.additional.full[,c("default","default_transformed")])
#bank.additional.full$default<-bank.additional.full$default_transformed
summary(bank.additional.full[,c("default","default_transformed")])
###########analyze about p days 
summary(bank.additional.full$pdays)
median(bank.additional.full$pdays)
#####lets convert to factor
summary(as.factor(bank.additional.full$pdays))
##339673/41188
table(as.factor(bank.additional.full$pdays),bank.additional.full$y)
hist(bank.additional.full$pdays)

#######Convert to factor
bank.additional.full$pdays<-as.factor(bank.additional.full$pdays)
summary(bank.additional.full$previous)
summary(as.factor(bank.additional.full$previous))
table(bank.additional.full$previous,bank.additional.full$y)
bank.additional.full$previous<-as.factor(bank.additional.full$previous)
summary(bank.additional.full)
colnames(bank.additional.full)
summary(bank.additional.full$emp.var.rate)
bank.additional.full$emp.var.rate<-bank.additional.full$emp.var.rate-min(bank.additional.full$emp.var.rate)/(max(bank.additional.full$emp.var.rate)-min(bank.additional.full$emp.var.rate))
summary(bank.additional.full$emp.var.rate)
str(bank.additional.full$cons.conf.idx)
summary(bank.additional.full$cons.conf.idx)

bank.additional.full$cons.conf.idx<-bank.additional.full$cons.conf.idx-min(bank.additional.full$cons.conf.idx)/(max(bank.additional.full$cons.conf.idx)-min(bank.additional.full$cons.conf.idx))
summary(bank.additional.full$cons.conf.idx)

bank.additional.full$euribor3m<-bank.additional.full$euribor3m-min(bank.additional.full$euribor3m)/(max(bank.additional.full$euribor3m)-min(bank.additional.full$euribor3m))
summary(bank.additional$cons.price.idx)
bank.additional.full$cons.price.idx<-bank.additional.full$cons.price.idx-min(bank.additional.full$cons.price.idx)/(max(bank.additional.full$cons.price.idx)-min(bank.additional.full$cons.price.idx))
summary(bank.additional$cons.price.idx)
summary(bank.additional$nr.employed)

bank.additional.full$nr.employed1<-bank.additional.full$nr.employed-min(bank.additional.full$nr.employed)/(max(bank.additional.full$nr.employed)-min(bank.additional.full$nr.employed))
summary(bank.additional.full$nr.employed1)
bank.additional.full[1:2,]
max(bank.additional.full$nr.employed)
min(bank.additional.full$nr.employed)
#5191-4963/(5228-4963)
transformed<-model.matrix(y~.,data=bank.additional.full)
colnames(transformed)
transformed_data<-as.data.frame(transformed)
names(transformed_data)
summary(transformed_data)
transformed_data[1:2,]
max(transformed_data$`(Intercept)`)
transformed_data$`(Intercept)`<-NULL
transformed_data[1:2,]
transformed_data$y<-bank.additional.full$y
##########Take a sample
record_no<-sample(nrow(transformed_data),0.7*nrow(transformed_data))
trainingdata<-transformed_data[record_no,]
testingdata<-transformed_data[-record_no,]
colnames(trainingdata)
testingdata[1:2,]
model1<-glm(y~.,family=binomial("logit"),data=trainingdata)
summary(model1)
dim(trainingdata)
#model_optim<-step(model1,direction="both")
#summary(model_optim)
glm(formula = y ~ `jobblue-collar` + jobentrepreneur + jobmanagement + 
      `jobself-employed` + loanyes + contacttelephone + monthaug + 
      monthmar + monthmay + monthnov + duration  + pdays2 + 
      pdays4 + pdays16 + previous1 + poutcomesuccess + emp.var.rate + 
      cons.price.idx + cons.conf.idx + nr.employed , family = binomial("logit"), 
    data = trainingdata)

summary(glm(formula = y ~ `jobblue-collar` + jobentrepreneur + jobmanagement + 
              `jobself-employed` + loanyes + contacttelephone + monthaug + 
              monthmar + monthmay + monthnov + duration  + pdays2 + 
              pdays4 + pdays16 + previous1 + poutcomesuccess + emp.var.rate + 
              cons.price.idx + cons.conf.idx + nr.employed , family = binomial("logit"), 
            data = trainingdata))

summary(glm(formula = y ~ `jobblue-collar` + jobentrepreneur + jobmanagement + 
              `jobself-employed` + loanyes + contacttelephone + monthaug + 
              monthmar + monthmay + monthnov + duration   + 
              pdays4  + previous1 + poutcomesuccess + emp.var.rate + 
              cons.price.idx + cons.conf.idx + nr.employed , family = binomial("logit"), 
            data = trainingdata))
summary(glm(formula = y ~  jobentrepreneur + jobmanagement + 
              `jobself-employed` + loanyes + contacttelephone + monthaug + 
              monthmar + monthmay + monthnov + duration   + 
              pdays4  + previous1 + poutcomesuccess + emp.var.rate + 
              cons.price.idx + cons.conf.idx + nr.employed , family = binomial("logit"), 
            data = trainingdata))
summary(glm(formula = y ~  jobentrepreneur + jobmanagement + 
              `jobself-employed` + loanyes + contacttelephone + monthaug + 
              monthmar + monthmay + monthnov + duration   + 
              pdays4  + previous1 + poutcomesuccess + emp.var.rate + 
              cons.price.idx + cons.conf.idx  , family = binomial("logit"), 
            data = trainingdata))
summary(glm(formula = y ~  jobentrepreneur  + 
              `jobself-employed` + loanyes + contacttelephone + monthaug + 
              monthmar + monthmay + monthnov + duration   + 
              pdays4  + previous1 + poutcomesuccess + emp.var.rate + 
              cons.price.idx + cons.conf.idx  , family = binomial("logit"), 
            data = trainingdata))
summary(glm(formula = y ~  jobentrepreneur  + 
              `jobself-employed`  + contacttelephone + monthaug + 
              monthmar + monthmay + monthnov + duration   + 
              pdays4  + previous1 + poutcomesuccess + emp.var.rate + 
              cons.price.idx + cons.conf.idx  , family = binomial("logit"), 
            data = trainingdata))
#########Save model
model_optim_fin<-glm(formula = y ~  jobentrepreneur  + 
                       `jobself-employed`  + contacttelephone + monthaug + 
                       monthmar + monthmay + monthnov + duration   + 
                       pdays4  + previous1 + poutcomesuccess + emp.var.rate + 
                       cons.price.idx + cons.conf.idx  , family = binomial("logit"), 
                     data = trainingdata)
summary(model)
#########################333
#install.packages("MKmisc")
require("MKmisc")
summary(model)
#HLgof.test(fit=fitted(model),obs=ifelse(trainingdata$y=="yes",1,0))
#HLgof.test(fit=fitted(model_optim),obs=ifelse(trainingdata$y=="yes",1,0))
HLgof.test(fit=fitted(model_optim_fin),obs=ifelse(trainingdata$y=="yes",1,0))
#######################arranget the prob in low to high
#####chop the data into 5 /10 groups
#########variance whould be different in each groups % of yes to be different between groups
############If students going to tuition and not tuition are same then the effect of tuition has no effect
#####so basically if i have different facukty based on exp for each section based on exp i need to have difference in  the results.
rchisq(n=1000,df=10)
hist(rchisq(n=1000,df=10))
#######Chi square is a naegatively skewed dist
##########Chi square reduced from 34 to 31 last model is the best 
#################we testd for variable and statistical significance
############Testug the generalization to be done
########Now we will plot training data dn testing data predictions
#############in banking you will need to put a model and submit to regultory body like rbi
##########they will need to approve the same 
colnames(trainingdata)
t1<-trainingdata
t1$predict_model<-predict.glm(model1,newdata=t1,type="terms")
t1[1:2,]
trainingdata[1:2,]
colnames(trainingdata)
trainingdata$predict_model
#########This is nothing but logodds
class(trainingdata)
#trainingdata[1:2,c(80,81)]
t1$predict_model<-predict.glm(model1,newdata=t1,type="response")
t1[1:2,]
trainingdata$predict_model<-predict.glm(model1,newdata=trainingdata,type="response")
trainingdata[1:2,]
testingdata$predict_model<-predict.glm(model1,newdata=testingdata,type="response")
model_optim<-model_optim_fin
trainingdata$predict_model_optim<-predict.glm(model_optim,newdata=trainingdata,type="response")
trainingdata[1:2,]
testingdata$predict_model_optim<-predict.glm(model_optim,newdata=testingdata,type="response")
trainingdata$predict_model_optim_fin<-predict.glm(model_optim_fin,newdata=trainingdata,type="response")
trainingdata[1:2,]
testingdata$predict_model_optim_fin<-predict.glm(model_optim_fin,newdata=testingdata,type="response")
#########understand the generalizability of the model
#install.packages("pROC")
library(pROC)
roc(y~predict_model,data=trainingdata)
plot(roc(y~predict_model,data=trainingdata))
############Decrease in specificity increases sensitivity
############Chemotheraphy sensitivity of 100% treatment kills tumor cells but it also kills healthy tissues which are also
###good so only cancer patients are weak
#################sniper is using high precision to shoot 
##########Sensitivity is good and also specificity is also good then it is good no person other than terrorist will be shot 
#############modle is about maximizing both sensotvty and specificity
###########Best is L shaped curve 
#####################but it happens only when data is wrong
colnames(testingdata)
testingdata[1:2,]
roc(y~predict_model,data=testingdata)
plot(roc(y~predict_model,data=testingdata))
##########Practicing in the practice games is different from playing in the actual field 
############3We do not expect drastoc diff
########we can still go with bad performance 5-10% based on ind analyst req
trainingdata[1:2,]
min(trainingdata$predict_model_optim_fin)
trainingdata[1:2,]
plot(roc(y~predict_model_optim,data=trainingdata))
plot(roc(y~predict_model_optim,data=testingdata))
######################
plot(roc(y~predict_model_optim_fin,data=trainingdata))
plot(roc(y~predict_model_optim_fin,data=testingdata))

roc(y~predict_model_optim_fin,data=trainingdata)
roc(y~predict_model_optim_fin,data=testingdata)

roc(y~predict_model,data=trainingdata)
roc(y~predict_model,data=testingdata)
dim(transformed_data)
##############Predict for entire data
transformed_data$predict<-predict.glm(model_optim_fin,newdata=transformed_data,type="response")
##############################33
plot(roc(y~predict,data=transformed_data))
roc(y~predict,data=transformed_data)
##############Confusion matrix
table(transformed_data$y,transformed_data$predict<=0.5)

table(transformed_data$y,transformed_data$predict>=0.5)

table(transformed_data$y,transformed_data$predict>=0.6)
#############specificity increases sensitivity decreases

table(transformed_data$y,transformed_data$predict>=0.2)
#########Sensitivity increase and specificity decreases
################Cutoff to be decided based on the model
##############Load a netwrork intrusion dataset
getwd()
train<-read.csv('./Day2/Python_Module_Day_16.2_Network_Intrusion_Train_data.csv')
test<-read.csv('./Day2/Python_Module_Day_16.3_Network_Intrusion_Test_data.csv')

val<-read.csv('./Day2/Python_Module_Day_16.4_Network_Intrusion_Validate_data.csv')

dim(train)
dim(test)
dim(val)
table(val$class)
table(train$class)
table(test$class)
############Model to understand network intrusion or network fraud 
summary(train)
###########50% issues are anomoly
############
View(train)
#########This is much more cleaner data
##########We will go nto model as we have covered basics
summary(train$protocol_type)
########33Categorical type
summary(train$service)
############Decision  trees bettwe for categorical data
######
summary(train$flag)
colnames(train)
##########Just convert only selected 3 variables 
########3Now transform data 
transformed_train<-model.matrix(class~.,data=train)
transformed_train<-as.data.frame(transformed_train)
summary(transformed_train)
#############Apply in  validation
transformed_val<-model.matrix(class~.,data=val)
transformed_val<-as.data.frame(transformed_val)
transformed_train$y<-train$class
transformed_val$y<-val$class
##############Build basic logistic regression
class(transformed_train$y)
model<-glm(y~.,data=transformed_train,family=binomial("logit"))
summary(model)
transformed_val$predict<-predict.glm(model,newdata=transformed_val,type="response")
############This variable is not available
############Introduce missed out variables
unique(transformed_train$servicehttp_8001)
transformed_val$servicehttp_8001<-0
##########Now run the model
transformed_val$predict<-predict.glm(model,newdata=transformed_val,type="response")
unique(transformed_train$servicered_i)
transformed_val$servicered_i<-0
###################
transformed_val$predict<-predict.glm(model,newdata=transformed_val,type="response")
###################
unique(transformed_train$serviceurh_i)
transformed_val$serviceurh_i<-0
transformed_val$predict<-predict.glm(model,newdata=transformed_val,type="response")
transformed_val[1:2,]
max(transformed_val$predict)
min(transformed_val$predict)
plot(transformed_val$predict)
plot(transformed_val$predict,transformed_val$y)
##############
#model_optim<-step(model,direction="both")
################This will take some time to run usually
#############Then compute roc for final model
#######################Some basic commands in logistic model
head(train)
train1<-as.matrix(train)
xtabs(~class+service,data=train)
#######
a1<-resid(model)
a2<-fitted.values(model)

############## hl test
#####################Load two other models one for random forest and one for decision trees and one for cluster analysis
############One for market basket analysis 
###########One for xgboost
############One detailed pca and factor analysis
###########################################################################################################
###########Learn decision trees ############3
iris = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
                  sep = ",", header = FALSE)
names(iris) = c("sepal.length", "sepal.width", "petal.length", "petal.width",
                "iris.type")
attach(iris)
install.packages("tree")
library(tree)
stree = tree(iris.type ~ ., data = iris)
stree
plot(stree)
text(stree)
stree = tree(iris.type ~ ., data = iris, split = "gini")
#########Training and Test error
test_sample = sample(1:150, 50)
test_data = iris[test_sample, ]
training_data = iris[-test_sample, ]
fit_tree = tree(iris.type ~ ., data = training_data)
test_predictions = predict(fit_tree, test_data, type = "class")
test_error = sum(test_predictions != test_data$iris.type)/nrow(test_data)
test_error
training_error = sum(predict(fit_tree, type = "class") != training_data$iris.type)/nrow(training_data)
training_error
##############################Decision tree example 
library(tree)
nn1<-bank.additional.full
colnames(nn1)
nn2<-nn1[,c(1,4,6,7,11,21)]
our.big.tree <- tree(y~., data=nn2)
summary(our.big.tree)
plot(our.big.tree)
text(our.big.tree)
#unique(bank.additional.full$job)
######################################################build and prune the tree
nn2<-nn1[,c(-22)]
our.big.tree <- tree(y~., data=nn2)
summary(our.big.tree)
######################
our.big.tree <- tree(y~., data=nn2,split="gini")
summary(our.big.tree)
#########Split into train and test and decide and best model
#############Now we will use rpart
library(rpart)
dim(bank.additional.full)
fulldata<-bank.additional.full
#############Split into train and test
set.seed(100)
AU<-fulldata
colnames(AU)
AU1<-AU[,c(-22)]

AU<-AU1
ss = sample.split(fulldata$y, SplitRatio = 0.8)
#write.csv(AU,'AU.csv')
train = subset(AU, ss == TRUE)
test = subset(AU, ss == FALSE)
table(train$y)
table(test$y)
###################
#Base Model

base_model <- rpart(y ~ ., data = train, method = "class",
                       control = rpart.control(cp = 0))


summary(base_model)
#Plot Decision Tree
plot(base_model)
text(base_model)
# Examine the complexity plot
printcp(base_model)
plotcp(base_model)


# Compute the accuracy of the pruned tree
test$pred <- predict(base_model, test, type = "class")
base_accuracy <- mean(test$pred == test$y)
base_accuracy
##################prepruning
# Grow a tree with minsplit of 100 and max depth of 8
model_preprun <- rpart(y ~ ., data = train, method = "class", 
                          control = rpart.control(cp = 0, maxdepth = 10,minsplit = 101))
# Compute the accuracy of the pruned tree
test$pred <- predict(model_preprun, test, type = "class")
accuracy_preprun <- mean(test$pred == test$y)
accuracy_preprun
####################

#Postpruning
# Prune the hr_base_model based on the optimal cp value
model_pruned <- prune(base_model, cp = 0.00692 )
# Compute the accuracy of the pruned tree
test$pred <- predict(model_pruned, test, type = "class")
accuracy_postprun <- mean(test$pred == test$y)
data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)
#########################
library(randomForest)
colnames(train)
rf <- randomForest(y ~ ., data=train)
getTree(rf, 1, labelVar=TRUE)
########
importance(rf)
varImpPlot(rf)
#install.packages("inTrees")
require("inTrees")
treeList <- RF2List(rf)  # transform rf object to an inTrees' format
  exec <- extractRules(treeList, train)  # R-executable conditions
exec[1:2,]
colnames(train)
############################
rf <- randomForest(as.factor(y )~ ., data=train)
treeList <- RF2List(rf)  # transform rf object to an inTrees' format
exec <- extractRules(treeList, train)  # R-executable conditions
exec[1:10,]
##########
summary(train$y)
##############
rf <- randomForest(as.factor(y )~ ., data=train)
treeList <- RF2List(rf)  # transform rf object to an inTrees' format
exec <- extractRules(treeList, train)  # R-executable conditions
exec[1:10,]
###########Take results 
train$pred_rf<-predict(rf,train)
test$pred_rf<-predict(rf,test)
table(train$pred_rf,train$y)
table(test$pred_rf,test$y)
711+42
753/824
colnames(train)
train<-train[,c(1:22)]
test<-test[,c(1:22)]
#############
require("caret")
control <- trainControl(method="repeatedcv", number=5, repeats=1)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)
tunegrid
rf_default <- train(y~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

##############Grid search
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(3:5))
rf_gridsearch <- train(y~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
################Another way to tune mtry
# Manual Search
control <- trainControl(method="repeatedcv", number=2, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train))))
modellist <- list()
for (ntree in c(50, 100, 200, 2500)) {
  set.seed(seed)
  fit <- train(y~., data=train, method="rf",  tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
#################################This is to just find best pair of mtry /ntree
###########mtry=5 also looks good for us as of now
colnames(train)
finalmodel<-randomForest(y~.,data=train,mtry=4,ntree=10000)

train$predict_rf_final<-predict(finalmodel,train)
test$predict_rf_final<-predict(finalmodel,test)
##################
table(test$predict_rf_final,test$y)
##########Increasung trees does not help much
######################Market basket analysis
library(arules)
#install.packages("arulesViz")
library(arulesViz)
library(datasets)
# Load the data set
data(Groceries)
class(Groceries)

Groceries[1:2,]
Groceries
# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])
rules<-sort(rules, by="confidence", decreasing=TRUE)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

#############
rules<-apriori(data=Groceries, parameter=list(supp=0.05,conf = 0.10), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
rules[1:3,]
rules

rules<-sort(rules, decreasing=TRUE,by="support")
rules[1:3,]
###############################################Cluster analysis example and validation of clusters

