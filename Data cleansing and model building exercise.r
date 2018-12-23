rm(list=ls(all=TRUE))
setwd("C:\\Users\\128564\\Desktop\\121\\")
bank=read.csv("./Day2/bank.csv")
str(bank)
require("plyr")
#######################Data Understanding Function
#######################################################################Function 1
data<-bank
##########################Function 1 to calculate data distributions
datadist<-function(data)
{

a<-colnames(data)
a<-as.data.frame(a)
i=1
for(i in 1:nrow(a))
{
  data1<-data[,c(i)]
  data1<-as.data.frame(data1)
  a2<-length(unique(data1$data1))
  
  a3<-sum(is.na(data1$data))
  a3<-nrow(data)-a3
  a2<-as.data.frame(a2)
  a3<-as.data.frame(a3)
  if(i==1)
  {
    out<-cbind(a2,a3)
    out$col<-as.character(a[i,1])
    out1<-out
    out1<-as.data.frame(out1)
  }
  if(i>1)
  {
    out<-cbind(a2,a3)
    out$col<-as.character(a[i,1])
    out<-as.data.frame(out)
    out1<-rbind(out1,out)
    out1<-as.data.frame(out1)
    
    
  }
  print(i)
}

colnames(out1)
class(out1)
out1[1:10,]
colnames(out1)<-c("Unique","NonBlanks","Colname")
out2<-out1[,c(3,1,2)]
return(out2)
}
##########################################
datacl1<-datadist(data)
datacl1[1:2,]
########################################No blank cells
########################Function to handle data as  it is into continous varaibles
######################Identidy categorical-ordered and unordered variables 
######################Split tran and test
datasplit<-function(AU,rat,seed)
{
set.seed(seed)
t<-ncol(AU)
ss = sample.split(AU[,c(t)], SplitRatio = rat)
#write.csv(AU,'AU.csv')
train = subset(AU, ss == TRUE)
test = subset(AU, ss == FALSE)
list1<-list("a"=train,"b"=test)
return(list1)
}
##################################Call data split R
data[1:2,]
colnames(data)
out<-datasplit(data,0.8,100)
class(out)
length(out)
train<-out[[1]]
test<-out[[2]]
table(train$deposit)
table(test$deposit)
table(data$deposit)
###########################################Now create feature vectors
##################Store all categorical -ordinal or nominal variables
datacl1
nrow(train)+nrow(test)
str(train)
datacl1$percent<-datacl1$Unique/datacl1$NonBlanks
datacl2<-datacl1[order(datacl1$percent),]
datacl2
##################
summary(train$pdays)
table(train$pdays)
##################Leave as continous
datacl2
dim(datacl2)
class(datacl2)
datacl3<-datacl2[(1:14),]
datacl3<-datacl3[-4,]
datacl3
colnames(train)
colnames(train)[17]<-"errorallind"
colnames(test)[17]<-"errorallind"

###################Now create feature vectors in train and apply in test and prepare dataset
fvector<-function(g1,train1)
{
for (i in 1:nrow(g1))
{
  a1<-g1[i,1]
  a1<-as.character(a1)
  f1<-nrow(train1)
  f2<-table(train1[,a1],train1$errorallind)
  colnames(f2)<-c("NonEvent","Event")
  f2<-as.data.frame(f2)
  f4<-f2
  
  f5<-f4[(f4$Var2=="NonEvent"),]
  f6<-f4[(f4$Var2=="Event"),]
  colnames(f5)<-c("VariableType","Ev_Type","Freq_NonEvent")
  colnames(f6)<-c("VariableType","Ev_Type","Freq_Event")
  f5<-f5[,c(1,3)]
  f6<-f6[,c(1,3)]
  f7<-join(f5,f6)
  f7$Totalrec<-f7$Freq_Event+f7$Freq_NonEvent
  f7$EventPercent<-f7$Freq_Event/f7$Totalrec
  f7$Coverage<-f7$Totalrec/sum(f7$Totalrec)
  f7$Varname<-a1
  if(i==1)
  {
    f9<-f7
  }
  if(i>1)
  {
    f9<-rbind(f9,f7)
  }
  i=i+1
  print(i)
  
}
  return(f9)
}
##############Generate feature vector
fdata<-fvector(g1=datacl3,train1=train)
write.csv(fdata,'featurevectors.csv')
############Load feature vector in train and test
loaddata<-function(g1,f10,train)
{
for (i in 1:nrow(g1))
{
  a1<-g1[i,1]
  a1<-as.character(a1)
  f11<-f10[(f10$Varname==a1),]
  f12<-f11[,c(1,5,6)]
  colnames(f12)[1]<-as.character(a1)
  colnames(f12)[2]<-paste(as.character(a1),"Event_Percent",sep="_")
  colnames(f12)[3]<-paste(as.character(a1),"Coverage",sep="_")
  train<-join(train,f12)
  i=i+1
}
return(train)  
}
trainall<-loaddata(g1=datacl3,f10=fdata,train=train)
testall<-loaddata(g1=datacl3,f10=fdata,train=test)
##############################View trainall and testall
###############################Replace NA by 0 by default as a starting point 
trainall[is.na(trainall)] <- 0
testall[is.na(testall)] <-0
###########################################Random forest model
colnames(trainall)
class(trainall)
trainall[1:2,]
model1<-glm(errorallind~.,data=trainall[,c(14:43)],family = binomial(link='logit'))
summary(model)
##################################
model2<-glm(errorallind ~.,data=trainall[1:17],family=binomial(link="logit"))
summary(model)
####################################
model3<-randomForest(errorallind~.,data=trainall[1:17],ntree=200,mtry=4)
model3
##################################
model4<-randomForest(errorallind~.,data=trainall[14:43],ntree=200,mtry=4)
model4
##########################################
trainall$model1_out<-predict.glm(model1,trainall,type="response")
max(trainall$model1_out)
min(trainall$model1_out)
trainall$model2_out<-predict.glm(model2,trainall,type="response")
trainall$model3_out<-predict(model3,trainall)
trainall$model4_out<-predict(model4,trainall)
###################Apply in test
testall$model1_out<-predict.glm(model1,testall,type="response")
max(trainall$model1_out)
min(trainall$model1_out)
testall$model2_out<-predict.glm(model2,testall,type="response")
testall$model3_out<-predict(model3,testall)
testall$model4_out<-predict(model4,testall)
###########################################Take rank tables out for all 3 models
colnames(trainall)
train_m<-trainall
test_m<-testall
colnames(train_m)


