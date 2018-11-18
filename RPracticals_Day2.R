################Basic plotting
data(mtcars)
head(mtcars)
View(mtcars)
####simple scatter plot
attach(mtcars)
plot(wt,mpg,main="ScatterplotExample",
xlab="Car Weight",ylab="Miles per Gallon",pch=22)
#####http://www.endmemo.com/program/R/pchsymbols.php
plot(wt,mpg,main="ScatterplotExample",
     xlab="Car Weight",ylab="Miles per Gallon",pch=22,col="red",bg="red")
######################Scalar 
x<-6

y<-x
y
y<-y+100

y
y=y*7
y
y=y^2

y
y/2
y%/%2

##############Other operators 
ifelse(y>=3,"Y is greater than 3 ","Y is less than 3")
###############
ifelse(y!=100,"Unmatched","Matched")
y
###############Log values 
log(20/5)

########bod mas rule 

1+4*5/3
#########Built in functions 
exp(1)

log(2)
factorial(3)

log(2,10)

log(2,100)

sqrt(729)
floor(29.5)
ceiling(678.3)

round(34.56,1)

#############Assign vectors 

a<-5:10
a
#############
length(a)

#####################
a<-c(5,6,7,8,9,10)
length(a)
###################Vector  multiplication

a<-c(2,3,4,3,4,6)
b<-c(1,2)
c<-a*b  
c
#############Vector manipulations

max(c)
min(c)

mean(c)
median(c)
range(c)
var(c)
#######Variance is x-xbar the whole square dicide by n-1
d<-c(8,8,9,9)
var(d)
#########Why we put n-1 for variance (as this is biased sample )

###########Correlation 

salary<-c(10000,10000,50000,60000,40000,30000)
loan<-c(5000,6000,30000,25000,16000,20000)



cor(salary,loan)
plot(salary,loan)
loan<-c(-5000,-6000,-30000,-25000,-16000,-20000)
cor(salary,loan)
############Negative correlation
##########Sort data 
sort(loan)
-sort(loan)
sort(-loan)
#########################Rank
rank(salary)
salary<-c(20,10,100,10)


order(salary)
#############Missing values 
a<-c(1:5,NA)
mean(a)
mean(a,na.rm=TRUE)

is.na(a)
which(is.na(a))
ifelse(is.na(a),0,a)

################Basic functions 

x<-0:5
sum(x)
sum(x<3)
sum(x[x<5])
sum(x[x>5])
x<3
########################sort and sum

y<-c(5,6,7,8)

rank(y)

sort(y)
sum(sort(y)[1:2])
#############Selecting vectors 
x<-c(1,2,3,4)
x[4]
x[-1]

######################Use tools and global options to choose between r environments 
installed.packages()
update.packages()
############Learn about one function
?join
help(join)
example(join)
example(mean)
help.search("mean")
??mean
#############Install multiple packages
install.packages("forecast","reshape2")
###########

  ###########Code to remove all from environment
##############Data Path
###https://www.kaggle.com/c/GiveMeSomeCredit/data
#############
rm(list=ls(all=TRUE))
#################setseed
set.seed(100)
###############################Load libraries or install
#install.packages("caret")
require("sqldf")
require("plyr")
require("lubridate")
require("readxl")
require("car")
require("caret")
require("caTools")
require("randomForest")
#############################set directory
getwd()
setwd('D:\\Philip\\Data backup\\Projects\\Kaggle\\credit\\all (1)\\')
getwd()
########################Load data and Replace space in column names
train<-read.csv('cs-training.csv')
colnames(train) <- gsub("\\ ","_",colnames(train))
#######################view data
train[1:2,]
##################find unique number of records
nrow(train)
##################View number of columns
ncol(train)
######################Remove duplicates based on unique id
train1<-train[!duplicated(train$X), ]
################No duplicates found
######View column names
colnames(train1)
########Arrange columns having independent columns then dependent at last 
train2<-train1[,c(3:12,2)]
colnames(train2)
#####################Analyze the data for age variable 
train<-train2
max(train$age)
min(train$age)
summary(train$age)
hist(train$age,10)
count(train$age<20)
################################Replace age less than 10 as average age 
train$age<-ifelse(train$age<10,mean(train$age),train$age)
summary(train$age)
#################View blanks 
count(is.na(train$age))
sum(!is.na(train$age))
#########################Replace age >100 by average value consider this as an outlier
#############View box plot for age variable
boxplot(train$age)
train$age<-ifelse(train$age>=100,mean(train$age),train$age)
summary(train$age)
######################Overall summary of the data
summary(train)
#############delinquency in 2 yrs
boxplot(train$RevolvingUtilizationOfUnsecuredLines)
sum(train$RevolvingUtilizationOfUnsecuredLines>1)
###################Limit the revolving credit limit to 1
train$RevolvingUtilizationOfUnsecuredLines<-ifelse(train$RevolvingUtilizationOfUnsecuredLines>=1,1,train$RevolvingUtilizationOfUnsecuredLines)
#####################Monthlyincome
boxplot(train$MonthlyIncome)
summary(train$MonthlyIncome)
##########View monthly income greater than 20k
sum(train$MonthlyIncome>=20000)
sum(train$MonthlyIncome>=20000,na.rm=TRUE)
###############Need to handle them 
#############################other commands to remove blanks
rmdata<-na.omit(train$MonthlyIncome)
class(rmdata)
rmdata<-as.data.frame(rmdata)
dim(rmdata)
##########
rmdata<-train[(na.omit(train$MonthlyIncome)),]
dim(rmdata)
