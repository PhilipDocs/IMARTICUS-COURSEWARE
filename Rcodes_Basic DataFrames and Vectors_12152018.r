number <- c("101000000000100" ,"1111","18888")
number
class(number)
#####all matches in this pattern
regmatches(number, gregexpr(pattern = "1.*1",text = number))
###take all ones only of 3 length
regmatches(number, gregexpr(pattern = "1.?1",text = number))
#############
a<-c("immmminnnni","iooooiiiiii")
regmatches(a, gregexpr(pattern = "i.?i",text = a))
a<-c("100010001","100111")
class(a)
regmatches(a, gregexpr(pattern = "1.?1",text = a))

a<-c("immmminnnni","iooooiiiiii")
regmatches(a, gregexpr(pattern = "i..?i",text = a))
a<-c("immmminnnni","iooooiiiiiiii")
regmatches(a, gregexpr(pattern = "i..?i",text = a))
#################################
names <- c("anna","crissy","puerto","cristian","garcia","steven","alex","rudy")
####doesnt bother if e required as match
grep(pattern = "e",x = names,value = T)

grep(pattern = "ri",x = names,value = T)


##############matches word containing t
names <- c("anna","crissy","puerto","cristian","garcia","steven","alex","rudy","rat")
grep(pattern = "t+",x = names,value = T)

#########N needs to occur 2 times
grep(pattern = "n{2}",x = names,value = T)
grep(pattern = "s{2}",x = names,value = T)

#################
###############3
string <- "I have been to Paris 20 times" 

#match a digit 
gsub(pattern = "\\d+",replacement = "_",x = string) 
##########Inclusive of space 
gsub(pattern = "\\d",replacement = "_",x = string) 
###########take matching decimal  or numberpoint
regmatches(string,regexpr(pattern = "\\d+",text = string))

###replace all non digit by _
gsub(pattern = "\\D+",replacement = "_",x = string) 
######
a<-"I drink 2 litres of water"
gsub(pattern = "\\D+",replacement = "_",x = a) 
########Replace for all strings lareg ________________
gsub(pattern = "\\D",replacement = "_",x = string) 

###########
regmatches(string,regexpr(pattern = "\\D+",text = string)) 

###########
#match a space - returns positions 
gregexpr(pattern = "\\s+",text = string) 
string
#################Match a non space
#match a non space 
gsub(pattern = "\\S+",replacement = "app",x = string) 
#match a non space for each alphabet replace by app
gsub(pattern = "\\S",replacement = "app",x = string)
string
#match a word character 
gsub(pattern = "\\w",replacement = "k",x = string) 
string

#match a non-word character 
gsub(pattern = "\\W",replacement = "k",x = string)  


#################Reg matches
string <- "20 people got killed in the mob attack. 14 got severely injured" 

#extract numbers 
regmatches(x = string,gregexpr("[0-9]+",text = string)) 
regmatches(x = string,gregexpr("[0-9]",text = string)) 


#############
string <- c("I sleep 16 hours\n, a day","I sleep 8 hours\n a day.","You sleep how many\t hours ?")

#get digits 
unlist(regmatches(string,gregexpr("[[:digit:]]+",text = string)))
unlist(regmatches(string,gregexpr("[[:digit:]]",text = string)))

##################
##########More text cleansing 
#remove punctuations 
gsub(pattern = "[[:punct:]]+",replacement = "",x = string) 
string
#remove spaces 
gsub(pattern = "[[:blank:]]",replacement = "-",x = string) 

#remove control characters like \n
gsub(pattern = "[[:cntrl:]]+",replacement = " ",x = string) 

#remove non graphical characters ideally remove the spaces
gsub(pattern = "[^[:graph:]]+",replacement = "",x = string)

#extract digits - all 4 works 
string <- "My roll number is 1006781" 
gsub(pattern = "[^0-9]",replacement = "",x = string) 

stringi::stri_extract_all_regex(str = string,pattern = "\\d+") #list 
regmatches(string, regexpr("[0-9]+",string)) 
regmatches(string, regexpr("[[:digit:]]+",string))  

################Remove spaces
#remove space 
gsub(pattern = "[[:space:]]",replacement = "",x = "and going there today tomorrow") 
gsub(pattern = "[[:blank:]]",replacement = "",x = "and going there today tomorrow") 
gsub(pattern = "\\s",replacement = "",x = "and going there today tomorrow")  

######################
#match values 
det <- c("A1","A2","A3","A4","A5","A6","A7") 
grep(pattern = "A1|A4",x = det,value =T) 
grep(pattern = "A1|A004",x = det,value =T) 

##########This will return the position of matches
grep(pattern = "A1|A4",x = det,value =F) 

grep(pattern = "A1|A004",x = det,value =F) 

###########Print some value
s<-5
sprintf("the cube of %d is %d",s,s^3)
s
################
###############matches
x<-c(1,3,6)
y<-c(3,6,9)
x%in%y


match(x,y)
##############Matrix operations
matrix(1:9, nrow = 3, ncol = 3)
matrix(1:9, nrow = 3)
####fill matrix by row by default column is used
matrix(1:9, nrow=3, byrow=TRUE)
x <- matrix(1:9, nrow = 3, dimnames = list(c("X","Y","Z"), c("A","B","C")))
colnames(x)
rownames(x)
class(x)
####Matrix cannot be combination of numeric and text one type only allowed
####rbind and cbind to define matrix
cbind(c(1,2,3),c(4,5,6))
rbind(c(1,2,3),c(4,5,6))

##############matrix definition
x <- c(1,2,3,4,5,6)
dim(x)<-c(1,6)
class(x)

##################modify a matrix
x[1,2] <- 10;
x
x[x<5] <- 0
x
##########Basic descriptive staats
library(MASS)
data(Boston)
Boston
dim(Boston)
nrow(Boston)
ncol(Boston)
colnames(Boston)
summary(Boston$crim)
hist(Boston$crim)
##################
###########Negatively skewed
########view first five rows
head(Boston)
Boston$newcol<-Boston$nox*100
max(Boston$newcol)
##############
max(Boston$zn)
min(Boston$zn)
Boston$newcol2<-ifelse(Boston$z>=90,1,0)
unique(Boston$newcol2)
###########################################################
head(Boston)
g1<-subset(Boston,newcol2>0)
length(g1)
nrow(g1)
g1<-subset(Boston,newcol2>0,select=c(1,2))
g1
###############333
summary(Boston$indus>=20)
table(Boston$indus>=20)

#################
vect=1:40
length(vect)
vect>=40
vect[vect>=30]
##########################View summary
summary(Boston)
library(dplyr)
data("Boston")
class(Boston)
dim(Boston)
#########Filter
filter(Boston,indus>=20)
test<-filter(Boston,indus>=20) %>% select(1:4)
test[1:2,]
dim(test)
colnames(Boston)
###############################
s1<-Boston %>% select(crim,zn)
dim(s1)
colnames(Boston)
s2<-Boston %>% select(crim,zn) %>% filter(zn<1)
max(s2$zn)
max(Boston$zn)
##############################
min(s2$zn)
max(s2$zn)
dim(s1)
dim(s2)
s3<-Boston %>% select(crim,zn) %>% filter(zn<1&crim>3)
dim(s3)
##################3
new_vect<-Boston$medv
class(new_vect)
dim(new_vect)
new_vect[15]
class(Boston)
nrow(Boston)
Boston[1,3]
###############################################Needs some practice
#######################################Class_12_!5_2018
#############Multiple elements of vector
new_vect[c(1,3)]
Boston[1:4,]
Boston$class<-ifelse(Boston$medv>=90,"High","Low")
h1<-as.factor(Boston$class)
class(h1)
##################View data
View(Boston)
###################Matrix add,subtract and multiply
A <- matrix(c(2,3,-2,1,2,2),3,2)
is.matrix(A)
c <- 3
c*A
B <- matrix(c(1,4,-2,1,2,1),3,2)
C <- A + B

C
D <- matrix(c(2,-2,1,2,3,1),2,3)

E=C%*%D
E

F=D%*%C
F
D
C

A
AT <- t(A)
AT

##########Common vectors
U <- matrix(1,3,1)
######unit matrix
U <- matrix(1,3,2)
U
########zER MATRIX
matrix(0,3,2)

##########Identity matrix
I <- diag(c(1,1,1))
I
#########Symmetric matrix
C <- matrix(c(2,1,5,1,3,4,5,4,-2),3,3)
C
################3
A <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)
A
C <- matrix(c(2,1,6,1,3,4,6,4,-2),3,3)
det(C)
A <- matrix(c(2,3,-2,1,2,2,4,7,0),3,3)
A$rank

#####################################3333
############UCI machine learning repository
data<-read.csv('file:///C:/Users/128564/Desktop/data.csv')
dim(data)
head(data)
############3333
require("xlsx")
getwd()
###################################
head(data)
##################################3
names(data)%in% c("workclass","doorno")
#####################detail about list 
alist <- list ("Red", "Blue", c(42,36,01), FALSE, 73.91, 128.6)
alist
alist[1]

alist[3]
##############appply neeed to be used only for matrix
m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum)
a_m1



####################lapply

####l in lapply() stands for list. The difference between lapply() and apply() lies between the output return. The output of lapply() is a list. lapply() can be used for other objects like data frames and lists.


movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower)
str(movies_lower)


Boston[1:2,]
f1<-Boston$class
Boston$all1<-lapply(f1,toupper)

Boston[1:2,]
Boston$all1<-tolower(Boston$all1)
head(Boston)


#############mapply

##############tapply

################sapply 

##############Join columns

###############cumulative sum 

#########basic sql commands 

############Date functions

###############Tidy r

#################R markdown basics

#################R loops

#################basic functions 

#################Loss functions 

##################calculate mean ,median ,stdev

###########Regression
install.packages("ISwR")
library(ISwR)
data(bp.obese)
bp.obese[1:5,]
attach(bp.obese)
plot(bp,obese)
###########Find correlations
cor(bp,obese)
####################
cor.test(obese,bp)
##########this tests correlation test statistics
##test statistic ta=r (sqrt(n-2/1-r^2))
###p value is low reject null hypothesis it is different from zero
bp.obese.lr1 <- lm(bp~obese)
summary(bp.obese.lr1)
#####The first row correspond to intercept
###the constant 96 with error 8.9
###t statistic =96-0/8.9 is test statistic the value is 10.86
####n-2 degrees of freedon
##3the last column gives p value
###similiar interpretation for obese also
###########Multiple r squared is nothing but square of product moment correlation 
#########adjusted r square penalises for the number of variables
#########f statistic is available below for regressio hypothesis
plot(obese,bp)
lines(obese,fitted(bp.obese.lr1))
names(bp.obese.lr1)
bp.lr1.resid <- bp.obese.lr1$residuals
bp.lr1.fit <- bp.obese.lr1$fitted.values
plot(bp.lr1.fit,bp.lr1.resid)
##################
qqnorm(bp.lr1.resid,type='n')
qqline(bp.lr1.resid)
#####################################rank variable to character
a<-c(1:10)
a
class(a)
a<-as.factor(a)
class(a)
levels(a)
a<-c(1:10)
levels(a)
################vector can be combined to form a vector
####merge matrix characterand numeric then data frame is formed 
#############or a long character vector
#########Date values
mydates<-as.Date("2017-06-22","2017-07-20")
mydates<-as.Date(c("2017-06-22","2017-07-20"))
mydates
class(mydates)
max(mydates)
#####
Sys.Date()
####Current date and time
date()
##########
today<-Sys.Date()
today
require("lubridate")
month(today)
year(today)
day(today)
format(today,"%B%d%y")
format(today,"%b%d%y")
#########Save dates in string format
d1<-c("2017-06-22","2017-07-20")
class(d1)
d1<-as.Date(d1,format="%m%d%y")
class(d1)
d1
######to convert to character
d2<-as.character(d1)
class(d2)
#################Functions in r
x<-1:4
for (i in x)
{
  if(i==2)
  {
    next
  }
  print(i) 
  
}
###############Condition 2
n<--5
{
  if(n<0){
    stop("The argument n must be +ve")
  }}
##############Basic functions 3
x<-c(0.6789,0.7856,0.6632,0.3487)
percent<-round(x*100,digits=2)
result<-paste(percent,"%",sep="")
print(result)
###########
###############Call a r function
source("file:///C:/Users/128564/Desktop/percentfunction.r")
###############add addpercent function 
###Ctrl+shift+c for commenting
# addpercent<-function(x)
# {
#   percent<-round(x,digits=2)
#   result<-paste(percent,"%",sep="")
#   print(result)
# }


source("file:///C:/Users/128564/Desktop/addpercent.r")
s<-c(0.2234,0.4522,0.44423,0.6744,0.9833,07833)
addpercent(s)
######################while function
i <- 1
while (i < 6) {
  print(i)
  i = i+1
}
#################3
x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 0)  count = count+1
}
print(count)
###############while function
x<-5
f.while<-function(x)
{
  i<-0
  while(i<x)
  {
    i<-i+1
    y<-i*2
    print(y)
  }
  return(c(i,y,y*2))
}
f.while(x)  
###################
w<-function(x)
{
  x1<-(x^2)+1
  return(x1)
}
w(100)
w(0)
w(9)
a<-c(0,23,12)
w(a)
############### two vectors
w1<-function(x,m)
{
  return(x*m+x+m)
}

w1(2,3)
################definig a matrix
matdata<-matrix(c(1:16),nrow=8,ncol=2)
matdata<-matrix(c(1:16),nrow=8,ncol=2,byrow=TRUE)
matdata
############Count of even number
even<-function(x)
{
  k<-0
  for(n in x)
  {
    if(n%%2==0)
      k<-k+1
  }
  return(k)
}
x<-c(1,3,2,4,5,6,2,6,4)
even(x)
###########3
x<-c(0,0,1,2)
even(x)
##########function to find factorial
# take input from the user
num = as.integer(readline(prompt="Enter a number: "))
factorial = 1
# check is the number is negative, positive or zero
if(num < 0) {
  print("Sorry, factorial does not exist for negative numbers")
} else if(num == 0) {
  print("The factorial of 0 is 1")
} else {
  for(i in 1:num) {
    factorial = factorial * i
  }
  print(paste("The factorial of", num ,"is",factorial))
}
################################
arithmetic_mean<-function(x,na.rm=FALSE)
{
  if(!is.numeric(x))
    Warning("Convert x to numeric")
  x<-as.numeric(x)
  if(any(x<=0&!is.na(x)))
  {stop("x contains 0 or negative values")}
  mean(x,na.rm=TRUE)
  
}

c1<-c(10,2,4,12,34,NA)
arithmetic_mean(c1)

c1<-c(10,2,4,-12,34,NA)
arithmetic_mean(c1)
c1<-c(10,2,0,12,34,NA)
arithmetic_mean(c1)

c1<-c(10,2,10,12,34,NA)
arithmetic_mean(c1)
#######################Find variable which is having highest correlation
library(MASS)
data(Boston)
cor_userdef<-function(data=data,dep_var=dep_var)
{
  data_for_cor=eval(parse(text=data))
  data_dep=data_for_cor[names(data_for_cor)==dep_var]
  data_indep=data_for_cor[!names(data_for_cor)==dep_var]
  cor<-cor(data_dep,data_indep)
  cor_names<-row.names(cor(data_dep,data_indep))
  # cor_df<-cbind(cor_names,cor)
  cor_df<-cor
  cor_value=cor_df[abs(cor)==max(abs(cor))]
  return(cor_value)
}
cor_userdef(data="Boston",dep_var="crim")

##################################################################33
##Exercise 1

###Create a function to print square of number

###Exercise 2

###Create a function to print a number raise to another with the one argument a default argument
###Exercise 3

##Create a function to print class of an argument