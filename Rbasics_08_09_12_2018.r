sample<-21:30
sample
###view vector
###retrieve 5th element
sample[5]
#####anything that stores some value is object 
###any operations you do on an object is a function
####basic operations with avector 
vect_one=sample
vect_one-1
vect_sub_by_1=vect_one-1
###########view this vector
vect_sub_by_1
####vector subtraction 
####[21....30]-[1....]
###this is waht happens
###
vect_sub<-vect_one-c(2,0)
vect_sub
###########Filtering data 
# select variables v1, v2, v3

vect_one
####vector recycling 
#####[21...30]-[2,0,2,0,2....0]
#####basic class of a object
class(vect_one)
###vector one is a object of class integer and the type is basic vector 
####two time of vector -atomic and non atomic 
###atomic is of similiar data types 
###matrix is enerally used for high scale computations and it is another data type
vect_1<-10:20
vect_2<-20:30
vect_3<-30:40
##########################create a new object
new_obj<-matrix(c(vect_1,vect_2,vect_3))
new_obj
class(new_obj)

##########Its a matrix and it is stacked 
#####Instead of matrix can be passed as a data frame
new_obj<-data.frame(vect_1,vect_2,vect_3)
class(new_obj)
######Usually we use data frame 
####dataframe typically represents a database
####anything we get from external world and connect it stores as 
###data frame in R
################data frame to another object 
new_obj<-as.matrix(new_obj)
class(new_obj)
new_obj
##############Now it is converted to matrix
######Instead of all rows we have row instance and column instance
##########Basic operations within R 
##########Understanding basic data types matrices,arrays lists etc and data frame 
####Also we will be covering factors 
####what is assignment in R
###X<-Y means Y is assigned to X
###it can also be witten as X=Y
######X<-a allocating character
####x<-"a" character
#### not equal to !=
####> ,<,| or ,& for and
####formula sign ~
####$ is indicative of accessing columns of dataframe 
#### colon operator for sequence :
###### R default is >
###default log is log to the base e 
log(2)
2+3;9-0;
###semilcolon to seperate statements
###exponent of 1
exp(1)
###log(x) base e
###exp(x) is antilog of x so e^x
log(100,10)
####pass the base for a log 
###factorial (x)
factorial(4)
###floor(x) to lower bound and ceiling for upper bound 
floor(3.56)
ceiling(9.3)
###truncation 
trunc(9.3)
trunc(9.3333)
#####round to round the numbers
round(3.48)
round(3.51)
round(3.53,1)
round(3.58,1)
###############random number 
runif(4)
runif(3,min=0,max=106)
########another command 
floor(runif(3, min=0, max=101))
###sample command
sample(1:100, 3, replace=TRUE)
sample(1:100, 3, replace=FALSE)
###normal distributed data 
rnorm(4, mean=50, sd=10)
###view it 
x <- rnorm(400, mean=50, sd=1000000)
hist(x)
##############trignometric functions
####absolute value
abs(-100)
#############################################Need to take from 12/09/2018
##########Install R tools
install.packages("installr")

require("installr")
install.Rtools() 
#################################
####power
2 ^(3)
2^(-3)
1/8
##########
ls()
a<-1
b<-100
c<-1001
d<-9090
ls()
##########Remove all r
rm(list= ls()[!(ls() %in% c('a','b'))])

ls()
rm(list=setdiff(ls(), "a"))

ls()
rm(list = ls())
ls()
########modulo function
200/11
200%%11
11*18+2
###vectors 
###vectors are variables with one more quantity 
a<-7
a
a<-1.3
a
a<-5:10
a
###sequence
a1<-11
a2<-c(a1,a)
a2
####combining vectors
A<-7
a<-7
a==A
a<-6
a==A
#####R is case sensitive 
####vector multiplication 
###longest vector remains as it is and shorted vector is recycled
a<-1:3
b<-10
a*b
b<-10:11
c=a*b
c
b<-10:15
c=a*b
a
b
#########Some functions which are for vectors not for data frame 
max(c)
min(c)
sum(c)

###########YOu can sum all vectors provided all are numeric vectors
A<-1:5
mean(A)
median(A)
range(A)
var(A)
sd(A)
sqrt(10)/2
#####sample variance and population variance is different but in research we divide n-1
###i also do not accept with that but in general stats the formula is n-1
sqrt(2.5)
####sorting data 
# sorting examples using the mtcars dataset
attach(mtcars)
mtcars
# sort by mpg
head(mtcars)
newdata <- mtcars[order(mpg),] 
head(newdata)
# sort by mpg and cyl
newdata <- mtcars[order(mpg, cyl),]

#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(-mtcars$mpg, -mtcars$cyl),] 
newdata[1:2,]
mtcars
detach(mtcars)

################Ranks using r

(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))

x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2)) # ties are averaged
###############Missing value is one of the challenges in analytics 
############create sequence
data<-c(1:5,NA)
data
###to compute mean of 
a<-c(1:5,NA,NA,8,10)
a
###Is.na applies to vector to display true or false
is.na(a)
####location to na
which(is.na(a))
###convert na to 0
a[is.na(a)]<-0
a
####total if number is less than 3
sum(a[(a<3)])
sum(a[a<8])
a<5
###
y<-c(8,9,2,3)
sort(y)
sum(sort(y))
y
??sort
sort(y,decreasing=TRUE)
###sum of sort by position
sum(sort(y[c(2,4)]))
###Trim vectors 
y
y[4]
y[c(1,3)] 
y1<-y[-4]
y1
#####True or false 1 or true and 0 as false in r
d1<-0:10
d1<10
d1>0
d1>=0
d1!=9
##################
any(d1<0)
all(d1>4)
all(d1>-100)
any(d1>100)
all(d1>-100)&any(d1==3)
all(d1>-100)&all(d1==3)
###Logical operators
######| is or operator 
all(d1>-100)|any(d1==3)
#####
all(d1>-100)!= any(d1==3)
###
##& and | do elementwise comparisons between vectors and return a logical vector. && and || compare just the first elements of each vector and return a single logical value.
a<-c(2,3,4,5)
((-2:2) >= 0) & ((-2:2) <= 0)
#########just comparesfirst two elements the below one

######sequence generation
rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))
rep(1:4, each = 2, len = 4)    # first 4 only.
##########sorting ranking ordering 
sales<-c(100,300,400,50)
sales
rank(sales)[4]
rank(sales)
rank(-sales)
###########View rank function
rank
#########################R is one such language we can reference same object again 
a<-a
a1<-a
a1
sort(a1)
######################convert to data frame
viw<-data.frame(a1)
viw
#############set.seed retains the values
set.seed(1234)
x <- matrix(rnorm(30, 1), ncol = 5)
y <- c(1, seq(5))
seq(4)

#combining x and y into one matrix
x <-as.data.frame( cbind(x, y))
class(x)
#converting x into a data frame called x.df
x.df <- data.frame(x)
x.df
names(x.df)


x.sub <- subset(x.df, y > 2)
x.sub
x.sub1 <- subset(x.df, y > 2 & V1 > 0.6)
x.sub1
x.sub8 <- x.df[c(1, 3), 3:6]
x.sub8 
#######
airquality[1:5,]
subset(airquality, Temp > 10, select = c(Ozone, Temp))
#########
x
class(x)
x<-as.vector(x)
subset(x,x>0)
x
##################
##selection function which 
###this outputs the position of vectors which are greater than 3
class(x)
which(x>3)
#############
which(x^2>100)
which(x^2>=10)
which(x^2>5)
#######################R has ifelse
ifelse(x%%3==0,"Yes","No")
####nestedifesle


x<-c(1,2,3,45)

y<-ifelse(x>3,1,ifelse(x>4,1,0))

y
#########Character strings 
a<-"Hello"
class(a)
a<-55
class(a)
####this is acharacter and 55 is a value 
###when you have vector with double quotesthis is a character
###difference betwen length of character and no of characters in a character
j1<-"I need to work"
###length counts as a single element and nchar to count the characters
length(j1)
nchar(j1)
###Concatenate strings
fi

j1<-paste("HELLO","all",sep="")
nchar(j1)

class(j1)
####checking whether this is a factor
a<-"Helloalal"
is.factor(j1)
#######
j1<-as.vector(a)
class(j1)
#########still its a vector but a character vector
j2<-as.data.frame(j1)
j2
class(j2)
dim(j2)
###Convert to fector
j2<-as.data.frame(j1)
j2
j2<-"helloall"


j3<-as.factor(j2)
levels(j3)
j3
j2
class(j1)
j1
############################
j1<-"Helloall"
j3<-as.factor(j1)
class(j3)
j3
##########letters small and capitals
##letters
##LETTERS
####returns position of the alphabet k
which(letters=="k")
#######
a<-"Hello"
b<-"everyone"
b1<-c(a,b)
b1
sports<-c("cricket","football","hockey","rugby")
popular<-c("cricket")
which(popular==sports)

sports<-c("football","cricket","hockey","rugby")
which(popular==sports)
#######Matching character vectors
z<-c("sports","fruits")
grep("p",z)
grep("[a-z]", letters)
letters
LETTERS
txt <- c("arm","foot","lefroo", "bafoobar")
class(txt)
grep("foo",txt)
####################
###substrings in r
substr(z[1],start=2,stop=3)
####string replace
gsub("s","j",z)
gsub("fr","@@",z)
##########strsplit
a<-c("arun-28-20k")
a1<-unlist(strsplit(a,"-")," ")
a1
length(a1)
a1[1]
#######################################substitute 
x <- "r tutorialr"
y<-sub("r","HTML",x)
y
###########Regular expression
x<-"r tutorialr"
y <- sub("t.*r","BBBBB", x)
y

x<-"r tutorialr"
y <- sub("t.*r","BB", x)
y
############
y <- sub("t.*r","BBBBB", x, fixed=TRUE)
y

k<-"t.*riii"
k1<- sub("t.*r","BBBBB", k, fixed=TRUE)
k1
############Replace only ine digit
x <- c("line 400", "good weather next 10 days", "899 percent")
y <- sub("[[:digit:]]","",x)
y
###replace all digits
y <- sub("[[:digit:]]+","",x)
y

##################
b<-"inautix"
b <- sub("i", "", b)
b

b <- sub("a", "", b)
b
c<-sub("n.*i","",b)
c

####remove text before and after
rs<-c("copyright @ rights reserved","I want you to see me @ the coffeshop")
###remove after @
s<-gsub("@.*","",rs)
s
s<-gsub(".*@","",rs)
s
####remove before e
s<-gsub(".*e","",rs        )
s


#######################sub and gsub
a<-"a1111a111a87"
###sub replace first occurence
sub("a","",a)
a
###gsub replace all
gsub("a","",a)
################
a<-"apple"
gsub("a","$",a)
#############regexpr first match it takes 
x<-c("apple","mango","banana")
r<-regexpr("a",x)
r
#########all matches use gregexpr
gregexpr("a",x)
############################

a<-c("I NEED HELP")
b<-tolower(a)
c<-toupper(b)
##############################chartr function to replace
a<-"i need help"
chartr("help","money",a) 
####Only take first 4 characters
####Check if two strings are equal
setequal(c("monday","tuesday","wednesday"),c("monday","tuesday","wednesday")) 
###this is same 
setequal(c("monday","tuessday","wednesday"),c("monday","tuesday","wednesday")) 