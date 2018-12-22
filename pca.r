
mtcars[1:20,]
#######PCA works with numerical variables
str(mtcars)
help(mtcars)
mtcars.pca <- prcomp(mtcars[,c(2:7,10,11)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)
#######Look at the object
str(mtcars.pca)
plot(mtcars.pca)
####Add principal components
train.data <- data.frame(mtcars$mpg, mtcars.pca$x)
train.data <- train.data[,1:3]
###########
train.data[1:2,]
mtcars[1:2,]
model1<-lm(mpg~.,data=mtcars)
summary(model1)
colnames(train.data)[1]<-"mpg"
model2<-lm(mpg~.,data=train.data)
summary(model2)
summary(model1)
