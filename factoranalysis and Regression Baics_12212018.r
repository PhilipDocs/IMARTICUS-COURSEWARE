data("USArrests")
USA<-USArrests
dim(USA)
fit9<-princomp(USA,cor=TRUE)
fit10<-princomp(USA,cor=FALSE)
summary(fit9)
summary(fit10)
loadings(fit9)
plot(fit9,type="line")
fit9$scores
biplot(fit9)
##########Higher the loading more importance for that 
##################Exploratory factor analysi 
dim(USA)
install.packages("nFactors")
require("nFactors")
n.factors <- 1   

fit <- factanal(USA, 
                n.factors,                # number of factors to extract
                scores=c("regression"),
                rotation="none")
fit
print(fit, digits=2, cutoff=.3, sort=TRUE)
head(fit$scores)
# plot factor 1 by factor 2 
load <- fit$loadings
plot(load,type="n") # set up plot 
text(load,labels=names(USA),cex=.7) # add variable names
fit <- factanal(USA, 
                n.factors,              # number of factors to extract
                rotation="varimax")     # 'varimax' is an ortho rotation

load <- fit$loadings 
load
plot(load,type="n") # set up plot 
text(load,labels=names(USA),cex=.7) # add variable names

library(psych)
solution <- fa(r = cor(USA), nfactors = 1, rotate = "oblimin", fm = "pa")
plot(solution,labels=names(USA),cex=.7, ylim=c(-.1,1)) 
solution
install.packages("psy")
library(psy)
ncol(USA)
scree.plot(fit$correlation)
# Determine Number of Factors to Extract
# install.packages("nFactors")
#####################################################
data(mtcars)
summary(mtcars)
mlmod<-lm(mpg~disp,data=mtcars)
summary(mlmod)
dim(mtcars)
mlmod$residuals
rownames(mtcars)
mtcars$pred<-mlmod$residuals+mtcars$mpg
mtcars[1:2,]
21-18.99456
###############################33
mlmod<-lm(mpg~.,data=mtcars)
summary(mlmod)
##########Variables are not significant 
######forward and back ward step wise regression
#######fstats explains that model is good
library(usdm)
vif(mtcars[,-1])
mlmod$coefficients
mlmod$fitted.values
##########