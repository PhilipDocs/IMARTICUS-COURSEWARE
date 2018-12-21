data(longley)
model <- lm( Employed ~ GNP, data=longley)
summary(model)
plot(density(resid(model))) #A density plot
qqnorm(resid(model)) # A quantile normal plot - good for checking normality
qqline(resid(model))