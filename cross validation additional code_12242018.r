#cross validation
ctrl2 <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit2 <- train(deposit~age+job+education+default+balance+housing+ 
                    loan+contact+day+ month + duration + campaign,  data=train, method="glm", family="binomial",
                  trControl = ctrl2)
summary(mod_fit2)
test$pred_glm3<-predict(mod_fit2,test)
test$deposit<-as.factor(test$deposit)
test$pred_glm3<-as.numeric(test$pred_glm3)
unique(test$deposit)
unique(test$pred_glm3)
test$class1<-ifelse(test$pred_glm3>=0.5,1,0)
unique(test$class1)
unique(test$deposit)
test$class1<-as.factor(test$class1)
s1<-confusionMatrix(test$class1, test$deposit)

model2<-glm(deposit~.,data=train,family=binomial("logit"))
summary(model2)

test$pred_glm3<-predict(model2,test)
test$deposit<-as.factor(test$deposit)
test$pred_glm3<-as.numeric(test$pred_glm3)
unique(test$deposit)
unique(test$pred_glm3)
test$class1<-ifelse(test$pred_glm3>=0.5,1,0)
unique(test$class1)
unique(test$deposit)
test$class1<-as.factor(test$class1)
s2<-confusionMatrix(test$class1, test$deposit)
s1
s2
1475/(1475+287)