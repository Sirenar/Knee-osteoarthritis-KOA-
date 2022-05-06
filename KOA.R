library(readxl)
data<-read.csv("C:\\Users\\Lyndon\\Desktop\\procedata.csv")
data <- data[,-1]
library(glmnet)
set.seed(1)
n <- sample(1:36,10)
train.x <- data.matrix(data[-n,-1])
train.y <- factor(data$Severity)[-n]
test.x <- data.matrix(data[n,-1])
test.y <- factor(data$Severity)[n]

logit <- cv.glmnet(train.x,train.y,
                   alpha=1,
                   family=binomial(link="logit"),nfold=5)
plot(logit)
pred <- predict(logit,test.x,s=logit$lambda.min,type="response")
pred1 <- ifelse(pred>0.5,"Severe","Early")

library(plotROC)

d <- data.frame(D=test.y,M=pred[,1])

ggplot(d,aes(m=M,d=D))+
  geom_roc()+
  geom_abline()+
  theme_classic()
coef(logit)

library("randomForest")
train <- data[-n,]
test <- data[n,]
err<-as.numeric()
for(i in 1:(length(names(train)))-1){
  mtry_test <- randomForest(factor(Severity)~., data=train, mtry=i)
  err<- append( err, mean( mtry_test$err.rate ) )
}
print(err)
mtry<-which.min(err)
ntree_fit<-randomForest(factor(Severity)~., data=train, mtry=mtry, ntree=1000)
plot(ntree_fit)
tree <- randomForest(data=train,factor(Severity)~.,mtry=2,ntree=100)
tree
plot(margin(tree, train$Severity), main = 'Probability of observations being judged correct')
pred_tree <- predict(tree,test,type="prob")

d1 <- data.frame(D=factor(test$Severity),M=pred_tree[,2])

ggplot(d1,aes(m=M,d=D))+
  geom_roc()+
  geom_abline()+
  theme_classic()

varImpPlot(tree)

