
#this didn't work b/c I don't think cubist is good for binary outcomes
library(Cubist)
x <- cubist(comb, as.numeric(outcome))



library(caret)
library(pROC)
library(e1071)

set.seed(1234)

splitIndex <- createDataPartition(outcome, p=.80, list=FALSE, times=1)
#get rid of group_1 and char_10.y 
train <- comb[splitIndex,c(-2, -50)]
test <- comb[-splitIndex,c(-2, -50)]

trainoutcome <- outcome[splitIndex]
testoutcome <- outcome[-splitIndex]

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(train, trainoutcome, 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

summary(objModel)
print(objModel)

predictions <- predict(object = objModel, test, type='raw')

print(postResample(pred=predictions, obs=testoutcome))




#tryglm
sum(test$activity_category == 'type 1')

train.type1 <- train[train$activity_category == 'type 1',]
trainoutcome.type1 <- trainoutcome[train$activity_category == 'type 1']

test.type1 <- test[test$activity_category == 'type 1',]
testoutcome.type1 <- testoutcome[test$activity_category == 'type 1']

objControl2 <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel2.type1 <- train(train.type1, trainoutcome.type1, method='glmnet',  metric = "ROC", trControl=objControl2)
predictions2 <- predict(object=objModel2, test)
auc <- roc(testoutcome, predictions2)
print(auc$auc)


