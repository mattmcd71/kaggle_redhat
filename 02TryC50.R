# try C5.0

library(caret)
library(pROC)
library(e1071)

set.seed(1234)

flaggedpeople <- data.frame(act_train[!duplicated(act_train$people_id),])[,c(1,15)]
head(flaggedpeople)

splitIndex <- createDataPartition(flaggedpeople$outcome, p=.80, list=FALSE, times=1)

combplus <-  merge(
  comb,
  allCompaniesAndDays,
  all.x = T,all.y = F,
  by.x = c("group_1","date.y"),
  by.y = c("group_1","date.p")
)
#this is a problem...we can't use this new column filled in our moded bc it's perfectly correlated with the outcome :(
table(combplus$filled, combplus$outcome)

train2 <- combplus[(combplus$people_id %in% flaggedpeople[splitIndex, 1]),]
test2 <- combplus[(combplus$people_id %in% flaggedpeople[-splitIndex, 1]),]

#don't include char_10.y, because the test dataset has different levels for that one
train2 <- data.frame(train2[,c(-1, -2, -3, -6, -43, -54, -56)])

trainoutcome <- outcome[(comb$people_id %in% flaggedpeople[splitIndex, 1])]
testoutcome <- outcome[(comb$people_id %in% flaggedpeople[-splitIndex, 1])]

library(C50)
#x <- C5.0(comb, outcome)

# objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
# 
# c50_mod1 <- objModel <- train(train2, trainoutcome, 
#                               method='C5.0', 
#                               trControl=objControl,  
#                               metric = "ROC",
#                               preProc = c("center", "scale"))
# 


c50_mod1 <- C5.0(train2, trainoutcome, trials=10, control=C5.0Control(winnow = TRUE, minCases = 10))
pred_c50_test <- predict.C5.0(c50_mod1, test2)

write(capture.output(summary(c50_mod1)), "c50model.txt")

auc <- roc(as.numeric(pred_c50_test), as.numeric(testoutcome))
print(auc$auc)
