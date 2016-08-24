library(data.table)
library(dplyr)
library(dtplyr)

people <- fread('people.csv', header=TRUE, stringsAsFactors = TRUE)
people$date <- as.Date(as.character(people$date))

act_train <- fread('act_train.csv', header=TRUE, stringsAsFactors = TRUE)
act_test <- fread('act_test.csv', header=TRUE, stringsAsFactors = TRUE)
act_train$date <- as.Date(as.character(act_train$date))
act_test$date <- as.Date(as.character(act_test$date))

comb <- inner_join(people, act_train, by='people_id')
comb$datediff <- as.numeric(as.Date(comb$date.y) - as.Date(comb$date.x))
comb$outcome <- factor(comb$outcome, levels=c(0,1) , labels=c('neg', 'pos'))

#outcome <- as.numeric(comb$outcome)
#outcome <- comb$outcome

comb <- data.frame(comb)
sapply(comb, function(x) if (class(x) == 'factor') length(levels(x)) else 0)

colnames(comb)

#get rid of people_id, date.x, activity_id, date.y and outcome
#comb <- comb[,c(-1, -5, -42, -43, -55)]
#comb <- comb[,-55]

#comb <- Reduce(cbind, lapply(comb, function(x) if(class(x) == 'logical') as.numeric(x) else x))
for (i in 1:ncol(comb)) {
  if (class(comb[,i]) == 'logical') {
    comb[,i] <- factor(comb[,i])
  }
}

for (i in 1:ncol(comb)) {
  if (class(comb[,i]) == 'factor') {
    if(sum(levels(comb[,i])=='') > 0) {
      levels(comb[,i])[which(levels(comb[,i])=='')]='missing'
    }
  }
}

#do the people from train overlap with test?
trainpeople <- unique(act_train$people_id)
str(trainpeople)
testpeople <- unique(act_test$people_id)
str(testpeople)
sum(testpeople %in% trainpeople)