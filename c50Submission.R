library(data.table)
library(dplyr)
library(dtplyr)

act_test <- fread('act_test.csv', header=TRUE, stringsAsFactors = TRUE)
act_test$date <- as.Date(as.character(act_test$date))

sub <- inner_join(people, act_test, by='people_id')
sub$datediff <- as.numeric(as.Date(sub$date.y) - as.Date(sub$date.x))

sub <- data.frame(sub)
sapply(sub, class)

for (i in 1:ncol(sub)) {
  if (class(sub[,i]) == 'logical') {
    sub[,i] <- factor(sub[,i])
  }
}

for (i in 1:ncol(sub)) {
  if (class(sub[,i]) == 'factor') {
    if(sum(levels(sub[,i])=='') > 0) {
      levels(sub[,i])[which(levels(sub[,i])=='')]='missing'
    }
  }
}

activityIDs <- sub$activity_id

#sub <- sub[,c(-1, -5, -42, -43)]

a <- sapply(comb, function(x) if(class(x)=='factor') length(levels(x)) else 0)
b <- sapply(sub, function(x) if(class(x)=='factor') length(levels(x)) else 0)
cbind(a,b)

#char_10y has 454 levels that are new in the new dataset...maybe we should get rid of this?
sapply(colnames(sub)[40:50], function(x) sum(!(levels(sub[,x]) %in% levels(comb[,x]))))

sub2 <-  predict.C5.0(c50_mod1, sub)
submission1 <- data.frame(activity_id = activityIDs, outcome=as.numeric(sub2)-1)
write.csv(submission1, 'submission1.csv', row.names = FALSE)
