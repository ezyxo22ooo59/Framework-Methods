setwd('/Users/emilyziyixiao/DataspellProjects/Framework&MethodsHW/')
library(ggplot2)
library(caTools)
data <- read.csv('eBayAssignment.csv', stringsAsFactors = TRUE)
#glimpse(data)

blackIpad <- data[data$color == 'Black', ]
#count(blackIpad)

highestID <- max(data$startprice)
print(highestID)


head(data)
set.seed(196)
split <- sample.split(Y = data$sold, SplitRatio = 0.8)
train <- data[split,]
test <- data[!split,]
#count(train)

soldIpad <- train[train$sold == 1, 'startprice']
median(soldIpad)

unSoldIpad <- train[train$sold == 0, 'startprice']
median(unSoldIpad)

model1 <- glm(sold~biddable+startprice+condition+cellular+carrier+color+storage+productline+noDescription+charCountDescription+upperCaseDescription+startprice_99end, family = 'binomial', data = train)
summary(model1)

model2 <- glm(sold~biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end, family = 'binomial', data = train)
summary(model2)

pred <- predict(model2,newdata=test,type='response')
ct <- table(sold = test$sold,
           predictions = as.integer(pred>0.5)); ct
accuracy <- sum(ct[1,1],ct[2,2])/nrow(test); accuracy

#Use model2 to generate predictions for 'sold' in the test set. What is the probability of sale for an iPad with UniqueID 10940?
#predict(model2,newdata=data.frame(UniqueID=10940),type='response')

library(ROCR)
ROCRpred <- prediction(pred,test$sold)
ROCRperf <- performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2), xlab="1 - Specificity",ylab="Sensitivity")
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure