install.packages('ISLR')
install.packages('caret')
install.packages('caTools')

library(ISLR)
#library(caret)
library(caTools)

#data(Sacramento)

set.seed(1706)
#split = sample(x = 1:nrow(diamonds),size = 0.8*nrow(diamonds))

#split = createDataPartition(y = diamonds$price, p = 0.8, list = F, groups = 50)

split = sample.split(Y = diamonds$MM, SplitRatio = 0.8)


train = diamonds[split,]
test = diamonds[-split,]

prop.table(rbind(train = table(train$MM), test = table(test$MM)), margin = 1)

#mean(train$price)
#mean(test$price)