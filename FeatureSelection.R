setwd('/Users/emilyziyixiao/DataspellProjects/Framework&MethodsHW/')

data <- read.csv('houses.csv', stringsAsFactors = TRUE)

library(ggplot2)
library(caTools)
head(data)
library(caret)
set.seed(1031)
split <- createDataPartition(y = data$price, p = 0.7, list = F, groups = 100)
train <- data[split,]
test <- data[!split,]
trainMean <- mean(train$price)
testMean <- mean(test$price)

cor(train[,-12])

library(ggcorrplot)
ggcorrplot(cor(train[,c(3:7, 10:13,16)]),type = 'lower',show.diag = F,colors = c('red','white','darkgreen'))

model1 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)

library(car)
vif(model1)

library(leaps)
subsets = regsubsets(price~.,data=train, nvmax=10)
summary(subsets)

subsets_measures <- data.frame(model=1:length(summary(subsets)$cp),
                              cp=summary(subsets)$cp,
                              bic=summary(subsets)$bic,
                              adjr2=summary(subsets)$adjr2)
subsets_measures

start_mod <- lm(price~1, data=train)
empty_mod <- lm(price~1,data=train)
full_mod <- lm(price~.,data=train)
forwardStepwise <- step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
library('dplyr')
forwardStepwise$anova %>%
  mutate(step_number = as.integer(rownames(forwardStepwise$anova))-1) %>%
  mutate(Step = as.character(Step))%>%
  ggplot(aes(x = reorder(Step,X = step_number), y = AIC))+
  geom_point(color = 'darkgreen', size = 2) +
  scale_x_discrete(name = 'Variable Entering Model')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))

forwardStepwise

start_mod = lm(price~.,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~.,data=train)
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')

#summary(backwardStepwise)

start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~.,data=train)
hybridStepwise = step(start_mod,
                      scope=list(upper=full_mod,lower=empty_mod),
                      direction='both')


library(glmnet)
x = model.matrix(price~.-1,data=train)
y = train$price
set.seed(1031)
cv_lasso = cv.glmnet(x = x,
                     y = y,
                     alpha = 1,
                     type.measure = 'mse')

coef(cv_lasso, s = cv_lasso$lambda.1se) %>%
  round(4)

model2 <- lm(price~bathrooms+sqft_living+waterfront+view+grade+age, data = train)

summary(model2)

library(caret)
trainPredictors = train[,c(3:11,16)]
testPredictors = test[,c(3:11,16)]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
trainComponents = predict(x,newdata=trainPredictors)
trainComponents$price = train$price

model3 <- lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+age, data = train)

summary(model3)

testComponents <- predict(x,newdata=testPredictors)

sse <- sum((testComponents - test$price)^2)
sst <- sum((mean(test$price)-test$price)^2)
r2 <- 1 - sse/sst
r2

model <- lm(price~bedrooms+sqft_living+waterfront+view+grade+age, data = train)

summary(model)