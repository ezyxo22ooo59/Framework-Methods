setwd('/Users/emilyziyixiao/DataspellProjects/Framework&MethodsHW/')

wages <- read.csv('assignment7_wages.csv',stringsAsFactors = TRUE)

wages <- wages[wages$earn>0,]

summary(wages)

black_earn <- wages[wages$race == 'black', "earn"]
white_earn <- wages[wages$race == 'white', "earn"]
hispanic_earn <- wages[wages$race == 'hispanic', "earn"]
other_earn <- wages[wages$race == 'other', "earn"]

mean(black_earn)
mean(white_earn)
mean(hispanic_earn)
mean(other_earn)

set.seed(1731)
split <- sample(1:nrow(wages),0.75*nrow(wages))
train <- wages[split,]
test <- wages[-split,]

mean(train$earn)
mean(train$height)

model1 <- lm(earn~height+sex+race+ed+age,data=train)
summary(model1)
pred <- predict(model1)
rmse1 <- sqrt(mean((pred-train$earn)^2)); rmse1