setwd('/Users/emilyziyixiao/DataspellProjects/Framework&MethodsHW/')

birthdays = read.csv('president_birthdays.csv')
heights = read.csv('president_heights.csv')
states = read.csv('president_states.csv')

as.Date(birthdays$birthday,format='%m / %d / %Y')

library(readr)
# parse_character(birthdays['Name'])

library(stringr)
# str_trim(birthdays['Name'])
# as.character(birthdays['Name'])
library(ggplot2)

#sort(birthdays$birthday)
#table(birthdays$birthday)

heightsInInches <- parse_number(heights$height)
mean(heightsInInches)

heightsInCm <- heightsInInches * 2.54
median(heightsInCm)

sd(heightsInInches)
(70.8-64)/2.760105

height_cat4 <- cut(heightsInInches, breaks = c(0, 66, 69, 72, 100), labels = c('Short', 'Average', 'Tall', 'Very Tall'))
table(height_cat4)

library(dplyr)
library(forcats)
heights %>%
  mutate(height_cat = fct_recode(.f = height_cat4, "Average" = "Short", "Average" = "Average","Tall" = "Tall","Very Tall" = "Very Tall"))

states_hights <- merge(states, heights, by="Name")
#toString(states_hights["Birth.State"])
#replace(" ", states_hights["Birth.State"], "")
#str_replace(states_hights["Birth.State"], " ", "")
#NewJersyPresidents <- states_hights[states_hights$states_hights$Birth.State =="New Jersey"), "height"]

model <- paste('model',1:10,sep = '')
r2 <- c(0.849,0.782,0.853,0.853,0.856,0.855,0.859,0.856,0.859,0.859)
cp <- c(3785.13,29492.891,2216.066,2515.617,1122.912,1729.176,11.453,1108.412,5.883,11.752)
rss <- c(129345695398,186953511457,125825141230,126496397331,123371039554,124729600876,120875920936,123334065616,120858956753,120872109331)
results <- data.frame(model, r2, cp, rss)

results %>%  pivot_wider(names_from = model,values_from = r2:rss)
