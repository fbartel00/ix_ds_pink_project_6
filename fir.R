library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)

df <- read.csv('data/raw/teaching_training_data.csv')

df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey)) %>% 
  mutate(numchildren = as.numeric(numchildren)) %>%
  mutate(peoplelive = as.numeric(peoplelive)) %>%
  mutate(working = as.factor(working))



df <- df %>% 
  filter(!is.na(numchildren)) %>% 
  filter(!is.na(working)) %>% 
  filter(!is.na(age_at_survey)) %>% 
  filter(!is.na(gender)) %>%
  filter(!is.na(peoplelive)) %>%
  filter(!is.na(anyhhincome)) %>%
  filter(!is.na(volunteer)) %>%
  select(peoplelive, gender, numchildren, age_at_survey, working)

preProcess_missingdata_model <- preProcess(df, method='knnImpute') 
preProcess_missingdata_model 
df <- predict(preProcess_missingdata_model, newdata = df)

pre_process_scale_model <- preProcess(df[c('age_at_survey', 'numchildren', 'peoplelive')], method=c('center', 'scale'))
df[c('age_at_survey', 'numchildren', 'peoplelive')] <- predict(pre_process_scale_model, newdata = df[c('age_at_survey', 'numchildren', 'peoplelive')])

set.seed(100)

trainRowNumbers <- createDataPartition(df$working, p=0.8, list=FALSE)

trainData <- df[trainRowNumbers,]

testData <- df[-trainRowNumbers,]
trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

new_model <- train(working ~ ., data=trainData, method='ctree', trControl = trControl)
new_model$results

predicted <- predict(new_model, testData[,-length(testData)])








