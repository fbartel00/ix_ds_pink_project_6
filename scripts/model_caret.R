### model_caret.R
# Author: Joe Delle Donne
# Date: 7/22/2019
# Attempted model using caret and ensemble methods

#rpart is a decision tree

## Install necessary packages
library(tidyverse)
library(pastecs)
library(ISLR)
library(lubridate)
library(caret)
library(RANN)

set.seed(1)

# turn off scientific notation
options(scipen=999)

### Reading Data
df <- read.csv("data/raw/teaching_training_data.csv")
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")

### Modifying data
## Create financial change variables and age variables
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey)) %>% 
  filter(!is.na(gender))
## Joining Scores Data to main dataframe
# Helper function to make uid's distinct
helper_function <- function(file_name) {
  file_name %>% 
    select(2:3) %>% 
    distinct(unid, .keep_all = TRUE)
}
# Modify score data with helper function
df_cft <- helper_function(df_cft)
df_com <- helper_function(df_com)
df_grit <- helper_function(df_grit)
df_num <- helper_function(df_num)
df_opt <- helper_function(df_opt)
# Join data
df_init_joined <- left_join(df_cft,df_com,by="unid")
df_scores_joined <- df_init_joined %>% 
  left_join(df_grit,by="unid") %>%
  left_join(df_num,by="unid") %>%
  left_join(df_opt,by="unid")
df <- left_join(df,df_scores_joined,by="unid")



## Split into 50/50 working and not working
df_true <- df %>% filter(working=="TRUE")
df_false <- df %>% filter(working=="FALSE")
df_sample_true = sample_n(df_true,15000,working="TRUE")
df_sample_false = sample_n(df_false,15000,working="FALSE")
df <- rbind(df_sample_true,df_sample_false)

## Imputing missing values using median
df$working <- as.factor(make.names(df$working)) 
preProcValues <- preProcess(df, method = c("medianImpute","center","scale"))
df_processed <- predict(preProcValues, df)

## Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(df_processed$working, p=0.75, list=FALSE)
trainSet <- df_processed[ index,]
testSet <- df_processed[-index,]

## Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)
#Defining the predictors and outcome
predictors<-c("cft_score","com_score","grit_score","num_score","opt_score")
outcomeName<-'working'

# ## Training the random forest model
# model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
# #Predicting using random forest model
# testSet$pred_rf<-predict(object = model_rf,testSet[,predictors])
# #Checking the accuracy of the random forest model
# confusionMatrix(testSet$working,testSet$pred_rf)

## Training the Logistic regression model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],
                method='rpart',
                trControl=fitControl,
                tuneLength=3,
                metric="Accuracy")
#Predicting using knn model
testSet$pred_lr<-predict(object = model_lr,testSet[,predictors])
#Checking the accuracy of the random forest model
confusionMatrix(testSet$working,testSet$pred_lr)


