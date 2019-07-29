### clean_data.R
# Author: Joe Delle Donne
# Date: 07/28/2019

rm(list=ls())

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
## Create financial change variables and age variables, filter out no gender
df <- df %>% filter(survey_num==1) %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey)) %>% 
  distinct(unid, .keep_all = TRUE) %>% 
  filter(!is.na(gender))
# get rid of useless columns
df <- df %>% 
  select(-survey_date_month) %>% 
  select(-survey_num) %>% 
  select(-age_at_survey) %>% 
  select(-dob) %>% 
  select(-job_start_date) %>% 
  select(-job_leave_date) %>% 
  select(-company_size) %>% 
  select(-monthly_pay) %>% 
  select(-financial_situation_now) %>% 
  select(-financial_situation_5years) %>% 
  select(-fin_situ_now) %>% 
  select(-fin_situ_future) %>% 
  select(-fin_situ_change) %>% 
  select(-province) %>% 
  select(-X)
# Convert various columns into numeric variables
df <- df %>% mutate(numchildren = parse_number(as.character(numchildren)))
df <- df %>% mutate(numearnincome = parse_number(as.character(numearnincome)))
df <- df %>% mutate(peoplelive = parse_number(as.character(peoplelive)))
df <- df %>% mutate(peoplelive_15plus = parse_number(as.character(peoplelive_15plus)))
df <- df %>% mutate(anygrant = as.numeric(anygrant))
df <- df %>% mutate(anyhhincome = as.numeric(anyhhincome))
df <- df %>% mutate(givemoney_yes = as.numeric(givemoney_yes))
df <- df %>% mutate(leadershiprole = as.character(leadershiprole)) %>% 
  mutate(leadershiprole = ifelse(leadershiprole=="Yes",1,0)) %>% 
  mutate(leadershiprole = as.numeric(as.logical(leadershiprole)))
df <- df %>% mutate(volunteer = as.character(volunteer)) %>% 
  mutate(volunteer = ifelse(volunteer=="Yes",1,0)) %>% 
  mutate(volunteer = as.numeric(as.logical(volunteer)))
# Make gender into numeric ( 0 - female, 1 - male )
df <- df %>% mutate(gender_male = ifelse(gender=="Male",1,0))
df <- df %>% select(-gender)

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
# Join data into main dataframe
df_scores_joined <- left_join(df_cft,df_com,by="unid") %>% 
  left_join(df_grit,by="unid") %>%
  left_join(df_num,by="unid") %>%
  left_join(df_opt,by="unid")
rm(df_cft,df_com,df_num,df_grit,df_opt)
df <- left_join(df,df_scores_joined,by="unid")

## Defining predictors and outcome
predictor_df <- df %>% select(-working) %>% select(-unid)
predictors<-colnames(predictor_df)
outcomeName<-'working'

## Imputing missing values using median
df$working <- as.factor(make.names(df$working)) 
preProcValues <- preProcess(df, method = c("medianImpute"))
df_processed <- predict(preProcValues, df)

## generate kmeans clusters
unid <- df_processed %>% select(unid)
df_cluster <- df_processed %>% select(numchildren,peoplelive,peoplelive_15plus,givemoney_yes)
k4 <- kmeans(df_cluster, centers = 4, nstart = 25)
k4_cluster <- as.data.frame(k4$cluster)
df_processed$cluster <- k4_cluster
group1 <- df_processed %>% filter(cluster==1) %>% select(-cluster)
group2 <- df_processed %>% filter(cluster==2) %>% select(-cluster)
group3 <- df_processed %>% filter(cluster==3) %>% select(-cluster)
group4 <- df_processed %>% filter(cluster==4) %>% select(-cluster)

## Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(group4$working, p=0.75, list=FALSE)
trainSet <- group4[ index,]
testSet <- group4[-index,]

## Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)

## Generate weights
model_weights <- ifelse(trainSet$working == "FALSE.",
                        (1 / table(trainSet$working)[1]),
                        (1 / table(trainSet$working)[2]))

## Training the model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],
                method='knn',
                trControl=fitControl,
                tuneLength=3,
                weights = model_weights,
                metric="Accuracy")
# Predicting using the model
testSet$pred_lr<-predict(object = model_lr,testSet[,predictors])
# Checking the accuracy of the model
confusionMatrix(testSet$pred_lr,testSet$working)



