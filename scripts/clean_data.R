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
df <- df %>% 
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
  select(-fin_situ_change)
# Convert various columns
df <- df %>% mutate(numchildren = parse_number(as.character(numchildren)))
df <- df %>% mutate(numearnincome = parse_number(as.character(numearnincome)))
df <- df %>% mutate(peoplelive = parse_number(as.character(peoplelive)))
df <- df %>% mutate(peoplelive_15plus = parse_number(as.character(peoplelive_15plus)))
df <- df %>% mutate(leadershiprole = as.character(leadershiprole)) %>% 
  mutate(leadershiprole = ifelse(leadershiprole=="Yes",1,0)) %>% 
  mutate(leadershiprole = as.logical(leadershiprole))
df <- df %>% mutate(volunteer = as.character(volunteer)) %>% 
  mutate(volunteer = ifelse(volunteer=="Yes",1,0)) %>% 
  mutate(volunteer = as.logical(volunteer))
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
# Join data
df_init_joined <- left_join(df_cft,df_com,by="unid")
df_scores_joined <- df_init_joined %>% 
  left_join(df_grit,by="unid") %>%
  left_join(df_num,by="unid") %>%
  left_join(df_opt,by="unid")
rm(df_cft,df_com,df_num,df_grit,df_opt)
df <- left_join(df,df_scores_joined,by="unid")

# ## Split into 50/50 working and not working
# df_true <- df %>% filter(working=="TRUE")
# df_false <- df %>% filter(working=="FALSE")
# df_sample_true = sample_n(df_true,15000,working="TRUE")
# df_sample_false = sample_n(df_false,15000,working="FALSE")
# df <- rbind(df_sample_true,df_sample_false)

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
  number = 3,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)
#Defining the predictors and outcome
predictors<-c("age","cft_score","com_score","num_score","grit_score","opt_score","peoplelive","peoplelive_15plus","numchildren","numearnincome")
outcomeName<-'working'

## Training the model
model_lr<-train(trainSet[,predictors],trainSet[,outcomeName],
                method='ranger',
                trControl=fitControl,
                tuneLength=3,
                metric="Accuracy")
#Predicting using the model
testSet$pred_lr<-predict(object = model_lr,testSet[,predictors])
#Checking the accuracy of the model
confusionMatrix(testSet$working,testSet$pred_lr)
















# ## impute province
# df_province <- df %>% select("province")
# val <- unique(df_province[!is.na(df_province)])
# mode <- val[which.max(tabulate(match(df_province, val)))]
# vec_imp[is.na(vec_imp)] <- mode

# ## Manipulate dummy variables
# # remove useless columns
# df_working <- df$working
# 
# dummies_model <- dummyVars(working ~ . , data=df)
# df_working <- df %>% select("unid","working")
# df_mat <- predict(dummies_model, newdata = df)
# # Convert to dataframe
# df_dummies <- data.frame(df_mat) #ignore the warning
# df_dummies <- df_dummies %>% distinct(unid, .keep_all = TRUE)
# df_new <- left_join(df_dummies,df_working,by="unid")






## Split into 50/50 working and not working
# df_true <- df %>% filter(working=="TRUE")
# df_false <- df %>% filter(working=="FALSE")
# df_sample_true = sample_n(df_true,15000,working="TRUE")
# df_sample_false = sample_n(df_false,15000,working="FALSE")
# df <- rbind(df_sample_true,df_sample_false)

