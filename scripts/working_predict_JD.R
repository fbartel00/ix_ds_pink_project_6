## working_predict_JD.R
# Author: Joe Delle Donne
# Date: 07/09/2019

# =====================================================================================================================
#   Using Caret to analyze working data
# =====================================================================================================================

## Install and library necessary packages
if(!require("caret")){
  install.packages("caret")
}
if(!require("skimr")){
  install.packages("skimr")
}
if(!require("RANN")){
  install.packages("RANN")
}
library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)

################################################################################
#### Setting up our data ####
################################################################################

# Load data and view
df <- read.csv("data/raw/teaching_training_data.csv")
#View(df)

summary(df) #Get a quick view of each data
sapply(df, class) #Get class info
#plot(df) #quick view of our data

# Add in age, change financial sit to ints
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

# Select groups to analyze
df <- df %>% select(working,gender,numchildren,age_at_survey) 
# filter out NAs
df <- df %>% filter(!is.na(numchildren)) %>% 
  filter(!is.na(working)) %>% 
  filter(!is.na(age_at_survey)) %>% 
  filter(!is.na(gender))
# convert numchildren to numeric
df$numchildren <- as.numeric(df$numchildren)
df$working <- as.factor(df$working)

################################################################################
#### Data Pre Processing  ####
################################################################################

# Scaling with Caret

preProcess_scale_model <- preProcess(df[c('age_at_survey', 'numchildren')],
                                     method=c('center', 'scale'))
df[c('age_at_survey', 'numchildren')] <- predict(preProcess_scale_model, newdata = df[c('age_at_survey', 'numchildren')])

################################################################################
#### Splitting Data ####
################################################################################

set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$working, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df[-trainRowNumbers,]

################################################################################
#### Training a model ####
################################################################################

# Caret model types
# modelnames <- paste(names(getModelInfo()), collapse=',  ')
# modelnames

# Choose model type
# modelLookup('rpart')

model_rpart <- train(working ~ ., data=trainData, method='rpart')

# predict for our test data
predicted <- predict(model_rpart, testData[,2:length(testData)])

model_rpart

################################################################################
#### Validation Techniques ####
################################################################################

trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
# this function stipulates:
#     - the method of training: Cross validation (cv) 
#     - Number of folds: 10
#     - If our process is going to be chatty: TRUE

model_rpart <- train(working ~ ., data=trainData, method='rpart', trControl = trControl)
# What did verboseIter actually do?

# Let's check on our hyperparameters, how are we evaluating success?
model_rpart$results

# What about if we want a different metric
model_rpart_kappa <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, metric = 'Kappa')

# What if we don't like the defaults for the hyper parameters we're testing?
model_rpart <- train(working ~ ., data=trainData, method='rpart', trControl = trControl, 
                     tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))

# Let's check on our hyperparameters again
model_rpart$results

#######################
#######################
#######################

################################################################################
#### Setting up our data ####
################################################################################

# Load data and view
df <- read.csv("data/raw/teaching_training_data.csv")
#View(df)

summary(df) #Get a quick view of each data
sapply(df, class) #Get class info
#plot(df) #quick view of our data

# Add in age, change financial sit to ints
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

# Select groups to analyze
df <- df %>% select(working,province,numchildren,age_at_survey) 
# filter out NAs
df <- df %>% filter(!is.na(numchildren)) %>% 
  filter(!is.na(working)) %>% 
  filter(!is.na(age_at_survey)) %>% 
  filter(!is.na(province))
# convert numchildren to numeric
df$numchildren <- as.numeric(df$numchildren)
df$working <- as.factor(df$working)

################################################################################
#### Data Pre Processing  ####
################################################################################

# Scaling with Caret

preProcess_scale_model <- preProcess(df[c('age_at_survey', 'numchildren')],
                                     method=c('center', 'scale'))
df[c('age_at_survey', 'numchildren')] <- predict(preProcess_scale_model, newdata = df[c('age_at_survey', 'numchildren')])

################################################################################
#### Splitting Data ####
################################################################################

set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$working, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df[-trainRowNumbers,]

################################################################################
#### Training a model ####
################################################################################

# Caret model types
# modelnames <- paste(names(getModelInfo()), collapse=',  ')
# modelnames

# Choose model type
# modelLookup('rpart')

model_rpart <- train(working ~ ., data=trainData, method='rpart')

# predict for our test data
predicted <- predict(model_rpart, testData[,2:length(testData)])

model_rpart

################################################################################
#### Validation Techniques ####
################################################################################

trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
# this function stipulates:
#     - the method of training: Cross validation (cv) 
#     - Number of folds: 10
#     - If our process is going to be chatty: TRUE

model_rpart <- train(working ~ ., data=trainData, method='rpart', trControl = trControl)
# What did verboseIter actually do?

# Let's check on our hyperparameters, how are we evaluating success?
model_rpart$results

# What about if we want a different metric
model_rpart_kappa <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, metric = 'Kappa')

# What if we don't like the defaults for the hyper parameters we're testing?
model_rpart <- train(working ~ ., data=trainData, method='rpart', trControl = trControl, 
                     tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))

# Let's check on our hyperparameters again
model_rpart$results

