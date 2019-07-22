### clean_data_JD.R
# Author: Joe Delle Donne
# Date: 07/18/2019
# Experiment with cleaning the data

# tensorflow fix:
# https://github.com/rstudio/tensorflow/blob/master/README.md

# Source code for keras model:
# https://heartbeat.fritz.ai/binary-classification-using-keras-in-r-ef3d42202aaa

# Converting char variables to numeric:
# d
# country          x         y
# 1     foo 0.84435112 0.7022875
# 2     bar 0.01343424 0.5019794
# 3     baz 0.09815888 0.5832612
# 4     qux 0.18397525 0.8049514
# > d$country = as.numeric(as.factor(d$country))
# > d
# country          x         y
# 1       3 0.84435112 0.7022875
# 2       1 0.01343424 0.5019794
# 3       2 0.09815888 0.5832612
# 4       4 0.18397525 0.8049514

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
if(!require("keras")){
  install.packages("keras")
}
if(!require("fastDummies")){
  install.packages("fastDummies")
}
if(!require("tensorflow")){
  install.packages("tensorflow")
}
library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)
library(randomForest)
library(keras)
library(fastDummies)
library(tensorflow)
library(ISLR)
library(tree)

## Load data
df <- read.csv("data/raw/teaching_training_data.csv")
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")

## Joining Data
# Helper function to make uid's distinct
helper_function <- function(file_name) {
  file_name %>% 
    select(2:3) %>% 
    distinct(unid, .keep_all = TRUE)
}
# Modify data with helper function
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

## Modify data to have selected columns
df$working <- as.numeric(df$working)
keep <- c("working","cft_score","com_score","grit_score","num_score","opt_score")
df_final <- df[keep]

###############################################################################
### Attempted decision tree model
df_final <- df_final %>% na.omit()
working.tree = tree(data=df_final)


###############################################################################
### Attempted Keras model
## Prepare data for classification model
# Generate trainign and test sets
index <- createDataPartition(df_final$working, p=0.7, list=FALSE)
final.training <- df_final[index,]
final.test <- df_final[-index,]
# Prepare training
X_train <- final.training %>% 
  select(-working) %>% 
  scale()
y_train <- to_categorical(final.training$working)
# Prepare test
X_test <- final.test %>% 
  select(-working) %>% 
  scale()
y_test <- to_categorical(final.test$working)

## Create model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')
history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)
# Fit model to data [ THIS TAKES A VERY LONG TIME ]
model %>% fit(
  X_train, y_train, 
  epochs = 5,      # 100 takes a LONG time
  batch_size = 5,
  validation_split = 0.3
)

## Evaluate model
model %>% evaluate(X_test, y_test)

## Generate predictions
predictions <- model %>% predict_classes(X_test)
# print confusion matrix
table(factor(predictions, levels=min(final.test$working):max(final.test$working)),factor(final.test$working, levels=min(final.test$working):max(final.test$working)))


#########################################################################################

## Filter data
# df_filtered <- df %>% filter(!is.na(dob)) %>%
#   filter(!is.na(survey_date_month)) %>%
#   filter(!is.na(working))
# # add age and age_at_survey
# df <- df_filtered %>%
#   mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
#   mutate(age = floor(age_at_survey))
# # create separate dataframes for males and females
# df_male <- filter(df,gender=="Male")
# df_female <- filter(df,gender=="Female")
# # create dataframes for provinces
# df_filtered_prov <- df %>% filter(!is.na(province))
# df_eastern_cape <- df_filtered_prov %>% filter(province=="Eastern Cape")
# df_free_state <- df_filtered_prov %>% filter(province=="Free state")
# df_gauteng <- df_filtered_prov %>% filter(province=="Gauteng")
# df_kwazulu_natal <- df_filtered_prov %>% filter(province=="KwaZulu-Natal")
# df_limpopo <- df_filtered_prov %>% filter(province=="Limpopo")
# df_mpumalanga <- df_filtered_prov %>% filter(province=="Mpumalanga")
# df_north_west <- df_filtered_prov %>% filter(province=="North West")
# df_northern_cape <- df_filtered_prov %>% filter(province=="Northern Cape")
# df_western_cape <- df_filtered_prov %>% filter(province=="Western Cape")

####################################################################################################
### Male Analysis

# Select groups to analyze
df_male <- df_male %>% select(working,numchildren,peoplelive,anygrant,anyhhincome,age) 
# filter out NAs
df_male <- df_male %>% filter(!is.na(numchildren)) %>% 
  filter(!is.na(working)) %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(peoplelive)) %>% 
  filter(!is.na(anygrant)) %>% 
  filter(!is.na(anyhhincome))
# convert variables
df_male$peoplelive <- as.numeric(df_male$peoplelive)
df_male$numchildren <- as.numeric(df_male$numchildren)
df_male$working <- as.factor(df_male$working)
df_male$anygrant <- as.factor(df_male$anygrant)
df_male$anyhhincome <- as.factor(df_male$anyhhincome)

## Splitting Male Data
set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df_male$working, p=0.8, list=FALSE)
# Step 2: Create the training  dataset
trainData <- df_male[trainRowNumbers,]
# Step 3: Create the test dataset
testData <- df_male[-trainRowNumbers,]

## Generate model
model_logreg <- train(working ~ ., data=trainData, method='rpart')
# predict for our test data
predicted <- predict(model_logreg, testData[,2:length(testData)])
model_rpart

