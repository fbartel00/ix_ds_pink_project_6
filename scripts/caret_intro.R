# =====================================================================================================================
#   Intro to Caret                                                                                              
#   Connor Lawless                                                                                                                 
#   iXperience 2019
# =====================================================================================================================

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

#Today we're going to go over caret, a function that is the gateway to a number of 
#different machine learning models packaged together with consistent syntax and a
#number of super helpful support functions.

################################################################################
#### Setting up our data ####
################################################################################


#For this workshop we're going to use a heart disease dataset, like any good data scientist
#start by taking a look at the data and preprocess it a bit

heart <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
                  header = FALSE)

View(heart) # What looks off?

# Choose a subset of columns.
heart <- heart[, c(1:5, 14)]

# Rename columns.
#
# age      - age in years
# sex      - gender (1 = male; 0 = female) 
# cp       - chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptomatic)
# trestbps - resting blood pressure [mm Hg]
# chol     - serum cholestorol
# num      - diagnosis of heart disease (0 = no disease)
#
names(heart) <- c("age", "sex", "cp", "trestbps", "chol", "num")


summary(heart) #Get a quick view of each data
sapply(heart, class) #Get class info
plot(heart) #quick view of our data

#What needs fixing? Fix it!

# Makes factor columns more useful

#Extension - Check out descriptive statistics with Skimkr! 

################################################################################
#### Data Pre Processing  ####
################################################################################

#Caret has a lot of quick built in functions to get your data ready for machine learning

# First question is how do we convert our categorical columns?
# What method have we already learned for this?

dummies_model <- dummyVars(num ~ . , data=heart)

# Create the dummy variables using predict. The Y variable (num) will not be present in heart_mat.
heart_num <- heart$num
heart_mat <- predict(dummies_model, newdata = heart)

# Convert to dataframe
heart <- data.frame(heart_mat) #ignore the warning

# See the structure of the new dataset 


# Scaling with Caret

# We can chain together a number of operations with the pre process function
# Checking out the rest of the pre process functions is left as a fun exercise for google :)
preProcess_scale_model <- preProcess(heart[c('age', 'trestbps','chol')], method=c('center', 'scale'))
heart[c('age', 'trestbps','chol')] <- predict(preProcess_scale_model, newdata = heart[c('age', 'trestbps','chol')])

# Append num back in
heart$num <- heart_num

# Let's take a peak at the result of all our processing work
skim_to_wide(heart)[, c(1:5, 9:11, 13, 15:16)] 

################################################################################
#### Splitting Data ####
################################################################################

# Like any good data scientist, we're going to split our data into training
# and testing sets. Why?
set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(heart$num, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- heart[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- heart[-trainRowNumbers,]

# Easy right! One issue with our process so far - what is it?

################################################################################
#### Training a model ####
################################################################################

# Where caret shines is as a way to train machine learning models in a consistent interface

# Caret has a lot of different models, check 'em out!
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames

# You can lookup some information on models using model lookup (but google is king)
modelLookup('rpart')

# Let's train a simple model (very similar to other algos we've looked at)

model_rpart <- train(num ~ ., data=trainData, method='rpart')
# Error check - did it say we need a package? What should we do?

# Now let's predict for our test data
predicted <- predict(model_rpart, testData[,-length(testData)])

# Simple right? So why even bother with Caret and no just use the individual packages
# A few reasons (come to mind):
#     - Cross validation! 
#     - Hyperparameter Selection
#     - Model Selection

# Caret was actually doing hyperparameter selection without us asking!
model_rpart

# What values did it test, and what did it pick?

################################################################################
#### Validation Techniques ####
################################################################################

#### Cross Validation
# CV is a validation technique where we retrain our model on different splits of our 
# data to get an 'average performance' 
# For more information on cross validation: https://towardsdatascience.com/cross-validation-70289113a072

# To control validation techniques during training we can use the train control function

trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
# this function stipulats:
#     - the method of training: Cross validation (cv) 
#     - Number of folds: 10
#     - I our process is going to be chatty: TRUE

model_rpart <- train(num ~ ., data=trainData, method='rpart', trControl = trControl)
# What did verboseIter actually do?

# Let's check on our hyperparameters, how are we evaluating success?
model_rpart$results

# What about if we want a different metric
model_rpart_kappa <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, metric = 'Kappa')

# What if we don't like the defaults for the hyper parameters we're testing?
model_rpart <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, 
                     tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))

# Let's check on our hyperparameters again
model_rpart$results

# Comparing multiple models 

# Train a new classification model of your choice
new_model <- 
  
  #Compare the two models using resamples
  model_comp <- resamples(list(my_new_model = new_model, Rpart = model_rpart))
summary(model_comp)

################################################################################
#### Exercises and Extensions ####
################################################################################

# 1. Implement a function to do k-fold cross validation yourself - test it out by training a 
# linear regression model on the mtcars dataset trying to predict mpg from the other variables

# 2. Research three classification algorithms that are included in caret and see which is better
# for predicting species in the iris data set. Make sure to test out different metrics and
# cross validation techinques

# 3. Dealing with missing values (aka imputation) is a tricky task in machine learning. For this exercise we're going
# to try to solve it on our own simply, and using caret's built in imputation functionality

# Helper code to artificially create data with missing values from the iris data set (run this first!)
iris_missVals <- iris
iris_missVals$Petal.Length[sample(1:nrow(iris_missVals), 50)] <- NA

# Attempt 1: Try to replace missing values with the mean value of that column

# Attempt 2 (Fancy Caret Attempt): Impute the data using PreProcess and KNN Impute
# Google around to see what KNN is (here's a great starting point: https://towardsdatascience.com/a-simple-introduction-to-k-nearest-neighbors-algorithm-b3519ed98e)