### MODELS.R
# Group members:
#     Joe Delle Donne
#     Woojin Lim
#     Matthew Yoon
#     Filip Bartel

rm(list=ls())

## Install necessary packages
library(tidyverse)
library(pastecs)
library(ISLR)
library(lubridate)
library(caret)
library(RANN)

## Load the cleaend data
df1 <- read_csv('data/processed/cleaned_data_model_01.csv')
df2 <- read_csv('data/processed/cleaned_data_model_02.csv')
df1 <- df1 %>% select(-X1)
df2 <- df2 %>% select(-X1)

########################################################################################################

### Logistic Regression method

## Create training inndex
df_train_index <- df1 %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)
df_train <- left_join(df_train_index, df1)
df_test <- anti_join(df1, df_train_index)

## Create model
reg_tot <- glm(working ~ peoplelive + numchildren + numearnincome + gender + cft_score + com_score, data = df_train)
summary(reg_tot)

## Create predictions
df_pred_tot <- as.data.frame(predict.lm(reg_tot, df_test)) %>% 
  rename(pred_tot = "predict.lm(reg_tot, df_test)")
# bind together test data and predictions
df_pred_tot <- bind_cols(df_test, df_pred_tot)
# now manually classify
stat.desc(df_pred_tot$pred_tot)
quantile(df_pred_tot$pred_tot, na.rm = TRUE)
# plots of prediction
ggplot(df_pred_tot) + 
  geom_density(mapping = aes(x = pred_tot))
ggplot(df_pred_tot) + 
  geom_density(mapping = aes(x = pred_tot, colour = gender))
# Set prediction into dataframe
df_pred_tot <- df_pred_tot %>% 
  mutate(binary_pred_tot = case_when(pred_tot >= 0.25 ~ TRUE, 
                                     pred_tot < 0.25 ~ FALSE))

## Observe results
table(df_pred_tot$binary_pred_tot, df_pred_tot$working)
# Confusion matrix 
confusion_matrix_tot <- df_pred_tot %>% 
  filter(!is.na(binary_pred_tot)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred_tot) %>% 
  summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()
# Plot of the confusion matrix
ggplot(confusion_matrix_tot) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred_tot), stat = 'identity') +
  ggtitle("Logistic Regression Predictions")
# proportions
confusion_matrix_tot <- confusion_matrix_tot %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)

########################################################################################################

### Clustering model method with knn

## Defining predictors and outcome
predictor_df <- df2 %>% select(-working) %>% select(-unid)
predictors<-colnames(predictor_df)
outcomeName<-'working'

## Imputing missing values using median
df2$working <- as.factor(make.names(df2$working)) 
preProcValues <- preProcess(df2, method = c("medianImpute"))
df2_processed <- predict(preProcValues, df2)

## generate kmeans clusters
unid <- df2_processed %>% select(unid)
df2_cluster <- df2_processed %>% select(numchildren,peoplelive,numearnincome,peoplelive_15plus,givemoney_yes)
k4 <- kmeans(df2_cluster, centers = 4, nstart = 25)
k4_cluster <- as.data.frame(k4$cluster)
df2_processed$cluster <- k4_cluster
group1 <- df2_processed %>% filter(cluster==1) %>% select(-cluster)
group2 <- df2_processed %>% filter(cluster==2) %>% select(-cluster)
group3 <- df2_processed %>% filter(cluster==3) %>% select(-cluster)
group4 <- df2_processed %>% filter(cluster==4) %>% select(-cluster)

### group1 model
## Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(group1$working, p=0.75, list=FALSE)
trainSet <- group1[ index,]
testSet <- group1[-index,]
## Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)
## Generate weights
model_weights <- ifelse(trainSet$working == "FALSE.",
                        (1 / table(trainSet$working)[1])*0.5,
                        (1 / table(trainSet$working)[2])*0.5)
## Training the model
model<-train(trainSet[,predictors],trainSet$working,
                method='knn',
                trControl=fitControl,
                tuneLength=3,
                weights = model_weights,
                metric="Accuracy")
# Predicting using the model
testSet$pred<-predict(object = model,testSet[,predictors])
# Checking the accuracy of the model
confusionMatrix(testSet$pred,testSet$working)

### group2 model
## Spliting training set into two parts based on outcome: 75% and 25%
index2 <- createDataPartition(group2$working, p=0.75, list=FALSE)
trainSet2 <- group2[ index2,]
testSet2 <- group2[-index2,]
## Defining the training controls for multiple models
fitControl2 <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)
## Generate weights
model_weights2 <- ifelse(trainSet2$working == "FALSE.",
                        (1 / table(trainSet2$working)[1])*0.5,
                        (1 / table(trainSet2$working)[2])*0.5)
## Training the model
model2<-train(trainSet2[,predictors],trainSet2$working,
             method='knn',
             trControl=fitControl,
             tuneLength=3,
             weights = model_weights2,
             metric="Accuracy")
# Predicting using the model
testSet2$pred2<-predict(object = model2,testSet2[,predictors])
# Checking the accuracy of the model
confusionMatrix(testSet2$pred2,testSet2$working)

### group3 model
## Spliting training set into two parts based on outcome: 75% and 25%
index3 <- createDataPartition(group3$working, p=0.75, list=FALSE)
trainSet3 <- group3[ index3,]
testSet3 <- group3[-index3,]
## Defining the training controls for multiple models
fitControl3 <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)
## Generate weights
model_weights3 <- ifelse(trainSet3$working == "FALSE.",
                         (1 / table(trainSet3$working)[1])*0.5,
                         (1 / table(trainSet3$working)[2])*0.5)
## Training the model
model3<-train(trainSet3[,predictors],trainSet3$working,
              method='knn',
              trControl=fitControl,
              tuneLength=3,
              weights = model_weights3,
              metric="Accuracy")
# Predicting using the model
testSet3$pred3<-predict(object = model3,testSet3[,predictors])
# Checking the accuracy of the model
confusionMatrix(testSet3$pred3,testSet3$working)

### group4 model
## Spliting training set into two parts based on outcome: 75% and 25%
index4 <- createDataPartition(group4$working, p=0.75, list=FALSE)
trainSet4 <- group4[ index4,]
testSet4 <- group4[-index4,]
## Defining the training controls for multiple models
fitControl4 <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = 'final',
  classProbs = T,
  verboseIter = TRUE)
## Generate weights
model_weights4 <- ifelse(trainSet4$working == "FALSE.",
                         (1 / table(trainSet4$working)[1])*0.5,
                         (1 / table(trainSet4$working)[2])*0.5)
## Training the model
model4<-train(trainSet4[,predictors],trainSet4$working,
              method='knn',
              trControl=fitControl,
              tuneLength=3,
              weights = model_weights4,
              metric="Accuracy")
# Predicting using the model
testSet4$pred4<-predict(object = model4,testSet4[,predictors])
# Checking the accuracy of the model
confusionMatrix(testSet4$pred4,testSet4$working)


