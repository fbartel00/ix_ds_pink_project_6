### model_lm.R
# Author: Joe Delle Donne
# Date: 7/22/2019
# Attempted model using linear regression

## Install necessary packages
library(tidyverse)
library(pastecs)
library(ISLR)
library(lubridate)
library(caret)
library(RANN)

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
  mutate(age = floor(age_at_survey))
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

###  Impute missing values for specific variables
# peoplelive mean imputation (originally a factor)
peoplelive_numeric <- as.numeric(as.character(df$peoplelive))
peoplelive_mean <- mean(peoplelive_numeric,na.rm=TRUE)
df$peoplelive <- peoplelive_numeric
df <- df %>% mutate(peoplelive = ifelse(is.na(peoplelive),peoplelive_mean,peoplelive))
# numchildren mean imputation (originally a factor)
numchildren_numeric <- as.numeric(as.character(df$numchildren))
numchildren_mean <- mean(numchildren_numeric,na.rm=TRUE)
df$numchildren <- numchildren_numeric
df <- df %>% mutate(numchildren = ifelse(is.na(numchildren),numchildren_mean,numchildren))
# numearnincome mean imputation (originally a factor)
numearnincome_numeric <- as.numeric(as.character(df$numearnincome))
numearnincome_mean <- mean(numearnincome_numeric,na.rm=TRUE)
df$numearnincome <- numearnincome_numeric
df <- df %>% mutate(numearnincome = ifelse(is.na(numearnincome),numearnincome_mean,numearnincome))
# age mean imputation
age_mean <- mean(df$age,na.rm=TRUE)
df <- df %>% mutate(age = ifelse(is.na(age),age_mean,age))
# cft score mean imputation
cft_mean <- mean(df$cft_score,na.rm=TRUE)
df <- df %>% mutate(cft_score = ifelse(is.na(cft_score),cft_mean,cft_score))
# com score mean imputation
com_mean <- mean(df$com_score,na.rm=TRUE)
df <- df %>% mutate(com_score = ifelse(is.na(com_score),com_mean,com_score))
# cft score mean imputation
grit_mean <- mean(df$grit_score,na.rm=TRUE)
df <- df %>% mutate(grit_score = ifelse(is.na(grit_score),grit_mean,grit_score))
# num score mean imputation
num_mean <- mean(df$num_score,na.rm=TRUE)
df <- df %>% mutate(num_score = ifelse(is.na(num_score),num_mean,num_score))
# opt score mean imputation
opt_mean <- mean(df$opt_score,na.rm=TRUE)
df <- df %>% mutate(opt_score = ifelse(is.na(opt_score),opt_mean,opt_score))
# Split data by gender
df_male <- filter(df,gender=="Male")
df_female <- filter(df,gender=="Female")

### Create training and test data sets for total, male, and female
set.seed(1234)
# Total
df_train_index <- df %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)
df_train <- left_join(df_train_index, df)
df_test <- anti_join(df, df_train_index)
# Males
df_train_index_male <- df_male %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)
df_train_male <- left_join(df_train_index_male, df_male)
df_test_male <- anti_join(df_male, df_train_index_male)
# Females
df_train_index_female <- df_female %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)
df_train_female <- left_join(df_train_index_female, df_female)
df_test_female <- anti_join(df_female, df_train_index_female)

### Create the models
# Total
#reg_tot <- glm(working ~ gender + age + numchildren + peoplelive, data = df_train)
reg_tot <- glm(working ~ peoplelive + numearnincome + numchildren + com_score + opt_score + anygrant, data = df_train)
summary(reg_tot)
# Male
reg_male <- lm(working ~ peoplelive + age + numchildren, data = df_train_male)
summary(reg_male)
# Female
reg_female <- lm(working ~ peoplelive + age + numchildren, data = df_train_female)
summary(reg_female)

### Create predictions
# Total
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
# Male
df_pred_male <- as.data.frame(predict.lm(reg_male, df_test_male)) %>% 
  rename(pred_male = "predict.lm(reg_male, df_test_male)")
# bind together test data and predictions
df_pred_male <- bind_cols(df_test_male, df_pred_male)
# now manually classify
stat.desc(df_pred_male$pred_male)
quantile(df_pred_male$pred_male, na.rm = TRUE)
# plots of prediction
ggplot(df_pred_male) + 
  geom_density(mapping = aes(x = pred_male))
# Set prediction into dataframe
df_pred_male <- df_pred_male %>% 
  mutate(binary_pred_male = case_when(pred_male >= 0.286 ~ TRUE, 
                                     pred_male < 0.286 ~ FALSE))
# Female
df_pred_female <- as.data.frame(predict.lm(reg_female, df_test_female)) %>% 
  rename(pred_female = "predict.lm(reg_female, df_test_female)")
# bind together test data and predictions
df_pred_female <- bind_cols(df_test_female, df_pred_female)
# now manually classify
stat.desc(df_pred_female$pred_female)
quantile(df_pred_female$pred_female, na.rm = TRUE)
# plots of prediction
ggplot(df_pred_female) + 
  geom_density(mapping = aes(x = pred_female))
# Set prediction into dataframe
df_pred_female <- df_pred_female %>% 
  mutate(binary_pred_female = case_when(pred_female >= 0.211 ~ TRUE, 
                                      pred_female < 0.211 ~ FALSE))

### Observe results
## Total
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
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred_tot), stat = 'identity')
# proportions
confusion_matrix_tot <- confusion_matrix_tot %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)
## Male
table(df_pred_male$binary_pred_male, df_pred_male$working)
# Confusion matrix 
confusion_matrix_male <- df_pred_male %>% 
  filter(!is.na(binary_pred_male)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred_male) %>% 
  summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()
# Plot of the confusion matrix
ggplot(confusion_matrix_male) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred_male), stat = 'identity')
# proportions
confusion_matrix_male <- confusion_matrix_male %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)
## Female
table(df_pred_female$binary_pred_female, df_pred_female$working)
# Confusion matrix 
confusion_matrix_female <- df_pred_female %>% 
  filter(!is.na(binary_pred_female)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred_female) %>% 
  summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()
# Plot of the confusion matrix
ggplot(confusion_matrix_female) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred_female), stat = 'identity')
# proportions
confusion_matrix_female <- confusion_matrix_female %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)
