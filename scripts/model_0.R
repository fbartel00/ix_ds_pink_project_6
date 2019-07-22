### model_0.R
# Author: Joe Delle Donne
# Date: 7/22/2019

## Install necessary packages
library(tidyverse)
library(pastecs)
library(ISLR)

# turn off scientific notation
options(scipen=999)

### Reading Data
df <- read.csv("data/raw/teaching_training_data.csv")
# Create financial change variables
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)
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
reg_tot <- lm(working ~ gender + as.factor(fin_situ_now) + anyhhincome, data = df_train)
summary(reg_tot)
# Male
reg_male <- lm(working ~ as.factor(fin_situ_now) + anyhhincome, data = df_train_male)
summary(reg_male)
# Female
reg_female <- lm(working ~ as.factor(fin_situ_now) + anyhhincome, data = df_train_female)
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
  mutate(binary_pred_tot = case_when(pred_tot >= 0.3 ~ TRUE, 
                                  pred_tot < 0.3 ~ FALSE))
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
  mutate(binary_pred_male = case_when(pred_male >= 0.3 ~ TRUE, 
                                     pred_male < 0.3 ~ FALSE))
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
  mutate(binary_pred_female = case_when(pred_female >= 0.3 ~ TRUE, 
                                      pred_female < 0.3 ~ FALSE))

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
table(df_pred_tot$binary_pred_male, df_pred_male$working)
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

# Is this model good or bad?