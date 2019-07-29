### CLEAN_DATA.R
# Group members:
#     Joe Delle Donne
#     Woojin Lim
#     Matthew Yoon
#     Filip Bartel

## Clear environmental variables
rm(list=ls())

## Install necessary packages
library(tidyverse)
library(pastecs)
library(ISLR)
library(lubridate)
library(caret)
library(RANN)

## Set seed and turn off scientific notation
set.seed(1)
options(scipen=999)

## Reading in data
df <- read.csv("data/raw/teaching_training_data.csv")
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")

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
rm(df_cft,df_com,df_num,df_grit,df_opt) # remove individual score dataframes from environment
df <- left_join(df,df_scores_joined,by="unid")

## Modifying data
# Create financial change variables and age variables, filter out gender NA's
df <- df %>% filter(survey_num==1) %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey)) %>% 
  distinct(unid, .keep_all = TRUE) %>% 
  filter(!is.na(gender))

## Generate model_01 dataframe
# Impute missing values for specific variables
df2 <- df
# peoplelive mean imputation (originally a factor)
peoplelive_numeric <- parse_number(as.character(df2$peoplelive))
peoplelive_mean <- mean(peoplelive_numeric,na.rm=TRUE)
df2$peoplelive <- peoplelive_numeric
df2 <- df2 %>% mutate(peoplelive = ifelse(is.na(peoplelive),peoplelive_mean,peoplelive))
# numchildren mean imputation (originally a factor)
numchildren_numeric <- parse_number(as.character(df2$numchildren))
numchildren_mean <- mean(numchildren_numeric,na.rm=TRUE)
df2$numchildren <- numchildren_numeric
df2 <- df2 %>% mutate(numchildren = ifelse(is.na(numchildren),numchildren_mean,numchildren))
# numearnincome mean imputation (originally a factor)
numearnincome_numeric <- parse_number(as.character(df2$numearnincome))
numearnincome_mean <- mean(numearnincome_numeric,na.rm=TRUE)
df2$numearnincome <- numearnincome_numeric
df2 <- df2 %>% mutate(numearnincome = ifelse(is.na(numearnincome),numearnincome_mean,numearnincome))
# age mean imputation
age_mean <- mean(df$age,na.rm=TRUE)
df2 <- df2 %>% mutate(age = ifelse(is.na(age),age_mean,age))
# cft score mean imputation
cft_mean <- mean(df$cft_score,na.rm=TRUE)
df2 <- df2 %>% mutate(cft_score = ifelse(is.na(cft_score),cft_mean,cft_score))
# com score mean imputation
com_mean <- mean(df$com_score,na.rm=TRUE)
df2 <- df2 %>% mutate(com_score = ifelse(is.na(com_score),com_mean,com_score))
# cft score mean imputation
grit_mean <- mean(df$grit_score,na.rm=TRUE)
df2 <- df2 %>% mutate(grit_score = ifelse(is.na(grit_score),grit_mean,grit_score))
# num score mean imputation
num_mean <- mean(df$num_score,na.rm=TRUE)
df2 <- df2 %>% mutate(num_score = ifelse(is.na(num_score),num_mean,num_score))
# opt score mean imputation
opt_mean <- mean(df$opt_score,na.rm=TRUE)
df2 <- df2 %>% mutate(opt_score = ifelse(is.na(opt_score),opt_mean,opt_score))

## Generate dataframe for model_02
# Eliminate unused columns
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
# Convert relevant columns into numeric variables
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

## Save the cleaned data to a new csv file
write.csv(df, "data/processed/cleaned_data_model_02.csv")
write.csv(df2, "data/processed/cleaned_data_model_01.csv")


