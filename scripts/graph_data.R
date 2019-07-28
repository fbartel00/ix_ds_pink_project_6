### graph_data.R
# Author: Joe Delle Donne
# Date: 07/28/2019

rm(list=ls())

## Install necessary packages
library(tidyverse)
library(lubridate)

### Reading Data
df <- read.csv("data/raw/teaching_training_data.csv")
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey)) %>% 
  filter(!is.na(gender))
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

### Graphs

df <- df %>% filter(age > 15) %>% filter(age < 40)

ggplot(data=df, aes(x = age, fill = working)) +
  geom_bar() 
  #facet_grid(gender ~ .)

df_peoplelive <- df %>% filter(!is.na(peoplelive))
ggplot(data=df_peoplelive, aes(x = peoplelive, fill = working)) +
  geom_bar()
