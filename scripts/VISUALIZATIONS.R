#######################################################################################
#######################################################################################
################################# DATA VISUALIZATION ##################################
#######################################################################################

#######################################################################################
################################# LOAD PACKAGES #######################################
#######################################################################################

library(ggplot2)
library(gridExtra)
library(dplyr)
library(forcats)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(plotly)
library(ggiraph)
library(ggiraphExtra)
library(scales)
library(reshape2)

################################# DATA CLEANING  ######################################
#######################################################################################

df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now) %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>%
  mutate(age = floor(age_at_survey)) %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(working))%>% 
  
  mutate(job_length = interval(job_start_date, job_leave_date)/months(1)) %>% 
  mutate(job_length_m = floor(job_length)) %>%
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)

# GGPLOT basics (covered in class)
ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score))
ggplot(data = df) + 
  geom_bar(mapping = aes(x = com_score, fill = working), position = "dodge")
# USING GGPLOT compares variable "working" with "age" (covered in class)
ggplot(data=df) +
  geom_bar(aes(x = age, fill = working), position = "dodge")
ggplot(df, aes(x = age, fill = working)) + 
  geom_density(col = NA, alpha = 0.3)

##################################### USING BARPLOT ###################################
#######################################################################################

################ CUSTOMIZATION  ##################
################ GENDER AND WORKING  ##################

cross<-table(df$working,df$gender)
addmargins(cross)
round(prop.table(cross,2)*100,digits=0)
#show two factors 
barplot(cross, xlab='Gender',ylab='Frequency',main="Working by Gender",
        col=c("darkblue","lightcyan")
        ,legend=rownames(cross), args.legend = list(x = "topleft",bg="transparent"))
#right beside each other 
barplot(prop.table(cross,2)*100, xlab='Gender',ylab='Percentages',main="Working by Gender(%)",
        beside=T,col=c("darkblue","lightcyan"),
        legend=rownames(cross), args.legend = list(x = "topleft"))
#show percentages
barplot(prop.table(cross,2)*100, xlab='Gender',ylab='Percentage',main="Working by Gender(%)",
        col=c("darkblue","lightcyan")
        ,legend=rownames(cross), args.legend = list(x = "topleft",bg="transparent"))
#show percentages - zoom 50 - 100 
barplot(prop.table(cross,2)*100, xlab='Gender',ylab='Percentage',main="Working by Gender(%)",
        col=c("darkblue","lightcyan"),
        legend=rownames(cross), args.legend = list(x = "topleft",bg="transparent"),
        ylim=c(50,100), xpd=FALSE)
#FINAL 2: another way to view the data
plot(cross)

#OBSERVATIONS
#(1)Males are more likely to be working than Females

################ AGE AND WORKING  ##################
cross1<-table(df$working,df$age)
addmargins(cross1)
round(prop.table(cross1,2)*100,digits=0)
#show two factors 
barplot(cross1, xlab='Age',ylab='Frequency',main="Working by Age",
        col=c("darkblue","lightcyan")
        ,legend=rownames(cross1), args.legend = list(x = "topleft",bg="transparent"))
#right beside each other 
barplot(prop.table(cross1,2)*100, xlab='Age',ylab='Percentages',main="Working by Age(%)",
        beside=T,col=c("darkblue","lightcyan"),
        legend=rownames(cross1), args.legend = list(x = "topleft",bg="transparent"))
#show percentages
barplot(prop.table(cross1,2)*100, xlab='Age',ylab='Percentage',main="Working by Age(%)",
        col=c("darkblue","lightcyan")
        ,legend=rownames(cross1), args.legend = list(x = "topleft",bg="transparent"))
#BAR (show percentages - zoom 50 - 100) 
barplot(prop.table(cross1,2)*100, xlab='Age',ylab='Percentage',main="Working by Age(%)",
        col=c("darkblue","lightcyan"),
        legend=rownames(cross1), args.legend = list(x = "topleft",bg="transparent"), 
        cex.names=.65, cex.lab=.95, srt=45,
        ylim=c(50,100), xpd=FALSE)

#OBSERVATIONS 
#(1)those aged 37 years old are most likely to be working
#(2)those aged 18 are least likely to be working 
#(3)oversampling certain people of a certain age might contribute to imbalances in data 
#(4)seems to be a dip in the middle, overall downsloping trend (one is more likely to be 
#working as one grows older 

################ PROVINCE AND WORKING  ##################
cross2<-table(df$working,df$province)
addmargins(cross2)
round(prop.table(cross2,2)*100,digits=0)
#show two factors 
barplot(cross2, xlab='Province',ylab='Frequency',main="Working by Province",
        col=c("darkblue","lightcyan")
        ,legend=rownames(cross2), args.legend = list(x = "topleft",bg="transparent"))
#right beside each other 
barplot(prop.table(cross2,2)*100, xlab='Province',ylab='Percentages',main="Working by Province(%)",
        beside=T,col=c("darkblue","lightcyan"),
        legend=rownames(cross2), args.legend = list(x = "topleft",bg="transparent"),
        las=2, cex.names=.65, cex.lab=.95, srt=45)
#show percentages - show all labels, rotate vertically  
barplot(prop.table(cross2,2)*100, xlab='Province',ylab='Percentage',main="Working by Province(%)",
        col=c("darkblue","lightcyan"),
        legend=rownames(cross2), args.legend = list(x = "topleft",bg="transparent"), 
        las=2, cex.names=.65, cex.lab=.95, srt=45)
#show percentages - zoom into 40-100 
barplot(prop.table(cross2,2)*100, xlab='Province',ylab='Percentage',main="Working by Province(%)",
        col=c("darkblue","lightcyan"),
        legend=rownames(cross2), args.legend = list(x = "topleft",bg="transparent"), 
        las=2, cex.names=.65, cex.lab=.95, srt=45,
        ylim=c(40,100), xpd=FALSE)
#OBSERVATIONS 
#(1)those in Western Cape are most likely to be working
#(2)those in Eastern Cape are least likely to be working 

################ XLAB AND WORKING  ##################
#insert variable XLAB and compare to see make observations based on the resulting information
cross3<-table(df$working,df$cft_score)
addmargins(cross3)
round(prop.table(cross3,2)*100,digits=0)
#show percentages - zoom into 40-100 
barplot(prop.table(cross3,2)*100, xlab='XLAB',ylab='Percentage',main="Working by XLAB(%)",
        col=c("darkblue","lightcyan"),
        legend=rownames(cross3), args.legend = list(x = "topleft",bg="transparent"), 
        cex.names=.75, cex.lab=.95, srt=45,
        ylim=c(50,100), xpd=FALSE)
#OBSERVATIONS
#(1) than intitially assumed, age seems not to have a clear relation to the working variable
#(2) from an eye's estimate, the variable cft_score seems to draw a clear relation to working 

##################################### USING GGPLOT ####################################
#######################################################################################
#barplot using COUNTS - cft score, Percentage working + facet grid by gender 
ggplot(data=df) + 
  geom_bar(mapping=aes(x=cft_score, fill=working)) +
  facet_grid(~gender)
#plot using DENSITY - cft score, density 
ggplot(df, aes(x = cft_score, col = working)) +
  geom_density() +
  facet_grid(~gender)
#barplot using PERCENTAGE - cft score, Percentage working 
ggplot(data = df, aes(x = cft_score)) +
  geom_bar(aes(fill = working), position = "fill") +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  scale_y_continuous(labels=scales::percent)
#barplot using PERCENTAGE - cft score, Percentage working + facet grid by gender 
ggplot(data = df, aes(x = cft_score)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~gender) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  scale_y_continuous(labels=scales::percent) 
#SUBSTITUTE another x variable - age, percentage working, facet grid by age 
ggplot(data = df, aes(x = age)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~gender) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  scale_y_continuous(labels=scales::percent) 
#CAUTION: one thing to beware of here is that there are some columns in which 100% of people 
#at a certain age are working: this probably means that there were very few of those belonging 
#to that age group represented in the survey 
#SUBSTITUTE another x variable - age, percentage working, facet grid by age 
ggplot(data = df, aes(x = province)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~gender) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  scale_y_continuous(labels=scales::percent)
#SUBSTITUTE this time a facet_grid variable
ggplot(data = df, aes(x = age)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~anyhhincome) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  scale_y_continuous(labels=scales::percent)
####### <EXTRA PRACTICE> #tIMPORTED TEMPLATE: colorful rainbow graident ########
#split views by gender, y=percent of those working, x=cft-score
ggplot(df, aes(x=cft_score, group=gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="cft_score") +
  facet_grid(~gender) +
  scale_y_continuous(labels = scales::percent)