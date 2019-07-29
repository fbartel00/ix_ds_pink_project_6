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
library(car)
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
# GGPLOT basics (covered in class) - com-score // working
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
#(1)Males are more likely to be working than Females by around 10 percent
#(2)nearly 80% of females surveyed are currently not working
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
#(1)those aged 37 years old are very likely to be working
#(2)those aged 18 are least likely to be working
#(3)due to overrepresentation of 22-27, and an undersurveying of certain peoples, 
#the data is not be comprehensively representative of the full population: ie. 100% people 
#aged 42 are working only because only 4 people aged 42 were sampled
#(4)there seems to be a slight dip of working rate in the middle (from age 18-25), and 
#there is overall a downsloping trend (one is more likely to be employed as one grows older)
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
#(1)those in Western Cape are most likely to be working, with around 35% of people working 
#(2)those in Eastern Cape are least likely to be working, with around 76% of people not working
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
#(2) from an eye's estimate, the variable cft_score seems to draw a strong relation to working 
################################# USING GGPLOT GEOM_BAR ###############################
#######################################################################################
#barplot using COUNTS - cft score, Percentage working + facet grid by gender 
ggplot(data=df) + 
  geom_bar(mapping=aes(x=cft_score, fill=working)) +
  labs(subtitle = "Gender") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("CFT Score") + ylab("Count") +
  facet_grid(~gender)
#plot using DENSITY - cft score, density 
ggplot(df, aes(x = cft_score, col = working)) +
  geom_density() +
  labs(subtitle = "Gender") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  xlab("CFT Score") + ylab("Density") +
  facet_grid(~gender)
#barplot using PERCENTAGE - cft score, Percentage working 
ggplot(data = df, aes(x = cft_score)) +
  geom_bar(aes(fill = working), position = "fill") +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  labs(subtitle = "Gender") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=scales::percent)
#barplot using PERCENTAGE - cft score, Percentage working + facet grid by gender 
ggplot(data = df, aes(x = cft_score)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~gender) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("CFT Score") +
  labs(subtitle = "Gender") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=scales::percent)
#SUBSTITUTE another x variable - age, percentage working, facet grid by age 
ggplot(data = df, aes(x = age)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~gender) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("Age") +
  labs(subtitle = "Gender") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=scales::percent) 
#CAUTION: one thing to beware of here is that there are some columns in which 100% of people 
#at a certain age are working: this probably means that there were very few of those belonging 
#to that age group represented in the survey 
#SUBSTITUTE another x variable - province, percentage working, facet grid by age 
ggplot(data = df, aes(x = province)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~gender) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("Province") +
  labs(subtitle = "Gender") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=scales::percent)
#SUBSTITUTE this time a facet_grid variable - with the centered subtitle "anyhhincome"
ggplot(data = df, aes(x = age)) +
  geom_bar(aes(fill = working), position = "fill") + facet_grid(~anyhhincome) +
  theme(axis.text.x = element_text(angle = 50, hjust =1)) +
  ylab("Percentage") + xlab("Age") + 
  labs(subtitle = "Household Income") + theme(plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=scales::percent)
#OBSERVATIONS
#(1)the lower the cft score, the lower the rate of working
#(2)after looking at facet grid, highest cft score for females is only about the same 
#as a low cft score for males; shows a deep and prevalent gender disparity
#(3)looked at age - an older age (35-40) female is the most likely to be working
#(4)a male from Western Cape is the most likely to be working 
################################# GEOM_POINT LINEAR REGRESSION ########################
#######################################################################################
#AGE AND CFT SCORE 
ggplot(df, aes(x=age, y=cft_score)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  ylab("cft score") +
  theme_bw()
#AGE AND JOB LENGTH 
ggplot(df, aes(x=age, y=job_length_m)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  ylab("job length") +
  theme_bw()
#AGE AND FIN SITU FUTURE 
ggplot(df, aes(x=age, y=fin_situ_future)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  ylab("expected future fin sit") +
  theme_bw()
#OBSERVATIONS
#(1)the older you get, the lower your cft_score (fluid intelligence/ability)
#(2)the older you get, the longer your average job length
#(3)the older you get, the less optimistic you get about your expected future financial situation
#(4)reverse these results with "the younger you get..." 
####### <EXTRA PRACTICE> #tIMPORTED TEMPLATE: colorful rainbow graident ########
#split views by gender, y=percent of those working, x=cft-score
ggplot(df, aes(x=cft_score, group=gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="cft_score") +
  facet_grid(~gender) +
  scale_y_continuous(labels = scales::percent)
################################# 3D DATA VISUALIZATION ###############################
#######################################################################################
#Try modeling with AGE, CFT SCORE, OPT SCORE 
plot_ly(df, x = ~age, y = ~cft_score, z = ~opt_score, 
        color = ~working, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'CFT Score'),
                      zaxis = list(title = 'OPT Score')))
#ADVANTAGE: can rotate, a different kind of dimensional visualization
#shows the highest and lowest points 
#DISADVANTAGE: if there are too many datapoints, it's hard to see what's going on 
#most values in the middle of the box are obscured 
#Try modeling with CFT SCORE, GRIT SCORE, OPT SCORE 
plot_ly(df, x = ~cft_score, y = ~grit_score, z = ~opt_score, 
        color = ~working, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'CFT Score'),
                      yaxis = list(title = 'GRIT Score'),
                      zaxis = list(title = 'OPT Score')))
#since all scores add up to ten, there's a lot more order to the variables:
#you can see the most outermost layers
#ie. when your grit score is toppingly high (and if you are optimistic, above zero),
#you are very likely to be employed; although you still might return notworking if your
#opt score is very low (keep turning the cube: see when grit/cft is high/low)
############################### DISPLAYING MOVING DATA ################################
#######################################################################################
#need to play around with this more but it's helpful to visualize things over a frame 
#AGE, COM SCORE, over frame CFT SCORE (color=working)
p <- df %>%
  plot_ly(
    x = ~age_at_survey, y = ~com_score, 
    frame = ~cft_score, color = ~working,
    hoverinfo = "text",type = 'scatter',mode = 'markers') %>%
  layout(xaxis = list(type = "log"))
p
#higher cft score leads to higher com score (graph moves up, clear upwards trend)
#higher cft score means lower age? (graph slightly seems to shift leftwards)
#####FURTHER STEPS
#mutate df for the percentage of people working for each different factor 
#get the average of those and plot them to show not as many (scattered and 
#possibly view-clouding) variables
############################### (UNFINISHED) DONUT PLOT #################################
#######################################################################################
# Create test data.
dat = data.frame(count=c(10, 60, 30), category=c("A", "B", "C"))
# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))
# Make the plot
p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "My Ring plot !") +
  labs(title="")
p1
############################### GGPLOT MORE PLOT PRACTICE #############################
#######################################################################################
df_province = df %>% filter(!is.na(province)) #clean data #alpha=low number, makes it transparent 
ggplot(data = df_province) + 
  geom_bar(alpha = 3/5, mapping = aes(x=cft_score, fill = province), position = "fill") 
#transparent bar, count (working in x), gender as fill  
ggplot(data = df, mapping = aes(x = working, colour = gender)) + 
  geom_bar(fill = NA, position = "identity")
#see proportions (fill to one), count (working in x), gender as fill
ggplot(data = df) + 
  geom_bar(alpha = 3/5, mapping = aes(x=working, fill = gender), position = "fill") 
################################## USEFUL RESOURCES ###################################
#######################################################################################
#summarising categorical variables in R:
#/www.sheffield.ac.uk/polopoly_fs/1.714591!/file/stcp-karadimitriou-categoricalR.pdf
#animating data visualizations 
#https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3