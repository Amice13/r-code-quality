##########################################################################################################
############### This file reproduces the main parts of the appendix ######################################
############### Data is not included due to Vkontakte's terms of service and my institutions' ############
############### assessment of the project (in terms of ethics, legality and GDPR); You can redownload ####
############### a very similar data set from Vkontakte's API using the scripts in 1_downloading_data.R ###
##########################################################################################################

### installing packages
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("robustbase")
install.packages("lmtest")
install.packages("sandwich")
install.packages("stargazer")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggmap")
install.packages("data.table")
install.packages("car")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("sp")
install.packages("AER")
install.packages("foreign")
install.packages("rdd")
install.packages("stargazer")
install.packages("dotwhisker")
install.packages("broom")
install.packages("stringr")
install.packages("reshape2")


### loading packages
library(ggplot2)
library(ggthemes)
library(robustbase)
library(lmtest)
library(sandwich)
library(stargazer)
library(tidyr)
library(dplyr)
library(ggmap)
library(data.table)
library(car)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(sp)
library(AER)
library(foreign)
library(rdd)
library(stargazer)
library(dotwhisker)
library(broom)
library(stringr)
library(reshape2)

### Setting working directory
setwd("/YOUR/WORKING DIRECTORY/HERE")


##########################################################################################################
###Creating Figure B1: Indexed search volume on Google for 'Vkontakte ban' and 'VPN for VK' in Ukraine####
##########################################################################################################

### download the Google trends data that was originally downloaded by me - see the appendix for details
google <- read.csv("http://golovchenko.github.io/data/vk_censorship/google_trends_clean.csv")

### formatting date
google$day <- as.Date(google$day)

### Plotting the figure
ggplot(google) +
  geom_line(aes(day, vpn, color = "VPN for VK"), size = 1) +
  geom_line(aes(day, zapret, color =  "Vkontakte ban"), size = 1) +
  scale_colour_manual(values=c("black", "grey"),
                      labels = c("Vkontakte ban", "VPN for VK")) +
  geom_vline(xintercept = as.Date("2017-05-16"), linetype = "dashed", color = "grey")+ 
  labs(x = "2017",
       y = "Search volume (indexed)", 
       color = "Search term (in Russian)") +
  theme_tufte(ticks = F) +
  theme(axis.text=element_text(size= 12),
        axis.title=element_text(size=14),
        legend.title =element_text(size = 14),
        legend.text = element_text(size = 12))


##########################################################################################################
##########################################################################################################
##############################Abandoned accounts from both sides of the border############################
##########################################################################################################
##########################################################################################################

##########################################################################################################
#########################Creating Figure I1: Last logins on both sides of the border######################
##########################################################################################################

### creating a binary variable indicating whether the user from Mainland Ukraine
df$border_ua <- 0
df$border_ua[df$border == 1] <- 1

### creating a binary variable indicating whether the user from Crimea
df$border_ru <- 0
df$border_ru[df$border == 2] <- 1

df$days1 <-  as.numeric(df$days -3 ) #(18th of may == 0)


### creating a dataframe where each row represents a week (before and after censorship)
df.weeks <- df %>% # loading the original data set where row represent users within 50 km from the bo5rder
  mutate(weeks = round((days1-3)/7, digits = 0)) %>% #creating a variable indicating weeks before/censorship for the last login day
  group_by(weeks) %>% # grouping by weeks
  mutate(sum_ua = sum(border_ua),  # counting number of users in Mainland Ukraine abandoning VK that week
         sum_ru = sum(border_ru),# counting number of users in Crimea abandoning VK that week
         prop_ua = sum_ua/40458*100, # proportion of users in Mainland Ukraine abandoning VK that week
         prop_ru = sum_ru/68733*100) %>% #proportion of users in Crimea abandoning VK that week
  distinct(weeks, .keep_all = T) # keepinly only unique weeks

## renaming the border variable
df.weeks$border[df.weeks$border == 1] <- "Mainland Ukraine"
df.weeks$border[df.weeks$border == 2] <- "Crimea"




#### vizualising proportion of abandoned accounts in Crimea and Ukraine
ggplot(df.weeks) +
  geom_point(aes(weeks, prop_ru, color = "Crimea"), size = 1) +
  geom_point(aes(weeks, prop_ua, color =  "Mainland Ukraine"), size = 1) +
  geom_line(aes(weeks, prop_ru, color = "Crimea")) +
  geom_line(aes(weeks, prop_ua, color = "Mainland Ukraine")) +
  scale_x_continuous(limits = c(-24,24)) +
  #  scale_y_log10(limits = c(0,150)) +
  scale_y_continuous(limits = c(0,2.0)) +
  scale_colour_manual(values=c("grey", "black"),
                      labels = c("Crimea", "Mainland Ukraine")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Weeks before and after censorship",
       y = "Last logins per week (pct.)", 
       color = "Location") +
  theme(strip.text = element_text(size=80)) + 
  theme_tufte(ticks = F) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=12))


##########################################################################################################
######## Creating Figure K1: Ratio between number of last logins among users with above-median ###########
#######  and up-to-median proportions of friends in Russia.###############################################
##########################################################################################################



############################ONLY in Mainland: computing the ratio for weekly number of users leaving VK (above median/up to median)
### adjusting the cuttof date. day 0 is 18th of may 2017
all_users_sna.1$days1 <- all_users_sna.1$days - 3
### selecting users from mainland Ukraine with SNA data
ukr_users_sna<- all_users_sna.1 %>% 
  filter(border == "Mainland Ukraine", 
         days1 >=-168) 
summary(ukr_users_sna$prop_rus_friends)

### creating a binary variable indicating whether the users have up to median of friends in Russia or Not
ukr_users_sna$up_to_median_ru <- 0
ukr_users_sna$up_to_median_ru[ukr_users_sna$prop_rus_friends <= median(ukr_users_sna$prop_rus_friends)] <-1

### creating a binary variable indicating whether the users have above median proportion of friends in Russia or Not
ukr_users_sna$above_median_ru <- 0
ukr_users_sna$above_median_ru[ukr_users_sna$prop_rus_friends > median(ukr_users_sna$prop_rus_friends)] <-1

### computing the ratio for weekly number of users leaving VK (above median/up to median)
ukr_users_sna <- ukr_users_sna %>%
  mutate(weeks = round((days1-3)/7, digits = 0)) %>%
  group_by(weeks) %>%
  mutate(sum_up_to_median_ru = sum(up_to_median_ru), 
         sum_above_median_ru = sum(above_median_ru), 
         ratio_soc = sum_above_median_ru/sum_up_to_median_ru) %>%
  distinct(weeks, .keep_all = T)

###############ONLY in Crimea: computing the ratio for weekly number of users leaving VK (above median/up to median)

### selecting users from mainland Ukraine with SNA data
crim_users_sna<- all_users_sna.1 %>% 
  filter(border == "Crimea", 
         days1 >=-168) 
summary(crim_users_sna$prop_rus_friends)

### creating a binary variable indicating whether the users have up to median  proportion of friends in Russia or Not
crim_users_sna$up_to_median_ru <- 0
crim_users_sna$up_to_median_ru[crim_users_sna$prop_rus_friends <= median(crim_users_sna$prop_rus_friends)] <-1

### creating a binary variable indicating whether the users have above median proportion of friends in Russia or Not
crim_users_sna$above_median_ru <- 0
crim_users_sna$above_median_ru[crim_users_sna$prop_rus_friends > median(crim_users_sna$prop_rus_friends)] <-1

### computing the ratio for weekly number of users leaving VK (above median/up to median)
crim_users_sna <- crim_users_sna %>%
  mutate(weeks = round(days1, digits = 0)) %>%
  group_by(weeks) %>%
  mutate(sum_up_to_median_ru = sum(up_to_median_ru), 
         sum_above_median_ru = sum(above_median_ru), 
         ratio_soc = sum_above_median_ru/(sum_up_to_median_ru)) %>%
  distinct(weeks, .keep_all = T)

##############In Crimea and Mainland; computing the ratio for weekly number of users leaving VK (above median/up to median)

## combining the two dataframes above (from Crimea and Mainland)
ukr_crim_users_sna <- rbind(crim_users_sna, ukr_users_sna)

## changing the order in the "border varia0ble" in order to ensure that "Mainland Ukraine" appears first (at the top) in ggplot
ukr_crim_users_sna$border <- factor(ukr_crim_users_sna$border, levels = c("Mainland Ukraine","Crimea"))



### vizualising the ratio in Mainland 
ggplot(ukr_users_sna, aes(weeks, ratio_soc)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(-10,10)) +
  scale_y_continuous(limits = c(-0,1.5)) +
  geom_smooth(se = T, colour = "black", linetype = "longdash") +
  labs(x = "Weeks before and after censorship",
       y = "Above median / up to median prop. of friends in RU", 
       color = "Location") +
  theme(strip.text = element_text(size=50), strip.text.x = element_text(size = 20)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte(ticks = F) +
  theme(strip.text = element_text(size=15)) +
  facet_wrap(.~ border, nrow = 1)  



##########################################################################################################
######## Counting number of users (with and without wallposts) who are active within 7 days ##############
####### from the collection date# ########################################################################
##########################################################################################################

### creating a dataframe of active users in Mainland
df.act.ua <- df.act %>%
  filter(border == 1) 
### creating a dataframe of active users in Crimea
df.act.crim <- df.act %>%
  filter(border == 2) 

### what is the maximum date in the data set

ua_max <- max(df.act.ua$days)
crim_max <- max(df.act.crim$days)
ua_max

## have logged in within 7 days of the collection day
df.act.ua.recent <- df.act.ua %>%
  filter(days >=430)

df.act.crim.recent <- df.act.crim %>%
  filter(days >=430)

### of all the active accounts, how many have logged in within 7 days from the data collection date?
nrow(df.act.ua.recent)/nrow(df.act.ua)
nrow(df.act.crim.recent)/nrow(df.act.crim)






##########################################################################################################
############## Figure K1: Ratio between number of last logins among users with above-median ##############
############## and up-to-median proportions of friends in Russia. ########################################
##########################################################################################################



############################ONLY in Mainland: computing the ratio for weekly number of users leaving VK (above median/up to median)
### adjusting the cuttof date. day 0 is 18th of may 2017
all_users_sna.1$days1 <- all_users_sna.1$days - 3
### selecting users from mainland Ukraine with SNA data
ukr_users_sna<- all_users_sna.1 %>% 
  filter(border == "Mainland Ukraine", 
         days1 >=-168) 
summary(ukr_users_sna$prop_rus_friends)

### creating a binary variable indicating whether the users have up to median (0.05634) proportion of friends in Russia or Not
ukr_users_sna$up_to_median_ru <- 0
ukr_users_sna$up_to_median_ru[ukr_users_sna$prop_rus_friends <= median(ukr_users_sna$prop_rus_friends)] <-1

### creating a binary variable indicating whether the users have above median (0.05634) proportion of friends in Russia or Not
ukr_users_sna$above_median_ru <- 0
ukr_users_sna$above_median_ru[ukr_users_sna$prop_rus_friends > median(ukr_users_sna$prop_rus_friends)] <-1

### computing the ratio for weekly number of users leaving VK (above median/up to median)
ukr_users_sna <- ukr_users_sna %>%
  mutate(weeks = round((days1-3)/7, digits = 0)) %>%
  group_by(weeks) %>%
  mutate(sum_up_to_median_ru = sum(up_to_median_ru), 
         sum_above_median_ru = sum(above_median_ru), 
         ratio_soc = sum_above_median_ru/sum_up_to_median_ru) %>%
  distinct(weeks, .keep_all = T)

############################ONLY in Crimea; computing the ratio for weekly number of users leaving VK (above median/up to median)

### selecting users from mainland Ukraine with SNA data
crim_users_sna<- all_users_sna.1 %>% 
  filter(border == "Crimea", 
         days1 >=-168) 
summary(crim_users_sna$prop_rus_friends)

### creating a binary variable indicating whether the users have up to median (0.15455) proportion of friends in Russia or Not
crim_users_sna$up_to_median_ru <- 0
crim_users_sna$up_to_median_ru[crim_users_sna$prop_rus_friends <= median(crim_users_sna$prop_rus_friends)] <-1

### creating a binary variable indicating whether the users have above median (0.15455) proportion of friends in Russia or Not
crim_users_sna$above_median_ru <- 0
crim_users_sna$above_median_ru[crim_users_sna$prop_rus_friends > median(crim_users_sna$prop_rus_friends)] <-1

### computing the ratio for weekly number of users leaving VK (above median/up to median)
crim_users_sna <- crim_users_sna %>%
  mutate(weeks = round(days1, digits = 0)) %>%
  group_by(weeks) %>%
  mutate(sum_up_to_median_ru = sum(up_to_median_ru), 
         sum_above_median_ru = sum(above_median_ru), 
         ratio_soc = sum_above_median_ru/(sum_up_to_median_ru)) %>%
  distinct(weeks, .keep_all = T)

############################In Crimea and Mainland; computing the ratio for weekly number of users leaving VK (above median/up to median)

## combining the two dataframes above (from Crimea and Mainland)
ukr_crim_users_sna <- rbind(crim_users_sna, ukr_users_sna)

## changing the order in the "border varia0ble" in order to ensure that "Mainland Ukraine" appears first (at the top) in ggplot
ukr_crim_users_sna$border <- factor(ukr_crim_users_sna$border, levels = c("Mainland Ukraine","Crimea"))




### vizualising the ratio in Mainland 

ggplot(ukr_users_sna, aes(weeks, ratio_soc)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(-10,10)) +
  scale_y_continuous(limits = c(-0,1.5)) +
  geom_smooth(se = T, colour = "black", linetype = "longdash") +
  labs(x = "Weeks before and after censorship",
       y = "Above median / up to median prop. of friends in RU", 
       color = "Location") +
  theme(strip.text = element_text(size=50), strip.text.x = element_text(size = 20)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte(ticks = F) +
  theme(strip.text = element_text(size=15)) +
  facet_wrap(.~ border, nrow = 1)  


##########################################################################################################
##########################################################################################################
############### Moving for analysis of last login dates for the large sample to ##########################
############### the analysis of posting activity in the sub-samples ######################################
##########################################################################################################
##########################################################################################################

##########################################################################################################
#################### Preparing wall post data for analysis and selecting users ###########################
################### who have posted at least once  before the ban ########################################
##########################################################################################################

############################ Filtering out hyperctive users######################################
walls.clean.1 <- walls
walls.clean.1$days1 <-  as.numeric(walls.clean.1$days -3 ) #(18th of may == 0)

### feeding the metadata about the users friendship  back into the wall post dataset
# selecting relevant network variables
# added ungroup below in the JoP RnR1 version due to change in R versions
walls.users.x <- walls.users.2 %>% ungroup() %>% select(owner_id, prop_rus_friends, prop_ukr_friends, prop_crimea_friends, sum_friends, sex)
# merging wall post data with network meta data
walls.clean.1 <- right_join(walls.clean.1, walls.users.x)
summary(walls.clean.1$prop_rus_friends)

#View(walls.clean.1)
walls.clean.1$before <-0
walls.clean.1$before[walls.clean.1$days1 < 0 & walls.clean.1$days1 >=-90] <- 1
walls.clean.1$after <-0
walls.clean.1$after[walls.clean.1$days1 >= 0 & walls.clean.1$days1 <= 90] <- 1

### computing basic activity metrics about the users and selecting only those who have have posted no more than 5 posts per day during 30 days before/after the ban
walls.clean.2 <- walls.clean.1 %>%
  group_by(owner_id) %>%  #grouping by users in the wallpost sample
  mutate(min_days = min(days1),
         sum_posts_before = sum(before),
         sum_posts_after = sum(after),
         mean_posts_before = sum_posts_before/90,
         mean_posts_after = sum_posts_after/91) %>%
  mutate(total_posts = sum_posts_after + sum_posts_before,
         mean_posts = total_posts/181) %>%
  filter(mean_posts_before <= 3, 
         min_days < 0)


### adding weak days variables
# creating a variable for week days
walls.clean.2$day <- weekdays(as.Date(walls.clean.2$date1))

# creating a dummy variable for weekend (Saturday/Sunday); yes == 1 and no == 0.
walls.clean.2$weekend <- 0 #creating an empty variable
walls.clean.2$weekend[walls.clean.2$day == "Lørdag" | walls.clean.2$day == "Søndag"] <- 1 ## note that the days are in Danish, because I am using a Danish OS system. If you use a different system, you should use the words corresponding to your language
### counting number of users who have posted at least once prior to the cutoff date
n_user_days <- nrow(walls.clean.2 %>%
                      filter(border == "Mainland Ukraine") %>%
                      distinct(owner_id, .keep_all = T))

n_user_days



##########################################################################################################
#################### Creating panel data (time-user) where the absence of post############################
#################### for a given day is indicated as 0 ###################################################
##########################################################################################################

### examining median number of friends livining in Russia ONLY FOR USERS IN MAINLAND UKRAINE

# Selecting users in Mainland Ukraine within 90 days range before/after censorship
median_rus<- walls.clean.2 %>% filter(border == "Mainland Ukraine", 
                                      days1 >= -90, 
                                      days1 >= 90) %>% 
  distinct(owner_id, .keep_all = T)


# Selecting users in Crimea within 90 days range before/after censorship
median_rus_crim<- walls.clean.2 %>% filter(border == "Crimea", 
                                           days1 >= -90, 
                                           days1 >= 90) %>% 
  distinct(owner_id, .keep_all = T)


### storing the median value for users in Crimea
median_rus_crim <- median(median_rus_crim$prop_rus_friends)

### storing the median value for users in Mainland
median_rus <- median(median_rus$prop_rus_friends)


### creating a binary variableFOR USERS IN MAINLAND indicating whether the users have up to median proportion of friends in Russia or not
walls.clean.2$up_to_median_ru <- 0
walls.clean.2$up_to_median_ru[walls.clean.2$prop_rus_friends <= median_rus & walls.clean.2$border == "Mainland Ukraine"] <-1
walls.clean.2$above_median_ru <- 0
walls.clean.2$above_median_ru[walls.clean.2$prop_rus_friends > median_rus  & walls.clean.2$border == "Mainland Ukraine"] <-1


### creating a binary variable  FOR USERS IN CRIMEA indicating whether the users have above median proportion of friends in Russia or not
walls.clean.2$up_to_median_ru[walls.clean.2$prop_rus_friends <= median_rus_crim  & walls.clean.2$border == "Crimea"] <-1
walls.clean.2$above_median_ru[walls.clean.2$prop_rus_friends > median_rus_crim & walls.clean.2$border == "Crimea"] <-1


### creating a data set
activity_per_day <- walls.clean.2 %>%
  ungroup() %>%
  group_by(days1, owner_id) %>%
  mutate(posts_days = n()) %>%
  distinct(owner_id, .keep_all = T) %>%
  ungroup() %>%
  select(owner_id, days1, posts_days)
### adding 0 values for the days when the user did not post anytning

activity_per_day1 <- activity_per_day
g <- with(activity_per_day1, expand.grid(days1 = seq(-90, 90), owner_id = unique(owner_id), posts_days = 0)) #
activity_per_day1 <- rbind(activity_per_day1, g) #
wide <- dcast(activity_per_day1, days1 ~ owner_id, fill = 0, fun = sum, value.var = "posts_days")
long <- melt(wide, id = "days1", variable.name = "owner_id", value.name = "posts_days")
long$owner_id <- as.numeric(as.character(long$owner_id)) # converting from factor to numeric
head(long$owner_id)
###create a list of variables for each distinct individual in walls.clean.2
walls.clean.2.unique <- walls.clean.2 %>%
  ungroup() %>%
  distinct(owner_id, .keep_all = T) 
### removing reduntant variables
walls.clean.2.unique$days1 <- NULL
walls.clean.2.unique$days <- NULL
walls.clean.2.unique$weeks <- NULL

### adding the user metadata to the paneldata
long.panel.original <- left_join(long, walls.clean.2.unique) 

########################### 90 days threshold ###########################

### filtered panel data frame
long.panel <- long.panel.original %>%
  filter(days1 >= -90, # selecting posts within 90 before and after censorship
         days1 <= 90)


#### Creating a dummy variable which indicates whether data point is from before or after censorship
long.panel$before <- 0
long.panel$before[long.panel$days1 < 0 & long.panel$days1 >=-90] <- 1

long.panel$after <-0
long.panel$after[long.panel$days1 >= 0 & long.panel$days1 <= 90] <- 1

### creating a dummy variable indicating wether the post is from Mainland Ukraine or not
long.panel$ua_border <- 0
long.panel$ua_border[long.panel$border == "Mainland Ukraine"] <-1


# Computing median proportion of friends living in Russia for users in Crimea
median_rus<- long.panel %>%
  ungroup() %>%
  filter(ua_border != 0) %>%
  distinct(owner_id, .keep_all = T)
median_rus <- median(median_rus$prop_rus_friends)
median_rus

# Computing median proportion of friends living in Russia for political users in Crimea
median_rus.pol <- long.panel %>%
  ungroup() %>%
  filter(ua_border == 0, 
         ran == 0) %>%
  distinct(owner_id, .keep_all = T)
median_rus.pol <- median(median_rus.pol$prop_rus_friends)
median_rus.pol


# Computing median proportion of friends living in Russia  for users in Mainland Ukraine
median_ua<- long.panel %>%
  ungroup() %>%
  filter(ua_border == 1) %>%
  distinct(owner_id, .keep_all = T)
median_ua <- median(median_ua$prop_rus_friends)
median_ua

# Computing median proportion of friends living in Russia  for users in Mainland Ukraine
median_ua.pol <- long.panel %>%
  ungroup() %>%
  filter(ua_border == 1, 
         ran == 0) %>%
  distinct(owner_id, .keep_all = T)
median_ua.pol <- median(median_ua.pol$prop_rus_friends)
median_ua.pol

# Computing median proportion of friends living in Russia for all users
median_all<- long.panel %>%
  ungroup() %>%
  distinct(owner_id, .keep_all = T)
median_all <- median(median_all$prop_rus_friends)
median_all

# Computing median proportion of friends living in Russia for all users
median_all.pol <- long.panel %>%
  ungroup() %>%
  filter( ran == 0) %>%
  distinct(owner_id, .keep_all = T)
median_all.pol <- median(median_all.pol$prop_rus_friends)


### creating a binary variable indicating whether the users have up to LOCAL median proportion of friends in Russia
long.panel$above_median_ru <- 0
long.panel$above_median_ru[long.panel$ua_border == 0 & long.panel$prop_rus_friends > median_rus] <-1 # 1 if users living in Crimea have above median proportion for that region
long.panel$above_median_ru[long.panel$ua_border == 1 & long.panel$prop_rus_friends > median_ua] <-1 # 1 if users living in Mainland Ukraine have above median proportion for that region

### creating a binary variable indicating whether the users have up to median proportion of friends in Russia or Not
long.panel$above_median_ru_all <- 0
long.panel$above_median_ru_all[long.panel$prop_rus_friends > median_all] <-1 # 1 if users living in Crimea have above median proportion for that region

### creating a binary variable indicating whether the  users have above LOCAL POLITICAL median proportion of friends in Russia
long.panel$above_median_ru.pol <- 0
long.panel$above_median_ru.pol[long.panel$ua_border == 0 & long.panel$prop_rus_friends > median_rus.pol] <-1 # 1 if users living in Crimea have above median proportion for that region
long.panel$above_median_ru.pol[long.panel$ua_border == 1 & long.panel$prop_rus_friends > median_ua.pol] <-1 # 1 if users living in Mainland Ukraine have above median proportion for that region

### creating a binary variable indicating whether the POLITICAL users have above median proportion of friends in Russia or Not
long.panel$above_median_ru_all.pol <- 0
long.panel$above_median_ru_all.pol[long.panel$prop_rus_friends >= median_all.pol] <-1 # 1 if users living in Crimea have above median proportion for that region

### examining the data and removing outliers # NOT IMPLEMENTING THIS IN THE FINAL ANALYSIS BECAUSE IT DOES NOT AFFECT THE RESULTS
long.panel.clean <- long.panel %>%
  arrange(posts_days) #%>%
#filter(posts_days <=5)

### dummy variable indicating whether the user is male or not
long.panel.clean$sex[long.panel.clean$sex == 2] <- 0

### keeping only pro-Russian and pro-Ukrainian users 
long.panel.clean.pol <- long.panel.clean %>%
  filter(ran == 0)


### keeping only users in Ukraine
long.panel.clean.ua <- long.panel.clean %>%
  filter(ua_border == 1)

### keeping only pro-Russian and pro-Ukrainian users in Ukraine
long.panel.clean.ua.pol <- long.panel.clean %>%
  filter(ua_border == 1,
         ran == 0)



######################################################################
##########Descriptive table with info about users#####################
######################################################################

##########################################################################################################
#################### Creating Table B1: Descriptive statistics (Mainland Ukraine) ########################
#################### Users sub-sample and proportion of Vkontakte friends in Russia ######################
##########################################################################################################

##################Creating a table only for users in Ukraine#################################
#### Creating descriptive table showing number of users in each political category and their friends


### Storing the necessary info for users in Ukraine
# selecting unique users in Ukraine
ukr_t <- long.panel.clean %>%
  filter(ua_border == 1) %>%
  distinct(owner_id, .keep_all = T)


### selecting users in Ukraine
ukr_pro_rus <- ukr_t %>% filter(rus == 1)
ukr_pro_ukr <- ukr_t %>% filter(ukr == 1)
ukr_pro_ran <- ukr_t %>% filter(ran == 1)

# counting number of users
n_ukr_pro_rus <- nrow(ukr_pro_rus)
n_ukr_pro_ukr <- nrow(ukr_pro_ukr)
n_ukr_pro_ran <- nrow(ukr_pro_ran)
n_ukr_all <- nrow(ukr_t)

# computing mean for proportion of friends living in Russia for each political category
fr_mean_ukr_pro_rus <- mean(ukr_pro_rus$prop_rus_friends)
fr_mean_ukr_pro_ukr <- mean(ukr_pro_ukr$prop_rus_friends)
fr_mean_ukr_pro_ran <- mean(ukr_pro_ran$prop_rus_friends)
fr_mean_ukr_all <- mean(ukr_t$prop_rus_friends)

# computing median proportion of friends living in Russia for each political category
fr_median_ukr_pro_rus <- median(ukr_pro_rus$prop_rus_friends)
fr_median_ukr_pro_ukr <- median(ukr_pro_ukr$prop_rus_friends)
fr_median_ukr_pro_ran <- median(ukr_pro_ran$prop_rus_friends)
fr_median_ukr_all <- median(ukr_t$prop_rus_friends)

# computing standard deviation for proportion of friends living in Russia for each political category
fr_sd_ukr_pro_rus <- sd(ukr_pro_rus$prop_rus_friends)
fr_sd_ukr_pro_ukr <- sd(ukr_pro_ukr$prop_rus_friends)
fr_sd_ukr_pro_ran <- sd(ukr_pro_ran$prop_rus_friends)
fr_sd_ukr_all <- sd(ukr_t$prop_rus_friends)

# creating labels for each political category
category <- c("Pro-Russian", "Pro-Ukrainian", "Random", "All")
### combining in a data frame
table1 <- data.frame(category = category,
                     user_n = c(n_ukr_pro_rus, n_ukr_pro_ukr, n_ukr_pro_ran, n_ukr_all),
                     mean_fr = c(fr_mean_ukr_pro_rus, fr_mean_ukr_pro_ukr, fr_mean_ukr_pro_ran, fr_mean_ukr_all),
                     median_fr = c(fr_median_ukr_pro_rus, fr_median_ukr_pro_ukr, fr_median_ukr_pro_ran, fr_median_ukr_all),
                     sd_fr = c(fr_sd_ukr_pro_rus, fr_sd_ukr_pro_ukr, fr_sd_ukr_pro_ran, fr_sd_ukr_all))


##################Creating a table only for users in Crimea#################################

### Storing the necessary info for users in Ukraine
# selecting unique users in Ukraine
crim_t <- long.panel.clean %>%
  filter(ua_border == 0) %>%
  distinct(owner_id, .keep_all = T)

### selecting users in Ukraine
crim_pro_rus <- crim_t %>% filter(rus == 1)
crim_pro_ukr <- crim_t %>% filter(ukr == 1)
crim_pro_ran <- crim_t %>% filter(ran == 1)

# counting number of users
n_crim_pro_rus <- nrow(crim_pro_rus)
n_crim_pro_ukr <- nrow(crim_pro_ukr)
n_crim_pro_ran <- nrow(crim_pro_ran)
n_crim_all <- nrow(crim_t)

# computing mean for proportion of friends living in Russia for each political category
fr_mean_crim_pro_rus <- mean(crim_pro_rus$prop_rus_friends)
fr_mean_crim_pro_ukr <- mean(crim_pro_ukr$prop_rus_friends)
fr_mean_crim_pro_ran <- mean(crim_pro_ran$prop_rus_friends)
fr_mean_crim_all <- mean(crim_t$prop_rus_friends)

# computing median proportion of friends living in Russia for each political category
fr_median_crim_pro_rus <- median(crim_pro_rus$prop_rus_friends)
fr_median_crim_pro_ukr <- median(crim_pro_ukr$prop_rus_friends)
fr_median_crim_pro_ran <- median(crim_pro_ran$prop_rus_friends)
fr_median_crim_all <- median(crim_t$prop_rus_friends)

# computing standard deviation for proportion of friends living in Russia for each political category
fr_sd_crim_pro_rus <- sd(crim_pro_rus$prop_rus_friends)
fr_sd_crim_pro_ukr <- sd(crim_pro_ukr$prop_rus_friends)
fr_sd_crim_pro_ran <- sd(crim_pro_ran$prop_rus_friends)
fr_sd_crim_all <- sd(crim_t$prop_rus_friends)

# creating labels for each political category
category <- c("Pro-Russian", "Pro-Ukrainian", "Random", "All")
### combining in a data frame
table2 <- data.frame(category = category,
                     user_n_crim = c(n_crim_pro_rus, n_crim_pro_ukr, n_crim_pro_ran, n_crim_all),
                     mean_fr_crim = c(fr_mean_crim_pro_rus, fr_mean_crim_pro_ukr, fr_mean_crim_pro_ran, fr_mean_crim_all),
                     median_fr_crim = c(fr_median_crim_pro_rus, fr_median_crim_pro_ukr, fr_median_crim_pro_ran, fr_median_crim_all),
                     sd_fr_crim = c(fr_sd_crim_pro_rus, fr_sd_crim_pro_ukr, fr_sd_crim_pro_ran, fr_sd_crim_all))


### Creating the table
stargazer(table1, table2, 
          type = "text",
          summary = F,
          rownames = F, 
          title = c("Mainland Ukraine", "Crimea"),
          align = T,
          covariate.labels = c("Group", "N", "Mean prop. of friends", "Median prop. of friends", "SD"), 
          single.row = F, 
          column.sep.width = "0.5pt")

##########################################################################################################
#################### Creating Table B1: Descriptive statistics (Mainland Ukraine) ########################
#################### Posting activity 90 days before and after the ban ###################################
##########################################################################################################

####################Computing values only for Mainland###################################
### mean number of posts before censorship for the different groups in the subsample of users
ukr_posts_rus_before <- mean(ukr_pro_rus$sum_posts_before)/90
ukr_posts_ukr_before <- mean(ukr_pro_ukr$sum_posts_before)/90
ukr_posts_ran_before <- mean(ukr_pro_ran$sum_posts_before)/90
ukr_posts_all_before <- mean(ukr_t$sum_posts_before)/90

### sd for posts before censorship for the different groups in the subsample of users
ukr_sd_rus_before <- sd(ukr_pro_rus$mean_posts_before)
ukr_sd_ukr_before <- sd(ukr_pro_ukr$mean_posts_before)
ukr_sd_ran_before <- sd(ukr_pro_ran$mean_posts_before)
ukr_sd_all_before <- sd(ukr_t$mean_posts_before)

### sum of posts before and after censorship for the different groups in the subsample of users
posts_ukr_all <- walls.clean.2 %>%# selecting dataframe where each row represents a post
  filter(border == "Mainland Ukraine",#selecting only posts from Mainland Ukraine
         days1 >= -90, # selecting posts within 90 before and after censorship
         days1 <= 90)

### posts from Ukraine before the ban 
posts_ukr_before <- posts_ukr_all %>%
  filter(before == 1)

### posts from Ukraine after the ban 
posts_ukr_after<- posts_ukr_all %>%
  filter(before == 0)



posts_crim_all <- walls.clean.2 %>%# selecting dataframe where each row represents a post
  filter(border == "Crimea",#selecting only posts from Mainland Ukraine
         days1 >= -90, # selecting posts within 90 before and after censorship
         days1 <= 90)


### posts from Ukraine before the ban 
posts_crim_before <- posts_crim_all %>%
  filter(before == 1)

### posts from Ukraine after the ban 
posts_crim_after<- posts_crim_all %>%
  filter(before == 0)


#### counting number of posts before the ban in Mainland Ukraine for each sub-group
ukr_sum_rus_before <- nrow(posts_ukr_before[posts_ukr_before$rus == 1, ])
ukr_sum_ukr_before <- nrow(posts_ukr_before[posts_ukr_before$ukr == 1, ])
ukr_sum_ran_before <- nrow(posts_ukr_before[posts_ukr_before$ran == 1, ])
ukr_sum_all_before <- nrow(posts_ukr_before)



### mean number of posts after censorship for the different groups in the subsample of users
ukr_posts_rus_after <- mean(ukr_pro_rus$sum_posts_after)/91

ukr_posts_ukr_after <- mean(ukr_pro_ukr$sum_posts_after)/91
ukr_posts_ran_after <- mean(ukr_pro_ran$sum_posts_after)/91

### sd for posts after censorship for the different groups in the subsample of users
ukr_sd_rus_after <- sd(ukr_pro_rus$mean_posts_after)
ukr_sd_ukr_after <- sd(ukr_pro_ukr$mean_posts_after)
ukr_sd_ran_after <- sd(ukr_pro_ran$mean_posts_after)
ukr_sd_all_after <- sd(ukr_t$mean_posts_after)

#### counting number of posts after the ban in Mainland Ukraine for each sub-group
ukr_sum_rus_after <- nrow(posts_ukr_after[posts_ukr_after$rus == 1, ])
ukr_sum_ukr_after <- nrow(posts_ukr_after[posts_ukr_after$ukr == 1, ])
ukr_sum_ran_after <- nrow(posts_ukr_after[posts_ukr_after$ran == 1, ])
ukr_sum_all_after <- nrow(posts_ukr_after)


####################Computing values only for Crimea###################################

### mean number of posts before censorship for the different groups in the subsample of users
crim_posts_rus_before <- mean(crim_pro_rus$sum_posts_before)/90
crim_posts_ukr_before <- mean(crim_pro_ukr$sum_posts_before)/90
crim_posts_ran_before <- mean(crim_pro_ran$sum_posts_before)/90
crim_posts_all_before <- mean(crim_t$sum_posts_before)/90


### sd for posts before censorship for the different groups in the subsample of users
crim_sd_rus_before <- sd(crim_pro_rus$mean_posts_before)
crim_sd_ukr_before <- sd(crim_pro_ukr$mean_posts_before)
crim_sd_ran_before <- sd(crim_pro_ran$mean_posts_before)
crim_sd_all_before <- sd(crim_t$mean_posts_before)

#### counting number of posts before the ban in Crimea for each sub-group
crim_sum_rus_before <- nrow(posts_crim_before[posts_crim_before$rus == 1, ])
crim_sum_ukr_before <- nrow(posts_crim_before[posts_crim_before$ukr == 1, ])
crim_sum_ran_before <- nrow(posts_crim_before[posts_crim_before$ran == 1, ])
crim_sum_all_before <- nrow(posts_crim_before)


### mean number of posts after censorship for the different groups in the subsample of users
crim_posts_rus_after <- mean(crim_pro_rus$sum_posts_after)/91
crim_posts_ukr_after <- mean(crim_pro_ukr$sum_posts_after)/91
crim_posts_ran_after <- mean(crim_pro_ran$sum_posts_after)/91
crim_posts_all_after <- mean(crim_t$sum_posts_after)/91

### sd for posts after censorship for the different groups in the subsample of users
crim_sd_rus_after <- sd(crim_pro_rus$mean_posts_after)
crim_sd_ukr_after <- sd(crim_pro_ukr$mean_posts_after)
crim_sd_ran_after <- sd(crim_pro_ran$mean_posts_after)
crim_sd_all_after <- sd(crim_t$mean_posts_after)

#### counting number of posts after the ban in Crimea for each sub-group
crim_sum_rus_after <- nrow(posts_crim_after[posts_crim_after$rus == 1, ])
crim_sum_ukr_after <- nrow(posts_crim_after[posts_crim_after$ukr == 1, ])
crim_sum_ran_after <- nrow(posts_crim_after[posts_crim_after$ran == 1, ])
crim_sum_all_after <- nrow(posts_crim_after)


###############Counting total number of posts before and after for Crimea and Ukraine#########################
### Ukraine
### sum of posts after censorship for the different groups in the subsample of users
ukr_sum_rus <- ukr_sum_rus_before + ukr_sum_rus_after
ukr_sum_ukr <- ukr_sum_ukr_before + ukr_sum_ukr_after
ukr_sum_ran <- ukr_sum_ran_before + ukr_sum_ran_after
ukr_sum_all <- ukr_sum_all_before + ukr_sum_all_after

### Crimea
### sum of posts after censorship for the different groups in the subsample of users
crim_sum_rus <- crim_sum_rus_before + crim_sum_rus_after
crim_sum_ukr <- crim_sum_ukr_before + crim_sum_ukr_after
crim_sum_ran <- crim_sum_ran_before + crim_sum_ran_after
crim_sum_all <- crim_sum_all_before + crim_sum_all_after




##################### Making a table for both Mainland and Crimea############

### making a table for posting activity in Mainland
table3 <- data.frame(category = category,
                     Nposts = c(ukr_sum_rus,
                                ukr_sum_ukr,
                                ukr_sum_ran,
                                ukr_sum_all),
                     mean_before_ukr = c(ukr_posts_rus_before,
                                         ukr_posts_ukr_before,
                                         ukr_posts_ran_before, 
                                         ukr_posts_all_before),
                     sd_before_ukr = c(ukr_sd_rus_before,
                                       ukr_sd_ukr_before,
                                       ukr_sd_ran_before, 
                                       ukr_sd_all_before),
                     mean_after_ukr = c(ukr_posts_rus_after,
                                        ukr_posts_ukr_after,
                                        ukr_posts_ran_after, 
                                        ukr_posts_all_after),
                     sd_after_ukr = c(ukr_sd_rus_after,
                                      ukr_sd_ukr_after,
                                      ukr_sd_ran_after, 
                                      ukr_sd_all_after))

### making a table for posting activity in Crimea
table4 <- data.frame(category = category,
                     Nposts = c(crim_sum_rus,
                                crim_sum_ukr,
                                crim_sum_ran,
                                crim_sum_all),
                     mean_before_crim = c(crim_posts_rus_before,
                                          crim_posts_ukr_before,
                                          crim_posts_ran_before, 
                                          crim_posts_all_before),
                     sd_before_crim = c(crim_sd_rus_before,
                                        crim_sd_ukr_before,
                                        crim_sd_ran_before, 
                                        crim_sd_all_before),
                     mean_after_crim= c(crim_posts_rus_after,
                                        crim_posts_ukr_after,
                                        crim_posts_ran_after, 
                                        crim_posts_all_after),
                     sd_after_crim = c(crim_sd_rus_after,
                                       crim_sd_ukr_after,
                                       crim_sd_ran_after, 
                                       crim_sd_all_after))


### Creating a Latex table from Mainland and Crimea
stargazer(table3, table4, 
          type = "text",
          summary = F,
          rownames = F, 
          title = c("Mainland Ukraine", "Crimea"),
          align = T,
          covariate.labels = c("Group", "Nposts", "Mean daily posts (before)", "SD", "Mean daily (after)", "SD"), 
          single.row = F, 
          column.sep.width = "0.5pt")



##########################################################################################################
#################### Figure B2: Posting activity per week before and after the ban #######################
##########################################################################################################
# median proportion of friends (in Mainland)
ua_users <- long.panel.clean %>%
  distinct(owner_id, .keep_all = T) %>%
  filter(border == "Mainland Ukraine")

median_mainland <- median(ua_users$prop_rus_friends)

# median proportion of friends (in Crimea)
crim_users <- long.panel.clean %>%
  distinct(owner_id, .keep_all = T) %>%
  filter(border == "Crimea")

median_crimea <- median(crim_users$prop_rus_friends)


### creating a binary variable indicating whether the  users have above LOCAL  median proportion of friends in Russia
walls.clean.2$above_median.local <- 0
walls.clean.2$above_median.local [walls.clean.2$border == "Mainland Ukraine" & walls.clean.2$prop_rus_friends > median_mainland] <- 1 # based on median for users in Mainland
walls.clean.2$above_median.local [walls.clean.2$border == "Crimea" & walls.clean.2$prop_rus_friends > median_crimea] <- 1 #based on median for users in Crimea


### creating a binary variable indicating whether the  users have up to LOCAL  median proportion of friends in Russia
walls.clean.2$below_median.local <- 0
walls.clean.2$below_median.local [walls.clean.2$border == "Mainland Ukraine" & walls.clean.2$prop_rus_friends <= median_mainland] <- 1 # based on median for users in Mainland
walls.clean.2$below_median.local [walls.clean.2$border == "Crimea" & walls.clean.2$prop_rus_friends <= median_crimea] <- 1 #based on median for users in Crimea



### creating a week variable
weeks.activity <- walls.clean.2 %>%
  mutate(weeks = round((days1-3)/7)) %>%
  ungroup() %>%
  group_by(weeks, border) %>%
  mutate(sum_ukr = sum(ukr), 
         sum_rus = sum(rus), 
         sum_ran =sum(ran),
         sum_above = sum(above_median.local),
         sum_below = sum(below_median.local)) %>%
  distinct(weeks, .keep_all = T)



#ordering the border variable so that "Mainland Ukraine" appears first in the graph
weeks.activity$border <- factor(weeks.activity$border,levels=c("Mainland Ukraine","Crimea"))

#### vizualising proportion of abandoned accounts in Crimea and Ukraine
ggplot(weeks.activity) +
  geom_point(aes(weeks, sum_rus, color =  "Pro-Russian"), size = 1.5, alpha = 0.8) +
  geom_point(aes(weeks, sum_ukr, color = "Pro-Ukrainian"), size = 1.5, alpha = 0.8) +
  geom_point(aes(weeks, sum_ran, color =  "Random"), size = 1.5, alpha = 0.8) +
  scale_x_continuous(limits = c(-20,20)) +
  scale_y_continuous(limits = c(0,1500)) +
  scale_colour_manual(values=c("black", "dark grey", "light grey"))+
  #                    labels = c("Crimea", "Mainland Ukraine")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Weeks before and after censorship",
       y = "Number of wall posts", 
       color = "Group") +
  theme(strip.text = element_text(size=80)) + 
  theme_tufte(ticks = F) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=12)) +
  facet_wrap(~border, nrow = 2) + 
  theme(strip.text = element_text(size=15))





##########################################################################################################
##########################################################################################################
########### Functions for selecting sub-samples and running models for DD and DDD#########################
##########################################################################################################
##########################################################################################################

##########################################################################################################
#############Functions for Expanding bandwidths on both sides of the cut-off for DD and DDD################
###########################################################################################################

### creating a function that selects the desired bandwidth
bw_trim <- function(df, bw){
  df <- df %>% filter(days1 >= -bw,  # selecting the minimum bandwidth range
                      days1 <= bw)   # selecting the maximum bandwidth range
  df
  
}

### creating a function that returns (non-cluster-robust DD or DDD for the long.planel.clean variable)

dif <- function(df, est){
  ### difference between Mainland and Crimea
  if(est == "after:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*ua_border + rus + above_median_ru.pol + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model
  }
  ### Difference between Many and few friends in Russia (for users in Mainland)
  else if(est == "after:above_median_ru.pol"){
    ### keeping only pro-Russian and pro-Ukrainian users in Mainalnd
    df <- df %>%
      filter(ua_border == 1)
    # running the regression
    model <- lm(posts_days ~ after*above_median_ru.pol +rus + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model
  }
  ### Difference between Pro-Russian and Pro-Ukrainian users (for users in Mainland)
  else if(est == "after:rus"){
    ### keeping only pro-Russian and pro-Ukrainian users in Mainland
    df <- df %>%
      filter(ua_border == 1)
    # running the regression
    model <- lm(posts_days ~ after*rus + above_median_ru.pol +sex + sum_friends + weekend + days1, data = df)
    model
  }
  ### Difference  between Many and few friends in Russia with Crimean users as a control (DDD)
  else if(est == "after:above_median_ru.pol:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*above_median_ru.pol*ua_border + rus + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model
  }
  ### Difference between Pro-Russian and Pro-Ukrainian users with Crimean users as a control (DDD)
  else if(est == "after:rus:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*rus*ua_border + ran +above_median_ru.pol + sex + sum_friends + weekend + days1, data = df)
    model
  }
  else{
    # returning an error message
    paste("*Error* Please indicate which estimate to return: 'after:ua_border', 'after:above_median_ru.pol', 'after:rus',
          'after:above_median_ru.pol:ua_border', or 'after:rus:ua_border'")
  }
  
}



### creating a function that returns a cluster robust DD or DDD for the long.planel.clean variable

dif_robust <- function(df, est){
  ### difference between Mainland and Crimea
  if(est == "after:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*ua_border + rus + above_median_ru.pol + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model.robust <- coeftest(model, vcov = vcovHC(model))
    model.robust
  }
  ### Difference between Many and few friends in Russia (for users in Mainland)
  else if(est == "after:above_median_ru.pol"){
    ### keeping only pro-Russian and pro-Ukrainian users in Mainalnd
    df <- df %>%
      filter(ua_border == 1)
    # running the regression
    model <- lm(posts_days ~ after*above_median_ru.pol +rus + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model.robust <- coeftest(model, vcov = vcovHC(model))
    model.robust
  }
  ### Difference between Pro-Russian and Pro-Ukrainian users (for users in Mainland)
  else if(est == "after:rus"){
    ### keeping only pro-Russian and pro-Ukrainian users in Mainland
    df <- df %>%
      filter(ua_border == 1)
    # running the regression
    model <- lm(posts_days ~ after*rus + above_median_ru.pol +sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model.robust <- coeftest(model, vcov = vcovHC(model))
    model.robust
  }
  ### Difference  between Many and few friends in Russia with Crimean users as a control (DDD)
  else if(est == "after:above_median_ru.pol:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*above_median_ru.pol*ua_border + rus + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model.robust <- coeftest(model, vcov = vcovHC(model))
    model.robust
  }
  ### Difference between Pro-Russian and Pro-Ukrainian users with Crimean users as a control (DDD)
  else if(est == "after:rus:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*rus*ua_border + ran +above_median_ru.pol + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model.robust <- coeftest(model, vcov = vcovHC(model))
    model.robust
  }
  else{
    # returning an error message
    paste("*Error* Please indicate which estimate to return: 'after:ua_border', 'after:above_median_ru.pol', 'after:rus',
          'after:above_median_ru.pol:ua_border', or 'after:rus:ua_border'")
  }
  
}





### creating a function that returns core estimates in 
tidy_dif <- function(model, est, bw){
  # Making the DD regression compatible with Tidy package and selecting the variables of interest
  m_short <- tidy(model) %>%
    filter(term == est) %>%
    mutate(model = as.character(bw))
  return(m_short)
}


### creating a function that takes dataframe as input from long.panel.clean.pol and returns estimates for one DD or DDD 
dif_est <- function(df, est, bw) {
  df <- bw_trim(df, bw) 
  model <- dif_robust(df, est)
  estimates <- tidy_dif(model, est, bw)
  estimates
}

### creating a function that takes dataframe as input and returns estimates for the different DD and DDD models depending on the bandwidth


dif_est_loop <- function(df, est, bw) {
  df.1 <- data.frame()
  count = bw/10
  for (i in 1:count) {
    i_10 = i *10
    est.b <- dif_est(df, est, i_10)
    df.1 <- rbind(df.1, est.b)
    #print(paste(i, "processing bandwidth =", i, sep = " "))
  }
  
  df.1
  
}

#####################################################################################################################################################
######################### Functions for Expanding bandwidths on one side of the cut-off (days >=0) for DD and DDD####################################
#####################################################################################################################################################

### creating a function that selects the desired bandwidth

bw_trim_fixed <- function(df, bw_fixed, bw){
  df <- df %>% filter(days1 >= bw_fixed,  # selecting the minimum bandwidth range - this range will be held constant
                      days1 <= bw)   # selecting the maximum bandwidth range
  df
  
}

### creating a function that takes dataframe as input from long.panel.clean.pol and returns estimates for one DD or DDD 
dif_est_fixed <- function(df, est, bw_fixed, bw) {
  df <- bw_trim_fixed(df, bw_fixed, bw) 
  model <- dif_robust(df, est)
  estimates <- tidy_dif(model, est, bw)
  estimates
}

### creating a function that takes dataframe as input and returns estimates for the different DD and DDD models depending on the bandwidth


dif_est_loop_fixed <- function(df, est, bw_fixed, bw) {
  df.1 <- data.frame()
  count = bw/10
  for (i in 1:count) {
    i_10 = i *10
    est.b <- dif_est_fixed(df, est,bw_fixed, i_10)
    df.1 <- rbind(df.1, est.b)
    #print(paste(i, "processing bandwidth =", i, sep = " "))
  }
  
  df.1
  
}



##########################################################################################################
####### Creating a data-frame with time-user observations and a bandwidith of 400 days on both sides #####
###### Figure L1: Change in mean number of posts per day after the ban (95% confidence intervals) ########
##########################################################################################################

### creating a data set
activity_per_day <- walls.clean.2 %>%
  ungroup() %>%
  group_by(days1, owner_id) %>%
  mutate(posts_days = n()) %>%
  distinct(owner_id, .keep_all = T) %>%
  ungroup() %>%
  select(owner_id, days1, posts_days)
### adding 0 values for the days when the user did not post anytning

activity_per_day1.1 <- activity_per_day
g.1 <- with(activity_per_day1.1, expand.grid(days1 = seq(-400, 400), owner_id = unique(owner_id), posts_days = 0)) #
activity_per_day1.1 <- rbind(activity_per_day1.1, g.1) #
wide.1 <- dcast(activity_per_day1.1, days1 ~ owner_id, fill = 0, fun = sum, value.var = "posts_days")
long.1 <- melt(wide.1, id = "days1", variable.name = "owner_id", value.name = "posts_days")
long.1$owner_id <- as.numeric(as.character(long.1$owner_id)) # converting from factor to numeric
head(long.1$owner_id)
###create a list of variables for each distinct individual in walls.clean.2
walls.clean.2.unique.1 <- walls.clean.2 %>%
  ungroup() %>%
  distinct(owner_id, .keep_all = T) 
### removing reduntant variables
walls.clean.2.unique.1$days1 <- NULL
walls.clean.2.unique.1$days <- NULL
walls.clean.2.unique.1$weeks <- NULL

### adding the user metadata to the paneldata
long.panel.original.1 <- left_join(long.1, walls.clean.2.unique.1) 

### clearing up memory
rm(activity_per_day1.1, g.1, wide.1, long.1)

########################### 90 days threshold ###########################

### filtered panel data frame
long.panel.1 <- long.panel.original.1 %>%
  filter(days1 >= -400, # selecting posts within 90 before and after censorship
         days1 <= 400)


#### Creating a dummy variable which indicates whether data point is from before or after censorship
long.panel.1$before <- 0
long.panel.1$before[long.panel.1$days1.1 < 0 & long.panel.1$days1 >=-400] <- 1

long.panel.1$after <-0
long.panel.1$after[long.panel.1$days1 >= 0 & long.panel.1$days1 <= 400] <- 1

### creating a dummy variable indicating wether the post is from Mainland Ukraine or not
long.panel.1$ua_border <- 0
long.panel.1$ua_border[long.panel.1$border == "Mainland Ukraine"] <-1



### creating a binary variable indicating whether the users have up to LOCAL median proportion of friends in Russia
long.panel.1$above_median_ru <- 0
long.panel.1$above_median_ru[long.panel.1$ua_border == 0 & long.panel.1$prop_rus_friends > median_rus] <-1 # 1 if users living in Crimea have above median proportion for that region
long.panel.1$above_median_ru[long.panel.1$ua_border == 1 & long.panel.1$prop_rus_friends > median_ua] <-1 # 1 if users living in Mainland Ukraine have above median proportion for that region

### creating a binary variable indicating whether the users have up to median proportion of friends in Russia or Not
long.panel.1$above_median_ru_all <- 0
long.panel.1$above_median_ru_all[long.panel.1$prop_rus_friends > median_all] <-1 # 1 if users living in Crimea have above median proportion for that region

### creating a binary variable indicating whether the  users have above LOCAL POLITICAL median proportion of friends in Russia
long.panel.1$above_median_ru.pol <- 0
long.panel.1$above_median_ru.pol[long.panel.1$ua_border == 0 & long.panel.1$prop_rus_friends > median_rus.pol] <-1 # 1 if users living in Crimea have above median proportion for that region
long.panel.1$above_median_ru.pol[long.panel.1$ua_border == 1 & long.panel.1$prop_rus_friends > median_ua.pol] <-1 # 1 if users living in Mainland Ukraine have above median proportion for that region

### creating a binary variable indicating whether the POLITICAL users have above median proportion of friends in Russia or Not
long.panel.1$above_median_ru_all.pol <- 0
long.panel.1$above_median_ru_all.pol[long.panel.1$prop_rus_friends >= median_all.pol] <-1 # 1 if users living in Crimea have above median proportion for that region

### examining the data and removing outliers # NOT IMPLEMENTING THIS IN THE FINAL ANALYSIS BECAUSE IT DOES NOT AFFECT THE RESULTS
long.panel.clean.1 <- long.panel.1 %>%
  arrange(posts_days) #%>%
#filter(posts_days <=5)

### dummy variable indicating wether the user is male or not
long.panel.clean.1$sex[long.panel.clean.1$sex == 2] <- 0

### keeping only pro-Russian and pro-Ukrainian users 
long.panel.clean.pol.1 <- long.panel.clean.1 %>%
  filter(ran == 0)


### For the paper draft

### keeping only users in Ukraine
long.panel.clean.ua.1 <- long.panel.clean.1 %>%
  filter(ua_border == 1)

### keeping only pro-Russian and pro-Ukrainian users in Ukraine
long.panel.clean.ua.pol.1 <- long.panel.clean.1 %>%
  filter(ua_border == 1,
         ran == 0)




########################################################################################################################
##########Holding pre-treatment bandwidth fixed while expanding post-treatment bandwidth################################
########################################################################################################################
m1_fixed <- dif_est_loop_fixed(long.panel.clean.pol.1, "after:ua_border",bw_fixed = -30, bw = 400)
m2_fixed <- dif_est_loop_fixed(long.panel.clean.pol.1, "after:above_median_ru.pol", bw_fixed = -30, bw = 400)
m3_fixed <- dif_est_loop_fixed(long.panel.clean.pol.1, "after:rus", bw_fixed = -30, bw = 400)
m4_fixed <- dif_est_loop_fixed(long.panel.clean.pol.1, "after:above_median_ru.pol:ua_border", bw_fixed = -30, bw = 400)
m5_fixed <- dif_est_loop_fixed(long.panel.clean.pol.1, "after:rus:ua_border", bw_fixed = -30, bw = 400)

# combinging the models
m_fixed <- rbind(m1_fixed, m2_fixed, m3_fixed, 
                 m4_fixed, m5_fixed)
m_fixed$model <- as.numeric(m_fixed$model)


m_fixed$term <- str_replace(m_fixed$term, ":", "_")
m_fixed$term <- str_replace(m_fixed$term, ":", "_")

m_fixed <- m_fixed %>%
  relabel_predictors(c(
    after_ua_border = "Ban x Mainland_      ",
    after_above_median_ru.pol = "Ban x Friends in Russia_      ", 
    after_rus = "Ban x Pro-Russian_  ", 
    after_above_median_ru.pol_ua_border = "Ban x Friends in Russia x_      Mainland",
    after_rus_ua_border = "Ban x Pro-Russian x_      Mainland"))

### vizualising models
levels(m_fixed$term) <- gsub("_", "\n", levels(m_fixed$term))


small_multiple(m_fixed) +
  theme_tufte() + ## addbase_family = "sans-serif" for the exact same font as in the paper
  ylab("Coefficient Estimate") +
  xlab("Days after the ban") + 
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text = element_text(size = 10, face  = "bold"),
        #axis.text=element_text(size= 10),
        axis.title=element_text(size=12, face = "bold")) +
  scale_y_continuous(limits = c(-0.135, 0.05))



##########################################################################################################
############## Figure N1: Regression discontinuity in posting activity 90 days before ####################
############## and after the ban(95% confidence interval) ################################################
##########################################################################################################

#### computing numbers of users with above median proportion of friends in Russia
# in Mainland
n_above_median_ua <- nrow(long.panel.clean %>%
                            distinct(owner_id, .keep_all = T) %>%
                            filter(border == "Mainland Ukraine",
                                   prop_rus_friends >median_mainland))



# in Crimea
n_above_median_crim <- nrow(long.panel.clean %>%
                              distinct(owner_id, .keep_all = T) %>%
                              filter(border == "Crimea",
                                     prop_rus_friends >median_crimea))




walls.clean.ua.days <- long.panel.clean.ua %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_ukr_all) %>%
  distinct(days1, .keep_all = T) %>%# ensuring that there is only one row per day 
  select(days1, after, posts_mean)

### pro-russian
walls.clean.ua.rus.days <- long.panel.clean.ua %>%
  filter(rus == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_ukr_pro_rus) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean)

### pro-Ukrainian
walls.clean.ua.ua.days <- long.panel.clean.ua %>%
  filter(ukr == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_ukr_pro_ukr) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean) 

### Random-users
walls.clean.ua.ran.days <- long.panel.clean.ua %>%
  filter(ran == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_ukr_pro_ran) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean) 


### Above median
walls.clean.ua.above.days <- long.panel.clean.ua %>%
  filter(above_median_ru == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_above_median_ua) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean) 

### Up to median
walls.clean.ua.below.days <- long.panel.clean.ua %>%
  filter(above_median_ru == 0) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/(n_ukr_all - n_above_median_ua)) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean) 


### Naming the different datasets
walls.clean.ua.days$group <- "All"
walls.clean.ua.rus.days$group <- "Pro-Russian"
walls.clean.ua.ua.days$group <- "Pro-Ukrainian"  
walls.clean.ua.ran.days$group <- "Random"
walls.clean.ua.above.days$group <- "Above median"
walls.clean.ua.below.days$group <- "Up to median"

### Combinging the three datasets  
bw90_combined <- rbind(walls.clean.ua.days,
                       walls.clean.ua.rus.days,
                       walls.clean.ua.ua.days,
                       walls.clean.ua.ran.days,
                       walls.clean.ua.above.days,
                       walls.clean.ua.below.days)

bw90_combined$after <- factor(bw90_combined$after)    

### vizualising all of the models
# changing the order of the models to organize the facet_wrap output
bw90_combined$group <- factor(bw90_combined$group,levels=c("All",
                                                           "Above median",
                                                           "Up to median",
                                                           "Random",
                                                           "Pro-Russian",
                                                           "Pro-Ukrainian"))

ggplot(bw90_combined, aes(x = days1, y = posts_mean, color = after)) + 
  geom_point() + 
  #  scale_color_brewer(NULL, type = 'qual', palette = 6) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  #theme_tufte() +
  labs(x = "Days before and after censorship",
       y = "Mean number of wall posts per user", 
       color = "Time period") +
  stat_smooth(method = "lm", level = 0.95) +
  scale_colour_manual(values=c("grey", "black"),
                      labels = c("Not censored", "Censored")) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=12)) + 
  facet_wrap(~group, nrow = 2)




##########################################################################################################
##########################################################################################################
########################### Regression tables J2, J3, L1, L2, N1, N2   ###################################
##########################################################################################################
##########################################################################################################


##################################################################################
###### Table J2: Change in posting activity after the ban (90 days bandwidth)#####
#####Cluster Robust DD and DDD####################################################
##################################################################################

### creating the DD and DDD models models
# selecting dataset with 90 days banwidth
long.panel.clean.pol.90 <- bw_trim(long.panel.clean.pol, 90)

# runing cluster-robust models models
m1_90.r <- dif_robust(long.panel.clean.pol.90, "after:ua_border")
m2_90.r <- dif_robust(long.panel.clean.pol.90, "after:above_median_ru.pol")
m3_90.r <- dif_robust(long.panel.clean.pol.90, "after:rus")
m4_90.r <- dif_robust(long.panel.clean.pol.90, "after:above_median_ru.pol:ua_border")
m5_90.r <- dif_robust(long.panel.clean.pol.90, "after:rus:ua_border")

# runing (non-cluster-robust models) models
m1_90. <- dif(long.panel.clean.pol.90, "after:ua_border")
m2_90. <- dif(long.panel.clean.pol.90, "after:above_median_ru.pol")
m3_90. <- dif(long.panel.clean.pol.90, "after:rus")
m4_90. <- dif(long.panel.clean.pol.90, "after:above_median_ru.pol:ua_border")
m5_90. <- dif(long.panel.clean.pol.90, "after:rus:ua_border")


### combining the models into one table
stargazer(m1_90.r, m2_90.r, m3_90.r, m4_90.r,
          m5_90.r,
          title="Change in posting activity after the ban (90 days bandwidth)",
          align = F,
          dep.var.labels=c("DD", "DD", "DD", "DDD", "DDD"),
          covariate.labels=c("Ban", "Mainland",
                             "Pro-Russian", "Friends in Russia",
                             "Male", "Number of friends", "Weekend", "Days after the ban", "Ban*Mainland",
                             "Friends in Russia*Mainland",
                             "Ban*Friends in Russia*Mainland",
                             "Ban*Friends in Russia", "Pro-Russian*Mainland",
                             "Ban*Pro-Russian*Mainland", "Ban*Pro-Russian"),
          omit.stat=c("LL","ser","f"),
          no.space = T, type = "text")


##################################################################################
######Table J3: Change in posting activity after the ban (30 days bandwidth) #####
#####Cluster Robust DD and DDD#####################################################
##################################################################################


# selecting dataset with 30 days banwidth
long.panel.clean.pol.30 <- bw_trim(long.panel.clean.pol, 30)

# runing cluster-robust models
m1_30.r <- dif_robust(long.panel.clean.pol.30, "after:ua_border")
m2_30.r <- dif_robust(long.panel.clean.pol.30, "after:above_median_ru.pol")
m3_30.r <- dif_robust(long.panel.clean.pol.30, "after:rus")
m4_30.r <- dif_robust(long.panel.clean.pol.30, "after:above_median_ru.pol:ua_border")
m5_30.r <- dif_robust(long.panel.clean.pol.30, "after:rus:ua_border")


# runing (non-cluster-robust models) models
m1_30. <- dif(long.panel.clean.pol.30, "after:ua_border")
m2_30. <- dif(long.panel.clean.pol.30, "after:above_median_ru.pol")
m3_30. <- dif(long.panel.clean.pol.30, "after:rus")
m4_30. <- dif(long.panel.clean.pol.30, "after:above_median_ru.pol:ua_border")
m5_30. <- dif(long.panel.clean.pol.30, "after:rus:ua_border")

### combining the models into one table
stargazer(m1_30.r, m2_30.r, m3_30.r, m4_30.r,
          m5_30.r,
          title="Change in posting activity after the ban (30 days bandwidth)",
          align = F,
          dep.var.labels=c("DD", "DD", "DD", "DDD", "DDD"),
          covariate.labels=c("Ban", "Mainland",
                             "Pro-Russian", "Friends in Russia",
                             "Male", "Number of friends", "Weekend", "Days after the ban", "Ban*Mainland",
                             "Friends in Russia*Mainland",
                             "Ban*Friends in Russia*Mainland",
                             "Ban*Friends in Russia", "Pro-Russian*Mainland",
                             "Ban*Pro-Russian*Mainland", "Ban*Pro-Russian"),
          omit.stat=c("LL","ser","f"),
          no.space = T, type = "latex")


##########################################################################################################
######################### DD and DDD with fixed pre-treatment bandwidth #################################
##########################################################################################################


###################################################################################
############ Table L1: Change in posting activity##################################
############ Timerange: from 30 days prior to 200 days after the ban) #############
########### Cluster Robust DD and DDD##############################################
###################################################################################


# selecting dataset within a time period of 30 days prior to the ban and 200 days post-ban
long.panel.clean.pol.fixed.200 <- bw_trim_fixed(long.panel.clean.pol.1, bw_fixed = -30, bw = 200)

# runing cluster-robust models models
m1_200.f <- dif_robust(long.panel.clean.pol.fixed.200, "after:ua_border")
m2_200.f <- dif_robust(long.panel.clean.pol.fixed.200, "after:above_median_ru.pol")
m3_200.f <- dif_robust(long.panel.clean.pol.fixed.200, "after:rus")
m4_200.f <- dif_robust(long.panel.clean.pol.fixed.200, "after:above_median_ru.pol:ua_border")
m5_200.f <- dif_robust(long.panel.clean.pol.fixed.200, "after:rus:ua_border")

### combinging the models into one table
stargazer(m1_200.f, m2_200.f, m3_200.f, m4_200.f,
          m5_200.f,
          title="Change in posting activity (Timerange: from 30 days prior to 200 days after the ban)",
          align = F,
          dep.var.labels=c("DD", "DD", "DD", "DDD", "DDD"),
          covariate.labels=c("Ban", "Mainland",
                             "Pro-Russian", "Friends in Russia",
                             "Male", "Number of friends", "Weekend", "Days after the ban", "Ban*Mainland",
                             "Friends in Russia*Mainland",
                             "Ban*Friends in Russia*Mainland",
                             "Ban*Friends in Russia", "Pro-Russian*Mainland",
                             "Ban*Pro-Russian*Mainland", "Ban*Pro-Russian"),
          omit.stat=c("LL","ser","f"),
          no.space = T, type = "latex")


###################################################################################
############ Table L2: Change in posting activity##################################
############ (Timerange: from 30 days prior to 400 days after the ban)#############
########### Cluster Robust DD and DDD##############################################
###################################################################################

# selecting dataset within a time period of 30 days prior to the ban and 400 days post-ban
long.panel.clean.pol.fixed.400 <- bw_trim_fixed(long.panel.clean.pol.1, bw_fixed = -30, bw = 400)

# runing cluster-robust models models
m1_400.f <- dif_robust(long.panel.clean.pol.fixed.400, "after:ua_border")
m2_400.f <- dif_robust(long.panel.clean.pol.fixed.400, "after:above_median_ru.pol")
m3_400.f <- dif_robust(long.panel.clean.pol.fixed.400, "after:rus")
m4_400.f <- dif_robust(long.panel.clean.pol.fixed.400, "after:above_median_ru.pol:ua_border")
m5_400.f <- dif_robust(long.panel.clean.pol.fixed.400, "after:rus:ua_border")

### combining the models into one table
stargazer(m1_400.f, m2_400.f, m3_400.f, m4_400.f,
          m5_400.f,
          title="Change in posting activity (Timerange: from 30 days prior to 400 days after the ban)",
          align = F,
          dep.var.labels=c("DD", "DD", "DD", "DDD", "DDD"),
          covariate.labels=c("Ban", "Mainland",
                             "Pro-Russian", "Friends in Russia",
                             "Male", "Number of friends", "Weekend", "Days after the ban", "Ban*Mainland",
                             "Friends in Russia*Mainland",
                             "Ban*Friends in Russia*Mainland",
                             "Ban*Friends in Russia", "Pro-Russian*Mainland",
                             "Ban*Pro-Russian*Mainland", "Ban*Pro-Russian"),
          omit.stat=c("LL","ser","f"),
          no.space = T, type = "latex")


##########################################################################################################
########################### Regression Discontinuity in for disagregated data ############################
##########################################################################################################

reg.1.lm.days.d <- lm(posts_days ~ days1 + after + weekend, long.panel.clean.ua)
reg.1.lm.interaction.days.d <- lm(posts_days ~ days1*after + weekend, long.panel.clean.ua)

### creating regression only for pro-russian users
long.panel.clean.ua.rus <- long.panel.clean.ua %>%
  filter(rus == 1)
reg.2.lm <- lm(posts_days ~ days1*after + weekend, long.panel.clean.ua.rus)

### creating regression only for pro-ukrainian users
long.panel.clean.ua.ua <- long.panel.clean.ua %>%
  filter(ukr == 1)
reg.3.lm <- lm(posts_days ~ days1*after + weekend, long.panel.clean.ua.ua)

### creating regression only for users with above median proportion of users
long.panel.clean.ua.above <- long.panel.clean.ua %>%
  filter(above_median_ru == 1)
reg.4.lm <- lm(posts_days ~ days1*after + weekend, long.panel.clean.ua.above)

### creating regression only for users with up to median proportion of users
long.panel.clean.ua.below <- long.panel.clean.ua %>%
  filter(above_median_ru == 0)
reg.5.lm <- lm(posts_days ~ days1*after + weekend, long.panel.clean.ua.below)

### creating regression only for random users
long.panel.clean.ua.ran <- long.panel.clean.ua %>%
  filter(ran == 1)
reg.6.lm <- lm(posts_days ~ days1*after + weekend, long.panel.clean.ua.ran)

### Creating lables for the tables
labels <- c("All", "All", "Above median", "Up to median")
labels1 <- c("Above median", "Up to median", "Pro-Russian", "Pro-Ukrainian")

####################################################################################
####### Table N1: Regression discontinuity in no. of posts in Mainland #############
####### (90 days before and afterthe ban)###########################################
####################################################################################

### RD disagregated for 90 days bandwidth
stargazer(reg.1.lm.days.d, reg.1.lm.interaction.days.d, reg.4.lm, reg.5.lm,
          title="Regression discontinuity in wall post activity (Split sample)",
          align = T,
          dep.var.labels= labels,
          covariate.labels=c("Days","Ban", "Weekend", "Days*ban"),
          omit.stat=c("LL","ser","f"),
          no.space = T,
          type = "text")


####################################################################################
####### Table N2: Regression discontinuity in no. of posts in Mainland #############
####### (90 days before and afterthe ban)###########################################
####################################################################################

### RD disagregated for 90 days bandwidth
stargazer(reg.2.lm, reg.3.lm, reg.6.lm,
          title="Regression discontinuity in wall post activity (Split sample)",
          align = F,
          dep.var.labels=c("Pro-Russian", "Pro-Ukrainian", "Random"),
          covariate.labels=c("Days","Ban", "Weekend", "Days*ban"),
          omit.stat=c("LL","ser","f"),
          no.space = T,
          type = "text")





##########################################################################################################
##########################################################################################################
########## Regressions for all users in the subsample: Now also including random users ###################
########## Figure 2 and Table J1 #########################################################################
##########################################################################################################
##########################################################################################################


################################################################################################
################################ Modifying functions ###########################################
################################################################################################

### creating a function that returns a cluster robust DD or DDD for the long.planel.clean variable

dif_robust <- function(df, est){
  ### difference between Mainland and Crimea
  if(est == "after:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*ua_border + rus + ukr + above_median_ru_all + sex + sum_friends + weekend + days1, data = df)
    # adding cluster robust standard errors
    model.robust <- coeftest(model, vcov = vcovHC(model))
    model.robust
  }
  
  else{
    # returning an error message
    paste("*Error* Please indicate which estimate to return: 'after:ua_border', 'after:above_median_ru.pol', 'after:rus',
          'after:above_median_ru.pol:ua_border', or 'after:rus:ua_border'")
  }
  
}



dif <- function(df, est){
  ### difference between Mainland and Crimea
  if(est == "after:ua_border"){
    # running the regression
    model <- lm(posts_days ~ after*ua_border + rus + ukr + above_median_ru_all + sex + sum_friends + weekend + days1, data = df)
    model
  }
  
  else{
    # returning an error message
    paste("*Error* Please indicate which estimate to return: 'after:ua_border', 'after:above_median_ru.pol', 'after:rus',
          'after:above_median_ru.pol:ua_border', or 'after:rus:ua_border'")
  }
}



################################################################################################
######### Table J1: Change in posting activity after the ban ###################################
######## (for all users in the sub-sample) #####################################################
################################################################################################


### cluster-robust models
# 90 days bandwidth
long.panel.clean.90 <- bw_trim(long.panel.clean,  bw = 90)
m1_90.all.r <- dif_robust(long.panel.clean.90, "after:ua_border")

# 30 days bandwidth
long.panel.clean.30 <- bw_trim(long.panel.clean, 30)
m1_30.all.r <- dif_robust(long.panel.clean.30, "after:ua_border")


### combining the models into one table
stargazer(m1_90.all.r, m1_30.all.r,
          title="Change in posting activity after the ban (for all users in the sub-sample)",
          align = F,
          dep.var.labels=c("90 days", "30 days"),
          covariate.labels=c("Ban", "Mainland",
                             "Pro-Russian", "Pro-Ukrainian", "Friends in Russia",
                             "Male", "Number of friends", "Weekend", "Days after the ban", "Ban*Mainland"),
          omit.stat=c("LL","ser","f"),
          no.space = T, type = "text")







######################################################################################################################
##################### Validating the analysing by including tie strength##############################################
#################### Table M1: Change in posting activity after the ban (DD and DDD)##################################
######################################################################################################################


#### loading the edgelist where each row represents a friendship link between two users 
unique.users.ties <- read.csv( "unique_user_ties.csv", header = T) 

### converting the variales in question into dummy
unique.users.ties$user_ties_rus.d <- 0
unique.users.ties$user_ties_rus.d[unique.users.ties$user_ties_rus ==  "At least 1 strong tie in Russia"] <- 1

unique.users.ties$user_ties_rus_raw.d <- 0 
unique.users.ties$user_ties_rus_raw.d[unique.users.ties$user_ties_rus_raw == "At least 5 posts from Russia"] <- 1

### creating dataset only with users from mainlaind
long.panel.clean.ua.pol.30 <- long.panel.clean.pol.30 %>%
  filter(border == "Mainland Ukraine")

long.panel.clean.ua.pol.90 <- long.panel.clean.pol.90 %>%
  filter(border == "Mainland Ukraine")


### adding the variables to the existing dataframes
# 30 days banwidth
long.panel.clean.ua.pol.30.ties <- left_join(long.panel.clean.ua.pol.30, unique.users.ties) 
long.panel.clean.pol.30.ties <- left_join(long.panel.clean.pol.30, unique.users.ties)
# 90 days banwidth
long.panel.clean.ua.pol.90.ties <- left_join(long.panel.clean.ua.pol.90, unique.users.ties)
long.panel.clean.pol.90.ties <- left_join(long.panel.clean.pol.90, unique.users.ties)

### replacing NAS with 0's
long.panel.clean.ua.pol.30.ties$user_ties_rus.d[is.na(long.panel.clean.ua.pol.30.ties$user_ties_rus.d)] <- 0
long.panel.clean.pol.30.ties$user_ties_rus.d[is.na(long.panel.clean.pol.30.ties$user_ties_rus.d)] <- 0
long.panel.clean.ua.pol.90.ties$user_ties_rus.d[is.na(long.panel.clean.ua.pol.90.ties$user_ties_rus.d)] <- 0
long.panel.clean.pol.90.ties$user_ties_rus.d[is.na(long.panel.clean.pol.90.ties$user_ties_rus.d)] <- 0


### using a dummy variable indicating whether there is at least 1 strong tie in Russia () instead of
m1.ties.30 <- lm(posts_days ~ after*user_ties_rus.d + rus + sex + sum_friends + weekend, data = long.panel.clean.ua.pol.30.ties)
m2.ties.30 <- lm(posts_days ~ after*user_ties_rus.d*ua_border + rus + sex + sum_friends + weekend, data = long.panel.clean.pol.30.ties)
m1.ties.90 <- lm(posts_days ~ after*user_ties_rus.d + rus + sex + sum_friends + weekend, data = long.panel.clean.ua.pol.90.ties)
m2.ties.90 <- lm(posts_days ~ after*user_ties_rus.d*ua_border + rus + sex + sum_friends + weekend, data = long.panel.clean.pol.90.ties)



### cluster robust standaard errors
m1.ties.30.r <- coeftest(m1.ties.30, vcov = vcovHC(m1.ties.30))
m2.ties.30.r <- coeftest(m2.ties.30, vcov = vcovHC(m2.ties.30))
m1.ties.90.r <- coeftest(m1.ties.90, vcov = vcovHC(m1.ties.30))
m2.ties.90.r <- coeftest(m2.ties.90, vcov = vcovHC(m2.ties.30))



###  DDD Table for the first draft 90 days bandwidth
stargazer(m1.ties.30.r, m2.ties.30.r, m1.ties.90.r, m2.ties.90.r,
          title="Change in posting activity after the ban (DD and DDD)",
          align = F,
          dep.var.labels=c("30 days", "90 days"),
          covariate.labels=c("Ban",
                             "Friends in Russia",
                             "Mainland",
                             "Pro-Russian",
                             "Male",
                             "Number of friends",
                             "Weekend",
                             "Ban*Friends in Russia",
                             "Ban*Mainland",
                             "Friends in Russia*Mainland",
                             "Ban*Friends in Russia*Mainland"),
          omit.stat=c("LL","ser","f"),
          no.space = T, type = "text")






############################################################################
############################################################################
##########Analysis Last logins for ALL the users in the sub-sample##########
########## This includes Figure B3 #########################################
############################################################################
############################################################################


### selecting unique users in the panel data set of user-day observation
users.test <- long.panel.clean %>% distinct(owner_id, .keep_all = T)

### How many users in the sub sample in Mainland continue logging in 30 days after the ban?
abandoned_date <- df%>%
  select(id, days1) %>%
  rename(owner_id = id, abandoned_days = days1) %>%
  select(owner_id, abandoned_days)

users.test <- left_join(users.test, abandoned_date)
nrow(users.test)

#### Creating an overview of active accounts in Mainland
users.test.ua <- users.test %>% filter(border == "Mainland Ukraine")

ua <- users.test.ua %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
ua1 <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

ua2 <- rbind(ua1, ua)

ua2<- ua2%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

ua2$sum_abandoned[is.na(ua2$sum_abandoned)] <- 0


ua2$total = nrow(users.test.ua)

ua2 <- ua2 %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.ua))



#### Creating an overview of active accounts in Crimea
users.test.crim <- users.test %>% filter(border == "Crimea")

crim <- users.test.crim %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
crim1 <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

crim2 <- rbind(crim1, crim)

crim2<- crim2%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

crim2$sum_abandoned[is.na(crim2$sum_abandoned)] <- 0


crim2$total = nrow(users.test.crim)

crim2 <- crim2 %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.crim))


### vizualising crimea and mainland in one graph
crim2$border <- "Crimea"
ua2$border <- "Mainland Ukraine"

login_all <- rbind(crim2, ua2)

ggplot(login_all) + 
  geom_point(aes(abandoned_days, prop_active*100, color = border)) +
  scale_x_continuous(limits = c(-30,30)) +
  scale_y_continuous(limits = c(0,100)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() 


############################################################################
############ Last logins for RANDOM users in the sub-sample##########
############################################################################


#### Creating an overview of active accounts in Mainland
users.test.ua.ran <- users.test %>% filter(border == "Mainland Ukraine", 
                                           pol == "Random")

ua.ran <- users.test.ua.ran %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
ua1.ran <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

ua2.ran <- rbind(ua1.ran, ua.ran)

ua2.ran<- ua2.ran%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

ua2.ran$sum_abandoned[is.na(ua2.ran$sum_abandoned)] <- 0


ua2.ran$total = nrow(users.test.ua.ran)

ua2.ran <- ua2.ran %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.ua.ran))



#### Creating an overview of active accounts in Crimea
users.test.crim.ran <- users.test %>% filter(border == "Crimea", 
                                             pol == "Random")

crim.ran <- users.test.crim.ran %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
crim1.ran <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

crim2.ran <- rbind(crim1.ran, crim.ran)

crim2.ran<- crim2.ran%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

crim2.ran$sum_abandoned[is.na(crim2.ran$sum_abandoned)] <- 0


crim2.ran$total = nrow(users.test.crim.ran)

crim2.ran <- crim2.ran %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.crim.ran))


### vizualising crimea and mainland in one graph
crim2.ran$border <- "Crimea"
ua2.ran$border <- "Mainland Ukraine"

login_ran <- rbind(crim2.ran, ua2.ran)


############################################################################
############ Last logins for Pro-Russian users in the sub-sample############
############################################################################


#### Creating an overview of active accounts in Mainland
users.test.ua.rus <- users.test %>% filter(border == "Mainland Ukraine", 
                                           pol == "Pro-Russian")

ua.rus <- users.test.ua.rus %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
ua1.rus <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

ua2.rus <- rbind(ua1.rus, ua.rus)

ua2.rus<- ua2.rus%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

ua2.rus$sum_abandoned[is.na(ua2.rus$sum_abandoned)] <- 0


ua2.rus$total = nrow(users.test.ua.rus)

ua2.rus <- ua2.rus %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.ua.rus))



#### Creating an overview of active accounts in Crimea
users.test.crim.rus <- users.test %>% filter(border == "Crimea", 
                                             pol == "Pro-Russian")

crim.rus <- users.test.crim.rus %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
crim1.rus <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

crim2.rus <- rbind(crim1.rus, crim.rus)

crim2.rus<- crim2.rus%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

crim2.rus$sum_abandoned[is.na(crim2.rus$sum_abandoned)] <- 0


crim2.rus$total = nrow(users.test.crim.rus)

crim2.rus <- crim2.rus %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.crim.rus))


### vizualising crimea and mainland in one graph
crim2.rus$border <- "Crimea"
ua2.rus$border <- "Mainland Ukraine"

login_rus <- rbind(crim2.rus, ua2.rus)


############################################################################
############ Last logins for Pro-Ukrainian users in the sub-sample##########
############################################################################

#### Creating an overview of active accounts in Mainland
users.test.ua.ukr <- users.test %>% filter(border == "Mainland Ukraine", 
                                           pol == "Pro-Ukrainian")

ua.ukr <- users.test.ua.ukr %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
ua1.ukr <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

ua2.ukr <- rbind(ua1.ukr, ua.ukr)

ua2.ukr<- ua2.ukr%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

ua2.ukr$sum_abandoned[is.na(ua2.ukr$sum_abandoned)] <- 0


ua2.ukr$total = nrow(users.test.ua.ukr)

ua2.ukr <- ua2.ukr %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.ua.ukr))



#### Creating an overview of active accounts in Crimea
users.test.crim.ukr <- users.test %>% filter(border == "Crimea", 
                                             pol == "Pro-Ukrainian")

crim.ukr <- users.test.crim.ukr %>%
  group_by(abandoned_days) %>%
  mutate(sum_abandoned = n()) %>%
  distinct(abandoned_days, .keep_all = T) %>%
  select(abandoned_days, sum_abandoned)

abandoned_all =c(-30:30)
crim1.ukr <- data.frame(abandoned_days = abandoned_all, sum_abandoned = NA)

crim2.ukr <- rbind(crim1.ukr, crim.ukr)

crim2.ukr<- crim2.ukr%>%
  arrange(sum_abandoned) %>% 
  distinct(abandoned_days, .keep_all = T) %>%
  filter(abandoned_days >= -30, 
         abandoned_days <= 30) %>%
  arrange(abandoned_days)

crim2.ukr$sum_abandoned[is.na(crim2.ukr$sum_abandoned)] <- 0


crim2.ukr$total = nrow(users.test.crim.ukr)

crim2.ukr <- crim2.ukr %>%
  mutate(cumsum = cumsum(sum_abandoned), 
         total = total - cumsum, 
         prop_active = total/nrow(users.test.crim.ukr))


### vizualising crimea and mainland in one graph
crim2.ukr$border <- "Crimea"
ua2.ukr$border <- "Mainland Ukraine"

login_ukr <- rbind(crim2.ukr, ua2.ukr)

##### vizualizing login/acess activity for all of the sub-groups
### indicating the different subgroups
login_ukr$sub <- "Pro-Ukrainian"
login_rus$sub <- "Pro-Russian"
login_ran$sub <- "Random"
login_all$sub <- "All"
login <- rbind(login_ukr, login_rus, login_ran, login_all)


#ordering the political affiliation variable so that "Pro-Russian" appears first in the graph
login$sub <- factor(login$sub,levels=c("All","Random", "Pro-Russian", "Pro-Ukrainian"))

### creating the plot
ggplot(login) + 
  geom_point(aes(abandoned_days, prop_active*100, color = border), size = 0.8) +
  scale_x_continuous(limits = c(-30,30)) +
  scale_y_continuous(limits = c(0,100)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte() +
  facet_wrap(~sub) +
  labs(x = "Days before and after censorship",
       y = "Proportion of users who continue logging on Vkontakte", 
       color = "Border") +
  scale_colour_manual(values=c("light blue", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=15),
        strip.text = element_text(size = 14, face  = "bold"),
        legend.title = element_text(size = 14)) +
  annotate("rect", xmin = 0, xmax = 30, ymin = 0, ymax = 100, alpha = 0.25, fill = "#8FBC8F")


