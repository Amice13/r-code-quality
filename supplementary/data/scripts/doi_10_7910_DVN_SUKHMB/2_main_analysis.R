### Setting working directory
setwd("/YOUR/WORKING DIRECTORY/HERE")

### installing packages 
install.packages("ggmap") # version 3.0.0
install.packages("tidyr")
install.packages("data.table")
install.packages("car")
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
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggpubr")
install.packages("foreign")
install.packages("rdd")
install.packages("robustbase")
install.packages("lmtest")
install.packages("sandwich")


# loading packages
library(tidyr)
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
library(robustbase)
library(lmtest)
library(sandwich)
library(ggpubr)



##########################################################################################################
###############Creating Figure 1: Vkontakte users within 50 km of the Crimean border.#####################
##########################################################################################################


### selecting "active users": those who have logged  on 30 days prior to censorship or later
df.act <- df %>% filter(days >= -30)
df.act$days <- as.numeric(df.act$days) # converting to numeric


### creating a data frame on a city level
df_down_cities <-  df.act %>% 
  group_by(city.id) %>% 
  mutate(count = n())%>%
  distinct(city.id, .keep_all = T)

### You will need a API key yo Aceess Google mas through API
register_google(key = "INSERT_YOUR_KEY_HERE")


### Downloading the relevant map from Google Maps using the Google API
bw_map <- get_googlemap(center = c(34.042078401,46.11984713), zoom = 8,
                        color = "bw",
                        style = "feature:road|visibility:off&style=element:label|visibility:off&style=feature:administrative|visibility:on=style=feature:all|element:labels|visibility:off")

### Figure 1 in color
ggmap(bw_map) +
  geom_point(data = df_down_cities[df_down_cities$border==1,],
             aes(x = lon1, y = lat1, size = count, color = "Mainland"), 
             alpha = 0.5) +
  geom_point(data = df_down_cities[df_down_cities$border==2,],
             aes(x = lon1, y = lat1, size = count, color = "Crimea"),
             alpha = 0.5) +
  scale_color_manual(values = c(Mainland = "red", Crimea = "purple"))+
  scale_size_continuous(range = c(2, 9)) + 
  labs(
    size = "Active VK users",
    color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 8))) +
  theme(legend.title =element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### Figure 1 in black and white
ggmap(bw_map) +
  geom_point(data = df_down_cities[df_down_cities$border==1,],
             aes(x = lon1, y = lat1, size = count, color = "Mainland"), 
             alpha = 0.5) +
  geom_point(data = df_down_cities[df_down_cities$border==2,],
             aes(x = lon1, y = lat1, size = count, color = "Crimea"),
             alpha = 0.5) +
  scale_color_manual(values = c(Mainland = "black", Crimea = "dark grey"))+
  scale_size_continuous(range = c(2, 9)) + 
  labs(
    size = "Active VK users",
    color = NULL) +
  guides(color = guide_legend(override.aes = list(size = 8))) +
  theme(legend.title =element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


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

#clearing up memory 
rm(g, activity_per_day1, wide, long)

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





###########################################################################################################
################### Descriptive overview ##################################################################
################### Figure 4: Change in mean number of posts per day after the ban ########################
################### (for all users and the split-sample) ##################################################
###########################################################################################################

#################### Storing descriptive count data for users in Mainland Ukraine

### Storing the necessary info for users in  MainlandUkraine
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



#################### storing descriptive count data for users in Crimea
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


##################################### # 90 days before and after the ban###################################
walls.clean.ua.days <- long.panel.clean %>%
  filter(border == "Mainland Ukraine") %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per day
         posts_mean = sum_posts/n_ukr_all) %>%
  distinct(days1, .keep_all = T) %>%# ensuring that there is only one row per day 
  select(days1, after, posts_mean, border)



walls.clean.crim.days <- long.panel.clean %>%
  filter(border == "Crimea") %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per day
         posts_mean = sum_posts/n_crim_all) %>%
  distinct(days1, .keep_all = T) %>%# ensuring that there is only one row per day 
  select(days1, after, posts_mean, border)

walls.clean.ua.days <- rbind(walls.clean.ua.days, walls.clean.crim.days)
nrow(walls.clean.ua.days)


### pro-russian
walls.clean.ua.rus.days <- long.panel.clean %>%
  filter(border  == "Mainland Ukraine",
         rus == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per day
         posts_mean = sum_posts/n_ukr_pro_rus) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border)


walls.clean.crim.rus.days <- long.panel.clean %>%
  filter(border  == "Crimea",
         rus == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per day
         posts_mean = sum_posts/n_crim_pro_rus) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border)

walls.clean.ua.rus.days <- rbind(walls.clean.ua.rus.days, walls.clean.crim.rus.days)
nrow(walls.clean.ua.rus.days)

### pro-Ukrainian
walls.clean.ua.ua.days <- long.panel.clean %>%
  filter(border == "Mainland Ukraine",
         ukr == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_ukr_pro_ukr) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 

walls.clean.ua.crim.days <- long.panel.clean %>%
  filter(border =="Crimea", 
         ukr == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_crim_pro_ukr) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.ua.ua.days <- rbind(walls.clean.ua.crim.days, walls.clean.ua.ua.days)

### Random-users
walls.clean.ua.ran.days <- long.panel.clean %>%
  filter(border == "Mainland Ukraine", 
         ran == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_ukr_pro_ran) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.crim.ran.days <- long.panel.clean %>%
  filter(border == "Crimea",
         ran == 1) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_crim_pro_ran) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.ua.ran.days <- rbind(walls.clean.ua.ran.days, walls.clean.crim.ran.days)



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


#### computing numbers of users with above median proportion of friends in russia
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

### selecting users in Mainland with above median proportion of friends in Russia
ukr_above_median <- ukr_t %>% filter(prop_rus_friends >median_mainland)
### selecting users in Mainland with up to median proportion of friends in Russia
ukr_upto_median <- ukr_t %>% filter(prop_rus_friends <= median_mainland)

### selecting users in Mainland with above median proportion of friends in Russia
crim_above_median <- crim_t %>% filter(prop_rus_friends >median_mainland)
### selecting users in Mainland with up to median proportion of friends in Russia
crim_upto_median <- crim_t %>% filter(prop_rus_friends <= median_crimea)

### mean number of posts before censorship for the different groups in the subsample of users
ukr_posts_above_median_before  <- mean(ukr_above_median$sum_posts_before)/90
ukr_posts_upto_median_before  <- mean(ukr_upto_median$sum_posts_before)/90
crim_posts_above_median_before <- mean(crim_above_median$sum_posts_before)/90
crim_posts_upto_median_before <- mean(crim_upto_median$sum_posts_before)/90


### mean number of posts after censorship for the different groups in the subsample of users
ukr_posts_above_median_after  <- mean(ukr_above_median$sum_posts_after)/91
ukr_posts_upto_median_after  <- mean(ukr_upto_median$sum_posts_after)/91
crim_posts_above_median_after <- mean(crim_above_median$sum_posts_after)/91
crim_posts_upto_median_after <- mean(crim_upto_median$sum_posts_after)/91


### computing mean number of posts (FOR USERS IN MAINLAND) prior to censorship with users above median proportion of frieands in Russian

### Above median
walls.clean.ua.above.days <- long.panel.clean %>%
  filter(border == "Mainland Ukraine",
         prop_rus_friends >median_mainland) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_above_median_ua) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.crim.above.days <- long.panel.clean %>%
  filter(border == "Crimea",
         prop_rus_friends >median_crimea) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/n_above_median_crim) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.ua.above.days <- rbind(walls.clean.ua.above.days, walls.clean.crim.above.days)

### Up to median
walls.clean.ua.below.days <- long.panel.clean%>%
  filter(border == "Mainland Ukraine",
         prop_rus_friends <=median_mainland) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/(n_ukr_all - n_above_median_ua)) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.crim.below.days <- long.panel.clean%>%
  filter(border == "Crimea",
         prop_rus_friends <=median_crimea) %>%
  ungroup() %>%
  group_by(days1) %>% #grouping by days
  mutate(sum_posts = sum(posts_days), #counting number of posts per days
         posts_mean = sum_posts/(n_crim_all - n_above_median_crim)) %>%
  distinct(days1, .keep_all = T) %>% # ensuring that there is only one row per day
  select(days1, after, posts_mean, border) 


walls.clean.ua.below.days <- rbind(walls.clean.ua.below.days, walls.clean.crim.below.days)

### Naming the different datasets
walls.clean.ua.days$group <- "All"
walls.clean.ua.rus.days$group <- "Pro-Russian"
walls.clean.ua.ua.days$group <- "Pro-Ukrainian"  
walls.clean.ua.ran.days$group <- "Random"
walls.clean.ua.above.days$group <- "Above median"
walls.clean.ua.below.days$group <- "Up to median"

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


### mean number of posts after censorship for the different groups in the subsample of users
ukr_posts_rus_after <- mean(ukr_pro_rus$sum_posts_after)/91

ukr_posts_ukr_after <- mean(ukr_pro_ukr$sum_posts_after)/91
ukr_posts_ran_after <- mean(ukr_pro_ran$sum_posts_after)/91
ukr_posts_all_after <- mean(ukr_t$sum_posts_after)/91


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


### Attaching values for mean number of wall posts per day for users in Mainland before the ban
walls.clean.ua.days$mean_ukr_before <- ukr_posts_all_before
walls.clean.ua.rus.days$mean_ukr_before <- ukr_posts_rus_before
walls.clean.ua.ua.days$mean_ukr_before <- ukr_posts_ukr_before
walls.clean.ua.ran.days$mean_ukr_before <- ukr_posts_ran_before
walls.clean.ua.above.days$mean_ukr_before <- ukr_posts_above_median_before
walls.clean.ua.below.days$mean_ukr_before <- ukr_posts_upto_median_before


### Attaching values for mean number of wall posts per day for users in Mainland after the ban
walls.clean.ua.days$mean_ukr_after <- ukr_posts_all_after
walls.clean.ua.rus.days$mean_ukr_after <- ukr_posts_rus_after
walls.clean.ua.ua.days$mean_ukr_after <- ukr_posts_ukr_after
walls.clean.ua.ran.days$mean_ukr_after <- ukr_posts_ran_after
walls.clean.ua.above.days$mean_ukr_after <- ukr_posts_above_median_after
walls.clean.ua.below.days$mean_ukr_after <- ukr_posts_upto_median_after


### Attaching values for mean number of wall posts per day for users in Crimea before the ban
walls.clean.ua.days$mean_crim_before <- crim_posts_all_before
walls.clean.ua.rus.days$mean_crim_before <- crim_posts_rus_before
walls.clean.ua.ua.days$mean_crim_before <- crim_posts_ukr_before
walls.clean.ua.ran.days$mean_crim_before <- crim_posts_ran_before
walls.clean.ua.above.days$mean_crim_before <- crim_posts_above_median_before
walls.clean.ua.below.days$mean_crim_before <- crim_posts_upto_median_before


### Attaching values for mean number of wall posts per day for users in Crimea after the ban
walls.clean.ua.days$mean_crim_after <- crim_posts_all_after
walls.clean.ua.rus.days$mean_crim_after <- crim_posts_rus_after
walls.clean.ua.ua.days$mean_crim_after <- crim_posts_ukr_after
walls.clean.ua.ran.days$mean_crim_after <- crim_posts_ran_after
walls.clean.ua.above.days$mean_crim_after <- crim_posts_above_median_after
walls.clean.ua.below.days$mean_crim_after <- crim_posts_upto_median_after


### Combinging the three datasets  
bw90_combined <- rbind(walls.clean.ua.days,
                       walls.clean.ua.rus.days,
                       walls.clean.ua.ua.days,
                       walls.clean.ua.ran.days,
                       walls.clean.ua.above.days,
                       walls.clean.ua.below.days)

bw90_combined$after <- factor(bw90_combined$after)    


### visualizing the changes in mean posting activity
# changing the order of the models to organize the facet_wrap output
bw90_combined$group <- factor(bw90_combined$group,levels=c("All",
                                                           "Above median",
                                                           "Up to median",
                                                           "Random",
                                                           "Pro-Russian",
                                                           "Pro-Ukrainian"))



### creating the plot in color
ggplot(bw90_combined, aes(x = days1, y = posts_mean, color = border)) + 
  geom_point(base_family = "sans-serif")+ 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte() + # add base_family = "sans-serif" for the exact same font as in the paper
  #theme_classic() +
  #theme_minimal() + 
  labs(x = "Days before and after censorship",
       y = "Mean number of wall posts per user", 
       color = "Border") +
  scale_y_continuous(limits = c(0, 0.45)) +
  scale_x_continuous(limits = c(-90, 90)) +
  scale_colour_manual(values=c("light blue", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=15),
        strip.text = element_text(size = 14, face  = "bold"),
        legend.title = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2) +
  geom_segment(aes(x = -90, xend = 0, y =mean_ukr_before, yend = mean_ukr_before),  col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_ukr_after, yend = mean_ukr_after), col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = -90, xend = 0, y =mean_crim_before, yend = mean_crim_before),  col = "light blue", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_crim_after, yend = mean_crim_after), col = "light blue", size = 1, linetype = 1) +
  annotate("rect", xmin = 0, xmax = 90, ymin = 0, ymax = 0.45, alpha = 0.25, fill = "#8FBC8F")


### creating a black and white version of the plot
ggplot(bw90_combined, aes(x = days1, y = posts_mean, color = border)) + 
  geom_point(base_family = "sans-serif")+ 
  geom_vline(xintercept = 0, linetype = "dashed") +
  #theme_tufte() + # add base_family = "sans-serif" for the exact same font as in the paper
  #theme_classic() +
  theme_minimal() + 
  #theme_bw() +
  labs(x = "Days before and after censorship",
       y = "Mean number of wall posts per user", 
       color = "Border") +
  scale_y_continuous(limits = c(0, 0.45)) +
  scale_x_continuous(limits = c(-90, 90)) +
  scale_colour_manual(values=c("light grey", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=15),
        strip.text = element_text(size = 14, face  = "bold"),
        legend.title = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2) +
  geom_segment(aes(x = -90, xend = 0, y =mean_ukr_before, yend = mean_ukr_before),  col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_ukr_after, yend = mean_ukr_after), col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = -90, xend = 0, y =mean_crim_before, yend = mean_crim_before),  col = "grey", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_crim_after, yend = mean_crim_after), col = "grey", size = 1, linetype = 1) 


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
##########################################################################################################
########################################## Difference in Differences Regressions #########################
##########################################################################################################
##########################################################################################################


##########################################################################################################
##########################################################################################################
######## Figure 5: Change in mean number of posts/day after the ban (95% confidence intervals) ###########
##########################################################################################################
##########################################################################################################

# A large proportion of the code originates from the following tutorial: 
# https://cran.r-project.org/web/packages/dotwhisker/vignettes/dotwhisker-vignette.html

m1_90 <- dif_est(long.panel.clean.pol, "after:ua_border", bw = 90)
m2_90 <- dif_est(long.panel.clean.pol, "after:above_median_ru.pol", bw = 90)
m3_90 <- dif_est(long.panel.clean.pol, "after:rus", bw = 90)
m4_90 <- dif_est(long.panel.clean.pol, "after:above_median_ru.pol:ua_border", bw = 90)
m5_90 <- dif_est(long.panel.clean.pol, "after:rus:ua_border", bw = 90)


m1_30 <- dif_est(long.panel.clean.pol, "after:ua_border", bw = 30)
m2_30 <- dif_est(long.panel.clean.pol, "after:above_median_ru.pol", bw = 30)
m3_30 <- dif_est(long.panel.clean.pol, "after:rus", bw = 30)
m4_30 <- dif_est(long.panel.clean.pol, "after:above_median_ru.pol:ua_border", bw = 30)
m5_30 <- dif_est(long.panel.clean.pol, "after:rus:ua_border", bw = 30)


models_ddd <- rbind(m1_90, m2_90, m3_90, m4_90, m5_90, 
                    m1_30, m2_30, m3_30, m4_30, m5_30)

### replacing string ":" with "_" in term names

models_ddd$term <- str_replace(models_ddd$term, ":", "_")
models_ddd$term <- str_replace(models_ddd$term, ":", "_")

## Adding labels to the predictors
models_ddd <- models_ddd %>%
  relabel_predictors(c(
    after_ua_border = "Ban x Mainland",
    after_above_median_ru.pol = "Ban x Friends in Russia", 
    after_rus = "Ban x Pro-Russian", 
    after_above_median_ru.pol_ua_border = "Ban x Friends in Russia x Mainland ",
    after_rus_ua_border = "Ban x Pro-Russian x Mainland "))

### grouping the predictors
brackets <- list(c("DD","Ban x Mainland", "Ban x Pro-Russian", "Ban x Friends in Russia"),
                 c("DDD", "Ban x Friends in Russia x Mainland ", "Ban x Pro-Russian x Mainland "))



#models_ddd$model[models_ddd$model == 10] <- "10 days" 
models_ddd$model[models_ddd$model == 30] <- "30 days" 
models_ddd$model[models_ddd$model == 90] <- "90 days" 

# changing the order of the models to organize the facet_wrap output
models_ddd$model <- factor(models_ddd$model,levels=c(#"10 days",
  "30 days", 
  "90 days"))



{dwplot(models_ddd,
        conf.level = .95,
        vline = geom_vline(xintercept = 0, colour = "grey60",
                           linetype = 2),
        dot_args = list(aes(shape = model)),
        whisker_args = list(aes(linetype = model))) +
    theme_minimal() + xlab("Coefficient estimate") + ylab("") +
    #ggtitle("Insert") +
    theme(plot.title = element_text(face="bold"),
          legend.position = c(0, 0),
          legend.justification = c(0, -0.1),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .1, end = .0, # if start and end same value, use same colour for all models
                      name = "Model", 
                      breaks = c(0, 1),
                      labels = c( "30 days", "90 days")) +
    scale_shape_discrete(name = "Bandwidth",
                         #breaks = c(-0.07, 0),
                         labels = c("30 days", "90 days"))} %>%
  add_brackets(brackets)


##########################################################################################################
##########################################################################################################
########## Regressions for all users in the subsample: Now also including random users ###################
########## Figure 2 and Table J1 #########################################################################
##########################################################################################################
##########################################################################################################


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


### Running the models
# 90 days bandwidth
m1_90.all <- dif_est(long.panel.clean, "after:ua_border", bw = 90)
# 30 days bandwidth
m1_30.all <- dif_est(long.panel.clean, "after:ua_border", bw = 30)

#combining the models
models_dd.all <- rbind(m1_90.all, m1_30.all)

### replacing string ":" with "_" in term names
models_dd.all$term <- str_replace(models_dd.all$term, ":", "_")
models_dd.all$term <- str_replace(models_dd.all$term, ":", "_")

## Adding labels to the predictors
models_dd.all <- models_dd.all %>%
  relabel_predictors(c(
    after_ua_border = "Ban x Mainland",
    after_above_median_ru.pol = "Ban x Friends in Russia", 
    after_rus = "Ban x Pro-Russian", 
    after_above_median_ru.pol_ua_border = "Ban x Friends in Russia x Mainland ",
    after_rus_ua_border = "Ban x Pro-Russian x Mainland "))


### adding labels
models_dd.all$model[models_dd.all$model == 30] <- "30 days" 
models_dd.all$model[models_dd.all$model == 90] <- "90 days" 

# changing the order of the models to organize the facet_wrap output
models_dd.all$model <- factor(models_dd.all$model,levels=c("30 days", 
                                                           "90 days"))



##########################################################################################################
####### Figure 3: Change in mean number of posts per day after the ban (95% confidence intervals) ########
##########################################################################################################

dwplot(models_dd.all, 
       conf.level = .95) + 
  geom_vline(xintercept = 0, colour = "grey60",
             linetype = 2)+ 
  theme_tufte() +
  scale_x_continuous(limits= c(-0.15, 0.001)) +
  labs(color = "Bandwidth",
       x = "Coefficient estimate")  +
  scale_colour_grey(
    #breaks = c(0, 1),
    labels = c("30 days", "90 days")) +
  theme(axis.text=element_text(size= 8),
        axis.title=element_text(size=8),
        legend.title = element_text(size = 8),
        legend.text=element_text(size=6))

############################################################################
############################################################################
##########Analysis Last logins for ALL the users in the sub-sample##########
########## This includes Figure 2 ##########################################
############################################################################
############################################################################


### selecting unique users in the panel data set of user-day observation
users.test <- long.panel.clean %>% distinct(owner_id, .keep_all = T)

df$days1 <-  as.numeric(df$days -3 ) #(18th of may == 0)
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

############################### Figure in color ###################################
### creating the plot showing proportion of users who continue logging on Vkontakte
a_color <- ggplot(login_all) + 
  geom_point(aes(abandoned_days, prop_active*100, color = border), size = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-30,30)) +
  scale_y_continuous(limits = c(0,100)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte() +
  labs(x = "Days before and after censorship",
       y = "Proportion of users who continue\n logging on Vkontakte                    ", 
       color = "Border") +
  scale_colour_manual(values=c("light blue", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=9.5),
        strip.text = element_text(size = 8, face  = "bold"),
        legend.title = element_text(size = 8)) +
  annotate("rect", xmin = 0, xmax = 30, ymin = 0, ymax = 100, alpha = 0.25, fill = "#8FBC8F")



b_color <- ggplot(walls.clean.ua.days, aes(x = days1, y = posts_mean, color = border)) + 
  geom_point(size = 0.5)+ 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte() + # add base_family = "sans-serif" for the exact same font as in the paper
  #theme_classic() +
  #theme_minimal() + 
  labs(x = "Days before and after censorship",
       y = "Mean number of wall posts per user", 
       color = "Border") +
  scale_y_continuous(limits = c(0, 0.45)) +
  scale_x_continuous(limits = c(-90, 90)) +
  scale_colour_manual(values=c("light blue", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=9.5),
        strip.text = element_text(size = 14, face  = "bold"),
        legend.title = element_text(size = 14)) +
  geom_segment(aes(x = -90, xend = 0, y =mean_ukr_before, yend = mean_ukr_before),  col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_ukr_after, yend = mean_ukr_after), col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = -90, xend = 0, y =mean_crim_before, yend = mean_crim_before),  col = "light blue", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_crim_after, yend = mean_crim_after), col = "light blue", size = 1, linetype = 1) +
  annotate("rect", xmin = 0, xmax = 90, ymin = 0, ymax = 0.45, alpha = 0.25, fill = "#8FBC8F")




ggarrange(a_color, b_color,
          labels = c("a", "b"),
          #label.x = 0.5,
          #hjust = 0.5,
          vjust = 1, 
          ncol = 2,
          heights = c(5, 5),
          widths = c(5, 7),
          label.x = 0, 
          font.label = list(size = 12, color = "black", face = "bold")) 


############################### Figure in black and white ###################################

a_bw <- ggplot(login_all) + 
  geom_point(aes(abandoned_days, prop_active*100, color = border), size = 0.5, show.legend = FALSE) +
  scale_x_continuous(limits = c(-30,30)) +
  scale_y_continuous(limits = c(0,100)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte() +
  #theme_minimal()+
  labs(x = "Days before and after censorship",
       y = "Proportion of users who continue\n logging on Vkontakte                    ", 
       color = "Border") +
  scale_colour_manual(values=c("light grey", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=9.5),
        strip.text = element_text(size = 8, face  = "bold"),
        legend.title = element_text(size = 8)) 



b_bw <- ggplot(walls.clean.ua.days, aes(x = days1, y = posts_mean, color = border)) + 
  geom_point(base_family = "sans-serif", size = 0.5)+ 
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_tufte() + # add base_family = "sans-serif" for the exact same font as in the paper
  #theme_classic() +
  #theme_minimal() + 
  labs(x = "Days before and after censorship",
       y = "Mean number of wall posts per user", 
       color = "Border") +
  scale_y_continuous(limits = c(0, 0.45)) +
  scale_x_continuous(limits = c(-90, 90)) +
  scale_colour_manual(values=c("light grey", "black"),
                      labels = c("Crimea", "Mainland")) +
  theme(axis.text=element_text(size= 10),
        axis.title=element_text(size=9.5),
        strip.text = element_text(size = 14, face  = "bold"),
        legend.title = element_text(size = 14)) +
  geom_segment(aes(x = -90, xend = 0, y =mean_ukr_before, yend = mean_ukr_before),  col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_ukr_after, yend = mean_ukr_after), col = "black", size = 1, linetype = 1) +
  geom_segment(aes(x = -90, xend = 0, y =mean_crim_before, yend = mean_crim_before),  col = "grey", size = 1, linetype = 1) +
  geom_segment(aes(x = 0, xend = 90, y =mean_crim_after, yend = mean_crim_after), col = "grey", size = 1, linetype = 1) 




ggarrange(a_bw, b_bw,
          labels = c("a", "b"),
          #label.x = 0.5,
          #hjust = 0.5,
          vjust = 1, 
          ncol = 2,
          heights = c(5, 5),
          widths = c(5, 7),
          label.x = 0, 
          font.label = list(size = 12, color = "black", face = "bold")) 

