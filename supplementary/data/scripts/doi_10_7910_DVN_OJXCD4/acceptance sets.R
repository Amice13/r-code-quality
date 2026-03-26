
#################################################################################
#
# What do parents want? Parental spousal prefernces in China
# Date: September 2021
# 
# Acceptance Sets
# 
# R version 4.0.3
#  
# This code needs the CFPS data files which are available online at: https://opendata.pku.edu.cn/dataverse/CFPS?language=en
#
# names: ecfps2016adult_201906.dta (CFPS 2016);  ecfps2014adult_201906.dta (CFPS 2014)
#
#################################################################################

rm(list=ls())
library(readstata13) #readstata13_0.9.2
library(haven) # haven_2.3.1 
library(tidyverse) # tidyverse 1.3.0

### set seed (chosen from random.org number between 1 and 10000)
seed = 4156
set.seed(seed)

########### QSAMPY data: Parents sample (defined as searching for someone else)
parents <- read_dta("summary_statistics_glp.dta")

########### CFPS data
# CFPS 2016, create data set with those who married in the past two years and the pertinent variables
cfps2016_original = read.dta13("ecfps2016adult_201906.dta")
cfps2016_original <- cfps2016_original %>%  dplyr::rename(birthyear = cfps_birthy,
                                                          edu = cfps2016edu )
cfps2016 = cfps2016_original %>%  filter(eeb401y_a_1 >= 2014 & eeb401y_a_1 <=2016) %>% 
  select(cfps_gender, eeb401y_a_1, edu, eeb4021_a_1, eeb402y_a_1, birthyear, pid)
cfps2016$year = 2016

# CFPS 2014, create data set with those who married in the past two years and the pertinent variables
cfps2014_original <- read.dta13("ecfps2014adult_201906.dta")
cfps2014_original <- cfps2014_original %>%  dplyr::rename(birthyear = cfps_birthy,
                                                          edu = cfps2014edu )
cfps2014 <-  cfps2014_original %>%  filter(eeb401y_a_1 >= 2012 & eeb401y_a_1 <=2014) %>% 
  select(cfps_gender, eeb401y_a_1, edu, eeb4021_a_1, eeb402y_a_1, birthyear, pid)
cfps2014$year = 2014

cfps = rbind(cfps2016, cfps2014)

### only those with complete information
cfps <- subset(cfps,is.na(edu) == FALSE & is.na(eeb401y_a_1)==FALSE &
                 is.na(cfps_gender) == FALSE & is.na(eeb4021_a_1)==FALSE &
                 is.na(birthyear) == FALSE & is.na(eeb402y_a_1)==FALSE &
                 edu > 0 & eeb4021_a_1 > 0 &
                 birthyear > 1900 & eeb402y_a_1 > 1900)

names(cfps) <- c("gender", "year_marriage", "edu", "edu_spouse", "birthyear_spouse", "birthyear", "pid", "year")

cfps$education = cfps$edu
cfps$education[cfps$education >=7] = 7 

cfps$education_spouse = cfps$edu_spouse
cfps$education_spouse[cfps$education_spouse >= 7] = 7

# Put in one category those illiterate and primary school (-> primary school and less)
cfps$education[cfps$education <=2] = 0 
cfps$education_spouse[cfps$education_spouse <=2] = 0
cfps$education_spouse[cfps$education_spouse ==3] = 1
cfps$education[cfps$education ==3] = 1
cfps$education_spouse[cfps$education_spouse ==4] = 2
cfps$education[cfps$education ==4] = 2
cfps$education_spouse[cfps$education_spouse ==5] = 3
cfps$education[cfps$education ==5] = 3
cfps$education_spouse[cfps$education_spouse ==6] = 4
cfps$education[cfps$education == 6] = 4
cfps$education_spouse[cfps$education_spouse >=7] = 5
cfps$education[cfps$education >= 7] = 5

# Put in one category those with master degree and doctorate, for both spouses

# female dummy
cfps$female = ifelse(cfps$gender == 0, 1, 0)

# create for wife and husband
cfps$education.wife[cfps$female ==1]= cfps$education[cfps$female ==1]
cfps$education.wife[cfps$female ==0]= cfps$education_spouse[cfps$female ==0]
cfps$education.husband[cfps$female ==0]= cfps$education[cfps$female ==0]
cfps$education.husband[cfps$female ==1]= cfps$education_spouse[cfps$female ==1]

# create husband and spouse age at marriage
#spouse
cfps$spouse_age = cfps$year - cfps$birthyear_spouse - (cfps$year - cfps$year_marriage)
#respondent
cfps$age = cfps$year - cfps$birthyear - (cfps$year - cfps$year_marriage)
cfps <- subset(cfps, age >= 20 & age <= 50 & spouse_age >= 20 & spouse_age <= 50)

# convert to husband and wife
cfps$age.wife[cfps$female==1] = cfps$age[cfps$female==1]
cfps$age.wife[cfps$female==0] = cfps$spouse_age[cfps$female==0]
cfps$age.husband[cfps$female==0] = cfps$age[cfps$female==0]
cfps$age.husband[cfps$female==1] = cfps$spouse_age[cfps$female==1]


cfps <- cfps %>% filter(is.na(age.wife) == FALSE, is.na(age.husband) == FALSE, 
                        is.na(education.husband) == FALSE, is.na(education.wife) == FALSE  )

## re-define the educational preferences including college 
redefine_education <- function(var){
  var.new =  var
  var.new[var.new ==99 | var.new==98 | var.new==9] = NA
  var.new[var<=2] = 0 ## primary school or less
  var.new[var==3] = 1 ## middle school
  var.new[var==4] = 2 ## high school
  var.new[var==5] = 2 ## high school
  var.new[var==6] = 3 ## college
  var.new[var==7 ] = 4 ## university
  var.new[var==8] = 5 ## graduate
  var.new
}


parents$lowest.edu <- parents$pref_educ_low %>% 
  redefine_education()
parents$lowest.edu[parents$lowest.edu==96] = 0 
parents$highest.edu <- parents$pref_educ_high %>% 
  redefine_education()
parents$highest.edu[parents$highest.edu==96] = 5
parents$edu <- parents$education%>% 
  redefine_education()

parents$edu[parents$education >8] = NA

## Female dummy
parents$gender= ifelse(parents$relation ==1 | parents$relation== 3 | parents$relation==5 |parents$relation ==7 | parents$relation==9, 1, 0)

## create data set with the information we need: we want averages for each educational level and confidence intervals
edu.limits = data.frame(parents$lowest.edu, parents$highest.edu, 
                        parents$edu, parents$gender)
edu = edu.limits %>% group_by(parents.edu, parents.gender) %>%  summarize(ave_low = mean(parents.lowest.edu, na.rm = TRUE),
                                                                          ave_high = mean(parents.highest.edu, na.rm = TRUE),
                                                                          count = n(),
                                                                          sd_low = sd(parents.lowest.edu, na.rm = TRUE),
                                                                          sd_high = sd(parents.highest.edu, na.rm = TRUE),
                                                                          CI_pos_low = ave_low + 1.96*sd_low/sqrt(count),
                                                                          CI_neg_low = ave_low - 1.96*sd_low/sqrt(count),
                                                                          CI_pos_high = ave_high + 1.96*sd_high/sqrt(count),
                                                                          CI_neg_high = ave_high - 1.96*sd_high/sqrt(count))

eduf = subset(edu, parents.gender==1) 
edum = subset(edu, parents.gender==0 & parents.edu !=0)
cfps_sum <- cfps %>% group_by(education.wife, education.husband)  %>% summarize(total = n())  %>% mutate(proportion = total/nrow(cfps))

##################### Educational Graph
acceptance_edu <- ggplot(eduf, aes(x = parents.edu)) + 
  geom_point(aes(y=ave_low, color = "Men: Lower Bound", shape = "Men: Lower Bound")) +
  geom_point(aes(y=ave_high, color = "Men: Upper Bound", shape = "Men: Upper Bound")) +
  geom_line(aes(y=ave_low), color = "red") + 
  geom_line(aes(y=ave_high), color = "red") + 
  geom_point(data = cfps_sum, aes(x= education.wife, y = education.husband, size = proportion), alpha = 0.7, shape = 21) +
  geom_ribbon(aes(ymin=CI_neg_low, ymax=CI_pos_low), alpha=0.2, fill = "salmon")  + 
  geom_ribbon(aes(ymin=CI_neg_high, ymax=CI_pos_high), alpha=0.2, fill = "salmon") +
  geom_point(data= edum, aes(x=ave_low, y= parents.edu , color = "Women: Lower Bound", shape = "Women: Lower Bound" )) +
  geom_point(data= edum, aes(x=ave_high, y= parents.edu, color = "Women: Upper Bound", shape = "Women: Upper Bound")) +
  geom_line(data= edum, aes(x=ave_low, y= parents.edu), color = "blue")  +
  geom_path(data= edum, aes(x=ave_high, y= parents.edu), color = "blue") +
  geom_ribbon(data= edum, aes(y= parents.edu, xmin=CI_neg_low, xmax=CI_pos_low), alpha=0.2, fill = "skyblue1")  + 
  geom_ribbon(data= edum, aes(y= parents.edu, xmin=CI_neg_high, xmax=CI_pos_high), alpha=0.2, fill = "skyblue1") +
  theme_bw() + scale_x_continuous(breaks=c(0,1,2,3,4,5),
                                  labels=c("Primary School", "Middle School", "High School", "College", "University", "Graduate/No limit")) + 
  theme(axis.text.x= element_text(angle=45, vjust=0.6)) + 
  scale_y_continuous(limits = c(0,5), breaks=c(0,1,2,3,4,5), labels=c("Primary School", "Middle School", "High School", "College", "University", "Graduate/ No limit")) +
  labs(x = "Woman's education", y = "Man's education") +
  scale_colour_manual(name = "", labels = c("Women: Lower Bound", "Women: Upper Bound", "Men: Lower Bound", "Men: Upper Bound"),
                      values = c("red", "red", "blue", "blue")) +   
  scale_shape_manual(name = "", labels = c("Women: Lower Bound", "Women: Upper Bound", "Men: Lower Bound", "Men: Upper Bound"),
                     values = c(16, 17, 21, 24)) +
  scale_size(range = c(1, 10), name="Actual Matches 2012-2016 (Proportion)")

ggsave("acceptance_edu.png", plot= acceptance_edu, width =9, height = 5)



##################### Age Graph
age.limits = parents %>% mutate(pref_age_high = ifelse(pref_age_high <=90, pref_age_high, NA),
                                pref_age_low = ifelse(pref_age_low <=90, pref_age_low, NA)) %>% 
  select(pref_age_high, pref_age_low, age, gender) 

age.limits <- age.limits %>% 
  mutate(age_cat = case_when(
    age <= 25 ~ 1,
    age > 25 & age <= 28   ~ 2,
    age > 28 & age <= 31 ~ 3,
    age > 31 & age <= 34 ~ 4,
    age > 34 & age <= 37 ~ 5,
    age > 37 & age <= 40 ~ 6,
    age > 40 & age <= 50 ~ 7,
  )) 

ggplot(age.limits, aes(x= age)) + geom_histogram(binwidth=1)

age = age.limits %>% group_by(age_cat, gender) %>%  summarize(
  age_average = mean(age), 
  ave_low = mean(pref_age_low, na.rm = TRUE),
  ave_high = mean(pref_age_high, na.rm = TRUE),
  count = n(),
  sd_low = sd(pref_age_low, na.rm = TRUE),
  sd_high = sd(pref_age_high, na.rm = TRUE),
  CI_pos_low = ave_low + 1.96*sd_low/sqrt(count),
  CI_neg_low = ave_low - 1.96*sd_low/sqrt(count),
  CI_pos_high = ave_high + 1.96*sd_high/sqrt(count),
  CI_neg_high = ave_high - 1.96*sd_high/sqrt(count))


agef = age %>% filter(gender==1, is.na(age_cat) ==FALSE) 
agem = age %>% filter(gender==0, is.na(age_cat) ==FALSE) 
cfps_sum <- cfps %>% group_by(age.wife, age.husband)  %>% summarize(total = n())  %>% mutate(proportion = total/nrow(cfps))


acceptance_age <- ggplot(agef, aes(x = age_average)) + 
  geom_point(aes(y=ave_low, color = "Men: Lower Bound", shape = "Men: Lower Bound")) +
  geom_point(aes(y=ave_high, color = "Men: Upper Bound", shape = "Men: Upper Bound")) +
  geom_line(aes(y=ave_low), color = "red") + 
  geom_line(aes(y=ave_high), color = "red") + 
  geom_point(data = cfps_sum, aes(x= age.wife, y = age.husband, size = proportion), alpha = 0.7, shape = 21) +
  geom_ribbon(aes(ymin=CI_neg_low, ymax=CI_pos_low), alpha=0.2, fill = "salmon")  + 
  geom_ribbon(aes(ymin=CI_neg_high, ymax=CI_pos_high), alpha=0.2, fill = "salmon") +
  geom_point(data= agem, aes(x=ave_low, y= age_average , color = "Women: Lower Bound", shape = "Women: Lower Bound" )) +
  geom_point(data= agem, aes(x=ave_high, y= age_average, color = "Women: Upper Bound", shape = "Women: Upper Bound")) +
  geom_line(data= agem, aes(x=ave_low, y= age_average), color = "blue")  +
  geom_line(data= agem, aes(x=ave_high, y= age_average), color = "blue") +
  geom_ribbon(data= agem, aes(y= age_average, xmin=CI_neg_low, xmax=CI_pos_low), alpha=0.2, fill = "skyblue1")  + 
  geom_ribbon(data= agem, aes(y= age_average, xmin=CI_neg_high, xmax=CI_pos_high), alpha=0.2, fill = "skyblue1") +
  theme_bw() +
  labs(x = "Woman's age", y = "Man's age") +
  scale_colour_manual(name = "", labels = c("Women: Lower Bound", "Women: Upper Bound", "Men: Lower Bound", "Men: Upper Bound"),
                      values = c("red", "red", "blue", "blue")) +   
  scale_shape_manual(name = "", labels = c("Women: Lower Bound", "Women: Upper Bound", "Men: Lower Bound", "Men: Upper Bound"),
                     values = c(16, 17, 21, 24)) +
  scale_size(range = c(1, 3), name="Actual Matches 2012-2016 (Proportion)")

ggsave("acceptance_age.png", plot= acceptance_age, width =9, height = 5)


