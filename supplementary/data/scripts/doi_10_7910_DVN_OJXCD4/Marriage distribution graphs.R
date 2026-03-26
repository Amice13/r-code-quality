
#################################################################################
#
# What do parents want? Parental spousal prefernces in China
# Date: September 2021
# 
# R version 4.0.3
# This code needs the CFPS data files which are available online at: https://opendata.pku.edu.cn/dataverse/CFPS?language=en
#
# names: cfps2010adult_112014.dta (CFPS 2010); ecfps2016adult_201906.dta (CFPS 2016);  ecfps2014adult_201906.dta (CFPS 2014)
#
#################################################################################

## Preparation

rm(list=ls())
library(readstata13) #readstata13_0.9.2
library(haven) # haven_2.3.1 
library(tidyverse) # tidyverse 1.3.0

#I now load the data set: The China family panel study. I keep only the variables needed. I append the data sets and give them convenient names.

########################################### Spousal Supply from CFPS #################################################
################################### Age and educational differences ##################################################

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


cfps2010_original <- read.dta13("cfps2010adult_112014.dta")
cfps2010_original <- cfps2010_original %>%  dplyr::rename(birthyear = qa1y) 
cfps2010 <- cfps2010_original %>%  filter(qe210y >= 2005) %>% select(birthyear, edu, pid, fid, code_a_s, qe210y, qa5code) 


# append 2014 and 2016 data sets
cfps = rbind(cfps2016, cfps2014)

### only those with complete information
cfps <- subset(cfps,is.na(edu) == FALSE & is.na(eeb401y_a_1)==FALSE &
                 is.na(cfps_gender) == FALSE & is.na(eeb4021_a_1)==FALSE &
                 is.na(birthyear) == FALSE & is.na(eeb402y_a_1)==FALSE &
                 edu > 0 & eeb4021_a_1 > 0 &
                 birthyear > 1900 & eeb402y_a_1 > 1900)

names(cfps) <- c("gender", "year_marriage", "edu", "edu_spouse", "birthyear_spouse", "birthyear", "pid", "year")


# Data cleaning. I transform the educational varibale, generate a female dummy, and create variables for husband and wife separately. I only use those between the age of 20 (marriage age) and 50. 

# Put in one category those with master degree and doctorate, for both spouses
cfps$education = cfps$edu
cfps$education[cfps$education ==6] = 5 
cfps$education[cfps$education >=7] = 6 

cfps$education_spouse = cfps$edu_spouse
cfps$education_spouse[cfps$education_spouse == 6] = 5 
cfps$education_spouse[cfps$education_spouse >= 7] = 6 

# female dummy
cfps$female = ifelse(cfps$gender == 0, 1, 0)

## declare the educational levels
edu_levels = c(1,2,3,4,5,6)
edu_labels = c("Illiterate/Semi-literate", "Primary school", "Middle School", 
               "High school", "Tertiary", "Graduate degree")
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


############################ Figure: how did you meet first spouse ########################

cbp2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


cfps2010_original %>% filter(qe609 >0 & qe605y >=1945) %>% group_by(qe605y, qe609) %>% summarise(n = n()) %>% 
  mutate(perc = n/sum(n)) %>% 
  filter(qe609 != 77 & qe609 != 4 & qe609 != 8 & qe609 != 6 & qe609 != 3) %>% 
  ggplot(aes(x =qe605y, y=perc)) + 
  geom_line(aes(color=as.factor(qe609),linetype =as.factor(qe609)), size= 1) +
  theme_bw() +
  xlab("Year of marriage (first spouse)") + ylab("Share of population") +  
  scale_color_manual(values = cbp2, labels = c("Met at school/university", "Met at work", 
                                               "Introduced by friends/relatives", "Arranged by parents"), name = "") +  
  scale_linetype_manual(values = c("dotdash", "dashed",  "solid", "longdash"), 
                        labels = c("Met at school/university", "Met at work", 
                                   "Introduced by friends/relatives", "Arranged by parents") , name = "") + 
  theme(legend.position = "bottom")

ggsave("how_met.png",
       height = 4, width =7)

cfps2016_original %>% filter(qea204 >0 & qea205y >=1980  & qea205y <=2014) %>% 
  group_by(qea204) %>% summarise(n = n()) %>%   
  mutate(perc = n/sum(n))


cfps2016_original %>% filter(qea204 >0 & qea205y >=1980  & qea205y <=2014) %>% 
  group_by(qea205y, qea204) %>% summarise(n = n()) %>%   
  mutate(perc = n/sum(n)) %>% 
  filter(qea204 != 77 & qea204 != 4 & qea204 != 9 & qea204 != 7 & qea204 !=  3& qea204 != 8) %>% 
  ggplot(aes(x =qea205y, y=perc)) + 
  geom_line(aes(color=as.factor(qea204),linetype =as.factor(qea204)), size= 1) +
  theme_bw() +
  xlab("Year of marriage (current spouse)") + ylab("Share of population") +  
  scale_color_manual(values = cbp2, labels = c("Met at school/university", "Met at work", 
                                               "Introduced by relatives","Introduced by friends"), name = "") +  
  scale_linetype_manual(values = c("dotdash", "dashed",  "solid", "longdash"), 
                        labels = c("Met at school/university", "Met at work", 
                                   "Introduced by relatives","Introduced by friends"), name = "") + 
  theme(legend.position = "bottom")

ggsave("how_met2.png", 
       height = 4, width =7)

############################ Figure: Education Distribution ########################

cfps$age_diff <-cfps$age.husband -  cfps$age.wife
cfps$edu_diff <-cfps$education.husband -  cfps$education.wife

cfps %>% filter(age_diff >= -10, age_diff <=15) %>% ggplot(aes(x=as.factor(age_diff))) + ylab("Frequency") +
  geom_histogram(stat= "count") + theme_bw() + xlab("") + 
  scale_x_discrete(breaks=seq(-10, 15, 2))

ggsave("Distribution_age_diff.png",   plot= last_plot(), width =4, height = 4)

cfps %>% ggplot(aes(x=as.factor(age.husband))) + ylab("Frequency") +
  geom_histogram(stat= "count") + theme_bw() + xlab("") + 
  scale_x_discrete(breaks=seq(20, 50, 5))

ggsave("Distribution_age_husband.png",   plot= last_plot(), width =3, height = 4)

cfps %>% ggplot(aes(x=as.factor(age.wife))) + ylab("Frequency") +
  geom_histogram(stat= "count") + theme_bw() + xlab("") + 
  scale_x_discrete(breaks=seq(20, 50, 5))

ggsave("Distribution_age_Wife.png",   plot= last_plot(), width =3, height = 4)

cfps %>% select(age.wife, age.husband) %>%  pivot_longer(cols = c(age.wife, age.husband), names_to = "age") %>%
  ggplot(aes(x=as.factor(value), fill = age)) + ylab("Frequency") +
  geom_histogram(stat= "count", position = "dodge") + theme_bw() + xlab("") + 
  scale_x_discrete(breaks=seq(20, 50, 5)) + 
  scale_fill_manual(values=  cbp2, name = "", labels = c("Husbands", "Wives")) + theme(legend.position="bottom")

ggsave("Distribution_age_both.png",   plot= last_plot(), width =3, height = 4)


cfps %>% ggplot(aes(x=as.factor(edu_diff))) + ylab("Frequency") +
  geom_histogram(stat= "count") + theme_bw() + xlab("") + 
  scale_x_discrete(labels= c("Wife 5 levels more","Wife 4 levels more","Wife 3 levels more","Wife 2 levels more",
                             "Wife 1 level more", "Same education", "Husband 1 level more", "Husband 2 levels more",
                             "Husband 3 levels more", "Husband 4 levels more", "Husband 5 levels more")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Distribution_edu_diff.png",   plot= last_plot(), width =4, height = 4)

cfps %>% ggplot(aes(x=as.factor(education.husband))) + 
  geom_histogram(stat= "count") + theme_bw() + xlab("") + ylim(c(0, 750)) +
  scale_x_discrete(labels= c("No/little  education",  "primary  school",  "secondary  school",
                             "high school", "tertiary education", "graduate degree")) + ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Distribution_edu_husband.png",   plot= last_plot(), width =3, height = 4)

cfps %>% ggplot(aes(x=as.factor(education.wife))) + ylab("Frequency") +
  geom_histogram(stat= "count") + theme_bw() + xlab("") + ylim(c(0, 750)) + 
  scale_x_discrete(labels= c("No/little  education",  "primary  school",  "secondary  school",
                             "high school", "tertiary education", "graduate degree")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Distribution_edu_wife.png",   plot= last_plot(), width =3 , height = 4)

cfps %>% select(education.wife, education.husband) %>%  pivot_longer(cols = c(education.wife, education.husband), names_to = "educ") %>%
  ggplot(aes(x=as.factor(value), fill = educ)) + ylab("Frequency") +
  geom_bar(position = "dodge") + theme_bw() + xlab("") + ylim(c(0, 750)) + 
  scale_x_discrete( labels= c("No/little  education",  "primary  school",  "secondary  school",
                             "high school", "tertiary education", "graduate degree")) + 
   scale_fill_manual(values=  cbp2, name = "", labels = c("Husbands", "Wives")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position="bottom")

ggsave("Distribution_edu_both.png",   plot= last_plot(), width =3 , height = 4)

#### test if distributions are significantly different
ks_education <- ks.test(cfps$education.wife, cfps$education.husband)
ks_education

ks_age <- ks.test(cfps$age.wife, cfps$age.husband)
ks_age

