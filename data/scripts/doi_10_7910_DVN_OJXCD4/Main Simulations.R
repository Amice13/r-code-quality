
#################################################################################
#
# What do parents want? Parental spousal prefernces in China
# Date: September 2021
#
# MAIN Simulation
#
# R version 4.0.3
# 
# This code needs the CFPS data files which are available online at: https://opendata.pku.edu.cn/dataverse/CFPS?language=en
#
# names: cfps2010adult_112014.dta (CFPS 2010); ecfps2016adult_201906.dta (CFPS 2016);  ecfps2014adult_201906.dta (CFPS 2014)
#
#################################################################################

## Preparation
# In part 1, I load all the packages needed for later. I also determine the paths where the figures should be saved later. I set the number of repetitions.

rm(list=ls())
library(readstata13) #readstata13_0.9.2
library(haven) # haven_2.3.1 
library(tidyverse) # tidyverse 1.3.0
library(xtable) # xtable_1.8-4 
library(matchingR) # matchingR_1.3.0
library(texreg) # texreg_1.37.5
library(reshape) # reshape_0.8.8 
library(ipw) #ipw_1.0-11  
library(Rfast) # Rfast_2.0.1 
library(margins) # margins_0.3.23
library(stargazer) # stargazer_5.2.2 
library(ggpubr) # ggpubr_0.4.0 
library(mgcv) # mgcv_1.8-33        

#I now load the data set: The China family panel study. I keep only the variables needed. I append the data sets and give them convenient names.

########################################### Spousal Supply from CFPS #################################################
################################### Age and educational differences ##################################################

# CFPS 2016, create data set with those who married in the past two years and the pertinent variables
cfps2016_original = read.dta13("ecfps2016adult_201906.dta")
cfps2016_original <- cfps2016_original %>%  dplyr::rename(birthyear = cfps_birthy,
                                                          edu = cfps2016edu, 
                                                          urban = urban16)
cfps2016 = cfps2016_original %>%  filter(eeb401y_a_1 >= 2014 & eeb401y_a_1 <=2016) %>% 
  select(cfps_gender, eeb401y_a_1, edu, eeb4021_a_1, eeb402y_a_1, birthyear, pid)
cfps2016$year = 2016

# CFPS 2014, create data set with those who married in the past two years and the pertinent variables
cfps2014_original <- read.dta13("ecfps2014adult_201906.dta")
cfps2014_original <- cfps2014_original %>%  dplyr::rename(birthyear = cfps_birthy,
                                                          edu = cfps2014edu, 
                                                          urban = urban14 )
cfps2014 <-  cfps2014_original %>%  filter(eeb401y_a_1 >= 2012 & eeb401y_a_1 <=2014) %>% 
  select(cfps_gender, eeb401y_a_1, edu, eeb4021_a_1, eeb402y_a_1, birthyear, pid)
cfps2014$year = 2014

# append 2014 and 2016 data sets
cfps = rbind(cfps2016, cfps2014)

### only those with complete information
cfps <- subset(cfps,is.na(edu) == FALSE & is.na(eeb401y_a_1)==FALSE &
                 is.na(cfps_gender) == FALSE & is.na(eeb4021_a_1)==FALSE &
                 is.na(birthyear) == FALSE & is.na(eeb402y_a_1)==FALSE &
                 edu > 0 & eeb4021_a_1 > 0 &
                 birthyear > 1900 & eeb402y_a_1 > 1900)

names(cfps) <- c("gender", "year_marriage", "edu", "edu_spouse", "birthyear_spouse", "birthyear", "pid", "year")

# Data cleaning. I transform the educational varibale, generate a female dummy, and create variables for husband and wife separately. 
# I only use those between the age of 20 (marriage age) and 50. 

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


cfps <- cfps %>% filter(is.na(age.wife) == FALSE, is.na(age.husband) == FALSE, 
                        is.na(education.husband) == FALSE, is.na(education.wife) == FALSE  )

# I create two data sets: one for women and one for men. I then calculate the age and educational difference matrixes. I create separate matrices for the positive difference and the negative difference.

## two different datasets
## women
women<- data.frame(cfps$education.wife, cfps$age.wife)
names(women) <-c("education", "age")

## men
men<- data.frame(cfps$education.husband, cfps$age.husband)
names(men) <-c("education", "age")

### number of observations
nobs <- length(men$education)

############# difference matrices
age.w.matrix <- matrix(c(women[,2]),nrow=nobs,ncol=nobs,byrow=TRUE) 
age.m.matrix <- matrix(c(men[,2]),nrow=nobs,ncol=nobs,byrow=TRUE) 
## men-women age difference matrices: for women utility
# i: rows
# j: columns
# age.menminuswomen.pos
# woman 1: (men(1) - women(1)    men(2) - women(1)  ....  men(n)- women (1))
# woman 2: (men(1) - women(2)    men(2) - women(2)  ....  men(n)- women (2))
# ...
# woman N: (men(n) - women(N)    men(2) - women(N)  ....  men(n)- women (N))

age.menminuswomen.pos <- age.m.matrix - t(age.w.matrix)
age.menminuswomen.pos[age.menminuswomen.pos<0] <- 0
age.menminuswomen.neg <- age.m.matrix - t(age.w.matrix)
age.menminuswomen.neg[age.menminuswomen.neg>0] <- 0
age.menminuswomen.neg <- abs(age.menminuswomen.neg)

### women-men age difference matrices: for men utility
age.womenminusmen.pos <- age.w.matrix - t(age.m.matrix)
age.womenminusmen.pos[age.womenminusmen.pos<0] <- 0
age.womenminusmen.neg <- age.w.matrix - t(age.m.matrix)
age.womenminusmen.neg[age.womenminusmen.neg>0] <- 0
age.womenminusmen.neg <- abs(age.womenminusmen.neg)

############### education
edu.w.matrix <- matrix(c(women[,1]),nrow=nobs,ncol=nobs,byrow=TRUE) 
edu.m.matrix <- matrix(c(men[,1]),nrow=nobs,ncol=nobs,byrow=TRUE) 

## men-women edu difference matrices
edu.menminuswomen.pos <- edu.m.matrix - t(edu.w.matrix)
edu.menminuswomen.pos[edu.menminuswomen.pos<0] <- 0
edu.menminuswomen.neg <- edu.m.matrix - t(edu.w.matrix)
edu.menminuswomen.neg[edu.menminuswomen.neg>0] <- 0
edu.menminuswomen.neg <- abs(edu.menminuswomen.neg)

### women-men edu difference matrices
edu.womenminusmen.pos <- edu.w.matrix - t(edu.m.matrix)
edu.womenminusmen.pos[edu.womenminusmen.pos<0] <- 0
edu.womenminusmen.neg <- edu.w.matrix - t(edu.m.matrix)
edu.womenminusmen.neg[edu.womenminusmen.neg>0] <- 0
edu.womenminusmen.neg <- abs(edu.womenminusmen.neg)

rm(edu.w.matrix, edu.m.matrix, age.w.matrix, age.m.matrix)

## Preference estimation
# To estimate the preferences, I use the data from the QSAMPY. I load the data and, for parental prefernces, focus on those on the GLP and who search on behalf of someone else. I then adjust the educational variable. I create the varibales for a positive and negative difference for age and education. I also create dummies for being over 30 and having a university degree.

################################################# BASELINE #################################################################
########### get the preferences 
qsampy = read_dta("meet_data_final.dta")
# Green Lake Park Sample
glp = subset(qsampy, uni==0)
# Parents sample (defined as searching for someone else)
parents = subset(glp, search_who ==1)

## re-define the educational preferences: make the categories broader so they fit to the CFPS data
redefine_education <- function(var){
  var.new =  var
  var.new[var.new ==99 | var.new==98] = NA
  var.new[var<=1] = 1 ## illiterate/semi-literate
  var.new[var==2] = 2 ## primary school
  var.new[var==3] = 3 ## middle school
  var.new[var==4] = 4 ## high school
  var.new[var==5] = 4 ## high school
  var.new[var==6] = 5 ## college/university
  var.new[var==7] = 5 ## college/university
  var.new[var==8] = 6 ## master
  var.new
}

parents$edu <- parents$education%>% 
  redefine_education()
parents$profile.edu <- parents$profileedu 
parents$profile.edu[parents$profileedu==4] = 6 ## graduate
parents$profile.edu[parents$profileedu==3] = 5 ## university
parents$profile.edu[parents$profileedu==2] = 4 ## high school
parents$profile.edu[parents$profileedu==1] = 3 ## middle school

## Difference in education : Education(profile) minus education(individual)
parents$edu.diff = parents$profile.edu -parents$edu
parents$edu.diff.pos = ifelse(parents$edu.diff > 0, parents$edu.diff[parents$edu.diff>0], 0)
parents$edu.diff.neg = ifelse(parents$edu.diff < 0, abs(parents$edu.diff[parents$edu.diff <0]), 0)

## squared terms
parents$age_diff_pos2= parents$age_diff_pos*parents$age_diff_pos
parents$age_diff_neg2 = parents$age_diff_neg*parents$age_diff_neg

############################ 2. two categories for age and education #######################################
parents$high.edu <- ifelse(parents$edu >=5,1,0)
parents$over30 <- ifelse(parents$age >=30, 1, 0)
women$high.edu <- ifelse(women$education >=5,1,0)
women$over30 <- ifelse(women$age >=30, 1, 0)
men$high.edu <- ifelse(men$education >=5,1,0)
men$over30 <- ifelse(men$age >=30, 1, 0)



# I now estimate the preferences using logit for men and women separatly, for the 3 specifications.


######################################################################################################################
#################################### Estimation Spezification 1 ######################################################
######################################################################################################################

logit1.male <- glm(meet ~ age_diff_pos + age_diff_pos2 + age_diff_neg + age_diff_neg2 + edu.diff.pos + edu.diff.neg
                   , data = subset(parents, choose_female==0), family = "binomial")

logit1.female <- glm(meet ~ age_diff_pos + age_diff_pos2 + age_diff_neg + age_diff_neg2 + edu.diff.pos + edu.diff.neg
                     , data = subset(parents, choose_female ==1), family = "binomial")

######################################################################################################################
#################################### Estimation Spezification 2 ######################################################
######################################################################################################################

logit2.male <- glm(meet ~ age_diff_pos + age_diff_pos2 + age_diff_neg + age_diff_neg2 +
                     edu.diff.pos*high.edu + edu.diff.neg*high.edu,
                   data = subset(parents, choose_female ==0), family = "binomial")
logit2.female <- glm(meet ~ age_diff_pos + age_diff_pos2 + age_diff_neg + age_diff_neg2 +
                       edu.diff.pos*high.edu + edu.diff.neg*high.edu,
                     data = subset(parents, choose_female ==1), family = "binomial")

######################################################################################################################
#################################### Estimation Spezification 3 ######################################################
######################################################################################################################

logit3.male <- glm(meet ~ age_diff_pos*over30 + age_diff_pos2*over30 + age_diff_neg*over30 + age_diff_neg2*over30 +
                     edu.diff.pos + edu.diff.neg,
                   data = subset(parents, choose_female ==0), family = "binomial")
logit3.female <- glm(meet ~ age_diff_pos*over30 + age_diff_pos2*over30 + age_diff_neg*over30 + age_diff_neg2*over30 +
                       edu.diff.pos + edu.diff.neg,
                     data = subset(parents, choose_female ==1), family = "binomial")

# In this part, I repeat the same thing, now using only the student sample. We only estimate one specification here.


###############################################################################################################
####################################### Students ##############################################################
###############################################################################################################

students = subset(qsampy, uni ==1)

## re-define the educational preferences: make the categories broader
students$edu <- students$Ceducation%>%  redefine_education() ###### TEST JB: I replaced "Ceducation" with "education" because Ceducation did not exist
###  ER: THis was the mistake I guess, I forgot to add the variable Ceducation to the data set
### The edu_diff that is already in the data set and the one created after this function are slightly different

students$profile.edu[students$profileedu==4] = 6 ## graduate
students$profile.edu[students$profileedu==3] = 5 ## university
students$profile.edu[students$profileedu==2] = 4 ## high school
students$profile.edu[students$profileedu==1] = 3 ## middle school

## Difference in education : Education(profile) minus education(individual)
students$edu_diff = students$profile.edu -students$edu
students$edu_diff_pos = ifelse(students$edu_diff > 0, students$edu_diff[students$edu_diff>0], 0)
students$edu_diff_neg = ifelse(students$edu_diff < 0, abs(students$edu_diff[students$edu_diff <0]), 0)


#### Students Estimation #### TEST JB: replaced edu.diff.pos and edu.diff.neg with edu_diff_pos and edu_diff_neg
### ER:I adapted the lines 286-288

logitst.male <- glm(meet ~ age_diff_pos + age_diff_pos2+ age_diff_neg + age_diff_neg2 +
                      edu_diff_pos + edu_diff_neg, 
                    data = subset(students, choose_female ==0), family = "binomial")
logitst.female <- glm(meet ~ age_diff_pos + age_diff_pos2 + age_diff_neg + age_diff_neg2 +
                        edu_diff_pos + edu_diff_neg, data = subset(students, choose_female ==1), family = "binomial")

# The final table is now created with stargazer.

################################################
################# Final table ##################
################################################

stargazer_table <- stargazer(logit1.female, logit2.female, logit3.female, 
                             logit1.male, logit2.male, logit3.male, logitst.female, logitst.male) 


##################################
############ probability weighting

### use the qsampy data that does not have meet data (one observation per respondent)
### ER: I changed the data set here as well, so we do not have to include the other data set

parents_ipw <- read_dta("summary_statistics_glp.dta")
parents_ipw$edu <- parents_ipw$education%>%  ## change educational definitions
  redefine_education()
parents_ipw <- parents_ipw %>%  ### change gender identicator, drop incomplete obs, add panel indicator
  select(edu, age, search_female) %>% mutate(gender = search_female) %>% select(-search_female) %>% mutate(panel=1) %>% 
  filter(is.na(edu)==FALSE, is.na(age)==FALSE)

## two dataset: one for women and one for men
parents_ipw_women <- parents_ipw %>%  filter(gender==1) %>% select(edu, age,panel)
parents_ipw_men <- parents_ipw %>%  filter(gender==0)%>% select(edu, age, panel)
### cfps data set for men and for women
for_ipw_women<- women %>% mutate(edu = education) %>% select(edu, age) %>% mutate(panel=0)
for_ipw_men<- men %>% mutate(edu = education) %>% select(edu, age) %>% mutate(panel=0)

## append the two data sets
ipw_matches_men <- rbind(parents_ipw_men, for_ipw_men)
ipw_matches_women <- rbind(parents_ipw_women, for_ipw_women)

mod_men <- gam(panel ~  s(age) + poly(edu,2), 
               data=ipw_matches_men, family = "binomial")
mod_women <- gam(panel ~  s(age) + poly(edu,2), 
                 data=ipw_matches_women, family = "binomial")


ipw_matches_men <- ipw_matches_men %>%
  mutate(propensity_gam = predict(mod_men, type = "response"),
         weight_gam =  1 / (1 - propensity_gam))

ipw_matches_women <- ipw_matches_women %>%
  mutate(propensity_gam = predict(mod_women, type = "response"),
         weight_gam =  1 / (1 - propensity_gam))

ipw_matches_men <- ipw_matches_men %>% filter(panel==0)
ipw_matches_women <- ipw_matches_women %>% filter(panel==0)


### add to cfps data set
cfps$weights_women <- ipw_matches_women$propensity_gam
cfps$weights_men <- ipw_matches_men$propensity_gam

#rm(parents, qsampy, glp,ipw_matches_men, ipw_matches_women, for_ipw_women, for_ipw_men,
#   parents_ipw, parents_ipw_women, parents_ipw_men)


######################################################################################################################
######################################################################################################################
###################### Simualtions WITHOUT the outside option to stay single #########################################
######################################################################################################################
######################################################################################################################

# As preparation, I create the vectors where the results are saved.


############ Vectors for descriptive statistics at the end ###############
# Spezification 1, 2, 3 and students
corr.age = c(NA, NA, NA, NA)
sharesame.age = c(NA, NA, NA, NA)
spread.age = c(NA, NA, NA, NA)
corr.edu = c(NA, NA, NA, NA)
sharesame.edu = c(NA, NA, NA, NA)
spread.edu = c(NA, NA, NA, NA)
mean.edu= c(NA, NA, NA, NA)
mean.age = c(NA, NA, NA, NA)

######################################################################################################################
#################################### Estimation Spezification 1 ######################################################
######################################################################################################################

### define
spe = 1

### calculate the predicted meeting likelihood for each man/women in the general population
### woman 1: utility for man 1    woman 1: utility for man 2 ................ woman 1: utility for man n
### woman 2: utility for man 2    woman 2: utility for man 2 ................ woman 2: utility for man n
### .......
### woman n: utility for man 1    woman n: utility for man 2 ................ woman n: utility for man n

utility.women <- exp(logit1.female$coefficients[1] + logit1.female$coefficients[2]*age.menminuswomen.pos +
                       logit1.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                       logit1.female$coefficients[4]*age.menminuswomen.neg +
                       logit1.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                       logit1.female$coefficients[6]*edu.menminuswomen.pos +
                       logit1.female$coefficients[7]*edu.menminuswomen.neg)/
  (1+  exp(logit1.female$coefficients[1] + logit1.female$coefficients[2]*age.menminuswomen.pos +
             logit1.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
             logit1.female$coefficients[4]*age.menminuswomen.neg +
             logit1.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
             logit1.female$coefficients[6]*edu.menminuswomen.pos +
             logit1.female$coefficients[7]*edu.menminuswomen.neg))
utility.men = exp(logit1.male$coefficients[1] + logit1.male$coefficients[2]*age.womenminusmen.pos +
                    logit1.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logit1.male$coefficients[4]*age.womenminusmen.neg +
                    logit1.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logit1.male$coefficients[6]*edu.womenminusmen.pos +
                    logit1.male$coefficients[7]*edu.womenminusmen.neg)/
  (1+  exp(logit1.male$coefficients[1] + logit1.male$coefficients[2]*age.womenminusmen.pos +
             logit1.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit1.male$coefficients[4]*age.womenminusmen.neg +
             logit1.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit1.male$coefficients[6]*edu.womenminusmen.pos +
             logit1.male$coefficients[7]*edu.womenminusmen.neg))

### create the output vectors
outcome.age.women =rep(NA, nobs)
outcome.edu.women =rep(NA,  nobs)
outcome.age.men =rep(NA,  nobs)
outcome.edu.men =rep(NA, nobs)

### establish ranking, the higher the utility the higher the rank
rank.women = matrix(NA, nrow = nobs, ncol=nobs)
rank.men = matrix(NA, nrow = nobs, ncol=nobs)
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}
## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)

################ Gale-Shapley without rejections ###############
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)
## outcome matrix
outcome.age.women = men[gs$engagements,2] 
outcome.edu.women = men[gs$engagements,1]
outcome.age.men = women[gs$proposals,2] 
outcome.edu.men = women[gs$proposals,1]

################################################################
########### Summary stats and Graphs ###########################
################################################################

## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### Descriptives: Correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr.age[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr.edu[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

######## AGE #####
df1 <- data.frame(cfps$age.husband - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband actual age - wife simulated age", "weights_men", "weights_women")

##Descriptives: spread & share of same level
spread.age[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame.age[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                    is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean.age[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)

age_spe1 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual age - wife actual age`,
                                 `Husband actual age - wife simulated age`))

df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age", "weights_men", "weights_women")

age_spe1_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual age - wife actual age`,
                                       `Husband simulated age - wife actual age`))

#### EDUCATION #######
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$education.husband - cfps$edu.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband actual education - wife simulated education", "weights_men", "weights_women")
### Descriptives
spread.edu[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame.edu[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                    is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean.edu[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)

edu_spe1 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual education - wife actual education`,
                                 `Husband actual education - wife simulated education`))

df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education", "weights_men", "weights_women")

edu_spe1_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual education - wife actual education`,
                                       `Husband simulated education - wife actual education`))

######################################################################################################################
#################################### Estimation Spezification 2 ######################################################
######################################################################################################################

### define
spe = 2

### calculate the predicted meeting likelihood for each man/women in the general population
utility.women = matrix(NA, nrow = nobs, ncol=nobs)

utility.women = exp(logit2.female$coefficients[1]  +
                      logit2.female$coefficients[2]*age.menminuswomen.pos +
                      logit2.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logit2.female$coefficients[4]*age.menminuswomen.neg +
                      logit2.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                      logit2.female$coefficients[6]*edu.menminuswomen.pos +
                      logit2.female$coefficients[7]*women$high.edu + 
                      logit2.female$coefficients[8]*edu.menminuswomen.neg +
                      logit2.female$coefficients[9]*edu.menminuswomen.pos*women$high.edu +
                      logit2.female$coefficients[10]*edu.menminuswomen.neg*women$high.edu)   /
  (1+ exp(logit2.female$coefficients[1]  +
            logit2.female$coefficients[2]*age.menminuswomen.pos +
            logit2.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
            logit2.female$coefficients[4]*age.menminuswomen.neg +
            logit2.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
            logit2.female$coefficients[6]*edu.menminuswomen.pos +
            logit2.female$coefficients[7]*women$high.edu + 
            logit2.female$coefficients[8]*edu.menminuswomen.neg +
            logit2.female$coefficients[9]*edu.menminuswomen.pos*women$high.edu +
            logit2.female$coefficients[10]*edu.menminuswomen.neg*women$high.edu))

utility.men = exp(logit2.male$coefficients[1] +
                    logit2.male$coefficients[2]*age.womenminusmen.pos +
                    logit2.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logit2.male$coefficients[4]*age.womenminusmen.neg +
                    logit2.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logit2.male$coefficients[6]*edu.womenminusmen.pos +
                    logit2.male$coefficients[7]*men$high.edu+
                    logit2.male$coefficients[8]*edu.womenminusmen.neg +
                    logit2.male$coefficients[9]*edu.womenminusmen.pos*men$high.edu +
                    logit2.male$coefficients[10]*edu.womenminusmen.neg*men$high.edu)   /
  (1+  exp(logit2.male$coefficients[1] +
             logit2.male$coefficients[2]*age.womenminusmen.pos +
             logit2.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit2.male$coefficients[4]*age.womenminusmen.neg +
             logit2.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit2.male$coefficients[6]*edu.womenminusmen.pos +
             logit2.male$coefficients[7]*men$high.edu+
             logit2.male$coefficients[8]*edu.womenminusmen.neg +
             logit2.male$coefficients[9]*edu.womenminusmen.pos*men$high.edu +
             logit2.male$coefficients[10]*edu.womenminusmen.neg*men$high.edu) )

### establish the output matrixes
outcome.age.women =rep(NA, nobs)
outcome.edu.women =rep(NA, nobs)
outcome.age.men =rep(NA,nobs)
outcome.edu.men =rep(NA, nobs)

rank.women = matrix(NA, nrow = nobs, ncol=nobs)
rank.men = matrix(NA, nrow = nobs, ncol=nobs)
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}

## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)

### gale shapley matching
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)

## outcome matrix
## outcome matrix
outcome.age.women = men[gs$engagements,2] 
outcome.edu.women = men[gs$engagements,1]
outcome.age.men = women[gs$proposals,2] 
outcome.edu.men = women[gs$proposals,1]

## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### Spe. 2 correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr.age[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr.edu[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

##################### Spe. 2 AGE ##################
df1 <- data.frame(cfps$age.husband - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband actual age - wife simulated age", "weights_men", "weights_women")

##Descriptives: spread & share of same level
spread.age[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame.age[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                    is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean.age[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)

age_spe2 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual age - wife actual age`,
                                 `Husband actual age - wife simulated age`))

df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age", "weights_men", "weights_women")

age_spe2_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual age - wife actual age`,
                                       `Husband simulated age - wife actual age`))

#################### Spe. 2 EDUCATION ##################################
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$education.husband - cfps$edu.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband actual education - wife simulated education", "weights_men", "weights_women")
## Descriptives
spread.edu[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame.edu[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                    is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean.edu[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)

edu_spe2 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual education - wife actual education`,
                                 `Husband actual education - wife simulated education`))

df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education", "weights_men", "weights_women")

edu_spe2_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual education - wife actual education`,
                                       `Husband simulated education - wife actual education`))



######################################################################################################################
#################################### Estimation Spezification 3 ######################################################
######################################################################################################################

### define
spe = 3

### calculate the predicted meeting likelihood for each man/women in the general population
utility.women = exp(logit3.female$coefficients[1]  +
                      logit3.female$coefficients[2]*age.menminuswomen.pos +
                      logit3.female$coefficients[3]* women$over30 +
                      logit3.female$coefficients[4]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logit3.female$coefficients[5]*age.menminuswomen.neg +
                      logit3.female$coefficients[6]*age.menminuswomen.neg*age.menminuswomen.neg +  
                      logit3.female$coefficients[7]*edu.menminuswomen.pos +
                      logit3.female$coefficients[8]*edu.menminuswomen.neg +
                      logit3.female$coefficients[9]*age.menminuswomen.pos*women$over30 +
                      logit3.female$coefficients[10]*age.menminuswomen.pos*age.menminuswomen.pos*women$over30+
                      logit3.female$coefficients[11]*age.menminuswomen.neg*women$over30 +
                      logit3.female$coefficients[12]*age.menminuswomen.neg*age.menminuswomen.neg*women$over30) /
  (1+ exp(logit3.female$coefficients[1]  +
            logit3.female$coefficients[2]*age.menminuswomen.pos +
            logit3.female$coefficients[3]* women$over30 +
            logit3.female$coefficients[4]*age.menminuswomen.pos*age.menminuswomen.pos +
            logit3.female$coefficients[5]*age.menminuswomen.neg +
            logit3.female$coefficients[6]*age.menminuswomen.neg*age.menminuswomen.neg +  
            logit3.female$coefficients[7]*edu.menminuswomen.pos +
            logit3.female$coefficients[8]*edu.menminuswomen.neg +
            logit3.female$coefficients[9]*age.menminuswomen.pos*women$over30 +
            logit3.female$coefficients[10]*age.menminuswomen.pos*age.menminuswomen.pos*women$over30 +
            logit3.female$coefficients[11]*age.menminuswomen.neg*women$over30  +
            logit3.female$coefficients[12]*age.menminuswomen.neg*age.menminuswomen.neg*women$over30 ))

tility.men = exp(logit3.male$coefficients[1] +
                   logit3.male$coefficients[2]*age.womenminusmen.pos +
                   logit3.male$coefficients[3]* men$over30 +
                   logit3.male$coefficients[4]*age.womenminusmen.pos*age.womenminusmen.pos +
                   logit3.male$coefficients[5]*age.womenminusmen.neg +
                   logit3.male$coefficients[6]*age.womenminusmen.neg*age.womenminusmen.neg +  
                   logit3.male$coefficients[7]*edu.womenminusmen.pos +
                   logit3.male$coefficients[8]*edu.womenminusmen.neg +
                   logit3.male$coefficients[9]*age.womenminusmen.pos*men$over30 +
                   logit3.male$coefficients[10]*age.womenminusmen.pos*age.womenminusmen.pos*men$over30 +
                   logit3.male$coefficients[11]*age.womenminusmen.neg*men$over30 +
                   logit3.male$coefficients[12]*age.womenminusmen.neg*age.womenminusmen.neg*men$over30) /
  (1+  exp(logit3.male$coefficients[1] +
             logit3.male$coefficients[2]*age.womenminusmen.pos +
             logit3.female$coefficients[3]* men$over30 +
             logit3.male$coefficients[4]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit3.male$coefficients[5]*age.womenminusmen.neg +
             logit3.male$coefficients[6]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit3.male$coefficients[7]*edu.womenminusmen.pos +
             logit3.male$coefficients[8]*edu.womenminusmen.neg +
             logit3.male$coefficients[9]*age.womenminusmen.pos*men$over30 +
             logit3.male$coefficients[10]*age.womenminusmen.pos*age.womenminusmen.pos*men$over30 +
             logit3.male$coefficients[11]*age.womenminusmen.neg*men$over30 +
             logit3.male$coefficients[12]*age.womenminusmen.neg*age.womenminusmen.neg*men$over30 ) )



### establish the output matrixes
outcome.age.women =rep(NA,nobs)
outcome.edu.women =rep(NA, nobs)
outcome.age.men =rep(NA, nobs)
outcome.edu.men =rep(NA, nobs)

rank.women = matrix(NA, nrow = nobs, ncol=nobs)
rank.men = matrix(NA, nrow = nobs, ncol=nobs)
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}

## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)

### gale shapley matching
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)

outcome.age.women = men[gs$engagements,2] 
outcome.edu.women = men[gs$engagements,1]
outcome.age.men = women[gs$proposals,2] 
outcome.edu.men = women[gs$proposals,1]


## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### Spe 3. correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr.age[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr.edu[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

#################### Spe. 3 AGE ##############
df1 <- data.frame(cfps$age.husband - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband actual age - wife simulated age", "weights_men", "weights_women")

##Descriptives: spread & share of same level
spread.age[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame.age[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                    is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean.age[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)

age_spe3 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual age - wife actual age`,
                                 `Husband actual age - wife simulated age`))

df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age", "weights_men", "weights_women")

age_spe3_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual age - wife actual age`,
                                       `Husband simulated age - wife actual age`))

########## Spe. 3 EDUCATION #############
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$education.husband - cfps$edu.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband actual education - wife simulated education", "weights_men", "weights_women")
## Descriptives
spread.edu[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame.edu[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                    is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean.edu[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)

edu_spe3 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual education - wife actual education`,
                                 `Husband actual education - wife simulated education`))

df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education", "weights_men", "weights_women")

edu_spe3_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual education - wife actual education`,
                                       `Husband simulated education - wife actual education`))


###############################################################################################################
####################################### Students ##############################################################
###############################################################################################################

### define
spe = 4

### calculate the predicted meeting likelihood for each man/women in the general population
utility.women = exp(logitst.female$coefficients[1] + logitst.female$coefficients[2]*age.menminuswomen.pos +
                      logitst.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logitst.female$coefficients[4]*age.menminuswomen.neg +
                      logitst.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                      logitst.female$coefficients[6]*edu.menminuswomen.pos +
                      logitst.female$coefficients[7]*edu.menminuswomen.neg)/
  (1+  exp(logitst.female$coefficients[1] + logitst.female$coefficients[2]*age.menminuswomen.pos +
             logitst.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
             logitst.female$coefficients[4]*age.menminuswomen.neg +
             logitst.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
             logitst.female$coefficients[6]*edu.menminuswomen.pos +
             logitst.female$coefficients[7]*edu.menminuswomen.neg))

utility.men = exp(logitst.male$coefficients[1] + logitst.male$coefficients[2]*age.womenminusmen.pos +
                    logitst.male$coefficients[3]*age.womenminusmen.pos *age.womenminusmen.pos +
                    logitst.male$coefficients[4]*age.womenminusmen.neg  +
                    logitst.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg  +  
                    logitst.male$coefficients[6]*edu.womenminusmen.pos +
                    logitst.male$coefficients[7]*edu.womenminusmen.neg) /
  (1+  exp(logitst.male$coefficients[1] + logitst.male$coefficients[2]*age.womenminusmen.pos +
             logitst.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logitst.male$coefficients[4]*age.womenminusmen.neg +
             logitst.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logitst.male$coefficients[6]*edu.womenminusmen.pos +
             logitst.male$coefficients[7]*edu.womenminusmen.neg))


### establish the output matrixes
outcome.age.women =rep(NA, ncol=nrep, nrow = nobs)
outcome.edu.women =rep(NA, ncol=nrep, nrow = nobs)
outcome.age.men =rep(NA, ncol=nrep, nrow = nobs)
outcome.edu.men =rep(NA, ncol=nrep, nrow = nobs)

rank.women = matrix(NA, nrow = nobs, ncol=nobs)
rank.men = matrix(NA, nrow = nobs, ncol=nobs)
###### Students Loop

for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}

## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)

### gale shapley matching
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)

## outcome matrix
outcome.age.women= men[gs$engagements,2] 
outcome.edu.women = men[gs$engagements,1]
outcome.age.men = women[gs$proposals,2] 
outcome.edu.men = women[gs$proposals,1]


## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr.age[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr.edu[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

############## Students AGE ########################
df1 <- data.frame(cfps$age.husband - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband actual age - wife simulated age", "weights_men", "weights_women")

##Descriptives: spread & share of same level
spread.age[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame.age[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                    is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean.age[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)

age_spe4 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual age - wife actual age`,
                                 `Husband actual age - wife simulated age`))

df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age", "weights_men", "weights_women")

age_spe4_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual age - wife actual age`,
                                       `Husband simulated age - wife actual age`))

########### Students EDUCATION ######################
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$education.husband - cfps$edu.wife.pred, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband actual education - wife simulated education", "weights_men", "weights_women")
## Descriptives
spread.edu[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame.edu[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                    is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean.edu[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
##

edu_spe4 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual education - wife actual education`,
                                 `Husband actual education - wife simulated education`))

df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education", "weights_men", "weights_women")

edu_spe4_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual education - wife actual education`,
                                       `Husband simulated education - wife actual education`))

#################################################################################################
##################################" Graphs ######################################################
##################################################################################################

#### Age
plot_age_spe1 <- age_spe1 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_age_spe2 <- age_spe2 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_age_spe3 <- age_spe3 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") +
  ylab("Density")  +  lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2")

plot_age_spe4 <- age_spe4 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") +
  ylab("Density")  +  lims(x=c(-20,20), y=c(0, 0.8)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 


#### Education
plot_edu_spe1 <- edu_spe1 %>% ggplot(aes(value, fill = variable)) +   
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2")

plot_edu_spe2 <- edu_spe2 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") + theme_classic() +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9))  + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_edu_spe3 <- edu_spe3 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9)) + theme_classic()  + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2")

plot_edu_spe4 <- edu_spe4 %>%  ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6),y=c(0, 0.9)) + theme_classic()  + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

edu_all <- ggarrange(plot_edu_spe1, plot_edu_spe2, plot_edu_spe3, 
                     ncol=1, nrow=3, common.legend = TRUE, legend="bottom", 
                     labels = c("Specification 1", "Specification 2", "Specification 3"))
age_all <- ggarrange(plot_age_spe1, plot_age_spe2, plot_age_spe3, 
                     ncol=1, nrow=3, common.legend = TRUE, legend="bottom", 
                     labels = c("Specification 1", "Specification 2", "Specification 3"))

######################## Weighted: MEN ########################
#### Age
plot_age_spe1_men <- age_spe1 %>% ggplot(aes(value, fill = variable, weight = weights_men)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

#### Education
plot_edu_spe1_men <- edu_spe1 %>% ggplot(aes(value, fill = variable, weight = weights_men)) +   
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + expand_limits(x=c(-6,6), y=c(0, 0.9)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2")


######################## Weighted: WOMEN ########################
#### Age
plot_age_spe1_women <- age_spe1_women %>% ggplot(aes(value, fill = variable, weight = weights_women)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

#### Education
plot_edu_spe1_women <- edu_spe1_women %>% ggplot(aes(value, fill = variable, weight = weights_women)) +   
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + expand_limits(x=c(-6,6), y=c(0, 0.9)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2")



################################### Save the graphs ############################################

ggsave("age_all.png", plot= age_all, width =5, height = 11)
ggsave("edu_all.png", plot= edu_all, width =5, height = 11)

ggsave("plot_age_spe4.png", plot= plot_age_spe4, width =5, height = 6)
ggsave("plot_edu_spe4.png", plot= plot_edu_spe4, width =5, height =6)
########################## Comparison table #############################################
### Age
corr.age = round(corr.age, 3)
sharesame.age = round(sharesame.age/nobs, 3)
spread.age = round(spread.age, 3)
mean.age = round(mean.age, 3)

## Education
corr.edu = round(corr.edu, 3)
sharesame.edu = round(sharesame.edu/nobs, 3)
spread.edu = round(spread.edu, 3)
mean.edu = round(mean.edu, 3)


### Descriptives: Correlations real
forcorr = cbind(cfps$age.husband, cfps$age.wife)
corr_age = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife)
corr_edu = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

##Descriptives: spread & share of same level
cfps <- cfps %>% mutate(age_difference = age.husband - age.wife,
                        edu_difference = education.husband - education.wife,
                        spead_age = sd(age_difference)/ mean(age_difference),
                        sharesame_age = length(which(abs(age_difference)<2.5 &
                                                       is.na(age_difference)==FALSE))/nobs,
                        spead_education = sd(edu_difference)/ mean(edu_difference),
                        sharesame_education = length(which(abs(edu_difference)==0 &
                                                             is.na(edu_difference)==FALSE))/nobs)

#### education
edu_spe1test <- edu_spe1 %>% filter(variable =="Husband actual education - wife simulated education")
edu_spe2test <- edu_spe2 %>% filter(variable =="Husband actual education - wife simulated education")
edu_spe3test <- edu_spe3 %>% filter(variable =="Husband actual education - wife simulated education")
edu_spe4test <- edu_spe4 %>% filter(variable =="Husband actual education - wife simulated education")

ks_edu1 <- ks.test(cfps$edu_difference, edu_spe1test$value )
ks_edu2 <- ks.test(cfps$edu_difference, edu_spe2test$value )
ks_edu3 <- ks.test(cfps$edu_difference, edu_spe3test$value )
ks_edu4 <- ks.test(cfps$edu_difference, edu_spe4test$value )

ks_edu = round(c(ks_edu1$statistic, ks_edu2$statistic, ks_edu3$statistic, ks_edu4$statistic), 3)
###############################################

#### age

age_spe1test <- age_spe1 %>% filter(variable =="Husband actual age - wife simulated age")
age_spe2test <- age_spe2 %>% filter(variable =="Husband actual age - wife simulated age")
age_spe3test <- age_spe3 %>% filter(variable =="Husband actual age - wife simulated age")
age_spe4test <- age_spe4 %>% filter(variable =="Husband actual age - wife simulated age")

ks_age1 <- ks.test(cfps$age_difference, age_spe1test$value )
ks_age2 <- ks.test(cfps$age_difference, age_spe2test$value )
ks_age3 <- ks.test(cfps$age_difference, age_spe3test$value )
ks_age4 <- ks.test(cfps$age_difference, age_spe4test$value )

ks_age = round(c(ks_age1$statistic, ks_age2$statistic, ks_age3$statistic, ks_age4$statistic), 3)

################ for table 9
cfps$edu_difference %>% summary() %>% round(3)
mean.edu

cor(cfps$education.husband, cfps$education.wife) %>% round(3)
corr.edu

cfps$spead_education %>% summary() %>% round(3)
spread.edu

cfps$sharesame_education %>% summary() %>% round(3)
sharesame.edu

ks_edu

###
cfps$age_difference %>% summary() %>% round(3)
mean.age

cor(cfps$age.husband, cfps$age.wife) %>% round(3)
corr.age

cfps$spead_age %>% summary() %>% round(3)
spread.age

cfps$sharesame_age %>% summary() %>% round(3)
sharesame.age

ks_age


######################################################################################################################
######################################################################################################################
######################### Simualtions WITH the outside option to stay single #########################################
######################################################################################################################
######################################################################################################################


############ Vectors for descriptive statistics at the end ###############
# Spezification 1, 2, 3 and students
corr_age_rej = c(NA, NA, NA, NA)
sharesame_age_rej = c(NA, NA, NA, NA)
spread_age_rej = c(NA, NA, NA, NA)
corr_edu_rej = c(NA, NA, NA, NA)
sharesame_edu_rej = c(NA, NA, NA, NA)
spread_edu_rej = c(NA, NA, NA, NA)
mean_edu_rej= c(NA, NA, NA, NA)
mean_age_rej = c(NA, NA, NA, NA)
## matrix for unmarried rates according to educational levels
unmarried_women = matrix(NA, nrow = 6, ncol=6)
unmarried_men = matrix(NA, nrow = 6, ncol=6)
unmarried_women_total = c(NA, NA, NA, NA)
unmarried_men_total = c(NA, NA, NA, NA)


rank.women = matrix(NA, nrow = 2*nobs, ncol=2*nobs)
rank.men = matrix(NA, nrow = 2*nobs, ncol=2*nobs)

######################################################################################################################
#################################### Estimation Spezification 1 ######################################################
######################################################################################################################
### define
spe = 1

### calculate the predicted meeting likelihood for each man/women in the general population
utility.women = exp(logit1.female$coefficients[1] + logit1.female$coefficients[2]*age.menminuswomen.pos +
                      logit1.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logit1.female$coefficients[4]*age.menminuswomen.neg +
                      logit1.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                      logit1.female$coefficients[6]*edu.menminuswomen.pos +
                      logit1.female$coefficients[7]*edu.menminuswomen.neg)/
  (1+  exp(logit1.female$coefficients[1] + logit1.female$coefficients[2]*age.menminuswomen.pos +
             logit1.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
             logit1.female$coefficients[4]*age.menminuswomen.neg +
             logit1.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
             logit1.female$coefficients[6]*edu.menminuswomen.pos +
             logit1.female$coefficients[7]*edu.menminuswomen.neg))

utility.men = exp(logit1.male$coefficients[1] + logit1.male$coefficients[2]*age.womenminusmen.pos +
                    logit1.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logit1.male$coefficients[4]*age.womenminusmen.neg +
                    logit1.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logit1.male$coefficients[6]*edu.womenminusmen.pos +
                    logit1.male$coefficients[7]*edu.womenminusmen.neg)/
  (1+  exp(logit1.male$coefficients[1] + logit1.male$coefficients[2]*age.womenminusmen.pos +
             logit1.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit1.male$coefficients[4]*age.womenminusmen.neg +
             logit1.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit1.male$coefficients[6]*edu.womenminusmen.pos +
             logit1.male$coefficients[7]*edu.womenminusmen.neg))


## likelihood assigned to actual partner and own educational level
realutility.woman = data.frame(diag(utility.women), women$education)
names(realutility.woman) <-c("realutility", "education")
realutility.man = data.frame(diag(utility.men), men$education)
names(realutility.man) <-c("realutility", "education")

################# education-specific rejection likelihoods
rejections_man = c( quantile(subset(realutility.man, education==1)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==2)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==3)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==4)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==5)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==6)[,1], probs=(0.01)))
rejections_woman = c( quantile(subset(realutility.woman, education==1)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==2)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==3)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==4)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==5)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==6)[,1], probs=(0.01)))
rejections_man1 <- rejections_man
rejections_woman1 <- rejections_woman

### add to utilities
vector_women <-case_when(
  (realutility.woman$education ==1) ~ rejections_woman[1],
  (realutility.woman$education==2) ~ rejections_woman[2],
  (realutility.woman$education ==3) ~ rejections_woman[3],
  (realutility.woman$education ==4) ~ rejections_woman[4],
  (realutility.woman$education ==5) ~ rejections_woman[5],
  (realutility.woman$education ==6) ~ rejections_woman[6]
) 
matrix_outside_women <- matrix(vector_women, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.women <- cbind( utility.women, matrix_outside_women )


vector_men <- case_when(
  (realutility.man$education ==1) ~ rejections_man[1],
  (realutility.man$education ==2) ~ rejections_man[2],
  (realutility.man$education ==3) ~ rejections_man[3],
  (realutility.man$education ==4) ~ rejections_man[4],
  (realutility.man$education ==5) ~ rejections_man[5],
  (realutility.man$education ==6) ~ rejections_man[6]
) 
matrix_outside_men <- matrix(vector_men, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.men <- cbind( utility.men, matrix_outside_men )

################ Gale-Shapley with rejections ###############

### establish the output vectors
outcome.age.women <-  rep(NA,  nobs)
outcome.edu.women <-  rep(NA,  nobs)
outcome.age.men <-  rep(NA,  nobs)
outcome.edu.men <- rep(NA, nobs)

### establish ranking, the higher the utility the higher the rank, then run gale-shapley
### Rank: the most preferred person gets the highest rank
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}
#### add the outside opportunity dummies
for (i in nobs+1:nobs) {
  rank.women[i,1:nobs] = sample(c(nobs+1:nobs))   
  rank.women[i,nobs+1:nobs] = sample(c(1:nobs))
  rank.men[i,1:nobs] = sample(c(nobs+1:nobs))  
  rank.men[i,nobs+1:nobs] =  sample(c(1:nobs))
}
## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)
### gale shapley matching 
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)
## outcome matrix
outcome.age.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],2], NA) 
outcome.edu.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],1], NA) 
outcome.age.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],2], NA) 
outcome.edu.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],1], NA) 

################################################################
########### Summary stats and Graphs ###########################
################################################################


## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### Descriptives: Correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr_age_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr_edu_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

########  AGE #####
df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age",
                "Husband actual age - wife simulated age")
##Descriptives: spread & share of same level
spread_age_rej[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame_age_rej[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                        is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean_age_rej[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)


df2_age_spe1_rej <- melt.data.frame(df1)

#### Graph EDUCATION #######
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife,
                  cfps$education.husband - cfps$edu.wife.pred)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education",
                "Husband actual education - wife simulated education")
### Descriptives
spread_edu_rej[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame_edu_rej[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                        is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean_edu_rej[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)

df2_edu_spe1_rej <- melt.data.frame(df1)

################################################################
##################### marriage rates ###########################
################################################################

women_all <- data.frame(women,outcome.edu.women)
### count NA per row (there are no NA in any of the other rows)
women_all$unmarried_count <- rowSums(is.na(women_all[,1:5]))

men_all <- data.frame(men,outcome.edu.men)
### count NA per row (there are no NA in any of the other rows)
men_all$unmarried_count <- rowSums(is.na(men_all[,1:5]))
############## Save the results

unmarried_women[spe, ] <- c(mean(women_all$unmarried_count[women_all$education ==1]),
                            mean(women_all$unmarried_count[women_all$education ==2]),
                            mean(women_all$unmarried_count[women_all$education ==3]),
                            mean(women_all$unmarried_count[women_all$education ==4]),
                            mean(women_all$unmarried_count[women_all$education ==5]),
                            mean(women_all$unmarried_count[women_all$education ==6]))
unmarried_women_total[spe]  <- mean(women_all$unmarried_count)  
unmarried_men[spe, ] <- c(mean(men_all$unmarried_count[men_all$education ==1]),
                          mean(men_all$unmarried_count[men_all$education ==2]),
                          mean(men_all$unmarried_count[men_all$education ==3]),
                          mean(men_all$unmarried_count[men_all$education ==4]),
                          mean(men_all$unmarried_count[men_all$education ==5]),
                          mean(men_all$unmarried_count[men_all$education ==6]))
unmarried_men_total[spe]  <- mean(men_all$unmarried_count)

############## Graph the results
women_all$education <- as.factor(women_all$education)
levels(women_all$education) <- edu_labels
men_all$education <- as.factor(men_all$education)
levels(men_all$education) <- edu_labels

women_un <- ggplot(women_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe1_rejections_women.png", plot= women_un, width =6, height = 4)

men_un <- ggplot(men_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe1_rejections_men.png", plot= men_un, width =6, height = 4)

######################################################################################################################
#################################### Estimation Spezification 2 ######################################################
######################################################################################################################
### define
spe = 2

### calculate the predicted meeting likelihood for each man/women in the general population
utility.women = exp(logit2.female$coefficients[1]  +
                      logit2.female$coefficients[2]*age.menminuswomen.pos +
                      logit2.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logit2.female$coefficients[4]*age.menminuswomen.neg +
                      logit2.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                      logit2.female$coefficients[6]*edu.menminuswomen.pos +
                      logit2.female$coefficients[7]*women$high.edu + 
                      logit2.female$coefficients[8]*edu.menminuswomen.neg +
                      logit2.female$coefficients[9]*edu.menminuswomen.pos*women$high.edu +
                      logit2.female$coefficients[10]*edu.menminuswomen.neg*women$high.edu)   /
  (1+ exp(logit2.female$coefficients[1]  +
            logit2.female$coefficients[2]*age.menminuswomen.pos +
            logit2.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
            logit2.female$coefficients[4]*age.menminuswomen.neg +
            logit2.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
            logit2.female$coefficients[6]*edu.menminuswomen.pos +
            logit2.female$coefficients[7]*women$high.edu + 
            logit2.female$coefficients[8]*edu.menminuswomen.neg +
            logit2.female$coefficients[9]*edu.menminuswomen.pos*women$high.edu +
            logit2.female$coefficients[10]*edu.menminuswomen.neg*women$high.edu))

utility.men = exp(logit2.male$coefficients[1] +
                    logit2.male$coefficients[2]*age.womenminusmen.pos +
                    logit2.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logit2.male$coefficients[4]*age.womenminusmen.neg +
                    logit2.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logit2.male$coefficients[6]*edu.womenminusmen.pos +
                    logit2.male$coefficients[7]*men$high.edu+
                    logit2.male$coefficients[8]*edu.womenminusmen.neg +
                    logit2.male$coefficients[9]*edu.womenminusmen.pos*men$high.edu +
                    logit2.male$coefficients[10]*edu.womenminusmen.neg*men$high.edu)   /
  (1+  exp(logit2.male$coefficients[1] +
             logit2.male$coefficients[2]*age.womenminusmen.pos +
             logit2.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit2.male$coefficients[4]*age.womenminusmen.neg +
             logit2.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit2.male$coefficients[6]*edu.womenminusmen.pos +
             logit2.male$coefficients[7]*men$high.edu+
             logit2.male$coefficients[8]*edu.womenminusmen.neg +
             logit2.male$coefficients[9]*edu.womenminusmen.pos*men$high.edu +
             logit2.male$coefficients[10]*edu.womenminusmen.neg*men$high.edu) )


## likelihood assigned to actual partner and own educational level
realutility.woman = data.frame(diag(utility.women), women$education)
names(realutility.woman) <-c("realutility", "education")
realutility.man = data.frame(diag(utility.men), men$education)
names(realutility.man) <-c("realutility", "education")


################# education-specific rejection likelihoods
rejections_man = c( quantile(subset(realutility.man, education==1)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==2)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==3)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==4)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==5)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==6)[,1], probs=(0.01)))
rejections_woman = c( quantile(subset(realutility.woman, education==1)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==2)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==3)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==4)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==5)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==6)[,1], probs=(0.01)))
rejections_man2 <- rejections_man
rejections_woman2 <- rejections_woman

### add to utilities
vector_women <-case_when(
  (realutility.woman$education ==1) ~ rejections_woman[1],
  (realutility.woman$education==2) ~ rejections_woman[2],
  (realutility.woman$education ==3) ~ rejections_woman[3],
  (realutility.woman$education ==4) ~ rejections_woman[4],
  (realutility.woman$education ==5) ~ rejections_woman[5],
  (realutility.woman$education ==6) ~ rejections_woman[6]
) 
matrix_outside_women <- matrix(vector_women, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.women <- cbind( utility.women, matrix_outside_women )


vector_men <- case_when(
  (realutility.man$education ==1) ~ rejections_man[1],
  (realutility.man$education ==2) ~ rejections_man[2],
  (realutility.man$education ==3) ~ rejections_man[3],
  (realutility.man$education ==4) ~ rejections_man[4],
  (realutility.man$education ==5) ~ rejections_man[5],
  (realutility.man$education ==6) ~ rejections_man[6]
) 
matrix_outside_men <- matrix(vector_men, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.men <- cbind( utility.men, matrix_outside_men )

###### GALE SHAPLEY  Spe. 2 LOOP

### Rank: the most preferred person gets the highest rank
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}
#### add the outside opportunity dummies
for (i in nobs+1:nobs) {
  rank.women[i,1:nobs] = sample(c(nobs+1:nobs))   
  rank.women[i,nobs+1:nobs] = sample(c(1:nobs))
  rank.men[i,1:nobs] = sample(c(nobs+1:nobs))  
  rank.men[i,nobs+1:nobs] =  sample(c(1:nobs))
}
## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)
### gale shapley matching 
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)
## outcome matrix
outcome.age.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],2], NA) 
outcome.edu.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],1], NA) 
outcome.age.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],2], NA) 
outcome.edu.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],1], NA)


## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### Spe. 2 correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr_age_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr_edu_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

##################### Spe. 2 Graph: AGE ##################
df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age",
                "Husband actual age - wife simulated age")
## Descriptives: spread & share of same level
spread_age_rej[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame_age_rej[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                        is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean_age_rej[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)

df2_age_spe2_rej <- melt.data.frame(df1)

#################### Spe. 2 Graph EDUCATION ##################################
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife,
                  cfps$education.husband - cfps$edu.wife.pred)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education",
                "Husband actual education - wife simulated education")
## Descriptives
spread_edu_rej[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame_edu_rej[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                        is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean_edu_rej[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)

df2_edu_spe2_rej <- melt.data.frame(df1)

################################################################
##################### marriage rates ###########################
################################################################

women_all <- data.frame(women,outcome.edu.women)
### count NA per row (there are no NA in any of the other rows)
women_all$unmarried_count <- rowSums(is.na(women_all[,1:5]))
summary(women_all$unmarried_count)
men_all <- data.frame(men,outcome.edu.men)
### count NA per row (there are no NA in any of the other rows)
men_all$unmarried_count <- rowSums(is.na(men_all[,1:5]))

############## Save the results

unmarried_women[spe, ] <- c(mean(women_all$unmarried_count[women_all$education ==1]),
                            mean(women_all$unmarried_count[women_all$education ==2]),
                            mean(women_all$unmarried_count[women_all$education ==3]),
                            mean(women_all$unmarried_count[women_all$education ==4]),
                            mean(women_all$unmarried_count[women_all$education ==5]),
                            mean(women_all$unmarried_count[women_all$education ==6]))
unmarried_women_total[spe]  <- mean(women_all$unmarried_count)
unmarried_men[spe, ] <- c(mean(men_all$unmarried_count[men_all$education ==1]),
                          mean(men_all$unmarried_count[men_all$education ==2]),
                          mean(men_all$unmarried_count[men_all$education ==3]),
                          mean(men_all$unmarried_count[men_all$education ==4]),
                          mean(men_all$unmarried_count[men_all$education ==5]),
                          mean(men_all$unmarried_count[men_all$education ==6]))
unmarried_men_total[spe]  <- mean(men_all$unmarried_count)
############## Graph the results
women_all$education <- as.factor(women_all$education)
levels(women_all$education) <- edu_labels
men_all$education <- as.factor(men_all$education)
levels(men_all$education) <- edu_labels

women_un <- ggplot(women_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe2_rejections_women.png", plot= women_un, width =6, height = 4)

men_un <- ggplot(men_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe2_rejections_men.png", plot= men_un, width =6, height = 4)

######################################################################################################################
#################################### Estimation Spezification 3 ######################################################
######################################################################################################################
### define
spe = 3

### calculate the predicted meeting likelihood for each man/women in the general population
utility.women = exp(logit3.female$coefficients[1]  +
                      logit3.female$coefficients[2]*age.menminuswomen.pos +
                      logit3.female$coefficients[3]* women$over30 +
                      logit3.female$coefficients[4]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logit3.female$coefficients[5]*age.menminuswomen.neg +
                      logit3.female$coefficients[6]*age.menminuswomen.neg *age.menminuswomen.neg +  
                      logit3.female$coefficients[7]*edu.menminuswomen.pos +
                      logit3.female$coefficients[8]*edu.menminuswomen.neg +
                      logit3.female$coefficients[9]*age.menminuswomen.pos*women$over30  +
                      logit3.female$coefficients[10]*age.menminuswomen.pos*age.menminuswomen.pos*women$over30 +
                      logit3.female$coefficients[11]*age.menminuswomen.neg*women$over30  +
                      logit3.female$coefficients[12]*age.menminuswomen.neg*age.menminuswomen.neg*women$over30) /
  (1+ exp(logit3.female$coefficients[1]  +
            logit3.female$coefficients[2]*age.menminuswomen.pos +
            logit3.female$coefficients[3]* women$over30 +
            logit3.female$coefficients[4]*age.menminuswomen.pos*age.menminuswomen.pos +
            logit3.female$coefficients[5]*age.menminuswomen.neg +
            logit3.female$coefficients[6]*age.menminuswomen.neg*age.menminuswomen.neg +  
            logit3.female$coefficients[7]*edu.menminuswomen.pos +
            logit3.female$coefficients[8]*edu.menminuswomen.neg +
            logit3.female$coefficients[9]*age.menminuswomen.pos*women$over30  +
            logit3.female$coefficients[10]*age.menminuswomen.pos*age.menminuswomen.pos*women$over30 +
            logit3.female$coefficients[11]*age.menminuswomen.neg*women$over30  +
            logit3.female$coefficients[12]*age.menminuswomen.neg*age.menminuswomen.neg*women$over30))

utility.men = exp(logit3.male$coefficients[1] +
                    logit3.male$coefficients[2]*age.womenminusmen.pos +
                    logit3.male$coefficients[3]* men$over30 +
                    logit3.male$coefficients[4]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logit3.male$coefficients[5]*age.womenminusmen.neg +
                    logit3.male$coefficients[6]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logit3.male$coefficients[7]*edu.womenminusmen.pos +
                    logit3.male$coefficients[8]*edu.womenminusmen.neg +
                    logit3.male$coefficients[9]*age.womenminusmen.pos*men$over30 +
                    logit3.male$coefficients[10]*age.womenminusmen.pos*age.womenminusmen.pos*men$over30 +
                    logit3.male$coefficients[11]*age.womenminusmen.neg*men$over30 +
                    logit3.male$coefficients[12]*age.womenminusmen.neg*age.womenminusmen.neg*men$over30) /
  (1+  exp(logit3.male$coefficients[1] +
             logit3.male$coefficients[2]*age.womenminusmen.pos +
             logit3.female$coefficients[3]* men$over30 +
             logit3.male$coefficients[4]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit3.male$coefficients[5]*age.womenminusmen.neg +
             logit3.male$coefficients[6]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit3.male$coefficients[7]*edu.womenminusmen.pos +
             logit3.male$coefficients[8]*edu.womenminusmen.neg +
             logit3.male$coefficients[9]*age.womenminusmen.pos*men$over30 +
             logit3.male$coefficients[10]*age.womenminusmen.pos*age.womenminusmen.pos*men$over30 +
             logit3.male$coefficients[11]*age.womenminusmen.neg*men$over30 +
             logit3.male$coefficients[12]*age.womenminusmen.neg*age.womenminusmen.neg*men$over30) )


## likelihood assigned to actual partner and own educational level
realutility.woman = data.frame(diag(utility.women), women$education)
names(realutility.woman) <-c("realutility", "education")
realutility.man = data.frame(diag(utility.men), men$education)
names(realutility.man) <-c("realutility", "education")

################# education-specific rejection likelihoods
rejections_man = c( quantile(subset(realutility.man, education==1)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==2)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==3)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==4)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==5)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==6)[,1], probs=(0.01)))
rejections_woman = c( quantile(subset(realutility.woman, education==1)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==2)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==3)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==4)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==5)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==6)[,1], probs=(0.01)))
rejections_man3 <- rejections_man
rejections_woman3 <- rejections_woman

### add to utilities
vector_women <-case_when(
  (realutility.woman$education ==1) ~ rejections_woman[1],
  (realutility.woman$education==2) ~ rejections_woman[2],
  (realutility.woman$education ==3) ~ rejections_woman[3],
  (realutility.woman$education ==4) ~ rejections_woman[4],
  (realutility.woman$education ==5) ~ rejections_woman[5],
  (realutility.woman$education ==6) ~ rejections_woman[6]
) 
matrix_outside_women <- matrix(vector_women, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.women <- cbind( utility.women, matrix_outside_women )


vector_men <- case_when(
  (realutility.man$education ==1) ~ rejections_man[1],
  (realutility.man$education ==2) ~ rejections_man[2],
  (realutility.man$education ==3) ~ rejections_man[3],
  (realutility.man$education ==4) ~ rejections_man[4],
  (realutility.man$education ==5) ~ rejections_man[5],
  (realutility.man$education ==6) ~ rejections_man[6]
) 
matrix_outside_men <- matrix(vector_men, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.men <- cbind( utility.men, matrix_outside_men )
rm(vector_men, vector_women, matrix_outside_women, matrix_outside_men)

### Rank: the most preferred person gets the highest rank
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}
#### add the outside opportunity dummies
for (i in nobs+1:nobs) {
  rank.women[i,1:nobs] = sample(c(nobs+1:nobs))   
  rank.women[i,nobs+1:nobs] = sample(c(1:nobs))
  rank.men[i,1:nobs] = sample(c(nobs+1:nobs))  
  rank.men[i,nobs+1:nobs] =  sample(c(1:nobs))
}
## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)
### gale shapley matching 
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)
## outcome matrix
outcome.age.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],2], NA) 
outcome.edu.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],1], NA) 
outcome.age.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],2], NA) 
outcome.edu.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],1], NA)

## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### Spe 3. correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr_age_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr_edu_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]


################################################################
###### Spe. 3 GRAPHS ###########################
################################################################

#################### Spe. 3 Graph: AGE ##############

df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age",
                "Husband actual age - wife simulated age")

df2_age_spe3_rej <- subset(melt.data.frame(df1), value >=-20 & value <=20)

########## Spe. 3 EDUCATION #############
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife,
                  cfps$education.husband - cfps$edu.wife.pred)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education",
                "Husband actual education - wife simulated education")

## Descriptives
spread_edu_rej[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame_edu_rej[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                        is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean_edu_rej[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)

df2_edu_spe3_rej <- melt.data.frame(df1)
################################################################
##################### marriage rates ###########################
################################################################

women_all <- data.frame(women,outcome.edu.women)
### count NA per row (there are no NA in any of the other rows)
women_all$unmarried_count <- rowSums(is.na(women_all[,1:5]))

men_all <- data.frame(men,outcome.edu.men)
### count NA per row (there are no NA in any of the other rows)
men_all$unmarried_count <- rowSums(is.na(men_all[,1:5]))

############## Save the results

unmarried_women[spe, ] <- c(mean(women_all$unmarried_count[women_all$education ==1]),
                            mean(women_all$unmarried_count[women_all$education ==2]),
                            mean(women_all$unmarried_count[women_all$education ==3]),
                            mean(women_all$unmarried_count[women_all$education ==4]),
                            mean(women_all$unmarried_count[women_all$education ==5]),
                            mean(women_all$unmarried_count[women_all$education ==6]))
unmarried_women_total[spe]  <- mean(women_all$unmarried_count)
unmarried_men[spe, ] <- c(mean(men_all$unmarried_count[men_all$education ==1]),
                          mean(men_all$unmarried_count[men_all$education ==2]),
                          mean(men_all$unmarried_count[men_all$education ==3]),
                          mean(men_all$unmarried_count[men_all$education ==4]),
                          mean(men_all$unmarried_count[men_all$education ==5]),
                          mean(men_all$unmarried_count[men_all$education ==6]))
unmarried_men_total[spe]  <- mean(men_all$unmarried_count)
############## Graph the results
women_all$education <- as.factor(women_all$education)
levels(women_all$education) <- edu_labels
men_all$education <- as.factor(men_all$education)
levels(men_all$education) <- edu_labels

women_un <- ggplot(women_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  +
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe3_rejections_women.png", plot= women_un, width =6, height = 4)

men_un <- ggplot(men_all, aes(men_all$education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe3_rejections_men.png", plot= men_un, width =6, height = 4)

###############################################################################################################
####################################### Students ##############################################################
###############################################################################################################

### define
spe = 4

### calculate the predicted meeting likelihood for each man/women in the general population

utility.women = exp(logitst.female$coefficients[1] + logitst.female$coefficients[2]*age.menminuswomen.pos +
                      logitst.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                      logitst.female$coefficients[4]*age.menminuswomen.neg +
                      logitst.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                      logitst.female$coefficients[6]*edu.menminuswomen.pos +
                      logitst.female$coefficients[7]*edu.menminuswomen.neg)/
  (1+  exp(logitst.female$coefficients[1] + logitst.female$coefficients[2]*age.menminuswomen.pos +
             logitst.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
             logitst.female$coefficients[4]*age.menminuswomen.neg +
             logitst.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
             logitst.female$coefficients[6]*edu.menminuswomen.pos +
             logitst.female$coefficients[7]*edu.menminuswomen.neg))

utility.men = exp(logitst.male$coefficients[1] + logitst.male$coefficients[2]*age.womenminusmen.pos +
                    logitst.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logitst.male$coefficients[4]*age.womenminusmen.neg +
                    logitst.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logitst.male$coefficients[6]*edu.womenminusmen.pos +
                    logitst.male$coefficients[7]*edu.womenminusmen.neg)/
  (1+  exp(logitst.male$coefficients[1] + logitst.male$coefficients[2]*age.womenminusmen.pos +
             logitst.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logitst.male$coefficients[4]*age.womenminusmen.neg +
             logitst.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logitst.male$coefficients[6]*edu.womenminusmen.pos +
             logitst.male$coefficients[7]*edu.womenminusmen.neg))

## likelihood assigned to actual partner and own educational level
realutility.woman = data.frame(diag(utility.women), women$education)
names(realutility.woman) <-c("realutility", "education")
realutility.man = data.frame(diag(utility.men), men$education)
names(realutility.man) <-c("realutility", "education")

################# education-specific rejection likelihoods
rejections_man = c( quantile(subset(realutility.man, education==1)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==2)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==3)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==4)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==5)[,1], probs=(0.01)),
                    quantile(subset(realutility.man, education==6)[,1], probs=(0.01)))
rejections_woman = c( quantile(subset(realutility.woman, education==1)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==2)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==3)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==4)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==5)[,1], probs=(0.01)),
                      quantile(subset(realutility.woman, education==6)[,1], probs=(0.01)))
rejections_man4 <- rejections_man
rejections_woman4 <- rejections_woman

### add to utilities
vector_women <-case_when(
  (realutility.woman$education ==1) ~ rejections_woman[1],
  (realutility.woman$education==2) ~ rejections_woman[2],
  (realutility.woman$education ==3) ~ rejections_woman[3],
  (realutility.woman$education ==4) ~ rejections_woman[4],
  (realutility.woman$education ==5) ~ rejections_woman[5],
  (realutility.woman$education ==6) ~ rejections_woman[6]
) 
matrix_outside_women <- matrix(vector_women, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.women <- cbind( utility.women, matrix_outside_women )


vector_men <- case_when(
  (realutility.man$education ==1) ~ rejections_man[1],
  (realutility.man$education ==2) ~ rejections_man[2],
  (realutility.man$education ==3) ~ rejections_man[3],
  (realutility.man$education ==4) ~ rejections_man[4],
  (realutility.man$education ==5) ~ rejections_man[5],
  (realutility.man$education ==6) ~ rejections_man[6]
) 
matrix_outside_men <- matrix(vector_men, nrow= nobs, ncol = nobs, byrow =FALSE)
utility.men <- cbind( utility.men, matrix_outside_men )
rm(vector_men, vector_women, matrix_outside_women, matrix_outside_men)

###### GALE SHAPLEY Students Loop
### Rank: the most preferred person gets the highest rank
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}
#### add the outside opportunity dummies
for (i in nobs+1:nobs) {
  rank.women[i,1:nobs] = sample(c(nobs+1:nobs))   
  rank.women[i,nobs+1:nobs] = sample(c(1:nobs))
  rank.men[i,1:nobs] = sample(c(nobs+1:nobs))  
  rank.men[i,nobs+1:nobs] =  sample(c(1:nobs))
}
## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)
### gale shapley matching 
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)
## outcome matrix
outcome.age.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],2], NA) 
outcome.edu.women = ifelse(gs$engagements[1:nobs] <= nobs, men[gs$engagements[1:nobs],1], NA) 
outcome.age.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],2], NA) 
outcome.edu.men = ifelse(gs$proposals[1:nobs] <= nobs, women[gs$proposals[1:nobs],1], NA)




## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

### correlations
forcorr = cbind(cfps$age.husband, cfps$age.wife.pred)
corr_age_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]
forcorr = cbind(cfps$edu.husband, cfps$edu.wife.pred)
corr_edu_rej[spe] = cor(forcorr, use="pairwise.complete.obs", method= "pearson")[1,2]

################################################################
###### Students GRAPHS ###########################
################################################################

############## Students Graph: AGE ########################
df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age",
                "Husband actual age - wife simulated age")
##Descriptives: spread & share of same level
spread_age_rej[spe] = sd(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)/
  mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
sharesame_age_rej[spe] = length(which(abs(df1$`Husband actual age - wife simulated age`)<2.5 &
                                        is.na(df1$`Husband actual age - wife simulated age`)==FALSE))
mean_age_rej[spe] = mean(df1$`Husband actual age - wife simulated age`, na.rm = TRUE)
##

df2_age_spe4_rej <- subset(melt.data.frame(df1), value >=-20 & value <=20)

########### Students Graph EDUCATION ######################
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife,
                  cfps$education.husband - cfps$edu.wife.pred)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education",
                "Husband actual education - wife simulated education")
## Descriptives
spread_edu_rej[spe] = sd(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)/ 
  mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
sharesame_edu_rej[spe] = length(which(abs(df1$`Husband actual education - wife simulated education`)<0.5 &
                                        is.na(df1$`Husband actual education - wife simulated education`)==FALSE))
mean_edu_rej[spe] = mean(df1$`Husband actual education - wife simulated education`, na.rm = TRUE)
##

df2_edu_spe4_rej <- subset(melt.data.frame(df1), value >=-20 & value <=20)

################################################################
##################### marriage rates ###########################
################################################################

women_all <- data.frame(women,outcome.edu.women)
### count NA per row (there are no NA in any of the other rows)
women_all$unmarried_count <- rowSums(is.na(women_all[,1:5]))
summary(women_all$unmarried_count)
men_all <- data.frame(men,outcome.edu.men)
### count NA per row (there are no NA in any of the other rows)
men_all$unmarried_count <- rowSums(is.na(men_all[,1:5]))

############## Save the results
unmarried_women[spe, ] <- c(mean(women_all$unmarried_count[women_all$education ==1]),
                            mean(women_all$unmarried_count[women_all$education ==2]),
                            mean(women_all$unmarried_count[women_all$education ==3]),
                            mean(women_all$unmarried_count[women_all$education ==4]),
                            mean(women_all$unmarried_count[women_all$education ==5]),
                            mean(women_all$unmarried_count[women_all$education ==6]))
unmarried_women_total[spe]  <- mean(women_all$unmarried_count)
unmarried_men[spe, ] <- c(mean(men_all$unmarried_count[men_all$education ==1]),
                          mean(men_all$unmarried_count[men_all$education ==2]),
                          mean(men_all$unmarried_count[men_all$education ==3]),
                          mean(men_all$unmarried_count[men_all$education ==4]),
                          mean(men_all$unmarried_count[men_all$education ==5]),
                          mean(men_all$unmarried_count[men_all$education ==6]))
unmarried_men_total[spe]  <- mean(men_all$unmarried_count)
############## Graph the results
women_all$education <- as.factor(women_all$education)
levels(women_all$education) <- edu_labels
men_all$education <- as.factor(men_all$education)
levels(men_all$education) <- edu_labels

women_un <- ggplot(women_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe4_rejections_women.png", plot= women_un, width =6, height = 4)

men_un <- ggplot(men_all, aes(education, unmarried_count)) + 
  geom_bar(stat = "summary", fun = "mean") + expand_limits(y=c(0, 0.5)) +
  geom_hline(yintercept=0.01) + labs(x = "Educational Level", y = "Average Unmarried Rate")  + 
  theme_classic()+ theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("spe4_rejections_men.png", plot= men_un, width =6, height = 4)

#################################################################################################
##################################" Graphs ######################################################
##################################################################################################
#### Age
plot_age_spe1_rej <- ggplot(subset(df2_age_spe1_rej, variable !="Husband simulated age - wife actual age"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.5)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_age_spe1_rej <- ggplot(subset(df2_age_spe1_rej, variable !="Husband simulated age - wife actual age"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_age_spe2_rej <- ggplot(subset(df2_age_spe2_rej, variable !="Husband simulated age - wife actual age"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density")  + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 
plot_age_spe3_rej <- ggplot(subset(df2_age_spe3_rej, variable !="Husband simulated age - wife actual age"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") +
  ylab("Density")  + lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 
plot_age_spe4_rej <- ggplot(subset(df2_age_spe4_rej, variable !="Husband simulated age - wife actual age"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") +
  ylab("Density")  +  lims(x=c(-20,20), y=c(0, 0.7)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 
#### Education
plot_edu_spe1_rej <- ggplot(subset(df2_edu_spe1_rej, variable !="Husband simulated education - wife actual education"), 
                            aes(value, fill = variable)) +   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_edu_spe2_rej <- ggplot(subset(df2_edu_spe2_rej, variable !="Husband simulated education - wife actual education"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9))  + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 
plot_edu_spe3_rej <- ggplot(subset(df2_edu_spe3_rej, variable !="Husband simulated education - wife actual education"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9))  + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 

plot_edu_spe4_rej <- ggplot(subset(df2_edu_spe4_rej, variable !="Husband simulated education - wife actual education"), 
                            aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 
#################################### Save the graphs ############################################

edu_all_rej <- ggarrange(plot_edu_spe1, plot_edu_spe1_rej, plot_edu_spe2, 
                         plot_edu_spe2_rej, plot_edu_spe3,  plot_edu_spe3_rej, 
                         ncol=2, nrow=3, common.legend = TRUE, legend="bottom", 
                         labels = c("Spec. 1: Without outside option", "Spec. 1: With outside option", "Spec. 2: Without outside option",
                                    "Spec. 2: With outside option", "Spec. 3: Without outside option", 
                                    "Spec. 3: With outside option"))
age_all_rej <- ggarrange(plot_age_spe1, plot_age_spe1_rej,  plot_age_spe2, 
                         plot_age_spe2_rej, plot_age_spe3,  plot_age_spe3_rej, 
                         ncol=2, nrow=3, common.legend = TRUE, legend="bottom", 
                         labels = c("Spec. 1: Without outside option",  "Spec. 1: With outside option", "Spec. 2: Without outside option", 
                                    "Spec. 2: With outside option", "Spec. 3: Without outside option", 
                                    "Spec. 3: With outside option"))

ggsave("age_all_rej.png", plot= age_all_rej, width =9, height = 11)
ggsave("edu_all_rej.png", plot= edu_all_rej, width =9, height = 11)


############### last column table 9

unmarried_men_total %>% round(3)

###############################################################################################################
################################### Urban Only ################################################################
###############################################################################################################


########################################### Spousal Supply from CFPS #################################################
################################### Age and educational differences ##################################################

# CFPS 2016, create data set with those who married in the past two years and the pertinent variables
cfps2016 = cfps2016_original %>%  filter(eeb401y_a_1 >= 2014 & eeb401y_a_1 <=2016) %>% 
  select(cfps_gender, eeb401y_a_1, edu, eeb4021_a_1, eeb402y_a_1, birthyear, pid, urban)
cfps2016$year = 2016


# CFPS 2014, create data set with those who married in the past two years and the pertinent variables
cfps2014 <-  cfps2014_original %>%  filter(eeb401y_a_1 >= 2012 & eeb401y_a_1 <=2014) %>% 
  select(cfps_gender, eeb401y_a_1, edu, eeb4021_a_1, eeb402y_a_1, birthyear, pid, urban)
cfps2014$year = 2014

# append 2014 and 2016 data sets
cfps = rbind(cfps2016, cfps2014)

### only those with complete information
cfps <- subset(cfps,is.na(edu) == FALSE & is.na(eeb401y_a_1)==FALSE &
                 is.na(cfps_gender) == FALSE & is.na(eeb4021_a_1)==FALSE &
                 is.na(birthyear) == FALSE & is.na(eeb402y_a_1)==FALSE &
                 edu > 0 & eeb4021_a_1 > 0 &
                 birthyear > 1900 & eeb402y_a_1 > 1900)

names(cfps) <- c("gender", "year_marriage", "edu", "edu_spouse", "birthyear_spouse", "birthyear", "pid", "urban", "year")


# Data cleaning. I transform the educational varibale, generate a female dummy, and create variables for husband and wife separately. 
# I only use those between the age of 20 (marriage age) and 50. 

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

cfps$age_diff <- cfps$age.husband - cfps$age.wife
cfps$edu_diff <- cfps$education.husband - cfps$education.wife
cfps <- cfps %>% filter(is.na(age.wife) == FALSE, is.na(age.husband) == FALSE, 
                        is.na(education.husband) == FALSE, is.na(education.wife) == FALSE , urban >=0)

cfps %>% ggplot(aes(edu_diff, fill = as.factor(urban))) +    geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity', binwidth=1) 
cfps %>% ggplot(aes(age_diff, fill = as.factor(urban))) +    geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity', binwidth=1) 


cfps <- cfps %>% filter(urban ==1)


# I create two data sets: one for women and one for men. I then calculate the age and educational difference matrixes. I create separate matrices for the positive difference and the negative difference.

## two different datasets
## women
women<- data.frame(cfps$education.wife, cfps$age.wife)
names(women) <-c("education", "age")

## men
men<- data.frame(cfps$education.husband, cfps$age.husband)
names(men) <-c("education", "age")

### number of observations
nobs <- length(men$education)

############# difference matrices
age.w.matrix <- matrix(c(women[,2]),nrow=nobs,ncol=nobs,byrow=TRUE) 
age.m.matrix <- matrix(c(men[,2]),nrow=nobs,ncol=nobs,byrow=TRUE) 
## men-women age difference matrices: for women utility
# i: rows
# j: columns
# age.menminuswomen.pos
# woman 1: (men(1) - women(1)    men(2) - women(1)  ....  men(n)- women (1))
# woman 2: (men(1) - women(2)    men(2) - women(2)  ....  men(n)- women (2))
# ...
# woman N: (men(n) - women(N)    men(2) - women(N)  ....  men(n)- women (N))

age.menminuswomen.pos <- age.m.matrix - t(age.w.matrix)
age.menminuswomen.pos[age.menminuswomen.pos<0] <- 0
age.menminuswomen.neg <- age.m.matrix - t(age.w.matrix)
age.menminuswomen.neg[age.menminuswomen.neg>0] <- 0
age.menminuswomen.neg <- abs(age.menminuswomen.neg)

### women-men age difference matrices: for men utility
age.womenminusmen.pos <- age.w.matrix - t(age.m.matrix)
age.womenminusmen.pos[age.womenminusmen.pos<0] <- 0
age.womenminusmen.neg <- age.w.matrix - t(age.m.matrix)
age.womenminusmen.neg[age.womenminusmen.neg>0] <- 0
age.womenminusmen.neg <- abs(age.womenminusmen.neg)

############### education
edu.w.matrix <- matrix(c(women[,1]),nrow=nobs,ncol=nobs,byrow=TRUE) 
edu.m.matrix <- matrix(c(men[,1]),nrow=nobs,ncol=nobs,byrow=TRUE) 

## men-women edu difference matrices
edu.menminuswomen.pos <- edu.m.matrix - t(edu.w.matrix)
edu.menminuswomen.pos[edu.menminuswomen.pos<0] <- 0
edu.menminuswomen.neg <- edu.m.matrix - t(edu.w.matrix)
edu.menminuswomen.neg[edu.menminuswomen.neg>0] <- 0
edu.menminuswomen.neg <- abs(edu.menminuswomen.neg)

### women-men edu difference matrices
edu.womenminusmen.pos <- edu.w.matrix - t(edu.m.matrix)
edu.womenminusmen.pos[edu.womenminusmen.pos<0] <- 0
edu.womenminusmen.neg <- edu.w.matrix - t(edu.m.matrix)
edu.womenminusmen.neg[edu.womenminusmen.neg>0] <- 0
edu.womenminusmen.neg <- abs(edu.womenminusmen.neg)

rm(edu.w.matrix, edu.m.matrix, age.w.matrix, age.m.matrix)

women$high.edu <- ifelse(women$education >=5,1,0)
women$over30 <- ifelse(women$age >=30, 1, 0)
men$high.edu <- ifelse(men$education >=5,1,0)
men$over30 <- ifelse(men$age >=30, 1, 0)


######################################################################################################################
#################################### Estimation Spezification 1 ######################################################
######################################################################################################################

###################### Simualtions WITHOUT the outside option to stay single #########################################



### calculate the predicted meeting likelihood for each man/women in the general population
### woman 1: utility for man 1    woman 1: utility for man 2 ................ woman 1: utility for man n
### woman 2: utility for man 2    woman 2: utility for man 2 ................ woman 2: utility for man n
### .......
### woman n: utility for man 1    woman n: utility for man 2 ................ woman n: utility for man n

utility.women <- exp(logit1.female$coefficients[1] + logit1.female$coefficients[2]*age.menminuswomen.pos +
                       logit1.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
                       logit1.female$coefficients[4]*age.menminuswomen.neg +
                       logit1.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
                       logit1.female$coefficients[6]*edu.menminuswomen.pos +
                       logit1.female$coefficients[7]*edu.menminuswomen.neg)/
  (1+  exp(logit1.female$coefficients[1] + logit1.female$coefficients[2]*age.menminuswomen.pos +
             logit1.female$coefficients[3]*age.menminuswomen.pos*age.menminuswomen.pos +
             logit1.female$coefficients[4]*age.menminuswomen.neg +
             logit1.female$coefficients[5]*age.menminuswomen.neg*age.menminuswomen.neg +  
             logit1.female$coefficients[6]*edu.menminuswomen.pos +
             logit1.female$coefficients[7]*edu.menminuswomen.neg))
utility.men = exp(logit1.male$coefficients[1] + logit1.male$coefficients[2]*age.womenminusmen.pos +
                    logit1.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
                    logit1.male$coefficients[4]*age.womenminusmen.neg +
                    logit1.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
                    logit1.male$coefficients[6]*edu.womenminusmen.pos +
                    logit1.male$coefficients[7]*edu.womenminusmen.neg)/
  (1+  exp(logit1.male$coefficients[1] + logit1.male$coefficients[2]*age.womenminusmen.pos +
             logit1.male$coefficients[3]*age.womenminusmen.pos*age.womenminusmen.pos +
             logit1.male$coefficients[4]*age.womenminusmen.neg +
             logit1.male$coefficients[5]*age.womenminusmen.neg*age.womenminusmen.neg +  
             logit1.male$coefficients[6]*edu.womenminusmen.pos +
             logit1.male$coefficients[7]*edu.womenminusmen.neg))

### create the output matrixes
outcome.age.women =rep(NA,nobs)
outcome.edu.women =rep(NA,nobs)
outcome.age.men =rep(NA, nobs)
outcome.edu.men =rep(NA, nobs)

### establish ranking, the higher the utility the higher the rank, then run gale-shapley without rejections
### here I introduce the randomeness: I start the loop here ###########################################
################ Gale-Shapley without rejections ###############
rank.women = matrix(NA, nrow = nobs, ncol=nobs)
rank.men = matrix(NA, nrow = nobs, ncol=nobs)
for (i in 1:nobs) {
  rank.women[i,] =  rank(utility.women[i,], na.last = TRUE, ties.method = c("random"))
  rank.men[i,] =  rank(utility.men[i,], na.last = TRUE, ties.method = c("random"))
}
## transpose to bring in the format needed for the gale shapley matchin algorithm
rank.women = t(rank.women)
rank.men = t(rank.men)
### gale shapley matching 
gs = galeShapley.marriageMarket(proposerUtils = rank.men, 
                                reviewerUtils = rank.women)
## outcome matrix
outcome.age.women = men[gs$engagements,2] 
outcome.edu.women = men[gs$engagements,1]
outcome.age.men = women[gs$proposals,2] 
outcome.edu.men = women[gs$proposals,1]


## add to data set
cfps$age.wife.pred = outcome.age.men
cfps$edu.wife.pred = outcome.edu.men
cfps$age.husband.pred = outcome.age.women
cfps$edu.husband.pred = outcome.edu.women

######## AGE #####
df1 <- data.frame(cfps$age.husband - cfps$age.wife,
                  cfps$age.husband - cfps$age.wife.pred)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband actual age - wife simulated age")


age_spe1 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual age - wife actual age`,
                                 `Husband actual age - wife simulated age`))

df1 <- data.frame(cfps$age.husband - cfps$age.wife, 
                  cfps$age.husband.pred - cfps$age.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual age - wife actual age", 
                "Husband simulated age - wife actual age", "weights_men", "weights_women")

age_spe1_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual age - wife actual age`,
                                       `Husband simulated age - wife actual age`))

#### EDUCATION #######
df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$education.husband - cfps$edu.wife.pred)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband actual education - wife simulated education")

edu_spe1 <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                        cols = c(`Husband actual education - wife actual education`,
                                 `Husband actual education - wife simulated education`))

df1 <- data.frame(cfps$education.husband - cfps$education.wife, 
                  cfps$edu.husband.pred - cfps$education.wife, cfps$weights_men, cfps$weights_women)
names(df1) <- c("Husband actual education - wife actual education", 
                "Husband simulated education - wife actual education", "weights_men", "weights_women")

edu_spe1_women <-pivot_longer(df1, names_to = "variable", values_to = "value", 
                              cols = c(`Husband actual education - wife actual education`,
                                       `Husband simulated education - wife actual education`))


#################################################################################################
##################################" Graphs ######################################################
##################################################################################################

#### Age
plot_age_spe1_urban <- age_spe1 %>% ggplot(aes(value, fill = variable)) + 
  geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Age of husband minus age of wife") + 
  ylab("Density") + lims(x=c(-20,20), y=c(0, 0.6)) + theme_classic() +
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2") 


#### Education
plot_edu_spe1_urban <- edu_spe1 %>% ggplot(aes(value, fill = variable)) +   
  geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity', binwidth=1) +
  xlab("Education of husband minus education of wife") +
  ylab("Density") + lims(x=c(-6,6), y=c(0, 0.9)) + theme_classic() + 
  theme(legend.position="bottom", legend.direction = "vertical",
        legend.title = element_blank()) + scale_fill_brewer(palette="Dark2")


edu_all_weighted <- ggarrange(plot_edu_spe1, plot_edu_spe1_urban, plot_edu_spe1_men, plot_edu_spe1_women , 
                              ncol=1, nrow=4, common.legend = TRUE, legend="bottom", 
                              labels = c("Baseline (Spec.1)", "  Urban only      ",  "Weighted: Men", "Weighted: Women"))
age_all_weighted <- ggarrange(plot_age_spe1, plot_age_spe1_urban, plot_age_spe1_men, plot_age_spe1_women , 
                              ncol=1, nrow=4, common.legend = TRUE, legend="bottom", 
                              labels = c("Baseline (Spec.1)", "  Urban only      ", "Weighted: Men", "Weighted: Women"))


ggsave("age_all_weighted.png", plot= age_all_weighted, width =5, height = 11)
ggsave("edu_all_weighted.png", plot= edu_all_weighted, width =5, height = 11)
