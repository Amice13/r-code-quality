
#========
# Ana S. Cardenal
# Project: Validation (revised)
# data: 01/07/2024
# date revision: 15/12/24 
#=========
# POQ Revision. Bayesian estimation with weights
#==========

# clean
rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

# libraries
install.packages("descr")
library(tidyverse)
library(data.table)
library(lubridate)
library(sjPlot)
library(stargazer)
library(descr)
library(gridExtra)
library(ggpubr)
library(foreign)
library(ggeffects)
library(magrittr)
library(broom)
library(broom.mixed)

# ------------------------------------------
# Step 1: Read and prepare survey data
# -----------------------------------------

# Read survey data
DE_survey <-  fread("Data/DE_surveys_comb.csv") %>% mutate(country = "DE") %>% as_tibble()
ES_survey <-  fread("Data/ES_surveys_comb.csv") %>% mutate(country = "ES") %>% as_tibble()
FR_survey <-  fread("Data/FR_surveys_comb.csv") %>% mutate(country = "FR") %>% as_tibble()
UK_survey <-  fread("Data/UK_surveys_comb.csv") %>% mutate(country = "UK") %>% as_tibble()
US_survey <-  fread("Data/US_surveys_comb.csv") %>% mutate(country = "US") %>% as_tibble()


# Fix person_id to do the matching later 
DE_survey <- DE_survey %>% 
  mutate(person_id = tolower(person_id)) 
FR_survey <- FR_survey %>%
  mutate(person_id = tolower(person_id)) 
US_survey <- US_survey %>%
  mutate(person_id = tolower(person_id)) 
UK_survey <- UK_survey %>%
  mutate(person_id = tolower(person_id)) 

# function to create an index for surveillance knowledge -- exclude q9
create_index_for_SK <- function (data) {
  data <- data %>%
    mutate(q7 = case_when(q7 != 3 ~ 0, TRUE ~ 1)) %>% 
    mutate(q8 = case_when(q8 != 2 ~ 0, TRUE ~ 1)) %>%
    #mutate(q9 = case_when(q9 != 2 ~ 0, TRUE ~ 1)) %>%
    mutate(q10 = case_when(q10 != 2 ~ 0, TRUE ~ 1))
  data$survknow <- data$q7 + data$q8 + data$q10
  
  return(data)
}

# apply function to create measure for surveillance knowledge:
US_survey = create_index_for_SK(US_survey)
UK_survey = create_index_for_SK(UK_survey)
DE_survey = create_index_for_SK(DE_survey)
ES_survey = create_index_for_SK(ES_survey)
FR_survey = create_index_for_SK(FR_survey)

# Create index general political knowledge but for each country since responses change by country
# US
# general political knowledge questions (wave 1) 
US_survey <- US_survey %>%
  mutate(PK1 = case_when(Q15 != 2 ~ 0, TRUE ~ 1)) %>% # here I would use ifelse and only mutate once 
  mutate(PK2 = case_when(Q16 != 1 ~ 0, TRUE ~ 1)) %>%
  mutate(PK3 = case_when(Q17 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK4 = case_when(Q18 != 2 ~ 0, TRUE ~ 1))

US_survey$polknow <- US_survey$PK1 + US_survey$PK2 + US_survey$PK3 + US_survey$PK4

## UK
UK_survey <- UK_survey %>%
  mutate(PK1 = case_when(Q15 != 1 ~ 0, TRUE ~ 1)) %>%
  mutate(PK2 = case_when(Q16 != 1 ~ 0, TRUE ~ 1)) %>%
  mutate(PK3 = case_when(Q17 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK4 = case_when(Q18 != 2 ~ 0, TRUE ~ 1))

UK_survey$polknow <- UK_survey$PK1 + UK_survey$PK2 + UK_survey$PK3 + UK_survey$PK4

## DE
DE_survey <- DE_survey %>%
  mutate(PK1 = case_when(Q15 != 1 ~ 0, TRUE ~ 1)) %>%
  mutate(PK2 = case_when(Q16 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK3 = case_when(Q17 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK4 = case_when(Q18 != 2 ~ 0, TRUE ~ 1))

DE_survey$polknow <- DE_survey$PK1 + DE_survey$PK2 + DE_survey$PK3 + DE_survey$PK4

## FR
FR_survey <- FR_survey %>%
  mutate(PK1 = case_when(Q15 != 1 ~ 0, TRUE ~ 1)) %>%
  mutate(PK2 = case_when(Q16 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK3 = case_when(Q17 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK4 = case_when(Q18 != 2 ~ 0, TRUE ~ 1))

FR_survey$polknow <- FR_survey$PK1 + FR_survey$PK2 + FR_survey$PK3 + FR_survey$PK4

#ES
ES_survey <- ES_survey %>%
  mutate(PK1 = case_when(Q15 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK2 = case_when(Q16 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK3 = case_when(Q17 != 3 ~ 0, TRUE ~ 1)) %>%
  mutate(PK4 = case_when(Q18 != 2 ~ 0, TRUE ~ 1))

ES_survey$polknow <- ES_survey$PK1 + ES_survey$PK2 + ES_survey$PK3 + ES_survey$PK4


# Recode education and age 
ES_survey <- ES_survey %>%
  mutate(educ_rec = case_when(educ_level_ES > 0 & educ_level_ES < 4 ~ 1,
                              educ_level_ES > 3 & educ_level_ES < 5 | educ_level_ES == 10 ~ 2,
                              educ_level_ES > 4 & educ_level_ES < 8 ~ 3,
                              educ_level_ES > 7 & educ_level_ES < 15 ~ 4),
         college = ifelse(educ_rec == 4,1,0))

US_survey <- US_survey %>%
  mutate(educ_rec = case_when(educ.x == 1 ~ 1,
                              educ.x == 2 ~ 2,
                              educ.x > 2 & educ.x < 5 ~ 3,
                              educ.x > 4 & educ.x < 7 ~ 4),
         college = ifelse(educ_rec == 4,1,0))


UK_survey <- UK_survey %>%
  mutate(educ_rec = case_when(profile_education_level.x < 9 ~ 1,
                              profile_education_level.x > 8 & profile_education_level.x < 11 ~ 2,
                              profile_education_level.x > 10 & profile_education_level.x < 15 ~ 3,
                              profile_education_level.x > 14 & profile_education_level.x < 19 ~ 4),
         college = ifelse(educ_rec == 4, 1,0))


FR_survey <- FR_survey %>%
  mutate(educ_rec = case_when(education.x < 3 ~ 1,
                              education.x == 3 ~ 2,
                              education.x > 3 & education.x < 6 ~ 3,
                              education.x > 5 & education.x < 8 ~ 4),
         college = ifelse(educ_rec == 4,1,0))

DE_survey <- DE_survey %>%
  mutate(educ_rec = case_when(educ.x == 8 ~ 1,
                              educ.x > 0 & educ.x < 3 ~ 2,
                              educ.x > 2 & educ.x < 6 ~ 3,
                              educ.x > 5 & educ.x < 8 ~ 4),
         college = ifelse(educ_rec == 4,1,0))


# Rename the columns that end with ".x" (probably a result of join comman) without it - so that the ID column will appear in the merged dataset for all countries.
US_survey = US_survey %>% rename_with(~sub(".x$", "", .), ends_with(".x"))
UK_survey = UK_survey %>% rename_with(~sub(".x$", "", .), ends_with(".x"))
DE_survey = DE_survey %>% rename_with(~sub(".x$", "", .), ends_with(".x"))
ES_survey = ES_survey %>% rename_with(~sub(".x$", "", .), ends_with(".x"))
FR_survey = FR_survey %>% rename_with(~sub(".x$", "", .), ends_with(".x"))


# Get variables that are common 
# Row bind
# select variables that are common to all
# first make sure Spain has the variable person_id
names(ES_survey)[116] <- "person_id" 
list_df = list(US_survey,UK_survey,FR_survey,DE_survey,ES_survey)

col_common = colnames(list_df[[1]])
for (i in 2:length(list_df)){
  col_common = intersect(col_common, colnames(list_df[[i]]))
}

# extract columns
extractColumns = function(x){
  select(x,all_of(col_common))
}

data = lapply(list_df,extractColumns) %>% bind_rows()

# Select survey data for analysis
fdata <- data %>%
  select(person_id, caseid, age, educ_rec, college, Q2_TV, Q2_Newspapers, Q2_Radio, Q3_websites, Q3_Socialmedia, Q5_politics, Q6, q12_ideology,
         country, Q26, Q24, polknow, survknow) %>%
  mutate (pol_int = max(Q6)-Q6,
          int_pnews = max(Q5_politics)-Q5_politics,
          female = ifelse(Q24 == 2,1,0),
          TV = ifelse(Q2_TV > 6, NA, Q2_TV),
          NP = ifelse(Q2_Newspapers > 6, NA, Q2_Newspapers),
          Radio = ifelse(Q2_Radio > 6, NA, Q2_Radio),
          Wsites = ifelse (Q3_websites > 6, NA, Q3_websites),
          Smedia = ifelse (Q3_Socialmedia > 6, NA, Q3_Socialmedia),
          TV_rec = max(TV, na.rm = T)-TV,
          Newspapers_rec = max(NP, na.rm = T)-NP,
          Radio_rec = max(Radio, na.rm = T)-Radio,
          Wsites_rec = max(Wsites, na.rm = T)-Wsites,
          Smedia_rec = max(Smedia, na.rm = T)-Smedia,
          educ_rec = educ_rec-1,
          income = ifelse(Q26 == 996 | Q26 == 997, NA, Q26),
          income = income -1)

# -----------------------
# Step 2: Get web-tracking data
# --------------------------

# Call data wp3_long
wp3_long <- read.csv("Data/wp3_long.csv")

# Transform start_time as date 
wp3_long$date <- as.Date(wp3_long$start_time)


# -------------------------
# 2.1. Apply filters  
# -----------------------  
# Data < 2022-05-23 (date of launching of wave 2)
# Lower bound > 4 sec
# Upper bound < 1,801 sec

# Check some stats on the data 
# Filtered 
stats_filtered <- wp3_long %>%
  filter (date < "2022-05-23" & page_duration < 1801 & page_duration > 4) %>% 
  group_by (iso2) %>%
  summarise( N = n(), political_visits = sum(political_url, na.rm = T),
             ukraine_visits = sum(political_url_russia_ukraine, na.rm = T))

# Non filtered data 
stats_nonfiltered <- wp3_long %>%
  group_by (iso2) %>%
  summarise( N = n(), political_visits = sum(political_url, na.rm = T),
             ukraine_visits = sum(political_url_russia_ukraine, na.rm = T))


# Get sample size of web-tracking data by country 
wp3_long_fil <- wp3_long %>%
  filter (date < "2022-05-23" & page_duration < 1801 & page_duration > 4) %>%
  group_by (iso2, person_id) %>%
  summarise(n()) %>% 
  group_by (iso2) %>%
  summarise(n())

#1 DE      283
#2 ES      280
#3 FR      292
#4 UK      339
#5 US      346


# -------------------------------------------------------------------------------
# Get key observed measures/predictors from filtered dataset 
observed_measures <- wp3_long %>%
  filter (date < "2022-05-23" & page_duration < 1801 & page_duration > 4) %>% 
  mutate (political_url_times = political_url*page_duration,
          political_url_ukraine_times = political_url_russia_ukraine*page_duration) %>%
  group_by (person_id) %>%
  summarize (news_visits = n(), news_minutes = sum(page_duration, na.rm=T)/60, 
             political_url_visits = sum(political_url, na.rm=T), 
             political_url_times = sum(political_url_times, na.rm=T)/60,
             political_url_ukraine = sum(political_url_russia_ukraine, na.rm=T),
             political_url_ukraine_times = sum(political_url_ukraine_times,na.rm=T)/60)


# Get other observed measures --social media frequency, frequency of direct access
# call data wp3_wide 
wp3_wide <- read.csv("Data/wp3_wide.csv")


observed_controls <- wp3_wide %>%
  select (person_id, social_media_visits, direct_news_visits)


# Histogram of key measures to identify outliers 
#p <- observed_measures %>% 
#  ggplot (aes(news_minutes)) + geom_histogram(bins = 50)

# left join to fdata
fdata <- fdata %>% 
  left_join(observed_measures, by = "person_id") %>% 
  left_join (observed_controls, by = "person_id")


# Normalize DV
fdata <- fdata %>% 
  mutate (s_know_nor = (survknow-min(survknow))/(max(survknow)-min(survknow))
  )

save(fdata, file = "Data/fdata")


# ============================================================
# ANALYSIS 
# ============================================================
#library(lme4)
#library(lmerTest)
# install.packages("ordinal")
#library(ordinal)

# Bayesian 
#install.packages("brms")
library (brms)


#================================= LOG-TRANSFORM PREDICTORS ============================================= #

fdata <- fdata %>%
  mutate (log_news_visits = log (news_visits),
          log_social_media_visits = log (social_media_visits),
          log_news_minutes = log (news_minutes),
          log_political_url_visits = log (political_url_visits + 1),
          log_political_url_times = log (political_url_times + 1),
          log_political_url_ukraine = log (political_url_ukraine +1),
          log_political_url_ukraine_times = log (political_url_ukraine_times +1),
          p_know_nor = (polknow-min(polknow))/(max(polknow)-min(polknow)),
          educ_rec_nor = (educ_rec-min(educ_rec, na.rm = T))/(max(educ_rec, na.rm =T)-min(educ_rec, na.rm =T)),
          pol_int_nor = (pol_int-min(pol_int))/(max(pol_int)-min(pol_int)),
          income_nor = (income-min(income, na.rm = T))/(max(income, na.rm = T)-min(income, na.rm =T)),
          TV_rec_nor = (TV_rec-min(TV_rec, na.rm =T))/(max(TV_rec, na.rm =T)-min(TV_rec, na.rm =T)),
          Newspapers_rec_nor = (Newspapers_rec-min(Newspapers_rec, na.rm = T ))/(max(Newspapers_rec, na.rm = T)-min(Newspapers_rec, na.rm = T)),
          Wsites_rec_nor = (Wsites_rec-min(Wsites_rec, na.rm =T))/(max(Wsites_rec, na.rm =T)-min(Wsites_rec, na.rm =T)),
          Smedia_rec_nor = (Smedia_rec-min(Smedia_rec, na.rm =T))/(max(Smedia_rec, na.rm =T)-min(Smedia_rec, na.rm =T)),
          q12_ideology_nor =  (q12_ideology-min(q12_ideology))/(max(q12_ideology)-min(q12_ideology))
  )

# ===============================
# Bayesian estimation. Run models without self-reported measures of online news consumption  
# ============================
fdata$s_know_nor_fc <- as.ordered(fdata$s_know_nor)
fdata$female_fc <- as.factor(fdata$female)


# Equations and estimation 
# Model 1
log_eq1 <- s_know_nor_fc ~ 1 + log_news_visits + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + p_know_nor + (1 + log_news_visits | country)

bmodel1 <- brm (
  log_eq1, 
  data = fdata,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel1)
plot(bmodel1)

# Model 2
log_eq2 <- s_know_nor_fc ~ 1 + log_news_minutes + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + p_know_nor + (1 + log_news_minutes | country)

bmodel2 <- brm (
  log_eq2, 
  data = fdata,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel2)
plot(bmodel2)

# Model 3
log_eq3 <- s_know_nor_fc ~ 1 + log_political_url_visits + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + p_know_nor + (1 + log_political_url_visits | country)

bmodel3 <- brm (
  log_eq3, 
  data = fdata,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel3)
plot(bmodel3)

# Model 4
log_eq4 <- s_know_nor_fc ~ 1 + log_political_url_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + p_know_nor + (1  + log_political_url_times | country)

bmodel4 <- brm (
  log_eq4, 
  data = fdata,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel4)
plot(bmodel4)

# Model 5
log_eq5 <- s_know_nor_fc ~ 1 + log_political_url_ukraine + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + p_know_nor + (1 + log_political_url_ukraine | country)

bmodel5 <- brm (
  log_eq5, 
  data = fdata,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel5)
plot(bmodel5)

# Model 6
log_eq6 <- s_know_nor_fc ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + p_know_nor + (1  + log_political_url_ukraine_times | country)

bmodel6 <- brm (
  log_eq6, 
  data = fdata,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel6)
plot(bmodel6)

# =================================================
# Run models with weighs. Use new age, gender and education variables and self-reported measures as controls  
# ===================================================
# load the new dataset with weights
load ("Data/fdata_with_weights")

fdata_w_weights <- fdata_with_weights %>%
  select (person_id, age_group, education, gender, composite_weight) %>%
  left_join (fdata, by = "person_id")


# Remove the one observation in Germany that has a weight of 44
fdata_without_obs <- dplyr::filter(fdata_w_weights, composite_weight <= 66)

# Model 1
# Define the model formula
log_eq1_weighted <- s_know_nor_fc | weights (composite_weight) ~ 1 + log_news_visits + log_social_media_visits + 
    female_fc + age + educ_rec_nor + income_nor + 
    q12_ideology_nor + pol_int_nor + TV_rec_nor + 
    Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + 
    (1 + log_news_visits | country)

# Run the Bayesian model with weights
bmodel1_weighted <- brm(
  formula = log_eq1_weighted,  # Correct use of weights in bf()
  data = fdata_without_obs,
  family = cumulative("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

# Summarize the results
summary(bmodel1_weighted)

# Plot the results
plot(bmodel1_weighted)

# Model 2
# Define the model formula
log_eq2_weighted <- s_know_nor_fc | weights (composite_weight) ~ 1 + log_news_minutes + log_social_media_visits + 
  female_fc + age + educ_rec_nor + income_nor + 
  q12_ideology_nor + pol_int_nor + TV_rec_nor + 
  Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + 
  (1 + log_news_minutes | country)

# Run the Bayesian model with weights
bmodel2_weighted <- brm(
  formula = log_eq2_weighted,  # Correct use of weights in bf()
  data = fdata_without_obs,
  family = cumulative("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

# Summarize the results
summary(bmodel2_weighted)

# Plot the results
plot(bmodel2_weighted)

# Model 3
# Define the model formula
log_eq3_weighted <- s_know_nor_fc | weights (composite_weight) ~ 1 + log_political_url_visits + log_social_media_visits + 
  female_fc + age + educ_rec_nor + income_nor + 
  q12_ideology_nor + pol_int_nor + TV_rec_nor + 
  Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + 
  (1 + log_political_url_visits | country)

# Run the Bayesian model with weights
bmodel3_weighted <- brm(
  formula = log_eq3_weighted,  # Correct use of weights in bf()
  data = fdata_without_obs,
  family = cumulative("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

# Summarize the results
summary(bmodel3_weighted)

# Plot the results
plot(bmodel3_weighted)

# Model 4
# Define the model formula
log_eq4_weighted <- s_know_nor_fc | weights (composite_weight) ~ 1 + log_political_url_times + log_social_media_visits + 
  female_fc + age + educ_rec_nor + income_nor + 
  q12_ideology_nor + pol_int_nor + TV_rec_nor + 
  Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + 
  (1 + log_political_url_times | country)

# Run the Bayesian model with weights
bmodel4_weighted <- brm(
  formula = log_eq4_weighted,  # Correct use of weights in bf()
  data = fdata_without_obs,
  family = cumulative("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

# Summarize the results
summary(bmodel4_weighted)

# Plot the results
plot(bmodel4_weighted)


# Model 5
log_eq5_weighted <- s_know_nor_fc | weights (composite_weight) ~ 1 + log_political_url_ukraine + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_political_url_ukraine | country)

bmodel5_weighted <- brm (
  formula = log_eq5_weighted, 
  data = fdata_without_obs,
  family = cumulative ("logit"),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel5_weighted)
plot(bmodel5)

# Model 6
log_eq6_weighted <- s_know_nor_fc | weights (composite_weight) ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1  + log_political_url_ukraine_times | country)

bmodel6_weighted <- brm (
  formula = log_eq6_weighted, 
  data = fdata_without_obs,
  family = cumulative ("logit"),
  iter = 6000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel6_weighted)
plot(bmodel6_weighted)

# =========================================================
# Report estimates
# ========================================================
library(tidyverse)
library(broom.mixed)
library(brms)


# Basic summary table
library(bayestestR)
library(ggplot2)

# Simulated data
posterior <- data.frame(
  parameter = c("Log News Visits", "Log News Minutes", "Log Political Visits",
                "Log Political Minutes", "Log Ukraine Visits", "Log Ukraine Minutes"),
  mean = c(0.06, 0.06, 0.05, 0.06, 0.13, 0.14),
  lower = c(-0.04, -0.02, -0.07, -0.06, 0, 0.02),
  upper = c(0.16, 0.14, 0.18, 0.17, 0.26, 0.27)
)

# Plot
png("intervals.png", width = 4000, height = 2000, res = 300)  # High resolution
ggplot(posterior, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    title = "", # Posterior Estimates with 95% Credible Intervals
    x = "",
    y = "Estimate"
  ) + 
  theme_bw() + 
  theme (
    axis.text = element_text(size = 14),
    axis.title = element_text (size =14)
  ) 


dev.off ()


# Table random effects
library(dplyr)
library(tibble)
library(knitr)

# Create the data frame (using tribble for readability)
table_data <- tribble(
  ~Parameter,                          ~Model_1,                 ~Model_2,                 ~Model_3,                ~Model_4,                ~Model_5,                ~Model_6,
  "sd__(Intercept)",                  "0.53 (0.08, 1.47)",      "0.46 (0.06, 1.27)",      "0.72 (0.14, 4.48)",     "0.51 (0.12, 1.36)",     "0.51 (0.14, 1.36)",     "0.5 (0.12, 1.41)",
  "sd__log_news_minutes",            "—",                       "0.05 (0.0, 0.18)",       "—",                     "—",                     "—",                     "—",
  "sd__log_news_visits",             "0.07 (0.0, 0.23)",       "—",                      "—",                     "—",                     "—",                     "—",
  "sd__log_political_url_times",     "—",                       "—",                      "—",                     "0.08 (0.0, 0.27)",      "—",                     "—",
  "sd__log_political_url_ukraine",   "—",                       "—",                      "—",                     "—",                     "0.07 (0.0, 0.29)",      "—",
  "sd__log_political_url_ukraine_times", "—",                   "—",                      "—",                     "—",                     "—",                     "0.08 (0.0, 0.29)",
  "sd__log_political_url_visits",    "—",                       "—",                      "0.1 (0.0, 0.32)",       "—",                     "—",                     "—"
)

# Print the data frame as a table
png ("table")
kable(table_data, format = "html", 
      caption = "Table of Random-Effects Parameters")
dev.off()




# List of models
models <- list(bmodel1_weighted, bmodel2_weighted, bmodel3_weighted, bmodel4_weighted, bmodel5_weighted, bmodel6_weighted)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")

# Extract fixed effects
fixed_effects_weighted <- map2_dfr(models, model_names, ~ {
  tidy(.x, effects = "fixed") %>%
    mutate(Model = .y)
})

# Extract random effects (group-level effects)
random_effects_weighted <- map2_dfr(models, model_names, ~ {
  tidy(.x, effects = "ran_pars") %>%
    mutate(Model = .y)
})

# View the extracted tables
fixed_effects_weighted
random_effects_weighted

# Fixed effects table for reporting
fixed_table_weighted <- fixed_effects_weighted %>%
  select(Model, term, estimate, std.error, conf.low, conf.high) %>%
  rename(
    Predictor = term,
    Mean = estimate,
    SD = std.error,
    `95% CI (Lower)` = conf.low,
    `95% CI (Upper)` = conf.high
  )

# Print the table
fixed_table_weighted

# Random effects table for reporting
random_table_weighted <- random_effects_weighted %>%
  select(Model, group, term, estimate, conf.low, conf.high) %>%
  rename(
    Group = group,
    Effect = term,
    SD = estimate,
    `95% CI (Lower)` = conf.low,
    `95% CI (Upper)` = conf.high
  )

# Print the table
random_table_weighted

# Save tables
write_csv(fixed_table_weighted, "fixed_effects_weighted_table.csv")
write_csv(random_table_weighted, "random_effects_weighted_table.csv")


# ===========================================
# Plotting
# ===========================================

# Plot coefficients
# Prepare data for coefficient plot
coef_plot_data <- fixed_effects %>%
  filter(term != "Intercept") %>% # Exclude intercepts if not needed
  mutate(term = factor(term, levels = unique(term)))

# Define the new order and names for predictors
new_order <- c(
  "log_news_visits", "log_news_minutes", "log_political_url_visits",
  "log_political_url_times", "log_political_url_ukraine", "log_political_url_ukraine_times",
  "log_social_media_visits", "female_fc1", "age", "educ_rec_nor",
  "income_nor", "q12_ideology_nor", "pol_int_nor",
  "TV_rec_nor", "Newspapers_rec_nor", "Wsites_rec_nor",
  "Smedia_rec_nor", "p_know_nor", 
  "(Intercept)[1]", "(Intercept)[2]", "(Intercept)[3]"
) # Replace with the original names in the desired order

new_labels <- c(
  "Log News Visits", "Log News Minutes", "Log Political Visits",
  "Log Political Minutes", "Log Ukraine Visits", "Log Ukraine Minutes",
  "Log Social Media Visits", "Female", "Age", "Education",
  "Income", "Ideology(Left_Right)", "Political Interest",
  "Self-Rep TV Frequency", "Self-Rep Newspaper Frequency", "Self-Rep Websites Frequency",
  "Self-Rep Social Media Frequency", "Political Knowledge", 
  "(Intercept)[1]", "(Intercept)[2]", "(Intercept)[3]"
) # Replace with the new labels

# Update the term column in coef_plot_data
coef_plot_data <- coef_plot_data %>%
  mutate(term = factor(term, levels = rev(new_order), labels = rev(new_labels)))

# Plot fixed effects
ggplot(coef_plot_data, aes(x = estimate, y = term, color = Model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.5)) +
  labs(
    title = "Posterior Estimates of Fixed Effects",
    x = "Posterior Mean (95% CI)",
    y = "Predictor",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

ggsave("fixed_effects_plot.png", dpi = 300, width = 10, height = 7)


# Enhanced graphed
library(ggplot2)

# Enhanced plot for journal submission
ggplot(coef_plot_data, aes(x = estimate, y = term, color = Model)) +
  # Add point markers with a distinct size for clarity
  geom_point(size = 3.5, position = position_dodge(width = 0.6)) +
  
  # Add horizontal error bars for 95% CI
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25, 
                 position = position_dodge(width = 0.6), linewidth = 0.8) +
  
  # Add vertical grid lines for reference
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  
  # Customize axis labels and title
  labs(
    #title = "Posterior Estimates of Fixed Effects",
    #subtitle = "Error bars represent 95% credible intervals",
    x = "Posterior Mean (95% CI)",
    y = NULL,  # Remove y-axis label for a cleaner look
    color = "Model"
  ) +
  
  # Customize color palette for better differentiation
  scale_color_brewer(palette = "Dark2") +
  
  # Adjust the minimal theme for a professional look
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",  # Place the legend at the top for better use of space
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"),  # Center subtitle
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "gray85"),  # Subtle grid lines for x-axis
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines
    panel.grid.major.y = element_blank()  # Remove grid lines for y-axis for a clean look
  )


# Prepare data for random effects plot
random_plot_data <- random_effects %>%
  filter(term == "sd__(Intercept)") # Focus on intercept SDs (adjust as needed)

# Plot random effects
ggplot(random_plot_data, aes(x = estimate, y = group, color = Model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.5)) +
  labs(
    title = "Random Effects: Standard Deviations of Intercepts",
    x = "Posterior Mean (95% CI)",
    y = "Group",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

# random slopes 
random_plot_data <- random_effects %>%
  filter(str_detect(term, "sd__") | str_detect(term, "KeyPredictorName"))

ggplot(random_plot_data, aes(x = estimate, y = term, color = Model)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.5)) +
  labs(
    title = "Random Effects: Standard Deviations",
    x = "Posterior Mean (95% CI)",
    y = "Effect",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )

ggsave("random_effects_plot.png", dpi = 300, width = 10, height = 7)




