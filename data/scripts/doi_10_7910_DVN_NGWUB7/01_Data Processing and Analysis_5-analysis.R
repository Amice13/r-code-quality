#========
# Ana S. Cardenal
# Project: Validation (revised)
# data: 01/07/2024
# date revision: 15/12/24 
#=========
## POQ Revision. Bayesian estimation
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
DE_survey <-  fread("DE_surveys_comb.csv") %>% mutate(country = "DE") %>% as_tibble()
ES_survey <-  fread("ES_surveys_comb.csv") %>% mutate(country = "ES") %>% as_tibble()
FR_survey <-  fread("FR_surveys_comb.csv") %>% mutate(country = "FR") %>% as_tibble()
UK_survey <-  fread("UK_surveys_comb.csv") %>% mutate(country = "UK") %>% as_tibble()
US_survey <-  fread("US_surveys_comb.csv") %>% mutate(country = "US") %>% as_tibble()


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
wp3_long <- read.csv("wp3_long.csv")

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
wp3_wide <- read.csv("wp3_wide.csv")


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

save(fdata, file = "fdata")


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
# Bayesian estimation. Run models with self-reported measures of online news consumption 
# ============================
fdata$s_know_nor_fc <- as.ordered(fdata$s_know_nor)
fdata$female_fc <- as.factor(fdata$female)

# Equations and estimation 
# Model 1
log_eq1 <- s_know_nor_fc ~ 1 + log_news_visits + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_news_visits | country)

bmodel1 <- brm (
  log_eq1, 
  data = fdata,
  family = cumulative ("logit"),
  control = list (adapt_delta = 0.99),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
  )

summary (bmodel1)
plot(bmodel1)

# Model 2
log_eq2 <- s_know_nor_fc ~ 1 + log_news_minutes + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_news_minutes | country)

bmodel2 <- brm (
  log_eq2, 
  data = fdata,
  family = cumulative ("logit"),
  control = list (adapt_delta = 0.99),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel2)
plot(bmodel2)

# Model 3
log_eq3 <- s_know_nor_fc ~ 1 + log_political_url_visits + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_political_url_visits | country)

bmodel3 <- brm (
  log_eq3, 
  data = fdata,
  family = cumulative ("logit"),
  control = list (adapt_delta = 0.99),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel3)
plot(bmodel3)

# Model 4
log_eq4 <- s_know_nor_fc ~ 1 + log_political_url_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1  + log_political_url_times | country)

bmodel4 <- brm (
  log_eq4, 
  data = fdata,
  family = cumulative ("logit"),
  control = list (adapt_delta = 0.99),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel4)
plot(bmodel4)

# Model 5
log_eq5 <- s_know_nor_fc ~ 1 + log_political_url_ukraine + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_political_url_ukraine | country)

bmodel5 <- brm (
  log_eq5, 
  data = fdata,
  family = cumulative ("logit"),
  control = list (adapt_delta = 0.99),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel5)
plot(bmodel5)

# Model 6
log_eq6 <- s_know_nor_fc ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1  + log_political_url_ukraine_times | country)

bmodel6 <- brm (
  log_eq6, 
  data = fdata,
  family = cumulative ("logit"),
  control = list (adapt_delta = 0.99),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary (bmodel6)
plot(bmodel6)

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
  mean = c(0.07, 0.06, 0.05, 0.07, 0.13, 0.14),
  lower = c(-0.04, -0.02, -0.09, -0.05, 0.01, 0.01),
  upper = c(0.17, 0.14, 0.17, 0.18, 0.26, 0.27)
)

# Plot
png("intervals.png", width = 4000, height = 2000, res = 300)  # High resolution

ggplot(posterior, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline (yintercept = 0.0, linetype = "dashed") + 
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


# =============================
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
# ===================================================


# List of models
models <- list(bmodel1, bmodel2, bmodel3, bmodel4, bmodel5, bmodel6)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")

# Extract fixed effects
fixed_effects <- map2_dfr(models, model_names, ~ {
  tidy(.x, effects = "fixed") %>%
    mutate(Model = .y)
})

# Extract random effects (group-level effects)
random_effects <- map2_dfr(models, model_names, ~ {
  tidy(.x, effects = "ran_pars") %>%
    mutate(Model = .y)
})

# View the extracted tables
fixed_effects
random_effects

# Fixed effects table for reporting
fixed_table <- fixed_effects %>%
  select(Model, term, estimate, std.error, conf.low, conf.high) %>%
  rename(
    Predictor = term,
    Mean = estimate,
    SD = std.error,
    `95% CI (Lower)` = conf.low,
    `95% CI (Upper)` = conf.high
  )

# Print the table
fixed_table

# Random effects table for reporting
random_table <- random_effects %>%
  select(Model, group, term, estimate, conf.low, conf.high) %>%
  rename(
    Group = group,
    Effect = term,
    SD = estimate,
    `95% CI (Lower)` = conf.low,
    `95% CI (Upper)` = conf.high
  )

# Print the table
random_table

# Save tables
write_csv(fixed_table, "fixed_effects_table.csv")
write_csv(random_table, "random_effects_table.csv")

# =====================
# Plot coefficients
# =======================
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




# ==============================================================================================================
# Computing marginal effects 
# ==============================================================================================================
# Model 1: news visits
# ==========================================
# Expand the grid for all countries 
new_data <- expand.grid (
  log_news_visits = seq(min(fdata$log_news_visits, na.rm =T), max(fdata$log_news_visits, na.rm =T), length =100),
  country = unique(fdata$country),
  log_social_media_visits = mean(fdata$log_social_media_visits, na.rm = T),
  female_fc = factor(1, levels = levels(fdata$female_fc)),
  age = mean(fdata$age),
  educ_rec_nor = mean(fdata$educ_rec_nor, na.rm =T),
  income_nor = mean(fdata$income_nor, na.rm =T),
  q12_ideology_nor = mean(fdata$q12_ideology_nor, na.rm =T),
  pol_int_nor = mean(fdata$pol_int_nor, na.rtm =T),
  TV_rec_nor = mean(fdata$TV_rec_nor, na.rm =T),
  Newspapers_rec_nor = mean(fdata$Newspapers_rec_nor, na.rm =T),
  Wsites_rec_nor = mean(fdata$Wsites_rec_nor, na.rm =T),
  Smedia_rec_nor = mean(fdata$Smedia_rec_nor, na.rm =T),
  p_know_nor = mean(fdata$p_know_nor, na.rm =T)
  )

# Generate a grid of predictor values
predicted_probs <- posterior_epred (bmodel1, newdata = new_data, re_formula = NULL)

# Summarize probabilities for each response level
mean_probs <- apply(predicted_probs, c(2,3), mean)
ci_lower <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.025))
ci_upper <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.975))

# As data frame
mean_probs_df <- as.data.frame(mean_probs) 
colnames(mean_probs_df) = c("Category 1", "Category 2", "Category 3", "Category 4")
ci_lower_df <- as.data.frame(ci_lower)
colnames(ci_lower_df) = c("Category 1", "Category 2", "Category 3", "Category 4")
ci_upper_df <- as.data.frame(ci_upper)
colnames(ci_upper_df) = c("Category 1", "Category 2", "Category 3", "Category 4")

# Add observation and response level identifiers
mean_probs_df$observation <- rep(1:nrow(mean_probs_df))
ci_lower_df$observation <- rep(1:nrow(ci_lower_df))
ci_upper_df$observation <- rep(1:nrow(ci_upper_df)) 

# Reshape to long format
prob_summary_long <- mean_probs_df %>%
  pivot_longer(-observation, names_to = "response_level", values_to = "probability") %>%
  left_join(ci_lower_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "lower"),
            by = c("observation", "response_level")) %>%
  left_join(ci_upper_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "upper"),
            by = c("observation", "response_level"))

# Merge with predictor and country information
prob_summary_long <- prob_summary_long %>%
  mutate(
    log_news_visits = rep(new_data$log_news_visits, each = ncol(predicted_probs[1, , ])),
    country = rep(new_data$country, each = ncol(predicted_probs[1, , ]))
  )


# Visualize 
ggplot(prob_summary_long, aes(x = log_news_visits, y = probability, color = as.factor(response_level))) +
  geom_line (size = 1) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.2, color = NA
  ) +
  facet_wrap ( ~ country) + 
  labs(
    x = "Predictor",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    title = "Predicted Probabilities by Response Level"
  ) +
  theme_minimal()


# Enhanced plot 
ggplot(prob_summary_long, aes(x = log_news_visits, y = probability, color = as.factor(response_level))) +
  # Use distinct line style and size
  geom_line(size = 1.2) +
  
  # Improved ribbon for confidence intervals
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.15, color = NA
  ) +
  
  # Facet by country with better layout
  facet_wrap(~ country, ncol = 3, scales = "free_x") +
  
  # Enhanced axis labels and legend
  labs(
    x = "Log of News Visits",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    #title = "Predicted Probabilities by Response Level and Country",
    #subtitle = "Exploring the association with news visits"
  ) +
  
  # Customize colors for better readability
  scale_color_brewer(palette = "Dark2", name = "Response Level") +
  scale_fill_brewer(palette = "Dark2", name = "Response Level") +
  
  # Add a horizontal reference line at 0.5 for context
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Customize the theme for a clean, professional look
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    strip.text = element_text(size = 14, face = "bold"), # Facet titles
    axis.title = element_text(size = 14), # Axis titles
    axis.text = element_text(size = 12), # Axis text
    legend.position = "top", # Legend placement
    legend.title = element_text(size = 14), # Legend title
    legend.text = element_text(size = 12), # Legend text
    plot.title = element_text(size = 16, face = "bold"), # Title
    plot.subtitle = element_text(size = 14, face = "italic"), # Subtitle
    panel.grid.major = element_line(color = "gray80"), # Grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# ################################################################################################
# Iterate over all models and predictors
# ##########################################################################################
# Model 2 : news minutes 
# -----------------------------------
# Expand the grid for all countries 
new_data <- expand.grid (
  log_news_minutes = seq(min(fdata$log_news_minutes, na.rm =T), max(fdata$log_news_minutes, na.rm =T), length =100),
  country = unique(fdata$country),
  log_social_media_visits = mean(fdata$log_social_media_visits, na.rm = T),
  female_fc = factor(1, levels = levels(fdata$female_fc)),
  age = mean(fdata$age),
  educ_rec_nor = mean(fdata$educ_rec_nor, na.rm =T),
  income_nor = mean(fdata$income_nor, na.rm =T),
  q12_ideology_nor = mean(fdata$q12_ideology_nor, na.rm =T),
  pol_int_nor = mean(fdata$pol_int_nor, na.rtm =T),
  TV_rec_nor = mean(fdata$TV_rec_nor, na.rm =T),
  Newspapers_rec_nor = mean(fdata$Newspapers_rec_nor, na.rm =T),
  Wsites_rec_nor = mean(fdata$Wsites_rec_nor, na.rm =T),
  Smedia_rec_nor = mean(fdata$Smedia_rec_nor, na.rm =T),
  p_know_nor = mean(fdata$p_know_nor, na.rm =T)
)

# Generate a grid of predictor values
predicted_probs <- posterior_epred (bmodel2, newdata = new_data, re_formula = NULL)

# Summarize probabilities for each response level
mean_probs <- apply(predicted_probs, c(2,3), mean)
ci_lower <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.025))
ci_upper <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.975))

# As data frame
mean_probs_df <- as.data.frame(mean_probs) 
colnames(mean_probs_df) = c("Category 1", "Category 2", "Category 3", "Category 4")
ci_lower_df <- as.data.frame(ci_lower)
colnames(ci_lower_df) = c("Category 1", "Category 2", "Category 3", "Category 4")
ci_upper_df <- as.data.frame(ci_upper)
colnames(ci_upper_df) = c("Category 1", "Category 2", "Category 3", "Category 4")

# Add observation and response level identifiers
mean_probs_df$observation <- rep(1:nrow(mean_probs_df))
ci_lower_df$observation <- rep(1:nrow(ci_lower_df))
ci_upper_df$observation <- rep(1:nrow(ci_upper_df)) 

# Reshape to long format
prob_summary_long <- mean_probs_df %>%
  pivot_longer(-observation, names_to = "response_level", values_to = "probability") %>%
  left_join(ci_lower_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "lower"),
            by = c("observation", "response_level")) %>%
  left_join(ci_upper_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "upper"),
            by = c("observation", "response_level"))

# Merge with predictor and country information
prob_summary_long <- prob_summary_long %>%
  mutate(
    log_news_minutes = rep(new_data$log_news_minutes, each = ncol(predicted_probs[1, , ])),
    country = rep(new_data$country, each = ncol(predicted_probs[1, , ]))
  )

# Visualize 
ggplot(prob_summary_long, aes(x = log_news_minutes, y = probability, color = as.factor(response_level))) +
  geom_line (size = 1) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.2, color = NA
  ) +
  facet_wrap ( ~ country) + 
  labs(
    x = "Predictor",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    title = "Predicted Probabilities by Response Level"
  ) +
  theme_minimal()


# Enhanced plot 
ggplot(prob_summary_long, aes(x = log_news_minutes, y = probability, color = as.factor(response_level))) +
  # Use distinct line style and size
  geom_line(size = 1.2) +
  
  # Improved ribbon for confidence intervals
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.15, color = NA
  ) +
  
  # Facet by country with better layout
  facet_wrap(~ country, ncol = 3, scales = "free_x") +
  
  # Enhanced axis labels and legend
  labs(
    x = "Log of News Minutes",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    title = "Predicted Probabilities by Response Level and Country",
    subtitle = "Exploring the association with news minutes"
  ) +
  
  # Customize colors for better readability
  scale_color_brewer(palette = "Dark2", name = "Response Level") +
  scale_fill_brewer(palette = "Dark2", name = "Response Level") +
  
  # Add a horizontal reference line at 0.5 for context
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Customize the theme for a clean, professional look
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    strip.text = element_text(size = 14, face = "bold"), # Facet titles
    axis.title = element_text(size = 14), # Axis titles
    axis.text = element_text(size = 12), # Axis text
    legend.position = "top", # Legend placement
    legend.title = element_text(size = 14), # Legend title
    legend.text = element_text(size = 12), # Legend text
    plot.title = element_text(size = 16, face = "bold"), # Title
    plot.subtitle = element_text(size = 14, face = "italic"), # Subtitle
    panel.grid.major = element_line(color = "gray80"), # Grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

# ===============================================
# MARGINAL EFFECTS THAT GO INTO THE PAPER
# ==============================================

# ===========================================
# Model 5 : Ukraine visits
# ==========================================
# Expand the grid for all countries 
new_data <- expand.grid (
  log_political_url_ukraine = seq(min(fdata$log_political_url_ukraine, na.rm =T), max(fdata$log_political_url_ukraine, na.rm =T), length =100),
  country = unique(fdata$country),
  log_social_media_visits = mean(fdata$log_social_media_visits, na.rm = T),
  female_fc = factor(1, levels = levels(fdata$female_fc)),
  age = mean(fdata$age),
  educ_rec_nor = mean(fdata$educ_rec_nor, na.rm =T),
  income_nor = mean(fdata$income_nor, na.rm =T),
  q12_ideology_nor = mean(fdata$q12_ideology_nor, na.rm =T),
  pol_int_nor = mean(fdata$pol_int_nor, na.rtm =T),
  TV_rec_nor = mean(fdata$TV_rec_nor, na.rm =T),
  Newspapers_rec_nor = mean(fdata$Newspapers_rec_nor, na.rm =T),
  Wsites_rec_nor = mean(fdata$Wsites_rec_nor, na.rm =T),
  Smedia_rec_nor = mean(fdata$Smedia_rec_nor, na.rm =T),
  p_know_nor = mean(fdata$p_know_nor, na.rm =T)
)

# Generate a grid of predictor values
predicted_probs <- posterior_epred (bmodel5, newdata = new_data, re_formula = NULL)

# Summarize probabilities for each response level
mean_probs <- apply(predicted_probs, c(2,3), mean)
ci_lower <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.025))
ci_upper <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.975))

# As data frame
mean_probs_df <- as.data.frame(mean_probs) 
colnames(mean_probs_df) = c("0 correct answers", "1 correct answer", "2 correct answers", "3 correct answers")
ci_lower_df <- as.data.frame(ci_lower)
colnames(ci_lower_df) = c("0 correct answers", "1 correct answer", "2 correct answers", "3 correct answers")
ci_upper_df <- as.data.frame(ci_upper)
colnames(ci_upper_df) = c("0 correct answers", "1 correct answer", "2 correct answers", "3 correct answers")

# Add observation and response level identifiers
mean_probs_df$observation <- rep(1:nrow(mean_probs_df))
ci_lower_df$observation <- rep(1:nrow(ci_lower_df))
ci_upper_df$observation <- rep(1:nrow(ci_upper_df)) 

# Reshape to long format
prob_summary_long <- mean_probs_df %>%
  pivot_longer(-observation, names_to = "response_level", values_to = "probability") %>%
  left_join(ci_lower_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "lower"),
            by = c("observation", "response_level")) %>%
  left_join(ci_upper_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "upper"),
            by = c("observation", "response_level"))

# Merge with predictor and country information
prob_summary_long <- prob_summary_long %>%
  mutate(
    log_political_url_ukraine = rep(new_data$log_political_url_ukraine, each = ncol(predicted_probs[1, , ])),
    country = rep(new_data$country, each = ncol(predicted_probs[1, , ]))
  )


# Visualize 
ggplot(prob_summary_long, aes(x = log_political_url_ukraine, y = probability, color = as.factor(response_level))) +
  geom_line (size = 1) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.2, color = NA
  ) +
  facet_wrap ( ~ country) + 
  labs(
    x = "Predictor",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    title = "Predicted Probabilities by Response Level"
  ) +
  theme_minimal()


# Enhanced plot
png("marginal_effects_ukv.png", width = 4000, height = 2000, res = 300)  # High resolution
ggplot(prob_summary_long, aes(x = log_political_url_ukraine, y = probability, color = as.factor(response_level))) +
  # Use distinct line style and size
  geom_line(size = 1.2) +
  
  # Improved ribbon for confidence intervals
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.15, color = NA
  ) +
  
  # Facet by country with better layout
  facet_wrap(~ country, ncol = 3, scales = "free_x") +
  
  # Enhanced axis labels and legend
  labs(
    x = "Log of Ukraine Visits",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    #title = "Predicted Probabilities by Response Level and Country",
    #subtitle = "Exploring the association with ukraine visits"
  ) +
  
  # Customize colors for better readability
  scale_color_brewer(palette = "Dark2", name = "Response Level") +
  scale_fill_brewer(palette = "Dark2", name = "Response Level") +
  
  # Add a horizontal reference line at 0.5 for context
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Customize the theme for a clean, professional look
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    strip.text = element_text(size = 14, face = "bold"), # Facet titles
    axis.title = element_text(size = 14), # Axis titles
    axis.text = element_text(size = 12), # Axis text
    legend.position = "top", # Legend placement
    legend.title = element_text(size = 14), # Legend title
    legend.text = element_text(size = 12), # Legend text
    plot.title = element_text(size = 16, face = "bold"), # Title
    plot.subtitle = element_text(size = 14, face = "italic"), # Subtitle
    panel.grid.major = element_line(color = "gray80"), # Grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

dev.off()



# ================================================
# Model 6 
# ===============================================
# Model 6 : Ukraine minutes
# Expand the grid for all countries 
new_data <- expand.grid (
  log_political_url_ukraine_times = seq(min(fdata$log_political_url_ukraine_times, na.rm =T), max(fdata$log_political_url_ukraine_times, na.rm =T), length =100),
  country = unique(fdata$country),
  log_social_media_visits = mean(fdata$log_social_media_visits, na.rm = T),
  female_fc = factor(1, levels = levels(fdata$female_fc)),
  age = mean(fdata$age),
  educ_rec_nor = mean(fdata$educ_rec_nor, na.rm =T),
  income_nor = mean(fdata$income_nor, na.rm =T),
  q12_ideology_nor = mean(fdata$q12_ideology_nor, na.rm =T),
  pol_int_nor = mean(fdata$pol_int_nor, na.rtm =T),
  TV_rec_nor = mean(fdata$TV_rec_nor, na.rm =T),
  Newspapers_rec_nor = mean(fdata$Newspapers_rec_nor, na.rm =T),
  Wsites_rec_nor = mean(fdata$Wsites_rec_nor, na.rm =T),
  Smedia_rec_nor = mean(fdata$Smedia_rec_nor, na.rm =T),
  p_know_nor = mean(fdata$p_know_nor, na.rm =T)
)

# Generate a grid of predictor values
predicted_probs <- posterior_epred (bmodel6, newdata = new_data, re_formula = NULL)

# Summarize probabilities for each response level
mean_probs <- apply(predicted_probs, c(2,3), mean)
ci_lower <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.025))
ci_upper <- apply(predicted_probs, c(2,3), function(x) quantile(x, probs = 0.975))

# As data frame
mean_probs_df <- as.data.frame(mean_probs) 
colnames(mean_probs_df) = c("0 correct answers", "1 correct answer", "2 correct answers", "3 correct answers")
ci_lower_df <- as.data.frame(ci_lower)
colnames(ci_lower_df) = c("0 correct answers", "1 correct answer", "2 correct answers", "3 correct answers")
ci_upper_df <- as.data.frame(ci_upper)
colnames(ci_upper_df) = c("0 correct answers", "1 correct answer", "2 correct answers", "3 correct answers")

# Add observation and response level identifiers
mean_probs_df$observation <- rep(1:nrow(mean_probs_df))
ci_lower_df$observation <- rep(1:nrow(ci_lower_df))
ci_upper_df$observation <- rep(1:nrow(ci_upper_df)) 

# Reshape to long format
prob_summary_long <- mean_probs_df %>%
  pivot_longer(-observation, names_to = "response_level", values_to = "probability") %>%
  left_join(ci_lower_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "lower"),
            by = c("observation", "response_level")) %>%
  left_join(ci_upper_df %>%
              pivot_longer(-observation, names_to = "response_level", values_to = "upper"),
            by = c("observation", "response_level"))

# Merge with predictor and country information
prob_summary_long <- prob_summary_long %>%
  mutate(
    log_political_url_ukraine_times = rep(new_data$log_political_url_ukraine_times, each = ncol(predicted_probs[1, , ])),
    country = rep(new_data$country, each = ncol(predicted_probs[1, , ]))
  )


# Visualize 
ggplot(prob_summary_long, aes(x = log_political_url_ukraine_times, y = probability, color = as.factor(response_level))) +
  geom_line (size = 1) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.2, color = NA
  ) +
  facet_wrap ( ~ country) + 
  labs(
    x = "Predictor",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    title = "Predicted Probabilities by Response Level"
  ) +
  theme_minimal()

# Enhanced plot
png("marginal_effects_uktimes.png", width = 4000, height = 2000, res = 300)  # High resolution
ggplot(prob_summary_long, aes(x = log_political_url_ukraine_times, y = probability, color = as.factor(response_level))) +
  # Use distinct line style and size
  geom_line(size = 1.2) +
  
  # Improved ribbon for confidence intervals
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = as.factor(response_level)),
    alpha = 0.15, color = NA
  ) +
  
  # Facet by country with better layout
  facet_wrap(~ country, ncol = 3, scales = "free_x") +
  
  # Enhanced axis labels and legend
  labs(
    x = "Log of Ukraine Minutes",
    y = "Predicted Probability",
    color = "Response Level",
    fill = "Response Level",
    title = "",
    subtitle = ""
  ) +
  
  # Customize colors for better readability
  scale_color_brewer(palette = "Dark2", name = "Response Level") +
  scale_fill_brewer(palette = "Dark2", name = "Response Level") +
  
  # Add a horizontal reference line at 0.5 for context
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Customize the theme for a clean, professional look
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    strip.text = element_text(size = 14, face = "bold"), # Facet titles
    axis.title = element_text(size = 14), # Axis titles
    axis.text = element_text(size = 12), # Axis text
    legend.position = "top", # Legend placement
    legend.title = element_text(size = 14), # Legend title
    legend.text = element_text(size = 12), # Legend text
    plot.title = element_text(size = 16, face = "bold"), # Title
    plot.subtitle = element_text(size = 14, face = "italic"), # Subtitle
    panel.grid.major = element_line(color = "gray80"), # Grid lines
    panel.grid.minor = element_blank() # Remove minor grid lines
  )

dev.off()
#ggsave("marginal_effects_uktimes.png", dpi = 300, width = 10, height = 7)

# #################################################################################################################
#                                         EXTENSIONS AND ROBUSTNESS CHECKS               
###################################################################################################################
# Run an ordinal logit 
# First transform the DV as an ordered factor
fdata$s_know_nor_fc <- as.ordered(fdata$s_know_nor)
fdata$female_fc <- as.factor(fdata$female)

olog_eq1 <- s_know_nor_fc ~ 1 + log_news_visits + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_news_visits | country)
olmodel1 <- clmm (olog_eq1, data = fdata)
summary(olmodel1)

olog_eq2 <- s_know_nor_fc ~ 1 + log_news_minutes + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_news_minutes | country)
olmodel2 <- clmm (olog_eq2, data = fdata)
summary(olmodel2)

olog_eq3 <- s_know_nor_fc ~ 1 + log_political_url_visits + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_political_url_visits | country)
olmodel3 <- clmm (olog_eq3, data = fdata)
summary(olmodel3)

olog_eq4 <- s_know_nor_fc ~ 1 + log_political_url_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1  + log_political_url_times | country)
olmodel4 <- clmm (olog_eq4, data = fdata)
summary(olmodel4)

olog_eq5 <- s_know_nor_fc ~ 1 + log_political_url_ukraine + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1 + log_political_url_ukraine | country)
olmodel5 <- clmm (olog_eq5, data = fdata)
summary(olmodel5)

olog_eq6 <- s_know_nor_fc ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female_fc + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
  TV_rec_nor + Newspapers_rec_nor + Wsites_rec_nor + Smedia_rec_nor + p_know_nor + (1  + log_political_url_ukraine_times | country)
olmodel6 <- clmm (olog_eq6, data = fdata)
summary(olmodel6)

# ================================= PLOT MARGINAL EFFECTS==============================
# Extract random effects for countries
random_effects <- ranef(olmodel1)$country
head(random_effects)

# Extract fixed effects
fixed_effects <- coef(olmodel1)
print(fixed_effects)

# Extract thresholds
thresholds <- olmodel1$Theta
print(thresholds)

# Example: Generate new data for prediction
new_data <- data.frame(
  log_news_visits = seq(min(fdata$log_news_visits, na.rm = TRUE), max(fdata$log_news_visits, na.rm = TRUE), length.out =100),
  log_social_media_visits = mean(fdata$log_social_media_visits, na.rm = T),
  female = factor(1, levels = levels(fdata$female_fc)),
  age = mean(fdata$age),
  educ_rec_nor = mean(fdata$educ_rec_nor, na.rm =T),
  income_nor = mean(fdata$income_nor, na.rm =T),
  q12_ideology_nor = mean(fdata$q12_ideology_nor, na.rm =T),
  pol_int_nor = mean(fdata$pol_int_nor, na.rtm =T),
  TV_rec_nor = mean(fdata$TV_rec_nor, na.rm =T),
  Newspapers_rec_nor = mean(fdata$Newspapers_rec_nor, na.rm =T),
  p_know_nor = mean(fdata$p_know_nor, na.rm =T))

# Create linear predictor using fixed and random effects
#new_data$linear_predictor <- with(new_data, 
#                                    log_news_visits * fixed_effects["log_news_visits"] +
#                                    log_social_media_visits * fixed_effects["log_social_media_visits"] +
#                                    as.numeric(female) * fixed_effects["female_fc1"] +
#                                    age * fixed_effects["age"] +
#                                    educ_rec_nor * fixed_effects ["educ_rec_nor"] +
#                                    income_nor * fixed_effects ["income_nor"] +
#                                    q12_ideology_nor * fixed_effects ["q12_ideology_nor"] +
#                                    pol_int_nor * fixed_effects ["pol_int_nor"] +
#                                    TV_rec_nor * fixed_effects ["TV_rec_nor"] +
#                                    Newspapers_rec_nor * fixed_effects ["Newspapers_rec_nor"] +
#                                    p_know_nor * fixed_effects ["p_know_nor"] + 
#                                    random_effects["US", "(Intercept)"]  # Random effect for the specified country
#                                  )

results <- lapply(selected_countries, function(country) {
  # Extract random effect for the country
  country_effect <- random_effects[country, "(Intercept)"]
  
  # Compute the linear predictor for this country
  new_data$linear_predictor <- with(new_data, 
                                    log_news_visits * fixed_effects["log_news_visits"] +
                                      log_social_media_visits * fixed_effects["log_social_media_visits"] +
                                      as.numeric(female) * fixed_effects["female_fc1"] +
                                      age * fixed_effects["age"] +
                                      educ_rec_nor * fixed_effects["educ_rec_nor"] +
                                      income_nor * fixed_effects["income_nor"] +
                                      q12_ideology_nor * fixed_effects["q12_ideology_nor"] +
                                      pol_int_nor * fixed_effects["pol_int_nor"] +
                                      TV_rec_nor * fixed_effects["TV_rec_nor"] +
                                      Newspapers_rec_nor * fixed_effects["Newspapers_rec_nor"] +
                                      p_know_nor * fixed_effects["p_know_nor"] +
                                      country_effect  # Add random effect for the country
  )
  
  # Compute cumulative probabilities
  cum_probs <- sapply(thresholds, function(thresh) {
    plogis(thresh - new_data$linear_predictor)
  })
  
  # Ensure dimensions are correct
  category_differences <- t(apply(cum_probs, 1, diff))
  
  # Combine into category probabilities
  category_probs <- as.data.frame(cbind(
    prob_1 = cum_probs[, 1],                 # First category
    category_differences,                    # Intermediate categories
    prob_last = 1 - cum_probs[, ncol(cum_probs)]  # Last category
  ))
  
  # Add probabilities and country information
  new_data_with_probs <- cbind(new_data, category_probs)
  new_data_with_probs$country <- country  # Add country label
  
  return(new_data_with_probs)
})


results_combined <- do.call(rbind, results)
head(results_combined)

# Make sure the columns have unique names
colnames(results_combined)[14:15] <- c("prob_2", "prob_3") 

#####
# Reshape data for plotting
plot_data <- results_combined %>%
  pivot_longer(cols = starts_with("prob_"), names_to = "category", values_to = "probability")

# Plot probabilities by country and category
ggplot(plot_data, aes(x = log_news_visits, y = probability, color = category)) +
  geom_line() +
  facet_wrap(~ country) +
  labs(
    title = "Predicted Probabilities by Country and Category",
    x = "Log News Visits",
    y = "Probability"
  ) +
  theme_minimal()

# Enhanced Plot
plot_nwvisits <- ggplot(plot_data, aes(x = log_news_visits, y = probability, color = category, linetype = category)) +
  geom_line(size = 1.2) +                                      # Thicker lines
  facet_wrap(~ country, scales = "free_y") +                   # Separate facets for each country
  scale_color_manual(
    values = c("darkblue", "darkgreen", "darkred", "purple"),  # Color-blind friendly palette
    labels = c("Category 1", "Category 2", "Category 3", "Category 4")
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),        # Differentiating line types
    labels = c("Category 1", "Category 2", "Category 3", "Category 4")
  ) +
  labs(
    title = "Predicted Probabilities by Country and Response Category",
    x = "Logarithm of News Visits",
    y = "Predicted Probability",
    color = "Response Category",   # Improved legend title
    linetype = "Response Category" # Line type legend
  ) +
  theme_minimal(base_size = 14) +                              # Adjust font size for publication
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),     # Center and bold title
    strip.text = element_text(face = "bold", size = 12),       # Bold facet labels (countries)
    legend.position = "right",                                 # Place legend to the right
    legend.key.width = unit(1.5, "cm"),                        # Widen legend keys for readability
    legend.key.height = unit(0.5, "cm")                        # Adjust legend key height
  )


# ==================== ITERATE ===================================

# Define models
models <- list(
  olmodel1,  # Model 1 with key predictor "log_news_visits"
  olmodel2,  # Model 2 with key predictor "log_news_visits_minutes"
  olmodel3,
  olmodel4,
  olmodel5,
  olmodel6     # Model 6 with key predictor "Ukraine minutes"
)

# Define the key predictors for each model
key_predictors <- c("log_news_visits", "log_news_minutes", "log_political_url_visits",
                    "log_political_url_times", "log_political_url_ukraine",
                    "log_political_url_ukraine_times")


model_results <- list()

for (i in seq_along(models)) {
  # Extract the current model and key predictor
  model <- models[[i]]
  key_predictor <- key_predictors[i]
  
  # Extract fixed and random effects
  fixed_effects <- coef(model)
  random_effects <- ranef(model)$country
  
  # Dynamically create new_data for this model
  new_data <- data.frame(
    key_predictor_values = seq(min(fdata[[key_predictor]], na.rm = TRUE), 
                               max(fdata[[key_predictor]], na.rm = TRUE), 
                               length.out = 100),
    female = factor(1, levels = levels(fdata$female_fc)),
    age = mean(fdata$age, na.rm = TRUE),
    educ_rec_nor = mean(fdata$educ_rec_nor, na.rm = TRUE),
    income_nor = mean(fdata$income_nor, na.rm = TRUE),
    q12_ideology_nor = mean(fdata$q12_ideology_nor, na.rm = TRUE),
    pol_int_nor = mean(fdata$pol_int_nor, na.rm = TRUE),
    Newspapers_rec_nor = mean(fdata$Newspapers_rec_nor, na.rm = TRUE),
    p_know_nor = mean(fdata$p_know_nor, na.rm = TRUE)
  )
  
  # Rename the dynamic column to match the key predictor name
  colnames(new_data)[1] <- key_predictor
  
  # Perform analysis for each country
  results <- lapply(selected_countries, function(country) {
    country_effect <- random_effects[country, "(Intercept)"]
    
    # Compute the linear predictor for this country
    new_data$linear_predictor <- with(new_data, 
                                      new_data[[key_predictor]] * fixed_effects[key_predictor] +
                                        as.numeric(female) * fixed_effects["female_fc1"] +
                                        age * fixed_effects["age"] +
                                        educ_rec_nor * fixed_effects["educ_rec_nor"] +
                                        income_nor * fixed_effects["income_nor"] +
                                        q12_ideology_nor * fixed_effects["q12_ideology_nor"] +
                                        pol_int_nor * fixed_effects["pol_int_nor"] +
                                        Newspapers_rec_nor * fixed_effects["Newspapers_rec_nor"] +
                                        p_know_nor * fixed_effects["p_know_nor"] +
                                        country_effect
    )
    
    # Compute cumulative probabilities
    cum_probs <- sapply(thresholds, function(thresh) {
      plogis(thresh - new_data$linear_predictor)
    })
    
    # Convert cumulative probabilities to category probabilities
    category_differences <- t(apply(cum_probs, 1, diff))
    category_probs <- as.data.frame(cbind(
      prob_1 = cum_probs[, 1],
      category_differences,
      prob_last = 1 - cum_probs[, ncol(cum_probs)]
    ))
    
    # Combine new_data with probabilities and country label
    new_data_with_probs <- cbind(new_data, category_probs)
    new_data_with_probs$country <- country
    return(new_data_with_probs)
  })
  
  # Combine results for all countries
  results_combined <- do.call(rbind, results)
  
  # Save results for the current model
  model_results[[i]] <- list(
    model_name = paste("Model", i),
    data = results_combined
  )
}

library(ggplot2)
library(tidyr)

# Iterate through the models and plot results
for (i in seq_along(model_results)) {
  # Extract the results for the current model
  model_name <- model_results[[i]]$model_name
  plot_data <- model_results[[i]]$data
  
  # Rename "V_" columns to "prob_2", "prob_3", etc.
  plot_data <- plot_data %>%
    rename_with(
      .fn = ~ paste0("prob_2", seq_along(.)),  # Dynamically create "prob_2", "prob_3", etc.
      .cols = matches("^V[2-3]$")              # Select columns starting with "V_"
    )
  
  # Make sure the columns have unique names
  colnames(results_combined)[14:15] <- c("prob_2", "prob_3") 
  
  # Reshape data to long format for ggplot
  plot_data_long <- plot_data %>%
    pivot_longer(cols = starts_with("prob_"), names_to = "category", values_to = "probability")
  
  # Create the plot
  p <- ggplot(plot_data_long, aes_string(x = key_predictors[i], y = "probability", color = "category", linetype = "category")) +
    geom_line(size = 1.2) +
    facet_wrap(~ country, scales = "free_y") +
    labs(
      title = paste("Predicted Probabilities for", model_name),
      x = paste("Effect of", key_predictors[i]),
      y = "Predicted Probability",
      color = "Response Category",
      linetype = "Response Category"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "right",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.5, "cm")
    )
  
  # Save the plot as a file
  ggsave(
    filename = paste0("plot_", model_name, ".png"),
    plot = p,
    width = 10, height = 7, dpi = 300
  )
  
  # Print a message
  print(paste("Saved plot for", model_name))
}






# ================================== TESTING FOR COEFFICIENT DIFFERENCES =============
# Run first all the regressions with the pooled dataset 
plmodel1 <- lm (s_know_nor ~ 1 + log_news_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                  TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = fdata)
summary(plmodel1) # coeffcient is almost equal to the multilevel model 


plmodel2 <- lm (s_know_nor ~ 1 + log_news_minutes + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                  TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = fdata)
summary(plmodel2)


plmodel3 <- lm (s_know_nor ~ 1 + log_political_url_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                  TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = fdata)
summary(plmodel3)


plmodel4 <- lm (s_know_nor ~ 1 + log_political_url_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                  TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = fdata)
summary(plmodel4)


plmodel5 <- lm (s_know_nor ~ 1 + log_political_url_ukraine + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                  TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = fdata)
summary(plmodel5)


plmodel6 <- lm (s_know_nor ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                  TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = fdata)
summary(plmodel6) # coeffcients 



# extract coefficients and standard errors from the different models and set them in a unified dataset
df1 = as.data.frame(summary(plmodel1)$coefficients[2, c(1:2)])
df2 = as.data.frame(summary(plmodel2)$coefficients[2, c(1:2)])
df3 = as.data.frame(summary(plmodel3)$coefficients[2, c(1:2)])
df4 = as.data.frame(summary(plmodel4)$coefficients[2, c(1:2)])
df5 = as.data.frame(summary(plmodel5)$coefficients[2, c(1:2)])
df6 = as.data.frame(summary(plmodel6)$coefficients[2, c(1:2)])


# set them in a dataframe 
df1 = data.frame(
  estimate = df1[1,1],
  std.error = df1[2,1],
  row.names = c('log_news_visits')
); df1

df2 = data.frame(
  estimate = df2[1,1],
  std.error = df2[2,1],
  row.names = c('log_news_minutes')
); df2


df3 = data.frame(
  estimate = df3[1,1],
  std.error = df3[2,1],
  row.names = c('log_political_url_visits')
); df3

df4 = data.frame(
  estimate = df4[1,1],
  std.error = df4[2,1],
  row.names = c('log_political_url_minutes')
); df4

df5 = data.frame(
  estimate = df5[1,1],
  std.error = df5[2,1],
  row.names = c('log_ukraine_url_visits')
); df5

df6 = data.frame(
  estimate = df6[1,1],
  std.error = df6[2,1],
  row.names = c('log_ukraine_url_minutes')
); df6


# Bind rows
estimates <- bind_rows(df1, df2, df3, df4, df5, df6)

# Extract coefficients and standard errors from the dataset
coefficients <- estimates$estimate 
standard_errors <- estimates$std.error

# Initialize a matrix to store p-values
p_matrix <- matrix(NA, nrow = 6, ncol = 6)

# Perform pairwise comparisons
for (i in 1:6) {
  for (j in (i+1):6) {
    beta_diff <- coefficients[i] - coefficients[j]
    se_diff <- sqrt(standard_errors[i]^2 + standard_errors[j]^2)
    z <- beta_diff / se_diff
    p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
    
    # Store p-values in the matrix
    p_matrix[i, j] <- p_value
    p_matrix[j, i] <- p_value
  }
}

# Print results
print(p_matrix)

# round 
p_matrix <- round(p_matrix, 2)

# write p-values in a csv file
write.csv(p_matrix, "Outputs/pairwise_coef_comparisons.csv", row.names = F)



# ================================= COUNTRY ANALYSIS ===============================================================
# Plot (country) effects from multilevel mixed models  
mvm.log.nv <- plot_model (lmodel1, type="pred", 
                          terms=c("log_news_visits","country"), pred.type="re", ci.lvl = NA) + 
  labs (x = "News visits (log)", y = "Political knowledge (%)", title = "RE in Reported Models") + theme_bw ()

mvm.log.pv <- plot_model (lmodel3, type="pred", 
                          terms=c("log_political_url_visits","country"), pred.type="re", ci.lvl = NA) + 
  labs (x = "Political visits (log)", y = "Political knowledge (%)", title = "") + theme_bw ()


mvm.log.uknv <- plot_model (lmodel5, type = "pred",
                            terms=c("log_political_url_ukraine","country"), pred.type="re", ci.lvl = NA) + 
  labs (x = "Ukraine visits (log)", y = "Political knowledge (%)", title = "") + theme_bw ()


# arrange effects for multilevel models
grid.arrange(mvm.log.nv, mvm.log.pv, mvm.log.uknv)



# Plot (country) effects for models in original scale
mvm.nv <- plot_model (model1, type="pred", 
                      terms=c("news_visits","country"), pred.type="re", ci.lvl = NA) + 
  labs (x = "News visits", y = "Political knowledge (%)", title = "RE in Models with Original Scales") + theme_bw ()

mvm.pv <- plot_model (model3, type="pred", 
                      terms=c("political_url_visits","country"), pred.type="re", ci.lvl = NA) + 
  labs (x = "Political visits", y = "Political knowledge (%)", title = "") + theme_bw ()


mvm.uknv <- plot_model (model5, type = "pred",
                        terms=c("political_url_ukraine","country"), pred.type="re", ci.lvl = NA) + 
  labs (x = "Ukraine visits", y = "Political knowledge (%)", title = "") + theme_bw ()


# arrange effects for multilevel models
grid.arrange(mvm.nv, mvm.pv, mvm.uknv)

# All 
grid.arrange(mvm.log.nv, mvm.nv, mvm.log.pv, mvm.pv, mvm.log.uknv, mvm.uknv)

# Run models separately for countries 
# Germany

DEfdata <- fdata %>%
  filter (country == "DE")

DElmodel1 <- lm (s_know_nor ~ 1 + log_news_visits +  log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = DEfdata)
summary(DElmodel1) # coeffcient is almost equal to the multilevel model 


DElmodel2 <- lm (s_know_nor ~ 1 + log_news_minutes + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = DEfdata)
summary(DElmodel2)


DElmodel3 <- lm (s_know_nor ~ 1 + log_political_url_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = DEfdata)
summary(DElmodel3)


DElmodel4 <- lm (s_know_nor ~ 1 + log_political_url_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = DEfdata)
summary(DElmodel4)


DElmodel5 <- lm (s_know_nor ~ 1 + log_political_url_ukraine + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = DEfdata)
summary(DElmodel5)


DElmodel6 <- lm (s_know_nor ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = DEfdata)
summary(DElmodel6) # coefficients 

# Spain
ESfdata <- fdata %>%
  filter (country == "ES")

ESlmodel1 <- lm (s_know_nor ~ 1 + log_news_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = ESfdata)
summary(ESlmodel1) # coeffcient is almost equal to the multilevel model 


ESlmodel2 <- lm (s_know_nor ~ 1 + log_news_minutes + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = ESfdata)
summary(ESlmodel2)


ESlmodel3 <- lm (s_know_nor ~ 1 + log_political_url_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = ESfdata)
summary(ESlmodel3)


ESlmodel4 <- lm (s_know_nor ~ 1 + log_political_url_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = ESfdata)
summary(ESlmodel4)


ESlmodel5 <- lm (s_know_nor ~ 1 + log_political_url_ukraine + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = ESfdata)
summary(ESlmodel5)


ESlmodel6 <- lm (s_know_nor ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = ESfdata)
summary(ESlmodel6) # coefficients 


# US
USfdata <- fdata %>%
  filter (country == "US")

USlmodel1 <- lm (s_know_nor ~ 1 + log_news_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = USfdata)
summary(USlmodel1) # coeffcient is almost equal to the multilevel model 


USlmodel2 <- lm (s_know_nor ~ 1 + log_news_minutes + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = USfdata)
summary(USlmodel2)


USlmodel3 <- lm (s_know_nor ~ 1 + log_political_url_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = USfdata)
summary(USlmodel3)


USlmodel4 <- lm (s_know_nor ~ 1 + log_political_url_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = USfdata)
summary(USlmodel4)


USlmodel5 <- lm (s_know_nor ~ 1 + log_political_url_ukraine + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = USfdata)
summary(USlmodel5)


USlmodel6 <- lm (s_know_nor ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = USfdata)
summary(USlmodel6) 


# UK
UKfdata <- fdata %>%
  filter (country == "UK")

UKlmodel1 <- lm (s_know_nor ~ 1 + log_news_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = UKfdata)
summary(UKlmodel1) # coeffcient is almost equal to the multilevel model 


UKlmodel2 <- lm (s_know_nor ~ 1 + log_news_minutes + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = UKfdata)
summary(UKlmodel2)


UKlmodel3 <- lm (s_know_nor ~ 1 + log_political_url_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = UKfdata)
summary(UKlmodel3)


UKlmodel4 <- lm (s_know_nor ~ 1 + log_political_url_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = UKfdata)
summary(UKlmodel4)


UKlmodel5 <- lm (s_know_nor ~ 1 + log_political_url_ukraine + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = UKfdata)
summary(UKlmodel5)


UKlmodel6 <- lm (s_know_nor ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = UKfdata)
summary(UKlmodel6) 



# FR
FRfdata <- fdata %>%
  filter (country == "FR")

FRlmodel1 <- lm (s_know_nor ~ 1 + log_news_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = FRfdata)
summary(FRlmodel1) # coeffcient is almost equal to the multilevel model 


FRlmodel2 <- lm (s_know_nor ~ 1 + log_news_minutes + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = FRfdata)
summary(FRlmodel2)


FRlmodel3 <- lm (s_know_nor ~ 1 + log_political_url_visits + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = FRfdata)
summary(FRlmodel3)


FRlmodel4 <- lm (s_know_nor ~ 1 + log_political_url_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = FRfdata)
summary(FRlmodel4)


FRlmodel5 <- lm (s_know_nor ~ 1 + log_political_url_ukraine + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = FRfdata)
summary(FRlmodel5)


FRlmodel6 <- lm (s_know_nor ~ 1 + log_political_url_ukraine_times + log_social_media_visits + female + age + educ_rec_nor + income_nor + q12_ideology_nor + pol_int_nor + 
                   TV_rec_nor + Newspapers_rec_nor + p_know_nor, data = FRfdata)
summary(FRlmodel6) 


# Tables for countries 
# Germany
stargazer(DElmodel1, DElmodel2, DElmodel3, DElmodel4, DElmodel5, DElmodel6, type="html", 
          dep.var.labels=c("Surveillence Knowledge (normalized)"),  
          #column.labels=c('Model1','Model2', 'Model3', 'Model4'),
          covariate.labels= c("News visits", 
                              "News minutes", 
                              "Political visits",
                              "Political minutes",
                              "Russia-Ukraine visits",
                              "Russia-Ukraine minutes",
                              "Social Media Visits",
                              "Female",
                              "Age",
                              "Education",
                              "Income",
                              "Ideology",
                              "Political Interest",
                              "TV Freq.", 
                              "Newspapers Freq.", 
                              "Political Knowledge"))

# Spain
stargazer(ESlmodel1, ESlmodel2, ESlmodel3, ESlmodel4, ESlmodel5, ESlmodel6, type="html", 
          dep.var.labels=c("Surveillence Knowledge (normalized)"),  
          #column.labels=c('Model1','Model2', 'Model3', 'Model4'),
          covariate.labels= c("News visits", 
                              "News minutes", 
                              "Political visits",
                              "Political minutes",
                              "Russia-Ukraine visits",
                              "Russia-Ukraine minutes",
                              "Social Media Visits",
                              "Female",
                              "Age",
                              "Education",
                              "Income",
                              "Ideology",
                              "Political Interest",
                              "TV Freq.", 
                              "Newspapers Freq.", 
                              "Political Knowledge"))

# US
stargazer(USlmodel1, USlmodel2, USlmodel3, USlmodel4, USlmodel5, USlmodel6, type="html", 
          dep.var.labels=c("Surveillence Knowledge (normalized)"),  
          #column.labels=c('Model1','Model2', 'Model3', 'Model4'),
          covariate.labels= c("News visits", 
                              "News minutes", 
                              "Political visits",
                              "Political minutes",
                              "Russia-Ukraine visits",
                              "Russia-Ukraine minutes",
                              "Social Media Visits",
                              "Female",
                              "Age",
                              "Education",
                              "Income",
                              "Ideology",
                              "Political Interest",
                              "TV Freq.", 
                              "Newspapers Freq.", 
                              "Political Knowledge"))


# UK
stargazer(UKlmodel1, UKlmodel2, UKlmodel3, UKlmodel4, UKlmodel5, UKlmodel6, type="html", 
          dep.var.labels=c("Surveillence Knowledge (normalized)"),  
          #column.labels=c('Model1','Model2', 'Model3', 'Model4'),
          covariate.labels= c("News visits", 
                              "News minutes", 
                              "Political visits",
                              "Political minutes",
                              "Russia-Ukraine visits",
                              "Russia-Ukraine minutes",
                              "Social Media Visits",
                              "Female",
                              "Age",
                              "Education",
                              "Income",
                              "Ideology",
                              "Political Interest",
                              "TV Freq.", 
                              "Newspapers Freq.", 
                              "Political Knowledge"))


# FR
stargazer(FRlmodel1, FRlmodel2, FRlmodel3, FRlmodel4, FRlmodel5, FRlmodel6, type="html", 
          dep.var.labels=c("Surveillence Knowledge (normalized)"),  
          #column.labels=c('Model1','Model2', 'Model3', 'Model4'),
          covariate.labels= c("News visits", 
                              "News minutes", 
                              "Political visits",
                              "Political minutes",
                              "Russia-Ukraine visits",
                              "Russia-Ukraine minutes",
                              "Social Media Visits",
                              "Female",
                              "Age",
                              "Education",
                              "Income",
                              "Ideology",
                              "Political Interest",
                              "TV Freq.", 
                              "Newspapers Freq.", 
                              "Political Knowledge"))

