#================
# POQ Revision
# Date: 15/12/24
#===============
# Sample composition
#===============

# clean
rm(list = ls())  

library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(data.table)
library(sjPlot)
library(stargazer)
library(descr)
library(ggpubr)
library(foreign)
library(ggeffects)
library(magrittr)
library(broom)
library(broom.mixed)


# Call fdata to get the final sample
load("Data/fdata")


# ===========================================
# 1) Variable stats for total sample 
# ==========================================
# Create a function 
summary_stats <- function(x) {
  tibble(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

# select variables 
var_selection <- fdata %>%
  select (news_visits, news_minutes, political_url_visits, political_url_times, political_url_ukraine, political_url_ukraine_times,
          social_media_visits, age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
          female, income, TV_rec, Newspapers_rec, Wsites_rec, Smedia_rec) 


# Apply function to selected variables 
summary_table <- var_selection %>%
  summarise(across(everything(), ~ summary_stats(.))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
  unnest(cols = c(stats))  

# ==============================================================
# 2) Compare web and non web samples and view sample sizes by countries 
# =============================================================
# survey sample for which there is no web data
survey_sample_no_web_data <- fdata %>%
  filter (is.na(news_visits)) 

# survey sample for which there is web data 
survey_sample_web_data <- fdata %>%
  filter (!is.na(news_visits))

# Get sample size by country 
survey_sample_no_web_data %>%
  group_by (country) %>%
  summarise(n())

# Get sample size by country
survey_sample_web_data %>%
  group_by (country) %>%
  summarise(n())

# Variable stats (only for survey data) for web sample 
var_selection_web <- survey_sample_web_data %>%
  select (age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
          female, income, TV_rec, Newspapers_rec, Wsites_rec, Smedia_rec)   

# Apply function to selected variables 
summary_table_web <- var_selection_web %>%
  summarise(across(everything(), ~ summary_stats(.))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
  unnest(cols = c(stats))  

# Variable stats (only for survey data) for non-web sample 
var_selection_noweb <- survey_sample_no_web_data %>%
  select (age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
          female, income, TV_rec, Newspapers_rec, Wsites_rec, Smedia_rec)  

# Apply function to selected variables 
summary_table_noweb <- var_selection_noweb %>%
  summarise(across(everything(), ~ summary_stats(.))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
  unnest(cols = c(stats))  

# =============================================
# 3) Country means 
# =============================================
# Apply function to selected variables 
var_selection$country <- fdata$country

summary_mean <- aggregate(. ~ country, var_selection, mean, na.rm = T )
summary_sd <- aggregate(. ~ country, var_selection, sd, na.rm =T)
summary_min <- aggregate(. ~ country, var_selection, min, na.rm =T)
summary_max <- aggregate(. ~ country, var_selection, max, na.rm =T)

# ==================================================
# 4) Check sample composition by education, age and visits --Are samples comparable between countries
# =================================================
# Survey sample for which there is web data 
survey_sample_web_data <- fdata %>%
  filter (!is.na(news_visits))

# Get sample size by country
survey_sample_web_data %>%
  group_by (country) %>%
  summarise(n())

 
#  Stats for Final sample 
# Create a function 
summary_stats <- function(x) {
  tibble(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

# Variable stats (only for survey data) for web sample 
var_selection_web <- survey_sample_web_data %>%
  select (age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
          female, income, TV_rec, Newspapers_rec)   

# Apply function to selected variables 
summary_table_web <- var_selection_web %>%
  summarise(across(everything(), ~ summary_stats(.))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
  unnest(cols = c(stats))  

# Compare country samples by age and education
# Cut age in meanful ranges 

survey_sample_web_data %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(18, 35, 50, 65, 80, Inf),  # Define age ranges
      labels = c("Young (18-35)", 
                 "Adult (36-50)", 
                 "Middle-Aged (51-65)", 
                 "Senior (66-80)", 
                 "Elderly (80+)"),
      right = TRUE  # Include the upper limit in each range
    )
  ) %>%
  group_by(country, age_group) %>%                 # Group by Country and Age Group
  summarize(count = n(), .groups = "drop") %>%     # Count number of people in each group
  group_by(country) %>%                            # Group by Country to calculate percentages
  mutate(percentage = (count / sum(count)) * 100) %>% 
  pivot_wider(names_from = age_group,
                       values_from = c(count, percentage)) -> result 


print(result)

write.csv(result, "age_groups.csv")

# The same for education 
survey_sample_web_data %>% 
  group_by(country, educ_rec) %>%                 # Group by Country and Education level 
  summarize(count = n(), .groups = "drop") %>%     # Count number of people in each group
  group_by(country) %>%                            # Group by Country to calculate percentages
  mutate(percentage = (count / sum(count)) * 100) %>% 
  pivot_wider(names_from = educ_rec,
              values_from = c(count, percentage)) -> educ_result 
  

write.csv(educ_result, "education_groups.csv")


# The same for gender
survey_sample_web_data %>% 
  group_by(country, female) %>%                 # Group by Country and Education level 
  summarize(count = n(), .groups = "drop") %>%     # Count number of people in each group
  group_by(country) %>%                            # Group by Country to calculate percentages
  mutate(percentage = (count / sum(count)) * 100) %>% 
  pivot_wider(names_from = female,
              values_from = c(count, percentage)) -> female_result 




# ==============================================================
# Compare web and non web samples and view sample sizes by countries 
# =============================================================
# survey sample for which there is no web data
#survey_sample_no_web_data <- fdata %>%
#  filter (is.na(news_visits)) 

# survey sample for which there is web data 
#survey_sample_web_data <- fdata %>%
#  filter (!is.na(news_visits))

# Get sample size by country 
#survey_sample_no_web_data %>%
#  group_by (country) %>%
#  summarise(n())

# Get sample size by country
#survey_sample_web_data %>%
#  group_by (country) %>%
#  summarise(n())


# 1) Variable stats for total sample 
# Create a function 
#summary_stats <- function(x) {
#  tibble(
#    mean = mean(x, na.rm = TRUE),
#    sd = sd(x, na.rm = TRUE),
#    min = min(x, na.rm = TRUE),
#    max = max(x, na.rm = TRUE)
#  )
#}

# select variables 
#var_selection <- fdata %>%
#  select (news_visits, news_minutes, political_url_visits, political_url_times, political_url_ukraine, political_url_ukraine_times,
#          social_media_visits, age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
#          female, income, TV_rec, Newspapers_rec) 


# Apply function to selected variables 
#summary_table <- var_selection %>%
#  summarise(across(everything(), ~ summary_stats(.))) %>%
#  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
#  unnest(cols = c(stats))  


# Variable stats (only for survey data) for web sample 
#var_selection_web <- survey_sample_web_data %>%
#  select (age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
#          female, income, TV_rec, Newspapers_rec)   


# Apply function to selected variables 
#summary_table_web <- var_selection_web %>%
#  summarise(across(everything(), ~ summary_stats(.))) %>%
#  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
#  unnest(cols = c(stats))  


# Variable stats (only for survey data) for non-web sample 
#var_selection_noweb <- survey_sample_no_web_data %>%
#  select (age, educ_rec, q12_ideology, polknow, survknow, s_know_nor, pol_int,
#          female, income, TV_rec, Newspapers_rec)  


# Apply function to selected variables 
#summary_table_noweb <- var_selection_noweb %>%
#  summarise(across(everything(), ~ summary_stats(.))) %>%
#  pivot_longer(cols = everything(), names_to = "variable", values_to = "stats") %>%
#  unnest(cols = c(stats))  


# Country samples 
# Apply function to selected variables 
#var_selection$country <- fdata$country

#summary_mean <- aggregate(. ~ country, var_selection, mean, na.rm = T )
#summary_sd <- aggregate(. ~ country, var_selection, sd, na.rm =T)
#summary_min <- aggregate(. ~ country, var_selection, min, na.rm =T)
#summary_max <- aggregate(. ~ country, var_selection, max, na.rm =T)


# ================================================
# Apply weights 
# =================================================

# Example target distributions for age, education, and gender by country
# Replace these with the actual target proportions

# Example target proportions by country (original)
target_distributions <- data.frame(
  country = c("FR", "DE", "ES", "US", "UK"),
  age_0_14 = c(0.17, 0.14, 0.13, 0.18, 0.18),    # Proportion for 0-14 years
  age_15_24 = c(0.12, 0.10, 0.10, 0.13, 0.12),   # Proportion for 15-24 years
  age_25_64 = c(0.49, 0.54, 0.56, 0.52, 0.52),  # Proportion for 25-64 years  
  age_66_plus = c(0.21, 0.22, 0.20, 0.17, 0.19)      # Proportion for 66+ years
)

# Remove the missing age group (e.g., age_0_14)
adjusted_distributions <- target_distributions %>%
  select(-age_0_14) %>%  # Exclude the missing group
  mutate(total_remaining = rowSums(select(., -country)),  # Calculate total remaining proportions
         missing_proportion = c (0.17, 0.14, 0.13, 0.18, 0.18)) %>%  # Proportion to redistribute
  mutate(across(starts_with("age"), ~ . + (. / total_remaining) * missing_proportion)) %>%  # Redistribute proportion
  rowwise() %>%  # Ensure operations are performed row by row
  mutate(across(starts_with("age"), ~ . / sum(c_across(starts_with("age"))))) %>%  # Normalize within each row
  ungroup() %>%  # Ungroup to return to a regular dataframe
  select(-total_remaining, -missing_proportion)

# View adjusted proportions
print(adjusted_distributions)

target_distributions <- list (
  age = data.frame (
    country = c("FR", "DE", "ES", "US", "UK"),
    age_15_24 = c(0.15, 0.12, 0.12, 0.16, 0.14),   # Proportion for 15-24 years
    age_25_64 = c(0.60, 0.63, 0.65, 0.63, 0.63),  # Proportion for 25-64 years
    age_66_plus = c(0.26, 0.26, 0.23, 0.21, 0.23)      # Proportion for 65+ years
  ),
  education = data.frame (
    country = c("FR", "DE", "ES", "US", "UK"),
    proportion_nocollege = c(0.58, 0.67, 0.59, 0.50, 0.49),
    proportion_college = c(0.42, 0.33, 0.41, 0.50, 0.51)  # Proportions of "High School" by country
  ),
  gender = data.frame (
    country = c("FR", "DE", "ES", "US", "UK"),
    proportion_male = c(0.49, 0.49, 0.49, 0.49, 0.49),
    proportion_female = c(0.51, 0.51, 0.51, 0.51, 0.51)  # Proportions of "Male" by country
  )
)

# Calculate weights for age
age_weights <- fdata %>%
  mutate (age_group = case_when(
    age >= 15 & age <= 24 ~ "15_24",
    age >= 25 & age <= 64 ~ "25_64",
    age >= 65 ~ "66_plus"
  )) %>%
  group_by (country, age_group) %>%
  summarize (age_group_count = n()) %>%
  left_join(target_distributions$age %>%
              pivot_longer(-country, names_to = "age_group", values_to = "target_proportion") %>%
              mutate(age_group = gsub("age_", "", age_group)), 
            by = c("country", "age_group")) %>%
  mutate(age_group_weight = target_proportion / (age_group_count / sum(age_group_count))) %>%
  select(country, age_group, age_group_weight)


# Calculate weights for education
education_weights <- fdata %>%
  filter (!is.na(college)) %>%
  mutate (education = factor (college, levels = c("0","1"), labels = c("nocollege", "college"))) %>%
  group_by(country, education) %>%
  summarize(education_count = n()) %>%
  left_join(target_distributions$education %>%
              pivot_longer(-country, names_to = "education", values_to = "target_proportion") %>%
              mutate(education = gsub("proportion_", "", education)), 
            by = c("country", "education")) %>%
  mutate(education_weight = target_proportion / (education_count / sum(education_count))) %>%
  select(country, education, education_weight)


# Calculate weights for gender
gender_weights <- fdata %>%
  mutate (gender = factor (female, levels = c("0","1"), labels = c("male", "female"))) %>%
  group_by(country, gender) %>%
  summarize(gender_count = n()) %>%
  left_join(target_distributions$gender %>%
              pivot_longer(-country, names_to = "gender", values_to = "target_proportion") %>%
              mutate(gender = gsub("proportion_", "", gender)),
            by = c("country", "gender")) %>%
  mutate(gender_weight = target_proportion / (gender_count / sum(gender_count))) %>%
  select(country, gender, gender_weight)


# Merge weights back to main data
fdata_with_weights <- fdata %>%
  mutate (age_group = case_when(
    age >= 15 & age <= 24 ~ "15_24",
    age >= 25 & age <= 64 ~ "25_64",
    age >= 65 ~ "66_plus"
  ),
  education = factor (college, levels = c("0","1"), labels = c("nocollege", "college")),
  gender = factor (female, levels = c("0","1"), labels = c("male", "female"))) %>%
  left_join(age_weights, by = c("country", "age_group")) %>%
  left_join(education_weights, by = c("country", "education")) %>%
  left_join(gender_weights, by = c("country", "gender")) %>%
  mutate(composite_weight = age_group_weight * education_weight * gender_weight)

# View the data with composite weights
head(fdata_with_weights)

# Use composite weights in analysis (e.g., weighted mean age)
weighted_means <- fdata_with_weights %>%
  filter (!is.na(composite_weight)) %>%
  group_by(country) %>%
  summarize(weighted_mean_age = weighted.mean(age, w = composite_weight),
            mean_age = mean(age, na.rm =T))

print(weighted_means)

# save the dataset with weights
save(fdata_with_weights, file = "Data/fdata_with_weights")
