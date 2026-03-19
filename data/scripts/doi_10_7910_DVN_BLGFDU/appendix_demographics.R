# SETUP ========================================================================
library(dplyr)
library(data.table)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  openxlsx,
  tidyverse,
  kableExtra)

rm(list = ls())

#### Set your WD
#setwd("")

# DEMOGRAPHICS US ==============================================================

demos <- readRDS("Respondent_Info.rds")
demos <- filter(demos, person_id > 0)
demos <- select(demos, person_id, AGE_1, GENDER, ZIP, ETHN, HISP, EDU, PARTY, IDEO_1, FOLLOW, ideo_score, affiliation, affil_party, opposite, ideology)

demos$AGE_1 <- demos$AGE_1 + 16

visits <- fread("NewsVisits.csv")
visits <- visits %>%
  group_by(person_id, wave) %>%
  dplyr::summarize(
    count = n()
  ) %>%
  ungroup()

visits <- filter(visits, wave >0)
visits <- filter(visits, wave <4)

USwavevisitsper <- left_join(visits, demos, by = "person_id")

# recode categories
USwavevisitsper <- USwavevisitsper %>% 
  mutate(
    age_cat = case_when(
      AGE_1 < 25 ~ "18-24",
      AGE_1 >= 25 & AGE_1 < 35 ~ "25-34",
      AGE_1 >= 35 & AGE_1 < 45 ~ "35-44",
      AGE_1 >= 45 & AGE_1 < 55 ~ "45-54",
      AGE_1 >= 55 & AGE_1 < 65 ~ "55-64",
      AGE_1 >= 65 ~ "65+"),
    edu_cat = case_when(
      EDU %in% c(1, 5, 6) ~ "Less than high school", 
      EDU %in% c(8) ~ "High school graduate", 
      EDU %in% c(10) ~ "Completed some college, but no degree", 
      EDU %in% c(9, 11) ~ "Associate Degree + Other post-high school vocational training", 
      EDU %in% c(12, 13) ~ "Bachelor", 
      EDU %in% c(14, 15) ~ "Master's degree + Doctoral degree"),
    gender_cat = case_when(
      GENDER == 1 ~ "Male",
      GENDER == 2 ~ "Female"))

# Subset participants per wave
USwavevisitsper_w1 <- USwavevisitsper %>% filter(wave == 1)
USwavevisitsper_w2 <- USwavevisitsper %>% filter(wave == 2)
USwavevisitsper_w3 <- USwavevisitsper %>% filter(wave == 3)

# Population data
pop_us <- read.xlsx("US_census_acs.xlsx", sheet = 5)

# Join to population
sample_stats_us <- pop_us %>%
  right_join(., rbind(
    data.frame(prop.table(table(USwavevisitsper_w1$gender_cat))*100),
    data.frame(prop.table(table(USwavevisitsper_w1$age_cat))*100), 
    data.frame(prop.table(table(USwavevisitsper_w1$edu_cat))*100)) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      rename(value = Var1, `Wave 1` = Freq)) %>%
  left_join(., rbind(
    data.frame(prop.table(table(USwavevisitsper_w2$gender_cat))*100),
    data.frame(prop.table(table(USwavevisitsper_w2$age_cat))*100), 
    data.frame(prop.table(table(USwavevisitsper_w2$edu_cat))*100)) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      rename(value = Var1, `Wave 2` = Freq)) %>%
  left_join(., rbind(
    data.frame(prop.table(table(USwavevisitsper_w3$gender_cat))*100),
    data.frame(prop.table(table(USwavevisitsper_w3$age_cat))*100), 
    data.frame(prop.table(table(USwavevisitsper_w3$edu_cat))*100)) %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      rename(value = Var1, `Wave 3` = Freq)) 

# Add chi-square tests
p_gender_us <- round(chisq.test(rbind(
  (table(USwavevisitsper_w1$gender_cat)),
  (table(USwavevisitsper_w2$gender_cat)),
  (table(USwavevisitsper_w3$gender_cat))))$p.value, 3)
p_age_us <- round(chisq.test(rbind(
  (table(USwavevisitsper_w1$age_cat)),
  (table(USwavevisitsper_w2$age_cat)),
  (table(USwavevisitsper_w3$age_cat))))$p.value, 3)
p_edu_us <- round(chisq.test(rbind(
  (table(USwavevisitsper_w1$edu_cat)),
  (table(USwavevisitsper_w2$edu_cat)),
  (table(USwavevisitsper_w3$edu_cat))))$p.value, 3)

sample_stats_us <- sample_stats_us %>% 
  mutate(sig = case_when(
    variable == "gender_cat" & value == "Female"  ~ p_gender_us,
    variable == "age_cat" & value == "65+" ~ p_age_us,
    variable == "edu_cat" & value == "Master's degree + Doctoral degree" ~ p_edu_us)) %>%
  mutate(sig = ifelse(is.na(sig), "", sig)) %>%
  mutate(value = case_when(
    variable == "gender_cat" ~ paste0("Gender: ", value),
    variable == "age_cat" ~ paste0("Age: ", value),
    variable == "edu_cat" ~ paste0("Education: ", value))) %>%
  mutate(proportion = round(proportion*100, 2)) %>%
  select(-variable) 

kable(sample_stats_us, 
      caption = "Demographics of population/sample and wave attrition (US)", 
      format = "html", booktabs = T, escape = F, linesep = "",
      row.names = F, 
      col.names = c("", "Population", "Wave 1", "Wave 2", "Wave 3", "p-value (chi-squared test)")) %>%
  kable_styling(full_width = T, font_size = 10,
                latex_options = c("scale_down", "HOLD_position")) %>%
  column_spec(1, width = "6cm") %>%
  column_spec(6, width = "2.5cm") %>%
  save_kable(., file = "Table A1 sample stats US.htm")
