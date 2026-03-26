#### ---- Descriptive statistics

library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(tinytable)
library(broom)
library(knitr)
library(magrittr)

setwd("/Users/srowa/Dropbox/Projects/Oil and gas cap/replication")

#### Load data ####

## Analysis data
full_field_analysis <- read_csv("input/full_field_analysis.csv")

#### Basic descriptive statistics ####

## Full sample
tt_summary <- full_field_analysis %>% 
  select(province_bucket,
         distance,
         # ff_employed,
         ff_familyfriends,
         # climate_concern, 
         climate_support, 
         bdn_baseline,
         bdn_update,
         bdn_shutdown,
         redistribution,
         dividends,
         therm_ab,
         therm_ownprov) %>% 
  # mutate(ff_employed = ifelse(ff_employed == "Yes", 1, 0)) %>% 
  mutate(in_alberta = ifelse(province_bucket == "Alberta", 1, 0),
         in_nl = ifelse(province_bucket == "Newfoundland", 1, 0)) %>%
  pivot_longer(names_to = "measure",
               values_to = "value",
               distance:in_nl) %>%
  group_by(measure) %>% 
  summarize(n = sum(!is.na(value)),
            mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T),
            min = min(value, na.rm = T),
            max = max(value, na.rm = T)) %>% 
  mutate_at(vars(mean:max), ~round(., digits = 2)) %>% 
  mutate(index = case_when(measure == "in_alberta" ~ 1.1,
                           measure == "in_nl" ~ 1.2,
                           # measure == "in_atlantic" ~ 1.3,
                           measure == "bdn_baseline" ~ 2,
                           measure == "bdn_update" ~ 2.1,
                           measure == "bdn_shutdown" ~ 2.2,
                           measure == "climate_support" ~ 3.1,
                           measure == "therm_ab" ~ 3.2,
                           measure == "therm_ownprov" ~ 3.3,
                           measure == "redistribution" ~ 3.4,
                           measure == "dividends" ~ 3.5,
                           measure == "distance" ~ 4.1,
                           measure == "ff_familyfriends" ~ 4.2)) %>% 
  mutate(measure = case_when(measure == "in_alberta" ~ "Alberta",
                             measure == "in_nl" ~ "Newfoundland",
                             measure == "bdn_baseline" ~ "Bay du Nord, baseline",
                             measure == "bdn_update" ~ "Bay du Nord, update",
                             measure == "bdn_shutdown" ~ "Bay du Nord, shutdown",
                             measure == "climate_support" ~ "Climate policy support",
                             measure == "therm_ab" ~ "Feelings toward Alberta",
                             measure == "therm_ownprov" ~ "Feelings toward own province",
                             measure == "redistribution" ~ "Allocation of spending to redistribution",
                             measure == "dividends" ~ "Allocation of spending to dividends",
                             measure == "distance" ~ "Distance to oil and gas extraction",
                             measure == "ff_familyfriends" ~ "Family and friends in oil and gas industry")) %>% 
  arrange(index) %>% 
  select(-index) %>% 
  set_colnames(c("Variable", "Obs.", "Mean", "S.D.", "Min.", "Max.")) %>% 
  tt() 

save_tt(x = tt_summary, output = "output/descriptives.docx", overwrite = F)



## Focus on province
tt_province <- full_field_analysis %>% 
  select(province,
         climate_support,
         bdn_baseline) %>% 
  # mutate(province_bucket = case_when(province == "AB" ~ "Alberta",
  #                                    province %in% c("NB", "NS", "PEI", "NL") ~ "Atlantic",
  #                                    .default = "Rest of Canada")) %>% 
  pivot_longer(names_to = "measure",
               values_to = "value",
               climate_support:bdn_baseline) %>%
  group_by(province, measure) %>% 
  summarize(n = sum(!is.na(value)),
            mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  pivot_wider(names_from = "measure", values_from = n:sd) %>% 
  select(province, n = n_bdn_baseline, 
         mean_bdn_baseline, sd_bdn_baseline, 
         mean_climate_support, sd_climate_support) %>% 
  mutate_at(vars(mean_bdn_baseline:sd_climate_support),
            ~round(., digits = 2)) %>% 
  set_colnames(c("Province", "Obs.", 
                 "Bay du Nord baseline (mean)", "Bay du Nord baseline (s.d.)", 
                 "Climate support (mean)", "Climate support (s.d.)")) %>% 
  tt()

save_tt(x = tt_province, output = "output/descriptives_by_province.docx", overwrite = F)



