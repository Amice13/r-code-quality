##################################################################################
# Agrawal and Chen (2026)
#
# Descriptions: Construct AGI adjustment factor from IRS SOI to match aggregate AGI 
# by income group in official statistics
# 
##################################################################################
# Clear environment
rm(list = ls())

library(ipumsr)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(haven)
library(data.table)
library(arrow)
library(purrr)
library(readxl)

# Read in IRS SOI data
soi_16 <- read_excel("data/irs_soi/16in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2016)

soi_17 <- read_excel("data/irs_soi/17in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2017)

soi_18 <- read_excel("data/irs_soi/18in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2018)


soi_19 <- read_excel("data/irs_soi/19in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2019)


soi_20 <- read_excel("data/irs_soi/20in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2020)


soi_21 <- read_excel("data/irs_soi/21in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2021)

soi_22 <- read_excel("data/irs_soi/22in11si.xls", skip = 5) |> 
  select(income_group = 1, soi_agg_fagi = 4) |> 
  filter(!is.na(income_group)) |> 
  slice(4:21) |> 
  mutate(year = 2022)


soi_combined <- rbind(
  soi_16, soi_17, soi_18, soi_19, soi_20, soi_21, soi_22
) |> 
  mutate(
    soi_agg_fagi = soi_agg_fagi*1000
    )


soi_regrouped <- soi_combined  |> 
  mutate(
    inc_bin = case_when(
      income_group %in% c("$1 under $5,000",
                          "$5,000 under $10,000",
                          "$10,000 under $15,000",
                          "$15,000 under $20,000",
                          "$20,000 under $25,000") ~ "[<$25k)",
      
      income_group %in% c("$25,000 under $30,000",
                          "$30,000 under $40,000",
                          "$40,000 under $50,000") ~ "[$25k–$50k)",
      
      income_group == "$50,000 under $75,000" ~ "[$50k–$75k)",
      income_group == "$75,000 under $100,000" ~ "[$75k–$100k)",
      income_group == "$100,000 under $200,000" ~ "[$100k–$200k)",
      income_group == "$200,000 under $500,000" ~ "[$200k–$500k)",
      
      income_group %in% c("$500,000 under $1,000,000",
                          "$1,000,000 under $1,500,000",
                          "$1,500,000 under $2,000,000",
                          "$2,000,000 under $5,000,000",
                          "$5,000,000 under $10,000,000",
                          "$10,000,000 or more") ~ "[$500k+)",
      
      TRUE ~ NA_character_
    ),
    inc_bin = factor(
      inc_bin,
      levels = c(
        "[<$25k)",
        "[$25k–$50k)",
        "[$50k–$75k)",
        "[$75k–$100k)",
        "[$100k–$200k)",
        "[$200k–$500k)",
        "[$500k+)"
      )
    )
  )  |> 
  filter(!is.na(inc_bin))  |> 
  group_by(year, inc_bin)  |> 
  summarise(
    soi_agg_fagi = sum(soi_agg_fagi, na.rm = TRUE),
    .groups = "drop"
  )



# Read in cleaned ACS + taxsim output data for all tax units ---------------------------

# already non-institutionalized, non-farm, 50 states + DC
cleaned_all <- read_csv("data/derived_ipums/cleaned_all_wATR.csv")

acs_regrouped <- cleaned_all |> 
  mutate(
    nom_fagi_bins = cut(
      fagi,
      breaks = c(-Inf, 25000, 50000, 75000, 100000, 200000, 500000, Inf),
      right  = FALSE,
      labels = c(
        "[<$25k)",
        "[$25k–$50k)",
        "[$50k–$75k)",
        "[$75k–$100k)",
        "[$100k–$200k)",
        "[$200k–$500k)",
        "[$500k+)"
      )
    )
  ) |> 
  filter(!is.na(nom_fagi_bins)) |> 
  group_by(year, nom_fagi_bins)  |> 
  summarise(
    acs_agg_fagi = sum(fagi*hhwt, na.rm = TRUE),
    .groups = "drop"
  )

adjust_agi_d <- acs_regrouped |> 
  left_join(soi_regrouped, by = c("year", "nom_fagi_bins" = "inc_bin")) |> 
  mutate(
    adjust_fagi_factor = soi_agg_fagi / acs_agg_fagi
  ) 

impute_adjust_agi_d_22 <- adjust_agi_d |> 
  filter(year==2022) |> 
  select(nom_fagi_bins, adjust_fagi_factor) |> 
  rename(
    imputed_adjust_fagi_factor = adjust_fagi_factor
  )


adjust_agi_d_allyrs <- adjust_agi_d |> 
  left_join(impute_adjust_agi_d_22, by = c("nom_fagi_bins")) |> 
  mutate(
    adjust_fagi_factor = ifelse(year>=2023, imputed_adjust_fagi_factor, adjust_fagi_factor)
  ) |> 
  select(-imputed_adjust_fagi_factor)

write_csv(adjust_agi_d_allyrs, "data/intermediate/adjust_agi_factor.csv")










