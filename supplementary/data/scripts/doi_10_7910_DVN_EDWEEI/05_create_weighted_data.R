# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)

g <- readRDS(here("Data", "inter", "19_wrp", "19_wrp_noweights.rds"))
wt <- readRDS(here("Data", "cluster", "19_wrp_weights.rds"))

df_out <- left_join(g, wt, by = "wpid_random")

# Subset to covariates used in 02_analysis scripts
vars_to_keep <- c(
  # Identifiers
  "wpid_random", "country", "cty_reg", "globalreg", "gid_0", "reg_id",
  
  # Outcomes
  "salient", "climatetop",
  
  # Placebo outcomes
  "placebo_politics2", "placebo_work2", "placebo_powerline_worry", 
  "placebo_appliance_worry", "placebo_ai", "placebo_transport2", "placebo_house2",
  
  # Vulnerability measures
  "reg_loser_50", "gdp_50",
  
  # Climate shocks (standardized)
  "best_tanom_7d_z", "best_tanom_2sd_7d_z", "modis_burnanomp_mu_6m_w1_z", 
  "noaa_cpc_tdev_7d_z", "modis_burnanomp_mu_6m_w5_z", "modis_burnanomp_mu_5m_w1_z", 
  "modis_burnanomp_mu_7m_w1_z", "modis_burnanomp_mu_6m_w.5_z",
  
  # Treatment variables
  "treat_best_tanom_7d", "treat_best_tanom_2sd_7d", "treat_modis_burnanomp_mu_6m_w1", 
  "treat_noaa_cpc_tdev_7d", "treat_modis_burnanomp_mu_6m_w5", "treat_modis_burnanomp_mu_5m_w1", 
  "treat_modis_burnanomp_mu_7m_w1", "treat_modis_burnanomp_mu_6m_w.5",
  
  # Individual covariates
  "age", "age2", "age_year", "age_gen", "female", "edu", "rural", "internet", 
  "hhsize", "incfeel", "kids_bin", "riskaverse", "riskund",
  
  # Zone/regional covariates
  "crop", "gdp_log", "pop_log", "co2_log", "oil", "coal", "urban_reg",
  
  # National covariates
  "v2x_polyarchy", "gdppc_nat_log", "edu_sec", "pop_nat_log", "co2_nat_log", "ag_share",
  
  # Weighted treatment variables (from weights file)
  "w_treat_best_tanom_7d", "w_treat_best_tanom_2sd_7d", 
  "w_treat_modis_burnanomp_mu_6m_w1", "w_treat_noaa_cpc_tdev_7d",
  "w_treat_modis_burnanomp_mu_6m_w5", "w_treat_modis_burnanomp_mu_5m_w1",
  "w_treat_modis_burnanomp_mu_7m_w1", "w_treat_modis_burnanomp_mu_6m_w.5"
)

# Select only the variables that exist in the merged data
vars_available <- vars_to_keep[vars_to_keep %in% names(df_out)]
vars_missing <- setdiff(vars_to_keep, vars_available)

if (length(vars_missing) > 0) {
  warning(paste("The following variables were not found in the data:", 
                paste(vars_missing, collapse = ", ")))
}

df_out <- df_out %>% select(all_of(vars_available))

message(paste("Retained", ncol(df_out), "variables out of", length(vars_to_keep), "requested."))

saveRDS(df_out, here("Data", "output", "19_wrp", "19_wrp_analysis.rds"))
message("Created 19_wrp_analysis.rds")