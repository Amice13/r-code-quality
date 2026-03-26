# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(modelsummary)
library(tidyverse)
library(tinytable)

source("Code/fun/desc_stat_fun.R")

# Table B1: Summary Statistics
g <- readRDS(here("Data", "output", "19_wrp", "19_wrp_analysis.rds"))

file_wrp_sum <- here("Output", "tables", "tab_B1_wrp_sumstats.tex")

capn <- "Summary Statistics for Cross-National Survey"
notesn <- paste(
  "\\textit{Notes}: Household size provided in categories by WRP.",
  sep = " "
)

g$female <- as.integer(g$female == "Female")

g %>%
  mutate(
    `1-2 in Household` = as.integer(hhsize == "1-2"),
    `3-4 in Household` = as.integer(hhsize == "3-4"),
    `5-9 in Household` = as.integer(hhsize == "5-9"),
    `10 or more in Household` = as.integer(hhsize == "10 or more"),
    
    `Primary Education` = as.integer(edu == "Primary"),
    `Secondary Education` = as.integer(edu == "Secondary"),
    `Tertiary Education` = as.integer(edu == "Tertiary"),
    `Other Education` = as.integer(edu == "Other"),
    
    `Income: Very Difficult` = as.integer(incfeel == "Very difficult"),
    `Income: Difficult` = as.integer(incfeel == "Difficult"),
    `Income: Getting By` = as.integer(incfeel == "Getting by"),
    `Income: Comfortable` = as.integer(incfeel == "Comfortable"),
    `Income: Other` = as.integer(incfeel == "Other"),
    
    `Risk is Danger` = ifelse(riskund == "Danger", 1, 0),
    `Risk is Opportunity` = ifelse(riskund == "Opportunity", 1, 0),
    `Risk is Opportunity and Danger` = ifelse(riskund == "OpportunityAndDanger", 1, 0),
    `Risk is Neither` = ifelse(riskund == "Neither", 1, 0)
  ) %>%
  rename(
    `7 day temperature anomalies` = best_tanom_7d_z,
    `6 month fire burn area` = modis_burnanomp_mu_6m_w1_z,
    `+2SD heat days` = best_tanom_2sd_7d_z,
    `7 day temperature anomalies (recent baseline)` = noaa_cpc_tdev_7d_z,
    
    `Climate Vulnerable` = reg_loser_50,
    
    `Climate is Top/Major Risk` = salient,
    `Climate is Top Risk` = climatetop,
    
    `Placebo: Politics Risk` = placebo_politics2,
    `Placebo: Work Accident Risk` = placebo_work2,
    `Placebo: Powerline Worry` = placebo_powerline_worry,
    `Placebo: Appliance Worry` = placebo_appliance_worry,
    `Placebo: AI Good` = placebo_ai,
    `Placebo: Transport Risk` = placebo_transport2,
    `Placebo: House Risk` = placebo_house2,
    
    Children = kids_bin,
    `Internet Use` = internet,
    
    `Zonal GDP (log)` = gdp_log,
    `Zonal CO2 Emissions (log)` = co2_log,
    `Zonal Population (log)` = pop_log,
    `Urbanization` = urban_reg,
    `Zonal Coal` = coal,
    `Zonal Oil` = oil,

    Polyarchy = v2x_polyarchy,
    `National GDP per capita (log)` = gdppc_nat_log,
    `National Education` = edu_sec,
    `Agriculture Share of GDP` = ag_share,
    `National Population (log)` = pop_nat_log,
    `National CO2 Emissions (log)` = co2_nat_log
  ) %>%
  datasummary(
    `Climate is Top/Major Risk` + `Climate is Top Risk` + 
      
      `Placebo: Politics Risk` + `Placebo: Work Accident Risk` + `Placebo: Powerline Worry` + `Placebo: Appliance Worry` +
      
      `Placebo: AI Good` + `Placebo: Transport Risk` + `Placebo: House Risk` +
      
      (Age = age) + (Female = female) + Children + `Primary Education` + `Secondary Education` + `Tertiary Education` + `Other Education` + `Internet Use` +
      
      `1-2 in Household` + `3-4 in Household` + `5-9 in Household` + `10 or more in Household` +
      
      `Income: Very Difficult` + `Income: Difficult` + `Income: Getting By` + `Income: Comfortable` + `Income: Other` + 
      
       `Risk is Danger` + `Risk is Opportunity` + `Risk is Opportunity and Danger` + `Risk is Neither` + (`Risk Aversion` = riskaverse) +
      
      `Climate Vulnerable` +
      
      `7 day temperature anomalies` + `6 month fire burn area` + `+2SD heat days` + `7 day temperature anomalies (recent baseline)` +
      
      `Zonal GDP (log)` + `Zonal CO2 Emissions (log)` + `Zonal Population (log)` + `Urbanization` + 
      
      `Zonal Coal` + `Zonal Oil` + (`Zonal Cropland` = crop) +
      
      Polyarchy + `National GDP per capita (log)` + `National Education` + `Agriculture Share of GDP` + `National Population (log)` + `National CO2 Emissions (log)` ~ Mean + SD + Min + Max + `NA`,
    data = .,
    escape = FALSE,
    title = capn,
    notes = notesn
  ) %>%
  group_tt(
    i = list("\\textbf{Outcomes:}" = 1,
             "\\textbf{Placebos:}" = 3,
             "\\textbf{Individual:}" = 10,
             "\\textbf{Zonal:}" = 32,
             "\\textbf{Country:}" = 44
    ),
    escape = FALSE
  ) %>%
  theme_tt("multipage") %>%
  save_tt(file_wrp_sum, overwrite = TRUE)

message("Created Table B1. Summary statistics.")