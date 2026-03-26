# ============================================================
# SCRIPT 02_Adjusted_Costs_Inflation_Deflation_Corrected.R
# Cervical Cancer Eradication Pipeline
# Lee los COSTOS CORREGIDOS que incluyen tratamiento y ahorros
# Calcula:
#   - Inflación anual
#   - Deflación a USD 2023
#   - Incluye Net Cost vs Inertia
# ============================================================

# Load packages
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringdist")) install.packages("stringdist")

library(openxlsx)
library(dplyr)
library(stringdist)

# Set working directory
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# PARAMETERS
annual_inflation <- 0.03
annual_discount <- 0.03

# Read master country list
econ_data <- read.xlsx("Economic_Indicators_Cleaned.xlsx")
countries <- econ_data$Country %>% unique() %>% sort()

# Read the NEW corrected cost files
cost_files <- list.files(pattern = "_CostProjections_Corrected.xlsx")

# Iterate over countries
for (country in countries) {
  
  message("\n🔹 Processing country: ", country)
  
  # Approximate matching
  distances <- stringdist::stringdist(
    tolower(country),
    tolower(gsub("_CostProjections_Corrected.xlsx","",cost_files)),
    method="jw"
  )
  
  min_dist <- min(distances)
  
  if (min_dist > 0.1) {
    warning("⚠️ No matching file for: ", country)
    next
  }
  
  matched_file <- cost_files[which.min(distances)]
  country_name_clean <- gsub("_CostProjections_Corrected.xlsx","",matched_file)
  
  # Load workbook
  wb_in <- loadWorkbook(matched_file)
  scenario_sheets <- names(wb_in)
  
  # Output workbook
  wb_out <- createWorkbook()
  
  for (sheet in scenario_sheets) {
    
    df <- read.xlsx(matched_file, sheet=sheet)
    
    if (!"Year" %in% names(df)) {
      warning("⚠️ No 'Year' column in ", sheet, " of ", country)
      next
    }
    
    # Inflation/deflation
    df <- df %>%
      mutate(
        YearsSince2023 = Year - 2023,
        Cost_Total_Inflated = Cost_Total * (1 + annual_inflation) ^ YearsSince2023,
        Cost_Total_Real2023 = Cost_Total / (1 + annual_discount) ^ YearsSince2023,
        Net_Cost_vs_Inertia_Inflated = Net_Cost_vs_Inertia * (1 + annual_inflation) ^ YearsSince2023,
        Net_Cost_vs_Inertia_Real2023 = Net_Cost_vs_Inertia / (1 + annual_discount) ^ YearsSince2023
      )
    
    # Columns to export
    df_out <- df %>%
      select(
        Year,
        ASIR_Mean,
        ASMR_Mean,
        Cases,
        Cases_Inertia,
        Delta_Cases,
        Deaths,
        Vaccinated_Est,
        Screened_Est,
        Cost_Vaccine,
        Cost_Screening,
        Cost_Treatment,
        Cost_Treatment_Inertia,
        Treatment_Savings,
        Cost_Total,
        Cost_Total_Inflated,
        Cost_Total_Real2023,
        Net_Cost_vs_Inertia,
        Net_Cost_vs_Inertia_Inflated,
        Net_Cost_vs_Inertia_Real2023
      )
    
    addWorksheet(wb_out, sheet)
    writeData(wb_out, sheet, df_out)
  }
  
  # Save output
  saveWorkbook(wb_out, paste0(country_name_clean,"_AdjustedCosts.xlsx"), overwrite=TRUE)
  
  message("✅ Finished: ", country)
}
