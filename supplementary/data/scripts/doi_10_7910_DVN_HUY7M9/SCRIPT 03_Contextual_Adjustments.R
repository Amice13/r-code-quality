# ============================================================
# SCRIPT 03_Contextual_Adjustments_Corrected.R
# Cervical Cancer Eradication Pipeline
# Lee los COSTOS AJUSTADOS (inflación/deflación) e incorpora:
# - Ajustes contextuales
# - Modificadores de cobertura y costos
# - Costos netos ajustados
# ============================================================

# Load packages
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringdist")) install.packages("stringdist")

library(openxlsx)
library(dplyr)
library(stringdist)

# Working directory
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Parameters
coverage_literacy_modifier <- 0.80
coverage_educational_index_modifier <- 0.90
coverage_gdp_education_modifier <- 0.90
coverage_ncd_modifier <- 0.85

cost_gdp_health_modifier <- 1.10
cost_haq_modifier <- 1.20

# Debt categories
debt_categories <- c(
  "High fiscal space",
  "Moderate fiscal space",
  "Low fiscal space",
  "Very low fiscal space"
)

# Economic indicators
econ_data <- read.xlsx("Economic_Indicators_Cleaned.xlsx")
countries <- econ_data$Country %>% unique() %>% sort()

# Read the adjusted cost files from step 2
adjusted_files <- list.files(pattern = "_AdjustedCosts.xlsx")

# Iterate countries
for (country in countries) {
  
  message("\n🔹 Processing country: ", country)
  
  # Approximate matching
  distances <- stringdist::stringdist(
    tolower(country),
    tolower(gsub("_AdjustedCosts.xlsx","",adjusted_files)),
    method="jw"
  )
  
  min_dist <- min(distances)
  
  if (min_dist > 0.1) {
    warning("⚠️ No matching adjusted file for: ", country)
    next
  }
  
  matched_file <- adjusted_files[which.min(distances)]
  country_name_clean <- gsub("_AdjustedCosts.xlsx","",matched_file)
  
  # Extract economic variables
  econ_row <- econ_data %>% filter(Country == country)
  
  if (nrow(econ_row)==0) {
    warning("⚠️ No economic data for: ", country)
    next
  }
  
  # Safe extraction
  literacy <- if ("Female_Literacy_percent" %in% names(econ_row)) econ_row$Female_Literacy_percent else NA
  educational_index <- if ("Educational_Index_2022" %in% names(econ_row)) econ_row$Educational_Index_2022 else NA
  gdp_education <- if ("Percent_GDP_Education" %in% names(econ_row)) econ_row$Percent_GDP_Education else NA
  gdp_health <- if ("Percent_GDP_Health" %in% names(econ_row)) econ_row$Percent_GDP_Health else NA
  ncd_death <- if ("Premature_NCD_Death_percent" %in% names(econ_row)) econ_row$Premature_NCD_Death_percent else NA
  haq_index <- if ("Healthcare_Access_Quality_Index" %in% names(econ_row)) econ_row$Healthcare_Access_Quality_Index else NA
  debt_ratio <- if ("Public_Debt_to_GDP_2017_percent" %in% names(econ_row)) econ_row$Public_Debt_to_GDP_2017_percent else NA
  income_level <- if ("Income_Level_WB_numbered" %in% names(econ_row)) econ_row$Income_Level_WB_numbered else NA
  
  if (is.na(income_level)) {
    warning("⚠️ No income level for: ", country)
    next
  }
  
  # Impute missing literacy
  if (is.na(literacy)) {
    literacy <- if (income_level==4) 100 else mean(econ_data$Female_Literacy_percent[econ_data$Income_Level_WB_numbered==income_level], na.rm=TRUE)
  }
  
  # Impute missing %GDP Health
  if (is.na(gdp_health)) {
    gdp_health <- mean(econ_data$Percent_GDP_Health[econ_data$Income_Level_WB_numbered==income_level], na.rm=TRUE)
  }
  
  # Compute modifiers
  coverage_mod <- 1
  if (!is.na(literacy) && literacy < 60) coverage_mod <- coverage_mod * coverage_literacy_modifier
  if (!is.na(educational_index) && educational_index < 0.6) coverage_mod <- coverage_mod * coverage_educational_index_modifier
  if (!is.na(gdp_education) && gdp_education < 3) coverage_mod <- coverage_mod * coverage_gdp_education_modifier
  if (!is.na(ncd_death) && ncd_death > 30) coverage_mod <- coverage_mod * coverage_ncd_modifier
  
  cost_mod <- 1
  if (!is.na(gdp_health) && gdp_health < 3) cost_mod <- cost_mod * cost_gdp_health_modifier
  if (!is.na(haq_index) && haq_index < 50) cost_mod <- cost_mod * cost_haq_modifier
  
  # Debt category
  if (is.na(debt_ratio)) {
    debt_cat <- NA
  } else if (debt_ratio < 30) {
    debt_cat <- debt_categories[1]
  } else if (debt_ratio < 60) {
    debt_cat <- debt_categories[2]
  } else if (debt_ratio < 90) {
    debt_cat <- debt_categories[3]
  } else {
    debt_cat <- debt_categories[4]
  }
  
  message("✅ Structural variables loaded for: ", country)
  
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
    
    # Adjust coverage and costs
    df <- df %>%
      mutate(
        Vaccinated_Adjusted = Vaccinated_Est * coverage_mod,
        Screened_Adjusted = Screened_Est * coverage_mod,
        Cost_Vaccine_Adjusted = Cost_Vaccine * cost_mod,
        Cost_Screening_Adjusted = Cost_Screening * cost_mod,
        Cost_Treatment_Adjusted = Cost_Treatment * cost_mod,
        Cost_Total_Adjusted = Cost_Total * cost_mod,
        Net_Cost_vs_Inertia_Adjusted = Net_Cost_vs_Inertia * cost_mod,
        Net_Cost_vs_Inertia_Inflated_Adjusted = Net_Cost_vs_Inertia_Inflated * cost_mod,
        Net_Cost_vs_Inertia_Real2023_Adjusted = Net_Cost_vs_Inertia_Real2023 * cost_mod,
        Coverage_Modifier = coverage_mod,
        Cost_Modifier = cost_mod,
        Debt_Ratio = debt_ratio,
        Debt_Category = debt_cat,
        Literacy_Used = literacy,
        Educational_Index_Used = educational_index,
        GDP_Education_Used = gdp_education,
        GDP_Health_Used = gdp_health,
        NCD_Death_Used = ncd_death,
        HAQ_Index_Used = haq_index
      )
    
    # Columns to output
    df_out <- df %>%
      select(
        Year,
        ASIR_Mean,
        ASMR_Mean,
        Cases,
        Deaths,
        Vaccinated_Est,
        Screened_Est,
        Vaccinated_Adjusted,
        Screened_Adjusted,
        Cost_Vaccine,
        Cost_Screening,
        Cost_Treatment,
        Cost_Total,
        Net_Cost_vs_Inertia,
        Net_Cost_vs_Inertia_Inflated,
        Net_Cost_vs_Inertia_Real2023,
        Cost_Total_Adjusted,
        Net_Cost_vs_Inertia_Adjusted,
        Net_Cost_vs_Inertia_Inflated_Adjusted,
        Net_Cost_vs_Inertia_Real2023_Adjusted,
        Coverage_Modifier,
        Cost_Modifier,
        Debt_Ratio,
        Debt_Category,
        Literacy_Used,
        Educational_Index_Used,
        GDP_Education_Used,
        GDP_Health_Used,
        NCD_Death_Used,
        HAQ_Index_Used
      )
    
    # Write sheet
    addWorksheet(wb_out, sheet)
    writeData(wb_out, sheet, df_out)
  }
  
  # Save output workbook
  saveWorkbook(wb_out, paste0(country_name_clean,"_ContextualCosts.xlsx"), overwrite=TRUE)
  
  message("✅ Finished: ", country)
}
