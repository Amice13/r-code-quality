# ================================================================
# SCRIPT: Calculate_Costs_with_Multidimensional_Penalizations.R
# Cervical Cancer Eradication Pipeline
#
# Este script:
# - Lee los archivos de costos ajustados por país y escenario
# - Extrae variables de infraestructura, política, género, etc.
# - Aplica penalizaciones por baja capacidad estructural
# - Guarda el detalle de penalizaciones y salidas finales
# ================================================================

# Paquetes
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringdist")) install.packages("stringdist")

library(openxlsx)
library(dplyr)
library(stringdist)

# Working directory
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Cargar dataset con todo consolidado
econ_data <- read.xlsx("Economic_Indicators_Cleaned.xlsx")

# Listar archivos de costos
contextual_files <- list.files(pattern = "_ContextualCosts.xlsx")

# Inicializar tabla resumen
results <- data.frame()

# Procesar cada país
for (file in contextual_files) {
  
  country <- gsub("_ContextualCosts.xlsx","",file)
  message("\n🔹 Processing: ", country)
  
  # Matching aproximado al dataframe
  dist <- stringdist::stringdist(
    tolower(country),
    tolower(econ_data$Country),
    method="jw"
  )
  if (min(dist) > 0.2) {
    warning("⚠️ No data found for: ", country)
    next
  }
  
  row <- econ_data[which.min(dist),]
  
  # Extraer Income Level (para imputaciones)
  income_level <- row$Income_Level_WB_numbered
  
  # Helper de imputación
  get_or_impute <- function(varname, default){
    val <- row[[varname]]
    if (is.na(val)){
      m <- mean(econ_data[[varname]][econ_data$Income_Level_WB_numbered == income_level], na.rm=TRUE)
      if (is.na(m)) return(default)
      return(m)
    }
    return(val)
  }
  
  # Variables con imputación
  uhc <- get_or_impute("Universal_Healthcare_Coverage_Index_2021",50)
  haq <- get_or_impute("Healthcare_Access_Quality_Index_2019_Age_Stand",50)
  phys <- get_or_impute("Physician_density_per_1000_2017",0.8)
  cancer_center <- get_or_impute("Cancer_Center_or_Department_Tertiary_Yes_or_Not",0)
  elec <- get_or_impute("Electricity_access_percent",60)
  roads <- get_or_impute("Roadways_to_surface_country",40)
  stab <- get_or_impute("Political_stability",-0.5)
  corrup <- get_or_impute("Corruption_perception_index",40)
  terror <- get_or_impute("Level_of_impact_terrorism",1)
  genderineq <- get_or_impute("Gender_Inequality_Index_2022",0.4)
  
  # Penalizaciones
  penalties <- c()
  if (uhc < 50) penalties <- c(penalties,0.15)
  if (haq < 50) penalties <- c(penalties,0.15)
  if (phys < 0.5) penalties <- c(penalties,0.10)
  if (cancer_center == 0) penalties <- c(penalties,0.20)
  if (elec < 60) penalties <- c(penalties,0.10)
  if (roads < 30) penalties <- c(penalties,0.10)
  if (stab < (-1)) penalties <- c(penalties,0.20)
  if (corrup < 30) penalties <- c(penalties,0.10)
  if (terror > 2) penalties <- c(penalties,0.10)
  if (genderineq > 0.6) penalties <- c(penalties,0.10)
  
  total_penalty <- sum(penalties)
  total_penalty <- min(total_penalty,0.8)
  
  # Leer el workbook
  wb_in <- loadWorkbook(file)
  scenarios <- names(wb_in)
  
  # Crear nuevo workbook de salida
  wb_out <- createWorkbook()
  
  for (sheet in scenarios) {
    
    df <- read.xlsx(file, sheet = sheet)
    if (!"Year" %in% names(df)) next
    
    df <- df %>%
      mutate(
        Vaccinated_Final = Vaccinated_Adjusted * (1 - total_penalty),
        Screened_Final = Screened_Adjusted * (1 - total_penalty)
      )
    
    # Seleccionar y agregar columnas
    df_out <- df %>%
      select(
        Year,
        ASIR_Mean,
        ASMR_Mean,
        Vaccinated_Est,
        Vaccinated_Adjusted,
        Vaccinated_Final,
        Screened_Est,
        Screened_Adjusted,
        Screened_Final,
        Cost_Total,
        Cost_Total_Adjusted
      ) %>%
      mutate(
        Total_Coverage_Penalty = round(total_penalty * 100,1)
      )
    
    # Guardar hoja
    addWorksheet(wb_out, sheet)
    writeData(wb_out, sheet, df_out)
  }
  
  # Guardar archivo del país
  saveWorkbook(wb_out, paste0(country,"_Costs_with_Penalizations.xlsx"), overwrite=TRUE)
  
  # Append resumen
  results <- rbind(
    results,
    data.frame(
      Country = country,
      Total_Penalty = round(total_penalty*100,1),
      Universal_Healthcare = uhc,
      HAQ = haq,
      Physician_Density = phys,
      Cancer_Center = cancer_center,
      Electricity = elec,
      Roads_Paved = roads,
      Stability = stab,
      Corruption = corrup,
      Terrorism = terror,
      Gender_Inequality = genderineq
    )
  )
}

# Guardar resumen
write.xlsx(results,"Countries_Penalization_Summary.xlsx",row.names=FALSE)
message("✅ Done! Penalizations applied and results saved.")
