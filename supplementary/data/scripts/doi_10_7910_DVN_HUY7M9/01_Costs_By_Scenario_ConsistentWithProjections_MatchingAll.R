# ================================================
# SCRIPT 01_Costs_By_Scenario_Corrected.R
# Cervical Cancer Eradication Pipeline (Revised)
# Incluye:
# - Tratamiento en inercia
# - Ahorros de tratamiento
# - Costo total neto y bruto
# ================================================

# Paquetes
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringdist")) install.packages("stringdist")

library(openxlsx)
library(dplyr)
library(stringdist)

# Carpeta de trabajo
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Parámetros
efficacy_vaccine <- 0.90
efficacy_screening <- 0.50

# Leer archivos base
unit_costs <- read.xlsx("Unit_Costs_By_IncomeLevel.xlsx")
econ_data <- read.xlsx("Economic_Indicators_Cleaned.xlsx")

# Vector de países
countries <- econ_data$Country %>% unique() %>% sort()

# Archivos de proyección
proj_files <- list.files(pattern = "_Projections.xlsx")

# Iterar sobre países
for (country in countries) {
  message("\n🔹 Procesando país: ", country)
  
  # Matching aproximado
  distances <- stringdist::stringdist(tolower(country), tolower(gsub("_Projections.xlsx","",proj_files)), method="jw")
  min_dist <- min(distances)
  
  if(min_dist > 0.1) {
    warning("⚠️ No se encontró proyección para: ", country)
    next
  }
  
  matched_file <- proj_files[which.min(distances)]
  
  wb <- loadWorkbook(matched_file)
  scenarios <- names(wb)
  
  # Epidemiología
  epi_file <- "EpiEcon_Projection_Pop_GDP.xlsx"
  sheets_epi <- getSheetNames(epi_file)
  
  distances_epi <- stringdist::stringdist(tolower(country), tolower(sheets_epi), method="jw")
  min_dist_epi <- min(distances_epi)
  
  if(min_dist_epi > 0.1) {
    warning("⚠️ No se encontró hoja epi: ", country)
    next
  }
  
  matched_sheet_epi <- sheets_epi[which.min(distances_epi)]
  epi_data <- read.xlsx(epi_file, sheet = matched_sheet_epi)
  names(epi_data) <- tolower(names(epi_data))
  
  # Datos económicos
  econ_row <- econ_data %>% filter(Country==country)
  if(nrow(econ_row)==0){
    warning("⚠️ No hay datos económicos para: ", country)
    next
  }
  
  income_level <- econ_row$Income_Level_WB_numbered
  base_vac <- econ_row$HPV_Vaccination_Coverage_Final_Dose_perc/100
  base_screen <- econ_row$Women_Ever_Screened_30_49_percent/100
  if(is.na(base_vac)) base_vac <- 0
  if(is.na(base_screen)) base_screen <- 0
  
  # Costos unitarios
  cost_vac <- unit_costs %>% filter(Income_Level_WB_numbered == income_level) %>% pull(HPV_Vaccine_Unit_Cost_USD)
  cost_screen <- unit_costs %>% filter(Income_Level_WB_numbered == income_level) %>% pull(Screening_Unit_Cost_USD)
  cost_treat <- unit_costs %>% filter(Income_Level_WB_numbered == income_level) %>% pull(Treatment_Unit_Cost_USD)
  
  # Leer inercia como referencia
  df_inertia <- read.xlsx(matched_file, sheet="Inertia")
  
  # Workbook salida
  wb_out <- createWorkbook()
  
  for(scn in scenarios){
    
    df <- read.xlsx(matched_file, sheet=scn)
    years_common <- intersect(df$Year, epi_data$year)
    if(length(years_common)==0){
      warning("⚠️ No años comunes en: ", country, " escenario ", scn)
      next
    }
    
    df <- df %>% filter(Year %in% years_common)
    df_inertia_f <- df_inertia %>% filter(Year %in% years_common)
    epi_data_f <- epi_data %>% filter(year %in% years_common)
    
    # Combinar
    df <- df %>% left_join(epi_data_f, by=c("Year"="year"))
    
    # Casos y muertes
    df$Cases <- df$ASIR_Mean * df$women_at_risk / 100000
    df$Deaths <- df$ASMR_Mean * df$women_at_risk / 100000
    
    # Casos inercia (para ahorro)
    df$Cases_Inertia <- df_inertia_f$ASIR_Mean * df$women_at_risk / 100000
    
    # Diferencias
    df$Delta_Cases <- df$Cases_Inertia - df$Cases
    df$Delta_Mortality <- (df_inertia_f$ASMR_Mean - df$ASMR_Mean) * df$women_at_risk / 100000
    
    # Vacunadas y tamizadas nuevas
    df$Vaccinated_Est <- df$Delta_Cases / efficacy_vaccine
    df$Screened_Est <- df$Delta_Mortality / efficacy_screening
    
    if(tolower(scn)=="inertia"){
      df$Vaccinated_Est <- df$women_at_risk * base_vac
      df$Screened_Est <- df$women_at_risk * base_screen
    } else {
      df$Vaccinated_Est <- df$Vaccinated_Est + df$women_at_risk * base_vac
      df$Screened_Est <- df$Screened_Est + df$women_at_risk * base_screen
    }
    
    df$Vaccinated_Est[df$Vaccinated_Est<0] <- 0
    df$Screened_Est[df$Screened_Est<0] <- 0
    
    # Costos
    df$Cost_Vaccine <- df$Vaccinated_Est * cost_vac
    df$Cost_Screening <- df$Screened_Est * cost_screen
    df$Cost_Treatment <- df$Cases * cost_treat
    df$Cost_Treatment_Inertia <- df$Cases_Inertia * cost_treat
    df$Treatment_Savings <- df$Cost_Treatment_Inertia - df$Cost_Treatment
    
    df$Cost_Total <- df$Cost_Vaccine + df$Cost_Screening + df$Cost_Treatment
    df$Cost_Total_Inertia <- df$Cost_Treatment_Inertia
    df$Net_Cost_vs_Inertia <- df$Cost_Total - df$Treatment_Savings
    
    # Salida
    df_out <- df %>% select(
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
      Cost_Total_Inertia,
      Net_Cost_vs_Inertia
    )
    
    addWorksheet(wb_out, scn)
    writeData(wb_out, scn, df_out)
  }
  
  saveWorkbook(wb_out, paste0(country, "_CostProjections_Corrected.xlsx"), overwrite=TRUE)
  message("✅ Terminado: ", country)
}

list.files(pattern = "_Projections.xlsx")
