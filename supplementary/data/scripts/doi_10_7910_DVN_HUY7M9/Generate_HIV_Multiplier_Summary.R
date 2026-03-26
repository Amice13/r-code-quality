# ================================================================
# SCRIPT: Generate_HIV_Multiplier_Summary.R
# Crea HIV_Adjustment_Summary.xlsx con multiplicadores
# ================================================================

library(openxlsx)
library(dplyr)
library(stringdist)

# Directorio de trabajo
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Cargar dataset económico con prevalencia VIH
econ_data <- read.xlsx("Economic_Indicators_Cleaned.xlsx")

# Listar países
countries <- econ_data$Country

# Inicializar resumen
results_summary <- data.frame()

# Parámetro de riesgo relativo
RR_HIV <- 4.0

# Procesar cada país
for (i in seq_along(countries)) {
  
  country <- countries[i]
  
  hiv_prev_raw <- as.character(econ_data$HIV_prevalence[i])
  hiv_prev_clean <- gsub(",", ".", hiv_prev_raw)
  hiv_prev <- as.numeric(hiv_prev_clean)
  
  # Si es -999, marcar como NA
  if (!is.na(hiv_prev) && hiv_prev <= -999) {
    hiv_prev <- NA
  }
  
  # Si sigue NA, asumir cero
  if (is.na(hiv_prev)) {
    hiv_prev <- 0
    note <- "No data – assumed 0"
  } else {
    hiv_prev <- hiv_prev / 100
    note <- "Data present"
  }
  
  # Calcular multiplicador
  multiplier <- (1 - hiv_prev) + hiv_prev * RR_HIV
  
  results_summary <- rbind(
    results_summary,
    data.frame(
      Country = country,
      HIV_Prevalence_Perc = round(hiv_prev*100,3),
      Multiplier = round(multiplier,3),
      Note = note
    )
  )
}

# Guardar
write.xlsx(results_summary, "HIV_Adjustment_Summary.xlsx", row.names=FALSE)

message("✅ HIV_Adjustment_Summary.xlsx created.")
