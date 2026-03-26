# =============================================
# SCRIPT: Extraer indicadores resumen por país y escenario
# =============================================

# Instalar paquetes si es necesario
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")

library(openxlsx)
library(dplyr)

# Cambia aquí el directorio con tus archivos de proyecciones
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Listar todos los archivos que terminan en _Projections.xlsx
files <- list.files(pattern = "_Projections.xlsx")

# Crear lista para acumular resultados
results_list <- list()

# Bucle por cada archivo (cada país)
for (file in files) {
  country_name <- gsub("_Projections.xlsx","",file)
  
  sheets <- getSheetNames(file)
  
  # Verificar que tenga la hoja Inertia
  if (!"Inertia" %in% sheets) {
    message("⚠️ ", file, " no tiene hoja Inertia. Se omite.")
    next
  }
  
  for (sheet in sheets) {
    df <- read.xlsx(file, sheet = sheet)
    
    # Verificar columnas necesarias
    cols_needed <- c("Year","ASIR_Mean","ASMR_Mean")
    if (!all(cols_needed %in% colnames(df))) {
      message("⚠️ ", country_name, " - ", sheet, " no tiene columnas esperadas. Se omite.")
      next
    }
    
    if (nrow(df)==0) {
      message("⚠️ ", country_name, " - ", sheet, " está vacía. Se omite.")
      next
    }
    
    # Asegurar numérico
    df$ASIR_Mean <- as.numeric(df$ASIR_Mean)
    df$ASMR_Mean <- as.numeric(df$ASMR_Mean)
    
    if (!any(df$Year==2022) || !any(df$Year==2050)) {
      message("⚠️ ", country_name, " - ", sheet, " no tiene años 2022 y 2050. Se omite.")
      next
    }
    
    # Valores base
    asir_2022 <- df$ASIR_Mean[df$Year==2022]
    asir_2050 <- df$ASIR_Mean[df$Year==2050]
    asmr_2022 <- df$ASMR_Mean[df$Year==2022]
    asmr_2050 <- df$ASMR_Mean[df$Year==2050]
    
    # % cambio
    pct_change_asir <- round(((asir_2050 - asir_2022)/asir_2022)*100,2)
    pct_change_asmr <- round(((asmr_2050 - asmr_2022)/asmr_2022)*100,2)
    
    # Reducción anual promedio
    annual_reduction_asir <- round((((asir_2050 / asir_2022)^(1/28))-1)*100,2)
    annual_reduction_asmr <- round((((asmr_2050 / asmr_2022)^(1/28))-1)*100,2)
    
    # Incidencia mínima y año
    asir_min <- round(min(df$ASIR_Mean, na.rm=TRUE),2)
    year_asir_min <- df$Year[which.min(df$ASIR_Mean)]
    
    # Mortalidad mínima y año
    asmr_min <- round(min(df$ASMR_Mean, na.rm=TRUE),2)
    year_asmr_min <- df$Year[which.min(df$ASMR_Mean)]
    
    # ASIR y ASMR en 2045
    asir_2045 <- round(df$ASIR_Mean[df$Year==2045],2)
    asmr_2045 <- round(df$ASMR_Mean[df$Year==2045],2)
    
    # Reducción absoluta
    abs_reduction_asir <- round(asir_2022 - asir_2050,2)
    abs_reduction_asmr <- round(asmr_2022 - asmr_2050,2)
    
    # Ratio ASMR/ASIR
    ratio_2022 <- round(asmr_2022 / asir_2022,3)
    ratio_2050 <- round(asmr_2050 / asir_2050,3)
    
    # Año en que ASIR <4
    year_asir4_vec <- df$Year[df$ASIR_Mean<4]
    year_asir4 <- ifelse(
      length(year_asir4_vec)==0,
      "not achieved 2050",
      paste0("year:", min(year_asir4_vec))
    )
    
    # Año en que ASMR <1
    year_asmr1_vec <- df$Year[df$ASMR_Mean<1]
    year_asmr1 <- ifelse(
      length(year_asmr1_vec)==0,
      "not achieved 2050",
      paste0("year:", min(year_asmr1_vec))
    )
    
    # WHO target
    who_target <- ifelse(asir_2050<4, "achieved", "not achieved")
    
    # Crear fila resumen
    row <- data.frame(
      Country = country_name,
      Scenario = sheet,
      ASIR_2022 = round(asir_2022,2),
      ASIR_2050 = round(asir_2050,2),
      Pct_Change_ASIR = pct_change_asir,
      Annual_Reduction_ASIR = annual_reduction_asir,
      ASIR_Min = asir_min,
      Year_ASIR_Min = year_asir_min,
      ASIR_2045 = asir_2045,
      Abs_Reduction_ASIR = abs_reduction_asir,
      ASMR_2022 = round(asmr_2022,2),
      ASMR_2050 = round(asmr_2050,2),
      Pct_Change_ASMR = pct_change_asmr,
      Annual_Reduction_ASMR = annual_reduction_asmr,
      ASMR_Min = asmr_min,
      Year_ASMR_Min = year_asmr_min,
      ASMR_2045 = asmr_2045,
      Abs_Reduction_ASMR = abs_reduction_asmr,
      Ratio_ASMR_ASIR_2022 = ratio_2022,
      Ratio_ASMR_ASIR_2050 = ratio_2050,
      Year_ASIR_Less4 = year_asir4,
      Year_ASMR_Less1 = year_asmr1,
      WHO_Target_2050 = who_target
    )
    
    results_list[[length(results_list)+1]] <- row
  }
}

# Consolidar
final_df <- bind_rows(results_list)

# Guardar
write.xlsx(final_df, "Projections_Summary_AllCountries.xlsx", overwrite=TRUE)

message("✅ Archivo resumen creado: Projections_Summary_AllCountries.xlsx")
