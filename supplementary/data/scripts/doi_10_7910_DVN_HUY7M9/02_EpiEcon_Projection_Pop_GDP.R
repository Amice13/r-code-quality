# ==========================================================
# 02_EpiEcon_Projection_Pop_GDP.R
# Proyección año a año (2023–2050) de población total,
# mujeres en riesgo (>15 años), GDP per capita y GDP total.
# ==========================================================

# Instalar paquetes si es necesario
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")

library(openxlsx)
library(dplyr)

# Ajusta tu directorio de trabajo
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Leer base limpia
data <- read.xlsx("Economic_Indicators_Cleaned.xlsx")

# Verificar nombres de campos importantes
required_cols <- c("Country", "GDP_Per_Capita_2023_USD", "GDP_Growth_CAGR_2021_2023",
                   "population", "percentage_population_growth", "Women_at_risk_Older_15_1")
if (!all(required_cols %in% colnames(data))) {
  stop("⛔ La base no tiene todos los campos requeridos. Revisa los nombres de columnas.")
}

# Crear workbook consolidado
wb <- createWorkbook()

# Años de proyección
years <- 2023:2050
n_years <- length(years)

# Loop por país
for (i in 1:nrow(data)) {
  
  country <- data$Country[i]
  pop_base <- as.numeric(data$population[i])
  pop_growth_pct <- as.numeric(data$percentage_population_growth[i]) / 100
  gdp_per_capita_base <- as.numeric(data$GDP_Per_Capita_2023_USD[i])
  gdp_growth_pct <- as.numeric(data$GDP_Growth_CAGR_2021_2023[i]) / 100
  female_risk_base <- as.numeric(data$Women_at_risk_Older_15_1[i])
  
  # Validación
  if (is.na(pop_base) | is.na(pop_growth_pct) | is.na(gdp_per_capita_base) | is.na(gdp_growth_pct)) {
    message("⚠️ Datos faltantes en ", country, ". Se omite.")
    next
  }
  
  # Vectores
  pop_vec <- numeric(n_years)
  female_vec <- numeric(n_years)
  gdp_pc_vec <- numeric(n_years)
  gdp_total_vec <- numeric(n_years)
  
  # Año base
  pop_vec[1] <- pop_base
  female_vec[1] <- female_risk_base
  gdp_pc_vec[1] <- gdp_per_capita_base
  gdp_total_vec[1] <- pop_base * gdp_per_capita_base
  
  # Proyección año a año
  for (y in 2:n_years) {
    pop_vec[y] <- pop_vec[y-1] * (1 + pop_growth_pct)
    female_vec[y] <- female_vec[y-1] * (1 + pop_growth_pct)
    gdp_pc_vec[y] <- gdp_pc_vec[y-1] * (1 + gdp_growth_pct)
    gdp_total_vec[y] <- pop_vec[y] * gdp_pc_vec[y]
  }
  
  # Tabla de salida
  df <- data.frame(
    Year = years,
    Population_Total = round(pop_vec,0),
    Women_At_Risk = round(female_vec,0),
    GDP_Per_Capita_USD = round(gdp_pc_vec,2),
    GDP_Total_USD = round(gdp_total_vec,2)
  )
  
  # Añadir hoja al workbook
  sheet_name <- substr(country,1,30)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  
  message("✅ Procesado: ", country)
}

# Guardar Excel
saveWorkbook(wb, "EpiEcon_Projection_Pop_GDP.xlsx", overwrite=TRUE)
message("🎯 Proyección completada: Archivo 'EpiEcon_Projection_Pop_GDP.xlsx' listo.")
