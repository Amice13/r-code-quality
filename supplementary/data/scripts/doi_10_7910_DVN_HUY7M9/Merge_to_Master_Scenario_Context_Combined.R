# Cargar librerías
library(readxl)
library(dplyr)
library(writexl)

# Ruta base Economic_Indicators
path_indicators <- "/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline"

# Ruta base Economic_Analyses
path_analyses <- "/Users/mauriciocuello/Desktop/LATAM 90_70_90"

# 1. Leer Economic_Indicators_Cleaned
econ_indicators <- read_excel(file.path(path_indicators, "Economic_Indicators_Cleaned.xlsx"))

# 2. Leer Economic_Analyses_Cleaned
econ_analyses <- read_excel(file.path(path_analyses, "Economic_Analyses_Cleaned.xlsx"))

# 3. Seleccionar variables relevantes de cada base
indicators_sel <- econ_indicators %>%
  select(
    Country,
    Alpha_3_code,
    Cluster_membership,
    Region,
    HPV_Vaccination_Programme_Scale,
    Educational_Index_2022,
    Gender_Inequality_Index_2022,
    Political_stability,
    Corruption_perception_index,
    Life_expectancy_birth
  )

analyses_sel <- econ_analyses %>%
  select(
    Country,
    Alpha_3_code,
    Healthcare_Access_Quality_Index_2019_Age_Stand,
    Physician_density_per_1000_2017,
    GNI_per_capita,
    obesity_prevalence,
    Smoking_prevalence_fem,
    Diabetes_prevalence
  )

# 4. Combinar por Country y Alpha_3_code
combined_context <- indicators_sel %>%
  left_join(
    analyses_sel %>% select(-Alpha_3_code), # evitamos duplicar Alpha_3_code
    by = "Country"
  )

# 5. Revisar
glimpse(combined_context)

# 6. Exportar resultado
write_xlsx(combined_context, file.path(path_indicators, "Combined_Economic_Context.xlsx"))

cat("✅ Archivo combinado creado en carpeta CervicalCancer_Eradication_Pipeline con nombre Combined_Economic_Context.xlsx\n")

library(readxl)
library(dplyr)

# Ruta
path_data <- "/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline"

# Leer Master
master <- read_excel(file.path(path_data, "Master_Costs_Feasibility_Consolidated.xlsx")) %>%
  select(
    Country, Scenario,
    Total_Nominal, Total_Adjusted, Total_HIV_Adjusted, Avg_Annual_HIV_Adjusted,
    Pct_GDP, Pct_Health_Budget, Pct_Education_Budget,
    Effort_Level, Feasibility
  )

# Leer Effort
effort <- read_excel(file.path(path_data, "Countries_Effort_Financial_Feasibility.xlsx")) %>%
  select(
    Country, Scenario,
    GDP_Per_Capita_2023_USD, GDP_Total_USD,
    health_expenditure_GDP_percentage, Education_expenditure_GDP_percent
  )

# Combinar ambos
costs_combined <- master %>%
  left_join(effort, by = c("Country", "Scenario")) %>%
  mutate(
    Years_to_Finance_2pct_GDP = ifelse(!is.na(GDP_Total_USD) & GDP_Total_USD > 0,
                                       Total_HIV_Adjusted / (0.02 * GDP_Total_USD),
                                       NA_real_)
  )

# Verificar resultado
glimpse(costs_combined)

# Guardar en Excel
library(writexl)
write_xlsx(costs_combined, "/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline/COSTS_Combined_Master_Effort.xlsx")

# O guardar en CSV
write.csv(costs_combined, "/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline/COSTS_Combined_Master_Effort.csv", row.names = FALSE)

# Cargar librerías
library(readxl)
library(dplyr)
library(writexl)

# Definir ruta donde tienes los archivos
path_data <- "/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline"

# Leer el dataset de COSTOS y ESCENARIOS
costs <- read_excel(file.path(path_data, "COSTS_Combined_Master_Effort.xlsx"))

# Leer el dataset ECONÓMICO / CONTEXTUAL
context <- read_excel(file.path(path_data, "Combined_Economic_Context.xlsx"))

# Unir ambos datasets por COUNTRY
combined_final <- costs %>%
  left_join(context, by = "Country")

# Verificar columnas resultantes
glimpse(combined_final)

# Guardar Excel final
write_xlsx(
  combined_final,
  path = file.path(path_data, "Master_Scenario_Context_Combined.xlsx")
)

# Mensaje confirmación
cat("✅ Archivo combinado generado correctamente: Master_Scenario_Context_Combined.xlsx\n")

