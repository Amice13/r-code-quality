# Instalar paquetes si es necesario
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")

library(openxlsx)
library(dplyr)

# Directorio de trabajo
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Fijar semilla reproducible
set.seed(20230420)

# 🔹 AQUÍ CAMBIAS EL PAÍS MANUALMENTE
country_name <- "Gaza"

message("📍 Simulando proyecciones para: ", country_name)

# Leer los archivos base
def_dataset <- read.xlsx("CERVI_STRUCT_PRO_Processed_Ecological_Indicators.xlsx")
scores_dataset <- read.xlsx("Country_Structural_Scores.xlsx")
incidence_dataset <- read.xlsx("Input_Incidence_StructuralScores_BaselineASR.xlsx")
mortality_dataset <- read.xlsx("Input_Mortality_StructuralScores_BaselineASR.xlsx")

# Extraer ASIR (columna correcta)
index_incidence <- grep(country_name, incidence_dataset$Country, ignore.case=TRUE)
if (length(index_incidence)==0) {
  stop(paste("⛔ Incidence data not found for", country_name))
} else {
  value_asir <- incidence_dataset[index_incidence[1], "Incidence_ASR_World_2022_Cervix"]
  if (length(value_asir)==0 || is.na(value_asir)) {
    stop(paste("⛔ Incidence value missing for", country_name))
  } else {
    ASIR_2022 <- value_asir
  }
}

# Extraer ASMR (columna correcta)
index_mortality <- grep(country_name, mortality_dataset$Country, ignore.case=TRUE)
if (length(index_mortality)==0) {
  stop(paste("⛔ Mortality data not found for", country_name))
} else {
  value_asmr <- mortality_dataset[index_mortality[1], "Mortality_ASR_World_2022_Cervix"]
  if (length(value_asmr)==0 || is.na(value_asmr)) {
    stop(paste("⛔ Mortality value missing for", country_name))
  } else {
    ASMR_2022 <- value_asmr
  }
}

# Extraer variables estructurales base
country_scores <- scores_dataset[scores_dataset$Country==country_name, ]

Infrastructure_Score <- country_scores$Health_System_Infrastructure_Score
Vaccine_Score <- country_scores$Vaccine_Implementation_Score
Education_Score <- country_scores$Education_Information_Score
Mortality_Score <- country_scores$Mortality_Longevity_Score

# Validar y asignar valores por defecto si faltan
if(length(Infrastructure_Score)==0 || is.na(Infrastructure_Score)) {
  Infrastructure_Score <- 0
  message("⚠️ Infrastructure_Score missing; set to 0")
}
if(length(Vaccine_Score)==0 || is.na(Vaccine_Score)) {
  Vaccine_Score <- 0
  message("⚠️ Vaccine_Score missing; set to 0")
}
if(length(Education_Score)==0 || is.na(Education_Score)) {
  Education_Score <- 0
  message("⚠️ Education_Score missing; set to 0")
}
if(length(Mortality_Score)==0 || is.na(Mortality_Score)) {
  Mortality_Score <- 0
  message("⚠️ Mortality_Score missing; set to 0")
}

# Buscar Annual_Percentage_Change
index_match <- grep(country_name, mortality_dataset$Country, ignore.case=TRUE)
Annual_Percentage_Change <- NA

if (length(index_match)==0) {
  message("⚠️ Annual_Percentage_Change not found for ", country_name, ". Setting default -2%.")
  Annual_Percentage_Change <- -0.02
} else {
  Annual_Percentage_Change_value <- tryCatch(
    {
      value <- mortality_dataset[index_match[1], "ZAnnual_Percentage_Change"]
      if (is.null(value) || length(value)==0 || is.na(value)) NA else value / 100
    },
    error = function(e) NA
  )
  
  if (is.na(Annual_Percentage_Change_value)) {
    message("⚠️ Annual_Percentage_Change missing or invalid for ", country_name, ". Setting default -2%.")
    Annual_Percentage_Change <- -0.02
  } else {
    Annual_Percentage_Change <- Annual_Percentage_Change_value
  }
}

# Medias y SD globales
mean_ASIR <- 18.1756
sd_ASIR <- 15.15176

mean_ASMR <- 10.0
sd_ASMR <- 7.0

years <- 2023:2050
n_years <- length(years)
iterations <- 500

# Coeficientes
coef_ASIR <- c(
  intercept = -0.130,
  infra = -0.337,
  vaccine = -0.513,
  education = -0.373,
  mortality = 0.834,
  pct_change = 0.084
)

coef_ASMR <- c(
  intercept = -0.063,
  infra = -0.596,
  vaccine = -0.185,
  education = -0.314,
  mortality = 1.116,
  pct_change = 0.055
)

# Z-scores base
Z_base_ASIR <- (ASIR_2022 - mean_ASIR) / sd_ASIR
Z_base_ASMR <- (ASMR_2022 - mean_ASMR) / sd_ASMR

# Escenarios
scenarios <- list(
  "Inertia" = list(vaccine_delta = 0, education_delta = 0, infra_delta = 0),
  "Vaccine" = list(vaccine_delta = 0.5, education_delta = 0, infra_delta = 0),
  "Empowerment" = list(vaccine_delta = 0.5, education_delta = 0.5, infra_delta = 0),
  "Progreso_Total" = list(vaccine_delta = 0.5, education_delta = 0.5, infra_delta = 0.5)
)

# Crear Workbook
wb <- createWorkbook()

# Loop escenarios
for (scenario_name in names(scenarios)) {
  scenario <- scenarios[[scenario_name]]
  
  asir_mat <- matrix(NA, nrow=n_years, ncol=iterations)
  asmr_mat <- matrix(NA, nrow=n_years, ncol=iterations)
  
  for (i in 1:iterations) {
    vaccine_series <- Vaccine_Score + cumsum(rep(scenario$vaccine_delta/n_years, n_years))
    education_series <- Education_Score + cumsum(rep(scenario$education_delta/n_years, n_years))
    infra_series <- Infrastructure_Score + cumsum(rep(scenario$infra_delta/n_years, n_years))
    
    mortality_random <- pmin(pmax(rnorm(n_years, mean=Annual_Percentage_Change, sd=0.015),0),0.10)
    mortality_series <- cumprod(1 + mortality_random)
    
    z_asir <- numeric(n_years)
    z_asmr <- numeric(n_years)
    
    for (year in 1:n_years) {
      safe_log <- ifelse(is.na(mortality_series[year]), 0, log(pmax(mortality_series[year], 1e-6)))
      
      delta_infra <- coef_ASIR["infra"] * (infra_series[year] - Infrastructure_Score)
      delta_vaccine <- coef_ASIR["vaccine"] * (vaccine_series[year] - Vaccine_Score)
      delta_education <- coef_ASIR["education"] * (education_series[year] - Education_Score)
      delta_mortality <- coef_ASIR["mortality"] * safe_log
      delta_pct <- coef_ASIR["pct_change"] * mortality_random[year]
      
      z_asir[year] <- Z_base_ASIR + delta_infra + delta_vaccine + delta_education + delta_mortality + delta_pct
      
      delta_infra_m <- coef_ASMR["infra"] * (infra_series[year] - Infrastructure_Score)
      delta_vaccine_m <- coef_ASMR["vaccine"] * (vaccine_series[year] - Vaccine_Score)
      delta_education_m <- coef_ASMR["education"] * (education_series[year] - Education_Score)
      delta_mortality_m <- coef_ASMR["mortality"] * safe_log
      delta_pct_m <- coef_ASMR["pct_change"] * mortality_random[year]
      
      z_asmr[year] <- Z_base_ASMR + delta_infra_m + delta_vaccine_m + delta_education_m + delta_mortality_m + delta_pct_m
    }
    
    asir_real <- pmax(z_asir * sd_ASIR + mean_ASIR, 1.0)
    asmr_real <- pmax(z_asmr * sd_ASMR + mean_ASMR, 0.5)
    
    asir_mat[,i] <- asir_real
    asmr_mat[,i] <- asmr_real
  }
  
  asir_mean <- apply(asir_mat, 1, mean)
  asir_lower <- apply(asir_mat, 1, quantile, 0.025)
  asir_upper <- apply(asir_mat, 1, quantile, 0.975)
  
  asmr_mean <- apply(asmr_mat, 1, mean)
  asmr_lower <- apply(asmr_mat, 1, quantile, 0.025)
  asmr_upper <- apply(asmr_mat, 1, quantile, 0.975)
  
  df_out <- data.frame(
    Year = years,
    ASIR_Mean = asir_mean,
    ASIR_Lower = asir_lower,
    ASIR_Upper = asir_upper,
    ASMR_Mean = asmr_mean,
    ASMR_Lower = asmr_lower,
    ASMR_Upper = asmr_upper
  )
  
  df_out <- rbind(
    data.frame(
      Year = 2022,
      ASIR_Mean = ASIR_2022,
      ASIR_Lower = NA,
      ASIR_Upper = NA,
      ASMR_Mean = ASMR_2022,
      ASMR_Lower = NA,
      ASMR_Upper = NA
    ),
    df_out
  )
  
  addWorksheet(wb, scenario_name)
  writeData(wb, scenario_name, df_out)
}

saveWorkbook(wb, paste0(country_name,"_Projections.xlsx"), overwrite=TRUE)
message("✅ Proyecciones generadas exitosamente para ", country_name)
