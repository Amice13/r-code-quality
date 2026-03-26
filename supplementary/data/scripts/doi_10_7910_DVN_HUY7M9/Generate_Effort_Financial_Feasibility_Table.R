# ================================================================
# SCRIPT: Generate_Effort_Financial_Feasibility_Table.R (fixed)
# Cervical Cancer Eradication – Effort and Feasibility Classification
# ================================================================

library(openxlsx)
library(dplyr)

# Directorio de trabajo
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Leer consolidación de costos
costs <- read.xlsx("Costs_Comparison_All_Steps.xlsx")

# Leer datos económicos
econ <- read.xlsx("Economic_Indicators_Cleaned.xlsx")

# Calcular totales y promedios
summary <- costs %>%
  group_by(Country, Scenario) %>%
  summarise(
    Total_Nominal = sum(Cost_Nominal, na.rm=TRUE),
    Total_Adjusted = sum(Cost_Total_Adjusted, na.rm=TRUE),
    Total_HIV_Adjusted = sum(Cost_Total_HIV_Adjusted, na.rm=TRUE),
    Avg_Annual_HIV_Adjusted = mean(Cost_Total_HIV_Adjusted, na.rm=TRUE)
  ) %>%
  ungroup()

# Hacer join con los datos económicos
summary <- summary %>%
  left_join(
    econ %>% select(
      Country,
      GDP_Per_Capita_2023_USD,
      population,
      health_expenditure_GDP_percentage,
      Education_expenditure_GDP_percent
    ),
    by="Country"
  ) %>%
  mutate(
    GDP_Total_USD = GDP_Per_Capita_2023_USD * population,
    Pct_Health_Budget = ifelse(!is.na(GDP_Total_USD) & !is.na(health_expenditure_GDP_percentage) & health_expenditure_GDP_percentage > 0,
                               round((Avg_Annual_HIV_Adjusted / (GDP_Total_USD * (health_expenditure_GDP_percentage/100)))*100,1),
                               NA),
    Pct_Education_Budget = ifelse(!is.na(GDP_Total_USD) & !is.na(Education_expenditure_GDP_percent) & Education_expenditure_GDP_percent >0,
                                  round((Avg_Annual_HIV_Adjusted / (GDP_Total_USD * (Education_expenditure_GDP_percent/100)))*100,1),
                                  NA)
  ) %>%
  mutate(
    Effort_Level = case_when(
      Pct_Health_Budget >10 | Pct_Education_Budget >15 ~ "Extreme",
      Pct_Health_Budget >5 | Pct_Education_Budget >10 ~ "High",
      Pct_Health_Budget >2 | Pct_Education_Budget >5 ~ "Moderate",
      Pct_Health_Budget >1 | Pct_Education_Budget >2 ~ "Low",
      TRUE ~ "Minimal"
    ),
    Feasibility = case_when(
      Effort_Level == "Extreme" ~ "Critical dependence on international aid",
      Effort_Level == "High" ~ "Requires international support",
      Effort_Level == "Moderate" ~ "Potentially feasible with domestic prioritization",
      Effort_Level %in% c("Low","Minimal") ~ "Likely feasible internally",
      TRUE ~ "Unknown"
    )
  )

# Guardar Excel final
write.xlsx(summary, "Countries_Effort_Financial_Feasibility.xlsx", row.names=FALSE)

message("\n✅ Table generated: Countries_Effort_Financial_Feasibility.xlsx")
