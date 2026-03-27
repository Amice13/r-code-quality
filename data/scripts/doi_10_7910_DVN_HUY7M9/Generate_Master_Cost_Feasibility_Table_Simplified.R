# ================================================================
# SCRIPT: Generate_Master_Cost_Feasibility_Table_Simplified.R
# ================================================================

library(openxlsx)
library(dplyr)

# Set working directory
setwd("/Users/mauriciocuello/Desktop/CervicalCancer_Eradication_Pipeline")

# Load the already consolidated table
effort <- read.xlsx("Countries_Effort_Financial_Feasibility.xlsx")

# Recalculate % GDP if needed (optional)
effort <- effort %>%
  mutate(
    Pct_GDP = ifelse(!is.na(GDP_Total_USD) & GDP_Total_USD >0,
                     round((Avg_Annual_HIV_Adjusted / GDP_Total_USD)*100,2), NA)
  )

# Rearrange and rename columns for clarity
summary <- effort %>%
  select(
    Country, Scenario,
    Total_Nominal,
    Total_Adjusted,
    Total_HIV_Adjusted,
    Avg_Annual_HIV_Adjusted,
    Pct_GDP,
    Pct_Health_Budget,
    Pct_Education_Budget,
    Effort_Level,
    Feasibility
  ) %>%
  arrange(Country, Scenario)

# Save as Excel
write.xlsx(summary, "Master_Costs_Feasibility_Consolidated.xlsx", row.names=FALSE)

message("\n✅ Master table generated: Master_Costs_Feasibility_Consolidated.xlsx")
