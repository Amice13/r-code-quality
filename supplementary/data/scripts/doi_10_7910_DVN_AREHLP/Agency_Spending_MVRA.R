#Installing packages
install.packages(c("tidyverse", "broom", "car", "modelsummary", "stargazer", "pandoc"))

#calling libraries
library(tidyverse)   # Data manipulation & visualization
library(broom)        # Clean regression output
library(car)          # VIF for multicollinearity checks
library(modelsummary) # Publication-ready regression tables
library(stargazer)   # Alternative regression tables
library(pandoc)  #To export to word doc (regression table)

#Importing data via filepath
agency_data <- read_csv("NAME HERE")

#Check data imports
glimpse(agency_data)
summary(agency_data)

#Formatting Whitespaces and removing NA values/extremes
names(agency_data) <- trimws(names(agency_data))

agency_data_cleaned <- agency_data %>%
  filter(Perc_Diff < 100)

#View spreadsheet to double-check
# view(agency_data)
# view(agency_data_cleaned)

#MV Regression: isolated and with interaction terms
model <- lm(Perc_Diff ~ Percieved_IDEO_EST + Dim1_DECM + Dim2_POLS, data = agency_data_cleaned)

#View Results of MV Regression "model"
summary(model)

#Clean coefficient table with confidence intervals
tidy(model, conf.int = TRUE)

#Check for multicollinearity
vif(model)

#Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

#Compare to single-variable LRM
model_ideo <- lm(Perc_Diff ~ Percieved_IDEO_EST, data = agency_data_cleaned)
model_dim1 <- lm(Perc_Diff ~ Dim1_DECM, data = agency_data_cleaned)
model_dim2 <- lm(Perc_Diff ~ Dim2_POLS, data = agency_data_cleaned)

#Publication-ready regression table
reg_view <- modelsummary(
  list(
    "Ideology Only" = model_ideo,
    "Dim1 Only" = model_dim1,
    "Dim2 Only" = model_dim2,
    "Full Model" = model
  ),
  stars = TRUE,
  title = "Multivariate Regression: Predictors of Percent Difference in Federal Spending",
  output = "INPUT HERE"
)

print(reg_view)
