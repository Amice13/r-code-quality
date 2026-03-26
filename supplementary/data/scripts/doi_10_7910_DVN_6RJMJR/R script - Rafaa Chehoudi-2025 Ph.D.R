# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(broom)
library(caret)

# Read the data
data <- read_excel("Doctoral project Data.xlsx", sheet = "Data before Computation")

# Define the significance levels function
get_significance_stars <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Data Imputation using KNN
preProcess_missingdata_model <- preProcess(data, method = 'knnImpute')
data_imputed <- predict(preProcess_missingdata_model, newdata = data)

# Data Standardization
preProcess_scale_model <- preProcess(data_imputed, method = c('center', 'scale'))
data_standardized <- predict(preProcess_scale_model, newdata = data_imputed)

# Convert to data frame
data_standardized <- as.data.frame(data_standardized)

# Define variable names
variable_names <- c(
  GDP = "GDP",
  INDUS = "Industrialization",
  NRR = "Natural Resources Revenues",
  PS = "Political System",
  FSI = "Fragile States Index",
  corruption = "Political Corruption",
  HIEF = "Ethnic Fractionalization",
  RG = "Religion",
  GDI = "Gender Development Index",
  FII = "Foreign Intervention Index",
  DAR = "Development Aid Received",
  UNDEF = "UNDEF"
)

# Function to plot coefficients
plot_coefficients <- function(model, title) {
  tidy_model <- tidy(model, conf.int = TRUE)
  tidy_model <- tidy_model %>%
    mutate(term = recode(term, !!!variable_names)) %>%
    mutate(significance = sapply(p.value, get_significance_stars)) %>%
    mutate(term = paste0(term, " ", significance))
  
  ggplot(tidy_model, aes(x = estimate, y = reorder(term, estimate))) +
    geom_point(color = "red") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    theme_minimal() +
    theme(axis.title.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold")) +
    labs(title = title,
         x = "Coefficient Value",
         y = "Variables")
}

# Model (1): Economic Factors
model_1 <- lm(DS ~ GDP + INDUS + NRR + UNDEF, data = data_standardized)
plot_coefficients(model_1, "Regression Coefficients with 95% Confidence Intervals for Model (1)")

# Model (2): Political Factors
model_2 <- lm(DS ~ PS + FSI + corruption + UNDEF, data = data_standardized)
plot_coefficients(model_2, "Regression Coefficients with 95% Confidence Intervals for Model (2)")

# Model (3): Cultural Factors
model_3 <- lm(DS ~ HIEF + RG + GDI + UNDEF, data = data_standardized)
plot_coefficients(model_3, "Regression Coefficients with 95% Confidence Intervals for Model (3)")

# Model (4): International Factors
model_4 <- lm(DS ~ FII + DAR + UNDEF, data = data_standardized)
plot_coefficients(model_4, "Regression Coefficients with 95% Confidence Intervals for Model (4)")

# Model (5): All Factors (2005-2011)
data_2005_2011 <- data_standardized %>%
  filter(Year >= 2005 & Year <= 2011)
model_5 <- lm(DS ~ GDP + INDUS + NRR + PS + FSI + corruption + HIEF + RG + GDI + FII + DAR + UNDEF, data = data_2005_2011)
plot_coefficients(model_5, "Regression Coefficients with 95% Confidence Intervals for Model (5)")

# Model (6): All Factors (2012-2023)
data_2012_2023 <- data_standardized %>%
  filter(Year >= 2012 & Year <= 2023)
model_6 <- lm(DS ~ GDP + INDUS + NRR + PS + FSI + corruption + HIEF + RG + GDI + FII + DAR + UNDEF, data = data_2012_2023)
plot_coefficients(model_6, "Regression Coefficients with 95% Confidence Intervals for Model (6)")

# Model (7): North African Countries (2005-2023)
north_african_countries <- c("Tunisia", "Algeria", "Morocco", "Libya", "Mauritania")
data_north_africa <- data_standardized %>%
  filter(Country %in% north_african_countries)
model_7 <- lm(DS ~ GDP + INDUS + NRR + PS + FSI + corruption + HIEF + RG + GDI + FII + DAR + UNDEF, data = data_north_africa)
plot_coefficients(model_7, "Regression Coefficients with 95% Confidence Intervals for Model (7)")

# Model (8): Non-North African Countries (2005-2023)
data_non_north_africa <- data_standardized %>%
  filter(!Country %in% north_african_countries)
model_8 <- lm(DS ~ GDP + INDUS + NRR + PS + FSI + corruption + HIEF + RG + GDI + FII + DAR + UNDEF, data = data_non_north_africa)
plot_coefficients(model_8, "Regression Coefficients with 95% Confidence Intervals for Model (8)")

# Model (9): All Factors, All Years, All Countries
model_9 <- lm(DS ~ GDP + INDUS + NRR + PS + FSI + corruption + HIEF + RG + GDI + FII + DAR + UNDEF, data = data_standardized)
plot_coefficients(model_9, "Regression Coefficients with 95% Confidence Intervals for Model (9)")
