## ============================================================================
## Principal Component Regression (PCR) & Partial Least Squares Regression (PLSR)
## ============================================================================

# Install and load the necessary packages
install.packages("pls")
install.packages("boot")
install.packages("dplyr")
install.packages("caret")
install.packages("readxl")

library(pls)
library(boot)
library(dplyr)
library(readxl)
library(caret)

# Prepare your data
# Load your dataset
data <- read_excel("C:/Users/a_pig/OneDrive/Pos_Doutorado_Adm/Special Issue/dataf.xlsx")
View(data)

# Prepare the data
predictors <- data[, c("DAC", "DAD", "DFI", "DFR", "DLI", "DOP", "DPR", "DRI", "DTA", "DTT", "NET")]
outcomes <- data[, c("ECO", "ENV", "SOC")]

# ============================================================================
# Principal Component Regression (PCR)
# ============================================================================

# Fit PCR model for each outcome
pcr_model_ECO <- pcr(ECO ~ ., data = data.frame(predictors, ECO = outcomes$ECO), validation = "CV")
pcr_model_ENV <- pcr(ENV ~ ., data = data.frame(predictors, ENV = outcomes$ENV), validation = "CV")
pcr_model_SOC <- pcr(SOC ~ ., data = data.frame(predictors, SOC = outcomes$SOC), validation = "CV")

# Print summary of the models
summary(pcr_model_ECO)
summary(pcr_model_ENV)
summary(pcr_model_SOC)

# Define the PCR model function to be bootstrapped
pcr_bootstrap <- function(data, indices) {
  d <- data[indices, ]
  pcr_model <- pcr(ECO ~ ., data = d, ncomp = 2, validation = "CV")
  as.vector(coef(pcr_model, ncomp = 2))
}

# Perform bootstrapping for PCR
set.seed(123)
boot_pcr_results <- boot(data = data.frame(predictors, ECO = outcomes$ECO), statistic = pcr_bootstrap, R = 1000)

# Extract the bootstrapped coefficients and compute summary statistics for PCR
boot_pcr_coefs <- boot_pcr_results$t
pcr_coef_means <- colMeans(boot_pcr_coefs)
pcr_coef_sds <- apply(boot_pcr_coefs, 2, sd)

# Combine the PCR results into a data frame
pcr_bootstrap_summary <- data.frame(
  Predictor = colnames(predictors),
  Mean = pcr_coef_means,
  SD = pcr_coef_sds
)

# Display the PCR results
print(pcr_bootstrap_summary)
print(boot_pcr_results)

# ============================================================================
# Partial Least Squares Regression (PLSR)
# ============================================================================

# Fit PLSR model for each outcome
plsr_model_ECO <- plsr(ECO ~ ., data = data.frame(predictors, ECO = outcomes$ECO), validation = "CV")
plsr_model_ENV <- plsr(ENV ~ ., data = data.frame(predictors, ENV = outcomes$ENV), validation = "CV")
plsr_model_SOC <- plsr(SOC ~ ., data = data.frame(predictors, SOC = outcomes$SOC), validation = "CV")

# Print summary of the models
summary(plsr_model_ECO)
summary(plsr_model_ENV)
summary(plsr_model_SOC)

# Define the PLSR model function to be bootstrapped
plsr_bootstrap <- function(data, indices) {
  d <- data[indices, ]
  plsr_model <- plsr(ECO ~ ., data = d, ncomp = 2, validation = "CV")
  as.vector(coef(plsr_model, ncomp = 2))
}

# Perform bootstrapping for PLSR
set.seed(123)
boot_plsr_results <- boot(data = data.frame(predictors, ECO = outcomes$ECO), statistic = plsr_bootstrap, R = 1000)

# Extract the bootstrapped coefficients and compute summary statistics for PLSR
boot_plsr_coefs <- boot_plsr_results$t
plsr_coef_means <- colMeans(boot_plsr_coefs)
plsr_coef_sds <- apply(boot_plsr_coefs, 2, sd)

# Combine the PLSR results into a data frame
plsr_bootstrap_summary <- data.frame(
  Predictor = colnames(predictors),
  Mean = plsr_coef_means,
  SD = plsr_coef_sds
)

# Display the PLSR results using xtable
print(plsr_bootstrap_summary)
print(boot_plsr_results)

# ============================================================================
# Model Performance Evaluation
# ============================================================================
# Predict using PCR model
pcr_predictions <- predict(pcr_model_SOC, ncomp = 11, newdata = data)

# Calculate PCR R-squared
pcr_r_squared <- cor(pcr_predictions, data$SOC)^2

# Calculate PCR MAE
pcr_mae <- mean(abs(pcr_predictions - data$SOC))

# Calculate PCR MSE
pcr_mse <- mean((pcr_predictions - data$SOC)^2)

# Calculate PCR RMSE
pcr_rmse <- sqrt(pcr_mse)

# Print PCR results
cat("PCR R-squared:", pcr_r_squared, "\n")
cat("PCR MAE:", pcr_mae, "\n")
cat("PCR MSE:", pcr_mse, "\n")
cat("PCR RMSE:", pcr_rmse, "\n")

 #==============================================================

# Predict using PLSR model
plsr_predictions <- predict(plsr_model_SOC, ncomp = 11, newdata = data)

# Calculate PLSR R-squared
plsr_r_squared <- cor(plsr_predictions, data$SOC)^2

# Calculate PLSR MAE
plsr_mae <- mean(abs(plsr_predictions - data$SOC))

# Calculate PLSR MSE
plsr_mse <- mean((plsr_predictions - data$SOC)^2)

# Calculate PLSR RMSE
plsr_rmse <- sqrt(plsr_mse)

# Print PLSR results
cat("PLSR R-squared:", plsr_r_squared, "\n")
cat("PLSR MAE:", plsr_mae, "\n")
cat("PLSR MSE:", plsr_mse, "\n")
cat("PLSR RMSE:", plsr_rmse, "\n")
