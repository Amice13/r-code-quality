# Define required packages
required_packages <- c("openxlsx", "lavaan", "blavaan", "bayestestR", "mice", "dplyr", "writexl", "ggplot2", "svglite")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# Set Your own with the dataset
setwd("C:\\Complex Networks\\Peťa")

# For reproducibility
set.seed(2012)

# Load the data
data <- read.xlsx("WA_data_v1v2.xlsx", sheet = 1)

# Filter the dataframe to keep only the teachers
data <- data %>%
  filter(Job_position %in% c("učitel/ka 1. stupně", "učitel/ka 2. stupně", "učitel/ka nerozlišeno"))

### Extract Relevant Columns and Transform Gender
# Extract relevant columns
data_subset <- data[, c(
  # Burnout T1 and T2 items
  "V1_Burnout_01", "V1_Burnout_02", "V1_Burnout_03", "V1_Burnout_04", 
  "V1_Burnout_05", "V1_Burnout_06", "V1_Burnout_07", "V1_Burnout_08", 
  "V1_Burnout_09", "V1_Burnout_10", "V1_Burnout_11", "V1_Burnout_12", 
  "V1_Burnout_13", "V1_Burnout_14", "V2_Burnout_01", "V2_Burnout_02", 
  "V2_Burnout_03", "V2_Burnout_04", "V2_Burnout_05", "V2_Burnout_06", 
  "V2_Burnout_07", "V2_Burnout_08", "V2_Burnout_09", "V2_Burnout_10", 
  "V2_Burnout_11", "V2_Burnout_12", "V2_Burnout_13", "V2_Burnout_14", 
  
  # PWA T1 and T2 items
  "V1_Work_ability_03", "V1_Work_ability_10", "V1_Work_ability_12", "V1_Work_ability_13",
  "V1_Work_ability_15", "V1_Work_ability_05", "V1_Work_ability_06", "V1_Work_ability_07",
  "V1_Work_ability_08", "V1_Work_ability_17", "V1_Work_ability_26", "V1_Work_ability_27",
  "V1_Work_ability_28", "V1_Work_ability_29", "V1_Work_ability_18", "V1_Work_ability_19",
  "V1_Work_ability_24", "V1_Work_ability_25", "V1_Work_ability_20", "V1_Work_ability_22", "V1_Work_ability_23",
  
  "V2_Work_ability_03", "V2_Work_ability_04", "V2_Work_ability_05", "V2_Work_ability_06",
  "V2_Work_ability_09", "V2_Work_ability_11", "V2_Work_ability_12", "V2_Work_ability_13",
  "V2_Work_ability_15", "V2_Work_ability_16", "V2_Work_ability_17", "V2_Work_ability_18",
  "V2_Work_ability_19", "V2_Work_ability_21", "V2_Work_ability_22", "V2_Work_ability_23",
  "V2_Work_ability_26", "V2_Work_ability_27", "V2_Work_ability_28", "V2_Work_ability_29", "V2_Work_ability_30",
  
  # Covariates
  "Gender", "Age", "Years_of_practice", "V1_Health_physical", "V1_Health_mental", 
  "V2_Health_physical", "V2_Health_mental"
)]

# Transform Gender into binary numeric
# 0 = muž, 1 = žena
data_subset$Gender <- ifelse(data_subset$Gender == "žena", 1, 0)

# Create data_noimp, keeping only rows where both V1_Burnout_01 and V2_Burnout_01 are not missing
data_noimp <- data_subset[!is.na(data_subset$V1_Burnout_01) & !is.na(data_subset$V2_Burnout_01), ]

### Perform Multiple Imputation with MICE
# Perform MICE imputation
imputed_data <- mice(data_subset, m = 10, maxit = 10, method = "pmm")

# Save imputed datasets as a list of 5 data frames
imputed_datasets <- lapply(1:10, function(i) complete(imputed_data, i))

### Define and Fit Measurement Models
# Define the measurement models
measurement_model_total <- '
  # Burnout as a whole (T1 and T2)
  # First-order factors
  Burnout_Factor1_T1 =~ V1_Burnout_01 + V1_Burnout_02 + V1_Burnout_03 + V1_Burnout_04 + V1_Burnout_05 + V1_Burnout_06
  Burnout_Factor2_T1 =~ V1_Burnout_07 + V1_Burnout_08 + V1_Burnout_09 + V1_Burnout_10 + V1_Burnout_11
  Burnout_Factor3_T1 =~ V1_Burnout_12 + V1_Burnout_13 + V1_Burnout_14
  
  # Second-order factor
  Burnout_T1 =~ Burnout_Factor1_T1 + Burnout_Factor2_T1 + Burnout_Factor3_T1
  
  # First-order factors
  Burnout_Factor1_T2 =~ V2_Burnout_01 + V2_Burnout_02 + V2_Burnout_03 + V2_Burnout_04 + V2_Burnout_05 + V2_Burnout_06
  Burnout_Factor2_T2 =~ V2_Burnout_07 + V2_Burnout_08 + V2_Burnout_09 + V2_Burnout_10 + V2_Burnout_11
  Burnout_Factor3_T2 =~ V2_Burnout_12 + V2_Burnout_13 + V2_Burnout_14
  
  # Second-order factor
  Burnout_T2 =~ Burnout_Factor1_T2 + Burnout_Factor2_T2 + Burnout_Factor3_T2

  # PWA as a whole (T1 and T2)
  # First-order factors
  PWA_Factor1_T1 =~ V1_Work_ability_05 + V1_Work_ability_06 + V1_Work_ability_07 + V1_Work_ability_08
  PWA_Factor2_T1 =~ V1_Work_ability_03 + V1_Work_ability_10 + V1_Work_ability_12 + V1_Work_ability_13 + V1_Work_ability_15
  PWA_Factor3_T1 =~ V1_Work_ability_18 + V1_Work_ability_19 + V1_Work_ability_24 + V1_Work_ability_25
  PWA_Factor4_T1 =~ V1_Work_ability_20 + V1_Work_ability_22 + V1_Work_ability_23
  PWA_Factor5_T1 =~ V1_Work_ability_17 + V1_Work_ability_26 + V1_Work_ability_27 + V1_Work_ability_28 + V1_Work_ability_29

  # Second-order factor
  PWA_T1 =~ PWA_Factor1_T1 + PWA_Factor2_T1 + PWA_Factor3_T1 + PWA_Factor4_T1 + PWA_Factor5_T1
  
  # First-order factors
  PWA_Factor1_T2 =~ V2_Work_ability_03 + V2_Work_ability_04 + V2_Work_ability_05 + V2_Work_ability_06
  PWA_Factor2_T2 =~ V2_Work_ability_09 + V2_Work_ability_11 + V2_Work_ability_12 + V2_Work_ability_13 + V2_Work_ability_15
  PWA_Factor3_T2 =~ V2_Work_ability_16 + V2_Work_ability_17 + V2_Work_ability_18 + V2_Work_ability_19
  PWA_Factor4_T2 =~ V2_Work_ability_21 + V2_Work_ability_22 + V2_Work_ability_23
  PWA_Factor5_T2 =~ V2_Work_ability_26 + V2_Work_ability_27 + V2_Work_ability_28 + V2_Work_ability_29 + V2_Work_ability_30
  
  # Second-order factor
  PWA_T2 =~ PWA_Factor1_T2 + PWA_Factor2_T2 + PWA_Factor3_T2 + PWA_Factor4_T2 + PWA_Factor5_T2
'

# Fit the measurement models to all 10 imputed datasets
imputed_datasets_with_scores <- lapply(imputed_datasets, function(data) {
  fit_total <- cfa(measurement_model_total, data = data, std.lv = TRUE, optim.method = "nlminb")
  factor_scores_total <- lavPredict(fit_total)
  cbind(data, factor_scores_total)  # Add factor scores to the dataset
})

# Fit the measurement model to non-imputed dataset
fit_total_noimp <- cfa(measurement_model_total, data = data_noimp, std.lv = TRUE, optim.method = "nlminb")
factor_scores_total_noimp <- lavPredict(fit_total_noimp)

# New datasets
if (nrow(data_noimp) == nrow(factor_scores_total_noimp)) {
  # Bind the factor scores to the filtered dataset
  data_noimp_val <- cbind(data_noimp, factor_scores_total_noimp)
} else {
  stop("Row counts do not match between data_noimp and factor_scores_total_noimp!")
}

# Identify numeric columns, excluding 'gender'
numeric_cols <- setdiff(names(data_noimp_val)[sapply(data_noimp_val, is.numeric)], "gender")

# Standardize numeric columns (z-score transformation)
data_noimp_val[numeric_cols] <- scale(data_noimp_val[numeric_cols])

# Standardize All Variables (except Gender)
imputed_datasets_standardized <- lapply(imputed_datasets_with_scores, function(data) {
  data_standardized <- data
  data_standardized[] <- lapply(data, function(x) {
    if (is.numeric(x) && !identical(x, data$Gender)) {
      scale(x, center = TRUE, scale = TRUE) # Center and scale (z-score transformation)
    } else {
      x
    }
  })
  return(data_standardized)
})

# Define the CLPM model with individual PWA factors
clpm_model <- '
  # Cross-lagged effects
  Burnout_T2 ~ PWA_T1 + Gender + Age + Years_of_practice
  PWA_T2 ~ Burnout_T1 + Gender + Age + Years_of_practice
  
  # Autoregressive effects
  Burnout_T2 ~ Burnout_T1
  PWA_T2 ~ PWA_T1
  
  # Cross-sectional effects
  PWA_T1 ~~ Burnout_T1
  PWA_T2 ~~ Burnout_T2

  # Covariance between demographic variables
  Years_of_practice ~~ Age
'

# Define the null model
HS.model_null <- '
  PWA_T1 ~~ PWA_T1 
  PWA_T2 ~~ PWA_T2 
  Burnout_T1 ~~ Burnout_T1
  Burnout_T2 ~~ Burnout_T2
  Gender ~~ Gender
  Age ~~ Age
  Years_of_practice ~~ Years_of_practice
'

# Fit the Bayesian SEM model on the non-imputed data
bsem_fit_noimp <- blavaan::bsem(
  model = clpm_model,
  data = data_noimp_val,
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model and check fit
summary(bsem_fit_noimp, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit_noimp)
fit_null_noimp <- bcfa(HS.model_null, data = data_noimp_val, n.chains = 5, burnin=1000, sample=5000)
gl_fits_all_noimp <- blavFitIndices(bsem_fit_noimp, baseline.model = fit_null_noimp)
summary(gl_fits_all_noimp)

# Save the bsem_fit object
saveRDS(bsem_fit_noimp, file = "bsem_fit_noimp.rds")

# Access raw posterior samples
raw_posterior_samples_noimp <- bsem_fit_noimp@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df_noimp <- bind_rows(lapply(raw_posterior_samples_noimp, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns_noimp <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df_noimp), value = TRUE)
effects_df_noimp <- posterior_df_noimp %>%
  select(all_of(effect_columns_noimp))

# Partable
pt <- partable(bsem_fit_noimp)[,c("lhs","op","rhs","pxnames")]

# Create new column names by combining lhs, op, and rhs
pt$new_colnames <- paste(pt$lhs, pt$op, pt$rhs, sep = "")

# Format 'pxnames' for matching: 
#   Replace '[' or ']' with a period and remove any trailing period
pt$pxnames_clean <- gsub("\\[|\\]", ".", pt$pxnames)
pt$pxnames_clean <- gsub("\\.$", "", pt$pxnames_clean)

# Format column names in effects_df_aggregate similarly (remove trailing dots)
colnames_clean <- gsub("\\.$", "", colnames(effects_df_noimp))

# Create a named vector mapping from the cleaned pxnames to the new column names
rename_map <- setNames(pt$new_colnames, pt$pxnames_clean)

# Rename columns in effects_df_aggregate that match the names in the mapping
colnames(effects_df_noimp) <- ifelse(colnames_clean %in% names(rename_map),
                                         rename_map[colnames_clean],
                                         colnames_clean)

# Print new column names to verify
print(colnames(effects_df_noimp))

# Describe posteriors
posteriors <- describe_posterior(effects_df_noimp, centrality = "mean")
print_md(posteriors, digits = 2)

# Extract and rename the relevant columns
results_df_noimp <- posteriors %>%
  dplyr::select(Parameter, Mean, CI_low, CI_high, pd) %>%
  dplyr::rename(
    effect_name = Parameter,
    mean = Mean,
    CI_lower = CI_low,
    CI_upper = CI_high,
    PD = pd
  )

# View the resulting dataframe
print(results_df_noimp)

# Export the final results as Excel file
write.xlsx(results_df_noimp, "final_results_noimp.xlsx")

# Fit the Bayesian SEM model on the first imputed dataset
bsem_fit1 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[1]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model and check fit
summary(bsem_fit1, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit1)
fit_null1 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[1]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all1 <- blavFitIndices(bsem_fit1, baseline.model = fit_null1)
summary(gl_fits_all1)

# Save the bsem_fit object
saveRDS(bsem_fit1, file = "bsem_fit1.rds")

# Access raw posterior samples
raw_posterior_samples1 <- bsem_fit1@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df1 <- bind_rows(lapply(raw_posterior_samples1, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns1 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df1), value = TRUE)
effects_df1 <- posterior_df1 %>%
  select(all_of(effect_columns1))

write.xlsx(effects_df1, "posterior_distributions1.xlsx")

# Fit the Bayesian SEM model on the second imputed dataset
bsem_fit2 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[2]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit2, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit2)
fit_null2 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[2]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all2 <- blavFitIndices(bsem_fit2, baseline.model = fit_null2)
summary(gl_fits_all2)

# Save the bsem_fit object
saveRDS(bsem_fit2, file = "bsem_fit2.rds")

# Access raw posterior samples
raw_posterior_samples2 <- bsem_fit2@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df2 <- bind_rows(lapply(raw_posterior_samples2, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns2 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df2), value = TRUE)
effects_df2 <- posterior_df2 %>%
  select(all_of(effect_columns2))

write.xlsx(effects_df2, "posterior_distributions2.xlsx")

# Fit the Bayesian SEM model on the third imputed dataset
bsem_fit3 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[3]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit3, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit3)
fit_null3 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[3]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all3 <- blavFitIndices(bsem_fit3, baseline.model = fit_null3)
summary(gl_fits_all3)

# Save the bsem_fit object
saveRDS(bsem_fit3, file = "bsem_fit3.rds")

# Access raw posterior samples
raw_posterior_samples3 <- bsem_fit3@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df3 <- bind_rows(lapply(raw_posterior_samples3, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns3 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df3), value = TRUE)
effects_df3 <- posterior_df3 %>%
  select(all_of(effect_columns3))

write.xlsx(effects_df3, "posterior_distributions3.xlsx")

# Fit the Bayesian SEM model on the fourth imputed dataset
bsem_fit4 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[4]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit4, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit4)
fit_null4 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[4]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all4 <- blavFitIndices(bsem_fit4, baseline.model = fit_null4)
summary(gl_fits_all4)

# Save the bsem_fit object
saveRDS(bsem_fit4, file = "bsem_fit4.rds")

# Access raw posterior samples
raw_posterior_samples4 <- bsem_fit4@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df4 <- bind_rows(lapply(raw_posterior_samples4, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns4 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df4), value = TRUE)
effects_df4 <- posterior_df4 %>%
  select(all_of(effect_columns4))

write.xlsx(effects_df4, "posterior_distributions4.xlsx")

# Fit the Bayesian SEM model on the fifth imputed dataset
bsem_fit5 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[5]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit5, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit5)
fit_null5 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[5]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all5 <- blavFitIndices(bsem_fit5, baseline.model = fit_null5)
summary(gl_fits_all5)

# Save the bsem_fit object
saveRDS(bsem_fit5, file = "bsem_fit5.rds")

# Access raw posterior samples
raw_posterior_samples5 <- bsem_fit5@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df5 <- bind_rows(lapply(raw_posterior_samples5, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns5 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df5), value = TRUE)
effects_df5 <- posterior_df5 %>%
  select(all_of(effect_columns5))

write.xlsx(effects_df5, "posterior_distributions5.xlsx")

# Fit the Bayesian SEM model on the sixth imputed dataset
bsem_fit6 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[6]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit6, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit6)
fit_null6 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[6]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all6 <- blavFitIndices(bsem_fit6, baseline.model = fit_null6)
summary(gl_fits_all6)

# Save the bsem_fit object
saveRDS(bsem_fit6, file = "bsem_fit6.rds")

# Access raw posterior samples
raw_posterior_samples6 <- bsem_fit6@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df6 <- bind_rows(lapply(raw_posterior_samples6, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns6 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df6), value = TRUE)
effects_df6 <- posterior_df6 %>%
  select(all_of(effect_columns6))

write.xlsx(effects_df6, "posterior_distributions6.xlsx")

# Fit the Bayesian SEM model on the seventh imputed dataset
bsem_fit7 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[7]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit7, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit7)
fit_null7 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[7]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all7 <- blavFitIndices(bsem_fit7, baseline.model = fit_null7)
summary(gl_fits_all7)

# Save the bsem_fit object
saveRDS(bsem_fit7, file = "bsem_fit7.rds")

# Access raw posterior samples
raw_posterior_samples7 <- bsem_fit7@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df7 <- bind_rows(lapply(raw_posterior_samples7, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns7 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df7), value = TRUE)
effects_df7 <- posterior_df7 %>%
  select(all_of(effect_columns7))

write.xlsx(effects_df7, "posterior_distributions7.xlsx")

# Fit the Bayesian SEM model on the seventh imputed dataset
bsem_fit7 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[7]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit7, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit7)
fit_null7 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[7]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all7 <- blavFitIndices(bsem_fit7, baseline.model = fit_null7)
summary(gl_fits_all7)

# Save the bsem_fit object
saveRDS(bsem_fit7, file = "bsem_fit7.rds")

# Access raw posterior samples
raw_posterior_samples7 <- bsem_fit7@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df7 <- bind_rows(lapply(raw_posterior_samples7, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns7 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df7), value = TRUE)
effects_df7 <- posterior_df7 %>%
  select(all_of(effect_columns7))

write.xlsx(effects_df7, "posterior_distributions7.xlsx")

# Fit the Bayesian SEM model on the eigth imputed dataset
bsem_fit8 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[8]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit8, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit8)
fit_null8 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[8]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all8 <- blavFitIndices(bsem_fit8, baseline.model = fit_null8)
summary(gl_fits_all8)

# Save the bsem_fit object
saveRDS(bsem_fit8, file = "bsem_fit8.rds")

# Access raw posterior samples
raw_posterior_samples8 <- bsem_fit8@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df8 <- bind_rows(lapply(raw_posterior_samples8, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns8 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df8), value = TRUE)
effects_df8 <- posterior_df8 %>%
  select(all_of(effect_columns8))

write.xlsx(effects_df8, "posterior_distributions8.xlsx")

# Fit the Bayesian SEM model on the ninth imputed dataset
bsem_fit9 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[9]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit9, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit9)
fit_null9 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[9]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all9 <- blavFitIndices(bsem_fit9, baseline.model = fit_null9)
summary(gl_fits_all9)

# Save the bsem_fit object
saveRDS(bsem_fit9, file = "bsem_fit9.rds")

# Access raw posterior samples
raw_posterior_samples9 <- bsem_fit9@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df9 <- bind_rows(lapply(raw_posterior_samples9, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns9 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df9), value = TRUE)
effects_df9 <- posterior_df9 %>%
  select(all_of(effect_columns9))

write.xlsx(effects_df9, "posterior_distributions9.xlsx")

# Fit the Bayesian SEM model on the tenth imputed dataset
bsem_fit10 <- blavaan::bsem(
  model = clpm_model,
  data = imputed_datasets_standardized[[10]],  # Use the first imputed dataset
  n.chains = 5, burnin=1000, sample=5000,
  control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 20)
)

# Summarize the model
summary(bsem_fit10, fit.measures = TRUE, standardized = TRUE)
pairs(bsem_fit10)
fit_null10 <- bcfa(HS.model_null, data = imputed_datasets_standardized[[10]], n.chains = 5, burnin=1000, sample=5000)
gl_fits_all10 <- blavFitIndices(bsem_fit10, baseline.model = fit_null10)
summary(gl_fits_all10)

# Save the bsem_fit object
saveRDS(bsem_fit10, file = "bsem_fit10.rds")

# Access raw posterior samples
raw_posterior_samples10 <- bsem_fit10@external$mcmcout@sim$samples

# Combine all chains into a single data frame
# Each chain is a list, so we'll bind them row-wise
posterior_df10 <- bind_rows(lapply(raw_posterior_samples10, as.data.frame))

# Select only columns corresponding to effects
# Assuming effects are named with patterns like 'bet_sign' or 'Psi_cov'
effect_columns10 <- grep("bet_sign|Psi_cov|Psi_var", colnames(posterior_df10), value = TRUE)
effects_df10 <- posterior_df10 %>%
  select(all_of(effect_columns10))

write.xlsx(effects_df10, "posterior_distributions10.xlsx")

# Load only in case You do not have the objects in the environment
# effects_df1 <- read.xlsx("posterior_distributions1.xlsx")
# effects_df2 <- read.xlsx("posterior_distributions2.xlsx")
# effects_df3 <- read.xlsx("posterior_distributions3.xlsx")
# effects_df4 <- read.xlsx("posterior_distributions4.xlsx")
# effects_df5 <- read.xlsx("posterior_distributions5.xlsx")
# effects_df6 <- read.xlsx("posterior_distributions6.xlsx")
# effects_df7 <- read.xlsx("posterior_distributions7.xlsx")
# effects_df8 <- read.xlsx("posterior_distributions8.xlsx")
# effects_df9 <- read.xlsx("posterior_distributions9.xlsx")
# effects_df10 <- read.xlsx("posterior_distributions10.xlsx")

# Pool the posterior distributions
effects_df_aggregate <- rbind(effects_df1, effects_df2, effects_df3, effects_df4, effects_df5, effects_df6, effects_df7, effects_df8, effects_df9, effects_df10)

# Partable
pt <- partable(bsem_fit10)[,c("lhs","op","rhs","pxnames")]

# Create new column names by combining lhs, op, and rhs
pt$new_colnames <- paste(pt$lhs, pt$op, pt$rhs, sep = "")

# Format 'pxnames' for matching: 
#   Replace '[' or ']' with a period and remove any trailing period
pt$pxnames_clean <- gsub("\\[|\\]", ".", pt$pxnames)
pt$pxnames_clean <- gsub("\\.$", "", pt$pxnames_clean)

# Format column names in effects_df_aggregate similarly (remove trailing dots)
colnames_clean <- gsub("\\.$", "", colnames(effects_df_aggregate))

# Create a named vector mapping from the cleaned pxnames to the new column names
rename_map <- setNames(pt$new_colnames, pt$pxnames_clean)

# Rename columns in effects_df_aggregate that match the names in the mapping
colnames(effects_df_aggregate) <- ifelse(colnames_clean %in% names(rename_map),
                                         rename_map[colnames_clean],
                                         colnames_clean)

# Print new column names to verify
print(colnames(effects_df_aggregate))

# Describe posteriors
posteriors <- describe_posterior(effects_df_aggregate, centrality = "mean")
print_md(posteriors, digits = 2)

# Extract and rename the relevant columns
results_df <- posteriors %>%
  dplyr::select(Parameter, Mean, CI_low, CI_high, pd) %>%
  dplyr::rename(
    effect_name = Parameter,
    mean = Mean,
    CI_lower = CI_low,
    CI_upper = CI_high,
    PD = pd
  )

# View the resulting dataframe
print(results_df)

# Export the final results as Excel file
write.xlsx(results_df, "final_results.xlsx")

# Visualizing the posterior distributions
# Calculate density data
density_data <- density(effects_df_aggregate$"Burnout_T2~PWA_T1")
dat <- data.frame(x = density_data$x, y = density_data$y)

# visu
svglite("T2burnoutT1PWA.svg", width = 4, height = 2)
# Plot with conditional coloring
ggplot(data = dat, aes(x = x, y = y)) +
  geom_area(data = dat, aes(x = x, y = y), fill = "#1e90ff", alpha = 1) +  # Red area for x < 0
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.8) +  # Vertical line at x = 0
  scale_x_continuous(limits = c(-0.40, 0.40), breaks = c(-0.3, 0, 0.3)) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
    panel.grid = element_blank(),  # No gridlines
    axis.title = element_blank(),  # Remove axis titles
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # Bottom line
    axis.line.y = element_blank(),  # Remove y-axis line
    axis.text = element_text(face = "bold"),  # Bold font for axis labels
    legend.position = "none",  # Remove legend
    axis.ticks.x = element_line(color = "black", linewidth = 0.8),  # Add bottom ticks
  ) +
  coord_cartesian(ylim = c(0.60, NA))
dev.off()  # Close the device

# Calculate density data
density_data <- density(effects_df_aggregate$"PWA_T2~Burnout_T1")
dat <- data.frame(x = density_data$x, y = density_data$y)

# visu
svglite("T2PWAT1burnout.svg", width = 4, height = 2)
# Plot with conditional coloring
ggplot(data = dat, aes(x = x, y = y)) +
  geom_area(data = dat, aes(x = x, y = y), fill = "#FF1E1F", alpha = 1) +  # Red area for x < 0
  geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.8) +  # Vertical line at x = 0
  scale_x_continuous(limits = c(-0.40, 0.40), breaks = c(-0.3, 0, 0.3)) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
    panel.grid = element_blank(),  # No gridlines
    axis.title = element_blank(),  # Remove axis titles
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.line.x = element_line(color = "black", linewidth = 0.8),  # Bottom line
    axis.line.y = element_blank(),  # Remove y-axis line
    axis.text = element_text(face = "bold"),  # Bold font for axis labels
    legend.position = "none",  # Remove legend
    axis.ticks.x = element_line(color = "black", linewidth = 0.8),  # Add bottom ticks
  ) +
  coord_cartesian(ylim = c(0.35, NA))
dev.off()  # Close the device
