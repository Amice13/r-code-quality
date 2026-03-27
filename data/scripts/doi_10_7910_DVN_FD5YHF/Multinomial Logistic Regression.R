#====NOTE: ALL DIRECTORIES, FILE NAMES AND VARIABLES SHOULD BE CHANGED TO MATCH YOUR FILES

# ===========================
#  Load Required Libraries
# ===========================
libs <- c("terra", "readxl", "dplyr", "nnet", "DescTools", "writexl", "ggplot2", "car", "caret")
lapply(libs, require, character.only = TRUE)

# ===========================
#  Load Data
# ===========================

# Load the SQI raster for the top layer
sqi_top <- rast("C:/Users/USER/Desktop/The final analysis/Transformed Rasters/Final_SQI_Map.tif")
plot(sqi_top)

#====================== Load the point shapefile
env_data_sf <- vect("C:/Users/USER/Desktop/END-Soil data analysis/Shapefiles/New_data_mlr_projected.shp")
env_data_sf_proj <- project(env_data_sf, crs(sqi_top))
points(env_data_sf_proj, col = "red", pch = 16)

#====================== Extract the values using the points
sqi_values <- terra::extract(sqi_top, env_data_sf_proj)

#====================== Join SQI Values Back to Your Attribute Table
env_data_df <- cbind(as.data.frame(env_data_sf_proj), SQI_top = sqi_values[, 2])


#=================================
# Assumption checks
#=================================

#========================= MULTICOLLINEARITY
env_data_df$Total_Abun <- as.numeric(as.character(env_data_df$Total_Abun))
env_data_df$Shade_Tree <- as.numeric(as.character(env_data_df$Shade_Tree))
env_data_df$Canopy_Cov <- as.numeric(as.character(env_data_df$Canopy_Cov))
env_data_df$SQI_top <- as.numeric(as.character(env_data_df$SQI_top))

cor(env_data_df[c("Total_Abun", "Shade_Tree", "Canopy_Cov", "SQI_top")], use = "complete.obs")

#===================== VARIANCE INFLATION FACTOR
library(car)
lm_model <- lm(Total_Abun ~ Shade_Tree + Canopy_Cov + SQI_top, data = env_data_df)
vif(lm_model)


#==================== Linearity
### NOTE: SHOULD BE REPEATED FOR EACH VARIABLE
#=======Total abundance

library(nnet)

env_data_df$log_Total_Abun <- log(env_data_df$Total_Abun + 1)  # Add +1 to avoid log(0)
env_data_df$interaction_TA <- env_data_df$Total_Abun * env_data_df$log_Total_Abun

#======== Recode outcome as factor if needed
env_data_df$Species_Ri <- as.factor(env_data_df$Species_Ri)

#========= Fit model with interaction term
model_bt <- multinom(Species_Ri ~ Total_Abun + interaction_TA + Shade_Tree + Canopy_Cov, data = env_data_df)

summary(model_bt)

z_vals <- summary(model_bt)$coefficients / summary(model_bt)$standard.errors
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
round(p_vals, 4)


#============   Linearity
#============  Shade tree diversity

# Add small constant to avoid log(0)
env_data_df$log_Shade_Tree <- log(env_data_df$Shade_Tree + 1)
env_data_df$interaction_ST <- env_data_df$Shade_Tree * env_data_df$log_Shade_Tree

env_data_df$log_Canopy_Cov <- log(env_data_df$Canopy_Cov + 1)
env_data_df$interaction_CC <- env_data_df$Canopy_Cov * env_data_df$log_Canopy_Cov


# Make sure response is a factor
env_data_df$Species_Ri <- as.factor(env_data_df$Species_Ri)

#==== Fit model
model_bt2 <- multinom(Species_Ri ~ Total_Abun + Shade_Tree + interaction_ST + 
                        Canopy_Cov + interaction_CC + SQI_top, data = env_data_df)


z_vals2 <- summary(model_bt2)$coefficients / summary(model_bt2)$standard.errors
p_vals2 <- 2 * (1 - pnorm(abs(z_vals2)))
round(p_vals2, 4)


########################################################################
#==========================  Multinomial model

top_mlr_model <- multinom(SQI_top ~ Shade_Tree + Canopy_Cov + Total_Abun, data = env_data_df)
summary(top_mlr_model)


# Null model for comparison
top_null_model <- multinom(SQI_top ~ 1, data = env_data_df)


# ===========================
# Model Evaluation
# ===========================
cat("Likelihood Ratio Test:\n")
print(anova(top_null_model, top_mlr_model, test = "Chisq"))


top_mlr_model <- multinom(SQI_top ~ Shade_Tree + Canopy_Cov + Total_Abun, data = env_data_df, model = TRUE)


cat("\nPseudo R-squared values:\n")
print(PseudoR2(top_mlr_model, which = c("CoxSnell", "Nagelkerke", "McFadden")))


# ==========================================
# Coefficient Summary, Odds Ratios, Confidence Intervals
# =========================================

coefs <- summary(top_mlr_model)$coefficients
se    <- summary(top_mlr_model)$standard.errors

#================= Z-values, p-values
z_vals <- coefs / se
pvals <- 2 * (1 - pnorm(abs(z_vals)))
wald  <- z_vals^2

#================ Odds Ratios and 95% CI
expB       <- exp(coefs)
lower_CI   <- exp(coefs - 1.96 * se)
upper_CI   <- exp(coefs + 1.96 * se)

#================ Assemble results in a data frame
results_list <- list()

for (i in 1:nrow(coefs)) {
  temp <- data.frame(
    Category   = rownames(coefs)[i],
    Predictor  = colnames(coefs),
    B          = coefs[i, ],
    Std.Error  = se[i, ],
    Wald       = wald[i, ],
    p.value    = pvals[i, ],
    Exp.B      = expB[i, ],
    Lower.CI   = lower_CI[i, ],
    Upper.CI   = upper_CI[i, ]
  )
  results_list[[i]] <- temp
}

#============= Final summary table
model_summary <- do.call(rbind, results_list)

#============ Print to console
cat("\n📌 Coefficient Summary with 95% CI for Exp(B):\n")
print(model_summary)

# ===========================
# Confusion Matrix and Accuracy
# ===========================

# Predict classes
pred_classes <- predict(model_bt2, type = "class")

# Check factor consistency
env_data_df$Species_Ri <- as.factor(env_data_df$Species_Ri)

# Confusion matrix
conf_mat <- caret::confusionMatrix(as.factor(pred_classes), env_data_df$Species_Ri)

cat("\n📊 Confusion Matrix:\n")
print(conf_mat$table)

overall_acc <- cat("\n✅ Overall Accuracy:", round(conf_mat$overall["Accuracy"], 4), "\n")

# ===========================
# Interpretation of Significant Predictors
# ===========================

cat("\n📝 Interpretation of Significant Predictors (p < 0.05):\n")
sig_results <- subset(model_summary, p.value < 0.05)

if (nrow(sig_results) == 0) {
  cat("No significant predictors found.\n")
} else {
  for (i in 1:nrow(sig_results)) {
    cat(sprintf(
      "- For category '%s' vs reference, predictor '%s': OR = %.2f (95%% CI: %.2f – %.2f), p = %.3f\n",
      sig_results$Category[i], sig_results$Predictor[i], sig_results$Exp.B[i],
      sig_results$Lower.CI[i], sig_results$Upper.CI[i], sig_results$p.value[i]
    ))
  }
}

###Extract confusion matrix as a data frame
conf_mat_df <- as.data.frame(conf_mat$table)

# Extract overall accuracy as a named data frame
overall_acc_df <- data.frame(Accuracy = conf_mat$overall["Accuracy"])

#=====================  Save results
# Save everything into an Excel workbook
write_xlsx(list(
  Model_Fitting        = as.data.frame(anova(top_null_model, top_mlr_model, test = "Chisq")),
  Pseudo_R2            = as.data.frame(t(PseudoR2(top_mlr_model, which = c("CoxSnell", "Nagelkerke", "McFadden")))),
  Odds_Ratios_and_CI   = model_summary,
  Confusion_Matrix     = conf_mat_df,
  Overall_Accuracy     = overall_acc_df
), path = "C:/Users/USER/Desktop/END-Soil data analysis/mlr results.xlsx")

