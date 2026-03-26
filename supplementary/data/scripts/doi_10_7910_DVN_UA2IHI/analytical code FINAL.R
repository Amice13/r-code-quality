# Define required packages
required_packages <- c("caret", "dplyr", "ggplot2", "glmnet", "GPArotation", "lavaan", 
                       "mice", "openxlsx", "patchwork", "pdp", "psych", "randomForest", 
                       "scales", "svglite", "writexl")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# Set Your own with the dataset
setwd("C:\\...")

# For reproducibility
set.seed(2012)

# Load the data
data <- read.xlsx("WA_data_v1v2.xlsx", sheet = 1)

# Filter the dataframe to keep only the teachers
data <- data %>%
  filter(Job_position %in% c("učitel/ka 1. stupně", "učitel/ka 2. stupně"))

# Transforming the data
data$Gender <- ifelse(data$Gender == "žena", 1,
                      ifelse(data$Gender == "muž", 0, NA))

data$Job_position <- ifelse(data$Job_position == "učitel/ka 2. stupně", 1,
                      ifelse(data$Job_position == "učitel/ka 1. stupně", 0, NA))

data$V1_Caring_for_close_person <- ifelse(
  data$V1_Caring_for_close_person %in% c(
    "pečuji o nezaopatřené dítě (nejdéle do 26. roku věku)",
    "pečuji o stárnoucí rodiče či jiné rodinné příslušníky",
    "pečuji o nezaopatřené dítě i o stárnoucí rodiče či jiné rodinné příslušníky"
  ), 1,
  ifelse(data$V1_Caring_for_close_person == "žádná z uvedených možností", 0, NA)
)

# Initialize results dataframe
results_df <- data.frame(
  Scale = character(),
  Cronbach_Alpha = numeric(),
  CFI = numeric(),
  TLI = numeric(),
  RMSEA = numeric(),
  SRMR = numeric(),
  stringsAsFactors = FALSE
)

add_scale_cfa <- function(scale_name, item_names, cfa_model, factor_names) {
  # Step 1: Extract and convert items to numeric
  items <- data[, item_names]
  items <- as.data.frame(lapply(items, function(x) as.numeric(as.character(x))))
  
  # Step 2: Calculate Cronbach's alpha
  alpha_check <- psych::alpha(items)
  
  # Step 3: Prepare temporary dataset
  data_temp <- data
  data_temp[, item_names] <- items
  
  # Step 4: Fit CFA model
  fit <- tryCatch({
    lavaan::cfa(cfa_model, data = data_temp, missing = "fiml", estimator = "MLR")
  }, error = function(e) {
    warning(paste("CFA failed for scale:", scale_name))
    return(NULL)
  })
  
  if (!is.null(fit) && lavInspect(fit, "converged")) {
    fit_vals <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
    
    # Step 5: Add factor scores to global data
    scores <- lavPredict(fit)
    for (f in colnames(scores)) {
      data[[f]] <<- as.numeric(scores[, f])
    }
    
    # Step 6: Add overall result
    results_df <<- rbind(results_df, data.frame(
      Scale = scale_name,
      Cronbach_Alpha = round(alpha_check$total$raw_alpha, 3),
      CFI = round(fit_vals["cfi"], 3),
      TLI = round(fit_vals["tli"], 3),
      RMSEA = round(fit_vals["rmsea"], 3),
      SRMR = round(fit_vals["srmr"], 3)
    ))
    
    # Step 7: Add subscale alphas if applicable
    if (length(factor_names) > 1) {
      for (f in factor_names) {
        item_list <- inspect(fit, "std")$lambda
        factor_items <- rownames(item_list)[which(item_list[, f] != 0)]
        if (length(factor_items) >= 2) {
          alpha_sub <- tryCatch(psych::alpha(items[, factor_items])$total$raw_alpha, error = function(e) NA)
          results_df <<- rbind(results_df, data.frame(
            Scale = paste0(scale_name, " - ", f),
            Cronbach_Alpha = round(alpha_sub, 3),
            CFI = NA,
            TLI = NA,
            RMSEA = NA,
            SRMR = NA
          ))
        }
      }
    }
  } else {
    results_df <<- rbind(results_df, data.frame(
      Scale = scale_name,
      Cronbach_Alpha = round(alpha_check$total$raw_alpha, 3),
      CFI = NA,
      TLI = NA,
      RMSEA = NA,
      SRMR = NA
    ))
  }
}

### Job Security
# Define the item names
job_insec_items <- c("V1_Job_insecurity_01", "V1_Job_insecurity_02")

# Compute Cronbach's alpha
job_alpha <- psych::alpha(data[, job_insec_items], check.keys = TRUE)$total$raw_alpha

# Fit CFA model
job_model <- '
  JobInsecurity =~ V1_Job_insecurity_01 + V1_Job_insecurity_02
'
job_fit <- lavaan::cfa(job_model, data = data, missing = "fiml", estimator = "MLR")

# 4. Extracting factor scores
job_scores <- tryCatch({
  lavPredict(job_fit)[, 1]
}, error = function(e) {
  warning("Factor scores could not be extracted. Returning row means instead.")
  rowMeans(data[, job_insec_items], na.rm = TRUE)
})

# Store factor scores into the dataset
data$JobInsecurity <- job_scores

# Add to results_df (fit indices set to NA for 2-item model)
results_df <- rbind(
  results_df,
  data.frame(
    Scale = "Job Insecurity",
    Cronbach_Alpha = round(job_alpha, 3),
    CFI = NA,
    TLI = NA,
    RMSEA = NA,
    SRMR = NA
  )
)

# Life Satisfaction
add_scale_cfa(
  "Life Satisfaction",
  c("V1_Life_satis_01", "V1_Life_satis_02", "V1_Life_satis_03", "V1_Life_satis_04", "V1_Life_satis_05"),
  'LifeSatisfaction =~ V1_Life_satis_01 + V1_Life_satis_02 + V1_Life_satis_03 + V1_Life_satis_04 + V1_Life_satis_05',
  "LifeSatisfaction"
)

# Job Satisfaction
add_scale_cfa(
  "Job Satisfaction",
  c("V1_Job_satis_01", "V1_Job_satis_02", "V1_Job_satis_03", "V1_Job_satis_04", "V1_Job_satis_05"),
  'jobSatisfaction =~ V1_Job_satis_01 + V1_Job_satis_02 + V1_Job_satis_03 + V1_Job_satis_04 + V1_Job_satis_05',
  "jobSatisfaction"
)

# Work Engagement
add_scale_cfa(
  "Work Engagement",
  c("V1_Work_engagement_01", "V1_Work_engagement_02", "V1_Work_engagement_03"),
  'Work_Engagement_T1 =~ V1_Work_engagement_01 + V1_Work_engagement_02 + V1_Work_engagement_03',
  "Work_Engagement_T1"
)

# Resilience
add_scale_cfa(
  "Resilience",
  paste0("V1_Resilience_0", 1:6),
  'Resilience_T1 =~ V1_Resilience_01 + V1_Resilience_02 + V1_Resilience_03 + V1_Resilience_04 + V1_Resilience_05 + V1_Resilience_06',
  "Resilience_T1"
)

# Turnover
add_scale_cfa(
  "Turnover",
  c("V1_Turnover_01", "V1_Turnover_02", "V1_Turnover_03"),
  'Turnover_T1 =~ V1_Turnover_01 + V1_Turnover_02 + V1_Turnover_03',
  "Turnover_T1"
)

# Burnout
add_scale_cfa(
  "Burnout",
  paste0("V1_Burnout_", sprintf("%02d", 1:14)),
  '
    Burnout_Factor1_T1 =~ V1_Burnout_01 + V1_Burnout_02 + V1_Burnout_03 + V1_Burnout_04 + V1_Burnout_05 + V1_Burnout_06
    Burnout_Factor2_T1 =~ V1_Burnout_07 + V1_Burnout_08 + V1_Burnout_09 + V1_Burnout_10 + V1_Burnout_11
    Burnout_Factor3_T1 =~ V1_Burnout_12 + V1_Burnout_13 + V1_Burnout_14
    Burnout_T1 =~ Burnout_Factor1_T1 + Burnout_Factor2_T1 + Burnout_Factor3_T1
  ',
  c("Burnout_Factor1_T1", "Burnout_Factor2_T1", "Burnout_Factor3_T1", "Burnout_T1")
)

# Self-Efficacy
add_scale_cfa(
  "Self-Efficacy",
  paste0("V1_Self_efficacy_", sprintf("%02d", 1:12)),
  '
    SE_Factor1 =~ V1_Self_efficacy_02 + V1_Self_efficacy_03 + V1_Self_efficacy_04 + V1_Self_efficacy_11
    SE_Factor2 =~ V1_Self_efficacy_05 + V1_Self_efficacy_09 + V1_Self_efficacy_10 + V1_Self_efficacy_12
    SE_Factor3 =~ V1_Self_efficacy_01 + V1_Self_efficacy_06 + V1_Self_efficacy_07 + V1_Self_efficacy_08
    Self_Efficacy =~ SE_Factor1 + SE_Factor2 + SE_Factor3
  ',
  c("SE_Factor1", "SE_Factor2", "SE_Factor3", "Self_Efficacy")
)

# Locus of Control
add_scale_cfa(
  "Locus of Control",
  paste0("V1_Locus_of_control_0", 1:8),
  '
    LOC_Factor1 =~ V1_Locus_of_control_03 + V1_Locus_of_control_04 + V1_Locus_of_control_06 + V1_Locus_of_control_08
    LOC_Factor2 =~ V1_Locus_of_control_01 + V1_Locus_of_control_02 + V1_Locus_of_control_05 + V1_Locus_of_control_07
  ',
  c("LOC_Factor1", "LOC_Factor2")
)

# PWA
add_scale_cfa(
  "PWA (Perceived Work Ability)",
  paste0("V1_Work_ability_", c(
    "03", "05", "06", "07", "08", "10", "12", "13", "15",
    "17", "18", "19", "20", "22", "23", "24", "25", "26", "27", "28", "29"
  )),
  '
    # First-order
    PWA_Factor1_T1 =~ V1_Work_ability_05 + V1_Work_ability_06 + V1_Work_ability_07 + V1_Work_ability_08
    PWA_Factor2_T1 =~ V1_Work_ability_03 + V1_Work_ability_10 + V1_Work_ability_12 + V1_Work_ability_13 + V1_Work_ability_15
    PWA_Factor3_T1 =~ V1_Work_ability_18 + V1_Work_ability_19 + V1_Work_ability_24 + V1_Work_ability_25
    PWA_Factor4_T1 =~ V1_Work_ability_20 + V1_Work_ability_22 + V1_Work_ability_23
    PWA_Factor5_T1 =~ V1_Work_ability_17 + V1_Work_ability_26 + V1_Work_ability_27 + V1_Work_ability_28 + V1_Work_ability_29

    # Second-order
    PWA_T1 =~ PWA_Factor1_T1 + PWA_Factor2_T1 + PWA_Factor3_T1 + PWA_Factor4_T1 + PWA_Factor5_T1
  ',
  c("PWA_Factor1_T1", "PWA_Factor2_T1", "PWA_Factor3_T1", "PWA_Factor4_T1", "PWA_Factor5_T1", "PWA_T1")
)

# CPQ Subscale: Influence
add_scale_cfa(
  "CPQ - Influence",
  paste0("V1_CPQ_0", 1:5, "_Influence"),
  '
    Influence =~ V1_CPQ_01_Influence + V1_CPQ_02_Influence + V1_CPQ_03_Influence + V1_CPQ_04_Influence + V1_CPQ_05_Influence
  ',
  "Influence"
)

# CPQ Subscale: Quantitative Demands
add_scale_cfa(
  "CPQ - Quantitative Demands",
  paste0("V1_CPQ_0", 6:9, "_Quantitative_demands"),
  '
    Quantitative_Demands =~ V1_CPQ_06_Quantitative_demands + V1_CPQ_07_Quantitative_demands + 
                            V1_CPQ_08_Quantitative_demands + V1_CPQ_09_Quantitative_demands
  ',
  "Quantitative_Demands"
)

# CPQ Subscale: Cognitive Demands
add_scale_cfa(
  "CPQ - Cognitive Demands",
  paste0("V1_CPQ_", 10:13, "_Cognitive_demands"),
  '
    Cognitive_Demands =~ V1_CPQ_10_Cognitive_demands + V1_CPQ_11_Cognitive_demands + 
                         V1_CPQ_12_Cognitive_demands + V1_CPQ_13_Cognitive_demands
  ',
  "Cognitive_Demands"
)

# CPQ Subscale: Emotional Demands
add_scale_cfa(
  "CPQ - Emotional Demands",
  paste0("V1_CPQ_", 14:16, "_Emotional_demands"),
  '
    Emotional_Demands =~ V1_CPQ_14_Emotional_demands + V1_CPQ_15_Emotional_demands + 
                         V1_CPQ_16_Emotional_demands
  ',
  "Emotional_Demands"
)

# CPQ Subscale: Sense of Community
add_scale_cfa(
  "CPQ - Sense of Community",
  paste0("V1_CPQ_", 17:19, "_Sense_of_Community"),
  '
    Sense_of_Community =~ V1_CPQ_17_Sense_of_Community + V1_CPQ_18_Sense_of_Community + 
                          V1_CPQ_19_Sense_of_Community
  ',
  "Sense_of_Community"
)

# CPQ Subscale: Support from Supervisor
add_scale_cfa(
  "CPQ - Support Supervisor",
  paste0("V1_CPQ_", 20:22, "_Social_support_supervisor"),
  '
    Support_Supervisor =~ V1_CPQ_20_Social_support_supervisor + V1_CPQ_21_Social_support_supervisor + 
                          V1_CPQ_22_Social_support_supervisor
  ',
  "Support_Supervisor"
)

# CPQ Subscale: Support from Colleagues
add_scale_cfa(
  "CPQ - Support Colleagues",
  paste0("V1_CPQ_", 23:25, "_Social_support_colleagues"),
  '
    Support_Colleagues =~ V1_CPQ_23_Social_support_colleagues + V1_CPQ_24_Social_support_colleagues + 
                          V1_CPQ_25_Social_support_colleagues
  ',
  "Support_Colleagues"
)

# CPQ Subscale: Work-Life Conflict
add_scale_cfa(
  "CPQ - Work-Life Conflict",
  paste0("V1_CPQ_", 26:28, "_Work_life_conflict"),
  '
    Work_Life_Conflict =~ V1_CPQ_26_Work_life_conflict + V1_CPQ_27_Work_life_conflict + 
                          V1_CPQ_28_Work_life_conflict
  ',
  "Work_Life_Conflict"
)

# Save to Excel file
write_xlsx(results_df, path = "fit_indices.xlsx")

# Outcome
y <- data$Turnover_T1

# Select predictors only
predictors <- c("Gender", "Age", "Years_of_practice", "Job_position", "Class_teacher", "Mentor_teacher",
                "V1_Caring_for_close_person", "LifeSatisfaction", "jobSatisfaction",
                "PWA_Factor1_T1", "PWA_Factor2_T1", "PWA_Factor3_T1", "PWA_Factor4_T1", "PWA_Factor5_T1",
                "SE_Factor1", "SE_Factor2", "SE_Factor3",
                "Burnout_Factor1_T1", "Burnout_Factor2_T1", "Burnout_Factor3_T1", "Resilience_T1",
                "Influence", "Quantitative_Demands", "Cognitive_Demands", "Emotional_Demands", 
                "Sense_of_Community", "Support_Supervisor", "Support_Colleagues", "Work_Life_Conflict",
                "Work_Engagement_T1", "LOC_Factor1", "LOC_Factor2",
                "V1_Health_total", "V1_Health_physical", "V1_Health_mental", "JobInsecurity")

# Build predictor matrix
X_raw <- data[, predictors]

# Run single imputation using PMM
imputed <- mice(X_raw, method = "cart", m = 1, maxit = 20, seed = 2012)

# Extract completed dataset
X_imputed <- complete(imputed, 1)

# Define binary variables
binary_vars <- c("Gender", "Class_teacher", "Mentor_teacher", "V1_Caring_for_close_person")
numeric_vars <- setdiff(predictors, binary_vars)

# Standardize numeric predictors
X_imputed_scaled <- X_imputed
X_imputed_scaled[, numeric_vars] <- scale(X_imputed_scaled[, numeric_vars])

# Create X matrix and y vector
X_matrix <- as.matrix(X_imputed_scaled)
y_vector <- data$Turnover_T1

# Make sure outcome is present
X_imputed_scaled$Turnover_T1 <- data$Turnover_T1

# Train/test split
train_idx <- sample(seq_len(nrow(X_imputed_scaled)), size = 0.7 * nrow(X_imputed_scaled))

train_data <- X_imputed_scaled[train_idx, ]
test_data <- X_imputed_scaled[-train_idx, ]

rf_model <- randomForest(Turnover_T1 ~ ., data = train_data, importance = TRUE, ntree = 10000)

# Predict on test set
rf_preds <- predict(rf_model, newdata = test_data)

# R²
rss_rf <- sum((rf_preds - test_data$Turnover_T1)^2)
tss_rf <- sum((test_data$Turnover_T1 - mean(test_data$Turnover_T1))^2)
r2_rf <- 1 - rss_rf / tss_rf
cat("Random Forest R² =", round(r2_rf, 3), "\n")

varImpPlot(rf_model, main = "Random Forest Variable Importance")

importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# Order by importance (using %IncMSE or IncNodePurity)
top_vars <- importance_df[order(importance_df$`%IncMSE`, decreasing = TRUE), ]

# View all
head(top_vars, 36)

train_data$random_noise <- rnorm(nrow(train_data))
rf_null <- randomForest(Turnover_T1 ~ ., data = train_data, importance = TRUE)
importance(rf_null)["random_noise", ]

mse_cutoff <- importance(rf_null)["random_noise", "%IncMSE"]

important_vars <- importance(rf_model)
important_vars_df <- as.data.frame(important_vars)
important_vars_df$Variable <- rownames(important_vars_df)

# Keep only variables with higher importance than random noise
selected_vars <- important_vars_df[important_vars_df$`%IncMSE` > mse_cutoff, ]

# View them
selected_vars[order(selected_vars$`%IncMSE`, decreasing = TRUE), ]

top_predictors <- selected_vars$Variable

train_selected <- train_data[, c(top_predictors, "Turnover_T1")]
test_selected  <- test_data[, c(top_predictors, "Turnover_T1")]

rf_selected <- randomForest(
  Turnover_T1 ~ .,
  data = train_selected,
  importance = TRUE,
  ntree = 100
)

rf_preds_selected <- predict(rf_selected, newdata = test_selected)

rss_sel <- sum((rf_preds_selected - test_selected$Turnover_T1)^2)
tss_sel <- sum((test_selected$Turnover_T1 - mean(test_selected$Turnover_T1))^2)
r2_sel <- 1 - rss_sel / tss_sel

cat("Random Forest R² with selected variables =", round(r2_sel, 3), "\n")

control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
result <- rfe(
  train_data[, top_predictors],
  train_data$Turnover_T1,
  sizes = 5:36,
  rfeControl = control
)

plot(result)
predictors(result)  # Best minimal subset

final_vars <- predictors(result)

# Complete name mapping
name_map <- c(
  "jobSatisfaction" = "Job Satisfaction",
  "Burnout_Factor1_T1" = "Burnout: Physical Exhaustion",
  "Support_Supervisor" = "Supervisor Support",
  "Quantitative_Demands" = "Quantitative Job Demands",
  "Sense_of_Community" = "Sense of Community at Work",
  "Work_Engagement_T1" = "Work Engagement",
  "Burnout_Factor3_T1" = "Burnout: Emotional Exhaustion",
  "PWA_Factor3_T1" = "PWA: Teacher-Staff Interaction ",
  "JobInsecurity" = "Job Insecurity",
  "Burnout_Factor2_T1" = "Burnout: Cognitive Weariness",
  "Work_Life_Conflict" = "Work-Life Conflict",
  "PWA_Factor5_T1" = "PWA: Non-Teaching Responsibilities",
  "Influence" = "Autonomy",
  "PWA_Factor2_T1" = "PWA: Teaching Organization",
  "PWA_Factor4_T1" = "PWA: Navigating Difficult Situations",
  "V1_Health_mental" = "Self-Rated Health: Mental",
  "LOC_Factor2" = "Locus of Control: Internal",
  "Age" = "Age",
  "Support_Colleagues" = "Coworker Support",
  "Cognitive_Demands" = "Cognitive Job Demands",
  "SE_Factor2" = "TSE: Instructional Strategies",
  "Emotional_Demands" = "Emotional Job Demands",
  "Resilience_T1" = "Resilience",
  "SE_Factor1" = "TSE: Student Engagement",
  "Years_of_practice" = "Years of Experience",
  "PWA_Factor1_T1" = "PWA: Instructional Management",
  "LOC_Factor1" = "Locus of Control: External",
  "SE_Factor3" = "TSE: Classroom Management ",
  "LifeSatisfaction" = "Life Satisfaction",
  "Job_position" = "Educational Level Taught",
  "Mentor_teacher" = "Mentor Teacher",
  "Class_teacher" = "Class Teacher",
  "V1_Caring_for_close_person" = "Caregiving Responsibilities",
  "V1_Health_physical" = "Self-Rated Health: Physical",
  "Gender" = "Gender",
  "V1_Health_total" = "Self-Rated Health: Overall"
)

# Variable importance dataframe from your fitted rf_model
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# Add display names and selection color
importance_df <- importance_df %>%
  mutate(
    DisplayName = ifelse(Variable %in% names(name_map), name_map[Variable], Variable),
    Selected = ifelse(Variable %in% final_vars, "selected", "not selected")
  )

# Arrange and convert DisplayName to ordered factor
importance_df <- importance_df %>%
  arrange(`%IncMSE`) %>%
  mutate(DisplayName = factor(DisplayName, levels = DisplayName))

# Export plot as SVG
svg("rf_variable_importance.svg", width = 14, height = 10)

ggplot(importance_df, aes(x = DisplayName, y = `%IncMSE`, color = Selected)) +
  geom_segment(aes(xend = DisplayName, y = 0, yend = `%IncMSE`),
               size = 2.5, alpha = 0.5) +
  geom_point(size = 6, alpha = 0.5) +
  geom_hline(yintercept = mse_cutoff, color = "#f98125", linetype = "dashed", linewidth = 2) +
  scale_color_manual(values = c("selected" = "#2c599d", "not selected" = "grey70")) +
  coord_flip() +
  labs(
    title = "random forest variable importance (%IncMSE)",
    x = "variable",
    y = "% increase in MSE",
    color = "variable type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

dev.off()

train_final <- train_data[, c(final_vars, "Turnover_T1")]
test_final  <- test_data[,  c(final_vars, "Turnover_T1")]

rf_final <- randomForest(
  Turnover_T1 ~ .,
  data = train_final,
  importance = TRUE,
  ntree = 10000
)

rf_preds_final <- predict(rf_final, newdata = test_final)

rss_final <- sum((rf_preds_final - test_final$Turnover_T1)^2)
tss_final <- sum((test_final$Turnover_T1 - mean(test_final$Turnover_T1))^2)
r2_final  <- 1 - rss_final / tss_final

cat("Final model R² =", round(r2_final, 3), "\n")

# Apply the mapping
renamed_vars <- unname(name_map[final_vars])

# pdp plots making
pdp_plots <- list()

for (var_name in final_vars) {
  message("Processing variable: ", var_name)
  
  tryCatch({
    pd <- partial(rf_final, pred.var = var_name, train = train_final, grid.resolution = 30)
    pd_df <- as.data.frame(pd)
    
    # Ensure valid PDP structure
    if (var_name %in% names(pd_df) && "yhat" %in% names(pd_df)) {
      p <- ggplot(pd_df, aes_string(x = var_name, y = "yhat")) +
        geom_point(color = "#2c599d", size = 2, alpha = 0.5) +
        geom_smooth(method = "loess", color = "#f98125", se = FALSE, size = 1.5, span = 0.5) +
        geom_rug(data = train_final, aes_string(x = var_name), sides = "b", alpha = 0.25, inherit.aes = FALSE) +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        labs(title = name_map[[var_name]], x = NULL, y = "predicted turnover") +
        theme_minimal(base_size = 10)
        theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.line.x.bottom = element_line(color = "black", size = 0.5),
          axis.line.y.left = element_line(color = "black", size = 0.5),
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(face = "bold")
        )
      
      pdp_plots[[var_name]] <- p
    } else {
      warning("Skipping: PDP returned invalid structure for ", var_name)
    }
  }, error = function(e) {
    warning("Skipping variable ", var_name, " due to error: ", e$message)
  })
}

# Export each plot as SVG
# for (var_name in names(pdp_plots)) {
#   file_name <- paste0("pdp_svgs/pdp_", var_name, ".svg")
#  
#   ggsave(filename = file_name,
#          plot = pdp_plots[[var_name]],
#          width = 1500, height = 1000, units = "px",
#          device = "svg")
#  }

# Export each plot as PNG with white background
# for (var_name in names(pdp_plots)) {
#   file_name <- paste0("pdp_pngs/pdp_", var_name, ".png")
#  
#   ggsave(filename = file_name,
#          plot = pdp_plots[[var_name]],
#          width = 1500, height = 1000, units = "px",
#          device = "png",
#          bg = "white")  # <- ensures white background
# }

# Wrap each plot
wrapped_pdp <- lapply(pdp_plots, function(p) wrap_elements(full = p))

# Create the plot object
pdp_grid_plot <- wrap_plots(wrapped_pdp, ncol = 4, nrow = 6) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  )

# Export as SVG
ggsave(
  filename = "pdp_final_grid.svg",
  plot = pdp_grid_plot,
  width = 33,       # Adjust as needed
  height = 44,      # Adjust as needed
  units = "cm",
  dpi = 300
)