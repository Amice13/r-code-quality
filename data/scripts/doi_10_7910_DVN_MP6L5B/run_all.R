# The instructions for this code is in the README file of the hazardous_times dataverse 
# Pooled Data Analysis
# 4.2 Hazard Model Estimates for a Labor-Market Peak, A2.2 PH Diagnostics, and A3.1 Parametric Robustness for Labor-Market Peak Estimate, and A4 Single-Case Influence Diagnostics

library(mice)
library(dplyr)
library(ggplot2)
library(rlang)
library(survival)
library(survminer)
library(tidyverse)
library(tidyr)
library(forcats) 
library(purrr)
library(ggsurvfit)
library(coxphf)
library(mitools)
library(readxl)
library(flexsurv)

# Load Excel Data
imp_2025 <- read_excel("~/Desktop/Recession_SA_Paper/Data/data_transform_2025_test.xlsx")
#imp_2025 <-data_transform_2025_test

# Initialize the predictor matrix (default: all variables predict all others)
init <- mice(imp_2025, maxit = 0)       
pred <- init$predictorMatrix      
meth <- init$method               

# Exclude 'id' from the imputation process:
pred[, "id"] <- 0     

# Set seed for reproducibility
set.seed(123)  

# Impute missing data
imputed_data <- mice(
  predictorMatrix = pred,
  imp_2025,
  method = meth,
  m = 20,
  maxit = 5,
  print = FALSE
)

# Transfer content from un_start to event for all imp. datasets
modify_dataset <- function(df) {
  if (ncol(df) < 4) stop("Dataset has fewer than 4 columns")
  df[, 2] <- df[, 4]       
  df <- df[, -4]           
  names(df)[2] <- "event"  
  return(df)
}

# Process mids object
if (inherits(imputed_data, "mids")) {
  # 1. Extract and modify original data
  modified_original <- imputed_data$data
  modified_original[, 2] <- modified_original[, 4]
  modified_original <- modified_original[, -4]
  colnames(modified_original)[2] <- "event"
  
  # Modify all imputed datasets
  modified_imputed_list <- lapply(1:imputed_data$m, function(i) {
    modify_dataset(complete(imputed_data, i))
  })
  
  # Create long-format dataset for mids conversion
  orig_long <- cbind(.imp = 0, .id = seq_len(nrow(modified_original)), modified_original)
  imp_long <- do.call(rbind, lapply(seq_along(modified_imputed_list), function(i) {
    cbind(.imp = i, .id = seq_len(nrow(modified_imputed_list[[i]])), modified_imputed_list[[i]])
  }))
  long_format <- rbind(orig_long, imp_long)
  
  # Reconstruct mids object 
  new_mids <- as.mids(long_format)
  
  # Preserve mids metadata
  vars <- colnames(modified_original)
  new_mids$call <- imputed_data$call
  new_mids$nmis <- colSums(is.na(modified_original))
  new_mids$method <- imputed_data$method[vars]
  new_mids$predictorMatrix <- imputed_data$predictorMatrix[vars, vars, drop = FALSE]
  
  # Assign final object
  imputed_data_modified <- new_mids
} else {
  stop("Input must be a mids object")
}

# Define data transformation function
transform_to_start_stop <- function(data) {
  # [Paste your function here, as above]
  results_list <- list()
  num_rows <- nrow(data)
  
  for (i in 1:num_rows) {
    row <- data[i, ]
    id <- row$id
    event_time <- row$event
    
    covariate_start_cols <- grep("(_start| start)$", names(row), value = TRUE)
    covariate_names <- gsub("(_start| start)$", "", covariate_start_cols)
    covariate_starts <- row[, covariate_start_cols]
    
    time_points <- sort(unique(c(0, unlist(covariate_starts), event_time)))
    
    individual_data <- data.frame()
    
    for (j in 1:(length(time_points) - 1)) {
      start_time <- time_points[j]
      end_time <- time_points[j + 1]
      
      event_status <- ifelse(end_time == event_time, 1, 0)
      
      covariate_status <- data.frame(matrix(0, nrow = 1, ncol = length(covariate_names)))
      colnames(covariate_status) <- covariate_names
      
      for (k in 1:length(covariate_names)) {
        if (!is.na(covariate_starts[[k]]) && covariate_starts[[k]] < end_time) {
          covariate_status[[covariate_names[k]]] <- 1
        }
      }
      
      interval_row <- data.frame(
        id = id,
        start = start_time,
        stop = end_time,
        event = event_status,
        covariate_status
      )
      
      individual_data <- bind_rows(individual_data, interval_row)
    }
    # only keep rows up to and including event
    if(any(individual_data$event == 1)) {                                     
      individual_data <- individual_data[1:which.max(individual_data$event), ]  
    }                                                                          
    results_list[[i]] <- individual_data
  }
  bind_rows(results_list)
}

# Extract all 20 completed datasets
imputed_datasets_list <- lapply(1:20, function(i) complete(imputed_data_modified, action = i))

#  Apply transform function to each imputed dataset
transformed_imputed_datasets <- lapply(imputed_datasets_list, transform_to_start_stop)

#  Add spline to duplicate datasets 
# Calculate overall knot (median stop time for all events==1 across all datasets)
all_stops <- unlist(
  lapply(transformed_imputed_datasets, function(df) df$stop[df$event == 1])
)

knot <- 42 # median(all_stops, na.rm = TRUE)
cat("Median knot is", knot, "\n")

# Duplicate the list
transformed_imputed_datasets_spline <- transformed_imputed_datasets

# Add cs_spline column 
transformed_imputed_datasets_spline <- lapply(
  transformed_imputed_datasets_spline, function(df) {
    # Defensive coding
    if (is.factor(df$cs)) df$cs <- as.numeric(as.character(df$cs))
    
    df$cs_spline <- 0
    uniq_ids <- unique(df$id)
    
    for (id_now in uniq_ids) {
      idx_id <- which(df$id == id_now)
      idx_cs1 <- idx_id[df$cs[idx_id] == 1]
      
      if (length(idx_cs1) == 0) next
      
      first_idx <- idx_cs1[which.min(df$start[idx_cs1])]
      spline_value <- df$cs[first_idx] * pmax(df$start[first_idx] - knot, 0)
      first_pos <- which(idx_id == first_idx)
      idx_later_cs1 <- idx_id[ (seq_along(idx_id) >= first_pos) & (df$cs[idx_id] == 1) ]
      df$cs_spline[idx_later_cs1] <- spline_value
    }
    df
  }
)

# Fit spline models to all datasets (omit the term “cs_spline” from the code lines below for results in Table 4 and A4)
spline_models <- lapply(transformed_imputed_datasets_spline, function(d) {
  coxph(Surv(start, stop, event) ~ cs + cs_spline + um, data = d)
})

# Pool results
pooled_results <- summary(pool(as.mira(spline_models)), 
                          conf.int = TRUE, 
                          exponentiate = TRUE)

# Format output
final_results <- pooled_results[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
names(final_results) <- c("Term", "HR", "CI_Low", "CI_High", "p_value")

extract_model_stats <- function(model) {
  # Extract coefficients and summary
  coefs <- coef(model)
  summary_model <- summary(model)
  confints <- confint(model)
  
  # Prepare results data frame
  data.frame(
    Term = names(coefs),
    HR = exp(coefs),
    CI_Low = exp(confints[, 1]),
    CI_High = exp(confints[, 2]),
    p_value = summary_model$coefficients[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
}

# Convert to mira object and pool
mira_spline <- as.mira(spline_models)
pooled_spline <- pool(mira_spline)

# Extract summary with hazard ratios
pooled_summary <- summary(pooled_spline, conf.int = TRUE, exponentiate = TRUE)

results_table <- data.frame(
  Term = pooled_summary$term,
  Hazard_Ratio = pooled_summary$estimate,
  CI_Lower = pooled_summary$`2.5 %`,
  CI_Upper = pooled_summary$`97.5 %`,
  p_value = pooled_summary$p.value
)

colnames(results_table) <- c("Term", "Hazard Ratio", "95% CI Lower", "95% CI Upper", "p-value")
print(results_table)

# Extract results for all 20 models
all_model_results <- lapply(1:20, function(i) {
  res <- extract_model_stats(spline_models[[i]])
  cbind(Imputation = i, res)
})

print(pooled_summary)

# Combine into single data frame
combined_results <- do.call(rbind, all_model_results)

# Print formatted results
print(combined_results, digits = 3, row.names = FALSE)

# PH test (cox.zhp) for all imputed datasets
# STEP 1: Run cox.zph on each model
zph_list <- lapply(spline_models, cox.zph)

# STEP 2: Extract p-values from each cox.zph result
p_values_list <- lapply(zph_list, function(zph) {
  cz_table <- as.data.frame(zph$table)
  # Change 'cs' and 'cs_spline' to your actual term names, if different
  data.frame(
    cs = cz_table["cs", "p"],
    cs_spline = cz_table["cs_spline", "p"],
    um = cz_table["um", "p"],
    global = cz_table["GLOBAL", "p"]   
  )
})

# Combine into data frame
p_values_df <- do.call(rbind, p_values_list)

# STEP 3: Summarise results
results_summary <- data.frame(
  Term = c("cs", "cs_spline", "um", "global"),
  Median_p = c(
    median(p_values_df$cs, na.rm = TRUE),
    median(p_values_df$cs_spline, na.rm = TRUE),
    median(p_values_df$um, na.rm = TRUE),
    median(p_values_df$global, na.rm = TRUE)
  ),
  Min_p = c(
    min(p_values_df$cs, na.rm = TRUE),
    min(p_values_df$cs_spline, na.rm = TRUE),
    min(p_values_df$um, na.rm = TRUE),
    min(p_values_df$global, na.rm = TRUE)
  ),
  Max_p = c(
    max(p_values_df$cs, na.rm = TRUE),
    max(p_values_df$cs_spline, na.rm = TRUE),
    max(p_values_df$um, na.rm = TRUE),
    max(p_values_df$global, na.rm = TRUE)
  ),
  Violation_proportion = c(
    mean(p_values_df$cs < 0.05, na.rm = TRUE),
    mean(p_values_df$cs_spline < 0.05, na.rm = TRUE),
    mean(p_values_df$um < 0.05, na.rm = TRUE),
    mean(p_values_df$global < 0.05, na.rm = TRUE)
  )
)

# Print the results
print(results_summary)


# Fit Exponential Models to All 20 Datasets
exp_models <- lapply(transformed_imputed_datasets, function(d){
  ## constant-hazard exponential; coefficients are log-rates
  flexsurvreg(Surv(start, stop, event) ~ um + cs,
              data = d,
              dist = "exp")
})

# Produce a summary 

exp_mira  <- as.mira(exp_models)     # convert list -> "mira"
pooled_exp <- pool(exp_mira)         # Rubin’s pooling

get_ph_results_pooled <- function(pooled_obj){
  
  # mice::summary() gives the pooled estimates, SEs, df & P
  smry <- summary(pooled_obj)
  
  # keep the row names – they are the parameter names
  smry$term <- rownames(smry)
  
  # Drop the baseline parameter “rate” (intercept for exp. model)
  smry <- subset(smry, term != "rate")
  
  # Derive HR, CI, etc.
  beta_hat <- smry$estimate
  se_hat   <- smry$std.error
  
  out <- data.frame(
    covariate   = smry$term,
    coefficient = beta_hat,
    se_coef     = se_hat,
    HR          = exp(beta_hat),
    lower_95CI  = exp(beta_hat - 1.96*se_hat),
    upper_95CI  = exp(beta_hat + 1.96*se_hat),
    p_value     = smry$p.value,
    row.names   = NULL
  )
  out
}

results_exp_imp <- get_ph_results_pooled(pooled_exp)
results_exp_imp

# Cox–Snell residuals for the 20 imputations

# helper ----------------------------------------------------------
cox_snell <- function(model, data){
  # coefficients -------------------------------------------------
  log_rate <- model$coefficients["rate"]             # baseline (log-hazard)
  betas    <- model$coefficients[
    setdiff(names(model$coefficients), "rate")]
  
  # linear predictor for every row ------------------------------
  linpred <- log_rate
  if (length(betas) > 0){
    X <- as.matrix(data[ , names(betas), drop = FALSE])
    linpred <- linpred + X %*% betas      # vectorised
  }
  hazard_row <- exp(linpred)              # row-wise constant hazard
  
  # row-wise contribution to cum. hazard ------------------------
  contrib <- as.numeric(hazard_row) * (data$stop - data$start)
  
  # aggregate to subject level ----------------------------------
  resid  <- tapply(contrib, data$id, sum)     # Σ over that id
  event  <- tapply(data$event,   data$id, max) # 1 if ever 1
  
  out <- data.frame(id     = as.numeric(names(resid)),
                    cs_res = as.numeric(resid),
                    event  = as.numeric(event),
                    row.names = NULL)
  out
}

# apply to all 20 imputations ------------------------------------
cs_residuals <- lapply(seq_along(exp_models), function(i){
  cox_snell(exp_models[[i]], transformed_imputed_datasets[[i]])
})

# Example: diagnostic plot for imputation 1  

cs1 <- cs_residuals[[1]]
fit  <- survfit(Surv(cs_res, event) ~ 1, data = cs1)

plot(fit$time, -log(fit$surv),
     xlab = "Cox–Snell residual",
     ylab = "Estimated cumulative hazard",
     main = "Exponential PH model – imputation 1")
abline(0, 1, col = "red", lty = 2)        # 45° reference line

#  Nelson–Aalen vs. exponential fit  (all 20 imputations) 
# Generate diagnostic plots for each imputation
cs_plots <- lapply(1:20, function(i) {
  cs_data <- cs_residuals[[i]]
  fit <- survfit(Surv(cs_res, event) ~ 1, data = cs_data)
  
  ggplot() +
    geom_step(aes(x = fit$time, y = fit$cumhaz), color = "black") +
    geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed") +
    labs(title = paste("Imputation", i),
         x = "Cox-Snell Residual",
         y = "Cumulative Hazard") +
    theme_minimal() +
    coord_cartesian(xlim = c(0, max(fit$time)), ylim = c(0, max(fit$cumhaz)))
})

# Arrange plots in grid
grid_plots <- do.call(gridExtra::grid.arrange, c(cs_plots, ncol = 5))
print(grid_plots)


#  DFBETA influence diagnostics  –  multiply imputed data
# Fit one model per imputation  +  subject DFBETA
cox_models <- lapply(transformed_imputed_datasets, function(d)
  coxph(Surv(start, stop, event) ~ um + cs, data = d)
)

dfbeta_list <- Map(function(mod, d){
  dfb <- residuals(mod, type = "dfbeta")            # line level
  dfb_df <- cbind(id = d$id, as.data.frame(dfb))    # add subject id
  names(dfb_df)[-1] <- names(coef(mod))             # proper col names
  aggregate(. ~ id, dfb_df, sum)                    # collapse to id level
}, cox_models, transformed_imputed_datasets)         # list length = m

# Pool across the m imputations  (RMS + exceedance frequencies)
coef_names <- names(cox_models[[1]]$coefficients)   
all_ids    <- sort(unique(do.call(rbind, dfbeta_list)$id))
m          <- length(dfbeta_list)                   # number of imputations

# Store m matrices in a 3-D array 
arr <- array(NA,
             dim      = c(length(all_ids), length(coef_names), m),
             dimnames = list(id   = all_ids,
                             term = coef_names,
                             imp  = 1:m))

for (j in seq_len(m)) {
  tmp <- dfbeta_list[[j]]
  ## make sure column names are right
  for (k in seq_along(coef_names))
    if (!coef_names[k] %in% names(tmp))
      names(tmp)[k + 1] <- coef_names[k]
  arr[match(tmp$id, all_ids), , j] <- as.matrix(tmp[ , coef_names])
}

# RMS pooling  
pooled_rms_mat <- apply(arr, c(1,2), function(x) sqrt(mean(x^2, na.rm = TRUE)))
pooled_rms_df  <- as.data.frame(pooled_rms_mat)
pooled_rms_df$id <- as.numeric(rownames(pooled_rms_df))
rownames(pooled_rms_df) <- NULL

# How often does each subject exceed the cut-off ? 
cnt_df <- data.frame(id      = all_ids,
                     matrix(0,
                            nrow = length(all_ids),
                            ncol = length(coef_names),
                            dimnames = list(NULL,
                                            paste0(coef_names, "_cnt"))),
                     any_cnt = 0)

for (j in seq_len(m)) {
  nsubj_j   <- sum(!is.na(arr[,1,j]))      # subjects in this imputation
  cutoff_j  <- 2 / sqrt(nsubj_j)           # rule-of-thumb threshold
  exc       <- abs(arr[,,j]) > cutoff_j    # logical matrix id × term
  
  # add 1 for every exceedance
  cnt_df[ , paste0(coef_names, "_cnt")] <-
    cnt_df[ , paste0(coef_names, "_cnt")] + exc * 1
  cnt_df$any_cnt <- cnt_df$any_cnt + apply(exc, 1, any) * 1
}

# Merge counts with pooled RMS 
pooled_rms_df <- merge(pooled_rms_df, cnt_df, by = "id")

# Identify influential subjects 
cutoff_pool <- 2 / sqrt(nrow(pooled_rms_df))        # pooled cut-off
infl <- subset(pooled_rms_df,
               apply(pooled_rms_df[ , coef_names], 1,
                     function(z) any(z > cutoff_pool)))

cat("Influential subjects (RMS > cut-off in pooled data):\n")
print(infl)

cat("\nHow many of the", m,
    "imputations were above the cut-off (per term / any):\n")
print(pooled_rms_df[pooled_rms_df$any_cnt > 0,
                    c("id",
                      paste0(coef_names, "_cnt"),
                      "any_cnt")])

# Plot (RMS) 
library(ggplot2)

ggplot(pooled_rms_df, aes(x = um, y = cs)) +
  geom_point() +
  geom_vline(xintercept =  cutoff_pool,  lty = 2, col = "red") +
  geom_hline(yintercept =  cutoff_pool,  lty = 2, col = "red") +
  labs(title = "Pooled DFBETA influence – RMS over 20 imputations",
       subtitle = "Numbers next to points = # of imputations above cut-off",
       x = "RMS(DFBETA) for um",
       y = "RMS(DFBETA) for cs") +
  geom_text(data = subset(pooled_rms_df, any_cnt > 0),
            aes(label = any_cnt),
            vjust = -0.5, size = 3)

# Complete Case Data Analysis
# 4.2 Hazard Model Estimates for a Labor-Market Peak, A2.2 PH Diagnostics, and A3.1 Parametric Robustness for Labor-Market Peak Estimate

data <- read_excel("~/Desktop/Recession_SA_Paper/Data/data_transform_2025/data_transform_2025_unevent_um.xlsx")

names(data)[2] <- "event"

transform_to_start_stop <- function(data) {
  results_list <- list()
  num_rows <- nrow(data)
  
  for (i in 1:num_rows) {
    row <- data[i, ]
    id <- row$id
    event_time <- row$event
    
    covariate_start_cols <- grep(" start$", names(row), value = TRUE)
    covariate_names <- gsub(" start$", "", covariate_start_cols)
    covariate_starts <- row[, covariate_start_cols]
    
    time_points <- sort(unique(c(0, unlist(covariate_starts), event_time)))
    
    individual_data <- data.frame()
    
    for (j in 1:(length(time_points) - 1)) {
      start_time <- time_points[j]
      end_time <- time_points[j + 1]
      
      event_status <- ifelse(end_time == event_time, 1, 0)
      
      covariate_status <- data.frame(matrix(0, nrow = 1, ncol = length(covariate_names)))
      colnames(covariate_status) <- covariate_names
      
      for (k in 1:length(covariate_names)) {
        if (!is.na(covariate_starts[[k]]) && covariate_starts[[k]] < end_time) {
          covariate_status[[covariate_names[k]]] <- 1
        }
      }
      
      interval_row <- data.frame(
        id = id,
        start = start_time,
        stop = end_time,
        event = event_status,
        covariate_status
      )
      
      individual_data <- bind_rows(individual_data, interval_row)
    }
    results_list[[i]] <- individual_data
  }
  bind_rows(results_list)
}

transformed_data_umtscs <- transform_to_start_stop(data)
print(transformed_data_umtscs)

# run complete case bivariate cox model 
cox_model_umcs <- coxph(Surv(start, stop, event) ~ cs + um, data = transformed_data_umtscs)
summary(cox_model_umcs)

# show number of events
table(transformed_data_umtscs$cs, transformed_data_umtscs$event)

# check ph assumtions
cox.zph(coxph(Surv(start, stop, event) ~ um, data = transformed_data_umtscs))
plot(cox.zph(coxph(Surv(start, stop, event) ~ um, data = transformed_data_umtscs)))

# Compare  parametric models 
# Vector of distributions for parametric models 
dists <- c("exp", "weibull", "weibullPH", "gompertz",
           "lnorm", "llogis", "gamma", "gengamma", "genf")

# Parametric models list
models <- lapply(setNames(dists, dists), function(d)
  flexsurvreg(Surv(start, stop, event) ~ um + cs,
              data = transformed_data_umtscs,
              dist = d))

# Comparing AICs & BICs for parametric models
AICs <- sapply(models, AIC)      
BICs <- sapply(models, BIC)
rbind(AIC = round(AICs,2), BIC = round(BICs,2))

# Fit shortlist of parametric models 
weibull_model <- flexsurvreg(
  Surv(start, stop, event) ~ um + cs,
  data = transformed_data_umtscs,
  dist = "weibullPH"  # Weibull in proportional hazards parameterization
)

lnorm_model <- flexsurvreg(
  Surv(start, stop, event) ~ um + cs,
  data = transformed_data_umtscs,
  dist = "lnorm"
)

exp_model <- flexsurvreg(
  Surv(start, stop, event) ~ um + cs,
  data = transformed_data_umtscs,
  dist = "exp"
)

summary(weibull_model)
summary(lnorm_model)
summary(exp_model)

# Chosen Model Summary 
get_ph_results <- function(fit){
  
  # 1. Names of all coefficients in the object
  all_pars <- names(fit$coefficients)
  
  # 2. Keep only the covariate effects (drop baseline "rate")
  cov_pars <- setdiff(all_pars, "rate")
  
  # 3. Point estimates and SEs
  beta_hat <-  fit$coefficients[cov_pars]
  se_hat   <- sqrt(diag(fit$cov))[cov_pars]
  
  # 4. Derived quantities
  HR       <-  exp(beta_hat)
  lower95  <-  exp(beta_hat - 1.96 * se_hat)
  upper95  <-  exp(beta_hat + 1.96 * se_hat)
  z        <-  beta_hat / se_hat
  pval     <-  2 * pnorm(-abs(z))     # two–sided Wald test
  
  # 5. Tidy data frame
  out <- data.frame(
    covariate   = cov_pars,
    coefficient = beta_hat,
    se_coef     = se_hat,
    HR          = HR,
    lower_95CI  = lower95,
    upper_95CI  = upper95,
    p_value     = pval,
    row.names   = NULL
  )
  out
}

results_exp <- get_ph_results(exp_model)
results_exp

# Visual Fit Comparison of Model Short-list 
# Fit Kaplan-Meier
km_fit <- survfit(Surv(stop, event) ~ 1, data = transformed_data_umtscs)

# Generate predicted survival curves at mean covariates
times <- seq(0, max(transformed_data_umtscs$stop), length.out = 100)
exp_pred <- summary(exp_model, t = times, tidy = TRUE, ci = FALSE)
lnorm_pred <- summary(lnorm_model, t = times, tidy = TRUE, ci = FALSE)
wb_pred <- summary(weibull_model, t = times, tidy = TRUE, ci = FALSE)

# Plot comparison with grey tones and distinct line types
plot(km_fit, xlim = c(0, 125), conf.int = FALSE, 
     xlab = "Time", ylab = "Survival Probability")
lines(exp_pred$time, exp_pred$est, col = "gray30", lwd = 2, lty = 2)   # Dashed
lines(lnorm_pred$time, lnorm_pred$est, col = "gray50", lwd = 2, lty = 3) # Dotted
lines(wb_pred$time, wb_pred$est, col = "gray70", lwd = 2, lty = 4)     # Dot-dash
legend("topright", 
       legend = c("Kaplan-Meier", "Exponential", "Log-normal", "Weibull"),
       col = c("black", "gray30", "gray50", "gray70"), 
       lty = c(1, 2, 3, 4),  # 1=solid, 2=dashed, 3=dotted, 4=dot-dash
       lwd = 2, 
       bty = "n")

# Cox-Snell (for exp. model only)
# Function to calculate Cox-Snell residuals for exponential model
get_cox_snell_exp <- function(model, data) {
  # Extract coefficients
  coeffs <- model$coefficients
  baseline_rate <- exp(coeffs["rate"])
  beta_um <- coeffs["um"]
  beta_cs <- coeffs["cs"]
  
  # Calculate for each event
  events <- data[data$event == 1, ]
  residuals <- numeric(nrow(events))
  
  for (i in 1:nrow(events)) {
    id_i <- events$id[i]
    event_time <- events$stop[i]
    
    # Get individual's history
    ind_data <- data[data$id == id_i, ]
    cum_hazard <- 0
    
    # Calculate hazard for each interval
    for (j in 1:nrow(ind_data)) {
      start <- ind_data$start[j]
      stop <- min(ind_data$stop[j], event_time)
      interval_time <- stop - start
      
      # Hazard rate for interval
      lp <- beta_um * ind_data$um[j] + beta_cs * ind_data$cs[j]
      hazard <- exp(log(baseline_rate) + lp) * interval_time
      cum_hazard <- cum_hazard + hazard
      
      if (stop == event_time) break
    }
    residuals[i] <- cum_hazard
  }
  data.frame(residual = residuals)
}

# Calculate residuals
cs_res_exp <- get_cox_snell_exp(exp_model, transformed_data_umtscs)

# Create survival object (all events are failures)
surv_obj <- Surv(cs_res_exp$residual, rep(1, nrow(cs_res_exp)))

# Fit KM curve for residuals
cs_fit <- survfit(surv_obj ~ 1)

# Calculate Nelson-Aalen cumulative hazard
na <- cumsum(cs_fit$n.event / cs_fit$n.risk)

# Plot results
plot(cs_fit$time, na, type = "s", 
     xlab = "Cox-Snell Residuals", 
     ylab = "Cumulative Hazard", 
     main = "Exponential Model: Residual Check")
abline(a = 0, b = 1, col = "red", lty = 2)  # Reference line H(t) = t

cor_test <- cor.test(cs_res_exp$residual, rexp(length(cs_res_exp$residual)))
print(paste("Exponential residual correlation: p=", cor_test$p.value))

# qq-plot (for ln. model only)
# Extract parameters correctly
mu <- lnorm_model$res["meanlog", "est"]
sdlog <- lnorm_model$res["sdlog", "est"]  # Changed from "sigma" to "sdlog"
beta_um <- lnorm_model$res["um", "est"]
beta_cs <- lnorm_model$res["cs", "est"]

# Calculate standardized residuals at event times
events <- transformed_data_umtscs[transformed_data_umtscs$event == 1, ]

# Calculate linear predictor and z-scores
linear_predictor <- mu + beta_um * events$um + beta_cs * events$cs
z_scores <- (log(events$stop) - linear_predictor) / sdlog

# QQ-plot with reference line
qqnorm(z_scores, main = "Log-normal: Normality Check (n=10)")
qqline(z_scores, col = "red")

# Shapiro-Wilk test (interpret cautiously with small n)
shapiro_test <- shapiro.test(z_scores)
print(shapiro_test)

# 4.3 Hazard Model Estimates for Recession Onset & A3.2 Parametric Robustness for Recession Onset & A4 Single-Case Influence Diagnostics

data_transform_2025_un <- read_excel("~/Desktop/Recession_SA_Paper/Data/data_transform_2025/data_transform_2025_un.xlsx")

data <- data_transform_2025_un

transform_to_start_stop <- function(data) {
  results_list <- list()
  num_rows <- nrow(data)
  
  for (i in 1:num_rows) {
    row <- data[i, ]
    id <- row$id
    event_time <- row$ex
    
    covariate_start_cols <- grep(" start$", names(row), value = TRUE)
    covariate_names <- gsub(" start$", "", covariate_start_cols)
    covariate_starts <- row[, covariate_start_cols]
    
    time_points <- sort(unique(c(0, unlist(covariate_starts), event_time)))
    
    individual_data <- data.frame()
    
    for (j in 1:(length(time_points) - 1)) {
      start_time <- time_points[j]
      end_time <- time_points[j + 1]
      
      event_status <- ifelse(end_time == event_time, 1, 0)
      
      covariate_status <- data.frame(matrix(0, nrow = 1, ncol = length(covariate_names)))
      colnames(covariate_status) <- covariate_names
      
      for (k in 1:length(covariate_names)) {
        if (!is.na(covariate_starts[[k]]) && covariate_starts[[k]] < end_time) {
          covariate_status[[covariate_names[k]]] <- 1
        }
      }
      
      interval_row <- data.frame(
        id = id,
        start = start_time,
        stop = end_time,
        event = event_status,
        covariate_status
      )
      
      individual_data <- bind_rows(individual_data, interval_row)
    }
    results_list[[i]] <- individual_data
  }
  bind_rows(results_list)
}

transformed_data <- transform_to_start_stop(data)
print(transformed_data)

#univariate un failing to converge
cox_model_un <- coxph(Surv(start, stop, event) ~ un, data = transformed_data)
summary(cox_model_un)

#univariate un firth
cox_model_un_f <- coxphf(Surv(start, stop, event) ~ un, data = transformed_data)
summary(cox_model_un_f)


# Compare  parametric models 
# Vector of distributions for parametric models 
dists <- c("exp", "weibull", "weibullPH", "gompertz",
           "lnorm", "llogis", "gamma", "gengamma", "genf")

# Parametric models list
models <- lapply(setNames(dists, dists), function(d)
  flexsurvreg(Surv(start, stop, event) ~ un,
              data = transformed_data,
              dist = d))

# Comparing AICs & BICs for parametric models
AICs <- sapply(models, AIC)      
BICs <- sapply(models, BIC)
rbind(AIC = round(AICs,2), BIC = round(BICs,2))

# Fit shortlist of parametric models 
weibull_model <- flexsurvreg(
  Surv(start, stop, event) ~ un,
  data = transformed_data,
  dist = "weibullPH"  # Weibull in proportional hazards parameterization
)

lnorm_model <- flexsurvreg(
  Surv(start, stop, event) ~ un,
  data = transformed_data,
  dist = "lnorm"
)

exp_model <- flexsurvreg(
  Surv(start, stop, event) ~ un,
  data = transformed_data,
  dist = "exp"
)

summary(weibull_model)
summary(lnorm_model)
summary(exp_model)


# Plot fitted survival curves:
plot(weibull_model, xlim = c(0, 50), col = "red")
plot(lnorm_model, xlim = c(0, 50), col = "blue", add = TRUE)


plot(lnorm_model, col = "blue", xlim = c(0, 50), 
     main = "Parametric Survival Comparison")
plot(weibull_model, col = "red", add = TRUE)

# Model Summary 
# Weibull model
beta  <- weibull_model$coefficients["un"]        # log-hazard ratio
se    <- sqrt(weibull_model$cov["un","un"])      # standard error
z     <- beta / se                           # Wald z–statistic
pval  <- 2 * (1 - pnorm(abs(z)))             # two-sided p

c(beta     = beta,
  HR       = exp(beta),                      # hazard ratio
  se       = se,
  z        = z,
  p.value  = pval)

# Lognormal model
beta  <- lnorm_model$coefficients["un"]      # log(time-ratio)
se    <- sqrt(lnorm_model$cov["un","un"])
z     <- beta / se
pval  <- 2 * (1 - pnorm(abs(z)))

c(beta        = beta,
  time_ratio  = exp(beta),                   # acceleration factor
  se          = se,
  z           = z,
  p.value     = pval)

# Exponential model
beta  <- exp_model$coefficients["un"]        # log-hazard ratio
se    <- sqrt(exp_model$cov["un","un"])      # standard error
z     <- beta / se                           # Wald z–statistic
pval  <- 2 * (1 - pnorm(abs(z)))             # two-sided p

c(beta     = beta,
  HR       = exp(beta),                      # hazard ratio
  se       = se,
  z        = z,
  p.value  = pval)


# Compare observed and estimated fit 
# log normal model
sf <- survfit(Surv(start, stop, event) ~ un, data = transformed_data)
plot(sf,                                     # survival by default
     col = c("steelblue", "red"),
     lwd = 2, conf.int = FALSE,
     xlab = "Time", ylab = "Survival probability")
# level un = 0
lines(lnorm_model, type   = "survival",        # or "cumhaz"
      newdata = data.frame(un = 0),
      ci = FALSE, col = "steelblue", lwd = 2)

# level un = 1
lines(lnorm_model, type   = "survival",
      newdata = data.frame(un = 1),
      ci = FALSE, col = "red", lwd = 2)

legend("topright",
       legend = c("KM  un=0","KM  un=1",
                  "Ln fit  un=0","Ln fit  un=1"),
       lwd = 2,
       col = c("steelblue","red",
               "steelblue","red"),
       lty = c(1,1,1,1))

# weibull model
sf <- survfit(Surv(start, stop, event) ~ un, data = transformed_data)
plot(sf,                                     # survival by default
     col = c("steelblue", "red"),
     lwd = 2, conf.int = FALSE,
     xlab = "Time", ylab = "Survival probability")
# level un = 0
lines(weibull_model, type   = "survival",        # or "cumhaz"
      newdata = data.frame(un = 0),
      ci = FALSE, col = "steelblue", lwd = 2)

# level un = 1
lines(weibull_model, type   = "survival",
      newdata = data.frame(un = 1),
      ci = FALSE, col = "red", lwd = 2)

legend("topright",
       legend = c("KM  un=0","KM  un=1",
                  "WB fit  un=0","WB fit  un=1"),
       lwd = 2,
       col = c("steelblue","red",
               "steelblue","red"),
       lty = c(1,1,1,1))

# Best fit for log-normal model

# Quantile–quantile plot of log(times) versus a normal distr. to see if the log-normal is reasonable
event_times <- with(transformed_data, stop[event == 1])
qqnorm(log(event_times),
       xlab = "Theoretical normal quantile",
       ylab = "Empirical log-time quantile")
qqline(log(event_times), col = "red")

# Shapiro-Wilk test strengthens the qq results for normality of log event times
shapiro.test(log(event_times))

# Median survival for each group 
med0 <- summary(lnorm_model,
                newdata = data.frame(un = 0),
                type = "quantile", quantiles = 0.5)

med1 <- summary(lnorm_model,
                newdata = data.frame(un = 1),
                type = "quantile", quantiles = 0.5)

med0[[1]]$est   # median when un = 0
med1[[1]]$est   # median when un = 1
med0[[1]]$lcl
med0[[1]]$ucl
med1[[1]]$lcl
med1[[1]]$ucl


# Cox-Snell residual check 
# coefficient vector
cf  <- coef(lnorm_model)

# flexsurv's parameter names for a log-normal model are:
#   "meanlog"  – location   μ0
#   "sdlog"    – log(σ)     (because σ>0, flexsurv stores log σ)
#   everything else is a regression coefficient for a covariate
mu0        <- cf["meanlog"]                 # intercept for μ
sigma      <- exp(cf["sdlog"])              # back-transform
beta_names <- setdiff(names(cf), c("meanlog","sdlog"))
beta_vec   <- cf[beta_names]                # β-vector

cumhaz_lnorm <- function(t, mu, sigma){
  -log( 1 - plnorm(t, meanlog = mu, sdlog = sigma) )
}

cs_row <- transformed_data %>%                    # <- the start-stop data set
  ## linear predictor (μ) for every row
  mutate(
    mu = mu0 +                        # intercept
      as.matrix(select(., all_of(beta_names))) %*% beta_vec
  ) %>%
  ## cumulative hazard at start and stop of the interval
  mutate(
    H_stop  = cumhaz_lnorm(stop,  mu, sigma),
    H_start = cumhaz_lnorm(start, mu, sigma),
    H_incr  = H_stop - H_start        # increment contributed by this row
  )

cs_subject <- cs_row %>%
  group_by(id) %>%
  summarise(
    cs_resid = sum(H_incr),     # Ri = Σ increments up to last interval
    event    = max(event)       # 1 = failure, 0 = censored
  ) %>% ungroup()

cs_fit <- survfit(Surv(cs_resid, event) ~ 1, data = cs_subject)

plot(-log(cs_fit$surv) ~ cs_fit$time, type = "s",
     xlab = "Cox–Snell residual", ylab = "Estimated cumulative hazard",
     main = "Cox–Snell residual check")
abline(0, 1, lty = 2, col = "red")  # the reference Exp(1) line

# DFBETA Analysis for cox model with Firth penalization 

full_fit <- coxphf(Surv(start, stop, event) ~ un,
                   data = transformed_data)
full_beta <- coef(full_fit)          # the FIRTH estimate β̂
full_beta

dfbeta_firth <- function(dat, id.var, formula){
  
  # unique subject IDs
  ids <- unique(dat[[id.var]])
  
  # vector that will hold the “dfbeta” for each subject
  dfb    <- numeric(length(ids))
  names(dfb) <- ids
  
  # fit once on the whole data set
  fit_full  <- coxphf(formula, data = dat)
  beta_full <- coef(fit_full)
  
  for(i in seq_along(ids)){
    
    # leave-one-subject-out data
    dat_i <- dat[ dat[[id.var]] != ids[i], ]
    
    # try–catch so that the loop does not stop if a sub-model fails
    sub_fit <- try(
      coxphf(formula, data = dat_i, maxit = 50),  # increase maxit if necessary
      silent = TRUE)
    
    if(inherits(sub_fit, "try-error")){
      dfb[i] <- NA
    } else {
      beta_i <- coef(sub_fit)
      dfb[i] <- as.numeric(beta_full - beta_i)    # sign follows the coxph dfbeta convention
    }
  }
  return(dfb)
}

dfb <- dfbeta_firth(transformed_data,
                    id.var = "id",
                    formula = Surv(start, stop, event) ~ un)

dfb

which(is.na(dfb))

# Plot Subject level Influence Analysis ####

dfb_data <- data.frame(
  id = names(dfb),
  dfbeta = as.numeric(dfb),
  influence = cut(abs(dfb),
                  breaks = c(0, 0.05, 0.1, Inf),
                  labels = c("Low (<0.05)", "Moderate (0.05-0.1)", "High (>0.1)"))
)

# Critical thresholds
n_subj <- length(dfb)
threshold <- 2 / sqrt(n_subj)

# Create publication-ready greyscale plot
ggplot(dfb_data, aes(x = reorder(id, abs(dfbeta)), y = dfbeta, fill = influence)) +
  geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = c(-threshold, 0, threshold), 
             linetype = c("dashed", "solid", "dashed"),
             color = "black",
             linewidth = c(0.7, 0.5, 0.7)) +
  scale_fill_manual(values = c("Low (<0.05)" = "grey90", 
                               "Moderate (0.05-0.1)" = "grey60", 
                               "High (>0.1)" = "grey30")) +
  labs(title = "Subject-Level Influence Analysis",
       x = "Subject ID (Ordered by Influence Magnitude)",
       y = "DFBETA (Δβ)",
       fill = "Influence Level:",
       caption = paste0("Influence threshold: |2/√n| = ±", round(threshold, 3), 
                        " (n = ", n_subj, " subjects)")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.25),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, 
                              margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0.5, size = 9, 
                                margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = -5, b = 0),
    legend.box.spacing = unit(0.3, "cm")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                             override.aes = list(color = "black")))

# A2.1 Variable Selection


library(dplyr)
library(ggplot2)
library(rlang)
library(survival)
library(survminer)
library(tidyverse)
library(tidyr)
library(ggsurvfit)
library(coxphf)
library(epiR)  
library(psych)
library(readxl)
library(car)

data_transform_2025_all <- read_excel("~/Desktop/Recession_SA_Paper/Data/data_transform_2025/data_transform_2025_unevent_um.xlsx")

data <- data_transform_2025_all

transform_to_start_stop <- function(data) {
  results_list <- list()
  num_rows <- nrow(data)
  
  for (i in 1:num_rows) {
    row <- data[i, ]
    id <- row$id
    event_time <- row$ex
    
    covariate_start_cols <- grep(" start$", names(row), value = TRUE)
    covariate_names <- gsub(" start$", "", covariate_start_cols)
    covariate_starts <- row[, covariate_start_cols]
    
    time_points <- sort(unique(c(0, unlist(covariate_starts), event_time)))
    
    individual_data <- data.frame()
    
    for (j in 1:(length(time_points) - 1)) {
      start_time <- time_points[j]
      end_time <- time_points[j + 1]
      
      event_status <- ifelse(end_time == event_time, 1, 0)
      
      covariate_status <- data.frame(matrix(0, nrow = 1, ncol = length(covariate_names)))
      colnames(covariate_status) <- covariate_names
      
      for (k in 1:length(covariate_names)) {
        if (!is.na(covariate_starts[[k]]) && covariate_starts[[k]] < end_time) {
          covariate_status[[covariate_names[k]]] <- 1
        }
      }
      
      interval_row <- data.frame(
        id = id,
        start = start_time,
        stop = end_time,
        event = event_status,
        covariate_status
      )
      
      individual_data <- bind_rows(individual_data, interval_row)
    }
    results_list[[i]] <- individual_data
  }
  bind_rows(results_list)
}

transformed_data <- transform_to_start_stop(data)

print(transformed_data)

# Create the contingency table
contingency_table <- table(transformed_data$cs, transformed_data$um, dnn = c("cs", "um"))
print(contingency_table)

# Perform a Chi-squared test for independence
chi_test <- chisq.test(contingency_table)
print(chi_test)

phi_coefficient <- sqrt(chi_test$statistic / sum(contingency_table))
print(phi_coefficient)

switch_times <- transformed_data %>%
  group_by(id) %>%
  summarise(
    cs_switch_time = first(start[cs == 1]),
    um_switch_time = first(start[um == 1])
  ) %>%
  # Handle cases where a switch never occurs
  mutate(
    cs_switch_time = ifelse(is.infinite(cs_switch_time), NA, cs_switch_time),
    um_switch_time = ifelse(is.infinite(um_switch_time), NA, um_switch_time)
  )

print(switch_times)

# Analyze the difference in switch times
switch_times <- switch_times %>%
  mutate(time_diff = cs_switch_time - um_switch_time)

summary(switch_times$time_diff)

# Model 1: cs only
model_cs <- coxph(Surv(start, stop, event) ~ cs, data = transformed_data)
summary(model_cs)

# Model 2: um only
model_um <- coxph(Surv(start, stop, event) ~ um, data = transformed_data)
summary(model_um)

# Model 3: Both cs and um
model_both <- coxph(Surv(start, stop, event) ~ cs + um, data = transformed_data)
summary(model_both)

AIC(model_cs)
AIC(model_um)
AIC(model_both)

# Request VIFs
vif_values <- vif(model_both)
print(vif_values)

# A1.3 Missing Data and Imputation

# Load Excel Data
imp_2025 <- read_excel("~/Desktop/Recession_SA_Paper/Data/data_transform_2025_test.xlsx")
transformed_data_ts <- read_excel("~/Desktop/Recession_SA_Paper/Data/2025_r_transformed_start_stop_data/transformed_data_ts.xlsx")
transformed_data_cs <- read_excel("~/Desktop/Recession_SA_Paper/Data/2025_r_transformed_start_stop_data/transformed_data_cs.xlsx")
transformed_data_um <- read_excel("~/Desktop/Recession_SA_Paper/Data/2025_r_transformed_start_stop_data/transformed_data_um.xlsx")
km_2025 <- read_excel("~/Desktop/Recession_SA_Paper/Data/km_2025_censored.xlsx")


# Initialize the predictor matrix (default: all variables predict all others)
init <- mice(imp_2025, maxit = 0)       
pred <- init$predictorMatrix      # get default pred matrix
meth <- init$method               # get default imputation methods

# Exclude 'id' from the imputation process:
pred[, "id"] <- 0     # id does NOT predict anything
#pred["id", ] <- 0     # id is NOT imputed--not predicted by others
#meth["id"] <- ""      # id is not imputed at all

# Set seed for reproducibility
set.seed(123)  

# Impute missing data
imputed_data <- mice(
  predictorMatrix = pred,
  imp_2025,
  method = meth,
  m = 20,
  maxit = 5,
  print = FALSE
)

plot(imputed_data)

# Define the transformation function
transform_to_start_stop <- function(data) {
  # [Paste function, as above]
  results_list <- list()
  num_rows <- nrow(data)
  
  for (i in 1:num_rows) {
    row <- data[i, ]
    id <- row$id
    event_time <- row$event
    
    covariate_start_cols <- grep("(_start| start)$", names(row), value = TRUE)
    covariate_names <- gsub("(_start| start)$", "", covariate_start_cols)
    covariate_starts <- row[, covariate_start_cols]
    
    time_points <- sort(unique(c(0, unlist(covariate_starts), event_time)))
    
    individual_data <- data.frame()
    
    for (j in 1:(length(time_points) - 1)) {
      start_time <- time_points[j]
      end_time <- time_points[j + 1]
      
      event_status <- ifelse(end_time == event_time, 1, 0)
      
      covariate_status <- data.frame(matrix(0, nrow = 1, ncol = length(covariate_names)))
      colnames(covariate_status) <- covariate_names
      
      for (k in 1:length(covariate_names)) {
        if (!is.na(covariate_starts[[k]]) && covariate_starts[[k]] < end_time) {
          covariate_status[[covariate_names[k]]] <- 1
        }
      }
      
      interval_row <- data.frame(
        id = id,
        start = start_time,
        stop = end_time,
        event = event_status,
        covariate_status
      )
      
      individual_data <- bind_rows(individual_data, interval_row)
    }
    results_list[[i]] <- individual_data
  }
  bind_rows(results_list)
}

# Extract all 20 completed datasets
imputed_datasets_list <- lapply(1:20, function(i) complete(imputed_data, action = i))

# Apply your transform function to each imputed dataset
transformed_imputed_datasets <- lapply(imputed_datasets_list, transform_to_start_stop)

# Optionally, add imputation number to each
for (i in seq_along(transformed_imputed_datasets)) {
  transformed_imputed_datasets[[i]]$imp_number <- i
}

# Combine all into a single dataframe if you like
all_transformed_data <- bind_rows(transformed_imputed_datasets)

# 'all_transformed_data' now contains the start-stop transformed data for ALL imputations

summary(complete(imputed_data))
summary(imp_2025)

print(imputed_data)

bwplot(imputed_data)
stripplot(imputed_data) 

xyplot(imputed_data, ts_start ~ event, groups = .imp, 
       pch = c(1, 20), col = c("blue", "red"), cex = 1.2)

xyplot(imputed_data, um_start ~ event, groups = .imp, 
       pch = c(1, 20), col = c("blue", "red"), cex = 1.2)

xyplot(imputed_data, cs_start ~ event, groups = .imp, 
       pch = c(1, 20), col = c("blue", "red"), cex = 1.2)

# Status == 1 means event occurred, 0 = censored

# Fit KM curve for observed data
fit_obs <- survfit(
  Surv(start, stop, ts) ~ 1, 
  data = transformed_data_ts  # Replace with your observed dataset
)
plot(fit_obs)

# Extract (for example) the 1st imputed dataset
imputed1 <- complete(imputed_data, 3)

view(imputed1)


# Get the imputed datasets as a list of data frames
imputed_data_list_km <- complete(imputed_data, "all")

# Create the transformed version
imputed_data_transformed <- lapply(imputed_data_list_km, function(df) {
  df %>%
    mutate(across(c(cs_start, un_start, um_start, ts_start),
                  ~ pmax(event - .x, 0)))
})


# Extract ALL imputed datasets as a list of data frames
imputed_dfs <- complete(imputed_data, "all")

# Now transform the variables
imputed_data_transformed <- lapply(imputed_dfs, function(df) {
  df %>%
    mutate(across(c(cs_start, un_start, um_start, ts_start),
                  ~ pmax(event - .x, 0)))
})

# Create survival fits (all events occur)
fits <- lapply(imputed_data_transformed, function(df) {
  # Create dummy event column (all events observed)
  df <- df %>% 
    mutate(event = 1) %>%  # Force all to be events
    filter(!is.na(ts_start), ts_start >= 0)  # Clean data
  
  survfit(Surv(time = ts_start, event = event) ~ 1, data = df)
})

# Create common time grid (0 to max observed time)
max_time <- max(sapply(fits, function(f) max(f$time)))
grid <- seq(0, max_time, length.out = 1000)

# Calculate survival probabilities at grid points
surv_matrix <- sapply(fits, function(f) {
  approx(x = f$time, y = f$surv, xout = grid, method = "constant", f = 0)$y
})

# Calculate mean survival curve
mean_surv <- data.frame(
  time = grid,
  surv = rowMeans(surv_matrix, na.rm = TRUE)
)

# Plot all curves with mean
ggplot() +
  # Individual curves (transparent grey)
  lapply(fits, function(f) {
    geom_step(data = data.frame(time = f$time, surv = f$surv),
              aes(x = time, y = surv), 
              color = "grey70", alpha = 0.3)
  }) +
  # Mean curve (red)
  geom_step(data = mean_surv, 
            aes(x = time, y = surv),
            color = "red", size = 0.3) +
  labs(x = "Time (months)", y = "Survival Probability",
       title = "Time-to-Event Distribution with Imputation Mean") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()


# Create survival object (all events observed)
observed_fit <- survfit(
  Surv(time = ts, event = rep(1, nrow(km_2025))) ~ 1, 
  data = km_2025
)

# Prepare observed curve data
observed_curve <- data.frame(
  time = observed_fit$time,
  surv = observed_fit$surv,
  source = "Observed"
)

# Process IMPUTED data 
# Create common time grid combining observed and imputed times
all_times <- c(
  unlist(lapply(fits, function(f) f$time)),
  observed_curve$time
)
grid <- sort(unique(all_times))

# Calculate survival probabilities at common time points
surv_matrix <- matrix(nrow = length(grid), ncol = length(fits))

for (i in seq_along(fits)) {
  sf <- stepfun(fits[[i]]$time, c(1, fits[[i]]$surv))
  surv_matrix[, i] <- sf(grid)
}

# Create combined data for plotting
mean_surv <- data.frame(
  time = grid,
  surv = rowMeans(surv_matrix),
  source = "Imputed Mean"
)

# Combine all data
plot_data <- bind_rows(
  # Individual imputed curves
  lapply(1:length(fits), function(i) {
    data.frame(
      time = fits[[i]]$time,
      surv = fits[[i]]$surv,
      source = paste("Imp", i)
    )
  }) %>% bind_rows(),
  # Mean imputed curve
  mean_surv,
  # Observed curve
  observed_curve
)

# Create final comparison plot
ggplot(plot_data, aes(x = time, y = surv)) +
  # Individual imputed curves
  geom_step(
    data = filter(plot_data, grepl("Imp", source)),
    aes(group = source),
    color = "grey70", alpha = 0.3
  ) +
  # Mean imputed curve
  geom_step(
    data = filter(plot_data, source == "Imputed Mean"),
    color = "red", size = 0.4
  ) +
  # Observed curve
  geom_step(
    data = filter(plot_data, source == "Observed"),
    color = "blue", size = 0.4
  ) +
  labs(
    x = "Time (months)",
    y = "Survival Probability",
    title = "TS Survival Curve Comparison",
    subtitle = "Blue dashed: Observed | Red: Imputed Mean | Grey: Individual Imputations"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")


# Process OBSERVED UM data from km_2025
observed_fit_um <- survfit(
  Surv(time = um, event = rep(1, nrow(km_2025))) ~ 1, 
  data = km_2025
)

# Prepare observed curve data
observed_curve_um <- data.frame(
  time = observed_fit_um$time,
  surv = observed_fit_um$surv,
  source = "Observed"
)

# Process IMPUTED UM_START data
# Create survival fits for um_start
fits_um <- lapply(imputed_data_transformed, function(df) {
  df <- df %>% 
    mutate(event = 1) %>%  # Force all to be events
    filter(!is.na(um_start), um_start >= 0)
  
  survfit(Surv(time = um_start, event = event) ~ 1, data = df)
})

# Create common time grid for UM
all_times_um <- c(
  unlist(lapply(fits_um, function(f) f$time)),
  observed_curve_um$time
)
grid_um <- sort(unique(all_times_um))

# Calculate survival probabilities at common time points
surv_matrix_um <- matrix(nrow = length(grid_um), ncol = length(fits_um))

for (i in seq_along(fits_um)) {
  sf <- stepfun(fits_um[[i]]$time, c(1, fits_um[[i]]$surv))
  surv_matrix_um[, i] <- sf(grid_um)
}

# Create combined data for UM plot
mean_surv_um <- data.frame(
  time = grid_um,
  surv = rowMeans(surv_matrix_um),
  source = "Imputed Mean"
)

# Combine all UM data
plot_data_um <- bind_rows(
  lapply(1:length(fits_um), function(i) {
    data.frame(
      time = fits_um[[i]]$time,
      surv = fits_um[[i]]$surv,
      source = paste("Imp", i)
    )
  }) %>% bind_rows(),
  mean_surv_um,
  observed_curve_um
)

#KM Plot Observed um vs imputed um 
ggplot() +
  # 1. Individual imputations (light grey)
  geom_step(
    data = plot_data_um %>% filter(grepl("Imp", source)),
    aes(x = time, y = surv, group = source, color = "Individual Imputations"),
    linewidth = 0.6,
    alpha = 0.4
  ) +
  # Imputed mean (dark grey, DASHED)
  geom_step(
    data = plot_data_um %>% filter(source == "Imputed Mean"),
    aes(x = time, y = surv, color = "Imputed Mean", linetype = "Imputed Mean"),
    linewidth = 1
  ) +
  # Observed (medium grey, SOLID)
  geom_step(
    data = plot_data_um %>% filter(source == "Observed"),
    aes(x = time, y = surv, color = "Observed", linetype = "Observed"),
    linewidth = 1
  ) +
  # Color scale
  scale_color_manual(
    name = NULL,
    values = c(
      "Individual Imputations" = "grey",
      "Imputed Mean" = "#636363",
      "Observed" = "#969696"
    ),
    breaks = c("Observed", "Imputed Mean", "Individual Imputations")
  ) +
  # Line type scale
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Imputed Mean" = "dashed",
      "Observed" = "solid"
    ),
    guide = "none"  # We'll show this through color instead
  ) +
  # Axis labels
  labs(
    x = "Time (Months)",
    y = "Survival Probability"
  ) +
  # Axis limits
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, max(plot_data_um$time), by = 12)) +
  # Theme
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.5, "cm"),
    legend.text = element_text(size = 10),
    #axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  # Legend customization
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = c(1, 1, 0.6),
        alpha = c(1, 1, 0.4),
        linetype = c("solid", "dashed", "solid")
      )
    )
  )


# Process OBSERVED CS data from km_2025
observed_fit_cs <- survfit(
  Surv(time = cs, event = rep(1, nrow(km_2025))) ~ 1, 
  data = km_2025
)

# Prepare observed curve data
observed_curve_cs <- data.frame(
  time = observed_fit_cs$time,
  surv = observed_fit_cs$surv,
  source = "Observed"
)

# 2. Process IMPUTED CS_START data
fits_cs <- lapply(imputed_data_transformed, function(df) {
  df <- df %>% 
    mutate(event = 1) %>%  # Force all events
    filter(!is.na(cs_start), cs_start >= 0)
  
  survfit(Surv(time = cs_start, event = event) ~ 1, data = df)
})

# Create common time grid for CS
all_times_cs <- c(
  unlist(lapply(fits_cs, function(f) f$time)),
  observed_curve_cs$time
)
grid_cs <- sort(unique(all_times_cs))

# Calculate survival probabilities
surv_matrix_cs <- matrix(nrow = length(grid_cs), ncol = length(fits_cs))

for (i in seq_along(fits_cs)) {
  sf <- stepfun(fits_cs[[i]]$time, c(1, fits_cs[[i]]$surv))
  surv_matrix_cs[, i] <- sf(grid_cs)
}

# Create combined CS plot data
mean_surv_cs <- data.frame(
  time = grid_cs,
  surv = rowMeans(surv_matrix_cs),
  source = "Imputed Mean"
)

plot_data_cs <- bind_rows(
  lapply(1:length(fits_cs), function(i) {
    data.frame(
      time = fits_cs[[i]]$time,
      surv = fits_cs[[i]]$surv,
      source = paste("Imp", i)
    )
  }) %>% bind_rows(),
  mean_surv_cs,
  observed_curve_cs
)

# Create CS comparison plot with your preferred styling
ggplot(plot_data_cs, aes(x = time, y = surv)) +
  geom_step(
    data = filter(plot_data_cs, grepl("Imp", source)),
    aes(group = source),
    color = "grey70", alpha = 0.3, linewidth = 0.4
  ) +
  geom_step(
    data = filter(plot_data_cs, source == "Imputed Mean"),
    color = "red", linewidth = 0.4
  ) +
  geom_step(
    data = filter(plot_data_cs, source == "Observed"),
    color = "blue", linewidth = 0.4
  ) +
  labs(
    x = "Time (months)",
    y = "Survival Probability",
    title = "CS Survival Curve Comparison",
    subtitle = "Blue: Observed CS | Red: Imputed Mean | Grey: Individual Imputations"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(plot.title = element_text(size = 14, face = "bold"))


# Create EX-specific subsets
ex_early <- km_2025[14:24, ] %>%
  mutate(
    ex_group = "Early",
    ex_event = 1  # All events observed
  )

ex_late <- km_2025[25:34, ] %>%
  mutate(
    ex_group = "Late",
    ex_event = 1
  )

# Combined EX data with unique names
ex_combined <- bind_rows(ex_early, ex_late) %>%
  filter(!is.na(ex), ex >= 0) %>%
  rename(time = ex, event = ex_event, group = ex_group)

# EX-specific survival fit
ex_fit <- survfit(Surv(time, event) ~ group, data = ex_combined)

# Dedicated plot data
ex_plotdf <- data.frame(
  time = ex_fit$time,
  surv = ex_fit$surv,
  group = rep(
    gsub("group=", "", names(ex_fit$strata)),  # Clean group names first
    times = ex_fit$strata
  )
) %>%
  mutate(group = factor(group, levels = c("Early", "Late")))


# Distinct plot object
ex_comparison_plot <- ggplot(ex_plotdf, aes(x = time, y = surv, color = group)) +
  geom_step(linewidth = 1) +  # Matched line thickness from km_plot
  scale_color_manual(
    values = c("Early" = "#636363", "Late" = "grey"),  # Grey palette similar to km_plot
    name = "Group"  # Matched legend title style
  ) +
  labs(
    x = "Time (Months)",  # Matched capitalization from km_plot
    y = "Survival Probability",  # Matched capitalization from km_plot
    #title = "Figure 2: EX Variable: Early (14-24) vs Late (25-34)",  # Optional: match title style
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),  # Matched title style from km_plot
    legend.position = "top",  # Changed to match typical km_plot position
    #legend.title = element_text(face = "bold"),  # Added to match km_plot style
    #axis.title = element_text(face = "bold")  # Added to match potential km_plot style
  ) +
  scale_y_continuous(limits = c(0, 1))

# Unique statistical test object
ex_logrank <- survdiff(Surv(time, event) ~ group, data = ex_combined)

# Display components
print(ex_comparison_plot)
print(ex_logrank)

