############################################
#            02_MAIN_ANALYSIS              #
############################################

## Libraries
library(poLCA)
library(mice)
library(ordinal)
library(dplyr)
library(tidyr)
library(purrr)
library(here)

###############################
#         FILE PATHS          #
###############################

derived_path <- here("data_derived")

###############################
#     LOADING DATA            #
###############################

## Load MICE object (covariates only; N = 116 finished respondents)
mice_obj <- readRDS(file.path(derived_path, "mice_imputation_object.rds"))

## Load cleaned raw data for belief items and adoption columns (never imputed)
survey_raw <- readRDS(file.path(derived_path, "survey_raw_clean.rds"))

## Load sample flow from 01_clean_data.R for cross-checking
sample_flow <- readRDS(file.path(derived_path, "sample_flow.rds"))
print(sample_flow)

###############################
#      PHASE 1: LCA SETUP     #
###############################

solar_types <- c("distributed", "utility", "shared", "agrivoltaics")

## Extract belief items from raw cleaned data (not from imputed object)
eval_items <- survey_raw[, c(
  grep("distributed_benefit_",    names(survey_raw), value = TRUE),
  grep("distributed_cost_",       names(survey_raw), value = TRUE),
  grep("distributed_mitigation_", names(survey_raw), value = TRUE),
  grep("utility_benefit_",        names(survey_raw), value = TRUE),
  grep("utility_cost_",           names(survey_raw), value = TRUE),
  grep("utility_mitigation_",     names(survey_raw), value = TRUE),
  grep("shared_benefit_",         names(survey_raw), value = TRUE),
  grep("shared_cost_",            names(survey_raw), value = TRUE),
  grep("shared_mitigation_",      names(survey_raw), value = TRUE),
  grep("agrivoltaics_benefit_",   names(survey_raw), value = TRUE),
  grep("agrivoltaics_cost_",      names(survey_raw), value = TRUE),
  grep("agrivoltaics_mitigation", names(survey_raw), value = TRUE)
)]
eval_items$respondent_id <- survey_raw$respondent_id

## Preparing LCA dataset, +1 offset required by poLCA (expects indicators coded 1/2, not 0/1)
lca_data <- eval_items %>%
  dplyr::select(respondent_id, matches("_(benefit|cost|mitigation)_")) %>%
  dplyr::select(-matches("_specify$")) %>%
  dplyr::mutate(across(-respondent_id, ~ . + 1))

## Remove constant variables
variance_check <- lca_data %>%
  dplyr::select(-respondent_id) %>%
  summarise(across(everything(), ~ var(., na.rm = TRUE)))

keep_vars <- names(variance_check)[variance_check > 0 & !is.na(variance_check)]

n_dropped_vars <- ncol(lca_data) - 1 - length(keep_vars)

## Listwise deletion for LCA
n_before_lca <- nrow(lca_data)

lca_data_clean <- lca_data %>%
  dplyr::select(respondent_id, all_of(keep_vars)) %>%
  filter(complete.cases(.))

n_lca_dropped <- n_before_lca - nrow(lca_data_clean)
n_lca_sample  <- nrow(lca_data_clean)

###############################
#     PHASE 1: FITTING LCA    #
###############################

belief_vars <- setdiff(names(lca_data_clean), "respondent_id")
lca_formula <- as.formula(paste("cbind(", paste(belief_vars, collapse = ", "), ") ~ 1"))

set.seed(12345)
lca_models <- list()

for (k in 1:6) {
  best_model <- NULL
  best_llik  <- -Inf
  
  for (rep in 1:20) {
    tryCatch({
      model <- poLCA(lca_formula,
                     data    = lca_data_clean %>% dplyr::select(-respondent_id),
                     nclass  = k,
                     maxiter = 5000,
                     nrep    = 1,
                     verbose = FALSE)
      if (model$llik > best_llik) {
        best_llik  <- model$llik
        best_model <- model
      }
    }, error = function(e) {})
  }
  
  lca_models[[k]] <- best_model
  
  if (is.null(best_model)) {
    message("K=", k, " — all 20 starts failed, model is NULL")
  } else {
    message("K=", k, " — OK  llik=", round(best_llik, 2))
  }
}

###############################
#   PHASE 1: MODEL SELECTION  #
###############################

fit_comparison <- data.frame(
  K              = 1:6,
  Log_Likelihood = sapply(lca_models, function(m) if (is.null(m)) NA else round(m$llik, 2)),
  AIC            = sapply(lca_models, function(m) if (is.null(m)) NA else round(m$aic,  2)),
  BIC            = sapply(lca_models, function(m) if (is.null(m)) NA else round(m$bic,  2)),
  Entropy        = sapply(lca_models, function(m) {
    if (is.null(m)) return(NA)
    post <- m$posterior
    round(-sum(post * log(post + 1e-10)) / (nrow(post) * log(ncol(post))), 3)
  }),
  N_Parameters   = sapply(lca_models, function(m) if (is.null(m)) NA else m$npar)
)

print(fit_comparison)

optimal_k   <- fit_comparison %>% filter(!is.na(BIC)) %>% filter(BIC == min(BIC)) %>% pull(K)
final_model <- lca_models[[optimal_k]]
message("Optimal K selected: ", optimal_k)

###############################
#  PHASE 1: CLASS LABELING    #
###############################

## Empirical labeling via weighted mean endorsement rate per class
belief_matrix <- lca_data_clean %>%
  dplyr::select(-respondent_id) %>%
  as.matrix() - 1   # convert back from poLCA coding (1/2) to 0/1

class_mean_endorsement <- sapply(seq_len(optimal_k), function(k) {
  weights <- final_model$posterior[, k]
  weighted.mean(rowMeans(belief_matrix, na.rm = TRUE), weights)
})

comprehensive_class <- which.max(class_mean_endorsement)
costfocused_class   <- which.min(class_mean_endorsement)

class_assignments <- data.frame(
  respondent_id      = lca_data_clean$respondent_id,
  deliberation_class = final_model$predclass,
  class_prob         = apply(final_model$posterior, 1, max)
) %>%
  mutate(
    class_label = case_when(
      deliberation_class == comprehensive_class ~ "Comprehensive",
      deliberation_class == costfocused_class   ~ "Cost-Focused",
      TRUE ~ paste("Class", deliberation_class)
    )
  )

class_labels <- data.frame(
  deliberation_class = seq_len(optimal_k),
  label = sapply(seq_len(optimal_k), function(k) {
    if (k == comprehensive_class) "Comprehensive"
    else if (k == costfocused_class) "Cost-Focused"
    else paste("Class", k)
  })
)

print(table(class_assignments$class_label))

###############################
#     PHASE 1: MI POOLING     #
###############################

## Recode so Cost-Focused = 1, Comprehensive = 2 (reference = Cost-Focused)
class_assignments_merge <- data.frame(
  respondent_id = lca_data_clean$respondent_id,
  lca_class     = ifelse(final_model$predclass == comprehensive_class, 2L, 1L)
)

## Adoption columns from raw data 
adoption_cols <- survey_raw %>%
  dplyr::select(respondent_id, starts_with("install_"))

## Expand MICE object to long format then restrict to LCA-eligible respondents (N = 107 per imputation)
imputed_long_full <- complete(mice_obj, "long", include = TRUE)

n_before_merge <- length(unique(imputed_long_full$respondent_id))

imputed_long_full <- imputed_long_full %>%
  left_join(class_assignments_merge, by = "respondent_id") %>%
  filter(!is.na(lca_class)) %>%           # drops the 9 without class assignment
  left_join(adoption_cols, by = "respondent_id")

n_after_merge <- length(unique(imputed_long_full$respondent_id))

## Derive model variables in each imputation
imputed_long_full <- imputed_long_full %>%
  mutate(
    age_numeric        = as.numeric(age),
    education_numeric  = as.numeric(education),
    farm_acres_numeric = as.numeric(farm_acres),
    income_numeric     = as.numeric(household_income),
    owner              = as.numeric(ownership == 0),
    male               = as.numeric(gender == 1),
    white              = as.numeric(race_white == 1),
    farmer             = as.numeric(farmer_type == 1),
    livestock          = as.numeric(main_product_livestock == 1)
  )

## Verify install_ columns present before converting to mids
install_check <- grep("^install_", names(imputed_long_full), value = TRUE)

## Convert back to mids object
imputed_with_class <- as.mids(imputed_long_full)

########################################
# PHASE 1: BELIEF COUNTS BY SOLAR TYPE #
########################################

## Computed on full eval_items (N = 116) but only LCA-eligible respondents (N = 107) will appear in class_data_full

exclude_patt <- c("_none$", "_noconcern$", "_remain$", "_specify$")

belief_counts_by_type <- eval_items %>%
  dplyr::select(respondent_id, matches("_(benefit|cost|mitigation)_"))

for (st in solar_types) {
  type_benefit    <- grep(paste0("^", st, "_benefit_"),    names(belief_counts_by_type), value = TRUE)
  type_benefit    <- type_benefit[!grepl("_none$|_specify$", type_benefit)]
  type_cost       <- grep(paste0("^", st, "_cost_"),       names(belief_counts_by_type), value = TRUE)
  type_cost       <- type_cost[!grepl("_none$|_specify$", type_cost)]
  type_mitigation <- grep(paste0("^", st, "_mitigation_"), names(belief_counts_by_type), value = TRUE)
  type_mitigation <- type_mitigation[!grepl("_remain$|_specify$", type_mitigation)]
  
  belief_counts_by_type <- belief_counts_by_type %>%
    rowwise() %>%
    mutate(
      !!paste0(st, "_benefits")    := sum(c_across(all_of(type_benefit)),    na.rm = TRUE),
      !!paste0(st, "_costs")       := sum(c_across(all_of(type_cost)),       na.rm = TRUE),
      !!paste0(st, "_mitigations") := sum(c_across(all_of(type_mitigation)), na.rm = TRUE),
      !!paste0(st, "_total")       := sum(c_across(all_of(
        c(type_benefit, type_cost, type_mitigation))), na.rm = TRUE)
    ) %>%
    ungroup()
  
  benefit_none_col      <- paste0(st, "_benefit_none")
  cost_none_col         <- paste0(st, "_cost_none")
  mitigation_remain_col <- paste0(st, "_mitigation_remain")
  
  if (benefit_none_col %in% names(belief_counts_by_type))
    belief_counts_by_type <- belief_counts_by_type %>%
    mutate(!!paste0(st, "_no_benefits") := .data[[benefit_none_col]])
  
  if (cost_none_col %in% names(belief_counts_by_type))
    belief_counts_by_type <- belief_counts_by_type %>%
    mutate(!!paste0(st, "_no_costs") := .data[[cost_none_col]])
  
  if (mitigation_remain_col %in% names(belief_counts_by_type))
    belief_counts_by_type <- belief_counts_by_type %>%
    mutate(!!paste0(st, "_concerns_remain") := .data[[mitigation_remain_col]])
}

belief_counts_by_type <- belief_counts_by_type %>%
  dplyr::select(respondent_id,
                matches("_(benefits|costs|mitigations|total|no_benefits|no_costs|concerns_remain)$"))

## Inner join, restricts to LCA-eligible
class_data_full <- class_assignments %>%
  left_join(belief_counts_by_type, by = "respondent_id") %>%
  left_join(class_labels, by = "deliberation_class")

########################################
#    PHASE 2: CLASS MEMBERSHIP MODELS  #
########################################
## Tests whether observable demographics/farm characteristics predict class membership

# Model 1: Demographics only
fit_demographics <- with(imputed_with_class,
                         glm(I(lca_class == 2) ~ income_numeric + age_numeric +
                               education_numeric + male + white,
                             family = binomial))

# Model 2: Farm characteristics only
fit_farm <- with(imputed_with_class,
                 glm(I(lca_class == 2) ~ farm_acres_numeric + owner + livestock + farmer,
                     family = binomial))

# Model 3: Combined
fit_combined <- with(imputed_with_class,
                     glm(I(lca_class == 2) ~ income_numeric + age_numeric +
                           education_numeric + male + white +
                           farm_acres_numeric + owner + livestock + farmer,
                         family = binomial))

## Pool with Rubin's rules
pooled_demographics <- pool(fit_demographics)
pooled_farm         <- pool(fit_farm)
pooled_combined     <- pool(fit_combined)

## McFadden R2 on first imputation
calc_mcfadden_r2 <- function(model) {
  null_formula <- update(formula(model), . ~ 1)
  null_model   <- glm(null_formula, data = model$data, family = binomial)
  1 - (logLik(model)[1] / logLik(null_model)[1])
}

imp1 <- complete(imputed_with_class, 1)

m1_demo <- glm(I(lca_class == 2) ~ income_numeric + age_numeric +
                 education_numeric + male + white,
               data = imp1, family = binomial)
m2_farm <- glm(I(lca_class == 2) ~ farm_acres_numeric + owner + livestock + farmer,
               data = imp1, family = binomial)
m3_comb <- glm(I(lca_class == 2) ~ income_numeric + age_numeric +
                 education_numeric + male + white +
                 farm_acres_numeric + owner + livestock + farmer,
               data = imp1, family = binomial)

r2_demo <- as.numeric(calc_mcfadden_r2(m1_demo))
r2_farm <- as.numeric(calc_mcfadden_r2(m2_farm))
r2_comb <- as.numeric(calc_mcfadden_r2(m3_comb))

########################################
#  PHASE 3: ADOPTION INTENTION MODELS  #
########################################

prepare_adoption_long <- function(imp_data) {
  imp_data |>
    pivot_longer(
      cols         = starts_with("install_"),
      names_to     = "solar_type",
      names_prefix = "install_",
      values_to    = "adoption"
    ) |>
    filter(!is.na(adoption)) |>           # drops solar types not answered
    mutate(
      adoption    = ordered(adoption),
      solar_type  = factor(solar_type,
                           levels = c("distributed", "utility",
                                      "shared", "agrivoltaics")),
      familiarity_matched = case_when(
        solar_type == "distributed"  ~ familiarity_distributed,
        solar_type == "utility"      ~ familiarity_utility,
        solar_type == "shared"       ~ familiarity_shared,
        solar_type == "agrivoltaics" ~ familiarity_agrivoltaics
      ),
      evaluation_style = factor(lca_class,
                            levels = c(1, 2),
                            labels = c("Cost-Focused", "Comprehensive"))
    )
}

## Confirm function and reference level
test_imp  <- complete(imputed_with_class, 1)
test_long <- prepare_adoption_long(test_imp)
message("evaluation_style reference level: ", levels(test_long$evaluation_style)[1],
        "  (should be Cost-Focused)")
print(head(test_long[, c("respondent_id", "solar_type", "adoption",
                         "evaluation_style", "familiarity_matched")]))

## Store first imputation long format for figures
adoption_long_1 <- prepare_adoption_long(complete(imputed_with_class, 1))

## Pooling function for clmm models (manual Rubin's rules)
pool_clmm_results <- function(model_list, model_name) {
  coef_list <- lapply(model_list, function(x) {
    if (is.null(x$model)) return(NULL)
    coef_mat <- summary(x$model)$coefficients
    coef_mat <- coef_mat[!grepl("\\|", rownames(coef_mat)), , drop = FALSE]
    
    # Flag quasi-complete separation: SE > 10 or NA
    se_col    <- coef_mat[, "Std. Error"]
    separated <- is.na(se_col) | se_col > 10
    
    data.frame(
      term      = rownames(coef_mat),
      estimate  = coef_mat[, "Estimate"],
      std.error = ifelse(separated, NA_real_, se_col),
      separated = separated,
      stringsAsFactors = FALSE
    )
  })
  
  coef_list  <- Filter(Negate(is.null), coef_list)
  all_terms  <- unique(unlist(lapply(coef_list, function(x) x$term)))
  
  pooled_results <- lapply(all_terms, function(term) {
    ests <- sapply(coef_list, function(x) {
      idx <- which(x$term == term)
      if (length(idx) > 0) x$estimate[idx] else NA
    })
    ses <- sapply(coef_list, function(x) {
      idx <- which(x$term == term)
      if (length(idx) > 0) x$std.error[idx] else NA
    })
    valid <- !is.na(ests) & !is.na(ses)
    ests  <- ests[valid]
    ses   <- ses[valid]
    if (length(ests) == 0) return(NULL)
    
    m     <- length(ests)
    qbar  <- mean(ests)
    ubar  <- mean(ses^2)
    b     <- var(ests)
    t     <- ubar + (1 + 1/m) * b
    se_p  <- sqrt(t)
    
    lambda  <- (1 + 1/m) * b / t
    df_old  <- (m - 1) / lambda^2
    n_clust <- length(unique(model_list[[1]]$data$respondent_id))
    df_obs  <- (n_clust - length(all_terms)) * (1 - lambda)
    df      <- (df_old * df_obs) / (df_old + df_obs)
    
    t_stat  <- qbar / se_p
    p_value <- 2 * pt(-abs(t_stat), df = df)
    t_crit  <- qt(0.975, df = df)
    
    data.frame(
      term      = term,
      estimate  = qbar,
      std.error = se_p,
      statistic = t_stat,
      p.value   = p_value,
      conf.low  = qbar - t_crit * se_p,
      conf.high = qbar + t_crit * se_p,
      df        = df
    )
  })
  
  pooled_df <- do.call(rbind, pooled_results[!sapply(pooled_results, is.null)])
  
  pooled_df %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ "†",
        TRUE            ~ ""
      ),
      Model = model_name
    )
}

## Fit Models A-D across all imputations
n_imp     <- imputed_with_class$m
results_A <- list()
results_B <- list()
results_C <- list()
results_D <- list()

for (i in seq_len(n_imp)) {
  imp_data      <- complete(imputed_with_class, i)
  adoption_long <- prepare_adoption_long(imp_data)
  
  mA <- tryCatch(
    clmm(adoption ~ evaluation_style + solar_type + (1 | respondent_id),
         data = adoption_long, link = "logit"),
    error = function(e) { message("Model A imp ", i, " failed: ", e$message); NULL }
  )
  
  mB <- tryCatch(
    clmm(adoption ~ evaluation_style +
           age_numeric + education_numeric + income_numeric +
           farm_acres_numeric + owner + male + white + livestock + farmer +
           solar_type + (1 | respondent_id),
         data = adoption_long, link = "logit"),
    error = function(e) { message("Model B imp ", i, " failed: ", e$message); NULL }
  )
  
  mC <- tryCatch(
    clmm(adoption ~ evaluation_style + familiarity_matched + new_developments +
           solar_type + (1 | respondent_id),
         data = adoption_long, link = "logit"),
    error = function(e) { message("Model C imp ", i, " failed: ", e$message); NULL }
  )
  
  mD <- tryCatch(
    clmm(adoption ~ evaluation_style +
           age_numeric + education_numeric + income_numeric +
           farm_acres_numeric + owner + male + white + livestock + farmer +
           familiarity_matched + new_developments +
           solar_type + (1 | respondent_id),
         data = adoption_long, link = "logit"),
    error = function(e) { message("Model D imp ", i, " failed: ", e$message); NULL }
  )
  
  results_A[[i]] <- list(model = mA, data = adoption_long)
  results_B[[i]] <- list(model = mB, data = adoption_long)
  results_C[[i]] <- list(model = mC, data = adoption_long)
  results_D[[i]] <- list(model = mD, data = adoption_long)
  
  message("Imputation ", i, " complete")
}

## Pool results
pooled_A <- pool_clmm_results(results_A, "Model 1: Class Only")
pooled_B <- pool_clmm_results(results_B, "Model 2: Class + Demographics + Farm-Level")
pooled_C <- pool_clmm_results(results_C, "Model 3: Class + Familiarity + Tech Adoption")
pooled_D <- pool_clmm_results(results_D, "Model 4: Full Model")

## Sanity check: class coefficient should be positive 
print(pooled_D %>% filter(grepl("evaluation_style", term)) %>%
        select(term, estimate, conf.low, conf.high, p.value, sig))

########################################
#            SAVING ALL OBJECTS        #
########################################

saveRDS(
  list(
    ## Sample flow
    sample_flow            = sample_flow,
    n_lca_sample           = n_lca_sample,
    n_lca_dropped          = n_lca_dropped,
    ## LCA
    lca_models             = lca_models,
    fit_comparison         = fit_comparison,
    optimal_k              = optimal_k,
    final_model            = final_model,
    comprehensive_class    = comprehensive_class,
    costfocused_class      = costfocused_class,
    class_mean_endorsement = class_mean_endorsement,
    class_assignments      = class_assignments,
    class_labels           = class_labels,
    class_data_full        = class_data_full,
    eval_items           = eval_items,
    lca_data_clean         = lca_data_clean,
    ## Imputation / data
    imputed_with_class     = imputed_with_class,
    adoption_long_1        = adoption_long_1,
    ## Phase 2: class membership
    pooled_demographics    = pooled_demographics,
    pooled_farm            = pooled_farm,
    pooled_combined        = pooled_combined,
    r2_demo                = r2_demo,
    r2_farm                = r2_farm,
    r2_comb                = r2_comb,
    m1_demo                = m1_demo,
    m2_farm                = m2_farm,
    m3_comb                = m3_comb,
    ## Phase 3: adoption models
    results_A              = results_A,
    results_B              = results_B,
    results_C              = results_C,
    results_D              = results_D,
    pooled_A               = pooled_A,
    pooled_B               = pooled_B,
    pooled_C               = pooled_C,
    pooled_D               = pooled_D,
    ## Functions (passed to appendix script)
    prepare_adoption_long  = prepare_adoption_long,
    pool_clmm_results      = pool_clmm_results
  ),
  file.path(derived_path, "analysis_objects.rds")
)