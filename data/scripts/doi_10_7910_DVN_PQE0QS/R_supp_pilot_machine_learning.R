# R script for: implementing the machine learning approach presented in Appendix D 
# This version dated to: 2025-10-08

# NOTES:
## Set your current working directory to where this file is before running;
## Expect (very) long running time for some parts of the codes in this script;
## Use high-performance clusters for better performance if possible;
## Increase native memory limit may be necessary for managing large forest fits.

# Example commands (in Mac terminal) for raising memory caps:
## R_MAX_VSIZE=40Gb R
## open -a RStudio --args --max-vsize=40GB

rm(list = ls())

# CONFIG: CTRLS ----

run_causal_forests <- FALSE # turn on for new runs

# PREP: Load packages ----

library(tidyverse)
library(grf)
library(ggplot2)

# PREP: Load data ----

data <- readr::read_csv("DAT_cleaned_study_data.csv")

# PREP: Prepare data ----

cols <- c(
  'attr_educ_match', 'attr_income_match', 'attr_skills_training', 
  'attr_occupation', 'attr_language', 
  'attr_north_south', 'attr_region_match', 'attr_religion',
  'attr_gender', 'attr_reason',
  'migrant_type', 'country_type'
)
cols2 <- c('choice', 'study')

df <- data[ , c(cols, cols2)]

thresh <- length(cols) * 1/2
bad_studies <- vapply(
  split(df[cols], df$study),
  function(d) {
    # count how many variables are entirely NA
    n_full_missing <- sum(colSums(!is.na(d)) == 0)
    n_full_missing >= thresh
  },
  logical(1)
)
drop_ids <- names(bad_studies)[bad_studies]
df <- df[!(df$study %in% drop_ids), ]

thresh2 <- 0.5 * length(cols)
df <- df[rowSums(is.na(df[cols])) < thresh2, ]

df <- df[!is.na(df$choice),]

# PREP: Prepare features ----

df[cols] <- lapply(df[cols], function(x) factor(x))
df$choice <- as.numeric(df$choice)
df$study <- as.factor(df$study)

for (col in cols) {
  if (col %in% c('migrant_type','country_type')) next
  cat("\n=== Column:", col, "===\n")
  
  tab <- with(
    df,
    tapply(df[[col]], list(migrant_type, country_type), function(v) {
      v <- v[!is.na(v)]
      if (length(v) == 0L) return(NA)
      tt <- table(v)
      names(tt)[which.max(tt)]   # modal level
    })
  )
  
  print(tab)
}

na_before <- vapply(df[cols], function(col) if (is.factor(col)) sum(is.na(col)) else 0L, integer(1))

mode_impute_factor_by_strata <- function(x, migrant_type, country_type,
                                         use_addNA = TRUE,
                                         do_global_fallback = TRUE) {
  # x: factor to impute
  if (!is.factor(x)) return(x)
  stopifnot(length(x) == length(migrant_type), length(x) == length(country_type))
  
  if (use_addNA) {
    migrant_type <- addNA(migrant_type)
    country_type <- addNA(country_type)
  }
  
  # helper: modal level (ties broken by factor level order)
  modal_level <- function(v) {
    vv <- v[!is.na(v)]
    if (length(vv) == 0L) return(NA_character_)
    tt <- table(vv)
    names(tt)[which.max(tt)]
  }
  
  grp <- interaction(migrant_type, country_type, drop = TRUE, lex.order = TRUE)
  out <- x
  
  # 1) fill within each (migrant_type × country_type) stratum
  idx_split <- split(seq_along(out), grp)
  for (idx in idx_split) {
    gvals <- out[idx]
    if (!anyNA(gvals)) next
    m <- modal_level(gvals)
    if (!is.na(m)) out[idx[is.na(gvals)]] <- m
  }
  
  # 2) global fallback to ensure full dataset
  if (do_global_fallback && anyNA(out)) {
    m_all <- modal_level(out)  # mode over current (partially filled) column
    if (!is.na(m_all)) out[is.na(out)] <- m_all
  }
  
  out
}

df[cols] <- lapply(
  df[cols],
  function(col) mode_impute_factor_by_strata(
    x = col,
    migrant_type = df$migrant_type,
    country_type = df$country_type,
    use_addNA = TRUE,          # treat NA in groupers as their own strata
    do_global_fallback = TRUE  # final global mode fill
  )
)

na_after <- vapply(df[cols], function(col) if (is.factor(col)) sum(is.na(col)) else 0L, integer(1))

# RUN: Fit causal forests ----

# 'treatments'
W_unemp <- as.integer(df$attr_occupation == "Unemployed")
W_gs    <- as.integer(df$attr_north_south == "Developing/Low income")

# outcome
y <- df$choice

# covariates (drop treatment parent)
covs_unemp   <- df[setdiff(names(df), c(cols2, 'attr_occupation'))]
covs_gs   <- df[setdiff(names(df), c(cols2, 'attr_north_south'))]

# X_unemp <- model.matrix(~ . - 1, data = covs_unemp)
# X_gs <- model.matrix(~ . - 1, data = covs_gs)

# one-hot encoded covariates
X_unemp <- model.matrix(
  ~ . - 1,
  data = covs_unemp,
  contrasts.arg = lapply(covs_unemp, function(x) {
    if (is.factor(x)) contrasts(x, contrasts = FALSE) else NULL
  })
)
X_gs <- model.matrix(
  ~ . - 1,
  data = covs_gs,
  contrasts.arg = lapply(covs_gs, function(x) {
    if (is.factor(x)) contrasts(x, contrasts = FALSE) else NULL
  })
)

if (run_causal_forests) {
  set.seed(2025)
  cf_unemp <- grf::causal_forest(
    X = X_unemp,
    Y = y,
    W = W_unemp,
    num.trees = 4000,          # adjust up/down based on time/memory
    sample.fraction = 0.25,     # default ~0.5; helps with very large N
    min.node.size = 50,
    honesty = TRUE
  )
  saveRDS(cf_unemp, 'FITTED_cf_unemployed.rds')
} else cf_unemp <- readRDS('CACHE_cf_unemployed.rds')

if (run_causal_forests) {
  set.seed(2025)
  cf_gs <- grf::causal_forest(
    X = X_gs,
    Y = y,
    W = W_gs,
    num.trees = 4000,          # adjust up/down based on time/memory
    sample.fraction = 0.25,     # default ~0.5; helps with very large N
    min.node.size = 50,
    honesty = TRUE
  )
  saveRDS(cf_gs, 'cf_globalsouth.rds')
} else cf_gs <- readRDS('CACHE_cf_globalsouth.rds')

gc()

# RUN: Estimate treatment effects ----

# Treatment Var = Unemployed

# ate_unemp <- grf::average_treatment_effect(cf_unemp, target.sample = "all")
att_unemp <- grf::average_treatment_effect(cf_unemp, target.sample = "treated")
atnt_unemp <- grf::average_treatment_effect(cf_unemp, target.sample = "control")
ateow_unemp <- grf::average_treatment_effect(cf_unemp, target.sample = "overlap")

rbind.data.frame(
  # ate_unemp, 
  att_unemp, atnt_unemp, ateow_unemp
) |> 
  `colnames<-`(c('estimate', 'std.err')) |>
  `rownames<-`(c('ATT', 'ATNT', 'ATE (OW)'))
#           estimate     std.err
# ATT      -0.1406351 0.001204738
# ATNT     -0.1372295 0.001278512
# ATE (OW) -0.1402863 0.001204078

cte_unemp <- predict(cf_unemp)$predictions

tauhats_unemp <- list(
  # 'ATE' = ate_unemp,
  'ATT' = att_unemp, 'ATNT' = atnt_unemp,
  'ATE-OW' = ateow_unemp,
  'CATE' = cte_unemp
)

covars_unemp <- c( # From 'X_unemp'
  "attr_educ_matchEducation match" = "Educ Match: Education match",                                  
  "attr_educ_matchEducation mismatch" = "Educ Match: Education mismatch",                               
  "attr_income_matchSkills/income match" = "Income match: Skills/income match",                            
  "attr_income_matchSkills/income mismatch" = "Income match: Skills/income mismatch",                        
  "attr_skills_trainingExtensive training/experience (more than 3y)" = "Skills training: Extensive training/experience (more than 3y)",
  "attr_skills_trainingNo/limited training/experience (less than 3y)" = "Skills training: No/limited training/experience (less than 3y)",
  "attr_languageBroken language" = "Language: Broken language",                                    
  "attr_languageFluent language" = "Language: Fluent language",                                    
  "attr_languageUnable to speak language" = "Language: Unable to speak language",                         
  "attr_north_southDeveloped/High income" = "North-South: Developed/High income",                            
  "attr_north_southDeveloping/Low income" = "North-South: Developing/Low income",                          
  "attr_region_matchDifferent world region" = "Region match: Different world region",                         
  "attr_region_matchSame world region" = "Region match: Same world region",                              
  "attr_religionChristian" = "Religion: Christian",                                           
  "attr_religionHindu" = "Religion: Hindu",                                              
  "attr_religionJew" = "Religion: Jew",                                                
  "attr_religionMuslim" = "Religion: Muslim",                                            
  "attr_religionNo religion" = "Religion: No religion",                                         
  "attr_genderFemale" = "Gender: Female",                                             
  "attr_genderMale" = "Gender: Male",                                                 
  "attr_genderMale and female" = "Gender: Male and female",                                       
  "attr_reasonClimate migrant" = "Reason: Climate migrant",                                       
  "attr_reasonEconomic migrant" = "Reason: Economic migrant",                                     
  "attr_reasonFamily reunification" = "Reason: Family reunification",                                 
  "attr_reasonForced migrant" = "Reason: Forced migrant",                                      
  "migrant_typeIDPs" = "Migrant Type: IDPs",                                              
  "migrant_typeInternal migrants" = "Migrant Type: Internal migrants",                                   
  "migrant_typeManipulated" = "Migrant Type: Manipulated",                                      
  "migrant_typeMigrants" = "Migrant Type: Migrants",                                            
  "migrant_typeRefugees" = "Migrant Type: Refugees",                                            
  "country_typeDeveloped/High income" = "Country Type: Developed/High income",                               
  "country_typeDeveloping/Low income" = "Country Type: Developing/Low income" 
)

vi_unemp <- grf::variable_importance(cf_unemp)
vi_unemp_df <- data.frame(
  variable = covars_unemp,
  importance = vi_unemp
)
vi_unemp_df <- vi_unemp_df[order(-vi_unemp_df$importance), ] |>
  dplyr::rename(variable2 = variable) |>
  tibble::rownames_to_column(var = 'variable')

write.csv(vi_unemp_df, 'CACHE_cf_vi_unemp.csv', row.names = FALSE)

vi_unemp_df_nonzero <- vi_unemp_df[vi_unemp_df$importance>0,]
vi_unemp_df_nonzero_vars <- vi_unemp_df_nonzero$variable
atesub1_unemp <- atesub0_unemp <- vector('list', length = nrow(vi_unemp_df_nonzero))
names(atesub1_unemp) <- names(atesub0_unemp) <- vi_unemp_df_nonzero_vars

for (var in vi_unemp_df_nonzero_vars) {
  print(sprintf('Now estimating ATE (overlap-weighted) for: %s', var))
  
  idx0 <- which(X_unemp[,var] == 0)
  idx1 <- which(X_unemp[,var] == 1)
  
  ate0 <- grf::average_treatment_effect(cf_unemp, subset = idx0, target.sample = "overlap")
  ate1 <- grf::average_treatment_effect(cf_unemp, subset = idx1, target.sample = "overlap")
  
  atesub0_unemp[[var]] <- ate0
  atesub1_unemp[[var]] <- ate1
}

atesub_unemp_df <- dplyr::bind_rows(
  dplyr::bind_rows(atesub0_unemp, .id = 'variable') |>
    dplyr::mutate(level = 0L),
  dplyr::bind_rows(atesub1_unemp, .id = 'variable') |>
    dplyr::mutate(level = 1L)
) %>% 
  dplyr::left_join(
    vi_unemp_df_nonzero,
    by = 'variable'
  ) |>
  dplyr::group_by(variable) |>
  dplyr::arrange(estimate, .by_group=TRUE) |>
  dplyr::ungroup()

atesub_unemp_plotdf <- atesub_unemp_df %>%
  dplyr::mutate(
    # 95% CI
    conf.low  = estimate - 1.96 * std.err,
    conf.high = estimate + 1.96 * std.err,
    level     = factor(level, levels = c(0, 1), labels = c("0", "1"))
  ) %>%
  # ensure a single importance per variable2 for ordering
  dplyr::group_by(variable2) %>%
  dplyr::mutate(imp_max = max(importance, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(variable2 = forcats::fct_reorder(variable2, imp_max))  # low→high; add .desc=TRUE for high→low

write.csv(atesub_unemp_plotdf, 'CACHE_cf_tauhats_sub_unemp.csv', row.names = FALSE)

# Treatment Var = Global South

# ate_gs <- grf::average_treatment_effect(cf_gs, target.sample = "all")
att_gs <- grf::average_treatment_effect(cf_gs, target.sample = "treated")
atnt_gs <- grf::average_treatment_effect(cf_gs, target.sample = "control")
ateow_gs <- grf::average_treatment_effect(cf_gs, target.sample = "overlap")

rbind.data.frame(
  # ate_gs, 
  att_gs, atnt_gs, ateow_gs
) |> 
  `colnames<-`(c('estimate', 'std.err')) |>
  `rownames<-`(c('ATT', 'ATNT', 'ATE (OW)'))
#             estimate     std.err
# ATT      -0.03023656 0.004992269
# ATNT     -0.03010947 0.001307674
# ATE (OW) -0.03070584 0.001161614

cte_gs <- predict(cf_gs)$predictions

tauhats_gs <- list(
  'ATE' = ate_gs,
  'ATT' = att_gs, 'ATNT' = atnt_gs,
  'ATE-OW' = ateow_gs,
  'CATE' = cte_gs
)

covars_gs <- c( # From 'X_gs'
  "attr_educ_matchEducation match" = "Educ Match: Education match",                                  
  "attr_educ_matchEducation mismatch" = "Educ Match: Education mismatch",                               
  "attr_income_matchSkills/income match" = "Income match: Skills/income match",                            
  "attr_income_matchSkills/income mismatch" = "Income match: Skills/income mismatch",                        
  "attr_skills_trainingExtensive training/experience (more than 3y)" = "Skills training: Extensive training/experience (more than 3y)",
  "attr_skills_trainingNo/limited training/experience (less than 3y)" = "Skills training: No/limited training/experience (less than 3y)",
  "attr_occupationProfessional occupation" = "Occupation: Professional occupation",
  "attr_occupationStudent/Pensioner" = "Occupation: Student/Pensioner",
  "attr_occupationUnemployed" = "Occupation: Unemployed",
  "attr_occupationWorker/Farmer" = "Occupation: Worker/Farmer",
  "attr_languageBroken language" = "Language: Broken language",                                    
  "attr_languageFluent language" = "Language: Fluent language",                                    
  "attr_languageUnable to speak language" = "Language: Unable to speak language",                         
  "attr_region_matchDifferent world region" = "Region match: Different world region",                         
  "attr_region_matchSame world region" = "Region match: Same world region",                              
  "attr_religionChristian" = "Religion: Christian",                                           
  "attr_religionHindu" = "Religion: Hindu",                                              
  "attr_religionJew" = "Religion: Jew",                                                
  "attr_religionMuslim" = "Religion: Muslim",                                            
  "attr_religionNo religion" = "Religion: No religion",                                         
  "attr_genderFemale" = "Gender: Female",                                             
  "attr_genderMale" = "Gender: Male",                                                 
  "attr_genderMale and female" = "Gender: Male and female",                                       
  "attr_reasonClimate migrant" = "Reason: Climate migrant",                                       
  "attr_reasonEconomic migrant" = "Reason: Economic migrant",                                     
  "attr_reasonFamily reunification" = "Reason: Family reunification",                                 
  "attr_reasonForced migrant" = "Reason: Forced migrant",                                      
  "migrant_typeIDPs" = "Migrant Type: IDPs",                                              
  "migrant_typeInternal migrants" = "Migrant Type: Internal migrants",                                   
  "migrant_typeManipulated" = "Migrant Type: Manipulated",                                      
  "migrant_typeMigrants" = "Migrant Type: Migrants",                                            
  "migrant_typeRefugees" = "Migrant Type: Refugees",                                            
  "country_typeDeveloped/High income" = "Country Type: Developed/High income",                               
  "country_typeDeveloping/Low income" = "Country Type: Developing/Low income" 
)

vi_gs <- grf::variable_importance(cf_gs)
vi_gs_df <- data.frame(
  variable = covars_gs,
  importance = vi_gs
)
vi_gs_df <- vi_gs_df[order(-vi_gs_df$importance), ] |>
  dplyr::rename(variable2 = variable) |>
  tibble::rownames_to_column(var = 'variable')

write.csv(vi_gs_df, 'CACHE_cf_vi_gs.csv', row.names = FALSE)

vi_gs_df_nonzero <- vi_gs_df[vi_gs_df$importance>0,]
vi_gs_df_nonzero_vars <- vi_gs_df_nonzero$variable
atesub1_gs <- atesub0_gs <- vector('list', length = nrow(vi_gs_df_nonzero))
names(atesub1_gs) <- names(atesub0_gs) <- vi_gs_df_nonzero_vars

for (var in vi_gs_df_nonzero_vars) {
  print(sprintf('Now estimating ATE (overlap-weighted) for: %s', var))
  
  idx0 <- which(X_gs[,var] == 0)
  idx1 <- which(X_gs[,var] == 1)
  
  ate0 <- grf::average_treatment_effect(cf_gs, subset = idx0, target.sample = "overlap")
  ate1 <- grf::average_treatment_effect(cf_gs, subset = idx1, target.sample = "overlap")
  
  atesub0_gs[[var]] <- ate0
  atesub1_gs[[var]] <- ate1
}

atesub_gs_df <- dplyr::bind_rows(
  dplyr::bind_rows(atesub0_gs, .id = 'variable') |>
    dplyr::mutate(level = 0L),
  dplyr::bind_rows(atesub1_gs, .id = 'variable') |>
    dplyr::mutate(level = 1L)
) %>% 
  dplyr::left_join(
    vi_gs_df_nonzero,
    by = 'variable'
  ) |>
  dplyr::group_by(variable) |>
  dplyr::arrange(estimate, .by_group=TRUE) |>
  dplyr::ungroup()

atesub_gs_plotdf <- atesub_gs_df %>%
  dplyr::mutate(
    # 95% CI
    conf.low  = estimate - 1.96 * std.err,
    conf.high = estimate + 1.96 * std.err,
    level     = factor(level, levels = c(0, 1), labels = c("0", "1"))
  ) %>%
  # ensure a single importance per variable2 for ordering
  dplyr::group_by(variable2) %>%
  dplyr::mutate(imp_max = max(importance, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(variable2 = forcats::fct_reorder(variable2, imp_max))  # low→high; add .desc=TRUE for high→low

write.csv(atesub_gs_plotdf, 'CACHE_cf_tauhats_sub_gs.csv', row.names = FALSE)

# CLEANUP ----

rm(list = ls())
gc()

# END SCRIPT
