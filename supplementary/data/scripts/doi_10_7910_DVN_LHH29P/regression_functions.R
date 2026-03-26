pacman::p_load(lmtest, sandwich, dplyr, arrow, AER, broom, stringr, margins, glue, ivpack, estimatr)

run_itt <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  # Add subgroup interaction terms (does nothing if subgroup == full_sample)
  data <- add_subgroup_interaction_terms(data, subgroup, tvar, take.var, treat_only = T)

  # Build ITT formula
  rhs <- configure_right_hand_side(data, tvar, take.var, xvars, subgroup)
  formula_str <- paste0(yvar, " ~ ", paste(rhs$treatment_side, collapse = " + "))

  # Run regression
  if (regression_type == 'xsection') {

    out <- run_lm(data, as.formula(formula_str),
                  is_true(more_params[['robust']], default = T),
                  more_params[['weight']])

  } else if (regression_type == 'panel') {
    out <- run_estimatr_lm_robust(data, as.formula(formula_str))
  } else {
    stop("Invalid regression_type for ITT.")
  }

  # Get stats
  itt_stats = get_model_stats(out$model, out$m.out, rhs$treatment_vars, formula_str, subgroup) %>%
    select(-term) %>%
    rename_with(~ str_c(., "_itt"), .cols = -subgroup)

  # Run subgroup f-test
  subgroup_ftest = is_true(more_params[['subgroup_ftest']], default = F)
  robust = is_true(more_params[['robust']], default = T)
  if ((subgroup_ftest) & (length(rhs$treatment_vars) > 1) & (regression_type == 'xsection') & (robust)) {
    p_subgroup_ftest = run_subgroup_ftest(out$model, rhs$treatment_vars)
    itt_stats <- itt_stats %>% mutate(p_subgroup_ftest_itt = p_subgroup_ftest)
  }

  return(itt_stats)
}


run_full_double_subgroup_itt <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  double_subgroup = subgroup
  subgroups = strsplit(double_subgroup, '_')[[1]]

  # Add subgroup interaction terms
  data <- add_subgroup_interaction_terms(data, double_subgroup, tvar, take.var, treat_only = T)
  data <- add_subgroup_interaction_terms(data, subgroups[1], tvar, take.var, treat_only = T)
  data <- add_subgroup_interaction_terms(data, subgroups[2], tvar, take.var, treat_only = T)

  # Build ITT formula
  double_subgroup_vars = names(data)[grepl(paste0("^", double_subgroup, "_subgroup__"), names(data))]
  double_subgroup_vars_drop1 = head(double_subgroup_vars, -1)
  treatment_vars = c(tvar, names(data)[grepl('subgroup_x_treatment', names(data))])
  treatment_side = c(double_subgroup_vars_drop1, treatment_vars, xvars)
  formula_str <- paste0(yvar, " ~ ", paste(treatment_side, collapse = " + "))

  # Run regression
  if (regression_type == 'xsection') {

    out <- run_lm(data, as.formula(formula_str),
                  is_true(more_params[['robust']], default = T),
                  more_params[['weight']])

  } else if (regression_type == 'panel') {
    out <- run_estimatr_lm_robust(data, as.formula(formula_str))
  } else {
    stop("Invalid regression_type for ITT.")
  }

  # Get stats
  itt_stats = get_model_stats(out$model, out$m.out, treatment_vars, formula_str, subgroup) %>%
    select(-term) %>%
    rename_with(~ str_c(., "_itt"), .cols = -subgroup)

  return(itt_stats)
}


run_tot <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  # Add subgroup interaction terms (does nothing if subgroup == full_sample)
  data <- add_subgroup_interaction_terms(data, subgroup, tvar, take.var)

  # Build TOT formula
  rhs <- configure_right_hand_side(data, tvar, take.var, xvars, subgroup)
  formula_str <- paste0(
    yvar, " ~ ",
    paste(rhs$takeup_side, collapse = " + "), " | ",
    paste(rhs$treatment_side, collapse = " + "))

  # Run regression
  if (regression_type == 'xsection') {

    out <- run_ivreg(data, as.formula(formula_str),
                     is_true(more_params[['robust']], default = T),
                     more_params$weight)

  } else if (regression_type == 'panel') {
    out <- run_estimatr_iv_robust(data, as.formula(formula_str))
  } else {
    stop("Invalid regression_type for TOT")
  }

  # Get stats
  tot_stats = get_model_stats(out$model, out$m.out, rhs$takeup_vars, formula_str, subgroup) %>%
    select(-term) %>%
    rename_with(~ str_c(., "_tot"), .cols = -subgroup)
  tot_ccm_stats = get_tot_ccm(
    data, out$m.out, rhs$treatment_vars, rhs$control_vars, rhs$takeup_vars, yvar, subgroup)
  tot_stats = tot_stats %>%
    left_join(tot_ccm_stats, by = 'subgroup')

  return(tot_stats)
}


run_joint <- function(all_data, yvar, tvar, take.var, xvars, subgroup_type, regression_type, more_params) {

  # Build formula
  formula_str <- paste0(yvar, " ~ ", paste(xvars, collapse = " + "))

  all_model_stats = list()
  subgroup_col = paste0(subgroup_type, '_subgroup')
  for (subgroup_value in unique(all_data[[subgroup_col]])) {

    data = all_data %>% filter(get(subgroup_col) == subgroup_value)

    # Run regression
    out <- run_lm(data, as.formula(formula_str), is_true(more_params[['robust']], default = T))

    # Get stats
    betas_of_interest = xvars[!grepl('^blockid|^new_blockid|_block_', xvars)]
    model_stats = get_model_stats(out$model, out$m.out, betas_of_interest, formula_str, subgroup_value, adj_r2 = T) %>%
      mutate(subgroup = case_when(subgroup_type == 'full_sample' ~ subgroup_type,
                                   TRUE ~ paste0(subgroup_type, '__', subgroup_value)))

    # Run joint f-test
    betas_to_test <- intersect(betas_of_interest, model_stats %>% filter(!is.na(beta)) %>% pull(term))
    hypotheses <- str_c(betas_to_test, " = 0")
    if (df.residual(out$model) != 0) {
      joint_ftest <- linearHypothesis(out$model, hypotheses, vcov = vcovHC(out$model, "HC1"), singular.ok = TRUE)
      p_joint_ftest <- joint_ftest[2, "Pr(>F)"]
    } else {
      p_joint_ftest <- NA
    }
    model_stats <- model_stats %>% mutate(p_joint_ftest = p_joint_ftest)

    all_model_stats[[subgroup_value]] = model_stats
  }

  all_model_stats = bind_rows(all_model_stats)

  return(all_model_stats)
}


run_poisson_itt_and_ame_itt <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  # Add subgroup interaction_terms (does nothing if subgroup == full_sample)
  data <- add_subgroup_interaction_terms(data, subgroup, tvar, take.var, treat_only = T)

  # Build ITT formula
  rhs <- configure_right_hand_side(data, tvar, take.var, xvars, subgroup)
  formula_str <- paste0(yvar, " ~ ", paste(rhs$treatment_side, collapse = " + "))

  # Run regression
  if (regression_type == 'xsection') {
    model <- glm(as.formula(formula_str), family = "poisson", data = data)
  } else {
    stop("Invalid regression_type for poisson ITT.")
  }

  ame_model <- model %>% margins(variables = rhs$treatment_vars, vcov = vcovHC(., "HC1"))

  if (is_true(more_params[['robust']], default = T)) {
    model <- model %>% coeftest(vcov = vcovHC(., "HC1"))
  }

  # Get stats
  itt_stats = get_model_stats(model, model, rhs$treatment_vars, formula_str, subgroup) %>%
    select(-term) %>% rename_with(~ str_c(., "_itt"), .cols = -subgroup)
  ame_itt_stats = get_model_stats(ame_model, ame_model, rhs$treatment_vars, formula_str, subgroup) %>%
    select(-term) %>% rename_with(~ str_c(., "_itt_ame"), .cols = -subgroup)

  poisson_stats = full_join(itt_stats, ame_itt_stats, by = 'subgroup')

  return(poisson_stats)
}


run_pathway_reweighted_itt_and_ao_itt <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  # Get weights by predicting pathway
  baseline_covars = xvars[!grepl('^blockid|^new_blockid|_block_', xvars)]
  phats_df = run_pathway_prediction_model(
    data,
    weight_model_yvar = more_params$weight_model_outcome,
    weight_model_xvars = baseline_covars
  )
  write_feather(phats_df, 'output/pathway_model_phats.feather')
  data <- left_join(data, phats_df %>% select(cluster, phat), by = 'cluster')

  # Create list of dataset with different exclusion criteria
  data_after_exclusions_list = filter_data_based_on_exclusions(
    data, more_params$cr_phat_upper_bound__ul, more_params$cr_phat_upper_bound__re)

  # Validate input type
  if (regression_type != 'xsection') {
    stop("Invalid regression_type for pathway reweighted ITT.")
  }

  # Run regressions and get stats
  all_res_list = list()
  for (exclusion_type in names(data_after_exclusions_list)) {

    data_after_exclusions = data_after_exclusions_list[[exclusion_type]]

    df.stats <- get_basic_descriptive_stats(data_after_exclusions, yvar, tvar, subgroup)
    df.reg <- run_itt(data_after_exclusions, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params)

    res <- full_join(df.stats, df.reg, by = 'subgroup')

    all_res_list[[exclusion_type]] = res
  }
  res = bind_rows(all_res_list, .id = 'type')

  # Create new primary outcome variables, multiplying control guys' primary
  # outcomes by -1/(1-d) and treatment guys by 1/d
  ao_data <- bind_rows(data_after_exclusions_list, .id = 'type') %>%
    mutate(d = mean(get(tvar)), .by = new_blockid) %>%
    mutate(!!sym(yvar) := case_when(get(tvar) == 1 ~ (1/d * get(yvar)),
                                     get(tvar) == 0 ~ (-1/(1-d) * get(yvar))))

  ao_res = get_ao_itt(ao_data, yvar, baseline_covars)

  res = left_join(res, ao_res, by = 'type')

  return(res)
}


run_covid_itt_and_diff_itt <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  # Get data setup for both the ITT and the "diff" model
  data <- add_subgroup_interaction_terms(data, subgroup, tvar, take.var)
  rhs <- configure_right_hand_side(data, tvar, take.var, xvars, subgroup)

  # Run ITT with pre covid vs. post covid as the subgroup
  itt_stats = run_itt(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params)

  # Build diff ITT formula
  group_indicator <- rhs$treatment_side[1]
  group_x_treatment <- rhs$treatment_vars[grepl(gsub('covid_subgroup__', '', group_indicator), rhs$treatment_vars)]

  formula_str <- paste0(yvar, ' ~ ', paste(
    c(tvar, group_indicator, group_x_treatment, xvars), collapse = " + "))

  # Run "diff" model
  diff_stats <- run_diff_model(data, formula_str, yvar, treatment_vars = group_x_treatment,
                               subgroup = 'full_sample', regression_type = regression_type)

  res <- bind_rows(diff_stats, itt_stats) %>%
    mutate(subgroup = ifelse(subgroup == 'full_sample', 'diff', subgroup))

  return(res)
}


run_fwer_resampling_itt <- function(data, yvar, tvar, take.var, xvars, subgroup, regression_type, more_params) {

  data <- data %>% mutate(row_num = 1:nrow(data))

  n_treat = data[[tvar]] %>% sum

  stat_df_list = list()
  reg_df_list = list()

  for (iter in 0:more_params$n_samples) {

    # Iteration 0 will be the observed value.
    # For all other iterations, randomly assign treatment (and recompute the index)
    if (iter != 0) {

      data <- data %>%
        mutate(
          permuted_tvar = as.numeric(row_num %in% sample(1:nrow(data), n_treat, replace = F)),
          i_three_components = create_index_for_window(dframe = data,
                                                       window = str_extract(yvar, "post_[0-9]+$"),
                                                       tvar = 'permuted_tvar',
                                                       components = c('v_shooting_or_homicide',
                                                                      'a_shooting_or_homicide',
                                                                      'a_partone_non_shooting_or_homicide')))

    } else {

      data <- data %>%
        mutate(permuted_tvar = get(tvar))

    }

    df.stats <- get_basic_descriptive_stats(data, yvar, 'permuted_tvar', subgroup)
    df.reg = run_itt(data, yvar, 'permuted_tvar', take.var, xvars, subgroup, regression_type, more_params)

    stat_df_list[[iter+1]] = df.stats
    reg_df_list[[iter+1]] = df.reg
  }

  df.stats = bind_rows(stat_df_list, .id = 'sampling_iter')
  df.reg = bind_rows(reg_df_list, .id = 'sampling_iter')

  res = full_join(df.stats, df.reg, by = c('subgroup', 'sampling_iter')) %>%
    select(subgroup, sampling_iter, everything()) %>%
    mutate(sampling_iter = as.numeric(sampling_iter) - 1)
    # Note: We subtract 1 because we want the sampling_iter to run from
    #       0 (the observed value) to n_samples

  return(res)
}
