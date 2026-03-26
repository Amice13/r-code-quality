
load_analysis_file <- function(analysis_file, df_type, subgroup_definitions) {

  df <- read_feather(analysis_file) %>%
    add_subgroup_indicators(subgroup_definitions[[df_type]])

  # add weights needed for IPW
  df <- df %>%
    group_by(blockid) %>%
    mutate(p_treat = mean(treatment)) %>%
    ungroup() %>%
    mutate(ipw_weight = case_when(treatment == T ~ 1/p_treat,
                                  treatment == F ~ 1-(1-p_treat)))

  return(df)
}


is_true <- function(null_or_bool, default = T) {

  if (is.null(null_or_bool)) {
    return(default)
  } else {
    return(null_or_bool)
  }

}


filter_to_correct_sample <- function(df, sample_name, sample_set_lookup) {

  if (sample_name == 'full') {
    df_sample = df
  } else {
    sample_set_logic = sample_set_lookup[[sample_name]]
    df_sample <- df %>%
      filter(eval(parse(text = sample_set_logic)))
  }

  return(df_sample)

}


limit_to_post_period_window <- function(df, more_params) {

  win = as.integer(gsub('post_', '', more_params$window_cut))
  panel_max = max(df$months)

  if (win > panel_max) {
    message(glue("WARNING: The post-period window you're using ({win}) may exceed the panel max ({round(panel_max, 3)})."))
  }

  df <- df %>%
    filter(months <= win)

  return(df)
}


get_matching_cols = function(data, regex_list) {

  if (length(regex_list) == 0) {
    return(NULL)
  }

  regex = paste(regex_list, collapse = '|')

  matching_cols = data %>%
    select(matches(regex)) %>%
    names()

  return(matching_cols)
}


get_covariates <- function(data, covariate_descr, covariate_set_lookup, subgroup) {

  # determine if the covariate_descr is the name of a
  # meta covariate set or a normal covariate set
  if (covariate_descr %in% names(covariate_set_lookup[['meta_covariate_sets']])) {
    covariate_set_list = covariate_set_lookup[['meta_covariate_sets']][[covariate_descr]]
  } else {
    covariate_set_list = c(covariate_descr)
  }

  all_covariates = c()
  for (covariate_set in covariate_set_list) {

    covariate_set_components = strsplit(covariate_set, split = '__')[[1]]

    for (covariate_set_component in covariate_set_components) {

      if (covariate_set_component == 'subgroup_specific') {
        covariate_set_component = paste0("subgroup__", subgroup)
      }

      covariates_to_add = covariate_set_lookup[['covariate_sets']][[covariate_set_component]][['add']]
      covariates_to_drop = covariate_set_lookup[['covariate_sets']][[covariate_set_component]][['drop']]

      covariates_to_add_regex = get_matching_cols(
        data, covariate_set_lookup[['covariate_sets']][[covariate_set_component]][['add_regex']])
      covariates_to_drop_regex = get_matching_cols(
        data, covariate_set_lookup[['covariate_sets']][[covariate_set_component]][['drop_regex']])

      all_covariates = c(all_covariates, covariates_to_add, covariates_to_add_regex)
      all_covariates = setdiff(all_covariates, c(covariates_to_drop, covariates_to_drop_regex))

    }
  }

  return(all_covariates)
}


add_subgroup_indicators <- function(df, subgroup_definitions) {

  person_level_df = df[!duplicated(df$UL_id_unique),]

  for (subgroup in names(subgroup_definitions)) {

    cat_col_name = paste0(subgroup, "_subgroup")
    df <- df %>% mutate(cat_col = NA_character_)

    for (subgroup_value in names(subgroup_definitions[[subgroup]])) {

      subgroup_value_logic = subgroup_definitions[[subgroup]][[subgroup_value]]

      # Ensure that we are always taking the median of a person-level file,
      # even if the input df is a panel dataset
      subgroup_value_logic = gsub(
        'median(', 'median(person_level_df$', subgroup_value_logic, fixed = T)

      ind_col_name = paste0(subgroup, "_subgroup__", subgroup_value)

      df <- df %>%
        mutate(ind_col = eval(parse(text = subgroup_value_logic)))

      if (df %>% pull(ind_col) %>% sum() > 0) {

        df <- df %>%
          mutate(cat_col = case_when(ind_col == TRUE ~ subgroup_value,
                                     TRUE ~ cat_col)) %>%
          mutate(!!ind_col_name := as.numeric(ind_col)) %>%
          select(-ind_col)
      }
    }

    df <- df %>%
      mutate(!!cat_col_name := cat_col) %>%
      select(-cat_col)

  }

  df <- df %>%
    mutate(full_sample_subgroup = 'full_sample')

  return(df)
}


add_subgroup_interaction_terms <- function(df, subgroup, tvar, take.var = NULL, treat_only = F) {

  if (subgroup == 'full_sample') {
    return(df)
  }

  subgroup_indicator_list = names(df)[grepl(paste0('^', subgroup, '_subgroup__'), names(df))]

  for (subgroup_indicator in subgroup_indicator_list) {

    subgroup_value = strsplit(subgroup_indicator, split = '__')[[1]][2]

    df <- df %>%
      mutate(
        !!paste0("subgroup_x_treatment__", subgroup_value) := get(subgroup_indicator) * get(tvar)
      )

    # Note: the treat_only parameter is only so we can save time during the FWER itt
    if (treat_only == FALSE) {
      df <- df %>%
        mutate(
          !!paste0("subgroup_x_control__", subgroup_value) := get(subgroup_indicator) * control,
          !!paste0("subgroup_x_takeup__", subgroup_value) := get(subgroup_indicator) * get(take.var),
        )
    }

  }

  return(df)
}


get_model_stats <- function(model, m.out, beta_vars, formula_str, subgroup_type, adj_r2 = F) {

  stats = m.out %>%
    broom::tidy() %>%
    as_tibble() %>%
    filter(term %in% beta_vars) %>%
    select(term,
            beta = estimate,
            stderr = `std.error`,
            tstat = statistic,
            pval = `p.value`) %>%
    mutate(
      istar = get_stars(pval),
      subgroup = gsub('^[a-zA-Z_]+__', '', term),
      formula = formula_str)

  if (adj_r2) {
    stats <- stats %>%
      mutate(adj_r2 = summary(model)$adj.r.squared)
  }

  if (subgroup_type == 'full_sample') {
    stats <- stats %>%
      mutate(subgroup = subgroup_type)
  } else {
    stats <- stats %>%
      mutate(subgroup = paste0(subgroup_type, "__", subgroup))
  }

  return(stats)
}


scale_by_avg_post_days <- function(data, res, more_params) {

  avg_days_in_time_period = more_params$days_per_month * get_numeric_window(more_params$window_cut)

  # Get the metrics we want to scale
  metrics_to_scale = res %>%
    select(starts_with('control_mean'),
           starts_with('treatment_mean'),
           starts_with('beta'),
           starts_with('stderr'),
           starts_with('ccm')) %>%
    names()

  # Add "scaled_" copies of columns to be scaled
  res <- res %>%
    select(all_of(metrics_to_scale)) %>%
    rename_with(~ paste0("scaled_", .)) %>%
    bind_cols(res, .)

  # Actually scale the "scaled_" copies
  scaled_cols = res %>% select(starts_with("scaled_")) %>% names()
  res <- res %>%
    mutate(across(all_of(scaled_cols), ~ .x * avg_days_in_time_period))

  if (!is_true(more_params[['keep_unscaled_values']], default = FALSE)) {
    res <- res %>%
      select(-all_of(metrics_to_scale)) %>%
      rename_with(gsub, pattern = '^scaled_', replacement = '')
  }

  return(res)
}


winz_top <- function(x, pctl_universe, pctl = 0.99) {

  if (pctl == 0 | !is.numeric(x)) {
    return(x)
  } else {
    pctl_max <- quantile(x[pctl_universe], pctl, na.rm = T, type = 6)
    i = which(x > pctl_max)
    x[i] = pctl_max
    return(x)
  }
}


standardize <- function(data, more_params) {

  non_binary_xvars = c()
  for (xvar in more_params$xvars) {
    if (any(!data[[xvar]] %in% c(0, 1))) {
      non_binary_xvars = c(non_binary_xvars, xvar)
    }
  }

  scaled_data <- data %>%
    mutate(across(all_of(non_binary_xvars), ~ scale(.x)))

  return(scaled_data)
}


winsorize <- function(data, more_params) {

  winsorize_regex_list = c('^a_', '^v_', '^y_')
  if (!is.null(more_params$winsorize_regex_list)) {
    winsorize_regex_list = more_params$winsorize_regex_list
  }

  winz_pctl = .99
  if (!is.null(more_params$winsorize_pctl)) {
    winz_pctl = more_params$winsorize_pctl
  }

  winsorize_regex = paste(winsorize_regex_list, collapse = '|')

  pctl_universe = rep(TRUE, nrow(data))
  if (is_true(more_params[['winsorize_w_control']], default = FALSE)) {
    pctl_universe = data$treatment == 0
  }

  winsorized_data <- data %>%
    mutate(across(matches(winsorize_regex), ~ winz_top(.x,
                                                       pctl_universe = pctl_universe,
                                                       pctl = winz_pctl
    )))

  return(winsorized_data)
}


configure_right_hand_side = function(data, tvar, take.var, xvars, subgroup) {

  if (subgroup == 'full_sample') {

    # if NOT doing a subgroup regression
    treatment_vars = c(tvar)
    control_vars = c('control')
    takeup_vars = c(take.var)
    treatment_side = c(treatment_vars, xvars)
    takeup_side = c(takeup_vars, xvars)

  } else {

    # if doing a subgroup regression
    subgroup_vars = names(data)[grepl(paste0("^", subgroup, "_subgroup__"), names(data))]
    subgroup_vars_drop1 = head(subgroup_vars, -1)

    treatment_vars = names(data)[grepl('subgroup_x_treatment', names(data))]
    control_vars = names(data)[grepl('subgroup_x_control', names(data))]
    takeup_vars = names(data)[grepl('subgroup_x_takeup', names(data))]
    treatment_side = c(subgroup_vars_drop1, treatment_vars, xvars)
    takeup_side = c(subgroup_vars_drop1, takeup_vars, xvars)
  }

  rhs = list(
    treatment_vars = treatment_vars,
    control_vars = control_vars,
    takeup_vars = takeup_vars,
    treatment_side = treatment_side,
    takeup_side = takeup_side
  )

  return(rhs)
}


get_basic_descriptive_stats <- function(mdata, yvar, tvar, subgroup_type) {

  subgroup_col = paste0(subgroup_type, '_subgroup')

  stats <- mdata %>%
    group_by_at(c(tvar, subgroup_col)) %>%
    summarize(
      n = length(unique(cluster)),
      m = mean.default(get(yvar), na.rm = TRUE),
      .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(tvar), values_from = c(n, m)) %>%
    rename(subgroup = all_of(subgroup_col)) %>%
    mutate(subgroup = case_when(
      subgroup == 'full_sample' ~ subgroup,
      TRUE ~ paste0(subgroup_type, '__', subgroup)))

  if (!'m_1' %in% names(stats)) {
    stats <- stats %>%
      mutate(m_1 = NA, n_1 = 0)
  }
  if (!'m_0' %in% names(stats)) {
    stats <- stats %>%
      mutate(m_0 = NA, n_0 = 0)
  }

  stats = stats %>%
    mutate(n = n_1 + n_0) %>%
    rename(treatment_mean = m_1,
           control_mean = m_0,
           treatment_n = n_1,
           control_n = n_0) %>%
    select(subgroup, n, control_n, treatment_n, control_mean, treatment_mean)

  return(stats)
}


get_tot_ccm <- function(tdata, m.out, treatment_vars, control_vars, takeup_vars, yvar, subgroup_type) {

  all_ccm_stats = list()
  for (var_ix in 1:length(takeup_vars)) {

    takeup_indicator = takeup_vars[var_ix]
    treatment_indicator = treatment_vars[var_ix]
    control_indicator = control_vars[var_ix]

    beta = m.out %>%
      broom::tidy() %>%
      filter(term == takeup_indicator) %>%
      pull(estimate)

    a <- tdata %>% filter(get(treatment_indicator) == 1, get(takeup_indicator) == 1) %>%
      pull(yvar) %>% mean(na.rm=T)
    b <- tdata %>% filter(get(treatment_indicator) == 1) %>%
      pull(takeup_indicator) %>% mean(na.rm=T)
    c <- tdata %>% filter(get(control_indicator) == 1, get(takeup_indicator) == 1) %>%
      pull(yvar) %>% mean(na.rm=T)
    d <- tdata %>% filter(get(control_indicator) == 1) %>%
      pull(takeup_indicator) %>% mean(na.rm=T)

    if (is.na(a)) a <- 0
    if (is.na(b)) b <- 0
    if (is.na(c)) c <- 0
    if (is.na(d)) d <- 0

    preCCM <- (a*b - c*d) / (b-d)
    ccm <- preCCM - beta

    if ((ccm < 0) & (substr(yvar, 1, 1) != "i")) ccm <- 0
    # See explanation on pg 94 of Experimental Analysis of Neighborhood Effects (Kling, Liebman, Katz)
    # https://www.povertyactionlab.org/sites/default/files/publications/705%20experimental%20analysis%20of%20neighborhood%20effects%202007_0.pdf

    subgroup = strsplit(takeup_indicator, split = '__')[[1]][2]
    ccm_stats = tibble(ccm_tot = ccm)

    if (subgroup_type == 'full_sample') {
      ccm_stats <- ccm_stats %>%
        mutate(subgroup = subgroup_type)
    } else {
      ccm_stats <- ccm_stats %>%
        mutate(subgroup = str_c(subgroup_type, "__", subgroup))
    }

    all_ccm_stats[[var_ix]] = ccm_stats

  }

  all_ccm_stats = bind_rows(all_ccm_stats)

  return(all_ccm_stats)

}


run_subgroup_ftest <- function(model, treatment_vars) {

  # get all pairs of treatment variables (then drop one to prevent error)
  treatment_var_pair_list = combn(treatment_vars, 2, simplify = F)
  if (length(treatment_var_pair_list) > 1) {
    treatment_var_pair_list <- head(treatment_var_pair_list, -1)
  }

  hypotheses = c()
  for (treatment_var_pair in treatment_var_pair_list) {
    hyp = glue("{treatment_var_pair[1]} = {treatment_var_pair[2]}")
    hypotheses = c(hypotheses, hyp)
  }
  subgroup_ftest <- linearHypothesis(model, hypotheses, vcov = vcovHC(model, "HC1"), singular.ok = TRUE)
  p_subgroup_ftest <- subgroup_ftest[2, "Pr(>F)"]

  return(p_subgroup_ftest)

}


# can be used for anything; used for xsectional ITT
run_lm <- function(data, formula, robust = T, weight_col = NULL) {

  if (!is.null(weight_col)) {
    data <- data %>% mutate(weight = get(weight_col))
    model <- lm(formula, data = data, weights = weight)
  } else {
    model <- lm(formula, data = data)
  }

  if (robust) {
    m.out <- coeftest(model, vcov = vcovHC(model, "HC1"))
  } else {
    m.out <- model
  }

  return(list("model"=model, "m.out"=m.out))
}


# used for panel ITT
run_estimatr_lm_robust <- function(data, formula) {

  model <- estimatr::lm_robust(formula,
                               data = data,
                               clusters = cluster,
                               se_type = "stata")

  return(list("model" = model, "m.out" = model))
}


# used for xsectional TOT
run_ivreg <- function(data, formula, robust = T, weight_col = NULL) {

  if (!is.null(weight_col)) {
    data <- data %>% mutate(weight = get(weight_col))
    model <- ivreg(formula, data = data, weights = weight)
  } else {
    model <- ivreg(formula, data = data)
  }

  if (robust) {
    m.out <- robust.se(model)
    # Note: ivpack::robust.se is deprecated because ivpack is no longer on CRAN;
    #     coeftest(model, vcov = vcovHC(model, "HC1")) is not equivalent;
    #     ivpack can still be installed via: devtools::install_version("ivpack", version = "1.2")
  } else {
    m.out <- model
  }

  return(list("model" = model, "m.out" = m.out))
}


# used for panel TOT
run_estimatr_iv_robust <- function(data, formula) {

  model <- estimatr::iv_robust(formula,
                               data = data,
                               clusters = cluster,
                               se_type = 'stata')

  return(list("model" = model, "m.out" = model))
}

