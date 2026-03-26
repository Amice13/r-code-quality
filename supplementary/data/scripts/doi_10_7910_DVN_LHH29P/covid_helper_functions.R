

postprocess_covid_diff <- function(data, res, more_params) {

  # rename subgroup to term
  res <- res %>%
    rename(term = subgroup) %>%
    mutate(subgroup = 'full_sample')

  # rename terms to be more descriptive
  res <- res %>%
    mutate(term = gsub('covid__', 'treatment_', term))

  # compute diff basic stats
  post_covid_row = res %>% filter(term == 'treatment_post_covid')
  pre_covid_row = res %>% filter(term == 'treatment_pre_covid')
  res <- res %>%
    mutate(n = ifelse(term != 'diff', n,
                post_covid_row$n + pre_covid_row$n),
            treatment_mean = ifelse(term != 'diff', treatment_mean,
                post_covid_row$treatment_mean - pre_covid_row$treatment_mean),
            control_mean = ifelse(term != 'diff', control_mean,
                   post_covid_row$control_mean - pre_covid_row$control_mean))

  return(res)
}


run_diff_model <- function(data, formula_str, yvar, treatment_vars, subgroup, regression_type) {

  formula = as.formula(formula_str)

  out <- run_estimatr_lm_robust(formula, data = data)

  diff_stats = get_model_stats(out$model, out$m.out, treatment_vars, formula_str, subgroup) %>%
    select(-term) %>%
    rename_with(~ paste0(., "_itt"), .cols = -subgroup) %>%
    mutate(subgroup = "diff")

  return(diff_stats)
}
