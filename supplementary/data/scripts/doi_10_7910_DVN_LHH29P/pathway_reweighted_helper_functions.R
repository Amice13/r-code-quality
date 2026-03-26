
run_pathway_prediction_model <- function(data, weight_model_yvar, weight_model_xvars) {

  formula_str <- paste0(weight_model_yvar, " ~ ",
                        paste(weight_model_xvars, collapse = " + "))
  formula <- as.formula(formula_str)

  # logistic regression
  model <- stats::glm(formula, data = data, family = "binomial")

  phats_df <- data %>%
    mutate(phat = model$fitted.values,
            prediction_outcome = weight_model_yvar) %>%
    select(cluster, prediction_outcome, phat, pathway_subgroup) %>%
    arrange(desc(phat))

  return(phats_df)
}


filter_data_based_on_exclusions <- function(data, ul_upper_bound, re_upper_bound) {

  exclusion_data = list()

  exclusion_data[['re_exclusion']] <- data %>%
    filter(phat <= re_upper_bound,
            pathway_subgroup == "re")

  exclusion_data[['cr_re_exclusion']] <- data %>%
    filter(phat <= re_upper_bound,
            pathway_subgroup == "cr")

  exclusion_data[['ul_exclusion']] <- data %>%
    filter(phat <= ul_upper_bound,
            pathway_subgroup == "ul")

  exclusion_data[['cr_ul_exclusion']] <- data %>%
    filter(phat <= ul_upper_bound,
            pathway_subgroup == "cr")

  return(exclusion_data)
}


run_ao_prediction_model <- function(data, yvar, ao_model_xvars) {

  formula_str <- paste0(yvar, " ~ ",
                        paste(ao_model_xvars, collapse = " + "))
  formula <- as.formula(formula_str)

  model <- lm(formula, data = data)

  betas <- model %>%
    tidy() %>%
    select(term = term, beta = estimate)

  return(betas)
}


get_ao_itt <- function(ao_data, yvar, ao_model_xvars) {

  ao_eligible_exclusions = ao_data %>%
    filter(!grepl('^cr_', type)) %>%
    pull(type) %>% unique()

  all_ao_res_list = list()
  for (exclusion_type in ao_eligible_exclusions) {

    non_outreach_data = ao_data %>% filter(type == exclusion_type)
    outreach_data = ao_data %>% filter(type == paste0('cr_', exclusion_type))

    ao_betas = run_ao_prediction_model(non_outreach_data, yvar, ao_model_xvars)

    mean_outreach_xs = outreach_data %>%
      select(all_of(ao_model_xvars)) %>%
      summarize(across(everything(), mean)) %>%
      t() %>% as_tibble(rownames = 'term') %>%
      rename(mean = V1)

    itt = left_join(ao_betas, mean_outreach_xs, by = 'term') %>%
      filter(!is.na(beta)) %>%
      mutate(itt = case_when(term == "(Intercept)" ~ beta,
                             T ~ beta * mean))  %>%
      pull(itt) %>%
      sum()

    ao_res = tibble(ao_itt = itt)

    all_ao_res_list[[exclusion_type]] = ao_res
  }

  ao_res = bind_rows(all_ao_res_list, .id = 'type')

  return(ao_res)
}
