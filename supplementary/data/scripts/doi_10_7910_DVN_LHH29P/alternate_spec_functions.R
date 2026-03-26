
read_format_dl_result <- function(path) {
  # warnings are usually created because the column names in line 5 are included in the first row of the spreadsheet
  # and enforcing column type numeric turns them into NA
  suppressWarnings(read_xlsx(path, col_types = c("text", rep("numeric", 6)))) %>%
    set_names("outcome", "beta_itt", "stderr_itt", "Z", "pval_itt", "95% CI Lower", "95% CI Upper") %>%
    mutate(outcome = lag(outcome)) %>%
    slice(2) %>%
    select(-c(Z, `95% CI Lower`, `95% CI Upper`)) %>%
    mutate(window = 'post_20',
           covariate_set = "baseline_lasso__blocks",
           subgroup = "full_sample",
           specification = 'OLS: Covariates selected using double LASSO') %>%
  select(outcome, window, covariate_set, subgroup, beta_itt, stderr_itt, pval_itt, specification) %>%
  mutate(outcome = case_when(outcome == "i_three_c~20" ~ "i_three_components",
                             outcome == "v_shootin~20" ~ "v_shooting_or_homicide",
                             outcome == "a~om_post_20" ~ "a_partone_non_shooting_or_homicide",
                             outcome == "a_shooting.." ~ "a_shooting_or_homicide"))
}

format_results <- function(results) {

  df_beta <- results %>%
    select(-stderr_itt)

  df_se <- results %>%
    select(-c(control_mean, beta_itt,pval_itt)) %>%
    mutate(stderr_itt = paste0('(', stderr_itt, ')')) %>%
    rename_at(vars(matches("stderr")), ~ gsub("stderr", "beta", .))

  df <- bind_rows(df_beta, df_se) %>%
    arrange(spec_order, primary_component_order) %>%
    rename(outcome = label) %>%
    mutate(outcome = case_when(is.na(control_mean) ~ "",
                               TRUE ~ outcome)) %>%
    select(-c(specification, spec_order, primary_component_order))

  linesep = rep(c('', '\\addlinespace'), times = 15)
  table <- df %>%
    kable(booktabs=T, longtable=T,
          col.names = c("", "CM", "ITT/AME", "P-value"),
          align=c("l", rep("c", 4)),
          linesep=linesep) %>%
    kable_styling(font_size = 10) %>%
    group_rows("OLS: Index pooling arrests into a single component", 1, 2) %>%
    group_rows("OLS: No covariates except randomization blocks", 3, 10) %>%
    group_rows("OLS: Covariates selected using double LASSO", 11, 18) %>%
    group_rows("OLS: Alternative covariates + IPW", 19, 26) %>%
    group_rows("Poisson: Standard covariates", 27, 31) %>%
    column_spec(1, width="24em") %>%
    column_spec(2:5, width="4.2em")

  return(table)
}
