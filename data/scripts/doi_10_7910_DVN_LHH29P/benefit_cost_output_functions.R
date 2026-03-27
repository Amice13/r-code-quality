
build_bca_table <- function(version, bca_results, transfer_to_participants_tot, less_inclusive_outcomes, more_inclusive_outcomes) {

  if (version == "less") yvars <- tibble(Outcome = less_inclusive_outcomes) %>% mutate(order = row_number())
  if (version == "more") yvars <- tibble(Outcome = more_inclusive_outcomes) %>% mutate(order = row_number())

  df <- bca_results %>%
    inner_join(yvars, "Outcome") %>%
    arrange(order) %>%
    mutate(label = case_when(order == 1 ~ "READI Sample Victims",
                             order == 2 ~ "READI Sample Offenders",
                             order == 3 ~ "Legal System Costs",
                             order == 4 ~ "Productivity Loss from Incarceration",
                             order == 5 ~ NA_character_,
                             order == 6 ~ "Total Social Cost of Crime")) %>%
    select(order, label,
           control_mean = Control_mean, beta_itt = ITT_Beta, stderr_itt = `ITT_(SE)`, pval_itt = ITT_pval,
           ccm_tot = CCM, beta_tot = TOT_Beta, stderr_tot = `TOT_(SE)`, pval_tot = TOT_pval)

  df_beta <- df %>%
    select(-matches("stderr")) %>%
    mutate(star_itt = case_when(pval_itt < .01 ~ "***",
                                pval_itt < .05 ~ "**",
                                pval_itt < .1 ~ "*",
                                TRUE ~ ""),
           star_tot = case_when(pval_tot < .01 ~ "***",
                                pval_tot < .05 ~ "**",
                                pval_tot < .1 ~ "*",
                                TRUE ~ "")) %>%
    mutate_at(vars(matches("mean|ccm")), ~ glue("{dollar(., accuracy = 1)}")) %>%
    mutate_at(vars(matches("beta_itt")), ~ glue("{dollar(., accuracy = 1)}{star_itt}")) %>%
    mutate_at(vars(matches("beta_tot")), ~ glue("{dollar(., accuracy = 1)}{star_tot}")) %>%
    select(-matches("star")) %>%
    mutate(x = "beta")

  df_se <- df %>%
    select(order, matches("stderr")) %>%
    rename_at(vars(matches("stderr")), ~ gsub("stderr", "beta", .)) %>%
    mutate_at(vars(matches("beta")), ~ glue("({dollar(., accuracy = 1)})")) %>%
    mutate(x = "se") %>%
    # remove SE row of extra row
    filter(order!=5)

  # inflation/deflation ratios
  reciprocal_empirical_tot_to_itt_beta_ratio <- round(1/unique(round(bca_results$TOT_Beta/bca_results$ITT_Beta,8)),3)

  # admin costs
  ## numbers from method here: code/R/outcomes/benefit_cost/heartland_expenses/markdown/cost_per_person.Rmd
  readi_admin_costs_tot <- 55274
  readi_admin_costs_itt <- readi_admin_costs_tot * reciprocal_empirical_tot_to_itt_beta_ratio
  transfer_to_participants_itt <- transfer_to_participants_tot * reciprocal_empirical_tot_to_itt_beta_ratio

  # net readi costs
  net_readi_costs_tot <- readi_admin_costs_tot - transfer_to_participants_tot
  net_readi_costs_itt <- readi_admin_costs_itt - transfer_to_participants_itt

  df_ratio <- df_beta %>%
    filter(label=="Total Social Cost of Crime") %>%
    select(label, beta_itt, beta_tot) %>%
    mutate(label = "Benefit-Cost Ratio",
           beta_itt = (gsub("\\$|\\*|\\,", "", beta_itt) %>% as.numeric() %>% .[1] * -1)/net_readi_costs_itt,
           beta_tot = (gsub("\\$|\\*|\\,", "", beta_tot) %>% as.numeric() %>% .[1] * -1)/net_readi_costs_tot) %>%
    mutate_at(vars(matches("beta")), ~ glue("{round(.,1)}:1"))

  bind_rows(df_beta,df_se) %>%
    arrange(order) %>%
    bind_rows(tibble(order = 6, label = "Administrative Costs",
                     beta_itt = glue("{dollar(readi_admin_costs_itt, accuracy = 1)}"),
                     beta_tot = glue("{dollar(readi_admin_costs_tot, accuracy = 1)}")),
              tibble(order = 6, label = "Transfer to Participants",
                     beta_itt = glue("{dollar(transfer_to_participants_itt*-1, accuracy = 1)}"),
                     beta_tot = glue("{dollar(transfer_to_participants_tot*-1, accuracy = 1)}")),
              tibble(order=7, label='', beta_itt = '', beta_tot = ''),
              tibble(order = 8, label = "Net READI Costs",
                     beta_itt = glue("{dollar(net_readi_costs_itt, accuracy = 1)}"),
                     beta_tot = glue("{dollar(net_readi_costs_tot, accuracy = 1)}")),
              df_ratio) %>%
    # include space for extra row by adding NAs
    mutate(across(c("control_mean", "beta_itt", "ccm_tot", "beta_tot"), ~if_else(order == 5 & !is.na(order), glue("NA"), .x))) %>%
    mutate(across(c("pval_itt", "pval_tot"), ~if_else(order == 5 & !is.na(order), NA_real_, .x))) %>%
    mutate(across(-order, ~ replace(.x, list = c(rep(FALSE,8), TRUE, rep(FALSE,6)), NA)))
}

print_bca_table <- function(bca_results, transfer_to_participants_tot, less_inclusive_outcomes, more_inclusive_outcomes) {

  bca_table_less <- build_bca_table("less", bca_results, transfer_to_participants_tot, less_inclusive_outcomes, more_inclusive_outcomes) %>%
    rename_with(~paste0(.x, "_less"))
  bca_table_more <- build_bca_table("more", bca_results, transfer_to_participants_tot, less_inclusive_outcomes, more_inclusive_outcomes) %>%
    rename_with(~paste0(.x, "_more"))

  full_table <- bind_cols(bca_table_less, bca_table_more) %>%
    select(-c(starts_with("order"), label_more, x_less, x_more, contains("pval"))) %>%
    mutate(label_less = case_when(label_less=="Productivity Loss from Incarceration" ~ "Productivity Loss from",
                                  lag(label_less)=="Productivity Loss from Incarceration" ~ "Incarceration",
                                  TRUE ~ label_less))

  full_table %>%
    kable(booktabs=T, longtable=T,
          col.names = c("", "CM", "ITT", "CCM", "TOT", "CM", "ITT", "CCM", "TOT"),
          align=c("l", rep("c", 9)),
          escape = TRUE) %>%
    kable_styling(font_size = 10) %>%
    add_header_above(c(" " = 1, "Less Inclusive Estimates" = 4, "More Inclusive Estimates" = 4)) %>%
    group_rows("Social Cost of Victimization", 1, 4, latex_gap_space = "0.6em") %>%
    group_rows("Social Cost of Punishment", 5, 8, latex_gap_space = "0.6em") %>%
    group_rows("Social Cost of Program", 12, 13, latex_gap_space = "0.6em") %>%
    row_spec(11, hline_after = TRUE) %>%
    row_spec(15, hline_after = TRUE) %>%
    column_spec(1, width="14em") %>%
    column_spec(c(3, 5, 7, 9), width="5.2em") %>%
    column_spec(c(2, 4, 6, 8), width="4.4em") %>%
    add_indent(8)

}