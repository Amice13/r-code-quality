
build_subgroup_table <- function(req_subgroup, model_results, fwer_results) {

  model_results %<>% filter(subgroup_type == req_subgroup)

  fwer <- fwer_results %>%
    filter(subgroup_type == req_subgroup) %>%
    select(outcome, subgroup, p_fwer_star, fdrq)

  df_beta <- model_results %>%
    select(-c(subgroup_type, p_subgroup_ftest_itt, matches("stderr"))) %>%
    left_join(fwer, c("outcome", "subgroup")) %>%
    mutate(across(matches("beta|control_mean|ccm"), ~ fmt(.x, decimals = 4))) %>%
    mutate(across(c(pval_itt, p_fwer_star, fdrq), ~ fmt(.x, decimals = 3)))

  df_se <- model_results %>%
    select(outcome, subgroup, matches("stderr")) %>%
    rename_at(vars(matches("stderr")), ~ gsub("stderr", "beta", .)) %>%
    mutate(across(matches("beta"), ~ fmt(.x, decimals = 4, parens = TRUE)))

  combo <- bind_rows(df_beta, df_se) %>%
    arrange(desc(outcome), subgroup) %>%
    select(outcome, subgroup, n, control_mean, beta_itt, ccm_tot, beta_tot, pval_itt, p_fwer_star, fdrq) %>%
    mutate(order = case_when(outcome =="i_three_components" ~ 1)) %>%
    arrange(order, subgroup)  %>%
    mutate(label1 = case_when(outcome == "i_three_components" ~ subgroup),
           label1 = case_when(label1 == "over_median" ~ "Over Median",
                              label1 == "under_median" ~ "Under Median",
                              label1 == "missing" ~ "Missing",
                              label1 == "awg" ~ "Austin/West Garfield Park",
                              label1 == "eng" ~ "Greater Englewood",
                              label1 == "nol" ~ "North Lawndale",
                              label1 == "cr" ~ "Outreach",
                              label1 == "ul" ~ "Algorithm",
                              label1 == "re" ~ "Re-entry"),
           label1 = case_when(outcome == "i_three_components" ~ glue("{label1} (N = {n})"),
                              TRUE ~ label1),
           label1 = case_when(!is.na(control_mean) ~ label1),
           label2 = case_when(outcome != "i_three_components" ~ outcome),
           label2 = case_when(label2 == "v_shooting_or_homicide" ~ "Shooting & Homicide Victimizations",
                              label2 == "a_shooting_or_homicide" ~ "Shooting & Homicide Arrests",
                              label2 == "a_partone_non_shooting_or_homicide" ~ "Other Serious Violent-Crime Arrests"),
           label2 = case_when(!is.na(control_mean) ~ label2),
           label = case_when(!is.na(label1) ~ label1,
                             TRUE ~ label2)
           #n = case_when(!is.na(label1) ~ glue("N = {as.character(n)}"),
          #               TRUE ~ '')
    ) %>%
    select(-c(label1, label2, n))

  if (req_subgroup == "risk"){
    combo %<>%
      mutate(subgroup = factor(subgroup,
                               levels = c("over_median",
                                          "under_median",
                                          "missing")),
             outcome = factor(outcome, levels = c("i_three_components",
                                                  "v_shooting_or_homicide",
                                                  "a_shooting_or_homicide",
                                                  "a_partone_non_shooting_or_homicide"))
      ) %>%
      arrange(order, subgroup, outcome)
  }

  if (req_subgroup == "pathway"){
    combo %<>%
      mutate(subgroup = factor(subgroup, levels = c("ul", "cr", "re")),
             outcome = factor(outcome, levels = c("i_three_components",
                                                  "v_shooting_or_homicide",
                                                  "a_shooting_or_homicide",
                                                  "a_partone_non_shooting_or_homicide"))) %>%
      arrange(order, subgroup, outcome)
  }

  combo %>%
    select(-c(order, outcome, subgroup)) %>%
    select(matches("label"), everything())

}

print_outcomes_by_subgroup_table <- function(req_subgroup, model_results, fwer_results = NULL, config) {

  config <- config[[req_subgroup]]

  if (is.null(config$col_names)) config$col_names <- c("", "CM", "ITT", "CCM", "TOT", "Observed ITT", "FWER", "FDR-q")
  if (is.null(config$align)) config$align <- c("l", rep("c", 7))
  if (is.null(config$font_size)) config$font_size <- 11

  if (!grepl('ftest', req_subgroup)) {
    subgroup_table <- build_subgroup_table(req_subgroup, model_results, fwer_results)
  } else {
    subgroup_table <- model_results
  }

  subgroup_table %<>%
    replace(is.na(.), '') %>%
    mutate(across(everything(), ~ gsub('NA', '', .x))) %>%
    kable(booktabs=T, longtable=T,
          col.names = config$col_names,
          align = config$align,
          linesep = '') %>%
    kable_styling(font_size = config$font_size)

  # group rows
  if (!is.null(config$group_rows)){
    for (row in 1:length(config$group_rows$labels)){
      subgroup_table %<>%
        group_rows(config$group_rows$labels[[row]],
                   config$group_rows$start_row[[row]],
                   config$group_rows$end_row[[row]],
                   latex_gap_space = ".6em")
    }
  }


  # header
  if (config$header) {
    subgroup_table %<>% add_header_above(c(" " = 1, "Estimates" = 4, "P-values" = 4))
  }

  # indent row
  if (!is.null(config$indent_row)){
    subgroup_table %<>% add_indent(c(config$indent_row$start:config$indent_row$end))
  }

  # column spec
  if (is.null(config$column_spec)) {
    subgroup_table %<>%
      column_spec(1, width="20em", latex_valign = 'm') %>%
      column_spec(3:10, width="3.5em", latex_valign = 'm')
  } else {
    for (col in 1:length(config$column_spec)){
      subgroup_table %<>%
        column_spec(config$column_spec$col[[col]],
                    width = config$column_spec$width[[col]])
    }

  }

  subgroup_table

}
