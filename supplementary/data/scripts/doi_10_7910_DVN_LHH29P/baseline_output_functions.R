
makeBaselineTable <- function(baseline_stats, joint_stats, subgroup_type) {

  means_df <- baseline_stats %>%
    filter(grepl(paste0("^", subgroup_type), subgroup)) %>%
    left_join(baseline_vars, "outcome") %>%
    arrange(order) %>%
    mutate(across(matches("mean|pval"), ~ case_when(.x > 1.15 ~ fmt(.x, decimals = 1),
                                                    TRUE ~ fmt(.x)))) %>%
    mutate(type = case_when(outcome %in% params$baseline_outcomes_varlist ~ "out",
                            outcome %in% params$baseline_demos_varlist ~ "demo",
                            outcome %in% params$baseline_uphat_varlist ~ "risk",
                            outcome %in% params$baseline_arrests_varlist ~ "arr",
                            outcome %in% params$baseline_vics_varlist ~ "vic",
                            outcome %in% params$baseline_incar_varlist ~ "incar",
                            TRUE ~ "test")) %>%
    mutate(label = case_when(label=="Risk Score" ~ "Predicted Involvement in a Violent Gun Crime (Risk Score)",
                             TRUE ~ label)) %>% 
    rename(pval = pval_itt) %>%
    select(subgroup, order, type, label, control_mean, treatment_mean, pval)

  n_df <- baseline_stats %>%
    filter(grepl(paste0("^", subgroup_type), subgroup)) %>%
    select(subgroup, contains("_n")) %>%
    group_by(subgroup) %>%
    summarize(across(everything(), max), .groups='drop') %>%
    select(subgroup, control_mean = control_n, treatment_mean = treatment_n) %>%
    mutate(across(matches("mean"), ~ as.character(.x))) %>%
    mutate(label = "N",
           type = "n",
           order = 0) %>%
    select(subgroup, order, type, label, control_mean, treatment_mean)

  ftest_df <- joint_stats %>%
    filter(grepl(paste0("^", subgroup_type), subgroup)) %>%
    select(subgroup, pval = p_joint_ftest) %>%
    distinct() %>%
    mutate(across(matches("pval"), ~ fmt(.x))) %>%
    mutate(label = 'p-value on F-test',
           type = 'test',
           order = max(means_df$order)+1)

  df = bind_rows(n_df, means_df, ftest_df)

  return(df)
}


printBaselineTable <- function(df, n_subgroups, font_size) {

  col_names <- c("", rep(c("Control Mean", "Treatment Mean", "Pairwise p-value"), n_subgroups))
  n_cols <- length(col_names) - 1

  for (i in c("n", "demo", "out", "risk", "arr", "vic", "incar", "test")) {
    r1 <- df %>% filter(type == i) %>% pull(order) %>% as.numeric %>% min
    r2 <- df %>% filter(type == i) %>% pull(order) %>% as.numeric %>% max
    assign(paste0(i, 1), r1)
    assign(paste0(i, 2), r2)
  }

  df_for_table <- df %>%
    select(-order, -type)

  table = df_for_table %>%
    kable(booktabs=T, longtable=T,
          col.names = col_names,
          align=c("l", rep("c", n_cols)),
          linesep = '') %>%
    kable_styling(font_size = font_size) %>%
    group_rows("Demographics", demo1 + 1, demo2 + 1) %>%
    group_rows("Primary Outcome Components, Counts", out1 + 1, out2 + 1) %>%
    group_rows("Risk Prediction", risk1 + 1, risk2 + 1) %>%
    group_rows("Arrest Counts", arr1 + 1, arr2 + 1) %>%
    group_rows("Victimization Counts", vic1 + 1, vic2 + 1) %>%
    group_rows("Incarceration Measures", incar1 + 1 , incar2 + 1 ) %>%
    group_rows("Joint Test", test1 + 1, test2 + 1)

  return(table)
}

printFullSampleTable <- function(baseline_stats, joint_stats, subgroup_type, latex = T) {

  df <- makeBaselineTable(baseline_stats, joint_stats, subgroup_type) %>%
    select(order, type, label, control_mean, treatment_mean, pval)

  table = printBaselineTable(df, n_subgroups = 1, font_size = 11)

  if (latex) {
    table <- table %>%
      column_spec(1, width="22em") %>%
      column_spec(2:5, width="5em")
  }

  return(table)
}

printSubgroupTable <- function(baseline_stats, joint_stats, subgroup_type, subgroup_order_and_label_map, latex = T) {

  subgroup_order = subgroup_order_and_label_map[[subgroup_type]]
  col_order = c(t(sapply(c('control_mean_', 'treatment_mean_', 'pval_'), paste0, names(subgroup_order))))

  headers = c(1, rep(3, length(subgroup_order)))
  names(headers) = c(" ", subgroup_order)

  df <- makeBaselineTable(baseline_stats, joint_stats, subgroup_type) %>%
    pivot_wider(names_from = subgroup,
                values_from = c(control_mean, treatment_mean, pval)) %>%
    select(order, type, label, all_of(col_order))

  if (subgroup_type == 'pathway') {
    df <- df %>%
      mutate(pval_pathway__ul = case_when(label == 'Missing Risk Score' ~ '-',
                                          TRUE ~ pval_pathway__ul))
  }

  table = printBaselineTable(df, n_subgroups = length(subgroup_order), font_size = 10) %>%
    add_header_above(headers)

  if (latex) {
    table <- table %>%
      column_spec(1, width="18em") %>%
      column_spec(2:10, width="3.6em")
  }

  return(table)
}
