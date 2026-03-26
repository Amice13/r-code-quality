
add_group_rows <- function(kable, gr_config = config$group_row_labels, table_config, n_rows) {

  for (i in 1:length(gr_config)) {
    first_row <- unlist(gr_config)[i]

    # Account for last line in gr_config
    if (i < length(gr_config)) {
      last_row <- unlist(gr_config)[i+1]
    } else {
      last_row <- n_rows
    }

    kable <- kable %>%
      group_rows(names(gr_config)[i],
                 first_row,
                 last_row)
  }
  # Issue: creates weird indenting
  return(kable)

}

stack_stderrs_under_betas <- function(outcomes_table) {

  stderrs <- outcomes_table %>%
    select(outcome_order, matches("stderr")) %>%
    mutate(across(.cols = matches("stderr"),
                  .fns = ~ case_when(.x == '' ~ '',
                                     TRUE ~ paste0("(", .x, ")"))),
           beta_se_order = 2) %>%
    rename_with(.fn = ~ gsub("stderr", "beta", .x))

  beta_se_stack <- outcomes_table %>%
    select(-matches("stderr")) %>%
    mutate(across(contains("beta"), ~as.character(.x)),
           beta_se_order = 1) %>%
    bind_rows(stderrs) %>%
    arrange(as.numeric(outcome_order), beta_se_order)

  return(beta_se_stack)

}


fmt <- function(x, decimals = 3, stars = FALSE, parens = FALSE) {
  # Example: df %>% mutate(across(matches("beta_"), ~ fmt(.x)))

  fmted_x <- sprintf(glue("%.{decimals}f"), round(x, decimals))

  fmted_x[as.numeric(fmted_x) == 0] <- '0'

  fmted_x <- paste0(fmted_x, get_stars(x, stars))

  if (parens) {
    fmted_x <- paste0("(", fmted_x, ")")
  }

  return(fmted_x)
}


fmt_col <- function(df, var, decimals = 3, stars = FALSE, parens = FALSE) {
  # Example: df %>% fmt_col("beta_itt")

  fmted_df <- df %>%
    mutate(var_col = round(get(var), decimals),
           var_col = sprintf(glue("%.{decimals}f"), var_col),
           var_col = case_when(as.numeric(var_col) == 0 ~ "0",
                               TRUE ~ var_col),
           !!var := paste0(var_col, get_stars(get(var), stars))) %>%
    select(-var_col)

  if (parens) {
    fmted_df <- fmted_df %>%
      mutate(!!var := paste0("(", get(var), ")"))
  }

  return(fmted_df)
}

