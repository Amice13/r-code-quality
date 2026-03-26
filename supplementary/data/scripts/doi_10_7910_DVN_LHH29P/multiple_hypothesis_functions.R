

# TODO make sure I'm doing this per time period for outcomes

wyoung <- function(df, outcomes, windows, covariate_sets, subgroup_types, subgroup_values,
                   multiple_test_col, key_cols) {

  # 0. Filter to correct set of results
  df <- df %>%
    filter.(outcome %in% outcomes,
            window %in% windows,
            covariate_set %in% covariate_sets,
            subgroup_type %in% subgroup_types,
            subgroup_value %in% subgroup_values)

  expect_equal(max(length(unique(df$outcome)), length(unique(df$subgroup_value))),
                 nrow(df %>% select(outcome, subgroup_value) %>% unique()))

  fwer_list = df %>% pull(get(multiple_test_col)) %>% unique()

  # 1. Rank observed p's (across multiple tests e.g. outcome or subgroup)
  obs_ps <- df %>%
    filter.(sampling_iter == 0) %>%
    arrange.(pval_itt) %>%
    mutate.(p_rank_obs = as.numeric(row_number()),
            p_obs = pval_itt) %>%
    select(all_of(c(multiple_test_col, 'p_rank_obs', 'p_obs')))

  # 2. Rank resampled p's (within sampling round, across multiple tests e.g. outcome or subgroup)
  resampled_ps <- df %>%
    filter.(sampling_iter != 0) %>%
    arrange.(sampling_iter, pval_itt) %>%
    mutate.(p_rank = as.numeric(row_number()), .by = sampling_iter) %>%
    rename.(p_star = pval_itt)

  # 3. Merge resampled and observed p's on dimension of multiple test e.g. outcome or subgroup
  combo_ps <- left_join.(
    resampled_ps,
    obs_ps,
    by = multiple_test_col) %>%
    arrange.(sampling_iter, p_rank_obs) %>%
    mutate.(p_2star = NA_real_)

  # 4. Enforce monotonicity by observed outcome rank
  for (i in 1:length(fwer_list)) {

    combo_ps <- combo_ps %>%
      mutate.(group = as.numeric(p_rank_obs >= i)) %>%
      mutate.(p_fill = min(p_star), .by = c(sampling_iter, group), .groups = 'drop') %>%
      mutate.(p_2star = case_when(p_rank_obs == i ~ p_fill,
                                  TRUE ~ p_2star)) %>%
      select.(-group, -p_fill)
  }

  # 5. Compute share of cases where p** < p_obs
  fwer_ps <- combo_ps %>%
    mutate.(p_fwer = as.numeric(p_2star < p_obs)) %>%
    summarize.(p_fwer = mean(p_fwer),
               .by = all_of(c(key_cols, 'p_rank_obs', 'p_obs')),
               .groups = 'drop') %>%
    arrange.(p_rank_obs) %>%
    mutate.(p_fwer_star = case_when(p_rank_obs == 1 ~ p_fwer))

  # 6. Enforce monotonicity one more time
  for (i in 2:length(fwer_list)) {

    fwer_ps <- fwer_ps %>%
      mutate.(group = as.numeric(p_rank_obs <= i),
              p_fill = case_when(p_rank_obs < i ~ p_fwer_star,
                                 TRUE ~ p_fwer)) %>%
      mutate.(p_fwer_star = case_when(p_rank_obs == i ~ max(p_fill),
                                      TRUE ~ p_fwer_star),
              .by = group) %>%
      select.(-group, -p_fill)
  }

  # 7. Add fdr-q's
  final_ps <- bind_cols.(fwer_ps, tidytable(fdrq = p.adjust(fwer_ps$p_obs, method = "BH")))

  return(final_ps)
}
