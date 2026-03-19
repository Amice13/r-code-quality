library(gt)
library(gtsummary)
library(here)
library(tidyverse)

# RDS files created by random_effects_models.R

table <- c(read_rds(here::here("data/pooled_mods_log_slope.rds")), 
           read_rds(here::here("data/y2018_mods_log_slope.rds")) ,
           read_rds(here::here("data/y2019_mods_log_slope.rds")),
           read_rds(here::here("data/y2020_mods_log_slope.rds")),
           read_rds(here::here("data/y2021_mods_log_slope.rds"))) %>% 
  map(tbl_regression, exponentiate = TRUE,
      label = list(
        PARCEL_sqft_std = "Parcel sq. ft. (std)",
        PARCEL_buildings = "Structures on parcel",
        PARCEL_STEEPSLOPE = "Parcel contains steep slope",
        TRT_rent_i_std = "Tract median gross rent (std)",
        TRT_rent_i_qtile = "Median rent quintile",
        TRT_prop_vacant_std = "Tract proportion vacant (std)",
        PL_ACS16_5yr_totpop_log_std = "Log city population (std)",
        PL_ACS16_5yr_pctoo_std = "City owner-occupied residences (std)",
        PL_hoa_cp_std = "City HOA intensity (std)"
      )) %>% 
  map(~ add_significance_stars(.x,
                               pattern = "{estimate}{stars}",
                               thresholds = c(0.001, 0.01, 0.05),
                               hide_ci = TRUE,
                               hide_p = TRUE,
                               hide_se = FALSE)) %>% 
  map(~ add_glance_table(.x, include = nobs)) %>% 
  tbl_merge(tab_spanner = FALSE) %>% 
  modify_table_body(
    ~.x %>% 
      arrange(desc(variable == "PL_hoa_cp_std")) %>% 
      arrange(desc(variable == "PL_ACS16_5yr_pctoo_std")) %>% 
      arrange(desc(variable == "PL_ACS16_5yr_totpop_log_std")) %>% 
      arrange(desc(variable == "TRT_prop_vacant_std")) %>% 
      arrange(desc(variable == "TRT_rent_i_qtile")) %>% 
      arrange(desc(variable == "TRT_rent_i_std")) %>% 
      arrange(desc(variable == "PARCEL_STEEPSLOPE")) %>%
      arrange(desc(variable == "PARCEL_buildings")) %>% 
      arrange(desc(variable == "PARCEL_sqft_std"))) %>% 
  modify_table_body(~.x %>% dplyr::arrange(row_type == "glance_statistic")) %>% 
  as_gt() %>% 
  gt::tab_spanner(
    columns = c(estimate_1, std.error_1, estimate_3  , std.error_3, 
                estimate_5, std.error_5, estimate_7 , std.error_7,
                estimate_9 , std.error_9), 
    label = "(1)",
    gather = FALSE) %>% 
  gt::tab_spanner(
    columns = c(estimate_2, std.error_2, estimate_4  , std.error_4, 
                estimate_6, std.error_6, estimate_8 , std.error_8,
                estimate_10, std.error_10), 
    label = "(2)",
    gather = FALSE) %>% 
  gt::tab_spanner(
    columns = c(estimate_1, std.error_1, estimate_2, std.error_2),
    label = "Pooled",
    level = 2,
    gather = FALSE) %>% 
  gt::tab_spanner(
    columns = c(estimate_3, std.error_3, estimate_4, std.error_4),
    label = "2018",
    level = 2,
    gather = FALSE) %>% 
  gt::tab_spanner(
    columns = c(estimate_5, std.error_5, estimate_6, std.error_6),
    label = "2019",
    level = 2,
    gather = FALSE) %>% 
  gt::tab_spanner(
    columns = c(estimate_7, std.error_7, estimate_8, std.error_8),
    label = "2020",
    level = 2,
    gather = FALSE) %>% 
  gt::tab_spanner(
    columns = c(estimate_9, std.error_9, estimate_10, std.error_10),
    label = "2021",
    level = 2,
    gather = FALSE)

table
