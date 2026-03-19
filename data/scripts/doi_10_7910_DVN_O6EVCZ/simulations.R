## set working directory to folder containing downloaded replication repository
## UNCOMMENT the line below and insert path for your working directory
# setwd("YOUR WORKING DIRECTORY HERE") 

## source base script for functions, paths, etc.
source("base.R")

## load packages
library(furrr)
library(spdep)
library(spatialreg)
library(tidyverse)

## set options for parallel processing using "furrr" package
future::plan(multicore)
options(future.globals.maxSize = Inf, future.rng.onMisuse = "ignore")

## determine whether each unit has low/medium/high pop of each racial/ethnic variable
## within-city levels; low/medium/high = 10/50/90 percentile +/- 5 
df2 <- df %>%
        dplyr::select(geo, city, pr_l_est, pr_a_est, pr_b_est, pr_w_est) %>%
        pivot_longer(-c("geo", "city")) %>%
        group_by(city, name) %>%
        mutate(ptile = percent_rank(value),
               cat = factor(case_when(ptile >= 0.05 & ptile < 0.15 ~ "low",
                                      ptile >= 0.45 & ptile < 0.55 ~ "medium",
                                      ptile >= 0.85 & ptile < 0.95 ~ "high",
                                      TRUE ~ NA_character_),
                            levels = c("low", "medium", "high"))) %>%
        ungroup() %>%
        pivot_wider(id_cols = c("geo", "city"), values_from = "cat", names_prefix = "cat_")

## determine plausible low/medium/high SES variables for each racial/ethnic level by city
## Table e4.4 in supplement comes from these data
df3 <- df %>%
        dplyr::select(all_of(c("geo", "n_15o_est", scratch$x))) %>%
        pivot_longer(starts_with("pr")) %>%
        left_join(df2, by = c("geo", "city")) %>%
        pivot_longer(starts_with("cat"), names_to = "cat_name", values_to = "cat_value") %>%
        group_by(city, name, cat_name, cat_value) %>%
        # pop-weighted average, within racial/ethnic category
        summarize(avg = weighted.mean(value, n_15o_est),
                  # low/medium/high values, within racial/ethnic category
                  lmh = list(quantile(value, c(0.1, 0.5, 0.9)))) %>%
        ungroup() %>%
        drop_na(cat_value) %>%
        rowwise() %>%
        # for 4 SES variables, take low/medium/high values;
        # for other adjustment variables, take pop-weighted average
        mutate(value = if_else(name %in% c("pr_pov_est", "pr_hig_mtp_est", 
                                           "pr_hig_ui_est", "pr_ni_est"), 
                               list(lmh), list(avg))) %>%
        ungroup() %>%
        pivot_wider(id_cols = city:cat_value) %>%
        mutate(re_group = case_when(cat_name == "cat_pr_a_est" ~ "Asian",
                                    cat_name == "cat_pr_b_est" ~ "Black",
                                    cat_name == "cat_pr_l_est" ~ "Hispanic",
                                    cat_name == "cat_pr_w_est" ~ "White")) %>%
        rename(re_level = cat_value) %>%
        pivot_longer(c("pr_pov_est", "pr_hig_mtp_est", "pr_hig_ui_est", "pr_ni_est"),
                     names_to = "ses_var") %>%
        unnest(value) %>%
        group_by(city, re_group, re_level, ses_var) %>%
        mutate(ses_rank = rank(value),
               ses_within = factor(case_when(ses_rank == 1 ~ "high",
                                             ses_rank == 2 ~ "medium",
                                             ses_rank == 3 ~ "low"),
                                   levels = c("low", "medium", "high")),
               across(where(is.list), as.numeric)) %>%
        ungroup() %>%
        pivot_wider(names_from = "ses_var") %>% 
        arrange(re_group, re_level, ses_within, city) %>%
        dplyr::select(all_of(c("re_group", "re_level", "ses_within", scratch$x))) 

## load (large) matrix containing the spatial weights matrices for the bootstrapped resamples
W_boot <- as.matrix(read_delim(scratch$W_boot, delim = ",", col_names = FALSE, na = "",
                               col_types = paste(c("i", rep("c", 553)), collapse = "")))

## determine the observations included in each resample 
## unique IDs are contained in first two columns
ids <- W_boot[, 1:2] %>%
        as.data.frame() %>%
        drop_na() %>%
        set_names(c("boot_rep", "geo")) %>%
        split(.$boot_rep) %>%
        set_names(NULL)

n_sim <- length(ids) # number of simulations
n_obs <- nrow(df) # number of observations per simulation

## split large matrix into 1,000 individual matrices, one for each iteration of simulation
W_split <- future_map(1:n_sim, ~ split_W_boot(., nr = n_obs, m = W_boot))

rm(W_boot)
gc()

## first, estimate models on each resampled data set 
## then, predict fitted models as if each unit had its values from df3
## finally, get CIs on resample means 
ci <- future_map2_dfr(W_split, ids, ~ boot_sem(.x, .y, df, df3), .id = "boot_rep") %>%
        pivot_longer(starts_with("yhat"), names_prefix = "yhat_", names_to = "y") %>%
        group_by(re_group, re_level, ses_within, y) %>%
        summarize(avg = mean(value),
                  q_hi = quantile(value, 0.975),
                  q_lo = quantile(value, 0.025),
                  .groups = "keep") %>%
        ungroup() %>%
        # pivot CIs
        mutate(ci_lo = 2 * avg - q_hi,
               ci_hi = 2 * avg - q_lo) %>%
        dplyr::select(-starts_with("q_"))

## calculate estimates using actual analysis data
out <- boot_sem(W, dplyr::select(df, geo), df, df3) %>%
        pivot_longer(starts_with("yhat"), names_prefix = "yhat_", 
                     names_to = "y", values_to = "yhat") %>%
        ## merge in CIs
        left_join(ci) %>%
        mutate(y = case_when(y == "mar" ~ "March",
                             y == "apr" ~ "April",
                             y == "dif" ~ "Difference")) %>%
        dplyr::select(re_group:yhat, ci_lo, ci_hi) 