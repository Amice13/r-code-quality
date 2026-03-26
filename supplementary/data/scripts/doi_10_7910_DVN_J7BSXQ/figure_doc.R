# load packages
library(tidyverse)
library(glmnet)
library(ranger)
library(gbm)
library(Matrix)
library(ggrepel)

# install multical package from github and load
devtools::install_github("ebenmichael/multical", force = TRUE)
library(multical)

# create results directory if it doesn't already exist
if(!dir.exists("../results")) {
  dir.create("../results")
}

set.seed(1011)
# read in pew and cces data, pull out analysis covariates and outcomes
# and combine into one data set

pew2016_full <- read_rds("../data/generated/pew.rds")

cces2016_full <- read_rds("../data/generated/cces.rds") %>%
  mutate(recode_relig = replace_na(recode_relig, "Something else"))


keep_cols <- c("age_bucket", "female", "race", 
              "region", "pid_3way", "educ", "inputstate",
              "relig", "born", "income", "income_5way", "educ_3way",
              "income_3way", "age_3way")

elect2016 <- bind_rows(pew2016_full %>% mutate(cces = 0, commonweight_vv_post = 0),
                    cces2016_full %>% mutate(cces = 1)) %>%
          mutate(rvote_2016 = recode_vote_2016 == "Republican",
                 dvote_2016 = recode_vote_2016 == "Democrat") %>%
          mutate(across(paste0("recode_", keep_cols), as.factor)) %>%
          select(rvote_2016, dvote_2016, cces, paste0("recode_", keep_cols), commonweight_vv_post) %>%
          rename_with(~str_replace_all(., "recode_", "")) %>%
          na.omit() %>%
          mutate(across(where(is.factor), droplevels))

# names of covariates to include in multilevel weighting and regression models
bal_cols <- c("educ", "income", "race", "female", "age_bucket",
              "pid_3way", "born", "region")

# names of covariates for labeling in figures
nice_names <-  c("age_bucket" = "Age", "female" = "Female",
                 "race" = "Race", "region" = "Region",
                 "pid_3way" = "Party ID", "educ" = "Education",
                 "relig" = "Religion", "born" = "Born-again",
                 "income" = "Income"
                 )
# load helper functions
source("helper_funcs.R")



# FIGURE 1
# Compute the population-weighted number of empty cells

sapply(1:length(bal_cols),
  function(i) {
    elect2016 %>%
      group_by(across(bal_cols[1:i])) %>%
      summarise(n_pew = sum(1 - cces), n_cces = sum(commonweight_vv_post)) %>%
      ungroup() %>%
      summarise(pct_0 = sum(n_cces[n_pew == 0]) / sum(n_cces)
                # pct_0 = mean(n_pew == 0)
                ) %>%
      pull(pct_0)
  }) -> pct_0

data.frame(pct_0 = pct_0,
           order = 1:length(bal_cols)
           ) %>%
  ggplot(aes(x = order, y = 1 - pct_0)) +
  geom_line() +
  scale_x_continuous("", 
                     labels = ifelse(nice_names[bal_cols] != "Education", 
                      paste("+", nice_names[bal_cols]), "Education"),
                     breaks = 1:length(bal_cols)) +
  scale_y_continuous("% of population represented", labels = scales::percent,
                     limits = c(0, 1)) +
  theme_bw(12) -> p

ggsave("figure_1.png", p, "png", "../results", width = 7, height = 3, limitsize=F)




# create multilevel calibration weighting formula to balance included covariates
form <- as.formula(paste("1 - cces ~", paste(bal_cols, collapse = "+")))

# fit weights for 40 different values of lambda
lam_results <- multical(form, commonweight_vv_post,
            elect2016, order = 6, verbose = TRUE, lambda = NULL,
            n_lambda = 40,
            eps_abs = 1e-12, eps_rel = 1e-12)

# 6-way interactions design matrix to compute imbalance below
D6 <- multical:::create_design_matrix(
        lam_results %>% filter(lambda == min(lambda)) %>%
                             select(-weight, -sample_count, -target_count, -lambda),
        order = 6)

lambdas <- unique(lam_results$lambda)




# FIGURE 2

# function to compute balance and effective sample size
compute_balance_neff <- function(lam) {
  res <- lam_results %>% filter(lambda == !!lam)
  N <- sum(res$target_count)

  imbal_vec <- res$target_count - res$weight * res$sample_count
  imbal <- sqrt(as.numeric(imbal_vec %*% D6 %*% t(D6) %*% imbal_vec) / N^2)

  neff <- sum(res$weight * res$sample_count) ^ 2 /
    sum(res$weight ^ 2 * res$sample_count)

  return(c(imbal, neff))
}

# compute balance and effective sample size as a function of lambda
cbind(lambdas, t(sapply(lambdas, compute_balance_neff))) %>%
  data.frame() %>%
  `names<-`(c("lambda", "imbal", "neff")) -> plotdf

# choose the value of lambda that gives 95% imbalance reduction
plotdf %>%
  filter(imbal <= .05 * (max(imbal) - min(imbal)) + min(imbal)) %>%
  summarise(lambda = max(lambda)) %>% pull(lambda) -> which_lam

plotdf %>%
  mutate(label = ifelse(lambda == which_lam,
                        paste("lambda == ", round(which_lam, 1)), NA)) %>%
  mutate(imbal = (imbal - min(imbal)) / (max(imbal) - min(imbal))) %>%
  ggplot(aes(x = neff, y = 1 - imbal)) +
  geom_path() +
  geom_label_repel(aes(label = label), parse = T, nudge_y = -.05) +
  geom_point(data = . %>% filter(lambda == which_lam)) +
  geom_hline(yintercept = .95, lty = 2) +
  scale_y_continuous("% Potential balance improvement",
                     limits = c(0, 1),
                     labels = scales::percent) +
  xlab("Effective sample size") +
  theme_bw() -> p
ggsave("figure_2.png", p, "png", "../results", width = 4, height = 3, limitsize=F)




# this snippet times the weighting procedure
# uncomment to time

# timings1 <- bench::mark(multical(form, commonweight_vv_post,
#             elect2016, order = 6, verbose = TRUE, lambda = NULL,
#             n_lambda = 40,
#             eps_abs = 1e-12, eps_rel = 1e-12), iterations = 1)


# filter down to selected lambda results
bal_results <- lam_results %>% filter(lambda == which_lam) %>% select(-lambda)

# raking results
rake_results <- multical(form, commonweight_vv_post,
                          elect2016, order = 1, verbose = TRUE)




# combine weights with post-stratification weights (where 1 / 0 = 0)
full_res <- inner_join(bal_results,
                       rake_results %>% rename(rake_weight = weight)) %>%
            mutate(ps_weight = ifelse(sample_count != 0,
                                      target_count / sample_count, 0))



# fit outcome models

# gradient boosted trees with cross validation
gb <- gbm(rvote_2016 ~ ., distribution = "bernoulli",
    data = elect2016 %>% filter(cces == 0) %>% select(bal_cols, rvote_2016),
    n.trees = 6000, cv.folds = 10, shrinkage = 0.001, interaction.depth = 8)
best_iter <- gbm.perf(gb, method = "cv")

# ridge regression with 4-way interactions
X4 <- Matrix::sparse.model.matrix(~ .^4 - 1,
                  elect2016 %>% filter(cces == 0) %>% select(bal_cols))
X4_full <- Matrix::sparse.model.matrix(~ .^4 - 1, full_res %>% select(bal_cols))

# fit ridge w/cross validation
ridge4 <- cv.glmnet(X4, elect2016 %>% filter(cces == 0) %>% pull(rvote_2016), alpha = 1, trace.it = 1)

ridge_pred <- c(predict(ridge4, X4_full, type = "response", s = "lambda.1se"))
gb_pred <- predict(gb, full_res, n.trees = best_iter, type = "response")

# add outcome regression predictions
full_res %>%
  mutate(ridge_pred = ridge_pred, gb_pred = gb_pred) -> full_res







# collapse cells to create feasible post-stratification weights

cces2016_full  %>%
  mutate(college = ifelse(recode_educ_3way == "No College", 0, 1),
         white = ifelse(recode_race == "White", 1, 0)) %>%
  select(recode_income, recode_income_3way,
         recode_age_bucket, recode_age_3way,
         recode_educ, college,
         recode_race, white
         ) %>%
  rename_with(~str_replace_all(., "recode_", "")) %>%
  distinct() -> collapsed_buckets


elect2016 %>%
  mutate(college = ifelse(educ_3way == "No College", 0, 1),
         white = ifelse(race == "White", 1, 0)) %>%
  group_by(income_3way, age_3way, college,
           white) %>%
  summarise(sample_count = sum(1 - cces),
            target_count = sum(commonweight_vv_post)) %>%
  ungroup() -> collapsed_cells


# compute post-stratification weights with collapsed cells and merge into results
right_join(collapsed_cells, collapsed_buckets) %>%
  rename(sample_count_collapsed = sample_count,
         target_count_collapsed = target_count) %>%
  select(any_of(bal_cols), sample_count_collapsed, target_count_collapsed) %>%
  left_join(full_res, .,
            suffix = c("", "_collapsed")) %>%
  mutate(ps_weight_collapsed = ifelse(sample_count_collapsed != 0 & !is.na(sample_count_collapsed),
                                      target_count_collapsed /
                                        sample_count_collapsed,
                                      0)) -> full_res
  





elect2016 %>%
  left_join(full_res, by = bal_cols) %>%
  summarise(
            # multilevel calibration weighting estimate
            adj_est = sum((weight * rvote_2016)[cces == 0]) / sum(weight[cces == 0]),
            adj_se2 = sum((weight * (rvote_2016 - adj_est))[cces == 0]^2) /
                        sum(weight[cces == 0])^2,
            # raking estimate
            rake_est = sum((rake_weight * rvote_2016)[cces == 0]) /
                          sum(rake_weight[cces == 0]),
            rake_se2 = sum((rake_weight * (rvote_2016 - rake_est))[cces == 0]^2) /
                        sum(rake_weight[cces == 0])^2,
            # collapsed post-stratification weights estimate
            ps_est = sum((ps_weight_collapsed * rvote_2016)[cces == 0]) /
                          sum(ps_weight_collapsed[cces == 0]),
            ps_se2 = sum((ps_weight_collapsed * (rvote_2016 - ps_est))[cces == 0]^2) /
                        sum(ps_weight_collapsed[cces == 0])^2,
            # DRP estimates
            mrp_ridge = sum((ridge_pred * commonweight_vv_post)[cces == 1]) /
                           sum(commonweight_vv_post[cces == 1]),
            mrp_gb = sum((gb_pred * commonweight_vv_post)[cces == 1]) /
                           sum(commonweight_vv_post[cces == 1]),
            # DRP w/ ridge regression estimate
            drp_est = mrp_ridge +
              sum((weight * (rvote_2016 - ridge_pred))[cces == 0]) /
                sum(weight[cces == 0]),
            drp_se2 = sum((weight * (rvote_2016 - ridge_pred))[cces == 0]^2) /
                        sum(weight[cces == 0])^2,
            # DRP with gradient boosted trees estimate
            drp_gb_est = mrp_gb +
              sum((weight * (rvote_2016 - gb_pred))[cces == 0]) /
                sum(weight[cces == 0]),
            drp_gb_se2 = sum((weight * (rvote_2016 - gb_pred))[cces == 0]^2) /
                        sum(weight[cces == 0])^2,
            # ground truth CCES estimate
            truth_est = sum((rvote_2016 * commonweight_vv_post)[cces == 1]) /
                      sum(commonweight_vv_post[cces == 1]),
            truth_se2 = sum((commonweight_vv_post * (rvote_2016 - truth_est))[cces == 1]^2) / sum(commonweight_vv_post[cces == 1]) ^ 2,
  ) -> res

# clean up variable names for plotting
res %>% select(-mrp_ridge, -mrp_gb) %>% pivot_longer(everything()) %>%
  mutate(se2 = ifelse(str_detect(name, "_se"), "se2", "est"),
         method = case_when(
            str_detect(name, "rake") ~ "Raking on margins",
            str_detect(name, "ps") ~ "Post-Stratification (collapsed cells)",
            str_detect(name, "adj") ~ "Multilevel Calibration",
            str_detect(name, "drp_gb") ~ "DRP: Gradient Boosting",
            str_detect(name, "drp_") ~ "DRP: Ridge",
            str_detect(name, "truth") ~ "Weighted CCES"),
        name = str_replace_all(name, "_est", ""),
        name = str_replace_all(name, "_se2", ""),
      ) %>%
  pivot_wider(names_from = se2, values_from = value) %>%
  (function(x) left_join(x %>% mutate(one=1),
                         x %>% mutate(one = 1) %>%
                            filter(method == "Weighted CCES") %>%
                            select(one, est, se2),
                         by = "one",
                         suffix = c("", "_true"))) %>%
  filter(method != "Weighted CCES") -> plotdf


# FIGURE 4b
# plot estimates of R vote share using different methods

plotdf %>%
  ggplot(aes(x = method, y = est)) +
  geom_pointrange(aes(ymin = est - 2 * sqrt(se2), ymax = est + 2 * sqrt(se2))) +
  geom_hline(aes(yintercept = est_true), lty = 2) +
  geom_hline(aes(yintercept = est_true - 2 * sqrt(se2_true)), lty = 3) +
  geom_hline(aes(yintercept = est_true + 2 * sqrt(se2_true)), lty = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Estimated Republican Vote Share") +
  xlab("") +
  theme_bw(12) -> p
ggsave("figure_4b.png", p, "png", "../results", width = 5, height = 4, limitsize=F)




# APPENDIX FIGURE A1

# helper function to get factor values from model matrix
extract_factor_vals <- function(x) {
  sapply(
    lapply(str_split(x, ":"), 
    function(y) sapply(str_split(y, "--"), `[`, 2)),
    paste, collapse = "\n")
}


# get 3-way cells
cells <- elect2016 %>%
  mutate(across(where(is.factor), ~ as.factor(paste0("--", .)))) %>%
  mutate(female = fct_relevel(female, rev)) %>%
  group_by(age_bucket, female, pid_3way) %>%
  summarise(sample_count = sum(1-cces),
            target_count = sum(commonweight_vv_post)) %>%
  ungroup()

# create model matrix and plot
D <- model.matrix(~ . ^ 3 - 1,
    data = cells %>% select(-contains("count")))

Ddat <- as.data.frame(D) %>%
  `colnames<-`(extract_factor_vals(colnames(.))) %>%
  rownames_to_column()

Ddat %>%
  pivot_longer(-rowname, names_to = "cov", values_to = "bin") %>%
  mutate(order = str_count(cov, "\n") + 1) %>%
  mutate(cov = str_replace(cov, " to ", "-")) %>%
  filter(order <= 3) %>%
  mutate(
         order = case_when(order == 1 ~ "D1: Margins",
                           order == 2 ~ "D2: 2nd order interactions",
                           order == 3 ~ "D3: 3rd order interactions"),
         cov = fct_relevel(cov, "Female")) %>% 
  ggplot(aes(y = fct_rev(rowname), x = cov, fill = as.factor(bin))) +
  geom_tile(color = "white", size = 2) +
  facet_grid(~ order, scales = "free_x", space = "free_x") + #, switch = "both") +
  ylab("") +
  xlab("") +
  scale_x_discrete(position = "bottom") +
  scale_fill_grey(end = 0, start = 1) +
  guides(fill = F) +
  theme_bw(12) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank()
        )-> p
  
ggsave("figure_a1.png", p, "png", "../results", width = 12, height = 5, limitsize=F)
  




# FIGURE 3

cells <- full_res %>%
  # select(-sample_count, -target_count, -weight, -rake_weight, -ps_weight) %>%
  select(!!sort(bal_cols)) %>%
  mutate(across(, ~ as.factor(paste0("--", .))))
total_count <- sum(full_res$target_count)


# full names of methods for plotting
weight_names <- c("ps_weight" = "Post-Stratification",
                  "ps_weight_collapsed" = "Post-Stratification (collapsed cells)",
                  "rake_weight" = "Raking on margins",
                  "weight" = "Multilevel Calibration")


extract_covnames <- function(x, name_vec) {
  sapply(
    lapply(str_split(x, ":"), 
    function(y) name_vec[sapply(str_split(y, "--"), `[`, 1)]),
    paste, collapse = ":")
}


# 4-way design matrix to compute imbalance
D <- sparse.model.matrix(~ . ^4 - 1 , 
    data = cells,
    contrasts = lapply(cells, contrasts, contrasts = F))

# compute imbalances for 1-4 way interactions and plot
full_res %>% 
  mutate(across(c(weight, rake_weight, ps_weight, ps_weight_collapsed), ~ . * sample_count)) %>%
  select(rake_weight, weight, ps_weight_collapsed, target_count) %>%
  map_df( ~ as.numeric(t(D) %*% . )) %>%
  mutate(cov = colnames(D)) %>%
  pivot_longer(-c(target_count, cov), values_to = "weight_count") %>%
  mutate(diff = abs(weight_count - target_count) / target_count) %>%
  filter(target_count > 10) %>%
  mutate(name = weight_names[name],
         order = str_count(cov, ":") + 1,
         order = case_when(order == 1 ~ "1st order",
                           order == 2 ~ "2nd order",
                           order == 3 ~ "3rd order",
                           order >= 4 ~ paste0(order, "th order"))
         ) %>%
  ggplot(aes(x = diff, y = name, color = name)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_boxplot() +
  ylab("") +
  xlab("Ratio of absolute difference and target count") +
  facet_wrap(~ order)  +
  scale_x_continuous(
                     limits = c(-0.05, 2), expand = c(-0.01, 0.01)) +
  scale_color_brewer("", type = "qual", palette = "Dark2") +
  guides(size = F, shape = F) +
  theme_bw(18) + 
  theme(legend.position =  "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
         panel.grid.major.y = element_blank(),
        panel.spacing = unit(0, "lines")) -> p

ggsave("figure_3.png", p, "png", "../results", width = 10, height = 6, limitsize=F)



# FIGURE 4a

# compute effective sample sizes
elect2016 %>%
  left_join(full_res, by = bal_cols) %>%
  filter(cces == 0) %>%
  mutate(one = 1) %>%
  summarise(across(c(weight, rake_weight, ps_weight, ps_weight_collapsed, one),
                   ~ sum(.) ^ 2 / sum(. ^ 2))) %>%
  mutate(across(, ~ one / .))

elect2016 %>%
  left_join(full_res, by = bal_cols) %>%
  filter(cces == 0) %>%
  select(weight, rake_weight, ps_weight_collapsed) %>%
  pivot_longer(everything(),names_to = "method", values_to = "weight") %>%
  mutate(method = weight_names[method]) %>%
  ggplot(aes(x = weight)) + 
  stat_ecdf(aes(color = method), size = 1) +
  geom_vline(xintercept = sum(elect2016$commonweight_vv_post) /
                            sum(1 - elect2016$cces), lty = 2) +
  scale_fill_brewer("", type = "qual", palette = "Dark2") +
  scale_color_brewer("", type = "qual", palette = "Dark2") +
  scale_x_continuous(expand = c(0, 0)) +
  guides(color = guide_legend(nrow = 2)) +
  xlab("Weight") +
  ylab("") + 
  theme_bw(12) +
  theme(legend.position = "bottom") -> p
ggsave("figure_4a.png", p, "png", "../results", width = 5, height = 4, limitsize=F)


# iterate through the states as targets to impute R vote share
# do not include region

states <- sort(unique(elect2016$inputstate))
bal_cols_noreg <- bal_cols[bal_cols != "region"]


# create target state X interaction order pairs and iterate
param_list_states_noreg <- cross(list(state = states,
                    covs = list(paste(bal_cols_noreg,
                              collapse = "+")),
                    order = c(1, 2, 3, 4, 5, 6)))

param_list_states_noreg %>%
  lapply(function(x) do.call(fit_pew_to_state, x)) -> cal_pew_to_state_noreg

# fit gradient boosted regression trees to R vote in PEW sample then
# get predictions for CCES sample in each target state
gb_pred_r_noreg <- fit_gb(bal_cols_noreg, "rvote_2016", shrinkage = 0.001,
                    n.trees = 5000, interaction.depth = 7)
pred_gbm_r_noreg <- lapply(states, get_preds, pred_func = gb_pred_r_noreg)

# combine weights with predictions
map2(cal_pew_to_state_noreg, rep(pred_gbm_r_noreg, 6),
     function(x, y) bind_cols(x, y %>% select(pred))) -> drp_gbm_r_noreg

# iterate through the states as targets to impute R vote share
# include regeion

states <- sort(unique(elect2016$inputstate))

# create target state X interaction order pairs and iterate
param_list_states <- cross(list(state = states,
                    covs = list(paste(bal_cols,
                              collapse = "+")),
                    order = c(1, 2, 3, 4, 5, 6)))


param_list_states %>%
  lapply(function(x) do.call(fit_pew_to_state, x)) -> cal_pew_to_state

# fit gradient boosted regression trees to R vote in PEW sample then
# get predictions for CCES sample in each target state
gb_pred_r <- fit_gb(bal_cols, "rvote_2016", shrinkage = 0.001, n.trees = 5000, interaction.depth = 8)

pred_gbm_r <- lapply(states, get_preds, pred_func = gb_pred_r)

# combine weights with predictions
map2(cal_pew_to_state, rep(pred_gbm_r, 6),
     function(x, y) bind_cols(x, y %>% select(pred))) -> drp_gbm_r





# FIGURE 5
bind_rows(
  get_metrics_2016(drp_gbm_r, param_list_states, "rvote_2016") %>%
    mutate(restricted = "Restricted within region"),
  get_metrics_2016(drp_gbm_r_noreg, param_list_states_noreg, "rvote_2016") %>%
    mutate(restricted = "Unrestricted by region")
 ) %>%
  ungroup() %>%
  (function(x) {
    inner_join(x, 
    x %>% filter(adj == "adj", order == 1) %>% select(-adj),
    by = c("covs", "restricted"), suffix = c("", "_rake"))
  }) %>%
  filter(adj != "unadj") %>%
  mutate(lty = as.factor(ifelse(adj == "mrp", 1, 0))) %>%
  mutate(adj = case_when(adj == "mrp" ~ "Gradient Boosted Trees",
                         adj == "adj" ~ "Multilevel Calibration",
                         adj == "drp" ~ "DRP",
                         adj == "unadj" ~ "Unadjusted"),
         adj = fct_relevel(adj, "Unadjusted", "Multilevel Calibration",
                           "Gradient Boosted Trees", "DRP")) %>%
  mutate(improve_rmse = (rmse / rmse_rake),
         improve_mad = (mad / mad_rake),
         improve_abs_bias = abs(bias) / abs(bias_rake),
         abs_bias = abs(bias)) %>%
  select(restricted, adj, order, lty, contains("improve"), rmse, mad, abs_bias, coverage, corr) %>%
  pivot_longer(-c(adj, order, lty, restricted),
               names_to = "measure", values_to = "value") %>%
  mutate(improve = ifelse(str_detect(measure, "improve"),
                          "% of rake", "Raw"),
         measure = str_replace(measure, "improve_", ""),
         measure = case_when(measure == "abs_bias" ~ "Absolute bias",
                             measure == "mad" ~ "MAD",
                             measure == "rmse" ~ "RMSE",
                             measure == "corr" ~ "Correlation",
                             measure == "coverage" ~ "Coverage")) %>%
  filter(improve == "Raw", measure %in% c("Absolute bias", "RMSE")) %>%
  ggplot(aes(x = order, y = value, color = adj)) + 
  geom_line(aes(lty = lty), size = 1.5) +
  scale_color_brewer("", type = "qual", palette = "Set1", direction = -1) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(measure ~ restricted, scales = "free_y", switch = "y") +
  xlab("Order of interactions") +
  ylab("") +
  guides(lty = F) +
  theme_bw(18) + 
  theme(legend.position = "bottom") -> p

ggsave("figure_5.png", p, "png", "../results", width = 10, height = 6, limitsize=F)


# APPENDIX FIGURE A2
reg_state <- elect2016 %>% distinct(inputstate, region)

bind_rows(
  lapply(drp_gbm_r, impute_outcome, outcome = "rvote_2016") %>%
    bind_rows()  %>%
    bind_cols(bind_rows(param_list_states)) %>%
    mutate(restricted = "Restricted within region"),
  lapply(drp_gbm_r_noreg, impute_outcome, outcome = "rvote_2016") %>%
    bind_rows()  %>%
    bind_cols(bind_rows(param_list_states_noreg)) %>%
    mutate(restricted = "Unrestricted by region")
    ) %>%
  inner_join(reg_state, by = c("state"="inputstate")) %>%
  filter(order == 6) %>%
  filter(restricted == "Restricted within region") %>%
  ggplot(aes(x = state)) + 
  geom_pointrange(aes(y = drp - truth, ymin = drp - truth - 2 * se_drp,
                      ymax = drp - truth + 2 * se_drp,
                      )) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual("", values = c("black", "red")) +
  guides(color = F) +
  facet_grid(region ~ ., space = "free", scales = "free") +
  coord_flip() +
  xlab("") + 
  ylab("DRP Estimate - Weighted CCES") +
  theme_bw() -> p
ggsave("figure_a2.png", p, "png", "../results", width = 6, height = 5, limitsize=F)

