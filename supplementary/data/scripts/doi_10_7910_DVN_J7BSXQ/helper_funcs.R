################################################################################
## Helper functions used for main analysis
################################################################################


# helper function to fit multilevel calibration weights to a particular focal state
fit_pew_to_state <- function(state, covs, order) {
  print(c(state, order))
  form <- paste("1 - cces ~ ", covs)
  elect2016 %>%
    filter(cces == 1 & inputstate == !!state | cces == 0) -> dat
    multical(as.formula(form), commonweight_vv_post, dat,
              lambda = 1, order = order) -> calw
  return(dat %>% left_join(calw))
}



# helper function to fit gradient boosted regression trees
fit_gb <- function(covs, outcome, ...) {
  # message(paste(paste(covs, collapse = "+")))
  dat_tr <- elect2016 %>%
    filter(cces == 0) %>%
    select(!!covs, !!outcome)
  form <- paste(outcome, "~", paste(covs, collapse = "+"))
  gb <- gbm(as.formula(form), data = dat_tr, cv.folds = 10, ...)
  best_iter <- gbm.perf(gb, method = "cv")

  pred_func <- function(x) predict(gb, x, n.trees = best_iter, type = "response")
  
  return(pred_func)
}

# helper function to predict outcomes for a particular state in CCES sample
get_preds <- function(state, pred_func) {

  return(elect2016 %>%
    filter(cces == 0 | cces == 1 & inputstate == !!state) %>%
    mutate(pred = pred_func(.))) 
}

# helper function to impute the outcome via weighting, mrp, and drp
impute_outcome <- function(caldat, outcome) {
  truth <- sum((caldat[[outcome]] * caldat$commonweight_vv_post)[caldat$cces == 1]) / 
    sum(caldat$commonweight_vv_post)
  # truth <- mean(caldat[[outcome]][caldat$cces == 1])
  unadj <- mean(caldat[[outcome]][caldat$cces == 0])
  adj <- sum((caldat$weight * caldat[[outcome]])[caldat$cces == 0]) / 
    sum(caldat$weight[caldat$cces == 0])
  mrp <- sum((caldat$pred  * caldat$commonweight_vv_post)[caldat$cces == 1]) / sum(caldat$commonweight_vv_post)
  # mrp <- mean(caldat$pred[caldat$cces == 1])
  drp <- mrp + sum((caldat$weight * (caldat[[outcome]] - caldat$pred))[caldat$cces == 0]) / 
    sum(caldat$weight[caldat$cces == 0])

  se2_adj <- sum((caldat$weight ^ 2 * (caldat[[outcome]] - adj)^2)[caldat$cces == 0]) / 
    sum(caldat$weight[caldat$cces == 0])^2
  se2_drp <- sum((caldat$weight ^ 2 * (caldat[[outcome]] - caldat$pred)^2)[caldat$cces == 0]) / 
    sum(caldat$weight[caldat$cces == 0])^2
  se2_mrp <- sum(((caldat[[outcome]] - caldat$pred)^2)[caldat$cces == 0]) / 
    sum(caldat$cces == 0)^2
  se2_unadj <- var(caldat[[outcome]][caldat$cces == 0]) / sum(caldat$cces == 0)

  n_eff <- sum(caldat$weight) ^ 2 / sum(caldat$weight ^2 )
  return(data.frame(truth = truth, unadj = unadj,
                    adj = adj, mrp = mrp, drp = drp,
                    se_adj = sqrt(se2_adj), se_drp = sqrt(se2_drp),
                    se_mrp = sqrt(se2_mrp),
                    se_unadj = sqrt(se2_unadj),
                    n_eff = n_eff))
}

# helper function to compute bias, rmse, etc. across the target states
get_metrics_2016 <- function(caldat, param_list, outcome) {
  bind_rows(lapply(caldat, impute_outcome, outcome = outcome)) %>%
  bind_cols(bind_rows(param_list)) %>%
  pivot_longer(-c(n_eff, truth, !!names(bind_rows(param_list))),
               names_to = "adj") %>%
  mutate(se = ifelse(str_detect(adj, "se"), "se", "est"),
         adj = str_replace(adj, "se_", "")) %>%
  pivot_wider(names_from = se, values_from = value) %>%
  group_by(adj, order, covs) %>%
  summarise(rmse = sqrt(mean((est - truth) ^ 2)),
            mad = mean(abs(est - truth)),
            bias = mean(est - truth),
            corr = cor(est, truth),
            coverage = mean((est - 2 * se <= truth) * (est + 2 * se >= truth) ))
}

get_imbalance <- function(caldat) {
  caldat %>%
    group_by(across(!!bal_cols)) %>%
    summarise(target_count = sum(commonweight_vv_post),
              weight_count = sum(weight[cces == 0])) %>%
    ungroup() %>%
    summarise(l2_imbal = sqrt(mean((target_count - weight_count) ^ 2)))
}