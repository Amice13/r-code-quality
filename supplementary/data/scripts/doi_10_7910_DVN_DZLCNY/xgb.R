# Aim: run a XGBoost model with features to predict terrorism a week ahead 
library(parallel)
library(purrr)
library(dplyr)
library(caret)
library(xgboost)

#provides number of cores for parallel computing
nbcores<-parallel::detectCores()

# set up xgboost hyperparameters (grid search)
gr <- expand.grid(
  max_depth        = 6, # default
  nrounds          = c(50, 200),
  eta              = c(.1, .2, .4),
  gamma            = c(0, 1),
  colsample_bytree = c(.1, .8),
  min_child_weight = c(5, 20),
  subsample        = .8)

eval <- list()
# run resampling
for (i in seq_along(temporal_csv[[1]])) {

  print(i)
  cv_i       <- setup_cv[[i]]
  test_ind_i <- cv_i$outer_test[[1]]
  index_i    <- purrr::map(cv_i$outer_train, ~.x[["train"]])
  indexOut_i <- purrr::map(cv_i$outer_train, ~.x[["test"]])
  tr_i       <- trainControl(
    method          = "cv",
    number          = length(index_i),
    summaryFunction = brierSummary,
    classProb       = TRUE,
    index           = index_i,
    indexOut        = indexOut_i,
    verboseIter     = TRUE)

  ## create model matrix + outcome for xgboost
  all_ind_i <- index_i %>% reduce(union)
  all_ind_i <- union(all_ind_i, indexOut_i %>% reduce(union))

  ## train test data
  mm_train_i   <- myreg[all_ind_i, ]
  y_train_i    <- y[all_ind_i]
  mm_test_i    <- myreg[test_ind_i, ]
  y_i_test     <- y[test_ind_i]

  xgb_i <- caret::train(mm_train_i, y_train_i,
    method        = 'xgbTree',
    trControl     = tr_i,
    tuneGrid      = gr,
    print_every_n = 50L,
    metric        = "brier",
    maximize = FALSE,
    verbose       = TRUE)

  pred_i <- predict(xgb_i, newdata = mm_test_i, type = "prob")[, 2]
  eval[[i]] <- data.frame(
    method         = "XGB",
    terror         = 1L*(y_i_test == "yes"),
    prediction     = pred_i,
    test_iteration = i)

}

saveRDS(xgb_i, paste0("results/xgb/xgb-until-2016_",mm,".Rds"))
xgboost::xgb.save(xgb_i$finalModel, paste0("results/xgb/xgbsave_",mm))

xgb_best_params <- xgb_i$bestTune

# mean AUC
eval_df <- do.call(rbind, eval) %>%
  dplyr::filter(!is.na(terror))

write.csv(eval_df, paste0("results/evaluation/eval-df-xgb_",mm,".csv"))