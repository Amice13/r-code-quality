# Aim: run a RF model to predict terrorism a week ahead 
library(dplyr)
library(ranger)
library(caret)
library(purrr)

nbcores <- parallel::detectCores()

## evaluation of Random Forrest

# todo: tune grid
gr <- expand.grid(
  mtry = c(5, 10, 20),
  splitrule = c("gini"),
  min.node.size = c(20, 100, 1000))

eval <- list()

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
    indexOut        = indexOut_i)

  ## create model matrix + outcome 
  all_ind_i <- index_i %>% reduce(union)
  all_ind_i <- union(all_ind_i, indexOut_i %>% reduce(union))

  ## train test data
  mm_train_i <- myreg[all_ind_i, ]
  y_train_i <- y[all_ind_i]
  mm_test_i  <- myreg[test_ind_i, ]
  y_i_test <- y[test_ind_i]

  xgb_i <- caret::train(mm_train_i, y_train_i, method = 'ranger',
    trControl = tr_i, tuneGrid  = gr,
    metric = "brier", maximize = FALSE,
    weights = c(1,30)[as.numeric(y_train_i)], num.threads = nbcores,
    preProcess = 'medianImpute')

  pred_i <- predict(xgb_i, newdata = mm_test_i, type = "prob")[, 2]

  eval[[i]] <- data.frame(
    method         = "RF",
    terror         = 1L*(y_i_test == "yes"),
    prediction     = pred_i,
    test_iteration = i)

}
# mean AUC
eval_df <- do.call(rbind, eval) %>%
  filter(!is.na(terror))

write.csv(eval_df, paste0("results/evaluation/eval-df-rf_",mm,".csv"))