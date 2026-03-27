# Aim: run an AR(1) model to predict terrorism a week ahead 

library(dplyr)

## evaluate AR1 model
eval <- list()

for (i in seq_along(temporal_csv[[1]])) {

  print(i)
  cv_i       <- setup_cv[[i]]
  test_ind_i <- cv_i$outer_test[[1]]
  index_i    <- map(cv_i$outer_train, ~.x[["train"]])
  indexOut_i <- map(cv_i$outer_train, ~.x[["test"]])

  ## create model matrix + outcome 
  all_ind_i <- index_i %>% reduce(union)
  all_ind_i <- union(all_ind_i, indexOut_i %>% reduce(union))

  ## train test data
  train_i <- data.frame(terrorl1 = myreg[all_ind_i, 'terrorl1'], y = y[all_ind_i])
  test_i  <- data.frame(terrorl1 = myreg[test_ind_i, 'terrorl1'], y = y[test_ind_i])
  test_i_x  <- data.frame(terrorl1 = myreg[test_ind_i, 'terrorl1'])

  m_i <- glm(y ~ terrorl1, data = train_i, family = binomial)

  pred_i <- predict(m_i, newdata = test_i_x, type = "response")
  eval[[i]] <- data.frame(
    method         = "ar1",
    terror         = 1L*(test_i$y == "yes"),
    prediction     = pred_i,
    test_iteration = i)
}

# mean AUC
eval_df <- do.call(rbind, eval) %>%
  filter(!is.na(terror))

write.csv(eval_df, paste0("results/evaluation/eval-df-ar1_",mm,".csv"))
