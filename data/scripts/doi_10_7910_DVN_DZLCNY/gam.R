# Aim: run a GLM model with features and a GP to predict terrorism a week ahead 
library(dplyr)

## model formula
v_names <- d %>%
  dplyr::select(-one_of("id", ".row_ind", "year", "week", "week_year", "terror")) %>%
  colnames()
form <- paste0("~",paste0(v_names[-c(1:2)], collapse = " + ")) %>%
  as.formula() %>%
  update(terror ~ . + s(x,y, bs = "gp") - total_terror_month -
    terrorl_month)
## GAM
eval <- list()

for (i in seq_along(temporal_csv[[1]])) {

  print(i)

  train_i <- d[temporal_csv$train[[i]], ]
  test_i  <- d[temporal_csv$test[[i]], ]

  mod_i_gam <- mgcv::bam(form, data = train_i, family = binomial(),
    method = "fREML", discrete = TRUE, control = list(trace = TRUE),
    weights = ifelse(train_i$y == "no", 1,
      sum(!train_i$terror)/sum(train_i$terror)))
  pred_i <- predict(mod_i_gam, newdata = test_i, discrete = FALSE)
  #here the prediction is on the link scale so will need transformation
  eval[[i]] <- data.frame(
    method         = "GAM",
    terror         = test_i$terror,
    prediction     = pred_i,
    test_iteration = i)

}

# mean AUC
eval_df <- do.call(rbind, eval) %>%
  filter(!is.na(terror))

write.csv(eval_df, paste0("results/evaluation/eval-df-gam_",mm,".csv"))