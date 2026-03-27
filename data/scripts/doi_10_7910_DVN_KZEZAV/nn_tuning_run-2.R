library(tfruns)

par <- list(
  dropout1 = c(0.2,0.3,0.4),
  dropout2 = c(0.2,0.3,0.4),
  neurons1 = c(64,128,256),
  neurons2 = c(64,128,256),
  l2 = c(0.0001,0.001,0.01),
  lr = c(0.0001,0.001,0.01)
)

on.exit(keras::backend()$clear_session())
runs <- tuning_run('nn_model_build.R', runs_dir = '_tuning', sample = 0.2, flags = par)

save(runs, file = "nn_tuning_runs.RData")

