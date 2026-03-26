my_targeting_policies = list(
  'grf_demog' = list(
    'train' = as.integer(tau.pred$grf_demog$train > quantile(tau.pred$grf_demog$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_demog$test > quantile(tau.pred$grf_demog$test, c(1-target_rate)))
  ),
  'grf' = list(
    'train' = as.integer(tau.pred$grf$train > quantile(tau.pred$grf$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf$test > quantile(tau.pred$grf$test, c(1-target_rate)))
  ),
  'grf_beat' = list(
    'train' = as.integer(tau.pred$grf_beat$train > quantile(tau.pred$grf_beat$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_beat$test > quantile(tau.pred$grf_beat$test, c(1-target_rate)))
  ),    
  'random' = list(
    'train' = as.integer(tau.pred$random$train > quantile(tau.pred$random$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$random$test > quantile(tau.pred$random$test, c(1-target_rate)))
  ),
  'uniform' = list(
    'train' = (sum(tau.pred$grf_demog$train)>0)*rep(1,length(tau.pred$grf_demog$train)),
    'test' = (sum(tau.pred$grf_demog$test)>0)*rep(1,length(tau.pred$grf_demog$test))
  ),
  'grf_residual_rf_x' = list(
    'train' = as.integer(tau.pred$grf_residual_rf_x$train > quantile(tau.pred$grf_residual_rf_x$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_residual_rf_x$test > quantile(tau.pred$grf_residual_rf_x$test, c(1-target_rate)))
  ),
  'grf_residual_rf_xy' = list(
    'train' = as.integer(tau.pred$grf_residual_rf_xy$train > quantile(tau.pred$grf_residual_rf_xy$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_residual_rf_xy$test > quantile(tau.pred$grf_residual_rf_xy$test, c(1-target_rate)))
  ),
  'grf_residual_tau_demog' = list(
    'train' = as.integer(tau.pred$grf_residual_tau_demog$train > quantile(tau.pred$grf_residual_tau_demog$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_residual_tau_demog$test > quantile(tau.pred$grf_residual_tau_demog$test, c(1-target_rate)))
  ),
  'plugin_rf' = list(
    'train' = as.integer(tau.pred$plugin_rf$train > quantile(tau.pred$plugin_rf$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$plugin_rf$test > quantile(tau.pred$plugin_rf$test, c(1-target_rate)))
  ),
  'plugin_xg' = list(
    'train' = as.integer(tau.pred$plugin_xg$train > quantile(tau.pred$plugin_xg$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$plugin_xg$test > quantile(tau.pred$plugin_xg$test, c(1-target_rate)))
  )
)