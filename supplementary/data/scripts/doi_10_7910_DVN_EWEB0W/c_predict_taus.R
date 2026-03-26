set.seed(my_seed) # we fix seed for the random prediction

X_rfr_test = X_test - predict(aux_forest_x,as.matrix(Z_test))$predictions

## Get score predictions
tau.pred = list(
  'grf_demog' = list(
    'train' = predict(fit_grf_demog)$predictions,
    'test' = predict(fit_grf_demog, X_test_demog)$predictions
  ),
  'grf' = list(
    'train' = predict(fit_grf)$predictions,
    'test' = predict(fit_grf, X_test)$predictions
  ),
  'grf_beat' =  list(
    'train' = predict(fit_beat)$predictions,
    'test' = predict(fit_beat, X_test)$predictions
  ),
  'random' =  list(
    'train' = runif(length(Y_train),min(predict(fit_grf)$predictions),max(predict(fit_grf)$predictions)),
    'test' = runif(length(Y_test),min(predict(fit_grf)$predictions),max(predict(fit_grf)$predictions))
  ),
  'grf_residual_rf_x' = list(
    'train' = predict(fit_rfr_x, X_rfr_train)$predictions,
    'test' = predict(fit_rfr_x, X_rfr_test)$predictions
  ),
  'grf_residual_rf_xy' = list(
    'train' = predict(fit_rfr_xy, X_rfr_train)$predictions,
    'test' = predict(fit_rfr_xy, X_rfr_test)$predictions
  ),
  'grf_residual_tau_demog' = list(
    'train' = tau_demog_res_train,
    'test' = tau_demog_res_test
  ),
  'plugin_rf' = list(
    'train' = predict(fit_rf1,X_train_demog)$predictions - predict(fit_rf0,X_train_demog)$predictions,
    'test' = predict(fit_rf1,X_test_demog)$predictions - predict(fit_rf0,X_test_demog)$predictions
  ),
  'plugin_xg' = list(
    'train' = predict(fit_xg1,as.matrix(X_train_demog)) - predict(fit_xg0,as.matrix(X_train_demog)),
    'test' = predict(fit_xg1,as.matrix(X_test_demog)) - predict(fit_xg0,as.matrix(X_test_demog))
  )
)