# We predict taus assuming the individual has a different value of Z
#   for scenarios: 1, 2, 3, 4, 6 -- We make the transformation Z1 => 1 - Z1
#   for scenarios: 5, 7, 8 -- We make the transformation Z5 => Z5 +/- SD(Z5)
  

# Prepare the NEW data
Z_test_new = Z_test
if (scenario==1 | scenario==2 | scenario==3 | scenario==4 | scenario==6) { # binary Z
  Z_test_new$Z.V1 = 1 - Z_test$Z.V1
} else if (scenario==5 | scenario==7 | scenario==8) { # continuous Z
    Z_test_new$Z.V5 = (Z_test_new$Z.V5>=0)*(Z_test_new$Z.V5 - sd(Z_test_new$Z.V5)) +  
                      (Z_test_new$Z.V5<0)*(Z_test_new$Z.V5 + sd(Z_test_new$Z.V5))
} else if (scenario==999) { #experiment
  Z_test_new$Age=scale(Z_test_new$Age)
  Z_test_new$Age = (Z_test_new$Age>=0)*(Z_test_new$Age - sd(Z_test_new$Age)) +  
    (Z_test_new$Age<0)*(Z_test_new$Age + sd(Z_test_new$Age))
}

X_test_demog_new = cbind(X_test,Z_test_new)

# Residual for X and X,Y models
X_rfr_test_new = X_test - predict(aux_forest_x,as.matrix(Z_test_new))$predictions


# Residual for X and X,Y models
tau_demog_test_new = predict(fit_grf_demog, X_test_demog_new)$predictions

aux_forest_tau_demog_test_new = multi_regression_forest(X = as.matrix(Z_test_new),
                                                    Y = as.matrix(tau_demog_test_new),
                                                    num.trees = num_trees,
                                                    seed=my_seed)
tau_demog_res_test_new = tau_demog_test_new - predict(aux_forest_tau_demog_test_new)$predictions


## Get score predictions
tau.pred_new = list(
  'grf_demog' = list(
    'test' = predict(fit_grf_demog, X_test_demog_new)$predictions
  ),
  'grf_residual_rf_x' = list(
    'test' = predict(fit_rfr_x, X_rfr_test_new)$predictions
  ),
  'grf_residual_rf_xy' = list(
    'test' = predict(fit_rfr_xy, X_rfr_test_new)$predictions
  ),
  'grf_residual_tau_demog' = list(
    'test' = tau_demog_res_test_new
  ),
  'plugin_rf' = list(
    'test' = predict(fit_rf1,X_test_demog_new)$predictions - predict(fit_rf0,X_test_demog_new)$predictions
  ),
  'plugin_xg' = list(
    'test' = predict(fit_xg1,as.matrix(X_test_demog_new)) - predict(fit_xg0,as.matrix(X_test_demog_new))
  )
)