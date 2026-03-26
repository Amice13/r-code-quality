## Note that this code needs 'fit_grf_demog' to predict the taus that will be debiased


# debiasing Tau -------------------------------------------
print('debiasing tau')

# a.- train data
tau_demog_train = predict(fit_grf_demog)$predictions

aux_forest_tau_demog_train = multi_regression_forest(X = as.matrix(Z_train),
                                                     Y = as.matrix(tau_demog_train),
                                                     num.trees = num_trees,
                                                     seed=my_seed)
tau_demog_res_train = tau_demog_train - predict(aux_forest_tau_demog_train)$predictions

# b.- test data
tau_demog_test = predict(fit_grf_demog, X_test_demog)$predictions

aux_forest_tau_demog_test = multi_regression_forest(X = as.matrix(Z_test),
                                                    Y = as.matrix(tau_demog_test),
                                                    num.trees = num_trees,
                                                    seed=my_seed)
tau_demog_res_test = tau_demog_test - predict(aux_forest_tau_demog_test)$predictions

