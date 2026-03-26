# debiasing X -------------------------------------------
print('estimating debias X')
aux_forest_x = multi_regression_forest(X = as.matrix(Z_train),
                                       Y = as.matrix(X_train),
                                       num.trees = num_trees,
                                       seed=my_seed)
X_rfr_train = X_train - predict(aux_forest_x)$predictions

fit_rfr_x <- causal_forest(X_rfr_train, Y_train, W_train,
                           honesty = TRUE,
                           tune.parameters = 'all',
                           num.trees = num_trees,
                           seed=my_seed)


# debiasing X and Y -------------------------------------------
print('estimating debias X, Y')
aux_forest_y = multi_regression_forest(X = as.matrix(Z_train),
                                       Y = as.matrix(Y_train),
                                       num.trees = num_trees,
                                       seed=my_seed)
Y_rfr_train = Y_train - predict(aux_forest_y)$predictions

fit_rfr_xy <- causal_forest(X_rfr_train, Y_rfr_train, W_train,
                            honesty = TRUE,
                            tune.parameters = 'all',
                            num.trees = num_trees,
                            seed=my_seed)  

