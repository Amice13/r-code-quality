# balanced tau -------------------------------------------
print('estimating beat')
fit_beat <- balanced_causal_forest(X_train, Y_train, W_train,
                                   target.weights = as.matrix(Z_train),
                                   target.weight.penalty = my_penalty,
                                   target.weight.standardize = TRUE,
                                   target.weight.bins.breaks = 256,
                                   honesty = TRUE,
                                   num.trees = num_trees,
                                   tune.parameters=my_tunable_params,
                                   seed=my_seed)