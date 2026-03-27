# tau with demographic -------------------------------------------
print('estimating grf demog')
fit_grf_demog <- causal_forest(X = X_train_demog, Y = Y_train, W = W_train,
                               honesty = TRUE,
                               num.trees = num_trees,
                               tune.parameters='all',
                               seed=my_seed)


# tau -- no demographics -------------------------------------------
print('estimating grf')
fit_grf <- causal_forest(X = X_train, Y = Y_train, W = W_train,
                         honesty = TRUE,
                         num.trees = num_trees,
                         tune.parameters='all',
                         seed=my_seed)


# balanced tau -------------------------------------------
source("c_beat.R")


# debiasing X -------------------------------------------
# debiasing X and Y -------------------------------------
source("c_debias_x_y.R")


# debiasing Tau -------------------------------------------
# we don't need to run anything at this point, we only need fit_grf_demog


# plug-in estimators -------------------------------------------
# run separately, as we need cross-validation and takes longer

# b.- random forest
print('estimating plugin RF')

X_train1 = train_data[W==1, .SD, .SDcols=Xcols]
Y_train1 = train_data[W==1,Y]
Z_train1 = train_data[W==1, .SD, .SDcols=Zcols]
X_train_demog1 = cbind(X_train1,Z_train1)

X_train0 = train_data[W==0, .SD, .SDcols=Xcols]
Y_train0 = train_data[W==0,Y]
Z_train0 = train_data[W==0, .SD, .SDcols=Zcols]
X_train_demog0 = cbind(X_train0,Z_train0)

fit_rf1 = regression_forest(X_train_demog1,Y=as.matrix(Y_train1),
                            num.trees = num_trees,
                            seed=my_seed)

fit_rf0 = regression_forest(X_train_demog0,as.matrix(Y_train0),
                            num.trees = num_trees,
                            seed=my_seed)


