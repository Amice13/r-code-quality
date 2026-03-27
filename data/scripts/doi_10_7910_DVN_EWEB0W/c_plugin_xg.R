library(xgboost)

# plug-in estimators -------------------------------------------
print('estimating plugin XG')

X_train1 = train_data[W==1, .SD, .SDcols=Xcols]
Y_train1 = train_data[W==1,Y]
Z_train1 = train_data[W==1, .SD, .SDcols=Zcols]
X_train_demog1 = cbind(X_train1,Z_train1)

X_train0 = train_data[W==0, .SD, .SDcols=Xcols]
Y_train0 = train_data[W==0,Y]
Z_train0 = train_data[W==0, .SD, .SDcols=Zcols]
X_train_demog0 = cbind(X_train0,Z_train0)

# a.- xgboost on treated
set.seed(my_seed)

optimum = optimum1
fit_xg1 <- xgboost(
  data = as.matrix(X_train_demog1), 
  label = as.matrix(Y_train1),
  verbose = xg_verbose,
  nrounds = optimum$optimal_trees,    
  eta = optimum$eta,   
  max_depth = optimum$max_depth,
  min_child_weight = optimum$min_child_weight,  
  subsample = optimum$subsample,  
  colsample_bytree = optimum$colsample_bytree 
)

# b.- xgboost on control
set.seed(my_seed)

optimum = optimum0
fit_xg0 <- xgboost(
  data = as.matrix(X_train_demog0), 
  label = as.matrix(Y_train0),
  verbose = xg_verbose,
  nrounds = optimum$optimal_trees,
  eta = optimum$eta,  
  max_depth = optimum$max_depth,
  min_child_weight = optimum$min_child_weight,  
  subsample = optimum$subsample,  
  colsample_bytree = optimum$colsample_bytree 
)

