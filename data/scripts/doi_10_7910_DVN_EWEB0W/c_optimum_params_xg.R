library(xgboost)
library(dplyr)

## create parameters grid 
params_grid <- expand.grid(
    eta = seq(0.1, 0.9, 0.2),             # learning rate (default=0.3)
    max_depth = c(1, 5, 10),            # tree depth (default=6)
    min_child_weight = c(1, 5, 10),     # min number of obs required in each terminal node (default=1)
    subsample = c(1),                # percent of training data to sample for each tree (default=1)
    colsample_bytree = c(1)          # percent of columns to sample for each tree (default=1)
  )
  
params_grid$optimal_trees = NA 
params_grid$min_RMSE = NA
# nrow(params_grid)  #100 

## get a smaller data to grid search (N = 2000)
pick_me = rbinom(dim(train_data)[1], 1, 0.2)

## 0 -- control
params_grid0 = params_grid

X_temp = train_data[W==0 & pick_me==1, .SD, .SDcols=Xcols]
Y_temp = train_data[W==0 & pick_me==1,Y]
Z_temp = train_data[W==0 & pick_me==1, .SD, .SDcols=Zcols]
X_temp_demog = cbind(X_temp,Z_temp)

for(i in c(1:nrow(params_grid0))){
#    if (5*round(i/5)==i) {print(c(i,"out of of",nrow(params_grid0),'control'))}
    params = list(
      eta = params_grid0$eta[i],
      max_depth = params_grid0$max_depth[i],
      min_child_weight = params_grid0$min_child_weight[i],
      subsample = params_grid0$subsample[i],
      colsample_bytree = params_grid0$colsample_bytree[i]
    )
    
    set.seed(1)
    fit_xgbcv_temp <- xgb.cv(
      data = as.matrix(X_temp_demog), 
      label = as.matrix(Y_temp),
      nrounds = xg_rounds,
      verbose = xg_verbose,
      nfold = 5,
      params = params
      # early_stopping_rounds = 10   # stop if no improvement for 10 consecutive trees
    )
    
    params_grid0$optimal_trees[i] <- which.min(fit_xgbcv_temp$evaluation_log$test_rmse_mean)
    params_grid0$min_RMSE[i] <- min(fit_xgbcv_temp$evaluation_log$test_rmse_mean)     
}
optimum0 <- params_grid0 %>% arrange(min_RMSE) %>% head(1)
#print(optimum0)


## 1 treated
params_grid1 = params_grid

X_temp = train_data[W==1 & pick_me==1, .SD, .SDcols=Xcols]
Y_temp = train_data[W==1 & pick_me==1,Y]
Z_temp = train_data[W==1 & pick_me==1, .SD, .SDcols=Zcols]
X_temp_demog = cbind(X_temp,Z_temp)
for(i in c(1:nrow(params_grid1))){
#  if (5*round(i/5)==i) {print(c(i,"out of of",nrow(params_grid0),'treatment'))}
  params = list(
    eta = params_grid1$eta[i],
    max_depth = params_grid1$max_depth[i],
    min_child_weight = params_grid1$min_child_weight[i],
    subsample = params_grid1$subsample[i],
    colsample_bytree = params_grid1$colsample_bytree[i]
  )
  
  set.seed(1)
  fit_xgbcv_temp <- xgb.cv(
    data = as.matrix(X_temp_demog), 
    label = as.matrix(Y_temp),
    nrounds = xg_rounds,
    verbose = xg_verbose,
    nfold = 5,
    params = params
    # early_stopping_rounds = 10   # stop if no improvement for 10 consecutive trees
  )
  
  params_grid1$optimal_trees[i] <- which.min(fit_xgbcv_temp$evaluation_log$test_rmse_mean)
  params_grid1$min_RMSE[i] <- min(fit_xgbcv_temp$evaluation_log$test_rmse_mean)     
}
optimum1 <- params_grid1 %>% arrange(min_RMSE) %>% head(1)
#print(optimum1)
