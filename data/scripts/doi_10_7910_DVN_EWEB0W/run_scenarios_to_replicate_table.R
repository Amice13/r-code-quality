# # -- How in install BEAT
# install.packages(c("devtools", "ggplot2")) ## ggplot2 only needed for example
# devtools::install_version("RcppEigen", "0.3.3.7.0") ## beat does not work with newer RcppEigen
# devtools::install_github("ayeletis/beat")  ## do not update the RcppEigen if prompted
# # --

library(grf)
library(beat)
library(data.table)
library(ggpubr)
library(dplyr)
library(xgboost)

rm(list=ls())

# ---- Working directory  
dir <- "~/dataverse files/"
setwd(dir)

source("f_policy_evaluation.R")
source("f_generate_data_illustrative_example.R")
source("f_counterfactual_results.R")

## Model specs
num_trees = 2000
my_penalty = 10
my_tunable_params = c("sample.fraction","mtry","min.node.size",
                      "honesty.fraction","honesty.prune.leaves",
                      "alpha","imbalance.penalty")
# This list has the tuneable parameters from regular GRF.

## Data specs
n1 = 10000  # training
n2 = 5000; # testing
p_continuous = 4
p_discrete = 1
sigma_y = 1
sigma_tau = 2

## Simulation specs
my_seed = 12345
target_rate = 0.5

for (scenario in c(1:8)) {
print(paste('scenario',scenario,sep=" "))

# ================================================================
#            Simulate DATA
# ----------------------------------------------------------------

data = generate_data(n1=n1, n2=n2, p_continuous = p_continuous,
                        p_discrete = p_discrete, sigma_y=sigma_y,
                        seed=my_seed)

train_data = data$train_data
test_data = data$test_data
Xcols = grep("X", names(train_data), value=TRUE)
Zcols =grep('Z', names(train_data), value=TRUE)

## train
X_train = train_data[, .SD, .SDcols=Xcols]
Y_train = train_data$Y
W_train = train_data$W
Z_train = train_data[, .SD, .SDcols=Zcols]
tau_train = train_data$tau

## test
X_test = test_data[, .SD, .SDcols=Xcols]
Y_test = test_data$Y
W_test = test_data$W
Z_test = test_data[, .SD, .SDcols=Zcols]
tau_test = test_data$tau

## with demographic variables
X_train_demog = cbind(X_train,Z_train)
X_test_demog = cbind(X_test,Z_test)


# ================================================================
#            Estimate Models
# ----------------------------------------------------------------
print('Estimate models...')

setwd(dir)
source("c_run_all_models.R")
source("c_debias_tau.R")
# Plug-in XGBoost -- This part below takes long as we tune XGB via cross validation
print('tuning XGBoost...')
xg_rounds = 1500 
xg_verbose = 0 # whether XG prints rounds
source("c_optimum_params_xg.R")
source("c_plugin_xg.R")

# Estimate Taus
source("c_predict_taus.R")

# Set policies
source("c_my_targeting_policies.R")


# ================================================================
#            Compute counterfactuals for different Z
# ----------------------------------------------------------------
source("c_predict_taus_counterfactual_Z.R")


# ================================================================
#            Create table results
# ----------------------------------------------------------------

# IPW and imbalance
W.hat_train = mean(W_train) # We use prop(treatment) because the data comes from an experiment
W.hat_test = mean(W_test)
results_scenario = collect_benchmark_results(target_rate)

# Individual fairness metrics
print('Compute individual fairness metrics...')
indiv_fairness = collect_counterfactual_results(target_rate = 0.5)

setwd(dir)
my_file = paste("results_scenario_",scenario,".RData",sep='')
save(list = c('results_scenario','indiv_fairness'), file = my_file)
}
