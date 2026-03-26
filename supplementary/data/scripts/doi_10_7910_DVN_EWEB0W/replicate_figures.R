# # -- How in install BEAT
# install.packages(c("devtools", "ggplot2")) ## ggplot2 only needed for example
# devtools::install_version("RcppEigen", "0.3.3.7.0") ## beat does not work with newer RcppEigen
# devtools::install_github("ayeletis/beat")  ## do not update the RcppEigen if prompted
# # --

library(grf)
library(beat)
library(data.table)
library(ggpubr)

rm(list=ls())

# ---- Working directory  
dir <- "~/dataverse files/"
setwd(dir)

source("f_generate_data_illustrative_example.R")
source("f_policy_evaluation.R")
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

## Simulation specs
target_rate = 0.5 # target ## % of the population
my_seed = 12345
scenario = 1

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

# GRF FD (full data) -------------------------------------------
print('estimating grf full data')
fit_grf_demog <- causal_forest(X = X_train_demog, Y = Y_train, W = W_train,
                               honesty = TRUE,
                               num.trees = num_trees,
                               tune.parameters='all',
                               seed=my_seed)


# GRF NP (Non Protected) -------------------------------------------
print('estimating grf non protected')
fit_grf <- causal_forest(X = X_train, Y = Y_train, W = W_train,
                         honesty = TRUE,
                         num.trees = num_trees,
                         tune.parameters='all',
                         seed=my_seed)

# BEAT -------------------------------------------
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


# ================================================================
#            Predict TAUs and Policies
# ----------------------------------------------------------------
set.seed(my_seed) # we fix seed for the random prediction

# Predict TAUs
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
  )
)

my_targeting_policies = list(
  'grf_demog' = list(
    'train' = as.integer(tau.pred$grf_demog$train > quantile(tau.pred$grf_demog$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_demog$test > quantile(tau.pred$grf_demog$test, c(1-target_rate)))
  ),
  'grf' = list(
    'train' = as.integer(tau.pred$grf$train > quantile(tau.pred$grf$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf$test > quantile(tau.pred$grf$test, c(1-target_rate)))
  ),
  'grf_beat' =  list(
    'train' = as.integer(tau.pred$grf_beat$train > quantile(tau.pred$grf_beat$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$grf_beat$test > quantile(tau.pred$grf_beat$test, c(1-target_rate)))
  ),    
  'random' = list(
    'train' = as.integer(tau.pred$random$train > quantile(tau.pred$random$train, c(1-target_rate))),
    'test' = as.integer(tau.pred$random$test > quantile(tau.pred$random$test, c(1-target_rate)))
  )
)


# ================================================================
##                Numbers reported in text
## ---------------------------------------------------------------

# Z1-Ratio for targeted individuals
ratio_grf_fd = c(mean((Z_test[,1]==1)*my_targeting_policies$grf_demog$test),
                 mean((Z_test[,1]==0)*my_targeting_policies$grf_demog$test))

ratio_grf_np = c(mean((Z_test[,1]==1)*my_targeting_policies$grf$test),
                 mean((Z_test[,1]==0)*my_targeting_policies$grf$test))

ratio_grf_beat = c(mean((Z_test[,1]==1)*my_targeting_policies$grf_beat$test),
                   mean((Z_test[,1]==0)*my_targeting_policies$grf_beat$test))

my_kpis = data.table(rbind(ratio_grf_fd,ratio_grf_np,ratio_grf_beat))
names(my_kpis) = c("z_1","z_0")
my_kpis[,ratio := z_1/z_0]


# IWP over random policy
ipw_grf_fd  = v_ips_function(Y_test, W_test, mean(W_test), my_targeting_policies$grf_demog$test)
ipw_grf_np  = v_ips_function(Y_test, W_test, mean(W_test), my_targeting_policies$grf$test)
ipw_beat  = v_ips_function(Y_test, W_test, mean(W_test), my_targeting_policies$grf_beat$test)
ipw_random  = v_ips_function(Y_test, W_test, mean(W_test), my_targeting_policies$random$test)

# ipw_random = 0.5 # Set by the simulation
my_kpis[,ipw_over_random := c(ipw_grf_fd/ipw_random-1,ipw_grf_np/ipw_random-1,ipw_beat/ipw_random-1)]

# Individual fairness metrics
Z_test_new = Z_test
Z_test_new$Z.V1 = 1 - Z_test$Z.V1
X_test_demog_new = cbind(X_test,Z_test_new)
tau.pred_new = list(
  'grf_demog' = list(
    'test' = predict(fit_grf_demog, X_test_demog_new)$predictions
  )
)
indiv_fairness = collect_counterfactual_results(target_rate = 0.5)


print(my_kpis)
#(ratios of 12, 5, and 1)
#(efficiency of 81%, 71%, and 44%)

print(indiv_fairness)
#(83% of users changing allocation)

# ================================================================
##                Create Figures 1, 2, 3
## ---------------------------------------------------------------
# Prepare data for figures
aux_data = data.table(tau_true = tau_test, tau_grf_d = tau.pred$grf_demog$test, tau_grf = tau.pred$grf$test,
                      tau_beat = tau.pred$grf_beat$test, Z = 1*(Z_test[,1]==1),
                      X1 = X_test[,1], X2 = X_test[,2], X3 = X_test[,3], X4 = X_test[,4])
names(aux_data)[5] = "Z"
aux_data$Z = as.factor((aux_data$Z))
names(aux_data)[6] = "X1"
names(aux_data)[7] = "X2"
names(aux_data)[8] = "X3"
names(aux_data)[9] = "X4"
aux_data[, Protected := factor(Z,
                               labels = c("0", "1"),
                               levels=c('0','1'))]

setwd(dir)
source("c_create_figures_1_3.R")

setwd(dir)
p_example_grf_fd
p_example_grf_np
p_example_beat
ggsave("Figure_1.jpg",plot = p_example_grf_fd, width = 14, height=3)
ggsave("Figure_2.jpg",plot = p_example_grf_np, width = 14, height=3)
ggsave("Figure_3.jpg",plot = p_example_beat, width = 14, height=3)


# ================================================================
##                Create Trade-off plot -- Figure 4
## ---------------------------------------------------------------
library(ggnewscale)
library(directlabels)
library(ggforce)

setwd(dir)
source("f_beat_multiple_penalties.R")


## ------
## WARNING: Estimating all taus (per penalty) could take a lot of memory. 
## In the paper, we used:
## penalty_vect = c(seq(0,2,0.05),3,4,5)
## To generate the exact some figure, consider splitting the penalty_vect in blocks,
## and saving each block's data before plottong. e.g., penalty_vect1 =  seq(0,0.7,0.05) , 
## penalty_vect2 =   seq(0.75,1.4,0.05), penalty_vect3 =   c(seq(1.45,2,0.05),3,4,5)
## another option to speed things up and generate a similar plot with fewer data 
## points is using (comment the below,and uncomment the above if desired)
penalty_vect = c(seq(0,1,0.1),seq(1.1,1.5,0.05),2,3,4,5)
## ------
## ------

taus_per_penalty = rbindlist(lapply(penalty_vect, estimate_taus_per_penalty))

# Add "is_targeted" column per penalty rate
taus_per_penalty[, is_targeted := target_tau_proportion(tau_predict,
                                                        target_rate=target_rate),
                 by=.(penalty_rate, data_type)]


# Collect results from GRF benchmarks
W.hat_train = mean(W_train)
W.hat_test = mean(W_test)
results_grf = collect_grf_results(target_rate)
results_grf[,penalty_rate:=10]
results_grf = results_grf[data_type=='test']

# Collect results
results_penalty_test = taus_per_penalty[data_type=="test",
                                        collect_penalty_results(is_targeted,"test"),
                                        by=.(penalty_rate )]

results_penalty = rbind(results_penalty_test)


setwd(dir)

# Combine results and prepare (relative) metrics for table
results_all = rbind(results_penalty,results_grf)

policy_results = results_all[,.(target_rate = target_rate,
                                   data_type, penalty_rate,policy_name, ipw,
                                   imbalance = target_weight_imbalance)]
  
policy_results[, ipw:= as.numeric(ipw)]
policy_results[, imbalance:= as.numeric(imbalance)]
policy_results[, ipw_over_random := (ipw/ipw_random)-1]

# We define bias of full := my_bias / bias_demog
policy_results[, group_id := .GRP, by= .(data_type,target_rate)]
dat_bias_demog = policy_results[policy_name=='grf_demog' & penalty_rate==10,.(bias_demog=imbalance,group_id)] 
policy_results[dat_bias_demog, on=.(group_id), bias_demog := bias_demog ]
policy_results[,bias_of_full := imbalance/bias_demog]
policy_results[,bias_demog:=NULL]
policy_results[,group_id:=NULL]

# validity check (same outcomes as in illustrative example max penalty)
policy_results[target_rate==target_rate & penalty_rate==10 & data_type=='test',.(policy_name,ipw_over_random)]


# ================================================================
#    SELECTED TRADE-OFF PLOT -- IPW gain vs. Imbalance on TEST Data
# ----------------------------------------------------------------
data_to_plot = policy_results

# last point per group, for labels
point_annot_dat = data_to_plot[target_rate==target_rate,
                               lapply(.SD, tail, n=1),
                               by=.(policy_name, data_type)]
# BEAT results
main_points = data_to_plot[target_rate==target_rate & policy_name=="balanced_grf"]

# Benchmarks
benchmark_points = data_to_plot[ target_rate==target_rate &  policy_name!="balanced_grf"]

# Fix labels
point_annot_dat[,Method := factor(policy_name,
                                  labels = c('CF (full)','CF (w/o protected)','BEAT',
                                             'Random'),
                                  levels=c('grf_demog','grf','balanced_grf',
                                           'random'))]
main_points[,Method := factor(policy_name,
                              labels = c('BEAT'),
                              levels=c('balanced_grf'))]

benchmark_points[,Method := factor(policy_name,
                                   labels = c('CF (full)','CF (w/o protected)','DeBiased'),
                                   levels=c('grf_demog','grf','grf_residual_rf'))]

dat_plot <- rbind(point_annot_dat[, .(imbalance, ipw_over_random, Method, penalty_rate)], main_points[, .(imbalance, ipw_over_random, Method, penalty_rate)])
dat_plot[!c(14:19), penalty_rate := NA]  # only keep gamma = 1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3
dat_plot$Method <- factor(dat_plot$Method, levels=c("CF (full)", "CF (w/o protected)", "BEAT"), labels = c("CF (full data)", "CF (w/o protected)", expression(paste("BEAT (varying ", gamma, ")"))))

theme_set(theme_bw())
tradeoff_plot <- ggplot(palette = get_palette("npg", 3)) +
  geom_point(data=dat_plot[!Method %in% c("CF (full data)", "CF (w/o protected)")], aes(x=imbalance, y=ipw_over_random, shape = Method, color = Method), size = 2) +
  geom_line(data=dat_plot[!Method %in% c("CF (full data)", "CF (w/o protected)")], aes(x=imbalance, y=ipw_over_random), size=1,color = "red2") +
  geom_point(data=dat_plot[Method %in% c("CF (full data)", "CF (w/o protected)")], aes(x=imbalance, y=ipw_over_random, shape = Method, color = Method), size = 3) +
  scale_shape_manual(name="Method",
                     values=c(15,17,19),
                     labels=c("CF (full data)", "CF (w/o protected)", expression(paste("BEAT (varying ", gamma, ")")))) +
  scale_color_manual(name="Method",
                     values=c('black', 'black', "red2"),
                     labels=c("CF (full data)", "CF (w/o protected)", expression(paste("BEAT (varying ", gamma, ")")))) +
  theme(legend.position = c(0.9, 0.15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text.align = 0, legend.key.size = unit(0.5, "cm")) +
  labs(x="Imbalance", y="Efficiency") +
  annotate("text", x = 0.01, y=0.41, label = "BEAT") +
  annotate("text", x=0.48, y=0.69, label = "CF (w/o protected)") +
  annotate("text", x=0.67, y=0.79, label = "CF (full data)") +
  annotate("text", x=0.42, y=0.73, label = expression(paste(gamma, "=0")), color="blue")+
  annotate("text", x = -0.025, y=0.43, label = expression(paste(gamma, "=5")), color="blue")+
  geom_text(data=dat_plot, aes(x=imbalance, y=ipw_over_random,label=penalty_rate), hjust = 1.2, vjust = -0.5, color="blue")

print(tradeoff_plot)
setwd(dir)
ggsave("Figure_4.jpg", tradeoff_plot, width = 8, height = 4)







