# # -- How in install BEAT
# install.packages(c("devtools", "ggplot2")) ## ggplot2 only needed for example
# devtools::install_version("RcppEigen", "0.3.3.7.0") ## beat does not work with newer RcppEigen
# devtools::install_github("ayeletis/beat")  ## do not update the RcppEigen if prompted
# # --

library(grf)
library(data.table)
library(ggpubr)

rm(list=ls())

# ---- Working directory  
dir <- "~/dataverse files/"
setwd(dir)

results_scenarios = NULL
for (scenario in c(1:8)) {

  print(paste('scenario',scenario,sep=" "))
  
  setwd(dir)
  my_file = paste("results_scenario_",scenario,".RData",sep='')
  load(my_file)
  
  ## Clean results_scenario
  if ('scenario' %in%  names(results_scenario)){
    results_scenario[, scenario:= NULL]
  }
  results_scenario = results_scenario[,.(ipw,target_ratio,target_weight_imbalance,policy_name,data_type)]
  results_scenario[policy_name=='uniform',target_weight_imbalance:=0]
  results_scenario[target_ratio!=0.5,target_ratio:=0.5] # sometimes the results target no one because the taus are all over the place, with variance = 20
  
  ## Clean indiv
  indiv_fairness[,data_type:='test']
  indiv_fairness[,target_ratio:=as.character(target_rate)]
  indiv_fairness[,target_rate:=NULL]
  indiv_fairness[,policy_name:=policy]
  indiv_fairness[,policy:=NULL]
  indiv_fairness[target_ratio!=0.5,target_ratio:=0.5]
  
  ## Merge them
  results_temp = merge.data.table(results_scenario,indiv_fairness, by=c('data_type','target_ratio','policy_name'),all=TRUE)
  results_temp[data_type=='test' & policy_name %in% c('uniform','grf','grf_beat','random') ,delta_ranking:=0]
  results_temp[data_type=='test' & policy_name %in% c('uniform','grf','grf_beat','random') ,delta_policy:=0]
  results_temp[,scenario:=scenario]

  results_scenarios = rbind(results_scenarios,results_temp)
}


# Prepare table with relative metrics: (1) performance over random, (2) bias as % of full
foo = results_scenarios
foo[, ipw:= as.numeric(ipw)]
foo[, target_weight_imbalance:= as.numeric(target_weight_imbalance)]

foo[, group_id := .GRP, by= .(data_type,scenario)]
ipw_random = foo[policy_name=='random',.(ipw_random=ipw,group_id)] # this change the name
foo[ipw_random, on=.(group_id), ipw_random := ipw_random ]
foo[, ipw_over_random := (ipw-ipw_random)/ipw_random]
#bias
dat_bias_demog = foo[policy_name=='grf_demog',.(bias_demog=target_weight_imbalance,group_id)] # this change the name
foo[dat_bias_demog, on=.(group_id), bias_demog := bias_demog ]
foo[,bias_of_full := target_weight_imbalance/bias_demog]
foo[,bias_demog:=NULL]
foo[,ipw_random:=NULL]
foo[,group_id:=NULL]


# Select Data to Summarize in Table
my_table = foo
my_table = my_table[data_type=='test']
my_table = my_table[target_ratio==0.5]
my_table[,ipw:=NULL]
my_table[,target_weight_imbalance:=NULL]
my_table[,target_ratio:=NULL]

Table_S_1 = my_table
print(Table_S_1)
setwd(dir)
my_file = paste("Tables_1_S1.RData",sep='')
save(Table_S_1, file = my_file)   

