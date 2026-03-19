rm(list=ls())

# adjust to reflect your own path

load('~/Dropbox/pa_replication/Appendix_E/Appendix E.2/cv_non_krls/kcv_results_cf.RData')

library(dplyr)
out %>% group_by(Model) %>% summarise_at(.vars = c('RMSE', 'RMSPE'), .funs=mean)

# average by seed first, then across seeds
out %>% group_by(Model, Seed) %>% summarise_at(.vars = c('RMSE', 'RMSPE'), .funs=mean) %>% 
  group_by(Model) %>% dplyr::summarise('Lower_RMSE'=quantile(RMSE, probs=0.025),
                                       'Upper_RMSE'=quantile(RMSE, probs=0.975),
                                       'Lower_RMSPE'=quantile(RMSPE, probs=0.025),
                                       'Upper_RMSPE'=quantile(RMSPE, probs=0.975))