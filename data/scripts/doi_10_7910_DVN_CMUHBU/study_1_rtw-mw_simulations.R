## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 1: right-to-work and minimum wage, simulations
## Produces tables reporting "how our experiment would have fared if we had used
## a standard static design instead of the adaptive design." 

version

library(ggrepel)
set.seed(95126)


# Set options ----
source('utils.R')
iter <- 1e+04

mturk_bandit <- read_rds('study_1_clean.rds')

fit_1 <-
  lm_robust(Y_mw ~ Z_mw - 1, weights = weights_mw, data = mturk_bandit)
fit_2 <-
  lm_robust(Y_rtw ~ Z_rtw - 1, weights = weights_rtw, data = mturk_bandit)

probs_mw <- coef(fit_1)
probs_rtw <- coef(fit_2)

if(file.exists('study_1_sims.rds')){
  study_1_sims <- read_rds('study_1_sims.rds')
}else{# Run iterated simulations ----
  
  # MW
  outmw <- mysims(probs = probs_mw, iter = iter, static = FALSE)
  outmw_stat <- mysims(probs = probs_mw, iter = iter, static = TRUE)
  
  # RTW
  outrtw <- mysims(probs = probs_rtw, iter = iter, static = FALSE)
  outrtw_stat <- mysims(probs = probs_rtw, iter = iter, static = TRUE)
  
  save(outmw, outmw_stat, 
       outrtw, outrtw_stat,
       file = 'study_1_simulations_supplement.RData')
  
  study_1_sims <- bind_rows(lapply(
    list(
    'outmw' = outmw, 
    'outmw_stat' = outmw_stat, 
    'outrtw' = outrtw, 
    'outrtw_stat' = outrtw_stat), as.data.frame),
    .id = 'simulation')
  
  write_rds(study_1_sims, 'study_1_sims.rds')
}

list2env(split(study_1_sims, study_1_sims['simulation']), envir=.GlobalEnv)

# Results table ----
# ** [study_1_simulations_iterated.csv] ----
df <- tibble(
  algorithm = rep(c('TS', 'Static'), times = 2),
  topic = rep(c('mw', 'rtw'), each = 2),
  correct = c(mean(outmw[,'correct']), 
              mean(outmw_stat[,'correct']), 
              mean(outrtw[,'correct']),
              mean(outrtw_stat[,'correct'])),
  rmse_best = c(mean(outmw[,'rmse_best']), 
                mean(outmw_stat[,'rmse_best']), 
                mean(outrtw[,'rmse_best']),
                mean(outrtw_stat[,'rmse_best'])),
  ses = c(mean(outmw[,'ses']), 
          mean(outmw_stat[,'ses']), 
          mean(outrtw[,'ses']),
          mean(outrtw_stat[,'ses']))
)

df

# write_csv(df, 'study_1_simulations_iterated.csv')