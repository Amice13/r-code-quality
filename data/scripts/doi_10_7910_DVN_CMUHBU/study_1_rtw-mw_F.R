## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 1: right-to-work and minimum wage, F-tests
## Produces F-statistics referenced in text for study 1

version

set.seed(95126)


# Set options ----
source('utils.R')
iter <- 1e+03

# Data ----
mturk_bandit <- read_rds('study_1_clean.rds')

# models
fit_1 <-
  lm_robust(Y_mw ~ Z_mw, weights = weights_mw, data = mturk_bandit, 
            se_type = 'classical')
fit_2 <-
  lm_robust(Y_rtw ~ Z_rtw, weights = weights_rtw, data = mturk_bandit, 
            se_type = 'classical')

# data
Y_mw <- mturk_bandit$Y_mw
Y_rtw <- mturk_bandit$Y_rtw
Z_mw <- mturk_bandit$Z_mw
Z_rtw <- mturk_bandit$Z_rtw

# null distributions
outmw_F <- mysims(Y = Y_mw, Z = sort(unique(Z_mw)), iter = iter, RI = TRUE)
outrtw_F <- mysims(Y = Y_rtw, Z = sort(unique(Z_rtw)), iter = iter, RI = TRUE)

# percentile
p_mw <- (1-max(which(sort(outmw_F[, 'F'])<summary(fit_1)$fstatistic['value']))/
  iter)
p_rtw <- (1-max(which(sort(outrtw_F[, 'F'])<summary(fit_2)$fstatistic['value']))/
  iter)


outF <- data.frame(F = c(summary(fit_1)$fstatistic['value'], 
                         summary(fit_2)$fstatistic['value']),
                   p = c(p_mw, p_rtw),
                   study = c('mw', 'rtw'))

outF

# ** [study_1_Fstats.csv] ----

# write_csv(outF, 'study_1_Fstats.csv')

