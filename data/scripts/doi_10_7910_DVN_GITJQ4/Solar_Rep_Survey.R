## 
## Chao-Yo Cheng
## ccheng11@ucla.edu
## October 10, 2017
## 
## Solar_Replication_Survey.R
## 
## Replication code for:
## Aklin, Michael, Chao-Yo Cheng, and Johannes Urpelainen. 2017. 
##   "Geography, Community, Household: Adoption of Distributed Solar
##   Power across India." Energy for Sustainable Development.
##   Forthcoming.
## 

rm(list=ls())
setwd("C:/...") # Set local working directory

##### Survey design #####
library(survey)
dta = dta[is.na(dta$weight) %in% F,]
sdesign = svydesign(id = ~id,
                    strata = ~m1_q11_village_code,
                    data = dta,
                    weights = ~weight,
                    nest = T)

##### Summary statistics (Table A3) #####
vars = c("solar_dummy", "solar_home", "solar_lantern", "m3_q85", "log_income", "m1_q34", "m2_q55", "m2_q58", "weight")
sums = dta[,vars]
sums = sums[is.na(sums$solar_dummy) == F,]

library(Weighted.Desc.Stat)
sw.sum = matrix(NA, 8, 5)
for(i in 1:8){
  sw.sum[,1] = rep(nrow(sums), 8)
  sw.sum[i,2] = round(w.mean(sums[,i], sums[,9]), 3)
  sw.sum[i,3] = round(w.sd(sums[,i], sums[,9]), 3)
  sw.sum[i,4] = round(min(sums[,i]), 3)
  sw.sum[i,5] = round(max(sums[,i]), 3)
}
sw.sum[,1] = format(sw.sum[,1], big.mark=",")

colnames(sw.sum) = c("Num. Obs.", "Mean", "St. Dev.", "Min", "Max")
rownames(sw.sum) = c("Solar technology adoption (=1)",
                     "Solar home system adoption (=1)",
                     "Solar lantern adoption (=1)",
                     "Satisfaction with lighting quality",
                     "Monthly expenditure (log)",
                     "Have a bank account (=1)",
                     "Grid electricity (=1)",
                     "Micro grid (=1)")
library(stargazer); stargazer(sw.sum)

##### Analysis: solar power adoption (Table 3 Panel A) #####
mod1 = svyglm(solar_dummy ~ log_income + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())
mod2 = svyglm(solar_dummy ~ log_income + m1_q34 + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())
mod3 = svyglm(solar_dummy ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())

or.se = function(mod){
  or = exp(mod$coef)
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  return(list(or.se=or.se))
}
res.mod1 = or.se(mod1)$or.se
res.mod2 = or.se(mod2)$or.se
res.mod3 = or.se(mod3)$or.se

glm1 = glm(solar_dummy ~ log_income + as.factor(m1_q8_state_code), data=dta, family=binomial())
glm2 = glm(solar_dummy ~ log_income + m1_q34 + as.factor(m1_q8_state_code), data=dta, family=binomial())
glm3 = glm(solar_dummy ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), data=dta, family=binomial())

llk = round(c(as.numeric(logLik(glm1)), as.numeric(logLik(glm2)), as.numeric(logLik(glm3))), 3)
llk = format(llk, big.mark=",")
llk = c("Log Likelihood", llk)

aic = round(c(glm1$aic, glm2$aic, glm3$aic), 3)
aic = format(aic, big.mark=",")
aic = c("Akaike Inf. Crit.", aic)

library(stargazer)
vars = c("Monthly expenditure (log)", "Have bank account (=1)", "Grid electricity (=1)", "Constant")
stargazer(list(mod1, mod2, mod3), coef=list(exp(mod1$coef), exp(mod2$coef), exp(mod3$coef)), se=list(res.mod1, res.mod2, res.mod3),
          add.lines = list(llk, aic),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), no.space=T, covariate.labels=vars)

##### Analysis: satisfaction with lighting/electricity (Table 3 Panel B) #####
mod1 = lm(m3_q85 ~ m2_q55 + as.factor(m1_q8_state_code), data=dta, weight=dta$weight)
mod2 = lm(m3_q85 ~ m2_q55 + solar_dummy + as.factor(m1_q8_state_code), data=dta, weight=dta$weight)
mod3 = lm(m3_q85 ~ m2_q55 + solar_dummy + m2_q58 + as.factor(m1_q8_state_code), data=dta, weight=dta$weight)

library(stargazer)
vars = c("Grid electricity (=1)", "Use solar power (=1)", "Micro grid (=1)", "Constant")
stargazer(list(mod1, mod2, mod3), digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), no.space=T, covariate.labels=vars)

##### Analysis: adoption of solar home system (Table 3 Panel C) #####
mod1 = svyglm(solar_home ~ log_income + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())
mod2 = svyglm(solar_home ~ log_income + m1_q34 + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())
mod3 = svyglm(solar_home ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())

or.se = function(mod){
  or = exp(mod$coef)
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  return(list(or.se=or.se))
}
res.mod1 = or.se(mod1)$or.se
res.mod2 = or.se(mod2)$or.se
res.mod3 = or.se(mod3)$or.se

glm1 = glm(solar_home ~ log_income + as.factor(m1_q8_state_code), data=dta, family=binomial())
glm2 = glm(solar_home ~ log_income + m1_q34 + as.factor(m1_q8_state_code), data=dta, family=binomial())
glm3 = glm(solar_home ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), data=dta, family=binomial())

llk = round(c(as.numeric(logLik(glm1)), as.numeric(logLik(glm2)), as.numeric(logLik(glm3))), 3)
llk = format(llk, big.mark=",")
llk = c("Log Likelihood", llk)

aic = round(c(glm1$aic, glm2$aic, glm3$aic), 3)
aic = format(aic, big.mark=",")
aic = c("Akaike Inf. Crit.", aic)

library(stargazer)
vars = c("Monthly expenditure (log)", "Have bank account (=1)", "Grid electricity (=1)", "Constant")
stargazer(list(mod1, mod2, mod3), coef=list(exp(mod1$coef), exp(mod2$coef), exp(mod3$coef)), se=list(res.mod1, res.mod2, res.mod3),
          add.lines = list(llk, aic),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), no.space=T, covariate.labels=vars)

##### Analysis: adoption of solar lantern (Table 3 Panel D) #####
mod1 = svyglm(solar_lantern ~ log_income + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())
mod2 = svyglm(solar_lantern ~ log_income + m1_q34 + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())
mod3 = svyglm(solar_lantern ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), sdesign, family=quasibinomial())

or.se = function(mod){
  or = exp(mod$coef)
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  return(list(or.se=or.se))
}
res.mod1 = or.se(mod1)$or.se
res.mod2 = or.se(mod2)$or.se
res.mod3 = or.se(mod3)$or.se

glm1 = glm(solar_lantern ~ log_income + as.factor(m1_q8_state_code), data=dta, family=binomial())
glm2 = glm(solar_lantern ~ log_income + m1_q34 + as.factor(m1_q8_state_code), data=dta, family=binomial())
glm3 = glm(solar_lantern ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), data=dta, family=binomial())

llk = round(c(as.numeric(logLik(glm1)), as.numeric(logLik(glm2)), as.numeric(logLik(glm3))), 3)
llk = format(llk, big.mark=",")
llk = c("Log Likelihood", llk)

aic = round(c(glm1$aic, glm2$aic, glm3$aic), 3)
aic = format(aic, big.mark=",")
aic = c("Akaike Inf. Crit.", aic)

library(stargazer)
vars = c("Monthly expenditure (log)", "Have bank account (=1)", "Grid electricity (=1)", "Constant")
stargazer(list(mod1, mod2, mod3), coef=list(exp(mod1$coef), exp(mod2$coef), exp(mod3$coef)), se=list(res.mod1, res.mod2, res.mod3),
          add.lines = list(llk, aic),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), no.space=T, covariate.labels=vars)

##### Analysis: Adoption (by wealth quantiles) (Table A11) #####
qnt = quantile(dta$c11_2011_asset_none, seq(0,1,.25))
dta$wealth = cut(dta$c11_2011_asset_none, unique(qnt), include.lowest=TRUE)

wq = function(i){
  mod1 = glm(solar_dummy ~ log_income + as.factor(m1_q8_state_code), data=dta[dta$wealth == levels(dta$wealth)[i],], family=binomial())
  mod2 = glm(solar_dummy ~ log_income + m1_q34 + as.factor(m1_q8_state_code), data=dta[dta$wealth == levels(dta$wealth)[i],], family=binomial())
  mod3 = glm(solar_dummy ~ log_income + m1_q34 + m2_q55 + as.factor(m1_q8_state_code), data=dta[dta$wealth == levels(dta$wealth)[i],], family=binomial())
  
  or.se = function(mod){
    or = exp(mod$coef)
    var.diag = diag(vcov(mod))  # Variance of each coefficient
    or.se = sqrt(or^2*var.diag)
    return(list(or.se=or.se))
  }
  
  res.mod1 = or.se(mod1)$or.se
  res.mod2 = or.se(mod2)$or.se
  res.mod3 = or.se(mod3)$or.se
  
  library(stargazer)
  vars = c("Monthly expenditure (log)", "Have bank account (=1)", "Grid electricity (=1)", "Constant")
  stargazer(list(mod1, mod2, mod3), coef=list(exp(mod1$coef), exp(mod2$coef), exp(mod3$coef)), se=list(res.mod1, res.mod2, res.mod3),
            digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), no.space=T, covariate.labels=vars)
}
wq(1) # Panel A
wq(2) # Panel B
wq(3) # Panel C
wq(4) # Panel D
