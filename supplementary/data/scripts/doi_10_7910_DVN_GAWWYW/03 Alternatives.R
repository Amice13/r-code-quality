rm(list=ls())
setwd("") ## set working directory

##### Tables A15 and A16 #####
rm(list=ls())

library(tidyverse)
X = readRDS("PovertyJJPS_Final.RData") %>%
  filter(year > 1994 & year <= 2001)

main_ff = function(treat, treat2, outcome){
  library(CBPS)
  library(DataCombine)
  library(multiwayvcov)
  library(lmtest)
  
  lg_outcome = paste("lg", outcome, sep="_")
  Xvar1 = c(outcome, lg_outcome, treat, "gdppcdiff", "subdiff", "growdiff", "densitydiff", "admin_ID", "year", "pre_e")
  X = X %>% dplyr::select(all_of(Xvar1)) %>% drop_na()
  colnames(X)[1:3] = c("outcome", "lg_outcome", "treat")
  
  fit = lm(outcome ~ treat + lg_outcome + gdppcdiff + subdiff + growdiff + densitydiff + as.factor(admin_ID) + as.factor(year), data=X)
  fitr = coeftest(fit, cluster.vcov(fit, X$admin_ID))
  
  #print(summary(fit))
  return(list(treat=X$treat, outcome=X$outcome, data=X, mod=fit, rse=as.numeric(fitr[,2])))
}

### Table A15
res_sec_fis = main_ff(treat="fisdiff", outcome="secdiff"); res_sec_wk = main_ff(treat="wkdiff", outcome="secdiff")
res_adm_fis = main_ff(treat="fisdiff", outcome="admdiff"); res_adm_wk = main_ff(treat="wkdiff", outcome="admdiff")
res_tax_fis = main_ff(treat="fisdiff", outcome="taxdiff"); res_tax_wk = main_ff(treat="wkdiff", outcome="taxdiff")
res_agr_fis = main_ff(treat="fisdiff", outcome="agrdiff"); res_agr_wk = main_ff(treat="wkdiff", outcome="agrdiff")
res_riot_fis = main_ff(treat="fisdiff", outcome="riot_dummy"); res_riot_wk = main_ff(treat="wkdiff", outcome="riot_dummy")

library(stargazer)
res_rev = list(res_sec_fis$mod, res_sec_wk$mod, res_adm_fis$mod, res_adm_wk$mod, res_tax_fis$mod, res_tax_wk$mod, res_agr_fis$mod, res_agr_wk$mod, res_riot_fis$mod, res_riot_wk$mod)
se_rev = list(res_sec_fis$rse, res_sec_wk$rse, res_adm_fis$rse, res_adm_wk$rse, res_tax_fis$rse, res_tax_wk$rse, res_agr_fis$rse, res_agr_wk$rse, res_riot_fis$rse, res_riot_wk$rse)
vars = c("Relief per capita", "GDP per capita", "Share of subsidy", "Economic growth rate", "Population density", "Constant")
stargazer(res_rev, se=se_rev, covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome", "as.factor"), no.space=T)

### Table A16
res_con_fis = main_ff(treat="fisdiff", outcome="condiff"); res_con_wk = main_ff(treat="condiff", outcome="hudiff")
res_hu_fis = main_ff(treat="fisdiff", outcome="hudiff"); res_hu_wk = main_ff(treat="wkdiff", outcome="hudiff")

library(stargazer)
res_rev = list(res_con_fis$mod, res_con_wk$mod, res_hu_fis$mod, res_hu_wk$mod)
se_rev = list(res_con_fis$rse, res_con_wk$rse, res_hu_fis$rse, res_hu_wk$rse)
vars = c("Relief per capita", "GDP per capita", "Share of subsidy", "Economic growth rate", "Population density", "Constant")
stargazer(res_rev, se=se_rev, covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome", "as.factor"), no.space=T)

##### Table A17 #####
rm(list=ls())
X = readRDS("PovertyJJPS_IV.RData")

main_iv = function(dv, res=T, check=F){
  library(AER)
  library(sandwich)
  mod = ivreg(dv ~ ped + lg_gdppc + lg_grow + lg_sub + s_uyghur + density + as.factor(pre_e) | 
                iv + lg_gdppc + lg_grow + lg_sub + s_uyghur + density + as.factor(pre_e), data = X)
  modr = coeftest(mod, cluster.vcov(mod, X$pre_e))
  return(list(mod=mod, rse=modr[,2]))
}

r1 = main_iv(X$secpc2)
r2 = main_iv(X$admpc2)
r3 = main_iv(X$taxpc2)
r4 = main_iv(X$riot_dummy)
r5 = main_iv(X$agrpc2)

library(stargazer)
vars = c("Per capita relief", "GDP per capita (log)", "Share of subsidy",
         "Economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
stargazer(list(r1$mod, r2$mod, r3$mod, r4$mod, r5$mod), se=list(r1$rse, r2$rse, r3$rse, r4$rse, r5$rse),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"),
          covariate.labels=vars,
          column.labels=c("Security","Admin","Revenues","Riot","Rural"),
          no.space=T)
