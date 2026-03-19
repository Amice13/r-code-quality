rm(list=ls())
setwd("") ## set working directory

##### Load data #####
library(tidyverse)
X = readRDS("PovertyJJPS_Final.RData") %>%
  filter(year > 1994 & year <= 2001)

##### CBPS baseline function #####
main = function(treat, treat2, ctreat, outcome, lag=F){
  library(CBPS)
  library(DataCombine)
  library(multiwayvcov)
  library(lmtest)
  
  lg_outcome = paste("lg", outcome, sep="_")
  name = paste(treat2, outcome, sep="_")
  
  Xvar1 = c(outcome, lg_outcome, treat, ctreat, "lg_gdppc", "lg_sub", "lg_grow", "s_uyghur", "density", "admin_ID", "year", "pre_e")
  X = X %>% dplyr::select(all_of(Xvar1)) %>% drop_na()
  colnames(X)[1:4] = c("outcome", "lg_outcome", "treat", "ctreat")

  X$outcome2 = lm(outcome ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_outcome2 = lm(lg_outcome ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$treat2 = lm(treat ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$ctreat2 = lm(ctreat ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  
  X$lg_gdppc2 = lm(lg_gdppc ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_sub2 = lm(lg_sub ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_grow2 = lm(lg_grow ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$density2 = lm(density ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$s_uyghur2 = lm(s_uyghur ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  
  Xvar2 = c("treat", "outcome", "outcome2", "lg_outcome2", "treat2", "ctreat2", "lg_gdppc2", "lg_sub2", "lg_grow2", "s_uyghur2", "density2", "admin_ID", "year", "pre_e")
  X2 = X %>% dplyr::select(all_of(Xvar2)) %>% drop_na()

  if(lag == T){
    cbpsfit = npCBPS(treat2 ~ lg_outcome2 + lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, data=X2, corprior=.1/nrow(X2))
    #pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_outcome2 + lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  } else {
    cbpsfit = npCBPS(treat2 ~ lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, data=X2, corprior=.1/nrow(X2))
    #pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  }

  #print(summary(fit))
  return(list(treat=X2$treat2, outcome=X2$outcome2, data=X2, mod=fit, rse=as.numeric(fitr[,2])))
}

##### Table A8 #####
res_riot_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="riot_raw", lag=T, treat2="fis_lg")
res_riot_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="riot_raw", lag=F, treat2="fis")
res_riot_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="riot_raw", lag=T, treat2="wk_lg")
res_riot_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="riot_raw", lag=F, treat2="wk")

library(stargazer)
res_riot = list(res_riot_fis_lg$mod, res_riot_fis$mod, res_riot_wk_lg$mod, res_riot_wk$mod)
se_riot = list(res_riot_fis_lg$rse, res_riot_fis$rse, res_riot_wk_lg$rse, res_riot_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_riot, se = se_riot,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A9 #####
res_con_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="congrow", lag=T, treat2="fis_lg")
res_con_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="congrow", lag=F, treat2="fis")
res_con_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="congrow", lag=T, treat2="wk_lg")
res_con_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="congrow", lag=F, treat2="wk")

library(stargazer)
res_con = list(res_con_fis_lg$mod, res_con_fis$mod, res_con_wk_lg$mod, res_con_wk$mod)
se_rev = list(res_con_fis_lg$rse, res_con_fis$rse, res_con_wk_lg$rse, res_con_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_con, se = se_rev,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A10 #####
res_placebo_sec = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="secgrow", lag=F, treat2="nonpa")
res_placebo_sec_lg = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="secgrow", lag=T, treat2="nonpa_lg")

res_placebo_adm = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="admgrow", lag=F, treat2="nonpa")
res_placebo_adm_lg = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="admgrow", lag=T, treat2="nonpa_lg")

res_placebo_riot = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="riot_dummy", lag=F, treat2="nonpa")
res_placebo_riot_lg = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="riot_dummy", lag=T, treat2="nonpa_lg")

res_placebo_agr = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="agrow", lag=F, treat2="nonpa")
res_placebo_agr_lg = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="agrow", lag=T, treat2="nonpa_lg")

res_placebo_tax = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="taxgrow", lag=F, treat2="nonpa")
res_placebo_tax_lg = main(treat="lg_nonpa_pc", ctreat="lg_wk_allpc", outcome="taxgrow", lag=T, treat2="nonpa_lg")

library(stargazer)

res_plb = list(res_placebo_sec$mod, res_placebo_sec_lg$mod,
               res_placebo_adm$mod, res_placebo_adm_lg$mod,
               res_placebo_riot$mod, res_placebo_riot_lg$mod,
               res_placebo_agr$mod, res_placebo_agr_lg$mod,
               res_placebo_tax$mod, res_placebo_tax_lg$mod)

se_plb = list(res_placebo_sec$rse, res_placebo_sec_lg$rse,
              res_placebo_adm$rse, res_placebo_adm_lg$rse,
              res_placebo_riot$rse, res_placebo_riot_lg$rse,
              res_placebo_agr$rse, res_placebo_agr_lg$rse,
              res_placebo_tax$rse, res_placebo_tax_lg$rse)

ldv = c("Lagged Dep Var", rep(c("No", "Yes"), 5))
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
stargazer(res_plb, se=se_plb, covariate.labels=vars, add.lines=list(ldv),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A11 #####
res_tel_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="telgrow", lag=T, treat2="fis_lg")
res_tel_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="telgrow", lag=F, treat2="fis")
res_tel_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="telgrow", lag=T, treat2="fis_lg")
res_tel_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="telgrow", lag=F, treat2="fis")

library(stargazer)
res_tel = list(res_tel_fis_lg$mod, res_tel_fis$mod, res_tel_wk_lg$mod, res_tel_wk$mod)
se_tel = list(res_tel_fis_lg$rse, res_tel_fis$rse, res_tel_wk_lg$rse, res_tel_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_tel, se=se_tel,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A12 #####
main_grow = function(treat, treat2, ctreat, outcome, lag=F){
  library(CBPS)
  library(DataCombine)
  library(multiwayvcov)
  library(lmtest)
  
  lg_outcome = paste("lg", outcome, sep="_")
  name = paste(treat2, outcome, sep="_")
  
  Xvar1 = c(outcome, lg_outcome, treat, ctreat, "lg_gdppc", "lg_sub", "lg_grow", "s_uyghur", "density", "admin_ID", "year", "pre_e")
  X = X %>% dplyr::select(all_of(Xvar1)) %>% drop_na()
  colnames(X)[1:4] = c("outcome", "lg_outcome", "treat", "ctreat")
  #colnames(X)[7] = c("lg_grow")
  
  X$outcome2 = lm(outcome ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_outcome2 = lm(lg_outcome ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$treat2 = lm(treat ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$ctreat2 = lm(ctreat ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  
  X$lg_gdppc2 = lm(lg_gdppc ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_sub2 = lm(lg_sub ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$density2 = lm(density ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$s_uyghur2 = lm(s_uyghur ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  
  Xvar2 = c("treat", "outcome", "outcome2", "lg_outcome2", "treat2", "ctreat2", "lg_gdppc2", "lg_sub2", "s_uyghur2", "density2", "admin_ID", "year", "pre_e")
  X2 = X %>% dplyr::select(all_of(Xvar2)) %>% drop_na()
  
  if(lag == T){
    cbpsfit = npCBPS(treat2 ~ lg_outcome2 + lg_gdppc2 + lg_sub2 + s_uyghur2 + density2, data=X2, corprior=.1/nrow(X2))
    #pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_outcome2 + lg_gdppc2 + lg_sub2 + s_uyghur2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  } else {
    cbpsfit = npCBPS(treat2 ~ lg_gdppc2 + lg_sub2 + s_uyghur2 + density2, data=X2, corprior=.1/nrow(X2))
    #pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_gdppc2 + lg_sub2 + s_uyghur2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  }  
  
  #print(summary(fit))
  return(list(treat=X2$treat2, outcome=X2$outcome2, data=X2, mod=fit, rse=as.numeric(fitr[,2])))
}

res_eco_fis_lg = main_grow(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="grow", lag=T, treat2="fis_lg")
res_eco_fis = main_grow(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="grow", lag=F, treat2="fis")
res_eco_wk_lg = main_grow(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="grow", lag=T, treat2="wk_lg")
res_eco_wk = main_grow(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="grow", lag=F, treat2="wk")

library(stargazer)
res_eco = list(res_eco_fis_lg$mod, res_eco_fis$mod, res_eco_wk_lg$mod, res_eco_wk$mod)
se_eco = list(res_eco_fis_lg$rse, res_eco_fis$rse, res_eco_wk_lg$rse, res_eco_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_eco, se = se_eco,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A13 #####
main_hu = function(treat, treat2, ctreat, outcome, lag=F){
  library(CBPS)
  library(DataCombine)
  library(multiwayvcov)
  library(lmtest)
  
  lg_outcome = paste("lg", outcome, sep="_")
  name = paste(treat2, outcome, sep="_")
  
  Xvar1 = c(outcome, lg_outcome, treat, ctreat, "lg_gdppc", "lg_sub", "lg_grow", "s_uyghur", "density", "admin_ID", "year", "pre_e")
  X = X %>% dplyr::select(all_of(Xvar1)) %>% drop_na()
  colnames(X)[1:4] = c("outcome", "lg_outcome", "treat", "ctreat")
  
  X$outcome2 = lm(outcome ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_outcome2 = lm(lg_outcome ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$treat2 = lm(treat ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$ctreat2 = lm(ctreat ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  
  X$lg_gdppc2 = lm(lg_gdppc ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_sub2 = lm(lg_sub ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$lg_grow2 = lm(lg_grow ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$density2 = lm(density ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  X$s_uyghur2 = lm(s_uyghur ~ as.factor(admin_ID) + as.factor(year), data=X)$residuals
  
  Xvar2 = c("treat", "outcome", "outcome2", "lg_outcome2", "treat2", "ctreat2", "lg_gdppc2", "lg_sub2", "lg_grow2", "s_uyghur2", "density2", "admin_ID", "year", "pre_e")
  X2 = X %>% dplyr::select(all_of(Xvar2)) %>% drop_na()
  
  if(lag == T){
    cbpsfit = npCBPS(treat2 ~ lg_outcome2 + lg_gdppc2 + lg_sub2 + lg_grow2 + density2, data=X2, corprior=.1/nrow(X2))
    #pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_outcome2 + lg_gdppc2 + lg_sub2 + lg_grow2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  } else {
    cbpsfit = npCBPS(treat2 ~ lg_gdppc2 + lg_sub2 + lg_grow2 + density2, data=X2, corprior=.1/nrow(X2))
    #pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_gdppc2 + lg_sub2 + lg_grow2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  }
  
  #print(summary(fit))
  return(list(treat=X2$treat2, outcome=X2$outcome2, data=X2, mod=fit, rse=as.numeric(fitr[,2])))
}

res_hu_fis_lg = main_hu(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="hugrow", lag=T, treat2="fis")
res_hu_fis = main_hu(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="hugrow", lag=F, treat2="fis_lg")
res_hu_wk_lg = main_hu(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="hugrow", lag=T, treat2="wk")
res_hu_wk = main_hu(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="hugrow", lag=F, treat2="wk_lg")

library(stargazer)
res_hu = list(res_hu_fis_lg$mod, res_hu_fis$mod, res_hu_wk_lg$mod, res_hu_wk$mod)
se_hu = list(res_hu_fis_lg$rse, res_hu_fis$rse, res_hu_wk_lg$rse, res_hu_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_hu, se = se_hu, 
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A14 #####
res_fisd_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="fisdepgrow", lag=T, treat2="fis_lg")
res_fisd_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="fisdepgrow", lag=F, treat2="fis")
res_fisd_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="fisdepgrow", lag=T, treat2="wk_lg")
res_fisd_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="fisdepgrow", lag=F, treat2="wk")

library(stargazer)
res_fisd = list(res_fisd_fis_lg$mod, res_fisd_fis$mod, res_fisd_wk_lg$mod, res_fisd_wk$mod)
se_fisd = list(res_fisd_fis_lg$rse, res_fisd_fis$rse, res_fisd_wk_lg$rse, res_fisd_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_fisd, se = se_fisd,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)
