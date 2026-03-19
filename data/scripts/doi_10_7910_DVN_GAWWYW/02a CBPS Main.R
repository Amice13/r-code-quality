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
    pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_outcome2 + lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  } else {
    cbpsfit = npCBPS(treat2 ~ lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, data=X2, corprior=.1/nrow(X2))
    pdf(paste(name, "pdf", sep="."), width=6, height=6); plot(cbpsfit); dev.off()
    fit = lm(outcome2 ~ treat2 + lg_gdppc2 + lg_sub2 + lg_grow2 + s_uyghur2 + density2, weights=cbpsfit$weights, data=X2)
    fitr = coeftest(fit, cluster.vcov(fit, X2$admin_ID))
  }
  
  #print(summary(fit))
  return(list(treat=X2$treat2, outcome=X2$outcome2, data=X2, mod=fit, rse=as.numeric(fitr[,2])))
}

##### Table A3 and Figures A1-A4(a) #####
res_sec_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="secgrow", lag=T, treat2="fis_lg")
res_sec_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="secgrow", lag=F, treat2="fis")
res_sec_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="secgrow", lag=T, treat2="wk_lg")
res_sec_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="secgrow", lag=F, treat2="wk")

library(stargazer)
res_sec = list(res_sec_fis_lg$mod, res_sec_fis$mod, res_sec_wk_lg$mod, res_sec_wk$mod)
se_sec = list(res_sec_fis_lg$rse, res_sec_fis$rse, res_sec_wk_lg$rse, res_sec_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_sec, se = se_sec,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A4 and Figures A1-A4(b) #####
res_adm_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="admgrow", lag=T, treat2="fis_lg")
res_adm_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="admgrow", lag=F, treat2="fis")
res_adm_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="admgrow", lag=T, treat2="wk_lg")
res_adm_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="admgrow", lag=F, treat2="wk")

library(stargazer)
res_adm = list(res_adm_fis_lg$mod, res_adm_fis$mod, res_adm_wk_lg$mod, res_adm_wk$mod)
se_adm = list(res_adm_fis_lg$rse, res_adm_fis$rse, res_adm_wk_lg$rse, res_adm_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_adm, se = se_adm,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A5 and Figures A1-A4(c) #####
res_rev_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="taxgrow", lag=T, treat2="fis_lg")
res_rev_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="taxgrow", lag=F, treat2="fis")
res_rev_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="taxgrow", lag=T, treat2="wk_lg")
res_rev_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="taxgrow", lag=F, treat2="wk")

library(stargazer)
res_rev = list(res_rev_fis_lg$mod, res_rev_fis$mod, res_rev_wk_lg$mod, res_rev_wk$mod)
se_rev = list(res_rev_fis_lg$rse, res_rev_fis$rse, res_rev_wk_lg$rse, res_rev_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_rev, se = se_rev,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A6 and Figures A1-A4(d) #####
res_aeco_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="agrow", lag=T, treat2="fis_lg")
res_aeco_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="agrow", lag=F, treat2="fis")
res_aeco_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="agrow", lag=T, treat2="wk_lg")
res_aeco_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="agrow", lag=F, treat2="wk")

library(stargazer)
res_eco = list(res_aeco_fis_lg$mod, res_aeco_fis$mod, res_aeco_wk_lg$mod, res_aeco_wk$mod)
se_eco = list(res_aeco_fis_lg$rse, res_aeco_fis$rse, res_aeco_wk_lg$rse, res_aeco_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
stargazer(res_eco, se = se_eco, covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Table A7 and Figures A1-A4(e) #####
res_riot_fis_lg = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="riot_dummy", lag=T, treat2="fis_lg")
res_riot_fis = main(treat="lg_fiscal_allpc", ctreat="lg_wk_allpc", outcome="riot_dummy", lag=F, treat2="fis")
res_riot_wk_lg = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="riot_dummy", lag=T, treat2="wk_lg")
res_riot_wk = main(treat="lg_wk_allpc", ctreat="lg_fiscal_allpc", outcome="riot_dummy", lag=F, treat2="wk")

library(stargazer)
res_riot = list(res_riot_fis_lg$mod, res_riot_fis$mod, res_riot_wk_lg$mod, res_riot_wk$mod)
se_riot = list(res_riot_fis_lg$rse, res_riot_fis$rse, res_riot_wk_lg$rse, res_riot_wk$rse)
vars = c("Treatment", "Lagged GDP per capita (log)", "Lagged share of subsidy", "Lagged economic growth rate", "Share of Uyghur population", "Population density (log)", "Constant")
ldv = c("Lagged Dep Var", "Yes", "No", "Yes", "No")
stargazer(res_riot, se = se_riot,
          add.lines = list(ldv), covariate.labels=vars,
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("outcome2"), no.space=T)

##### Figure 4 #####
res_all = c(coef(res_rev_fis$mod)[2], coef(res_rev_wk$mod)[2],
            coef(res_sec_fis$mod)[2], coef(res_sec_wk$mod)[2],
            coef(res_adm_fis$mod)[2], coef(res_adm_wk$mod)[2],
            coef(res_aeco_fis$mod)[2], coef(res_aeco_wk$mod)[2],
            coef(res_riot_fis$mod)[2], coef(res_riot_wk$mod)[2])

rse_all = c(res_rev_fis$rse[2], res_rev_wk$rse[2],
            res_sec_fis$rse[2], res_sec_wk$rse[2],
            res_adm_fis$rse[2], res_adm_wk$rse[2],
            res_aeco_fis$rse[2], res_aeco_wk$rse[2],
            res_riot_fis$rse[2], res_riot_wk$rse[2])

res_all_lg = c(coef(res_rev_fis_lg$mod)[2], coef(res_rev_wk_lg$mod)[2],
               coef(res_sec_fis_lg$mod)[2], coef(res_sec_wk_lg$mod)[2],
               coef(res_adm_fis_lg$mod)[2], coef(res_adm_wk_lg$mod)[2],
               coef(res_aeco_fis_lg$mod)[2], coef(res_aeco_wk_lg$mod)[2],
               coef(res_riot_fis_lg$mod)[2], coef(res_riot_wk_lg$mod)[2])

rse_all_lg = c(res_rev_fis_lg$rse[2], res_rev_wk_lg$rse[2],
               res_sec_fis_lg$rse[2], res_sec_wk_lg$rse[2],
               res_adm_fis_lg$rse[2], res_adm_wk_lg$rse[2],
               res_aeco_fis_lg$rse[2], res_aeco_wk_lg$rse[2],
               res_riot_fis_lg$rse[2], res_riot_wk_lg$rse[2])

coef = res_all_lg; sb = rse_all_lg
ub = coef + 1.96 * sb
lb = coef - 1.96 * sb
mod = c(rep(c("Fiscal Assistance", "Work-for-relief"), times=5, each=1))
var = c(rep(c("Revenue", "Security", "Admin", "Rural", "Violence"), times=1, each=2))
var = factor(var, levels=c("Violence", "Rural", "Revenue", "Admin", "Security"))
all_lg_95 = data.frame(var, coef, lb, ub, mod)

coef = res_all; sb = rse_all
ub = coef + 1.96 * sb
lb = coef - 1.96 * sb
mod = c(rep(c("Fiscal Assistance", "Work-for-relief"), times=5, each=1))
var = c(rep(c("Revenue", "Security", "Admin", "Rural", "Violence"), times=1, each=2))
var = factor(var, levels=c("Violence", "Rural", "Revenue", "Admin", "Security"))
all_95 = data.frame(var, coef, lb, ub, mod)

coef.plot = function(data=all){
  library(ggplot2)
  pd = position_dodge(width=0.5)
  ggplot(data, aes(var, coef, color=mod)) +
    geom_point(aes(shape=mod),size=4, position=pd) +
    scale_color_manual(name="",values=c("red", "blue")) +
    scale_shape_manual(name="",values=c(15,17)) +
    theme_bw() + xlab("") + ylab("Estimated Treatment Effect") +
    geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1, position=pd) +
    geom_hline(yintercept = 0, colour="grey", linetype = "longdash") + ylim(-0.02, 0.03) + 
    theme(text = element_text(size=20)) + coord_flip()
}

pdf("res_cbps_95.pdf", width=8, height=5); coef.plot(all_95); dev.off()
pdf("res_cbps_lg_95.pdf", width=8, height=5); coef.plot(all_lg_95); dev.off()
