rm(list=ls())
setwd("") ## set working directory

library(tidyverse)
library(stargazer)
library(multiwayvcov)

##### Load data #####
X = readRDS("PovertyJJPS_Final.RData") %>%
  filter(year > 1994 & year <= 2001)

##### Table 1 #####
main_col = function(dv, adv){
  library(multiwayvcov)
  
  # Baseline
  mod1 = lm(dv ~ adv + lg_gdppc + lg_sub + lg_grow + density + as.factor(year) + as.factor(admin_ID), data=X)
  mod2 = lm(dv ~ adv + lg_riot_dummy + density + as.factor(year) + as.factor(admin_ID), data=X)
  mod3 = lm(dv ~ adv + lg_s_uyghur + density + as.factor(year) + as.factor(admin_ID), data=X)
  mod4 = lm(dv ~ adv + lg_gdppc + lg_sub + lg_grow + lg_riot_dummy + lg_s_uyghur + density + as.factor(year) + as.factor(admin_ID), data=X)
  
  # Robust-cluster
  rse1 = sqrt(diag(cluster.vcov(mod1, X$admin_ID)))
  rse2 = sqrt(diag(cluster.vcov(mod2, X$admin_ID)))
  rse3 = sqrt(diag(cluster.vcov(mod3, X$admin_ID)))
  rse4 = sqrt(diag(cluster.vcov(mod4, X$admin_ID)))
  
  return(list(m1=mod1, m2=mod2, m3=mod3, m4=mod4, r1=rse1, r2=rse2, r3=rse3, r4=rse4))
}

res1 = main_col(X$fiscal_allpc, X$wk_allpc)
res2 = main_col(X$wk_allpc, X$fiscal_allpc)

mod = list(res1$m1, res1$m2, res1$m3, res1$m4, res2$m1, res2$m2, res2$m3, res2$m4)
rse = list(res1$r1, res1$r2, res1$r3, res1$r4, res2$r1, res2$r2, res2$r3, res2$r4)

library(stargazer)
vars = c("Concurrent aid", "Lagged GDP per capita (log)", "Fiscal dependence (percent)", "Economic growth (percent)", "Riot (=1)", "Share of Uyghur (percent)", "Population density (log)", "Constant")
stargazer(mod, se=rse, digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), covariate.labels=vars, no.space=T)

##### Table A18 #####
main_a18 = function(dv, adv){
  # Baseline
  mod1 = lm(dv ~ adv + year2 + adv:year2 + lg_gdppc + lg_sub + lg_grow + density + as.factor(year) + as.factor(admin_ID), data=X)
  mod2 = lm(dv ~ adv + year2 + adv:year2 + lg_riot_raw + density + as.factor(year) + as.factor(admin_ID), data=X)
  mod3 = lm(dv ~ adv + year2 + adv:year2 + lg_s_uyghur + density + as.factor(year) + as.factor(admin_ID), data=X)
  mod4 = lm(dv ~ adv + year2 + adv:year2 + lg_gdppc + lg_sub + lg_grow + lg_riot_raw + lg_s_uyghur + density + as.factor(year) + as.factor(admin_ID), data=X)
  
  # Robust-cluster
  rse1 = sqrt(diag(cluster.vcov(mod1, X$admin_ID)))
  rse2 = sqrt(diag(cluster.vcov(mod2, X$admin_ID)))
  rse3 = sqrt(diag(cluster.vcov(mod3, X$admin_ID)))
  rse4 = sqrt(diag(cluster.vcov(mod4, X$admin_ID)))
  
  return(list(m1=mod1, m2=mod2, m3=mod3, m4=mod4, r1=rse1, r2=rse2, r3=rse3, r4=rse4))
}

res1 = main_a18(X$fiscal_allpc, X$wk_allpc)
res2 = main_a18(X$wk_allpc, X$fiscal_allpc)

mod = list(res1$m1, res1$m2, res1$m3, res1$m4, res2$m1, res2$m2, res2$m3, res2$m4)
rse = list(res1$r1, res1$r2, res1$r3, res1$r4, res2$r1, res2$r2, res2$r3, res2$r4)

vars = c("Concurrent aid", "Year", "Lagged GDP per capita (log)", "Fiscal dependence (percent)", "Economic growth (percent)", "Riot (=1)", "Share of Uyghur (percent)", "Population density (log)", "Concurrent:Year", "Constant")
stargazer(mod, se=rse, digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), covariate.labels=vars, no.space=T)

##### Figure 6 #####
cmf_res1 = res1
cmf_res2 = res2

### Fiscal assistance
modr = cmf_res1$m4
coef = na.omit(coef(modr))
k = length(coef); k
yr_new = 1:7
slopes = coef[2] + coef[k]*yr_new
var = cluster.vcov(modr, X$admin_ID)
SE = rep(NA, length(yr_new))
for(i in 1:length(yr_new)){
  require(msm)
  j = yr_new[i]
  SE[i] = deltamethod(~(x2) + (x98)*j, coef, var)
}
upper = slopes + 1.96*SE
lower = slopes - 1.96*SE

res = cbind(yr_new, slopes, upper, lower, SE)
res = data.frame(res)
res[,1] = as.factor(res[,1])

res1.cmf = res

modr = cmf_res2$m4
coef = na.omit(coef(modr))
k = length(coef); k
yr_new = 1:7
slopes = coef[2] + coef[k]*yr_new
var = cluster.vcov(modr, X$admin_ID)
SE = rep(NA, length(yr_new))
for(i in 1:length(yr_new)){
  require(msm)
  j = yr_new[i]
  SE[i] = deltamethod(~(x2) + (x98)*j, coef, var)
}
upper = slopes + 1.96*SE
lower = slopes - 1.96*SE

res = cbind(yr_new, slopes, upper, lower, SE)
res = data.frame(res)
res[,1] = as.factor(res[,1])

res2.cmf = res

res = rbind(res1.cmf, res2.cmf)
coef = res[,2]
ub = res[,3]
lb = res[,4]
mod = c(rep(c("Fiscal Assistance", "Work-for-relief"), times=1, each=7))
var = as.factor(c(rep(1994:2000, times=2, each=1)))
all = data.frame(var, coef, lb, ub, mod)

col2 = function(data=all){
  library(ggplot2)
  pd = position_dodge(width=0.5)
  ggplot(data, aes(var, coef, color=mod)) +
    geom_point(aes(shape=mod),size=4, position=pd) +
    scale_color_manual(name="",values=c("red", "blue")) +
    scale_shape_manual(name="",values=c(15,17)) +
    theme_bw() + xlab("Year") + ylab("Estimated Correlation") +
    geom_errorbar(aes(ymin=lb, ymax=ub), width=0.1, position=pd) +
    geom_hline(yintercept = 0, colour="grey", linetype = "longdash") +
    theme(text = element_text(size=20))
}

pdf("fig6.pdf", width=9, height=4.5)
col2()
dev.off()
