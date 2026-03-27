## 
## Chao-Yo Cheng
## ccheng11@ucla.edu
## October 10, 2017
## 
## Solar_Replication_Census.R
## 
## Replication code for:
## Aklin, Michael, Chao-Yo Cheng, and Johannes Urpelainen. 2017. 
##   "Geography, Community, Household: Adoption of Distributed Solar
##   Power across India." Energy for Sustainable Development.
##   Forthcoming.
## 

rm(list=ls())
setwd("C:/...") # Set local working directory
load("DataverseCensus.RData")

##### Summary statistics (Tables A1 and A2) #####
main = data.frame(
  c11_2011_solar_light_dummy = dta$c11_2011_solar_light_dummy,
  GHI_AnnGLO = dta$GHI_AnnGLO,
  c11_2011_total_pop = dta$c11_2011_total_pop,
  c11_2011_pucca_road = dta$c11_2011_pucca_road,
  c11_2011_asset_availing_bank = dta$c11_2011_asset_availing_bank,
  c11_2011_asset_none = dta$c11_2011_asset_none,
  c01_2001_dist_town = dta$c01_2001_dist_town,
  c01_2001_power_supl = dta$c01_2001_power_supl
)
colnames(main) = c("Solar for Lighting (=1)",
                   "Solar radiation (GHI)",
                   "Total population (log)",
                   "Pucca road (=1)",
                   "Share of HHs with banking",
                   "Share of HHs lacking assets",
                   "Distance to the nearest town (log)",
                   "Electricity in 2001 (=1)")

### Table A1 (summary statistics)
library(stargazer)
stargazer(main, summary=T, digits=2)

### Table A2 (correlation matrix)
cor.main = cor(main, use="na.or.complete")
colnames(cor.main) = paste("(", 1:ncol(cor.main), ")", sep="")
stargazer(cor.main, digits=2)

##### Regression analysis (Table 1) #####
##### For Table 2, replace "c11_2011_st_code" with "c11_2011_dist_code."
vars = c("c11_2011_vill_code", "c11_2011_solar_light_dummy", "GHI_AnnGLO", "c01_2001_dist_town", "c11_2011_total_pop", "c11_2011_pucca_road", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + as.factor(fe), data=test, family=binomial(link="logit"))
mod3 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + as.factor(fe), data=test, family=binomial(link="logit"))
mod4 = glm(c11_2011_solar_light_dummy ~ c11_2011_pucca_road + as.factor(fe), data=test, family=binomial(link="logit"))
mod5 = glm(c11_2011_solar_light_dummy ~ c11_2011_asset_availing_bank + as.factor(fe), data=test, family=binomial(link="logit"))
mod6 = glm(c11_2011_solar_light_dummy ~ c11_2011_asset_none + as.factor(fe), data=test, family=binomial(link="logit"))
mod7 = glm(c11_2011_solar_light_dummy ~ c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod8 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))

mse1 = round(mean((mod1$fitted.values-mod1$y)^2), 3)
mse2 = round(mean((mod2$fitted.values-mod2$y)^2), 3)
mse3 = round(mean((mod3$fitted.values-mod3$y)^2), 3)
mse4 = round(mean((mod4$fitted.values-mod4$y)^2), 3)
mse5 = round(mean((mod5$fitted.values-mod5$y)^2), 3)
mse6 = round(mean((mod6$fitted.values-mod6$y)^2), 3)
mse7 = round(mean((mod7$fitted.values-mod7$y)^2), 3)
mse8 = round(mean((mod8$fitted.values-mod8$y)^2), 3)

or = function(mod){
  library(broom)
  modt = tidy(mod) # Convert model to dataframe for easy manipulation
  or = exp(modt$estimate)  # Odds ratio/gradient
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  res = cbind(modt, or, var.diag, or.se)
  return(list(res=res))
}
res.mod1 = or(mod1)$res
res.mod2 = or(mod2)$res
res.mod3 = or(mod3)$res
res.mod4 = or(mod4)$res
res.mod5 = or(mod5)$res
res.mod6 = or(mod6)$res
res.mod7 = or(mod7)$res
res.mod8 = or(mod8)$res

library(stargazer)
mse = c("Mean Squared Error", mse1, mse2, mse3, mse4, mse5, mse6, mse7, mse8)
vars = c("Solar power (GHI)", "Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of availing bank", "Share of lack of asset", "Electricity in 2001 (=1)", "Constant")
stargazer(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8),
          coef = list(res.mod1[,6], res.mod2[,6], res.mod3[,6], res.mod4[,6], res.mod5[,6], res.mod6[,6], res.mod7[,6], res.mod8[,6]),
          se = list(res.mod1[,8], res.mod2[,8], res.mod3[,8], res.mod4[,8], res.mod5[,8], res.mod6[,8], res.mod7[,8], res.mod8[,8]),
          add.lines = list(mse),
          star.cutoffs=c(0.1, 0.05, 0.01), no.space=T, covariate.labels=vars, intercept.bottom=T, omit=c("as.factor"))

##### Regression analysis (Table A4) #####
vars = c("c11_2011_vill_code", "c11_2011_solar_light_dummy", "GHI_AnnGLO_weighted", "c01_2001_dist_town", "c11_2011_total_pop", "c11_2011_pucca_road", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO_weighted + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO_weighted + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))

or = function(mod){
  library(broom)
  modt = tidy(mod) # Convert model to dataframe for easy manipulation
  or = exp(modt$estimate)  # Odds ratio/gradient
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  res = cbind(modt, or, var.diag, or.se)
  return(list(res=res))
}
res.mod1 = or(mod1)$res
res.mod2 = or(mod2)$res

library(stargazer)
vars = c("Solar power (GHI)", "Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of availing bank", "Share of lack of asset", "Electricity in 2001 (=1)")
stargazer(list(mod1, mod2), coef = list(res.mod1[,6], res.mod2[,6]), se = list(res.mod1[,8], res.mod2[,8]),
          star.cutoffs=c(0.1, 0.05, 0.01), no.space=T, covariate.labels=vars, intercept.bottom=T, omit=c("as.factor"))

##### Regression analysis (Table A5) #####
vars = c("c11_2011_vill_code", "c11_2011_solar_light_log", "GHI_AnnGLO", "c01_2001_dist_town", "c11_2011_total_pop", "c11_2011_pucca_road", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = lm(c11_2011_solar_light_log ~ GHI_AnnGLO + as.factor(fe), data=test)
mod2 = lm(c11_2011_solar_light_log ~ c01_2001_dist_town + as.factor(fe), data=test)
mod3 = lm(c11_2011_solar_light_log ~ c11_2011_total_pop + as.factor(fe), data=test)
mod4 = lm(c11_2011_solar_light_log ~ c11_2011_pucca_road + as.factor(fe), data=test)
mod5 = lm(c11_2011_solar_light_log ~ c11_2011_asset_availing_bank + as.factor(fe), data=test)
mod6 = lm(c11_2011_solar_light_log ~ c11_2011_asset_none + as.factor(fe), data=test)
mod7 = lm(c11_2011_solar_light_log ~ c01_2001_power_supl + as.factor(fe), data=test)
mod8 = lm(c11_2011_solar_light_log ~ GHI_AnnGLO + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test)

library(stargazer)
vars = c("Solar power (GHI)", "Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of availing bank", "Share of lack of asset", "Electricity in 2001 (=1)", "Constant")
stargazer(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8),
          digits=3, intercept.bottom=TRUE,
          star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), covariate.labels=vars, no.space=T)

##### Interaction (Table A6 and Figure 3) #####
vars = c("c11_2011_vill_code", "c11_2011_solar_light_dummy", "GHI_AnnGLO", "c01_2001_dist_town", "c11_2011_total_pop", "c11_2011_pucca_road", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + GHI_AnnGLO:c11_2011_asset_availing_bank + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + GHI_AnnGLO:c11_2011_asset_none + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod3 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + c01_2001_dist_town:c11_2011_asset_availing_bank + GHI_AnnGLO + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod4 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + c01_2001_dist_town:c11_2011_asset_none + GHI_AnnGLO + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod5 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + c11_2011_total_pop:c11_2011_asset_availing_bank +  GHI_AnnGLO + c01_2001_dist_town + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod6 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + c11_2011_total_pop:c11_2011_asset_none + GHI_AnnGLO + c01_2001_dist_town + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))

library(stargazer)
vars = c("Solar power (GHI)", "Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of HHs with banking", "Share of HHs lacking assets", "Electricity in 2001 (=1)",
         "Solar power (GHI) * HHs with banking", "Solar power (GHI) * HHs lacking assets",
         "Dist. to nearest town * HHs with banking", "Dist. to nearest town * HHs lacking assets",
         "Total population * HHs with banking", "Total population * HHs lacking assets",
         "Constant")
stargazer(list(mod1, mod2, mod3, mod4, mod5, mod6),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), covariate.labels=vars, no.space=T)

## Interaction plot
library(MASS)
mod3 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + c01_2001_dist_town:c11_2011_asset_availing_bank + GHI_AnnGLO + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(c11_2011_st_code), data=dta, family=binomial(link="logit"), x=T)
mod4 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + c01_2001_dist_town:c11_2011_asset_none + GHI_AnnGLO + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_none + c11_2011_asset_availing_bank + c01_2001_power_supl + as.factor(c11_2011_st_code), data=dta, family=binomial(link="logit"), x=T)
mod5 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + c11_2011_total_pop:c11_2011_asset_availing_bank +  GHI_AnnGLO + c01_2001_dist_town + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(c11_2011_st_code), data=dta, family=binomial(link="logit"), x=T)
mod6 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + c11_2011_total_pop:c11_2011_asset_none + GHI_AnnGLO + c01_2001_dist_town + c11_2011_pucca_road + c11_2011_asset_none + c11_2011_asset_availing_bank + c01_2001_power_supl + as.factor(c11_2011_st_code), data=dta, family=binomial(link="logit"), x=T)

int = function(mod, xlab, ub){
  beta = coef(mod)
  covvar = summary(mod)$cov.unscaled
  coefs.sim = mvrnorm(100000, beta, covvar)
  move = seq(0, 1, 0.01)

  effect0 = apply(mod$x, 2, mean)
  effect0[2] = effect0[2]
  effects0 = matrix(NA, length(move), length(effect0))
  colnames(effects0) = names(effect0)
  for (i in 1:length(move)){
    effect0[6] = move[i]
    effect0[length(effect0)] = effect0[2]*move[i]
    effects0[i,] = effect0
  }
  results0 = coefs.sim %*% t(effects0)
  results0 = apply(results0, 1:2, function(x) (exp(x)/(1+exp(x))))

  effect1 = apply(mod$x, 2, mean)
  effect1[2] = effect1[2]+sd(mod$x[,2], na.rm=T)
  effects1 = matrix(NA, length(move), length(effect1))
  colnames(effects1) = names(effect1)
  for (i in 1:length(move)){
    effect1[6] = move[i]
    effect1[length(effect1)] = effect1[2]*move[i]
    effects1[i,] = effect1
  }
  results1 = coefs.sim %*% t(effects1)
  results1 = apply(results1, 1:2, function(x) (exp(x)/(1+exp(x))))
  
  #results = results1/results0
  results = results1-results0
  
  x = seq(0, 1, 0.01)
  means = apply(results, 2, mean)
  sdUpper = apply(results, 2, function(x) quantile(x, .975))
  sdLower = apply(results, 2, function(x) quantile(x, .025))
  plot(x, means, type = "l", xlab=xlab, ylab="Marginal Effect", ylim=c(0,ub))
  lines(x, sdUpper, lty = 2)
  lines(x, sdLower, lty = 2)
}

# Distance to the nearest town
pdf("../Manuscript/Figures/int_dist.pdf", height=5, width=10)
par(mfrow=c(1,2))
int(mod3, "Share of Households with Banking", ub=0.06)
int(mod4, "Share of Households Lacking Assets", ub=0.06)
dev.off()

# Population
pdf("../Manuscript/Figures/int_pop.pdf", height=5, width=10)
par(mfrow=c(1,2))
int(mod5, "Share of Households with Banking", ub=0.30)
int(mod6, "Share of Households Lacking Assets", ub=0.30)
dev.off()

##### Marginal effect (Table A7) #####
vars = c("c11_2011_vill_code", "c11_2011_solar_light_dummy", "GHI_AnnGLO", "c01_2001_dist_town", "c11_2011_total_pop", "c11_2011_pucca_road", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + as.factor(fe), data=test, family=binomial(link="logit"))
mod3 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + as.factor(fe), data=test, family=binomial(link="logit"))
mod4 = glm(c11_2011_solar_light_dummy ~ c11_2011_pucca_road + as.factor(fe), data=test, family=binomial(link="logit"))
mod5 = glm(c11_2011_solar_light_dummy ~ c11_2011_asset_availing_bank + as.factor(fe), data=test, family=binomial(link="logit"))
mod6 = glm(c11_2011_solar_light_dummy ~ c11_2011_asset_none + as.factor(fe), data=test, family=binomial(link="logit"))
mod7 = glm(c11_2011_solar_light_dummy ~ c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod8 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))

library(mfx)
fe = test$c11_2011_st_code
mfx1 = logitmfx(c11_2011_solar_light_dummy ~ GHI_AnnGLO + as.factor(fe), data=test)
mfx2 = logitmfx(c11_2011_solar_light_dummy ~ c01_2001_dist_town + as.factor(fe), data=test)
mfx3 = logitmfx(c11_2011_solar_light_dummy ~ c11_2011_total_pop + as.factor(fe), data=test)
mfx4 = logitmfx(c11_2011_solar_light_dummy ~ c11_2011_pucca_road + as.factor(fe), data=test)
mfx5 = logitmfx(c11_2011_solar_light_dummy ~ c11_2011_asset_availing_bank + as.factor(fe), data=test)
mfx6 = logitmfx(c11_2011_solar_light_dummy ~ c11_2011_asset_none + as.factor(fe), data=test)
mfx7 = logitmfx(c11_2011_solar_light_dummy ~ c01_2001_power_supl + as.factor(fe), data=test)
mfx8 = logitmfx(c11_2011_solar_light_dummy ~ GHI_AnnGLO + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test)

library(stargazer)
vars = c("Solar power (GHI)", "Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of availing bank", "Share of lack of asset", "Electricity in 2001 (=1)")
stargazer(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8),
          coef=list(mfx1$mfxest[,1], mfx2$mfxest[,1], mfx3$mfxest[,1], mfx4$mfxest[,1], mfx5$mfxest[,1], mfx6$mfxest[,1], mfx7$mfxest[,1], mfx8$mfxest[,1]),
          se=list(mfx1$mfxest[,2], mfx2$mfxest[,2], mfx3$mfxest[,2], mfx4$mfxest[,2], mfx5$mfxest[,2], mfx6$mfxest[,2], mfx7$mfxest[,2], mfx8$mfxest[,2]),
          digits=3, intercept.bottom=TRUE, star.cutoffs=c(0.1, 0.05, 0.01), omit=c("as.factor"), covariate.labels=vars, no.space=T)

##### Regression (Table A8) #####
vars = c("c11_2011_vill_code", "c11_2011_solar_light_dummy", "GHI_AnnGLO", "GHI_Max", "GHI_Min", "GHI_Var", "c11_2011_st_code")
test = dta[,vars]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_solar_light_dummy ~ GHI_Max + as.factor(fe), data=test, family=binomial(link="logit"))
mod3 = glm(c11_2011_solar_light_dummy ~ GHI_Min + as.factor(fe), data=test, family=binomial(link="logit"))
mod4 = glm(c11_2011_solar_light_dummy ~ GHI_Var + as.factor(fe), data=test, family=binomial(link="logit"))
mod5 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + GHI_Max + GHI_Min + GHI_Var + as.factor(fe), data=test, family=binomial(link="logit"))

or = function(mod){
  library(broom)
  modt = tidy(mod) # Convert model to dataframe for easy manipulation
  or = exp(modt$estimate)  # Odds ratio/gradient
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  res = cbind(modt, or, var.diag, or.se)
  return(list(res=res))
}
res.mod1 = or(mod1)$res
res.mod2 = or(mod2)$res
res.mod3 = or(mod3)$res
res.mod4 = or(mod4)$res
res.mod5 = or(mod5)$res

library(stargazer)
vars = c("Mean of solar radiation (GHI)", "Max of solar radiation (GHI)", "Min of solar radiation (GHI)", "Variation of solar radiation (GHI)", "Constant")
stargazer(list(mod1, mod2, mod3, mod4, mod5),
          coef = list(res.mod1[,6], res.mod2[,6], res.mod3[,6], res.mod4[,6], res.mod5[,6]),
          se = list(res.mod1[,8], res.mod2[,8], res.mod3[,8], res.mod4[,8], res.mod5[,8]),
          star.cutoffs=c(0.1, 0.05, 0.01), no.space=T, covariate.labels=vars, intercept.bottom=T, omit=c("as.factor"))

##### Regression (Table A9) #####
dta2 = dta[dta$c11_2011_power_all %in% 0,]
vars = c("c11_2011_vill_code", "c11_2011_solar_light_dummy", "GHI_AnnGLO", "c01_2001_dist_town", "c11_2011_total_pop", "c11_2011_pucca_road", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta2[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_solar_light_dummy ~ c01_2001_dist_town + as.factor(fe), data=test, family=binomial(link="logit"))
mod3 = glm(c11_2011_solar_light_dummy ~ c11_2011_total_pop + as.factor(fe), data=test, family=binomial(link="logit"))
mod4 = glm(c11_2011_solar_light_dummy ~ c11_2011_pucca_road + as.factor(fe), data=test, family=binomial(link="logit"))
mod5 = glm(c11_2011_solar_light_dummy ~ c11_2011_asset_availing_bank + as.factor(fe), data=test, family=binomial(link="logit"))
mod6 = glm(c11_2011_solar_light_dummy ~ c11_2011_asset_none + as.factor(fe), data=test, family=binomial(link="logit"))
mod8 = glm(c11_2011_solar_light_dummy ~ GHI_AnnGLO + c01_2001_dist_town + c11_2011_total_pop + c11_2011_pucca_road + c11_2011_asset_availing_bank + c11_2011_asset_none + as.factor(fe), data=test, family=binomial(link="logit"))

or = function(mod){
  library(broom)
  modt = tidy(mod) # Convert model to dataframe for easy manipulation
  or = exp(modt$estimate)  # Odds ratio/gradient
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  res = cbind(modt, or, var.diag, or.se)
  return(list(res=res))
}
res.mod1 = or(mod1)$res
res.mod2 = or(mod2)$res
res.mod3 = or(mod3)$res
res.mod4 = or(mod4)$res
res.mod5 = or(mod5)$res
res.mod6 = or(mod6)$res
res.mod8 = or(mod8)$res

library(stargazer)
vars = c("Solar radiation (GHI)", "Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of availing bank", "Share of lack of asset", "Constant")
stargazer(list(mod1, mod2, mod3, mod4, mod5, mod6, mod8),
          coef = list(res.mod1[,6], res.mod2[,6], res.mod3[,6], res.mod4[,6], res.mod5[,6], res.mod6[,6], res.mod8[,6]),
          se = list(res.mod1[,8], res.mod2[,8], res.mod3[,8], res.mod4[,8], res.mod5[,8], res.mod6[,8], res.mod8[,8]),
          star.cutoffs=c(0.1, 0.05, 0.01), no.space=T, covariate.labels=vars, intercept.bottom=T, omit=c("as.factor"))

##### Regression (Table A10) #####
vars = c("c11_2011_vill_code", "c11_2011_power_all", "GHI_AnnGLO", "c01_2001_dist_town", "c11_2011_total_pop", "c01_2001_app_pr", "c11_2011_asset_availing_bank", "c11_2011_asset_none", "c01_2001_power_supl", "c11_2011_st_code")
test = dta[,vars]
test = test[duplicated(test) %in% F,]

fe = test$c11_2011_st_code
mod1 = glm(c11_2011_power_all ~ c01_2001_dist_town + as.factor(fe), data=test, family=binomial(link="logit"))
mod2 = glm(c11_2011_power_all ~ c11_2011_total_pop + as.factor(fe), data=test, family=binomial(link="logit"))
mod3 = glm(c11_2011_power_all ~ c01_2001_app_pr + as.factor(fe), data=test, family=binomial(link="logit"))
mod4 = glm(c11_2011_power_all ~ c11_2011_asset_availing_bank + as.factor(fe), data=test, family=binomial(link="logit"))
mod5 = glm(c11_2011_power_all ~ c11_2011_asset_none + as.factor(fe), data=test, family=binomial(link="logit"))
mod6 = glm(c11_2011_power_all ~ c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))
mod7 = glm(c11_2011_power_all ~ c01_2001_dist_town + c11_2011_total_pop + c01_2001_app_pr + c11_2011_asset_availing_bank + c11_2011_asset_none + c01_2001_power_supl + as.factor(fe), data=test, family=binomial(link="logit"))

or = function(mod){
  library(broom)
  modt = tidy(mod) # Convert model to dataframe for easy manipulation
  or = exp(modt$estimate)  # Odds ratio/gradient
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  res = cbind(or, or.se)
  return(list(res=res))
}
res.mod1 = or(mod1)$res
res.mod2 = or(mod2)$res
res.mod3 = or(mod3)$res
res.mod4 = or(mod4)$res
res.mod5 = or(mod5)$res
res.mod6 = or(mod6)$res
res.mod7 = or(mod7)$res

library(stargazer)
vars = c("Dist. to the nearest town (log)", "Total population (log)", "Pucca road (=1)", "Share of HHs with banking", "Share of HHs lacking assets", "Electricity in 2001", "Constant")
stargazer(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7),
          coef = list(res.mod1[,1], res.mod2[,1], res.mod3[,1], res.mod4[,1], res.mod5[,1], res.mod6[,1], res.mod7[,1]),
          se = list(res.mod1[,2], res.mod2[,2], res.mod3[,2], res.mod4[,2], res.mod5[,2], res.mod6[,2], res.mod7[,2]),
          star.cutoffs=c(0.1, 0.05, 0.01), no.space=T, covariate.labels=vars, intercept.bottom=T, omit=c("as.factor"))
