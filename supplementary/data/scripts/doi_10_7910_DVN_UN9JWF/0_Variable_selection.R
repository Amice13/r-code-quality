
#https://eight2late.wordpress.com/2017/07/11/a-gentle-introduction-to-logistic-regression-and-lasso-regularisation-using-r/
#load required library
load("./Clean_data/data.RData")
library(dplyr)
library(glmnet)
library(BAS)
library(extrafont)
library(showtext)
library(ggplot2)
library(ggthemes)
library(GGally)
library(xtable)
showtext_auto(enable = TRUE)
#font_add('SimSun', 'SimSun.ttf')
font_add('KaiTi', 'KaiTi.ttf')

df_selection <- data %>%
            dplyr::select(-gid, -year, -gwno, -row, -col,  -xcoord, -subnational,
                          -ycoord, -lon, -lan, -nterr,-recur, -violence, -deaths_civ,  
                          -terr_dum_f1, -terr_dum_f2, -terr_dum_f3, -terr_dum_f4, 
                          -nterr_t1, -nterr_t2, -nterr_t3, -nterr_t4, 
                          -state_vi, -nonstate_vi, -onside_vi,
                          -date, -failure, -ongoing,-end.spell, -cured,
                          -atrisk, -censor,-duration,-t.0)


##using BAS glm to select variables
#times: 7:33-7:45
bas_mcmc <- bas.glm(terr_dum ~ .,
                    data = df_selection, n.models= 2^7,
                    method="MCMC", MCMC.iterations=1000000,
                    betaprior=bic.prior(), family=binomial(),
                    modelprior=uniform())
save(bas_mcmc, file = "Clean_data/bas_mcmc.RData")


#8:45—9：28 method = MCMC
BAS_ZS = bas.lm(peaceyrs_terr ~ ., 
                    data = df_selection,
                    method="MCMC",
                    prior = "ZS-null",
                   modelprior = uniform(),
                   initprobs = "marg-eplogp")
save(BAS_ZS, file = "./Clean_data/BAS_ZS.RData")
load("./Clean_data/BAS_ZS.RData")

summary(BAS_ZS)
## make a data frame for BAS-mcmc
BAS_ZS_df <- data.frame(vars = BAS_ZS$namesx,
                     probs = BAS_ZS$probne0,
                     method = "ZS-MCMC")




## load
#method = BAS
BAS_ZS_BAS = bas.lm(peaceyrs_terr ~ ., 
                    data = df_selection,
                    method="BAS",
                    prior = "ZS-null",
                    modelprior = uniform(),
                    initprobs = "marg-eplogp")
save(BAS_ZS_BAS, file = "./Clean_data/BAS_ZS_BAS.RData")

summary(BAS_ZS_BAS)

load("./Clean_data/BAS_ZS_BAS.RData")

## make a data frame for BAS
BAS_ZSBAS_df <- data.frame(vars = BAS_ZS_BAS$namesx,
                        probs = BAS_ZS_BAS$probne0,
                        method = "ZS-BAS")


HPM <- predict(BAS_ZS_BAS, estimator = "HPM")
# show the indices of variables in the best model where 0 is the intercept
HPM$bestmodel
variable.names(HPM)

#median probability model:
MPM <- predict(BAS_ZS_BAS, estimator = "MPM")
variable.names(MPM)

#best predictive model:
BPM <- predict(BAS_ZS_BAS, estimator = "BPM")
variable.names(BPM)

#BMA
BMA <- predict(BAS_ZS_BAS, estimator = "BMA")
names(BMA)

GGally::ggpairs(data.frame(
  HPM = as.vector(HPM$fit), # this used predict so we need to extract fitted values
  MPM = as.vector(MPM$fit), # this used fitted
  BPM = as.vector(BPM$fit), # this used fitted
  BMA = as.vector(BMA$fit)
)) # this used predict

# We recommend using the implementation using the Jeffreys-Zellner-Siow prior (prior='JZS') which uses numerical integration rahter than the Laplace approximation
BAS_JZS_BAS = bas.lm(peaceyrs_terr ~ ., 
                    data = df_selection,
                    method="BAS",
                    prior = "EB-global",
                    modelprior = uniform(),
                    initprobs = "marg-eplogp")
save(BAS_EB_BAS, file = "./Clean_data/BAS_EB_BAS.RData")
load("./Clean_data/BAS_EB_BAS.RData")


## make a data frame for BAS-mcmc
BAS_EB_BAdf <- data.frame(vars = BAS_EB_BAS$namesx,
                        probs = BAS_EB_BAS$probne0,
                        method = "EB-BAS")



BAS_EB = bas.lm(peaceyrs_terr ~ ., 
                     data = df_selection,
                     method="MCMC",
                     prior = "EB-global",
                     modelprior = uniform(),
                     initprobs = "marg-eplogp")
save(BAS_EB, file = "./Clean_data/BAS_EB.RData")
load("./Clean_data/BAS_EB.RData")

## make a data frame for BAS-mcmc
BAS_EB_mcmcdf <- data.frame(vars = BAS_EB$namesx,
                          probs = BAS_EB$probne0,
                          method = "EB-MCMC")


#convert training data to matrix format
x <- model.matrix(recur ~. , df_selection)
y <- df_selection$recur
#make sure y class is numerical
class(y)
#check docs to explore other type.measure options
# Parallel
library(parallel)
require(doMC)
registerDoMC(cores=4)

cv.out <- cv.glmnet(x, y, family='binomial', type.measure = 'auc',
                    parallel = TRUE, nfolds = 10)
#plot result
plot(cv.out)
#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se

coef(cv.out,s=lambda_1se)
coef(cv.out,s=lambda_min)
#coef(cv.out,s=0.001)
save(cv.out, file = "Clean_data/cv.out.RData")

lass <- glmnet(x, y,family='binomial', alpha=1)
plot(lass, label = TRUE)
coef(lass, s = lambda_1se)
save(lass, file = "Clean_data/lass.RData")



###logit model
source("Code/setup_machinelearning.R")
library(BAS)

df_selection <- data %>%
  dplyr::select(-gid, -gwarea, -gwno, -row, -col, -gem_s, -goldplacer_s, -state_vi, 
                -nonstate_vi, -onside_vi, -deaths_civ, -lon, -lan,
                -subnational, -neighboringInterStateWar
  )

#8:50—9：28
model_BAS = bas.glm(violence ~ ., 
                    data = df_selection, family=binomial(),
                    method="MCMC", n.models=20000,
                    betaprior=bic.prior(n = nrow(df_selection)),
                    modelprior=uniform())
save(model_BAS, file = "./Data/model_BAS.RData")


load("./Data/model_BAS.RData")
plot(model_BAS, ask = F)
#top model 

#plot(model_BAS, which = 4, ask=FALSE, caption="", sub.caption="")

model_BAS


## make a data frame for BAS
bas_df <- data.frame(vars = model_BAS$namesx,
                     probs = model_BAS$probne0)



