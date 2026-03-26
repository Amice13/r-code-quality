########################## APPENDIX: NATRUAL GAS IN WV #########################

###WARNING: This example takes 17.04 hours to run 20,000 iterations with a optimized
### AWS EC2 T2 instance.

# FRONT MATTER
## Clean up
#rm(list=ls())

## Load packages
#install.packages("krige", dependencies = TRUE)
library(krige)

# Descriptive Statistics
summary(WVwells)

# DATA CLEANING
# Record means of predictors: 
# These are used BOTH to eliminate the intercept and to recover predictions later.
mean.logGas<-mean(WVwells$logGProd2012); mean.logGas
mean.logElevation<-mean(WVwells$logElevation); mean.logElevation
mean.RockPres<-mean(WVwells$RockPres); mean.RockPres

# Outcome Variable, De-Meaned
WVwells$logGas <- WVwells$logGProd2012-mean.logGas

# Explanatory Variable: DE-MEANED PREDICTORS AND NO CONSTANT TERM
# Because we deducted the mean from all predictors and the outcome,
# it is valid to do regression through the origin. 
WVwells$LogElevation <- WVwells$logElevation-mean.logElevation
WVwells$RockPressure <- WVwells$RockPres-mean.RockPres

# OLS MODEL
fracking.ols<-lm(logGas~LogElevation+RockPressure-1, data = WVwells)
summary(fracking.ols)

intercept.mod<-lm(logGProd2012~ logElevation+RockPres,data=WVwells)
summary(intercept.mod)

# RUN THE SAMPLER
# Set Number of Iterations:
M<-5000
#M<-20 # Fewer number of iterations for verification

set.seed(1000, kind="Mersenne-Twister")#SET SEED FOR CONSISTENCY

# Linear Model of Ideology with Geospatial Errors Using Metropolis-Hastings:
wv.fit <- metropolis.krige(logGas~LogElevation+RockPressure-1, 
                           coords = c("UTMESrf", "UTMNSrf"), data = WVwells, 
                           n.iter=M, powered.exp=0.5, spatial.share=0.60, 
                           range.share=0.31, beta.var=1000, range.tol=0.1, b.tune=1, 
                           nugget.tune=1, psill.tune=30)

wv.fit1 <- burnin(wv.fit, M/5)

# SUMMARY AND POST PROCESSING
class(wv.fit1)
## Summarize Results
summary(wv.fit1)

mcmcMat <- mcmc.samples(wv.fit1); class(mcmcMat)
class(wv.fit1$mcmc.mat)
mcmcMat2 <- as.matrix(wv.fit1); class(mcmcMat2)
mcmcDf <- as.data.frame(wv.fit1); class(mcmcDf)
wvData.mcmc <- coda::as.mcmc(wv.fit1, start = 1, end = 4000); class(wvData.mcmc)

## Convergence Diagnostics
geweke(wv.fit1)
heidel.welch(wv.fit1)

## Draw Semivariogram
semivariogram(wv.fit1)
#plot(wv.fit1)

# PREDICTION
## Predictive Data for the well Tapped in Putnam Co. in 2013
wv.newdata <- data.frame(LogElevation = log(643)-mean.logElevation,
                         RockPressure = 630-mean.RockPres,
                         UTMESrf = 434515.7, UTMNSrf = 4258449)

## Make predictions with 90% interval
set.seed(1000, kind="Mersenne-Twister")#SET SEED FOR CONSISTENCY
(cred.pred <- predict(wv.fit1, newdata = wv.newdata, credible = 0.9))

## Prediction in thousands of cubic feet (MCF) and the true yield in 2013:
wv.pred <- cbind(exp(cred.pred+mean.logGas), Actual.Yield = 7211)

# SAVE OUTPUT
save(wv.fit, wv.fit1, wv.pred, file = "results/wv.out.Rdata")