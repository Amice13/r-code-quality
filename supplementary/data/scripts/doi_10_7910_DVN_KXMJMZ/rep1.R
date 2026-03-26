# rep1.R: Simulation

library(foreign)
library(stats)
library(MASS)
library(Matrix)
library(sandwich)
library(lmtest)

rm(list = ls())

# Load raw data
d <- read.dta("gp_data.dta")

# Authors say to drop cases of partial change (2.5, 3.5, 4.5) and 99s
d <- subset(d, !(OUTCOME==2.5|OUTCOME==3.5|OUTCOME==4.5|OUTCOME==99))

# Create study's outcome variable from raw OUTCOME measure
d$outcome <- 0
d$outcome[d$OUTCOME>=2 & d$OUTCOME<=4] <- 1

# Transforms of authors' independent variables as indicated in the paper
# Note: log() in R is natural log
d$logit.pred90_sw <- log(d$pred90_sw / (1-d$pred90_sw))
d$logit.pred50_sw <- log(d$pred50_sw / (1-d$pred50_sw))
d$net.intgrp <- log(d$INTGRP_STFAV + (0.5*d$INTGRP_SWFAV) + 1) - log(d$INTGRP_STOPP + (0.5*d$INTGRP_SWOPP)+1)

# Following author instructions, rescale and shift so that each variable ranges 0 to 1
d$scaled.90 <- (d$logit.pred90_sw-min(d$logit.pred90_sw)) / (max(d$logit.pred90_sw)-min(d$logit.pred90_sw))
d$scaled.50 <- (d$logit.pred50_sw-min(d$logit.pred50_sw)) / (max(d$logit.pred50_sw)-min(d$logit.pred50_sw))
d$scaled.intgrp <- (d$net.intgrp-min(d$net.intgrp)) / (max(d$net.intgrp)-min(d$net.intgrp))


##### Begin simulation

n.trials <- 2500

# Set the covariance deflator to 0.83 to deflate by 17% as the authors do;
# this means a target correlation between independent variables of 0.78. 
# For a target correlation of 0.25 instead, use 0.27 as the deflator.
cov.deflator <- 0.83

# Reliability of interest group variable as reported by authors
intgrp.reliability <- 0.87

# Set desired chosen (true) coefficients. First one corresponds to random
# analogue of 50th percentile variable, the second corresponds to analogue of 90th,
# third to analogue of interest group measure. Values reported in paper are 0.03, 0.76, 0.56.
beta1 <- .41

beta2 <- .76
beta3 <- .56

# Define multivariate normal distribution from which 3 randomly generated
# independent variables will be drawn. First variable will have properties
# of the authors' 50th percentile variable, second will have properties of
# the 90th percentile variable, and third will have properties of scaled
# interest group variable except with lower variance to account for the 
# authors' estimation of measurement error (0.87 is the proportion of "true" variance
# to observed variance, with (1-0.87) representing error variance). 
# Covariances will be as measured except we use the covariance deflation factor 
# defined above to account for the authors' correlated error correction.
Mu <- c(mean(d$scaled.50),mean(d$scaled.90),mean(d$scaled.intgrp))
Sigma <- matrix(c(var(d$scaled.50),
                  cov(d$scaled.90,d$scaled.50)*cov.deflator,
                  cov(d$scaled.intgrp,d$scaled.50),
                  cov(d$scaled.50,d$scaled.90)*cov.deflator,
                  var(d$scaled.90),
                  cov(d$scaled.intgrp,d$scaled.90),
                  cov(d$scaled.50,d$scaled.intgrp),
                  cov(d$scaled.90,d$scaled.intgrp),
                  intgrp.reliability*var(d$scaled.intgrp)),3,3)

coverage.count <- 0
nearzero.beta1hat.count <- 0
extreme.divergence.count <- 0
for (i in 1:n.trials) {
  
  similar.conditions <- 0
  while (similar.conditions == 0) {
    
    # Make 1779 draws of 3 from the multivariate normal distribution
    randvecs <- mvrnorm(n=nrow(d), Mu, Sigma)
  
    # Rename each column for clarity and use these names moving forward
    X1.rand <- randvecs[,1]
    X2.rand <- randvecs[,2]
    X3.rand <- randvecs[,3]
  
    # Make sure all independent variable values within zero to one bounds
    X1.rand[X1.rand>1] <- 1
    X1.rand[X1.rand<0] <- 0
    X2.rand[X2.rand>1] <- 1
    X2.rand[X2.rand<0] <- 0
    X3.rand[X3.rand>1] <- 1
    X3.rand[X3.rand<0] <- 0
  
    # Set intercept so that simulated outcome will have mean similar to mean of outcome variable in study
    alpha <- mean(d$outcome) - (beta1*mean(X1.rand) + beta2*mean(X2.rand) + beta3*mean(X3.rand))
    
    # Construct simulated outcome variable from true coefficients set above
    Y <- rnorm(nrow(d), alpha + beta1*X1.rand + beta2*X2.rand + beta3*X3.rand, sd(d$outcome))
    #Dichotomize to match data in study
    Y[Y>0.5] <- 1
    Y[Y<=0.5] <- 0

    # Check that bivariate regressions yield similar numbers as in Models 1-3 with p<0.001.
    # Use coeftest() to get p-values based on heteroskedasticity robust standard errors. Use summary() to 
    # use vanilla standard errors instead; similar results.
    lone.lm1 <- lm(Y ~ X1.rand)
    lone.lm2 <- lm(Y ~ X2.rand)
    lone.lm3 <- lm(Y ~ X3.rand)
    
    # First check that the income-based variables' coefficients are within 0.05 of their bivariate values
    if (summary(lone.lm1)$coefficients[2,1]>=0.59 & summary(lone.lm1)$coefficients[2,1]<= 0.69 & coeftest(lone.lm1, vcov = vcovHC(lone.lm1))[2,4]<0.001) {
      if (summary(lone.lm2)$coefficients[2,1]>=0.76 & summary(lone.lm2)$coefficients[2,1]<=0.86 & coeftest(lone.lm2, vcov = vcovHC(lone.lm2))[2,4]<0.001) {
        # Check that interest group variable is close to its bivariate value; larger range
        # improves computation time. Choice robust to small range or to removing check altogether
        if (summary(lone.lm3)$coefficients[2,1]>=0.34 & summary(lone.lm3)$coefficients[2,1]<=0.84 & coeftest(lone.lm3, vcov = vcovHC(lone.lm3))[2,4]<0.001) {  
          similar.conditions <- 1        
        }
      }
    }     
    
  } # End of loop that finalizes a set of randomized versions of the variables
  
  # Perform the multiple regression for this simulation step to generate estimated coefficients
  # This is represents the OLS test in the paper's core submodel
  this.lm <- lm(Y ~  X1.rand + X2.rand + X3.rand)

  # Check coverage ratio: does 95% CI for first coefficient contain the true beta1?
  upper.CI <- summary(this.lm)$coefficients[2,1] + qnorm(0.975)*coeftest(this.lm, vcov = vcovHC(this.lm))[2,2]
  lower.CI <- summary(this.lm)$coefficients[2,1] - qnorm(0.975)*coeftest(this.lm, vcov = vcovHC(this.lm))[2,2]
  if (beta1>lower.CI & beta1<upper.CI) { 
    coverage.count <- coverage.count + 1
  }
  
  # Check whether beta1 is estimated to be essentially zero (<=0.05 according to authors) despite its true value
  if (summary(this.lm)$coefficients[2,1] <= 0.05) {
    nearzero.beta1hat.count <- nearzero.beta1hat.count + 1
  }
  
  # Check how often we see an estimated coefficient divergence at least as extreme as reported in the study,
  # making sure that the larger coefficient (on the analogue to the 90th percentile variable) is
  # significant at the 99.9% level (p-value based on robust SE from coeftest())
  if (coeftest(this.lm, vcov = vcovHC(this.lm))[3,4] < 0.001 & summary(this.lm)$coefficients[3,1] >= .76 & summary(this.lm)$coefficients[2,1] <= 0.03) {
    extreme.divergence.count <- extreme.divergence.count + 1  
  }
  
  # Track simulation progress if desired
  if(i%%100==0){print(i)} # Track simulation progress if desired 
}

# Report results as proportions:
i
print(coverage.count/i)
print(nearzero.beta1hat.count/i)
print(extreme.divergence.count/i)


