rm(list=ls())
library("rstanarm")



## potential outcomes and treatment assignment
## Example from my book: https://arxiv.org/abs/2305.18793 
n  = 500
Y0 = rnorm(n)
tau = - 0.5 + Y0
Y1 = Y0 + tau
 
## perfect doctor 
Z = (tau >= 0)
Y = Z*Y1 + (1 - Z)*Y0
mean(Y[Z==1]) - mean(Y[Z==0])
 
## clueless doctor: randomized trial
Z = rbinom(n, 1, 0.5)
Y = Z*Y1 + (1 - Z)*Y0
mean(Y[Z==1]) - mean(Y[Z==0])



## Bayesian inference for SATE in RCT without X
library("Matching")
data(lalonde)
head(lalonde)
## fit a normal model under treatment
fit1 = stan_glm(re78 ~ 1,
                data = lalonde,
                subset = (treat == 1))
draws1 = as.matrix(fit1)
head(draws1)
dim(draws1)
## fit a normal model under control
fit0 = stan_glm(re78 ~ 1,
                data = lalonde,
                subset = (treat == 0))
draws0 = as.matrix(fit0)
head(draws0)
dim(draws0)

# ## infering PATE is simple and omitted 
# PATE = draws1[, 1] - draws0[, 1]
# hist(PATE)
# mean(PATE)
# sd(PATE)
# quantile(PATE, probs = c(0.25, 0.5, 0.75))


## data
n.mc = dim(draws0)[1]
Y1 = ifelse(lalonde$treat == 1, lalonde$re78, 0)
Y0 = ifelse(lalonde$treat == 0, lalonde$re78, 0)
n  = length(lalonde$treat)
n1 = sum(lalonde$treat)
n0 = sum(1 - lalonde$treat)

## impute the potential outcomes with rho = 0.5
rho = 0.5
SATE = rep(0, n.mc)
for(mc in 1:n.mc){
  for(i in 1:n){
    if(lalonde$treat[i] == 1){
      Y0[i] = rnorm(1, draws0[mc, 1] + rho*draws0[mc, 2]/draws1[mc, 2]*(Y1[i] - draws1[mc, 1]),
                    draws0[mc, 2]*sqrt(1 - rho^2))
    }else{
      Y1[i] = rnorm(1, draws1[mc, 1] + rho*draws1[mc, 2]/draws0[mc, 2]*(Y0[i] - draws0[mc, 1]),
                    draws1[mc, 2]*sqrt(1 - rho^2))
    }
  }
  SATE[mc] = mean(Y1) - mean(Y0)
}
hist(SATE)
mean(SATE)
sd(SATE)
quantile(SATE, probs = c(0.25, 0.5, 0.75))



## Bayesian Analysis of Covariance
## fit a normal model under treatment
fit1 = stan_glm(re78 ~ re74 + re75,
                data = lalonde,
                subset = (treat == 1))
draws1 = as.matrix(fit1)
head(draws1)
dim(draws1)
## fit a normal model under control
fit0 = stan_glm(re78 ~ re74 + re75,
                data = lalonde,
                subset = (treat == 0))
draws0 = as.matrix(fit0)
head(draws0)
dim(draws0)

## impute the potential outcomes with rho = 0.5
X = cbind(1, lalonde$re74, lalonde$re75)
rho = 0.5
SATE = rep(0, n.mc)
for(mc in 1:n.mc){
  for(i in 1:n){
    beta1x = sum(draws1[mc, 1:3]*X[i, ])
    beta0x = sum(draws0[mc, 1:3]*X[i, ])
    if(lalonde$treat[i] == 1){
      Y0[i] = rnorm(1, beta0x + rho*draws0[mc, 4]/draws1[mc, 4]*(Y1[i] - beta1x),
                    draws0[mc, 4]*sqrt(1 - rho^2))
    }else{
      Y1[i] = rnorm(1, beta1x + rho*draws1[mc, 4]/draws0[mc, 4]*(Y0[i] - beta0x),
                    draws1[mc, 4]*sqrt(1 - rho^2))
    }
  }
  SATE[mc] = mean(Y1) - mean(Y0)
}
hist(SATE)
mean(SATE)
sd(SATE)
quantile(SATE, probs = c(0.25, 0.5, 0.75))


## comments on the outcome model
## outcome distribution: zero-inflated and heavy-tailed
## we can improve the Normal models -- exercise
hist(lalonde$re78, breaks = 50)



## observational studies
## Data "nhanes_bmi" from the "ATE" package in R
## used in Chapter 11.1.3 of my book
nhanes_bmi = read.csv("nhanes_bmi.csv")[, -1]
z = nhanes_bmi$School_meal
y = nhanes_bmi$BMI
x = as.matrix(nhanes_bmi[, -c(1, 2)])
x = scale(x)


## outcome model only
## fit a normal model under treatment
fit1 = stan_glm(y ~ x,
                subset = (z == 1))
draws1 = as.matrix(fit1)
head(draws1)
dim(draws1)
## fit a normal model under control
fit0 = stan_glm(y ~ x,
                data = lalonde,
                subset = (z == 0))
draws0 = as.matrix(fit0)
head(draws0)
dim(draws0)

## data
n.mc = dim(draws0)[1]
Y1 = ifelse(z == 1, y, 0)
Y0 = ifelse(z == 0, y, 0)
n  = length(z)
n1 = sum(z)
n0 = sum(1 - z)


## impute the potential outcomes with rho = 0.5
X = cbind(1, x)
rho = 0.5
SATE = rep(0, n.mc)
for(mc in 1:n.mc){
  for(i in 1:n){
    beta1x = sum(draws1[mc, 1:12]*X[i, ])
    beta0x = sum(draws0[mc, 1:12]*X[i, ])
    if(z[i] == 1){
      Y0[i] = rnorm(1, beta0x + rho*draws0[mc, 13]/draws1[mc, 13]*(Y1[i] - beta1x),
                    draws0[mc, 13]*sqrt(1 - rho^2))
    }else{
      Y1[i] = rnorm(1, beta1x + rho*draws1[mc, 13]/draws0[mc, 13]*(Y0[i] - beta0x),
                    draws1[mc, 13]*sqrt(1 - rho^2))
    }
  }
  SATE[mc] = mean(Y1) - mean(Y0)
}
hist(SATE)
mean(SATE)
sd(SATE)
quantile(SATE, probs = c(0.25, 0.5, 0.75))



## discretized by the quantiles of the pscore
pscore = glm(z ~ x, family = binomial)$fitted.values
q.pscore = quantile(pscore, (1:4)/5)
ps.strata = cut(pscore, breaks = c(0,q.pscore,1), labels = 1:5)

 



