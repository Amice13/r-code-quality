## packages
library("randomizr")
library("estimatr")

## normally distributed control group with mean 0 and SD 1
make_Y0 <- function(N){  rnorm(n = N) }


## function for randomized treatment assignment where the difference between
## treated and untreated observations is tau
repeat_experiment_and_test <- function(N, Y0, tau){
  Z <- complete_ra(N = N)
  Y1 <- Y0 + Z * tau
  Yobs <- Z * Y1 + (1 - Z) * Y0
  estimator <- lm_robust(Yobs ~ Z)
  pval <- estimator$p.value[2]
  return(pval)
}


## function to solve for power
power_sim <- function(N,tau,sims){
  Y0 <- make_Y0(N)
  pvals <- replicate(n=sims, 
                     repeat_experiment_and_test(N=N,Y0=Y0,tau=tau))
  pow <- sum(pvals < .05) / sims
  return(pow)
}

## power analysis
set.seed(2023) 
power_sim(N=6400,tau=.1,sims=1000)


## power simulations for various effect sizes
possible.taus <- seq(from = 0, to = 1, by = 0.01)
power <- rep(NA, length(possible.taus)) 

for (j in 1:length(possible.taus)){
  tau.f <- possible.taus[j]            
  power[j] <- power_sim(N = 6400, tau = tau.f, sims=1000) 
 
}

## power plot for set N
plot(possible.taus, power, ylim=c(0,1), cex=2, pch=16, cex.lab=2, cex.axis=2, cex.main=2,
     main= "A: Power analysis for set N = 6400, p < 0.05 ",
     xlab = expression(paste("standardized effect size ", delta)))
abline(h=0.8, col="red", lwd=5)
abline(h=0.9, col="orange", lwd=5)



## power simulations for various sample sizes
possible.N <- seq(from = 200, to = 6400, by = 62)
power <- rep(NA, length(possible.N))

for (j in 1:length(possible.N)){
  N.f <- possible.N[j]            
  power[j] <- power_sim(N = N.f, tau = 0.1, sims=1000) 
  
}

## power plot for set delta
plot(possible.N, power, ylim=c(0,1), cex=2, pch=16, cex.lab=2, cex.axis=2, cex.main=2, 
     xaxp = c(200, 6400, 62),
     main= "B: Power analysis for set δ = 0.1, p < 0.05",
     xlab = expression(paste("N")))
abline(h=0.8, col="red", lwd=5)
abline(h=0.9, col="orange", lwd=5)



