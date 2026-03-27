################################################################################
# SAIP2018.R
#
# 11-yr analysis of Saipan TMAPS data
# Created 08.12.2019
# Last edited 09.27.2019
#
# Author: J. Saracco
################################################################################

library(jagsUI)
library(reshape2)

################################################################################
# Read in data
################################################################################

ch <- read.csv("dataverse/ch.csv") # individual capture histories
ecov <- read.csv("dataverse/ecov.csv") # effort covariate
int <- read.csv("dataverse/int.csv") # interval covariate"

# write hierarchical JAGS model with station and year effects

sink("pradel_random_sta_yr.txt")
cat("
    data{
    C <- 10000
    zeros  <- 0
    }
    
    model {
    ###########  PRIORS  #######################

    for (s in 1:nsta){
    gamma [s, 1]  <- 0
    phi[s, nyears] <- 0
    for (t in 1:nyears){
        # no  constraints  for mu
    mu[s,t] ~ dunif (0,1) 
    }
    for (t in 1:(nyears-1)){
    # logit  constraint  for  survival  probability (phi)
    phi[s,t] <- 1 / (1 + exp(-logit.phi[s,t])) 
    logit.phi[s,t] <- alpha.phi + beta.phi*int[s,t] + sta.phi[s] + yr.phi[t]
    # log  constraint  for  population  growth  rate (lambda)
    lambda[s,t] <- exp(log.lambda[s,t])
    log.lambda[s,t] <- alpha.lam + sta.lam[s] + yr.lam[t]
    }
    # random station effects
    sta.phi[s] ~ dnorm(0, tau.sta.phi)
    sta.lam[s] ~ dnorm(0, tau.sta.lam)
    }
    
    for (s in 1:4){
    for (t in 1:nyears){
    # logit  constraint  for  detectability (p)
    p[s,t] <- (1 / (1 + exp(-logit.p[s,t])))
    logit.p[s,t] <- alpha.p  + beta.p*ecov[s,t]
    }
    }
    
    for (t in 1:(nyears-1)){
    # logit  constraint  for  detectability (p)
    p[5,t] <- (1 / (1 + exp(-logit.p[5,t])))
    logit.p[5,t] <- alpha.p + beta.p*ecov[5,t] 
    }
    p[5,nyears] <- 0
    
    for (t in 1:8){
    p[6,t] <- (1 / (1 + exp(-logit.p[6,t])))
    logit.p[6,t] <- alpha.p +beta.p*ecov[6,t]
    }
    
    for (t in 9:11){
    p[6,t] <- 0
    }
    
    for (t in 1:(nyears-1)){
    # random year effects
    yr.phi[t] ~ dnorm(0, tau.yr.phi)
    yr.lam[t] ~ dnorm(0, tau.yr.lam)
    }    
    alpha.phi ~ dnorm(0, 0.1)T(-5,5)
    # alpha.phi on prob  scale
    mean.phi  <- 1 / (1 + exp(-alpha.phi))

    tau.sta.phi <- pow(sigma.sta.phi, -2)
    sigma.sta.phi ~ dunif(0, 10)
    tau.yr.phi <- pow(sigma.yr.phi, -2)
    sigma.yr.phi ~ dunif(0, 10)

    alpha.lam ~ dnorm(0, 0.01)
    # alpha.lam on real  scale
    mean.lam  <- exp(alpha.lam)
    # station  random  variation
    tau.sta.lam  <- pow(sigma.sta.lam, -2)
    sigma.sta.lam ~ dunif(0, 10)
    tau.yr.lam <- pow(sigma.yr.lam, -2)
    sigma.yr.lam ~ dunif(0, 10)    

    alpha.p ~ dnorm(0, 0.01)
    # alpha.p on prob  scale
    mean.p <- 1 / (1 + exp(-alpha.p))
    beta.p ~ dnorm(0, 0.01)
    beta.phi ~dnorm(0,0.01)

    ###########  LIKELIHOOD (ZERO -TRICK) ######
    zeros ~ dpois(zero.mean)
    zero.mean  <- -L + C
    L <- sum(l.num[1:nsta, 1:nyears]) - l.denom
    
    for (s in 1:nsta){
    ##### log -likelihood  for the  first  occasion
    l.num[s,1]  <- (u[s,1] * log(xi[s,1])) + (n[s,1] * log(p[s,1])) + (secondexpo [s,1] * log(1-p[s,1])) +
    (thirdexpo [s,1] * log(phi [s,1])) + (fourthexpo [s,1] * log(mu[s,1])) +
    (d[s,1] * log(1-mu[s,1])) + (fifthexpo [s,1] * log(1-(p[s,1]*(1 -mu [s,1])))) +
    (sixthexpo [s,1] * log(chi [s,1]))
    xi[s,1]  <- 1
    secondexpo_a [s,1]  <- sum(u[s,1:1])
    secondexpo_b [s,1]  <- 0
    secondexpo [s,1]  <- secondexpo_a [s,1] - secondexpo_b [s,1] - n[s,1]
    thirdexpo [s,1]  <- sum(v[s,2:nyears])
    fourthexpo [s,1]  <- n[s,1]-d[s,1]
    fifthexpo [s,1]  <- sum(u[s,2:nyears])
    sixthexpo [s,1]  <- v[s,1]-d[s,1]

    ##### log -likelihood  for the  last  occasion
    l.num[s, nyears] <- (u[s, nyears] * log(xi[s, nyears])) + 
      (firstexpo[s, nyears] * (log(phi[s,(nyears-1)]) - log(lambda[s,(nyears-1)]))) +
    (n[s, nyears] * log(p[s,1])) + (secondexpo[s, nyears] * log(1-p[s, nyears])) +
    (fourthexpo[s, nyears] * log(mu[s, nyears])) + (d[s, nyears] * log(1-mu[s, nyears])) +
    (fifthexpo[s, nyears] * log(1-(p[s, nyears]*(1-mu[s, nyears])))) +
    (sixthexpo[s, nyears] * log(chi[s, nyears]))
    chi[s, nyears] <- 1
    firstexpo[s, nyears] <- sum(u[s, 1:(nyears-1)])
    secondexpo_a[s, nyears] <- sum(u[s, 1:nyears])
    secondexpo_b[s, nyears] <- sum(v[s, 1:(nyears -1)])
    secondexpo[s, nyears] <- secondexpo_a[s, nyears] - secondexpo_b[s, nyears] - n[s, nyears]
    fourthexpo[s, nyears] <- n[s, nyears]-d[s, nyears]
    fifthexpo[s, nyears] <- 0
    sixthexpo[s, nyears] <- v[s, nyears]-d[s, nyears]

    #####  likelihood  from  occasion 2 to s-1
    for (i in 2:(nyears-1)) {

    l.num[s,i] <- (u[s,i] * log(xi[s,i])) + (firstexpo[s,i] * (log(phi[s,(i-1)]) - log(lambda[s,(i-1)]))) +
    (n[s,i] * log(p[s,i])) + (secondexpo[s,i] * log(1-p[s,i])) +
    (thirdexpo[s,i] * log(phi[s,i])) + (fourthexpo[s,i] * log(mu[s,i])) +

    (d[s,i] * log(1-mu[s,i])) + (fifthexpo[s,i] * log(1-(p[s,i]*(1-mu[s,i])))) +
    (sixthexpo[s,i] * log(chi[s,i]))
    # first  exponent

    firstexpo[s,i] <- sum(u[s, 1:(i-1)])
    # second  exponent

    secondexpo_a[s,i] <- sum(u[s, 1:i])

    secondexpo_b[s, i] <- sum(v[s, 1:(i -1)])
    secondexpo[s, i] <- secondexpo_a[s, i] - secondexpo_b[s, i] - n[s, i]
    # third  exponent

    thirdexpo[s, i] <- sum(v[s, (i+1):nyears])
    # fourth  exponent

    fourthexpo[s, i] <- n[s, i]-d[s, i]
    # fifth  exponent

    fifthexpo[s, i] <- sum(u[s, (i+1):nyears])
    # sixth  exponent
 
    sixthexpo[s, i] <- v[s, i]-d[s, i]
    }
    #####  likelihood  denominator
    # 1st  product
 
    PROD1 [s, 1]  <- 1
    for (j in 1:(nyears -1)){
    PROD1_tmp [s, 1, j] <- 0
    }

    # fill  part of  PROD1_tmp
    for (i in 2:(nyears-1)) {
    for (j in i:(nyears -1)){
    PROD1_tmp[s, i, j] <- 0
    }
    }
    for (i in 2:nyears) {
    for (j in 1:(i -1)){
    PROD1_tmp[s,i,j] <- phi[s,j] * (1-(p[s,j]*(1-mu[s,j])))
    }
    }
    PROD1 [s, 2]  <- PROD1_tmp [s,2,1]
    for (i in 3:nyears) {
    PROD1[s, i] <- prod(PROD1_tmp[s,i,1:(i-1)])
    }
    # 2nd  product

    PROD2[s, nyears] <- 1

    for (i in 1:(nyears-1)) {
    for (j in (i+1):nyears) {
    PROD2_tmp[s,i,j] <- gamma[s,j]
    }
    }

    # fill  part of  PROD2_tmp
    for (i in 1:(nyears-1)) {
    for (j in 1:i) {
    PROD2_tmp[s,i,j] <- 0
    }
    }

    PROD2[s,nyears-1]  <- PROD2_tmp [s,(nyears-1),nyears]
    for (i in 1:(nyears-2)) {
    PROD2[s,i] <- prod(PROD2_tmp[s, i,(i+1):nyears])
    }
    for (i in 1:nyears) {
    denom_base_tmp[s,i] <- xi[s,i] * PROD1[s,i] * PROD2[s,i] * p[s,i]
    }
    }
    denom_base  <- sum(denom_base_tmp [,])

    denom_expo  <- sum(u[1:nsta,1:nyears])

    l.denom  <- denom_expo * log(denom_base)

    #################  Define  xi and  chi
    for (s in 1:nsta){
    for (i in 2:nyears) {
    xi.tmp[s,i] <- (1-gamma[s,i]) +
    (gamma[s,i] * ((1-p[s,(i-1)])/(1 -(p[s,(i-1)] * (1-mu[s,(i -1)])))) * xi[s,(i-1)])
    xi[s,i] <- max(xi.tmp[s,i] ,0.00001)
    }
    for (i in 1:(nyears-1)) {
    chi[s,i] <- (1-phi[s,i]) + (phi[s,i] * (1-p[s,(i+1)]) * chi[s,(i+1)])
    }
    #################  Gamma  as  derived  parameter
    for (i in 2:nyears) {
    gamma[s,i] <- phi[s,(i-1)] / lambda[s,(i-1)]
    }
    }
    }
    ",fill = TRUE)
sink()

#### Rufous fantail ############################################################

rufa.ch <- ch[ch$SPEC%in%"RUFA",]
CH <- rufa.ch[,4:14]

### summary data needed for  the JAGS models
# e = index  of the  earliest  observation
get.first  <- function(x) min(which(x!=0))
rufa.ch$e <- apply(CH, 1, get.first)
# l = index  of the  last  observation
get.last  <- function(x) max(which(x!=0))
rufa.ch$l <- apply(CH, 1, get.last)
# u = number  of  animals  observed  for the  first  time at i
u <- acast(rufa.ch, STATION ~ e, value.var = "BAND", length)
# n = number  of  animals  observed  at i
nsta <- dim(u)[1]
nyears <- dim(u)[2]
sta <- as.numeric(rufa.ch$STATION)
n <- matrix(NA, nrow = nsta, ncol = nyears)
for (i in 1:nsta){
  n[i,] <- colSums(CH[sta%in%i,])
}
# v = number  of  animals  observed  for the  last  time at i
v <- acast(rufa.ch, STATION ~ l, value.var = "BAND", length)
# d = number  of  animals  removed  from  the  population  at time i
d <- array(0, dim(v))


# Bundle  data
jags.data  <- list(u=u, n=n, v=v, d=d, nsta=nsta, nyears=nyears, ecov=ecov, int = int)

#  Initial  values
inits  <- function (){ list(alpha.phi = runif(1,  0, 1),
                            beta.p = runif(1,  -2, 2),
                            beta.phi = runif(1, -1, 0),
                            alpha.lam = runif(1,  -0.5, 0.5),
                            sigma.sta.lam = runif(1, 0, 1),
                            sigma.sta.phi = runif(1, 0, 1),
                            sigma.yr.phi = runif(1), sigma.yr.lam = runif(1),
                            alpha.p = runif(1,  -0.5, 0.5),
                            mu = matrix(runif(nsta*nyears, 0.9, 1), 
                                        nrow = dim(v)[1], ncol = dim(v)[2])
)}

# Parameters to monitor 
parameters  <- c("phi", "alpha.phi", "mean.phi", "beta.phi", "beta.p", "yr.phi", "yr.lam", 
                 "sigma.yr.phi", "sigma.yr.lam", "gamma", "lambda", "alpha.lam", 
                 "mean.lam", "sta.phi", "sta.lam", "sigma.sta.lam", "sigma.sta.phi", 
                 "p", "alpha.p", "mean.p")

# MCMC  settings
niter  <- 100000;  nthin  <- 10;  nburn  <- 10000;  na= 30000; nchains  <- 3

# Call  JAGS
rufa.phi.lam  <- jags(jags.data , inits=inits , parameters=parameters , "pradel_random_sta_yr.txt", 
                    n.chains = nchains , n.thin = nthin , n.iter = niter , n.adapt = na,
                    n.burnin = nburn, parallel = T)

# to review summary of results
print(rufa.phi.lam)

# to access posteriors of parameters monitored: 
# rufa.phi.lam$sims.list$<parameter>


#### Golden white-eye ##########################################################

gowe.ch <- ch[ch$SPEC%in%"GOWE",]
CH <- gowe.ch[,4:14]
gowe.ch$e <- apply(CH, 1, get.first)
gowe.ch$l <- apply(CH, 1, get.last)
u <- acast(gowe.ch, STATION ~ e, value.var = "BAND", length)
sta <- as.numeric(gowe.ch$STATION)
n <- matrix(NA, nrow = nsta, ncol = nyears)
for (i in 1:nsta){
  n[i,] <- colSums(CH[sta%in%i,])
}
v <- acast(gowe.ch, STATION ~ l, value.var = "BAND", length)

# Bundle  data
jags.data  <- list(u=u, n=n, v=v, d=d, nsta=nsta, nyears=nyears, ecov=ecov, int = int)

# Call  JAGS
gowe.phi.lam  <- jags(jags.data , inits=inits , parameters=parameters , "pradel_random_sta_yr.txt", 
                      n.chains = nchains , n.thin = nthin , n.iter = niter , n.adapt = na,
                      n.burnin = nburn, parallel = T)


# Write space-time constant jags model #########################################

sink("pradel_dot.txt")
cat("
    data{
    C <- 10000
    zeros  <- 0
    }
    
    model {
    ###########  PRIORS  #######################

    for (s in 1:nsta){
    gamma [s, 1]  <- 0
    phi[s, nyears] <- 0
    for (t in 1:nyears){
        # no  constraints  for mu
    mu[s,t] ~ dunif (0,1) 
    }
    for (t in 1:(nyears-1)){
    # logit  constraint  for  survival  probability (phi)
    phi[s,t] <- 1 / (1 + exp(-logit.phi[s,t])) 
    logit.phi[s,t] <- alpha.phi 
    # log  constraint  for  population  growth  rate (lambda)
    lambda[s,t] <- exp(log.lambda[s,t])
    log.lambda[s,t] <- alpha.lam 
    }
    }
    
    for (s in 1:4){
    for (t in 1:nyears){
    # logit  constraint  for  detectability (p)
    p[s,t] <- (1 / (1 + exp(-logit.p[s,t])))
    logit.p[s,t] <- alpha.p  + beta.p*ecov[s,t]
    }
    }
    
    for (t in 1:(nyears-1)){
    # logit  constraint  for  detectability (p)
    p[5,t] <- (1 / (1 + exp(-logit.p[5,t])))
    logit.p[5,t] <- alpha.p + beta.p*ecov[5,t] 
    }
    p[5,nyears] <- 0.00001
    
    for (t in 1:8){
    p[6,t] <- (1 / (1 + exp(-logit.p[6,t])))
    logit.p[6,t] <- alpha.p +beta.p*ecov[6,t]
    }
    
    for (t in 9:11){
    p[6,t] <- 0.00001
    }
    
    alpha.phi ~ dnorm(0, 0.1)T(-5,5)
    # alpha.phi on prob  scale
    mean.phi  <- 1 / (1 + exp(-alpha.phi))

    alpha.lam ~ dnorm(0, 0.01)
    # alpha.lam on real  scale
    mean.lam  <- exp(alpha.lam)

    alpha.p ~ dnorm(0, 0.01)
    # alpha.p on prob  scale
    mean.p <- 1 / (1 + exp(-alpha.p))
    beta.p ~ dnorm(0, 0.01)

    ###########  LIKELIHOOD (ZERO -TRICK) ######
    zeros ~ dpois(zero.mean)
    zero.mean  <- -L + C
    L <- sum(l.num[1:nsta, 1:nyears]) - l.denom
    
    for (s in 1:nsta){
    ##### log -likelihood  for the  first  occasion
    l.num[s,1]  <- (u[s,1] * log(xi[s,1])) + (n[s,1] * log(p[s,1])) + (secondexpo [s,1] * log(1-p[s,1])) +
    (thirdexpo [s,1] * log(phi [s,1])) + (fourthexpo [s,1] * log(mu[s,1])) +
    (d[s,1] * log(1-mu[s,1])) + (fifthexpo [s,1] * log(1-(p[s,1]*(1 -mu [s,1])))) +
    (sixthexpo [s,1] * log(chi [s,1]))
    xi[s,1]  <- 1
    secondexpo_a [s,1]  <- sum(u[s,1:1])
    secondexpo_b [s,1]  <- 0
    secondexpo [s,1]  <- secondexpo_a [s,1] - secondexpo_b [s,1] - n[s,1]
    thirdexpo [s,1]  <- sum(v[s,2:nyears])
    fourthexpo [s,1]  <- n[s,1]-d[s,1]
    fifthexpo [s,1]  <- sum(u[s,2:nyears])
    sixthexpo [s,1]  <- v[s,1]-d[s,1]

    ##### log -likelihood  for the  last  occasion
    l.num[s, nyears] <- (u[s, nyears] * log(xi[s, nyears])) + 
      (firstexpo[s, nyears] * (log(phi[s,(nyears-1)]) - log(lambda[s,(nyears-1)]))) +
    (n[s, nyears] * log(p[s,1])) + (secondexpo[s, nyears] * log(1-p[s, nyears])) +
    (fourthexpo[s, nyears] * log(mu[s, nyears])) + (d[s, nyears] * log(1-mu[s, nyears])) +
    (fifthexpo[s, nyears] * log(1-(p[s, nyears]*(1-mu[s, nyears])))) +
    (sixthexpo[s, nyears] * log(chi[s, nyears]))
    chi[s, nyears] <- 1
    firstexpo[s, nyears] <- sum(u[s, 1:(nyears-1)])
    secondexpo_a[s, nyears] <- sum(u[s, 1:nyears])
    secondexpo_b[s, nyears] <- sum(v[s, 1:(nyears -1)])
    secondexpo[s, nyears] <- secondexpo_a[s, nyears] - secondexpo_b[s, nyears] - n[s, nyears]
    fourthexpo[s, nyears] <- n[s, nyears]-d[s, nyears]
    fifthexpo[s, nyears] <- 0
    sixthexpo[s, nyears] <- v[s, nyears]-d[s, nyears]

    #####  likelihood  from  occasion 2 to s-1
    for (i in 2:(nyears-1)) {

    l.num[s,i] <- (u[s,i] * log(xi[s,i])) + (firstexpo[s,i] * (log(phi[s,(i-1)]) - log(lambda[s,(i-1)]))) +
    (n[s,i] * log(p[s,i])) + (secondexpo[s,i] * log(1-p[s,i])) +
    (thirdexpo[s,i] * log(phi[s,i])) + (fourthexpo[s,i] * log(mu[s,i])) +

    (d[s,i] * log(1-mu[s,i])) + (fifthexpo[s,i] * log(1-(p[s,i]*(1-mu[s,i])))) +
    (sixthexpo[s,i] * log(chi[s,i]))
    # first  exponent

    firstexpo[s,i] <- sum(u[s, 1:(i-1)])
    # second  exponent

    secondexpo_a[s,i] <- sum(u[s, 1:i])

    secondexpo_b[s, i] <- sum(v[s, 1:(i -1)])
    secondexpo[s, i] <- secondexpo_a[s, i] - secondexpo_b[s, i] - n[s, i]
    # third  exponent

    thirdexpo[s, i] <- sum(v[s, (i+1):nyears])
    # fourth  exponent

    fourthexpo[s, i] <- n[s, i]-d[s, i]
    # fifth  exponent

    fifthexpo[s, i] <- sum(u[s, (i+1):nyears])
    # sixth  exponent
 
    sixthexpo[s, i] <- v[s, i]-d[s, i]
    }
    #####  likelihood  denominator
    # 1st  product
 
    PROD1 [s, 1]  <- 1
    for (j in 1:(nyears -1)){
    PROD1_tmp [s, 1, j] <- 0
    }

    # fill  part of  PROD1_tmp
    for (i in 2:(nyears-1)) {
    for (j in i:(nyears -1)){
    PROD1_tmp[s, i, j] <- 0
    }
    }
    for (i in 2:nyears) {
    for (j in 1:(i -1)){
    PROD1_tmp[s,i,j] <- phi[s,j] * (1-(p[s,j]*(1-mu[s,j])))
    }
    }
    PROD1 [s, 2]  <- PROD1_tmp [s,2,1]
    for (i in 3:nyears) {
    PROD1[s, i] <- prod(PROD1_tmp[s,i,1:(i-1)])
    }
    # 2nd  product

    PROD2[s, nyears] <- 1

    for (i in 1:(nyears-1)) {
    for (j in (i+1):nyears) {
    PROD2_tmp[s,i,j] <- gamma[s,j]
    }
    }

    # fill  part of  PROD2_tmp
    for (i in 1:(nyears-1)) {
    for (j in 1:i) {
    PROD2_tmp[s,i,j] <- 0
    }
    }

    PROD2[s,nyears-1]  <- PROD2_tmp [s,(nyears-1),nyears]
    for (i in 1:(nyears-2)) {
    PROD2[s,i] <- prod(PROD2_tmp[s, i,(i+1):nyears])
    }
    for (i in 1:nyears) {
    denom_base_tmp[s,i] <- xi[s,i] * PROD1[s,i] * PROD2[s,i] * p[s,i]
    }
    }
    denom_base  <- sum(denom_base_tmp [,])

    denom_expo  <- sum(u[1:nsta,1:nyears])

    l.denom  <- denom_expo * log(denom_base)

    #################  Define  xi and  chi
    for (s in 1:nsta){
    for (i in 2:nyears) {
    xi.tmp[s,i] <- (1-gamma[s,i]) +
    (gamma[s,i] * ((1-p[s,(i-1)])/(1 -(p[s,(i-1)] * (1-mu[s,(i -1)])))) * xi[s,(i-1)])
    xi[s,i] <- max(xi.tmp[s,i] ,0.00001)
    }
    for (i in 1:(nyears-1)) {
    chi[s,i] <- (1-phi[s,i]) + (phi[s,i] * (1-p[s,(i+1)]) * chi[s,(i+1)])
    }
    #################  Gamma  as  derived  parameter
    for (i in 2:nyears) {
    gamma[s,i] <- phi[s,(i-1)] / lambda[s,(i-1)]
    }
    }
    }
    ",fill = TRUE)
sink()



#### Bridled white-eye ####################################

brwe.ch <- ch[ch$SPEC%in%"BRWE",]
CH <- brwe.ch[,4:14]

brwe.ch$e <- apply(CH, 1, get.first)
brwe.ch$l <- apply(CH, 1, get.last)
u <- acast(brwe.ch, STATION ~ e, value.var = "BAND", length)
nsta <- dim(u)[1]
nyears <- dim(u)[2]
sta <- as.numeric(brwe.ch$STATION)
n <- matrix(NA, nrow = nsta, ncol = nyears)
for (i in 1:nsta){
  n[i,] <- colSums(CH[sta%in%i,])
}
v <- acast(brwe.ch, STATION ~ l, value.var = "BAND", length)

# Bundle  data
jags.data  <- list(u=u, n=n, v=v, d=d, nsta=nsta, nyears=nyears, ecov=ecov)

# Call  JAGS
brwe.phi.lam  <- jags(jags.data , inits=inits , parameters=parameters , "pradel_dot.txt", 
                      n.chains = nchains , n.thin = nthin , n.iter = niter , n.adapt = na,
                      n.burnin = nburn, parallel = T)


#### Marianas kingfisher (previously Micronesian kingfisher) ###################

mick.ch <- ch[ch$SPEC%in%"MICK",]
CH <- mick.ch[,4:14]
mick.ch$e <- apply(CH, 1, get.first)
mick.ch$l <- apply(CH, 1, get.last)
u <- acast(mick.ch, STATION ~ e, value.var = "BAND", length)
nsta <- dim(u)[1]
nyears <- dim(u)[2]
sta <- as.numeric(mick.ch$STATION)
n <- matrix(NA, nrow = nsta, ncol = nyears)
for (i in 1:nsta){
  n[i,] <- colSums(CH[sta%in%i,])
}
v <- acast(mick.ch, STATION ~ l, value.var = "BAND", length)
d <- array(0, dim(v))

# Bundle  data
jags.data  <- list(u=u, n=n, v=v, d=d, nsta=nsta, nyears=nyears, ecov=ecov)

# Call  JAGS
mick.phi.lam  <- jags(jags.data , inits=inits , parameters=parameters , "pradel_dot.txt", 
                      n.chains = nchains , n.thin = nthin , n.iter = niter , n.adapt = na,
                      n.burnin = nburn, parallel = T)


#### Micronesian myzomela (previously Micronesian honeyeater) ##################

miho.ch <- ch[ch$SPEC%in%"MIHO",]
CH <- miho.ch[,4:14]
miho.ch$e <- apply(CH, 1, get.first)
miho.ch$l <- apply(CH, 1, get.last)
u <- acast(miho.ch, STATION ~ e, value.var = "BAND", length)
nsta <- dim(u)[1]
nyears <- dim(u)[2]
sta <- as.numeric(miho.ch$STATION)
n <- matrix(NA, nrow = nsta, ncol = nyears)
for (i in 1:nsta){
  n[i,] <- colSums(CH[sta%in%i,])
}
v <- acast(miho.ch, STATION ~ l, value.var = "BAND", length)

# Bundle  data
jags.data  <- list(u=u, n=n, v=v, d=d, nsta=nsta, nyears=nyears, ecov=ecov)

# Call  JAGS
miho.phi.lam  <- jags(jags.data , inits=inits , parameters=parameters , "pradel_dot.txt", 
                      n.chains = nchains , n.thin = nthin , n.iter = niter , n.adapt = na,
                      n.burnin = nburn, parallel = T)

