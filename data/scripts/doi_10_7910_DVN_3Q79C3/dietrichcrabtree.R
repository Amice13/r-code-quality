###############
### Setup R ###
###############

### Clear terminal
cat("\014")

### Clear space
rm(list = ls())

## This is a slightly modified version of Michal Kolesar's BM_StandardErrors.R
## The original file was downloaded from:
##   https://github.com/kolesarm/Robust-Small-Sample-Standard-Errors
##   (Latest commit 2a80873 on Aug 26, 2015)
## We made minor changes for efficiency, as well as two changes that affect the
## code's functionality:
## 1) MatSqrtInverse() now stops with an error message if its argument is not of
##    full rank:
##    "Bell-McCaffrey SE undefined. This happens, e.g., when a dummy regressor is 1
##     for one cluster and 0 otherwise."
## 2) The list returned by BMlmSE() now includes "se".

# The MIT License (MIT)
# 
# Copyright (c) 2015 Michal Kolesar
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

## Compute Bell-McCaffrey Standard Errors
library(sandwich)
library(Matrix)

message1 <- paste0(
  'Bell-McCaffrey SE undefined. This happens, e.g., when a dummy regressor is 1 ',
  'for one cluster and 0 otherwise.'
)

MatSqrtInverse <- function(A) {
  ##  Compute the inverse square root of a matrix
  if (rankMatrix(A) < NROW(A)) stop(message1)
  ei <- eigen(A, symmetric = TRUE)
  d2 <- 1/sqrt(ei$values)
  ## diag(d2) is d2 x d2 identity if d2 is scalar, instead we want 1x1 matrix
  ei$vectors %*% (if (length(d2)==1) d2 else diag(d2)) %*% t(ei$vectors)
}

BMlmSE <- function(model, clustervar=NULL, ell=NULL, IK=TRUE) {
  X <- model.matrix(model)
  sum.model <- summary.lm(model)
  n <- sum(sum.model$df[1:2])
  K <- model$rank
  XXinv <- sum.model$cov.unscaled # XX^{-1}
  u <- residuals(model)
  
  df <- function(GG) {                # Compute DoF given G'*Omega*G
    sum(diag(GG))^2 / sum(GG * GG)
  }
  
  if(is.null(clustervar)) {           # no clustering
    Vhat <- vcovHC(model, type="HC2")
    Vhat.Stata <- Vhat*NA
    
    M <- diag(n)-X %*% XXinv %*% t(X)       # annihilator matrix
    GOG <- function(ell) {           # G'*Omega*G
      Xtilde <- drop(X %*% XXinv %*% ell / sqrt(diag(M)))
      crossprod(M * Xtilde)
    }
  } else {
    if(!is.factor(clustervar)) stop("'clustervar' must be a factor")
    
    ## Stata
    S <- length(levels(clustervar)) # number clusters
    uj <- apply(u*X, 2, function(x) tapply(x, clustervar, sum))
    Vhat.Stata <- S/(S-1) * (n-1)/(n-K) * sandwich(model, meat = crossprod(uj)/n)
    
    ## LZ2
    tXs <- function(s) {
      Xs <- X[clustervar==s, , drop=FALSE]
      MatSqrtInverse(diag(NROW(Xs))-Xs%*% XXinv %*% t(Xs)) %*% Xs
    }
    tX <- lapply(levels(clustervar), tXs) # list of matrices
    
    tu <- split(u, clustervar)
    tutX <- sapply(seq_along(tu),function(i) crossprod(tu[[i]],tX[[i]]))
    Vhat <- sandwich(model, meat = tcrossprod(tutX)/n)
    
    ## DOF adjustment
    tHs <- function(s) {
      Xs <- X[clustervar==s, , drop=FALSE]
      index <- which(clustervar==s)
      ss <- outer(rep(0,n),index)     # n x ns matrix of 0
      ss[cbind(index,1:length(index))] <- 1
      ss-X %*% XXinv %*% t(Xs)
    }
    tH <- lapply(levels(clustervar), tHs) # list of matrices
    
    Moulton <- function() {
      ## Moulton estimates
      ns <- tapply(u, clustervar,length)
      ssr <- sum(u^2)
      rho <- max((sum(sapply(seq_along(tu), function(i)
        sum(tu[[i]] %o% tu[[i]])))-ssr) / (sum(ns^2)-n), 0)
      c(sig.eps=max(ssr/n - rho, 0), rho=rho)
    }
    
    GOG <- function(ell) {
      G <- sapply(seq_along(tX),
                  function(i)  tH[[i]] %*% tX[[i]] %*% XXinv %*% ell)
      GG <- crossprod(G)
      
      if (IK==TRUE) {            # IK method
        Gsums <- apply(G, 2, function(x) tapply(x, clustervar, sum)) # Z'*G
        GG <- Moulton()[1]*GG + Moulton()[2]*crossprod(Gsums)
      }
      GG
    }
  }
  
  if (!is.null(ell)) {
    se <- drop(sqrt(crossprod(ell,Vhat) %*% ell))
    dof <- df(GOG(ell))
    se.Stata <- drop(sqrt(crossprod(ell,Vhat.Stata) %*% ell))
  } else {
    se <- sqrt(diag(Vhat))
    dof <- sapply(seq(K), function(k) df(GOG(diag(K)[,k])))
    se.Stata <- sqrt(diag(Vhat.Stata))
  }
  names(dof) <- names(se)
  
  return(list(vcov=Vhat, dof=dof, adj.se=se*qt(0.975,df=dof)/qnorm(0.975),
              se=se,
              se.Stata=se.Stata))
}

##################
### Setup Data ###
##################

### Set working directory
setwd("~/Dropbox/dietrich-crabtree/")

### Load data and create state indicator
dat <- rio::import("dietrichcrabtree.csv")
colnames(dat)
rownames(dat) <- dat$V1
dat$V1 <- NULL
dim(dat)
head(dat)

################
### Analysis ###
################

### Omnibus chi-sq test
chisq.test(xtabs(~dat$risk + dat$increased_support)) 
chisq.test(xtabs(~dat$very_severe + dat$increased_support)) 
chisq.test(xtabs(~dat$distant + dat$increased_support)) 

### Estimate treatment effects
ols.fit <- lm(dat$increased_support ~ as.factor(dat$risk) + as.factor(dat$very_severe)
              + as.factor(dat$distant), singular.ok = TRUE)
estimates <- matrix(nrow = length(ols.fit$coefficients)-1, ncol = 4)
estimates[,1] <- as.vector(names(ols.fit$coefficients)[-1])
bm <- BMlmSE(model = ols.fit)
point.estimates <- coef(ols.fit)[2:5]
critical.value <- qt(0.975, df = bm$dof)
margin.of.error <- critical.value * bm$se
point.estimates  # Estimated average treatment effect
bm$se           # HC2 robust SE
bm$se <- bm$se[2:5]
estimates[,2:4] <- c(point.estimates, bm$se, point.estimates/bm$se)
estimates

### Plot estimates
par(mfrow=c(1,1))
var.names <- rev(c("High Risk", "Very High Risk", "Very Severe", "State"))
par(
  oma = c(0,0,0,0), # Since it is a single plot, I set the outer margins to zero.
  mar = c(5,4,5,4) # margins adjusted to reflect changing the locations of the labels
)
# create an empty plot for total customization
plot(NULL, # create empty plot
     xlim = c(-.1, .1), # set xlim by guessing
     ylim = c(1, length(var.names)), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA) 
abline(v=0, lty=3)
est <- as.numeric(rev(estimates[,2])) # conveniently store the estimates (minus the constant)
se <- as.numeric(rev(estimates[,3])) # conveniently store the std. errors (minus the constant)
for (i in 1:length(est)) { # loop over a counter the length of the estimate vector
  lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd = 3, col="grey60")
  lines(c(est[i] + 1.64*se[i], est[i] - 1.64*se[i]), c(i, i), lwd = 7, col="grey30")
  points(est[i], i, pch = 19, cex = 4, col="white") 
  points(est[i], i, pch = 19, cex = 3, col="black") 
  text(est[i], i, var.names[i], xpd = T, cex = .8, pos = 3, offset=1) # add variable labels above the points
}
axis(side = 1) # add bottom axis
mtext(side = 1, "Estimated Treatment Effects", line = 2.5) 

### Make a table of estimates
x <- cbind(estimates[,1],round(as.numeric(estimates[,2]),3),round(as.numeric(estimates[,3]),3))
stargazer::stargazer(x)

### Permutation test
library(randomizr)
library(sandwich)
set.seed(1234567)

#### Duplicate data
dat.sim <- dat

est.ate.risk1 <- coef(ols.fit)['as.factor(dat$risk)1']  # Estimated average treatment effect
se.risk1 <- sqrt( vcovHC(ols.fit, type = 'HC2')['as.factor(dat$risk)1','as.factor(dat$risk)1'] )  # HC2 robust SE
t.observed.risk1 <- est.ate.risk1 / se.risk1

est.ate.risk2 <- coef(ols.fit)['as.factor(dat$risk)2']  # Estimated average treatment effect
se.risk2 <- sqrt( vcovHC(ols.fit, type = 'HC2')['as.factor(dat$risk)2','as.factor(dat$risk)2'] )  # HC2 robust SE
t.observed.risk2 <- est.ate.risk2 / se.risk2

est.ate.sev1 <- coef(ols.fit)['as.factor(dat$very_severe)1']  # Estimated average treatment effect
se.sev1 <- sqrt( vcovHC(ols.fit, type = 'HC2')['as.factor(dat$very_severe)1','as.factor(dat$very_severe)1'] )  # HC2 robust SE
t.observed.sev1 <- est.ate.sev1 / se.sev1

est.ate.dist1 <- coef(ols.fit)['as.factor(dat$distant)1']  # Estimated average treatment effect
se.dist1 <- sqrt( vcovHC(ols.fit, type = 'HC2')['as.factor(dat$distant)1','as.factor(dat$distant)1'] )  # HC2 robust SE
t.observed.dist1 <- est.ate.dist1 / se.dist1

n.rands <- 10000

t.sim.risk1 <- rep(NA, n.rands)
t.sim.risk2 <- rep(NA, n.rands)
t.sim.sev1 <- rep(NA, n.rands)
t.sim.dist1 <- rep(NA, n.rands)

for (i in 1:n.rands) {
  ## Simulate random assignment of treatment
  dat.sim$very_severe <- complete_ra(nrow(dat.sim),nrow(dat.sim)/2)
  dat.sim$risk <- complete_ra(nrow(dat.sim),num_arms=3,condition_names=c(0,1,2))
  dat.sim$distant <- complete_ra(nrow(dat.sim),nrow(dat.sim)/2)
  
  ## Run the pre-specified regression with the simulated data.
  ## Save the simulated t-statistic for ATE.
  fit.sim <- lm(dat.sim$increased_support ~ as.factor(dat.sim$risk) + as.factor(dat.sim$very_severe)
                + as.factor(dat.sim$distant), singular.ok = TRUE)
  sim.ate.risk1 <- coef(fit.sim)['as.factor(dat.sim$risk)1']  # Estimated average treatment effect
  sim.se.risk1 <- sqrt( vcovHC(fit.sim, type = 'HC2')['as.factor(dat.sim$risk)1','as.factor(dat.sim$risk)1'] )  # HC2 robust SE
  t.sim.risk1[i] <- sim.ate.risk1 / sim.se.risk1
  
  sim.ate.risk2 <- coef(fit.sim)['as.factor(dat.sim$risk)2']  # Estimated average treatment effect
  sim.se.risk2 <- sqrt( vcovHC(fit.sim, type = 'HC2')['as.factor(dat.sim$risk)2','as.factor(dat.sim$risk)2'] )  # HC2 robust SE
  t.sim.risk2[i] <- sim.ate.risk2 / sim.se.risk2
  
  sim.ate.sev1 <- coef(fit.sim)['as.factor(dat.sim$very_severe)1']  # Estimated average treatment effect
  sim.se.sev1 <- sqrt( vcovHC(fit.sim, type = 'HC2')['as.factor(dat.sim$very_severe)1','as.factor(dat.sim$very_severe)1'] )  # HC2 robust SE
  t.sim.sev1[i] <- sim.ate.sev1 / sim.se.sev1
  
  sim.ate.dist1 <- coef(fit.sim)['as.factor(dat.sim$distant)1']  # Estimated average treatment effect
  sim.se.dist1 <- sqrt( vcovHC(fit.sim, type = 'HC2')['as.factor(dat.sim$distant)1','as.factor(dat.sim$distant)1'] )  # HC2 robust SE
  t.sim.dist1[i] <- sim.ate.dist1 / sim.se.dist1
  
  print(i)
}

## "In general, if you want a two-sided P-value, compute both one-sided P-values, 
##  double the smaller one, and take the minimum of this value and 1."
##    Rosenbaum (2010), Design of Observational Studies, p. 33, note 2
## (Other options exist, but this is our default.)

p.left.risk1  <- mean(t.sim.risk1 <= t.observed.risk1)
p.right.risk1 <- mean(t.sim.risk1 >= t.observed.risk1)
p.value.risk1 <- min(2 * min(p.left.risk1, p.right.risk1), 1)
p.value.risk1

p.left.risk2  <- mean(t.sim.risk2 <= t.observed.risk2)
p.right.risk2 <- mean(t.sim.risk2 >= t.observed.risk2)
p.value.risk2 <- min(2 * min(p.left.risk2, p.right.risk2), 1)
p.value.risk2

p.left.sev1  <- mean(t.sim.sev1 <= t.observed.sev1)
p.right.sev1 <- mean(t.sim.sev1 >= t.observed.sev1)
p.value.sev1 <- min(2 * min(p.left.sev1, p.right.sev1), 1)
p.value.sev1

p.left.dist1  <- mean(t.sim.dist1 <= t.observed.dist1)
p.right.dist1 <- mean(t.sim.dist1 >= t.observed.dist1)
p.value.dist1 <- min(2 * min(p.left.dist1, p.right.dist1), 1)
p.value.dist1

### Robustness checks reported in appendix
# (1)
summary(ols.fit)

# (2)
logit.estimates <- glm(dat$increased_support ~ as.factor(dat$risk) + as.factor(dat$very_severe)
                       + as.factor(dat$distant),family=binomial)
summary(logit.estimates)

# (3)
probit.estimates <- glm(dat$increased_support ~ as.factor(dat$risk) + as.factor(dat$very_severe)
                        + as.factor(dat$distant),family=binomial(link="probit"))
summary(probit.estimates)

## Table
stargazer::stargazer(ols.fit, logit.estimates, probit.estimates)
