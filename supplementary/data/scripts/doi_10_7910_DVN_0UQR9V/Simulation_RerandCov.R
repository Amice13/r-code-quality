##########################################################################
### This code implements the simulations in Example 1 and Section 9.1 ####
##########################################################################

### read the R functions
source("Function_RerandCov.R")

##########################################################################
##### generate the covariate X and the assignments from ReM ##############
##########################################################################
### sample size 
n = 100
m = n/2
X = matrix(rnorm(n), nrow = n, ncol = 1)
eta = rnorm(n)
delta = rnorm(n)
tau = 1
### number of assignments from ReM
nperm = 10^5
pa=0.001
Z_rem = assign_ReM(n, m, X, pa=pa, nperm)

##########################################################################
######## generate the covariate W and potential outcomes #################
##########################################################################

### choice of rho: 0 or 0.9
rho = 0

data = list(rho = rho, X = X)
data$W = X + eta
data$Y0 = 2*X + rho * eta + sqrt(1-rho^2) * delta
data$Y1 = tau + data$Y0

### input data
rho = data$rho
X = data$X
W = data$W
Y0 = data$Y0
Y1 = data$Y1


##########################################################################
#### estimators and variance estimators over simulated assignments #######
##########################################################################

### point and varianc estimators
tau_diff = rep(NA, nperm)
var_diff = rep(NA, nperm)

tau_adj = rep(NA, nperm)
var_adj = rep(NA, nperm)

tau_adj_XW = rep(NA, nperm)
var_adj_XW = rep(NA, nperm)

for(iter in 1:nperm){
  Z = Z_rem[iter,]
  Y = Y1 * Z + Y0 * (1-Z)
  
  tau_diff[iter] = diff_est(Z, Y)
  var_diff[iter] = var_diff_est(Z, Y, W)
  
  tau_adj[iter] = reg_adj_est(Z, Y, W)
  var_adj[iter] = var_adj_est(Z, Y, W)
  
  tau_adj_XW[iter] = reg_adj_est(Z, Y, cbind(X, W))
  var_adj_XW[iter] = var_adj_est(Z, Y, cbind(X, W))
  
}

##########################################################################
####### histograms and asymptotic approximations of the estimators #######
##########################################################################

### asymptotic approximations
K = ncol(X)
v.Ka = pchisq(qchisq(pa, df=K), df = K+2)/pa

# difference-in-means
Vtt = as.numeric( n* (var(Y1)/m + var(Y0)/(n-m) - var(Y1-Y0)/n) )
R2tx = as.numeric( cor(Y0, X)^2 )

# adjusted estimator using x
Vtt_adj = n* (var(lm(Y1~W)$residuals)/m + var(lm(Y0~W)$residuals)/(n-m) - var(lm(Y1~W)$residuals-lm(Y0~W)$residuals)/n)
R2tx_adj = as.numeric( cor(lm(Y0~W)$residuals, X)^2 )

# adjusted estimator using (x,w)
Vtt_adj_XW = n* (var(lm(Y1~cbind(X,W))$residuals)/m + var(lm(Y0~cbind(X,W))$residuals)/(n-m) - var(lm(Y1~cbind(X,W))$residuals-lm(Y0~cbind(X,W))$residuals)/n)
R2tx_adj_XW = as.numeric( cor(lm(Y0~cbind(X,W))$residuals, X)^2 )

### bias, mse, and asymptotic variance
mean(tau_diff - tau)
n * mean((tau_diff - tau)^2)
Vtt*(1 - (1-v.Ka)*R2tx)

mean(tau_adj - tau)
n * mean((tau_adj - tau)^2)
Vtt_adj*(1 - (1-v.Ka)*R2tx_adj)

mean(tau_adj_XW - tau)
n * mean((tau_adj_XW - tau)^2)
Vtt_adj_XW*(1 - (1-v.Ka)*R2tx_adj_XW)

### histogram of difference-in-means and adjusted estimator using (x,w)
hist(sqrt(n)*(tau_diff-tau), breaks = 30, freq = FALSE, col = "grey", border=FALSE, xlim=c(-10,10), ylim=c(0, 0.25), main=NULL, xlab = expression(paste(sqrt(n), "(", hat(tau)-tau, ")", " or ", sqrt(n), "{", hat(tau), "(", tilde(beta)[1], ",", tilde(beta)[0], ")",-tau, "}")) )
lines( density( sqrt(Vtt)*sample_std_rerand(10^7, R2=R2tx, K=1, pa = 0.001) ), lty=2)
hist(sqrt(n)*(tau_adj-tau), breaks = 30, freq = FALSE, add = TRUE)
lines( density( sqrt(Vtt_adj)*sample_std_rerand(10^7, R2=R2tx_adj, K=1, pa = 0.001) ), lty=1)
legend("topright", legend = c("adjusted", "unadjusted", "adjusted", "unadjusted"), pch=c(0,15,NA,NA), pt.cex=3, col=c("black", "grey","black","black"),lty = c(0, 0, 1, 2), cex=2)


##########################################################################
####### coverage probability of the Wald-type confidence intervals #######
##########################################################################

### calculate coverage probability
diff_lower = tau_diff - sqrt(var_diff/n) * qnorm(0.975)
diff_upper = tau_diff + sqrt(var_diff/n) * qnorm(0.975)
coverage_diff = mean( (tau - diff_lower)*(diff_upper - tau) >= 0 )

adj_lower = tau_adj - sqrt(var_adj/n) * qnorm(0.975)
adj_upper = tau_adj + sqrt(var_adj/n) * qnorm(0.975)
coverage_adj = mean( (tau - adj_lower)*(adj_upper - tau) >= 0 )

adj_lower_XW = tau_adj_XW - sqrt(var_adj_XW/n) * qnorm(0.975)
adj_upper_XW = tau_adj_XW + sqrt(var_adj_XW/n) * qnorm(0.975)
coverage_adj_XW = mean( (tau - adj_lower_XW)*(adj_upper_XW - tau) >= 0 )


