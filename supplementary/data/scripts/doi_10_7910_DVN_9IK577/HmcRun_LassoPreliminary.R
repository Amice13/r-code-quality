######################################################################
# Preliminary script for running Hamiltonian Monte Carlo coalscence
#   code for the Bayesian Lasso, using the Full Random Uniform
#   Trajectory Sampler (FRUTS)
# Author: George Leigh
# Copyright 2020 George Leigh
# Associated journal paper: Leigh, G. M. and Northrop, A. R. (2020).
#   "Hamiltonian Monte Carlo and coalescence towards perfect
#   simulation of continuous distributions".
# Citation of that journal paper in any publication that uses this
#   algorithm would be greatly appreciated.

# This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published
#   by the Free Software Foundation, either version 2 of the License,
#   or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
# You should have received a copy of the GNU General Public License
#   along with this program.  If not, see
#   <https://www.gnu.org/licenses/>.

# This script is intended to be called using the "source" function in R.

source("HmcRun_Preliminary.R")

######################################################################
# LU decomposition routine for a symmetric, positive definite,
#   non-sparse matrix.  Library routines don't seem worth the trouble.
#   Here U = t(L), L is lower triangular and U is upper triangular.
# This seems to be the easiest way to scale a vector to have variance
#   matrix equal to the identity matrix.
# We don't bother to check that the matrix is indeed symmetric and
#   positive definite.
# Do in place: overwrite x by L.
LUdecomp = function(x) {
 d = dim(x)[1]
 x[1, 1] = sqrt(x[1, 1])
 if (d > 1) {
  x[2:d, 1] = x[2:d, 1] / x[1, 1]
  for (j in 2:d) {
   x[j, j] = sqrt(x[j, j] - sum(x[j, 1:(j - 1)]^2))
   if (j < d) # Fill in the remainder of column j.
    for (i in (j + 1):d)
     x[i, j] = (x[i, j] - sum(x[j, 1:(j - 1)] * x[i, 1:(j - 1)])) / x[j, j]
  }
  for (i in 1:(d - 1)) # Put zeros in the upper triangle.
   x[i, (i + 1):d] = 0
 }
 x
}
#Upper triangle: comment out; we want the lower one for de-scaling.
#LUdecomp = function(x) {
# d = dim(x)[1]
# x[1, 1] = sqrt(x[1, 1])
# if (d > 1) {
#  x[1, 2:d] = x[1, 2:d] / x[1, 1]
#  for (i in 2:d) {
#   x[i, i] = sqrt(x[i, i] - sum(x[1:(i - 1), i]^2))
#   if (i < d)
#    for (j in (i + 1):d)
#     x[i, j] = (x[i, j] - sum(x[1:(i - 1), i] * x[1:(i - 1), j])) / x[i, i]
#  }
#  for (j in 1:(d - 1))
#   x[(j + 1):d, j] = 0
# }
# x
#}

######################################################################
# Bayesian Lasso with diabetes data from Efron et al. (2004)
# The Lasso parameter Llambda is assumed fixed.  Estimating it is
#   beyond the scope of this example.  (Prefix "L" stands for "Lasso",
#   just to make sure that variable names don't get confused.)

######################################## Setup
# Note that in most instances this code uses the parameter tau = 1 /
#   sigma instead of sigma.  The parameter used in HMC is log(tau).
diab = read.table("diabetes.data", header = TRUE)
Xdiab = data.frame(Int = 1, diab[, -match("Y", names(diab))])
LassoY = diab$Y
ndiab = length(LassoY)
pdiab = sum(names(Xdiab) != "Int") # Omit the constant from the list
#   of parameters subject to the Lasso.
# Standardise each x variable to have mean zero and variance 1.  We'll
#   accept R's default to divide by n - 1 rather than n in calculating
#   the variance.  It doesn't really matter which we use; the
#   important thing is to standardise them all in the same way.
f = function(x) (x - mean(x)) / sqrt(var(x))
IndBeta = names(Xdiab)[names(Xdiab) != "Int"] # Remove intercept.
for (i in IndBeta)
 Xdiab[, i] = f(Xdiab[, i])
LassoX = as.matrix(Xdiab) # Design matrix
Hessian = t(LassoX) %*% LassoX # Without scale factor of tau^2 = 1 / sigma^2
betahat = solve(Hessian, t(LassoX) %*% LassoY) # Ordinary least squares (OLS)
#   estimates of the beta parameters
varbetahat = solve(Hessian, diag(dim(Hessian)[1])) # Estimated
#   variance of OLS estimators; lacks factor sigma^2 = 1 / tau^2.
#   Note that this doesn't change with the addition of the Lasso term.
dimnames(varbetahat)[[2]] = dimnames(varbetahat)[[1]]
corbetahat = varbetahat # Correlations
for (i in 1:(pdiab + 1))
 corbetahat[i,] = corbetahat[i,] / sqrt(varbetahat[i, i])
for (j in 1:(pdiab + 1))
 corbetahat[, j] = corbetahat[, j] / sqrt(varbetahat[j, j])
#round(corbetahat, 3) # betahats for S1, S2, S3 and S5 are quite highly
#   correlated with each other.  Also the betahats for S3 and S4 are
#   moderately highly correlated.
# It looks desirable to adjust for the correlations, not merely adjust
#   each beta parameter separately.
Fitted = LassoX %*% betahat # OLS fitted values 
Shat = sum((LassoY - Fitted)^2) # OLS residual sum of squares

# We presume that the intercept parameter is not part of the Lasso
#   formulation.  Nevertheless, it should be part of the Bayesian
#   sampling and should not be standardised out.  Efron et al. 2004
#   imply that they standardised it out by adjusting y to have mean
#   zero and omitting the intercept from the model, which in my
#   opinion is incorrect.  The other published papers don't seem to
#   say what they did.
That = sum(abs(betahat[IndBeta,]))
LassoDf = ndiab + pdiab # Degrees of freedom: subtract 1 if using tau
#   as a model parameter but not if using log(tau).
sigmahat = sqrt(Shat / LassoDf) # OLS estimate with a few extra
#   degrees of freedom

# Value for Llambda is 0.237 from Park and Casella, but note that
#   their prior for rho in their eq. (A.2) is wrong, and they don't
#   say how (or indeed whether) they standardised their x variables.
#   The correct uninformative prior for rho is 1 / rho.
# This value appears to be much too low.

# Some theory:
# cv(sigmahat) = 1 / sqrt(2 * nu); nu = LassoDf; lambda = Llambda.
# tauhat * sigmahat ~ 1 - lambda * T / (2 * nu * sigmahat).
# Value of lambda to make tauhat differ from sigmahat by one s.d.:
# lambda = sqrt(2 * nu) * sigmahat / T = sqrt(2 * S) / T = 9.65.
# This is a very long way from 0.237!  I don't know where that
#   estimate could have come from.
# Trial values of lambda: 0, 10, 30, 100, being roughly 0, 1, 3 and 10
#   standard deviations difference in sigmathat.

# Define a wrapper for the generic Hmc function which sets some global
#   variables used in the Bayesian Lasso calculation.
LHmc = function(alpha = 2, Llambda = 1, d = 12, npast = 50,
  beta_in = rep(2, maxpast), h = 0.05, w = 0.01) {
 bquaddiab <<- Llambda * That # "b" parameter in quadratic formula for tauhat
 tauhat <<- 2 * LassoDf / (bquaddiab + sqrt(bquaddiab^2 + 4 * LassoDf * Shat))
 # tauhat is the approximate maximum likelihood estimate of tau = 1 /
 #   sigma.  It is approximate because we are using the OLS estimate
 #   of beta instead of the maximum likelihood estimate.  It should be
 #   good enough for our purposes of scaling the coordinates.  For
 #   scaling I'm more worried about the correlations and getting rid
 #   of them.
 cvtauhat <<- 1 / sqrt(LassoDf + Shat * tauhat^2) # Coefficient of variation

 Ldiab <<- LUdecomp(varbetahat) / tauhat # Include factor of sigma = 1/tau here.
 # If u ~ N(0, I), Ldiab %*% u ~ N(0, varbetahat).  This is how we
 #   convert back from the coordinates q used in HMC to the actual
 #   coordinates beta, which we need to do frequently to apply the
 #   likelihood formula.
 # We need the inverse of Ldiab to begin; i.e., to convert the initial
 #   estimates to HMC coordinates, which we only need to do once.
 Ldiabinv <<- solve(Ldiab, diag(dim(Ldiab)[1]))
 Hmc(UfuncLasso, UderivLasso, Llambda = Llambda, d = d, npast = npast,
  beta_in = beta_in, h = h, w = w)
}

# Also define a wrapper for the generic Rocftp function to sets some
#   global variables used in the Bayesian Lasso calculation.
LRocftp = function(alpha = 2, Llambda = 1, d = 12, npast = 50,
  beta_in = rep(2, npast), h = 0.05, w = 0.01, nBlockAcc = 10) {
 bquaddiab <<- Llambda * That # "b" parameter in quadratic formula for tauhat
 tauhat <<- 2 * LassoDf / (bquaddiab + sqrt(bquaddiab^2 + 4 * LassoDf * Shat))
 # tauhat is the approximate maximum likelihood estimate of tau = 1 /
 #   sigma.  It is approximate because we are using the OLS estimate
 #   of beta instead of the maximum likelihood estimate.  It should be
 #   good enough for our purposes of scaling the coordinates.  For
 #   scaling I'm more worried about the correlations and getting rid
 #   of them.
 cvtauhat <<- 1 / sqrt(LassoDf + Shat * tauhat^2) # Coefficient of variation

 Ldiab <<- LUdecomp(varbetahat) / tauhat # Include factor of sigma = 1/tau here.
 # If u ~ N(0, I), Ldiab %*% u ~ N(0, varbetahat).  This is how we
 #   convert back from the coordinates q used in HMC to the actual
 #   coordinates beta, which we need to do frequently to apply the
 #   likelihood formula.
 # We need the inverse of Ldiab to begin; i.e., to convert the initial
 #   estimates to HMC coordinates, which we only need to do once.
 Ldiabinv <<- solve(Ldiab, diag(dim(Ldiab)[1]))
 Rocftp(UfuncLasso, UderivLasso, Llambda = Llambda, d = d, npast = npast,
  beta_in = beta_in, h = h, w = w, nBlockAcc = nBlockAcc)
}

# Define list to hold the final trajectory from each starting point in
#   each run.  We could usually comment out the following line, but it
#   does not harm to leave it in.
trajsto = list()

ShowTrajsto = function(nstart, Llambda, trajsto, irun, istart) {
 qmat = trajsto[[(irun - 1) * nstart + istart]] # Matrix
 d = dim(qmat)[2]
 d1 = d - 1
 npoint = dim(qmat)[1]
 Lbeta = array(0, dim = c(npoint, d1))
 Ltau = rep(0, npoint)
 Ssum = rep(0, npoint)
 Tsum = rep(0, npoint)
 for (i in 1:npoint) {
  Lbeta[i,] = c(betahat + Ldiab %*% cbind(qmat[i, 1:d1])) # De-scale
  #   Lasso beta (regression coefficients)
  Ltau[i] = tauhat * exp(cvtauhat * qmat[i, d])
  Ssum[i] = sum((LassoY - LassoX %*% Lbeta[i,])^2)
  Tsum[i] = sum(abs(Lbeta[i, 2:d1])) # Omit the intercept coefficient.
 }
 x = cbind(Lbeta, Ltau, Ssum, Tsum)
 dimnames(x)[[2]][1:d1] = dimnames(betahat)[[1]]
 x
}
