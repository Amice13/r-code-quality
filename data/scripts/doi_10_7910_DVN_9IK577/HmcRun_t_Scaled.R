######################################################################
# R script for running Hamiltonian Monte Carlo coalscence code for the
#   scaled multivariate t-distribution, up to 100 dimensions, using
#   the Full Random Uniform Trajectory Sampler (FRUTS)
# By "scaled" we mean that the parameter vector is scaled by the
#   square-root of the Hessian matrix at the origin
#   (maximum-likelihood point).
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

# This script is not intended to be run automatically (e.g., using the
#   "source" function of R).  Rather, it is intended to be run
#   interactively, one line at a time.

source("HmcRun_Preliminary.R")

######################################## Multivariate t-distribution,
#   scaled by square-root of Hessian at the maximum-likelihood point
# Parameter nu = degrees of freedom; small value (e.g., 4) =
#   long-tailed distribution.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 39)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 38)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 37)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 36)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 35)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 34)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 33)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 32)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 31)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 30)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 29) #*
# Mostly about 20 points per trajectory, sometimes around 50
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 28) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 27)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 26)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 25)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 24)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 23) #X1*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 22) #XX4
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 21) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, npast = 20) #XX3
Rocftp(Ufunc = Ufunc_tscal, Uderiv = Uderiv_tscal, d = 1, nu = 4, npast = 23,
 nBlockAcc = 10000)
# Block  11460 , initialised  1 , accepted  10000 
# Max blocks between coalescence =  7 
hist(samplesto, 100, xlab = "Sample values",
 main = "Scaled t with nu = 4, d = 1, n = 10,000")
nqq = 10000
qqplot(qt(((1:nqq) - 0.5) / nqq, df = 4), samplesto, asp = 1, pch = "+",
 xlab = "Theoretical quantiles", ylab = "Sample quantiles",
 cex = 0.8, main = "Scaled t with nu = 4, d = 1, n = 10,000")
mean(samplesto) # 0.0007394156, I haven't worked out the theoretical s.d.
var(samplesto) # 2.454352, again I haven't worked out the theoretical one.

Hmc(Ufunc_tscal, Uderiv_tscal, d = 1, nu = 4, alpha = 1.5, npast = 23) #X2
# Slightly better, fewer points per trajectory when these numbers are
#   above 20, but really not worth worrying about for d = 1.

# We'll be lazy and do the unscaled one for which we know the
#   theoretical variance.
Rocftp(Ufunc = Ufunc_t, Uderiv = Uderiv_t, d = 1, nu = 4, npast = 23,
 nBlockAcc = 10000)
# Block  11583 , initialised  1 , accepted  10000 
# Max blocks between coalescence =  6 
hist(samplesto, 100, xlab = "Sample values",
 main = "Unscaled t with nu = 4, d = 1, n = 10,000")
nqq = 10000
qqplot(qt(((1:nqq) - 0.5) / nqq, df = 4), samplesto, asp = 1, pch = "+",
 xlab = "Theoretical quantiles", ylab = "Sample quantiles",
 cex = 0.8, main = "Scaled t with nu = 4, d = 1, n = 10,000")
mean(samplesto) # 0.001047909, too small?  Theoretical s.d. = 0.0141.
var(samplesto) # 1.980791, theoretical = 2, good!
sum(samplesto > 0) # 5007, close to mean; s.d. = 50; in accordance with mean.

Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 70)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 65) #*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 64) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 63) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 62) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 61) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 60) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 59) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 58) #X2*
# About 24 points per trajectory, some around 50
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, npast = 57) #X3

Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 58)
# Slightly lesser numbers of points per trajectory, better than alpha
#   = 2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 66)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 65)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 64)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 63)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 62)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 61)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 60)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 59)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 58)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 57)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 56) #*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 55) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 54) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 53) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 52) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 51) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 50) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 49) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 48) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 47) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 46) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 45) #X2*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 44) #XX4
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 43) #XX3
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 42) #XX3
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 41) #XX4
Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1.5, npast = 40) #XX3
Rocftp(Ufunc = Ufunc_tscal, Uderiv = Uderiv_tscal, d = 2, nu = 4,
 alpha = 1.5, npast = 45, nBlockAcc = 1000)
# Block  1157 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  3 
save(samplesto, file = "t4_2_1000.RData")

Hmc(Ufunc_tscal, Uderiv_tscal, d = 2, nu = 4, alpha = 1, npast = 58)
# Many trajectories less than 20 points; I think alpha = 1 is cutting
#   it too fine.

Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 350)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 345)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 340)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 335)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 330)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 325) #*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 320) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 315)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 310)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 305)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 300)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 295) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 290) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 285) #X1
# Generally around 50 points per trajectory, some over 100
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 280) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 275) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 250) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 225) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 220) #X1*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 215) #X3
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 200) #X3

Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 300)
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 290) #*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 285) #X1
# Many trajectories about 20 points, few < 20, but some up to 180.
#   Looks like alpha = 1.5 is the best we can do.  We don't want many
#   to go below 20 points.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 280) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 260) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 230) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 220) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 210) #X2
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 205) #X2*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, alpha = 1.5, npast = 200) #X3
# Again many trajectories about 20 points, few < 20, but some up to 180.
Rocftp(Ufunc = Ufunc_tscal, Uderiv = Uderiv_tscal, d = 10, nu = 4,
 alpha = 1.5, npast = 205, nBlockAcc = 1000)
# Block  1102 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "t4_10_1000.RData")

Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 325, alpha = 1) #OK
# 8 to 40 points per trajectory, generally around 14, maybe cutting to
#   too fine.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 220, alpha = 1) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 10, nu = 4, npast = 200, alpha = 1) #XX3

Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, npast = 2000) #XX3
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, alpha = 1, npast = 2500) #XX20
# Didn't work!  About 10 points per trajectory, many around 7, must be
#   too small.

# maxpast = 2500 for this next one.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, npast = 2500) #XX2
# About 100 points per trajectory; varies between about 70 and 170.

# Now maxpast = 4000.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, npast = 4000)
# Around 110 points per trajectory, range about 80 to 250
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, npast = 3000) #*
# Around 110 points per trajectory, range about 80 to 250

Rocftp(Ufunc = Ufunc_tscal, Uderiv = Uderiv_tscal, d = 100, nu = 4,
 npast = 2500, nBlockAcc = 100)
# Block  105 , initialised  1 , accepted  100 
# Max blocks between coalescence =  2 
save(samplesto, file = "t4_100_100.RData")

# Still maxpast = 4000.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, alpha = 1.5, npast = 2500) #*
# Average about 50 points per trajectory, range about 30 to 100
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 4, alpha = 1.5, npast = 2000) #X1*
# About 50 points per trajectory, range 30 to 70, good!
Rocftp(Ufunc = Ufunc_tscal, Uderiv = Uderiv_tscal, d = 100, nu = 4,
 alpha = 1.5, npast = 2000, nBlockAcc = 1000)
# Block  1076 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  3 
save(samplesto, file = "t4_100_1000.RData")

# Back to maxpast = 2000 here.
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 2000)
# Generally 70-200 points per trajectory
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 1700) #*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 1600) #X1
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 1500) #X2*
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 1400) #XX3
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 1300) #XX4
Hmc(Ufunc_tscal, Uderiv_tscal, d = 100, nu = 5, npast = 1000) #XX10

#################### 100-D with records of the total number of
#   coalesced trajectories (over all runs and starting points), for
#   figure in paper that shows the proportion of coalesced runs as the
#   number of trajectories increases.
# We use the highest number of trajectories first (the one for which
#   all runs coalesce) in order to gauge what are the "correct"
#   coalesced results.  For the lesser numbers of trajectories, we
#   record the proportion of results equal to the fully coalesced
#   ones.
# maxpast = 4000 here (affects exact reproducibility of the results).
dset = 100
nuset = 4
alphaset = 1.5
npastcoal = 2500
npastrocftp = 2000
xcoal = seq(25, npastcoal, 25)
Results = list()
eps = 1e-5
ycoal = rep(0, length(xcoal))
names(ycoal) = xcoal
maxabs = function(x) max(abs(x))
for (i in xcoal) {
 cat(i, "\n")
 Results[[as.character(i)]] = Hmc(Ufunc_tscal, Uderiv_tscal, d = dset,
  nu = nuset, alpha = alphaset, npast = i)
}
# Assume the definitive results are in the final element of Results;
#   i.e., results have been generated in ascending order of number of
#   trajectories.
for (i in names(ycoal)) { # Character this time; don't need numeric version
 ycoal[[i]] = sum(
  apply(Results[[i]] - Results[[length(Results)]], 1, maxabs) <= eps)
}
plot(xcoal, ycoal, xaxs = "i", yaxs = "i", type = "l",
 xlab = "Number of trajectories into the past",
 ylab = "Number of coalesced run–starting-point combinations",
 main = paste("t-distribution, nu = 4, d =", dset))
lines(c(npastrocftp, npastrocftp), c(0, max(ycoal)), lty = 2)
save(Results, file = paste("t", nuset, "1p5", dset, "Coalescence.RData",
 sep = "_"))
