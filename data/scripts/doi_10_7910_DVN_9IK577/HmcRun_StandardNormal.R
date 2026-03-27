######################################################################
# R script for running Hamiltonian Monte Carlo coalscence code for the
#   standard normal distribution, up to 100 dimensions, using the Full
#   Random Uniform Trajectory Sampler (FRUTS)
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

######################################################################
# Run the standard normal distributions documented in the paper.
# Displayed output from functions is arranged so that a final column
#   of single-digit zeros denotes convergence.  Two decimal places in
#   the final column denotes success of the rounding step (i.e.,
#   acceptance of Metropolis-Hastings update) but lack of convergence
#   in at least one of the simulations (i.e., different final results
#   from different starting points).  More than two decimal places
#   usually means that the rounding step failed in at least one of the
#   simulations, but sometimes results from numerical roundoff error.

# Coalescence indicators: Runs that almost coalesced but not quite
#   (e.g., a subset of 90% of the trajectories coalesced) are marked
#   "#X".  These are still useful for gauging the number of
#   trajectories required for read-once coupling from the past
#   (ROCFTP).  Runs that didn't coalesce and weren't close are marked
#   "#XX".  Runs that coalesced are not marked, except for the one
#   with the lowest number of trajectories to produce coalescence from
#   all attempted starting points, which is marked "#*".  The setting
#   that we suggest for ROCFTP (minimum of the ones that almost
#   coalescenced) is marked "#X*".
# Comments on number of points per trajectory come from running
#   "Diag()" after executing the line above the comment.

######################################## Standard normal distribution

#################### 1-D
Hmc(d = 1, npast = 50)
Hmc(d = 1, npast = 49)
Hmc(d = 1, npast = 48)
Hmc(d = 1, npast = 47)
Hmc(d = 1, npast = 46)
Hmc(d = 1, npast = 45)
Hmc(d = 1, npast = 44)
Hmc(d = 1, npast = 43)
Hmc(d = 1, npast = 42)
Hmc(d = 1, npast = 41)
Hmc(d = 1, npast = 40)
Hmc(d = 1, npast = 39)
Hmc(d = 1, npast = 38)
Hmc(d = 1, npast = 37)
Hmc(d = 1, npast = 36)
Hmc(d = 1, npast = 35)
Hmc(d = 1, npast = 34)
Hmc(d = 1, npast = 33)
Hmc(d = 1, npast = 32)
Hmc(d = 1, npast = 31)
Hmc(d = 1, npast = 30)
Hmc(d = 1, npast = 29)
Hmc(d = 1, npast = 28)
Hmc(d = 1, npast = 27)
Hmc(d = 1, npast = 26)
Hmc(d = 1, npast = 25)
Hmc(d = 1, npast = 24) #*
# Very steady 20 points per trajectory
Hmc(d = 1, npast = 23) #X1
Hmc(d = 1, npast = 22)
Hmc(d = 1, npast = 21) #X1
Hmc(d = 1, npast = 20) #X1
Hmc(d = 1, npast = 19)
Hmc(d = 1, npast = 18) #X1*
Hmc(d = 1, npast = 17) #XX3
Hmc(d = 1, npast = 16) #XX5
Hmc(d = 1, npast = 15) #XX5
Hmc(d = 1, npast = 14) #XX9
Hmc(d = 1, npast = 13) #XX8
Hmc(d = 1, npast = 12) #XX11
Hmc(d = 1, npast = 11) #XX10
Hmc(d = 1, npast = 10) #XX12

Rocftp(d = 1, npast = 18, nBlockAcc = 50000)
# Block  56240 , initialised  1 , accepted  50000 
# Max blocks between coalescence =  6 
# Histogram and Q-Q plot provided
# Mean = -0.00555867, predicted s.e. 0.0045, good!
# S.d. = 0.9980484, predicted s.e. 0.0032, good!
save(samplesto, file = "Normal_1_50000.RData")
# Serial trace plots to show that points are uncorrelated
plot(samplesto, cex = 0.5, pch = 3,
 main = "Standard normal, d = 1",
 xlab = "Sample number", ylab = "Sample value")
plot(samplesto[1:1000,], cex = 0.5, pch = 3,
 main = "Standard normal, d = 1",
 xlab = "Sample number", ylab = "Sample value")

# Switch off the final rounding by setting w very small, so we can
#   judge how far apart the points from different starting values are.
Hmc(d = 1, npast = 24, w = 1e-10) #X
# Maximum difference 0.0015.  Note many converge to at least 8 decimal
#   places!  Also note many are not monotonic with respect to the
#   starting points.

# Try beta < 2.  Note beta = 2 is the only one that produces
#   coalescence near the end.  We always want to finish with some
#   trajectories with beta = 2.
# Performance in this case is spectacularly bad if we have less than
#   12 trajectories with beta = 2.
betatrial = c(rep(1, maxpast - 12), rep(2, 12))
Hmc(d = 1, beta_in = betatrial, npast = 40)
Hmc(d = 1, beta_in = betatrial, npast = 39)
Hmc(d = 1, beta_in = betatrial, npast = 38)
Hmc(d = 1, beta_in = betatrial, npast = 37)
Hmc(d = 1, beta_in = betatrial, npast = 36)
Hmc(d = 1, beta_in = betatrial, npast = 35)
Hmc(d = 1, beta_in = betatrial, npast = 34)
Hmc(d = 1, beta_in = betatrial, npast = 33)
Hmc(d = 1, beta_in = betatrial, npast = 32)
Hmc(d = 1, beta_in = betatrial, npast = 31)
Hmc(d = 1, beta_in = betatrial, npast = 30)
Hmc(d = 1, beta_in = betatrial, npast = 29)
Hmc(d = 1, beta_in = betatrial, npast = 28)
Hmc(d = 1, beta_in = betatrial, npast = 27)
Hmc(d = 1, beta_in = betatrial, npast = 26)
Hmc(d = 1, beta_in = betatrial, npast = 25)
Hmc(d = 1, beta_in = betatrial, npast = 24)
Hmc(d = 1, beta_in = betatrial, npast = 23)
Hmc(d = 1, beta_in = betatrial, npast = 22)
Hmc(d = 1, beta_in = betatrial, npast = 21)
Hmc(d = 1, beta_in = betatrial, npast = 20) #*
Hmc(d = 1, beta_in = betatrial, npast = 19) #X
Hmc(d = 1, beta_in = betatrial, npast = 18) #X
Hmc(d = 1, beta_in = betatrial, npast = 17) #X
Hmc(d = 1, beta_in = betatrial, npast = 16) #X
Hmc(d = 1, beta_in = betatrial, npast = 15)
Hmc(d = 1, beta_in = betatrial, npast = 14) #X*
Hmc(d = 1, beta_in = betatrial, npast = 13) #X?
# We save 4 trajectories by putting beta = 1 for the initial ones.  We
#   have a lot more points per trajectory on average though, so it's
#   not worth it for the normal distribution as the target.  (Unless
#   somebody has a you-beaut way to set the time step when beta < 2.
#   I've found it extremely difficult.)

#################### 1-D with records of the total number of coalesced
#   trajectories (over all runs and starting points), for figure in
#   paper that shows the proportion of coalesced runs as the number of
#   trajectories increases.
# We use the highest number of trajectories first (the one for which
#   all runs coalesce) in order to gauge what are the "correct"
#   coalesced results.  For the lesser numbers of trajectories, we
#   record the proportion of results equal to the fully coalesced
#   ones.
dset = 1
#npastcoal = 24
npastcoal = 30
npastrocftp = 18
xcoal = 1:npastcoal
Results = list()
eps = 1e-5
ycoal = rep(0, length(xcoal))
names(ycoal) = xcoal
maxabs = function(x) max(abs(x))
for (i in xcoal) {
 cat(i, "\n")
 Results[[as.character(i)]] = Hmc(d = dset, npast = i)
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
 main = paste("Standard normal, d =", dset))
lines(c(npastrocftp, npastrocftp), c(0, max(ycoal)), lty = 2)
save(Results, file = paste("Normal", dset, "Coalescence.RData", sep = "_"))

#################### 2-D
Hmc(d = 2, npast = 55)
Hmc(d = 2, npast = 54)
Hmc(d = 2, npast = 53)
Hmc(d = 2, npast = 52)
Hmc(d = 2, npast = 51)
Hmc(d = 2, npast = 50)
Hmc(d = 2, npast = 49)
Hmc(d = 2, npast = 48)
Hmc(d = 2, npast = 47)
Hmc(d = 2, npast = 46)
Hmc(d = 2, npast = 45) #*
Hmc(d = 2, npast = 44) #X1
Hmc(d = 2, npast = 43) #X1
Hmc(d = 2, npast = 42)
Hmc(d = 2, npast = 41) #X1
Hmc(d = 2, npast = 40) #X1
Hmc(d = 2, npast = 39) #X1
Hmc(d = 2, npast = 38) #X1
Hmc(d = 2, npast = 37)
Hmc(d = 2, npast = 36) #X2*
Hmc(d = 2, npast = 35) #XX3
Hmc(d = 2, npast = 34) #XX3
Hmc(d = 2, npast = 33) #XX4
Hmc(d = 2, npast = 32) #XX5
Hmc(d = 2, npast = 31) #XX4
Hmc(d = 2, npast = 30) #XX4
Hmc(d = 2, npast = 29) #XX6
Hmc(d = 2, npast = 28) #XX6
Hmc(d = 2, npast = 27) #XX5

Rocftp(d = 2, npast = 36, nBlockAcc = 100)
# Block  107 , initialised  1 , accepted  100 
# Max blocks between coalescence =  3 
Rocftp(d = 2, npast = 27, nBlockAcc = 100)
# Block  131 , initialised  1 , accepted  100 
# Max blocks between coalescence =  5
# Execution time is much the same: ratio of npast = 0.750, ratio of
#   no. of blocks = 1 / 0.817.
Rocftp(d = 2, npast = 36, nBlockAcc = 1000) # 20 s run time
# Block  1045 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  3 
Rocftp(d = 2, npast = 36, nBlockAcc = 10000)
# Block  10387 , initialised  1 , accepted  10000 
# Max blocks between coalescence =  3
hist(samplesto[, 1], 100, xlab = "Sample values",
 main = "Standard normal, d = 2, n = 10,000, dim. 1")
hist(samplesto[, 2], 100, xlab = "Sample values",
 main = "Standard normal, d = 2, n = 10,000, dim. 2")
qqnorm(samplesto[, 1], asp = 1, pch = "+", cex = 0.8,
 main = "Standard normal, d = 2, n = 10,000, dim. 1")
qqnorm(samplesto[, 2], asp = 1, pch = "+", cex = 0.8,
 main = "Standard normal, d = 2, n = 10,000, dim. 2")
apply(samplesto, 2, mean)
# -0.004394647  0.011466286, theoretical s.d. = 0.01, good!
cov(samplesto) # Theoretical s.d. of diagonal elements = 0.0141,
#   off-diagonal = 0.01.
#              [,1]         [,2]
# [1,]  0.979798312 -0.001082357
# [2,] -0.001082357  0.993991206

betatrial = c(rep(1, maxpast - 30), rep(2, 30)) # 25 doesn't work.
Hmc(d = 2, beta_in = betatrial, npast = 60)
Hmc(d = 2, beta_in = betatrial, npast = 50)
Hmc(d = 2, beta_in = betatrial, npast = 49)
Hmc(d = 2, beta_in = betatrial, npast = 48)
Hmc(d = 2, beta_in = betatrial, npast = 47)
Hmc(d = 2, beta_in = betatrial, npast = 46)
Hmc(d = 2, beta_in = betatrial, npast = 45)
Hmc(d = 2, beta_in = betatrial, npast = 44)
Hmc(d = 2, beta_in = betatrial, npast = 43)
Hmc(d = 2, beta_in = betatrial, npast = 42)
Hmc(d = 2, beta_in = betatrial, npast = 41)
Hmc(d = 2, beta_in = betatrial, npast = 40)
Hmc(d = 2, beta_in = betatrial, npast = 39)
Hmc(d = 2, beta_in = betatrial, npast = 38) #*
# Av. points per trajectory only slightly > 21.
Hmc(d = 2, beta_in = betatrial, npast = 37) #X
Hmc(d = 2, beta_in = betatrial, npast = 36) #X
Hmc(d = 2, beta_in = betatrial, npast = 35) #X
Hmc(d = 2, beta_in = betatrial, npast = 34) #X
Hmc(d = 2, beta_in = betatrial, npast = 33) #X
Hmc(d = 2, beta_in = betatrial, npast = 32) #X
Hmc(d = 2, beta_in = betatrial, npast = 31) #X
Hmc(d = 2, beta_in = betatrial, npast = 30) #X
# Some benefit in this but I suspect that by chance the random numbers
#   are easier than we had for beta = 2.

#################### 10-D
Hmc(d = 10, npast = 100)
Hmc(d = 10, npast = 90)
Hmc(d = 10, npast = 80)
Hmc(d = 10, npast = 75)
Hmc(d = 10, npast = 74)
Hmc(d = 10, npast = 73)
Hmc(d = 10, npast = 72)
Hmc(d = 10, npast = 71)
Hmc(d = 10, npast = 70)
Hmc(d = 10, npast = 69)
Hmc(d = 10, npast = 68)
Hmc(d = 10, npast = 67)
Hmc(d = 10, npast = 66)
Hmc(d = 10, npast = 65) #*
Hmc(d = 10, npast = 64) #X
Hmc(d = 10, npast = 63)
Hmc(d = 10, npast = 62) #X
Hmc(d = 10, npast = 61)
Hmc(d = 10, npast = 60) #X
Hmc(d = 10, npast = 59) #X
Hmc(d = 10, npast = 58) #X
Hmc(d = 10, npast = 57) #X
Hmc(d = 10, npast = 56) #X2*
Hmc(d = 10, npast = 55) #XX3
Hmc(d = 10, npast = 54) #XX3
Rocftp(d = 10, npast = 56, nBlockAcc = 1000)
# Block  1120 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "Normal_10_1000.RData")

#################### 10-D with records of the total number of
#   coalesced trajectories (over all runs and starting points), for
#   figure in paper that shows the proportion of coalesced runs as the
#   number of trajectories increases.
# We use the highest number of trajectories first (the one for which
#   all runs coalesce) in order to gauge what are the "correct"
#   coalesced results.  For the lesser numbers of trajectories, we
#   record the proportion of results equal to the fully coalesced
#   ones.
dset = 10
npastcoal = 65
npastrocftp = 56
xcoal = 1:npastcoal
Results = list()
eps = 1e-5
ycoal = rep(0, length(xcoal))
names(ycoal) = xcoal
maxabs = function(x) max(abs(x))
for (i in xcoal) {
 cat(i, "\n")
 Results[[as.character(i)]] = Hmc(d = dset, npast = i)
}
# Assume the definitive results are in the final element of Results;
#   i.e., results have been generated in ascending order of number of
#   trajectories.
for (i in names(ycoal)) { # Character this time; don't need numeric version
 ycoal[[i]] = sum(
  apply(Results[[i]] - Results[[length(Results)]], 1, maxabs) <= eps)
}
plot(xcoal, ycoal, xaxs = "i", yaxs = "i", type = "l",
 xlim = c(1, 70),
 xlab = "Number of trajectories into the past",
 ylab = "Number of coalesced run–starting-point combinations",
 main = paste("Standard normal, d =", dset))
lines(c(npastrocftp, npastrocftp), c(0, max(ycoal)), lty = 2)
save(Results, file = paste("Normal", dset, "Coalescence.RData", sep = "_"))

#################### 100-D
Hmc(d = 100, npast = 150)
Hmc(d = 100, npast = 123)
Hmc(d = 100, npast = 122)
Hmc(d = 100, npast = 121)
Hmc(d = 100, npast = 120)
Hmc(d = 100, npast = 119)
Hmc(d = 100, npast = 118)
Hmc(d = 100, npast = 117)
Hmc(d = 100, npast = 116)
Hmc(d = 100, npast = 115)
Hmc(d = 100, npast = 114)
Hmc(d = 100, npast = 113) #*
# Steady average 22 points per trajectory with only a small proportion
#   of Metropolis-Hastings rejections
Hmc(d = 100, npast = 112) #X
Hmc(d = 100, npast = 111) #X
Hmc(d = 100, npast = 110) #X
Hmc(d = 100, npast = 100) #X
Hmc(d = 100, npast = 90)  #X
Hmc(d = 100, npast = 80)  #X
Hmc(d = 100, npast = 70)  #X*
Hmc(d = 100, npast = 65)  #XX
Hmc(d = 100, npast = 60)  #XX
Rocftp(d = 100, npast = 70, nBlockAcc = 1000)
# Block  1111 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  5 
save(samplesto, file = "Normal_100_1000.RData")

#################### 100-D with records of the total number of
#   coalesced trajectories (over all runs and starting points), for
#   figure in paper that shows the proportion of coalesced runs as the
#   number of trajectories increases.
# We use the highest number of trajectories first (the one for which
#   all runs coalesce) in order to gauge what are the "correct"
#   coalesced results.  For the lesser numbers of trajectories, we
#   record the proportion of results equal to the fully coalesced
#   ones.
dset = 100
npastcoal = 113
npastrocftp = 70
xcoal = 1:npastcoal
Results = list()
eps = 1e-5
ycoal = rep(0, length(xcoal))
names(ycoal) = xcoal
maxabs = function(x) max(abs(x))
for (i in xcoal) {
 cat(i, "\n")
 Results[[as.character(i)]] = Hmc(d = dset, npast = i)
}
# Assume the definitive results are in the final element of Results;
#   i.e., results have been generated in ascending order of number of
#   trajectories.
for (i in names(ycoal)) { # Character this time; don't need numeric version
 ycoal[[i]] = sum(
  apply(Results[[i]] - Results[[length(Results)]], 1, maxabs) <= eps)
}
plot(xcoal, ycoal, xaxs = "i", yaxs = "i", type = "l",
 xlim = c(1, 120),
 xlab = "Number of trajectories into the past",
 ylab = "Number of coalesced run–starting-point combinations",
 main = paste("Standard normal, d =", dset))
lines(c(npastrocftp, npastrocftp), c(0, max(ycoal)), lty = 2)
save(Results, file = paste("Normal", dset, "Coalescence.RData", sep = "_"))
