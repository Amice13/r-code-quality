######################################################################
# R script for running Hamiltonian Monte Carlo coalscence code for the
#   correlated normal distribution, up to 100 dimensions, using the
#   Full Random Uniform Trajectory Sampler (FRUTS)
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

######################################## Normal distribution with
#   standard deviation equal to 1 for all components but with
#   correlation rho for each pair of components
# It makes no sense to run this function with d = 1, so we start at 2.
# Numbers of trajectories to coalescence from all starting points with
#   beta = 2 in all trajectories, numbers of trajectories good enough
#   for ROCFTP with beta = 2, together with aspect ratio of variance
#   ellipsoid = (1 + (d - 1) * rho) / (1 - rho)
# d = 2,   rho = 0:      45,   36,   1
# d = 2,   rho = 0.1:    43,   27,   1.22
# d = 2,   rho = 0.6:    45,   41,   4
# d = 2,   rho = 0.95:  119,   90,  39
# d = 10,  rho = 0:      65,   56,   1
# d = 10,  rho = 0.1:    84,   73,   2.11
# d = 10,  rho = 0.6:   375,  245,  16
# d = 10,  rho = 0.95: 1600, 1300, 191
# d = 100, rho = 0:     113,   70,   1
# d = 100, rho = 0.1:   375,  325,  12.11
# d = 100, rho = 0.45: 1775, 1550,  82.82

#################### 2-D
# Check with rho = 0, should give same results as above.
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0, npast = 45) #*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0, npast = 44) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0, npast = 36) #X*

Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 53)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 52)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 51)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 50)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 49)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 48)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 47)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 46)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 45)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 44)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 43) #*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 42) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 41)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 40)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 39)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 38)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 37) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 36)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 35) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 30) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 29) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 28) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 27) #X*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 26) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1, npast = 25) #XX
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.1,
 npast = 27, nBlockAcc = 1000)
# Block  1205 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  5 
save(samplesto, file = "NormCorr_2_010_1000.RData")

Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 55)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 54)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 53)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 52)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 51)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 50)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 49)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 48)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 47)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 46)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 45) #*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 44) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 43) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 42) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 41) #X*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6, npast = 40) #XX
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.6,
 npast = 41, nBlockAcc = 1000)
# Block  1137 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  6 
save(samplesto, file = "NormCorr_2_060_1000.RData")

Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 150)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 140)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 130)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 129)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 128)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 127)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 126)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 125)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 124)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 123)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 122)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 121)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 120)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 119) #*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 118) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 117) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 116) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 115) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 110) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 100) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 95) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 90) #X*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 85) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 80) #XX

Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 2, rho = 0.95, npast = 90,
 nBlockAcc = 2500)
# Block  2670 , initialised  1 , accepted  2500 
# Max blocks between coalescence =  3 
hist(samplesto[, 1], 100, xlab = "Sample values",
 main = "Normal with rho = 0.95, d = 2, n = 2500, dim. 1")
hist(samplesto[, 2], 100, xlab = "Sample values",
 main = "Normal with rho = 0.95, d = 2, n = 2500, dim. 2")
qqnorm(samplesto[, 1], asp = 1, pch = "+", cex = 0.8,
 main = "Normal with rho = 0.95, d = 2, n = 2500, dim. 1")
qqnorm(samplesto[, 2], asp = 1, pch = "+", cex = 0.8,
 main = "Normal with rho = 0.95, d = 2, n = 2500, dim. 2")
apply(samplesto, 2, mean)
# 0.007786711 0.015540998, theoretical s.d. = 0.02, good!
cov(samplesto) # Theoretical s.d. of diagonal elements = 0.0282
#           [,1]      [,2]
# [1,] 0.9797228 0.9333031
# [2,] 0.9333031 0.9876432

Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 100)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 94)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 93)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 92)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 91)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 90)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 89)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 88)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 87)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 86)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 85)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 84) #*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 83) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 82) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 81) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 80) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 75) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 74) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 73) #X*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 72) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1, npast = 70) #XX
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.1,
 npast = 73, nBlockAcc = 1000)
# Block  1114 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "NormCorr_10_010_1000.RData")

Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 400)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 395)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 390)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 385)
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 380)
# About 15 points per trajectory
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 375) #*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 370) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 360) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 350) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 300) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 250) #X
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 245) #X*
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 240) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 225) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 200) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 150) #XX
Hmc(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6, npast = 100) #XX
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.6,
 npast = 245, nBlockAcc = 1000)
# Block  1177 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  5 
save(samplesto, file = "NormCorr_10_060_1000.RData")

Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1800)
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1700)
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1600) #*
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1500) #X1
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1400) #X1
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1300) #X1*
# Most around 5 points per trajectory, some around 50.  Most of the
#   time we hit the short side of the very eccentric ellipsoid and get
#   few points, but occasionally we hit the long side and get a lot of
#   points.
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 1200) #XX3
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 800)  #XX6
Hmc(UfuncCorr, UderivCorr, d = 10, rho = 0.95, npast = 500)  #XX
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 10, rho = 0.95,
 npast = 1300, nBlockAcc = 1000)
# Block  1049 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "NormCorr_10_095_1000.RData")

Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 1000)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 500)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 400)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 395)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 390)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 385)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 380)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 375) #*
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 370) #X1
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 350) #X2
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 325) #X2*
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 300) #XX
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.1, npast = 200) #XX
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 100, rho = 0.1,
 npast = 325, nBlockAcc = 1000)
# Block  1016 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  2 
save(samplesto, file = "NormCorr_100_010_1000.RData")

Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.5, npast = 2000) #XX3

Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 2000)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1900)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1800)
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1775) #*
# About 17 points per trajectory, a few very large when it happens to
#   run along the major axis of the variance ellipsoid.
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1750) #X1
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1700) #X2
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1600) #X1
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1550) #X2*
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1525) #XX3
Hmc(UfuncCorr, UderivCorr, d = 100, rho = 0.45, npast = 1500) #XX3
Rocftp(Ufunc = UfuncCorr, Uderiv = UderivCorr, d = 100, rho = 0.45,
 npast = 1550, nBlockAcc = 1000)
# Block  1083 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  3 
save(samplesto, file = "NormCorr_100_045_1000.RData")
