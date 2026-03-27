######################################################################
# R script for running Hamiltonian Monte Carlo coalscence code for the
#   normal mixture distribution, up to 100 dimensions, using the Full
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

######################################## Mixture of normal distributions
# Parameter mu = location of second mode.  Both distributions are
#   standard normal.  Mixing proportion is 0.5; i.e., equal weighting.
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 500)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 400)
# Mostly around 20, some up to 50 points per trajectory
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 300)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 200)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 100)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 50)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 47)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 46)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 45)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 44)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 43)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 42)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 41)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 40)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 39)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 38)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 37) #*
# Mostly around 20, some up to 50 points per trajectory
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 36) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 35) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 30) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 29) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 28) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 27) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 26) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 25) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 24) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 23) #XX4
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 4, npast = 20) #XX4

Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 1, mu = 4, npast = 25,
 nBlockAcc = 2500)
# Block  3195 , initialised  1 , accepted  2500 
# Max blocks between coalescence =  7
hist(samplesto, 100, xlab = "Sample values",
 main = "Normal mixture with mu = 4, d = 1, n = 2500")
nqq = 2500
qqplot(qt(((1:nqq) - 0.5) / nqq, df = 4), samplesto, asp = 1, pch = "+",
 xlab = "Theoretical quantiles", ylab = "Sample quantiles",
 cex = 0.8, main = "Scaled t with nu = 4, d = 1, n = 10,000")
mean(samplesto) # 0.001047909, too small?  Theoretical s.d. = 0.0141.
var(samplesto) # 1.980791, theoretical = 2, good!
sum(samplesto > 0) # 5007, close to mean; s.d. = 50; in accordance with mean.

# Try some runs with beta < 2.  High energy should work for this
#   distribution?
betatr = c(rep(0.5, maxpast - 15), rep(2, 15))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 4, npast = 50)
# XX3

Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 300)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 250)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 249)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 248)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 247)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 246)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 245)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 244)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 243)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 242)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 241)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 240)
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 239) #*
# Nearly all 20 points per trajectory, a few around 50 but these are
#   evidently the ones where it switches modes.
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 238) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 230) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 200) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 150) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 145) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 144) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 143) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 142) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 140) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 1, mu = 6, npast = 100) #XX7
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 1, mu = 6,
 npast = 144, nBlockAcc = 1000)
# Block  1093 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "NormMix_1_6_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 100)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 94)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 93)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 92)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 91)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 90)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 89)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 88)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 87)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 86)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 85)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 84) #*
# Mostly around 20 points per trajectory, some around 40
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 83) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 82)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 81)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 80) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 70) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 60)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 55) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 54) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 53) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 52) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 51) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 4, npast = 50) #XX3
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 2, mu = 4,
 npast = 52, nBlockAcc = 1000)
# Block  1178 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  5 
save(samplesto, file = "NormMix_2_4_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 500)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 450)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 430)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 420)
# Nearly all around 20 points per trajectory, a few around 40
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 417)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 416)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 415)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 414)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 413)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 412)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 411)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 410)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 409)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 408)
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 407) #*
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 406) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 405) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 400) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 300) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 200) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 190) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 185) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 180) #XX3
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 150) #XX6
Hmc(UfuncNormMix, UderivNormMix, d = 2, mu = 6, npast = 100) #XX9
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 2, mu = 6,
 npast = 190, nBlockAcc = 1000)
# Block  1116 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "NormMix_2_6_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 150)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 143)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 142)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 141)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 140)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 139)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 138)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 137)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 136)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 135)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 134)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 133) #*
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 132) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 130) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 110) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast = 100) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast =  95) #XX4
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 4, npast =  90) #XX4
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 10, mu = 4,
 npast = 100, nBlockAcc = 1000)
# Block  1151 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  4 
save(samplesto, file = "NormMix_10_4_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 900)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 810)
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 800) #*
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 750) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 700) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 500) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 450) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 10, mu = 6, npast = 400) #XX3
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 10, mu = 6,
 npast = 450, nBlockAcc = 1000)
# Block  1080 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  3 
save(samplesto, file = "NormMix_10_6_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 200)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 150)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 140)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 134)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 133)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 132)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 131)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 130)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 129)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 128)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 127)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 126)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 125)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 124) #*
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 123) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 120)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 119)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 118)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 117)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 116)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 115)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 114)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 113) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 112)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 110) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 105) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 100)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 99) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 95) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 90) #X1*
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 89) #X4
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 85) #XX4
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 80) #XX4
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 4, npast = 50) #XX14
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 100, mu = 4,
 npast = 90, nBlockAcc = 1000)
# Block  1266 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  6 
save(samplesto, file = "NormMix_100_4_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 1200)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 900)
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 875) #*
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 850) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 800) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 750) #X1
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 700) #X2
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 650) #X2*
Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 6, npast = 600) #XX3
Rocftp(Ufunc = UfuncNormMix, Uderiv = UderivNormMix, d = 100, mu = 6,
 npast = 650, nBlockAcc = 1000)
# Block  1033 , initialised  1 , accepted  1000 
# Max blocks between coalescence =  3 
save(samplesto, file = "NormMix_100_6_1000.RData")

Hmc(UfuncNormMix, UderivNormMix, d = 100, mu = 7, npast = 2000) #X2

betatr = c(rep(1, maxpast - 1500), rep(2, 1500))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr,
 d = 100, mu = 7, npast = 2000) #X1

# Try beta < 2, only for large mu and d.  We seem to have established
#   that it provides no benefit for non-large mu or non-large d.
betatr = c(rep(1, maxpast - 700), rep(2, 700))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 800)
# Around 22 points per trajectory for beta = 2, 18 for beta = 1
betatr = c(rep(1, maxpast - 600), rep(2, 600))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 700)
#X2
betatr = c(rep(1, maxpast - 500), rep(2, 500))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 600)
#OK
betatr = c(rep(1, maxpast - 400), rep(2, 400))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 500)
#X2
betatr = c(rep(1, maxpast - 300), rep(2, 300))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 400)
#XX4
betatr = c(rep(1, maxpast - 350), rep(2, 350))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 400)
#XX4

betatr = c(rep(0.5, maxpast - 400), rep(2, 400))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 100, mu = 6, npast = 500)
#XX3
# Around 22 points per trajectory for beta = 2, 14 for beta = 1

#################### Try beta < 2.
betatr = c(rep(0.5, maxpast - 100), rep(2, 100))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 200)
#X1
# Some trajectories around 700 points when beta = 0.5, some only 3 points

betatr = c(rep(0.5, maxpast - 150), rep(2, 150))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 200)
# Again some trajectories up to 600 points (average over the starting points)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 190)
#X1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 180)
#X1

betatr = c(rep(1, maxpast - 150), rep(2, 150))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 200)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 180)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 179)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 178)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 177)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 176)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 175)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 174)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 173)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 172)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 171)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 170)
#*
# Now typically 25-100 points per trajectory when beta = 1, better than 0.5
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 169)
#X1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 165)
#X2
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 164)
#X2
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 163)
#X2
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 162)
#X2
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 161)
#X2*
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 160)
#XX3
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 159)
#XX3

betatr = c(rep(1, maxpast - 100), rep(2, 100))
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 160)
#*
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 155)
#X1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 150)
#X1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 140)
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 130)
#X1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 120)
#X1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 115)
#X2
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 114)
#X2*
# Around 20-100 points per trajectory when beta = 1
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 113)
#X3
Hmc(UfuncNormMix, UderivNormMix, beta_in = betatr, d = 1, mu = 6, npast = 110)
#XX3
# Conclusion: Only very minor gains from using beta = 1 when mu = 6.
#   Maybe the gains will be larger for larger values of mu.
