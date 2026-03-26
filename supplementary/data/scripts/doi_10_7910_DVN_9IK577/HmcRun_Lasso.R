######################################################################
# R script for running Hamiltonian Monte Carlo coalscence code for the
#   Bayesian Lasso, using the Full Random Uniform Trajectory Sampler
#   (FRUTS)
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

source("HmcRun_LassoPreliminary.R")

######################################## Negative log-likelihood and
#   its derivative are in "Hmc.R".

######################################## Run Bayesian Lasso.

#################### Lambda = 0
LHmc(d = 12, Llambda = 0, npast = 100)
LHmc(d = 12, Llambda = 0, npast = 96)
LHmc(d = 12, Llambda = 0, npast = 95)
LHmc(d = 12, Llambda = 0, npast = 94)
LHmc(d = 12, Llambda = 0, npast = 93)
LHmc(d = 12, Llambda = 0, npast = 92)
LHmc(d = 12, Llambda = 0, npast = 91)
LHmc(d = 12, Llambda = 0, npast = 90)
LHmc(d = 12, Llambda = 0, npast = 89)
# Very consistent at about 22 points per trajectory
LHmc(d = 12, Llambda = 0, npast = 88) #*
LHmc(d = 12, Llambda = 0, npast = 87) #X1
LHmc(d = 12, Llambda = 0, npast = 86) #X1
LHmc(d = 12, Llambda = 0, npast = 85) #X1
LHmc(d = 12, Llambda = 0, npast = 82) #X1
LHmc(d = 12, Llambda = 0, npast = 81) #X1
LHmc(d = 12, Llambda = 0, npast = 80) #X1
LHmc(d = 12, Llambda = 0, npast = 79) #X1
LHmc(d = 12, Llambda = 0, npast = 78) #X1
LHmc(d = 12, Llambda = 0, npast = 77) #X1
LHmc(d = 12, Llambda = 0, npast = 76) #X1
LHmc(d = 12, Llambda = 0, npast = 75) # 20/20 coalesced but not the answer
LHmc(d = 12, Llambda = 0, npast = 74) #X1
LHmc(d = 12, Llambda = 0, npast = 73) #X1
LHmc(d = 12, Llambda = 0, npast = 70) #X1
LHmc(d = 12, Llambda = 0, npast = 67) #X1
LHmc(d = 12, Llambda = 0, npast = 66) #X2
LHmc(d = 12, Llambda = 0, npast = 65) #X1
LHmc(d = 12, Llambda = 0, npast = 64) #X2*
LHmc(d = 12, Llambda = 0, npast = 63) #XX3

LRocftp(Llambda = 0, npast = 64, nBlockAcc = 10000)
# Block  11386 , initialised  1 , accepted  10000 
# Max blocks between coalescence =  5 
save(samplesto, file = "Lasso_0_10000.RData")
#load("Lasso_0_10000.RData")
dSamp = 12
d1Samp = 11
nBlockSamp = dim(samplesto)[1]
LambdaSamp = 0
LbetaSamp = rep(betahat, nBlockSamp) +
 Ldiab %*% t(samplesto[, 1:d1Samp]) # De-scale Lasso beta (regression
 #   coefficients)
LtauSamp = tauhat * exp(cvtauhat * samplesto[,dSamp])
SsumSamp = apply((rep(LassoY, nBlockSamp) - LassoX %*% LbetaSamp)^2,
 2, sum)
TsumSamp = apply(abs(LbetaSamp[2:d1Samp,]), 2, sum) # Omit the
#   intercept coefficient.
range(SsumSamp) # 1266k to 1373k; range for lambda = 5 is 1270k to 1371k
range(TsumSamp) # 81.2 to 381.9; range for lambda = 5 is 79.3 to 188.6
# Calculate quantiles for lambda = 0, to put on graphs for other
#   values of lambda.
Qtau = quantile(LtauSamp, probs = c(0.025, 0.5, 0.975))
QSsum = quantile(SsumSamp, probs = c(0.025, 0.5, 0.975))
QTsum = quantile(TsumSamp, probs = c(0.025, 0.5, 0.975))
hist(1 / LtauSamp, 50, xlab = "Sample values",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sigma parameter, n = ", nBlockSamp, sep = ""))
#lines(1 / c(tauhat, tauhat), c(0, nBlockSamp), lty = 2)
#lines(1 / c(Qtau["2.5%"], Qtau["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["97.5%"], Qtau["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["50%"], Qtau["50%"]), c(0, nBlockSamp), lty = 5)
hist(SsumSamp / 1000, 50, xlab = "Sample values (× 1000)",
 xaxs = "i", xlim = c(1266, 1374),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of squares, n = ", nBlockSamp, sep = ""))
#lines(c(Shat, Shat) / 1000, c(0, nBlockSamp), lty = 2)
#lines(c(QSsum["2.5%"], QSsum["2.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["97.5%"], QSsum["97.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["50%"], QSsum["50%"]) / 1000, c(0, nBlockSamp), lty = 5)
hist(TsumSamp, 50, xlab = "Sample values",
 xaxs = "i", xlim = c(75, 385),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""))
#lines(c(That, That), c(0, nBlockSamp), lty = 2)
#lines(c(QTsum["2.5%"], QTsum["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["97.5%"], QTsum["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["50%"], QTsum["50%"]), c(0, nBlockSamp), lty = 5)
# Serial trace plot of TsumSamp
plot(TsumSamp, cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:1000], cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:100], cex = 0.5, pch = 3, type = "b",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")

#################### Lambda = 0.237
LHmc(d = 12, Llambda = 0.237, npast = 100)
LHmc(d = 12, Llambda = 0.237, npast = 95)
LHmc(d = 12, Llambda = 0.237, npast = 94)
LHmc(d = 12, Llambda = 0.237, npast = 93)
LHmc(d = 12, Llambda = 0.237, npast = 92)
LHmc(d = 12, Llambda = 0.237, npast = 91)
LHmc(d = 12, Llambda = 0.237, npast = 90)
LHmc(d = 12, Llambda = 0.237, npast = 89)
LHmc(d = 12, Llambda = 0.237, npast = 88)
LHmc(d = 12, Llambda = 0.237, npast = 87)
# Very consistent 22 points per trajectory
LHmc(d = 12, Llambda = 0.237, npast = 86)
# Very consistent 22 points per trajectory
LHmc(d = 12, Llambda = 0.237, npast = 85)
LHmc(d = 12, Llambda = 0.237, npast = 84) #*
LHmc(d = 12, Llambda = 0.237, npast = 83) #X1
LHmc(d = 12, Llambda = 0.237, npast = 82) #X2
LHmc(d = 12, Llambda = 0.237, npast = 80) #X1
LHmc(d = 12, Llambda = 0.237, npast = 75) #X2
LHmc(d = 12, Llambda = 0.237, npast = 70) #X2
LHmc(d = 12, Llambda = 0.237, npast = 65) #X2*
LHmc(d = 12, Llambda = 0.237, npast = 64) #XX3
LHmc(d = 12, Llambda = 0.237, npast = 63) #XX3
LHmc(d = 12, Llambda = 0.237, npast = 60) #XX3
LHmc(d = 12, Llambda = 0.237, npast = 55) #XX4

LRocftp(Llambda = 0.237, npast = 65, nBlockAcc = 10000)
# Block  11360 , initialised  1 , accepted  10000 
# Max blocks between coalescence =  5 
save(samplesto, file = "Lasso_0237_10000.RData")
#load("Lasso_0237_10000.RData")
dSamp = 12
d1Samp = 11
nBlockSamp = dim(samplesto)[1]
LambdaSamp = 0.237
LbetaSamp = rep(betahat, nBlockSamp) +
 Ldiab %*% t(samplesto[, 1:d1Samp]) # De-scale Lasso beta (regression
 #   coefficients)
LtauSamp = tauhat * exp(cvtauhat * samplesto[,dSamp])
SsumSamp = apply((rep(LassoY, nBlockSamp) - LassoX %*% LbetaSamp)^2,
 2, sum)
TsumSamp = apply(abs(LbetaSamp[2:d1Samp,]), 2, sum) # Omit the
#   intercept coefficient.
range(SsumSamp) # 1267k to 1368k; inside range for lambda = 0
range(TsumSamp) # 83.4 to 366.4; inside range for lambda = 0
hist(1 / LtauSamp, 50, xlab = "Sample values",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sigma parameter, n = ", nBlockSamp, sep = ""))
#lines(1 / c(tauhat, tauhat), c(0, nBlockSamp), lty = 2)
#lines(1 / c(Qtau["2.5%"], Qtau["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["97.5%"], Qtau["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["50%"], Qtau["50%"]), c(0, nBlockSamp), lty = 5)
hist(SsumSamp / 1000, 50, xlab = "Sample values (× 1000)",
 xaxs = "i", xlim = c(1266, 1374),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of squares, n = ", nBlockSamp, sep = ""))
#lines(c(Shat, Shat) / 1000, c(0, nBlockSamp), lty = 2)
#lines(c(QSsum["2.5%"], QSsum["2.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["97.5%"], QSsum["97.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["50%"], QSsum["50%"]) / 1000, c(0, nBlockSamp), lty = 5)
hist(TsumSamp, 50, xlab = "Sample values",
 xaxs = "i", xlim = c(75, 385),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""))
#lines(c(That, That), c(0, nBlockSamp), lty = 2)
#lines(c(QTsum["2.5%"], QTsum["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["97.5%"], QTsum["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["50%"], QTsum["50%"]), c(0, nBlockSamp), lty = 5)
# Serial trace plot of TsumSamp
plot(TsumSamp, cex = 0.5, pch = 3,
 xaxs = "i", xlim = c(75, 385),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:1000], cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:100], cex = 0.5, pch = 3, type = "b",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")

#################### Lambda = 1
LHmc(d = 12, Llambda = 1, npast = 112)
LHmc(d = 12, Llambda = 1, npast = 111)
LHmc(d = 12, Llambda = 1, npast = 110)
LHmc(d = 12, Llambda = 1, npast = 109)
LHmc(d = 12, Llambda = 1, npast = 108)
LHmc(d = 12, Llambda = 1, npast = 107)
LHmc(d = 12, Llambda = 1, npast = 106)
LHmc(d = 12, Llambda = 1, npast = 105)
LHmc(d = 12, Llambda = 1, npast = 104)
LHmc(d = 12, Llambda = 1, npast = 103)
LHmc(d = 12, Llambda = 1, npast = 102) #*
LHmc(d = 12, Llambda = 1, npast = 101) #X1
LHmc(d = 12, Llambda = 1, npast = 100)
LHmc(d = 12, Llambda = 1, npast = 99)
# As below, around 22 points per trajectory
LHmc(d = 12, Llambda = 1, npast = 98)
# As below, around 22 points per trajectory
LHmc(d = 12, Llambda = 1, npast = 97)
LHmc(d = 12, Llambda = 1, npast = 96)
LHmc(d = 12, Llambda = 1, npast = 95)
LHmc(d = 12, Llambda = 1, npast = 94)
# Consistently around 22 points per trajectory, but a little more
#   variation above and below than for lambda = 0 and 0.237.
LHmc(d = 12, Llambda = 1, npast = 93) #X1
LHmc(d = 12, Llambda = 1, npast = 90) #X1
LHmc(d = 12, Llambda = 1, npast = 85) #X2
LHmc(d = 12, Llambda = 1, npast = 80) #X2
# As above, around 22 points per trajectory
LHmc(d = 12, Llambda = 1, npast = 75) #X2*
LHmc(d = 12, Llambda = 1, npast = 70) #X3

LRocftp(Llambda = 1, npast = 75, nBlockAcc = 5000)
# Block  5962 , initialised  1 , accepted  5000 
# Max blocks between coalescence =  7 
save(samplesto, file = "Lasso_1_5000.RData")
#load("Lasso_1_5000.RData")
dSamp = 12
d1Samp = 11
nBlockSamp = dim(samplesto)[1]
LambdaSamp = 1
LbetaSamp = rep(betahat, nBlockSamp) +
 Ldiab %*% t(samplesto[, 1:d1Samp]) # De-scale Lasso beta (regression
 #   coefficients)
LtauSamp = tauhat * exp(cvtauhat * samplesto[,dSamp])
SsumSamp = apply((rep(LassoY, nBlockSamp) - LassoX %*% LbetaSamp)^2,
 2, sum)
TsumSamp = apply(abs(LbetaSamp[2:d1Samp,]), 2, sum) # Omit the
#   intercept coefficient.
hist(1 / LtauSamp, 50, xlab = "Sample values",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sigma parameter, n = ", nBlockSamp, sep = ""))
#lines(1 / c(tauhat, tauhat), c(0, nBlockSamp), lty = 2)
#lines(1 / c(Qtau["2.5%"], Qtau["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["97.5%"], Qtau["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["50%"], Qtau["50%"]), c(0, nBlockSamp), lty = 5)
hist(SsumSamp / 1000, 50, xlab = "Sample values (× 1000)",
 xaxs = "i", xlim = c(1266, 1374),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of squares, n = ", nBlockSamp, sep = ""))
#lines(c(Shat, Shat) / 1000, c(0, nBlockSamp), lty = 2)
#lines(c(QSsum["2.5%"], QSsum["2.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["97.5%"], QSsum["97.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["50%"], QSsum["50%"]) / 1000, c(0, nBlockSamp), lty = 5)
hist(TsumSamp, 50, xlab = "Sample values",
 xaxs = "i", xlim = c(75, 385),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""))
#lines(c(That, That), c(0, nBlockSamp), lty = 2)
#lines(c(QTsum["2.5%"], QTsum["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["97.5%"], QTsum["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["50%"], QTsum["50%"]), c(0, nBlockSamp), lty = 5)
# Serial trace plot of TsumSamp
plot(TsumSamp, cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:1000], cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:100], cex = 0.5, pch = 3, type = "b",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")

#################### Lambda = 2
LHmc(d = 12, Llambda = 2, npast = 170)
LHmc(d = 12, Llambda = 2, npast = 165)
# Generally around 16 to 25 points per trajectory
LHmc(d = 12, Llambda = 2, npast = 161)
LHmc(d = 12, Llambda = 2, npast = 160)
LHmc(d = 12, Llambda = 2, npast = 159)
LHmc(d = 12, Llambda = 2, npast = 158)
LHmc(d = 12, Llambda = 2, npast = 157)
LHmc(d = 12, Llambda = 2, npast = 156)
LHmc(d = 12, Llambda = 2, npast = 155)
LHmc(d = 12, Llambda = 2, npast = 154)
LHmc(d = 12, Llambda = 2, npast = 153)
LHmc(d = 12, Llambda = 2, npast = 152)
LHmc(d = 12, Llambda = 2, npast = 151) #*
LHmc(d = 12, Llambda = 2, npast = 150) # X1
# Generally around 16 to 25 points per trajectory
LHmc(d = 12, Llambda = 2, npast = 145) # X0*
LHmc(d = 12, Llambda = 2, npast = 140) # XX3
LHmc(d = 12, Llambda = 2, npast = 135) # XX3
LHmc(d = 12, Llambda = 2, npast = 130) # XX3
LHmc(d = 12, Llambda = 2, npast = 125) # XX3

LRocftp(Llambda = 2, npast = 145, nBlockAcc = 5000)
# Block  5361 , initialised  1 , accepted  5000 
# Max blocks between coalescence =  4 
save(samplesto, file = "Lasso_2_5000.RData")
#load("Lasso_2_5000.RData")
dSamp = 12
d1Samp = 11
nBlockSamp = dim(samplesto)[1]
LambdaSamp = 2
LbetaSamp = rep(betahat, nBlockSamp) +
 Ldiab %*% t(samplesto[, 1:d1Samp]) # De-scale Lasso beta (regression
 #   coefficients)
LtauSamp = tauhat * exp(cvtauhat * samplesto[,dSamp])
SsumSamp = apply((rep(LassoY, nBlockSamp) - LassoX %*% LbetaSamp)^2,
 2, sum)
TsumSamp = apply(abs(LbetaSamp[2:d1Samp,]), 2, sum) # Omit the
#   intercept coefficient.
hist(1 / LtauSamp, 50, xlab = "Sample values",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sigma parameter, n = ", nBlockSamp, sep = ""))
#lines(1 / c(tauhat, tauhat), c(0, nBlockSamp), lty = 2)
#lines(1 / c(Qtau["2.5%"], Qtau["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["97.5%"], Qtau["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["50%"], Qtau["50%"]), c(0, nBlockSamp), lty = 5)
hist(SsumSamp / 1000, 50, xlab = "Sample values (× 1000)",
 xaxs = "i", xlim = c(1266, 1374),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of squares, n = ", nBlockSamp, sep = ""))
#lines(c(Shat, Shat) / 1000, c(0, nBlockSamp), lty = 2)
#lines(c(QSsum["2.5%"], QSsum["2.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["97.5%"], QSsum["97.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["50%"], QSsum["50%"]) / 1000, c(0, nBlockSamp), lty = 5)
hist(TsumSamp, 50, xlab = "Sample values",
 xaxs = "i", xlim = c(75, 385),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""))
#lines(c(That, That), c(0, nBlockSamp), lty = 2)
#lines(c(QTsum["2.5%"], QTsum["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["97.5%"], QTsum["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["50%"], QTsum["50%"]), c(0, nBlockSamp), lty = 5)
# Serial trace plot of TsumSamp
plot(TsumSamp, cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:1000], cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:100], cex = 0.5, pch = 3, type = "b",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")

# Try beta < 1 for initial iterations.
betatrial = c(rep(1, maxpast - 90), rep(2, 90))
LHmc(d = 12, Llambda = 2, beta_in = betatrial, npast = 100) #XX6
# No advantage

#################### Lambda = 5
LHmc(d = 12, Llambda = 5, npast = 900)
# Mode around 22 points per trajectory, many in range 9 to 26, and
#   some Metropolis-Hastings rejections
LHmc(d = 12, Llambda = 5, npast = 875)
LHmc(d = 12, Llambda = 5, npast = 850)
LHmc(d = 12, Llambda = 5, npast = 825)
LHmc(d = 12, Llambda = 5, npast = 800)
LHmc(d = 12, Llambda = 5, npast = 775)
LHmc(d = 12, Llambda = 5, npast = 750)
LHmc(d = 12, Llambda = 5, npast = 725) #*
LHmc(d = 12, Llambda = 5, npast = 700) #X1
LHmc(d = 12, Llambda = 5, npast = 675) #X1
LHmc(d = 12, Llambda = 5, npast = 650)
LHmc(d = 12, Llambda = 5, npast = 625) #X1
# Around 12-27 points per trajectory but some around 6 and some
#   Metropolis-Hastings rejections
LHmc(d = 12, Llambda = 5, npast = 600) #X1
LHmc(d = 12, Llambda = 5, npast = 575) #X1
LHmc(d = 12, Llambda = 5, npast = 525) #X2
LHmc(d = 12, Llambda = 5, npast = 500) #X2*
LHmc(d = 12, Llambda = 5, npast = 475) #XX4
LHmc(d = 12, Llambda = 5, npast = 425) #XX5

#*** Runs with incorrect likelihood, including prior of 1/tau

# Try smaller time step.
LHmc(d = 12, Llambda = 5, h = 0.01, npast = 10) # Prev XX20
# Generally around 100 points per trajectory but some around 50.  No
#   rejections.  So a solution to the problem of rejections but takes
#   longer to run than putting up with the rejections.

LRocftp(Llambda = 5, npast = 650, nBlockAcc = 10) # 7 min 5 sec on laptop
# Block  14 , initialised  1 , accepted  10 
# Max blocks between coalescence =  2
# Gives TsumSamp all between 85 and 120; cf. original "That" = 165.
#   It looks like Llambda = 5 is as high as we need to go, as it
#   compresses the regression coefficients probably more than anybody
#   would want in practice.  That's lucky, as the FRUTS algorithm
#   starts to break down above that level!  Breakdown (as in require
#   an impractical number of trajectories) is probably due to
#   discontinuities from the absolute value function and frequent
#   switching of sign of the regression coefficients.
# Residual sum of squares / 1000 ranges between 1280 and 1315;
#   cf. original "Shat" / 1000 = 1264.  So some worsening in the fit
#   of the regression, but not too bad.

betatrial = c(rep(1, maxpast - 150), rep(2, 150))
LHmc(d = 12, Llambda = 5, beta_in = betatrial, npast = 200) #XX17
# No advantage from beta = 1 over beta = 2.

#*** Back to runs with correct likelihood

LRocftp(Llambda = 5, npast = 500, nBlockAcc = 5000)
save(samplesto, file = "Lasso_5_5000.RData")
# Block  6910 , initialised  1 , accepted  5000 
# Max blocks between coalescence =  6 
#load("Lasso_5_5000.RData")
dSamp = 12
d1Samp = 11
nBlockSamp = dim(samplesto)[1]
LambdaSamp = 5
LbetaSamp = rep(betahat, nBlockSamp) +
 Ldiab %*% t(samplesto[, 1:d1Samp]) # De-scale Lasso beta (regression
 #   coefficients)
LtauSamp = tauhat * exp(cvtauhat * samplesto[,dSamp])
SsumSamp = apply((rep(LassoY, nBlockSamp) - LassoX %*% LbetaSamp)^2,
 2, sum)
TsumSamp = apply(abs(LbetaSamp[2:d1Samp,]), 2, sum) # Omit the
#   intercept coefficient.
range(SsumSamp) # 1270k to 1371k
range(TsumSamp) # 79.3 to 188.6
hist(1 / LtauSamp, 50, xlab = "Sample values",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sigma parameter, n = ", nBlockSamp, sep = ""))
#lines(1 / c(tauhat, tauhat), c(0, nBlockSamp), lty = 2)
#lines(1 / c(Qtau["2.5%"], Qtau["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["97.5%"], Qtau["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(1 / c(Qtau["50%"], Qtau["50%"]), c(0, nBlockSamp), lty = 5)
hist(SsumSamp / 1000, 50, xlab = "Sample values (× 1000)",
 xaxs = "i", xlim = c(1266, 1374),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of squares, n = ", nBlockSamp, sep = ""))
#lines(c(Shat, Shat) / 1000, c(0, nBlockSamp), lty = 2)
#lines(c(QSsum["2.5%"], QSsum["2.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["97.5%"], QSsum["97.5%"]) / 1000, c(0, nBlockSamp), lty = 3)
#lines(c(QSsum["50%"], QSsum["50%"]) / 1000, c(0, nBlockSamp), lty = 5)
hist(TsumSamp, 25, xlab = "Sample values",
 xaxs = "i", xlim = c(75, 385),
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""))
#lines(c(That, That), c(0, nBlockSamp), lty = 2)
#lines(c(QTsum["2.5%"], QTsum["2.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["97.5%"], QTsum["97.5%"]), c(0, nBlockSamp), lty = 3)
#lines(c(QTsum["50%"], QTsum["50%"]), c(0, nBlockSamp), lty = 5)
# Serial trace plot of TsumSamp
plot(TsumSamp, cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:1000], cex = 0.5, pch = 3,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:100], cex = 0.5, pch = 3, type = "b",
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Sample number", ylab = "Sample value")
plot(TsumSamp[1:(nBlockSamp - 1)], TsumSamp[2:nBlockSamp],
 pch = 3, cex = 0.5, asp = 1,
 main = paste("Bayesian Lasso, lambda = ", LambdaSamp,
 ", sum of abs(beta), n = ", nBlockSamp, sep = ""),
 xlab = "Values 1 to n – 1", ylab = "Values 2 to n")
cor(TsumSamp[1:(nBlockSamp - 1)], TsumSamp[2:nBlockSamp])
# 0.00958565: standard deviation of this = 1 / sqrt(5000) = 0.01414214.
cov(cbind(TsumSamp[1:(nBlockSamp - 1)], TsumSamp[2:nBlockSamp]))

#################### Lambda = 10
# No run with Llambda >= 10 coalesced.
LHmc(d = 12, Llambda = 10, npast = 2000) #XX18
# Mode of number of points per trajectory is about 22 but there are
#   plenty of trajectories with numbers of points in the range 2 to 5,
#   and some Metropolis-Hastings rejections.  We would really need a
#   smaller value of h to make these work (e.g., h = 0.01 or 0.005).
#   Whether we use that or not, it's becoming intractable for a
#   stock-standard computer with unoptimised code.

# Note: The following runs were made with the previous version of the
#   code, in which the likelihood was incorrect.  It mistakenly
#   included the prior of 1/tau instead of having no prior for
#   log(tau).  It makes very little difference.

LHmc(d = 12, Llambda = 100, npast = 1000) #XX20
LHmc(d = 12, Llambda = 10, npast = 1000) #XX20
# Shows some promise, even though no run coalesced from all starting
#   points.
# Average around 15 points per trajectory, although some values are
#   very low (1 to 6).

# Try smaller time step.
LHmc(d = 12, Llambda = 10, h = 0.01, npast = 2000) #XX16
# Pretty good at about 100 points per trajectory, although some
#   trajectories have 8-25 points.  The value lambda = 10 looks
#   impractical; would need > 2000 trajectories in combination with h
#   = 0.01.
LHmc(d = 12, Llambda = 10, h = 0.01, npast = 1000) #XX20
# Generally about 100 points per trajectory but some down around 8 to
#   18.  Very few rejections.  So that's all OK.  But it looks like we
#   still won't get coalescence for lambda = 10 with any reasonable
#   number of trajectories.
LHmc(d = 12, Llambda = 10, h = 0.01, npast = 10) #XX20
# About 100 points per trajectory, some around 50, no rejections.
