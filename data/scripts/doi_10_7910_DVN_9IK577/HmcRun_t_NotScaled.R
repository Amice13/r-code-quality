######################################################################
# R script for running Hamiltonian Monte Carlo coalscence code for the
#   unscaled multivariate t-distribution, up to 100 dimensions, using
#   the Full Random Uniform Trajectory Sampler (FRUTS)
# By "unscaled" we mean that it is derived directly from a standard
#   normal distribution.  These results were not reported in the paper
#   because the situation is not realistic.  Our recommended usage is
#   to scale by the square-root of the Hessian matrix at the
#   maximum-likelihood point.
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

######################################## Multivariate t-distribution
# Parameter nu = degrees of freedom; small value (e.g., 4) =
#   long-tailed distribution.
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 100)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 90)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 80)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 70)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 60)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 50)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 40)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 30)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 29)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 28)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 27)
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 26) #*
# Mostly around 20 points per trajectory, some around 50
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 25) #X2
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 24) #X*
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 23) #XX3
Hmc(Ufunc_t, Uderiv_t, d = 1, nu = 4, npast = 20) #XX3

Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 100)
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 90)
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 80)
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 70)
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 60)
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 59)
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 58) #*
# Mostly around 20 points per trajectory, some around 50
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 57) #X2
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 56) #X2
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 55) #X1
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 54) #X2
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 53) #X1*
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 52) #XX4
Hmc(Ufunc_t, Uderiv_t, d = 2, nu = 4, npast = 50) #XX3

Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 300)
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 290)
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 280)
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 270)
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 250) #*
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 245) #X1
# Many trajectories around 20 points, some around 50, some over 100
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 240) #X2
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 230) #X2*
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 225) #XX3
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 220) #XX3
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 200) #XX3
Hmc(Ufunc_t, Uderiv_t, d = 10, nu = 4, npast = 100) #XX14

Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 2000)
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 1900) #*
# Most trajectories around 20 points, some around 50, a few around 100
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 1800) #X1
# Most trajectories around 20 points, some around 50
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 1700) #X1
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 1600) #X2*
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 1500) #XX3
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, npast = 1000) #XX12

# Try varying alpha to change the number of points per trajectory.
# Took too long to run: must have had huge numbers of points per trajectory.
# Above results show that there is no real need for it anyway.
#   Numbers of points with alpha = 2 are not too far away from our
#   goal of 20.
Hmc(Ufunc_t, Uderiv_t, d = 100, nu = 4, alpha = 0.5, npast = 100)

# Try varying beta to reduce the number of trajectories required for
#   coalescence.  Conclusion: didn't work.
betatr = c(rep(0.5, maxpast - 500), rep(2, 500))
Hmc(Ufunc_t, Uderiv_t, beta_in = betatr, d = 100, nu = 4, npast = 1000) #XX7
# Trajectories still around 20 points when beta = 0.5, some 50-100
#   early on
Hmc(Ufunc_t, Uderiv_t, beta_in = betatr, d = 100, nu = 4, npast = 600) #XX17

betatr = c(rep(0.5, maxpast - 1000), rep(2, 1000))
Hmc(Ufunc_t, Uderiv_t, beta_in = betatr, d = 100, nu = 4, npast = 1100) #XX9
