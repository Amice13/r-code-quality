######################################################################
# Preliminary script for running Hamiltonian Monte Carlo coalscence
#   code using the Full Random Uniform Trajectory Sampler (FRUTS)
# This script is intended to be called using the "source" function in R.
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

# This script is intended to be called using the "source" function of R.

# Parameter maxpast is the maximum number of trajectories allowed in
#   coupling from the past.  Different values of maxpast give
#   different sets of random numbers, which affects the exact
#   reproducibility of the results, but otherwise the value of this
#   parameter makes no difference to the theory.  It's value is 2000
#   for most of the results reported.  Sometimes a larger value is
#   required.

# Note default value of beta = 2 (Gaussian sampling, the only HMC
#   formulation in widespread use)

maxpast = 2000 # Other values used for t-distributions: 2500, 4000
source("Hmc.R")
source("Rocftp.R")

# Code to display diagnostics after the "Hmc" function is run: number
#   of points per trajectory and trajectories on which the
#   Metropolis-Hastings update failed.  It omits the rows that we're
#   not using.
# Comments below about the number of points per trajectory come from
#   running this line of code.
Diag = function () {
 x = cbind(acceptsto, npointsto)[((1:nrow) - 1) %% maxpast >
  maxpast - npaststo - 0.5,]
 round(cbind(x, x[, 2] / x[, 1]), 2)
}
