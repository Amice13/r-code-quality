######################################################################
# Hamiltonian Monte Carlo (HMC) code for coalescence using the Full
#   Random Uniform Trajectory Sampler (FRUTS) (pronounced like
#   "fruits" to go with "nuts")
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

# This script contains the functions that conduct HMC.  The companion
#   scripts beginning with "HmcRun" call the functions.  Results are
#   documented in there.  This script is intended to be called using
#   the "source" function of R.

nrun = 20 # Number of independent simulations
#  Moved the setting of maxpast into "HmcRun.R", as very high values
#   (over 2000) were required for the multivariate t-distribution with
#   d = 100 and nu = 4.
maxpoint = 1000 # Maximum number of points that we are allowed to
#   construct on each side of a trajectory

nsing = 4 # Number of single (scalar) random numbers per trajectory
nmult = 3 # Number of d-dimensional random vectors per trajectory.
nsing1 = nsing + 1

nrow = nrun * maxpast # Total number of trajectories that we may simulate

# Diagnostics for whether the HMC is working properly
acceptsto = rep(0, nrow) # Number of Metropolis-Hastings acceptances
#   for each trajectory, accumulated over different starting points
#   (score out of nstart)
npointsto = rep(0, nrow) # Number of points in each trajectory,
#   accumulated over different starting points (average length of
#   trajectory = npointsto / nstart)

######################################## Generic HMC function, takes
#   functions as arguments to calculate negative log-likelihood
#   (Ufunc) and its derivative in d dimensions (Uderiv).
Hmc = function(Ufunc = UfuncNorm, Uderiv = UderivNorm, alpha = 2, rho = 0,
 nu = 1000, mu = 0, Llambda = 0.237, d = 1, npast = 50,
 beta_in = rep(2, maxpast), h = 0.05, w = 0.01) {
 # Parameters:
 # - Ufunc: Function to calculate the negative log-likelihood U given q
 # - Uderiv: Function to calculate the vector derivative of U w.r.t. q
 # - alpha: Asymptotic power, so that U is proportional to |q|^alpha
 #   for large q.
 # - rho: Correlation between elements of q.  This argument is ignored
 #   for likelihoods other than the normal distribution with
 #   correlated components.
 # - nu: Degrees of freedom for multivariate t-distribution.  This
 #   argument is ignored for likelihoods other than a t-distribution.
 # - mu: Position of *second* mode of a mixture model, assumed to be
 #   positive and in the first dimension of q.  First mode is assumed
 #   to be at zero.  *Note* that this argument must be set to zero for
 #   likelihoods other than a mixture model, because it affects the
 #   setting of the starting values.
 # - Llambda: Value of Lasso lambda parameter in the Bayesian Lasso
 #   (fixed, not estimated)
 # - d: Number of dimensions
 # - npast: Number of trajectories going back into the past (<= maxpast)
 # - beta_in: Values of beta for past trajectories (length npast)
 # - h: Parameter to generate time step
 # - w: Interval width for final rounding
 pars = c(rho, nu, mu, Llambda)
 names(pars) = c("rho", "nu", "mu", "Llambda") # Shorthand mu for modepos

 muvec = rep(0, d) # Position of second mode
 muvec[1] = mu

 npaststo <<- npast # To facilitate printing diagnostics

 dlim = min(d, 5) # Limited value of d; we have 2^dlim + 1 different
 #   starting points.
 nstart = 2^dlim + 1 # %%% marks dependent on number of dimensions d:
 #   number of different starting points, in order to check that the
 #   trajectories converge to the same answers from anywhere: in the
 #   "coupling from the past" algorithm these trials all use the same
 #   random numbers.

 nsingd = nsing + d
 nsingd1 = nsingd + 1
 nsing2d = nsingd + d
 nsing2d1 = nsing2d + 1
 nsing3d = nsing2d + d
 ncol = nsing + nmult * d # Total number of random numbers per trajectory:
 # - sampling K (1)
 # - selection of a point in the trajectory (1)
 # - Metropolis-Hastings step for change in H (1)
 # - Metropolis-Hastings step for rounding of q (1)
 # - direction of p (d) (could be reduced to d - 1)
 # - direction of trajectory to gauge turn-back (d) (could be reduced
 #   to d - 1).
 # - congruence of rounded values of q (d)

 qsto = array(0, dim = c(2 * maxpoint + 1, d)) # Array for storing
 #   points in a trajectory, so that we can sample one after the set
 #   is generated
 psto = array(0, dim = c(2 * maxpoint + 1, d)) # Array for storing
 #   momentum values in a trajectory, needed for calculating H to do
 #   the Metropolis-Hastings step on the final selected point from the
 #   trajectory.  Except for point 1, the value of p will be at an
 #   integer-plus-a-half multiple of the time step.
 # Store diagnostics in global variables.
 acceptsto <<- rep(0, nrow) # Number of Metropolis-Hastings acceptances
 #   for each trajectory, accumulated over different starting points
 npointsto <<- rep(0, nrow) # Number of points in each trajectory,
 #   accumulated over different starting points
 resultsto = array(0, dim = c(nrun * nstart, d))

 betavec = rep(2, maxpast) # Initialise all to 2
 # Note that R's routine "rgamma" uses a variable number of random
 #   numbers depending on its shape parameter.  Therefore the length
 #   of "beta_in" is important even if it is greater than "npast".
 betavec[(maxpast - length(beta_in) + 1):maxpast] = beta_in # Assign to input
 betarep = rep(betavec, nrun) # Replicate the beta vector out to the
 #   number of rows (nrun * maxpast).

 # Generate random numbers
 set.seed(1)
 rsim = array(0, dim = c(nrow, ncol)) # Matrix to hold the simulations
 # I have checked that the rgamma routine correctly uses a vector for
 #   the shape parameter, even though the help doesn't mention that.
 rsim[, 2:nsing] = runif(nrow * (nsing - 1)) # Uniform distributions:
 # - column 2: # To select a point from the trajectory
 # - column 3: # Metropolis-Hastings acceptance of change in H
 # - column 4: # Metropolis-Hastings acceptance of rounding of q
 rsim[, nsing1:ncol] = rnorm(nrow * (ncol - nsing))
 # I've put the gamma random variables last, as they use a variable
 #   number of random numbers.  If put first, a change in the values
 #   in betavec would change the other columns.
 # I have also reversed the order of them, as the variable number of
 #   random numbers only seems to set in for beta < 2.
 rsim[, 1] = rev(rgamma(nrow, d / rev(betarep))) # K, gamma distribution
 
 # Convert the directions of p and the direction vector from normal
 #   random variables to directions.  Also multiply the direction unit
 #   vector of p by the magnitude calculated from K.
 f = function(x) x / sqrt(sum(x^2))
 # The cbind function below is for when d = 1, to force it into a matrix.
 rsim[, nsing1:nsingd] = rep((betarep * rsim[, 1])^(1 / betarep), d) *
  t(apply(cbind(rsim[, nsing1:nsingd]), 1, f))
 rsim[, nsingd1:nsing2d] = t(apply(cbind(rsim[, nsingd1:nsing2d]), 1, f))
 
 # Convert the rounding congruences of q from uniform on the interval
 #   [0, 1] to uniform on [0, w].
 rsim[, nsing2d1:nsing3d] = w * rsim[, nsing2d1:nsing3d]
 
 # Assign starting points %%%
 # Note terminology: Suffix "start" means start of a run which
 #   consists of multiple trajectories.  Suffix "init" means start of
 #   a trajectory.  Suffix "end" means end of a run, after executing
 #   multiple trajectories.
 qstart = array(0.0, dim = c(nrun * nstart, d))
 # Last starting point is always zeros, so no need to change the
 #   initial values for that one.
 # Deterministic starting values for the first dlim coordinates
 for (istart in 1:(2^dlim)) {
  x = istart - 1
  xdig = rep(0, dlim) # Binary digits of x
  for (j in 1:dlim) {
   xdig[j] = x %% 2
   x = x %/% 2
  }
  qstart[seq(istart, nrun * nstart + 0.1, nstart), 1:dlim] = 6.0 *
   rep(2 * xdig - 1, each = nrun)
 }
 # Random starting values for the other coordinates, if any; we use
 #   the same starting points for each run, although this doesn't have
 #   to be the case.
 if (d > dlim)
  for (istart in 1:(nstart - 1)) # Omit last starting point which is zero.
   qstart[seq(istart, nrun * nstart + 0.1, nstart), (dlim + 1):d] =
    6.0 * rep(2 * round(runif(d - dlim)) - 1, each = nrun)

 # Adjust for a bimodal distribution to make sure that the starting
 #   points that are in the direction of muvec are extreme relative to
 #   the second mode.
 # GML 20200129: Bug fixed: Need matrix multiplication here.
 l = c(sum(qstart %*% cbind(muvec))) > 0 # Positive dot product of the
 #   starting vector with muvec: then we need to make it more extreme,
 #   so that it stays extreme with respect to the second mode at muvec.
 qstart[l,] = qstart[l,] + muvec # *** marks dependent on target
 #   distribution
 
 npastround = 1 # Number of past trajectories on which to do parameter
 #   rounding: there is no point doing it until the trajectory is close
 #   to convergence, but should still be good to give it multiple
 #   opportunities to move to the rounded point, rather than only one
 #   opportunity.
 
 # Calculate time step for the leapfrog method.
 # The time step could be set adaptively but that is beyond the scope
 #   of this project.
 # Comment out old version which didn't work well for high values of d.
 #dtrep = h * betarep^(1 / betarep - 1.0) *
 # sqrt(gamma((d + 4) / betarep) / gamma((d + 2) / betarep))
 #dtrep = h * betarep^(1 / betarep - 1.0) *
 # gamma((d + 1) / betarep - 1.0) / gamma(d / betarep)
 # Still a problem with large d! Need to use log of gamma function.
 #dtrep = h * betarep^(1 / betarep - 1.0) *
 # gamma(d / betarep) / gamma(1.0 + (d - 1) / betarep)
 #dtrep = h * betarep^(1 / betarep - 1.0) *
 # exp(lgamma(d / betarep) - lgamma(1.0 + (d - 1) / betarep))
 #dtrep = sqrt(8.0) * h * betarep^(1 / betarep - 1.0) *
 # exp(lgamma(d / betarep) - lgamma(1.0 + (d - 1) / betarep) +
 #  lgamma(0.5 * (d + 1) + d / betarep) - lgamma(0.5 * d + d / betarep))
 # This formula is the best I can do.  It works outstandingly well
 #   when beta = 2 and alpha = 2; i.e., Gaussian sampling and Gaussian
 #   target distribution.
 dtrep = 2 * h * alpha^(1 / alpha) * betarep^(1 / betarep - 1.0) *
  exp(lgamma(d / betarep) - lgamma((d - 1) / betarep + 1.0) +
   lgamma(d / alpha + (d - 1) / betarep + 1.0) -
   lgamma((d - 1) / alpha + (d - 1) / betarep + 1.0))

 for (irun in 1:nrun) {
  ipastmax = irun * maxpast # Final row of random numbers for this simulation
  ipastmin = ipastmax - npast + 1 # Backwards in sequence of
  #   trajectories; random numbers further back than this are not used.
  ipastroundmin = ipastmax - npastround + 1 # Trajectories on which to
  #   do the rounding (experiment shows that we only want the last one)
  for (istart in 1:nstart) {
   q = qstart[(irun - 1) * nstart + istart,] # Coordinate vector
   for (ipast in ipastmin:ipastmax) {
    dirvec = rsim[ipast, nsingd1:nsing2d] # Direction vector, FRUTS innovation
    p = rsim[ipast, nsing1:nsingd] # Momentum vector
    pdot = -Uderiv(q, pars) # ***
    beta = betarep[ipast]
    dt = dtrep[ipast]
    hdt = 0.5 * dt
    # Store initial value of q for when we go in the opposite
    #   direction, and initial values of Uinit and Hinit for use in
    #   Metropolis-Hastings steps at the end of the trajectory
    qinit = q
    Uinit = Ufunc(q, pars) # ***
    Hinit = Uinit + rsim[ipast, 1]

    # Calculate p at plus and minus one half time step.
    pplus = p + hdt * pdot
    pminus = p - hdt * pdot
    pExPlus = pplus # Value of p at the extreme point one half
    #   timestep beyond the furthest trajectory point on the positive
    #   side
    pExMinus = pminus # Same on negative side
    # Calculate the signs of the dot products of pplus and pminus with
    #   dirvec.  We don't need to worry about a zero dot product
    #   because it has probability zero.  Still, my inclination is to
    #   bias towards including extra points in the trajectory, so that
    #   we sample from as long a trajectory as possible.
    SignPlus = sum(pplus * dirvec)
    SignMinus = sum(pminus * dirvec)
    SignMid = sum(p * dirvec) # Only needed if following test fails,
    #   but convenient to do here anyway
    # We will store the trajectory points from the lowest index at the
    #   extreme of the negative side to highest index at the extreme
    #   of the positive side.  These may need to be switched at the
    #   end to ensure that the trajectory increments make positive dot
    #   products with the direction vector.  This is recorded in the
    #   logical variable lSwitchNumbering.
    if (SignPlus * SignMinus >= 0.0) { # Do both sides of trajectory.
     lDoPlus = TRUE # Whether to do the plus side
     lDoMinus = TRUE # Whether to do the minus side
     lTermPlus = FALSE # Whether plus side has terminated yet
     lTermMinus = FALSE # Whether minus side has terminated yet
     lSwitchNumbering = SignPlus <= 0.0
    } else if (SignPlus * SignMid >= 0.0) {
     lDoPlus = TRUE
     lDoMinus = FALSE
     lTermPlus = FALSE
     lTermMinus = TRUE
     lSwitchNumbering = SignPlus <= 0.0
    } else { # SignPlus * SignMid < 0.0
     lDoPlus = FALSE
     lDoMinus = TRUE
     lTermPlus = TRUE
     lTermMinus = FALSE
     lSwitchNumbering = SignMinus <= 0.0
    }

    # Variables firstpoint and lastpoint record the first point
    #   (furthest on the negative side) and last point (furthest on
    #   the positive side) of the trajectory.  They will be switched
    #   if lSwitchNumbering is true, so that progressing from
    #   firstpoint to lastpoint always makes positive dot products
    #   with the direction vector.
    origpoint = maxpoint + 1 # Trajectory origin == halfway through
    #   storage array.
    firstpoint = origpoint
    lastpoint = origpoint
    limpoint = 2 * maxpoint + 1 # Limit of storage
    qsto[firstpoint,] = q # Trajectory points
    psto[firstpoint,] = p # Momentum points, needed to calculate final H
    # Along the trajectory, qsto will store values of q at integer
    #   time points 1, 2, 3, ..., and -1, -2, -3, ..., while, except
    #   for the initial point, psto will store p at integer-minus-a-
    #   half time points 0.5, 1.5, 2.5, ..., and -0.5, -1.5, -2.5,
    #   ....  The variables pExPlus and pExMinus will store the values
    #   of p half a time step *beyond* the furthest positive and
    #   negative points on the trajectory.
    # Then the value of p at an integer time point will be the average
    #   of the values at the integer-plus-or-minus-a-half points on
    #   either side.  This needs to be calculated only at the extreme
    #   points and the final, randomly selected point.
 
    # Positive side of trajectory: increment lastpoint
    if (lDoPlus) {
     for (ipoint in (origpoint + 1):limpoint) {
      # Hamiltonian dynamics
      qdot = sum(pExPlus * pExPlus)^(beta / 2 - 1) * pExPlus
      q = q + dt * qdot
      pdot = -Uderiv(q, pars) # ***
      p = pExPlus + dt * pdot
      if (sum(p * dirvec) * SignPlus >= 0.0) { # Continue trajectory.
       lastpoint = ipoint
       qsto[ipoint,] = q
       psto[ipoint,] = pExPlus
       pExPlus = p
      } else { # Termination condition: use momentum at q (integer
      #   time point) (= average of p at the integer-plus-a-half time
      #   points on either side) to determine whether to include the
      #   current point.
       if (sum((pExPlus + p) * dirvec) * SignPlus >= 0.0) { # Include.
        lastpoint = ipoint
	qsto[ipoint,] = q
        psto[ipoint,] = pExPlus
	pExPlus = p
       } # Include
       lTermPlus = TRUE
       break
      } # Termination condition
     } # ipoint
    } # lDoPlus
 
    # Negative side of trajectory: decrement firstpoint
    if (lDoMinus) {
     # GML 20200217: Bug fixed: Need to reset q.
     q = qinit
     for (ipoint in seq(origpoint - 1, 1, -1)) {
      # Hamiltonian dynamics
      qdot = sum(pExMinus * pExMinus)^(beta / 2 - 1) * pExMinus
      q = q - dt * qdot
      pdot = -Uderiv(q, pars) # ***
      p = pExMinus - dt * pdot
      if (sum(p * dirvec) * SignMinus >= 0.0) { # Continue trajectory.
       firstpoint = ipoint
       qsto[ipoint,] = q
       psto[ipoint,] = pExMinus
       pExMinus = p
      } else { # Termination condition: use momentum at q (integer
      #   time point) (= average of p at the integer-plus-a-half time
      #   points on either side) to determine whether to include the
      #   current point.
       if (sum((pExMinus + p) * dirvec) * SignMinus >= 0.0) { # Include.
        firstpoint = ipoint
	qsto[ipoint,] = q
        psto[ipoint,] = pExMinus
	pExMinus = p
       } # Include
       lTermMinus = TRUE
       break
      } # Termination condition
     } # ipoint
    } # lDoMinus

    # Possible outcomes:
    # - Both sides terminated: Select the destination uniformly from
    #   the range firstpoint:lastpoint.
    # - Neither side terminated: Select the destination uniformly from
    #   the range firstpoint:lastpoint.
    # - Positive side terminated but negative side didn't: Extend the
    #   negative side using storage positions limpoint down to
    #   lastpoint + 1.  Adjust firstpoint as we go, noting that now
    #   firstpoint > lastpoint.
    # - - Negative side now terminates: Select the destination
    #     uniformly from the combined range c(firstpoint:limpoint,
    #     1:lastpoint).
    # - - Negative side still does not terminate: Discard the extra
    #     points and select the destination from the range
    #     1:lastpoint.  Selection is not uniform.  Probability of any
    #     point other than origpoint is 1 / limpoint.  Probability of
    #     selecting origpoint is (limpoint - lastpoint) / limpoint.
    #     For coalescence: Generate a random uniform variable i on the
    #     range 1:limpoint.  If i < origpoint, select point i.  If i >
    #     origpoint + (limpoint - lastpoint), select point i -
    #     (limpoint - lastpoint).  Otherwise select origpoint.
    # - Negative side terminated but positive side didn't: Extend the
    #   positive side using storage positions 1:(firstpoint - 1).
    #   Adjust lastpoint as we go, noting that now lastpoint <
    #   firstpoint.
    # - - Positive side now terminates: Select the destination
    #     uniformly from the combined range c(firstpoint:limpoint,
    #     1:lastpoint).
    # - - Positive side still does not terminate: Discard the extra
    #     points and select the destination from the range
    #     firstpoint:limpoint.  Selection is not uniform.  Probability
    #     of any point other than origpoint is 1 / limpoint.
    #     Probability of selecting origpoint is (limpoint - lastpoint)
    #     / limpoint.  For coalescence: Generate a random uniform
    #     variable i on the range 1:limpoint.  If i > origpoint,
    #     select point i.  If i < origpoint - (firstpoint - 1), select
    #     point i + firstpoint - 1.  Otherwise select origpoint.
    # We have to count the points in reverse order if lSwitchNumbering
    #   is true.

    # I like a clear flow of ideas in the code.  Therefore we will
    #   first generate the extra points if necessary, and only after
    #   that use our random number to select the destination point.
    # Note that we have set lTermPlus to true if lDoPlus is false
    #   (i.e., no positive side to trajectory), and lTermMinus to true
    #   if lDoMinus is false (no negative side to trajectory).
    # The condition lastpoint < limpoint below is necessary only
    #   because R's "seq" function is silly and produces an error
    #   where it should produce an empty range.
    lExtendMinus = lTermPlus & !lTermMinus & lastpoint < limpoint
    lExtendPlus = lTermMinus & !lTermPlus & firstpoint > 1

    if (lExtendMinus) { # Extend on negative side.
     q = qsto[firstpoint,]
     for (ipoint in seq(limpoint, lastpoint + 1, -1)) {
      # Hamiltonian dynamics
      qdot = sum(pExMinus * pExMinus)^(beta / 2 - 1) * pExMinus
      q = q - dt * qdot
      pdot = -Uderiv(q, pars) # ***
      p = pExMinus - dt * pdot
      if (sum(p * dirvec) * SignMinus >= 0.0) { # Continue trajectory.
       firstpoint = ipoint
       qsto[ipoint,] = q
       psto[ipoint,] = pExMinus
       pExMinus = p
      } else { # Termination condition: use momentum at q (integer
      #   time point) (= average of p at the integer-plus-a-half time
      #   points on either side) to determine whether to include the
      #   current point.
       if (sum((pExMinus + p) * dirvec) * SignMinus >= 0.0) { # Include.
        firstpoint = ipoint
	qsto[ipoint,] = q
        psto[ipoint,] = pExMinus
	pExMinus = p
       } # Include
       lTermMinus = TRUE
       break
      } # Termination condition
     } # ipoint
    } # lExtendMinus

    if (lExtendPlus) { # Extend on positive side.
     q = qsto[lastpoint,]
     for (ipoint in 1:(firstpoint - 1)) {
      # Hamiltonian dynamics
      qdot = sum(pExPlus * pExPlus)^(beta / 2 - 1) * pExPlus
      q = q + dt * qdot
      pdot = -Uderiv(q, pars) # ***
      p = pExPlus + dt * pdot
      if (sum(p * dirvec) * SignPlus >= 0.0) { # Continue trajectory.
       lastpoint = ipoint
       qsto[ipoint,] = q
       psto[ipoint,] = pExPlus
       pExPlus = p
      } else { # Termination condition: use momentum at q (integer
      #   time point) (= average of p at the integer-plus-a-half time
      #   points on either side) to determine whether to include the
      #   current point.
       if (sum((pExPlus + p) * dirvec) * SignPlus >= 0.0) { # Include.
        lastpoint = ipoint
	qsto[ipoint,] = q
        psto[ipoint,] = pExPlus
	pExPlus = p
       } # Include
       lTermPlus = TRUE
       break
      } # Termination condition
     } # ipoint
    } # lExtendPlus

    # Sample our point from the trajectory.  It will have index ipoint.
    # Also need to record whether the sampled point is from the
    #   positive or negative side of the trajectory.  Both lSamplePlus
    #   and lSampleMinus will remain false if the sampled point is the
    #   original point.
    lSamplePlus = FALSE
    lSampleMinus = FALSE

    # Easy case: both terminated or both failed to terminate: uniform
    #   sampling.  We have to check whether firstpoint <= lastpoint.
    if ((lTermPlus & lTermMinus) | (!lTermPlus & !lTermMinus)) {
     if (firstpoint <= lastpoint) {
      npoint = lastpoint - firstpoint + 1
      if (!lSwitchNumbering) {
       # min function is for case random number exactly == 1.
       ipoint = firstpoint + min(floor(npoint * rsim[ipast, 2]), npoint - 1)
      } else {
       ipoint = lastpoint - min(floor(npoint * rsim[ipast, 2]), npoint - 1)
      }
      lSamplePlus = ipoint > origpoint
      lSampleMinus = ipoint < origpoint
     } else { # Code is the same whether lExtendMinus or lExtendPlus is true
      npoint = limpoint - firstpoint + 1 + lastpoint
      if (!lSwitchNumbering) {
       ipoint = firstpoint + min(floor(npoint * rsim[ipast, 2]), npoint - 1)
       ipoint = (ipoint - 1) %% limpoint + 1 # Stay in range.
      } else {
       ipoint = lastpoint - min(floor(npoint * rsim[ipast, 2]), npoint - 1)
       ipoint = (ipoint - 1) %% limpoint + 1
      }
      if (firstpoint <= origpoint) { # Positive side wrapped around.
       lSamplePlus = (ipoint > origpoint) | (ipoint <= lastpoint)
       lSampleMinus = (ipoint < origpoint) & (ipoint >= firstpoint)
      } else { # firstpoint > origpoint: Negative side wrapped around.
       lSamplePlus = (ipoint > origpoint) & (ipoint <= lastpoint)
       lSampleMinus = (ipoint < origpoint) | (ipoint >= firstpoint)
      }
     } # firstpoint > lastpoint
    } else {

     # Difficult case: one side terminated but the other didn't.  But
     #   at least we exclude the extra points, so don't have to loop
     #   around.  This case is undesirable because it gives a
     #   substantial probability to staying in the sample place
     #   (destination = origin), but it is necessary for detailed
     #   balance.  Hence we want to set maxpoint high to maximise the
     #   probability of terminating on both sides.  And, of course, we
     #   want to choose the time step accurately so as not to get too
     #   many points in the trajectory.
     # First remove the extra points.
     if (lExtendPlus & lastpoint < firstpoint) {
      lastpoint = limpoint
      pExPlus = psto[1,]
     }
     if (lExtendMinus & firstpoint > lastpoint) {
      firstpoint = 1
      pExMinus = psto[limpoint,]
     }

     # Proceed with sampling.
     npoint = lastpoint - firstpoint + 1 # Note: Don't sample uniformly.
     nsurplus = limpoint - npoint
     if (!lSwitchNumbering) {
      ipoint = firstpoint + min(floor(limpoint * rsim[ipast, 2]), limpoint - 1)
      if (ipoint > origpoint) {
       if (ipoint <= origpoint + nsurplus) {
        ipoint = origpoint
       } else {
        ipoint = ipoint - nsurplus
       }
      } # No change if ipoint <= origpoint
     } else { # lSwitchNumbering
      ipoint = lastpoint - min(floor(limpoint * rsim[ipast, 2]), limpoint - 1)
      if (ipoint < origpoint) {
       if (ipoint >= origpoint - nsurplus) {
        ipoint = origpoint
       } else {
        ipoint = ipoint + nsurplus
       }
      } # No change if ipoint >= origpoint
     }
    } # Difficult case

    q = qsto[ipoint,]
    p = psto[ipoint,] # Integer-minus-a-half time point except at origpoint

    # Need to adjust p to the full-integer time point.
    if (lSamplePlus) { # ipoint > origpoint except that it may wrap around.
     if (ipoint == lastpoint) {
      p = 0.5 * (p + pExPlus)
     } else if (ipoint == limpoint) { # Wrapped around on positive side
      p = 0.5 * (p + psto[1,])
     } else {
      p = 0.5 * (p + psto[ipoint + 1,])
     }
    }
    if (lSampleMinus) { # ipoint < origpoint except that it may wrap around.
     if (ipoint == firstpoint) {
      p = 0.5 * (p + pExMinus)
     } else if (ipoint == 1) { # Wrapped around on negative side
      p = 0.5 * (p + psto[limpoint,])
     } else {
      p = 0.5 * (p + psto[ipoint - 1,])
     }
    }
 
    # Do Metropolis-Hastings update to account for the changed value of
    #   H: H is preserved in continuous time but not when we discretise
    #   it.  H represents negative log-likelihood, so we have a penalty
    #   (probability of acceptance less than 1) if H has increased.
    U = Ufunc(q, pars) # ***
    H = U + sum(p * p)^(beta / 2) / beta
    paccept = min(exp(Hinit - H), 1)
    if (rsim[ipast, 3] > paccept) { # Sample has been rejected.
     q = qinit # p will be resampled so we don't have to restore it.
     U = Uinit
     #cat(Hinit, " ", H, "\n")
    } else {
     acceptsto[ipast] <<- acceptsto[ipast] + 1
     npointsto[ipast] <<- npointsto[ipast] + npoint
    }
 
    # Do rounding step on q when we are near the end of the set of
    #   trajectories.
    if (ipast >= ipastroundmin) {
     qnew = w * (q %/% w) + rsim[ipast, nsing2d1:nsing3d]
     Unew = Ufunc(qnew, pars) # ***
     pacceptround = min(exp(U - Unew), 1)
     if (rsim[ipast, 4] <= pacceptround) # Rounded sample has been accepted.
      q = qnew
    } # ipast >= ipastroundmin
   } # ipast
   resultsto[(irun - 1) * nstart + istart,] = q
   # Diagnostic to store the whole final trajectory: usually comment
   #   out the following line.
   #trajsto[[(irun - 1) * nstart + istart]] <<- qsto[firstpoint:lastpoint,]
  } # istart
 } # irun
 
 g1 = function(x) c(0, diff(x))
 g2 = function(x) max(abs(x))
 g3 = function(x) sum(x) > 1e-5
 y = apply(apply(resultsto, 2, g1), 1, g2)
 y[(1:length(y)) %% nstart == 1] = 0
 #t(array(resultsto, dim = c(nstart, nrun))) # Suitable for 1-D only
 z = cbind(resultsto, round(y, 6))
 print(z)
 cat(sum(tapply(y, rep(1:nrun, each = nstart), g3)),
  " runs failed to coalesce\n")
 invisible(resultsto)
} # Hmc

######################################## Standard normal distribution
#   in d dimensions
UfuncNorm = function(q, pars) 0.5 * sum(q^2)
UderivNorm = function(q, pars) q

######################################## Normal distribution with unit
#   variances but correlation between components
# Formula:
# - variance matrix V = (1 - rho) * I + rho * ones
# - inverse variance matrix V^-1 = (I - (rho / (1 + (d - 1) * rho)) *
#   ones) / (1 - rho)
# Obviously in practice we would try to scale q to remove the
#   correlations, but for this trial, as a test of the method, we are
#   not scaling anything.
UfuncCorr = function(q, pars) {
 d = length(q)
 rho = pars["rho"]
 0.5 * (sum(q^2) - rho * sum(q)^2 / (1.0 + (d - 1) * rho)) / (1.0 - rho)
}

UderivCorr = function(q, pars) {
 d = length(q)
 rho = pars["rho"]
 (q - rho * sum(q) / (1.0 + (d - 1) * rho)) / (1.0 - rho)
}

######################################## Multivariate t distribution
#   in d dimensions, unscaled (Gaussian variance matrix from which it
#   is derived = I)
Ufunc_t = function(q, pars) {
 d = length(q)
 nu = pars["nu"]
 0.5 * (nu + d) * log(1.0 + sum(q^2) / nu)
}

Uderiv_t = function(q, pars) {
 d = length(q)
 nu = pars["nu"]
 (nu + d) * q / (nu + sum(q^2))
}

######################################## Multivariate t distribution
#   in d dimensions, scaled by square root of Hessian at the
#   maximum-likelihood point
Ufunc_tscal = function(q, pars) {
 d = length(q)
 nu = pars["nu"]
 0.5 * (nu + d) * log(1.0 + sum(q^2) / (nu + d))
}

Uderiv_tscal = function(q, pars) {
 d = length(q)
 nu = pars["nu"]
 (nu + d) * q / (nu + d + sum(q^2))
}

######################################## Mixture of standard normal
#   distributions
UfuncNormMix = function(q, pars) {
 mu = pars["mu"]
 muvec = rep(0, length(q))
 muvec[1] = mu
 # Code that is explanatory but susceptible to numerical underflow,
 #   commented out
 #Exp1 = exp(-0.5 * sum(q^2))
 #Exp2 = exp(-0.5 * sum((q - muvec)^2))
 #ExpSum = Exp1 + Exp2
 #-log(ExpSum)

 # Code that allows for underflow
 # Find the q-vector with minimum norm.  Include the factor of 0.5
 #   in the norm, for convenience.
 qNorm = 0.5 * c(sum(q^2), sum((q - muvec)^2))
 imin = which.min(qNorm)
 imax = 3 - imin
 qNormMin = qNorm[imin]
 qNormMax = qNorm[imax]
 Exp2 = exp(qNormMin - qNormMax) # OK if underflow
 qNormMin - log(1.0 + Exp2)
}

UderivNormMix = function(q, pars) {
 mu = pars["mu"]
 muvec = rep(0, length(q))
 muvec[1] = mu
 # Code that is explanatory but susceptible to numerical underflow,
 #   commented out
 #Exp1 = exp(-0.5 * sum(q^2))
 #Exp2 = exp(-0.5 * sum((q - muvec)^2))
 #ExpSum = Exp1 + Exp2
 #(q * Exp1 + (q - muvec) * Exp2) / ExpSum

 # Code that allows for underflow
 # Find the q-vector with minimum norm.  Include the factor of 0.5
 #   in the norm, for convenience.
 qmat = rbind(q, q - muvec)
 qNorm = 0.5 * diag(qmat %*% t(qmat))
 imin = which.min(qNorm)
 imax = which.max(qNorm)
 qmin = qmat[imin,]
 qmax = qmat[imax,]
 qNormMin = qNorm[imin]
 qNormMax = qNorm[imax]
 Exp2 = exp(qNormMin - qNormMax) # OK if underflow
 (qmin + qmax * Exp2) / (1.0 + Exp2)
}

######################################## Bayesian Lasso with Efron et
#   al. 2004 diabetes data
UfuncLasso = function(q, pars) {
 # Additional global variables used that are not passed as parameters:
 # - LassoX: design matrix
 # - LassoY: y (dependent) variable
 # - betahat: OLS estimates of regression coefficients
 # - tauhat: estimate of parameter tau = 1 / sigma
 # - Ldiab: lower triangular de-scaling matrix from q[1:(d - 1)] to
 #   regression coefficients
 # - cvtauhat: coefficient of variation of tauhat; q[d] is the scaled
 #   log of tau (see calculation of Ltau below).
 # - LassoDf: number of degrees of freedom for the model, = number of
 #   data records + number of Lasso parameters.  We would subtract 1
 #   if using tau as a model parameter but not if using log(tau) which
 #   is what we do.
 d = length(q)
 d1 = d - 1
 Llambda = pars["Llambda"]
 Lbeta = betahat + Ldiab %*% cbind(q[1:d1]) # De-scale Lasso beta
 #   (regression coefficients)
 Ltau = tauhat * exp(cvtauhat * q[d])
 Ssum = sum((LassoY - LassoX %*% Lbeta)^2)
 Tsum = sum(abs(Lbeta[2:d1])) # Omit the intercept coefficient.
 # We could remove the log function below, but there is so much chance
 #   of introducing a bug that I haven't done it.
 Ltau * (0.5 * Ltau * Ssum + Llambda * Tsum) - LassoDf * log(Ltau)
}

UderivLasso = function(q, pars) {
 d = length(q)
 d1 = d - 1
 Llambda = pars["Llambda"]
 Lbeta = betahat + Ldiab %*% cbind(q[1:d1])
 signbeta = sign(Lbeta)
 signbeta[1] = 0 # Remove intercept parameter from Lasso.
 Ltau = tauhat * exp(cvtauhat * q[d])
 dtaudq = cvtauhat * Ltau # Derivative of Ltau w.r.t. q[d]
 Ssum = sum((LassoY - LassoX %*% Lbeta)^2)
 Tsum = sum(abs(Lbeta[2:d1])) # Omit the intercept coefficient.
 c(t((Llambda * Ltau) * signbeta - Ltau^2 * t(LassoX) %*%
  (LassoY - LassoX %*% Lbeta)) %*% Ldiab,
  dtaudq * (Ltau * Ssum + Llambda * Tsum - LassoDf / Ltau))
}
