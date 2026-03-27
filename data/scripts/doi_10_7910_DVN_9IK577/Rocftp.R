######################################################################
# Read-once coupling from the past (ROCFTP) for Hamiltonian Monte
#   Carlo (HMC) using the Full Random Uniform Trajectory Sampler
#   (FRUTS) (pronounced like "fruits" to go with "nuts")
# Author: George Leigh
# Copyright 2020 George Leigh
# Associated journal paper: Leigh, G. M., Campbell, A. B. and
#   Northrop, A. R. (2020).  "Hamiltonian Monte Carlo and coalescence
#   towards perfect simulation of continuous distributions".
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

# Most of this script consists of the functions that conduct ROCFTP.
#   The functions to evaluate negative log-likelihood functions and
#   their derivatives are in "Hmc.R".  The code that calls ROCFTP is
#   in the scripts beginning with "HmcRun".  Results are documented
#   there.  Some of the variables used in this script are assumed to
#   have been already set in either an HmcRun script or in Hmc.R.
# This script is intended to be run using the "source" function of R.

# Idea of ROCFTP (Wilson, 2000, "How to couple from the past using a
#   read-once source of randomness", Random Structures & Algorithms
#   vol 16 pp. 85-113):
# Set the number of trajectories per block, denoted npast here, from
#   some exploratory analysis.  For this project, the exploratory
#   analysis is documented in the script "HmcRun.R" which calls
#   functions in "Hmc.R".  The number of trajectories is one that we
#   think will produce coalescence most of the time.  For this
#   project, "coalescence" means convergence to the same answers from
#   a fixed set of different starting points (currently set to at most
#   33, comprising the maximum likelihood point plus 32 extreme
#   points; fewer starting points if the number of dimensions d < 5).
# Initialisation: Generate one block of random numbers and run the
#   algorithm.  If it doesn't coalesce, generate another block.
#   Repeat until a block coalesces.  Take the coalesced point as the
#   initial primary chain point for the reported results.
# Generation of results: Generate a block of random numbers and run
#   the algorithm on both the primary chain point and the fixed set of
#   starting points.  If it coalesces from the fixed set of starting
#   points, accept the current point immediately *before* that block
#   began as a result.  If it doesn't coalesce, repeat until one block
#   does coalesce.  Note: We do not demand that the primary chain be
#   included in the coalescence.  Occasionally its value may differ
#   from the coalesced point (which is supposed to be reached from
#   *any* starting point).  To demand that the primary chain be
#   included in this test would introduce a bias towards "easy" points
#   for producing coalescence.
# Repeat until the requisite number of result points have been
#   generated.  These are close to i.i.d. points from the target
#   distribution.  They would be exactly i.i.d. points from the target
#   distribution if we could include every possible starting point in
#   our fixed set of starting points, but this is not possible in
#   continuous space.  Also, although (or perhaps we should say
#   *because*) HMC is extremely good at sampling the sample space, HMC
#   appears not to be amenable to monotonicity properties that would
#   allow coalescence to be proven from every possible starting point
#   by achieving it on a finite set of starting points.

######################################## Generic HMC function, takes
#   functions as arguments to calculate negative log-likelihood
#   (Ufunc) and its derivative in d dimensions (Uderiv).
# Most of the code in the function "Rocftp" is taken from the function
#   "Hmc" in "Hmc.R".
fEuclScale = function(x) x / sqrt(sum(x^2)) # Small function for
#   Euclidean scaling, converts a vector to a unit vector.
CoalesceTol = 1.0e-6 # Tolerance for coalescence: purely to cover
#   numerical round-off error.  Coalescence is exact if the computer's
#   arithmetic is.

Rocftp = function(Ufunc = UfuncNorm, Uderiv = UderivNorm, alpha = 2, rho = 0,
 nu = 1000, mu = 0, Llambda = 0.237, d = 1, npast = 50,
 beta_in = rep(2, npast), h = 0.05, w = 0.01, nBlockAcc = 10) {
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
 # - nBlockAcc: Number of accepted blocks required; this is the
 #   required size of the sample of approximate i.i.d. points from the
 #   target distribution.
 RepToCoal = 0
 MaxRepToCoal = 0
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
 # Change from Hmc.R: Introduce another "starting point" being the
 #   actual process value.  This is the sample that we will take if
 #   the conditions of the ROCFTP algorithm are met.  In order to
 #   avoid bias, this point is not used in judging coalescence.
 #   Variable "nresult" is the number of results including the process
 #   value.
 nresult = nstart + 1

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

 # Store diagnostics in global variables.  These are summed over all
 #   blocks.
 # Change dimensions from "Hmc.R".  Use npast instead of nrow.  Use
 #   nresult as first dimensions in resulsto, instead of nrun *
 #   nstart.  For ROCFTP we have only one run and keep going until we
 #   have as many results as we need.
 acceptsto <<- rep(0, npast) # Number of Metropolis-Hastings acceptances
 #   for each trajectory, accumulated over different starting points
 npointsto <<- rep(0, npast) # Number of points in each trajectory,
 #   accumulated over different starting points
 # Change from Hmc.R: Introduce global variable samplesto, to store
 #   the answers.
 samplesto <<- array(0, dim = c(nBlockAcc, d))

 # Change from Hmc.R: Use nresult instead of nstart.
 resultsto = array(0, dim = c(nresult, d)) # Store result from each
 #   starting point.

 # Change from Hmc.R: Use npast instead of maxpast.
 betavec = rep(2, npast) # Initialise all to 2
 # Note that R's routine "rgamma" uses a variable number of random
 #   numbers depending on its shape parameter.  Therefore the length
 #   of "beta_in" is important even if it is greater than "npast".
 betavec[(npast - length(beta_in) + 1):npast] = beta_in # Assign to input
 # Remove variable "betarep" from Hmc.R.  We don't need it here.  Use
 #   "betavec" instead, as we have only one run.

 # Make random numbers repeatable.
 set.seed(1)
 # Create an array to hold the random numbers.  These will be
 #   overwritten by each block that is run.
 # Change from Hmc.R: Replace nrow by npast.
 rsim = array(0, dim = c(npast, ncol)) # Matrix to hold the simulations
 # I have checked that R's rgamma routine correctly uses a vector for
 #   the shape parameter, even though the help doesn't mention that.

 # Assign starting points %%%
 # Note terminology: Suffix "start" means start of a run which
 #   consists of multiple trajectories.  Suffix "init" means start of
 #   a trajectory.  Suffix "end" means end of a run, after executing
 #   multiple trajectories.
 # Change from Hmc.R: First dimension is nresult, not nrun * nstart.
 #   We have only one run, and we have an extra "starting point",
 #   being the process point.
 qstart = array(0, dim = c(nresult, d))
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
  # Change from Hmc.R: Dimension of qstart is reduced to nresult, not
  #   nrun * nstart.
  qstart[istart, 1:dlim] = 6.0 * 2 * xdig - 1
 }
 # Random starting values for the other coordinates, if any; we use
 #   the same starting points for each block.  This seems desirable in
 #   ROCFTP, so that the test of coalescence is consistent for all
 #   block; no bias towards starting points that make it easy to
 #   achieve coalescence.
 if (d > dlim)
  for (istart in 1:(nstart - 1)) # Omit last starting point which is
  #   zero, and the process point.
   # Change from Hmc.R: Again dimension is reduced.
   qstart[istart, (dlim + 1):d] = 6.0 * 2 * round(runif(d - dlim)) - 1

 # Change from Hmc.R: Fill in the final row of qstart.  I am, however,
 #   inclined to use the maximum likelihood point for this (i.e., all
 #   zeros), so we actually don't need to do anything!  Note that we
 #   don't start to accumulate results until we get a coalesced block,
 #   so in theory it doesn't matter what starting point we use for the
 #   process.
 # Then the last two rows will be identical in the first block.  But
 #   they will diverge in the second block, as we don't reset the
 #   process value back to the starting value.

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
 # Change from Hmc.R: Use betavec instead of betarep.  We'll retain
 #   the name dtrep.
 dtrep = 2 * h * alpha^(1 / alpha) * betavec^(1 / betavec - 1.0) *
  exp(lgamma(d / betavec) - lgamma((d - 1) / betavec + 1.0) +
   lgamma(d / alpha + (d - 1) / betavec + 1.0) -
   lgamma((d - 1) / alpha + (d - 1) / betavec + 1.0))

 lFirstBlock = FALSE # Whether we have done our first coalesced block,
 #   needed before we can start generating the answers in ROCFTP.
 CurBlockAcc = 0 # Current number of accepted blocks: keep going until
 #   we reach "nBlockAcc".
 BlockCounter = 0

 while (CurBlockAcc < nBlockAcc) {

  # Simulate random numbers.  Note that we do the gamma distributions
  #   (column 1) last, because they use a variable number of random
  #   numbers when beta < 2.  Doesn't matter so much in this script,
  #   but it did in Hmc.R.
  # Change from Hmc.R: Replace nrow by npast.
  rsim[, 2:nsing] = runif(npast * (nsing - 1)) # Uniform distributions:
  # - column 2: # To select a point from the trajectory
  # - column 3: # Metropolis-Hastings acceptance of change in H
  # - column 4: # Metropolis-Hastings acceptance of rounding of q
  # Change from Hmc.R: Replace nrow by npast.
  rsim[, nsing1:ncol] = rnorm(npast * (ncol - nsing))
  # I've put the gamma random variables last, as they use a variable
  #   number of random numbers.  If put first, a change in the values
  #   in betavec would change the other columns.
  # I have also reversed the order of them, as the variable number of
  #   random numbers only seems to set in for beta < 2.
  # Change from Hmc.R: Replace betarep by betavec and nrow by npast.
  rsim[, 1] = rev(rgamma(npast, d / rev(betavec))) # K, gamma distribution

  # Convert the directions of p and the direction vector from normal
  #   random variables to directions.  Also multiply the direction unit
  #   vector of p by the magnitude calculated from K.
  # The cbind function below is for when d = 1, to force it into a matrix.
  # Change from Hmc.R: Replace betarep by betavec.
  rsim[, nsing1:nsingd] = rep((betavec * rsim[, 1])^(1 / betavec), d) *
   t(apply(cbind(rsim[, nsing1:nsingd]), 1, fEuclScale))
  rsim[, nsingd1:nsing2d] =
   t(apply(cbind(rsim[, nsingd1:nsing2d]), 1, fEuclScale))

  # Convert the rounding congruences of q from uniform on the interval
  #   [0, 1] to uniform on [0, w].
  rsim[, nsing2d1:nsing3d] = w * rsim[, nsing2d1:nsing3d]

  # Delete the loop over "irun" from Hmc.R.  We have only one run here.
  ipastmax = npast # Final row of random numbers for this run
  ipastmin = 1 # Backwards in sequence of trajectories; random numbers
  #   further back than this are not used.
  ipastroundmin = ipastmax - npastround + 1 # Trajectories on which to
  #   do the rounding (experiment shows that we only want the last one)
  # Change from Hmc.R: Use nresult instead of nstart, so we include
  #   the process value.
  for (istart in 1:nresult) {
   # Change from Hmc.R: We have only one run.  Remove multiple of
   #   nstart from index.
   q = qstart[istart,] # Coordinate vector
   for (ipast in ipastmin:ipastmax) {
    dirvec = rsim[ipast, nsingd1:nsing2d] # Direction vector, FRUTS innovation
    p = rsim[ipast, nsing1:nsingd] # Momentum vector
    pdot = -Uderiv(q, pars) # ***
    # Change from Hmc.R: Replace betarep by betavec.
    beta = betavec[ipast]
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
     #cat(Hinit, " ", H, "\n") # Debugging statement
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
     if (rsim[ipast, 4] <= pacceptround) # Rounded step has been accepted.
      q = qnew
    } # ipast >= ipastroundmin
   } # ipast
   # Change from Hmc.R: Use simply istart as the first index, as we
   #   have only one run.
   resultsto[istart,] = q
   # Diagnostic to store the whole final trajectory: usually comment
   #   out the following line.
   #trajsto[[(irun - 1) * nstart + istart]] <<- qsto[firstpoint:lastpoint,]
  } # istart
  # Check coalescence using only the fixed starting values, not the
  #   process value (final row).
  # cbind function is needed when d = 1.
  ResultMin = apply(cbind(resultsto[1:nstart,]), 2, min)
  ResultMax = apply(cbind(resultsto[1:nstart,]), 2, max)
  RepToCoal = RepToCoal + 1
  if (max(ResultMax - ResultMin) <= CoalesceTol) {
   # We have a coalesced block!
   if (!lFirstBlock) { # This is our first block, required before
   #   generating any results.
    lFirstBlock = TRUE # Don't change CurBlockAcc, as we don't have
    #   any accepted blocks yet.
   } else { # We have a result (a quasi-i.i.d. sample).  By the ROCFTP
   #   algorithm, the result is the (vector) value of the process
   #   immediately *before* this block.
    CurBlockAcc = CurBlockAcc + 1
    # Store the process value from the *beginning* of this block.
    samplesto[CurBlockAcc,] <<- qstart[nresult,]
   }
   MaxRepToCoal = max(MaxRepToCoal, RepToCoal)
   RepToCoal = 0
  }
  # Write the process value from the *end* of this block to final row
  #   of starting values for the next block.  We do this whether we
  #   have a coalesced block or not.
  qstart[nresult,] = resultsto[nresult,] # Process value = final row;
  #   other rows are only to assess coalescence.
  BlockCounter = BlockCounter + 1
  cat("Block ", BlockCounter, ", initialised ", as.numeric(lFirstBlock),
   ", accepted ", CurBlockAcc, "\n")
 } # CurBlockAcc < nBlockAcc
 cat("Max blocks between coalescence = ", MaxRepToCoal, "\n")
} # Rocftp function
