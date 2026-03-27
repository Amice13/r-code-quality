######################################################################
# R code for Fisher's Exact Test to analyse cod tagging data reported
#   by Lear 1984, and Barrowman & Myers 1996
# George Leigh, begun 201409, Fisher's Exact Test begun 20170810, code
#   modified from the Bayesian version "CodData.R"
# Tag type A = Petersen discs, tag type B = spaghetti tags
# R version 3.2.1 (2015-06-18), Microsoft Windows 7 Enterprise

######################################## Setup
dfRel = read.csv("CodDataReleases.csv", header = TRUE)
dfRec = read.csv("CodDataRecoveries.csv", header = TRUE)

NAA = dfRel$Number[dfRel$TagsReleased == "AA"]
N_A = dfRel$Number[dfRel$TagsReleased == "A"] # NA is a reserved word.
NAB = dfRel$Number[dfRel$TagsReleased == "AB"]
NBB = dfRel$Number[dfRel$TagsReleased == "BB"]
NB = dfRel$Number[dfRel$TagsReleased == "B"]

rAA_AA = sum(dfRec$Number[dfRec$TagsReleased == "AA" &
 dfRec$TagsRecovered == "AA"])
rAA_A = sum(dfRec$Number[dfRec$TagsReleased == "AA" &
 dfRec$TagsRecovered == "A"])
rAB_AB = sum(dfRec$Number[dfRec$TagsReleased == "AB" &
 dfRec$TagsRecovered == "AB"])
rAB_A = sum(dfRec$Number[dfRec$TagsReleased == "AB" &
 dfRec$TagsRecovered == "A"])
rAB_B = sum(dfRec$Number[dfRec$TagsReleased == "AB" &
 dfRec$TagsRecovered == "B"])
rBB_BB = sum(dfRec$Number[dfRec$TagsReleased == "BB" &
 dfRec$TagsRecovered == "BB"])
rBB_B = sum(dfRec$Number[dfRec$TagsReleased == "BB" &
 dfRec$TagsRecovered == "B"])
rA_A = sum(dfRec$Number[dfRec$TagsReleased == "A" &
 dfRec$TagsRecovered == "A"])
rB_B = sum(dfRec$Number[dfRec$TagsReleased == "B" &
 dfRec$TagsRecovered == "B"])

######################################## Test for whether A and B tags
#   shed at equal rates (sections 3.1 and 4.3 of the paper)
# NB: This ignores the AA and BB releases.  The only two-tag releases
#   are the AB ones.

#################### Double-tagging component only, ignore
#   single-tagging component for now.
# The test is an exact binomial test for this case.  Using the spirit
#   of the Fisher test, we fix the total rAB_A + rAB_B and find a
#   significance level for the split into rAB_A and rAB_B.
c(rAB_A / NAB, rAB_B / NAB)
# Arrange the programming to minimise the number of terms we need;
#   this is given by r1 <= r2 below.
if (rAB_A < rAB_B) {
 r1 = rAB_A
 r2 = rAB_B
} else {
 r1 = rAB_B
 r2 = rAB_A
}
n1 = r1 + r2
# Calculate first term in the probability sum.  We expect this to be
#   the biggest term: the other events should be less likely.
# We use logs through here for numerical stability in case some of the
#   "choose" function values are very large.  Also use the first term
#   as an offset and calculate the other terms relative to it.
# By far the easiest way to do this is to use the R function "pbinom"
#   to do this.  For the sake of completeness I'm doing it the long
#   way to make the code more consistent with the use of Fisher's
#   Exact Test below.  We'll use "pbinom" to check it.
LogProbNum = lchoose(n1, r1) # Numerator only
Prob = exp(LogProbNum - n1 * log(2.0)) # Include denominator
ProbTotUnscal = 1.0
# Add the probabilities of less likely events, i.e., number of returns
#   of type 1 < r1.
# i is the *difference* in the number of fish recovered compared to
#   the first term.
cat(Prob, "\n")
for (i in 1:min(r1, n1 - r2)) { # Could just use r1 here but we'll be
 #   consistent.
 # Denominator is the same for all i so is excluded.
 ProbTotUnscal = ProbTotUnscal + exp(lchoose(n1, r1 - i) - LogProbNum)
 cat(i, " ", ProbTotUnscal, "\n") # For checking
}
ProbTot = ProbTotUnscal * Prob
cat(ProbTot, " ", pbinom(r1, n1, 0.5), "\n")
# 0.1148

#################### Now include the single-tagging component.
# Decide direction of test by overall proportions of fish returned.
# Type 1 is defined as the tag type with the lesser proportion of
#   recoveries.
# Use suffices "D" double, "S" single.
if ((rAB_AB + rAB_A + rA_A) / (NAB + N_A) <
  (rAB_AB + rAB_B + rB_B) / (NAB + NB)) {
 r1D = rAB_A
 r2D = rAB_B
 r1S = rA_A
 r2S = rB_B
 n1S = N_A
 n2S = NB
} else {
 r1D = rAB_B
 r2D = rAB_A
 r1S = rB_B
 r2S = rA_A
 n1S = NB
 n2S = N_A
}
nD = r1D + r2D
# We will not allow the test if the single-tagging and double-tagging
#   results indicate opposite directions for the test.  The theory for
#   the exact test is unclear in this case.  We need to make sure that
 #   both the hypergeometric and binomial probabilities increase with
 #   r1S and r1D.
lValid = r1D <= r2D & r1S / n1S <= r2S / n2S
if (!lValid) cat(
 "Single-tagging and double-tagging data indicate opposite directions.\n",
 "Exact test is invalid.\n")

if (lValid) {
 r1Dlim = floor(0.5 * nD) # Maximum allowed potential value for r1D
 r1Slim = floor(((r1S + r2S) / (n1S + n2S)) * n1S) # Maximum allowed
 #   potential value for r1S
 # I'm going to be lazier this time and use only the "pbinom" function
 #   to calculate cumulative binomial probabilities and "dbinom" for
 #   non-cumulative binomial probabilities.  Probabilities for the
 #   double-tagging component are binomial, whereas those for the
 #   single-tagging component are hypergeometric.
 # We need to store the non-cumulative overall probability (binomial
 #   multiplied by hypergeometric) in order to know which results are
 #   "as extreme or more extreme" (i.e., have smaller or equal
 #   probabilities) than the observed one.
 LogHyper = lchoose(n1S, r1S) + lchoose(n2S, r2S) # Calculations will
 #   be relative to this offset which is the numerator of the
 #   hypergeometric probability for the observed result.
 LogProbNum = LogHyper + dbinom(r1D, nD, 0.5, log = TRUE) # Include
 #   binomial probability (non-cumulative); this is still a numerator
 #   and will be used to judge whether other potential results are "as
 #   extreme or more extreme" than the observed one.
 Prob = exp(LogHyper - lchoose(n1S + n2S, r1S + r2S)) # Hypergeometric
 #   probability for observed result; now include denominator: this is
 #   the factor by which to scale the result at the end.
 ProbTotUnscal = pbinom(r1D, nD, 0.5) # Initial value of unscaled total
 #   probability; we will add the values for other potential choices of
 #   the hypergeometric outcome.
 # Add the probabilities of events that are as likely or less likely.
 # i and j are *differences* in the numbers of fish recovered compared
 #   to the first term: i is for fish originally double-tagged
 #   (binomial distribution), while j is for fish originally
 #   single-tagged (hypergeometric distribution).
 cat("Hypergeometric probability ", Prob, "\n")
 cat("j = 0: ", ProbTotUnscal, "\n") # For checking

 # First do cases with a greater (more likely) number of
 #   hypergeometric (single-tag) tag-type 1 returns and a lesser (less
 #   likely) number of binomial (double-tag) tag-type 1 returns.
 i = 0 # Will vary as we go; don't reset when we take a new value of j.
 for (j in 1:min(r2S, r1Slim - r1S)) {
  LogHyperCurrent = lchoose(n1S, r1S + j) + lchoose(n2S, r2S - j)
  LogProbNumCurrent = LogHyperCurrent + dbinom(r1D - i, nD, 0.5, log = TRUE)
  while (LogProbNumCurrent > LogProbNum) {
   # Increase i to make the binomial probability smaller.
   i = i + 1
   if (i > r1D) {
    break
   } else {
    LogProbNumCurrent = LogHyperCurrent + dbinom(r1D - i, nD, 0.5, log = TRUE)
   }
  } # while
  if (i > r1D) {
   break
  } else {
   ProbTotUnscal = ProbTotUnscal + exp(LogHyperCurrent - LogHyper) *
    pbinom(r1D - i, nD, 0.5)
  }
  cat("j = ", j, ": i = ", i, ": ", ProbTotUnscal, "\n") # For checking
 } # j

 # Next do cases with a lesser (less likely) number of hypergeometric
 #   (single-tag) tag-type 1 returns and a greater (more likely)
 #   number of binomial (double-tag) tag-type 1 returns.
 i = 0 # Again varies as we go; don't reset when we take a new value of j.
 for (j in 1:min(r1S, n2S - r2S)) {
  LogHyperCurrent = lchoose(n1S, r1S - j) + lchoose(n2S, r2S + j)
  LogProbNumCurrent = LogHyperCurrent + dbinom(r1D + i, nD, 0.5, log = TRUE)
  while (LogProbNumCurrent < LogProbNum & r1D + i < r1Dlim) {
   # Try the next larger value of i (which has a larger binomial
   #   probability) and check whether the combined
   #   hypergeometric-binomial probability is still small enough to be
   #   considered "as extreme or more extreme" than the observed
   #   result.
   LogProbNumTemp = LogHyperCurrent + dbinom(r1D + i + 1, nD, 0.5, log = TRUE)
   if (LogProbNumTemp > LogProbNum) {
    break
   } else {
    LogProbNumCurrent = LogProbNumTemp
    i = i + 1
   }
  } # while
  ProbTotUnscal = ProbTotUnscal + exp(LogHyperCurrent - LogHyper) *
   pbinom(r1D + i, nD, 0.5)
  cat("j = ", j, ": i = ", i, ": ", ProbTotUnscal, "\n") # For checking
 } # j

 ProbTot = Prob * ProbTotUnscal
 cat("Final combined probability ", ProbTot, "\n")
} # lValid
# 0.0008206397, cf. 0.00117 from the Bayesian version.
# We find that the A tags (Petersen discs) have a lower shedding rate
#   than the B tags (spaghetti tags).

######################################## Tests for whether tags on the
#   same fish are independent

#################### Test A tags in AB releases (sections 2.2 and 4.2
#   of the paper)
c((rAB_AB + rAB_A) / NAB, rA_A / N_A)
# Run a one-tailed Fisher test.  Arrange the programming to minimise
#   the number of terms we need; this is given by r1 <= r2 below.  We
#   will assume that only a minority of tags are recovered in any
#   tagging episode.
if ((rAB_A + rAB_AB) / NAB <= rA_A / N_A) {
 r1 = rAB_A + rAB_AB
 r2 = rA_A
 n1 = NAB
 n2 = N_A
} else {
 r1 = rA_A
 r2 = rAB_A + rAB_AB
 n1 = N_A
 n2 = NAB
}
# Calculate first term in the probability sum.  We expect this to be
#   the biggest term: the other events should be less likely.
# We use logs through here for numerical stability in case some of the
#   "choose" function values are very large.  Also use the first term
#   as an offset and calculate the other terms relative to it.
LogProbNum = lchoose(n1, r1) + lchoose(n2, r2) # Numerator only
Prob = exp(LogProbNum - lchoose(n1 + n2, r1 + r2)) # Include denominator
ProbTotUnscal = 1.0
# Add the probabilities of less likely events, i.e., number of returns
#   from component 1 < r1.
# i is the *difference* in the number of fish recovered compared to
#   the first term.
cat(Prob, "\n")
for (i in 1:min(r1, n2 - r2)) {
 ProbTotUnscal = ProbTotUnscal + exp(lchoose(n1, r1 - i) +
  lchoose(n2, r2 + i) - LogProbNum) # Denominator same for all i so excluded
 cat(i, " ", ProbTotUnscal, "\n") # For checking
}
ProbTot = ProbTotUnscal * Prob
cat("ProbTot = ", ProbTot, "\n")
# 0.11969998, not significant

#################### Test B tags in AB releases (sections 2.2 and 4.2
#   of the paper)
c((rAB_AB + rAB_B) / NAB, rB_B / NB)
# Run a one-tailed Fisher test.  Arrange the programming to minimise
#   the number of terms we need; this is given by r1 <= r2 below.  We
#   will assume that only a minority of tags are recovered in any
#   tagging episode.
if ((rAB_B + rAB_AB) / NAB <= rB_B / NB) {
 r1 = rAB_B + rAB_AB
 r2 = rB_B
 n1 = NAB
 n2 = NB
} else {
 r1 = rB_B
 r2 = rAB_B + rAB_AB
 n1 = NB
 n2 = NAB
}
# Calculate first term in the probability sum.  We expect this to be
#   the biggest term: the other events should be less likely.
# We use logs through here for numerical stability in case some of the
#   "choose" function values are very large.  Also use the first term
#   as an offset and calculate the other terms relative to it.
LogProbNum = lchoose(n1, r1) + lchoose(n2, r2) # Numerator only
Prob = exp(LogProbNum - lchoose(n1 + n2, r1 + r2)) # Include denominator
ProbTotUnscal = 1.0
# Add the probabilities of less likely events, i.e., number of returns
#   from component 1 < r1.
# i is the *difference* in the number of fish recovered compared to
#   the first term.
cat(Prob, "\n")
for (i in 1:min(r1, n2 - r2)) {
 ProbTotUnscal = ProbTotUnscal + exp(lchoose(n1, r1 - i) +
  lchoose(n2, r2 + i) - LogProbNum) # Denominator same for all i so excluded
 cat(i, " ", ProbTotUnscal, "\n") # For checking
}
ProbTot = ProbTotUnscal * Prob
cat("ProbTot = ", ProbTot, "\n")
# 0.5191462, not significant; slightly surprising that this is greater
#   than 0.5 when r1 / n1 is slightly less than r2 / n2 (0.1704 v
#   0.1712) but can happen.

#################### Test A tags in AA releases (sections 3.2 and 4.4
#   of the paper).
c((rAA_AA + 0.5 * rAA_A) / NAA, rA_A / N_A)
# Here we use a binomial split of rAA_A into two equally probable
#   categories, followed by Fisher's Exact Test.
# It's more difficult to take care of numerical precision here.  I'm
#   doing it the easy way and summing the product of binomial
#   probability with the P-value of Fisher's Exact Test.  We will,
#   however, take care of the numerical precision of the individual
#   values of Fisher's Exact Test (conditional on the binomial split).
lDir = (rAA_AA + 0.5 * rAA_A) / NAA <= rA_A / N_A # Direction of test
ProbTot = 0.0
for (r in 0:rAA_A) {
 ProbBin = dbinom(r, rAA_A, 0.5, log = FALSE)
 if (lDir) {
  r1 = rAA_AA + r
  r2 = rA_A
  n1 = NAA
  n2 = N_A
 } else { 
  r1 = rA_A
  r2 = rAA_AA + r
  n1 = N_A
  n2 = NAA
 }
 # See previous section for comments on methodology.
 LogProbNum = lchoose(n1, r1) + lchoose(n2, r2) # Numerator only
 Prob = exp(LogProbNum - lchoose(n1 + n2, r1 + r2)) # Include denominator
 ProbHyperUnscal = 1.0
 # Add the probabilities of events with number of returns from
 #   component 1 less than r1.
 # i is the *difference* in the number of fish recovered compared to
 #   the first term.
 cat(" Prob = ", Prob, " ProbBin = ", ProbBin, "\n")
 for (i in 1:min(r1, n2 - r2)) {
  ProbHyperUnscal = ProbHyperUnscal + exp(lchoose(n1, r1 - i) +
   lchoose(n2, r2 + i) - LogProbNum)
  cat("  i = ", i, ": ", ProbHyperUnscal, "\n") # For checking
 }
 ProbHyper = ProbHyperUnscal * Prob
 cat(" ProbHyper = ", ProbHyper, "\n")
 ProbTot = ProbTot + ProbBin * ProbHyper
 cat("r = ", r, ": ProbTot = ", ProbTot, "\n")
} # r
# Final value = 0.2017196

#################### Test B tags in BB releases (sections 3.2 and 4.4
#   of the paper).
c((rBB_BB + 0.5 * rBB_B) / NBB, rB_B / NB)
# Here we use a binomial split of rAA_A into two equally probable
#   categories, followed by Fisher's Exact Test.
# It's more difficult to take care of numerical precision here.  I'm
#   doing it the easy way and summing the product of binomial
#   probability with the P-value of Fisher's Exact Test.  We will,
#   however, take care of the numerical precision of the individual
#   values of Fisher's Exact Test (conditional on the binomial split).
lDir = (rBB_BB + 0.5 * rBB_B) / NBB <= rB_B / NB # Direction of test
ProbTot = 0.0
for (r in 0:rBB_B) {
 ProbBin = dbinom(r, rBB_B, 0.5, log = FALSE)
 if (lDir) {
  r1 = rBB_BB + r
  r2 = rB_B
  n1 = NBB
  n2 = NB
 } else { 
  r1 = rB_B
  r2 = rBB_BB + r
  n1 = NB
  n2 = NBB
 }
 # See previous section for comments on methodology.
 LogProbNum = lchoose(n1, r1) + lchoose(n2, r2) # Numerator only
 Prob = exp(LogProbNum - lchoose(n1 + n2, r1 + r2)) # Include denominator
 ProbHyperUnscal = 1.0
 # Add the probabilities of events with number of returns from
 #   component 1 less than r1.
 # i is the *difference* in the number of fish recovered compared to
 #   the first term.
 cat(" Prob = ", Prob, " ProbBin = ", ProbBin, "\n")
 for (i in 1:min(r1, n2 - r2)) {
  ProbHyperUnscal = ProbHyperUnscal + exp(lchoose(n1, r1 - i) +
   lchoose(n2, r2 + i) - LogProbNum)
  cat("  i = ", i, ": ", ProbHyperUnscal, "\n") # For checking
 }
 ProbHyper = ProbHyperUnscal * Prob
 cat(" ProbHyper = ", ProbHyper, "\n")
 ProbTot = ProbTot + ProbBin * ProbHyper
 cat("r = ", r, ": ProbTot = ", ProbTot, "\n")
} # r
# Final value = 0.01125009
