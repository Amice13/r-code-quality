######################################################################
# R code to analyse cod tagging data reported by Lear 1984, and
#   Barrowman & Myers 1996
# George Leigh, begun September 2014
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

nsim = 1e7

######################################## Test for whether A and B tags
#   shed at equal rates (Bayesian version only) (sections 3.1 and 4.3
#   of the paper)
# NB: This ignores the AA and BB releases.  The only two-tag releases
#   are the AB ones.
nsim2 = 20 * nsim # Increase the number of simulations to give us
#   enough that are not rejected
set.seed(1)
pAB_AB = rbeta(nsim2, rAB_AB, NAB - rAB_AB)
pAB_A = (1.0 - pAB_AB) * rbeta(nsim2, rAB_A, NAB - rAB_AB - rAB_A)
pAB_B = (1.0 - pAB_AB - pAB_A) * rbeta(nsim2, rAB_B,
 NAB - rAB_AB - rAB_A - rAB_B)
u = runif(nsim2)

pAB_Aplus_Max = rA_A / N_A
pAB_Bplus_Max = rB_B / NB
# Use logs as actual numbers may be very small.
# Nll_Max is actually the *minimum* value of negative log-likelihood.
Nll_Max = -rA_A * log(pAB_Aplus_Max) -
 (N_A - rA_A) * log(1.0 - pAB_Aplus_Max) -
 rB_B * log(pAB_Bplus_Max) -
 (NB - rB_B) * log(1.0 - pAB_Bplus_Max)
Nll = -rA_A * log(pAB_A + pAB_AB) -
 (N_A - rA_A) * log(1.0 - pAB_A - pAB_AB) -
 rB_B * log(pAB_B + pAB_AB) -
 (NB - rB_B) * log(1.0 - pAB_B - pAB_AB)
uTest = Nll_Max - Nll
lTest = log(u) < uTest
lSelect = lTest & cumsum(lTest) <= nsim
mean(pAB_A < pAB_B)
sum(pAB_A < pAB_B) # Double-tagging component only
# 0.0758, 15156330 / 2e8
# Now include single-tagging components.
mean(pAB_A[lSelect] < pAB_B[lSelect])
sum(pAB_A[lSelect] < pAB_B[lSelect])
# 0.00117, 11732 / 1e7
mean(pAB_A[lTest] < pAB_B[lTest]) # Include all simulated data, not
#   just the first nsim selected values; 0.001175288, barely changes.
# We find that the A tags (Petersen discs) have a lower shedding rate
#   than the B tags (spaghetti tags).

######################################## Tests for whether tags on the
#   same fish are independent

#################### Test A tags in AB releases (sections 2.2 and 4.2
#   of the paper)
c((rAB_AB + rAB_A) / NAB, rA_A / N_A)
# Bayesian exact test
set.seed(2) # So we can reproduce the random numbers
# What I have called pAB is p^AB_A+ in the paper.
pAB = rbeta(nsim, rAB_AB + rAB_A, NAB - rAB_AB - rAB_A)
pA = rbeta(nsim, rA_A, N_A - rA_A)
sum(pAB < pA)
# 8982419
# Proportion = 0.8982419 or, using > instead of <, 0.1017581, not
#   significant.

# Approximate test using a normal distribution
mu = (rAB_AB + rAB_A) / NAB - rA_A / N_A
sigma2 = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) / (NAB^2 * (NAB + 1)) +
 rA_A * (N_A - rA_A) / (N_A^2 * (N_A + 1))
sigma = sqrt(sigma2)
-mu / sigma
pnorm(-mu / sigma) # 0.899526, fairly close to the Bayesian one, but
#   significantly different

# Frequentist version
sigma2f = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) / NAB^3 +
 rA_A * (N_A - rA_A) / N_A^3
sigmaf = sqrt(sigma2f)
-mu / sigmaf
pnorm(-mu / sigmaf) # 0.899297, indistinguishable from the one above

########## Divide all the numbers by something and see what happens  (not
#   used in paper).
Div = 20
set.seed(3)
pAB = rbeta(nsim, (rAB_AB + rAB_A) / Div, (NAB - rAB_AB - rAB_A) / Div)
pA = rbeta(nsim, rA_A / Div, (N_A - rA_A) / Div)
sum(pAB < pA) / nsim # 0.635621
# Approximate test using a normal distribution
mu = (rAB_AB + rAB_A) / NAB - rA_A / N_A
sigma2 = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) /
 (NAB^2 * (NAB / Div + 1)) + rA_A * (N_A - rA_A) / (N_A^2 * (N_A / Div + 1))
sigma = sqrt(sigma2)
pnorm(-mu / sigma) # 0.614634
# Frequentist version
sigma2f = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) / (NAB^3 / Div) +
 rA_A * (N_A - rA_A) / (N_A^3 / Div)
sigmaf = sqrt(sigma2f)
pnorm(-mu / sigmaf) # 0.612434

Div = 50
set.seed(4)
pAB = rbeta(nsim, (rAB_AB + rAB_A) / Div, (NAB - rAB_AB - rAB_A) / Div)
pA = rbeta(nsim, rA_A / Div, (N_A - rA_A) / Div)
sum(pAB < pA) / nsim # 0.611719
# Approximate test using a normal distribution
mu = (rAB_AB + rAB_A) / NAB - rA_A / N_A
sigma2 = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) /
 (NAB^2 * (NAB / Div + 1)) + rA_A * (N_A - rA_A) / (N_A^2 * (N_A / Div + 1))
sigma = sqrt(sigma2)
pnorm(-mu / sigma) # 0.575179
# Frequentist version
sigma2f = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) / (NAB^3 / Div) +
 rA_A * (N_A - rA_A) / (N_A^3 / Div)
sigmaf = sqrt(sigma2f)
pnorm(-mu / sigmaf) # 0.571688

Div = 100
set.seed(5)
pAB = rbeta(nsim, (rAB_AB + rAB_A) / Div, (NAB - rAB_AB - rAB_A) / Div)
pA = rbeta(nsim, rA_A / Div, (N_A - rA_A) / Div)
sum(pAB < pA) / nsim # 0.610079
# Approximate test using a normal distribution
mu = (rAB_AB + rAB_A) / NAB - rA_A / N_A
sigma2 = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) /
 (NAB^2 * (NAB / Div + 1)) + rA_A * (N_A - rA_A) / (N_A^2 * (N_A / Div + 1))
sigma = sqrt(sigma2)
pnorm(-mu / sigma) # 0.555649
# Frequentist version
sigma2f = (rAB_AB + rAB_A) * (NAB - rAB_AB - rAB_A) / (NAB^3 / Div) +
 rA_A * (N_A - rA_A) / (N_A^3 / Div)
sigmaf = sqrt(sigma2f)
pnorm(-mu / sigmaf) # 0.550829

#################### Test B tags in AB releases (sections 2.2 and 4.2
#   of the paper)
c((rAB_AB + rAB_B) / NAB, rB_B / NB)
# Bayesian exact test
set.seed(6) # So we can reproduce the random numbers
# What I have called pAB is p^AB_B+ in the paper.
pAB = rbeta(nsim, rAB_AB + rAB_B, NAB - rAB_AB - rAB_B)
pB = rbeta(nsim, rB_B, NB - rB_B)
sum(pAB < pB)
# 5226331
# Proportion = 0.5226331, not significant.

# Approximate test using a normal distribution
mu = (rAB_AB + rAB_B) / NAB - rB_B / NB
sigma2 = (rAB_AB + rAB_B) * (NAB - rAB_AB - rAB_B) / (NAB^2 * (NAB + 1)) +
 rB_B * (NB - rB_B) / (NB^2 * (NB + 1))
sigma = sqrt(sigma2)
pnorm(-mu / sigma) # 0.514168

# Frequentist version
sigma2f = (rAB_AB + rAB_B) * (NAB - rAB_AB - rAB_B) / NAB^3 +
 rB_B * (NB - rB_B) / NB^3
sigmaf = sqrt(sigma2f)
pnorm(-mu / sigmaf) # 0.514153

nsim = 1e7

#################### Test A tags in AA releases (sections 3.2 and 4.4
#   of the paper).
c((rAA_AA + 0.5 * rAA_A) / NAA, rA_A / N_A)
# Bayesian exact test
set.seed(7) # So we can reproduce the random numbers
pAA_AA = rbeta(nsim, rAA_AA, NAA - rAA_AA)
pAA_A = (1.0 - pAA_AA) * rbeta(nsim, rAA_A, NAA - rAA_AA - rAA_A)
pAA = pAA_AA + pAA_A # = pAA_A+ in paper
pA = rbeta(nsim, rA_A, N_A - rA_A)
sum(pAA_AA + 0.5 * pAA_A < pA)
# 8505177
# Proportion = 0.8505177, not significant.
c(mean(pAA_AA + 0.5 * pAA_A), mean(pA)) # Check against the r / N
#   ratios above; should be very similar.
# 0.1874944, 0.2179522

# Approximate test using a normal distribution
mu = (rAA_AA + 0.5 * rAA_A) / NAA - rA_A / N_A
sigma2 = (rAA_AA * (NAA - rAA_AA - rAA_A) + 0.25 * rAA_A * (NAA - rAA_A)) /
 (NAA^2 * (NAA + 1)) +
 rA_A * (N_A - rA_A) / (N_A^2 * (N_A + 1))
sigma = sqrt(sigma2)
pnorm(-mu / sigma) # 0.850792, very similar to the Bayesian one

# Frequentist version
sigma2f = (rAA_AA * (NAA - rAA_AA - rAA_A) + 0.25 * rAA_A * (NAA - rAA_A)) /
 NAA^3 + rA_A * (N_A - rA_A) / N_A^3
sigmaf = sqrt(sigma2f)
pnorm(-mu / sigmaf) # 0.850284, again very similar to the Bayesian one

#################### Test B tags in BB releases (sections 3.2 and 4.4
#   of the paper).
c((rBB_BB + 0.5 * rBB_B) / NBB, rB_B / NB)
# Bayesian exact test
set.seed(8) # So we can reproduce the random numbers
pBB_BB = rbeta(nsim, rBB_BB, NBB - rBB_BB)
pBB_B = (1.0 - pBB_BB) * rbeta(nsim, rBB_B, NBB - rBB_BB - rBB_B)
pBB = pBB_BB + pBB_B # = pBB_B+ in paper
pB = rbeta(nsim, rB_B, NB - rB_B)
sum(pBB_BB + 0.5 * pBB_B < pB)
# 54351
# Proportion = 0.0054351, highly significant.

# Approximate test using a normal distribution
mu = (rBB_BB + 0.5 * rBB_B) / NBB - rB_B / NB
sigma2 = (rBB_BB * (NBB - rBB_BB - rBB_B) + 0.25 * rBB_B * (NBB - rBB_B)) /
 (NBB^2 * (NBB + 1)) +
 rB_B * (NB - rB_B) / (NB^2 * (NB + 1))
sigma = sqrt(sigma2)
pnorm(-mu / sigma) # 0.0070318

# Frequentist version
sigma2f = (rBB_BB * (NBB - rBB_BB - rBB_B) + 0.25 * rBB_B * (NBB - rBB_B)) /
 NBB^3 + rB_B * (NB - rB_B) / NB^3
sigmaf = sqrt(sigma2f)
pnorm(-mu / sigmaf) # 0.0070845

# The normal distribution doesn't work so well near the tails of the
#   distribution; i.e., when the results are significant, which is
#   where we most want it to work.  Its significance levels are some
#   distance from the Bayesian ones which are exact.
# Likelihood-ratio test statistics (i.e., 2 * difference in
#   log-likelihood ~ chi-square) usually work better but I haven't
#   investigated them for these models.  They still won't be as
#   accurate as the Bayesian version.
