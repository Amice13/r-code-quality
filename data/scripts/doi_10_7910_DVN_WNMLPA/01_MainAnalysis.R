################################################################
# Replication File: "On Board with Banks"
# Authors: Jonas Markgraf & Rosas, Guillermo
# R version 3.4.3 (2017-11-30)
################################################################


# clean working environment
rm(list = ls())

# load packages
library(boot)
library(foreign)
library(stargazer)
library(ebal)
library(MASS)
library(cem)

library(Matching)
library(lattice)
library(survey)
library(plyr)
library(ggplot2)
library(repmis)
library (plm)
library(lmtest)
library (xtable)

# set seed
set.seed(12345)

# set working directory
# setwd(...)

# load data
party         <- read.table(file = "partyLevelReelection.txt", header = T)  # party-level reelection
individual    <- read.table(file = "indLevelReelection.txt", header = T, stringsAsFactors = F)  # individual-level reelection
bankBranch    <- read.table(file = "branchData.txt", header = T)  # mechanisms branches
bankDonation  <- read.table(file = "donationData.txt", header = T)  # mechanisms branches

## WHERE TO PUT THIS? -------------------------------------------------------

# Change reference categories
party$year <- as.factor (party$year)
party$year <- C(party$year, contr.treatment, base=which(levels(party$year)=="2013"))
party$county <- as.factor (party$county)
party$county <- C(party$county, contr.treatment, base=which(levels(party$county)=="6634"))

# TABLE 1: SUMMARY STATISTICS -----------------------------------------

variables.party <- c("reelection", "bankboard_lag", "NumberEligVoter", 
  "votemargin_lag", "PartyTerm_lag",
  "SPD", "bankowner", "Contested")
var_names.party <- c("Reelection", "Bank Board Member", 
  "Population Size", "Vote Margin", "Number of Terms",
  "SPD", "Bank-Owning Municipality", "Contested Election")

# we use the 'stargazer' package to create easy-to-interpret tables.
# These are not always in the same order as presented in the paper.
stargazer(party[party$Contested==1,variables.party], title = "Summary Statistics", 
  label = "T:summary", digits = 3, covariate.labels = var_names.party, type = "text")

# In addition, report mean of log population size in the same table
mean (log(party$NumberEligVoter[party$Contested==1]))

# TABLE 2: LOGISTIC REGRESSION MODELS, PARTY REELECTION -------------

# model 1: basic model
party.fit1 <- glm(reelection~bankboard_lag, 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

# model 2: model adds some election-specific variables, contested sample
party.fit2 <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD +
    year + county , 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

# model 3: all covariates, contested sample
party.fit3 <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
    year + county, 
    family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

# model 4: all covariates, full sample
party.fit4 <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
    year + county, 
  family = binomial(link = logit),
  data = party)

# export results
covariates.party <- c(bankboard_lag = "Bank Board Member$_{t-1}$",
  votemargin_lag = "Vote Margin$_{t-1}$",
  PartyTerm_lag = "Terms in Office$_{t-1}$",
  SPD = "Party: SPD$_{t-1}$",
  bankowner = "Bank-Owning Municipality",
  NumberEligVoter = "log(Population Size)")

stargazer(party.fit1, party.fit2, party.fit3, party.fit4,
  title = "Main Results", 
  label = "T:results.maineffect",
  covariate.labels = covariates.party,
  type = "text",
  dep.var.labels = c("Reelection"),
  column.labels = c("Contested", "Contested", 
    "Contested", "Full Sample"),
  add.lines = list(c("County Fixed effects", 
    "no", "yes", "yes", "yes"),
    c("Time Fixed Effects", 
      "no", "yes", "yes", "yes")),
  omit.stat = c("aic", "rsq", 
    "f", "ser", "adj.rsq"), digits = 3,
  omit = c("FedState", "county", "year"))


# Calculate probability that CDU/CSU will keep seat
# First estimate first model without constant, to "marginalize" county and year
party.pref.nocons <- glm(reelection~bankboard_lag + 
                     votemargin_lag + 
                     PartyTerm_lag + 
                     SPD + 
                     bankowner + 
                     log(NumberEligVoter) + 
                     year + county - 1, 
                  family = binomial(link = logit),
                  data = party,
                  subset = Contested == 1)

vote.margn <- mean (party$votemargin_lag[party$Contested==1])
party.term <- mean (party$PartyTerm_lag[party$Contested==1])
elig.voter <- log (mean (party$NumberEligVoter[party$Contested==1]))

relevant.coefs <- coef(party.pref.nocons)[1:6] # The rest are random year, county effects
relevant.vcov  <- vcov(party.pref.nocons)[1:6,1:6]

sim.coefs <- mvrnorm (1000, mu=relevant.coefs, Sigma=relevant.vcov)
vec.covariates.1 <- c(1, vote.margn, party.term, 0, 1, elig.voter)
vec.covariates.2 <- c(0, vote.margn, party.term, 0, 1, elig.voter)
y.star.board <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.noboard <- inv.logit (sim.coefs %*% vec.covariates.2)

# Expected probability of retaining seat with/without board
mean (y.star.board); sd (y.star.board)
mean (y.star.noboard); sd (y.star.noboard)

# Relative increase in odds of winning the election
# (This is basically the exponentiated coefficient of bankboard_lag)
# (Values above 1 suggest an increase, values below 1 suggest a worsening of the odds)
quantile ( (y.star.board / (1-y.star.board)) / (y.star.noboard /(1-y.star.noboard)), prob=c(0.025, 0.5, 0.975))


# BALANCED SAMPLE (TABLE A3, TABLE A4) -------------------------------------
# note: tables are in the appendix, but estimated coefficients reported in the text.

smallParty <- party[party$Contested==1, c("reelection", "bankboard_lag", "NumberEligVoter", "votemargin_lag", "PartyTerm_lag", "SPD", "bankowner","FedState","year")]
smallParty$NumberEligVoter <- log (smallParty$NumberEligVoter)
smallParty$year <- as.numeric (smallParty$year)+2005
covariates <- c("NumberEligVoter", "votemargin_lag", "PartyTerm_lag", "SPD", "bankowner","year")

# TABLE A3: imbalance in distribution ********************************************************
# generate covariate matrix and treatment and outcome vector
X <- smallParty[, covariates]
Y <- smallParty$reelection
W <- smallParty$bankboard_lag

stargazer (cbind (apply (X[W==1,], 2, mean),
  apply (X[W==1,], 2, sd),
  apply (X[W==0,], 2, mean),
  apply (X[W==0,], 2, sd))
  , title="Imbalance in distribution of bank board membership"
  , label="T:imbalance"
  , digits=2
  , type = "text")

# TABLE A4 Balance in treatment and control before and after matching ************************
X <- smallParty[smallParty$bankowner==1, covariates]; X <- X[,-grep("bankowner",colnames(X))]
Y <- smallParty$reelection[smallParty$bankowner==1]
W <- smallParty$bankboard_lag[smallParty$bankowner==1]

# CEM matching
tr <- which(W == 1)
ct <- which(W == 0)
ntr <- length(tr)
nct <- length(ct)
mean(Y[tr]) - mean(Y[ct])  # Difference in means

state <- as.factor(smallParty$FedState[smallParty$bankowner==1])
year  <- as.factor(smallParty$year[smallParty$bankowner==1])

cemData <- as.data.frame (cbind (W, Y, X, state) )
cemData$SPD <- factor (cemData$SPD)
cemData$year  <- factor (cemData$year)
cemData$state <- factor (cemData$state)

todrop <- c("Y", "W")

cutpts <- list(size=quantile(party$NumberEligVoter, prob=c(0.25,0.4,0.5,0.6,0.75))
  , votemargin_lag=c(0.2,0.4,0.5,0.6,0.8)
  , PartyTerm_lag=c(1.5,2.5,4.5,7.5))

orig.imb <- imbalance(group = cemData$W, data = cemData, drop=todrop)

mat.all <- cem(treatment = "W", data = cemData
  , drop=todrop
  , cutpoints=cutpts
  , baseline.group="1"
  , verbose=5
  , eval.imbalance=TRUE
  , k2k=FALSE
  , keep.all=TRUE)
mat.all

BalanceStats <- as.data.frame (cbind(orig.imb$tab[,c(1,3)], mat.all$imbalance$tab[,c(1,3)]))

BalanceStats[7:8,2] <- c(orig.imb$L1$L1, orig.imb$L1$LCS)
BalanceStats[7:8,4] <- c(mat.all$imbalance$L1$L1, mat.all$imbalance$L1$LCS)
rownames(BalanceStats)[7] <- "Multivariate imbalance"
rownames(BalanceStats)[8] <- "Percent common support"
xtable (round (BalanceStats,2))


out.all <- att(mat.all, Y ~ W, data=cemData)
print (out.all)

# Footnote 27: --------------------------------------------------------
todrop2 <- c("Y", "W", "year", "state")

mat.all2 <- cem(treatment = "W", data = cemData
  , drop=todrop2
  , cutpoints=cutpts
  , baseline.group="1"
  , L1.breaks=cutpts
  , verbose=5
  , eval.imbalance=TRUE
  , k2k=FALSE
  , keep.all=TRUE)
mat.all2

out.all2 <- att(mat.all2, Y ~ W, data=cemData)
print (out.all2)


# TABLE A5: Produce ATT, but including controls *****************************************
party.full.all <- glm(Y~W+NumberEligVoter+votemargin_lag+PartyTerm_lag
  +SPD+year+state
  , family = quasibinomial(link = logit)
  , data = cemData
  , weights=mat.all$w)
summary(party.full.all)

stargazer (party.full.all, type = "text")
# note: stargazer reports 455 observations, but 345 of those observations have weight = 0, i.e. actual number of observations is 110.

# Calculating odds
relevant.coefs <- coef(party.full.all)[1:6] # The rest are random year, county effects
relevant.vcov  <- vcov(party.full.all)[1:6,1:6]

sim.coefs <- mvrnorm (1000, mu=relevant.coefs, Sigma=relevant.vcov)
vec.covariates.1 <- c(1,1, elig.voter, vote.margn, party.term, 1)
vec.covariates.2 <- c(1,0, elig.voter, vote.margn, party.term, 1)
y.star.board <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.noboard <- inv.logit (sim.coefs %*% vec.covariates.2)

# Relative increase in odds of winning the election
# (This is basically the exponentiated coefficient of bankboard_lag)
# (Values above 1 suggest an increase, values below 1 suggest a worsening of the odds)
quantile ( (y.star.board / (1-y.star.board)) / (y.star.noboard /(1-y.star.noboard)), prob=c(0.025, 0.5, 0.975))

# TABLE 3: ADDITIONAL MODELS ---------------------------------------------------

# model 5: mayors with and without ownership: interaction owner*board
party.fit5 <- glm(reelection~bankboard_lag*bankowner + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    log(NumberEligVoter) + 
    year + county, 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

# Figure 1, Model 5: Predicted Reelection Probabilities for Incumbent Parties (Bank Owner)
vote.margn <- mean (party$votemargin_lag[party$Contested==1])
party.term <- mean (party$PartyTerm_lag[party$Contested==1])
elig.voter <- log (mean (party$NumberEligVoter[party$Contested==1]))

relevant.coefs <- coef(party.fit5)[c(1:7,length(coef(party.fit5)))] # The rest are random year, county effects
relevant.vcov  <- vcov(party.fit5)[c(1:7,length(coef(party.fit5))),c(1:7,length(coef(party.fit5)))]

sim.coefs <- mvrnorm (1000, mu=relevant.coefs, Sigma=relevant.vcov)

board <- owner <- c(0,1)
exp.y <- c()
for (i in 1:2){
  for (j in 1:2) {
    brd <- board[j]
    own <- owner[i]
    vec.covariates <- c(1, brd, own, vote.margn, party.term, 0, elig.voter, brd*own)
    y.star <- inv.logit (sim.coefs %*% vec.covariates)
    y.hat <- quantile (y.star, prob=c(0.05,0.25,0.5,0.75,0.95))
    tmp <- c(y.hat, brd, own)
    exp.y <- rbind (exp.y, tmp)
  }
}
colnames (exp.y)[6:7] <- c("board","owner")
exp.y

# non-bank-owning municipalities: board member vs no board member
own <- owner[1]
vec.covariates.1 <- c(1, 0, own, vote.margn, party.term, 0, elig.voter, 0*own)
vec.covariates.2 <- c(1, 1, own, vote.margn, party.term, 0, elig.voter, 1*own)
y.star.noboard <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.board <- inv.logit (sim.coefs %*% vec.covariates.2)
# Placing a 95\% confidence interval on difference
quantile ( (y.star.board / (1-y.star.board)) / (y.star.noboard /(1-y.star.noboard)), prob=c(0.025, 0.5, 0.975))

# bank-owning municipalities: board member vs no board member
own <- owner[2]
vec.covariates.1 <- c(1, 0, own, vote.margn, party.term, 0, elig.voter, 0*own)
vec.covariates.2 <- c(1, 1, own, vote.margn, party.term, 0, elig.voter, 1*own)
y.star.noboard <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.board <- inv.logit (sim.coefs %*% vec.covariates.2)
# Placing a 95\% confidence interval on difference
quantile ( (y.star.board / (1-y.star.board)) / (y.star.noboard /(1-y.star.noboard)), prob=c(0.025, 0.5, 0.975))

# generating Figure 1.1
par (mar=c(4,4,0,0))
plot (c(1,7), c(0.2,0.8), type="n", bty="n"
  , ylab=""
  , xlab=""
  , axes=F)
axis (2, at=seq(0.2,0.8, by=0.1))
mtext (text="Expected probability of reelection", side=2, line=2.5)
mtext (text=c("No","Yes"), side=1, at=c(2.5,5.5), line=0.5)
mtext (text="Municipality Owns Bank?", side=1, line=2)
points (xy.coords(c(2.3,2.7,5.3,5.7), exp.y[,3])
  , pch=19, col=c("grey","black","grey","black"))
segments (x0=c(2.3,2.7,5.3,5.7), x1=c(2.3,2.7,5.3,5.7), y0=exp.y[,2], y1=exp.y[,4]
  , lwd=3, col=c("grey","black","grey","black"))
legend ("topleft", lwd=3
  , bty="n"
  , col=c("grey","black")
  , legend=c("Mayor does not seat in supervisory board","Mayor seats in supervisory board"))
dev.off()

# model 6: partisan board effects -----------------------------------------------------------
party.fit6 <- glm(reelection~bankboard_lag*SPD + 
    votemargin_lag + 
    PartyTerm_lag + 
    bankowner + 
    log(NumberEligVoter) + 
    year + county, 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

# Figure 1, Model 6: Predicted Reelection Probabilities for Incumbent Parties (Partisan)
relevant.coefs <- coef(party.fit6)[c(1:7,length(coef(party.fit6 )))] # The rest are random year, county effects
relevant.vcov  <- vcov(party.fit6 )[c(1:7,length(coef(party.fit6 ))),c(1:7,length(coef(party.fit6)))]

sim.coefs <- mvrnorm (1000, mu=relevant.coefs, Sigma=relevant.vcov)

board <- SPD <- c(0,1)
exp.y <- c()
for (i in 1:2){
  for (j in 1:2) {
    brd <- board[j]
    spd <- SPD[i]
    vec.covariates <- c(1, brd, spd, vote.margn, party.term, 0, elig.voter, brd*spd)
    y.star <- inv.logit (sim.coefs %*% vec.covariates)
    y.hat <- quantile (y.star, prob=c(0.05,0.25,0.5,0.75,0.95))
    tmp <- c(y.hat, brd, spd)
    exp.y <- rbind (exp.y, tmp)
  }
}
colnames (exp.y)[6:7] <- c("board","spd")
exp.y

# conservative party: board member vs no board member
spd <- SPD[1]
vec.covariates.1 <- c(1, 0, spd, vote.margn, party.term, 0, elig.voter, 0*spd)
vec.covariates.2 <- c(1, 1, spd, vote.margn, party.term, 0, elig.voter, 1*spd)
y.star.noboard <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.board <- inv.logit (sim.coefs %*% vec.covariates.2)
# Placing a 95\% confidence interval on difference
quantile ( (y.star.board / (1-y.star.board)) / (y.star.noboard /(1-y.star.noboard)), prob=c(0.025, 0.5, 0.975))

# socialdemocratic party: board member vs no board member
spd <- SPD[2]
vec.covariates.1 <- c(1, 0, spd, vote.margn, party.term, 0, elig.voter, 0*spd)
vec.covariates.2 <- c(1, 1, spd, vote.margn, party.term, 0, elig.voter, 1*spd)
y.star.noboard <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.board <- inv.logit (sim.coefs %*% vec.covariates.2)
# Placing a 95\% confidence interval on difference
quantile ( (y.star.board / (1-y.star.board)) / (y.star.noboard /(1-y.star.noboard)), prob=c(0.025, 0.5, 0.975))

# generating Figure 1.2
par (mar=c(4,4,0,0))
plot (c(1,7), c(0.2,0.8), type="n", bty="n"
  , ylab=""
  , xlab=""
  , axes=F)
axis (2, at=seq(0.2, 0.8, by=0.1))
mtext (text="Expected probability of reelection", side=2, line=2.5)
mtext (text=c("No","Yes"), side=1, at=c(2.5,5.5), line=0.5)
mtext (text="Incumbent mayor belongs to SPD?", side=1, line=2)
points (xy.coords(c(2.3,2.7,5.3,5.7), exp.y[,3])
  , pch=19, col=c("grey","black","grey","black"))
segments (x0=c(2.3,2.7,5.3,5.7), x1=c(2.3,2.7,5.3,5.7), y0=exp.y[,2], y1=exp.y[,4]
  , lwd=3, col=c("grey","black","grey","black"))
legend ("topleft", lwd=3
  , bty="n"
  , col=c("grey","black")
  , legend=c("Mayor does not seat in supervisory board","Mayor seats in supervisory board"))
dev.off()

# model 7: divided boards: interaction divided*board
party$unified.board <- ifelse (party$bankboard_lag==1 & party$divided==0, 1, 0)
party$divided.board <- ifelse (party$bankboard_lag==1 & party$divided==1, 1, 0)

party.fit7 <- glm(reelection~unified.board + divided.board +
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
    year + county, 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

# odds of winning election for divided vs unified boards
relevant.coefs <- coef(party.fit7)[c(1:8)] # The rest are random year, county effects
relevant.vcov  <- vcov(party.fit7)[c(1:8),c(1:8)]

sim.coefs <- mvrnorm (1000, mu=relevant.coefs, Sigma=relevant.vcov)

boardType <- c(1,2,3)
vec.covariates.1 <- c(1, 0, 0, vote.margn, party.term, spd, 0, elig.voter)
vec.covariates.2 <- c(1, 1, 0, vote.margn, party.term, spd, 0, elig.voter)
vec.covariates.3 <- c(1, 0, 1, vote.margn, party.term, spd, 0, elig.voter)
y.star.1 <- inv.logit (sim.coefs %*% vec.covariates.1)
y.star.2 <- inv.logit (sim.coefs %*% vec.covariates.2)
y.star.3 <- inv.logit (sim.coefs %*% vec.covariates.3)
y.hat.1 <- quantile (y.star.1, prob=c(0.05,0.25,0.5,0.75,0.95))
y.hat.2 <- quantile (y.star.2, prob=c(0.05,0.25,0.5,0.75,0.95))
y.hat.3 <- quantile (y.star.3, prob=c(0.05,0.25,0.5,0.75,0.95))

# Placing a 95\% confidence interval on difference in relative odds
quantile ( (y.star.2 / (1-y.star.2)) / (y.star.1 /(1-y.star.1)), prob=c(0.025, 0.5, 0.975))
quantile ( (y.star.3 / (1-y.star.3)) / (y.star.1 /(1-y.star.1)), prob=c(0.025, 0.5, 0.975))
quantile ( (y.star.3 / (1-y.star.3)) / (y.star.2 /(1-y.star.2)), prob=c(0.025, 0.5, 0.975))


# Is the difference between unified and divided board statistically significant?
# H0: unified - divided = 0
# Var[X]+Var[Y]−2Cov[X,Y]
diffMeans <- coef(party.fit7)[2]-coef(party.fit7)[3]
varDiffMeans <- vcov(party.fit7)[2,2] + vcov(party.fit7)[3,3] - vcov(party.fit7)[2,3]
tDiffMeans <- diffMeans/sqrt(varDiffMeans) # t-value too low, not statistically significant difference
tDiffMeans

# export results from model 5-7 in stargazer
covariates.party2 <- c(bankboard_lag = "Bank Board Member$_{t-1}$",
  unified.board = "Mayor in unified board$_{t-1}$",
  divided.board = "Mayor in divided board$_{t-1}$",
  bankowner = "Bank-Owning Municipality",
  votemargin_lag = "Vote Margin$_{t-1}$",
  PartyTerm_lag = "Terms in Office$_{t-1}$",
  SPD = "Party: SPD$_{t-1}$",
  NumberEligVoter = "log(Population Size)",
  "Board Member$_{t-1}$*Bank Owner",
  "Bank Board Member$_{t-1}$*SPD")

stargazer(party.fit5, party.fit6, party.fit7, 
  title = "Heterogeneous Effects",
  label = "T:results.heterogeneous",
  dep.var.labels = c("Reelection"),
  covariate.labels = covariates.party2,
  column.labels = c("Bank Owner", "Partisan", "Divided"),
  add.lines = list(c("County Fixed effects", "yes", "yes", "yes"),
    c("Time Fixed Effects",  "yes", "yes", "yes")),
  omit.stat = c("aic", "rsq", "f", "ser", "adj.rsq"), digits = 3,
  omit = c("FedState", "county", "year"), type = "text")

##### APPENDIX ###########################################################


# TABLE A1: LINEAR PROBABILITY MODELS ------------------------------------

# fit OLS model using 'plm' package
partyPanel <- pdata.frame(party, index = c("year")
                          , drop.index = F
                          , row.names = T)
# adjust degrees of freedom
G <- length(unique(partyPanel[partyPanel$Contested == 1,]$county))
N <- length(partyPanel[partyPanel$Contested == 1,]$county)

# model 1
party.fit1.OLSclustered <- lm(reelection~bankboard_lag + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD +
    year + county , 
  data = party,
  subset = Contested == 1)

# model 2
party.fit2.OLSclustered <- lm(reelection~bankboard_lag +
                     votemargin_lag +
                     PartyTerm_lag +
                     SPD +
                     bankowner +
                     log(NumberEligVoter) +
                     year + county,
                   data = party,
                   subset = Contested ==1)

# model 3
party.fit3.OLSclustered <- lm(reelection~bankboard_lag + 
                    votemargin_lag + 
                    PartyTerm_lag + 
                    SPD + 
                    bankowner + 
                    log(NumberEligVoter) + 
                    year + county, 
                  data = party)

# display results with clustered SE and adjusted DF
party.fit1.adjDF <- (G/(G-1)) * (N-1)/party.fit1.OLSclustered$df.residual
party.fit2.adjDF <- (G/(G-1)) * (N-1)/party.fit2.OLSclustered$df.residual
party.fit3.adjDF <- (G/(G-1)) * (N-1)/party.fit3.OLSclustered$df.residual

# Huber-White matrix
party.fit1.county_c_vcov <- party.fit1.adjDF * vcovHC(party.fit1.OLSclustered
                                           , type = "HC0"
                                           , cluster = "group"
                                           , adjust = T)
party.fit2.county_c_vcov <- party.fit2.adjDF * vcovHC(party.fit2.OLSclustered
                                           , type = "HC0"
                                           , cluster = "group"
                                           , adjust = T)
party.fit3.county_c_vcov <- party.fit3.adjDF * vcovHC(party.fit3.OLSclustered
                                           , type = "HC0"
                                           , cluster = "group"
                                           , adjust = T)


party.fit1.OLSmodel <- coeftest(party.fit1.OLSclustered, vcov = party.fit1.county_c_vcov)
party.fit2.OLSmodel <- coeftest(party.fit2.OLSclustered, vcov = party.fit2.county_c_vcov)
party.fit3.OLSmodel <- coeftest(party.fit3.OLSclustered, vcov = party.fit3.county_c_vcov)

## Results for linear probability models in stargazer
stargazer(party.fit1.OLSmodel, party.fit2.OLSmodel, party.fit3.OLSmodel,
          title = "Linear probability models of party reelection conditional on board membership, with county and time fixed effects (robust standard errors clustered at county level)",
          label = "T:results.lpm",
          covariate.labels = covariates.party,
          dep.var.labels = c("Reelection"),
          column.labels = c("Contested",
                            "Contested", "Full Sample"),
          digits = 3,
          omit = c("FedState", "county", "year"),
          type = "text")
# Need to add likelihoods and N by hand
logLik(party.fit1.OLSclustered)
logLik(party.fit2.OLSclustered)
logLik(party.fit3.OLSclustered)

nrow (model.matrix(party.fit1.OLSclustered))
nrow (model.matrix(party.fit2.OLSclustered))
nrow (model.matrix(party.fit3.OLSclustered))

# TABLE A2: individual-level reelection ------------------------------

# model A1: contested elections
individual.fit1 <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    IndTerm_lag + 
    SPD + Others +
    bankowner + 
    log(NumberEligVoter) + 
    as.factor(year) + as.factor(county), 
  family = binomial(link = logit),
  data = individual,
  subset = Contested == 1)

# model A2: contested and non-contested elections
individual.fit2 <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    IndTerm_lag + 
    SPD + Others +
    bankowner + 
    log(NumberEligVoter) + 
    as.factor(year) + as.factor(county), 
  family = binomial(link = logit),
  data = individual)

stargazer(individual.fit1, individual.fit2, type = "text"
  ,  digits = 3,
  omit = c("county", "year"))

# TABLE A6: state Fixed Effects --------------------------------------
party.stateFE <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
       year + FedState,  
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

stargazer(party.stateFE,
  label = "T:results.stateFE",
  covariate.labels = covariates.party,
  dep.var.labels = c("Reelection"),
  column.labels = c("State FE"),
  add.lines = list(c("State Fixed effects", "yes"),
    c("Time Fixed Effects", "yes")),
  omit.stat = c("aic", "rsq",
    "f", "ser", "adj.rsq"), digits = 3,
  omit = c("FedState", "county", "year"),
  type = "text")


# TABLE A7: Dummy for Bayern -----------------------------------
party$BY <- ifelse(party$FedState == "Bavaria", 1, 0)

fit.BY <- glm(reelection~bankboard_lag + BY + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
    factor(year) , 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1)

covariates.BY <- c(bankboard_lag = "Bank Board Member$_{t-1}$",
  BY = "Bavaria dummy",
  votemargin_lag = "Vote Margin$_{t-1}$",
  PartyTerm_lag = "Terms in Office$_{t-1}$",
  SPD = "Party: SPD$_{t-1}$",
  bankowner = "Bank-Owning Municipality",
  NumberEligVoter = "log(Population Size)")

stargazer(fit.BY,
  title = "TBF",
  label = "T:results.bavaria",
  covariate.labels = covariates.BY,
  dep.var.labels = c("Reelection"),
  add.lines = list(c("County Fixed effects", "no"),
    c("Time Fixed Effects", "yes")),
  omit.stat = c("aic", "rsq",
    "f", "ser", "adj.rsq"), digits = 3,
  omit = c("FedState", "county", "year"),
  type = "text")


#  TABLE A8: first term mayors only ---------------------------

party.1stterm <- glm(reelection~bankboard_lag + 
    votemargin_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
       year + county,  
  family = binomial(link = logit),
  data = party,
  subset = PartyTerm_lag == 1)

covariates.1stTerm <- c(bankboard_lag = "Bank Board Member$_{t-1}$",
  votemargin_lag = "Vote Margin$_{t-1}$",
  SPD = "Party: SPD$_{t-1}$",
  bankowner = "Bank-Owning Municipality",
  NumberEligVoter = "log(Population Size)")

stargazer(party.1stterm,
  label = "T:results.1stterm",
  covariate.labels = covariates.1stTerm,
  dep.var.labels = c("Reelection"),
  add.lines = list(c("County Fixed effects", "yes"),
    c("Time Fixed Effects", "yes")),
  omit.stat = c("aic", "rsq",
    "f", "ser", "adj.rsq"), digits = 3,
  omit = c("FedState", "county", "year"),
  type = "text")

# TABLE A9: merger vs non-merger banks -------------------------------------------

party$merger.board <- ifelse (party$bankboard_lag==1 & party$merger==1, 1, 0)
party$notmerger.board <- ifelse (party$bankboard_lag==1 & party$merger==0, 1, 0)

party.merger <- glm(reelection~merger.board + notmerger.board + 
    votemargin_lag + 
    PartyTerm_lag + 
    SPD + 
    bankowner + 
    log(NumberEligVoter) + 
    factor(year) + factor(county), 
  family = binomial(link = logit),
  data = party,
  subset = Contested == 1 )

covariates.merger <- c(merged.board = "Bank subject to merger",
  notmerged.board = "Bank not subject to merger",
  votemargin_lag = "Vote Margin$_{t-1}$",
  PartyTerm_lag = "Terms in Office$_{t-1}$",
  SPD = "Party: SPD$_{t-1}$",
  bankowner = "Bank-Owning Municipality",
  NumberEligVoter = "log(Population Size)")

stargazer(party.merger,
  label = "T:results.merger",
  covariate.labels = covariates.merger,
  dep.var.labels = c("Reelection"),
  add.lines = list(c("County Fixed effects", "yes", "yes"),
    c("Time Fixed Effects", "yes", "yes")),
  omit.stat = c("aic", "rsq",
    "f", "ser", "adj.rsq"), digits = 3,
  omit = c("FedState", "county", "year"),
  type = "text")


# TABLE A10: MECHANISMS (BRANCHES; DONATIONS) --------------------------------------

# model A3: number of donations
fit.noDon <- glm.nb(nrDonations ~ onBoard  
  + log(popMunicipality)
  + as.factor(BLZ)
  + as.factor(year)
  , init.theta=1
  , data = bankDonation)
summary(fit.noDon)

# Find change in expectations
mean.pop <- log(mean (bankDonation$popMunicipality))
simulCoefs <- mvrnorm (1000, coef(fit.noDon)[1:3], vcov(fit.noDon)[1:3,1:3])

mean (simulCoefs %*% c(1,1,mean.pop)); sd (simulCoefs %*% c(1,1,mean.pop)) # board
mean (simulCoefs %*% c(1,0,mean.pop)); sd (simulCoefs %*% c(1,0,mean.pop)) # no board

quantile (simulCoefs %*% c(1,1,mean.pop), prob=c(0.025,0.975)) # board quantiles
quantile (simulCoefs %*% c(1,0,mean.pop), prob=c(0.025,0.975)) # no board quantiles

# model A4: amount of donations
fit.amountDon <- lm(I(log(amountDonations+0.01)) ~ onBoard  # log (amountDonations) plus one pfennig
  + log(popMunicipality)
  + as.factor(BLZ)
  + as.factor(year)
  , data = bankDonation
  , subset = amountInfo == 1)
summary(fit.amountDon)

# model A5: branch closures
fit.branch <- lm(d.branch ~ inBoard 
  + log(popMunicipality) + area
  + noBranch16
  + as.factor(BLZ)
  , data = bankBranch
  , subset = popMunicipality != 0)
summary(fit.branch)
