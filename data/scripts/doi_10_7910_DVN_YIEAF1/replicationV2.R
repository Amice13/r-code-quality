###########################################################################
# Replication Script for                                                  #
#                                                                         #
# Causal Modeling with Multi-Value and Fuzzy-Set Coincidence Analysis     #
# Michael Baumgartner & Mathias Ambuehl                                   #
#                                                                         #
# 31.07.2019                                                              #
#                                                                         #
###########################################################################
# Log console output:
console <- file("console.log")
sink(console, append=TRUE)
sink(console, append=TRUE, type="message")

# Required packages QCApro_1.1-2, cna_2.1.0, tikzDevice_0.11,
# reshape2_1.4.3, ggplot2_2.2.1 
library(QCApro)
library(cna)
library(ggplot2)
library(reshape2)
library(tikzDevice)
suppressWarnings(RNGversion("3.5.0")) # needed to enforce the sampling method
                                      # used for this script.


# SECTION 2.3 --------------------------------------------
# -----------
# Data examples for which the models output by QCA do not meet the
# pre-defined consistency threshold of 0.8.
# First, build a fuzzy-set data set with 32 configurations
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1
set.seed(15) # alternatively choose seeds 3, 14, 16, 220
dat1 <- makeFuzzy(mydata,fuzzvalues = seq(0, 0.45, 0.01))
# Second, select all cases from dat1 that are compatible with
# the causal structure "a*B + c*D <-> E" such that
# that structure scores 0.8 on consistency and coverage.
dat2 <- tt2df(selectCases1("a*B + c*D <-> E", con = 0.8, cov = 0.8, dat1))
# Third, run QCA on dat2 with consistency threshold at 0.8.
eQMC(dat2, outcome="E", incl.cut1 = 0.8, details = TRUE)
# Fourth, run CNA on dat2 with con = cov = 0.8.
fscna(dat2, ordering = list("E"), strict = TRUE, con=.8, cov=.8)
# By repeatedly re-running lines 20-26 only (i.e. without the seed),
# the reader can develop a sense of how frequently QCA models miss 
# consistency thresholds.


# SECTION 3.1 -------------------------------------------
# -----------
# cs data example from Table 1(a)
tt <- truthTab(data.frame(A = c(1,1,1,1,1,1,0,1,0),
                          B = c(1,1,1,1,0,0,1,0,0),
                          C = c(1,1,0,0,1,0,1,0,0),
                          D = c(1,0,1,0,0,1,0,0,0)), 
                  frequency = c(3,1,5,2,1,7,4,1,3))
print(tt, show.cases = T)
# Illustrating the non-monotony of consistency violations:
as.condTbl(condition(c("A*B*C -> D", "A*B*c -> D", "A*B -> D", "A -> D"), tt))
# A top-down procedure as QCA cannot eliminate all redundancies from
# A*B*C -> D at a consistency threshold of 0.75 (or even at lower thresholds)
eQMC(tt2df(tt), outcome = "D", incl.cut1 = 0.75, details = TRUE)
# The bottom-up approach of CNA, by contrast, straightforwardly finds
# the model "A <-> D".
cna(tt, con= 0.75)
# These are the relevant consistency tests the bottom-up approach must 
# conduct to find "A <-> D".
as.condTbl(condition(c("A -> D", "B -> D", "C -> D", "B*C -> D"), tt))

# fs data example from Tables 1(b)/1(c)
dat3 <- data.frame(A=c(1,1,0.4,1),
                   B=c(1,1,1,0.3),
                   C=c(0.4,0.6,0.4,0.3),
                   D=c(1,1,0.1,0))
dat3
tt2 <-truthTable(dat3, outcome="D", incl.cut1 = .75, sort.by = "incl")
tt2
eQMC(tt2) # This produces an error message showing that eQMC abandons the analysis.
fscna(dat3, con=.75)


# SECTION 3.2 ------------------------------------------------------------
# -----------
# cs data in Table 2(a)
dat1 <- allCombs(c(2,2,2,2)) -1 
dat2 <- tt2df(selectCases("A*b + B*C <-> D", dat1))
dat2
cscna(dat2)


# mv data in Table 2(b)
dat1 <- allCombs(c(3,4,3,4))
dat3 <- tt2df(selectCases("(A=1 + B=4 + A=3 <-> C=3)*(B=1 + A=2 +  A=3 <-> D=2)",
                          dat1, type ="mv"))
set.seed(60)
dat4 <- some(dat3, n=8, replace = F)
dat4
mvcna(dat4, ordering = list(c("C","D")), strict = T)


# fs data in Table 2(c)
dat1 <- allCombs(c(2,2,2,2,2)) -1
dat2 <- tt2df(selectCases("(A + B <-> C)*(A*B + D <-> E)", dat1))
set.seed(28)
dat3 <- tt2df(makeFuzzy(dat2, fuzzvalues = seq(0, 0.4, 0.01)))
dat3
fscna(dat3, con=.8, cov=.9)



# SECTION 4 ----------------------------------------------------------------------
# ---------
# In the Preamble to the test series, the auxiliary functions are loaded and the 
# score sheets are generated.
# Tests are grouped in three categories: tests with cs data, tests with fs data,
# and tests with mv data. In the header of each test, the exact data scenario is 
# described.
# 
# Each test involves three parts: (i) the test setup, (ii) the test loop, (iii) the
# evaluation.
# 
# In the test setup, the number of trials and the data-generating 
# structures (DGS) are defined, the seeds are drawn (which allow for exact replication;
# of course, the reader is invited to play around with the seeds to re-run the 
# tests with different DGS and/or data), and the output lists for CNA and QCA are 
# generated.
# 
# The test loops feature the actual execution of the trials. They must always be
# run as a package.
# 
# In the evaluation, the following ratios are culled
# 
# - Correctness, i.e. the ratio of trials that satisfy CC (p. 12, in the paper).
# - Correctness without ambiguities, i.e. the ratio of trials that satisfy CC
#   without model ambiguities
# - Correctness by unique model, i.e. the ratio of trials that satisfy CC with 
#   exactly one model
# - Completeness, i.e. the ratio of trials where the complete data-generating structure 
#   is recovered
# - Ratio of model ambiguities, i.e. the ratio of trials where more than one model
#   is returned
# - Ratio of no issued model, i.e. the ratio of trials where no model is returned
# - Ratio of a unique model, i.e. the ratio of trials where exactly one model is 
#   returned
# 
# As the syntax used by the QCApro package is not exactly the same as the 
# syntax of the cna package, function calls for syntax homogenization are
# regularly inserted.
# 
# Before running the tests, users must load the source file mentioned in the preamble
# of the test series, which provides various auxiliary functions.
#
# 
# PREAMBLE
##########
# Place the file aux_functions.R in working directory and set the correct path
# using setwd().
source("aux_functions.R")


# Define score sheets
test_types <- c("oufi", "Oufi", "oUfi", "ouFi", "oufI",
                "OUfi", "OuFi", "oUFi", "ouFI", "OufI",
                "oUfI", "OUFi", "OUfI", "OuFI", "oUFI",
                "OUFI")
cols <- c("#","csCNA","csQCA", "#", "fsCNA", "fsQCA", "#", "mvCNA", "mvQCA")
# Correctness score sheet: cor_score
# Correctness without ambiguities score sheet: cor_without_score
# Correctness by unique model: cor_unique_score
# Completeness score sheet: com_score
# Ambiguity score sheet: amb_score
# No model score sheet: no_model_score
# Unique model score sheet: unique_model_score
cor_score <- cor_without_score <- cor_unique_score <- com_score <- amb_score <- no_model_score <- unique_model_score <-
  matrix(nrow = 16, ncol = 9, byrow = T,
        dimnames = list(test_types,cols))
cor_score[,1] <- cor_without_score[,1] <- cor_unique_score[,1] <- com_score[,1] <- amb_score[,1] <- no_model_score[,1] <- unique_model_score[,1] <- 1:16
cor_score[,4] <- cor_without_score[,4] <- cor_unique_score[,4] <- com_score[,4] <- amb_score[,4] <- no_model_score[,4] <- unique_model_score[,4] <- 17:32
cor_score[,7] <- cor_without_score[,7] <- cor_unique_score[,7] <- com_score[,7] <- amb_score[,7] <- no_model_score[,7] <- unique_model_score[,7] <- 33:48

# CS DATA
#########

# Test 1 ------------------------------------------------------------------------
#         IDEAL DATA; i.e. no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation, perfect solution consistencies 
#         and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(45)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=41, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1 

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> D"), mydata))
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat1, ordering=list("D"), strict=T, 
                           con=1, cov=1, maxstep = c(3,4,9)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
  }

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[1,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[1,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[1,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[1,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[1,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[1,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[1,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[1,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -------------------------
# QCA
table(sapply(qca.list, length))
amb_score[1,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[1,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# ------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[1,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[1,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ---------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[1,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[1,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 2 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation, perfect solution consistencies 
#         and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(16)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=3222, seed.2=1232,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from the frame 
# F' > F, with irrelevant factor E:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> D"), mydata))
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat1, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[2,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[2,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[2,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[2,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[2,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[2,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[2,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[2,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[2,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[2,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[2,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[2,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[2,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[2,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 3 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation, perfect solution consistencies 
#         and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(464)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=337, seed.2=1232,
                  prob = 0.6, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> D"), mydata))
  # Eliminate the relevant factor C
  dat2 <- dat1[,c("A","B","D")]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat2, ordering=list("D"), strict=T, con=1, cov=.6,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[3,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[3,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[3,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[3,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[3,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[3,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[3,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[3,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[3,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[3,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[3,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[3,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[3,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[3,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 4 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(433)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=41, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> D"), mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat2, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,4,12), rm.dup.factors = F, rm.const.factors = F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[4,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[4,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[4,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[4,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[4,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[4,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[4,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[4,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[4,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[4,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[4,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[4,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[4,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[4,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 5 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(24)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=45, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con = .8, cov = .8, mydata))
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat1, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D", incl.cut1=.8))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[5,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[5,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[5,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[5,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[5,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[5,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[5,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[5,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[5,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[5,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[5,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[5,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[5,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[5,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 6 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         perfect solutions consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(3)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> E"), mydata))
  # Eliminate the relevant factor A
  dat2 <- dat1[,c("B","C","D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat2, ordering=list("E"), strict=T, con=1, cov=.7,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[6,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[6,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[6,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[6,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[6,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[6,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[6,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[6,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n


# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[6,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[6,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[6,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[6,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[6,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[6,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 7 -----------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(31)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=525, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)


# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> D"), mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat2, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[7,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[7,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[7,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[7,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[7,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[7,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[7,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[7,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n


# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[7,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[7,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[7,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[7,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[7,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[7,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 8 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(30)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=525, seed.2=46,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> E"), mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C", "D", "E")]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat3, ordering=list("E"), strict=T, con=1, cov=.75,
                            maxstep = c(3,6,17), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[8,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[8,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[8,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[8,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[8,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[8,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[8,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[8,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[8,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[8,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[8,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[8,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[8,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[8,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 9 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(612)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=64, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con = .8, cov = .8, mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat2, ordering=list("D"), strict=T, con= .8, cov= .8,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1= .8))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[9,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[9,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[9,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[9,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[9,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[9,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[9,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[9,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n


# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[9,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[9,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[9,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[9,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[9,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[9,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 10 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(12) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=18, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con=.8, cov=.8, mydata))
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat1, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[10,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[10,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[10,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[10,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[10,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[10,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[10,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[10,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[10,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[10,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[10,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[10,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[10,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[10,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 11 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(113) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=29, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con=.8, cov=.8, mydata))
  # Eliminate the relevant factor B
  dat2 <- dat1[,c("A","C", "D", "E")]
  # Run CNA and QCA [Consistencies are set to lower values than the targeted 
  # DGS in order to strike an optimal balance between informativeness and correctness of
  # the output. The reader is explicitly invited to play around with these values. 
  # It will be seen that at higher consistency cutoffs the correctness ratio of QCA tends
  # towards zero.]
  cna.sol <- tryTest(cscna(dat2, ordering=list("E"), strict=T, con=.75, cov=.7,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E", incl.cut1= .6, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[11,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[11,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[11,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[11,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[11,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[11,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[11,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[11,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n


# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[11,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[11,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[11,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[11,2] <- na.zero(table(no.model.cna)["TRUE"])/n


# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[11,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[11,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 12 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(92) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(sols[i]," <-> E"), mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C", "D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat3, ordering=list("E"), strict=T, con=1, cov=.75,
                            maxstep = c(3,6,14), rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1=1, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[12,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[12,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[12,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[12,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[12,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[12,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[12,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[12,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[12,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[12,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[12,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[12,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[12,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[12,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 13 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(37)  #35
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con= .8, cov=.8, mydata))
  # Eliminate the relevant factor A
  dat2 <- dat1[,c("B", "C", "D","E","F")]
  # Run CNA and QCA [Consistencies are set to lower values than the targeted 
  # DGS in order to strike an optimal balance between informativeness and correctness of
  # the output. The reader is explicitly invited to play around with these values. 
  # It will be seen that at higher consistency cutoffs the correctness ratio of QCA tends
  # towards zero.]
  cna.sol <- tryTest(cscna(dat2, ordering=list("E"), strict=T, con=.75, cov=.75,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E", incl.cut1=.6))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[13,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[13,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[13,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[13,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[13,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[13,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[13,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[13,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[13,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[13,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[13,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[13,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[13,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[13,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 14 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(19) # 17
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=18, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con=.8, cov=.8, mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(cscna(dat2, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,6,14), rm.dup.factors = F, rm.const.factors= F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[14,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[14,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[14,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[14,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[14,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[14,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[14,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[14,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n


# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[14,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[14,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[14,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[14,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[14,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[14,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 15 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(12)  
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=233, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con = .8, cov = .8, mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C", "D","E")]
  # Run CNA and QCA [Consistencies and coverages are set to lower values than the targeted 
  # DGS in order to strike an optimal balance between informativeness and correctness of
  # the output. The reader is explicitly invited to play around with these values. 
  # It will be seen that at higher consistency cutoffs the correctness ratio of QCA tends
  # towards zero.]
  cna.sol <- tryTest(cscna(dat3, ordering=list("E"), strict = T, con = .8, cov = .75,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1= .6, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[15,3] <- table(correctSolution.qca)["TRUE"]/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[15,2] <- table(correctSolution.cna)["TRUE"]/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[15,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[15,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[15,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[15,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[15,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[15,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[15,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[15,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[15,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[15,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[15,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[15,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 16 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(28)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con = .8, cov = .8, mydata))
  # Sample random rows with diversity index about 50%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*50, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C","D","E","F")]
  # Run CNA and QCA 
  cna.sol <- tryTest(cscna(dat3, ordering=list("E"), strict = T, con = .8, cov = .75,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[16,3] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[16,2] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[16,3] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[16,2] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[16,3] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[16,2] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[16,3] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[16,2] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[16,3] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[16,2] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[16,3] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[16,2] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[16,3] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[16,2] <- na.zero(table(unique.model.cna)["TRUE"])/n


# ---------------
# FS-DATA
#########


# Test 17 -----------------------------------------------------------------------
#         IDEAL DATA: no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(20)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)
# 43
# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=68, seed.2=47,
                  prob = 0.6, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=400, replace =T), fuzzvalues = seq(0, 0.2, 0.1))
  # select cases that are compatible with structure i 
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> D"), dat1, type = "fs"))
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat2, ordering=list("D"), strict=T, con=1, cov=1,
                             maxstep=c(3,4,12), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=1, details =T))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[1,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[1,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[1,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[1,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[1,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[1,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[1,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[1,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[1,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[1,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[1,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[1,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[1,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[1,5] <- na.zero(table(unique.model.cna)["TRUE"])/n



# Test 18 -----------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(63)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=68, seed.2=47,
                  prob = 0.6, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=400, replace =T), fuzzvalues = seq(0, 0.2, 0.1))
  # select cases that are compatible with structure i 
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> D"), dat1, type = "fs"))
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat2, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,4,9), rm.dup.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=1, details =T))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[2,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[2,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[2,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[2,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[2,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[2,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[2,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[2,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[2,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[2,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[2,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[2,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[2,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[2,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 19 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(36) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=34, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=300, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i 
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> E"), dat1, type = "fs"))
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C", "D","E")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("E"), strict=T, con=1, cov=.65,
                            maxstep = c(3,6,18)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1=1,details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[3,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[3,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[3,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[3,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[3,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[3,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[3,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[3,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[3,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[3,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[3,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[3,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[3,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[3,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 20 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(63) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=82, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=500, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i 
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> D"), dat1, type = "fs"))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,4,9), rm.const.factors=F,rm.dup.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "D", incl.cut1=1, details =T))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[4,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[4,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[4,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[4,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[4,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[4,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[4,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[4,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[4,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[4,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[4,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[4,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[4,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[4,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 21 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(7) #6
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=45, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.01))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con = .8, cov = .8, dat1))
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat2, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=.8))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[5,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[5,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[5,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[5,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[5,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[5,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[5,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[5,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[5,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[5,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[5,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[5,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[5,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[5,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 22 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         perfect solutions consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(46) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i 
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> E"), dat1, type = "fs"))
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C","D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("E"), strict=T, con=1, cov=1,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[6,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[6,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[6,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[6,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[6,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[6,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[6,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[6,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[6,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[6,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[6,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[6,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[6,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[6,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 23 -----------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(31)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=525, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=150, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> D"), dat1, type = "fs"))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat2, ordering=list("D"), strict=T, con=1, cov=.8,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[7,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[7,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[7,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[7,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[7,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[7,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[7,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[7,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[7,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[7,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[7,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[7,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[7,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[7,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 24 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(40)  # 40
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=525, seed.2=46,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=300, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> E"), dat1, type ="fs"))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C","D", "E")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("E"), strict=T, con=1, cov=.8,
                            maxstep = c(3,6,17), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[8,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[8,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[8,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[8,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[8,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[8,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[8,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[8,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[8,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[8,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[8,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[8,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[8,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[8,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 25 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the compatible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(34)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=57, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2)) - 1


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.01))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con = .8, cov = .8, dat1))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "D", incl.cut1=.8))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[9,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[9,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[9,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[9,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[9,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[9,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[9,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[9,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[9,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[9,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[9,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[9,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[9,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[9,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 26 -----------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(91)  
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=62, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.01))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con = .8, cov = .8, dat1))
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat2, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D", incl.cut1=.8))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[10,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[10,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[10,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[10,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[10,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[10,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[10,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[10,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[10,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[10,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[10,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[10,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[10,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[10,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 27 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(59)  
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C","D"), seed.1=42, seed.2=447,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=50, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con = .8, cov = .8, dat1))
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C", "D", "E")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("E"), strict=T, con=.8, cov=.8, 
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[11,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[11,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[11,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[11,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[11,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[11,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[11,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[11,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[11,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[11,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[11,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[11,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[11,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[11,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 28 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(49) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=1, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  dat1 <- makeFuzzy(some(mydata, n=300, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i 
  dat2 <- tt2df(selectCases(paste0(sols[i]," <-> E"), dat1, type = "fs"))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Eliminate the relevant factor A
  dat4 <- dat3[,c("B","C","D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat4, ordering=list("E"), strict=T, con=1, cov=.9, 
                            maxstep = c(3,6,14), rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat4, outcome = "E", incl.cut1=1, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[12,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[12,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[12,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[12,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[12,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[12,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[12,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[12,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[12,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[12,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[12,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[12,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[12,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[12,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 29 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(18)  
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C","D"), seed.1=70, seed.2=447,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=30, replace =T), fuzzvalues = seq(0, 0.3, 0.01))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con = .8, cov = .8, dat1))
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C","D","E","F")]
  # Run CNA and QCA 
  cna.sol <- tryTest(fscna(dat3, ordering=list("E"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E", incl.cut1=.8))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[13,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[13,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[13,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[13,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[13,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[13,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[13,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[13,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[13,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[13,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[13,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[13,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[13,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[13,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 30 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(32) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=18, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.01))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> D"), con = .8, cov = .8, dat1))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat3, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,6,14), rm.dup.factors = F, rm.const.factors= F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "D", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "D")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[14,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[14,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[14,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[14,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[14,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[14,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[14,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[14,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[14,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[14,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[14,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[14,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[14,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[14,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 31 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(35) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=26, seed.2=47,
                  prob = 0.75, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' = F:
mydata <- allCombs(c(2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.1))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con = .8, cov = .8, dat1, type = "fs"))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Eliminate the relevant factor C
  dat4 <- dat3[,c("A","B","D","E")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat4, ordering=list("E"), strict = T, con = .8, cov = .75,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat4, outcome = "E", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[15,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[15,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[15,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[15,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[15,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[15,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[15,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[15,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[15,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[15,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[15,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[15,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[15,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[15,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 32 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(70) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=56, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS 
sols <- addStar(sols)

# Generating the space of all logically possible configurations from frame 
# F' > F:
mydata <- allCombs(c(2, 2, 2, 2, 2, 2)) - 1 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # Fuzzify and expand
  dat1 <- makeFuzzy(some(mydata, n=80, replace =T), fuzzvalues = seq(0, 0.3, 0.01))
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat2 <- tt2df(selectCases1(paste0(sols[i]," <-> E"), con = .8, cov = .8, dat1))
  # Sample random rows with diversity index about 50%
  dat3 <- dat2[sample(nrow(dat2),(nrow(dat2)/100)*50, replace = F),]
  # Eliminate the relevant factor B
  dat4 <- dat3[,c("A","C", "D","E", "F")]
  # Run CNA and QCA
  cna.sol <- tryTest(fscna(dat4, ordering=list("E"), strict = T, con = .8, cov = .8,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat4, outcome = "E", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[16,6] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[16,5] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[16,6] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[16,5] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[16,6] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[16,5] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(sols[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[16,6] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(sols[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[16,5] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[16,6] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[16,5] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[16,6] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[16,5] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[16,6] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[16,5] <- na.zero(table(unique.model.cna)["TRUE"])/n


# -----------------
# MV DATA
#########

# Test 33 ------------------------------------------------------------------------
#         IDEAL DATA; i.e. no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation, perfect solution consistencies 
#         and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(45)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=41, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3)) 

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> D=3"), mydata, type = "mv"))
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat1, ordering=list("D"), strict=T, con=1, cov=1, 
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D{3}", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[1,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[1,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[1,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[1,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[1,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[1,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[1,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[1,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n


# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[1,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[1,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[1,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[1,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[1,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[1,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 34 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation, perfect solution consistencies 
#         and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(16)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=3222, seed.2=1232,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' > F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> D=3"), mydata, type ="mv"))
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat1, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D{3}", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[2,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[2,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[2,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[2,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[2,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[2,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[2,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[2,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[2,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[2,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[2,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[2,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[2,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[2,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 35 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation, perfect solution consistencies 
#         and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(46)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=352, seed.2=12,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> E=2"), mydata, type = "mv"))
  # Eliminate the relevant factor B
  dat2 <- dat1[,c("A","C","D", "E")]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat2, ordering=list("E"), strict=T, con=1, cov=.6,
                            maxstep = c(4,6,16)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E{2}", incl.cut1=1, details = T ))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[3,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[3,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[3,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[3,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[3,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[3,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[3,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[3,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[3,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[3,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[3,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[3,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[3,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[3,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 36 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(433) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=41, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3))

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> D=3"), mydata, type = "mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat2, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,4,12), rm.dup.factors = F, rm.const.factors = F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D{3}", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[4,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[4,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[4,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[4,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[4,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[4,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[4,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[4,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[4,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[4,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[4,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[4,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[4,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[4,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 37 -----------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(28) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=45, seed.2=47,
                  prob = 0.5, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> D=3"), con = .8, cov = .8, mydata,
                             type = "mv"))
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat1, ordering=list("D"), strict=T, con=.8, cov=.8, 
                            con.msc=.7, maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D{3}", incl.cut1=.8,details=T))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[5,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[5,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[5,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[5,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[5,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[5,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[5,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[5,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[5,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[5,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[5,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[5,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[5,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[5,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[5,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[5,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 38 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         perfect solutions consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(33)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=232, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' > F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3 ,3,3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> E=2"), mydata, type="mv"))
  # Eliminate the relevant factor A
  dat2 <- dat1[,c("B","C","D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat2, ordering=list("E"), strict=T, con=1, cov=.7,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E{2}", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[6,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[6,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[6,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[6,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[6,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[6,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[6,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[6,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[6,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[6,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[6,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[6,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[6,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[6,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 39 -----------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(31)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=525, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' > F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> D=3"), mydata, type = "mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat2, ordering=list("D"), strict=T, con=1, cov=1,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D{3}", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[7,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[7,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[7,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[7,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[7,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[7,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[7,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[7,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[7,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[7,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[7,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[7,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[7,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[7,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 40 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only half of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(32)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=95, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3))



# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> E=2"), mydata, type ="mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Eliminate the relevant factor C
  dat3 <- dat2[,c("A","B","D", "E")]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat3, ordering=list("E"), strict=T, con=1, cov=.75,
                            maxstep = c(3,7,16), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E{2}", incl.cut1=1))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[8,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[8,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[8,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[8,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[8,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[8,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[8,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[8,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------
# QCA
table(sapply(qca.list, length))
amb_score[8,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[8,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[8,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[8,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[8,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[8,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 41 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(614) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=64, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3))

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> D=3"), con = .8, cov = .8, mydata, type = "mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat2, ordering=list("D"), strict=T, con= .8, cov= .8,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D{3}", incl.cut1= .8))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[9,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[9,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[9,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[9,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[9,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[9,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[9,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[9,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[9,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[9,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[9,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[9,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[9,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[9,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 42 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(12) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=18, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' > F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3))

# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> D=3"), con=.8, cov=.8, mydata, type = "mv"))
  # Run CNA and QCA 
  cna.sol <- tryTest(mvcna(dat1, ordering=list("D"), strict=T, con=.8, cov=.8, con.msc=.7,
                             maxstep = c(3,4,12)))
  qca.sol <- tryTest(eQMC(dat1, outcome = "D{3}", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}

# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[10,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[10,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[10,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[10,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[10,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[10,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[10,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[10,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[10,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[10,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[10,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[10,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[10,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[10,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 43 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(13)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=95, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3)) 


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> E=2"), con=.8, cov=.8, mydata, type = "mv"))
  # Eliminate the relevant factor C
  dat2 <- dat1[,c("A","B","D", "E")]
  # Run CNA and QCA 
  cna.sol <- tryTest(mvcna(dat2, ordering=list("E"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,6,14), rm.dup.factors=F, rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E{2}", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[11,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[11,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[11,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[11,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[11,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[11,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[11,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[11,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -----------------------------
# QCA
table(sapply(qca.list, length))
amb_score[11,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[11,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[11,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[11,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[11,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[11,8] <- na.zero(table(unique.model.cna)["TRUE"])/n

# Test 44 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         perfect solution consistencies and coverages, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(9) 
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3,3,3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases(paste0(dgs[i]," <-> E=2"), mydata, type = "mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C", "D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat3, ordering=list("E"), strict=T, con=1, cov=.75, con.msc=.75,
                            maxstep = c(3,6,14), rm.const.factors=F))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E{2}", incl.cut1=1, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[12,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[12,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[12,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[12,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[12,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[12,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[12,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[12,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[12,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[12,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[12,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[12,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[12,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[12,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 45 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         no data fragmentation,
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
#         ATTENTION: this test will take about 3 minutes to terminate 
# -------------------------------------------------------------------------------
# Test setup 
# ----------
# number of trials 
n <- 30 

# Seeds and output lists 
set.seed(35)   
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' > F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> E=2"), con= .8, cov=.8, mydata, type = "mv"))
  # Eliminate the relevant factor A
  dat2 <- dat1[,c("B","C","D","E","F")]
  # Run CNA and QCA [Consistencies are set to lower values than the targeted 
  # DGS in order to strike an optimal balance between informativeness and correctness of
  # the output. The reader is explicitly invited to play around with these values. 
  # It will be seen that at higher consistency cutoffs the correctness ratio of QCA tends
  # towards zero.]
  cna.sol <- tryTest(mvcna(dat2, ordering=list("E"), strict=T, con=.6, cov=.6,
                             maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat2, outcome = "E{2}", incl.cut1=.6))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[13,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[13,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[13,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[13,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[13,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[13,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[13,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[13,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# -------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[13,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[13,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[13,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[13,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[13,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[13,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 46 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         no relevant factor omitted (no model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 50

# Seeds and output lists 
set.seed(19) # 17
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D}, where D=3 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C"), seed.1=18, seed.2=47,
                  prob = 0.4, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> D=3"), con=.8, cov=.8, mydata, type ="mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat2, ordering=list("D"), strict=T, con=.8, cov=.8,
                            maxstep = c(3,6,14), rm.dup.factors = F, rm.const.factors= F))
  qca.sol <- tryTest(eQMC(dat2, outcome = "D{3}", incl.cut1= .8))
  cna.list[[i]] <- getSolution(cna.sol, "D=3")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[14,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[14,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# ------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[14,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[14,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[14,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[14,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[14,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[14,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[14,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[14,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[14,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[14,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[14,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[14,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 47 ------------------------------------------------------------------------
#         no irrelevant factor included (no model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(2)  
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C", "D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1", 
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' = F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> E=2"), con = .8, cov = .8, mydata, type = "mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Eliminate the relevant factor A 
  dat3 <- dat2[,c("B","C","D","E")]
  # Run CNA and QCA  [Consistencies are set to lower values than the targeted 
  # DGS in order to strike an optimal balance between informativeness and correctness of
  # the output. The reader is explicitly invited to play around with these values. 
  # It will be seen that at higher consistency cutoffs the correctness ratio of QCA tends
  # towards zero.]
  cna.sol <- tryTest(mvcna(dat3, ordering=list("E"), strict = T, con = .8, cov = .75, con.msc= .7,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E{2}", incl.cut1= .6, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[15,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[15,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n

# Correctness without ambiguities evaluation
# -------------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[15,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[15,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[15,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[15,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n

# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[15,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[15,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# ------------------------------
# QCA
table(sapply(qca.list, length))
amb_score[15,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[15,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# -------------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[15,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[15,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[15,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[15,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# Test 48 ------------------------------------------------------------------------
#         IRRELEVANT FACTOR INCLUDED (model overspecification),
#         RELEVANT FACTOR OMITTED (model underspecification),
#         DATA FRAGMENTATION (only 20% of the possible cases observed),
#         IMPERFECT SOLUTION CONSISTENCIES AND COVERAGES, varying DGS
# -------------------------------------------------------------------------------
# Test setup
# ----------
# number of trials 
n <- 30

# Seeds and output lists 
set.seed(25)
seeds <- sample(.Machine$integer.max, n)
cna.list <- qca.list <- vector("list", n)

# Generating n random models from frame F = {A,B,C,D,E}, where E=2 is the outcome:
sols <- randomDGS(n.DGS = n,exo.facs = c("A","B","C","D"), seed.1=25, seed.2=47,
                  prob = 0.8, diversity = 1, delete.trivial = TRUE)$DGS
sols <- addStar(sols)

# Multi-value assignments
dgs <- modifyStr(sols, A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
                 D = "D=3", d = "D=1")

# Generating the space of all logically possible configurations from frame 
# F' > F, where each factor has 3 possible values {1,2,3}:
mydata <- allCombs(c(3, 3, 3, 3, 3, 3))


# Test LOOP
# ---------
for (i in seq_len(n)){
  cat(i, "\n")
  set.seed(seeds[i]) 
  # select cases that are compatible with structure i to degrees con = .8, cov = .8
  dat1 <- tt2df(selectCases1(paste0(dgs[i]," <-> E=2"), con = .8, cov = .8, mydata, type ="mv"))
  # Sample random rows with diversity index about 20%
  dat2 <- dat1[sample(nrow(dat1),(nrow(dat1)/100)*20, replace = F),]
  # Eliminate the relevant factor A
  dat3 <- dat2[,c("B","C","D","E","F")]
  # Run CNA and QCA
  cna.sol <- tryTest(mvcna(dat3, ordering=list("E"), strict = T, con = .8, cov = .75, con.msc=.7,
                            maxstep = c(3,6,14)))
  qca.sol <- tryTest(eQMC(dat3, outcome = "E{2}", incl.cut1= .8, details=T))
  cna.list[[i]] <- getSolution(cna.sol, "E=2")
  qca.list[[i]] <- getSolution(qca.sol)
}


# Correctness evaluation
# ----------------------
# Generating the correctness-preserving models by using the submodels
# function from QCApro and then manually adding the same multi-value assignments
# chosen for the data-generating structures in the test setup.
correctModels <- vector("list", n)
for (i in seq_len(n)){
  solNoStar <- removeStar(standardizedCond(sols[i]))
  corMod <- addStar(submodels.var(solNoStar)$submodels)
  correctModels[[i]] <- corMod[nzchar(corMod)]  # remove "empty" solution"
}
# Multi-value assignments from the test setup and standardize Syntax:
correctModels <-
  lapply(correctModels, modifyStr, 
         A = "A=1", a = "A=2", B = "B=2", b = "B=3", C = "C=2", c = "C=1",
         D = "D=3", d = "D=1")
correctModels <-
  lapply(correctModels, standardizedCond)

# Define score lists
correctSolution.cna <- correctSolution.qca <- logical(n)
completeSolution.cna <- completeSolution.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  correctSolution.qca[i] <- matchCond(qca.list[[i]], correctModels[[i]])
}
table(correctSolution.qca)
cor_score[16,9] <- na.zero(table(correctSolution.qca)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  correctSolution.cna[i] <- matchCond(cna.list[[i]], correctModels[[i]])
}
table(correctSolution.cna)
cor_score[16,8] <- na.zero(table(correctSolution.cna)["TRUE"])/n


# Correctness without ambiguities evaluation
# -----------------------------------------
# Define score lists
corr.cna.without <- corr.qca.without <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.without[i] <- matchCond.without(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.without)
cor_without_score[16,9] <- na.zero(table(corr.qca.without)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.without[i] <- matchCond.without(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.without)
cor_without_score[16,8] <- na.zero(table(corr.cna.without)["TRUE"])/n

# Correctness by unique model
# ---------------------------
# Define score lists
corr.cna.unique <- corr.qca.unique <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  corr.qca.unique[i] <- matchCond.unique(qca.list[[i]], correctModels[[i]])
}
table(corr.qca.unique)
cor_unique_score[16,9] <- na.zero(table(corr.qca.unique)["TRUE"])/n

# CNA Solution
for (i in seq_len(n)) {
  corr.cna.unique[i] <- matchCond.unique(cna.list[[i]], correctModels[[i]])
}
table(corr.cna.unique)
cor_unique_score[16,8] <- na.zero(table(corr.cna.unique)["TRUE"])/n


# Completeness evaluation
# -----------------------
# QCA
for (i in seq_len(n)) {
  completeSolution.qca[i] <- standardizedCond(dgs[i]) %in% standardizedCond(qca.list[[i]])
}
table(completeSolution.qca)
com_score[16,9] <- na.zero(table(completeSolution.qca)["TRUE"])/n

# CNA
for (i in seq(n)) {
  completeSolution.cna[i] <- standardizedCond(dgs[i]) %in% standardizedCond(cna.list[[i]])
}
table(completeSolution.cna)
com_score[16,8] <- na.zero(table(completeSolution.cna)["TRUE"])/n

# Ratio of model ambiguities
# --------------------------
# QCA
table(sapply(qca.list, length))
amb_score[16,9] <- length(which(sapply(qca.list, length)>1))/n
# CNA
table(sapply(cna.list, length))
amb_score[16,8] <- length(which(sapply(cna.list, length)>1))/n

# Ratio of no issued model
# ------------------------
# Define score lists
no.model.cna <- no.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  no.model.qca[i] <- no.model(qca.list[[i]])
}
no_model_score[16,9] <- na.zero(table(no.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  no.model.cna[i] <- no.model(cna.list[[i]])
}
no_model_score[16,8] <- na.zero(table(no.model.cna)["TRUE"])/n

# Ratio of unique model
# ------------------------
# Define score lists
unique.model.cna <- unique.model.qca <- logical(n)

# QCA Solution 
for (i in seq_len(n)) {
  unique.model.qca[i] <- unique.model(qca.list[[i]])
}
unique_model_score[16,9] <- na.zero(table(unique.model.qca)["TRUE"])/n

# CNA Solution 
for (i in seq_len(n)) {
  unique.model.cna[i] <- unique.model(cna.list[[i]])
}
unique_model_score[16,8] <- na.zero(table(unique.model.cna)["TRUE"])/n


# OVERALL RESULT --------------------------------
# #############

# Correctness score
round(cor_score,2)

# Correctness without ambiguities score
round(cor_without_score,2)

# Correctness by unique model score
round(cor_unique_score,2)

# Completeness score
round(com_score,2)

# Ambiguity ratio score
round(amb_score,2)

# "No model" ratio score
round(no_model_score,2)

# Unique model ratio score
round(unique_model_score,2)


# PLOTS --------------
# #################


theme_set(theme_bw())


# CORRECTNESS
# -----------
score2 <- cbind(round(cor_score,2),paste0(row.names(cor_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.cor <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                         "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.cor <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                         "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.cor <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                         "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.cor.tex", width = 6, height = 2)
print(plot1.cor)
dev.off()

tikz(file = "plot2.cor.tex", width = 6, height = 2)
print(plot2.cor)
dev.off()

tikz(file = "plot3.cor.tex", width = 6, height = 2)
print(plot3.cor)
dev.off()



# COMPLETENESS
# ------------

score2 <- cbind(round(com_score,2),paste0(row.names(com_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.com <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                   "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.com <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                   "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.com <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                   "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.com.tex", width = 6, height = 2)
print(plot1.com)
dev.off()

tikz(file = "plot2.com.tex", width = 6, height = 2)
print(plot2.com)
dev.off()

tikz(file = "plot3.com.tex", width = 6, height = 2)
print(plot3.com)
dev.off()


# RATIOS OF NO MODELS
# -------------------
score2 <- cbind(round(no_model_score,2),paste0(row.names(no_model_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.no.model <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                   "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.no.model <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                   "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.no.model <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                   "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.no.model.tex", width = 6, height = 2)
print(plot1.no.model)
dev.off()

tikz(file = "plot2.no.model.tex", width = 6, height = 2)
print(plot2.no.model)
dev.off()

tikz(file = "plot3.no.model.tex", width = 6, height = 2)
print(plot3.no.model)
dev.off()


# RATIOS OF MULTIPLE MODELS
# -------------------------

score2 <- cbind(round(amb_score,2),paste0(row.names(amb_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.amb <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                   "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.amb <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                   "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.amb <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                   "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.amb.tex", width = 6, height = 2)
print(plot1.amb)
dev.off()

tikz(file = "plot2.amb.tex", width = 6, height = 2)
print(plot2.amb)
dev.off()

tikz(file = "plot3.amb.tex", width = 6, height = 2)
print(plot3.amb)
dev.off()


# RATIOS OF EXACTLY ONE MODELL
# -----------------------------
score2 <- cbind(round(unique_model_score,2),paste0(row.names(unique_model_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.unique.model <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                   "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.unique.model <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                   "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.unique.model <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                   "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.unique.model.tex", width = 6, height = 2)
print(plot1.unique.model)
dev.off()

tikz(file = "plot2.unique.model.tex", width = 6, height = 2)
print(plot2.unique.model)
dev.off()

tikz(file = "plot3.unique.model.tex", width = 6, height = 2)
print(plot3.unique.model)
dev.off()


# CORRECTNESS BY UNIQUE MODELS
# -------------------------------

score2 <- cbind(round(cor_unique_score,2),paste0(row.names(cor_unique_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.cor.unique <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                   "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.cor.unique <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                   "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.cor.unique <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                   "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.cor.unique.tex", width = 6, height = 2)
print(plot1.cor.unique)
dev.off()

tikz(file = "plot2.cor.unique.tex", width = 6, height = 2)
print(plot2.cor.unique)
dev.off()

tikz(file = "plot3.cor.unique.tex", width = 6, height = 2)
print(plot3.cor.unique)
dev.off()


# CORRECTNESS WITHOUT AMBIGUITIES
# -------------------------------

score2 <- cbind(round(cor_without_score,2),paste0(row.names(cor_without_score), sep=" ", paste0("(",1:16,")")))
colnames(score2) <- c("n", "csCNA", "csQCA", "n", "fsCNA", "fsQCA", "n", "mvCNA", "mvQCA", "test_types")
row.names(score2) <- NULL
score3 <- data.frame(score2)
score3$test_types2 <- factor(score3$test_types, as.character(score3$test_types))
score4 <- melt(score3[,c(2,3,5,6,8,9,11)],id.vars='test_types2',variable.name = 'method')
colnames(score4) <- c("test_types", "method", "ratios")
score4$ratios <- as.numeric(score4$ratios)

# Crisp Data
plot1.cor.without <- ggplot(score4[1:32,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("csCNA" = "black", 
                                   "csQCA" = "grey"))+
  labs(title="Crisp-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Fuzzy Data
plot2.cor.without <- ggplot(score4[33:64,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("fsCNA" = "black", 
                                   "fsQCA" = "grey"))+
  labs(title="Fuzzy-set data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

# Multi-value Data
plot3.cor.without <- ggplot(score4[65:96,], aes(test_types,ratios, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width=.5)+
  scale_fill_manual("", values = c("mvCNA" = "black", 
                                   "mvQCA" = "grey"))+
  labs(title="Multi-value data")+
  theme(plot.title = element_text(size = 9))+
  scale_x_discrete(name ="")+
  scale_y_continuous(name="", limits=c(0, 1))+
  theme(axis.text.x = element_text(size=8, angle=45,hjust = 1))

options(tz="CA")
tikz(file = "plot1.cor.without.tex", width = 6, height = 2)
print(plot1.cor.without)
dev.off()

tikz(file = "plot2.cor.without.tex", width = 6, height = 2)
print(plot2.cor.without)
dev.off()

tikz(file = "plot3.cor.without.tex", width = 6, height = 2)
print(plot3.cor.without)
dev.off()
