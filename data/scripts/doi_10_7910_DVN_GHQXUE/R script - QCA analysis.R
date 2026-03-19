rm(list = ls())

library(SetMethods)

## Import calibrated data
DFnew <- read.csv("Complete calibrated data.csv", header = T, row.names = 1)
head(DFnew)

##### Appendix: Calibration (histograms) #####
# MOS
hist(DFnew$MOS, main = "Distribution of MOS", xlab = "MOS (cal)")

# UNSOS
hist(DFnew$UNSOS, main = "Distribution of UNSOS", xlab = "UNSOS (cal)")

# RES
hist(DFnew$RES, main = "Distribution of RES", xlab = "RES (cal)")

# POL
hist(DFnew$POL, main = "Distribution of POL", xlab = "POL (cal)")

# MARKET&COMB
hist(DFnew$MARCOMB, main = "Distribution of MARCOMB", xlab = "MARCOMB")

#STATE&COMB
hist(DFnew$STACOMB, main = "Distribution of STACOMB", xlab = "STACOMB")

#### Analyses for STATE(~MARKET&COMB) ####

### Necessity for STATE(~MARKET&COMB)
SUIN_nmc <- superSubset(data = DFnew, 
                        outcome = "~MARCOMB",
                        conditions = c("MOS", "UNSOS", "POL", "RES"),
                        relation = "nec", 
                        incl.cut = 0.8, 
                        cov.cut = 0.5, 
                        ron.cut = 0.5)
SUIN_nmc

pimplot(data = DFnew, results = SUIN_nmc, outcome = "~MARCOMB", 
        neg.out = T, necessity = T, all_labels = T, 
        jitter = T)

### Sufficiency for STATE (~MARCOMB)
## Truth table for STATE (~MARCOMB)
tt_nmc <- truthTable(DFnew, 
                     outcome = "MARCOMB", 
                     conditions = c("MOS", "UNSOS", "POL", "RES"),
                     incl.cut = 0.750, 
                     show.cases = T, 
                     sort.by = c("out", "n", "incl"), 
                     neg.out = T)
tt_nmc

# Remove TT rows with low PRI
tt_nmc$tt[c("12"), "OUT"] <- 0

# Final truth table for STATE (~MARCOMB)
tt_nmc

## Minimise
# Conservative solution
sol_nmcc <- minimize(tt_nmc, use.tilde = T, details = T)
sol_nmcc

# Parsimonious solution
sol_nmcp <- minimize(tt_nmc, use.tilde = T, details = T, 
                     include = "?")
sol_nmcp

# Intermediate solution
sol_nmci <- minimize (tt_nmc, details = T, 
                      include = "?", dir.exp = c("0, 0, 0, 0"))
sol_nmci

# Sufficiency plot
pimplot(data = DFnew, results = sol_nmci, outcome = "MARCOMB", 
        neg.out = T, jitter = T, all_labels = T)

#### Analyses for MARKET&COMB ####

### Necessity for MARKET&COMB
SUIN_ymc <- superSubset(data = DFnew, 
                        outcome = "MARCOMB",
                        conditions = c("MOS", "UNSOS", "POL", "RES"),
                        relation = "nec", 
                        incl.cut = 0.8, 
                        cov.cut = 0.5, 
                        ron.cut = 0.5)
SUIN_ymc

pimplot(data = DFnew, results = SUIN_ymc, outcome = "MARCOMB", 
        necessity = T, all_labels = T, jitter = T)

### Sufficiency for MARKET&COMB
## Truth table for MARKET&COMB
TT_ymc <- truthTable(DFnew, outcome = "MARCOMB",
                     conditions = c("MOS", "UNSOS", "POL", "RES"),
                     incl.cut = 0.75, 
                     show.cases = T, 
                     sort.by = c("out", "incl", "n"),
                     complete = T)
TT_ymc

# Remove TT rows with low PRI
TT_ymc$tt[c("5", "8"), "OUT"] <- 0

# Final truth table - MARKET&COMB
TT_ymc

### Minimise
# Conservative solution
sol_ymcc <- minimize(TT_ymc, use.tilde = T, 
                     details = T)
sol_ymcc

# Parsimonious solution
sol_ymcp <- minimize(TT_ymc, use.tilde = T, 
                     details = T, 
                     include = "?")
sol_ymcp

# Intermediate solution
sol_ymci <- minimize(TT_ymc, 
                     details = T, 
                     include = "?", 
                     dir.exp = c("1, 1, 1, 1"))
sol_ymci

#Sufficiency plots
pimplot(data = DFnew, results = sol_ymci, outcome = "MARCOMB",
        all_labels = T, jitter = T)


### Enhanced Standard Analysis (tests)
CSAmc<- intersect(rownames(sol_ymci$SA), rownames(sol_nmci$SA))
CSAmc

SSRmc <- intersect(rownames(TT_ymc$tt)[TT_ymc$tt$OUT==1],
                   rownames(tt_nmc$tt)[tt_nmc$tt$OUT==1])
SSRmc

# Remove conflicting cases
tt_esa <- esa(tt_nmc, nec_cond = "~MOS")
tt_esa

sol_nesami <- minimize(tt_esa, details = T, include = "?", 
                       dir.exp = c("0, 0, 0, 0"))
sol_nesami

pimplot(data = DFnew, results = sol_nesami, outcome = "MARCOMB", 
        neg.out = T, jitter = T, all_labels = T)

#### Analyses for MARKET (~STATE&COMB) ####

### Necessity for MARKET(~STATE&COMB)
SUIN_nsc <- superSubset(data = DFnew, 
                        outcome = "~STACOMB",
                        conditions = c("MOS", "UNSOS", "POL", "RES", "MEMB"),
                        relation = "nec", 
                        incl.cut = 0.8, 
                        cov.cut = 0.5, 
                        ron.cut = 0.5)
# Necessary conditions
SUIN_nsc

# Necessity plots
pimplot(data = DFnew, results = SUIN_nsc, outcome = "~STACOMB", 
        neg.out = T, necessity = T,
        all_labels = T, jitter = T)

### Sufficiency for MARKET (~STATE&COMB)
## Truth table for MARKET (~STATE&COMB)
tt_nsc <- truthTable(DFnew, 
                     outcome = "STACOMB", 
                     conditions = c("MOS", "UNSOS", "POL", "RES"),
                     incl.cut = 0.750, 
                     show.cases = T, 
                     sort.by = c("out", "n", "incl"), 
                     neg.out = T)
tt_nsc

## Minimise
# Conservative solution
sol_nscc <- minimize(tt_nsc, use.tilde = T, details = T)
sol_nscc

# Parsimonious solution
sol_nscp <- minimize(tt_nsc, use.tilde = T, details = T, 
                     include = "?")
sol_nscp

# Intermediate solution
sol_nsci <- minimize (tt_nsc, details = T, 
                      include = "?", dir.exp = c("1, 1, 1, 1"))
sol_nsci

# Sufficiency plots
pimplot(data = DFnew, results = sol_nsci, outcome = "STACOMB", 
        all_labels = T, jitter = T, neg.out = T)

##### ANALYSES FOR STATE&COMB #####

### Necessity for STATE&COMB
SUIN_ysc <- superSubset(data = DFnew, 
                        outcome = "STACOMB",
                        conditions = c("MOS", "UNSOS", "POL", "RES"),
                        relation = "nec", 
                        incl.cut = 0.8, 
                        cov.cut = 0.5, 
                        ron.cut = 0.5)
SUIN_ysc

pimplot(data = DFnew, results = SUIN_ysc, outcome = "STACOMB", sol = 1,
        necessity = T, jitter = T, all_labels = T)

### Sufficiency for MARKET(~STATE&COMB)
## Truth table for MARKET(~STATE&COMB)
TT_ysc <- truthTable(DFnew, outcome = "STACOMB",
                     conditions = c("MOS", "UNSOS", "POL", "RES"),
                     incl.cut = 0.8, 
                     show.cases = T, 
                     sort.by = c("out", "incl", "n"),
                     complete = T)
TT_ysc

## Minimise
# Conservative solution
sol_yscc <- minimize(TT_ysc, use.tilde = T, 
                     details = T)
sol_yscc

# Parsimonious solution
sol_yscp <- minimize(TT_ysc, use.tilde = T, 
                     details = T, 
                     include = "?")
sol_yscp

# Intermediate solution
sol_ysci <- minimize(TT_ysc, 
                     details = T, 
                     include = "?", 
                     dir.exp = c("0, 0, 0, 0"))
sol_ysci

# Sufficiency plots
pimplot(data = DFnew, results = sol_ysci, outcome = "STACOMB", 
        jitter = T, all_labels = T)

### Enhanced Standard Analysis (tests)
CSAsc<- intersect(rownames(sol_ysci$SA), rownames(sol_nsci$SA))
CSAsc

SSRsc <- intersect(rownames(TT_ysc$tt)[TT_ysc$tt$OUT==1],
                   rownames(tt_nsc$tt)[tt_nsc$tt$OUT==1])
SSRsc

tt_nesa <- esa(tt_nsc, nec_cond = c("MOS", "~POL"))
tt_nesa

sol_nesac <- minimize (tt_nesa, details = T, use.tilde = T)
sol_nesac

sol_nesap <- minimize(tt_nesa, details = T, use.tilde = T, 
                      include = "?")
sol_nesap

sol_nesai<- minimize(tt_nesa, details = T, include = "?", 
                     dir.exp = c("1, 1, 1, 1"))

sol_nesai

pimplot(data = DFnew, results = sol_nesai, outcome = "STACOMB", 
        all_labels = T, jitter = T, neg.out = T)