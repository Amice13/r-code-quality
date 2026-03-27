# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) #free up memory

# Packages for data processing, parallel computing, simulation and estimation
libdat <- c("haven") # to read dta file
libest <- c("glmnet", "sandwich", "stats")
libpar <- c("foreach", "iterators", "parallel", "doParallel")
# If packages not already installed, install them first
lapply(append(append(libdat, libest), libpar), require, character.only = TRUE)

source("bcvBinary.R") # own estimation and inference routines

## DATA PROCESSING

# Unzip the data file
unzip("application/ppcs_full.zip", exdir = "application")

# Load PPCS data
ppcs <- read_dta("application/ppcs_full.dta")

# Removing other forces (as in Fryer)
table(ppcs$anyuseofforce_coded, useNA  = "ifany")
ppcs$anyuseofforce_coded[ppcs$oforce_other == 1 & ppcs$anyuseofforce_coded == 0] <- NA
table(ppcs$anyuseofforce_coded, useNA  = "ifany")
# ^- consequence: 108 zeros -> NA

# Grouping variables as in Fryer and using same naming
# Race dummies [reference = "white" (and thus observed)]:
p_srace <- c("r_sblack", "r_shisp", "r_sother", "M_sblack")
# ^- no one is missing race, so M_sblack has no variance
p_sdemo <- c("r_smale", "M_smale", "r_sage", "r_sage_2", "M_sage",
             "r_sempl", "M_sempl", "r_sincome", "M_sincome", "r_spop", "M_spop")
# ^- no one is missing sex or age, M_smale and M_sage have no variance
p_echar <- c("r_daytime", "M_daytime", "r_inctype_lin", "M_inctype_lin",
             "r_omajblack", "r_omajhisp", "r_omajwhite", "r_omajother",
             "r_osplit", "M_omajblack")
p_sbeh <- c("r_sbehavior", "M_sbehavior")
p_fe <- "year"

# Collect variables listed in Table 2, Row l
row_l <- c(p_srace, p_sdemo, p_echar, p_sbeh, p_fe)

# Find complete cases
ppcs_l <- ppcs[append(row_l[!is.na(row_l)], "anyuseofforce_coded")]
rm(ppcs) # no further need for full PPCS => drop it
ppcs_l <- ppcs_l[complete.cases(ppcs_l), ]
# ^- n = 59,668 observations (as in Fryer)
# Note: this total is after introducing missing indicators and recoding.
# It is *not* the number of complete observations.

# Removing variables with zero variance and exact multicollinearity
l_var <- apply(ppcs_l[row_l[!is.na(row_l)]], 2, var) > 0
row_l_var <- row_l[l_var]
# ^- drops "M_black", "M_smale", "M_age" (b/o no one missing race, gender, age)

# The following encounter characteristics sum to one by construction:
# "r_omajblack", "r_omajhisp", "r_omajwhite",
# "r_omajother", "r_osplit" and "M_omajblack"
# Drop one to avoid dummy trap. (Stata drops the latter for Fryer.)
# We follow suit -v
row_l_var <- setdiff(row_l_var, "M_omajblack")

# The missing indicators "M_sincome" and "M_spop" (subject population/income)
# are equal throughout PPCS. We drop the latter.
row_l_var <- setdiff(row_l_var, "M_spop")

# This leaves us with 23 regressors in addition to the outcome
ppcs_l <- ppcs_l[append(row_l_var, "anyuseofforce_coded")]

# Inspection shows all remaining variables are integer -v
str(ppcs_l)

# Converting to integer to save memory -v
ppcs_l[] <- lapply(ppcs_l, as.integer) # the "[]" keeps the dataframe structure

## ESTIMATION

# REPRODUCING FRYER
# Reproducing Table 2, Panel B, Row l
fmla_l <- reformulate(row_l_var, response = "anyuseofforce_coded")
fit_l <- glm(fmla_l, data = ppcs_l, family = binomial(link = "logit"))
sumfit_l <- summary(fit_l)$coefficients
racedum <- c("r_sblack", "r_shisp", "r_sother")
coef_l <- sumfit_l[racedum, 1] # coef estimates
rse_l <- sqrt(diag(vcovHC(fit_l, type = "HC1"))[racedum]) # robust se(coef)
or_l <- exp(coef_l) # odds ratios
rseor_l <- rse_l * or_l # robust se(odds ratio)
fryer_row_l <- rbind(or_l, rseor_l) # Row l reproduced

# EXPANDING ON FRYER

# (1) TREATING FACTOR VARIABLES AS SUCH (still keeping regressors as in Fryer)

# (i) Including income bracket as factor (in Fryer: linear)
ppcs_l$r_sincome <- as.factor(ppcs_l$r_sincome)
levels(ppcs_l$r_sincome) <- list("missing" = 0, "0_20k" = 1,
                                 "20k_50k" = 2, "50kplus" = 3)
incomedum <- model.matrix(~ 0 + r_sincome, ppcs_l) # create dummies for levels
incomedum <- incomedum[, !colnames(incomedum) %in%
                         c("r_sincomemissing", "r_sincome0_20k")]
# ^- dropping dummy for missing income bracket (which already exists as
# "M_sincome") and making the lower bracket the reference income

# (ii) Including incident type as factor (in Fryer: linear)
names(ppcs_l)[names(ppcs_l) == "M_inctype_lin"] <- "M_inctype"
names(ppcs_l)[names(ppcs_l) == "r_inctype_lin"] <- "r_inctype"
ppcs_l$r_inctype <- as.factor(ppcs_l$r_inctype) # create dummies for levels
levels(ppcs_l$r_inctype) <- list("missing" = 0, "street" = 1,
                                 "traffic" = 2, "other" = 3)
inctypedum <- model.matrix(~ 0 + r_inctype, ppcs_l)
inctypedum <- inctypedum[, !colnames(inctypedum) %in%
                           c("r_inctypemissing", "r_inctypestreet")]
# ^- dropping dummy for missing incident type (which already exists as
# "M_inctype") and making street stop the reference incident

# (iii) Including population size as factor (in Fryer: linear)
ppcs_l$r_spop <- as.factor(ppcs_l$r_spop)
levels(ppcs_l$r_spop) <- list("missing" = 0, "0_100k" = 1, "100k_500k" = 2,
                              "500k_1m" = 3, "1mplus" = 4)
popdum <- model.matrix(~ 0 + r_spop, ppcs_l) # create dummies for levels
popdum <- popdum[, !colnames(popdum) %in% c("r_spopmissing", "r_spop0_100k")]
# ^- dropping dummy for missing population size (which is still equal to that of
# subject income) and making the smaller population size the reference

# (iv) Converting year to dummies (in Fryer: linear trend) and dropping 1996
# (=first year)
ppcs_l$year <- as.factor(ppcs_l$year)
yeardum <- model.matrix(~ 0 + year, ppcs_l) # create dummies for levels
yeardum <- yeardum[, !colnames(yeardum) %in% "year1996"]

# Replace factor variables with dummies for levels
ppcs_l_dum <- data.frame(ppcs_l[, setdiff(colnames(ppcs_l),
                                          c("r_inctype", "r_sincome",
                                            "r_spop", "year"))],
                         inctypedum, incomedum, popdum, yeardum)

# Finally, subject income info was only gathered in some years and then for all
# subjects, cf.
table(ppcs_l$M_sincome, ppcs_l$year)
# so M_sincome or an additional year dummy must be dropped due to perfect
# multicollinearity (with the constant regressor, implicit in the intercept).
# Specifically, we have M_sincome = year1996 + year2008 so M_sincome + year1999
# + year2002 + year2005 + year2011 = 1 for all rows. We here drop M_sincome and
# include an intercept.
ppcs_l_dum <- ppcs_l_dum[, !colnames(ppcs_l_dum) %in% "M_sincome"]
# Now the regressor matrix (including a column of ones) is full rank.

# Create Row l' (= Fryer's Row l with factor variables treated as such)
xnames <- setdiff(colnames(ppcs_l_dum), "anyuseofforce_coded")
fmla_l_dum <- reformulate(xnames, response = "anyuseofforce_coded")
links <- c("logit", "probit")
numlinks <- length(links)
fits_unpen_l_dum <- vector(mode = "list", length = numlinks)
tictocs_unpen <- array(NA, dim = numlinks, dimnames = list(timeused = links))
for (thislink in 1:numlinks) {
  tictoc <- system.time(
    fit_l_dum <- glm(fmla_l_dum, data = ppcs_l_dum,
                     family = binomial(link = links[thislink]), maxit = 50)
  )
  fits_unpen_l_dum[[thislink]] <- fit_l_dum
  tictocs_unpen[thislink] <- tictoc[3]
}
tstats_unpen_l_dum <- matrix(NA, nrow = 1, ncol = numlinks,
                             dimnames = list(tstat = "t", link = links))
for (thislink in 1:numlinks) {
  fit_l_dum <- fits_unpen_l_dum[[thislink]]
  sumfit_l_dum <- summary(fit_l_dum)$coefficients
  coef_l_dum <- sumfit_l_dum["r_sblack", 1] # coef estimates
  rse_l_dum <- sqrt(diag(vcovHC(fit_l_dum, type = "HC1"))["r_sblack"])
  # ^-- robust se of coef
  tstats_unpen_l_dum[thislink] <- coef_l_dum / rse_l_dum
}
# Notes: Comparing odds-ratio (=exp(coef) in logit) obtained with dummies to the
# one in Fryer's Table 2, Row l, shows difference only at first decimals. The
# standard error of the odds-ratio (=se(coef) * exp(coef) in logit) is very
# close too.
tstats_unpen_l_dum
# ^-- t-values in Table 1 for "Basic Controls" and "Unpenalized MLE"

# Uncomment for odds-ratios --v
# exp(summary(fits_unpen_l_dum[[1]])$coefficients[racedum,1])
# Uncomment for standard errors of odds-ratios --v
# summary(fits_unpen_l_dum[[1]])$coefficients[racedum,2] * exp(summary(fits_unpen_l_dum[[1]])$coefficients[racedum,1])

# Our methods applied to Row l' setting
x <- as.matrix(ppcs_l_dum[, xnames]) # regressors
xs <- Matrix(x, sparse = TRUE) # ... as sparse matrix
y <- as.matrix(ppcs_l_dum$anyuseofforce_coded) # outcome
ys <- Matrix(y, sparse = TRUE) # ... as sparse matrix
links <- c("logit", "probit")
numlinks <- length(links)
fits_postbcv3_l_dum <- vector(mode = "list", length = numlinks)
tictocs_dum <- array(NA, dim = numlinks, dimnames = list(timeused = links))
for (thislink in 1:numlinks) {
  # Parallel computing and randomization
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  RNGkind(normal.kind = "Kinderman-Ramage") # faster normal draws
  set.seed(2345) # seed (for reproducibility)
  link <- links[thislink]
  tictoc <- system.time(
    postbcv3_fit <- debias_post_bcv(xs, ys, link = link, parallel = TRUE)
  )
  fits_postbcv3_l_dum[[thislink]] <- postbcv3_fit
  tictocs_dum[thislink] <- tictoc[3]
  stopCluster(cl)
}

bhats_postbcv3_l_dum <- array(NA, dim = numlinks)
sebhats_postbcv3_l_dum <- array(NA, dim = numlinks)
tstats_postbcv3_l_dum <- array(NA, dim = numlinks)
n <- nrow(x)
for (thislink in 1:numlinks) {
  bhat <- fits_postbcv3_l_dum[[thislink]]$bhat # coef on "r_sblack"
  vhat <- fits_postbcv3_l_dum[[thislink]]$vhat # variance estimate
  sebhat <- sqrt(vhat / n) # se of coef
  bhats_postbcv3_l_dum[thislink] <- bhat
  sebhats_postbcv3_l_dum[thislink] <- sebhat
  tstats_postbcv3_l_dum[thislink] <- bhat / sebhat # t-stat
}
bhats_postbcv3_l_dum
sebhats_postbcv3_l_dum
tstats_postbcv3_l_dum
# ^-- t-values in Table 1 for "Basic Controls" and "Post-BCV"

# (2) INCLUDING FIRST-ORDER INTERACTIONS (thus blowing up the regressor set)

# Row l''

# Creating first-order interactions of all but race and outcome dummies
tointeract <- setdiff(names(ppcs_l_dum), c(racedum, "anyuseofforce_coded"))

ppcs_l_interact <- model.matrix(~ 0 + .^2,
                                ppcs_l_dum[, tointeract]) # inter's and levels
interact_var <- apply(ppcs_l_interact, 2, var) > 0 # check for zero variance
ppcs_l_interact <- ppcs_l_interact[, interact_var] # keeping nonconstants

ppcs_l_x_mod <- data.frame(ppcs_l[racedum],
                           ppcs_l_interact) # bring back race dummies
xnames <- colnames(ppcs_l_x_mod) # regressor names
ppcs_l_mod <- data.frame(ppcs_l_x_mod,
                         ppcs_l["anyuseofforce_coded"]) # bring back outcome
ppcs_l_mod[] <- lapply(ppcs_l_mod, as.integer) # store as integers for memory
# ^- the "[]" keeps the dataframe structure

# Fit _without_ penalization (***WARNING: DOES NOT CONVERGE***)
# fmla <- reformulate(xnames, response = "anyuseofforce_coded") 
# fit_l_mod <- glm(fmla, data = ppcs_l_mod, family = binomial(link = "logit"))
# Outputs -v
# Warning messages:
# 1: glm.fit: algorithm did not converge
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred
# sumfit_mod <- summary(fit_l_mod)$coefficients[racedum, 1]
# ^- Coefficients are bananas. No point in reporting this.

# Our methods applied to Row l' setting
x <- as.matrix(ppcs_l_mod[, xnames]) # regressors
xs <- Matrix(x, sparse = TRUE) # ... as sparse matrix
y <- as.matrix(ppcs_l_mod$anyuseofforce_coded) # outcome
ys <- Matrix(y, sparse = TRUE) # ... as sparse matrix
links <- c("logit", "probit")
numlinks <- length(links)
fits_postbcv3_l_mod <- vector(mode = "list", length = numlinks)
tictocs_mod <- array(NA, dim = numlinks, dimnames = list(timeused = links))
for (thislink in 1:numlinks) {
  # Parallel computing and randomization
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  RNGkind(normal.kind = "Kinderman-Ramage") # faster normal draws
  set.seed(2345) # seed (for reproducibility)
  link <- links[thislink]
  tictoc <- system.time(
    postbcv3_fit <- debias_post_bcv(xs, ys, link = link, parallel = TRUE)
  )
  fits_postbcv3_l_mod[[thislink]] <- postbcv3_fit
  tictocs_mod[thislink] <- tictoc[3]
  stopCluster(cl)
}

bhats_postbcv3_l_mod <- array(NA, dim = numlinks)
sebhats_postbcv3_l_mod <- array(NA, dim = numlinks)
tstats_postbcv3_l_mod <- array(NA, dim = numlinks)
n <- nrow(x)
for (thislink in 1:numlinks) {
  bhat <- fits_postbcv3_l_mod[[thislink]]$bhat # coef on "r_sblack"
  vhat <- fits_postbcv3_l_mod[[thislink]]$vhat # variance estimate
  sebhat <- sqrt(vhat / n) # se(coef)
  bhats_postbcv3_l_mod[thislink] <- bhat
  sebhats_postbcv3_l_mod[thislink] <- sebhat
  tstats_postbcv3_l_mod[thislink] <- bhat / sebhat # t-stat
}
bhats_postbcv3_l_mod
sebhats_postbcv3_l_mod
tstats_postbcv3_l_mod
# ^-- t-values in Table 1 for "Basic Controls + Interactions" and "Post-BCV"

# t-statistics gathered
tstats_matrix <- array(NA, dim = c(2, 2, 2))
dimnames(tstats_matrix) <- list(controls = c("basic", "inter"), link = links,
                                method = c("MLE", "Post-BCV"))
tstats_matrix[1, , 1] <- tstats_unpen_l_dum
tstats_matrix[1, , 2] <- tstats_postbcv3_l_dum
tstats_matrix[2, , 2] <- tstats_postbcv3_l_mod
round(tstats_matrix, digits = 1) # t-values in Table 1

# timings gathered
timings_matrix <- array(NA, dim = c(2, 2, 2))
dimnames(timings_matrix) <- list(controls = c("basic", "inter"), link = links,
                                 method = c("MLE", "Post-BCV"))
timings_matrix[1, , 1] <- tictocs_unpen
timings_matrix[1, , 2] <- tictocs_dum
timings_matrix[2, , 2] <- tictocs_mod
round(timings_matrix, digits = 1) # timings in Table 3

# Calculating the average partial effect (APE) of r_sblack
# Based on unpenalized MLE
ape_unpen <- function(fit = fit) {
  wnames <- setdiff(colnames(fit$data),
                    c(racedum, "anyuseofforce_coded")) # non-racial controls
  bhat <- unname(coef(fit)["r_sblack"])
  intr <- unname(coef(fit)["(Intercept)"])
  gtil <- matrix(unname(coef(fit)[wnames]), ncol = 1)
  w <- as.matrix(fit$data[, wnames])
  wgtil <- intr + w %*% gtil # contributions from controls (incl. intercept)
  cd_func <- fit$family$linkinv # CDF
  ape <- mean(cd_func(bhat + wgtil) - cd_func(wgtil))
  return(ape)
}
# (i) Basic controls
apes_unpen_l_dum <- array(NA, dim = numlinks)
for (thislink in 1:numlinks) {
  apes_unpen_l_dum[thislink] <- ape_unpen(fit = fits_unpen_l_dum[[thislink]])
}
# (ii) Interactions included
# NA

# Based on penalized MLE
ape_pen <- function(data = data, fit = fit, link = link) {
  bhat <- fit$bhat
  intr <- fit$postbcv1$intr
  gtil <- matrix(fit$postbcv1$that[-c(1, 2, 3), 1], ncol = 1)
  wnames <- setdiff(colnames(data),
                    c(racedum, "anyuseofforce_coded")) # non-racial controls
  w <- as.matrix(data[, wnames])
  wgtil <- intr + w %*% gtil
  cd_func <- binomial(link = link)$linkinv # CDF
  ape <- mean(cd_func(bhat + wgtil) - cd_func(wgtil))
  return(ape)
}

# (i) Basic controls, penalized
apes_pen_l_dum <- array(NA, dim = numlinks)
for (thislink in 1:numlinks) {
  apes_pen_l_dum[thislink] <- ape_pen(data = ppcs_l_dum,
                                      fit = fits_postbcv3_l_dum[[thislink]],
                                      link = links[thislink])
}

# (ii) Interactions included, penalized
apes_pen_l_mod <- array(NA, dim = numlinks)
for (thislink in 1:numlinks) {
  apes_pen_l_mod[thislink] <- ape_pen(data = ppcs_l_mod,
                                      fit = fits_postbcv3_l_mod[[thislink]],
                                      link = links[thislink])
}

# APEs gathered
apes_matrix <- array(NA, dim = c(2, 2, 2))
dimnames(apes_matrix) <- list(controls = c("basic", "inter"),
                              link = links, method = c("MLE", "Post-BCV"))
apes_matrix[1, , 1] <- apes_unpen_l_dum
apes_matrix[1, , 2] <- apes_pen_l_dum
apes_matrix[2, , 2] <- apes_pen_l_mod
round(100 * apes_matrix, digits = 1) # APEs in Table 2

do_save <- TRUE
if (do_save == TRUE) {
  save.image(file = "application/application_results_JPE_rev2.Rdata")
}