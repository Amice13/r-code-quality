#----------------------------------------------------------------------------------------------------#
# Code for Measurement Equivalence Analyses
# Appendix Tables A8-A14
# Engelhardt, Feldman, and Hetherington "Improving the Measurement of Authoritarianism"
#----------------------------------------------------------------------------------------------------#
library(lavaan)
library(semTools)

# Set working directory
# setwd("")

# Loading Data ---------------------------------------------
qual_dat <- read.csv("qualtrics_study.csv")

#  Black-Latino-White -------------------------------------------------------
pf_bwl <- subset(qual_dat, white == 1 | black == 1 | latino == 1)

# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

auth_vars <- c("obedience", "manners", "behaved", "orderly", "polite", "respect", "loyal", "discip")

GROUP <- "race"

mod_auth <- '
auth =~ NA*manners + polite + respect + discip + obedience + behaved + orderly + loyal 
auth ~~ 1*auth
'
fit_auth <- cfa(mod_auth, pf_bwl, ordered = auth_vars,
                group = GROUP)
summary(fit_auth, standardized = T)
fitMeasures(fit_auth)[FIT]

fit_config <- cfa(mod_auth, pf_bwl, mimic = "Mplus", group = GROUP, ordered = auth_vars)
summary(fit_config, standardized = T)
round(fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
config_fit <- fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_metric <- cfa(mod_auth, pf_bwl, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings"),
                  ordered = auth_vars)
summary(fit_metric, standardized = T)
round(fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
metric_fit <- fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_scalar <- cfa(mod_auth, pf_bwl, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings", "thresholds"),
                  ordered = auth_vars)
summary(fit_scalar, standardized = T)
round(fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
scalar_fit <- fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit

#### Permuatations
fit_measures <- c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")
nperms <- 1000

## test configural invariance
set.seed(1693)
out_config <- permuteMeasEq(nPermute = nperms, 
                            con = fit_config, AFIs = fit_measures)
out_config

## test metric equivalence
set.seed(1693) # same permutations
out_metric <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_config, con = fit_metric,
                            param = "loadings", AFIs = fit_measures)
summary(out_metric)


## test scalar equivalence
set.seed(1693) # same permutations
out_scalar <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_metric, con = fit_scalar,
                            param = "thresholds", AFIs = fit_measures)
summary(out_scalar)


## test partial scalar equivalence
fit_scalar_part <- cfa(mod_auth, pf_bwl, mimic = "Mplus", 
                       group.partial = c("behaved | t1"), group = GROUP,
                       group.equal = c("loadings", "intercepts"),
                       ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar_part <- permuteMeasEq(nPermute = nperms, 
                                 uncon = fit_metric, con = fit_scalar_part,
                                 param = "thresholds", AFIs = fit_measures)
summary(out_scalar_part)


comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit_part <- rbind(comb_fit,
                       fitMeasures(fit_scalar_part)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")])
rownames(comb_fit_part)[4] <- c("scalar_fit_part")


permute_fit <- array(NA, c(4, 12))
permute_fit <- as.data.frame(permute_fit)
rownames(permute_fit) <- c("Configural", "Metric", "Scalar", "Scalar--Partial")
colnames(permute_fit) <- c("$\\chi^2$", "CFI", "SRMR", "RMSEA", 
                           "$\\Delta\\chi^2$", "$\\chi^2$ p-value", 
                           "$\\Delta$CFI", "CFI p-value", 
                           "$\\Delta$SRMR", "SRMR p-value", 
                           "$\\Delta$RMSEA", "RMSEA p-value")
permute_fit["Configural", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["config_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]

permute_fit["Metric", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric@AFI.obs
permute_fit["Metric", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric@AFI.pval

permute_fit["Scalar", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar@AFI.obs
permute_fit["Scalar", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar@AFI.pval

permute_fit["Scalar--Partial", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit_part", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar--Partial", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar_part@AFI.obs
permute_fit["Scalar--Partial", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar_part@AFI.pval

permute_fit
write.csv(permute_fit, "~/desktop/tabA8.csv")

invar_tab(config = fit_config, 
          metric = fit_metric, 
          scalar = fit_scalar, 
          scalar_partial_1 = fit_scalar_part,
          ordered = T,
          file_name = "~/desktop/tabA9.csv")

#  Male-Female -------------------------------------------------------
pf <- qual_dat

# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

auth_vars <- c("obedience", "manners", "behaved", "orderly", "polite", "respect", "loyal", "discip")

GROUP <- "fem"

mod_auth <- '
auth =~ NA*manners + polite + respect + discip + obedience + behaved + orderly + loyal 
auth ~~ 1*auth
'
fit_auth <- cfa(mod_auth, pf, ordered = auth_vars,
                group = GROUP)
summary(fit_auth, standardized = T)
fitMeasures(fit_auth)[FIT]

fit_config <- cfa(mod_auth, pf, mimic = "Mplus", group = GROUP, ordered = auth_vars)
summary(fit_config, standardized = T)
round(fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
config_fit <- fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_metric <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings"),
                  ordered = auth_vars)
summary(fit_metric, standardized = T)
round(fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
metric_fit <- fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_scalar <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings", "thresholds"),
                  ordered = auth_vars)
summary(fit_scalar, standardized = T)
round(fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
scalar_fit <- fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit

#### Permuatations
fit_measures <- c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")
nperms <- 1000

## test configural invariance
set.seed(1693)
out_config <- permuteMeasEq(nPermute = nperms, 
                            con = fit_config, AFIs = fit_measures)
out_config

## test metric equivalence
set.seed(1693) # same permutations
out_metric <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_config, con = fit_metric,
                            param = "loadings", AFIs = fit_measures)
summary(out_metric)
# lavTestScore(fit_metric, epc = T)


## test scalar equivalence
set.seed(1693) # same permutations
out_scalar <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_metric, con = fit_scalar,
                            param = "thresholds", AFIs = fit_measures)
summary(out_scalar)


## test partial scalar equivalence
fit_scalar_part <- cfa(mod_auth, pf, mimic = "Mplus", 
                       group.partial = c("orderly | t1"), group = GROUP,
                       group.equal = c("loadings", "intercepts"),
                       ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar_part <- permuteMeasEq(nPermute = nperms, 
                                 uncon = fit_metric, con = fit_scalar_part,
                                 param = "thresholds", AFIs = fit_measures)
summary(out_scalar_part)


## test partial scalar equivalence
fit_scalar_part2 <- cfa(mod_auth, pf, mimic = "Mplus", 
                        group.partial = c("orderly | t1",
                                          "behaved | t1"), group = GROUP,
                        group.equal = c("loadings", "intercepts"),
                        ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar_part2 <- permuteMeasEq(nPermute = nperms, 
                                  uncon = fit_metric, con = fit_scalar_part2,
                                  param = "thresholds", AFIs = fit_measures)
summary(out_scalar_part2)


comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit_part <- rbind(comb_fit,
                       fitMeasures(fit_scalar_part)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")],
                       fitMeasures(fit_scalar_part2)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")])
rownames(comb_fit_part)[4:5] <- c("scalar_fit_part", "scalar_fit_part2")


permute_fit <- array(NA, c(5, 12))
permute_fit <- as.data.frame(permute_fit)
rownames(permute_fit) <- c("Configural", "Metric", "Scalar", "Scalar--Partial", "Scalar--Partial2")
colnames(permute_fit) <- c("$\\chi^2$", "CFI", "SRMR", "RMSEA", 
                           "$\\Delta\\chi^2$", "$\\chi^2$ p-value", 
                           "$\\Delta$CFI", "CFI p-value", 
                           "$\\Delta$SRMR", "SRMR p-value", 
                           "$\\Delta$RMSEA", "RMSEA p-value")
permute_fit["Configural", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["config_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]

permute_fit["Metric", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric@AFI.obs
permute_fit["Metric", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric@AFI.pval

permute_fit["Scalar", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar@AFI.obs
permute_fit["Scalar", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar@AFI.pval

permute_fit["Scalar--Partial", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit_part", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar--Partial", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar_part@AFI.obs
permute_fit["Scalar--Partial", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar_part@AFI.pval

permute_fit["Scalar--Partial2", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit_part2", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar--Partial2", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar_part2@AFI.obs
permute_fit["Scalar--Partial2", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar_part2@AFI.pval

permute_fit

write.csv(permute_fit, "~/desktop/tabA10.csv")

#  College Degree -------------------------------------------------------
pf <- qual_dat

# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

auth_vars <- c("obedience", "manners", "behaved", "orderly", "polite", "respect", "loyal", "discip")

GROUP <- "college"

mod_auth <- '
auth =~ NA*manners + polite + respect + discip + obedience + behaved + orderly + loyal 
auth ~~ 1*auth
'
fit_auth <- cfa(mod_auth, pf, ordered = auth_vars,
                group = GROUP)
summary(fit_auth, standardized = T)
fitMeasures(fit_auth)[FIT]

fit_config <- cfa(mod_auth, pf, mimic = "Mplus", group = GROUP,ordered = auth_vars)
summary(fit_config, standardized = T)
round(fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
config_fit <- fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_metric <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings"),
                  ordered = auth_vars)
summary(fit_metric, standardized = T)
round(fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
metric_fit <- fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_scalar <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings", "thresholds"),
                  ordered = auth_vars)
summary(fit_scalar, standardized = T)
round(fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
scalar_fit <- fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit

#### Permuatations
fit_measures <- c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")
nperms <- 1000

## test configural invariance
set.seed(1693)
out_config <- permuteMeasEq(nPermute = nperms, 
                            con = fit_config, AFIs = fit_measures)
out_config

## test metric equivalence
set.seed(1693) # same permutations
out_metric <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_config, con = fit_metric,
                            param = "loadings", AFIs = fit_measures)
summary(out_metric)

## test partial metric equivalence
fit_metric_part <- cfa(mod_auth, pf, mimic = "Mplus", 
                       group.partial = c("auth =~ orderly"), group = GROUP,
                       group.equal = c("loadings"),
                       ordered = auth_vars)
set.seed(1693) # same permutations
out_metric_part <- permuteMeasEq(nPermute = nperms, 
                                 uncon = fit_config, con = fit_metric_part,
                                 param = "loadings", AFIs = fit_measures)
summary(out_metric_part)


## test partial metric equivalence
fit_metric_part2 <- cfa(mod_auth, pf, mimic = "Mplus", 
                        group.partial = c("auth =~ orderly",
                                          "auth =~ behaved"), group = GROUP,
                        group.equal = c("loadings"),
                        ordered = auth_vars)
set.seed(1693) # same permutations
out_metric_part2 <- permuteMeasEq(nPermute = nperms, 
                                  uncon = fit_config, con = fit_metric_part2,
                                  param = "loadings", AFIs = fit_measures)
summary(out_metric_part2)

## test scalar equivalence
fit_scalar <- cfa(mod_auth, pf, mimic = "Mplus", 
                  group.partial = c("auth =~ orderly",
                                    "auth =~ behaved"), group = GROUP,
                  group.equal = c("loadings", "intercepts"),
                  ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_metric_part2, con = fit_scalar,
                            param = "thresholds", AFIs = fit_measures)
summary(out_scalar)


## test partial scalar equivalence
fit_scalar_part <- cfa(mod_auth, pf, mimic = "Mplus", 
                       group.partial = c("auth =~ orderly",
                                         "auth =~ behaved",
                                         "manners | t1"), group = GROUP,
                       group.equal = c("loadings", "intercepts"),
                       ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar_part <- permuteMeasEq(nPermute = nperms, 
                                 uncon = fit_metric_part2, con = fit_scalar_part,
                                 param = "thresholds", AFIs = fit_measures)
summary(out_scalar_part)


comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit_part <- rbind(comb_fit,
                       fitMeasures(fit_metric_part)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")],
                       fitMeasures(fit_metric_part2)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")],
                       fitMeasures(fit_scalar_part)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")])
rownames(comb_fit_part)[4:6] <- c("metric_fit_part", "metric_fit_part2", "scalar_fit_part")


permute_fit <- array(NA, c(6, 12))
permute_fit <- as.data.frame(permute_fit)
rownames(permute_fit) <- c("Configural", "Metric", "Metric--Partial", "Metric--Partial2", "Scalar", "Scalar--Partial")
colnames(permute_fit) <- c("$\\chi^2$", "CFI", "SRMR", "RMSEA", 
                           "$\\Delta\\chi^2$", "$\\chi^2$ p-value", 
                           "$\\Delta$CFI", "CFI p-value", 
                           "$\\Delta$SRMR", "SRMR p-value", 
                           "$\\Delta$RMSEA", "RMSEA p-value")
permute_fit["Configural", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["config_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]

permute_fit["Metric", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric@AFI.obs
permute_fit["Metric", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric@AFI.pval

permute_fit["Metric--Partial", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit_part", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric--Partial", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric_part@AFI.obs
permute_fit["Metric--Partial", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric_part@AFI.pval

permute_fit["Metric--Partial2", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit_part2", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric--Partial2", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric_part2@AFI.obs
permute_fit["Metric--Partial2", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric_part2@AFI.pval


permute_fit["Scalar", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar@AFI.obs
permute_fit["Scalar", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar@AFI.pval

permute_fit["Scalar--Partial", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit_part", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar--Partial", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar_part@AFI.obs
permute_fit["Scalar--Partial", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar_part@AFI.pval

permute_fit

write.csv(permute_fit, "~/desktop/tabA11.csv")

#  None/Atheist -------------------------------------------------------
pf <- qual_dat

# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

auth_vars <- c("obedience", "manners", "behaved", "orderly", "polite", "respect", "loyal", "discip")

GROUP <- "relig_AthNoneDK"

mod_auth <- '
auth =~ NA*manners + polite + respect + discip + obedience + behaved + orderly + loyal 
auth ~~ 1*auth
'
fit_auth <- cfa(mod_auth, pf, ordered = auth_vars,
                group = GROUP)
summary(fit_auth, standardized = T)
fitMeasures(fit_auth)[FIT]

fit_config <- cfa(mod_auth, pf, mimic = "Mplus", group = GROUP,ordered = auth_vars)
summary(fit_config, standardized = T)
round(fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
config_fit <- fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_metric <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings"),
                  ordered = auth_vars)
summary(fit_metric, standardized = T)
round(fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
metric_fit <- fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_scalar <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings", "thresholds"),
                  ordered = auth_vars)
summary(fit_scalar, standardized = T)
round(fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
scalar_fit <- fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit

#### Permuatations
fit_measures <- c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")
nperms <- 1000

## test configural invariance
set.seed(1693)
out_config <- permuteMeasEq(nPermute = nperms, 
                            con = fit_config, AFIs = fit_measures)
out_config

## test metric equivalence
set.seed(1693) # same permutations
out_metric <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_config, con = fit_metric,
                            param = "loadings", AFIs = fit_measures)
summary(out_metric)

## test scalar equivalence
set.seed(1693) # same permutations
out_scalar <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_metric, con = fit_scalar,
                            param = "thresholds", AFIs = fit_measures)
summary(out_scalar)


## test partial scalar equivalence
fit_scalar_part <- cfa(mod_auth, pf, mimic = "Mplus", 
                       group.partial = c("respect | t1"), group = GROUP,
                       group.equal = c("loadings", "intercepts"),
                       ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar_part <- permuteMeasEq(nPermute = nperms, 
                                 uncon = fit_metric, con = fit_scalar_part,
                                 param = "thresholds", AFIs = fit_measures)
summary(out_scalar_part)


comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit_part <- rbind(comb_fit,
                       fitMeasures(fit_scalar_part)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")])
rownames(comb_fit_part)[4] <- c("scalar_fit_part")


permute_fit <- array(NA, c(4, 12))
permute_fit <- as.data.frame(permute_fit)
rownames(permute_fit) <- c("Configural", "Metric", "Scalar", "Scalar--Partial")
colnames(permute_fit) <- c("$\\chi^2$", "CFI", "SRMR", "RMSEA", 
                           "$\\Delta\\chi^2$", "$\\chi^2$ p-value", 
                           "$\\Delta$CFI", "CFI p-value", 
                           "$\\Delta$SRMR", "SRMR p-value", 
                           "$\\Delta$RMSEA", "RMSEA p-value")
permute_fit["Configural", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["config_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]

permute_fit["Metric", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric@AFI.obs
permute_fit["Metric", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric@AFI.pval

permute_fit["Scalar", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar@AFI.obs
permute_fit["Scalar", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar@AFI.pval

permute_fit["Scalar--Partial", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit_part", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar--Partial", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar_part@AFI.obs
permute_fit["Scalar--Partial", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar_part@AFI.pval

permute_fit

write.csv(permute_fit, "~/desktop/tabA12.csv")

#  South -------------------------------------------------------
pf <- qual_dat

# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

auth_vars <- c("obedience", "manners", "behaved", "orderly", "polite", "respect", "loyal", "discip")

GROUP <- "south"

mod_auth <- '
auth =~ NA*manners + polite + respect + discip + obedience + behaved + orderly + loyal 
auth ~~ 1*auth
'
fit_auth <- cfa(mod_auth, pf, ordered = auth_vars,
                group = GROUP)
summary(fit_auth, standardized = T)
fitMeasures(fit_auth)[FIT]

fit_config <- cfa(mod_auth, pf, mimic = "Mplus", group = GROUP,ordered = auth_vars)
summary(fit_config, standardized = T)
round(fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
config_fit <- fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_metric <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings"),
                  ordered = auth_vars)
summary(fit_metric, standardized = T)
round(fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
metric_fit <- fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_scalar <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings", "thresholds"),
                  ordered = auth_vars)
summary(fit_scalar, standardized = T)
round(fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
scalar_fit <- fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit

#### Permuatations
fit_measures <- c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")
nperms <- 1000

## test configural invariance
set.seed(1693)
out_config <- permuteMeasEq(nPermute = nperms, 
                            con = fit_config, AFIs = fit_measures)
out_config

## test metric equivalence
set.seed(1693) # same permutations
out_metric <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_config, con = fit_metric,
                            param = "loadings", AFIs = fit_measures)
summary(out_metric)

## test scalar equivalence
set.seed(1693) # same permutations
out_scalar <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_metric, con = fit_scalar,
                            param = "thresholds", AFIs = fit_measures)
summary(out_scalar)


comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit_part <- rbind(comb_fit)

permute_fit <- array(NA, c(3, 12))
permute_fit <- as.data.frame(permute_fit)
rownames(permute_fit) <- c("Configural", "Metric", "Scalar")
colnames(permute_fit) <- c("$\\chi^2$", "CFI", "SRMR", "RMSEA", 
                           "$\\Delta\\chi^2$", "$\\chi^2$ p-value", 
                           "$\\Delta$CFI", "CFI p-value", 
                           "$\\Delta$SRMR", "SRMR p-value", 
                           "$\\Delta$RMSEA", "RMSEA p-value")
permute_fit["Configural", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["config_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]

permute_fit["Metric", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric@AFI.obs
permute_fit["Metric", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric@AFI.pval

permute_fit["Scalar", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar@AFI.obs
permute_fit["Scalar", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar@AFI.pval

permute_fit

write.csv(permute_fit, "~/desktop/tabA13.csv")

#  Income -------------------------------------------------------
pf <- qual_dat

# Desired Fit Measures
FIT <- c("chisq.scaled","df","cfi.scaled", "tli.scaled", "srmr", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")

auth_vars <- c("obedience", "manners", "behaved", "orderly", "polite", "respect", "loyal", "discip")

GROUP <- "inc_40plus"

mod_auth <- '
auth =~ NA*manners + polite + respect + discip + obedience + behaved + orderly + loyal 
auth ~~ 1*auth
'
fit_auth <- cfa(mod_auth, pf, ordered = auth_vars,
                group = GROUP)
summary(fit_auth, standardized = T)
fitMeasures(fit_auth)[FIT]

fit_config <- cfa(mod_auth, pf, mimic = "Mplus", group = GROUP,ordered = auth_vars)
summary(fit_config, standardized = T)
round(fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
config_fit <- fitMeasures(fit_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_metric <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings"),
                  ordered = auth_vars)
summary(fit_metric, standardized = T)
round(fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
metric_fit <- fitMeasures(fit_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

fit_scalar <- cfa(mod_auth, pf, mimic = "Mplus",
                  group = GROUP, group.equal = c("loadings", "thresholds"),
                  ordered = auth_vars)
summary(fit_scalar, standardized = T)
round(fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")], 3)
scalar_fit <- fitMeasures(fit_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")]

comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit

#### Permuatations
fit_measures <- c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")
nperms <- 1000

## test configural invariance
set.seed(1693)
out_config <- permuteMeasEq(nPermute = nperms, 
                            con = fit_config, AFIs = fit_measures)
out_config

## test metric equivalence
set.seed(1693) # same permutations
out_metric <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_config, con = fit_metric,
                            param = "loadings", AFIs = fit_measures)
summary(out_metric)

## test scalar equivalence
set.seed(1693) # same permutations
out_scalar <- permuteMeasEq(nPermute = nperms, 
                            uncon = fit_metric, con = fit_scalar,
                            param = "thresholds", AFIs = fit_measures)
summary(out_scalar)


## test partial scalar equivalence
fit_scalar_part <- cfa(mod_auth, pf, mimic = "Mplus", 
                       group.partial = c("polite | t1"), group = GROUP,
                       group.equal = c("loadings", "intercepts"),
                       ordered = auth_vars)
set.seed(1693) # same permutations
out_scalar_part <- permuteMeasEq(nPermute = nperms, 
                                 uncon = fit_metric, con = fit_scalar_part,
                                 param = "thresholds", AFIs = fit_measures)
summary(out_scalar_part)


comb_fit <- rbind(config_fit, metric_fit, scalar_fit)
comb_fit_part <- rbind(comb_fit,
                       fitMeasures(fit_scalar_part)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled")])
rownames(comb_fit_part)[4] <- c("scalar_fit_part")


permute_fit <- array(NA, c(4, 12))
permute_fit <- as.data.frame(permute_fit)
rownames(permute_fit) <- c("Configural", "Metric", "Scalar", "Scalar--Partial")
colnames(permute_fit) <- c("$\\chi^2$", "CFI", "SRMR", "RMSEA", 
                           "$\\Delta\\chi^2$", "$\\chi^2$ p-value", 
                           "$\\Delta$CFI", "CFI p-value", 
                           "$\\Delta$SRMR", "SRMR p-value", 
                           "$\\Delta$RMSEA", "RMSEA p-value")
permute_fit["Configural", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["config_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]

permute_fit["Metric", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["metric_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Metric", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_metric@AFI.obs
permute_fit["Metric", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_metric@AFI.pval

permute_fit["Scalar", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar@AFI.obs
permute_fit["Scalar", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar@AFI.pval

permute_fit["Scalar--Partial", 
            c("$\\chi^2$", "CFI", "SRMR", "RMSEA")] <- comb_fit_part["scalar_fit_part", c("chisq.scaled", "cfi.scaled", "srmr", "rmsea.scaled")]
permute_fit["Scalar--Partial", 
            c("$\\Delta\\chi^2$", "$\\Delta$CFI", "$\\Delta$SRMR", "$\\Delta$RMSEA")] <- out_scalar_part@AFI.obs
permute_fit["Scalar--Partial", 
            c("$\\chi^2$ p-value", "CFI p-value", "SRMR p-value", "RMSEA p-value")] <- out_scalar_part@AFI.pval


permute_fit

write.csv(permute_fit, "~/desktop/tabA14.csv")