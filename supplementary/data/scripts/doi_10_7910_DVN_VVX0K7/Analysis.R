########################################
# Analysis Replication Code for
# "The Effect of Citizenship on the Long-Term Earnings of Marginalized Immigrants: 
# Quasi-Experimental Evidence from Switzerland"
# Science Advances (DOI: 10.1126/sciadv.aay1610)
# Jens Hainmueller, Dominik Hangartner, and Dalston Ward
# December 2019
########################################

# IMPORTANT: For exact replication (especially of quantile regressions), use R 3.5.2 
# (original estimation done on MacOS 10.14.6)

library(data.table) # version 1.12.6
library(lfe) # version 2.8-3
library(texreg)  # version 1.36.23
library(rdd) # version 0.57
library(quantreg) # version 5.51

# load the RDD and did datasets
did <- fread("<<DID data>>")
did2 <- fread("<<DID2 data") # Same as did but without dropping observations above retirment age
RDD <- fread("<<RDD data>>")

# Note: Below the functions, we present code for the main DID and RDD results, the results split by origins, and the main quantile regression results. We present all subsequent code in the order the results appear in the Supplementary Materials. Code for Tables S31 and S32 can be found in separate files.

########################################
# Functions 
########################################

# Adjusts the DOF correctly for year + person FE
felm_correct <- function(f, data, chatty = T){
  temp <- felm(f, data = data)
  out <- felm(f, data = data, exactDOF = temp$df.residual + nlevels(temp$clustervar$personID))
  
  if(chatty ){
    print(summary(out))
  }
  return(out)
}

# saves time on typing the formula, also prints results!
rd_fit <- function(DV, data, cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(ref_age) + factor(cabjahr)", chatty = T){
  out <- lm(as.formula(paste0(DV, cov.string)), data = data)
  if(chatty){
    print(summary(out))
  }
  return(out)
}

# Function that fits quantile regressions while bootstrapping (DD design)
RQ_Boot_fit <- function(data, tau){
  temp <- data[ .(sample(unique(personID), replace = T))]
  return(rq(sal2k ~ post_naturalized + factor(personID) +  factor(dacot), data = temp, tau = tau, method = "sfn", model = F)$coefficients[2])
}

# Function that fits a quantile regression (DD design)
RQ_BS <- function(data, n_boots, tau = .5, level = .95){
  setDT(data, key = "personID")

  b.sample <- replicate(n_boots, RQ_Boot_fit(data, tau = tau))

   CI <- quantile(b.sample, probs = c((1 - level)/2, (1 - level)/2 + level))
  main <- rq(sal2k ~ post_naturalized + factor(personID) + factor(dacot) , tau = tau, data = data, method = "sfn")
  main$CI <- CI
  main$se <- sd(b.sample)

  print(c(main$coefficients[2], CI, "SE" = sd(b.sample)))
  return(main)
}

# Function that fits quantile regressions while bootstrapping (DD design; placebo)
RQ_Boot_fit_placebo <- function(data, tau){
  temp <- data[ .(sample(unique(personID), replace = T))]
  return(rq(sal2k ~ post_naturalized_placebo + factor(personID) + factor(dacot), data = temp, tau = tau, method = "sfn", model = F)$coefficients[2])
}

# Function that fits a quantile regression (DD design; placebo)
RQ_BS_placebo <- function(data, n_boots, tau = .5, level = .95){
  setDT(data, key = "personID")
  
  b.sample <- replicate(n_boots, RQ_Boot_fit_placebo(data, tau = tau))
  
  CI <- quantile(b.sample, probs = c((1 - level)/2, (1 - level)/2 + level))
  main <- rq(sal2k ~ post_naturalized_placebo + factor(personID) +  factor(dacot), tau = tau, data = data, method = "sfn")
  main$CI <- CI
  main$se <- sd(b.sample)
  
  print(c(main$coefficients[2], CI, "SE" = sd(b.sample)))
  return(main)
}

# for printing these results to put into the table
quantile_print <- function(RQ_out, digits = 2){
  print(round(c(RQ_out$coefficients[2], RQ_out$se), 2))
  print(2*(1 - pnorm(abs(RQ_out$coefficients[2]/RQ_out$se))))
}
########################################

########################################
# Main D-i-D Models (Table S5)
########################################

# Main results
did_formula <- as.formula("sal2k ~ post_naturalized | personID + dacot | 0 | personID")
did_formula_placebo <- as.formula("sal2k ~ post_naturalized_placebo | personID  + dacot | 0 | personID")
did_bw <- 40:60

did_placebo <- felm_correct(f = did_formula_placebo, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw])
did_0to5 <- felm_correct(f = did_formula, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw])
did_6to10 <- felm_correct(f = did_formula, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw])
did_11to15 <- felm_correct(f = did_formula, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw])
did_0to10 <- felm_correct(f = did_formula, data = did[prct_ja %in% did_bw & ref_time %in% -5:10])
did_0to15 <- felm_correct(f = did_formula, data = did[ prct_ja %in% did_bw])

# create table of D-i-D results
texreg(
  list(
    did_placebo,
    did_0to5,
    did_6to10,
    did_11to15,
    did_0to10,
    did_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption.above = T,
  caption = "Effect of Referendum Success on Earnings, Difference-in-Dfferences Design",
  label = "tab:mainDD",
  include.rsquared = F,
  dcolumn = T,
  booktabs = T,
  fontsize = "footnotesize"
)
########################################

########################################
# Main RD Models (Table S6)
########################################

# main results 
rd_bw <- 40:60

rd_placebo <- rd_fit("sal2k_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw ])
rd_0to5 <- rd_fit("sal2k_avg_post_0to5", data = RDD[prct_ja %in% rd_bw])
rd_6to10 <- rd_fit("sal2k_avg_post_6to10", data = RDD[prct_ja %in% rd_bw])
rd_11to15 <- rd_fit("sal2k_avg_post_11to15", data = RDD[prct_ja %in% rd_bw])
rd_0to10 <- rd_fit("sal2k_avg_post_0to10", data = RDD[prct_ja %in% rd_bw])
rd_0to15 <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw])

texreg(
  list(
    rd_placebo,
    rd_0to5,
    rd_6to10,
    rd_11to15,
    rd_0to10,
    rd_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Effect of Referendum Success on Earnings, Regression Discontinuity Design",
  caption.above = T,
  label = "tab:mainRD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize"
)
########################################

########################################
# Origin Subgroup Analyses (Table S21)
########################################

did_0to15_SEE <- felm_correct(f = did_formula, data = did[ref_time %in% -5:15 & prct_ja %in% did_bw & SEE == T])
did_0to15_Other <- felm_correct(f = did_formula, data = did[ref_time %in% -5:15 & prct_ja %in% did_bw & SEE == F])

rd_0to15_SEE <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & SEE == T], cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)")
rd_0to15_Other <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & SEE == F], cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)")

texreg(
  list(
    did_0to15_SEE,
    did_0to15_Other,
    rd_0to15_SEE,
    rd_0to15_Other
    ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%",
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
    ),
  custom.model.names = paste0("(", 1:4,")"),
  caption = "Effects of Referendum Success for Applicants from Marginalized Origin Countries",
  caption.above = T,
  label = "tab:originEffects",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F
  )

########################################

########################################
# Quantile Regressions (Table S23)
########################################
set.seed(8092)
qr_25_4060_placebo <- RQ_BS_placebo(data = did[ref_time %in% -5:-1 & prct_ja %in% 40:60], n_boots = 500, tau = .25, level = .95)
qr_50_4060_placebo <- RQ_BS_placebo(data = did[ref_time %in% -5:-1 & prct_ja %in% 40:60], n_boots = 500, tau = .5, level = .95)
qr_75_4060_placebo <- RQ_BS_placebo(data = did[ref_time %in% -5:-1 & prct_ja %in% 40:60], n_boots = 500, tau = .75, level = .95)

qr_25_4060_015 <- RQ_BS(data = did[ref_time %in% -5:15 & prct_ja %in% 40:60], n_boots = 500, tau = .25, level = .95)
qr_50_4060_015 <- RQ_BS(data = did[ref_time %in% -5:15 & prct_ja %in% 40:60], n_boots = 500, tau = .5, level = .95)
qr_75_4060_015 <- RQ_BS(data = did[ref_time %in% -5:15 & prct_ja %in% 40:60], n_boots = 500, tau = .75, level = .95)

qr_25_4060_05 <- RQ_BS(data = did[ref_time %in% -5:5 & prct_ja %in% 40:60], n_boots = 500, tau = .25, level = .95)
qr_50_4060_05 <- RQ_BS(data = did[ref_time %in% -5:5 & prct_ja %in% 40:60], n_boots = 500, tau = .5, level = .95)
qr_75_4060_05 <- RQ_BS(data = did[ref_time %in% -5:5 & prct_ja %in% 40:60], n_boots = 500, tau = .75, level = .95)

qr_25_4060_610 <- RQ_BS(data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% 40:60], n_boots = 500, tau = .25, level = .95)
qr_50_4060_610 <- RQ_BS(data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% 40:60], n_boots = 500, tau = .5, level = .95)
qr_75_4060_610 <- RQ_BS(data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% 40:60], n_boots = 500, tau = .75, level = .95)

qr_25_4060_1115 <- RQ_BS(data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% 40:60], n_boots = 500, tau = .25, level = .95)
qr_50_4060_1115 <- RQ_BS(data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% 40:60], n_boots = 500, tau = .5, level = .95)
qr_75_4060_1115 <- RQ_BS(data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% 40:60], n_boots = 500, tau = .75, level = .95)

qr_25_4060_010 <- RQ_BS(data = did[ref_time %in% -5:10 & prct_ja %in% 40:60], n_boots = 500, tau = .25, level = .95)
qr_50_4060_010 <- RQ_BS(data = did[ref_time %in% -5:10 & prct_ja %in% 40:60], n_boots = 500, tau = .5, level = .95)
qr_75_4060_010 <- RQ_BS(data = did[ref_time %in% -5:10 & prct_ja %in% 40:60], n_boots = 500, tau = .75, level = .95)
########################################

########################################
# Sorting Test (in "Statistical Analyses" section of "Materials and Methods")
########################################
binom.test(33, #failures
           33+36, #trials
           p = 0.5,
           alternative = "two.sided",
           conf.level = 0.95)
########################################

########################################
# Table S1: Information on match rates in close referendums 
########################################

# This requires reading in the un-processed versions of the OASI and referendum data
# tax <- fread("OASI_data.csv", colClasses = c("nnaaff" = "character", "s_noaff11" = "character"))
# ref <- fread("Archival_data.csv")

# Pre-processessing (calculates the match rate by referendum vote share and outcome)
tax_ind <- tax[!origin_grouped == "land_ch" , .(prct_ja = unique(prct_ja), naturalized = unique(naturalized), origin_grouped = unique(origin_grouped), csex = unique(csex), cabjahr = unique(cabjahr), ref_age = unique(cabjahr - cgebj)), by = personID]

# Calculate overall match rate
tax_ind[,.N]/ref[!origin_grouped == "land_ch",.N]

# Get sample size information
ref[!origin_grouped == 'land_ch', .N] # Archival N
tax_ind[, .N] # Matches in OASI data
zasi[origin_grouped != "land_ch" &  age %in% 18:65 &  obs_pre5 == T & unreal_ages == F, .N] # overall annual applicant observations (for people meeting criteria to be included in DD sample)

### Calculations for Table S.1 (Match Rate for Archival and OASI data across covariates)

# Origin
tax_tab_origin <- tax_ind[ , table(origin_grouped)]
ref_tab_origin <- ref[ !origin_grouped == "land_ch" , table(origin_grouped)]
round( tax_tab_origin/ref_tab_origin, 2)

# Gender
tax_tab_gender <- tax_ind[ , table(csex)]
ref_tab_gender <- ref[!origin_grouped == "land_ch" , table(geschlecht)]
round(tax_tab_gender/ref_tab_gender, 2)

# Referendum Year
brks <- seq(1970, 2003, 5)
tax_ind[ , bin := findInterval(cabjahr, brks)]
tax_tab_refyear <- tax_ind[ ,.N, keyby = bin]

ref[!origin_grouped == "land_ch", bin := findInterval(abstimmungsjahr, brks)]
ref_tab_refyear <- ref[!origin_grouped == "land_ch" , .N, keyby = bin]

round(tax_tab_refyear/ref_tab_refyear, 2)

# Referendum Age
brks <- c(-10, seq(20, 70, 10))
tax_ind[ , bin := findInterval(ref_age, brks)]
tax_tab_refage <- tax_ind[ , .N, keyby = bin]

ref[ !origin_grouped == "land_ch", bin := findInterval(abstimmungsjahr - geburtsjahr, brks)]
ref_tab_refage <- ref[!origin_grouped == "land_ch", .N, keyby = bin]

round(tax_tab_refage/ref_tab_refage, 2)
########################################

########################################
# Table S3: Descriptive Statistics (RDD data)
########################################

# mean
means <- RDD[ , lapply(.SD, function(x) mean(as.numeric(x), na.rm = T)), .SDcols = colnames(RDD)[!colnames(RDD) %in% c("origin_grouped", "prct_ja_0", "obs_pre5")]]

# sd
sds <- RDD[ , lapply(.SD, function(x) sd(as.numeric(x), na.rm = T)), .SDcols = colnames(RDD)[!colnames(RDD) %in% c("origin_grouped", "prct_ja_0", "obs_pre5")]]

# prop. NA
naprops <- RDD[ , lapply(.SD, function(x) sum(is.na(x))/length(x)), .SDcols = colnames(RDD)[!colnames(RDD) %in% c("origin_grouped", "prct_ja_0", "obs_pre5")]]

descriptive_stats <- base::rbind(means, sds, naprops)
descriptive_stats <- t(descriptive_stats)
colnames(descriptive_stats) <- c("Mean","Standard Deviation", "Proportion Missing")
descriptive_stats <- round(descriptive_stats, 3)

analysis_vars <- 
  c(
    "Percent \"Yes\" Votes" = "prct_ja",
    "Above 50%" = "naturalized",
    "Referendum Year" = "cabjahr",
    "Birth Year" = "cgebj",
    "Age at Referendum" = "ref_age",
    "Female" = "female",
    "Origin: Africa" = "Africa",
    "Origin: Asia" = "Asia",
    "Origin: Western Europe" = "EU",
    "Origin: Central and Eastern Europe" = "MEE",
    "Origin: Southern Europe" = "South",
    "Origin: Turkey & (former) Yugoslavia" = "SEE",
    "Origin: Other" = "Other",
    "Earnings, 5 years before Ref." = "sal2k_pre5",
    "Earnings, 4 years before Ref." = "sal2k_pre4",
    "Earnings, 3 years before Ref." = "sal2k_pre3",
    "Earnings, 2 years before Ref." = "sal2k_pre2",
    "Earnings, 1 years before Ref." = "sal2k_pre1",
    "Mean Earnings, 1 to 5 years before Ref." = "sal2k_avg_pre_1to5",
    "Mean Earnings, 0 to 5 years after Ref." = "sal2k_avg_post_0to5",
    "Mean Earnings, 6 to 10 years after Ref." = "sal2k_avg_post_6to10",
    "Mean Earnings, 11 to 15 years after Ref." = "sal2k_avg_post_11to15",
    "Mean Earnings, 0 to 10 years after Ref." = "sal2k_avg_post_0to10",
    "Mean Earnings, 0 to 15 years after Ref." = "sal2k_avg_post_0to15",
    "Mean Unemployment, 1 to 5 years before Ref." = "unempMonths_avg_pre_1to5",
    "Mean Unemployment, 0 to 5 years after Ref." = "unempMonths_avg_post_0to5",
    "Mean Unemployment, 6 to 10 years after Ref." = "unempMonths_avg_post_6to10",
    "Mean Unemployment, 11 to 15 years after Ref." = "unempMonths_avg_post_11to15",
    "Mean Unemployment, 0 to 10 years after Ref." = "unempMonths_avg_post_0to10",
    "Mean Unemployment, 0 to 15 years after Ref." = "unempMonths_avg_post_0to15",
    "Mean Disability Ben., 1 to 5 years before Ref." = "disability_avg_pre_1to5",
    "Mean Disability Ben., 0 to 5 years after Ref." = "disability_avg_post_0to5",
    "Mean Disability Ben., 6 to 10 years after Ref." = "disability_avg_post_6to10",
    "Mean Disability Ben., 11 to 15 years after Ref." = "disability_avg_post_11to15",
    "Mean Disability Ben., 0 to 10 years after Ref." = "disability_avg_post_0to10",
    "Mean Disability Ben., 0 to 15 years after Ref." = "disability_avg_post_0to15",
    "Early Retirement, 0 to 5 years after Ref." = "early_retirement0to5",
    "Early Retirement, 6 to 10 years after Ref." = "early_retirement6to10",
    "Early Retirement., 11 to 15 years after Ref." = "early_retirement11to15",
    "Early Retirement, 0 to 10 years after Ref." = "early_retirement0to10",
    "Early Retirement, 0 to 15 years after Ref." = "early_retirement0to15",
    "Number of Post-Ref. Observations" = "obs_freq_0to15"
  )

to_tex <- data.frame(descriptive_stats)[ analysis_vars, 1:3]
rownames(to_tex) <- names(analysis_vars)
# to_tex$Proportion.Missing <- prettyNum(round(to_tex$Proportion.Missing, 2), zero.print = "0")
# to_tex$Mean <- prettyNum(round(to_tex$Mean, 2), big.mark = ",")
# to_tex$Standard.Deviation <- prettyNum(round(to_tex$Standard.Deviation, 2), big.mark = ",")
xtable::xtable(to_tex, caption = "Descriptive Statistics", digits = c(0,2,2,2), label = "tab:stats")
########################################

########################################
# Table S4: Descriptive Statistics (did data)
########################################

did_ds <- unique(did[ , .(prct_ja, naturalized, cabjahr, cgebj, ref_age, female, Africa, Asia, EU, MEE, South, SEE, Other, obs_freq_0to15)])

# mean
means <- did_ds[ , lapply(.SD, function(x) mean(as.numeric(x), na.rm = T))]

# sd
sds <- did_ds[ , lapply(.SD, function(x) sd(as.numeric(x), na.rm = T))]

# prop. NA
naprops <- did_ds[ , lapply(.SD, function(x) sum(is.na(x))/length(x))]

descriptive_stats <- base::rbind(means, sds, naprops)
descriptive_stats <- t(descriptive_stats)

########### Now add in the time-varying variables

# mean
means <- did[ , lapply(.SD, function(x) mean(as.numeric(x), na.rm = T)), .SDcols = c("sal2k","unemp_months", "disability_yearly", "dacot", "ref_time")]

# sd
sds <- did[ , lapply(.SD, function(x) sd(as.numeric(x), na.rm = T)), .SDcols = c("sal2k","unemp_months", "disability_yearly", "dacot", "ref_time")]

# prop. NA
naprops <- did[ , lapply(.SD, function(x) sum(is.na(x))/length(x)), .SDcols = c("sal2k","unemp_months", "disability_yearly", "dacot", "ref_time")]

descriptive_stats_tv <- base::rbind(means, sds, naprops)
descriptive_stats_tv <- t(descriptive_stats_tv)
descriptive_stats <- base::rbind(descriptive_stats, descriptive_stats_tv)
colnames(descriptive_stats) <- c("Mean", "Standard Deviation", "Proportion Missing")
descriptive_stats <- round(descriptive_stats, 3)

analysis_vars <- 
  c(
    "Percent \"Yes\" Votes" = "prct_ja",
    "Above 50%" = "naturalized",
    "Referendum Year" = "cabjahr",
    "Birth Year" = "cgebj",
    "Age at Referendum" = "ref_age",
    "Female" = "female",
    "Origin: Africa" = "Africa",
    "Origin: Asia" = "Asia",
    "Origin: Western Europe" = "EU",
    "Origin: Central and Eastern Europe" = "MEE",
    "Origin: Southern Europe" = "South",
    "Origin: Turkey & (former) Yugoslavia" = "SEE",
    "Origin: Other" = "Other",
    "Earnings (CHF)" = "sal2k",
    "Unemployment (Months)" = "unemp_months",
    "Disability Benefits (Yearly)" = "disability_yearly",
    "Calendar Year" = "dacot",
    "Years Since Referendum" = "ref_time",
    "Number of Post-Ref. Observations" = "obs_freq_0to15"
  )

to_tex <- data.frame(descriptive_stats)[ analysis_vars, 1:3]
rownames(to_tex) <- names(analysis_vars)
xtable::xtable(to_tex, caption = "Descriptive Statistics", digits = c(0,2,2,2), label = "tab:stats")
########################################

########################################
# Figure S5/Table S7D-i-D without CPI adjustment
########################################
did_formula_noCPI <- as.formula("salary ~ post_naturalized | personID + dacot | 0 | personID")
did_formula_placebo_noCPI <- as.formula("salary ~ post_naturalized_placebo | personID  + dacot | 0 | personID")

did_placebo_noCPI <- felm_correct(f = did_formula_placebo_noCPI, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw])
did_0to5_noCPI <- felm_correct(f = did_formula_noCPI, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw])
did_6to10_noCPI <- felm_correct(f = did_formula_noCPI, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw])
did_11to15_noCPI <- felm_correct(f = did_formula_noCPI, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw])
did_0to10_noCPI <- felm_correct(f = did_formula_noCPI, data = did[ref_time %in% -5:10 & prct_ja %in% did_bw])
did_0to15_noCPI <- felm_correct(f = did_formula_noCPI, data = did[ prct_ja %in% did_bw])

# create table of D-i-D results
texreg(
  list(
    did_placebo_noCPI,
    did_0to5_noCPI,
    did_6to10_noCPI,
    did_11to15_noCPI,
    did_0to10_noCPI,
    did_0to15_noCPI
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption.above = T,
  caption = "Difference-in-Differences Results without CPI Adjustment to Earnings (CHF)",
  label = "tab:noCPIDD",
  include.rsquared = F,
  dcolumn = T,
  booktabs = T,
  fontsize = "footnotesize",
  center = F
)
########################################

########################################
# Figure S5/Table S8: RD without CPI adjustment
########################################

rd_placebo_noCPI <- rd_fit("sal_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw])
rd_0to5_noCPI <- rd_fit("sal_avg_post_0to5", data = RDD[prct_ja %in% rd_bw])
rd_6to10_noCPI <- rd_fit("sal_avg_post_6to10", data = RDD[prct_ja %in% rd_bw])
rd_11to15_noCPI <- rd_fit("sal_avg_post_11to15", data = RDD[prct_ja %in% rd_bw])
rd_0to10_noCPI <- rd_fit("sal_avg_post_0to10", data = RDD[prct_ja %in% rd_bw])
rd_0to15_noCPI <- rd_fit("sal_avg_post_0to15", data = RDD[prct_ja %in% rd_bw])

texreg(
  list(
    rd_placebo_noCPI,
    rd_0to5_noCPI,
    rd_6to10_noCPI,
    rd_11to15_noCPI,
    rd_0to10_noCPI,
    rd_0to15_noCPI
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results without CPI Adjustment to Earnings (CHF)",
  caption.above = T,
  label = "tab:noCPIRD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize"
)
########################################

########################################
# Table S9: D-i-D models without people that ever have self-employment
########################################

did_noSE_placebo <- felm_correct(f = did_formula_placebo, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw & self_emp_ever == F])
did_noSE_0to5 <- felm_correct(f = did_formula, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw & self_emp_ever == F])
did_noSE_6to10 <- felm_correct(f = did_formula, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw & self_emp_ever == F])
did_noSE_11to15 <- felm_correct(f = did_formula, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw & self_emp_ever == F])
did_noSE_0to10 <- felm_correct(f = did_formula, data = did[ref_time %in% -5:10 & prct_ja %in% did_bw & self_emp_ever == F])
did_noSE_0to15 <- felm_correct(f = did_formula, data = did[ prct_ja %in% did_bw & self_emp_ever == F])

# create table of D-i-D results
texreg(
  list(
    did_noSE_placebo,
    did_noSE_0to5,
    did_noSE_6to10,
    did_noSE_11to15,
    did_noSE_0to10,
    did_noSE_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption.above = T,
  caption = "Difference-in-Differences Results without Applicants without Self-Employment Earnings",
  label = "tab:noSEDD",
  include.rsquared = F,
  dcolumn = T,
  booktabs = T,
  fontsize = "footnotesize",
  center = F
)
########################################

########################################
# Table S10: RD models without any self-employment
########################################

rd_noSE_placebo <- rd_fit("sal2k_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw & self_emp_ever == F])
rd_noSE_0to5 <- rd_fit("sal2k_avg_post_0to5", data = RDD[prct_ja %in% rd_bw & self_emp_ever == F])
rd_noSE_6to10 <- rd_fit("sal2k_avg_post_6to10", data = RDD[prct_ja %in% rd_bw & self_emp_ever == F])
rd_noSE_11to15 <- rd_fit("sal2k_avg_post_11to15", data = RDD[prct_ja %in% rd_bw & self_emp_ever == F])
rd_noSE_0to10 <- rd_fit("sal2k_avg_post_0to10", data = RDD[prct_ja %in% rd_bw & self_emp_ever == F])
rd_noSE_0to15 <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & self_emp_ever == F])

texreg(
  list(
    rd_noSE_placebo,
    rd_noSE_0to5,
    rd_noSE_6to10,
    rd_noSE_11to15,
    rd_noSE_0to10,
    rd_noSE_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results without Applicants with Self-Employment Earnings",
  caption.above = T,
  label = "tab:noSERD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize"
)
########################################

########################################
# Table S11: DD models w/ ref-time and dacot FE
########################################

did_formula_3W <- as.formula("sal2k ~ post_naturalized | personID + ref_time + dacot | 0 | personID")
did_formula_placebo_3W <- as.formula("sal2k ~ post_naturalized_placebo | personID  + ref_time + dacot | 0 | personID")

did_placebo_3W <- felm_correct(f = did_formula_placebo_3W, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw])
did_0to5_3W <- felm_correct(f = did_formula_3W, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw])
did_6to10_3W <- felm_correct(f = did_formula_3W, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw])
did_11to15_3W <- felm_correct(f = did_formula_3W, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw])
did_0to10_3W <- felm_correct(f = did_formula_3W, data = did[ ref_time %in% -5:10 & prct_ja %in% did_bw])
did_0to15_3W <- felm_correct(f = did_formula_3W, data = did[ prct_ja %in% did_bw])

# create table of D-i-D results
texreg(
  list(
    did_placebo_3W,
    did_0to5_3W,
    did_6to10_3W,
    did_11to15_3W,
    did_0to10_3W,
    did_0to15_3W
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption.above = T,
  caption = "Difference-in-Differences Results with \\textit{Years Since Referendum} and \\textit{Year} Fixed Effects",
  label = "tab:DDboth",
  include.rsquared = F,
  dcolumn = T,
  booktabs = T,
  fontsize = "footnotesize",
  center = F
)
########################################

########################################
# Table S12: RD without covariates
########################################

rd_placebo_noCov <- rd_fit("sal2k_avg_pre_1to5", cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_0to5_noCov <- rd_fit("sal2k_avg_post_0to5", cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_6to10_noCov <- rd_fit("sal2k_avg_post_6to10", cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_11to15_noCov <- rd_fit("sal2k_avg_post_11to15", cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_0to10_noCov <- rd_fit("sal2k_avg_post_0to10", cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_0to15_noCov <- rd_fit("sal2k_avg_post_0to15", cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])

texreg(
  list(
    rd_placebo_noCov,
    rd_0to5_noCov,
    rd_6to10_noCov,
    rd_11to15_noCov,
    rd_0to10_noCov,
    rd_0to15_noCov
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results without Covariates",
  caption.above = T,
  label = "tab:RDnocov",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize"
)
########################################

########################################
# Table S13: RD models with only D-i-M 
########################################
dim_formula <- "~ naturalized  + female + origin_grouped + factor(ref_age) + factor(cabjahr)"

rd_placebo_dim <- rd_fit("sal2k_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula)
rd_0to5_dim <- rd_fit("sal2k_avg_post_0to5", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula)
rd_6to10_dim <- rd_fit("sal2k_avg_post_6to10", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula)
rd_11to15_dim <- rd_fit("sal2k_avg_post_11to15", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula)
rd_0to10_dim <- rd_fit("sal2k_avg_post_0to10", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula)
rd_0to15_dim <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula)

texreg(
  list(
    rd_placebo_dim,
    rd_0to5_dim,
    rd_6to10_dim,
    rd_11to15_dim,
    rd_0to10_dim,
    rd_0to15_dim
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results with Difference-in-Means Specification",
  caption.above = T,
  label = "tab:RDdim",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)

########################################

########################################
# Table S14: RD models with only D-i-M & no cov
########################################
dim_formula_nc <- "~ naturalized + factor(cabjahr)"

rd_placebo_dim_nc <- rd_fit("sal2k_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula_nc)
rd_0to5_dim_nc <- rd_fit("sal2k_avg_post_0to5", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula_nc)
rd_6to10_dim_nc <- rd_fit("sal2k_avg_post_6to10", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula_nc)
rd_11to15_dim_nc <- rd_fit("sal2k_avg_post_11to15", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula_nc)
rd_0to10_dim_nc <- rd_fit("sal2k_avg_post_0to10", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula_nc)
rd_0to15_dim_nc <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw], cov.string = dim_formula_nc)

texreg(
  list(
    rd_placebo_dim_nc,
    rd_0to5_dim_nc,
    rd_6to10_dim_nc,
    rd_11to15_dim_nc,
    rd_0to10_dim_nc,
    rd_0to15_dim_nc
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results with Difference-in-Means Specification and without Covariates",
  caption.above = T,
  label = "tab:RDdimnc",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S15: RD with median imputed referendum ages unreal ages
########################################

rd_placebo_impAge <- rd_fit("sal2k_avg_pre_1to5", cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(alt_ref_age) + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_0to5_impAge <- rd_fit("sal2k_avg_post_0to5", cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(alt_ref_age) + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_6to10_impAge <- rd_fit("sal2k_avg_post_6to10", cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(alt_ref_age) + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_11to15_impAge <- rd_fit("sal2k_avg_post_11to15", cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(alt_ref_age) + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_0to10_impAge <- rd_fit("sal2k_avg_post_0to10", cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(alt_ref_age) + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])
rd_0to15_impAge <- rd_fit("sal2k_avg_post_0to15", cov.string = " ~ naturalized*prct_ja_0 + female + origin_grouped + factor(alt_ref_age) + factor(cabjahr)", data = RDD[prct_ja %in% rd_bw])

texreg(
  list(
    rd_placebo_impAge,
    rd_0to5_impAge,
    rd_6to10_impAge,
    rd_11to15_impAge,
    rd_0to10_impAge,
    rd_0to15_impAge
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results with Unrealistic \\textit{Referendum Age} Values Recoded",
  caption.above = T,
  label = "tab:age_recode",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S16: RD dropping unrealistic ages
########################################

rd_placebo_dropAge <- rd_fit("sal2k_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw & unreal_ages == F])
rd_0to5_dropAge <- rd_fit("sal2k_avg_post_0to5", data = RDD[prct_ja %in% rd_bw & unreal_ages == F])
rd_6to10_dropAge <- rd_fit("sal2k_avg_post_6to10", data = RDD[prct_ja %in% rd_bw  & unreal_ages == F])
rd_11to15_dropAge <- rd_fit("sal2k_avg_post_11to15", data = RDD[prct_ja %in% rd_bw & unreal_ages == F])
rd_0to10_dropAge <- rd_fit("sal2k_avg_post_0to10", data = RDD[prct_ja %in% rd_bw & unreal_ages == F])
rd_0to15_dropAge <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & unreal_ages == F])

texreg(
  list(
    rd_placebo_dropAge,
    rd_0to5_dropAge,
    rd_6to10_dropAge,
    rd_11to15_dropAge,
    rd_0to10_dropAge,
    rd_0to15_dropAge
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results Excluding Applicants with Unrealistic \\textit{Referendum Age} Values",
  caption.above = T,
  label = "tab:age_drop",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S17: DD, dropping the no-earnigns ever people
########################################

did_ilm_placebo <- felm_correct(f = did_formula_placebo, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw & no_labor_market == F ])
did_ilm_0to5 <- felm_correct(f = did_formula, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw & no_labor_market == F])
did_ilm_6to10 <- felm_correct(f = did_formula, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw & no_labor_market == F])
did_ilm_11to15 <- felm_correct(f = did_formula, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw & no_labor_market == F])
did_ilm_0to10 <- felm_correct(f = did_formula, data = did[ prct_ja %in% did_bw & ref_time %in% -5:10 & no_labor_market == F])
did_ilm_0to15 <- felm_correct(f = did_formula, data = did[ prct_ja %in% did_bw & no_labor_market == F])

# create table of D-i-D results
texreg(
  list(
    did_ilm_placebo,
    did_ilm_0to5,
    did_ilm_6to10,
    did_ilm_11to15,
    did_ilm_0to10,
    did_ilm_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo) ",
    "post_naturalized" = "Above 50% "
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Difference-in-Differences Results Excluding Applicants with Zero Earnings in All Years",
  caption.above = T,
  label = "tab:nozeroDD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)

########################################

########################################
# table S18: excluding people with 0 earnings in all years, RD design 
########################################

rd_ilm_placebo <- rd_fit("sal2k_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw & no_labor_market == F])
rd_ilm_0to5 <- rd_fit("sal2k_avg_post_0to5", data = RDD[prct_ja %in% rd_bw & no_labor_market == F])
rd_ilm_6to10 <- rd_fit("sal2k_avg_post_6to10", data = RDD[prct_ja %in% rd_bw & no_labor_market == F])
rd_ilm_11to15 <- rd_fit("sal2k_avg_post_11to15", data = RDD[prct_ja %in% rd_bw & no_labor_market == F])
rd_ilm_0to10 <- rd_fit("sal2k_avg_post_0to10", data = RDD[prct_ja %in% rd_bw & no_labor_market == F])
rd_ilm_0to15 <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & no_labor_market == F])

texreg(
  list(
    rd_ilm_placebo,
    rd_ilm_0to5,
    rd_ilm_6to10,
    rd_ilm_11to15,
    rd_ilm_0to10,
    rd_ilm_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Results Excluding Applicants with Zero Earnings in All Years",
  caption.above = T,
  label = "tab:nozeroRD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)

########################################

########################################
# Table S19: DD models without 64/65 age limit on earnings
########################################

did2_placebo <- felm_correct(f = did_formula_placebo, data = did2[ref_time %in% -5:-1 & prct_ja %in% did_bw])
did2_0to5 <- felm_correct(f = did_formula, data = did2[ref_time %in% -5:5 & prct_ja %in% did_bw])
did2_6to10 <- felm_correct(f = did_formula, data = did2[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw])
did2_11to15 <- felm_correct(f = did_formula, data = did2[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw])
did2_0to10 <- felm_correct(f = did_formula, data = did2[prct_ja %in% did_bw & ref_time %in% -5:10])
did2_0to15 <- felm_correct(f = did_formula, data = did2[ prct_ja %in% did_bw])

# create table of D-i-D results
texreg(
  list(
    did2_placebo,
    did2_0to5,
    did2_6to10,
    did2_11to15,
    did2_0to10,
    did2_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption.above = T,
  caption = "Difference-in-Differences Results Including Earnings After Retirement Age",
  label = "tab:allagesDD",
  include.rsquared = F,
  dcolumn = T,
  booktabs = T,
  fontsize = "footnotesize"
)
########################################

########################################
# Table S20: RD models without age limit on earnings
########################################

rd_allAge_placebo <- rd_fit("sal2k_allAge_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw ])
rd_allAge_0to5 <- rd_fit("sal2k_allAge_avg_post_0to5", data = RDD[prct_ja %in% rd_bw])
rd_allAge_6to10 <- rd_fit("sal2k_allAge_avg_post_6to10", data = RDD[prct_ja %in% rd_bw])
rd_allAge_11to15 <- rd_fit("sal2k_allAge_avg_post_11to15", data = RDD[prct_ja %in% rd_bw])
rd_allAge_0to10 <- rd_fit("sal2k_allAge_avg_post_0to10", data = RDD[prct_ja %in% rd_bw])
rd_allAge_0to15 <- rd_fit("sal2k_allAge_avg_post_0to15", data = RDD[prct_ja %in% rd_bw])

texreg(
  list(
    rd_allAge_placebo,
    rd_allAge_0to5,
    rd_allAge_6to10,
    rd_allAge_11to15,
    rd_allAge_0to10,
    rd_allAge_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Regression Discontinuity Design Results Including Earnings After Retirement Age",
  caption.above = T,
  label = "tab:allagesRD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize"
)
########################################

########################################
# Figure S6: Robustness to alternative
########################################

BW_mat <- cbind(50:0, 50:100)
BW_results_DD <- data.frame(
  "Beta" = rep(as.numeric(NA), nrow(BW_mat)),
  "SE" = rep(as.numeric(NA), nrow(BW_mat)),
  "df" = rep(as.numeric(NA), nrow(BW_mat)),
  "N" = rep(as.numeric(NA), nrow(BW_mat)),
  "BW" = 0:(nrow(BW_mat)-1))

for(row in 1:nrow(BW_mat)){
  temp <- felm_correct(f = did_formula, data = did[prct_ja %in% BW_mat[row,1]:BW_mat[row,2]], chatty = F)
  BW_results_DD[row, "Beta"] <- summary(temp)$coef[1]
  BW_results_DD[row, "SE"] <- summary(temp)$coef[2]
  BW_results_DD[row, "df"] <- df.residual(temp)
  BW_results_DD[row, "N"] <- temp$N
}

BW_results_DD

########################################

########################################
# Figure S7: Robustness to alternative BW
########################################

BW_mat <- cbind(50:0, 50:100)
BW_results_RD <- data.frame(
  "Beta" = rep(as.numeric(NA), nrow(BW_mat)),
  "SE" = rep(as.numeric(NA), nrow(BW_mat)),
  "df" = rep(as.numeric(NA), nrow(BW_mat)),
  "N" = rep(as.numeric(NA), nrow(BW_mat)),
  "BW" = 0:(nrow(BW_mat)-1))

for(row in 1:nrow(BW_mat)){
  temp <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% BW_mat[row,1]:BW_mat[row,2]], chatty = F)
  BW_results_RD[row, "Beta"] <- summary(temp)$coef["naturalized",1]
  BW_results_RD[row, "SE"] <- summary(temp)$coef["naturalized",2]
  BW_results_RD[row, "df"] <- df.residual(temp)
  BW_results_RD[row, "N"] <- nobs(temp)
}

BW_results_RD

########################################

########################################
# Table S22: Gender Subgroup Analyses
########################################
did_0to15_female <- felm_correct(f = did_formula, data = did[ref_time %in% -5:15 & prct_ja %in% did_bw & female == T])
did_0to15_male <- felm_correct(f = did_formula, data = did[ref_time %in% -5:15 & prct_ja %in% did_bw & female == F])

rd_0to15_female <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & female == T], cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)")
rd_0to15_male <- rd_fit("sal2k_avg_post_0to15", data = RDD[prct_ja %in% rd_bw & female == F], cov.string = " ~ naturalized*prct_ja_0 + factor(cabjahr)")

texreg(
  list(
    did_0to15_female,
    did_0to15_male,
    rd_0to15_female,
    rd_0to15_male
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%",
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:4,")"),
  caption = "Effects of Referendum Success by Gender",
  caption.above = T,
  label = "tab:genderEffects",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F
)

########################################

########################################
# Table S24: RD analysis of attrition
########################################
rd_attrition <- rd_fit("obs_freq_0to15", data = RDD[prct_ja %in% rd_bw])
rd_attrition_noCov <- rd_fit("obs_freq_0to15", data = RDD[prct_ja %in% rd_bw], cov.string = "~ naturalized*prct_ja_0 + factor(cabjahr)")


texreg(
  list(
    rd_attrition,
    rd_attrition_noCov
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:2,")"),
  caption = "Effect of Referendum Success on Post-referendum Attrition, Regression Discontinuity Design",
  caption.above = T,
  label = "tab:attritionEffects",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F
)
########################################

########################################
# Table S25: D-i-D for unemployment months 
########################################
did_formula_unemp <- as.formula("unemp_months ~ post_naturalized | personID + dacot | 0 | personID")
did_formula_placebo_unemp <- as.formula("unemp_months ~ post_naturalized_placebo | personID + dacot | 0 | personID")

did_placebo_unemp <- felm_correct(f = did_formula_placebo_unemp, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw])
did_0to5_unemp <- felm_correct(f = did_formula_unemp, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw])
did_6to10_unemp <- felm_correct(f = did_formula_unemp, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw])
did_11to15_unemp <- felm_correct(f = did_formula_unemp, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw])
did_0to10_unemp <- felm_correct(f = did_formula_unemp, data = did[ref_time %in% -5:10 & prct_ja %in% did_bw])
did_0to15_unemp <- felm_correct(f = did_formula_unemp, data = did[ prct_ja %in% did_bw])

# create table of D-i-D results
texreg(
  list(
    did_placebo_unemp,
    did_0to5_unemp,
    did_6to10_unemp,
    did_11to15_unemp,
    did_0to10_unemp,
    did_0to15_unemp
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Effect of Referendum Success on Unemployment, Difference-in-Differences Design",
  caption.above = T,
  label = "tab:unempDD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S26: RD for unemployment Months
########################################

rd_placebo_unemp <- rd_fit("unempMonths_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw ])
rd_0to5_unemp <- rd_fit("unempMonths_avg_post_0to5", data = RDD[prct_ja %in% rd_bw])
rd_6to10_unemp <- rd_fit("unempMonths_avg_post_6to10", data = RDD[prct_ja %in% rd_bw])
rd_11to15_unemp <- rd_fit("unempMonths_avg_post_11to15", data = RDD[prct_ja %in% rd_bw])
rd_0to10_unemp <- rd_fit("unempMonths_avg_post_0to10", data = RDD[prct_ja %in% rd_bw])
rd_0to15_unemp <- rd_fit("unempMonths_avg_post_0to15", data = RDD[prct_ja %in% rd_bw])

texreg(
  list(
    rd_placebo_unemp,
    rd_0to5_unemp,
    rd_6to10_unemp,
    rd_11to15_unemp,
    rd_0to10_unemp,
    rd_0to15_unemp
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Effect of Referendum Success on Unemployment, Regression Discontinuity Design",
  caption.above = T,
  label = "tab:unempRD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S27: D-i-D models for disability usage 
########################################

did_formula_disab <- as.formula("disability_yearly ~ post_naturalized | personID + dacot | 0 | personID")
did_formula_placebo_disab <- as.formula("disability_yearly ~ post_naturalized_placebo | personID + dacot | 0 | personID")

did_disab_placebo <- felm_correct(f = did_formula_placebo_disab, data = did[ref_time %in% -5:-1 & prct_ja %in% did_bw ])
did_disab_0to5 <- felm_correct(f = did_formula_disab, data = did[ref_time %in% -5:5 & prct_ja %in% did_bw])
did_disab_6to10 <- felm_correct(f = did_formula_disab, data = did[ref_time %in% c(-5:-1, 6:10) & prct_ja %in% did_bw])
did_disab_11to15 <- felm_correct(f = did_formula_disab, data = did[ref_time %in% c(-5:-1, 11:15) & prct_ja %in% did_bw])
did_disab_0to10 <- felm_correct(f = did_formula_disab, data = did[ prct_ja %in% did_bw & ref_time %in% -5:10 ])
did_disab_0to15 <- felm_correct(f = did_formula_disab, data = did[ prct_ja %in% did_bw ])

# create table of D-i-D results
texreg(
  list(
    did_disab_placebo,
    did_disab_0to5,
    did_disab_6to10,
    did_disab_11to15,
    did_disab_0to10,
    did_disab_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "post_naturalized_placebo" = "Above 50% (placebo)",
    "post_naturalized" = "Above 50%"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Effect of Referendum Success on Receiving Disability Benefits, Differences-in-Differences Design",
  caption.above = T,
  label = "tab:disabilityDD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S28: Disability usage, RD design
########################################

rd_disab_placebo <- rd_fit("disability_avg_pre_1to5", data = RDD[prct_ja %in% rd_bw])
rd_disab_0to5 <- rd_fit("disability_avg_post_0to5", data = RDD[prct_ja %in% rd_bw ])
rd_disab_6to10 <- rd_fit("disability_avg_post_6to10", data = RDD[prct_ja %in% rd_bw ])
rd_disab_11to15 <- rd_fit("disability_avg_post_11to15", data = RDD[prct_ja %in% rd_bw ])
rd_disab_0to10 <- rd_fit("disability_avg_post_0to10", data = RDD[prct_ja %in% rd_bw ])
rd_disab_0to15 <- rd_fit("disability_avg_post_0to15", data = RDD[prct_ja %in% rd_bw ])

texreg(
  list(
    rd_disab_placebo,
    rd_disab_0to5,
    rd_disab_6to10,
    rd_disab_11to15,
    rd_disab_0to10,
    rd_disab_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:6,")"),
  caption = "Effect of Referendum Success on Receiving Disability Benefits, Regression Discontinuity Design",
  caption.above = T,
  label = "tab:disabilityRD",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)
########################################

########################################
# Table S29: RD Early Retirement Results
########################################

rd_retire_0to5 <- rd_fit("early_retirement0to5", data = RDD[prct_ja %in% rd_bw ])
rd_retire_6to10 <- rd_fit("early_retirement6to10", data = RDD[prct_ja %in% rd_bw ])
rd_retire_11to15 <- rd_fit("early_retirement11to15", data = RDD[prct_ja %in% rd_bw ])
rd_retire_0to10 <- rd_fit("early_retirement0to10", data = RDD[prct_ja %in% rd_bw ])
rd_retire_0to15 <- rd_fit("early_retirement0to15", data = RDD[prct_ja %in% rd_bw ])

texreg(
  list(
    rd_retire_0to5,
    rd_retire_6to10,
    rd_retire_11to15,
    rd_retire_0to10,
    rd_retire_0to15
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = list(
    "naturalized" = "Above 50%",
    "prct_ja_0" = "Margin",
    "naturalized:prct_ja_0" = "Above 50% * Margin"
  ),
  custom.model.names = paste0("(", 1:5,")"),
  caption = "Effect of Referendum Success on Early Retirement",
  caption.above = T,
  label = "tab:early_retirement",
  include.rsquared = F,
  include.rmse = F,
  dcolumn = T,
  booktabs = T,
  center = F,
  fontsize = "footnotesize",
  custom.note = ""
)

########################################

########################################
# Table S30: First stage result using the AHV + ref result measure of citizenship
########################################

firstStage_out <- did[ ref_time >= 0 & prct_ja %in% 40:60 , .("$N$ Accepted" = sum(naturalized), "$N$ Rejected" = sum(!naturalized), "Effect Est." = summary(lm(nat2 ~ naturalized))$coefficients[2,1], "(S.E.)" = summary(lm(nat2 ~ naturalized))$coefficients[2,2]), keyby = ref_time]

print(xtable::xtable(firstStage_out, label = "tab:firstStage", digits = 2, caption = "Effect of Referendum Success on Citizenship According to CCO Records. Estimates are based on the DD sample. We code all referendum winners as naturalized regardless of their entry in CCO record."), include.rownames = F, caption.placement = "top", booktabs = T)

########################################