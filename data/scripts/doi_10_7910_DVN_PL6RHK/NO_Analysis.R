#################################
# Norway Analysis Replication
# Dalston Ward (2018)
# "Dynamic Effects of Electoral Laws"
# Journal of Elections, Public Opinion, and Parties
# DOI: 10.1080/17457289.2018.1537279
#################################

library(Synth)
library(plyr)

setwd("<<Location of replication files on your computer>>")

source("SynthFunctions.R")

#################################
#### Data Loading and Preparation
#################################

# Set up data for main analyses and robustness checks
data <- read.csv("DEEL_data.csv", stringsAsFactors=FALSE)
data <- data[, -c(4,5)] # remove two ID variables

# The next few lines transform the election level data
# into the five-year period data used in the analysis
five_years <- seq(2014,1940,-5)
avgData <- avgData.maker(data, five_years)

# Select only years inlcuded in the analysis and drop Luxembourg due to no elections in the 1969-1973 period.
NO <- avgData[avgData$yr >=1949 & avgData$yr <= 2014 & !avgData$ctr_n %in% c("Luxembourg", "New Zealand"),]

# create outcome, acounting for MMP in Germany
NO$outcome <- NA
NO$outcome <- NO$ENP_avg
NO$outcome[NO$ctr_n == " Germany"] <- NO$ENP_pr[NO$ctr_n == "Germany"]

# create log magnitude (again accounting for MMP)
NO$log_mag <- log(NO$tier1_avemag)
NO$log_mag[NO$ctr_n == "Germany"] <- log(NO$upperseats[NO$ctr_n == "Germany"]/NO$tier2_districts[NO$ctr_n == "Germany"])

#################################
#### Main analysis (Table 1 and Figure 1)
#################################

Analysis_controls = c(
  "Ethnic", 
  "Language", 
  "federalism",
  "presidentialism",
  "ENP_seats",
  "log_mag"
)

NO_five_year <- dataprep(
  foo = NO,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1984, "mean"),
    list("outcome", 1979, "mean")),
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO$CC[ ! NO$CC %in% c(17) ]),
  time.predictors.prior = seq(1949, 1984, 5),
  time.optimize.ssr = seq(1949, 1984, 5),
  unit.names.variable = 2,
  time.plot = seq(1949, 2009, 5))

NO_five_year.synth <- synth(NO_five_year)

# for information on covariates, as presented in Table 1, and on weights, as presented in SI.3.1. 
SynthSummary(NO_five_year, NO_five_year.synth, treatment_time = 1989)

# This code produces the gaps in effective parties numbers that are reported on pages 12 and 13 of the main text.
gaps <- NO$outcome[NO$ctr_n == "Norway" & NO$yr %in% seq(1949, 2009, 5)][order(NO$yr[NO$ctr_n == "Norway"])] - (NO_five_year$Y0plot %*% NO_five_year.synth$solution.w)
pre_gaps <- gaps[1:8,]; post_gaps <- gaps[9:13,] 
summary(abs(pre_gaps))
sd(abs(pre_gaps))
post_gaps

# expansions in party systems of comparison countries (as referenced on page 13)
NO$outcome[NO$yr == 2009 & NO$ctr_n == "Netherlands"] - NO$outcome[NO$yr == 2004 & NO$ctr_n == "Netherlands"]
NO$outcome[NO$yr == 2009 & NO$ctr_n == "Denmark"] - NO$outcome[NO$yr == 2004 & NO$ctr_n == "Denmark"]
NO$outcome[NO$yr == 2009 & NO$ctr_n == "Costa Rica"] - NO$outcome[NO$yr == 2004 & NO$ctr_n == "Costa Rica"]
NO$outcome[NO$yr == 2009 & NO$ctr_n == "United Kingdom"] - NO$outcome[NO$yr == 2004 & NO$ctr_n == "United Kingdom"]

####################################
#### In Space Placebos Analysis (Figure 1 and Table SI 3.4)
####################################

NO_five_year_placebos_prep <- SynthPlacebosPrep(
  data = NO,
  predictors = Analysis_controls,
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  control.units = unique(NO$CC),
  time.units.prior = seq(1949, 1984, 5),
  time.units.ssr = seq(1949, 1984, 5),
  unit.names = "ctr_n",
  time.units.plot = seq(1949, 2009, 5),
  special.predictors = list(
    list("outcome", 1984, "mean"),
    list("outcome", 1979, "mean")
  ))

NO_five_year_placebos_Synth <- MultiSynth(NO_five_year_placebos_prep, parallel = FALSE)

# The code below produces the values in Table SI 3.4

# RMSPE Ratios (used to generate p-value reported on page 13 of main text)
round(MultiSynthErrorRatios(NO_five_year_placebos_prep, NO_five_year_placebos_Synth, treatment_time = 1989),2)

# Pre-Reform RMSPE
round(MultiSynthPreErrorRatios(NO_five_year_placebos_prep, NO_five_year_placebos_Synth, begin_time = 1949, end_time = 1984), 2)

# Post-Reform RMSPE
round(MultiSynthPreErrorRatios(NO_five_year_placebos_prep, NO_five_year_placebos_Synth, begin_time = 1989, end_time = 2009), 2)

# ATE
round(MultiSynthMeanEffects(NO_five_year_placebos_prep, NO_five_year_placebos_Synth, begin_time = 1989, end_time = 2009), 2)

#################################
#### Only Pre-Reform Outcomes as Covariates (SI 3.2)
#################################

NO_no_cov <- dataprep(
  foo = NO,
  predictors = "ENP_seats",
  special.predictors = list(
    list("outcome", 1984, "mean"),
    list("outcome", 1979, "mean")),
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO$CC[ ! NO$CC %in% c(17) ]),
  time.predictors.prior = seq(1949, 1984, 5),
  time.optimize.ssr = seq(1949, 1984, 5),
  unit.names.variable = 2,
  time.plot = seq(1949, 2009, 5))

NO_no_cov.synth <- synth(NO_no_cov)

SynthSummary(NO_no_cov, NO_no_cov.synth, treatment_time = 1989)

# code to calculate pre-treatment outcome gaps as mentioned on page 22-23 of SI
gaps <- NO$outcome[NO$ctr_n == "Norway" & NO$yr %in% seq(1949, 2009, 5)][order(NO$yr[NO$ctr_n == "Norway"])] - (NO_no_cov$Y0plot %*% NO_no_cov.synth$solution.w)
pre_gaps <- gaps[1:8,]; post_gaps <- gaps[9:13,] 
summary(abs(pre_gaps))
sd(abs(pre_gaps))


#################################
#### No Pre-Reform outcomes as Covariates (SI 3.2)
#################################

NO_no_lag <- dataprep(
  foo = NO,
  predictors = Analysis_controls[-5],
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO$CC[ ! NO$CC %in% c(17) ]),
  time.predictors.prior = seq(1949, 1984, 5),
  time.optimize.ssr = seq(1949, 1984, 5),
  unit.names.variable = 2,
  time.plot = seq(1949, 2009, 5))

NO_no_lag.synth <- synth(NO_no_lag)

SynthSummary(NO_no_lag, NO_no_lag.synth, treatment_time = 1989)

# Code to calculate pre-treatment outcome gaps as mentioned on page 22-23 of SI
gaps <- NO$outcome[NO$ctr_n == "Norway" & NO$yr %in% seq(1949, 2009, 5)][order(NO$yr[NO$ctr_n == "Norway"])] - (NO_no_lag$Y0plot %*% NO_no_lag.synth$solution.w)
pre_gaps <- gaps[1:8,]; post_gaps <- gaps[9:13,] 
summary(abs(pre_gaps))

#################################
### Leave-One-Out Analysis (SI 3.3.1)
#################################

NO_loo_cou_pre <- SynthLeaveOneOutPrep(
  data = NO,
  predictors = Analysis_controls,
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 17,
  control.units = unique(NO$CC[NO$CC != 17]),
  time.units.prior = seq(1949, 1984, 5),
  time.units.ssr = seq(1949, 1984, 5),
  unit.names = "ctr_n",
  time.units.plot = seq(1949, 2009, 5),
  special.predictors = list(
    list("outcome", 1979, "mean"),
    list("outcome", 1984, "mean")
  )
)

NO_loo_cou_Synth <- MultiSynth(NO_loo_cou_pre, parallel = FALSE)

# for identifying which country's exclusion is most impactful
MultiSynthErrorRatios(NO_loo_cou_pre, NO_loo_cou_Synth, treatment_time = 1989)

#################################
#### Analysis Excluding Denmark and Iceland (SI 3.3.2)
#################################

NO_noDKIC <- dataprep(
  foo = NO,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1984, "mean"),
    list("outcome", 1979, "mean")),
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO$CC[ ! NO$CC %in% c(17, 6, 9) ]),
  time.predictors.prior = seq(1949, 1984, 5),
  time.optimize.ssr = seq(1949, 1984, 5),
  unit.names.variable = 2,
  time.plot = seq(1949, 2009, 5))

NO_noDKIC.synth <- synth(NO_noDKIC)

SynthSummary(NO_noDKIC, NO_noDKIC.synth, treatment_time = 1989)

#################################
#### Analysis using 4-year Panel (SI 3.3.3)
#################################

# Recreate the data set
four_years <- seq(2013,1940,-4)
avgData <- avgData.maker(data, four_years)
NO_4 <- avgData[avgData$yr >= 1953 & avgData$yr <= 2009 & !is.na(avgData$ENP_avg),]

#Figure out what countries don't make for a balanced panel (and then remove them)
drop_us <- Panel_Balance_test(NO_4, "Norway")
NO_4 <- NO_4[ ! NO_4$ctr_n %in% c(drop_us, "New Zealand"),]

# Create the outcome
NO_4$outcome <- NA
NO_4$outcome <- NO_4$ENP_avg
NO_4$outcome[NO_4$ctr_n == "Germany"] <- NO_4$ENP_pr[NO_4$ctr_n == "Germany"]

# Create the log magnitude variable
NO_4$log_mag <- log(NO_4$tier1_avemag)
NO_4$log_mag[NO_4$ctr_n == "Germany"] <- log(NO_4$upperseats[NO_4$ctr_n == "Germany"]/NO_4$tier2_districts[NO_4$ctr_n == "Germany"])

# Order for easier plotting 
index <- with(NO_4, order(ctr_n, yr))
NO_4 <- NO_4[index,]

NO_four_year <- dataprep(
  foo = NO_4,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1981, "mean"),
    list("outcome", 1985, "mean")),
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO_4$CC[ ! NO_4$CC %in% c(17) ]),
  time.predictors.prior = seq(1953, 1985, 4),
  time.optimize.ssr = seq(1953, 1985, 4),
  unit.names.variable = 2,
  time.plot = seq(1953, 2009, 4))

NO_four_year.synth <- synth(NO_four_year)

SynthSummary(NO_four_year, NO_four_year.synth, treatment_time = 1989) 

#################################
#### Analysis using 10-year Panel (SI 3.3.3)
#################################

# Recreate the dataset
ten_years <- seq(2019,1940,-10)
avgData <- avgData.maker(data, ten_years)
NO_10 <- avgData[avgData$yr >=1949 & avgData$yr <= 2009 & !is.na(avgData$ENP_avg),]

# Remove New Zealand
NO_10 <- NO_10[ ! NO_10$ctr_n %in% c("New Zealand"),]

# Create the outcome
NO_10$outcome <- NA
NO_10$outcome <- NO_10$ENP_avg
NO_10$outcome[NO_10$ctr_n == "Germany"] <- NO_10$ENP_pr[NO_10$ctr_n == "Germany"]

# Create the log magnitude variable
NO_10$log_mag <- log(NO_10$tier1_avemag)
NO_10$log_mag[NO_10$ctr_n == "Germany"] <- log(NO_10$upperseats[NO_10$ctr_n == "Germany"]/NO_10$tier2_districts[NO_10$ctr_n == "Germany"])

# Order for easier plotting
index <- with(NO_10, order(ctr_n, yr))
NO_10 <- NO_10[index,]

NO_ten_year <- dataprep(
  foo = NO_10,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1979, "mean"),
    list("outcome", 1969, "mean")),
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO_10$CC[ ! NO_10$CC %in% c(17) ]),
  time.predictors.prior = seq(1949, 1979, 10),
  time.optimize.ssr = seq(1949, 1979, 10),
  unit.names.variable = 2,
  time.plot = seq(1949, 2009, 10))

NO_ten_year.synth <- synth(NO_ten_year)

SynthSummary(NO_ten_year, NO_ten_year.synth, treatment_time = 1989) 

#################################
#### In-Time Placebo Analysis (SI 3.5)
#################################

NO_time_placebo <- dataprep(
  foo = NO,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1964, "mean"),
    list("outcome", 1969, "mean")),
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO$CC[ ! NO$CC %in% c(17) ]),
  time.predictors.prior = seq(1949, 1969, 5),
  time.optimize.ssr = seq(1949, 1969, 5),
  unit.names.variable = 2,
  time.plot = seq(1949, 1984, 5))

NO_time_placebo.synth <- synth(NO_time_placebo)

SynthSummary(NO_time_placebo, NO_time_placebo.synth, treatment_time = 1974)

#################################
### Analysis of ENP seats (SI 4)
#################################

NO_mech_effect <- dataprep(
  foo = NO,
  predictors = c(Analysis_controls[-5],"outcome"),
  special.predictors = list(
    list("ENP_seats", 1984, "mean"),
    list("ENP_seats", 1979, "mean")),
  dependent = "ENP_seats",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 17,
  controls.identifier = unique(NO$CC[ ! NO$CC %in% c(17,14) ]),
  time.predictors.prior = seq(1949, 1984, 5),
  time.optimize.ssr = seq(1949, 1984, 5),
  unit.names.variable = 2,
  time.plot = seq(1949, 2009, 5))

NO_mech_effect.synth <- synth(NO_mech_effect)

SynthSummary(NO_mech_effect, NO_mech_effect.synth, treatment_time = 1989)