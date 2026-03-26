#################################
# New Zealand Analysis Replication
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
five_years <- seq(2016,1950,-5)
avgData <- avgData.maker(data, five_years)

#pick only years included in analysis and drop Malta due to no elections in 1956-1960 period.
NZ <- avgData[avgData$yr >= 1951 & avgData$yr <= 2016 & !avgData$ctr_n %in% c("Malta", "Norway"),]

# Create the outcome variable, accounting for MMP in NZ and Germany
NZ$outcome <- NA
NZ$outcome[!NZ$ctr_n %in% "New Zealand"] <- NZ$ENP_avg[!NZ$ctr_n %in% "New Zealand"]
NZ$outcome[NZ$ctr_n == "Germany"] <- NZ$ENP_pr[NZ$ctr_n == "Germany"]
NZ$outcome[NZ$ctr_n %in% "New Zealand"] <- ifelse(NZ$yr[NZ$ctr_n %in% "New Zealand"] >= 1996, NZ$ENP_pr[NZ$ctr_n %in% "New Zealand"], NZ$ENP_avg[NZ$ctr_n %in% "New Zealand"])

# create the log magnitude variable, accounting again for MMP in Germany. 
NZ$log_mag <- log(NZ$tier1_avemag)
NZ$log_mag[NZ$ctr_n == "Germany"] <- log(NZ$upperseats[NZ$ctr_n == "Germany"]/NZ$tier2_districts[NZ$ctr_n == "Germany"])

#Reorder the data for easier plotting 
index <- with(NZ, order(ctr_n, yr))
NZ <- NZ[index,]

#################################
#### Main Analysis (Table 1 and Figure 1)
#################################

Analysis_controls = c(
  "Ethnic",
  "Language",
  "federalism",
  "presidentialism",
  "ENP_seats",
  "log_mag"
)

NZ_five_year <- dataprep(
  foo = NZ,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1986, "mean"),
    list("outcome", 1991, "mean")),
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 16,
  controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16) ]),
  time.predictors.prior = seq(1951, 1991, 5),
  time.optimize.ssr = seq(1951, 1991, 5),
  unit.names.variable = 2,
  time.plot = seq(1951, 2011, 5)
  )

NZ_five_year.synth <- synth(NZ_five_year)

# for information on covariates, as presented in Table 1, and on weights, as presented in SI.2.1. 
SynthSummary(NZ_five_year, NZ_five_year.synth, treatment_time = 1996) 

# This code produces the gaps in effective parties reported on page 10 of the main text.
gaps <- NZ$outcome[NZ$ctr_n == "New Zealand" & NZ$yr %in% seq(1951,2011,5)] - (NZ_five_year$Y0plot %*% NZ_five_year.synth$solution.w)
pre_gaps <- gaps[1:9,]; post_gaps <- gaps[10:13,] 
summary(abs(pre_gaps))
post_gaps

#################################
#### In-Space Placebos Analysis (Figure 1)
#################################

NZ_five_year_placebos_prep <- SynthPlacebosPrep(
  data = NZ,
  predictors = Analysis_controls,
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  control.units = unique(NZ$CC),
  time.units.prior = seq(1951, 1991, 5),
  time.units.ssr = seq(1951, 1991, 5),
  unit.names = "ctr_n",
  time.units.plot = seq(1951, 2011, 5),
  special.predictors = list(
    list("outcome", 1986, "mean"),
    list("outcome", 1991, "mean")
  ))

NZ_five_year_placebos_Synth <- MultiSynth(NZ_five_year_placebos_prep, parallel = FALSE)

# The code below produces the values in Table SI.2.4 

# RMSPE Ratios (used to calculate the p-value reported on page 12 of the main text.)
round(MultiSynthErrorRatios(NZ_five_year_placebos_prep, NZ_five_year_placebos_Synth, treatment_time = 1996),2)

# Pre-Reform RMSPE
round(MultiSynthPreErrorRatios(NZ_five_year_placebos_prep, NZ_five_year_placebos_Synth, begin_time = 1951, end_time = 1991), 2)

# Post-Reform RMSPE
round(MultiSynthPreErrorRatios(NZ_five_year_placebos_prep, NZ_five_year_placebos_Synth, begin_time = 1996, end_time = 2011), 2)

# ATE
round(MultiSynthMeanEffects(NZ_five_year_placebos_prep, NZ_five_year_placebos_Synth, begin_time = 1996, end_time = 2011), 2)

#################################
#### Only Pre-Reform Outcomes as Covariates (SI 2.2)
#################################

NZ_no_cov <- dataprep(foo = NZ,
                      predictors = "ENP_seats",
                      special.predictors = list(
                        list("outcome", 1986, "mean"),
                        list("outcome", 1991, "mean")),
                      dependent = "outcome",
                      unit.variable = "CC" ,
                      time.variable = "yr",
                      treatment.identifier = 16,
                      controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16) ]),
                      time.predictors.prior = seq(1951, 1991, 5),
                      time.optimize.ssr = seq(1951, 1991, 5),
                      unit.names.variable = 2,
                      time.plot = seq(1951, 2011, 5))
NZ_no_cov.synth <- synth(NZ_no_cov)

SynthSummary(NZ_no_cov, NZ_no_cov.synth, treatment_time = 1996)

# for pre-treatment gaps reported on SI page 9
gaps <- NZ$outcome[NZ$ctr_n == "New Zealand" & NZ$yr %in% seq(1951,2011,5)] - (NZ_no_cov$Y0plot %*% NZ_no_cov.synth$solution.w)
pre_gaps <- gaps[1:9,]; post_gaps <- gaps[10:13,] 
summary(abs(pre_gaps))


#################################
#### No Pre-Reform outcomes as Covariates (SI 2.2)
#################################

NZ_no_lag <- dataprep(foo = NZ,
                      predictors = Analysis_controls[-5],
                      dependent = "outcome",
                      unit.variable = "CC" ,
                      time.variable = "yr",
                      treatment.identifier = 16,
                      controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16) ]),
                      time.predictors.prior = seq(1951, 1991, 5),
                      time.optimize.ssr = seq(1951, 1991, 5),
                      unit.names.variable = 2,
                      time.plot = seq(1951, 2011, 5))
NZ_no_lag.synth <- synth(NZ_no_lag)

SynthSummary(NZ_no_lag, NZ_no_lag.synth, treatment_time = 1996)

# for pre-treatment gaps reported on SI page 9
gaps <- NZ$outcome[NZ$ctr_n == "New Zealand" & NZ$yr %in% seq(1951,2011,5)] - (NZ_no_lag$Y0plot %*% NZ_no_lag.synth$solution.w)
pre_gaps <- gaps[1:9,]; post_gaps <- gaps[10:13,] 
summary(abs(pre_gaps))


#################################
### Leave-One-Out Analysis (SI 2.3.1)
#################################

NZ_loo_cou_pre <- SynthLeaveOneOutPrep(
  data = NZ,
  predictors = Analysis_controls,
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 16,
  control.units = unique(NZ$CC[NZ$CC != 16]),
  time.units.prior = seq(1951, 1991, 5),
  time.units.ssr = seq(1951, 1991, 5),
  unit.names = "ctr_n",
  time.units.plot = seq(1951, 2011, 5),
  special.predictors = list(
    list("outcome", 1991, "mean"),
    list("outcome", 1986, "mean")
  )
)

NZ_loo_cou_Synth <- MultiSynth(NZ_loo_cou_pre, parallel = FALSE)

# to identify how each country's exclusion impacts the fit
MultiSynthErrorRatios(NZ_loo_cou_pre, NZ_loo_cou_Synth, treatment_time = 1996)

#################################
#### Analysis Excluding Denmark and Iceland (SI 2.3.2)
#################################

NZ_five_year_noDKIC <- dataprep(
  foo = NZ,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1986, "mean"),
    list("outcome", 1991, "mean")),
  dependent = "outcome",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 16,
  controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16,6 ,9) ]),
  time.predictors.prior = seq(1951, 1991, 5),
  time.optimize.ssr = seq(1951, 1991, 5),
  unit.names.variable = 2,
  time.plot = seq(1951, 2011, 5))

NZ_five_year.synth_noDKIC <- synth(NZ_five_year_noDKIC)

SynthSummary(NZ_five_year_noDKIC, NZ_five_year.synth_noDKIC, treatment_time = 1996) 

#################################
#### Analysis Assigning Equal Weights to Covariates (SI 2.3.3)
#################################

NZ_equal_weight <- synth(NZ_five_year, custom.v = rep(1/8, 8))

SynthSummary(NZ_five_year, NZ_equal_weight, treatment_time = 1996) 

#################################
#### Analysis including only UK in Synthetic Control (SI 2.3.3)
#################################

# create the UK identifier
NZ$UK <- 0
NZ$UK[NZ$CC %in% c(16,21)] <- 1

NZ_UK <- dataprep(foo = NZ,
                  predictors = "UK",
                  dependent = "outcome",
                  unit.variable = "CC" ,
                  time.variable = "yr",
                  treatment.identifier = 16,
                  controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16) ]),
                  time.predictors.prior = seq(1951, 1991, 5),
                  time.optimize.ssr = seq(1951, 1991, 5),
                  unit.names.variable = 2,
                  time.plot = seq(1951, 2011, 5))
NZ_UK.synth <- synth(NZ_UK)

SynthSummary(NZ_UK, NZ_UK.synth, treatment_time = 1996)

# get pre-treatment covariate values for UK
apply(NZ[NZ$CC == 21 & NZ$yr <= 1991 & NZ$yr >= 1951, c("Ethnic", "Language", "presidentialism", "federalism", "log_mag", "ENP_seats") ], 2, mean)
NZ[NZ$CC == 21 & NZ$yr == 1991, "outcome" ] # "district level ENP r-5"
NZ[NZ$CC == 21 & NZ$yr == 1986, "outcome" ]  # "district level ENP r-10"

# to reproduce party system gaps described on page 14 of the SI
gaps <- NZ$outcome[NZ$ctr_n == "New Zealand" & NZ$yr %in% seq(1951,2011,5)] - (NZ_UK$Y0plot %*% NZ_UK.synth$solution.w)
pre_gaps <- gaps[1:9,]; post_gaps <- gaps[10:13,] 
post_gaps

#################################
#### Analysis using 4-year Panel (SI 2.3.4)
#################################

# recreate the data set
four_years <- seq(2016,1940,-4)
avgData <- avgData.maker(data, four_years)
NZ_4 <- avgData[avgData$yr >= 1952 & avgData$yr <= 2008 & !is.na(avgData$ENP_avg),]

#Figure out what countries don't make for a balanced panel (and then remove them)
drop_us <- Panel_Balance_test(NZ_4, "New Zealand")
NZ_4 <- NZ_4[ ! NZ_4$ctr_n %in% c(drop_us, "Norway"), ]

#create the outcome
NZ_4$outcome <- NA
NZ_4$outcome[!NZ_4$ctr_n %in% "New Zealand"] <- NZ_4$ENP_avg[!NZ_4$ctr_n %in% "New Zealand"]
NZ_4$outcome[NZ_4$ctr_n == "Germany"] <- NZ_4$ENP_pr[NZ_4$ctr_n == "Germany"]
NZ_4$outcome[NZ_4$ctr_n %in% "New Zealand"] <- ifelse(NZ_4$yr[NZ_4$ctr_n %in% "New Zealand"] >= 1996, NZ_4$ENP_pr[NZ_4$ctr_n %in% "New Zealand"], NZ_4$ENP_avg[NZ_4$ctr_n %in% "New Zealand"])

# create log magnitude variable
NZ_4$log_mag <- log(NZ_4$tier1_avemag)
NZ_4$log_mag[NZ_4$ctr_n == "Germany"] <- log(NZ_4$upperseats[NZ_4$ctr_n == "Germany"]/NZ_4$tier2_districts[NZ_4$ctr_n == "Germany"])

#Order for easier plotting
index <- with(NZ_4, order(ctr_n, yr))
NZ_4 <- NZ_4[index,]

NZ_four_year <- dataprep(
  foo = NZ_4,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1992, "mean"),
    list("outcome", 1988, "mean")),
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 16,
  controls.identifier = unique(NZ_4$CC[ ! NZ_4$CC %in% c(16) ]),
  time.predictors.prior = seq(1952, 1992, 4),
  time.optimize.ssr = seq(1952, 1992, 4),
  unit.names.variable = 2,
  time.plot = seq(1952, 2008, 4))

NZ_four_year.synth <- synth(NZ_four_year)

#summary 
SynthSummary(NZ_four_year, NZ_four_year.synth, treatment_time = 1996) 

#################################
#### Analysis using 10-year Panel (SI 2.3.4)
#################################

# follow the same steps to create a 10-year data set
ten_years <- seq(2016,1946,-10)
avgData <- avgData.maker(data, ten_years)
NZ_10 <- avgData[avgData$yr >=1946 & avgData$yr <= 2008 & !is.na(avgData$ENP_avg),]

#Remove Norway from the data
NZ_10 <- NZ_10[ ! NZ_10$ctr_n%in%c("Norway"),]

# create outcome
NZ_10$outcome <- NA
NZ_10$outcome[!NZ_10$ctr_n %in% "New Zealand"] <- NZ_10$ENP_avg[!NZ_10$ctr_n %in% "New Zealand"]
NZ_10$outcome[NZ_10$ctr_n == "Germany"] <- NZ_10$ENP_pr[NZ_10$ctr_n == "Germany"]
NZ_10$outcome[NZ_10$ctr_n %in% "New Zealand"] <- ifelse(NZ_10$yr[NZ_10$ctr_n %in% "New Zealand"] >= 1996, NZ_10$ENP_pr[NZ_10$ctr_n %in% "New Zealand"], NZ_10$ENP_avg[NZ_10$ctr_n %in% "New Zealand"])

# create log magnitude measure
NZ_10$log_mag <- log(NZ_10$tier1_avemag)
NZ_10$log_mag[NZ_10$ctr_n == "Germany"] <- log(NZ_10$upperseats[NZ_10$ctr_n == "Germany"]/NZ_10$tier2_districts[NZ_10$ctr_n == "Germany"])

# Order for easier plotting
index <- with(NZ_10, order(ctr_n, yr))
NZ_10 <- NZ_10[index,]

NZ_ten_year <- dataprep(
  foo = NZ_10,
  predictors = Analysis_controls,
  special.predictors = list(
    list("outcome", 1986, "mean"),
    list("outcome", 1976, "mean")),
  dependent = "outcome",
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 16,
  controls.identifier = unique(NZ_10$CC[ ! NZ_10$CC %in% c(16) ]),
  time.predictors.prior = seq(1946, 1986, 10),
  time.optimize.ssr = seq(1946, 1986, 10),
  unit.names.variable = 2,
  time.plot = seq(1946, 2006, 10))

NZ_ten_year.synth <- synth(NZ_ten_year)

#summary 
SynthSummary(NZ_ten_year, NZ_ten_year.synth, treatment_time = 1996) 


#################################
#### In-Time Placebo Analysis (SI 2.5)
#################################

NZ_time_placebo <- dataprep(
  foo = NZ,
  predictors = Analysis_controls,
  dependent = "outcome",
  special.predictors = list(
    list("outcome", 1971, "mean"),
    list("outcome", 1976, "mean")
  ),
  unit.variable = "CC",
  time.variable = "yr",
  treatment.identifier = 16,
  controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16) ]),
  time.predictors.prior = seq(1951, 1976, 5),
  time.optimize.ssr = seq(1951, 1976, 5),
  unit.names.variable = 2,
  time.plot = seq(1951, 1991, 5))

NZ_time_placebo.synth <- synth(NZ_time_placebo)

SynthSummary(NZ_time_placebo, NZ_time_placebo.synth, treatment_time = 1981) 

#################################
### Analysis of ENP seats (SI 4)
#################################

NZ_mech_effect <- dataprep(
  foo = NZ,
  predictors = c(Analysis_controls[-5], "outcome"),
  special.predictors = list(
    list("ENP_seats", 1986, "mean"),
    list("ENP_seats", 1991, "mean")),
  dependent = "ENP_seats",
  unit.variable = "CC" ,
  time.variable = "yr",
  treatment.identifier = 16,
  controls.identifier = unique(NZ$CC[ ! NZ$CC %in% c(16) ]),
  time.predictors.prior = seq(1951, 1991, 5),
  time.optimize.ssr = seq(1951, 1991, 5),
  unit.names.variable = 2,
  time.plot = seq(1951, 2011, 5))

NZ_mech_effect.synth <- synth(NZ_mech_effect); counter <- counter + 1

SynthSummary(NZ_mech_effect, NZ_mech_effect.synth, treatment_time = 1996)

