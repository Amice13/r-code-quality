########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS IS THE MAIN CODE (#1) THAT DRAWS ON OTHER CODES AVAILABLE IN THIS REPLICATION ARCHIVE TO CARRY OUT ANALYSES


#########################
##### LIBRARIES #########
#########################


packages_needed <- c("tidyverse",
                     "foreign", 
                     "Hmisc", 
                     "survey", 
                     "fmsb", 
                     "stargazer",
                     "Weighted.Desc.Stat", 
                     "gridExtra", 
                     "car", 
                     "arsenal", 
                     "broom",
                     "margins",
                     "poliscidata",
                     "svydiags",
                     "ggeffects",
                     "effects",
                     "cowplot",
                     "xtable",
                     "poliscidata",
                     "svydiags",
                     "texreg",
                     "mfx"
                     
)

for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}



for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

# load this for crosstab function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# make a function for calculating odds ratios' standard errors
or.se = function(mod){
  or = exp(mod$coef)
  var.diag = diag(vcov(mod))  # Variance of each coefficient
  or.se = sqrt(or^2*var.diag)
  return(list(or.se=or.se))
}


#########################
##### EXTERNAL SCRIPTS ##
#########################

# (Note: The present script is "Code 1")




####
#### Code 2: Loads data, makes variables, creates survey weights, creates subsets
####

source("~/Code/2_GouldUrpelainen_Variables.R")





####
#### Code 3: Makes descriptive tables
####

source("~/Code/3_GouldUrpelainen_DescriptiveTables.R")

# Table 1: Summary statistics of dependent, explanatory, and control variables.

sw.sum

# Table 2: Summary statistics presented by household decision-makers using the 
# ACCESS data using survey weights to account for village-clustered sampling scheme.

sw.sum.decision

# Table 4: Summary statistics of reasons for LPG non-adoption presented by household decision-makers.

sw.sum.nonadopters

# Table 5: Mean and standard error are shown for overall satisfaction with the 
# primary household cooking arrangement by gender of the respondent, as well as the 
# distribution of households falling in to each sub-category.

gender_lpg_cookingsatisfaction

# Table A3: Summary statistics of dependent, explanatory, and control variables by study state in
# ACCESS data.

sw.sum.state

# Table A6: Counts of number of reasons for LPG non-adoption cited by respondents by decision-making
# types, as well as a calculation of the mean number of reasons per household.


nlpg_table


####
#### Code 4: Makes descriptive figures
####

source("~/Code/4_GouldUrpelainen_DescriptiveFigures.R")

# Figure 1: For categorical covariates of LPG adoption, the proportion of households with LPG is
# shown along with values in each bar showing the number of participants in each category with
# LPG.

fullbar


# Figure 2: We show scatter plots with continuous covariates on the x-axis and LPG ownership
# status on the y-axis. Marginal histograms show the distribution of the x-axis continuous variable.
# A locally estimated scatterplot smoothing function (LOESS) provides an estimation of the fraction
# of households across the distribution of the continuous covariate owning LPG.

fullscatter

# Figure A3: Distribution of cooking fuel use per capita in ACCESS data.

combined_lpguse_fig


####
#### Code 5: Makes Figure A2 (Map)
####

source("~/Code/5_GouldUrpelainen_ACCESS1Map.R")

# Figure A2: Figure A2: Percent LPG adoption in the study at the state (left) and district (right) level in the
# ACCESS dataset. States and districts in white are outside the study sample. Reproduced with
# permission from the authors (Gould and Urpelainen, 2018).


combined_map


####
#### Code 6: Regressions presented as primary analyses and associated figures
####

source("~/Code/6_GouldUrpelainen_MainRegressions.R")


# ** Important Note** #
# The tables in the manuscript were hand made in Latex, transferring the results from the R console to a Latex file.
# We provide the appropriate columns of results to produce each table below the description.


# Table 3: Quasi-binomial survey-weighted logistic regressions (logit link) of LPG adoption by households
# in the sample reporting average marginal effects using the ACCESS data. Standard errors
# reported in parenthesis are adjusted for the villa3g5e-clustered sampling strategy.

mmod1 # Model 1
mmod2 # Model 2
mmod3 # Model 3
mmod4 # Model 4
mmod5 # Model 5
mmod6 # Model 6


# Table 6: In regressions 1-2, we use quasi-binomial logistic regressions (logit link) to examine the
# association between covariates and households using LPG as their primary cooking fuel. In regressions
# 3-4, we use generalized linear models (Ordinary Least Squares) with the outcome logarithmized
# LPG use (kg/month). In regressions 5-6, we use generalized linear models (Ordinary Least
# squares) with the outcome firewood use (kg/month). Average marginal effects can be interpreted
# as a percentage point change in probability of LPG adoption in regressions 1-2 and a percent change
# in the amount of fuel use when multiplied by 100 in regressions 3-6. Standard errors are adjusted
# for the village-clustered sampling strategy.

pusageme1 # Model 1 (uses LPG as the primary cooking fuel)
pusageme2 # Model 2 (uses LPG as the primary cooking fuel)

usageme1 # Model 3 (LPG kg/month)
usageme2 # Model 4 (LPG kg/month)

fusageme1 # Model 5 (firewood kilograms/month)
fusageme2 # Model 6 (firewood kilograms/month)





####
#### Code 7: Regressions presented as secondary analyses
####

source("~/Code/7_GouldUrpelainen_SupportingRegressions.R")


# Table A1: Non-survey weighted linear regressions (OLS) including village-level dummy variables.
# In total, there are 714 villages included in the linear regressions as dummy variables. Data come
# from ACCESS.

summary(asmod1)
summary(asmod2)
summary(asmod3)
summary(asmod4)
summary(asmod5)
summary(asmod6)




# Table A2: Regression models replicating main analyses using having a woman household head as
# the primary indicator of women’s decision-making power. Results are from quasi-binomial logistic
# regressions (logit link) of LPG adoption by households in the sample reporting average marginal
# effects. Data used come from ACCESS and standard errors are adjusted for the village-clustered
# sampling strategy.

fhhmmod1 # Female HH Head Model 1
fhhmmod2 # Female HH Head Model 2
fhhmmod3 # Female HH Head Model 3
fhhmmod4 # Female HH Head Model 4
fhhmmod5 # Female HH Head Model 5
fhhmmod6 # Female HH Head Model 6

# Table A4: Quasi-binomial logistic regressions (logit link) of LPG adoption by households within
# individual states reporting average marginal effects. Data used come from ACCESS and standard
# errors are adjusted for the village-clustered sampling strategy.


mm6_J # Jharkhand
mm6_M # Madhya Pradesh
mm6_0 # Odisha
mm6_WB # West Bengal
mm6_B # Bihar
mm6_U # Uttar Pradesh




# Table A5: Quasi-binomial logistic regressions (logit link) of reasons for LPG non-adoption among
# households in the sample reporting marginal effects. Standard errors are adjusted for the villageclustered
# sampling strategy. Data are from ACCESS.

nmemod1 # Unavailable Mod 1
nmemod2 # Unavailable Mod 2
nmemod3 # Installation Cost Mod 1
nmemod4 # Installation Cost Mod 2
nmemod5 # Montly Cost Mod 1
nmemod6 # Montly Cost Mod 2
nmemod7 # Lack of Information Mod 1
nmemod8 # Lack of Information Mod 2


# Table A7: Generalized linear models (Ordinary Least Squares) testing association of covariates with
# predicting a household using LPG as their primary cooking fuel after adoption. ACCESS data is
# used and standard errors account for village-clustered sampling strategy.


pusagememod3
pusagememod4
pusagememod6



# Table A8: Generalized linear models (Ordinary Least Squares) testing association of covariates
# with predicting a household LPG use in terms of kilograms per capita per month. ACCESS data
# is used and standard errors account for village-clustered sampling strategy.

lusagemmod1
lusagemmod2
lusagemmod3


# Table A9: Generalized linear models (Ordinary Least Squares) testing association of covariates with
# predicting a household firewood use in terms of kilograms per capita per month among households
# that have LPG. ACCESS data is used and standard errors account for village-clustered sampling
# strategy.

firewood_use_m_mod1
firewood_use_m_mod2
firewood_use_m_mod3


# Table A10: Generalized linear models (Ordinary Least Squares) testing association of fuel access and
# cost characteristics with predicting a household firewood use in terms of kilograms per capita per
# month. ACCESS data is used and standard errors account for village-clustered sampling strategy.

fusagemmod3
fusagemmod4
fusagemmod5
fusagemmod6
fusagemmod7

