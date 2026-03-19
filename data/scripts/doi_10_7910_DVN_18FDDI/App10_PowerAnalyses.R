################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 10 - Power Calculations
################################################################################

################################ SET UP #######################################


rm(list = ls())

#if(!require(devtools)) install.packages("devtools")
library(devtools)
#devtools::install_github("m-freitag/cjpowR")
library(cjpowR)

############################ Calculations #####################################

df_80_1 <- cjpowr_amce(amce = 0.05, power = 0.8, levels = 2, alpha=0.1)

df_80_05 <- cjpowr_amce(amce = 0.05, power = 0.8, levels = 2, alpha=0.05)

df_68_1 <- cjpowr_amce(amce = 0.05, power = 0.68, levels = 2, alpha=0.1)
df_70_1 <- cjpowr_amce(amce = 0.05, power = 0.70, levels = 2, alpha=0.1)



#the MES can be divided by the number of profiles (2 candidates) times
# the number of tasks (5 tasks per person)

#BUT in the case of conditional AMCES - sample size needed for testing can be
# calculated by multiplying this value by 1 / P(W = 0) where P(W = 0) is the 
#known or estimated marginal distribution of the pre-treatment covariate
# of interest

# Note that marginal distributions are calculated in App10_SampleSizes.R

# Table A10.1 - Minimum Effective Sample Size by Confidence Interval and Power Levels

#safety
(df_80_1$n/(2*5)) * (1/0.44) #559 for p < 0.1
(df_80_05$n/(2*5)) * (1/0.44) #710 for p < 0.05

#victimization
(df_80_1$n/(2*5)) * (1/0.17) # 1451 for p < 0.1
(df_80_05$n/(2*5)) * (1/0.17) #1842 for p < 0.05
#####extra check for victims bc of high sample size required
(df_68_1$n/(2*5)) * (1/0.17)  #1047 for power of 
(df_70_1$n/(2*5)) * (1/0.17)  #1104 for power of 

#assist effect
(df_80_1$n/(2*5)) * (1/0.34) #726 for p < 0.1
(df_80_05$n/(2*5)) * (1/0.34) #921 for p < 0.05

#gang
(df_80_1$n/(2*5)) * (1/0.82) #301 for p < 0.1
(df_80_05$n/(2*5)) * (1/0.82) #382 for p < 0.05


