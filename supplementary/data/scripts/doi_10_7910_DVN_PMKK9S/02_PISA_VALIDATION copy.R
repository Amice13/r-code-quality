# R Project
#
# Janine Campbell
# Contact: janineinchile@gmail.com
#
# Date last modified: 05 MAR 2019
#
###############################################################################
# VALIDATION OF MACRO FINDINGS - CORRELATIONS AND T-TESTS
###############################################################################
#
#------------------------------------------------------------------------------
# Remove all objects surrently stored in active memory
#------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
# rm means remove, and it can also be used for an individual condition/variable
#------------------------------------------------------------------------------
# Check / set working directory
#------------------------------------------------------------------------------
getwd()
# Check that working directory is correct.  To change use following code:
setwd("/Users/janine/OneDrive/Study OTAGO/DATA") 
options(prompt="R> ")
#------------------------------------------------------------------------------
# Save Script with an original name 
#------------------------------------------------------------------------------
#
#    *****Manually save script*****
#
#------------------------------------------------------------------------------
# Activate packages for session
#------------------------------------------------------------------------------
library(readr)
library(venn)
library(QCA)
library(SetMethods)
library(devtools)
library(plyr)
library(ggplot2)
library(ggthemes)
library(sjPlot)
library(sjmisc)
library(jtools)
library(interactions)
library("Hmisc")
#
options("jtools-digits" = 4)
options(scipen = 999)
#
#------------------------------------------------------------------------------
# Import csv file
#------------------------------------------------------------------------------
MACROVAL <- read.csv("MACRO_VALIDATION.csv", row.names = 1)
head(MACROVAL)
#
#------------------------------------------------------------------------------
# Analysis of correlations
#------------------------------------------------------------------------------
#
corr <- rcorr(as.matrix(MACROVAL[,2:10]))
corr
#
#------------------------------------------------------------------------------
# Analysis of t-tests by individual subjects compared to aggregated PISASCORE
#------------------------------------------------------------------------------
#
# MATH
t.test(MATH ~ HIINCEQ, data = MACROVAL, var.equal = TRUE) # 447 / 496 / SIGNIFICANT
t.test(MATH ~ HIGENEQ, data = MACROVAL, var.equal = TRUE) # 434 / 502 / SIGNIFICANT
t.test(MATH ~ HIHDI, data = MACROVAL, var.equal = TRUE) # 446 / 506 / SIGNIFICANT
t.test(MATH ~ HIINDIV, data = MACROVAL, var.equal = TRUE) # 445 / 496 / SIGNIFICANT
#
# READ
t.test(READ ~ HIINCEQ, data = MACROVAL, var.equal = TRUE) # 456 / 495 / SIGNIFICANT
t.test(READ ~ HIGENEQ, data = MACROVAL, var.equal = TRUE) # 443 / 502 / SIGNIFICANT
t.test(READ ~ HIHDI, data = MACROVAL, var.equal = TRUE) # 453 / 505 / SIGNIFICANT
t.test(READ ~ HIINDIV, data = MACROVAL, var.equal = TRUE) # 451 / 497 / SIGNIFICANT
#
# SCIE
t.test(SCIE ~ HIINCEQ, data = MACROVAL, var.equal = TRUE) # 456 / 498 / SIGNIFICANT
t.test(SCIE ~ HIGENEQ, data = MACROVAL, var.equal = TRUE) # 444 / 504 / SIGNIFICANT
t.test(SCIE ~ HIHDI, data = MACROVAL, var.equal = TRUE) # 454 / 508 / SIGNIFICANT
t.test(SCIE ~ HIINDIV, data = MACROVAL, var.equal = TRUE) # 453 / 499 / SIGNIFICANT
#
# PISASCORE (3 tests aggregated)
t.test(PISASCORE ~ HIINCEQ, data = MACROVAL, var.equal = TRUE) # 453 / 496 / SIGNIFICANT
t.test(PISASCORE ~ HIGENEQ, data = MACROVAL, var.equal = TRUE) # 440 / 503 / SIGNIFICANT
t.test(PISASCORE ~ HIHDI, data = MACROVAL, var.equal = TRUE) # 451 / 506 / SIGNIFICANT
t.test(PISASCORE ~ HIINDIV, data = MACROVAL, var.equal = TRUE) # 451 / 506 / SIGNIFICANT
#
#------------------------------------------------------------------------------
# Save Script
#------------------------------------------------------------------------------
#
#    *****Manually save script*****
#
#------------------------------------------------------------------------------
# Quit R
#------------------------------------------------------------------------------
q()


