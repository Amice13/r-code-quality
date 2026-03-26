##  All options are on the table? Appendix  ##
##  Rotem Dvir  ##
##  December 2020  ##
## R version 4.0.2 (June 2020) ##

# Set Working Directory
setwd("~/Dropbox/TAMU/Diss./Theory/Choice_set//RnR/Dataverse")

# Packages
library(foreign)
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(devtools)
library(dplyr)
library(ggpubr)

# Set Randomizer
set.seed(2020)

# Upload master data file
MyData <- read.csv("ChoiceSetData_June2019.csv", header=TRUE, sep = ",", strip.white = T, na.strings = "")


### Power analysis
library(pwr)

# Compute sample size: ANOVA with different effect sizes (Table 1 results)
## Power = 0.8; Number of groups = 10
### The test provides the suggested sample size (n) PER GROUP (must be multiplied by 10 for required full sample)
pwr.anova.test(k=10, f=.1, sig.level=.05, power=.8)
pwr.anova.test(k=10, f=.25, sig.level=.05, power=.8)
pwr.anova.test(k=10, f=.4, sig.level=.05, power=.8)

## Power = 0.9; Number of groups = 10
pwr.anova.test(k=10, f=.1, sig.level=.05, power=.9)
pwr.anova.test(k=10, f=.25, sig.level=.05, power=.9)
pwr.anova.test(k=10, f=.4, sig.level=.05, power=.9)


# Figure 2: plots of effect and sample size versus power
es <- seq(.1, .5, .01)
nes <- length(es)

# Compute values: 10 groups; power = 0.8
samsize <- NULL
for (i in 1:nes){
  result <- pwr.anova.test(k=10, f=es[i], sig.level=.05, power=.8)
  samsize[i] <- ceiling(result$n)
}

# Panel 1: Power = 0.8
plot(samsize,es, type="l", lwd=2, col="red",
     ylab="Effect Size",
     xlab="Sample Size (per cell)",
     main="One Way ANOVA: Power=.80 and Alpha=.05")

# Compute values: 10 groups; power = 0.9
samsize <- NULL
for (i in 1:nes){
  result <- pwr.anova.test(k=10, f=es[i], sig.level=.05, power=.9)
  samsize[i] <- ceiling(result$n)
}

# Panel 2: Power = 0.9
plot(samsize,es, type="l", lwd=2, col="blue",
     ylab="Effect Size",
     xlab="Sample Size (per cell)",
     main="One Way ANOVA: Power=.90 and Alpha=.05")


# Summary stats of sample
## Appendix B.3.
library(gtsummary)

## Filter dataset to sample variables
samp.data <- MyData %>%
  select(Gender, Age, party, FP_Know, Edu_cat)

## Generate summary stats (results were inserted into table 2)
tbl_summary(samp.data,
            statistic = list(all_continuous() ~ "{mean} ({median})",
                             all_categorical() ~ "{p}% ({n})"))  

# Computing mean, median for categorical variables (results were inserted into table 2)
samp.mean <- samp.data %>%
  summarise(
    party.mean = mean(party),
    party.median = median(party),
    fp.mean = mean(FP_Know),
    fp.median = median(FP_Know),
    edu.mean = mean(Edu_cat, na.rm = T),
    edu.median = median(Edu_cat, na.rm = T))

samp.mean


