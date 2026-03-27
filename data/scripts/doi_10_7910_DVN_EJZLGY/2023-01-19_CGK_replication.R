# This file contains replication material for 
# Kern, F. G., Gleditsch, K. S., & Cordell, R. (2023). 
# Judiciary institutions and violent crime in American Indian nations. 
# Governance, 1–23. https://doi.org/10.1111/gove.12756
# 
# ksg@essex.ac.uk
#
# This version: 2023-01-19

# Set working directory here
setwd("XXX")


# Read data
ndat <- read.table("CGK_data.txt",
                   header=T, sep="\t", quote="")

# Regressions reported in Table 1 

# Base Model 1
base1 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + log(median.inc),
    data = ndat)
summary(base1)

# Model 2
base2 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate,
    data = ndat)
summary(base2)

# Model 3
base3 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate,
    data = ndat, subset=crime.pc<0.10)
summary(base3)

# Model 4 institutions 
#b3_6 =	Does your tribal justice system handle Domestic violence protective
#orders? This variable is binary: Yes or no.
inst1 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate +
    I(b3_6=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst1)

# Model 5 institutions
# b3_1 =	Does your tribal justice system handle Criminal misdemeanor
#cases? This variable is binary: Yes or no.
inst2 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate +
    I(b3_1=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst2)

# Model 6 institutions
#b3_2 =	Does your tribal justice system handle Traffic violations? This
#variable is binary: Yes or no.
inst3 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate +
    I(b3_2=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst3)


# Model 7 institutions
#b4_6 =	Does your tribal justice system have a separate Peacemaking/circle
#sentencing? This variable is binary: Yes or no.
inst4 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate +
    I(b4_6=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst4)

# Model 8 PL280
#b3_2 =	Does your tribal justice system handle Traffic violations? This
#variable is binary: Yes or no.
inst5 <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate +
    I(pl280==1), data = ndat, subset = crime.pc <0.10)
summary(inst5)



library(stargazer)
stargazer(base1, base2, base3,
    inst1,inst2,inst3,inst4,inst5,
    dep.var.labels = "Violent crime per capita",
    covariate.labels = c("Area, ln","Total population",
        "Poverty rate","Median income, ln",
        "Jud. sys: Domestic violence protective orders",
        "Jud. sys: Criminal misdemeanors",
        "Jud. sys: Traffic offenses",
        "Trad.methods or peacemaking/circle sentencing",
        "PL280","Constant"),
      type = "text")
# Or use something like below for latex output
#    type = "latex",
#    out = "table1.tex")


# Robustness tests

# Flagg units in Oklahoma
ndat$okt <- 0
ndat$okt[ndat$state=="Oklahoma"] <- 1


# Base 1
base1.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + log(median.inc) + okt,
            data = ndat)
summary(base1.okt)

# Base 2
base2.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt,
            data = ndat)
summary(base2.okt)

# Base 3
base3.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt,
            data = ndat, subset=crime.pc<0.10)
summary(base3.okt)

# Inst 1
inst1.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt
                + I(b3_6=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst1.okt)

# Inst 2
inst2.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt
                + I(b3_1=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst2.okt)

# Inst 3
inst3.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt
                + I(b3_2=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst3.okt)

# Inst 4
inst4.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt
                + I(b4_6=="Yes"), data = ndat, subset = crime.pc <0.10)
summary(inst4.okt)

# PL280
inst5.okt <- lm(crime.pc ~ log(area/1000) + log(totpop) + pov.rate + okt
                + I(pl280==1), data = ndat, subset = crime.pc <0.10)
summary(inst5.okt)


# Please note that for versions Starting with R 4.2.0, stargazer outputs an error if model 
# names are too long
# Quick fix code below will allow these to print correctly, adapted from
# https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53 


## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# Unload stargazer if loaded
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")

library(stargazer)
stargazer(base1.okt, base2.okt, base3.okt,
  inst1.okt, inst2.okt, inst3.okt, inst4.okt, inst5.okt,
          dep.var.labels = "Violent crime per capita",
          covariate.labels = c("Area, ln",
                               "Total population",
                               "Poverty rate",
                               "Median income, ln",
                               "Oklahoma",
                               "Jud. sys: Domestic violence protective orders",
                               "Jud. sys: Criminal misdemeanors",
                               "Jud. sys: Traffic offenses",
                               "Trad.methods or peacemaking/circle sentencing",
                               "PL280","Constant"),
          type = "text")
#          type = "latex",
#          out = "table2.text")
#)




