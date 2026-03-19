
### package installation (if needed)

#install.packages('tidyverse')
#install.packages('devtools')
#install.packages('gridExtra')

#library(devtools)
#devtools::install_github("https://github.com/rgiordan/zaminfluence/",ref="master",subdir="zaminfluence",force=TRUE)

### call packages 
library(tidyverse)
library(haven)
#library(GridExtra)
library(zaminfluence)
library(foreign)

#Change setwd below to temp folder
setwd("XXX")

ZAM_sample <- read_dta("ZAM_sample_in.dta")

ZAM_sample$ctry.f <- factor(ZAM_sample$countrycode)
is.factor(ZAM_sample$ctry.f)

ZAM_sample$year.f <- factor(ZAM_sample$year)
is.factor(ZAM_sample$year.f)

reg <-lm(lngdp14 ~ lndn13 + fiw + fiw2 + lndn13_fiw + ctry.f + year.f, data=ZAM_sample, x=TRUE, y=TRUE)
summary(reg)

# Get influence scores for main regressor.
model_grads <-
  ComputeModelInfluence(reg) %>%
  AppendTargetRegressorInfluence("lndn13_fiw")

# Compute the changes needed to change sign, significance, and both
signals <- GetInferenceSignals(model_grads)

# Create dummies for observations that change sign, significance or both
ZAM_sample$out_zam_sign <- 0
for(i in signals[["lndn13_fiw"]][["sign"]][["apip"]][["inds"]]){
  ZAM_sample[i,14] <- 1
}

ZAM_sample$out_zam_sig <- 0
for(i in signals[["lndn13_fiw"]][["sig"]][["apip"]][["inds"]]){
  ZAM_sample[i,15] <- 1
}

ZAM_sample$out_zam_both <- 0
for(i in signals[["lndn13_fiw"]][["both"]][["apip"]][["inds"]]){
  ZAM_sample[i,16] <- 1
}

# Export file to Stata
write.dta(ZAM_sample, "ZAM_sample_out.dta")

