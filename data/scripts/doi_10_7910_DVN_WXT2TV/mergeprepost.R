library(tidyverse)
library(foreign)
library(survey)
library(readstata13)

load("Data/Cleaned/preelection.Rda")
dat$rid_pre <- dat$rid_pre_pre

pre <- dat

# Get targets from ACS
acs <- read.csv("Data/Raw/acs_2019_5yr.csv")
acs$age_pre <- as.numeric(acs$age)
acs$gender_pre <- as.factor(acs$sex)
levels(acs$gender_pre) <- c("male","female")
acs$gender_pre <- relevel(acs$gender_pre,ref="female")
acs$education_pre <- as.factor(acs$educ)
levels(acs$education_pre) <- c("No college","No college","College","Graduate","No college","No college","No college","No college","No college","No college","No college")
acs$race_pre <- as.factor(acs$race)
levels(acs$race_pre) <- c("Other","Black","Asian","Asian","Asian","Other","Other","Other","White")
acs$race_pre <- factor(acs$race_pre,levels=c("Other","Black","White","Asian"))
acs$hispanic_pre <- as.factor(acs$hispan)
levels(acs$hispanic_pre) <- c("hispanic","hispanic","not hispanic","hispanic","hispanic")
acs$hispanic_pre <- factor(acs$hispanic_pre,levels=c("not hispanic","hispanic"))
acs$agecat_pre <- NA
acs$agecat_pre[acs$age_pre>=18 & acs$age_pre<=44] <- "18-44"
acs$agecat_pre[acs$age_pre>=45 & acs$age_pre<=64] <- "45-64"
acs$agecat_pre[acs$age_pre>=65] <- "65+"
acs$agecat_pre <- as.factor(acs$agecat_pre)

# Remove rows with missing values
acs <- acs[!is.na(acs$agecat_pre) & !is.na(acs$gender_pre) & !is.na(acs$education_pre) & !is.na(acs$race_pre) & !is.na(acs$hispanic_pre),]

# Weighting
acs_dwt <- svydesign(ids = ~1, weights = ~perwt, 
                     data = acs)

create_targets <- function (target_design, target_formula) {
  target_mf <- model.frame(target_formula, model.frame(target_design))
  target_mm <- model.matrix(target_formula, target_mf)
  wts <- weights(target_design)
  colSums(target_mm * wts) / sum(wts) # returns vector of targets
}

target_formula <- ~ agecat_pre + gender_pre + education_pre + race_pre + hispanic_pre

(targets <- create_targets(acs_dwt, target_formula))

# Check that they've answered at least one of demographics and one of self-placements in next block
pre <- pre[!is.na(pre$agecat_pre) & !is.na(pre$gender_pre) & !is.na(pre$race_pre) & !is.na(pre$hispanic_pre) & !is.na(pre$education_pre),]
pre <- pre[!is.na(pre$workpermit_pre) | !is.na(pre$highincomes_pre) | !is.na(pre$blackswork_pre) | !is.na(pre$healthinsurance_pre) | !is.na(pre$abortionlast3_pre) | !is.na(pre$abortionfirst3_pre) | !is.na(pre$deportillegals_pre) | !is.na(pre$roevwade_pre) | !is.na(pre$whiteadvantage_pre) | !is.na(pre$affirmativeaction_pre),]
# If a particular demographic is still missing, impute modal value

# Recode education and race
levels(pre$education_pre) <- c("No college","No college","No college","No college","College","Graduate","Graduate","Graduate")
levels(pre$race_pre) <- c("Other", "Asian", "Black", "Other", "Other", 
                          "White")
pre$race_pre <- factor(pre$race_pre,levels=c("Other","Black","White","Asian"))

# recode agecat
levels(pre$agecat_pre) <- c("18-44","18-44","45-64","65+")

# For race, replace missing values by modal combination of racial identifications
pre$agecat_pre[is.na(pre$agecat_pre)] <- "18-44"
pre$gender_pre[is.na(pre$gender_pre)] <- "female"
pre$race_pre[is.na(pre$race_pre)] <- "White"
pre$hispanic_pre[is.na(pre$hispanic_pre)] <- "not hispanic"
pre$education_pre[is.na(pre$education_pre)] <- "No college"

pre_dwt <- svydesign(ids = ~1, weights = ~1, 
                     data = pre)

pre_w <- calibrate(design = pre_dwt,
                 formula= target_formula,
                 population = targets,
                 calfun="raking")

pre$weights_pre <- weights(pre_w)

# Post-election 
load("Data/Cleaned/postelection.Rda")
post <- dat
post$rid_pre <- post$rid_pre_post

dat <- right_join(pre,post,by="rid_pre")

# Post-election weighting
post_dwt <- svydesign(ids = ~1, weights = ~weights_pre, data = dat)

post_w <- calibrate(design = post_dwt,
                 formula= target_formula,
                 population = targets,
                 calfun="raking")

dat$weights_post <- weights(post_w)

save(dat,file="Data/Cleaned/panel.Rda")
write.dta(dat,file="Data/Cleaned/panel.dta")
