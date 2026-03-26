#=====================================================================================================================================#
#                                                                                                                                     #
# Title: "Americans’ Responses to COVID-19 and the Conditional Role of Dispositional Needs for Security: A Replication and Extension" #
# Authors: Adam R. Panish, Trent Ollerenshaw, & Joseph A. Vitriol                                                                     #
# Date: 9/21/2025                                                                                                                     #
#                                                                                                                                     #
############################################# Code for running power analysis simulations ############################################# 
#                                                                                                                                     #
#=====================================================================================================================================#


#================================================================#
#                      Load Packages and Data                    #
#================================================================#

# Load Required Packages
library(readxl)
library(dplyr)
library(ltm)
library(InteractionPoweR)
library(doParallel)

# Set Number of Cores for Parallel Processing, Leaving 1 Free (Used For Power Simulations)
registerDoParallel(cores = detectCores() - 1)

# Load Data
Bovitz2020 <- read_xlsx("Bovitz2020_coded.xlsx")
MTurk2020 <- read_xlsx("MTurk2020_coded.xlsx")
Lucid2020 <- read_xlsx("Lucid2020_coded.xlsx")
Lucid2021 <- read_xlsx("Lucid2021_coded.xlsx")
ANESGSS2020 <- read_xlsx("ANESGSS2020_coded.xlsx")
VSG <- read_xlsx("VSG_coded.xlsx")
pooled_data <- rbind(Bovitz2020, MTurk2020, Lucid2020, Lucid2021, ANESGSS2020, VSG)

# Set Seed
set.seed(12345)


#================================================================#
#                    Figure 1: Power Analyses                    #
#================================================================#

# Part 1: Post Hoc Power Analysis of Ollerenshaw 2022

# Create Standardized Variables 
ANESGSS2020 <- 
  ANESGSS2020 %>% 
  mutate(
    dv_concern_z = (dv_concern - mean(na.omit(dv_concern))) / sd(na.omit(dv_concern)),
    auth_scale_z = (auth_scale - mean(na.omit(auth_scale))) / sd(na.omit(auth_scale)),
    polengage_scale_z = (polengage_scale - mean(na.omit(polengage_scale))) / sd(na.omit(polengage_scale)),
    age_z = (age - mean(na.omit(age))) / sd(na.omit(age)),
    education_z = (education - mean(na.omit(education))) / sd(na.omit(education)),
    income_z = (income - mean(na.omit(income))) / sd(na.omit(income))
  )

Lucid2020 <- 
  Lucid2020 %>% 
  mutate(
    dv_behavior_z = (dv_behavior - mean(na.omit(dv_behavior))) / sd(na.omit(dv_behavior)),
    dv_restrictions_z = (dv_restrictions - mean(na.omit(dv_restrictions))) / sd(na.omit(dv_restrictions)),
    auth_scale_z = (auth_scale - mean(na.omit(auth_scale))) / sd(na.omit(auth_scale)),
    polengage_scale_z = (polengage_scale - mean(na.omit(polengage_scale))) / sd(na.omit(polengage_scale)),
    age_z = (age - mean(na.omit(age))) / sd(na.omit(age)),
    education_z = (education - mean(na.omit(education))) / sd(na.omit(education)),
    income_z = (income - mean(na.omit(income))) / sd(na.omit(income))
  )

Lucid2021 <- 
  Lucid2021 %>% 
  mutate(
    dv_behavior_z = (dv_behavior - mean(na.omit(dv_behavior))) / sd(na.omit(dv_behavior)),
    dv_restrictions_z = (dv_restrictions - mean(na.omit(dv_restrictions))) / sd(na.omit(dv_restrictions)),
    auth_scale_z = (auth_scale - mean(na.omit(auth_scale))) / sd(na.omit(auth_scale)),
    polengage_scale_z = (polengage_scale - mean(na.omit(polengage_scale))) / sd(na.omit(polengage_scale)),
    age_z = (age - mean(na.omit(age))) / sd(na.omit(age)),
    education_z = (education - mean(na.omit(education))) / sd(na.omit(education)),
    income_z = (income - mean(na.omit(income))) / sd(na.omit(income))
  )

## DV: Concern

# ANES-GSS

# Model
ANESGSS.concern.mod <- lm(dv_concern_z ~ auth_scale_z * polengage_scale_z + age_z + factor(male) + factor(race) + education_z + income_z + factor(south), data = ANESGSS2020)

# Sample size
ANESGSS.Con.N <- nobs(ANESGSS.concern.mod)

# Correlations
ANESGSS.Con.r.x1.y <- cor.test(ANESGSS.concern.mod$model$auth_scale, ANESGSS.concern.mod$model$dv_concern)$estimate
ANESGSS.Con.r.x2.y <- cor.test(ANESGSS.concern.mod$model$polengage_scale, ANESGSS.concern.mod$model$dv_concern)$estimate
ANESGSS.Con.r.x1.x2 <- cor.test(ANESGSS.concern.mod$model$auth_scale, ANESGSS.concern.mod$model$polengage_scale)$estimate
ANESGSS.Con.r.x1x2.y <- ANESGSS.concern.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities
ANESGSS.Con.rel.x1 <- cronbach.alpha(na.omit(ANESGSS2020[c("auth1", "auth2", "auth3", "auth4")]))$alpha
ANESGSS.Con.rel.x2 <- cronbach.alpha(na.omit(ANESGSS2020[c("polknow4", "polknow5", "polknow6", "polknow7", "polknow8")]))$alpha
ANESGSS.Con.rel.y <- 0.8


## DV: Restrictions

# Lucid 2020

# Model
Lucid2020.restrictions.mod <- lm(dv_restrictions_z ~ auth_scale_z * polengage_scale_z + age_z + factor(male) + factor(race) + education_z + income_z + factor(south), data = Lucid2020)

# Sample size
Lucid2020.Res.N <- nobs(Lucid2020.restrictions.mod)

# Correlations
Lucid2020.Res.r.x1.y <- cor.test(Lucid2020.restrictions.mod$model$auth_scale, Lucid2020.restrictions.mod$model$dv_restrictions)$estimate
Lucid2020.Res.r.x2.y <- cor.test(Lucid2020.restrictions.mod$model$polengage_scale, Lucid2020.restrictions.mod$model$dv_restrictions)$estimate
Lucid2020.Res.r.x1.x2 <- cor.test(Lucid2020.restrictions.mod$model$auth_scale, Lucid2020.restrictions.mod$model$polengage_scale)$estimate
Lucid2020.Res.r.x1x2.y <- Lucid2020.restrictions.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities
Lucid2020.Res.rel.x1 <- cronbach.alpha(na.omit(Lucid2020[c("auth1", "auth2", "auth3", "auth4", "auth5")]))$alpha
Lucid2020.Res.rel.x2 <- cronbach.alpha(na.omit(Lucid2020[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6")]))$alpha
Lucid2020.Res.rel.y <- cronbach.alpha(na.omit(Lucid2020[c("dv_businesses", "dv_tracking", "dv_lockdowns", "dv_maskmandates")]))$alpha


# Lucid 2021

# Model
Lucid2021.restrictions.mod <- lm(dv_restrictions_z ~ auth_scale_z * polengage_scale_z + age_z + factor(male) + factor(race) + education_z + income_z + factor(south), data = Lucid2021)

# Sample size
Lucid2021.Res.N <- nobs(Lucid2021.restrictions.mod)

# Correlations
Lucid2021.Res.r.x1.y <- cor.test(Lucid2021.restrictions.mod$model$auth_scale, Lucid2021.restrictions.mod$model$dv_restrictions)$estimate
Lucid2021.Res.r.x2.y <- cor.test(Lucid2021.restrictions.mod$model$polengage_scale, Lucid2021.restrictions.mod$model$dv_restrictions)$estimate
Lucid2021.Res.r.x1.x2 <- cor.test(Lucid2021.restrictions.mod$model$auth_scale, Lucid2021.restrictions.mod$model$polengage_scale)$estimate
Lucid2021.Res.r.x1x2.y <- Lucid2020.restrictions.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities
Lucid2021.Res.rel.x1 <- cronbach.alpha(na.omit(Lucid2021[c("auth1", "auth2", "auth3", "auth4", "auth5", "auth6", "auth7", "auth8")]))$alpha
Lucid2021.Res.rel.x2 <- cronbach.alpha(na.omit(Lucid2021[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5")]))$alpha
Lucid2021.Res.rel.y <- cronbach.alpha(na.omit(Lucid2021[c("dv_businesses", "dv_tracking", "dv_lockdowns", "dv_mmindoors", "dv_mmoutdoors", "dv_vaxmandate")]))$alpha


## DV: Behavior

# Lucid 2020

# Model
Lucid2020.behavior.mod <- lm(dv_behavior_z ~ auth_scale_z * polengage_scale_z + age_z + factor(male) + factor(race) + education_z + income_z + factor(south), data = Lucid2020)

# Sample size
Lucid2020.Beh.N <- nobs(Lucid2020.behavior.mod)

# Correlations
Lucid2020.Beh.r.x1.y <- cor.test(Lucid2020.behavior.mod$model$auth_scale, Lucid2020.behavior.mod$model$dv_behavior)$estimate
Lucid2020.Beh.r.x2.y <- cor.test(Lucid2020.behavior.mod$model$polengage_scale, Lucid2020.behavior.mod$model$dv_behavior)$estimate
Lucid2020.Beh.r.x1.x2 <- cor.test(Lucid2020.behavior.mod$model$auth_scale, Lucid2020.behavior.mod$model$polengage_scale)$estimate
Lucid2020.Beh.r.x1x2.y <- Lucid2020.behavior.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities
Lucid2020.Beh.rel.x1 <- cronbach.alpha(na.omit(Lucid2020[c("auth1", "auth2", "auth3", "auth4", "auth5")]))$alpha
Lucid2020.Beh.rel.x2 <- cronbach.alpha(na.omit(Lucid2020[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6")]))$alpha
Lucid2020.Beh.rel.y <- cronbach.alpha(na.omit(Lucid2020[c("dv_distancing", "dv_masking")]))$alpha


# Lucid 2021

# Model
Lucid2021.behavior.mod <- lm(dv_behavior_z ~ auth_scale_z * polengage_scale_z + age_z + factor(male) + factor(race) + education_z + income_z + factor(south), data = Lucid2021)

# Sample size
Lucid2021.Beh.N <- nobs(Lucid2021.behavior.mod)

# Correlations
Lucid2021.Beh.r.x1.y <- cor.test(Lucid2021.behavior.mod$model$auth_scale, Lucid2021.behavior.mod$model$dv_behavior)$estimate
Lucid2021.Beh.r.x2.y <- cor.test(Lucid2021.behavior.mod$model$polengage_scale, Lucid2021.behavior.mod$model$dv_behavior)$estimate
Lucid2021.Beh.r.x1.x2 <- cor.test(Lucid2021.behavior.mod$model$auth_scale, Lucid2021.behavior.mod$model$polengage_scale)$estimate
Lucid2021.Beh.r.x1x2.y <- Lucid2020.behavior.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities
Lucid2021.Beh.rel.x1 <- cronbach.alpha(na.omit(Lucid2021[c("auth1", "auth2", "auth3", "auth4", "auth5", "auth6", "auth7", "auth8")]))$alpha
Lucid2021.Beh.rel.x2 <- cronbach.alpha(na.omit(Lucid2021[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5")]))$alpha
Lucid2021.Beh.rel.y <- cronbach.alpha(na.omit(Lucid2021[c("dv_distancing1", "dv_distancing2", "dv_masking1", "dv_masking2", "dv_vaxstatus")]))$alpha


# Part 2: A Priori Power Analysis of Current Study

## DV: Concern
dv_concern_data <- pooled_data[!is.na(pooled_data$dv_concern) & !is.na(pooled_data$auth_scale) & !is.na(pooled_data$polengage_scale),]
Bovitz_concern <- dv_concern_data[dv_concern_data$sample == "Bovitz",]
MTurk_concern <- dv_concern_data[dv_concern_data$sample == "MTurk",]
ANESGSS_concern <- dv_concern_data[dv_concern_data$sample == "ANESGSS",]
VSG_concern <- dv_concern_data[dv_concern_data$sample == "VSG",]

# Sample size
Rep.Con.N <- nrow(dv_concern_data)

# Correlations (weighted averages)
Rep.Con.r.x1.y <- sum((dv_concern_data %>% group_by(sample) %>% summarize(cor(auth_scale, dv_concern, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Con.N
Rep.Con.r.x2.y <- sum((dv_concern_data %>% group_by(sample) %>% summarize(cor(polengage_scale, dv_concern, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Con.N
Rep.Con.r.x1.x2 <- sum((dv_concern_data %>% group_by(sample) %>% summarize(cor(auth_scale, polengage_scale, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Con.N

# Effect size observed in Ollerenshaw 2022
Rep.Con.r.x1x2.y <- ANESGSS.concern.mod$coefficients["auth_scale_z:polengage_scale_z"] 

# Reliabilities (weighted averages)
Rep.Con.rel.x1 <- ((cronbach.alpha(na.omit(ANESGSS_concern[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(ANESGSS_concern)) +
                     (cronbach.alpha(na.omit(Bovitz_concern[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(Bovitz_concern)) +
                     (cronbach.alpha(na.omit(MTurk_concern[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(MTurk_concern)) +
                     (cronbach.alpha(na.omit(VSG_concern[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(VSG_concern))) / (nrow(ANESGSS_concern) + nrow(Bovitz_concern) + nrow(MTurk_concern) + nrow(VSG_concern))
Rep.Con.rel.x2 <- ((cronbach.alpha(na.omit(ANESGSS_concern[c("polknow4", "polknow5", "polknow6", "polknow7", "polknow8")]))$alpha * nrow(ANESGSS_concern)) +
                     (cronbach.alpha(na.omit(Bovitz_concern[c("polint1", "polint2", "polint3", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6", "polknow7", "polknow8")]))$alpha * nrow(Bovitz_concern)) +
                     (cronbach.alpha(na.omit(MTurk_concern[c("polint1", "polint2", "polint3", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6", "polknow7", "polknow8")]))$alpha * nrow(MTurk_concern)) +
                     (cronbach.alpha(na.omit(VSG_concern[c("polint1", "polint2", "polint3", "polint4", "polint5", "polint6", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6", "polknow7")]))$alpha * nrow(VSG_concern))) / (nrow(ANESGSS_concern) + nrow(Bovitz_concern) + nrow(MTurk_concern) + nrow(VSG_concern))
Rep.Con.rel.y <- 0.8


## DV: Restrictions
dv_restrictions_data <- pooled_data[!is.na(pooled_data$dv_restrictions) & !is.na(pooled_data$auth_scale) & !is.na(pooled_data$polengage_scale),]
Lucid2020_restrict <- dv_restrictions_data[dv_restrictions_data$sample == "Lucid2020",]
Lucid2021_restrict <- dv_restrictions_data[dv_restrictions_data$sample == "Lucid2021",]
VSG_restrict <- dv_restrictions_data[dv_restrictions_data$sample == "VSG",]

# Sample size
Rep.Res.N <- nrow(dv_restrictions_data)

# Correlations (weighted averages)
Rep.Res.r.x1.y <- sum((dv_restrictions_data %>% group_by(sample) %>% summarize(cor(auth_scale, dv_restrictions, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Res.N
Rep.Res.r.x2.y <- sum((dv_restrictions_data %>% group_by(sample) %>% summarize(cor(polengage_scale, dv_restrictions, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Res.N
Rep.Res.r.x1.x2 <- sum((dv_restrictions_data %>% group_by(sample) %>% summarize(cor(auth_scale, polengage_scale, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Res.N

# Effect size observed in Ollerenshaw 2022
Rep.Res.r.x1x2.y <- Lucid2020.restrictions.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities (weighted averages)
Rep.Res.rel.x1 <- ((cronbach.alpha(na.omit(VSG_restrict[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(VSG_restrict)) +
                     (cronbach.alpha(na.omit(Lucid2020_restrict[c("auth1", "auth2", "auth3", "auth4", "auth5")]))$alpha * nrow(Lucid2020_restrict)) +
                     (cronbach.alpha(na.omit(Lucid2021_restrict[c("auth1", "auth2", "auth3", "auth4", "auth5", "auth6", "auth7", "auth8")]))$alpha * nrow(Lucid2021_restrict))) / (nrow(VSG_restrict) + nrow(Lucid2020_restrict) + nrow(Lucid2021_restrict))
Rep.Res.rel.x2 <- ((cronbach.alpha(na.omit(VSG_restrict[c("polint1", "polint2", "polint3", "polint4", "polint5", "polint6", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6", "polknow7")]))$alpha * nrow(VSG_restrict)) +
                     (cronbach.alpha(na.omit(Lucid2020_restrict[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6")]))$alpha * nrow(Lucid2020_restrict)) +
                     (cronbach.alpha(na.omit(Lucid2021_restrict[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5")]))$alpha * nrow(Lucid2021_restrict))) / (nrow(VSG_restrict) + nrow(Lucid2020_restrict) + nrow(Lucid2021_restrict))
Rep.Res.rel.y <- ((cronbach.alpha(na.omit(VSG_restrict[c("dv_businesses", "dv_staterestrict", "dv_cancelevents", "dv_schools", "dv_workhome", "dv_travelrest", "dv_stayhome", "dv_testingreq")]))$alpha * nrow(VSG_restrict)) +
                    (cronbach.alpha(na.omit(Lucid2020_restrict[c("dv_distancing", "dv_masking")]))$alpha * nrow(Lucid2020_restrict)) +
                    (cronbach.alpha(na.omit(Lucid2021_restrict[c("dv_distancing1", "dv_distancing2", "dv_masking1", "dv_masking2", "dv_vaxstatus")]))$alpha * nrow(Lucid2021_restrict))) / (nrow(VSG_restrict) + nrow(Lucid2020_restrict) + nrow(Lucid2021_restrict))


## DV: Behavior
dv_behavior_data <- pooled_data[!is.na(pooled_data$dv_behavior) & !is.na(pooled_data$auth_scale) & !is.na(pooled_data$polengage_scale),]
Bovitz_behav <- dv_behavior_data[dv_behavior_data$sample == "Bovitz",]
MTurk_behav <- dv_behavior_data[dv_behavior_data$sample == "MTurk",]
Lucid2020_behav <- dv_behavior_data[dv_behavior_data$sample == "Lucid2020",]
Lucid2021_behav <- dv_behavior_data[dv_behavior_data$sample == "Lucid2021",]

# Sample size
Rep.Beh.N <- nrow(dv_behavior_data)

# Correlations (weighted averages)
Rep.Beh.r.x1.y <- sum((dv_behavior_data %>% group_by(sample) %>% summarize(cor(auth_scale, dv_behavior, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Beh.N
Rep.Beh.r.x2.y <- sum((dv_behavior_data %>% group_by(sample) %>% summarize(cor(polengage_scale, dv_behavior, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Beh.N
Rep.Beh.r.x1.x2 <- sum((dv_behavior_data %>% group_by(sample) %>% summarize(cor(auth_scale, polengage_scale, use="complete.obs") * length(auth_scale)))[,2]) / Rep.Beh.N

# Effect size observed in Ollerenshaw 2022
Rep.Beh.r.x1x2.y <- Lucid2020.behavior.mod$coefficients["auth_scale_z:polengage_scale_z"]

# Reliabilities (weighted averages)
Rep.Beh.rel.x1 <- ((cronbach.alpha(na.omit(Bovitz_behav[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(Bovitz_behav)) +
                     (cronbach.alpha(na.omit(MTurk_behav[c("auth1", "auth2", "auth3", "auth4")]))$alpha * nrow(MTurk_behav)) +
                     (cronbach.alpha(na.omit(Lucid2020_behav[c("auth1", "auth2", "auth3", "auth4", "auth5")]))$alpha * nrow(Lucid2020_behav)) +
                     (cronbach.alpha(na.omit(Lucid2021_behav[c("auth1", "auth2", "auth3", "auth4", "auth5", "auth6", "auth7", "auth8")]))$alpha * nrow(Lucid2021_behav))) / (nrow(Bovitz_behav) + nrow(MTurk_behav) + nrow(Lucid2020_behav) + nrow(Lucid2021_behav))
Rep.Beh.rel.x2 <- ((cronbach.alpha(na.omit(Bovitz_behav[c("polint1", "polint2", "polint3", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6", "polknow7", "polknow8")]))$alpha * nrow(Bovitz_behav)) +
                     (cronbach.alpha(na.omit(MTurk_behav[c("polint1", "polint2", "polint3", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6", "polknow7", "polknow8")]))$alpha * nrow(MTurk_behav)) +
                     (cronbach.alpha(na.omit(Lucid2020_behav[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5", "polknow6")]))$alpha * nrow(Lucid2020_behav)) +
                     (cronbach.alpha(na.omit(Lucid2021_behav[c("polint1", "polint2", "polknow1", "polknow2", "polknow3", "polknow4", "polknow5")]))$alpha * nrow(Lucid2021_behav))) / (nrow(Bovitz_behav) + nrow(MTurk_behav) + nrow(Lucid2020_behav) + nrow(Lucid2021_behav))
Rep.Beh.rel.y <- ((cronbach.alpha(na.omit(Bovitz_behav[c("dv_travel", "dv_work_home", "dv_wash_hands", "dv_avoid_dine", "dv_cancel_event", "dv_sanitize", "dv_distancing", "dv_masking")]))$alpha * nrow(Bovitz_behav)) +
                    (cronbach.alpha(na.omit(MTurk_behav[c("dv_travel", "dv_work_home", "dv_wash_hands", "dv_avoid_dine", "dv_cancel_event", "dv_sanitize", "dv_distancing", "dv_masking")]))$alpha * nrow(MTurk_behav)) +
                    (cronbach.alpha(na.omit(Lucid2020_behav[c("dv_distancing", "dv_masking")]))$alpha * nrow(Lucid2020_behav)) +
                    (cronbach.alpha(na.omit(Lucid2021_behav[c("dv_distancing1", "dv_distancing2", "dv_masking1", "dv_masking2", "dv_vaxstatus")]))$alpha * nrow(Lucid2021_behav))) / (nrow(Bovitz_behav) + nrow(MTurk_behav) + nrow(Lucid2020_behav) + nrow(Lucid2021_behav))

# Simulating Power

# # Power For Observed Effect Sizes in Ollerenshaw 2022 (Not reported in text)
# ANESGSS.concern.pwr.obs <- power_interaction(n.iter = 1000, alpha = 0.05, N = ANESGSS.Con.N, r.x1.y = ANESGSS.Con.r.x1.y, r.x2.y = ANESGSS.Con.r.x2.y, r.x1.x2 = ANESGSS.Con.r.x1.x2, r.x1x2.y = ANESGSS.Con.r.x1x2.y, rel.x1 = ANESGSS.Con.rel.x1, rel.x2 = ANESGSS.Con.rel.x2, rel.y = ANESGSS.Con.rel.y, k.y = 5)           
# Lucid2020.restrictions.pwr.obs <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2020.Res.N, r.x1.y = Lucid2020.Res.r.x1.y, r.x2.y = Lucid2020.Res.r.x2.y, r.x1.x2 = Lucid2020.Res.r.x1.x2, r.x1x2.y = Lucid2020.Res.r.x1x2.y, rel.x1 = Lucid2020.Res.rel.x1, rel.x2 = Lucid2020.Res.rel.x2, rel.y = Lucid2020.Res.rel.y, k.y = 5)           
# Lucid2021.restrictions.pwr.obs <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2021.Res.N, r.x1.y = Lucid2021.Res.r.x1.y, r.x2.y = Lucid2021.Res.r.x2.y, r.x1.x2 = Lucid2021.Res.r.x1.x2, r.x1x2.y = Lucid2021.Res.r.x1x2.y, rel.x1 = Lucid2021.Res.rel.x1, rel.x2 = Lucid2021.Res.rel.x2, rel.y = Lucid2021.Res.rel.y, k.y = 5)           
# Lucid2020.behavior.pwr.obs <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2020.Beh.N, r.x1.y = Lucid2020.Beh.r.x1.y, r.x2.y = Lucid2020.Beh.r.x2.y, r.x1.x2 = Lucid2020.Beh.r.x1.x2, r.x1x2.y = Lucid2020.Beh.r.x1x2.y, rel.x1 = Lucid2020.Beh.rel.x1, rel.x2 = Lucid2020.Beh.rel.x2, rel.y = Lucid2020.Beh.rel.y, k.y = 5)           
# Lucid2021.behavior.pwr.obs <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2021.Beh.N, r.x1.y = Lucid2021.Beh.r.x1.y, r.x2.y = Lucid2021.Beh.r.x2.y, r.x1.x2 = Lucid2021.Beh.r.x1.x2, r.x1x2.y = Lucid2021.Beh.r.x1x2.y, rel.x1 = Lucid2021.Beh.rel.x1, rel.x2 = Lucid2021.Beh.rel.x2, rel.y = Lucid2021.Beh.rel.y, k.y = 5)           

# Power Across Range of Effect Sizes (Plotted in Figure 1)
ANESGSS.concern.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = ANESGSS.Con.N, r.x1.y = ANESGSS.Con.r.x1.y, r.x2.y = ANESGSS.Con.r.x2.y, r.x1.x2 = ANESGSS.Con.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = ANESGSS.Con.rel.x1, rel.x2 = ANESGSS.Con.rel.x2, rel.y = ANESGSS.Con.rel.y, k.y = 5)           
Lucid2020.restrictions.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2020.Res.N, r.x1.y = Lucid2020.Res.r.x1.y, r.x2.y = Lucid2020.Res.r.x2.y, r.x1.x2 = Lucid2020.Res.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Lucid2020.Res.rel.x1, rel.x2 = Lucid2020.Res.rel.x2, rel.y = Lucid2020.Res.rel.y, k.y = 5)           
Lucid2021.restrictions.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2021.Res.N, r.x1.y = Lucid2021.Res.r.x1.y, r.x2.y = Lucid2021.Res.r.x2.y, r.x1.x2 = Lucid2021.Res.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Lucid2021.Res.rel.x1, rel.x2 = Lucid2021.Res.rel.x2, rel.y = Lucid2021.Res.rel.y, k.y = 5)           
Lucid2020.behavior.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2020.Beh.N, r.x1.y = Lucid2020.Beh.r.x1.y, r.x2.y = Lucid2020.Beh.r.x2.y, r.x1.x2 = Lucid2020.Beh.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Lucid2020.Beh.rel.x1, rel.x2 = Lucid2020.Beh.rel.x2, rel.y = Lucid2020.Beh.rel.y, k.y = 5)           
Lucid2021.behavior.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Lucid2021.Beh.N, r.x1.y = Lucid2021.Beh.r.x1.y, r.x2.y = Lucid2021.Beh.r.x2.y, r.x1.x2 = Lucid2021.Beh.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Lucid2021.Beh.rel.x1, rel.x2 = Lucid2021.Beh.rel.x2, rel.y = Lucid2021.Beh.rel.y, k.y = 5)           
replication.concern.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Rep.Con.N, r.x1.y = Rep.Con.r.x1.y, r.x2.y = Rep.Con.r.x2.y, r.x1.x2 = Rep.Con.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Rep.Con.rel.x1, rel.x2 = Rep.Con.rel.x2, rel.y = Rep.Con.rel.y, k.y = 5)           
replication.restrictions.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Rep.Res.N, r.x1.y = Rep.Res.r.x1.y, r.x2.y = Rep.Res.r.x2.y, r.x1.x2 = Rep.Res.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Rep.Res.rel.x1, rel.x2 = Rep.Res.rel.x2, rel.y = Rep.Res.rel.y, k.y = 5)           
replication.behavior.pwr.seq <- power_interaction(n.iter = 1000, alpha = 0.05, N = Rep.Beh.N, r.x1.y = Rep.Beh.r.x1.y, r.x2.y = Rep.Beh.r.x2.y, r.x1.x2 = Rep.Beh.r.x1.x2, r.x1x2.y = seq(-.2, -.01, by = .01), rel.x1 = Rep.Beh.rel.x1, rel.x2 = Rep.Beh.rel.x2, rel.y = Rep.Beh.rel.y, k.y = 5)           

# Assemble Results for Plotting
fig1.data <- 
  data.frame(
    sample = factor(c(rep("Ollerenshaw (2022): 2020 Samples", 20), 
                      rep(c(rep("Ollerenshaw (2022): 2020 Samples", 20), 
                            rep("Ollerenshaw (2022): 2021 Samples", 20)), 2), 
                      rep("Current Study", 60)),
                    levels = c("Ollerenshaw (2022): 2020 Samples",
                               "Ollerenshaw (2022): 2021 Samples", 
                               "Current Study")),
    dv = factor(c(rep("DV: Concern", 20), 
                  rep("DV: Behavior", 40),
                  rep("DV: Restrictions", 40),
                  rep("DV: Concern", 20), 
                  rep("DV: Behavior", 20),
                  rep("DV: Restrictions", 20)),
                levels = c("DV: Concern",
                           "DV: Restrictions", 
                           "DV: Behavior")),
    `Effect Size` = c(ANESGSS.concern.pwr.seq$r.x1x2.y,
                      Lucid2020.behavior.pwr.seq$r.x1x2.y,
                      Lucid2021.behavior.pwr.seq$r.x1x2.y,
                      Lucid2020.restrictions.pwr.seq$r.x1x2.y,
                      Lucid2021.restrictions.pwr.seq$r.x1x2.y,
                      replication.concern.pwr.seq$r.x1x2.y,
                      replication.behavior.pwr.seq$r.x1x2.y,
                      replication.restrictions.pwr.seq$r.x1x2.y) * -1,
    Power = c(ANESGSS.concern.pwr.seq$pwr,
              Lucid2020.behavior.pwr.seq$pwr,
              Lucid2021.behavior.pwr.seq$pwr,
              Lucid2020.restrictions.pwr.seq$pwr,
              Lucid2021.restrictions.pwr.seq$pwr,
              replication.concern.pwr.seq$pwr,
              replication.behavior.pwr.seq$pwr,
              replication.restrictions.pwr.seq$pwr),
    check.names = F
  )

save(fig1.data, file = "simulation results.RData")
