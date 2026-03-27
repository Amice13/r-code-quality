###############################################################################
## SCRIPT ID:
## Blame Shifting in Presidential Systems: Ministerial Terminations' Corrective Effect on Approval
## Table 1: IV estimates of dismissals of tainted ministers on presidential approval

## Original code: February, March, and August 2023
## R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"

## Author: Bastián González-Bustamante
## ORCID: 0000-0003-1510-6820
## Website: https://bgonzalezbustamante.com

## DPhil/PhD Project at the University of Oxford
## Critical Events and Ministerial Turnover in Latin American Presidential Democracies

## Current positions of the author:
## Postdoctoral Researcher and Lecturer at Leiden University
## Lecturer at the Universidad Diego Portales

## Deposited code: April and August 2025
## The data set and replication code are deposited on:
## Public Opinion Quarterly (POQ) Harvard Dataverse
## R version 4.5.0 (2025-04-11) -- "How About a Twenty-Six"
###############################################################################

###############################################################################
## 1. Clean environment
###############################################################################

rm(list = ls())

###############################################################################
## 2. Installation notes (if needed)
###############################################################################

## It is relevant to use ivreg v0.6-1 and performance v0.10.2 
## You can install from source as follows:
## install.packages("ivreg_0.6-1.tar.gz", repos = NULL, type = "source")
## install.packages("performance_0.10.2.tar.gz", repos = NULL, type = "source")

###############################################################################
## 3. Load packages
###############################################################################

library(tidyverse)
library(prais)
library(fmsb)
library(performance)
library(stargazer)
library(ivreg)
library(lmtest)

###############################################################################
## 4. Import data
###############################################################################

## Data
merged_data <- read.csv("blame_shifting_dataset.csv",
                        sep = ",",
                        encoding = "UTF-8")

## Log transform inflation and handle NaNs
merged_data$inflation <- log(merged_data$inflation)
merged_data$inflation[is.nan(merged_data$inflation)] <- NA

###############################################################################
## 5. Prepare data for Prais-Winsten
###############################################################################

prais_data <- merged_data %>%
  select(
    approval, lagged_approval, lagged_dism_tainted, lagged_rc, visibility,
    allhouse, coalition, reelect_last_year, govfrac, gdp, inflation,
    honeymoon_1q, honeymoon_2q, honeymoon_3q, preelection_1q, preelection_2q,
    country, qtr
  ) %>%
  na.omit()

###############################################################################
## 6. Prais-Winsten estimation
###############################################################################

## Prais-Winsten regression
pw <- prais_winsten(
  approval ~ lagged_dism_tainted + lagged_rc + I(lagged_rc^2) + visibility +
    allhouse + coalition + reelect_last_year + govfrac + gdp + inflation +
    honeymoon_2q + honeymoon_3q + preelection_1q + preelection_2q,
  data  = prais_data,
  index = c("country", "qtr")
)

## Extract the rho
rho <- (1 - (pw$rho[nrow(pw$rho) - 1])^2)^0.5

## Transform variables by rho
transf_data <- merged_data
vars_to_transform <- c(
  "approval", "lagged_rc", "visibility", "allhouse", "coalition",
  "reelect_last_year", "govfrac", "gdp", "inflation", "honeymoon_1q",
  "honeymoon_2q", "honeymoon_3q", "preelection_1q", "preelection_2q",
  "lagged_dism_tainted", "lagged_nonpartisan", "lagged_age"
)

for (v in vars_to_transform) {
  transf_data[[v]] <- transf_data[[v]] * rho
}

## Save data
write.csv(transf_data, "transf_data.csv", 
          fileEncoding = "UTF-8", 
          row.names = FALSE)

###############################################################################
## 7. Estimate IV models
###############################################################################

## Model 1
iv1 <- ivreg(
  approval ~ I(lagged_rc) + I(lagged_rc^2) + visibility + allhouse +
    coalition + reelect_last_year + govfrac + gdp + inflation +
    honeymoon_2q + honeymoon_3q + preelection_1q + preelection_2q
  | lagged_dism_tainted
  | lagged_nonpartisan + poly(lagged_age, 2),
  data = subset(transf_data, !is.na(lagged_age))
)

## Model 2
iv2 <- ivreg(
  approval ~ I(lagged_rc) + I(lagged_rc^2) + visibility + allhouse +
    coalition + reelect_last_year + govfrac + gdp + inflation +
    honeymoon_1q + honeymoon_2q + honeymoon_3q + preelection_1q +
    preelection_2q
  | lagged_dism_tainted * I(lagged_rc^2)
  | lagged_nonpartisan + poly(lagged_age, 2),
  data = subset(transf_data, !is.na(lagged_age))
)

## Model 3
iv3 <- ivreg(
  approval ~ I(lagged_rc) + I(lagged_rc^2) + visibility + allhouse +
    coalition + reelect_last_year + govfrac + gdp + inflation +
    honeymoon_1q + honeymoon_2q + honeymoon_3q + preelection_1q +
    preelection_2q
  | lagged_dism_tainted * visibility
  | lagged_nonpartisan + poly(lagged_age, 2),
  data = subset(transf_data, !is.na(lagged_age))
)

## Model 4
iv4 <- ivreg(
  approval ~ I(lagged_rc) + I(lagged_rc^2) + visibility + allhouse +
    coalition + reelect_last_year + govfrac + gdp + inflation +
    honeymoon_1q + honeymoon_2q + honeymoon_3q + preelection_1q +
    preelection_2q
  | lagged_dism_tainted * coalition
  | lagged_nonpartisan + poly(lagged_age, 2),
  data = subset(transf_data, !is.na(lagged_age))
)

###############################################################################
## 8. Generate models table
###############################################################################

stargazer(
  iv1, iv2, iv3, iv4,
  type            = "html",
  header          = FALSE,
  style           = "ajps",
  out             = "table_1.html",
  title           = "IV estimates of dismissals of tainted ministers on presidential approval",
  model.names     = FALSE,
  dep.var.labels  = c("Smoothed Approval"),
  se = list(
    coeftest(iv1, vcov = vcovHC(iv1, type = "HC1"))[, 2],
    coeftest(iv2, vcov = vcovHC(iv2, type = "HC1"))[, 2],
    coeftest(iv3, vcov = vcovHC(iv3, type = "HC1"))[, 2],
    coeftest(iv4, vcov = vcovHC(iv4, type = "HC1"))[, 2]
  ),
  notes.align     = "c",
  model.numbers   = FALSE,
  omit.stat       = c("f", "ser", "rsq", "adj.rsq"),
  column.labels   = c("2SLS", "2SLS", "2SLS", "2SLS"),
  omit            = c(
    "allhouse", "reelect_last_year", "govfrac", "gdp", "inflation",
    "honeymoon_1q", "honeymoon_2q", "honeymoon_3q",
    "preelection_1q", "preelection_2q"
  ),
  add.lines = list(
    c("Robust SE", "Yes", "Yes", "Yes", "Yes"),
    c("Presidential leverage", "Yes", "Yes", "Yes", "Yes"),
    c("Re-election permitted", "Yes", "Yes", "Yes", "Yes"),
    c("Gov. fragmentation", "Yes", "Yes", "Yes", "Yes"),
    c("GDP growth", "Yes", "Yes", "Yes", "Yes", "Yes"),
    c("Inflation (log)", "Yes", "Yes", "Yes", "Yes"),
    c("Honeymoon effects", "Yes", "Yes", "Yes", "Yes"),
    c("Pre-election effects", "Yes", "Yes", "Yes", "Yes"),
    c("Weak instruments",
      format(round(summary(iv1)$diagnostics[1, 3], 3), nsmall = 3),
      format(round(summary(iv2)$diagnostics[1, 3], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[1, 3], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[1, 3], 3), nsmall = 3)
    ),
    c("Weak instruments p-value",
      format(round(summary(iv1)$diagnostics[1, 4], 3), nsmall = 3),
      format(round(summary(iv2)$diagnostics[1, 4], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[1, 4], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[1, 4], 3), nsmall = 3)
    ),
    c("Weak interaction",
      "",
      format(round(summary(iv2)$diagnostics[2, 3], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[2, 3], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[2, 3], 3), nsmall = 3)
    ),
    c("Weak interaction p-value",
      "",
      format(round(summary(iv2)$diagnostics[2, 4], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[2, 4], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[2, 4], 3), nsmall = 3)
    ),
    c("Wu-Hausman",
      format(round(summary(iv1)$diagnostics[2, 3], 3), nsmall = 3),
      format(round(summary(iv2)$diagnostics[3, 3], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[3, 3], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[3, 3], 3), nsmall = 3)
    ),
    c("Wu-Hausman p-value",
      format(round(summary(iv1)$diagnostics[2, 4], 3), nsmall = 3),
      format(round(summary(iv2)$diagnostics[3, 4], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[3, 4], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[3, 4], 3), nsmall = 3)
    ),
    c("Sargan",
      format(round(summary(iv1)$diagnostics[3, 3], 3), nsmall = 3),
      format(round(summary(iv2)$diagnostics[4, 3], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[4, 3], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[4, 3], 3), nsmall = 3)
    ),
    c("Sargan p-value",
      format(round(summary(iv1)$diagnostics[3, 4], 3), nsmall = 3),
      format(round(summary(iv2)$diagnostics[4, 4], 3), nsmall = 3),
      format(round(summary(iv3)$diagnostics[4, 4], 3), nsmall = 3),
      format(round(summary(iv4)$diagnostics[4, 4], 3), nsmall = 3)
    ),
    c("VIF",
      format(round(as.numeric(VIF(iv1)), 3), nsmall = 3),
      format(round(as.numeric(VIF(iv2)), 3), nsmall = 3),
      format(round(as.numeric(VIF(iv3)), 3), nsmall = 3),
      format(round(as.numeric(VIF(iv4)), 3), nsmall = 3)
    ),
    c("AIC",
      format(round(as.numeric(model_performance(iv1)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(iv2)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(iv3)[1]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(iv4)[1]), 3), nsmall = 3)
    ),
    c("RMSE",
      format(round(as.numeric(model_performance(iv1)[6]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(iv2)[6]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(iv3)[6]), 3), nsmall = 3),
      format(round(as.numeric(model_performance(iv4)[6]), 3), nsmall = 3)
    )
  ),
  covariate.labels = c(
    "Dismissals of tainted ministers (t - 1)",
    "Resignation calls (t - 1)",
    "Resignation calls (t - 1) squared",
    "Issue visibility (t - 1)",
    "Coalition government",
    "Dismissals x visibility",
    "Dismissals x calls squared",
    "Dismissals x coalition"
  )
)

## Display summaries in the console for additional checks
summary(iv1)
summary(iv2)
summary(iv3)
summary(iv4)

## Display robust SEs
coeftest(iv1, vcov = vcovHC(iv1, type = "HC1"))
coeftest(iv2, vcov = vcovHC(iv2, type = "HC1"))
coeftest(iv3, vcov = vcovHC(iv3, type = "HC1"))
coeftest(iv4, vcov = vcovHC(iv4, type = "HC1"))
