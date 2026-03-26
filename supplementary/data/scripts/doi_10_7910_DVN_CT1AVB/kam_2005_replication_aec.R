# Replication Script for
# Kam, C.D. Who Toes the Party Line? Cues, Values, and Individual Differences.
# Polit Behav 27, 163–182 (2005). https://doi.org/10.1007/s11109-005-1764-y
# Translated to tidyverse R from Kam's do file by Alex Coppock

library(tidyverse)
library(broom)
library(MASS)
library(haven)

dat <- read_dta("Kam 2005 PB all vars clean.dta")

#-----------------------------
# TABLE 1
#-----------------------------

fit_1 <-
  polr(
    formula = as.ordered(foodop1) ~ inptycue3 + outptycue3 + trssci,
    method = "probit",
    Hess = TRUE,
    data = filter(dat, dem3cata > 0 | gop3cata > 0)
  )

table_1 <- tidy(fit_1, conf.int = TRUE)

#-----------------------------
# TABLE 2: variable construction
#-----------------------------

dat <-
  dat |>
  mutate(
    ptysup3 = if_else(inptycue3 == 0, -1 * outptycue3, inptycue3),
    infofignes = (info1 + info3 + info5 + info6) / 4,
    nfccommon = case_when(
      pilotdata == 1 ~ (nfc1 + nfc2) / 2,
      pilotdata == 0 ~ (nfc1 + nfc4) / 2,
      TRUE ~ NA_real_
    ),
    nfccommon = (nfccommon - 0.125) / 0.875,
    nfccommonptysup3 = nfccommon * ptysup3,
    nfccommontrssci = nfccommon * trssci,
    infofignesptysup3 = infofignes * ptysup3,
    infofignestrssci = infofignes * trssci
  )

#-----------------------------
# TABLE 2 models
#-----------------------------

# Table 2, Model 1 (nfc)
fit_2 <- polr(
  as.ordered(foodop1) ~ ptysup3 + nfccommontrssci + trssci +
    nfccommonptysup3 + nfccommon,
  data = filter(dat, dem3cata > 0 | gop3cata > 0),
  method = "probit",
  Hess = TRUE
)

table_2_col_1 <- tidy(fit_2, conf.int = TRUE)

# Table 2, Model 2 (info)

fit_3 <- polr(
  as.ordered(foodop1) ~ ptysup3 + infofignesptysup3 + trssci +
    infofignestrssci + infofignes,
  data = filter(dat, dem3cata > 0 | gop3cata > 0),
  method = "probit",
  Hess = TRUE
)

table_2_col_2 <- tidy(fit_3, conf.int = TRUE)

table_1
table_2_col_1
table_2_col_2
