## Eszter Hargittai & Aaron Shaw
## 2019
## 05-alternative_specifications

## Description of this file:
## Fits alternative model specifications using the log-transformed
## income measure as requested during the review process for Hargittai
## & Shaw, 2019.

setwd("~/research/web_use_US_survey/mturk_norc_comparison/analysis/")

covars <-
    "age + female + lincome + employed + rural + eduhsorless + edusc + hispanic + black + asian + nativeam + raceother + amtsample"

source("04-gen_models.R", echo=T)
