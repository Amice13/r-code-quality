## Eszter Hargittai & Aaron Shaw
## 2019
## 04-gen_models.R

## Description of this file:
## Fit and report regression models in Tables 2 and 3 of Hargittai &
## Shaw, 2019.

library(stargazer)
library(pscl)

setwd("~/research/web_use_US_survey/mturk_norc_comparison/analysis/")

if(!exists("d")){
    source("01-import.R", echo=T)
}

if(!exists("covars")){
    covars <-
        "age + female + incomesqrt + employed + rural + eduhsorless + edusc + hispanic + black + asian + nativeam + raceother + amtsample"
}

## Table 2 model:
f.amt <- as.formula(paste("amtsample ~ ", covars, sep=""))
f.amt <- update.formula(f.amt, . ~ . -amtsample)

m2 <- glm(f.amt, data = d, family=binomial(link="logit"))

stargazer(m2,
          type="text",
          title="Table 2: Logisitic regression on whether a participant is in the AMT sample",
          single.row=TRUE,
          digits=2,
          digits.extra=0,
          keep.stat=c("n", "ll") )

### calculate McFadden's pseudo R-squared using pscl
round(pR2(m2)["McFadden"], 3)


## Table 3 models:
dv3.f <- c("accesssum",
           "webweekhrs",
           "std_skillsmean",
           "snssumcompare",
           "do_sum")

dv4.f <- c("std_trust_scale",
           "std_altru_scale",
           "std_pts_give")

gen.model.result <- function(dv.name){
    f.call <- as.formula(
        paste(as.character(dv.name), " ~ ", covars, sep=""),
        )
    m.out <- lm(f.call, data=d)
    return(m.out)
}

t3.results <- lapply(as.list(dv3.f), gen.model.result)

names(t3.results) <- dv3.f

stargazer(t3.results,
          type="text",
          dep.var.labels= as.character(dv.map$var.name.pretty[1:5]),
          title="Table 3: Regression models on Internet experiences",
          single.row=TRUE,
          digits=2,
          digits.extra=0,
          keep.stat=c("n", "adj.rsq") )


t4.results <- lapply(as.list(dv4.f), gen.model.result)

names(t4.results) <- dv4.f

stargazer(t4.results,
          type="text",
          dep.var.labels= as.character(dv.map$var.name.pretty[6:8]),
          title="Table 4: Regression models on prosocial behaviors and attitudes",
          single.row=TRUE,
          digits=2,
          digits.extra=0,
          keep.stat=c("n", "adj.rsq") )



