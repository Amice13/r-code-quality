### REPLICATION FILE -- Table 5
### Jeff Gill & Jonathan Homola
### "Issues in Polling Methodologies: Inference and Uncertainty" 
### In: Lonna Rae Atkinson and R. Michael Alvarez (eds.): The 
### Oxford Handbook of Polling and Polling Methods. Oxford: Oxford 
### University Press.
### http://dx.doi.org/10.1093/oxfordhb/9780190213299.013.11

## clear environment, set seed, install/load packages
rm(list=ls())
set.seed(12435); options(stringsAsFactors=F)
#install.packages("foreign"); install.packages("stargazer")
library("foreign"); library("stargazer")
library("sandwich"); library("lmtest")

## function to calculate STATA-style clustered standard errors
cl <- function(dat,fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)}

## set working directory
#setwd(" ... ")

## read in the main dataset 
oup <- read.dta("GillHomola_OUPChapter_Data.dta")



####
#### Table 5 The Effect of Policy Distance on Vote Share (CSES)
####
mod1 <- lm(voteshare ~ dist, data=oup)
mod2 <- lm(voteshare_transf ~ dist, data=oup)
mod3 <- lm(intentshare ~ dist, data=oup)
mod4 <- lm(intentshare_transf ~ dist, data=oup)

## clustered SEs
oup1 <- na.omit(oup[ , c("election", all.vars(formula(mod1)))])
oup2 <- na.omit(oup[ , c("election", all.vars(formula(mod3)))])
oup3 <- na.omit(oup[ , c("election", all.vars(formula(mod4)))])
mod1 <- cl(oup1, mod1, oup1$election)
mod2 <- cl(oup1, mod2, oup1$election)
mod3 <- cl(oup2, mod3, oup2$election)
mod4 <- cl(oup3, mod4, oup3$election)

stargazer(mod1, mod2, mod3, mod4,
          omit.stat=c("f", "ser", "bic", "ll"), 
          star.cutoffs = c(0.1, 0.05, 0.01),
          column.sep.width="1pt", digits=2,
          dep.var.labels.include = FALSE,
          dep.var.caption="Outcome variable: Normalized Vote Share")