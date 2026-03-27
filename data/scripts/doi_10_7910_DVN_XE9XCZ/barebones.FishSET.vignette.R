## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
zi <- read.csv("zi.csv", header=FALSE)
zi$V1 <- as.numeric(zi$V1)

catch <- read.csv("catch.csv", header=FALSE)

choice <- read.csv("choice.csv", header=FALSE)
choice$V1 <- as.numeric(choice$V1)

predicted_catch <- read.csv("predicted_catch.csv", header=FALSE)
predicted_catch$V3 <- as.numeric(predicted_catch$V3)

distance <- read.csv("distance.csv", header=FALSE)

otherdat <- list(griddat=list(si=as.matrix(predicted_catch)),
    intdat=list(zi=as.matrix(zi)))

library(barebones.FishSET)

## ---- echo=TRUE----------------------------------------------------------
str(catch)
str(choice)

## ---- echo=TRUE----------------------------------------------------------
str(distance)

## ---- echo=TRUE----------------------------------------------------------
str(otherdat)

## ---- echo=TRUE----------------------------------------------------------
initparams <- c(2.5, -0.8)
#Initial paramters for revenue then cost.

optimOpt <- c(1000,1.00000000000000e-08,1,1)
#Optimization options for the maximum iterations, the relative tolerance of x,
    #report frequency, and whether to report iterations.

func <- logit_c
#The conditional logit likelihood function.

methodname <- "BFGS"
#The optimization method chosen, which must be one of the base R `optim`
    #options.

## ---- echo=TRUE----------------------------------------------------------
results <- discretefish_subroutine(catch,choice,distance,otherdat,initparams,
    optimOpt,func,methodname)
results

## ---- echo=TRUE----------------------------------------------------------
results$OutLogit[1,1]/results$OutLogit[2,1]

