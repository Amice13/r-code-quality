# Code from Marble, William, and Matthew Tyler. 2022. “The Structure of Political Choices: Distinguishing Between Constraint and Multidimensionality.” Political Analysis 30 (3): 328–45. 
# libraries and data -------------------------------------------

library(tidyverse)
library(haven)
library(emIRT)
#library(MultiScale)
library(data.table)

# functions -----------------------------------------------------
source(here("functions", "MultiScale.R"))

one.fold <- function(data, vote.grid, test.index) {
  test.grid <- vote.grid[test.index,]
  Y.train <- data$Y
  Y.train[test.index] <- NA # how missing values are stored
  data.train <- list(Y = Y.train, N = data$N, J = data$J, D = data$D)
  prior <- adapt.emIRT.prior(makePriors(data.train$N, data.train$J, data.train$D))
  init <- make.starts(data.train)
  use.method <- ifelse(data$D == 0, "intercepts", "sparse")
  lout <- MultiScale(method = use.method, prior = prior, data = data.train, init = init, tol = 1e-4, verbose = FALSE)
  holdout.lik <- array(NA, dim = length(test.index))
  for (i in 1:length(test.index)) {
    actor <- test.grid[i,1]
    question <- test.grid[i,2]
    if (data$D == 0) {
      eta <- lout$alpha[question]
    } else {
      eta <- lout$alpha[question] + lout$gamma[actor,] %*% lout$beta[question,]
    }
    prob.yea <- pnorm(c(eta))
    if (data$Y[actor,question] == 1) {
      holdout.lik[i] <- prob.yea
    } else if (data$Y[actor,question] == -1 ) {
      holdout.lik[i] <- 1 - prob.yea
    } else {
      holdout.lik[i] <- NA
    }
  }
  return(holdout.lik)
}

# function to return cluster SE’s of the mean
get_mean_se = function(x, cl = cl, multi0 = FALSE) {
  reg = lm(x ~ 1)
  se = sqrt(sandwich::vcovCL(reg, cl, multi0 = multi0))[1, 1]
  return(se)
}

se_mean = function(x) {
  if (any(is.na(x))) {
    warning("there are ", sum(is.na(x)), " missing values")
  }
  return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))
}

get.lik <- function(data, vote.grid, test.index) {
  test.grid <- vote.grid[test.index, ]
  data.train <- data
  prior <-
    adapt.emIRT.prior(makePriors(data.train$N, data.train$J, data.train$D))
  init <- make.starts(data.train)
  use.method <- ifelse(data$D == 0, "intercepts", "sparse")
  lout <-
    MultiScale(
      method = use.method,
      prior = prior,
      data = data.train,
      init = init,
      tol = 1e-4,
      verbose = FALSE
    )
  insample.lik <- array(NA, dim = length(test.index))
  for (i in 1:length(test.index)) {
    actor <- test.grid[i, 1]
    question <- test.grid[i, 2]
    if (data$D == 0) {
      eta <- lout$alpha[question]
    } else {
      eta <-
        lout$alpha[question] + lout$gamma[actor, ] %*% lout$beta[question, ]
    }
    prob.yea <- pnorm(c(eta))
    if (data$Y[actor, question] == 1) {
      insample.lik[i] <- prob.yea
    } else if (data$Y[actor, question] == -1) {
      insample.lik[i] <- 1 - prob.yea
    } else {
      insample.lik[i] <- NA
    }
  }
  return(insample.lik)
}
