# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code conducts the hypothesis tests reported at the bottom of 
# Table 7.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

library(haven)
library(tidyverse)
library(manyRegressors)

### Test significance of job-title effects
# Loop over characteristics
todos <- c("white","male","over40")
for (k in 1:length(todos)) {
  tostudy <- todos[[k]]

  # Load up the data
  df <- read_dta(file='data/data.dta')

  # Job data
  jobs <- df %>% group_by(firm_id,soc3,job_id) %>%
    summarize(white_cb = sum(cb*.data[[tostudy]])/sum(.data[[tostudy]]),
              black_cb = sum(cb*(1-.data[[tostudy]]))/sum(1-.data[[tostudy]])) %>%
    mutate(cb = white_cb - black_cb) %>% drop_na()

  # Set up model and restrictions
  mod <- lm(cb~ factor(firm_id) + factor(soc3), data=jobs)
  coefs <- as.data.frame(coef(mod))
  nfid <- coefs %>% filter(str_detect(rownames(coefs), 'soc3')) %>% nrow()
  R <- matrix(0,nfid,nrow(coefs))
  j <- 1
  for (k in 1:length(rownames(coefs))) {
    if (str_detect(rownames(coefs)[[k]],"soc3")) {
      R[j,k] <- 1
      j <- j + 1
    }
  }
  q = integer(nrow(R))

  # Run LFO test
  print("soc3")
  print(tostudy)
  print(LOFtest(mod, R, q, nCores=32))

  nfid <- coefs %>% filter(str_detect(rownames(coefs), 'firm_id')) %>% nrow()
  R <- matrix(0,nfid,nrow(coefs))
  j <- 1
  for (k in 1:length(rownames(coefs))) {
    if (str_detect(rownames(coefs)[[k]],"firm_id")) {
      R[j,k] <- 1
      j <- j + 1
    }
  }
  q = integer(nrow(R))

  # Run LFO test
  print("soc3")
  print(tostudy)
  print(LOFtest(mod, R, q, nCores=32))
}

### Test significance of state effects
todos <- c("white","male","over40")
for (k in 1:length(todos)) {
  tostudy <- todos[[k]]

  # Load up the data
  df <- read.dta(file='replication/data.dta')

  # Job data
  jobs <- df %>% group_by(firm_id,state,job_id) %>%
    summarize(white_cb = sum(cb*.data[[tostudy]])/sum(.data[[tostudy]]),
              black_cb = sum(cb*(1-.data[[tostudy]]))/sum(1-.data[[tostudy]])) %>%
    mutate(cb = white_cb - black_cb) %>% drop_na()

  # Set up model and restrictions
  mod <- lm(cb~ factor(firm_id) + factor(state), data=jobs)
  coefs <- as.data.frame(coef(mod))
  nfid <- coefs %>% filter(str_detect(rownames(coefs), 'state')) %>% nrow()
  R <- matrix(0,nfid,nrow(coefs))
  j <- 1
  for (k in 1:length(rownames(coefs))) {
    if (str_detect(rownames(coefs)[[k]],"state")) {
      R[j,k] <- 1
      j <- j + 1
    }
  }
  q = integer(nrow(R))

  # Run LFO test
  print("state")
  print(tostudy)
  print(LOFtest(mod, R, q, nCores=32))

  nfid <- coefs %>% filter(str_detect(rownames(coefs), 'firm_id')) %>% nrow()
  R <- matrix(0,nfid,nrow(coefs))
  j <- 1
  for (k in 1:length(rownames(coefs))) {
    if (str_detect(rownames(coefs)[[k]],"firm_id")) {
      R[j,k] <- 1
      j <- j + 1
    }
  }
  q = integer(nrow(R))

  # Run LFO test
  print("state")
  print(tostudy)
  print(LOFtest(mod, R, q, nCores=32))
}