library(tidyverse)
library(scales)
library(truncnorm)

# SIM 1
# Baseline simulation.
run_sim_1 <- function(baseline_believe_true, baseline_believe_fake, composition_true, N, news_max){
  population <- tibble(believe_true = rtruncnorm(N, a = 0, b = 1, baseline_believe_true, .1), # truncated normal distributions for belief
                      believe_fake = rtruncnorm(N, a = 0, b = 1, baseline_believe_fake, .1),
                      exposure = rescale(rlnorm(N))) # log-normal distribution for exposure (rescaled between 0 and 1)
  info <- matrix(NA, N, news_max)
  news <- sample(c(1,-1), news_max, prob = c(composition_true, 100 - composition_true), replace = TRUE)
  for(n in 1:N){
    exposure_n <- sample(c(1, 0), news_max, prob = c(population$exposure[n], 1 - population$exposure[n]), replace = TRUE)
    for(t in 1:news_max){
      if(exposure_n[t] == 1 & news[t] == 1){ # true news
        info[n, t] <- sample(c(1, 0), 1, prob = c(population$believe_true[n], 1 - population$believe_true[n]))
      }
      if(exposure_n[t] == 1 & news[t] == -1){ # fake news
        info[n, t] <- sample(c(-1, 0), 1, prob = c(population$believe_fake[n], 1 - population$believe_fake[n]))
      }
    }  
  }
  sum(info, na.rm = TRUE) / sum(!is.na(info)) * 100
}

# SIM 2
# kt and kf added. 
# when individuals encounter a TN and they believe it, their believe_false is decreased by kt.
# when individuals encounter a FN and they believe it, their believe_true is decreased by kf.
# the output is the historical record of believe_true and believe_fake.

run_sim_2 <- function(baseline_believe_true, baseline_believe_fake, composition_true, kt, kf, N, news_max){
  population <- tibble(believe_true = rtruncnorm(N, a = 0, b = 1, baseline_believe_true, .1), # truncated normal distributions for belief
                       believe_fake = rtruncnorm(N, a = 0, b = 1, baseline_believe_fake, .1),
                       exposure = rescale(rlnorm(N))) # log-normal distribution for exposure (rescaled between 0 and 1)
  info <- matrix(NA, N, news_max)
  exposure <- matrix(NA, N, news_max)
  news <- sample(c(1,-1), news_max, prob = c(composition_true, 100 - composition_true), replace = TRUE)
  
  # additional output:
  out <- matrix(NA, news_max, 2)
  for(n in 1:N){
    exposure[n,] <- sample(c(1, 0), news_max, prob = c(population$exposure[n], 1 - population$exposure[n]), replace = TRUE)
  }
  for(t in 1:news_max){
    for(n in 1:N){
      if(exposure[n, t] == 1 & news[t] == 1){ # true news
        info[n, t] <- sample(c(1, 0), 1, prob = c(population$believe_true[n], 1 - population$believe_true[n]))
        if(info[n, t] == 1){
          # when individuals encounter a true news and they believe it, their believe_false is decreased by kt:
          population$believe_fake[n] <- population$believe_fake[n] - kt
          if(population$believe_fake[n] < 0) population$believe_fake[n] <- 0
        }
      }
      if(exposure[n, t] == 1 & news[t] == -1){ # fake news
        info[n, t] <- sample(c(-1, 0), 1, prob = c(population$believe_fake[n], 1 - population$believe_fake[n]))
        if(info[n, t] == -1){
          # when individuals encounter a fake news and they believe it, their believe_true is decreased by kf:
          population$believe_true[n] <- population$believe_true[n] - kf
          if(population$believe_true[n] < 0) population$believe_true[n] <- 0
        }
      }
    } 
    out[t, 1] <- mean(population$believe_true, na.rm = TRUE)
    out[t, 2] <- mean(population$believe_fake, na.rm = TRUE)
  }
  out
}


# SIM 3
# pt and pf added. 
# when individuals encounter a TN and they believe it, composition_true is increased by pt.
# when individuals encounter a FN and they believe it, composition_false is increased by pf.
# the output is the historical record of believe_true and believe_fake.

run_sim_3 <- function(baseline_believe_true, baseline_believe_fake, composition_true, pt, pf, N, news_max){
  population <- tibble(believe_true = rtruncnorm(N, a = 0, b = 1, baseline_believe_true, .1), # truncated normal distributions for belief
                       believe_fake = rtruncnorm(N, a = 0, b = 1, baseline_believe_fake, .1),
                       exposure = rescale(rlnorm(N))) # log-normal distribution for exposure (rescaled between 0 and 1)
  info <- matrix(NA, N, news_max)
  exposure <- matrix(NA, N, news_max)
  news <- sample(c(1,-1), news_max, prob = c(composition_true, 100 - composition_true), replace = TRUE)
  
  # additional output:
  out <- matrix(NA, news_max, 2)
  for(n in 1:N){
    exposure[n,] <- sample(c(1, 0), news_max, prob = c(population$exposure[n], 1 - population$exposure[n]), replace = TRUE)
  }
  for(t in 1:news_max){
    for(n in 1:N){
      if(exposure[n, t] == 1 & news[t] == 1){ # true news
        info[n, t] <- sample(c(1, 0), 1, prob = c(population$believe_true[n], 1 - population$believe_true[n]))
        if(info[n, t] == 1){
          # when individuals encounter a true news and they believe it, the proportion of TN is increase by pt:
          composition_true <- composition_true + pt
          if(composition_true > 100) composition_true <- 100
        }
      }
      if(exposure[n, t] == 1 & news[t] == -1){ # fake news
        info[n, t] <- sample(c(-1, 0), 1, prob = c(population$believe_fake[n], 1 - population$believe_fake[n]))
        if(info[n, t] == -1){
          # when individuals encounter a fake news and they believe it, the proportion of FN is increase by pf:
          composition_true <- composition_true - pf
          if(composition_true < 0) composition_true <- 0
        }
      }
    } 
    news <- sample(c(1,-1), news_max, prob = c(composition_true, 100 - composition_true), replace = TRUE)
    out[t, 1] <- composition_true
    out[t, 2] <- 100 - composition_true
    if(composition_true == 100 | composition_true == 0) { # end sim when equilibrium is reached
      break
    }
  }
  out
}
