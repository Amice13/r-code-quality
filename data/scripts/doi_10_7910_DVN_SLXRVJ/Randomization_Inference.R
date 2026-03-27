## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## 4
### ### ### ###

library(rdd)
library(lubridate)
library(tidyverse)
library(parallel)

## Helper function to get the RDD estimates
## Note that we don't use the custom function w/ two-way cluster here
## We are only interested in the distribution of effect sizes

rdd_aux <- function(data, 
                    runvar = "time_dist",
                    yvar = "cos_dist", 
                    bw = 20, cvars_do = F,
                    cvars = "", 
                    data_bt = F) {
  
  ## set formula
  
  if (!cvars_do) {
    rd_form <- as.formula(paste0(yvar, " ~ ", runvar))
  } else { 
    rd_form <- as.formula(paste0(yvar, " ~", runvar, "|",
                                 paste0(cvars, collapse = "+")))
    }
  
  ## do rdd
  
  if (data_bt == F) { #exx
  
    est_out <- rdd::RDestimate(rd_form, 
                               data = data[data$cat_match_th == 1, ],
                               cutpoint = 0,
                               bw = bw)
  } else {
    est_out <- rdd::RDestimate(rd_form, 
                               data = data[select2_gov, ],
                               cutpoint = 0,
                               bw = bw)
  }
  coef <- est_out$est[1]
  p_val <- est_out$p[1]
  
  ## output coef and p-val
  c("coef" = coef, "p_val" = p_val)
}


## Permutation function
## The function creates estimates of the treatment effect
## This is done for a counterfactual distribution of report release dates

perm_test <- function(data = df, runvar = "time_dist",
                      yvar = "cos_dist",
                      bw = 20, rep_date_var = "date_rep",
                      speech_date_var = "date_spe",
                      dist_var = "cos_dist",
                      n_rep = 20,
                      n_spe = 2000,
                      cvars_do = F,
                      cvars = "") {
  
  ## Get vector of all possible release dates
  
  earliest <- min(data[, rep_date_var])
  latest <- max(data[, rep_date_var])
  
  date_vec <- seq(earliest, 
                  latest, by="day")
  
  ## Delete weekends (reports are not released on weekends)
  
  weekend <- wday(date_vec, label = T)
  weekend <- weekend %in% c("Sun", "Sat")
  date_vec <- date_vec[!weekend]
  
  ## Sample from new date list
  
  dates_rep_new <- sample(date_vec, n_rep, replace = T)
  
  ## Gen new DF for RDD
  
  df_temp <- data
  
  ## Replace release dates with counterfactual dates
  df_temp[, rep_date_var] <- rep(dates_rep_new, n_spe)
  
  ## Gen counterfactual distance
  df_temp[, runvar] <- df_temp[, speech_date_var] - df_temp[, rep_date_var]
  
  ## do RDD
  
  res <- rdd_aux(data = df_temp, runvar = runvar, 
                 yvar = yvar, bw = bw, cvars_do = cvars_do,
                 cvars = cvars)
  
  ## Return results
  res
}
## Get data

df <- read_rds('Data_Speeches_V2.RDS')

## Convert dates to Date format

df$survey_date <- as.Date(df$survey_date)
df$release_date <- as.Date(df$release_date)

## Declare number of opinion reports, speech documents

n_rep <- 125
n_speech <- nrow(df) / n_rep

#### get baseline RD estimates

cvars <- c("surv_words_stem_log", "surv_source", "release_year",
           "release_month", "speech_words_stem_log", "type",
           "survey_time_to_rel", "words_prod_log")

## This correponds to the estimate in Table 2, Column 2

rd1_22 <- rdd_aux(data = df, runvar = "dist_days_raw",
                  yvar = "cos_dist_raw", bw = 22,
                  cvars_do = T, cvars = cvars)

## Permutation test

set.seed(02138)

## Declare number of iterations

n_iter = 1000

## To speed up the computation, we use parallelization

# Get number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl1 <- makeCluster(no_cores)

# Load libraries on all nodes
clusterEvalQ(cl1, library(lubridate))
clusterEvalQ(cl1, library(rdd))

## Set seeds on each node

clusterApply(c(1,2,3), set.seed, cl = cl1)

## Export all necessary objects

clusterExport(cl1, c("df", "n_rep", "n_speech", "perm_test",
                    "rdd_aux", "cvars"))

## Run randomization inference for the optimal BW (22 days)
## Take ~14 min on i7-8550U / 16GB Ram

system.time(perm_res_bw22 <- parallel::parSapply(cl = cl1, X = 1:n_iter, FUN = function(x) {
  perm_test(data = df, runvar = "dist_days_raw",
            yvar = "cos_dist_raw", bw = 22, 
            rep_date_var = "survey_date", 
            speech_date_var = "release_date",
            dist_var = "cos_dist_raw", 
            n_rep = n_rep, n_spe = n_speech,
            cvars_do = T, cvars = cvars)
  }))

## Srop cluster

stopCluster(cl1)

## Prep for output

perm_res_bw22 <- t(perm_res_bw22)
perm_res_bw22 <- data.frame(perm_res_bw22)
colnames(perm_res_bw22) <- c("coef", "p_val")

## RI P-value

sum(abs(perm_res_bw22$coef) > rd1_22[1]) / n_iter

#### Figure 4 ####

p1 <- ggplot(perm_res_bw22, aes(x = coef)) + 
  geom_histogram(fill = "grey95",
                 colour = "black",
                 binwidth = 0.002) +
  theme_bw() +
  geom_vline(xintercept = 0.00, 
             linetype = "dashed",
             size = 0.8) +
  geom_vline(xintercept = rd1_22[1],
             size = 0.7) +
  ylab("") + 
  xlab("Estimated effect")
p1
