## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Tables reproduced in this file: 
## 4, 5, 8 (Columns 3 and 4), 9 (Columns 3 and 4), 10
##
### ### ### ###

rm(list = ls())

## Packages

library(tidyverse)
library(pbapply)
library(rdrobust)
library(stargazer)
library(rdd)

## Read Parliament data

df <- read_rds('Data_Parliament.RDS')

## Select only reports / speech docs within 120 days of each others

select <- abs(df$dist_days_raw) < 120 # limit to +- 120 for bandwidth selection

## We now source the custom RD function

source("../Code/Custom_RDD_Function.R")

#### RDD helper function ####

## This function helps us run the RD specification for a given bandwith

## Arguments:

### bw : desired bandwidth
### data : data file
### covs : list of covariates
### se_option : desired standard errors
### cv : Outcome variable
### cluster_tw : 2-element vector of variables to cluster by

get_rd_stats <- function(bw = 30, data,
                         covs = c(""), se_option = "HC1",
                         cv = "cos_dist_raw", cluster_tw = NULL) {
  
  ## If covars are desired:
  
  if (nchar(covs[1]) > 0) {
    f <- stats::as.formula(paste0(cv, "~ dist_days_raw |", 
                           paste0(covs, collapse = "+")))
  } else {
    f <- stats::as.formula(paste0(cv, "~ dist_days_raw"))
  }
  
  ## Do RDD
  
  if (is.null(cluster_tw)) {
    
    ## This is the case where we want regular SEs
    rd1 <- RDestimate(f, data = data,
                      cutpoint = 0, bw = bw,
                      se.type = se_option)
    
  } else {
    
    ## Here we want two-way clustered SEs
    rd1 <- my_rdd(f, data = data,
                  cutpoint = 0, bw = bw,
                  cluster_tw = cluster_tw)
  }
  
  
  coef <- rd1$est; bws <- rd1$bw
  upper <- rd1$ci[, 2]; lower <- rd1$ci[, 1]
  se <- rd1$se; pval <- rd1$p
  obs <- rd1$obs
  
  ## Return results
  data.frame(coef, lower, upper, se, pval, obs, bw = bws)
}

## Declare subsets for RD estimation

select2_opp <- df$bt_opp == 1
select2_gov <- df$bt_executive

## Placebo (non-matching topics)

select2_opp_plac <- df$cat_match == 0 & select2_opp & !is.na(df$cat_match)
select2_gov_plac <- df$cat_match == 0 & select2_gov & !is.na(df$cat_match)

## Main subset (matching topics)

select2_opp <- df$cat_match == 1 & select2_opp & !is.na(df$cat_match)
select2_gov <- df$cat_match == 1 & select2_gov & !is.na(df$cat_match)

## Bandwidth

bw <- rdbwselect(y = df$cos_dist_raw[select2_gov], 
                 df$dist_days_raw[select2_gov])
bw_optimal <- round(bw$bws[1], 0) # save opt bw

## Speech ID Variable

df$bt_speech_id <- as.character(df$bt_speechnumber)

## Declare Covars

cvars <- c("surv_words_stem_log", "surv_source", "bt_year",
           "bt_month", "bt_words_stem_log", "words_prod_log")

## Subset where all covars are observed

cvars_complete <- complete.cases(df[, c(cvars, 'cos_dist_raw')])

#### Table 4 ####

## Gov / no covars
## Table 4 Column 1

(m1 <- get_rd_stats(bw = bw_optimal, 
             data = df[select2_gov & cvars_complete, ],
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>%
  round(4)

## Gov / covars
## Table 4 Column 2

(m2 <- get_rd_stats(bw = bw_optimal, 
             data = df[select2_gov & cvars_complete, ],
             covs = cvars, 
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>%
  round(4)

## Mean and SD of DV

(m_dv <- mean(df[select2_gov & cvars_complete, 'cos_dist_raw'])) %>%
  round(4)

(sd_dv <- sd(df[select2_gov & cvars_complete, 'cos_dist_raw'])) %>%
  round(4)

## Effect size in SD

(c(m1$coef, m2$coef) / sd_dv) %>% round(4)

#### Table 5 ####

## Opposition / no covars
## Table 5 Column 1

(m3 <- get_rd_stats(bw = bw_optimal, data = df[select2_opp & cvars_complete, ],
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>%
  round(4)

## Opposition / covars
## Table 5 Column 2

(m4 <- get_rd_stats(bw = bw_optimal, data = df[select2_opp & cvars_complete, ],
             covs = cvars, 
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>%
  round(4)

## Mean and SD of DV

(m_dv <- mean(df[select2_opp & cvars_complete, 'cos_dist_raw'])) %>%
  round(4)

(sd_dv <- sd(df[select2_opp & cvars_complete, 'cos_dist_raw'])) %>%
  round(4)

## Effect size in SD

(c(m3$coef, m4$coef) / sd_dv) %>% round(4)

#### Table 10 ####

## By list membership

listcand <- df$bt_won_district == 0
directcand <- df$bt_won_district == 1
list_df <-  df[select2_gov & cvars_complete & listcand, ]
not_on_list_df <-  df[select2_gov & cvars_complete & directcand, ]

## Gov / covars / onlist

get_rd_stats(bw = bw_optimal, data = df[select2_gov & 
                                          cvars_complete & 
                                          listcand, ],
             covs = cvars, 
             cluster_tw = c("survey", "bt_speech_id"))[1, ] %>%
  round(4)

## Gov / covars / through district

get_rd_stats(bw = bw_optimal, data = df[select2_gov & 
                                          cvars_complete & 
                                          directcand, ],
             covs = cvars, 
             cluster_tw = c("survey", "bt_speech_id"))[1, ] %>% 
  round(4)

#### Table 9, Columns 3 and 4 ####

## Subsets: Above / below median of cosine distance to previous report

select2_abovemed <- select2_gov & cvars_complete & df$mr_dist_above_med == 1 & 
  !is.na(df$mr_dist_above_med)

select2_belowmed <- select2_gov & cvars_complete& df$mr_dist_above_med == 0 & 
  !is.na(df$mr_dist_above_med)

## Get number of obs

sum(select2_abovemed); sum(select2_belowmed)

## Above median
(m1 <- get_rd_stats(bw = bw_optimal , data = df[select2_abovemed, ],
             cv = "cos_dist_raw",
             covs = cvars,
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>% round(4)

## Mean and SD of DV

(mean_dv <- mean(df[select2_abovemed, 'cos_dist_raw'])) %>% round(4)
(sd_dv <- sd(df[select2_abovemed, 'cos_dist_raw'])) %>% round(4)

## Effect size in SD

round(m1$coef / sd_dv, 4)

## Below median

(m2 <- get_rd_stats(bw = bw_optimal , data = df[select2_belowmed, ],
             cv = "cos_dist_raw",
             covs = cvars,
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>% 
  round(4)

## Mean and SD of DV

(mean_dv2 <- mean(df[select2_belowmed, 'cos_dist_raw'])) %>% round(4)
(sd_dv2 <- sd(df[select2_belowmed, 'cos_dist_raw'])) %>% round(4)

## Effect size in SD

round(m2$coef / sd_dv2, 5)

#### Table 8, Columns 3 and 4 ####

## Above median cosine time since previous report

select2_abovemed_time <- select2_gov & cvars_complete & 
  df$mr_time_med == 1 & 
  !is.na(df$mr_time_med)

## Below median cosine time since previous report

select2_belowmed_time <- select2_gov & cvars_complete & 
  df$mr_time_med == 0 & 
  !is.na(df$mr_time_med)

## Table 8 Column 4

m1 <- get_rd_stats(bw = bw_optimal , data = df[select2_abovemed_time, ],
             cv = "cos_dist_raw",
             covs = cvars,
             cluster_tw = c("survey", "bt_speech_id"))[1, ] %>%
  round(4)
m1

## Get the mean, SD of the dependent variable 

(mean_dv1 <- mean(df[select2_abovemed_time, 'cos_dist_raw'])) %>% round(4)
(sd_dv1 <- sd(df[select2_abovemed_time, 'cos_dist_raw'])) %>% round(4)

## Get effect size in SD

round(m1$coef / sd_dv1, 4)

## Table 8 Column 3

(m2 <- get_rd_stats(bw = bw_optimal , data = df[select2_belowmed_time, ],
             cv = "cos_dist_raw",
             covs = cvars,
             cluster_tw = c("survey", "bt_speech_id"))[1, ]) %>%
  round(4)

## Get the mean, SD of the dependent variable 

(mean_dv2 <- mean(df[select2_belowmed_time, 'cos_dist_raw'])) %>% round(4)
(sd_dv2 <- sd(df[select2_belowmed_time, 'cos_dist_raw'])) %>% round(4)

## Get effect size in SD

round(m2$coef / sd_dv2, 4)
