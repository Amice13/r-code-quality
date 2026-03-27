## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Tables reproduced in this file: 
## 2, 3, 7 (Columns 1 and 3), 8 (Columns 1 and 2), 9 (Columns 1 and 2), A3, A4
##
## Figures reproduced in this file
## 2, 3, A14
### ### ### ###

rm(list = ls())

## Packages

library(tidyverse)
library(pbapply)
library(rdd)
library(stargazer)
library(rddensity)
library(fastDummies)
library(lfe)
library(lme4)
library(rdrobust)

setwd('C:/Users/Hanno/Dropbox/Shared/03_WZB/Merkel Rhetoric/zz_Replication/12_FINAL_V2/Data')

## Load the data

df = read_rds('Data_Speeches_V2.RDS')

## Subset: less than 120 days

select_df <- abs(df$dist_days_raw) < 120

## Get the optimal bandwidth
## We only look at observations 

bw2 <- rdbwselect(df$cos_dist_raw[df$cat_match_th == 1 & select_df], 
                  df$dist_days_raw[df$cat_match_th == 1 & select_df])

## Extract optimal BW

bw_optimal <- round(bw2$bws[1], 0) 

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
    f <- as.formula(paste0(cv, "~ dist_days_raw |", 
                           paste0(covs, collapse = "+")))
  } else {
    f <- as.formula(paste0(cv, "~ dist_days_raw"))
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

## First, select only matching topic observations

select2 <- select_df & df$cat_match_th == 1

#### Figure 2 ####

# Subset data for histogram
# 1. Within 60 days aroung bandwidth
# 2. Matching topics

select_hist <- df$dist_days_raw < 61 & 
  df$cat_match_th == 1 &
  df$dist_days_raw > -61

## Histogram 

p_rv_dens <- ggplot(data = df[select_hist, ], 
                    aes(x = dist_days_raw)) + 
  geom_histogram(breaks=seq(-60,60,by=6), 
                 fill = "grey95", color = "black") + 
  theme_bw() +
  geom_vline(xintercept = 0, size = 1,
             linetype = "solid") + 
  xlab("Days betw. speech and opinion report (running variable)") +
  ylab("Frequency")
p_rv_dens

## McCrary Test ##

# Runnig variable

runvar <- df[select_hist, "dist_days_raw"]

## McCrary Test
## As reported in Section 4.2.2. (Regression Discontinuity Design)

mccrary <- rddensity(X = runvar, vce="jackknife", h =  bw_optimal) %>%
  summary(mccrary)

#### Figure 3 ####

## Bandwidth vector 

bw_vec <- seq(4, bw_optimal*2, by = 3)

## If the optimal bandwidth is not included, add it to the vector

if (!(bw_optimal %in% bw_vec)) {
  bw_vec <- c(bw_vec, bw_optimal)
  bw_vec <- sort(bw_vec)
}

## This is the main RD command - run RD specification for each bandwidth in the vector
## No covariates are not included

out_clu <- pblapply(bw_vec, function(i) {
  get_rd_stats(bw = i, data = df[select2, ],
               cv = "cos_dist_raw",
               se_option = "HC1",
               cluster_tw = c("survey", "speech_id"))[1, ]
}) 


## Convert to data frame 

out_df <- do.call("rbind", out_clu)

## Declare covariates

cvars <- c("surv_words_stem_log", "surv_source", "release_year",
           "release_month", "release_weekday", "survey_weekday", 
           "speech_words_stem_log", "type", 'survey_time_to_rel',
           "words_prod_log")

## This is the main RD command - run RD specification for each bandwidth in the vector
## Covariates are included

out_cv_clu <- pblapply(bw_vec, function(i) {
  get_rd_stats(bw = i, data = df[select2, ],
               cv = "cos_dist_raw",
               covs = cvars,
               cluster_tw = c("survey", "speech_id"))[1, ]
})

## Convert to df

out_df_cv <- do.call("rbind", out_cv_clu)

## Combine DFs for plotting

out_df_all <- rbind(out_df, out_df_cv)

## Add var to indicate whether covars are included

out_df_all$cvars <- rep(c("No Covariates", "Covariates included"), 
                        each = length(bw_vec))

## Make sure they are in the correct order for the plot

out_df_all$cvars <- factor(out_df_all$cvars, 
                           levels = c("No Covariates", "Covariates included"))

## Plot this

p1 <- ggplot(out_df_all, aes(x = bw, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = bw_optimal, linetype = "dashed",
             color = "grey40") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.0) +
  theme_bw() +
  geom_point(shape = 21, fill = "white", size = 3) +
  xlab("Bandwidth (days)") + ylab("") +
  theme(plot.margin=unit(c(0.3,1.2,0.3,0.0),"cm")) +
  facet_wrap(~ cvars) +
  scale_y_continuous(breaks = seq(-.1, 0.15, 0.05), limits = c(-.018, .06))
p1

#### Table 2 ####

## Get the effect at the optimal BW
## This is based on the previous part of this file

effect_at_bw <- out_df_all[out_df_all$bw == bw_optimal, ]

## Obtain mean and SD of the dependent variable for the optimal BW

## First, declare observations w/o missing values
na_ok <- complete.cases(df[, c(cvars, 'cos_dist_raw', 'survey', 'speech_id',
                               'dist_days_raw')])

## Mean

(m_dv <- mean(df$cos_dist_raw[select2 & na_ok & abs(df$dist_days_raw) < 
                                (bw_optimal + 1)])) %>%
  round(4)

## SD

(sd_dv <- (sd(df$cos_dist_raw[select2 & na_ok & abs(df$dist_days_raw) < 
                                (bw_optimal + 1)]))) %>%
  round(4)

## Output as table

stargazer(effect_at_bw, summary = F, digits = 4)

## Effect in Standard deviations

round(effect_at_bw$coef / sd_dv, 4)

#### Table 3 ####

## Subset of pairs that do not consider the same topic

select3 <- select_df& df$cat_match_th == 0

## Model w/o covars

m1 <- get_rd_stats(bw = bw_optimal, data = df[select3, ],
                   covs = c(""), cv = "cos_dist_raw",
                   cluster_tw = c("survey", "speech_id"))[1, ] %>%
  round(4)
m1

## Model w/ covars
## Subset: Previous subset + complete cases for all variables
select3_tw <- select3 & complete.cases(df[, c(cvars, "cos_dist_raw",
                                              "dist_days_raw")])

m2 <- get_rd_stats(bw = bw_optimal, data = df[select3_tw, ],
                   covs = cvars, cv = "cos_dist_raw",
                   cluster_tw = c("survey", "speech_id"))[1, ] %>%
  round(4)
m2

## Mean and SD of DV

(mean_dv_plac <- mean(df[select3_tw, 'cos_dist_raw']) %>% round(4))
(sd_dv_plac <- sd(df[select3_tw, 'cos_dist_raw']) %>% round(6))

## Get this in SD

cat('Effect size in SD (No covs / covs)', 
    round(c(m1$coef, m2$coef) / sd_dv_plac, 4))

#### Table 7, Columns 1 and 3 ####

## Make subset

df_subset = df %>%
  filter(!is.na(is_salient))

## First model: Salient issues (Column 1 of Table 7)

m1 = get_rd_stats(bw = bw_optimal, 
                  data = df_subset[df_subset$is_salient == 1, ],
                  cv = "cos_dist_raw",
                  covs = cvars,
                  cluster_tw = c("survey", "speech_id"))[1, ]
m1 %>% round(4)

## Second model: non-salient issues (Column 3 of Table 7)

m2 = get_rd_stats(bw = bw_optimal, 
                  data = df_subset[df_subset$is_salient == 0, ],
                  cv = "cos_dist_raw",
                  covs = cvars,
                  cluster_tw = c("survey", "speech_id"))[1, ]
m2 %>% round(4)

## Get means of the DV

(m_dv_salient <- mean(df_subset[df_subset$is_salient == 1, 'cos_dist_raw'], 
                      na.rm = T))
(m_dv_notsalient <- mean(df_subset[df_subset$is_salient == 0, 'cos_dist_raw'], 
                         na.rm = T))

## Get SDs of the DV

(sd_dv_salient <- sd(df_subset[df_subset$is_salient == 1, 'cos_dist_raw'], 
                     na.rm = T))
(sd_dv_notsalient <- sd(df_subset[df_subset$is_salient == 0, 'cos_dist_raw'], 
                        na.rm = T))

## Get Effect size in SDs

round(m1$coef / sd_dv_salient, 5)
round(m2$coef / sd_dv_notsalient, 4)

#### Table 8, Columns 1 and 2 ####

## Select subsets

select2_abovemed_time <- select2 & df$mr_time_med == 1 & 
  !is.na(df$mr_time_med)

select2_belowmed_time <- select2 & df$mr_time_med == 0 & 
  !is.na(df$mr_time_med)

## Above median time since last report  - Table 8, Column 2

(m1 <- get_rd_stats(bw = bw_optimal , data = df[select2_abovemed_time, ],
                    cv = "cos_dist_raw",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>% round(4)

## Get the mean, SD of the dependent variable 

(mean_dv1 <- mean(df[select2_abovemed_time, 'cos_dist_raw'])) %>% round(4)
(sd_dv1 <- sd(df[select2_abovemed_time, 'cos_dist_raw'])) %>% round(4)

## Get effect size in SD

round(m1$coef / sd_dv1, 4)

## Below median time since last report - Table 8, column 1

(m2 <- get_rd_stats(bw = bw_optimal , data = df[select2_belowmed_time, ],
                    cv = "cos_dist_raw",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>% round(4)


## Get the mean, SD of the dependent variable 

(mean_dv2 <- mean(df[select2_belowmed_time, 'cos_dist_raw'])) %>% round(4)
(sd_dv2 <- sd(df[select2_belowmed_time, 'cos_dist_raw'])) %>% round(5)

## Get this in SD

round(m2$coef / sd_dv2, 4)

#### Table 9, Columns 1 and 2 ####

## Select subsets based on novelty of opinion report

select2_abovemed <- select2 & df$mr_dist_above_med == 1 & 
  !is.na(df$mr_dist_above_med)

select2_belowmed <- select2 & df$mr_dist_above_med == 0 & 
  !is.na(df$mr_dist_above_med)

## Above median linguistic distance to last report - Table 9, Column 2

(m1 <- get_rd_stats(bw = bw_optimal , data = df[select2_abovemed, ],
                    cv = "cos_dist_raw",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4)

## Get mean and SD of DV

(mean_dv <- mean(df[select2_abovemed, 'cos_dist_raw'])) %>% round(4)
(sd_dv <- sd(df[select2_abovemed, 'cos_dist_raw'])) %>% round(4)

## Get effect size in SD

round(m1$coef / sd_dv, 4)

## Below median linguistic distance to last report - Table 9, Column 1

(m2 <- get_rd_stats(bw = bw_optimal , data = df[select2_belowmed, ],
                    cv = "cos_dist_raw",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>% 
  round(4)

## Get mean and SD of DV

(mean_dv2 <- mean(df[select2_belowmed, 'cos_dist_raw'])) %>% round(4)
(sd_dv2 <- sd(df[select2_belowmed, 'cos_dist_raw'])) %>% round(4)

## Get effect size in SD

round(m2$coef / sd_dv2, 4)

#### Table A3 #### 

## Table A3 Column 1

suppressWarnings(get_rd_stats(bw = bw_optimal , data = df[select2, ],
                              cv = "dist_jac",
                              cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4) %>% dplyr::select(coef, se, pval, obs)

## Table A3 Column 2

suppressWarnings(get_rd_stats(bw = bw_optimal , data = df[select2, ],
                              cv = "dist_jac",
                              covs = cvars,
                              cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4) %>% dplyr::select(coef, se, pval, obs)

## Table A3 Column 3

suppressWarnings(get_rd_stats(bw = bw_optimal , data = df[select2, ],
                              cv = "dist_words",
                              cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4) %>% dplyr::select(coef, se, pval, obs) 

## Table A3 Column 4

suppressWarnings(get_rd_stats(bw = bw_optimal , data = df[select2, ],
                              cv = "dist_words",
                              covs = cvars,
                              cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(5) %>% dplyr::select(coef, se, pval, obs) 

#### Table A4 ####

## Treatment indicator - report released after speech

df$treated <- ifelse(df$dist_days_raw >= 0, 1, 0)

## Only consider matching topics, as in all main models

select_ml <- df$cat_match_th == 1

## Add time between speech and report release in months

df <- df %>%
  mutate(dist_months_abs = abs(dist_days_raw / 30.42))

## Model 1 : OLS w/ covariates, FEs and cluster SE
## The merkel2 variable is an indicator for the second merkel term

m1 <- felm(cos_dist_raw ~ dist_months_abs + surv_words_stem_log  + 
             speech_words_stem_log + 
             survey_time_to_rel + words_prod_log | 
             cat2 + release_year +  release_month  + release_weekday +
             survey_weekday + type + surv_source | 0 |
             survey + speech_id, 
           data = df[df$merkel2 == 1 & select_ml & df$dist_days_raw > -1, ])

## Model 2: Multilevel
## The merkel2 variable is an indicator for the second merkel term

m2 <- lmer(cos_dist_raw ~ dist_months_abs +  surv_words_stem_log +  
             surv_source + 
             release_year + release_month +
             release_weekday + survey_weekday + speech_words_stem_log + 
             type + survey_time_to_rel + words_prod_log +
             (1 + dist_months_abs | cat2) + (1 | speech_id) + (1 | survey), 
           data = df[df$merkel2 == 1 & select_ml & df$dist_days_raw > -1, ])

## List of models

m_list <- list(m1, m2) %>%
  lapply(broom::tidy, conf.int = T)

## Need to do some extra work to feed those into stargazer on this

m_list[[2]]$group <- NULL
m_list[[1]]$p.value <- NULL

## To DF, only look at time distance in months
## Add labels to model
## Show results

m_list <- m_list %>% reduce(rbind) %>%
  filter(term == 'dist_months_abs') %>%
  mutate(model = c('OLS, Within Topic + Covs',
                   'ML, Topic RE + Speech RE + Survey RE + Covs')) %>%
  print()

## Sample means and SDs for table A4

(m_dv <- mean(df[df$merkel2 == 1 & select_ml & 
                   df$dist_days_raw > -1, 'cos_dist_raw']))
(sd_dv <- sd(df[df$merkel2 == 1 & select_ml & 
                  df$dist_days_raw > -1, 'cos_dist_raw']))

## Effect sizes in standard deviations

(m_list$estimate / sd_dv)

## Output as table

stargazer(list(m1, m2), 
          keep = 'months', digits = 4,
          covariate.labels = 'Time since exposure',
          keep.stat = 'n')

#### Figure A14 ####

select2 <- select_df & df$cat_match_th == 1

## Select var for balance test

keepvars <- c('speech_words_stem_log', 'release_weekday', 
              'type', 'dist_days_raw', 'cat2',
              'survey', 'speech_id')

bal_df <- df %>%
  dplyr::select(one_of(keepvars))

## Dichotomous vars to dummies

out <- dummy_cols(bal_df[, c('release_weekday', 'type', 'cat2')]) %>%
  dplyr::select(-1:-3)

## Merge back to the original DF
## Select only the relevant rows

bal_df <- bal_df %>%
  cbind(., out) %>%
  filter(select2)

## Declare balance vars

balvars <- c(colnames(out), 'speech_words_stem_log')

## Sun/Sat should be excluded, likely they are coding errors

balvars <- setdiff(balvars, c('release_weekday_Sun', 'release_weekday_Sat'))

## Setting up the function
## At opt BW

out <- pblapply(balvars, function(v) {
  ret <- get_rd_stats(cv = v,
                      bw = bw_optimal, 
                      data = bal_df,
                      se_option = "HC1",
                      cluster_tw = c("survey", "speech_id"))[1, ]
  ret$var <- v
  ret
})

## To df

out <- out %>%
  reduce(rbind)

## Rename the categories 

cats_proper <- c('Released on Thursday', 'Released on Wednesday', 
                 'Released on Friday', 
                 'Released on Tuesday', 'Released on Monday', 
                 'Medium: Article',
                 'Medium: Press Release', 'Medium: Speech', 
                 'Topic: Social Policy',
                 'Topic: Foreign Policy', 'Topic: Education',
                 'Topic: Environmental Policy', 'Topic: Economic Policy', 
                 'Topic: Interior',
                 'Topic: Culture', 'Length of Government Speech (Log)')
out$var <- cats_proper

## Order levels of factor

out$var <- factor(out$var, levels = cats_proper[c(5,4,2,1,3, 6:8, 13, 9, 10, 
                                                  11, 12, 14, 15, 16)][16:1])

## Plot this 

p1 <- ggplot(data = out, aes(x = var, y = coef)) + 
  geom_hline(yintercept = 0, linetype = 'dotted') +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0) +
  geom_point(shape = 21, fill = 'white') +
  xlab('') + ylab('') + coord_flip() + 
  theme_bw()
p1

