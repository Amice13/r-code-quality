## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## 5, A3, A15
##
## Tables reproduced in this file 
## 6, 7 (Columns 2 and 4), A5
### ### ### ###

rm(list = ls())

## Packages

library(tidyverse)
library(pbapply)
library(reshape2)
library(rdd)
library(stargazer)
library(rdrobust)

## Get data

df_combined <- read_rds('Data_Speeches_V2.RDS')

## Limit to +- 120 days for bandwidth selection

select <- abs(df_combined$dist_days_raw) < 120 

## Source custom RDD function

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

## Subset: Matching categories, manually coded, <50 days, DV not missing

select2_th <- df_combined$cat_match_th == 1 & 
  df_combined$agree_manual == 1 & 
  df_combined$dist_days_raw < 50 & 
  !is.na(df_combined$agree_scale_combined)

## Make subset

df_combined_th <- df_combined[select2_th, ]

## Optimal BW - Save as scalar

bw2 <- rdbwselect(df_combined$agree_scale_combined[select2_th], 
                  df_combined$dist_days_raw[select2_th])
bw_optimal <- round(bw2$bws[1], 0) 

## Make a vector of bandwidths for sensitivity

bw_vec <- seq(4, bw_optimal*2, by = 3)

## if optimal bw not included, add to vector, sort

if (!(bw_optimal %in% bw_vec)) {
  bw_vec <- c(bw_vec, bw_optimal)
  bw_vec <- sort(bw_vec)
}

## apply function over all values in bw vector, 2-way cluster

out_clu <- pblapply(bw_vec, function(i) {
  get_rd_stats(bw = i, data = df_combined[select2_th, ],
               cv = "agree_scale_combined",
               se_option = "HC1",
               cluster_tw = c("survey", "speech_id"))[1, ]
}) %>%
  reduce(rbind)

## w/ covars 

cvars <- c("surv_words_stem_log", "surv_source", "release_year",
           "release_month", "release_weekday", "survey_weekday", 
           "speech_words_stem_log", "type",
           "survey_time_to_rel", "words_prod_log")

## apply, w/ covars

out_cv_clu <- pblapply(bw_vec, function(i) {
  get_rd_stats(bw = i, data = df_combined[select2_th, ],
               cv = "agree_scale_combined",
               covs = cvars,
               cluster_tw = c("survey", "speech_id"))[1, ]
}) %>%
  reduce(rbind)

## Combine results w/ and w/o covars

out_df_combined_all2 <- rbind(out_clu, out_cv_clu) %>%
  mutate(cvars = rep(c("No Covariates", "Covariates included"), 
                        each = length(bw_vec))) %>%
  mutate(cvars = factor(cvars, 
                         levels = c("No Covariates", 
                                    "Covariates included")))

#### Figure A15 ####

p1 <- ggplot(out_df_combined_all2, aes(x = bw, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = bw_optimal, linetype = "dashed",
             color = "grey40") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.0) +
  theme_bw() +
  geom_point(shape = 21, fill = "white", size = 3) +
  xlab("Bandwidth (days)") + ylab("") +
  theme(plot.margin=unit(c(0.3,1.2,0.3,0.0),"cm")) +
  ylim(c(-1.15, 1.65)) +
  facet_wrap(~ cvars)
p1

#### Table 6 ####
## This relies on the output produced in the previous part

effect_at_bw <- out_df_combined_all2 %>%
  filter(bw == bw_optimal) %>%
  dplyr::select(-cvars) %>%
  round(4) %>%
  print()

## Table Output (rounded)

round(effect_at_bw$coef, 4); round(effect_at_bw$se, 4); effect_at_bw$obs

## Mean of DV and SD of DV

(mean_dv <- mean(df_combined[select2_th, 'agree_scale_combined'])) %>%
  round(4) %>% print()

(sd_dv <- sd(df_combined[select2_th, 'agree_scale_combined'])) %>%
  round(4) %>% print()

## Effect size in SD

round(effect_at_bw$coef / sd_dv, 4)

#### Table A5 ####

select2_unrelated <- df_combined$cat_match_th == 0 & 
  df_combined$agree_manual == 1 & 
  !is.na(df_combined$agree_scale_combined)

## W/o covars

(m1 <- get_rd_stats(bw = bw_optimal, 
                    data = df_combined[select2_unrelated, ],
                    cv = "agree_scale_combined",
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4)

## W/ covars

cvars <- c("surv_words_stem_log", "surv_source", "release_year",
           "release_month", "release_weekday", "survey_weekday", 
           "speech_words_stem_log", "type",
           "survey_time_to_rel", "words_prod_log")

## Check that none of these are missing

select2_ur_covars <- select2_unrelated & complete.cases(df_combined[, cvars])

## Run model

(m2 <- get_rd_stats(bw = bw_optimal, 
                    data = df_combined[select2_ur_covars, ],
                    cv = "agree_scale_combined",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4)

#### Table 7, columns 2 and 4 ####

## Make the data frame

df_salience <- df_combined[select2_th & !is.na(df_combined$is_salient) & 
                             df_combined$agree_scale_combined > -1 , ]

## Do regressions
## Is salient

(m1 <- get_rd_stats(bw = bw_optimal, 
                    data = df_salience[df_salience$is_salient == 1, ],
                    cv = "agree_scale_combined",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4)

## Not salient

(m2 <- get_rd_stats(bw = bw_optimal, 
                    data = df_salience[df_salience$is_salient == 0, ],
                    cv = "agree_scale_combined",
                    covs = cvars,
                    cluster_tw = c("survey", "speech_id"))[1, ]) %>%
  round(4)

## Get mean and SD of outcome

(m_salient <- mean(df_salience[df_salience$is_salient == 1, 'agree_scale_combined'],
                   na.rm = T)) %>% round(5)
(m_notsalient <- mean(df_salience[df_salience$is_salient == 0, 'agree_scale_combined'],
                      na.rm = T)) %>% round(4)

(sd_salient <- sd(df_salience[df_salience$is_salient == 1, 'agree_scale_combined'],
                  na.rm = T)) %>% round(4)
(sd_notsalient <- sd(df_salience[df_salience$is_salient == 0, 'agree_scale_combined'],
                     na.rm = T)) %>% round(4)

## Get effect size in SD

(m1$coef / sd_salient) %>% round(4)
(m2$coef / sd_notsalient) %>% round(4)

#### Figure 5 ####

figure5_df <- df_combined[!is.na(df_combined$unrelated) & 
                                  df_combined$cat_match_th == 1, ]

## Drop pairs that do not address topics in the 

figure5_df <- figure5_df[figure5_df$unrelated == 0, ]

## Box plot?

set.seed(123)

sp_dist <- ggplot(aes(x =agree_scale_combined, 
                      y = cos_dist_raw), 
                  data = figure5_df) + 
  geom_jitter(alpha = 0.3, width = 0.15) +
  geom_smooth(method = 'lm', color = 'grey50', fill = 'grey80') +
  theme_bw() + scale_x_continuous(breaks = -3:3) +
  xlab('Substantive Agreement') + ylab('Cosine Similarity')
sp_dist

#### Figure A3 ####

temp_df_combined <- df_combined[!is.na(df_combined$unrelated) & 
                                  df_combined$cat_match_th == 1, ]

## Distribution of 

p_dist <- ggplot(aes(x = factor(agree_scale_combined)), 
                 data = temp_df_combined) + 
  geom_bar(fill = 'grey95', color = 'black') +
  theme_bw() + 
  xlab('Agreement scale') + 
  ylab('')
p_dist

