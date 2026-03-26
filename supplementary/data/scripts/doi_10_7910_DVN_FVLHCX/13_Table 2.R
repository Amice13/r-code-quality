#############################
# Load packages
#############################
library(tidyverse)
library(lme4)
library(arm)
library(stargazer)

#############################
# Load data 
#############################
rm(list=setdiff(ls(), c('script', 'scripts', 'log_file')))

# Load candidate data
open <- subset(readRDS('model_df.rds'), year >= 2010)
open$fundraising_amount_cp_th <- open$fundraising_amount_cp/1000

#############################
# Table 3: Application---early fundraising and success
#############################

# Estimate candidate success model, with fundraising share and normalized 
mr1 <- glmer(winwin ~ rescale(fundraising_amount_cp_th) +
               rescale(fundshare_norm) +
               cand_party + seat_safety + quality_total + primary_type +
               as.factor(female) + quality_cand + as.factor(year)  +
               (1|raceid), family = 'binomial',  data = open) 
summary(mr1)

# Estimate candidate success model, with fundraising share only 
mr1_direct <- glmer(winwin ~ rescale(fundraising_amount_cp_th) +
                      cand_party + seat_safety + quality_total + primary_type +
                      as.factor(female) + quality_cand + as.factor(year)  +
                      (1|raceid), family = 'binomial',  data = open) 
summary(mr1_direct)

# Estimate candidate success model, with fundraising normalized only 
mr1_share <- glmer(winwin ~ rescale(fundshare_norm) +
                     cand_party + seat_safety + quality_total + primary_type +
                     as.factor(female) + quality_cand + as.factor(year)  +
                     (1|raceid), family = 'binomial',  data = open) 
summary(mr1_share)

# Build post-registration fundraising
open$fundraising_amount_post_registration <- (open$fundraising_amount_pre_primary - 
                                                open$fundraising_amount_pre_filing_deadline)/1000


# Estimate post-reg. fundraising, with fundraising share and normalized  
mr3 <- lmer(fundraising_amount_post_registration ~ rescale(fundraising_amount_cp_th) +
              rescale(fundshare_norm) + cand_party + seat_safety + 
              quality_total + primary_type + as.factor(female) + quality_cand + 
              as.factor(year) + (1|raceid), data = open, REML = FALSE) 
summary(mr3)

# Estimate post-reg. fundraising, with fundraising share only
mr3_direct <- lmer(fundraising_amount_post_registration ~  rescale(fundraising_amount_cp_th) +
                     cand_party + seat_safety + quality_total + primary_type +
                     as.factor(female) + quality_cand + as.factor(year) +
                     (1|raceid), data = open, REML = FALSE) 
summary(mr3_direct)

# Estimate post-reg. fundraising, with fundraising normalized only
mr3_share <- lmer(fundraising_amount_post_registration ~ rescale(fundshare_norm) +
                    cand_party + seat_safety + quality_total + primary_type +
                    as.factor(female) + quality_cand + as.factor(year) +
                    (1|raceid), data = open, REML = FALSE) 
summary(mr3_share)

# Run ANOVA tests
anova(mr1 , mr1_direct, test = "Chisq")
anova(mr1 , mr1_share, test = "Chisq")
anova(mr3 , mr3_direct, test = "Chisq")
anova(mr3 , mr3_share, test = "Chisq")

# Print models 
stargazer(mr3_direct, mr3_share, mr3, mr1_direct, mr1_share, mr1,
          covariate.labels = c("Early Contributions: Candidate Centered (Standardized)", 
                               "Early Contributions: Election-Centered (Standardized)", 
                               "Candidate Party: Republican", "Seat Safety: Same Party",
                               "Seat Safety: Competitive", 
                               "Number of Quality Candidates", 
                               'Primary Type: Open', 
                               'Primary Type: Closed', 
                               'Female', 'Quality Candidate', 
                               'Constant'),
          omit = 'year',
          digits = 3,
          out = 'table2.txt')
