
rm(list = ls())
library(data.table)
library(here)
library(estimatr)
library(tidyr)
library(stargazer)
library(texreg)

cands <- readRDS(here('data','plataformas','clean_plataformas.rds'))


lm0 <- (lm_robust(total_n_lg ~ law_enforcement + not_ballot, data = cands))
lm1 <- (lm_robust(total_n_lg ~ law_enforcement + not_ballot + as.factor(year), data = cands))
lm2 <- (lm_robust(total_n_lg ~ as.factor(sigla_uf) + as.factor(year) + law_enforcement + not_ballot, data = cands))
lm00 <- (lm_robust(total_n_lg ~ law_enforcement, data = cands[law_enforcement == 1 | not_ballot == 1]))
lm3 <- (lm_robust(total_n_lg ~ law_enforcement + as.factor(year), data = cands[law_enforcement == 1 | not_ballot == 1]))
lm4 <- (lm_robust(total_n_lg ~ as.factor(year) + as.factor(sigla_uf) + law_enforcement, data = cands[law_enforcement == 1 | not_ballot == 1]))

#THIS PRODUCES TABLE A2
texreg(list(lm0, lm1, lm2, lm00,lm3,lm4), 
       include.ci = FALSE, omit.coef = 'as.factor',stars = c(0.01,0.05,0.1),
       custom.model.names = c('[1]','[2]','[3]','[4]','[5]','[6]'),
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE, 
       custom.coef.map = list("law_enforcement" = 'Ballot name', "not_ballot" = "No ballot name","(Intercept)" = "Intercept"),
       custom.gof.rows = list("State FE" = c("-", "-", "Yes",'-','-','Yes'),"Year FE" = c("-", "Yes", "Yes",'-','Yes','Yes')))


lm0_a <- (lm_robust(total_words_lg ~ law_enforcement + not_ballot, data = cands))
lm1_a <- (lm_robust(total_words_lg ~ law_enforcement + not_ballot + as.factor(year), data = cands))
lm2_a <- (lm_robust(total_words_lg ~ as.factor(sigla_uf) + as.factor(year) + law_enforcement + not_ballot, data = cands))
lm00_a <- (lm_robust(total_words_lg ~ law_enforcement, data = cands[law_enforcement == 1 | not_ballot == 1]))
lm3_a <- (lm_robust(total_words_lg ~ law_enforcement + as.factor(year), data = cands[law_enforcement == 1 | not_ballot == 1]))
lm4_a <- (lm_robust(total_words_lg ~ as.factor(year) + as.factor(sigla_uf) + law_enforcement, data = cands[law_enforcement == 1 | not_ballot == 1]))

# THIS PRODUCES TABLE 1
texreg(list(lm0_a, lm1_a, lm2_a, lm00_a, lm3_a, lm4_a), 
          include.ci = FALSE,
          omit.coef = 'as.factor',
          stars = c(0.01,0.05,0.1),
          custom.model.names = c('[1]','[2]','[3]','[4]','[5]','[6]'),
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          include.rmse = FALSE,
          custom.coef.map = list("law_enforcement" = 'Ballot name',
                                 "not_ballot" = "No ballot name",
                                 "(Intercept)" = "Intercept"),
          custom.gof.rows = list("State FE" = c("-", "-", "Yes",'-','-','Yes'),
                                 "Year FE" = c("-", "Yes", "Yes",'-','Yes','Yes')))


