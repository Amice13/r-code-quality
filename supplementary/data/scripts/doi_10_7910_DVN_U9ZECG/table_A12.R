
rm(list = ls())
library(googlesheets4)
library(data.table)
library(estimatr)
library(here)
library(modelsummary)
library(kableExtra)

lococ <- readRDS(here('data','social_media.rds'))

lm1 <- lm_robust(credit_claim ~ loc,data = lococ)
lm2 <- lm_robust(police_pic ~ loc,data = lococ)
lm3 <- lm_robust(police_chief_pic ~ loc,data = lococ)

# tables

cm <- c('loc'    = 'Law-and-order police',
        '(Intercept)' = 'Baseline')

models <- list(
  "Credit claim"     = lm1,
  "Post with police" = lm2,
  "Post with police chief"     = lm3)

#Table A12

modelsummary(models,
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             coef_map = cm,
             gof_omit = 'R2|Std.Errors',
             title = 'Credit claiming, connection with police, and police chiefs in social media (Jan/21-Feb/22)') %>%
  footnote(general = 'Robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

