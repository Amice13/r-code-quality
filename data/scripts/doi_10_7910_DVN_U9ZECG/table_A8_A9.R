rm(list = ls())

library(texreg)
library(data.table)
library(here)
library(estimatr)


urna <- readRDS(here("data","data_census.rds"))
urna <- urna[p_elected ==0 & lec_elected ==1]
# plots ----


m1 <- lm_robust(z.prop_local_total_college ~ zrich, data = urna)
m2 <- lm_robust(z.prop_local_total_college ~ zrich, data = urna, fixed_effects = ~ muni_code)
m3 <- lm_robust(z.prop_local_total_college ~ zpoor, data = urna)
m4 <- lm_robust(z.prop_local_total_college ~ zpoor, data = urna, fixed_effects = ~ muni_code)

texreg(list(m1, m2, m3, m4), 
       include.ci = FALSE,
       omit.coef = 'as.factor',
       stars = c(0.01,0.05,0.1),digits = 3,
       custom.model.names = c('[1]','[2]','[3]','[4]'),
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE, 
       custom.coef.map = list("zrich" = 'Rich neighborhood', "zpoor" = "Poor neighborhood","(Intercept)" = "Intercept"),
       custom.gof.rows = list("Municipal FE" = c("-", "Yes", "-",'Yes')))


mm1 <- lm_robust(z.prop_local_total_low_school ~ zrich, data = urna)
mm2 <- lm_robust(z.prop_local_total_low_school ~ zrich, data = urna, fixed_effects = ~ muni_code)
mm3 <- lm_robust(z.prop_local_total_low_school ~ zpoor, data = urna)
mm4 <- lm_robust(z.prop_local_total_low_school ~ zpoor, data = urna, fixed_effects = ~ muni_code)
mm5 <- lm_robust(z.prop_local_total_low_school ~ z.prop_alfabetizado_censo, data = urna)
mm6 <- lm_robust(z.prop_local_total_low_school ~ z.prop_alfabetizado_censo, data = urna, fixed_effects = ~ muni_code)

texreg(list(mm1, mm2, mm3, mm4,mm5,mm6), 
          include.ci = FALSE,
          omit.coef = 'as.factor',
          stars = c(0.01,0.05,0.1),digits = 3,
          custom.model.names = c('[1]','[2]','[3]','[4]','[5]','[6]'),
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          include.rmse = FALSE,
          label = 'table:correlation census tse low education',
          caption =  "Relationship between \\textbf{Low educational attainment} (electoral data), income and educational data (census)",
          custom.coef.map = list("zrich" = 'Rich neighborhood', "zpoor" = "Poor neighborhood",'z.prop_alfabetizado_censo' = 'Literacy',"(Intercept)" = "Intercept"),
          custom.gof.rows = list("Municipal FE" = c("-", "Yes", "-",'Yes', "-",'Yes')))
