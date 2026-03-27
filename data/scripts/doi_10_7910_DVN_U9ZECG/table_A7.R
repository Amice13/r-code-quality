rm(list = ls())
library(data.table)
library(estimatr)
library(modelsummary)


dados_all <- readRDS(here('data','data_census.rds'))

# No support
dados[, no_support := ifelse(lower_poll == 1 | relative_support == 0, 1, 0)]
dados_test <- dados[p_elected ==0]

dados_test[, squared_support := relative_support^2]
dados_test[, hhi_muni := sum(squared_support), by = 'muni_code']

dados_lm <- unique(dados_test[,.(muni_code,hhi_muni,sc_before,lag_pop, gini, lag_nonwhite, prop_lec, lec_elected)])

models <- list(
  "All"     = lm_robust(hhi_muni ~ sc_before + I(lag_pop/1000) + gini + lag_nonwhite, data = dados_lm),
  "Elected Law-and-order" = lm_robust(hhi_muni ~ sc_before + I(lag_pop/1000) + gini + lag_nonwhite, data = dados_lm[lec_elected == 1])
)

modelsummary(models,
             output = "latex",
             title = 'Vote concentration (HHI) across polling stations and the presence of local security committee',
             coef_rename = c("sc_before" = "Local security committee","I(lag_pop/1000)" = "Population","gini" = "Inequality (gini)","lag_nonwhite" = "Non-white pop."),
             label = "hhi polling stations",
             gof_omit = "R2|R2 Adj.|Std.Errors")


             