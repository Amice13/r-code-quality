#Please restart R
#options(scipen=999)

rm(list = ls())
library(estimatr)
library(data.table)
library(here)
library(modelsummary)
library(tidyr)
library(ggplot2)

local <- readRDS(here("data","local.rds"))

table.data <- function(model, dv, ...) {
  ret_a <- data.frame(
    dependent_var = paste(dv),
    group = 'Security Committee',
    avg = model$coefficients[[2]] + model$coefficients[[1]],
    u_ci = model$coefficients[[1]] + model$conf.high[[2]],
    l_ci = model$coefficients[[1]] + model$conf.low[[2]]
  )
  ret_b <-data.frame(
    dependent_var = paste(dv),
    group = 'No Security Committee',
    avg = model$coefficients[[1]],
    u_ci = model$conf.high[[1]],
    l_ci = model$conf.low[[1]]
  )
  ret <- rbind(ret_a, ret_b)
  row.names(ret) <- NULL
  ret
}

reg2 <- lm_robust(fundo_sp ~ sc  , data = local)
reg3 <- lm_robust(plano_sp ~ sc , data = local)
reg4 <- lm_robust(conseg ~ sc , data = local)
reg5 <- lm_robust(I(conseg + fundo_sp + plano_sp ) ~ sc , data = local)

plot_data <- rbind(table.data(reg2, 'Dedicated\nbudget'),
                   table.data(reg3, 'Multi-year\nplan'),
                   table.data(reg4, 'Community\ncouncils'),
                   table.data(reg5, 'Sum of\nMeasures'))

table_munic <- list('Dedicated budget' = reg2, 
                         'Multi-year plan' = reg3, 
                         'Community council' = reg4, 
                         'Sum of Measures' = reg5)

# THIS PRODUCES TABLE A.4

modelsummary(table_munic, gof_omit = 'DF|Deviance|R2|AIC|BIC', output = 'latex')

