
rm(list = ls())
library(data.table)
library(rdrobust)
library(here)
library(modelsummary)
library(kableExtra)

# Load functions for modelsummary 

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = 'Robust Coef.',
    estimate = model$coef[3, 1],
    std.error = model$se[3, 1],
    p.value = model$pv[3, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    #Kernel = model$kernel,
    Bandwidth = paste(round(model$bws[2,2],4) * 100,'%'),
    #Bw.Selection = model$bwselect,
    N.obs = as.character(model$N_h[[1]] + model$N_h[[2]])
  )
  ret
}

# Load Data

load(here("data","ver_data.RData"))

tt <- unique(ver_b[abs(marginal_muni_pm) < 0.01  & law_enforcement == 1 & pm ==1, .(muni_code, year, marginal_muni_pm, p_elected_muni, dv_pm_rais, robbery_pc, theft_pc, rape_pc, latro_pc, y2008, y2012, lag_robbery_pc, lag_theft_pc, lag_rape_pc, lag_latro_pc)])

tt_a <- tt[p_elected_muni==0]

# robbery ----

# All municipalities

robust_all <- rdrobust(tt$robbery_pc - tt$lag_robbery_pc, tt$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))
summary(robust_all)

# MSERD

robust_mserd <- rdrobust(tt$robbery_pc - tt$lag_robbery_pc,tt$marginal_muni_pm, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC

robust_p <- rdrobust(tt_a$robbery_pc - tt_a$lag_robbery_pc, tt_a$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008, tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$robbery_pc - tt$lag_robbery_pc,tt$marginal_muni_pm, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

table_robbery <- list('All municipalities' = robust_all, 
                      'MSE-optimal' = robust_mserd, 
                      '2nd Polynomial' = robust_2, 
                      'No Previous L&O' = robust_p)


# theft ----

# All municipalities

robust_all <- rdrobust(tt$theft_pc - tt$lag_theft_pc, tt$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))

# MSERD

robust_mserd <- rdrobust(tt$theft_pc - tt$lag_theft_pc,tt$marginal_muni_pm, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC

robust_p <- rdrobust(tt_a$theft_pc - tt_a$lag_theft_pc, tt_a$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008, tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$theft_pc - tt$lag_theft_pc,tt$marginal_muni_pm, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

table_theft <- list('All municipalities' = robust_all, 
                      'MSE-optimal' = robust_mserd, 
                      '2nd Polynomial' = robust_2, 
                      'No Previous L&O' = robust_p)

# sexual assault ----

# All municipalities

robust_all <- rdrobust(tt$rape_pc - tt$lag_rape_pc, tt$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))

# MSERD

robust_mserd <- rdrobust(tt$rape_pc-tt$lag_rape_pc,tt$marginal_muni_pm, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC

robust_p <- rdrobust(tt_a$rape_pc - tt_a$lag_rape_pc, tt_a$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008, tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$rape_pc - tt$lag_rape_pc,tt$marginal_muni_pm, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

table_sexual_assault <- list('All municipalities' = robust_all, 
                             'MSE-optimal' = robust_mserd, 
                             '2nd Polynomial' = robust_2, 
                             'No previous L&O' = robust_p)



# latrocinio ----

# All municipalities
robust_all <- rdrobust(tt$latro_pc - tt$lag_latro_pc, tt$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))

# MSERD

robust_mserd <- rdrobust(tt$latro_pc - tt$lag_latro_pc,tt$marginal_muni_pm, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC


robust_p <- rdrobust(tt_a$latro_pc - tt_a$lag_latro_pc, tt_a$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008, tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$latro_pc - tt$lag_latro_pc,tt$marginal_muni_pm, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

table_latrocinio <- list('All municipalities' = robust_all, 
                       'MSE-optimal' = robust_mserd, 
                       '2nd Polynomial' = robust_2, 
                       'No Previous L&O' = robust_p)

# table B1
modelsummary(table_robbery, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of electing a law-and-order candidate on strong-arm car robbery') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

#Table B2
modelsummary(table_theft, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of electing a police law-and-order candidate on theft') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

#Table B3
modelsummary(table_sexual_assault, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of electing a police law-and-order candidate on sexual assault') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

#Table B4
modelsummary(table_latrocinio, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of electing a police law-and-order candidate on robbery homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
 