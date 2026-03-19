
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

tt <- unique(ver_b[abs(marginal_muni_pm) < 0.01  & law_enforcement == 1 & pm ==1 & sc_before == 1, .(muni_code, year, marginal_muni_pm, p_elected_muni, dv_homicides, y2008, y2012)])

tt_a <- tt[p_elected_muni==0]

# robbery ----

# All municipalities

robust_all <- rdrobust(tt$dv_homicides, tt$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))
summary(robust_all)

# MSERD

robust_mserd <- rdrobust(tt$dv_homicides,tt$marginal_muni_pm, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC

robust_p <- rdrobust(tt_a$dv_homicides, tt_a$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008, tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$dv_homicides,tt$marginal_muni_pm, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

table_police_sc <- list('All municipalities' = robust_all, 
                      'MSE-optimal' = robust_mserd, 
                      '2nd Polynomial' = robust_2, 
                      'No Previous L&O' = robust_p)




modelsummary(table_police_sc, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of electing a police law-and-order candidate on homicides in municipalities with a security committee') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
