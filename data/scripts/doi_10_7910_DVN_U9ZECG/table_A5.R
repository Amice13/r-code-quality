rm(list = ls())
library(data.table)
library(rdrobust)
library(here)
library(modelsummary)
library(tidyr)
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
      Bandwidth = paste(round(model$bws[2,2],1)),
      #Bw.Selection = model$bwselect,
      N.obs = as.character(model$N_h[[1]] + model$N_h[[2]])
    )
    ret
  }

# Load Data
  
load(here("data","ver_data.RData"))


tt <- unique(ver_b[law_enforcement ==1 & is.finite(marginal_muni_raw), 
                 .(muni_code, year, law_enforcement, marginal_muni_raw, marginal_muni, elected_muni, p_elected_muni, dv_homicides, y2008, y2012)])

median_001 <- median(abs(tt[abs(marginal_muni) < 0.01]$marginal_muni_raw), na.rm = TRUE)
mean_001 <- mean(abs(tt[abs(marginal_muni) < 0.01]$marginal_muni_raw), na.rm = TRUE)

tt <- tt[abs(marginal_muni_raw) < mean_001]
tt <- tt[abs(marginal_muni) < 0.01]

# dv_homicides ----
  
  # Benchmark
    robust_all <- rdrobust(tt$dv_homicides, tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                         covs = cbind(tt$y2008,tt$y2012))

  # MSERD
    robust_mserd <- rdrobust(tt$dv_homicides,tt$marginal_muni_raw, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
  # No previous LOC
  
    tt_a <- tt[law_enforcement == 1 & p_elected_muni == 0 & is.finite(marginal_muni_raw) & !is.na(dv_homicides)]
    robust_p <- rdrobust(tt_a$dv_homicides,tt_a$marginal_muni_raw, all = TRUE, bwselect = 'msetwo', 
                         covs = cbind(tt_a$y2008,tt_a$y2012))
    
  # second-degree polynomial

    robust_2 <- rdrobust(tt$dv_homicides,tt$marginal_muni_raw, all = TRUE, p = 2,
                           bwselect = 'msetwo', 
                           covs = cbind(tt$y2008,tt$y2012))
    
    table_homicides <- list('Benchmark' = robust_all, 
                              'MSE-optimal' = robust_mserd, 
                              '2nd Polynomial' = robust_2, 
                              'No previous L&O' = robust_p)

  modelsummary(table_homicides, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Homicides (raw vote margin)') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses. Includes municipalities whose raw vote margin is smaller than the median of municipalities within 0.01 percent margin (or smaller than 110 votes)', threeparttable = TRUE)

