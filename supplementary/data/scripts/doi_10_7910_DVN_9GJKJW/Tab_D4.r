
# ====
# Appendix Table D4 Replication
# Number of Products Newly Associated with FDI Projects in Vietnam
# input: tariff_paneldata_withiv.RData
# R version 4.4.2 (2024-10-31)
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(xtable)

# set main directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

################################################################################
## 0. load data  ------
################################################################################

# load data --------
load(file = paste0(REPLICATION_DATA_DIR, "tariff_paneldata_withiv.RData"))
tariff.panel %>% glimpse()

## NOTE: For imports, the FDI-related products are subset to those above median upstreamness

# count the number of products associated with FDI projects by year
exp_yearly <- tariff.panel %>% count(fdi_dm_exp_first_2003,
                                   fdi_dm_exp_first_2004,
                                   fdi_dm_exp_first_2005,
                                   fdi_dm_exp_first_2006,
                                   fdi_dm_exp_first_2007,
                                   fdi_dm_exp_first_2008,
                                   fdi_dm_exp_first_2009,
                                   fdi_dm_exp_first_2010,
                                   fdi_dm_exp_first_2011,
                                   fdi_dm_exp_first_2012,
                                   fdi_dm_exp_first_2013,
                                   fdi_dm_exp_first_2014)  %>%
  mutate(across(!n, function(x) ifelse(x == 1, deparse(substitute(x)), NA))) %>% 
  tidyr::gather(key, value = fdi, -n) %>%
  filter(!is.na(fdi)) %>%
  dplyr::select(fdi, n) %>%
  mutate(fdi = dplyr::recode(fdi,
                             fdi_dm_exp_first_2003 = "2003",
                             fdi_dm_exp_first_2004 = "2004",
                             fdi_dm_exp_first_2005 = "2005",
                             fdi_dm_exp_first_2006 = "2006",
                             fdi_dm_exp_first_2007 = "2007",
                             fdi_dm_exp_first_2008 = "2008",
                             fdi_dm_exp_first_2009 = "2009",
                             fdi_dm_exp_first_2010 = "2010",
                             fdi_dm_exp_first_2011 = "2011",
                             fdi_dm_exp_first_2012 = "2012",
                             fdi_dm_exp_first_2013 = "2013",
                             fdi_dm_exp_first_2014 = "2014")) %>%
  dplyr::rename(Year = fdi,
                `Export-Related FDI` = n)

imp_yearly <- tariff.panel %>% count(fdi_dm_imp_first_2003,
                                   fdi_dm_imp_first_2004,
                                   fdi_dm_imp_first_2005,
                                   fdi_dm_imp_first_2006,
                                   fdi_dm_imp_first_2007,
                                   fdi_dm_imp_first_2008,
                                   fdi_dm_imp_first_2009,
                                   fdi_dm_imp_first_2010,
                                   fdi_dm_imp_first_2011,
                                   fdi_dm_imp_first_2012,
                                   fdi_dm_imp_first_2013,
                                   fdi_dm_imp_first_2014)  %>%
  mutate(across(!n, function(x) ifelse(x == 1, deparse(substitute(x)), NA))) %>% 
  tidyr::gather(key, value = fdi, -n) %>% 
  filter(!is.na(fdi)) %>% dplyr::select(fdi, n) %>% 
  mutate(fdi = dplyr::recode(fdi,
                             fdi_dm_imp_first_2003 = "2003",
                             fdi_dm_imp_first_2004 = "2004",
                             fdi_dm_imp_first_2005 = "2005",
                             fdi_dm_imp_first_2006 = "2006",
                             fdi_dm_imp_first_2007 = "2007",
                             fdi_dm_imp_first_2008 = "2008",
                             fdi_dm_imp_first_2009 = "2009",
                             fdi_dm_imp_first_2010 = "2010",
                             fdi_dm_imp_first_2011 = "2011",
                             fdi_dm_imp_first_2012 = "2012",
                             fdi_dm_imp_first_2013 = "2013",
                             fdi_dm_imp_first_2014 = "2014")) %>%
  dplyr::rename(Year = fdi,
                `Import-Related FDI` = n)

expimp_yearly <- full_join(exp_yearly, imp_yearly, by = "Year")
expimp_yearly

print(xtable(expimp_yearly, type = "latex"),
             floating = FALSE, include.rownames = FALSE,
             file = paste0(REPLICATION_FIG_DIR, "TD4_tariff_treated_obs.tex"))

# # html
print(xtable(expimp_yearly), type = "html",
            include.rownames = FALSE,
            file = paste0(REPLICATION_FIG_DIR, "TD4_tariff_treated_obs.html"))
