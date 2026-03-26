# ====
# Table B2 Replication:
# Summary statistics for the intensive margin
# input: intensive_margin_paneldata.RData
# R version 4.4.2 (2024-10-31)
# ====

# clean slate
rm(list = ls())
date()

library(tidyverse)
library(stargazer)

# set main directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"


################################################################################
## 0. load data  ------
################################################################################
load(file = paste0(REPLICATION_DATA_DIR, "intensive_margin_paneldata.RData"))
data.panel.pm %>% glimpse()
summary(data.panel.pm)


################################################################################
## sumary table -----
################################################################################
dat.summary <- data.panel.pm %>%
            dplyr::select("tradevol_exp_aug.ln", "tradevol_imp_aug.ln",
                          "fdi_count_dm_exp.once",
                          "fdi_count_dm_imp.once",
                          "row_exp.ln", "row_imp.ln",
                          "expmean.ln",  "impmean.ln",
                          "export_country", "import_country",
                          "vnm_avgMFN",
                          "intermediate",
                          "gvc_ui", "gvc_di",
                          "rauch_n", "rauch_w")
summary(dat.summary)

# latex table
stargazer(dat.summary,
          covariate.labels = c("Export volume (logged)",
                               "Import volume (logged)",
                               "Export-related FDI (cumulative binary, Customs)", 
                               "Import-related FDI (cumulative binary, Customs)", 
                               "ROW export volume (logged)",
                               "ROW import volume (logged)",
                               "Mean export (logged)",
                               "Mean import (logged)",
                               "Number of countries Vietnam exports to",
                               "Number of countries Vietnam imports from",
                               "Vietnamese average MFN tariff rate",
                               "Intermediateness",
                               "Upstreamness", "Downstreamness",
                               "Differentiation (Rauch-N)",
                               "Homogeneous goods (Rauch-W)"),
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          float = F,
          out = paste0(REPLICATION_FIG_DIR, "TB2_summary_stats_HS6_panel.tex"))

# html table
stargazer(dat.summary,
          covariate.labels = c("Export volume (logged)",
                               "Import volume (logged)",
                               "Export-related FDI (cumulative binary, Customs)", 
                               "Import-related FDI (cumulative binary, Customs)", 
                               "ROW export volume (logged)",
                               "ROW import volume (logged)",
                               "Mean export (logged)",
                               "Mean import (logged)",
                               "Number of countries Vietnam exports to",
                               "Number of countries Vietnam imports from",
                               "Vietnamese average MFN tariff rate",
                               "Intermediateness",
                               "Upstreamness", "Downstreamness",
                               "Differentiation (Rauch-N)",
                               "Homogeneous goods (Rauch-W)"),
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),
          type = "html",
          out = paste0(REPLICATION_FIG_DIR, "TB2_summary_stats_HS6_panel.html"))
