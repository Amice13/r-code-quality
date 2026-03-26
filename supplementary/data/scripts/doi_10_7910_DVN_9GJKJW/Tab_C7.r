# ====
# Table C7 Replication:
# PanelMatch balace table for export 
# The results with covariates are saved from Fig_2.r and Fig_C3_C4.r
# Input: PM_balance_NONREF_allexp_aug.RData, PM_balance_allexp_aug.RData,
# R version 4.4.2 (2024-10-31)
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()

library(tidyverse)
library(kableExtra)
library(modelsummary)
library(broom)
`%!in%` <- Negate(`%in%`)



# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

################################################################################
## 0. load results  ------
################################################################################

VERSION <- "allexp_aug"

# without refinements 
load(file = paste0(REPLICATION_DATA_DIR, "PM_balance_NONREF_", VERSION, ".RData"))

# with refinements 
load(file = paste0(REPLICATION_DATA_DIR, "PM_balance_", VERSION, ".RData"))



################################################################################
## 1. format data  ------
################################################################################

# before refinement
Balancedat_before <- as.data.frame(balance.nonref)
Balancedat_before <- tidyr::gather(data.frame(time = rownames(Balancedat_before),
                                            Balancedat_before),
                                key = var, value = SD, -time)

Balancedat_before$refinement <- "Before refinement"
Balancedat_before$var <- gsub("att\\.", "", Balancedat_before$var)

Balancedat_comb_tab_bfr <- Balancedat_before %>%
  mutate(time = paste0(time, "_before")) %>%
  tidyr::spread(time, SD) %>%
  dplyr::select(-refinement)


# after refinement
Balancedat_after <- as.data.frame(Balances[[1]])
Balancedat_after <- tidyr::gather(data.frame(time = rownames(Balancedat_after),
                                            Balancedat_after),
                                key = var, value = SD, -time) 
                                                    
Balancedat_after$refinement <- "After refinement"
Balancedat_after$var <- gsub("att\\.", "", Balancedat_after$var)

Balancedat_comb_tab_aft <- Balancedat_after %>%
  mutate(time = paste0(time, "_after")) %>%
  tidyr::spread(time, SD) %>%
  dplyr::select(-refinement)


# combine
Balancedat_comb_tab <- left_join(Balancedat_comb_tab_bfr, Balancedat_comb_tab_aft, 
                                 by = c("var"))

covslist_balance_nonref <- c("row_exp.ln", "row_imp.ln",
                                "expmean.ln", "impmean.ln",
                                "export_country", "import_country",
                                "intermediate", "gvc_ui", "gvc_di",
                                "rauch_n", "rauch_w", "vnm_avgMFN") 

OUTCOME.NONREF <- "tradevol_exp_aug.ln"

Balancedat_comb_tab <- Balancedat_comb_tab %>%
  filter(var %in% c(OUTCOME.NONREF, covslist_balance_nonref)) %>% 
  mutate(var = dplyr::recode_factor(var,
                                    tradevol_exp_aug.ln = "Export volume (logged)",
                                    expmean.ln = "Mean export volume (logged)",
                                    impmean.ln = "Mean import volume (logged)",
                                    export_country = "Number of countries Vietnam exports to",
                                    import_country = "Number of countries Vietnam imports from",
                                    row_exp.ln = "ROW export volume (logged)",
                                    row_imp.ln = "ROW import volume (logged)",
                                    vnm_avgMFN = "Vietnamese average MFN tariff rate",
                                    intermediate = "Intermediateness",
                                    gvc_ui = "Upstreamness",
                                    gvc_di = "Downstreamness",
                                    rauch_n = "Differentiation (Rauch-N)",
                                    rauch_w = "Homogeneous goods (Rauch-W)")) %>%
  dplyr::rename(Variable = var) %>% 
  arrange(Variable)


# #################################################################
## 1. Balance table -----------------------------------------------
# #################################################################

options(modelsummary_factory_html='kableExtra')
options(modelsummary_factory_latex='kableExtra')
options(modelsummary_factory_markdown='kableExtra')

Balancetab <- datasummary_df(Balancedat_comb_tab %>%
                                dplyr::select(Variable,
                                              `t_4_before`,`t_3_before`,
                                              `t_2_before`, `t_1_before`,
                                              `t_4_after`, `t_3_after`,
                                              `t_2_after`, `t_1_after`),
                             output = "latex")  %>%
  add_header_above(c(" " = 1, "Before refinement" = 4, "After refinement" = 4)) %>%
  row_spec(0, align = "c")

Balancetab <- Balancetab %>% 
  gsub("\\{lllllllll\\}", "\\{lqqqqqqqq\\}", .) %>%
  gsub("_before|_after", "", .) %>%
  gsub("t_1", "$t-1$", .) %>%
  gsub("t_2", "$t-2$", .) %>%
  gsub("t_3", "$t-3$", .) %>%
  gsub("t_4", "$t-4$", .) %>%
  gsub("\\{c\\}\\{Variable\\}", "\\{l\\}\\{Variable\\}", .) %>%
  gsub("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\centering", "", .)

Balancetab

writeLines(Balancetab,
           paste0(REPLICATION_FIG_DIR, "TC7_balance_table_export.tex"))

# html output
Balancetab.html <- datasummary_df(Balancedat_comb_tab %>%
                                dplyr::select(Variable,
                                              `t_4_before`,`t_3_before`,
                                              `t_2_before`, `t_1_before`,
                                              `t_4_after`, `t_3_after`,
                                              `t_2_after`, `t_1_after`),
                             output = "html")  %>%
  add_header_above(c(" " = 1, "Before refinement" = 4, "After refinement" = 4))

Balancetab.html <- Balancetab.html %>% 
  gsub("_before|_after", "", .) %>%
  gsub("t_1", "t-1", .) %>%
  gsub("t_2", "t-2", .) %>%
  gsub("t_3", "t-3", .) %>%
  gsub("t_4", "t-4", .)

writeLines(Balancetab.html,
           paste0(REPLICATION_FIG_DIR, "TC7_balance_table_export.html"))
