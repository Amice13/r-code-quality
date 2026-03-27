# ====
# Table C8 Replication:
# PanelMatch table 
# input (saved results from Fig_2.r): Estimated_results_allexp_aug.RData; Estimated_results_allimp_aug.RData 
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

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

################################################################################
## 0. load results  ------
################################################################################

# Loop over Import/Export
Est.results.plot.list = list()

for (VERSION in c("allexp_aug", "allimp_aug")){

  if (VERSION == "allexp_aug"){
    PLOTTITLE <- "Export"
  }else if (VERSION == "allimp_aug"){
    PLOTTITLE <- "Import"
  }

  print(PLOTTITLE)

  # load estimates
  load(file = paste0(REPLICATION_DATA_DIR, "Estimated_results_", VERSION, ".RData"))
  print(Est.results.all)

  Est.results.plot <- Est.results.all %>%
    filter((time == "t-1" & lag == 2) | (time == "t-1" & lag == 1) |
          (time %in% c("t", "t+1", "t+2", "t+3", "t+4") & lag == 0)) %>%
    mutate(time = ifelse(lag == 2, "t-2", as.character(time))) %>% # placebo 2: code the t-3 vs t-1 comparison as "t-2"
    mutate(time = factor(time,  levels = c("t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4"))) 

  Est.results.plot$version <- PLOTTITLE
  Est.results.plot.list[[VERSION]] <- Est.results.plot

}

Est.results.plot.comb <- bind_rows(Est.results.plot.list)
Est.results.plot.comb

# ############################
## 1. Table --------------------
# ############################

Est.results.plot.comb_tab <- Est.results.plot.comb %>%
  mutate(std.error = paste0("(", sprintf("%.3f", std.error), ")"),
         estimate = sprintf("%.3f", estimate)) %>%
  dplyr::select(time, version, estimate, std.error) %>%
  tidyr::gather(key = "key", value = "value", -version, -time) %>%
  mutate(time_key = paste(time, key, sep = "_")) %>%
  mutate(time_key = recode_factor(time_key,
                                  "t-2_estimate" = "t-2",
                                  "t-2_std.error" = "t-2 std.error",
                                  "t-1_estimate" = "t-1",
                                  "t-1_std.error" = "t-1 std.error",
                                  "t_estimate" = "t",
                                  "t_std.error" = "t std.error",
                                  "t+1_estimate" = "t+1",
                                  "t+1_std.error" = "t+1 std.error",
                                  "t+2_estimate" = "t+2",
                                  "t+2_std.error" = "t+2 std.error",
                                  "t+3_estimate" = "t+3",
                                  "t+3_std.error" = "t+3 std.error",
                                  "t+4_estimate" = "t+4",
                                  "t+4_std.error" = "t+4 std.error"))

Est.results.plot.comb_tab_exp <- Est.results.plot.comb_tab %>%
  filter(version == "Export") %>%
  dplyr::rename(Export = value) %>%
  dplyr::select(time_key, Export)

Est.results.plot.comb_tab_imp <- Est.results.plot.comb_tab %>%
  filter(version == "Import") %>%
  dplyr::rename(Import = value) %>%
  dplyr::select(time_key, Import)

Est.results.plot.comb_tab <- left_join(Est.results.plot.comb_tab_exp,
                                       Est.results.plot.comb_tab_imp,
                                       by = c("time_key")) %>%
  arrange(time_key) %>%
  dplyr::rename(` ` = time_key)

Est.results.plot.comb_tab

# latex table --------------------
PMtab.appendix <- datasummary_df(Est.results.plot.comb_tab,
                                 output = "latex") %>%
  row_spec(0, align = "c") %>%
  footnote(general_title = "Note: Bootstrapped standard errors in parentheses.",
           general = "",
           escape = F)

PMtab.appendix <- PMtab.appendix %>%
  gsub("\\{lll\\}", "{@\\{\\\\hspace\\{0.5cm\\}\\}l@\\{\\\\hspace\\{1.5cm\\}\\}qq@\\{\\\\hspace\\{0.5cm\\}\\}\\}", .) %>%
  gsub("t[-+]. std.error", "", .) %>%
  gsub("t std.error", "", .) %>%
  gsub("t-1", "$t-1$", .) %>%
  gsub("t-2", "$t-2$", .) %>%
  gsub("t &", "$t$ \\&", .) %>%
  gsub("t\\+1", "$t+1$", .) %>%
  gsub("t\\+2", "$t+2$", .) %>%
  gsub("t\\+3", "$t+3$", .) %>%
  gsub("t\\+4", "$t+4$", .) %>%
  gsub("\\\\textit", "", .) %>%
  gsub("\\\\multicolumn\\{3\\}\\{l\\}", "\\\\multicolumn\\{3\\}\\{r\\}", .) %>%
  gsub("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\centering", "", .) #remove table float

PMtab.appendix

writeLines(PMtab.appendix,
           paste0(REPLICATION_FIG_DIR, "TC8_PM_appendix_table.tex"))

# html table --------------------
PMtab.appendix.html <- datasummary_df(Est.results.plot.comb_tab,
                                 output = "html") %>%
  footnote(general_title = "Note: Bootstrapped standard errors in parentheses.",
           general = "",
           escape = F) 

PMtab.appendix.html <- PMtab.appendix.html %>%
  gsub("t[-+]. std.error", "", .) %>%
  gsub("t std.error", "", .)

writeLines(PMtab.appendix.html,
           paste0(REPLICATION_FIG_DIR, "TC8_PM_appendix_table.html"))
