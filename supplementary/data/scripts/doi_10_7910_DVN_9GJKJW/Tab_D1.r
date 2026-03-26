
# ====
# Appendix Table D1 Replication
# Effects of FDI (2003–2014) on Tariff Cuts
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
library(concordance)
library(countrycode)
library(AER)
library(modelsummary)
library(kableExtra)
library(broom)


`%!in%` = Negate(`%in%`)

atable <- function(x){
  return(table(x, useNA = "always"))}

aatable <- function(x, y){
  return(table(x, y, useNA = "always"))}


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

################################################################################
# 1.  Entire time period (2003-14)  -----
################################################################################

## 1.1 load data from Fig 4--------
load(file = paste0(REPLICATION_DATA_DIR, "tobit_dif_fe_sec_korvnm_covars_allyrs.rda"))
tobit.dif.fe.sec.kor.covars.allyrs <- results.list[[1]]
tobit.dif.fe.sec.vnm.covars.allyrs <- results.list[[2]]

summary(tobit.dif.fe.sec.kor.covars.allyrs, vcov = vcovCL(tobit.dif.fe.sec.kor.covars.allyrs, type = "HC0"))
summary(tobit.dif.fe.sec.vnm.covars.allyrs, vcov = vcovCL(tobit.dif.fe.sec.vnm.covars.allyrs, type = "HC0"))

## 1.2 Vietnamese Tariffs (~ FDI Import), controlling for existing cut ------
summary(tariff.panel$pref_mfn_dif) # missing tariffs in 5 products

tobit.dif.fe.sec.vnm.covars.allyrs.prevcut <- tobit(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~
                                              fdi_03_14_dm_imp +
                                              pref_mfn_dif + 
                                              ln_row_exp_03_14 +
                                              ln_row_imp_03_14 +
                                              ln_expmean_03_14 +
                                              ln_impmean_03_14 +
                                              ln_vnmexp_03_14 +
                                              ln_vnmimp_03_14 +
                                              ln_export_country_03_14 +
                                              ln_import_country_03_14 +
                                              rauch_n +
                                              rauch_w +
                                              intermediate +
                                              gvc_ui_03_14 +
                                              gvc_di_03_14 +
                                              factor(HSsection),
                                            left = -Inf,
                                            right = 0,
                                            data = tariff.panel)

summary(tobit.dif.fe.sec.vnm.covars.allyrs.prevcut, vcov = vcovCL(tobit.dif.fe.sec.vnm.covars.allyrs.prevcut, type = "HC0"))

# #########
# 2. Table ------
# #########

tidy_custom.tobit <- function(x, ...) {

  s <- coeftest(x,
                vcov = vcovCL(x, type = "HC0"))

  out <- data.frame(term = names(s[, 1]),
                    std.error = s[, 2],
                    p.value = s[, 4])

  return(out)
}

glance_custom.tobit <- function(x, ...) {
  s <- glance(x)

  out <- data.frame("N.new" = paste("mc", round(s["nobs"], digits = 0), "}"),
                    "BIC.new" = paste("mc", round(s["BIC"], digits = 1), "}"),
                    "Log.Lik.new" = paste("mc", round(s["logLik"], digits = 1), "}"))


  return(out)
}

coef.labels <- c("fdi_03_14_dm_exp" = "FDI (export-related), 2003-14",
                               "fdi_03_14_dm_imp" = "FDI (import-related), 2003-14",
                               "pref_mfn_dif" = "Previous tariff cut (preferential - MFN)",
                               "ln_row_exp_03_14" = "ROW export (logged)",
                               "ln_row_imp_03_14" = "ROW import (logged)",
                               "ln_expmean_03_14" = "Mean export (logged)",
                               "ln_impmean_03_14" = "Mean import (logged)",
                               "ln_vnmexp_03_14" = "Vietnamese export (logged)",
                               "ln_vnmimp_03_14" = "Vietnamese import (logged)",
                               "ln_export_country_03_14" = "Num. of exporting countries (logged)",
                               "ln_import_country_03_14" = "Num. of importing countries (logged)",
                               "rauch_n" = "Rauch-N",
                               "rauch_w" = "Rauch-W",
                               "intermediate" = "Intermediateness",
                               "gvc_ui_03_14" = "Upstreamness",
                               "gvc_di_03_14" = "Downstreamness",
                               "Constant" = "(Intercept)")

options(modelsummary_format_numeric_latex = "plain")
options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_factory_latex_tabular = "kableExtra")
options(modelsummary_factory_markdown = "kableExtra")
options(modelsummary_factory_html = "kableExtra")



tariff.tab.appendix <- modelsummary(list("Korean Tariff" = tobit.dif.fe.sec.kor.covars.allyrs,
                                         "Vietnamese Tariff" = tobit.dif.fe.sec.vnm.covars.allyrs,
                                         "Vietnamese Tariff" = tobit.dif.fe.sec.vnm.covars.allyrs.prevcut),
             stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
             escape = FALSE,
             coef_map = coef.labels,
             output = "latex_tabular",
             gof_map = c("N.new", "BIC.new", "Log.Lik.new")
)

tariff.tab.appendix


# Adjust latex output format
tab.clean <- tariff.tab.appendix %>%
  gsub("lcc", "lqq", .) %>%
  gsub("N.new", "N", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log Likelihood", .) %>%
  gsub("Korean Tariff", "\\\\multicolumn{1}{c}{\\\\text{$\\\\Delta$Korean  tariff}}",.) %>%
  gsub("Vietnamese Tariff", "\\\\multicolumn{1}{c}{\\\\text{$\\\\Delta$Vietnamese  tariff}}", .) %>%
  gsub("\\+", "$+$", .) %>%
  gsub("mc ", "\\\\mc{", .) %>%
  gsub("\\+", "$+$", .) %>%
  gsub("\\\\bottomrule",
       "\\\\bottomrule \\\\multicolumn{4}{r}{Note: Robust standard errors in parentheses.}\\\\\\\\ \\\\multicolumn{4}{r}{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01}", .)

tab.clean <- capture.output(tab.clean)

writeLines(tab.clean, paste0(REPLICATION_FIG_DIR, "TD1_tariff_appendix_table_allyrs.tex"))

# html output
glance_custom.tobit <- function(x, ...) {
  s <- glance(x)

  out <- data.frame("N.new" =  s[["nobs"]],
                    "BIC.new" =  formatC(round(s[["BIC"]],3), digits = 3, format = "f"),
                    "Log.Lik.new" =  formatC(round(s[["logLik"]],3), digits = 3, format = "f"))

  return(out)
}

tariff.tab.appendix.html <-  modelsummary(list("Korean Tariff" = tobit.dif.fe.sec.kor.covars.allyrs,
                                         "Vietnamese Tariff" = tobit.dif.fe.sec.vnm.covars.allyrs,
                                         "Vietnamese Tariff" = tobit.dif.fe.sec.vnm.covars.allyrs.prevcut),
             stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
             escape = FALSE,
             coef_map = coef.labels,
             output = "html",
             gof_map = c("N.new", "BIC.new", "Log.Lik.new")
) %>% 
  kable_styling() %>%
  row_spec(0, bold = TRUE, hline_after = TRUE) %>%
  footnote(general = "Robust standard errors in parentheses. + p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01",  threeparttable = TRUE, escape = FALSE)

tariff.tab.appendix.html


tariff.tab.appendix.html <- tariff.tab.appendix.html %>%
  gsub("lcc", "lqq", .) %>%
  gsub("N.new", "N", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log Likelihood", .) %>% 
  gsub("Korean Tariff", "∆ Korean  tariff",.) %>%
  gsub("Vietnamese Tariff", "∆ Vietnamese  tariff", .)

tariff.tab.appendix.html <- capture.output(tariff.tab.appendix.html)

writeLines(tariff.tab.appendix.html, paste0(REPLICATION_FIG_DIR, "TD1_tariff_appendix_table_allyrs.html"))