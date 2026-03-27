# ====
# Figure D1 and Table D3 Replication: 
# Effect of FDI on tariff by three year windows
# input: 
#   - tariff_paneldata_withiv.RData
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
library(AER)
library(modelsummary)
library(kableExtra)
library(broom)

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"


################################################################################
## 0. load data  ------
################################################################################
load(file = paste0(REPLICATION_DATA_DIR, "tariff_paneldata_withiv.RData"))
tariff.panel %>% glimpse()

## NOTE: For imports, the FDI-related products are subset to those above median upstreamness


################################################################################
# 1. Estimate 3-year window, joint model--------
################################################################################
## 1.1 Korean Tariffs (~ FDI Export) --------------
summary(tariff.panel$ln_mean_tariff_reduction_dif_kortrff_innegative)
table(tariff.panel$ln_mean_tariff_reduction_dif_kortrff_innegative == 0)

tobit.dif.fe.sec.kor.covars.3ywindow <- tobit(ln_mean_tariff_reduction_dif_kortrff_innegative ~
                                                fdi_03_05_dm_exp_first +
                                                fdi_06_08_dm_exp_first +
                                                fdi_09_11_dm_exp_first +
                                                fdi_12_14_dm_exp_first +
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

summary(tobit.dif.fe.sec.kor.covars.3ywindow)

tobit.dif.fe.sec.kor.covars.3ywindow.out <- coeftest(tobit.dif.fe.sec.kor.covars.3ywindow, 
                                                     vcov = vcovCL(tobit.dif.fe.sec.kor.covars.3ywindow, type = "HC0"))

tobit.dif.fe.sec.kor.covars.3ywindow.out

# extract coefs and SEs
coef.df.tobit.dif.3y.kortrff.joint <- tibble(model = c("2003-2005",
                                                       "2006-2008",
                                                       "2009-2011",
                                                       "2012-2014"),
                                             coef = c(tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Estimate"]["fdi_03_05_dm_exp_first"],
                                                      tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Estimate"]["fdi_06_08_dm_exp_first"],
                                                      tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Estimate"]["fdi_09_11_dm_exp_first"],
                                                      tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Estimate"]["fdi_12_14_dm_exp_first"]),
                                             se = c(tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Std. Error"]["fdi_03_05_dm_exp_first"],
                                                    tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Std. Error"]["fdi_06_08_dm_exp_first"],
                                                    tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Std. Error"]["fdi_09_11_dm_exp_first"],
                                                    tobit.dif.fe.sec.kor.covars.3ywindow.out[,"Std. Error"]["fdi_12_14_dm_exp_first"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.dif.3y.kortrff.joint <- coef.df.tobit.dif.3y.kortrff.joint %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model,
                        levels = c("2003-2005",
                                   "2006-2008",
                                   "2009-2011",
                                   "2012-2014")))
coef.df.tobit.dif.3y.kortrff.joint



## 1.2 Vietnamese Tariffs (~ FDI Import) ------
summary(tariff.panel$ln_mean_tariff_reduction_dif_vnmtrff_innegative)
table(tariff.panel$ln_mean_tariff_reduction_dif_vnmtrff_innegative == 0)

# 3 year window, combined
tobit.dif.fe.sec.vnm.covars.3ywindow <- tobit(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~
                                                fdi_03_05_dm_imp_first +
                                                fdi_06_08_dm_imp_first +
                                                fdi_09_11_dm_imp_first +
                                                fdi_12_14_dm_imp_first +
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

summary(tobit.dif.fe.sec.vnm.covars.3ywindow)

tobit.dif.fe.sec.vnm.covars.3ywindow.out <- coeftest(tobit.dif.fe.sec.vnm.covars.3ywindow, 
                                                     vcov = vcovCL(tobit.dif.fe.sec.vnm.covars.3ywindow, type = "HC0"))
tobit.dif.fe.sec.vnm.covars.3ywindow.out

## Plot 3-year window joint model
coef.df.tobit.dif.3y.vnmtrff.joint <- tibble(model = c("2003-2005",
                                                       "2006-2008",
                                                       "2009-2011",
                                                       "2012-2014"),
                                             coef = c(tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Estimate"]["fdi_03_05_dm_imp_first"],
                                                      tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Estimate"]["fdi_06_08_dm_imp_first"],
                                                      tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Estimate"]["fdi_09_11_dm_imp_first"],
                                                      tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Estimate"]["fdi_12_14_dm_imp_first"]),
                                             se = c(tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Std. Error"]["fdi_03_05_dm_imp_first"],
                                                    tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Std. Error"]["fdi_06_08_dm_imp_first"],
                                                    tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Std. Error"]["fdi_09_11_dm_imp_first"],
                                                    tobit.dif.fe.sec.vnm.covars.3ywindow.out[,"Std. Error"]["fdi_12_14_dm_imp_first"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.dif.3y.vnmtrff.joint <- coef.df.tobit.dif.3y.vnmtrff.joint %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model,
                        levels = c("2003-2005",
                                   "2006-2008",
                                   "2009-2011",
                                   "2012-2014")))
coef.df.tobit.dif.3y.vnmtrff.joint


################################################################################
# 2. Plot D1 --------
################################################################################

axis.title.size <- 22
Y_ARROW <- 0.425
Y_ARROWTEXT <- 0.475

plot.theme <- theme_bw() + theme(legend.position = "none",
                     plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"),
                     axis.title.y = element_text(size = axis.title.size,
                                                 margin = margin(0, 10, 0, 0)),
                     axis.title.x = element_text(size = axis.title.size,
                                                 margin = margin(0, 0, 10, 0)),
                     axis.text.x = element_text(size = axis.title.size,
                                                margin = margin(5, 10, 10, 0),
                                                color = "black"),
                     strip.text = element_text(size = axis.title.size + 2,
                                               face = "bold",
                                               margin = margin(20, 10, 10, 10)),
                     strip.background = element_blank(),
                     panel.grid = element_blank(),
                     panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25))

# korean tariff
p.tobit.dif.3y.kortrff.joint <- ggplot(data = coef.df.tobit.dif.3y.kortrff.joint,
                                          aes(x = model, y = coef,
                                              ymin = lwr,
                                              ymax = upr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) + 
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("Period of the first FDI",
                   labels = c("2003-05", "2006-08", "2009-11", "2012-14")) + 
  scale_y_continuous(TeX("Estimated effect of first FDI on $\\Delta$ tariff"),
                     breaks = round(seq(-0.6, 0.4, 0.2), 1),
                     labels = round(seq(-0.6, 0.4, 0.2), 1),
                     limits = c(-0.65, 0.5)
  ) +　
  ggplot2::annotate("segment", x = 2, xend = 1, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("segment", x = 3, xend = 4, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("text", x = 1.45, y = Y_ARROWTEXT, label = "Older FDI", size = axis.title.size - 14) +
  ggplot2::annotate("text", x = 3.45, y = Y_ARROWTEXT, label = "Recent FDI", size = axis.title.size - 14) +
  plot.theme + 
  theme(axis.text.y = element_text(size = axis.title.size, margin = margin(5, 10, 10, 0), color = "black"))

p.tobit.dif.3y.kortrff.joint

# vietnamese tariff
p.tobit.dif.3y.vnmtrff.joint <- ggplot(data = coef.df.tobit.dif.3y.vnmtrff.joint,
                                          aes(x = model, y = coef,
                                              ymin = lwr,
                                              ymax = upr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) + 
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("Period of the first FDI",
                   labels = c("2003-05", "2006-08", "2009-11", "2012-14")) + 
  scale_y_continuous(" ", 
                     breaks = round(seq(-0.6, 0.4, 0.2), 1),
                     labels = round(seq(-0.6, 0.4, 0.2), 1),
                     limits = c(-0.65, 0.5)) +　
  ggplot2::annotate("segment", x = 2, xend = 1, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("segment", x = 3, xend = 4, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("text", x = 1.45, y = Y_ARROWTEXT, label = "Older FDI", size = axis.title.size - 14) +
  ggplot2::annotate("text", x = 3.45, y = Y_ARROWTEXT, label = "Recent FDI", size = axis.title.size - 14)  + 
  plot.theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p.tobit.dif.3y.vnmtrff.joint

# adjust the width of the panels
p.tobit.dif.3y.kortrff.joint <- ggplotGrob(p.tobit.dif.3y.kortrff.joint)
p.tobit.dif.3y.vnmtrff.joint <- ggplotGrob(p.tobit.dif.3y.vnmtrff.joint)

p.tobit.dif.3y.vnmtrff.joint$widths[6] <- p.tobit.dif.3y.kortrff.joint$widths[6] # Force panel width to match

# save
ggsave(p.tobit.dif.3y.kortrff.joint, 
    file = paste0(REPLICATION_FIG_DIR, "FD1_tariff_3ywindow_kortrff.pdf"), 
    width = 7, height = 6.5)

ggsave(p.tobit.dif.3y.vnmtrff.joint, 
    file = paste0(REPLICATION_FIG_DIR, "FD1_tariff_3ywindow_vnmtrff.pdf"), 
    width = 7, height = 6.5)

###############################################################################
# 3. Table D3 --------
###############################################################################
tidy_custom.tobit <- function(x, ...) {

  s <- coeftest(x,
                vcov = vcovCL(x,  type = "HC0"))

  out <- data.frame(term = names(s[, 1]),
                    std.error = s[, 2],
                    p.value = s[, 4])

  return(out)
}

glance_custom.tobit <- function(x, ...) {
  s <- glance(x)

  out <- data.frame("N.new" =  paste("mc", round(s["nobs"], digits = 0), "}"),
                    "BIC.new" = paste("mc", formatC(round(s[["BIC"]],3), digits = 3, format = "f"), "}"),
                    "Log.Lik.new" = paste("mc", formatC(round(s[["logLik"]],3), digits = 3, format = "f"), "}"))

  return(out)
}


coef.labels <- c("fdi_03_05_dm_exp_first" = "FDI (export-related), 2003-05",
                               "fdi_06_08_dm_exp_first" = "FDI (export-related), 2005-08",
                               "fdi_09_11_dm_exp_first" = "FDI (export-related), 2009-11",
                               "fdi_12_14_dm_exp_first" = "FDI (export-related), 2012-14",
                               "fdi_03_05_dm_imp_first" = "FDI (import-related), 2003-05",
                               "fdi_06_08_dm_imp_first" = "FDI (import-related), 2005-08",
                               "fdi_09_11_dm_imp_first" = "FDI (import-related), 2009-11",
                               "fdi_12_14_dm_imp_first" = "FDI (import-related), 2012-14",
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

tariff.tab.appendix <- modelsummary(list("Korean Tariff" = tobit.dif.fe.sec.kor.covars.3ywindow,
                                         "Vietnamese Tariff" = tobit.dif.fe.sec.vnm.covars.3ywindow),
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
  gsub("Korean Tariff", "\\\\multicolumn{1}{c}{\\\\text{$\\\\Delta$Korean  Tariff}}",.) %>%
  gsub("Vietnamese Tariff", "\\\\multicolumn{1}{c}{\\\\text{$\\\\Delta$Vietnamese  Tariff}}", .) %>%
  gsub("\\+", "$+$", .) %>%
  gsub("mc ", "\\\\mc{", .) %>%
  gsub("\\+", "$+$", .) %>%
  gsub("\\\\bottomrule",
       "\\\\bottomrule \\\\multicolumn{3}{r}{Note: Robust standard errors in parentheses.}\\\\\\\\ \\\\multicolumn{3}{r}{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01}", .)

tab.clean <- capture.output(tab.clean)

tab.clean
writeLines(tab.clean, paste0(REPLICATION_FIG_DIR, "TD3_tariff_appendix_table.tex"))

# html output
glance_custom.tobit <- function(x, ...) {
  s <- glance(x)

  out <- data.frame("N.new" =  s[["nobs"]],
                    "BIC.new" =  formatC(round(s[["BIC"]],3), digits = 3, format = "f"),
                    "Log.Lik.new" =  formatC(round(s[["logLik"]],3), digits = 3, format = "f"))
                    
  return(out)
}

tariff.tab.appendix.html <- modelsummary(list("Korean Tariff" = tobit.dif.fe.sec.kor.covars.3ywindow,
                                         "Vietnamese Tariff" = tobit.dif.fe.sec.vnm.covars.3ywindow),
             stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
             escape = FALSE,
             coef_map = coef.labels,
             output = "html",
             gof_map = c("N.new", "BIC.new", "Log.Lik.new")
) %>% 
  kable_styling() %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = "Robust standard errors in parentheses.", threeparttable = TRUE) 

tariff.tab.appendix.html


tariff.tab.appendix.html <- tariff.tab.appendix.html %>%
  gsub("lcc", "lqq", .) %>%
  gsub("N.new", "N", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log Likelihood", .)

tariff.tab.appendix.html <- capture.output(tariff.tab.appendix.html)

writeLines(tariff.tab.appendix.html, paste0(REPLICATION_FIG_DIR, "TD3_tariff_appendix_table.html"))
