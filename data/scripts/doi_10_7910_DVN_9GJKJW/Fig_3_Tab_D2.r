# ====
# Figure 3 and Table D2 Replication:
# Aggregate effect of FDI on tariff
# input:
#   - tariff_paneldata_withiv.RData
# intermediary output:
#   - tobit_dif_fe_sec_korvnm_covars_allyrs.rda
#   - coef_df_tobit_dif_allyrs_korvnmtrff_forplot.rda
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
library(latex2exp)
library(boot)
library(censReg)
library(modelsummary)
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
## 1.  Entire time period (2003-14): Tobit  -----
################################################################################
## 1.1 Korean Tariffs (~ FDI Export) ----------
tobit.dif.fe.sec.kor.covars.allyrs <- tobit(ln_mean_tariff_reduction_dif_kortrff_innegative ~
                                                fdi_03_14_dm_exp +
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

summary(tobit.dif.fe.sec.kor.covars.allyrs)

# heteroskedasticity-robust standard errors
tobit.dif.fe.sec.kor.covars.allyrs.out <- coeftest(tobit.dif.fe.sec.kor.covars.allyrs, 
                                                     vcov = vcovCL(tobit.dif.fe.sec.kor.covars.allyrs, 
                                                                   type = "HC0"))
tobit.dif.fe.sec.kor.covars.allyrs.out

# extract coefs and SEs
coef.df.tobit.dif.allyrs.kortrff <- tibble(model = c("Korean tariff"),
                                          coef = c(tobit.dif.fe.sec.kor.covars.allyrs.out[,"Estimate"]["fdi_03_14_dm_exp"]),
                                          se = c(tobit.dif.fe.sec.kor.covars.allyrs.out[,"Std. Error"]["fdi_03_14_dm_exp"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.dif.allyrs.kortrff <- coef.df.tobit.dif.allyrs.kortrff %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model))

coef.df.tobit.dif.allyrs.kortrff


## 1.2 Vietnamese Tariffs (~ FDI Import) ------

tobit.dif.fe.sec.vnm.covars.allyrs <- tobit(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~
                                              fdi_03_14_dm_imp +
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

summary(tobit.dif.fe.sec.vnm.covars.allyrs)

tobit.dif.fe.sec.vnm.covars.allyrs.out <- coeftest(tobit.dif.fe.sec.vnm.covars.allyrs, 
                                                   vcov = vcovCL(tobit.dif.fe.sec.vnm.covars.allyrs, 
                                                                #  cluster = cluster_level, 
                                                                 type = "HC0"))
tobit.dif.fe.sec.vnm.covars.allyrs.out 

# extract coefs and SEs
coef.df.tobit.dif.allyrs.vnmtrff <- tibble(model = c("Vietnamese tariff"),
                                           coef = c(tobit.dif.fe.sec.vnm.covars.allyrs.out[,"Estimate"]["fdi_03_14_dm_imp"]),
                                           se = c(tobit.dif.fe.sec.vnm.covars.allyrs.out[,"Std. Error"]["fdi_03_14_dm_imp"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.dif.allyrs.vnmtrff <- coef.df.tobit.dif.allyrs.vnmtrff %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model))

coef.df.tobit.dif.allyrs.vnmtrff


# # # save for appendix table and figures -----
results.list <- list(tobit.dif.fe.sec.kor.covars.allyrs, tobit.dif.fe.sec.vnm.covars.allyrs)
save(results.list , file = paste0(REPLICATION_DATA_DIR, "tobit_dif_fe_sec_korvnm_covars_allyrs.rda"))

## 1.3 Combine ------
coef.df.tobit.dif.allyrs.korvnmtrff <- bind_rows(
  coef.df.tobit.dif.allyrs.kortrff[c("model", "coef", "lwr", "upr")], 
  coef.df.tobit.dif.allyrs.vnmtrff[c("model", "coef", "lwr", "upr")])

coef.df.tobit.dif.allyrs.korvnmtrff <- coef.df.tobit.dif.allyrs.korvnmtrff %>% 
  mutate(model = factor(model,
         levels = c("Korean tariff",
                    "Vietnamese tariff")))



###############################################################################
## 2.  Entire time period (2003-14): Tobit IV -----
##############################################################################
################################################################################
# 2.1  Korean Tariff --------
################################################################################
## first stage as discrete variable-------
first_stage.kor.probit <- glm(fdi_03_14_dm_exp ~ FDI_nonvnm_03_14_ave_export + 
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
                                                family = binomial("probit"),
                                                data = tariff.panel)

summary(first_stage.kor.probit)
summary((fitted(first_stage.kor.probit)))
summary(residuals(first_stage.kor.probit))

## second stage tobit ----------------
consistent.tobit.prob <- censReg(ln_mean_tariff_reduction_dif_kortrff_innegative ~ 
                                    fitted(first_stage.kor.probit) +
                                    residuals(first_stage.kor.probit) +
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
summary(consistent.tobit.prob)

# bootstrapped standard errors
# bootstrap_run <- FALSE # set to false to use saved results
bootstrap_run <- TRUE 

if (bootstrap_run == TRUE){
  tobit_2siv_coef.kor.prob <- function(data, indices){

    d <- data[indices, ]

    reduced.form <- glm(fdi_03_14_dm_exp ~ FDI_nonvnm_03_14_ave_export +
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
                                          family = binomial("probit"),
                                          data = d)
                                                  
    consistent.tobit.prob <- censReg(ln_mean_tariff_reduction_dif_kortrff_innegative ~ 
                                      fitted(reduced.form) +
                                      residuals(reduced.form) +
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
                                      data = d,
                                      left = -Inf,
                                      right = 0)

    return(summary(consistent.tobit.prob)$estimate["fitted(reduced.form)", 1])
  }


  set.seed(08540)
  boot.results.wres.kor.prob <- boot(data = tariff.panel,
                                statistic = tobit_2siv_coef.kor.prob,
                                R = 1000)

  save(boot.results.wres.kor.prob, file = paste0(REPLICATION_DATA_DIR, "boot_results_wres_instrumentH3_kortrff_probit_VNMcustoms_1000itr.RData"))

  boot.results.wres.kor.prob
  boot.ci(boot.results.wres.kor.prob, type = "perc")$percent


  coef.df.tobit.dif.allyrs.kortrff.iv<- tibble(model = c("Korean tariff (IV, probit)"), coef = boot.results.wres.kor.prob[[1]]) %>%
        mutate(lwr = boot.ci(boot.results.wres.kor.prob, type = "perc")$percent[4],
               upr = boot.ci(boot.results.wres.kor.prob, type = "perc")$percent[5],
               model = factor(model))

  coef.df.tobit.dif.allyrs.kortrff.iv

  save(coef.df.tobit.dif.allyrs.kortrff.iv, 
      file = paste0(REPLICATION_DATA_DIR, "coef_df_tobit_dif_allyrs_kortrff_iv_probit_VNMcustoms_1000itr.rda"))

}else{
  load(file = paste0(REPLICATION_DATA_DIR, "coef_df_tobit_dif_allyrs_kortrff_iv_probit_VNMcustoms_1000itr.rda"))

}



################################################################################
# 2.2  Vietnamese Tariff --------
################################################################################
## first stage as discrete variable-------
# ## 2.2 first stage: probit --------
first_stage.vnm.prob <- glm(fdi_03_14_dm_imp ~ FDI_nonvnm_03_14_ave_import +
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
                                                family = binomial("probit"),
                                                data = tariff.panel)

summary(first_stage.vnm.prob) 
summary((fitted(first_stage.vnm.prob)))
summary(residuals(first_stage.vnm.prob))


consistent.tobit.vnm.prob <- censReg(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~ fitted(first_stage.vnm.prob) + residuals(first_stage.vnm.prob)+ 
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
summary(consistent.tobit.vnm.prob)

# # bootstrapped standard errors

# bootstrap_run <- FALSE # set to false to use saved results
bootstrap_run <- TRUE 

if (bootstrap_run == TRUE){

  tobit_2siv_coef.vnm.prob <- function(data, indices){

    d <- data[indices, ]
    
    reduced.form <- glm(fdi_03_14_dm_imp ~ FDI_nonvnm_03_14_ave_import + 
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
                                                  family = binomial(probit),
                                                  data = d)
                                                  
    consistent.tobit.vnm <- censReg(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~ 
                                                  fitted(reduced.form) +
                                                  residuals(reduced.form) +
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
                                                  data = d,
                                                  left = -Inf,
                                                right = 0)

  return(summary(consistent.tobit.vnm)$estimate["fitted(reduced.form)", 1])

  }

  set.seed(08540)

  boot.results.wres.vnm.prob <- boot(data = tariff.panel,
                                    statistic = tobit_2siv_coef.vnm.prob, 
                                    R = 1000)

  save(boot.results.wres.vnm.prob, file = paste0(REPLICATION_DATA_DIR, "boot_results_wres_instrumentH3_vnmtrff_probit_VNMcustoms_1000itr.RData"))

  boot.results.wres.vnm.prob
  boot.ci(boot.results.wres.vnm.prob, type = "perc")$percent

  coef.df.tobit.dif.allyrs.vnmtrff.iv <- tibble(model = c("Vietnamese tariff (IV), probit"), coef = boot.results.wres.vnm.prob[[1]]) %>%
        mutate(lwr = boot.ci(boot.results.wres.vnm.prob, type = "perc")$percent[4],
              upr = boot.ci(boot.results.wres.vnm.prob, type = "perc")$percent[5],
              model = factor(model))

  coef.df.tobit.dif.allyrs.vnmtrff.iv

  save(coef.df.tobit.dif.allyrs.vnmtrff.iv, 
  file = paste0(REPLICATION_DATA_DIR, "coef_df_tobit_dif_allyrs_vnmtrff_iv_probit_VNMcustoms_1000itr.rda")) 

}else{
  load(file = paste0(REPLICATION_DATA_DIR, "coef_df_tobit_dif_allyrs_vnmtrff_iv_probit_VNMcustoms_1000itr.rda"))
}


################################################################################
# 3. Plot  --------
###############################################################################

# combine results
coef.df.tobit.dif.allyrs.korvnmtrff.all <- bind_rows(coef.df.tobit.dif.allyrs.korvnmtrff, coef.df.tobit.dif.allyrs.kortrff.iv, coef.df.tobit.dif.allyrs.vnmtrff.iv)

coef.df.tobit.dif.allyrs.korvnmtrff.all$estimation <- c("Tobit", "Tobit",  "Tobit (IV)", "Tobit (IV)")

coef.df.tobit.dif.allyrs.korvnmtrff.all$outcome <- c("Korean tariff", "Vietnamese tariff",  "Korean tariff", "Vietnamese tariff")


axis.title.size <- 20

plot.allyear.korvnmtrff.comb <- ggplot(data = coef.df.tobit.dif.allyrs.korvnmtrff.all, 
                                      aes(x = outcome, y = coef, shape = estimation)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 1.2, alpha = 0.8, position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous(TeX("Estimated effect of FDI (2003-14) on $\\Delta$ tariff"),
                     breaks = round(seq(-0.6, 0.4, 0.2), 1),
                     labels = round(seq(-0.6, 0.4, 0.2), 1),
                     limits = c(-0.8, 0.5)) +
  theme_bw() +
  guides(shape = guide_legend(keyheight = 3)) +
  theme(legend.position = c(0.85, 0.85),
        legend.text = element_text(size = axis.title.size),
        legend.title = element_blank(),
        plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"),
        axis.title.y = element_text(size = axis.title.size + 1,
                                    margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = axis.title.size + 2,
                                    margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = axis.title.size + 2,
                                   margin = margin(10, 5, 10, 0),
                                   color = "black"),
        axis.text.x = element_text(size = axis.title.size,
                                   margin = margin(5, 10, 10, 0),
                                   color = "black"),
        strip.text = element_text(size = axis.title.size + 6,
                                  face = "bold",
                                  margin = margin(20, 10, 10, 10)),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25))

plot.allyear.korvnmtrff.comb


ggsave(plot.allyear.korvnmtrff.comb, 
  file = paste0(REPLICATION_FIG_DIR, "F3_tariff_aggregated_years.pdf"),
  width = 8, height = 7.5)

# ########################################
## 4. Table D.2 ------
# ########################################
# first-stage table

coef.labels <- c("FDI_nonvnm_03_14_ave_export" = "FDI to ROW (export-related), 2003-14",
                               "FDI_nonvnm_03_14_ave_import" = "FDI to ROW (import-related), 2003-14",
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


options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_factory_latex_tabular = "kableExtra")
options(modelsummary_factory_markdown = "kableExtra")
options(modelsummary_factory_html = "kableExtra")
options(modelsummary_format_numeric_latex = "kableExtra")

# set the statistics to center aligned
glance_custom.glm <- function(x, ...) { 
  s <- glance(x)
  out <- data.frame(
    "N.new" = paste("mc", s[["nobs"]], "}"),
    "BIC.new" = paste("mc", formatC(round(s[["BIC"]],3), digits = 3, format = "f"), "}"),
    "Log.Lik.new" = paste("mc", formatC(round(s[["logLik"]],3), digits = 3, format = "f"), "}"),
    "FE.hssection" = paste("mc", "\\checkmark", "}") # add FE note
  )
  return(out)
}


table_first_stage <- modelsummary(list(
  "Vietnamese FDI (Export)" = first_stage.kor.probit,
  "Vietnamese FDI (Import)" = first_stage.vnm.prob),
             stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
             escape = FALSE,
             coef_map = coef.labels,
             output = "latex_tabular",
             gof_map = c("N.new",  "BIC.new", "Log.Lik.new", "FE.hssection"))

table_first_stage

# Adjust latex output format
table_first_stage.clean <- table_first_stage %>%
  gsub("lcc", "lqq", .) %>%
  gsub("N.new", "N", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log likelihood", .) %>%
  gsub("FE.hssection", "FE: HS section", .) %>%
  gsub("mc ", "\\\\mc{", .) %>% # to set these to center
  gsub(" \\\\}", "}", .) %>%
  gsub("\\+", "$+$", .) %>%
  gsub("Vietnamese FDI \\(Export\\)", "\\{Vietnamese FDI (export)\\}", .) %>%
  gsub("Vietnamese FDI \\(Import\\)", "\\{Vietnamese FDI (import)\\}", .) %>%
  gsub("\\\\bottomrule",
      "\\\\bottomrule \\\\multicolumn{3}{r}{Note: Standard errors in parentheses.}\\\\\\\\ \\\\multicolumn{3}{r}{+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01}", .)

table_first_stage.clean <- capture.output(table_first_stage.clean)

table_first_stage.clean

writeLines(table_first_stage.clean, paste0(REPLICATION_FIG_DIR, "TD2_iv_firststage_table.tex"))

# save as html
# html output ----------------
glance_custom.glm <- function(x, ...) {
  s <- glance(x)
  out <- data.frame(
    "N.new" =  s[["nobs"]],
    "BIC.new" =  formatC(round(s[["BIC"]],3), digits = 3, format = "f"),
    "Log.Lik.new" =  formatC(round(s[["logLik"]],3), digits = 3, format = "f"),
    "FE.hssection" =  "✔" # add FE note
  )
  return(out)
}


table_first_stage.clean.html <- modelsummary(list(
  "Vietnamese FDI (Export)" = first_stage.kor.probit,
  "Vietnamese FDI (Import)" = first_stage.vnm.prob),
                             stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
                             coef_map = coef.labels,
                             output = "html",
                             gof_map = c("N.new",  "BIC.new", "Log.Lik.new", "FE.hssection")
                             ) %>% 
  kable_styling() %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = "standard errors in parentheses.", threeparttable = TRUE) 

table_first_stage.clean.html <- table_first_stage.clean.html %>%
  gsub("N.new", "N", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log likelihood", .) %>% 
  gsub("FE.hssection", "FE: HS section", .)

table_first_stage.clean.html <- capture.output(table_first_stage.clean.html)
table_first_stage.clean.html

writeLines(table_first_stage.clean.html, paste0(REPLICATION_FIG_DIR,
                         "TD2_iv_firststage_table.html"))
