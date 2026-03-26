# ====
# Figure D3 Replication
# Effect of cumulative FDI counts on tariff cuts
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
library(latex2exp)


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
# 1.  Entire time period (2003-14) with cumulative counts --------
################################################################################
## 1.1 Korean Tariffs (~ FDI Export) --------------
summary(tariff.panel$ln_mean_tariff_reduction_dif_kortrff_innegative)
table(tariff.panel$ln_mean_tariff_reduction_dif_kortrff_innegative == 0)
summary(log(tariff.panel$fdi_03_14_count_dm_exp + 1))

tobit.dif.fe.sec.kor.covars.cumsum.ct <- tobit(ln_mean_tariff_reduction_dif_kortrff_innegative ~
                                                log(fdi_03_14_count_dm_exp + 1) +
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

summary(tobit.dif.fe.sec.kor.covars.cumsum.ct)

tobit.dif.fe.sec.kor.covars.cumsum.ct.out <- coeftest(tobit.dif.fe.sec.kor.covars.cumsum.ct, 
                                                     vcov = vcovCL(tobit.dif.fe.sec.kor.covars.cumsum.ct, type = "HC0"))

tobit.dif.fe.sec.kor.covars.cumsum.ct.out

# extract coefs and SEs
coef.df.tobit.dif.cumsum.kortrff.joint.ct <- tibble(model = c("2003-2014"),
                                             coef = c(tobit.dif.fe.sec.kor.covars.cumsum.ct.out[,"Estimate"]["log(fdi_03_14_count_dm_exp + 1)"]),
                                             se = c(tobit.dif.fe.sec.kor.covars.cumsum.ct.out[,"Std. Error"]["log(fdi_03_14_count_dm_exp + 1)"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.dif.cumsum.kortrff.joint.ct <- coef.df.tobit.dif.cumsum.kortrff.joint.ct %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model,
                        levels = c("2003-2014")))
coef.df.tobit.dif.cumsum.kortrff.joint.ct



## 1.2 Vietnamese Tariffs (~ FDI Import) ------
summary(tariff.panel$ln_mean_tariff_reduction_dif_vnmtrff_innegative)
table(tariff.panel$ln_mean_tariff_reduction_dif_vnmtrff_innegative == 0)
summary(log(tariff.panel$fdi_03_14_count_dm_imp + 1))


tobit.dif.fe.sec.vnm.covars.cumsum.ct <- tobit(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~
                                                log(fdi_03_14_count_dm_imp + 1) +
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

summary(tobit.dif.fe.sec.vnm.covars.cumsum.ct)


tobit.dif.fe.sec.vnm.covars.cumsum.ct.out <- coeftest(tobit.dif.fe.sec.vnm.covars.cumsum.ct, 
                                                     vcov = vcovCL(tobit.dif.fe.sec.vnm.covars.cumsum.ct, type = "HC0"))
tobit.dif.fe.sec.vnm.covars.cumsum.ct.out

# extract coefs and SEs
coef.df.tobit.dif.cumsum.vnmtrff.joint.ct <- tibble(model = c("2003-2014"),
                                             coef = c(tobit.dif.fe.sec.vnm.covars.cumsum.ct.out[,"Estimate"]["log(fdi_03_14_count_dm_imp + 1)"]),
                                             se = c(tobit.dif.fe.sec.vnm.covars.cumsum.ct.out[,"Std. Error"]["log(fdi_03_14_count_dm_imp + 1)"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.dif.cumsum.vnmtrff.joint.ct <- coef.df.tobit.dif.cumsum.vnmtrff.joint.ct %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model,
                        levels = c("2003-2014")))
coef.df.tobit.dif.cumsum.vnmtrff.joint.ct


## 1.3 Combine ------
coef.df.tobit.dif.cumsum.kortrff.joint.ct$outcome <- "export"
coef.df.tobit.dif.cumsum.vnmtrff.joint.ct$outcome <- "import"

coef.df.tobit.dif.cumsum.korvnmtrff.joint.comb.ct <- rbind(coef.df.tobit.dif.cumsum.kortrff.joint.ct, coef.df.tobit.dif.cumsum.vnmtrff.joint.ct)

coef.df.tobit.dif.cumsum.korvnmtrff.joint.comb.ct <- coef.df.tobit.dif.cumsum.korvnmtrff.joint.comb.ct %>% 
  mutate(outcome = factor(outcome))

levels(coef.df.tobit.dif.cumsum.korvnmtrff.joint.comb.ct$outcome) <-
  c("export" = TeX("$\\Delta$ Korean Import Tariff"),
    "import" = TeX("$\\Delta$ Vietnamese Import Tariff"))

coef.df.tobit.dif.cumsum.korvnmtrff.joint.comb.ct


################################################################################
# 2. Plot --------
################################################################################

axis.title.size <- 22

# #  convert to 1% increase in FDI count:
p.tobit.dif.cumsum.korvnmtrff.joint.ct <- ggplot(data = coef.df.tobit.dif.cumsum.korvnmtrff.joint.comb.ct, aes(x = outcome, y = coef, ymin = lwr, ymax = upr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) + 
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("",
                   labels = c(TeX("$\\Delta$ Korean tariff"),
                   TeX("$\\Delta$ Vietnamese tariff"))) +
  scale_y_continuous(TeX(r"(\overset{Estimated effect of 1\% increase in}{FDI on $\Delta$ tariff})"),
                     breaks = round(seq(-0.2, 0.05, 0.05), 1),
                     labels = round(seq(-0.2, 0.05, 0.05), 1),
                     limits = c(-0.2, 0.05)) +
  theme_bw() + theme(legend.position = "none",
                     plot.title = element_text(size = axis.title.size + 4,
                                               face = "bold",
                                               hjust = 0.5,
                                               margin = margin(0, 0, 20, 0)),
                     plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"),
                     axis.title.y = element_text(size = axis.title.size + 2,
                                                 margin = margin(0, 10, 0, 0)),
                     axis.title.x = element_text(size = axis.title.size,
                                                 margin = margin(0, 0, 10, 0)),
                     axis.text.y = element_text(size = axis.title.size,
                                                margin = margin(10, 5, 10, 0),
                                                color = "black"),
                     axis.text.x = element_text(size = axis.title.size,
                                                margin = margin(5, 10, 10, 0),
                                                color = "black"),
                     strip.text = element_text(size = axis.title.size + 4,
                                               face = "bold",
                                               margin = margin(20, 10, 10, 10)),
                     strip.background = element_blank(),
                     panel.grid = element_blank(),
                     panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25))

p.tobit.dif.cumsum.korvnmtrff.joint.ct

# save
ggsave(file = paste0(REPLICATION_FIG_DIR, "FD3_tariff_count_cumsum.pdf"),
    width = 8, height = 7.5)