# ====
# Figure 4 Replication:
# Aggregate effect of Korean and Non-Korean FDI on tariff
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


# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"



################################################################################
## 0. load data  ------
################################################################################
load(file = paste0(REPLICATION_DATA_DIR, "tariff_paneldata_withiv.RData"))
tariff.panel %>% glimpse()

## NOTE: For imports, the FDI-related products are subset to those above median upstreamness

cluster_level <- eval(parse(text="~HS_6d"))

###############################################################################
## 1. Korean vs Non-Korean FDI, entire period, joint model-------- 
################################################################################
## 1.1 Korean Tariffs (~ Korean FDI Export) ----------
tobit.korfdi.dif.fe.sec.kor.covars.allyrs <- tobit(ln_mean_tariff_reduction_dif_kortrff_innegative ~ fdi_03_14_dm_exp_kor +
                                                     fdi_03_14_dm_exp_onlynon_kor + 
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

summary(tobit.korfdi.dif.fe.sec.kor.covars.allyrs)

tobit.korfdi.dif.fe.sec.kor.covars.allyrs.out <- coeftest(tobit.korfdi.dif.fe.sec.kor.covars.allyrs, vcov = vcovCL(tobit.korfdi.dif.fe.sec.kor.covars.allyrs,  type = "HC0"))

tobit.korfdi.dif.fe.sec.kor.covars.allyrs.out 

# extract coefs and SEs
coef.df.tobit.korfdi.dif.allyrs.kortrff <- tibble(model = c("Korean FDI", "Non-Korean FDI"),
                                                  coef = c(tobit.korfdi.dif.fe.sec.kor.covars.allyrs.out[,"Estimate"]["fdi_03_14_dm_exp_kor"],
                                                           tobit.korfdi.dif.fe.sec.kor.covars.allyrs.out[,"Estimate"]["fdi_03_14_dm_exp_onlynon_kor"]),
                                                  se = c(tobit.korfdi.dif.fe.sec.kor.covars.allyrs.out[,"Std. Error"]["fdi_03_14_dm_exp_kor"],
                                                         tobit.korfdi.dif.fe.sec.kor.covars.allyrs.out[,"Std. Error"]["fdi_03_14_dm_exp_onlynon_kor"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.korfdi.dif.allyrs.kortrff <- coef.df.tobit.korfdi.dif.allyrs.kortrff %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model))

coef.df.tobit.korfdi.dif.allyrs.kortrff


## 1.2 Vietnamese Tariffs (~ Korean FDI Import) ------
tobit.korfdi.dif.fe.sec.vnm.covars.allyrs <- tobit(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~
                                                     fdi_03_14_dm_imp_kor +
                                                     fdi_03_14_dm_imp_onlynon_kor +
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

summary(tobit.korfdi.dif.fe.sec.vnm.covars.allyrs)


tobit.korfdi.dif.fe.sec.vnm.covars.allyrs.out <- coeftest(tobit.korfdi.dif.fe.sec.vnm.covars.allyrs, vcov = vcovCL(tobit.korfdi.dif.fe.sec.vnm.covars.allyrs, 
type = "HC0"))

tobit.korfdi.dif.fe.sec.vnm.covars.allyrs.out 

# extract coefs and SEs
coef.df.tobit.korfdi.dif.allyrs.vnmtrff <- tibble(model =  c("Korean FDI", "Non-Korean FDI"),
                                                  coef = c(tobit.korfdi.dif.fe.sec.vnm.covars.allyrs.out[,"Estimate"]["fdi_03_14_dm_imp_kor"],
                                                           tobit.korfdi.dif.fe.sec.vnm.covars.allyrs.out[,"Estimate"]["fdi_03_14_dm_imp_onlynon_kor"]),
                                                  se = c(tobit.korfdi.dif.fe.sec.vnm.covars.allyrs.out[,"Std. Error"]["fdi_03_14_dm_imp_kor"],
                                                         tobit.korfdi.dif.fe.sec.vnm.covars.allyrs.out[,"Std. Error"]["fdi_03_14_dm_imp_onlynon_kor"]))

# calculate C.I.
alpha <- 0.05
coef.df.tobit.korfdi.dif.allyrs.vnmtrff <- coef.df.tobit.korfdi.dif.allyrs.vnmtrff %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model))

coef.df.tobit.korfdi.dif.allyrs.vnmtrff


################################################################################
# 2. Plot --------
################################################################################
axis.title.size <- 22

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
plot.korfdi.allyear.kortrff <- ggplot(data = coef.df.tobit.korfdi.dif.allyrs.kortrff, aes(x = model, y = coef)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) + 
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous(TeX("Estimated effect of FDI (2003-14) on $\\Delta$ tariff"),
                     breaks = round(seq(-0.6, 0.4, 0.2), 1),
                     labels = round(seq(-0.6, 0.4, 0.2), 1),
                     limits = c(-0.65, 0.5)) +
  plot.theme + 
  theme(axis.text.y = element_text(size = axis.title.size, 
                                   margin = margin(5, 10, 10, 0),
                                   color = "black")) 


# vietnamese tariff

plot.korfdi.allyear.vnmtrff <- ggplot(data = coef.df.tobit.korfdi.dif.allyrs.vnmtrff, aes(x = model, y = coef)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) + 
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous(TeX(" "),
                     breaks = round(seq(-0.6, 0.4, 0.2), 1),
                     labels = round(seq(-0.6, 0.4, 0.2), 1),
                     limits = c(-0.65, 0.5)) +
  plot.theme + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# adjust the width of the panels
plot.korfdi.allyear.kortrff <- ggplotGrob(plot.korfdi.allyear.kortrff)
plot.korfdi.allyear.vnmtrff <- ggplotGrob(plot.korfdi.allyear.vnmtrff)

plot.korfdi.allyear.vnmtrff$widths[6] <- plot.korfdi.allyear.kortrff$widths[6] # Force panel width to match


ggsave(plot.korfdi.allyear.kortrff, file = paste0(REPLICATION_FIG_DIR, "F4_tariff_aggregated_years_korfdi_kortrff.pdf"),
    width = 7, height = 7)

ggsave(plot.korfdi.allyear.vnmtrff, file = paste0(REPLICATION_FIG_DIR, "F4_tariff_aggregated_years_korfdi_vnmtrff.pdf"),
    width = 7, height = 7)
