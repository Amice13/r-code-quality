# ====
# Figure D2 Replication: 
# Effect of FDI on tariff cuts by year
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
# 1.  Yearly plot, jointly estimated -----
################################################################################
## 1.1 Korean Tariffs (~ FDI Export)-----
tobit.dif.fe.sec.first.covars.byyear.kortrff <- tobit(ln_mean_tariff_reduction_dif_kortrff_innegative ~
                                                    fdi_dm_exp_first_2003 +
                                                    fdi_dm_exp_first_2004 +
                                                    fdi_dm_exp_first_2005 +
                                                    fdi_dm_exp_first_2006 +
                                                    fdi_dm_exp_first_2007 +
                                                    fdi_dm_exp_first_2008 +
                                                    fdi_dm_exp_first_2009 +
                                                    fdi_dm_exp_first_2010 +
                                                    fdi_dm_exp_first_2011 +
                                                    fdi_dm_exp_first_2012 +
                                                    fdi_dm_exp_first_2013 + 
                                                    fdi_dm_exp_first_2014 + 
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
                                                      right = 0,
                                                      left = -Inf,
                                                      data = tariff.panel)

summary(tobit.dif.fe.sec.first.covars.byyear.kortrff)

tobit.dif.fe.sec.first.covars.byyear.kortrff.out <- coeftest(tobit.dif.fe.sec.first.covars.byyear.kortrff, 
                                                             vcov = vcovCL(tobit.dif.fe.sec.first.covars.byyear.kortrff, type = "HC0"))

tobit.dif.fe.sec.first.covars.byyear.kortrff.out

# plot data frame: extract coefs and SEs
plot.byyear.kortrff.df <- tibble(model = c("2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                                           "2010", "2011", "2012", "2013", "2014"),
                                 coef = c(tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2003",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2004",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2005",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2006",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2007",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2008",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2009",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2010",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2011",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2012",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2013",1],
                                          tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2014",1]),
                                 se = c(tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2003",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2004",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2005",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2006",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2007",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2008",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2009",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2010",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2011",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2012",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2013",2],
                                        tobit.dif.fe.sec.first.covars.byyear.kortrff.out["fdi_dm_exp_first_2014",2]))

print(plot.byyear.kortrff.df)

# calculate C.I.
alpha <- 0.05

plot.byyear.kortrff.df <- plot.byyear.kortrff.df %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model, 
                        levels = c("2003", "2004", "2005", "2006",
                                   "2007", "2008", "2009", "2010",
                                   "2011", "2012", "2013", "2014")))
print(plot.byyear.kortrff.df)


## 1.2. Vietnamese Tariffs (~ FDI Import) -----
tobit.dif.fe.sec.first.covars.byyear.vnmtrff <- tobit(ln_mean_tariff_reduction_dif_vnmtrff_innegative ~
                                                        fdi_dm_imp_first_2003 +
                                                        fdi_dm_imp_first_2004 +
                                                        fdi_dm_imp_first_2005 +
                                                        fdi_dm_imp_first_2006 +
                                                        fdi_dm_imp_first_2007 +
                                                        fdi_dm_imp_first_2008 +
                                                        fdi_dm_imp_first_2009 +
                                                        fdi_dm_imp_first_2010 +
                                                        fdi_dm_imp_first_2011 +
                                                        fdi_dm_imp_first_2012 +
                                                        fdi_dm_imp_first_2013 + 
                                                        fdi_dm_imp_first_2014 + 
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
                                                      right = 0,
                                                      left = -Inf,
                                                      data = tariff.panel)

summary(tobit.dif.fe.sec.first.covars.byyear.vnmtrff)

tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out <- coeftest(tobit.dif.fe.sec.first.covars.byyear.vnmtrff, 
                                                             vcov = vcovCL(tobit.dif.fe.sec.first.covars.byyear.vnmtrff, type = "HC0"))

tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out

# plot data frame: extract coefs and SEs
plot.byyear.vnmtrff.df <- tibble(model = c("2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                                           "2010", "2011", "2012", "2013", "2014"),
                                 coef = c(tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2003",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2004",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2005",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2006",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2007",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2008",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2009",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2010",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2011",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2012",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2013",1],
                                          tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2014",1]),
                                 se = c(tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2003",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2004",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2005",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2006",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2007",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2008",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2009",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2010",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2011",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2012",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2013",2],
                                        tobit.dif.fe.sec.first.covars.byyear.vnmtrff.out["fdi_dm_imp_first_2014",2]))

print(plot.byyear.vnmtrff.df)


# calculate C.I.
alpha <- 0.05

plot.byyear.vnmtrff.df <- plot.byyear.vnmtrff.df %>%
  mutate(multiplier = qnorm(1 - alpha/2),
         lwr = coef - multiplier * se,
         upr = coef + multiplier * se,
         model = factor(model,
                        levels = c("2003", "2004", "2005", "2006",
                                   "2007", "2008", "2009", "2010",
                                   "2011", "2012", "2013", "2014")))
print(plot.byyear.vnmtrff.df)


################################################################################
# 2. Plot --------
################################################################################

axis.title.size <- 22
Y_ARROW <- 1
Y_ARROWTEXT <- 1.1

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
plot.byyear.kortrff <- ggplot(data = plot.byyear.kortrff.df,
                              aes(x = model, y = coef)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("Year of the first FDI",
                   breaks = c("2003", "2005", "2007",
                              "2009", "2011",  "2013")) +
  scale_y_continuous(TeX("Estimated effect of first FDI on $\\Delta$ tariff"),
                     breaks = round(seq(-1, 1, 0.5), 1),
                     labels = round(seq(-1, 1, 0.5), 1),
                     limits = c(-1.2, 1.2)
                     ) +
  ggplot2::annotate("segment", x = 5, xend = 1, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("segment", x = 8, xend = 12, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("text", x = 3, y = Y_ARROWTEXT, label="Older FDI", size = axis.title.size - 14) +
  ggplot2::annotate("text", x = 10, y = Y_ARROWTEXT, label="Recent FDI", size = axis.title.size - 14) + 
  plot.theme + 
  theme(axis.text.y = element_text(size = axis.title.size, margin = margin(5, 10, 10, 0), color = "black"))

plot.byyear.kortrff

# vietnamese tariff
plot.byyear.vnmtrff <- ggplot(data = plot.byyear.vnmtrff.df,
                              aes(x = model, y = coef)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 4) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.8, alpha = 0.8) +
  geom_hline(yintercept = 0,
             color = "red",
             linetype = "dashed") +
  scale_x_discrete("Year of the first FDI",
                   breaks = c("2003", "2005", "2007",
                              "2009", "2011",  "2013")) +
  scale_y_continuous(" ",
                     breaks = round(seq(-1, 1, 0.5), 1),
                     labels = round(seq(-1, 1, 0.5), 1),
                     limits = c(-1.2, 1.2)
                     ) +
  ggplot2::annotate("segment", x = 5, xend = 1, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("segment", x = 8, xend = 12, y = Y_ARROW, yend = Y_ARROW, arrow = arrow(length=unit(0.25,"cm")), lwd = 0.5) + # arrows 
  ggplot2::annotate("text", x = 3, y = Y_ARROWTEXT, label="Older FDI", size = axis.title.size - 14) +
  ggplot2::annotate("text", x = 10, y = Y_ARROWTEXT, label="Recent FDI", size = axis.title.size - 14) + 
  plot.theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plot.byyear.vnmtrff

# adjust the width of the panels
plot.byyear.kortrff <- ggplotGrob(plot.byyear.kortrff)
plot.byyear.vnmtrff <- ggplotGrob(plot.byyear.vnmtrff)

plot.byyear.vnmtrff$widths[6] <- plot.byyear.kortrff$widths[6] # Force panel width to match

# save
ggsave(plot.byyear.kortrff, 
    file = paste0(REPLICATION_FIG_DIR, "FD2_tariff_yearly_kortrff.pdf"),
    width = 7, height = 6.5)

ggsave(plot.byyear.vnmtrff, 
    file = paste0(REPLICATION_FIG_DIR, "FD2_tariff_yearly_vnmtrff.pdf"),
    width = 7, height = 6.5)
