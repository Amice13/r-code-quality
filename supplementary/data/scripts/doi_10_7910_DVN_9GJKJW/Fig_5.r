# ====
# Figure 5 Replication:
# Effect of FDI on Mean Tariff Cut across FTAs
# input:
#  - lastFDI_tariff_FTA_dyad_hs6_deepest_FDIvar.RData
#  - lastFDI_tariff_FTA_dyad_hs6_exp_deepest_FDIvar.RData
# intermediary results:
#  - brms_fit3-mean.RDS
#  - brms_fit3_exp-mean.RDS
# R version 4.4.2 (2024-10-31)
# ====


################################################################################
## setup -----
################################################################################

# clean slate
rm(list = ls())
date()

library(tidyverse)
library(brms)
library(ggthemes)
library(ggridges)
library(tidybayes)
library(glue)
library(bayesplot)

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"


################################################################################
## 0. load data  ------
################################################################################
load(file = paste0(REPLICATION_DATA_DIR, "lastFDI_tariff_FTA_dyad_hs6_deepest_FDIvar.RData"))
load(file = paste0(REPLICATION_DATA_DIR, "lastFDI_tariff_FTA_dyad_hs6_exp_deepest_FDIvar.RData"))

lastFDI_dat %>% glimpse()
lastFDI_dat_exp %>% glimpse()

summary(lastFDI_dat)
summary(lastFDI_dat_exp)

################################################################################
## 1. Hierarchical models ------
################################################################################
# RUN_BRM <- FALSE
RUN_BRM <- TRUE # set to false to use saved results

# varying slopes
if (RUN_BRM){
  fit.mean <-
    brms::brm(
      -log(export_tariff_cut_hs6_mean + 1) ~ fdicount_cum_lag_bin +
        ln_wldexp +
        ln_expmean +
        ln_impmean +
        polity2_P4 + 
        polity2_P4_home + 
        lngdppc_WDI_PW +
        lngdppc_WDI_PW_home +
        rauch_n +
        rauch_w +
        intermediate +
        gvc_ui +
        gvc_di +
        (1|host) +
        (1|hs2), 
      family = gaussian(),
      data = lastFDI_dat,
      iter = 6000, warmup = 1000,
      thin = 10,
      chains = 5, cores = 4,
      seed = 08542)

  summary(fit.mean)
  saveRDS(fit.mean, paste0(REPLICATION_DATA_DIR, "brms_fit3-mean.RDS"))


  ## Hierarchical models, export version-----
  # varying slopes
  fit.exp.mean <-
    brms::brm(
      -log(export_tariff_cut_hs6_mean + 1) ~ fdicount_cum_lag_bin +
        ln_wldexp +
        ln_expmean +
        ln_impmean +
        polity2_P4 + 
        polity2_P4_home + 
        lngdppc_WDI_PW +
        lngdppc_WDI_PW_home +
        rauch_n +
        rauch_w +
        intermediate +
        gvc_ui +
        gvc_di +
        (1|host) +
        (1|hs2),
      family = gaussian(),
      data = lastFDI_dat_exp,
      iter = 6000, warmup = 1000,
      thin = 10,
      chains = 5, cores = 4,
      seed = 08542)

  summary(fit.exp.mean)
  saveRDS(fit.exp.mean, paste0(REPLICATION_DATA_DIR, "brms_fit3_exp-mean.RDS"))

}else{
  fit.mean <- readRDS(paste0(REPLICATION_DATA_DIR, "brms_fit3-mean.RDS"))
  fit.exp.mean <- readRDS(paste0(REPLICATION_DATA_DIR, "brms_fit3_exp-mean.RDS"))
}


#################################################################################
## 2. Effect plots  ------
################################################################################

pooled.draws <- spread_draws(fit.mean, b_fdicount_cum_lag_bin) 

pooled.draws.exp <- spread_draws(fit.exp.mean, b_fdicount_cum_lag_bin) 


pooled.draws <- pooled.draws %>%
  mutate(outcome = "Host tariff")

pooled.draws.exp <- pooled.draws.exp %>%
  mutate(outcome = "Partner tariff")

randomint.data <- bind_rows(pooled.draws,
                            pooled.draws.exp)

randomint.data %>% glimpse

randomint.data.summary <- data.frame(outcome = c("Host tariff",
                                                 "Partner tariff"),
                                     est = c(fixef(fit.mean)["fdicount_cum_lag_bin", 1],
                                             fixef(fit.exp.mean)["fdicount_cum_lag_bin", 1]),
                                     lb = c(fixef(fit.mean)["fdicount_cum_lag_bin", 3],
                                            fixef(fit.exp.mean)["fdicount_cum_lag_bin", 3]),
                                     ub = c(fixef(fit.mean)["fdicount_cum_lag_bin", 4],
                                            fixef(fit.exp.mean)["fdicount_cum_lag_bin", 4]))

randomint.data.summary


axis.title.size <- 22

randomint.plot <- randomint.data.summary %>% 
  ggplot(aes(x = est, y = outcome)) +
  geom_point(color = "black",
             size = 0.1, alpha = 0.8) +
  geom_pointrange(aes(xmin = lb, xmax = ub),
                  color = "black",
                  size = 0.8, alpha = 0.8) +
  geom_density_ridges(data = randomint.data,
                      aes(b_fdicount_cum_lag_bin, outcome),
                      fill = "black",
                      rel_min_height = 0.01,
                      col = NA, scale = 0.95,
                      alpha = 0.3) +
  labs(x = "Estimated effect of FDI (2003-14) \n on average preferential margin", 
       y = element_blank()) +
  geom_vline(xintercept = 0,
             color = "red",
             linetype = "dashed") + 
  coord_flip() +
  xlim(-0.15, 0.025) +
  scale_y_discrete(expand = expansion(mult = c(1, 1))) +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size = axis.title.size + 4,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(0, 0, 20, 0)),
        plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"),
        axis.title.y = element_text(size = axis.title.size,
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

randomint.plot

ggsave(randomint.plot, file = paste0(REPLICATION_FIG_DIR, "F5_brm_randomint_summary_meancut.pdf"),
       width = 8, height = 6.5)
