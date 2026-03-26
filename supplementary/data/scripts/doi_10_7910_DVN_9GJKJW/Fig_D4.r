# ====
# Figure D4 Replication:
# Trace Plots for Effect of FDI on Mean Tariff Cut across FTAs
# input (results obtained in Fig_5.r):
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
library(ggthemes)
library(ggridges)
library(tidybayes)
library(glue)
library(bayesplot)
library(cowplot)


# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

################################################################################
## 0. load results  ------
################################################################################

fit.mean <- readRDS(paste0(REPLICATION_DATA_DIR, "brms_fit3-mean.RDS"))
fit.exp.mean <- readRDS(paste0(REPLICATION_DATA_DIR, "brms_fit3_exp-mean.RDS"))


#################################################################################
## 1. Trace plots  ------
# ################################################################################

post.fit.mean <- as.array(fit.mean)
dim(post.fit.mean)

post.fit.exp.mean <- as.array(fit.exp.mean)
dim(post.fit.exp.mean)

axis.title.size <- 22

mytheme <-  theme(legend.text = element_text(size = axis.title.size),
                  legend.title = element_text(size = axis.title.size),
                  plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm"),
                  axis.text.y = element_text(size = axis.title.size,
                                            margin = margin(10, 5, 10, 0),
                                            color = "black"),
                  axis.text.x = element_text(size = axis.title.size,
                                             margin = margin(5, 10, 10, 0),
                                             color = "black"),
                  strip.text = element_text(size = axis.title.size + 2,
                                            face = "bold",
                                            margin = margin(20, 10, 10, 10)),
                  strip.background = element_blank(),
                  panel.grid = element_blank(),
                  panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.5))


trace.fit.mean <- mcmc_trace(post.fit.mean, pars = "b_fdicount_cum_lag_bin") + 
  ylab("") + scale_y_continuous(limits = c(-0.12, -0.02), breaks = seq(-0.12, 0, 0.02))+
#   ggtitle("host Tariff") +
  theme_bw() +
  mytheme

legend <- get_legend(trace.fit.mean)  #extract legend

trace.fit.mean <- trace.fit.mean + theme(legend.position = "none")


trace.fit.exp.mean <- mcmc_trace(post.fit.exp.mean, pars = "b_fdicount_cum_lag_bin") + 
  ylab("") + scale_y_continuous(limits=c(-0.12, -0.02), breaks = seq(-0.12, 0, 0.02)) +
#   ggtitle("Partner tariff") +
  theme_bw() + 
  mytheme + 
  theme(legend.position = "none")



ggsave(trace.fit.mean, file = paste0(REPLICATION_FIG_DIR, "FD4_brm_randomint_traceplots_meancut_host.pdf"),
       width = 8, height = 6)

ggsave(trace.fit.exp.mean, file = paste0(REPLICATION_FIG_DIR, "FD4_brm_randomint_traceplots_meancut_partner.pdf"),
       width = 8, height = 6)

ggsave(legend, file = paste0(REPLICATION_FIG_DIR, "FD4_brm_randomint_traceplots_meancut_legend.pdf"),
       width = 1, height = 6)


