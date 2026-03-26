# ====
# Fig B2 Replication:
# Plot FDI against change in trade volume
# input: fdi_trade_scatter.csv
# R version 4.4.2 (2024-10-31)
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()

library(tidyverse)
library(ggrepel)
library(latex2exp)

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"


################################################################################
## 0. load data  ------
################################################################################
tradeplot.dat <- read_csv(paste0(REPLICATION_DATA_DIR, "fdi_trade_scatter.csv"))
tradeplot.dat %>% glimpse


################################################################################
# 1. Plot --------
################################################################################
axis.title.size <- 22

fdi_trade_plot_exp <- tradeplot.dat %>%
  mutate(Vietnam = ifelse(iso3c == "VNM", 1, 0)) %>% # for labeling
  ggplot(aes((total.fdi/(gdp_WDI_PW/1e+9)), log(delta_exp_0317)), alpha = 0.4) +
  geom_point(aes(color = as.factor(Vietnam)), alpha = 0.6, size = 4) +
  geom_text_repel(aes(label = "Vietnam"),
                  data = tradeplot.dat %>%
                    filter(iso3c %in% c("VNM")),
                  box.padding   = 0.3,
                  point.padding = 0.3,
                  segment.color = NA,
                  size = 7) +
  scale_color_manual(breaks = c("0", "1"),
                     values = c("black", "red")) +
  scale_shape_manual(breaks = c("0", "1"),
                     values = c(17, 16)) +
  xlab("Total FDI count / average GDP, 2003-2017") + 
  ylab(TeX("$\\Delta$ Total export volume (log), 2003 vs. 2017")) + 
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"),
        axis.title.y = element_text(size = axis.title.size,
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = axis.title.size,
                                    margin = margin(20, 0, 0, 0)),
        axis.text.y = element_text(size = axis.title.size, color = "black"),
        axis.text.x = element_text(size = axis.title.size, color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = axis.title.size,
                                  margin = margin(20, 0, 20, 0)),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25))

fdi_trade_plot_exp

ggsave(fdi_trade_plot_exp,
       file = paste0(REPLICATION_FIG_DIR, "FB2_GFDI_to_Deltatrade_exp.pdf"),
       width = 8.5, height = 7)
