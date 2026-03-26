# ====
# Appendix Figure B.1 Replication:
# Compare Datamyne with UN data at HS 2 digit level
# input: vnm_2018_DM_UNCOM_export.csv
# R version 4.4.2 (2024-10-31)
# ====

################################################################################
## setup -----
################################################################################

# clean slate
rm(list = ls())
date()

library(tidyverse) 

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"

################################################################################
## 0. load data  ------
################################################################################

vnm_2018 <- read_csv(file=paste0(REPLICATION_DATA_DIR, "vnm_2018_DM_UNCOM_export.csv"))
vnm_2018 %>% glimpse()

################################################################################
# 1. Plot --------
################################################################################
axis.title.size <- 22

dm_comtrade_plot <- vnm_2018 %>%
  mutate(datamyne = log(Invoice), comtrade = log(`Trade Value (US$)`)) %>%
  ggplot(aes(datamyne, comtrade)) +
  geom_point(alpha = 0.4, size = 4, col = "black") +
  geom_abline(linetype = "dashed") +
  geom_text(aes(label = ifelse((comtrade - datamyne) > 3, as.character(HS2digit), "")),
            hjust = 0, vjust = 0, size = 8) +
  geom_text(aes(label = ifelse((comtrade > 25), as.character(HS2digit), "")),
            hjust = 0, vjust = 0, size = 8) +
  geom_text(aes(label = ifelse((comtrade < 10), as.character(HS2digit), "")),
            hjust = 0, vjust = 0, size = 8) +
  xlab("Export volume from Datamyne (log USD)") + 
  ylab("Export volume from UN Comtrade (log USD)") + 
  scale_x_continuous(limits = c(5, 27), breaks = c(5, 10, 15, 20, 25)) +
  scale_y_continuous(limits = c(5, 27), breaks = c(5, 10, 15, 20, 25)) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = axis.title.size),
    legend.title = element_text(size = axis.title.size),
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
    panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25)
  )

dm_comtrade_plot

ggsave(dm_comtrade_plot,
       file = paste0(REPLICATION_FIG_DIR, "FB1_VNM_export_2018_CT_DM_comparison.pdf"),
       width = 8.5, height = 7)
