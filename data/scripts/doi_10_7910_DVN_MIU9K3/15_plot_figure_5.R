#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 15_plot_figure_5.R
## Updated: 2016-11-12 16:26:10 AEDT

setwd('~/Documents/replication_packages/ics_2016')

library(ggplot2)
library(gridExtra)

load('sim_v_remove_on_rt_graph.RData')
load('net_stats_on_rt_graph.RData')


df <- data.frame(mar14 = clusters_sim_rt[[1]],
                 may14 = clusters_sim_rt[[2]],
                 jul14 = clusters_sim_rt[[3]],
                 aug14 = clusters_sim_rt[[4]],
                 mar15a = clusters_sim_rt[[5]],
                 mar15b = clusters_sim_rt[[6]])

p <- lapply(names(clusters_sim_rt), function(i) {
  interc <- clusters_no_news_rt[which(names(clusters_sim_rt)==i)]
  ggplot() + geom_density(data=df, aes_string(x=i), color='red') + labs(y=NULL) +
    geom_vline(xintercept=interc,
               color="red", linetype="dashed", size=1) +
    scale_x_log10(breaks=100)})

do.call(grid.arrange, c(p, nrow=2))
