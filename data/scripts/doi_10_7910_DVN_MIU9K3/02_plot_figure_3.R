#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 02_plot_figure_3.R
## Updated: 2016-11-11 15:34:56 AEDT

setwd('~/Documents/replication_packages/ics_2016')

load('newspaper_articles_df.RData')

# Figure 3
library(ggplot2)
library(scales)
ggplot(newspaper_articles_df, aes(x=as.Date(date, format="%d %B %Y"), fill=search)) + 
  geom_histogram(binwidth=4) +
  theme_bw() +
  labs(x=NULL) +
  theme(legend.position="bottom") + 
  scale_x_date(limits=c(as.Date("2014-01-01"), as.Date("2015-04-01")), 
               breaks=c(as.Date("2014-03-16"),
                        as.Date("2014-05-18"),
                        as.Date("2014-07-06"),
                        as.Date("2014-08-31"),
                        as.Date("2015-03-04")),
               labels = date_format("%b %y"))