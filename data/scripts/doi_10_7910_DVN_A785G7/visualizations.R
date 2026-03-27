# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

library(ggplot2)
library(dplyr)
library(reshape2)

load('~/Dropbox/pa_replication/Appendix_C/Appendix_C.3/results_sims_subsampling.RData')

names(summaries) <- c('Type', 'Coverage', 'Seed', 'Number_Subgroups')
levels(summaries$Type) <- c('corrected'='Corrected', 'uncorrected'='Uncorrected')

to_plot <- summaries %>% select(-Seed) %>% 
  group_by(Number_Subgroups, Type) %>% summarise_all(mean) %>%
  melt(id.vars=c('Type', 'Number_Subgroups'))

to_plot$Number_Subgroups <- as.factor(to_plot$Number_Subgroups)
to_plot$value <- to_plot$value/8

ggplot(to_plot, aes(x=Number_Subgroups, y=value, color=Type, group=Type)) + geom_point(stat='summary', fun.y=sum) + 
  stat_summary(fun.y=sum, geom="line") + theme_minimal() + xlab('Number of Subgroups') + ylab('Proportion')
