rm(list = ls())
library(data.table)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(here)

load(here('data','ver_data.RData'))

tt <- unique(ver_b[,.(muni, security_council)])
tt[, total_sc := .N, by = 'security_council']
tt <- unique(tt[security_council <2018,.(security_council,total_sc)])
setkey(tt,security_council)

tt <- tt %>%
        mutate(acc = cumsum(total_sc))

plot <- ggplot(tt, aes(x = security_council, y = acc)) + 
  geom_area(alpha=.9) +
  theme_minimal() +
  ylab('Total municipalities with Security Committees') +
  xlab('') +
  NULL

#ggsave(here("writing","img","fig_A1_panel_a.pdf"), plot = plot, device = 'pdf',height = 10, width = 10, units = 'cm')

