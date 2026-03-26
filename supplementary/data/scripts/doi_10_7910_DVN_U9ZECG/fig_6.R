rm(list = ls())
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggrepel)
library(here)

head <- readRDS(here('data','daily_homicides.rds'))

homicides = ggplot() +
  geom_line(data = head[year>2005], 
            aes(x = date_death, y = assault_ma, group = group, linetype = as.factor(group))) +
  theme_minimal() +
  xlab('') +
  ylab('Daily Homicides') +
  coord_cartesian(y=c(0,115)) +
  theme(legend.title = element_blank(),
        legend.position = 'none') + 
  geom_label_repel(data = tail(head[year>2005],4), 
                   aes(x = date_death, y = assault_ma, label = group), 
                   nudge_y = 10, family = 'Times', nudge_x = -1000) +
  NULL

ggsave(here("writing","img","fig_6.pdf"), plot = homicides, device = 'pdf',height = 15, width = 15, units = 'cm')





