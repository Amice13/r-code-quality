## Hager / Hilbig - Inheritance replication code - June 12, 2019
## hhilbig@g.harvard.edu
##
## This file : Maps in the appendix
## Figures reproduced in this file: A6, A8, A9, A10
##
### ### ### ###

rm(list = ls())

## packages

library(tidyverse)
library(viridis)

## Load the two data files to create the maps

map_data <- read_csv('Map_Data.csv')
map_child <- read_csv('Map_ChildLabor.csv')

#### Figure A6: Peasant War involvement ####

p1 <- ggplot() +
  geom_polygon(data = map_data, aes(fill = peas_total,
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  geom_path(data = map_data, aes(x = long, 
                                  y = lat, 
                                  group = group), 
            color = "black", size = 0.1) +
  coord_map() +
  # add the previously defined basic theme
  labs(x = NULL, 
       y = NULL) + 
  scale_fill_distiller(name = 'Peasant War\ninvolvement (1422-25)',
                       direction = 1, palette = 'Reds') +
  theme(legend.position = 'right', panel.border = element_blank(),
        legend.text = element_text(size = 16))
p1

## Save the plot

ggsave(p1, filename = 'Map_PeasantWar_A5.pdf')

#### Figure A8: Child Labor Prevalence in the German Empire ####

p2 <- ggplot() +
  geom_polygon(data = map_child, aes(fill = childlabor_mean,
                                  x = long, 
                                  y = lat, 
                                  group = group)) +
  # outlines
  geom_path(data = map_child, aes(x = long, 
                               y = lat, 
                               group = group), 
            color = "white", size = 0.1) +
  coord_map() +
  # add the previously defined basic theme
  labs(x = NULL, 
       y = NULL) + 
  scale_fill_viridis(name = 'Child labor\nprevalence (%)') +
  theme(legend.position = 'right', panel.border = element_blank(),
        legend.text = element_text(size = 16))
p2

## Save the plot


#### Figure A9: Legal Code Map ####

p3 <- ggplot() +
  geom_polygon(data = map_data, aes(fill = law_cat,
                                        x = long, 
                                        y = lat, 
                                        group = group)) +
  coord_map() +
  labs(x = NULL, 
       y = NULL) + 
  scale_fill_brewer(name = '', type = 'qual', palette = 1) +
  theme(legend.position = 'right', panel.border = element_blank(),
        legend.text = element_text(size = 16))
p3

## Save the plot

ggsave(p3, filename = 'Map_LegalCode_A8.pdf')

#### Figure A10: Welfare Spending in 1890 ####

p4 <- ggplot() +
  geom_polygon(data = map_data, aes(fill = support_expenses_total_capita,
                                        x = long, 
                                        y = lat, 
                                        group = group)) +
  coord_map() +
  # add the previously defined basic theme
  labs(x = NULL, 
       y = NULL) + 
  scale_fill_viridis(name = 'Support for the poor\n(Mark / capita)') +
  theme(legend.position = 'right', panel.border = element_blank(),
        legend.text = element_text(size = 16))
p4
## Save the plot

ggsave(p4, filename = 'Map_SupportPoor_A9.pdf')


