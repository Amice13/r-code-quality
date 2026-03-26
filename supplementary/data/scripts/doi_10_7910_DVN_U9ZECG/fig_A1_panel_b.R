rm(list = ls())
library(dplyr)
library(ggthemes)
library(geobr)
library(ggplot2)
library(sf)
library(here)

load(here("data","ver_data.RData"))

muni <- read_municipality(year=2013)
states <- read_state(year=2013)

muni_a <- merge(muni, unique(ver[year == 2016,.(codeibge.x,sc_before,uf)]), by.x = 'code_muni', by.y = 'codeibge.x',all.x = T)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

map_sc <- ggplot() +
              geom_sf(data = muni_a, aes(fill=as.factor(sc_before)), color="black", size=.005,  show.legend = FALSE) +
              geom_sf(data = states, fill = NA,  color="gray50", size=.25,  show.legend = FALSE) +
              scale_fill_manual(values = c('white','navyblue')) +
              theme_minimal() +
              no_axis

#ggsave(here("writing","img","fig_A1_panel_b.png"), plot = map_sc, device = 'png',height = 14, width = 14, units = 'cm')