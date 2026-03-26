rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(ggplot2)
library(sf)

## load data
article_locations <- readRDS('./data/media/article_locations.RDS')
wm_boundaries <- st_read('./data/shapefiles/Westminster_Parliamentary_Constituencies_2017/Westminster_Parliamentary_Constituencies__December_2017__UK_BSC_V2.shp')
wm_boundaries <- merge(wm_boundaries, article_locations, by.x='PCON17CD', by.y='pcon18cd', all.y=TRUE)

### Figure D1. Geographical coverage of extracted articles
ggplot(data = wm_boundaries) + 
  geom_sf(aes(fill = articles/1000), lwd=0.005) +
  scale_fill_continuous(high = '#154360', low = '#a9cce3') +
  labs(fill = paste("Num. extracted articles", "(in thousands)", sep="\n")) + 
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())
ggsave('./output/figures/figureD1.pdf', width=6, height=4.85)

