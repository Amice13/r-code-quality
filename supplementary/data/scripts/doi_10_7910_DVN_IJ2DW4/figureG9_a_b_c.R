rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(sf)
library(ggplot2)
library(wesanderson)

## load data
load('./data/crime/hate_crime.RDS')
wm_boundaries <- st_read('./data/shapefiles/Westminster_Parliamentary_Constituencies_2017/Westminster_Parliamentary_Constituencies__December_2017__UK_BSC_V2.shp')

### Figure G.9: Constituencies with ethnic minority candidate winners and losers in close elections

crime <- unique(hate_crimes_w_at_1m, by=c('PCON17CD', 'election'))
crime[, treat:=ifelse(victory_margin>0,1,0)]
crime_2015 <- crime[election==2015, .(treat, victory_margin, PCON17CD)]
setnames(crime_2015, c('treat', 'victory_margin'), c('treat2015','margin2015'))
crime_2017 <- crime[election==2017, .(treat, victory_margin, PCON17CD)]
setnames(crime_2017, c('treat', 'victory_margin'), c('treat2017','margin2017'))
crime_2019 <- crime[election==2019, .(treat, victory_margin, PCON17CD)]
setnames(crime_2019, c('treat', 'victory_margin'), c('treat2019','margin2019'))

wm_boundaries <- merge(wm_boundaries,crime_2015,by='PCON17CD', all=TRUE)
wm_boundaries <- merge(wm_boundaries,crime_2017,by='PCON17CD', all=TRUE)
wm_boundaries <- merge(wm_boundaries,crime_2019,by='PCON17CD', all=TRUE)
wm_boundaries$country <- substr(wm_boundaries$PCON17CD,1,1)

ggplot() + 
  geom_sf(data=wm_boundaries[(wm_boundaries$country=='E' | wm_boundaries$country=='W'), ], aes(fill=NA), lwd=0.05) +
  geom_sf(data=wm_boundaries[!is.na(wm_boundaries$treat2015) & abs(wm_boundaries$margin2015)<22, ], aes(fill=as.factor(treat2015)),color=NA) +
  scale_fill_manual(values = wes_palette("Royal1", n = 2)) +
  labs(fill = paste('treatment','2015', sep='\n')) +
  theme(legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
  )

ggsave('./output/figures/figureG9a.pdf', width=6, height=4.85)

ggplot() + 
  geom_sf(data=wm_boundaries[(wm_boundaries$country=='E' | wm_boundaries$country=='W'), ], aes(fill=NA), lwd=0.05) +
  geom_sf(data=wm_boundaries[!is.na(wm_boundaries$treat2017) & abs(wm_boundaries$margin2017)<22, ], aes(fill=as.factor(treat2017)),color=NA) +
  scale_fill_manual(values = wes_palette("Royal1", n = 2)) +
  labs(fill = paste('treatment','2017', sep='\n')) +
  theme(legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
  )

ggsave('./output/figures/figureG9b.pdf', width=6, height=4.85)

ggplot() + 
  geom_sf(data=wm_boundaries[(wm_boundaries$country=='E' | wm_boundaries$country=='W'), ], aes(fill=NA), lwd=0.05) +
  geom_sf(data=wm_boundaries[!is.na(wm_boundaries$treat2019) & abs(wm_boundaries$margin2019)<22, ], aes(fill=as.factor(treat2019)),color=NA) +
  scale_fill_manual(values = wes_palette("Royal1", n = 2)) +
  labs(fill = paste('treatment','2019', sep='\n')) +
  theme(legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
  )

ggsave('./output/figures/figureG9c.pdf', width=6, height=4.85)