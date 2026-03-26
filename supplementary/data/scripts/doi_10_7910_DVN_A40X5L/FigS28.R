rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(tibble)
library(data.table)
library(spatstat)
library(scales)
library(readr)


### This script outputs Fig S28, which plots the within voter difference between spatial exposure
### and within reace spatial exposure for each racial subset.


## Load data
dat = fread('us-voters-exposure-isolation-within-race.csv')



# Format data for ggplot
l = dat %>%
  as_tibble%>%
  filter(race%in%c('Hispanic and Portuguese', 'Likely African-American', 'East and South Asian'))%>%
  mutate(race=if_else(race=='Likely African-American', 'Black',
                               if_else(race=='Hispanic and Portuguese', 'Hispanic',
                                       'Asian')),
         w.mean.d.post = if_else(race=='Black', w.mean.d.black.post, 
                                 if_else(race=='Hispanic', w.mean.d.hispanic.post,w.mean.d.asian.post)),
         w.mean.r.post = if_else(race=='Black', w.mean.r.black.post, 
                                 if_else(race=='Hispanic', w.mean.r.hispanic.post,w.mean.r.asian.post)),
         
         
         
         )%>%
  select(source_id,d.post, r.post,w.mean.d.post,w.mean.r.post, race) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d.post', 'w.mean.r.post'))
#gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

## Bring in main spatial exposure statistics
m=fread('us-voters-exposure-isolation-main.csv',select=c('source_id','w.mean.d.post','w.mean.r.post'))

l = left_join(l,as_tibble(m),by='source_id')

l = l%>%
  mutate(diff = ifelse((Party=='Democratic' & Exposure.Type == 'Isolation') |(Party=='Republican' & Exposure.Type == 'Exposure'), w.mean.d.post-Exposure, 
                        ifelse((Party=='Republican' & Exposure.Type == 'Isolation') |(Party=='Democratic' & Exposure.Type == 'Exposure'),w.mean.r.post-Exposure, NA))
         )


## Calculate mean and medians
vline.dat <- 
  l %>%
  drop_na %>%
  group_by(Exposure.Type, Party, race) %>%
  summarise(v.mean = weighted.mean(diff, w = Posterior, na.rm=T),v.med = weighted.median(diff, w = Posterior, na.rm=T)) 

gc()

## Output Figure S28
plot.name = 'FigS28.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=diff,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior, y =..density..),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~race) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .5))+
  ylab('Density')+
  theme_jake()

dev.off()
gc()

