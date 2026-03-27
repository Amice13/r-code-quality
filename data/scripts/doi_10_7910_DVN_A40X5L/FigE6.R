rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(tibble)
library(data.table)
library(scales)
library(spatstat)

## This script outputs Extended Data Figure 6, which plots the difference between spatial exposure
## and spatial exposure within same race neighbors for white voters. 


## load data
dat = fread('us-voters-exposure-isolation-white-neighbors.csv', select = c('source_id', 'd.post', 'r.post', 'w.mean.d.white.post', 'w.mean.r.white.post', 'race'))
m = fread('us-voters-variables.csv', select = c('source_id',  'race'))

dat=merge(dat,m, by='source_id', all.x=T)




## Load spatial exposure stats to compare to within race exposure
df = fread('us-voters-exposure-isolation-main.csv', select=c('source_id', 'w.mean.d.post', 'w.mean.r.post'))%>%as_tibble

dat=left_join(dat,df, by='source_id')
dat = as_tibble(dat)
rm(df)
gc()


## Format data for ggplot
l = dat %>%
  filter(race=='European')%>%
  select(d.post, r.post,w.mean.d.white.post,w.mean.r.white.post, w.mean.d.post, w.mean.r.post) %>%
  mutate(diff.r = w.mean.r.post-w.mean.r.white.post,
         diff.d = w.mean.d.post-w.mean.d.white.post)%>%
  select(d.post, r.post, diff.r, diff.d)%>%
  gather(Exposure.Type,Exposure,c('diff.r', 'diff.d'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'diff.d', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))




l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior, na.rm=T),v.med = weighted.median(Exposure, w = Posterior, na.rm=T)) 


## Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

## Output Figure E6
plot.name = 'FigE6.jpeg'
ggsave(plot.name, width = 180, height = 100,units = 'mm' ,

plot=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  ylab('Count')+
  theme_jake()+
  theme(text=element_text(family='sans'))+
  scale_y_continuous(labels=comma)
  

)
gc()


