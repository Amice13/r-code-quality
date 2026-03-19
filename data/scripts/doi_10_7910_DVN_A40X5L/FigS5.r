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


### Here we output FigS5, which plots the nationwide histogram of Democratic and Republican exposure 
### with discrete neighbors' partisanship



### Load data
dat = fread('us-voters-exposure-isolation-discrete.csv')
m = fread('us-voters-exposure-isolation-main.csv',select=c('source_id','d.post','r.post'))

dat=merge(dat,m,by='source_id',all.x=T)
dat = as.tibble(dat)
rm(m)
gc()


### Format data for ggplot
l = dat %>%
  select(d.post, r.post,w.mean.d,w.mean.r) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d', 'w.mean.r'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Calculate means and medians
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 


## Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")


## Output FigS5
plot.name = 'FigS5.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  ylab('Count')+
  scale_y_continuous(labels=comma)+
  theme_jake()
  

dev.off()
gc()



