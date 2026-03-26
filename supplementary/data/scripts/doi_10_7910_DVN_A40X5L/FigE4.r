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
library(cowplot)


## In this script we create Extended Data Figure 4, which compares the nationwide histogram of Democratic and Republican
### spatial Exposure and Isolation with imputation (as in the main analysis) and without.


# Load in the data

dat = fread('us-voters-exposure-isolation-main.csv')
dat = as.tibble(dat)
gc()


## Format data for ggplot 

l = dat %>%
  select(d.post, r.post,w.mean.d.post,w.mean.r.post) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d.post', 'w.mean.r.post'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))




l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Calculate weighted mean and median to plot verticla lines in the histograms
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 

source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")


plot.name = 'FigE4.jpeg'

#Create left panel of Fig E4

e4a=     ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0,2250000))+
  ylab('Count')+
  theme_jake()+
  theme(text=element_text( family = 'sans'))

# Bring in data on Isolation and Exposure without imputation
m = dat[,c('source_id','d.post','r.post')]

dat = fread('us-voters-exposure-isolation-no-imps.csv')
gc()

# 
dat = merge(dat,m,by='source_id',all.x=T)
dat = as.tibble(dat)
rm(m)
gc()


# Format data for ggplot

l = dat %>%
  select(d.post, r.post,w.mean.d.noimp,w.mean.r.noimp) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d.noimp', 'w.mean.r.noimp'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d.noimp', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Calculate weighted median and mean for vertical lines in histogram
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 



# Create right panel of Fig E4

e4b=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0,2250000))+
  ylab('Count')+
  theme_jake()+
  theme(text=element_text(family='sans'))


gc()

# Output Fig E4
ggsave(filename = plot.name, plot = plot_grid(e4a,e4b,nrow=1) , width = 360, height = 100,units='mm')
