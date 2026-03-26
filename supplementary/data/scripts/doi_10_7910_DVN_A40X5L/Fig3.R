rm(list = ls())
gc()
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(tibble)
library(data.table)
library(spatstat)
library(scales)
library(tibble)
library(glue)
library(cowplot)


### This script creates Figure 3 in the main text, which consists of 
### the nationwide histogram of Republican and Democratic Isolation and Exposure,
### and the colored quantile tables for these distributions.



### Load in the data
dat = fread('us-voters-exposure-isolation-main.csv')
dat = as.tibble(dat)
gc()



## transform data for ggplot
l = dat %>%
  select(d.post, r.post,w.mean.d.post,w.mean.r.post) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d.post', 'w.mean.r.post'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Calculate weighted quantiles for the table

 e.quants = l %>%
   group_by(Exposure.Type, Party) %>%
   summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
   unnest()
 e.quants = spread(e.quants,name,value)  


## Calculate weighted mean and median values in plot data format to plot
## vertical lines in the distribution

gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 


## Load specialized themes for ggplot
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

plot.name = 'Fig3.pdf'



## Create histogram

p=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma)+
  ylab('Count')+
  theme_jake()+
  theme(text=element_text(family='sans'))
  





### Create quantile data in format for ggplot 


d = tibble(value = round(unlist(c(as.vector(e.quants[1,3:ncol(e.quants)]),
                              as.vector( e.quants[2,3:ncol(e.quants)]),
                               as.vector(e.quants[3,3:ncol(e.quants)]),
                               as.vector(e.quants[4,3:ncol(e.quants)]))),2),
           Percentile = rep(c(1,10,25,50,75,90,99),4),
           party = c(rep('Democratic', 7), rep('Republican', 7),rep('Democratic', 7), rep('Republican',7)),
           exposure.type = c(rep('Exposure',14), rep('Isolation', 14))
)%>%
  mutate(Percentile = if_else(Percentile==1, paste0(Percentile,'st'), paste0(Percentile,'th')),
         # exposure.type=if_else(exposure.type=='Exposure', 'E*', 'I*'),
         Percentile = factor(Percentile, levels = c('1st', '10th', '25th',
                                                    '50th', '75th', '90th', '99th'))
  )

## create ggplot quantile table

t=ggplot(d, aes(y = party, x = Percentile, fill = party, alpha = ((1 - value)^as.numeric(exposure.type=='Exposure'))*(value^as.numeric(exposure.type=='Isolation')), label = sprintf("%0.2f", round(value, digits = 2))))+
  geom_tile()+
  scale_fill_manual(values=colors)+
  geom_text( size = 5, alpha =1, color = 'black') +
  #scale_x_discrete(position = "top") +
  coord_equal() +
  geom_vline(xintercept = 1:8 - 0.5, colour = "white", size = 1.5) +
  geom_hline(yintercept = 1:8 - 0.5, colour = "white", size = 1.5) +
  ylab("") +  
  coord_equal() +
  theme_jake_tile2()+
  theme(text=element_text(family='sans'))+
  facet_grid(rows = vars(exposure.type))#+
#labs(caption = "\nCells: Spatially weighted proportion of out-party (Exposure) or in-party (Isolation) neighbors.\nDistributions are weighted by posterior partisanship probabilities.")#+
#ggtitle('Spatial Exposure and Isolation\n')


rm(l,dat)
gc()


## output Figure 3

ggsave(filename = plot.name, width = 180, height=200,units = 'mm',
plot = plot_grid(p,t,ncol=1)
)


