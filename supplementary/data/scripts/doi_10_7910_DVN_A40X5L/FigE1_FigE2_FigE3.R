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
library(cowplot)

# In this script we create Extended Data Figures 1-3, which plot the nationwide distribution
# of aspatial Democratic and Republican Isolation and Exposure, the within-voter differences in spatial 
# verus aspatial democratic and Republican Isolation and Exposure, and the absolute differences in spatial
# versus aspatial Democratic and Republican Isolation and Exposure. Figure E1 also contains the nationwide 
# distribution of spatial exposure from Figure 1 in the main text, so we recreate that here then combine the figures
# when outputting Figure E1.


## Load in the data

dat = fread('us-voters-exposure-isolation-main.csv')
dat = as.tibble(dat)
gc()


###############
## Figure E1 ##
###############

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
gc()

## Calculated weighted mean and median to plot vertical lines in the histograms

vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 

source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")


plot.name = 'FigE1.jpeg'


## Create left panel of figure E1

  e1a=     ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
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


#### Create plot data for the aspatial distribution
l = dat %>%
  select(d.post, r.post,mean.d.post,mean.r.post) %>%
  gather(Exposure.Type,Exposure,c('mean.d.post', 'mean.r.post'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))
gc()


## Calculated weighted mean and median to plot vertical lines in the histograms

vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 

## Create right panel of figure E1

e1b = ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
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


### Output figure E1


ggsave(filename = plot.name, width = 360, height = 100, units = 'mm', plot = plot_grid(e1a, e1b, nrow =1))

rm(e1a,e1b)
gc()



###############
## Figure E2 ##
###############

## Create within voter difference variables

dat = dat %>% mutate(diff.d = w.mean.d.post-mean.d.post, diff.r=w.mean.r.post-mean.r.post)
gc()


## Format data for ggplot

l = dat %>%
  select(d.post, r.post,diff.d,diff.r) %>%
  gather(Exposure.Type,Exposure,c('diff.d', 'diff.r'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'diff.d', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))

gc()
require(spatstat)

l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))
##exposure
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)

## Calculate weighted median and mean values to plot vertical lines in the histogram
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 


# Create left panel of Figure E2

p1=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  scale_y_continuous(labels=comma, limits = c(0,8250000))+
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  ylab('Count')+
  xlab('Percentage Point Change')+
  theme_jake()+
  theme(text=element_text(family='sans'))


### pct diff

# Create percentage distance variables 

dat = dat %>% mutate(pct.d = (w.mean.d.post/mean.d.post-1)*100, pct.r=(w.mean.r.post/mean.r.post-1)*100)
gc()


# Format data for ggplot

l = dat %>%
  select(d.post, r.post,pct.d,pct.r) %>%
  gather(Exposure.Type,Exposure,c('pct.d', 'pct.r'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'pct.d', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))

gc()


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


##exposure
# e.quants = l %>%
#   group_by(Exposure.Type, Party) %>%
#   summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
#   unnest()
# e.quants = spread(e.quants,name,value)  
# out.table = xtable(e.quants)

gc()

# Calculate weighted median and means to plot vertical lines in the histograms

vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior, na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 


     

# Create right panel of figure E2

p2=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=1,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(labels=comma, limits = c(0,8250000))+
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  xlim(-100,100)+
  ylab('Count')+
  xlab('Percent Change')+
  theme_jake()+
  theme(text=element_text(family='sans'))



#Output figure E2

ggsave(plot = plot_grid(p1,p2,nrow=1), file = 'FigE2.jpeg', width = 360, height=100,units='mm')

rm(p1,p2)
gc()

###############
## Figure E3 ##
###############

## Create absolute difference variables

dat = dat %>% mutate(abs.diff.d = abs(w.mean.d.post-mean.d.post), abs.diff.r=abs(w.mean.r.post-mean.r.post))
gc()


## Format data for ggplot

l = dat %>%
  select(d.post, r.post,abs.diff.d,abs.diff.r) %>%
  gather(Exposure.Type,Exposure,c('abs.diff.d', 'abs.diff.r'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'abs.diff.d', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))

gc()


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))



## Calculated weighted medians and means
gc()


vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 



# Create left panel of Figure E3

p1=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_y_continuous(labels=comma, limits = c(0, 12750000))+
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  ylab('Count')+
  xlab('Absolute Percentage Point Change')+
  theme(text=element_text(family='sans'))+
  theme_jake()


gc()



### Create absolute percentage difference variables

dat = dat %>% mutate(abs.pct.d = abs((w.mean.d.post/mean.d.post-1)*100), abs.pct.r=abs((w.mean.r.post/mean.r.post-1)*100))
gc()


## Format data for ggplot

l = dat %>%
  select(d.post, r.post,abs.pct.d,abs.pct.r) %>%
  gather(Exposure.Type,Exposure,c('abs.pct.d', 'abs.pct.r'))
gc()


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'abs.pct.d', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))




l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Calculated weighted means and medians
gc()


vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior, na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

# Create right panel of figure E3

p2=ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=1,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10))+
  scale_y_continuous(labels=comma, limits = c(0, 12750000))+
  xlim(0,100)+
  ylab('Count')+
  xlab('Absolute Percent Change')+
  theme(family='sans')+
  theme_jake()


# Output figure E3

ggsave(plot = plot_grid(p1,p2,nrow=1), file = 'FigE3.jpeg',width=360,height=100,units='mm')

rm(p1,p2)

gc()
