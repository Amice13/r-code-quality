rm(list = ls())
gc()
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(tibble)
library(data.table)
library(scales)
library(spatstat)


## This script outputs the precinct exposure histograms in figures S17, S18, and S19, and the statistics for Table S17

## Load data
dat = fread('us-voters-exposure-isolation-main.csv', select = c('source_id',  'w.mean.d.post','w.mean.r.post','d.post','r.post'))

df = fread('us-voters-variables.csv', select = c('source_id', 'clinton.voteshare','trump.voteshare'))

dat = merge(dat,df,by.='source_id', by.y='source_id', all.x=T)

rm(df)
gc()


## Calculate difference between spatial exposure and precinct exposure
dat[,precinct.d.post.diff:=w.mean.d.post-clinton.voteshare]
dat[,precinct.r.post.diff:=w.mean.r.post-trump.voteshare]


## Calculate percentage change between spatial exposure and precinct exposure
dat[,precinct.d.post.pct:=(w.mean.d.post/clinton.voteshare-1)*100]

dat[,precinct.r.post.pct:=(w.mean.r.post/trump.voteshare-1)*100]


## Figure S17

## format data for ggplot 

l = dat %>%
  select(d.post, r.post,clinton.voteshare,trump.voteshare) %>%
  gather(Exposure.Type,Exposure,c('clinton.voteshare', 'trump.voteshare'))
gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'clinton.voteshare', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


# Output top row of Table S17
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS17_precinct.tex', type='latex')


## Calculate means and medians
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## load custom ggplot themes
source('code/theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

## Output Figure S17
plot.name = 'FigS17.pdf'

pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0,2250000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()



## Figure S18
## Figure S18 consists of two panels, the distribution of the difference between spatial exposure and precinct exposure,
## and the percentage change. 


### Format data for ggplot2

l = dat %>%
  select(d.post, r.post,precinct.d.post.diff,precinct.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('precinct.d.post.diff', 'precinct.r.post.diff'))
gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'precinct.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Output second row of Table S17
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS17_precinct_diff.tex', type='latex')


## Calculate means and medians
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output left paenl of FigS18
plot.name = 'FigS18a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  scale_y_continuous(labels=comma, limits = c(0, 7000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

## Format data for ggplot

l = dat %>%
  select(d.post, r.post,precinct.d.post.pct,precinct.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('precinct.d.post.pct', 'precinct.r.post.pct'))
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'precinct.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output third row of Table S17
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS17_exposure_quants_posteriors_precinct_pct.tex', type='latex')


gc()

## Caculate means and medians
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output right panel of FigS18
plot.name = 'FigS18b.pdf'
gc()

pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=2,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  xlim(-100,100)+
  scale_y_continuous(labels=comma, limits = c(0,7000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

## Figure S19: Absolute differences


## Calculate absolute difference and percentage changes
dat[,precinct.d.post.diff:=abs(w.mean.d.post-clinton.voteshare)]
dat[,precinct.r.post.diff:=abs(w.mean.r.post-trump.voteshare)]

dat[,precinct.d.post.pct:=abs((w.mean.d.post/clinton.voteshare-1)*100)]
dat[,precinct.r.post.pct:=abs((w.mean.r.post/trump.voteshare-1)*100)]


gc()


##

## Format data for ggplot2
l = dat %>%
  select(d.post, r.post,precinct.d.post.diff,precinct.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('precinct.d.post.diff', 'precinct.r.post.diff'))
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'precinct.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output foruth row of TabS17
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS17_precinct_diff_abs.tex', type='latex')


## Calculate mean and medians
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output left panel of FigS19
plot.name = 'FigS19a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0,9000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

### Format percentage change data for ggplot

l = dat %>%
  select(d.post, r.post,precinct.d.post.pct,precinct.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('precinct.d.post.pct', 'precinct.r.post.pct'))
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'precinct.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))



l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fifth row of Table S17
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS17_pct_abs.tex', type='latex')


## Calculate mean and medians
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output right panel of Fig S19
plot.name = 'FigS19b.pdf'
require(scales)
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=1,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  xlim(0,100)+
  scale_y_continuous(labels=comma,limits =c(0,9000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

