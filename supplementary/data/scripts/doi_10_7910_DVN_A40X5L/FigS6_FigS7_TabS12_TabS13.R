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


#### Here we output the nationwide distributions of partisan exposure using squared distance weights (FigS6), 
#### and weighting by ordinal rank of neighbor proximity (FigS7).


### Load in data
dat = fread('us-voters-exposure-isolation-square-rank.csv')
df = fread('us-voters-exposure-isolation-main.csv', select=c('source_id', 'r.post','d.post'))
dat=merge(dat,df,all.x=T,by='source_id')
rm(df)


dat = as.tibble(dat)
dat=as.data.table(dat)
gc()


# Format data for ggplot for Figure S6
l.square = dat %>%
  select(d.post, r.post,w2.mean.d.post,w2.mean.r.post) %>%
  gather(Exposure.Type,Exposure,c('w2.mean.d.post', 'w2.mean.r.post'))


l.square = l.square%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w2.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))

l.square = l.square %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

gc()

# Format data for ggplot for Figure S7
l.rank = dat %>%
  select(d.post, r.post,rank.mean.d.post,rank.mean.r.post) %>%
  gather(Exposure.Type,Exposure,c('rank.mean.d.post', 'rank.mean.r.post'))


l.rank = l.rank%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'rank.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l.rank = l.rank %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

gc()

## Output Table S12
e.quants = l.square %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS12.tex', type='latex')

# Calculate mean and medians for Fig S6
vline.dat <- 
  l.square %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

# Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

# Output Figure S6
plot.name = 'FigS6.pdf'
require(scales)
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l.square,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma)+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

###


## Output TabS13
e.quants = l.rank %>%
group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS13.tex', type='latex')


## Calculate mean and median for FigS7
vline.dat <- 
  l.rank %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output Figure S7
plot.name = 'FigS7.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l.rank,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma)+
  ylab('Count')+
  theme_jake()

dev.off()
gc()
