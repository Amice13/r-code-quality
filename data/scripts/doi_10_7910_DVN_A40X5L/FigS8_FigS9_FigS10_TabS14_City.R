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


## This script outputs the City exposure histograms in figures S8, S9, and S10, and the statistics for Table S14


## Load in data 
dat = fread('us-voters-exposure-isolation-main.csv', select = c('source_id',  'w.mean.d.post','w.mean.r.post','d.post','r.post'))

df = fread('us-voters-variables.csv', select = c('source_id', 'res_state', 'res_place'))

dat = merge(dat,df,by.='source_id', by.y='source_id', all.x=T)

rm(df)
gc()



## Create city statistics


city = dat[!is.na(res_place)&res_place!=''& !is.na(res_state) & res_state!=''][,
          list(
            n = .N,
            d = sum(d.post),
            r= sum(r.post),
            
            d.mean = mean(d.post, na.rm=T),
            r.mean = mean(r.post,na.rm=T)
          )
          ,
          by = c('res_state',  'res_place')]

##### Merge city data

d = merge(dat, city[,-c('d.mean', 'r.mean')], by =  c('res_state', 'res_place'))

# net out each voter from their own city exposure measure
d[,d.mean:=(d-d.post)/(n-1)]
d[,r.mean:=(r-r.post)/(n-1)]

# calculate difference between spatial exposure and city exposure
d[,city.d.post.diff:=w.mean.d.post-d.mean]
d[,city.r.post.diff:=w.mean.r.post-r.mean]

d = d[!is.na(d.mean)&!is.na(r.mean)]
rm(dat)
gc()


# Format data for ggplot
l = d %>%
  select(d.post, r.post,d.mean,r.mean) %>%
  gather(Exposure.Type,Exposure,c('d.mean', 'r.mean'))
gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'd.mean', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## output first row of Table S14
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS14_city.tex', type='latex')


gc()

# Calculate mean and medians
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 

## Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

# Output Figure S8
plot.name = 'FigS8.pdf'
gc()

pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
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



##


### city diff

# Format data for ggplot
l = d %>%
  select(d.post, r.post,city.d.post.diff,city.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('city.d.post.diff', 'city.r.post.diff'))
gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'city.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output second row of Table S14
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS14_city_diff.tex', type='latex')

## Calculate mean and medians
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 

gc()

#Output left panel of Figure S9
plot.name = 'FigS9a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  scale_y_continuous(labels=comma, limits = c(0,4000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()



### pct

# Calculate percentage change between spatial expousre and city exposure
d[,city.d.post.pct:=(w.mean.d.post/d.mean-1)*100]
d[,city.r.post.pct:=(w.mean.r.post/r.mean-1)*100]

d = d[!is.na(d.mean)&!is.na(r.mean)]
gc()


### city pct

# Format data for ggplot

l = d %>%
  select(d.post, r.post,city.d.post.pct,city.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('city.d.post.pct', 'city.r.post.pct'))


l = l%>%
  filter(!is.infinite(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'city.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Output third row of Table S14
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS14_city_pct.tex', type='latex')

## Calculate mean and median
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 


gc()

## Output right panel of Figure S9
plot.name = 'FigS9b.pdf'
gc()
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=2,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  #scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  xlim(-100,100)+
  scale_y_continuous(labels=comma, limits=c(0,4000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()


## Absolute

# Calculate absolute difference and absolute percentage change
d[,city.d.post.diff:=abs(w.mean.d.post-d.mean)]
d[,city.r.post.diff:=abs(w.mean.r.post-r.mean)]


d[,city.d.post.pct:=abs((w.mean.d.post/d.mean-1)*100)]
d[,city.r.post.pct:=abs((w.mean.r.post/r.mean-1)*100)]

d = d[!is.na(d.mean)&!is.na(r.mean)]
gc()

### city diff


## format data for ggpot
l = d %>%
  select(d.post, r.post,city.d.post.diff,city.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('city.d.post.diff', 'city.r.post.diff'))
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'city.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fourth row of Table S14
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS14_city_diff_abs.tex', type='latex')

## Calculate mean and median
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 

gc()

# Output left panel of Fig S10
plot.name = 'FigS10a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0,5000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

### city pct

# Format data for ggplot
l = d %>%
  select(d.post, r.post,city.d.post.pct,city.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('city.d.post.pct', 'city.r.post.pct'))
rm(d)
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'city.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fifth row of TabS15
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS14_city_pct_abs.tex', type='latex')


gc()

## Calculate mean and median
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior),v.med = weighted.median(Exposure, w = Posterior)) 


## Output right panel of Figure S10
plot.name = 'FigS10b.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=1,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  #scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  xlim(0,100)+
  scale_y_continuous(labels=comma, limits = c(0,5000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()
