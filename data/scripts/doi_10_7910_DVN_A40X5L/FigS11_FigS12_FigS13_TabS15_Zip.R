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


## This script outputs the Zip Code exposure histograms in figures S11, S12, and S13, and the statistics for Table S15


### Load data

dat = fread('us-voters-exposure-isolation-main.csv', select = c('source_id',  'w.mean.d.post','w.mean.r.post','d.post','r.post'))

df = fread('us-voters-variables.csv', select = c('source_id', 'res_state', 'zip'))

dat = merge(dat,df,by.='source_id', by.y='source_id', all.x=T)

rm(df)
gc()

## Create zip code statistics

zip = dat[!is.na(zip)& zip!='' & !is.na(res_state) & res_state!=''][,
          list(
            n = .N,
            d = sum(d.post),
            r= sum(r.post),
           
            
            d.mean = mean(d.post, na.rm=T),
            r.mean = mean(r.post,na.rm=T)
             )
             ,
             by = c('res_state',  'zip')]





#### Merge data 

d = merge(dat, zip[,-c('d.mean', 'r.mean')], by =  c('res_state', 'zip'))

## Net out each voter from their own zip code level exposure
d[,d.mean:=(d-d.post)/(n-1)]
d[,r.mean:=(r-r.post)/(n-1)]

## Calculate difference between spatial exposure and 
d[,zip.d.post.diff:=w.mean.d.post-d.mean]
d[,zip.r.post.diff:=w.mean.r.post-r.mean]

d = d[!is.na(d.mean)&!is.na(r.mean)]
gc()
rm(dat)
gc()

## Format dat for ggplot

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


## output first row of Table S15
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'tables/supplementary/TabS15_zip.tex', type='latex')

# Calculate mean and median
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 
gc()

## Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")


## Output Figure S11
plot.name = 'FigS11.pdf'

pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0, 2250000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()



##


### zip diff

# Format data for ggplot

l = d %>%
  select(d.post, r.post,zip.d.post.diff,zip.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('zip.d.post.diff', 'zip.r.post.diff'))
gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'zip.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Output second row of Table S15
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'tables/supplementary/TabS15_zip_diff.tex', type='latex')

## Calculate mean and median
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 


gc()

## Output left panel of Figure S12
plot.name = 'FigS12a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  scale_y_continuous(labels=comma, limits = c(0,6000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()


# PCT

## Calculate percentage change from spatial exposure to zip code exposure
d[,zip.d.post.pct:=(w.mean.d.post/d.mean-1)*100]
d[,zip.r.post.pct:=(w.mean.r.post/r.mean-1)*100]

d = d[!is.na(d.mean)&!is.na(r.mean)]
gc()




### Format data for ggplot

l = d %>%
  select(d.post, r.post,zip.d.post.pct,zip.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('zip.d.post.pct', 'zip.r.post.pct'))


gc()

l = l%>%
  filter(!is.infinite(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'zip.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output third row of Table S15
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'tables/supplementary/TabS15_zip_pct.tex', type='latex')


## Mean and median
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

gc()

## output right panel of Figure S12
plot.name = 'FigS12b.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=2,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  xlim(-100,100)+
  scale_y_continuous(labels=comma, limits = c(0,6000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()



## Absolute

## Calculate absolute difference and absolute percent change
d[,zip.d.post.diff:=abs(w.mean.d.post-d.mean)]
d[,zip.r.post.diff:=abs(w.mean.r.post-r.mean)]

d[,zip.d.post.pct:=abs((w.mean.d.post/d.mean-1)*100)]
d[,zip.r.post.pct:=abs((w.mean.r.post/r.mean-1)*100)]


# Format data for ggplot
l = d %>%
  select(d.post, r.post,zip.d.post.diff,zip.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('zip.d.post.diff', 'zip.r.post.diff'))
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'zip.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fourth row of Table S15
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'tables/supplementary/TabeS15zip_diff_abs.tex', type='latex')


gc()

# Calculate mean and median
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 


gc()


# output left panel of Figure S13
plot.name = 'FigS13a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits=c(0,8000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()

### zip pct


# Format data for ggplot
l = d %>%
  select(d.post, r.post,zip.d.post.pct,zip.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('zip.d.post.pct', 'zip.r.post.pct'))
rm(d)
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'zip.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fifth row of Table S15
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'tables/supplementary/TabS15_zip_pct_abs.tex', type='latex')

## Calculate mean and median
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 


gc()

## output lright panel of Figure S13
plot.name = 'FigS13b.pdf'

pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=1,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  xlim(0,100)+
  #scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  scale_y_continuous(labels=comma, limits=c(0,8000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()
