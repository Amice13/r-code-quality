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


## This script outputs the tract exposure histograms in figures S14, S15, and S16, and the statistics for Table S16


## Load in data
dat = fread('us-voters-exposure-isolation-main.csv', select = c('source_id',  'w.mean.d.post','w.mean.r.post','d.post','r.post'))
gc()

df = fread('us-voters-variables.csv', select = c('source_id', 'res_state', 'res_countyfips','res_tract'))

dat = merge(dat,df,by.='source_id', by.y='source_id', all.x=T)

rm(df)
gc()

## Construct tract statistics
tracts = dat[!is.na(res_tract)&res_tract!='' & !is.na(res_countyfips) & res_countyfips!='' & !is.na(res_state) & res_state!=''][,
             list(
               n = .N,
               d = sum(d.post),
               r= sum(r.post),
               
               d.mean = mean(d.post, na.rm=T),
               r.mean = mean(r.post,na.rm=T)
               
               )
             ,
             by = c('res_state', 'res_countyfips', 'res_tract')]



### tract comparison

# merge in tract data
d = merge(dat, tracts[,-c('d.mean', 'r.mean')], by =  c('res_state', 'res_countyfips', 'res_tract'))
rm(dat)
gc()
# net out each voter from their own tract exposure measure
d[,d.mean:=(d-d.post)/(n-1)]
d[,r.mean:=(r-r.post)/(n-1)]

## calculate difference between spatial exposure and tract exposure
d[,tract.d.post.diff:=w.mean.d.post-d.mean]
d[,tract.r.post.diff:=w.mean.r.post-r.mean]


## calculate percent change between spatial exposure and tract.exposure
d[,tract.d.post.pct:=((w.mean.d.post/d.mean-1)*100)]
d[,tract.r.post.pct:=((w.mean.r.post/r.mean-1)*100)]

d = d[!is.na(d.mean)&!is.na(r.mean)]
gc()

# format data for ggplot

l = d %>%
  select(d.post, r.post,d.mean,r.mean) %>%
  gather(Exposure.Type,Exposure,c('d.mean', 'r.mean'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'd.mean', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Output first row of Table S16
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS16_tract.tex', type='latex')

# load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

## Calculate means and medians
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## output Figure S14
plot.name = 'FigS14.pdf'
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



##


### Fig S15



## Format data for ggplot
l = d %>%
  select(d.post, r.post,tract.d.post.diff,tract.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('tract.d.post.diff', 'tract.r.post.diff'))
gc()

l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'tract.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output second row of Table S16
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS16_tract_diff.tex', type='latex')

## Calculate mean and median
gc()
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output left panel of Fig S15
plot.name = 'FigS15a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = .2))+
  scale_y_continuous(labels=comma, limits = c(0,7250000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()


## PCT


## Format data for ggplot
l = d %>%
  select(d.post, r.post,tract.d.post.pct,tract.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('tract.d.post.pct', 'tract.r.post.pct'))

gc()


l = l%>%
  filter(!is.infinite(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'tract.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Output third row of Table S16
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS16_tract_pct.tex', type='latex')

## Calculate mean and median
gc()
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 

## Output FigS15b
plot.name = 'FigS15b.pdf'

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
  scale_y_continuous(labels=comma, limits = c(0,7250000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()




## FigS16: Absolute Differences

## Calculate absolute difference and absolute percent change
d[,tract.d.post.diff:=abs(w.mean.d.post-d.mean)]
d[,tract.r.post.diff:=abs(w.mean.r.post-r.mean)]

d[,tract.d.post.pct:=abs((w.mean.d.post/d.mean-1)*100)]
d[,tract.r.post.pct:=abs((w.mean.r.post/r.mean-1)*100)]

## Format data for ggplot
l = d %>%
  select(d.post, r.post,tract.d.post.diff,tract.r.post.diff) %>%
  gather(Exposure.Type,Exposure,c('tract.d.post.diff', 'tract.r.post.diff'))
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'tract.d.post.diff', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fourth row of table S16
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS16_tract_diff_abs.tex', type='latex')


gc()

## Calculat mean and medians
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 


## Output left panel of Figure S16

plot.name = 'FigS16a.pdf'

pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=Exposure,fill= Party,color = Party))+
  geom_histogram(aes(weight=Posterior),binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Exposure.Type ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from =0, to = 1, by = .1))+
  scale_y_continuous(labels=comma, limits = c(0,10000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()


### tract absolute pct

## Format data for ggplot
l = d %>%
  select(d.post, r.post,tract.d.post.pct,tract.r.post.pct) %>%
  gather(Exposure.Type,Exposure,c('tract.d.post.pct', 'tract.r.post.pct'))
rm(d)
gc()

l = l%>%
  filter(!is.na(Exposure))%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'tract.d.post.pct', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))


## Output fifth row of Table S16
e.quants = l %>%
  group_by(Exposure.Type, Party) %>%
  summarise(q.1 = list(enframe(weighted.quantile(Exposure,probs = c(.01,.1,.25,.5,.75,.9,.99), w = Posterior)))) %>%
  unnest()
e.quants = spread(e.quants,name,value)  
out.table = xtable(e.quants)
print(out.table,'TabS16_tract_pct_abs.tex', type='latex')


gc()

## Calculate mean and median
vline.dat <- 
  l %>%
  filter(!is.infinite(Exposure))%>%
  group_by(Exposure.Type, Party) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior,na.rm=T),v.med = weighted.median(Exposure, w = Posterior)) 


## Output right panel of Figure S16
plot.name = 'FigS16b.pdf'

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
  scale_y_continuous(labels=comma, limits = c(0,10000000))+
  ylab('Count')+
  theme_jake()

dev.off()
gc()
