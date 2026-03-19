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
library(gridExtra)
library(grid)



## In this script we create the nationwide histograms subset by density and metro area classifications.
## We also output Table 1, which reports the percentage of voters in each density classification.


## Load in data
dat = fread('us-voters-exposure-isolation-main.csv', select = c('source_id',  'w.mean.d.post','w.mean.r.post','d.post','r.post'))

df = fread('us-voters-variables.csv', select=c('source_id','urban.area', 'classification2'))


## Table 1

t = df[classification2!='',list(p=.N/nrow(df)),by='classification2']
t[,hh:=c('< 102', '[102,800)','[800,2213)','>=2213')]
t[,p:=paste0(round(100*p,2),'%')]
t=t[,c('classification2', 'hh', 'p')]
names(t)= c('Classification', 'Households Per Square Mile', '% of Voters')
t=xtable(t)
print(t,file='Tab1_density_classifications.tex')
##

## Merge data
dat=merge(dat,df,by.x='source_id', by.y='source_id', all.x=T)
rm(df)
gc()

dat=as_tibble(dat)
gc()


## Format data to plot in ggplot
l = dat %>%
  filter(!is.na(classification2) & classification2!='')%>%
  select(d.post, r.post,w.mean.d.post,w.mean.r.post, classification2, urban.area) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d.post', 'w.mean.r.post'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))
gc()


l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))%>%
  filter(Exposure.Type=='Exposure')
gc()



l = l%>%
  mutate(classification2 = factor(classification2, levels = c('High density', 'Medium density', 'Low density', 'Very low density')))

l = l %>%
  drop_na

## Load in personalized ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")

plot.name = 'Fig4.pdf'



## Create counts in each metro area/density classification combination
counts = l %>%
  group_by(Party, urban.area,classification2)%>%
  summarize(n = sum(Posterior))

### Calculate weighted mean and weighted median to plot vertical lines in histograms
vline.dat <- 
  l %>%
  group_by(Exposure.Type, Party, classification2,urban.area) %>%
  summarise(v.mean = weighted.mean(Exposure, w = Posterior, na.rm=T),v.med = weighted.median(Exposure, w = Posterior, na.rm=T)) 

gc()

## Create plot
p=ggplot(l,aes(x= Exposure, color = Party, fill=Party))+
  geom_histogram(aes(weight=Posterior, y = ..density..), binwidth=.02,alpha = .5, position = 'identity') +
  geom_text(data=counts %>% filter(Party=='Democratic'),aes(label = prettyNum(round(n), ',')), x = .85, y = 4, size=2)+
  geom_text(data=counts %>% filter(Party=='Republican'),aes(label = prettyNum(round(n), ',')), x = .85, y = 3.5,size=2)+
#geom_density(aes(y=(..density..)*2000),alpha =0,adjust = .5)+
  facet_grid(urban.area~classification2) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .2))+
  theme_jake()+
  theme(text=element_text(family='sans'))+
  scale_y_continuous(labels=comma)+
  ylab('Distribution Density')


rm(dat,l)
gc()


### Create Grob object in order to customize plot
g = ggplotGrob(p)
gc()


## Customize plot
rm_grobs = g$layout$name %in% c('panel-3-1')
g$grobs[rm_grobs] = NULL
g$layout = g$layout[!rm_grobs,]
g$layout[g$layout$name == 'axis-l-3', c('l', 'r')]=c('6', '6')
g$layout[g$layout$name == 'axis-b-1', c('t', 'b')]=c('11', '11')


## output Figure 4
grid.newpage()
ggsave(filename=plot.name, width = 180, height = 150,units='mm' ,

plot=grid.draw(g)
)


