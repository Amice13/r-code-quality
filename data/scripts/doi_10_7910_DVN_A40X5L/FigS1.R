rm(list=ls())

library(dplyr)
library(readr)
library(data.table)
library(tidyr)
library(tibble)
library(ggplot2)
library(spatstat)


### Here, we output Figure S1 in the Supplemental Information, which plots how the Isolation and Exposure
### calculations change with larger numbers of neighbors. 


df=fread('sample-50k.csv')
p=fread('us-voters-exposure-isolation-main.csv',select=c('source_id', 'r.post', 'd.post'))
df= merge(df,p,by='source_id', all.x=T)


# Format data for ggplot

l =df %>%
  select(source_id,d.post, r.post,w.mean.d.post,w.mean.r.post, neighbors) %>%
  gather(Exposure.Type,Exposure,c('w.mean.d.post', 'w.mean.r.post'))


l = l%>%
  mutate(Exposure.Type = ifelse(Exposure.Type == 'w.mean.d.post', 'Democratic', 'Republican'))%>%
  gather(Party, Posterior, c('d.post', 'r.post'))

l = l %>%
  mutate(Party = ifelse(Party=='d.post', 'Democratic', 'Republican'))%>%
  mutate(Exposure.Type = ifelse(Party==Exposure.Type, 'Isolation', 'Exposure'))

## Calculate quantiles
m = l %>%
  drop_na %>%
  group_by(neighbors, Party, Exposure.Type)%>%
  summarize(mean = weighted.mean(Exposure,w=Posterior),
            q.01 = weighted.quantile(Exposure,w=Posterior, probs = .01),
            q.10 = weighted.quantile(Exposure,w=Posterior, probs = .1),
            q.25 = weighted.quantile(Exposure,w=Posterior, probs = .25),
            q.5 = weighted.quantile(Exposure,w=Posterior, probs = .5),
            q.75 = weighted.quantile(Exposure,w=Posterior, probs=.75),
            q.90 = weighted.quantile(Exposure,w=Posterior, probs=.9),
            q.99 = weighted.quantile(Exposure,w=Posterior, probs=.99))

m = m %>%
  pivot_longer(mean:q.99)%>%
  mutate(type = if_else(name=='mean', 'mean', 'percentile'))


# Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")


m= m %>% mutate(lt = if_else(name=='q.5', 'A', ifelse(name%in% c('q.25','q.75'), 'C', 'B')))


pdf('FigS1.pdf',width=9,height=5,pointsize=9)
ggplot(m %>% filter(name%in%c('q.5', 'q.25', 'q.90', 'q.10', 'q.75')), aes(x=neighbors, y = value, color = Party, fill=Party, group = name))+
  geom_line(aes(linetype=lt))+
  theme_jake2()+
  xlab('Neighbors')+
  ylab('')+
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(xintercept=1000, color = 'black',linetype='dashed')+
  facet_grid(Exposure.Type~Party)
dev.off()

