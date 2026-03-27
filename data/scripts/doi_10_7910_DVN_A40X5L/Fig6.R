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

## In this script we output Figure 6 in the main text, which reports the average levels of 
## Democratic and Republican Exposure subset by voter race


## Load in dataa
d = fread('us-voters-exposure-isolation-main.csv', select = c('source_id',  'w.mean.d.post','w.mean.r.post', 'd.post','r.post'))

df = fread('us-voters-variables.csv', select=c('source_id','race'))

d=merge(d,df,by.x='source_id', by.y='source_id', all.x=T)
rm(df)
gc()



d[,white:=ifelse(race == 'European', 'White', ifelse(race == '', 'Unknown', 'Non-White'))]
d[,white:= factor(white, levels = c('White', 'Non-White', 'Unknown'))]



## Create White/No-white weighted quantiles

r= d[white !='Unknown'][,list(n=.N, d = sum(d.post), r=sum(r.post),
                              iso.r=weighted.mean(x=w.mean.r.post, w=r.post),
                              iso.r.50=weighted.median(x=w.mean.r.post, w= r.post),
                              iso.r.25 = weighted.quantile(x=w.mean.r.post, w=r.post, probs = .25),
                              iso.r.75 = weighted.quantile(x=w.mean.r.post, w=r.post, probs = .75),
                              
                              exp.r=weighted.mean(x=w.mean.d.post, w=r.post),
                              exp.r.50=weighted.median(x=w.mean.d.post, w= r.post),
                              exp.r.25 = weighted.quantile(x=w.mean.d.post, w=r.post, probs = .25),
                              exp.r.75 = weighted.quantile(x=w.mean.d.post, w=r.post, probs = .75),
                              
                              iso.d=weighted.mean(x=w.mean.d.post, w=d.post),
                              iso.d.50=weighted.median(x=w.mean.d.post, w= d.post),
                              iso.d.25 = weighted.quantile(x=w.mean.d.post, w=d.post, probs = .25),
                              iso.d.75 = weighted.quantile(x=w.mean.d.post, w=d.post, probs = .75),
                              
                              exp.d=weighted.mean(x=w.mean.r.post, w=d.post),
                              exp.d.50=weighted.median(x=w.mean.r.post, w= d.post),
                              exp.d.25 = weighted.quantile(x=w.mean.r.post, w=d.post, probs = .25),
                              exp.d.75 = weighted.quantile(x=w.mean.r.post, w=d.post, probs = .75)
                              
                              
                              
),by='white']


## Format data for ggplot

dat = r%>%
  as_tibble%>%
  select(white,exp.r:exp.r.75,exp.d:exp.d.75)%>%
  pivot_longer(exp.r:exp.d.75)%>%
  mutate(party = if_else(grepl('d', name), 'Democratic', 'Republican'),
         stat = substr(name,nchar(name)-1,nchar(name)),
         stat = if_else(grepl('d', stat)|grepl('r', stat), 'Mean', paste0(stat,'th Percentile')),
         value = round(value, 3))%>%
  select(-name)


# Output Fig 6

source('theme_jake.R')
colors = c(Democratic = '#377EB8', Republican = '#E41A1C')

ggsave(filename='Fig6.pdf', width = 100, height = 100, units='mm',
plot=ggplot(dat, aes(y = stat, x = white, fill = party, alpha = 1 - value, label = sprintf("%0.2f", round(value, digits = 2))))+
  geom_tile()+
  scale_fill_manual(values=colors)+
  geom_text( size = 5, alpha =1, color = 'black') +
  # scale_x_discrete(position = "top") +
  coord_equal() +
  geom_vline(xintercept = 1:4 - 0.5, colour = "white", size = 1.5) +
  geom_hline(yintercept = 1:4 - 0.5, colour = "white", size = 1.5) +
  ylab("") +
  xlab("")+
  theme_jake_tile2()+
  theme(text=element_text(family='sans'))+
  facet_grid(cols = vars(party))#+

)

