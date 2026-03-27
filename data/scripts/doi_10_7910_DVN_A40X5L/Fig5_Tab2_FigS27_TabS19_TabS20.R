


rm(list=ls())
source('theme_jake.R')
library(spatstat)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(glue)
library(scales)
library(data.table)


#### In this script, we create the relative exposure histograms in Figure 5 in the main text and 
#### the relative exposure histograms with same household neighbors dropped from Fig S27 in the 
#### Supplementary Information. We also create Tables S19 and S20, which show the significance tests
#### for relative exposure. We also create Table 2 in the main text, reporting the number of people covered
#### by each geographic variable.


## Load in data

load("relative-exposure-aggregate-data.Rdata")

## Specify colors for plots
colors = c(Democratic = '#377EB8', Republican = '#E41A1C')


## combine data into long plot format

df = state %>% 
  as_tibble %>%
  select(n, diff.r, diff.d)%>% 
  mutate(basegeo = 'State')%>%
  bind_rows(cbsa %>%
              as_tibble %>%
              select(n, diff.r, diff.d) %>% 
              mutate(basegeo = 'CBSA'))%>%
  bind_rows(county %>%
              as_tibble %>%
              select(n, diff.r, diff.d)%>% 
              mutate(basegeo = 'County'))%>%
  bind_rows(city %>%
              as_tibble %>%
              select(n, diff.r, diff.d)%>% 
              mutate(basegeo = 'City/Town'))%>%
  bind_rows(zip %>%
              as_tibble %>%
              select(n, diff.r, diff.d)%>% 
              mutate(basegeo = 'Zip Code'))%>%
  bind_rows(tracts %>%
              as_tibble %>%
              select(n, diff.r, diff.d)%>% 
              mutate(basegeo = 'Tract'))%>%
  mutate(basegeo=factor(basegeo, levels = rev(c('Tract', 'Zip Code', 'City/Town', 'County', 'CBSA', 'State'))))%>%
  filter(n>1)

pt = df %>%
  select(n, diff.r,diff.d,basegeo)%>%
  pivot_longer(diff.r:diff.d)%>%
  mutate(Party = ifelse(name == 'diff.r', 'Democratic', 'Republican'))


### Calculate weighted means and medians to plot vertical lines in the plots
vline.dat = pt %>%
  group_by(basegeo, Party)%>%
  summarize(mean = weighted.mean(value,w=n,na.rm=T),
            median = weighted.median(value,w=n,na.rm=T)
  )


### output Figure 5

plot.name = 'Fig5.pdf'
ggsave(filename=plot.name, width = 180, height = 180, units = 'mm',
plot=ggplot(pt , aes(x=value, weight = n, color = Party, fill = Party))+
  geom_histogram(alpha=.5,  binwidth = .01, position = 'identity')+
  geom_vline(xintercept  = 0, color = "black", linetype = "dotted")+
  theme_jake()+
  guides(color = F, fill = F)+
  ylab('Count')+
  xlab('Relative Exposure')+
scale_y_continuous(labels = comma)+
 xlim(-.75,.1)+
  geom_vline(data = vline.dat, aes(xintercept= mean, color = Party, fill = Party)) +
  geom_vline(data= vline.dat, aes(xintercept = median, color = Party, fill = Party), linetype="dashed") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  facet_grid(rows = vars(basegeo), cols = vars(Party)) +
  theme(strip.text=element_text(size = 10), axis.title=element_text(size=10), panel.margin = unit(2, "lines"))
  
)



### format no same household neighbor data for ggplot

df_nohh = state %>% 
  as_tibble %>%
  select(n, diff.r_nohh, diff.d_nohh)%>% 
  mutate(basegeo = 'State')%>%
  bind_rows(cbsa %>%
              as_tibble %>%
              select(n, diff.r_nohh, diff.d_nohh) %>% 
              mutate(basegeo = 'CBSA'))%>%
  bind_rows(county %>%
              as_tibble %>%
              select(n, diff.r_nohh, diff.d_nohh)%>% 
              mutate(basegeo = 'County'))%>%
  bind_rows(city %>%
              as_tibble %>%
              select(n, diff.r_nohh, diff.d_nohh)%>% 
              mutate(basegeo = 'City/Town'))%>%
  bind_rows(zip %>%
              as_tibble %>%
              select(n, diff.r_nohh, diff.d_nohh)%>% 
              mutate(basegeo = 'Zip'))%>%
  bind_rows(tracts %>%
              as_tibble %>%
              select(n, diff.r_nohh, diff.d_nohh)%>% 
              mutate(basegeo = 'Tract'))%>%
  mutate(basegeo=factor(basegeo, levels = rev(c('Tract', 'Zip', 'City/Town', 'County', 'CBSA', 'State'))))%>%
  filter(n>1)

pt = df_nohh %>%
  select(n, diff.r_nohh,diff.d_nohh,basegeo)%>%
  pivot_longer(diff.r_nohh:diff.d_nohh)%>%
  mutate(Party = ifelse(name == 'diff.r_nohh', 'Democratic', 'Republican'))


### Calculate weighted means and medians to plot vertical lines in the plots

vline.dat = pt %>%
  group_by(basegeo, Party)%>%
  summarize(mean = weighted.mean(value,w=n,na.rm=T),
            median = weighted.median(value,w=n,na.rm=T)
  )


## output figure S27

plot.name = 'FigS27.pdf'
pdf(plot.name, width = 12, height = 12, pointsize = 9)
ggplot(pt , aes(x=value, weight = n, color = Party, fill = Party))+
  geom_histogram(alpha=.5,  binwidth = .01, position = 'identity')+
  # geom_histogram(aes(x = -1*diff.r), alpha = .5, binwidth = .01,color = '#E41A1C', fill = '#E41A1C', position = 'identity')+
  geom_vline(xintercept  = 0, color = "black", linetype = "dotted")+
  theme_jake()+
  guides(color = F, fill = F)+
  ylab('Count')+
  xlab('Relative Exposure')+
  xlim(-.75,.1)+
  geom_vline(data = vline.dat, aes(xintercept= mean, color = Party, fill = Party)) +
  geom_vline(data= vline.dat, aes(xintercept = median, color = Party, fill = Party), linetype="dashed") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  scale_y_continuous(labels=comma)+
  theme(strip.text=element_text(size = 10), axis.title=element_text(size=10), panel.margin = unit(2, "lines"))+
  facet_grid(rows = vars(basegeo), cols = vars(Party)) 
dev.off()



### Tables


## Calculate t-tests

m.tracts.d=lm(d.exp.r-r.exp.r~1,weights=n,data=tracts[n>1])
m.tracts.d = as.data.table(summary(m.tracts.d)$coefficients)[,df:=m.tracts.d$df][,basegeo:='Tract'][,party :='Democratic'][,type:='main']

m.tracts.r = lm(r.exp.d-d.exp.d~1,weights=n,data=tracts[n>1])
m.tracts.r = as.data.table(summary(m.tracts.r)$coefficients)[,df:=m.tracts.r$df][,basegeo:='Tract'][,party :='Republican'][,type:='main']

m.tracts.d_nohh=lm(d.exp.r_nohh-r.exp.r_nohh~1,weights=n,data=tracts[n>1])
m.tracts.d_nohh = as.data.table(summary(m.tracts.d_nohh)$coefficients)[,df:=m.tracts.d_nohh$df][,basegeo:='Tract'][,party :='Democratic'][,type:='nohh']

m.tracts.r_nohh = lm(r.exp.d_nohh-d.exp.d_nohh~1,weights=n,data=tracts[n>1])
m.tracts.r_nohh = as.data.table(summary(m.tracts.r_nohh)$coefficients)[,df:=m.tracts.r_nohh$df][,basegeo:='Tract'][,party :='Republican'][,type:='nohh']

m.zip.d = lm(d.exp.r-r.exp.r~1,weights=n,data=zip[n>1])
m.zip.d = as.data.table(summary(m.zip.d)$coefficients)[,df:=m.zip.d$df][,basegeo:='Zip Code'][,party :='Democratic'][,type:='main']

m.zip.r = lm(r.exp.d-d.exp.d~1,weights=n,data=zip[n>1])
m.zip.r =as.data.table(summary(m.zip.r)$coefficients)[,df:=m.zip.r$df][,basegeo:='Zip Code'][,party :='Republican'][,type:='main']

m.zip.d_nohh = lm(d.exp.r_nohh-r.exp.r_nohh~1,weights=n,data=zip[n>1])
m.zip.d_nohh =as.data.table(summary(m.zip.d_nohh)$coefficients)[,df:=m.zip.d_nohh$df][,basegeo:='Zip Code'][,party :='Democratic'][,type:='nohh']

m.zip.r_nohh=lm(r.exp.d_nohh-d.exp.d_nohh~1,weights=n,data=zip[n>1])
m.zip.r_nohh =as.data.table(summary(m.zip.r_nohh)$coefficients)[,df:=m.zip.r_nohh$df][,basegeo:='Zip Code'][,party :='Republican'][,type:='nohh']


m.city.d = lm(d.exp.r-r.exp.r~1,weights=n,data=city[n>1])
m.city.d = as.data.table(summary(m.city.d)$coefficients)[,df:=m.city.d$df][,basegeo:='City/Town'][,party :='Democratic'][,type:='main']

m.city.r= lm(r.exp.d-d.exp.d~1,weights=n,data=city[n>1])
m.city.r =as.data.table(summary(m.city.r)$coefficients)[,df:=m.city.r$df][,basegeo:='City/Town'][,party :='Republican'][,type:='main']

m.city.d_nohh = lm(d.exp.r_nohh-r.exp.r_nohh~1,weights=n,data=city[n>1])
m.city.d_nohh =as.data.table(summary(m.city.d_nohh)$coefficients)[,df:=m.city.d_nohh$df][,basegeo:='City/Town'][,party :='Democratic'][,type:='nohh']


m.city.r_nohh = lm(r.exp.d_nohh-d.exp.d_nohh~1,weights=n,data=city[n>1])
m.city.r_nohh =as.data.table(summary(m.city.r_nohh)$coefficients)[,df:=m.city.r_nohh$df][,basegeo:='City/Town'][,party :='Republican'][,type:='nohh']

m.county.d = lm(d.exp.r-r.exp.r~1,weights=n,data=county[n > 1])
m.county.d =as.data.table(summary(m.county.d)$coefficients)[,df:=m.county.d$df][,basegeo:='County'][,party :='Democratic'][,type:='main']

m.county.r = lm(r.exp.d-d.exp.d~1,weights=n,data=county[n>1])
m.county.r = as.data.table(summary(m.county.r)$coefficients)[,df:=m.county.r$df][,basegeo:='County'][,party :='Republican'][,type:='main']

m.county.d_nohh=lm(d.exp.r_nohh-r.exp.r_nohh~1,weights=n,data=county[n>1])
m.county.d_nohh = as.data.table(summary(m.county.d_nohh)$coefficients)[,df:=m.county.d_nohh$df][,basegeo:='County'][,party :='Democratic'][,type:='nohh']


m.county.r_nohh = lm(r.exp.d_nohh-d.exp.d_nohh~1,weights=n,data=county[n>1])
m.county.r_nohh =as.data.table(summary(m.county.r_nohh)$coefficients)[,df:=m.county.r_nohh$df][,basegeo:='County'][,party :='Republican'][,type:='nohh']

m.cbsa.d = lm(d.exp.r-r.exp.r~1,weights=n,data=cbsa)
m.cbsa.d =as.data.table(summary(m.cbsa.d)$coefficients)[,df:=m.cbsa.d$df][,basegeo:='CBSA'][,party :='Democratic'][,type:='main']

m.cbsa.r = lm(r.exp.d-d.exp.d~1,weights=n,data=cbsa)
m.cbsa.r =as.data.table(summary(m.cbsa.r)$coefficients)[,df:=m.cbsa.r$df][,basegeo:='CBSA'][,party :='Republican'][,type:='main']

m.cbsa.d_nohh = lm(d.exp.r_nohh-r.exp.r_nohh~1,weights=n,data=cbsa)
m.cbsa.d_nohh =as.data.table(summary(m.cbsa.d_nohh)$coefficients)[,df:=m.cbsa.d_nohh$df][,basegeo:='CBSA'][,party :='Democratic'][,type:='nohh']

m.cbsa.r_nohh = lm(r.exp.d_nohh-d.exp.d_nohh~1,weights=n,data=cbsa)
m.cbsa.r_nohh =as.data.table(summary(m.cbsa.r_nohh)$coefficients)[,df:=m.cbsa.r_nohh$df][,basegeo:='CBSA'][,party :='Republican'][,type:='nohh']

m.state.d = lm(d.exp.r-r.exp.r~1,weights=n,data=state)
m.state.d =as.data.table(summary(m.state.d)$coefficients)[,df:=m.state.d$df][,basegeo:='State'][,party :='Democratic'][,type:='main']

m.state.r=lm(r.exp.d-d.exp.d~1,weights=n,data=state)
m.state.r =as.data.table(summary(m.state.r)$coefficients)[,df:=m.state.r$df][,basegeo:='State'][,party :='Republican'][,type:='main']

m.state.d_nohh = lm(d.exp.r_nohh-r.exp.r_nohh~1,weights=n,data=state)
m.state.d_nohh =as.data.table(summary(m.state.d_nohh)$coefficients)[,df:=m.state.d_nohh$df][,basegeo:='State'][,party :='Democratic'][,type:='nohh']

m.state.r_nohh = lm(r.exp.d_nohh-d.exp.d_nohh~1,weights=n,data=state)
m.state.r_nohh =as.data.table(summary(m.state.r_nohh)$coefficient)[,df:=m.state.r_nohh$df][,basegeo:='State'][,party :='Republican'][,type:='nohh']


# combine t-test output

m = rbindlist(list(m.state.d, m.state.r, m.state.d_nohh, m.state.r_nohh,
                   m.cbsa.d, m.cbsa.r, m.cbsa.d_nohh, m.cbsa.r_nohh,
                   m.county.d, m.county.r, m.county.d_nohh, m.county.r_nohh,
                   m.city.d, m.city.r, m.city.d_nohh, m.city.r_nohh,
                   m.zip.d, m.zip.r, m.zip.d_nohh, m.zip.r_nohh,
                   m.tracts.d, m.tracts.r, m.tracts.d_nohh, m.tracts.r_nohh
                   ))

m[,ci_lower := Estimate - qnorm(.975)*`Std. Error`]
m[,ci_upper := Estimate + qnorm(.975)*`Std. Error`]

m = m[,c('Estimate', 'Std. Error', 't value', 'Pr(>|t|)', 'ci_lower', 'ci_upper', 'df', 'basegeo', 'party', 'type')]



# Output Tables S19 and S20
require(xtable)
t1=xtable(m[type=='main',-c('type')],digits=3)
t2=xtable(m[type=='nohh', -c('type')],digits=3)
print(t1, file = 'TabS19.tex')
print(t2, file = 'TabS20.tex')




### Count tables

x = data.table(
  Geography = c('State', 'CBSA', 'County', 'City/Town', 'Zip', 'Tracts'),
  `0.1%` = c(quantile(state$n, probs = .001), quantile(cbsa$n, probs = .001), quantile(county$n, probs = .001),quantile(city$n, probs = .001), quantile(zip$n, probs = .001), quantile(tracts$n, probs = .001)),
  
  `1%` = c(quantile(state$n, probs = .01), quantile(cbsa$n, probs = .01), quantile(county$n, probs = .01),quantile(city$n, probs = .01), quantile(zip$n, probs = .01), quantile(tracts$n, probs = .01)),
`10%` = c(quantile(state$n, probs = .1), quantile(cbsa$n, probs = .1), quantile(county$n, probs = .1),quantile(city$n, probs = .1), quantile(zip$n, probs = .1), quantile(tracts$n, probs = .1)),
`25%` = c(quantile(state$n, probs = .25), quantile(cbsa$n, probs = .25), quantile(county$n, probs = .25),quantile(city$n, probs = .25), quantile(zip$n, probs = .25), quantile(tracts$n, probs = .25)),
`50%` = c(quantile(state$n, probs = .5), quantile(cbsa$n, probs = .5), quantile(county$n, probs = .5),quantile(city$n, probs = .5), quantile(zip$n, probs = .5), quantile(tracts$n, probs = .5)),
`75%` = c(quantile(state$n, probs = .75), quantile(cbsa$n, probs = .75), quantile(county$n, probs = .75),quantile(city$n, probs = .75), quantile(zip$n, probs = .75), quantile(tracts$n, probs = .75)),
`90%` = c(quantile(state$n, probs = .9), quantile(cbsa$n, probs = .9), quantile(county$n, probs = .9),quantile(city$n, probs = .9), quantile(zip$n, probs = .9), quantile(tracts$n, probs = .9)),
`99%` = c(quantile(state$n, probs = .99), quantile(cbsa$n, probs = .99), quantile(county$n, probs = .99),quantile(city$n, probs = .99), quantile(zip$n, probs = .99), quantile(tracts$n, probs = .99)),
`99.9%` = c(quantile(state$n, probs = .999), quantile(cbsa$n, probs = .999), quantile(county$n, probs = .999),quantile(city$n, probs = .999), quantile(zip$n, probs = .999), quantile(tracts$n, probs = .999)),

Mean = c(mean(state$n), mean(cbsa$n), mean(county$n),mean(city$n), mean(zip$n), mean(tracts$n))
)

library(scales)
x=as.data.frame(x)
for(i in 2:ncol(x)){
    x[,i]=round( x[,i],0)
  
}

for(i in 2:ncol(x)){
    x[,i]=comma( x[,i])
}

t3=xtable(x)


print(t3,file='Tab2.tex')
