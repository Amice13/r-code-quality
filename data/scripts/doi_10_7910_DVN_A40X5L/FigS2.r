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

## Here we output Figure S2, which plots posterior partisanship in 3 plots:
    ## FigS2a - Posterior partisanship for all voters where we impute partisanship using the Bayesian imputation
    ## FigS2b - Posterior partisanship for all voters who are not registered to the Democratic or Republican party in the L2 files, so for whom we code partisanship through their primary voting, third party affiliation, or Bayesian imputation
    ## FigS2c - Posterior partisanship for all voters in the L2 file, registered to a major party or otherwise.

## Load in data
dat = fread('us-voters-exposure-isolation-main.csv', select = c('source_id', 'r.post', 'd.post'))
dat = as.tibble(dat)
gc()

p = fread('us-voters-variables.csv', select= c('source_id', 'party', 'pre_imp_pid'))%>%as_tibble


## Merge in data
dat = left_join(dat,p,by=c('source_id' = 'source_id'))

## Format data for ggplot for FigS2a
l = dat %>%
  select(d.post, r.post) %>%
  gather(Party,post,c('d.post', 'r.post'))


l = l%>%
  mutate(Party = ifelse(Party == 'd.post', 'Democratic', 'Republican'))




## Calculate mean and medians of posterior partisanship
gc()
vline.dat <- 
  l %>%
  group_by(Party) %>%
  summarise(v.mean = mean(post),v.med = median(post)) 

## Load custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")


## Output FigS2a
plot.name = 'FigS2a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=post,fill= Party,color = Party))+
  geom_histogram(binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Party ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  ylab('Count')+
  scale_y_continuous(labels=comma)+
  xlab('Posterior')+
  theme_jake()+
  theme(text=element_text(size = 24))

dev.off()
gc()



### Format data for plot Figs2b
l = dat %>%
  filter(party=='Non-Partisan')%>%
  select(d.post, r.post,d.post,r.post) %>%
  gather(Party,post,c('d.post', 'r.post'))


l = l%>%
  mutate(Party = ifelse(Party == 'd.post', 'Democratic', 'Republican'))




gc()

## Calculate mean and medians of posterior partisanship
vline.dat <- 
  l %>%
  group_by(Party) %>%
  summarise(v.mean = mean(post),v.med = median(post)) 


## Output Figure S2b
plot.name = 'FigS2b.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=post,fill= Party,color = Party))+
  geom_histogram(binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Party ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  ylab('Count')+
  scale_y_continuous(labels=comma)+
  theme_jake()+
  xlab('Posterior')+
  theme(text=element_text(size = 24))

dev.off()
gc()





### Format data for FigS2c
l = dat %>%
  filter(pre_imp_pid=='i')%>%
  select(d.post,r.post) %>%
  gather(Party,post,c('d.post', 'r.post'))


l = l%>%
  mutate(Party = ifelse(Party == 'd.post', 'Democratic', 'Republican'))


## Calculate mean and median of posterior partisanship
gc()
vline.dat <- 
  l %>%
  group_by(Party) %>%
  summarise(v.mean = mean(post),v.med = median(post)) 



## Output Figure S2c
plot.name = 'FigS2c.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=post,fill= Party,color = Party))+
  geom_histogram(binwidth=.01,alpha = .5,position="identity") +
  facet_grid(Party ~.) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.med,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .1))+
  ylab('Count')+
  theme_jake()+
  scale_y_continuous(labels=comma)+
  xlab('Posterior')+
  theme(text=element_text(size = 24))

dev.off()
gc()
