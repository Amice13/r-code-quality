##########################################################################
# Figure A1: Variation in Mayoral Candidacies and Victories (2008-2016)
##########################################################################

rm(list=ls())

library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(here)

load(here('data','time_trends.rdata'))

data.all <- tempo[year>=2008 & baseline > 200 & baseline_win > 0 & !party %in% c('PRB')]
data.prb <- tempo[year>=2008 & baseline > 200 & baseline_win > 0 & party %in% c('PRB')]

ggplot(data.all, aes(x=as.factor(year), y= as.numeric(ratio), group = party, fill='grey')) + 
  geom_line(size=.5,color='grey') +
  geom_line(size=.75,color='red',data=data.prb) +
  theme_minimal() +
  xlab('Year') +
  ylab('Growth in number of mayoral CANDIDATES\n(2008=100)')

ggplot(data.all, aes(x=as.factor(year), y = as.numeric(ratio_win), group = party,fill='grey')) + 
  geom_line(size=.5,color='grey') +
  geom_line(size=.75,color='red',data=data.prb) +
  theme_minimal() +
  xlab('Year') +
  ylab('Growth in number of mayoral VICTORIES\n(2008=100)')

