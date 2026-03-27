
#############
# Census Data Comparison
rm(list=ls())
gc()



##

library(sp)
library(maps)
library(maptools)
library(dplyr)
library(stargazer)
library(tigris)
library(tidyverse)

data = read_csv('02-data/tract-vars-link-eligible-voters.csv')

## bring in linked data
load("02-data/befm-main-analysis.Rdata")
df0509 = df %>%
  as_tibble%>%
  mutate( sample = 'Linked', vfile.year='2005/2009') %>%
  select( sample, vfile.year, white.prop.tract,hh.income.tract,hs.prop.tract)


load("02-data/seg_analysis_2017.Rdata")

df17 = df %>%
  as_tibble%>%
  mutate( sample = 'Linked', vfile.year='2017') %>%
  select( sample, vfile.year, white.prop.tract,hh.income.tract,hs.prop.tract)

rm(df)


data = df17 %>%
  bind_rows(df0509)%>%
  bind_rows(data)


rm(df17,df0509,cens0509,cens17)
gc()

##census plots

source("01-code/r_utils.R")
require(scales)




p = data %>%
  mutate(sample = ifelse(sample=='General', 'Link Eligible', sample))%>%
  ggplot(aes(x=white.prop.tract*100))+
  geom_histogram(aes(y=..ncount..))+
  xlab('% White')+
  ylab('Density')+
  theme_shom()+
  facet_wrap(vfile.year~sample)

ggsave(filename = "03-output/01-plots/FigS3.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)

p = data %>%
  mutate(sample = ifelse(sample=='General', 'Link Eligible', sample))%>%
  ggplot(aes(x=hs.prop.tract*100))+
  geom_histogram(aes(y=..ncount..))+
  xlab('% High School Educated')+
  ylab('Density')+
  theme_shom()+
  facet_wrap(vfile.year~sample)

ggsave(filename = "03-output/01-plots/FigS4.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)


p = data %>%
  mutate(sample = ifelse(sample=='General', 'Link Eligible', sample))%>%
  ggplot(aes(x=hh.income.tract))+
  geom_histogram(aes(y=..ncount..))+
  xlab('Median Household Income')+
  ylab('Density')+
  theme_shom()+
  facet_wrap(vfile.year~sample)
ggsave(filename = "03-output/01-plots/FigS5.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)
