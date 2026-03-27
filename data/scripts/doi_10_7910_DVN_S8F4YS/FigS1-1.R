##########################################################################################
rm(list = ls())

### This script creates the age histograms in Figure S1.

#### SETUP ####

#packages
library(tidyverse) #tidyverse functions
library(haven) #read stata files
library(data.table)
library(scales)
source("01-code/r_utils.R")


## read in data
load("02-data/befm-main-analysis.Rdata")
df=as.data.table(df)


## THERE IS A CLUSTER OF LINKED VOTERS WITH VOTERFILE AGE OVER 100 THAT ARE LIKELY MISRECORDED.
## RESTRICT SAMPLE TO VOTERS WITH 1940 AGES SUCH THAT THEY ARE NOT OVER 100 IN CONTEMPORARY VOTERFILES
df0509 = df[(df$state.vfile == 'CA05' & df$age <=35) |(df$state.vfile == 'NC09' & df$age <=31),]


df0509=df0509[,c('age')][,sample:='2005/2009']
rm(df)


## 2017 Sample
load("02-data/seg_analysis_2017.Rdata")
df2017=as.data.table(df)
df2017=df2017[,c('age')][,sample:='2017']
rm(df)

df = rbind(df0509,df2017,fill=T)

# histogram of age
p = df %>%
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 1) + 
  theme_shom() + 
  xlab("Age") + 
  ylab("Count")+
  facet_wrap(sample~.)+
  scale_y_continuous(labels=comma)


ggsave(file = "03-output/01-plots/FigS1.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)




