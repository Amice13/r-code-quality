rm(list = ls())
#gc()

library(data.table)
library(ggplot2)
library(scales)

# This script outputs Figure S26, the distribution of same household neighbors for all voters

dat = fread('us-voters-variables.csv', ,select = c('source_id', 'same_household'))
source('theme_jake.R')
plot.name = 'FigS26.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(dat,aes(x=same_household))+
  geom_histogram(binwidth=1,position="identity") +
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1))+
  ylab('Count')+
  scale_y_continuous(labels=comma)+
  xlab('Same-household Neighbors')+
  theme_jake()

dev.off()
gc()

