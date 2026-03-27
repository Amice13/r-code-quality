## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A8
### ### ### ###

rm(list = ls())
library(tidyverse)

## Get the data

results_plag <- read.csv('LCS_Matches.csv',
                         stringsAsFactors = F)

#### Figure A8 ####

p1 <- ggplot(data = results_plag, aes(x = results_plag$match_length)) + 
  geom_histogram(color = 'black', fill = 'grey93',
                 aes(y=..density..)) + 
  theme_bw() + geom_density(adjust = 4) +
  ylab('Density') +
  xlab('Length of LCS (characters)')
p1
