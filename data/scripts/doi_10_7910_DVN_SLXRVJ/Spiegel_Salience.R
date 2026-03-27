## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A2
### ### ### ###

rm(list = ls())

library(tidyverse)


## Get data

spiegel_df <- read_rds('Spiegel_Salience.RDS')

#### Figure A2 ####

p1_w2 <- ggplot(data = spiegel_df, aes(x = date, y = salience, group = 1)) + 
  geom_line() + theme_bw() + facet_wrap(~ topic) + xlab("") +
  ylab("Relative salience") + stat_smooth(se = T, span = 0.7, color = "grey60",
                                          fill = "grey87", size = 0.75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1_w2
