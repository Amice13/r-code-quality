## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A1
### ### ### ###

rm(list = ls())

library(tidyverse)

## ## ## ## 

df <- read_rds('Accuracy_Prediction.RDS')

## Category names

cats_proper <- c("Culture", "Econ. policy", 
                 "Education", "Environmental policy",
                 "Foreign policy", "Interior", "Social policy")
df$cat_proper <- cats_proper

## Melt data

df_melt <- melt(df, id.vars = 'cat_proper', 
                measure.vars = c('test_acc', 'train_acc'))

#### Figure A1 ####

pd <- position_dodge(0.5)

p1 <- ggplot(data = df_melt, aes(x = cat_proper, y = value)) + 
  geom_point(position = pd, aes(group = variable, shape = variable),
             size = 2.8) +
  theme_bw() + xlab('') + ylab('Prediction accuracy') + 
  coord_flip() + scale_shape_manual(values = c(16, 1), 
                                    labels = c('Validation set\n(Out-of-sample)',
                                               'Training set'), 
                                    name = '') + 
  scale_y_continuous(limits = c(0.8, 1)) +
  theme(legend.direction = 'vertical', 
        legend.position = 'right',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(2, 'lines'))
p1
