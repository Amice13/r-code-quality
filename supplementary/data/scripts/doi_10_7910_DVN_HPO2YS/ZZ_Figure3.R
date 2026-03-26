#The code to be used for creating Figure 3 - Violin plot

library(tidyr)
library(ggplot2)
library(dplyr)
ZZ_violin_plot %>% 
  gather(key="defend_silent_shame", value="v2x_polyarchy") %>%
  ggplot( aes(x=defend_silent_shame, y=v2x_polyarchy, fill=defend_silent_shame)) +
  geom_violin()

p <- ggplot(for_violin, aes(factor(defend_silent_shame), v2x_polyarchy))
p + geom_violin()

p + geom_violin(scale = "count")
p + geom_violin(aes(fill = factor(defend_silent_shame)))+ theme_classic()



