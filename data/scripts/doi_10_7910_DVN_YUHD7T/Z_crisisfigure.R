# Script to create figure of yearly crisis incidence 1920-39

library(tidyverse) # CRAN v1.3.1
library(ggpubr)    # CRAN v0.4.0


crisis = read.csv("data/crisis_figuredata.csv")

crisis = crisis %>%
  mutate(number = 1)
crisis_collapsed = crisis %>%
  group_by(year) %>%
  summarise(count = sum(number), intensity = sum(geostr))

f = function(x) 1 + x^2
x = seq(-9, 9)
crisis_collapsed$uline = f(x) / 5


ggplot(crisis_collapsed, aes(x = year)) +
  geom_line(aes(y = count), linetype = "dotted") +
  geom_point(aes(y = count)) +
  geom_line(aes(y = intensity), linetype = "dashed") +
  geom_point(aes(y = intensity)) +
  geom_line(aes(y = uline), linetype = "solid", size = 1.1) +
  scale_x_continuous(breaks = round(seq(min(crisis_collapsed$year), max(crisis_collapsed$year), by = 3), 1)) +
  xlab("Year") + ylab("Number") + theme_pubr() + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave("salw/figures/crisis_fig.pdf", width = 10, height = 5)
