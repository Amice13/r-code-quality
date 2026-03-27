##########################################################################
# Figure A4: Governance and Size
###########################################################################

library(here)
library(ggrepel)
library(ggplot2)

load(here('data','denom_centralization.rda'))

centr$main <- as.factor(centr$denom %in% 
                           c("Universal Church of the Kingdom of God", "Assemblies of God"))

ggplot(centr, 
       aes(x = pop_perc, y = HHI, size=nr_total,
           color = main, shape = denom_family, label = denom)) + 
  coord_trans(x = "log10", y = "log10") +
  geom_point() + 
  geom_label_repel(size=2.75) + 
  #scale_color_brewer(palette = "Set1", name="Religious\nTradition") +
  scale_color_manual(values=c("grey50", "red"), guide="none") +
  scale_shape_discrete(name = "Religious\nTradition") + 
  scale_size_continuous(name="Nr. of Branches") +
  theme_minimal() + 
  ylab("Centralization Index (log scale)") + 
  xlab("Adherents (Pop % - log scale)") + 
  theme(legend.position = "right") + 
  annotation_logticks(scaled = FALSE)

