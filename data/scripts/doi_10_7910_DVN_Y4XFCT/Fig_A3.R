##########################################################################
# Figure A3: Candidates’ Use of Evangelical Religious Appeals by Party
##########################################################################

rm(list = ls())

library(here)
library(ggplot2)
options(scipen = 99)

load(here('data','religious_candidates.rdata'))


ggplot(religious_candidates[religious_candidates$year==2012,], 
                         aes(x=reorder(party, -urna_prop), 
                             y=urna_prop, 
                             fill=as.factor(party_rep))) +
  geom_col() +
  scale_fill_manual(values = c("lightgrey", "#E41A1C")) + 
  theme_bw() + theme(legend.position = "none", 
                     axis.text.x=element_text(angle=90,hjust=1,vjust=.5)) +
  labs(#title="",
    x ="Party", y = "Proportion of 2012 Mayoral Candidates\nwith an Evangelical Urna Name")

