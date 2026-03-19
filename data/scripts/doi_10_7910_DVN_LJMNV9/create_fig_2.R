# This R-file is generating the Figure 2

rm(list = ls())

library(textreg);library(pder);library(ggrepel);library(broom);library(margins);library(stargazer);library(modelr)
library(ggthemes);library(plm);library(readstata13);library(gridExtra);library(visreg);library(memisc);library(splines)
library(ggeffects);library(modelr);library(pglm);library(lmtest);library(lfe);library(clubSandwich);library(jtools)
library(ggstance);library(interflex);library(ggthemes);library(clusterSEs);library(AER); library(tidyverse); library(gtsummary)
library(tidyr)
library(dplyr)

setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

getwd()

sanction <- read.dta13("Data/sanctions_survey_final.dta")
length(sanction)

library(plyr)
sanction <- sanction %>% dplyr::filter(!is.na(Q16_5))
table(sanction$Q16_5, sanction$ideology)

myplot <-  ggplot(sanction, aes(Q16_5, group = ideology)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  scale_y_continuous(labels=scales::percent) +
  ylab("") + xlab("") + theme_light() +
  theme(legend.position="none", axis.text.x = element_text(face = "bold", color = "blue", angle = 45,hjust = 1)) +
  facet_grid(~ideology)

ggsave(filename = "Figures/Fig_2.png",
       plot = myplot,
       height = 4, width = 7, units = "in")
