### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

### 
### EXTENDED MODELS: MAPPING PROJECTED IMPACTS OF CHANGES IN AI ---------------- 
###  

rm(list=ls())

##
## PACKAGES --------------------------------------------------------------------
## 

library(tidyverse)
library(ggpubr)

citation("tidyverse")
citation("ggpubr")

##
## LOAD DATA -------------------------------------------------------------------
## 

load(file="figure_predicted migration under 4 degree scenarios.RData")
g2 <- g1 
rm(g1)
load(file="figure_effect differences by worldregions.RData")


##
## PREPARING FIGURE ------------------------------------------------------------
## 

#> combining panels in one figure
g12 <- ggarrange(g1, g2,
                labels=c("A", "B"),
                nrow=2,
                 align = c("v"),
                 widths=c(0.6,1.5),
                 heights=c(1,1.7),
                font.label=list(size=18))

#> save final plots
#> 
ggsave(g12, 
       filename="figure 2.png", 
       width=10, height = 10)

ggsave(g12, 
       filename="figure 2.pdf", 
       width=12, height = 10)
