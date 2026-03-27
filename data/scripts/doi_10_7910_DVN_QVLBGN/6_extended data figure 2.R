### ****************************************************************************
### CLIMATE CHANGE, ARIDITY, MIGRATION 
### ****************************************************************************

###
### CENSUS COVERAGE OVER TIME  -------------------------------------------------
### 

##
## PACKAGES --------------------------------------------------------------------
## 

library(tidyverse)
library(ggpubr)
library(RColorBrewer)

##
## DATA ------------------------------------------------------------------------
## 

rm(list = ls())

load(file="full migration data.RData")

##
## DATA ------------------------------------------------------------------------
## 

library("ggsci")

mycolors <- pal_lancet()(8) 
mycolors[8] <- "#4d4f4d"


display.brewer.pal(n = 8, name = 'Paired')

table(d$worldregion)

d <- d %>% arrange(worldregion)

g1 <- d %>% 
  group_by(cntry_name, year, worldregion)  %>%
  group_by(cntry_name) %>% 
  mutate(year_min = min(year), year_max = max(year)) %>% 
  ungroup() %>% 
  mutate(order=recode(worldregion, 
                      "North America"= 6L,
                      "Central America & Caribbean"=5L,
                      "South America"=7L,
                      "Africa & Middle East"=1L,
                      "South Asia"=4L,
                      "East Asia & Pacific"=2L,
                      "Northeastern Europe & Central Asia" = 3L,
                      "Southern Europe" = 8L
                      
  )) %>% 
  ggplot()+
  geom_point(aes(y=fct_reorder(cntry_name, -order),x=year, 
                 color=fct_reorder(worldregion, order)))+
  geom_segment(aes(x=year_min, xend=year_max, 
                   y=cntry_name, 
                   yend=cntry_name, 
                   color=worldregion))+
  scale_x_continuous(labels=seq(1960,2020,5), breaks=seq(1960,2020,5))+
  ylab("")+xlab("Year")+
  theme_bw()+
  labs(color="worldregion")+
  theme(
    strip.text = element_text(face = "bold", size=11),
    strip.background = element_rect(fill = "#daebdd"),
    legend.position = "right",
    legend.text = element_text(size=12),
    legend.title = element_text(size=14),
    
    axis.text=element_text(size=11),
    axis.title=element_text(size=12),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill="#daebdd",colour = "Grey"))+
  scale_color_manual(values=mycolors, name="Worldregion") 
          


ggsave(plot=g1, filename="Hoffmann et al_ED_Figure 2.jpg", 
       width = 12, height=10)
 
