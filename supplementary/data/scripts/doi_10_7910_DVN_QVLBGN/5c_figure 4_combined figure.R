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
## LOAD DATA--------------------------------------------------------------------
## 

load("figure_effect differences by age group2.RData")
load("figure_effect differences by education group2.RData")


##
## PREPARING FIGURE ------------------------------------------------------------
## 



g1 <- 
  m.coef.agesex %>% 
  ggplot(aes(x=coef, y=fct_reorder(age, -order), color=sex, group=sex))+ 
  geom_vline(aes(xintercept=0), color="Red")+
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_up), # , colour = climatic, shape = climatic, fill = climatic
    position = position_dodge(0.65), 
    size = 0.6,
    shape=18#, key_glyph = draw_key_point
  ) +
  geom_path(position = position_dodge(0.65), alpha=0.7,  linetype="dashed")+
  facet_grid(developed~fct_reorder(aridity,order2), space="free_x")+
  scale_color_manual(name="", values = c("#328da8", "#1aa156"))+
  scale_x_continuous(labels=scales::percent)+
  xlab("Migration impacts")+ylab("")+
  #ggtitle("(a) Effect differences by world regions")+
  theme_bw() +
  ggtitle("")+
  theme(
    strip.text = element_text(face = "bold", size=11),
    strip.background = element_rect(fill = "#daebdd"),
    legend.position = "bottom",
    legend.text = element_text(size=12),
    axis.text=element_text(size=11),
    axis.title=element_text(size=12),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill="#daebdd",colour = "Grey"))+
  coord_cartesian(xlim=c(-0.1,0.25))

g1
ggsave(plot = g1, filename="figure_effect differences by age group.png", width=12, height = 5)

g2 <- 
  m.coef.edusex %>% 
  ggplot(aes(x=coef, y=fct_reorder(edu, -order), color=sex, group=sex))+ 
  geom_vline(aes(xintercept=0), color="Red")+
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_up), # , colour = climatic, shape = climatic, fill = climatic
    position = position_dodge(0.7), 
    size = 0.65,
    shape=18#, key_glyph = draw_key_point
  ) +
  geom_path(position = position_dodge(0.65), alpha=0.7,  linetype="dashed")+
  facet_grid(developed~fct_reorder(aridity,order2), space="free_x")+
  scale_color_manual(name="", values = c("#328da8", "#1aa156"))+
  scale_x_continuous(labels=scales::percent)+
  xlab("Migration impacts")+ylab("")+
  #ggtitle("(a) Effect differences by world regions")+
  theme_bw() +
  ggtitle("")+
  theme(
    strip.text = element_text(face = "bold", size=11),
    strip.background = element_rect(fill = "#daebdd"),
    legend.position = "bottom",
    legend.text = element_text(size=12),
    axis.text=element_text(size=11),
    axis.title=element_text(size=12),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill="#daebdd",colour = "Grey"))+
  coord_cartesian(xlim=c(-0.1,0.25))
g2

ggsave(plot = g2, filename="figure_dot whisker coef plot_effect differences by education group2.png",
       width=12, height = 4.5)

#> combining panels in one figure
g12 <- ggarrange(g1, g2, 
                 labels=c("A", "B"),
                 nrow = 2,
                 align = "hv",
                 heights = c(1.2,1.0),
                 font.label=list(size=18))

g12

#> save final plots
ggsave(filename="figure 4.png",
       width=10, height = 9.5)


#> save final plots
ggsave(filename="figure 4.pdf",
       width=10, height = 9.5)
