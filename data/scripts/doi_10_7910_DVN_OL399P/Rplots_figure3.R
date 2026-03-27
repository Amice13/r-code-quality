# LOAD PACKAGES
library(readxl)
library(tidyverse)
library(randomcoloR)
library(ggpubr)
library(psych)
library(irr)
library(ggbeeswarm)
library(Hmisc)


# # import data from xls

# Generate plots
read.csv("DATA_HRVA.csv") %>% 
  mutate(HRV_CLASS = factor(HRV_CLASS, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), ordered = T),
         HRV_g2 = factor(HRV_g2, levels = c("A101", "A49", "A78", "A12", "HRV-A Other"), ordered = T),
         year = as.factor(year)) %>% 
  ggplot()+
  geom_bar(aes(HRV_CLASS, Prop, fill = HRV_g2), stat = "Identity", position = position_stack())+
  scale_fill_manual(name = "HRV A types", values = c("black", "green", "blue", "red", "grey"))+
  facet_grid(~year)+
  labs(y = "% Proportion", x = " ")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  theme(axis.title.x = element_text(size = 16, face = "bold", vjust = -.5),
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold", angle = 90),
        axis.title.y = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(linetype = "blank", fill = NA),
        legend.title = element_text(size = 0, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom") -> HRVA_plot
HRVA_plot

read.csv("DATA_HRVB.csv") %>% 
  mutate(HRV_CLASS = factor(HRV_CLASS, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), ordered = T),
         HRV_g2 = factor(HRV_g2, levels = c("B4", "B42", "B48", "B70", "HRV-B Other"), ordered = T),
         year = as.factor(year)) %>%
  ggplot()+
  geom_bar(aes(HRV_CLASS, Prop, fill = HRV_g2), stat = "Identity", position = position_stack())+
  scale_fill_manual(name = "HRV B types", values = c("black", "green", "blue", "red", "grey"))+
  facet_grid(~year)+
  labs(y = "% Proportion", x = " ")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  theme(axis.title.x = element_text(size = 16, face = "bold", vjust = -.5),
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold", angle = 90),
        axis.title.y = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(linetype = "blank", fill = NA),
        legend.title = element_text(size = 0, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom") -> HRVB_plot
HRVB_plot

read.csv("DATA_HRVC.csv") %>% 
  mutate(HRV_CLASS = factor(HRV_CLASS, levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"), ordered = T),
         HRV_g2 = factor(HRV_g2, levels = c("C2", "C6", "C11", "C38", "HRV-C Other"), ordered = T),
         year = as.factor(year)) %>%
  ggplot()+
  geom_bar(aes(HRV_CLASS, Prop, fill = HRV_g2), stat = "Identity", position = position_stack())+
  scale_fill_manual(name = "HRV C types", values = c("black", "green", "blue", "red", "grey"))+
  facet_grid(~year)+
  labs(y = "% Proportion", x = " ")+
  theme_classic()+
  theme(panel.grid = element_blank())+
  theme(axis.title.x = element_text(size = 16, face = "bold", vjust = -.5),
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold", angle = 90),
        axis.title.y = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        strip.background = element_rect(linetype = "blank", fill = NA),
        legend.title = element_text(size = 0, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom")-> HRVC_plot
HRVC_plot






