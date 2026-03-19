## Hager / Hilbig - Replication code - Jan 7, 2020
## hhilbig@g.harvard.edu
##
## Figures reproduced in this file
## A9, A12
### ### ### ###

## Packages 

library(lubridate)
library(reshape2)
library(tidyverse)
library(gridExtra)
library(ggmap)
library(cowplot)

## Clear

rm(list = ls())


## Get data

term_df <- read_rds('Term_Contribution.RDS')

## Define plotting function

make_plot <- function(df) {
  
  df <- df[order(df[, 'diff_rd'], decreasing = T), ]  # order
  df$terms <- factor(df$terms, levels = df$terms[10:1]) # reorder
  
  ## Plot
  
  p1 <- ggplot(data = df, aes(y = terms, x = diff_rd,
                              group = sign_rd)) +
    geom_point(aes(shape = sign_rd), size = 4) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    ylab("") + xlab("") +
    scale_shape_manual(labels = c("Increased use", "Decreased use"),
                       values = c("+", "-"), name = "") +
    ggtitle(df$topic[1]) +
    theme(axis.text.y=
            element_text(face=ifelse(df[, "sign_rd"] == "-1", 
                                     "bold","plain")[10:1]),
          legend.position = "none",
          text = element_text(size = 17))  +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
  p1
}

#### Figure A9 ####

gA <- ggplotGrob(make_plot(term_df %>% filter(topic == "econ_policy")))
gB <- ggplotGrob(make_plot(term_df %>% filter(topic == "soc_policy")))
gC <- ggplotGrob(make_plot(term_df %>% filter(topic == "foreign_policy")))
gD <- ggplotGrob(make_plot(term_df %>% filter(topic == "interior")))

plot_grid(gA, gB, gC, gD, labels = NULL, ncol = 2, align = 'v')

#### Figure A12 ####

df <- data.frame()
blank_p <- ggplot(df) + theme_nothing() + 
  geom_point(shape = 19, color = "white", size = 3)

## Make plot

gA <- ggplotGrob(make_plot(term_df %>% filter(topic == "environmental")))
gB <- ggplotGrob(make_plot(term_df %>% filter(topic == "education")))
gC <- ggplotGrob(make_plot(term_df %>% filter(topic == "culture")))
gD <- ggplotGrob(blank_p)

## Plot

plot_grid(gA, gB, gC, gD, labels = NULL, ncol = 2, align = 'v')


