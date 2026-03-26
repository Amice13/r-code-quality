# load required packages
library(tidyverse)
library(ggplot2)
library(gplots)
library(dplyr)
library(ggpubr)

#Reading the ct data into R
fractions <- read.csv("Fractions_cts.csv", sep = ',', header = TRUE)
colnames(fractions) <- c("sample","Fraction", "Ct_score")

# making a boxplot of the different sample fractions against ct score
P <- ggboxplot(fractions, "Fraction","Ct_score",
               color = "Fraction", palette =c("#FBAE17", "#F52100", "#0071BC","#CE50CA"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))
P + aes(x = fct_inorder(Fraction)) + theme(legend.position = "none") + xlab("Fraction") + ylab("Ct score")+labs(tag = "B", plot.tag.position = c(0.2, -0.1))


#Reading the Runs results into R
Runs <- read.csv("Runs_cts.csv", sep = ',', header = TRUE)
Runs <- Runs %>%
  gather(value = "ct_score", key = "Runs", -Sample)

#making a boxplot of different runs against the corresponding ct scores
P <- ggboxplot(Runs, "Runs","ct_score",
               color = "Runs", palette =c("#5F7FC7", "#C0717C", "#CBD588"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))
P + aes(x = fct_inorder(Runs)) + theme(legend.position = "none") + xlab("Runs") + ylab("Ct score")

#Reading the qubit results into R
qubit <- read.csv("Qubit_reads.csv", sep = ',', header = TRUE)
colnames(qubit) <- c("sample", "fraction", "qubit_scores")

#making a boxplot of the different fractions against qubit scores
P <- ggboxplot(qubit, "fraction","qubit_scores",
               color = "fraction", palette =c("#FBAE17", "#F52100", "#0071BC"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))
P + aes(x = fct_inorder(fraction)) + theme(legend.position = "none") + xlab("Fraction") + ylab("qubit score")+labs(tag = "A", plot.tag.position = c(0.2, -0.1))







