library(ggplot2)
library(reshape2)
library(ggthemes)
library(wesanderson)
library(grid)
library(gapminder)
library(gridExtra)

# clear workspace
rm(list = ls())

setwd("E:/Dropbox/Pass-It-Along/Analysis")



extractions <- read.csv("figure-data_extraction.csv")

contributions <- read.csv("figure-data_contribution.csv")


# order the factors in our own customized way: ggplot default is alphabetical
  # need to create a an integer vector for the purposes of ordering (ggplot will order largest to smallest)
extractions$order <- c(9:1)
contributions$order <- c(6:1)

# re-order the factors for plotting, according to the values in "order":
extractions$treatment <- factor(extractions$treatment, levels = extractions$treatment[order(extractions$order)])
contributions$treatment <- factor(contributions$treatment, levels = contributions$treatment[order(contributions$order)])


# EXTRACTION: FIGURE A

figure.a <- ggplot(extractions, aes(fill=self, color=self, shape=study)) +
  geom_pointrange(aes(x = treatment, y = prop.mean, ymin = prop.mean - prop.se, ymax = prop.mean + prop.se), lwd = 1, position = position_dodge(width = 1/2)) +
  xlab("") + ylab("Mean Proportional Extractions: Studies 1 and 2") +
  scale_fill_manual(
    name = "Self",
    labels = c("Other", "Self"),
    values = c("red", "blue"), 
    guide = guide_legend(title = "Create threshold for:")) +
  scale_color_manual(
    name = "Self",
    labels = c("Other", "Self"),
    values = c("red", "blue"), 
    guide = guide_legend(title = "Create threshold for:")) +
  scale_shape_manual(
    #name = "Self",
    #labels = c("Study 1", "Study 2"),
    values = c(1, 22),
    guide=FALSE) +
  scale_y_continuous(minor_breaks = c(0.55,0.65,0.75,0.85)) +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank()) +
  coord_flip() + 
  #geom_vline(xintercept = 7.5, linetype = "longdash") +
  #geom_vline(xintercept = 2.5, linetype = "longdash") +
  ggtitle("Study 2 (MTurk)                                  Study 1 (Lab)") +
  theme(legend.position=c(0.8,0.2)) +
  theme(plot.title = element_text(size=22)) +
  theme(legend.title = element_text(size=20)) +
  theme(legend.text = element_text(size=18)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16), axis.title.y = element_text(size=18), axis.title.x = element_text(size=18)) +
  theme(axis.ticks = element_blank()) # removes only the tick *marks* and not the labels of the axes so that it's not confused with the minus sign in the differences displayed on the labels.


figure.a

dev.copy(jpeg, width = 800, height = 400,
         filename="/Users/Reuben/Dropbox/Pass-It-Along/Analysis/figureA.png");
dev.off();

# Code for generating studio quality image. Must be resized in external program.
ggsave(filename="E:/Dropbox/Pass-It-Along/Analysis/figureA_large.tiff",
       height=3.125,width=6.25,scale=1.5,dpi=300)

#theme(panel.grid.minor = element_line(colour="black", size=10)) +

#scale_x_discrete(labels=c()) +

#theme_tufte(base_size = 32) +


# CONTRIBUTION: FIGURE B

# add lighter gray backround gray90

figure.b <- ggplot(contributions, aes(fill=multiplier, color=multiplier, shape=multiplier)) +
  geom_pointrange(aes(x = treatment, y = prop.mean, ymin = prop.mean - prop.se, ymax = prop.mean + prop.se), lwd = 1, position = position_dodge(width = 1/2)) +
  xlab("") + ylab("Mean Proportional Contributions: Study 2") +
  scale_fill_manual(
    name = "Multiplier",
    labels = c("1:1", "1:2", "2:1"),
    values = c("black", "grey52", "gray42"), 
    guide = guide_legend(title = "Multiplier")) +
  scale_color_manual(
    name = "Multiplier",
    labels = c("1:1", "1:2", "2:1"),
    values = c("black", "grey52", "gray42"), 
    guide = guide_legend(title = "Multiplier")) +
  scale_shape_manual(
    labels = c("1:1", "1:2", "2:1"),
    values = c(22, 1, 23),
    guide = guide_legend(title = "Multiplier")) +
  scale_y_continuous(breaks = c(0.24,0.26,0.28,0.30,0.32)) +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank()) +
  coord_flip() + 
  theme(legend.position=c(0.85,0.2)) +
  theme(panel.background = element_rect(fill= "gray93", color="gray93")) +
  theme(plot.title = element_text(size=22)) +
  theme(legend.title = element_text(size=20)) +
  theme(legend.text = element_text(size=18)) +
  theme(axis.text.y = element_text(size=16, color="black"), axis.text.x = element_text(size=16), axis.title.y = element_text(size=18), axis.title.x = element_text(size=18)) +
  theme(axis.ticks = element_blank()) # removes only the tick *marks* and not the labels of the axes so that it's not confused with the minus sign in the differences displayed on the labels.


figure.b

dev.copy(jpeg, width = 800, height = 400,
         filename="/Users/Reuben/Dropbox/Pass-It-Along/Analysis/figureB.png");
dev.off();


# Code for generating studio quality image. Must be resized in external program.
ggsave(filename="E:/Dropbox/Pass-It-Along/Analysis/figureB_large.tiff",
       height=3.125,width=6.25,scale=1.5,dpi=300)


