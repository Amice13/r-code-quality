#plotting the distribuion of the quantitative predictors: offspring age, FAI (prevailing food availability index), 
#and daily average association size (excluding the focal individual)
#written by T Revathe

rm(list=ls())

xdata=read.table(file="FAI_assoc_size_dist.txt", header=T,
                 stringsAsFactors=T, sep="\t")
str(xdata)
names(xdata)

#violin plot
library(ggplot2)  

p <- ggplot(xdata, aes(x = "", y = FAI_fruit)) +
  geom_violin(scale = "width", trim = FALSE, fill="grey", alpha = 0.7, col="black") +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.9, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, size = 1, color = "black", alpha = 0.2) +
  xlab("") +
  ylab("Fruit Availability Index") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  
p + stat_summary(fun=mean, geom="point", shape=23, size=3, fill="blue")


q <- ggplot(xdata, aes(x = "", y = assoc_size)) +
  geom_violin(scale = "width", trim = FALSE, fill="grey", alpha = 0.7, col="black") +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.9, outlier.shape = NA) +
  geom_jitter(width = 0.1, height = 0, size = 0.5, color = "black", alpha = 0.4) +
  xlab("") +
  ylab("Daily average association size") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  
q + stat_summary(fun=mean, geom="point", shape=23, size=3, fill="blue") + coord_cartesian(ylim = c(0, 12)) 

#---------------------------------------------
xdata=read.table(file="age_dist.txt", header=T,
                 stringsAsFactors=T, sep="\t")
str(xdata)
names(xdata)

n <- ggplot(xdata, aes(x = "", y = offspringAge)) +
  geom_violin(scale = "width", trim = FALSE, fill="grey", alpha = 0.7, col="black") +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.9, outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, size = 1.5, color = "black", alpha = 0.2) +
  xlab("") +
  ylab("Offspring age (in years)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  
n + stat_summary(fun=mean, geom="point", shape=23, size=3, fill="blue") + coord_cartesian(ylim = c(0, 10)) 

