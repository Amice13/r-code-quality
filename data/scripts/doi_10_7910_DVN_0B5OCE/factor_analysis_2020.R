# Packages #
library(tidyverse)
library(haven)
library(margins)
library(stargazer)
library(ggthemes)
library(broom)
library(magrittr)
library(ggridges)
library(survey)
library(descr)
library(reshape2) #for corr plot
library(anesrake)
library(data.table)


#Read in Factor Analysis Dataset 
factor_mat <- read_csv("Lucid2020_Factor_Analysis_Data.csv")

summary(factor_mat)

# 3-factor analysis #
fit <- factanal(factor_mat, 
                factors = 3, 
                rotation="varimax", 
                scores="regression")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# Create of Scores -- note the one we want is Factor1
fscores <- fit$scores %>%
  as.data.frame() %>%
  mutate(RESENTFACTOR = Factor1,
         f_denial = Factor2,
         f_identity = Factor3) %>%
  dplyr::select(-Factor1, 
                 -Factor2, 
                 -Factor3)


summary(fscores$RESENTFACTOR)

# Look at Resentment Scale #
ggplot(data=fscores, aes(x=RESENTFACTOR))  +
  geom_histogram(aes(y =..density..), binwidth = .5, color = "gray8", fill = "#FF6666") +
  scale_y_continuous("Percent", labels = scales::percent) +#, breaks =  seq(0, .35, .05), limits=c(0,.37)) +
  scale_x_continuous("Immigrant Resentment (Low to High)") +
  theme_light()+
  ggtitle("Factor Histogram") +
  theme(legend.position = "none",
        plot.title = element_text(size=24, hjust = .5, face="bold", family = "serif"),
        plot.subtitle = element_text(size=20, hjust = .5, face="italic", family = "serif"),
        axis.title.x = element_text(size=25, hjust = .5, family = "serif"), #% Immigrant Resentment
        axis.title.y = element_text(size=25, hjust = .5, family = "serif"), #% percent
        #plot.caption = element_text(size=10, hjust = .5, face="bold", family = "serif"),
        axis.text.y = element_text(size = 16, family = "serif"),
        axis.text.x = element_text(size = 16, family = "serif"),
        legend.title =element_text(size=12, face="bold", family = "serif"),
        legend.text =element_text(size=12, face="bold", family = "serif"),
        strip.text.x = element_text(size=15, face="bold",family = "serif")
  )#panel text size 
ggsave("FigureD1.png", 
       width = 10, height = 7)

