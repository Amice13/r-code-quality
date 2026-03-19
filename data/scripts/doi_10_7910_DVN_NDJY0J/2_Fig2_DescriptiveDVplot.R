###################################################################
# Project:  Replication of Rauh/Bes/Schoonvelde 2019 EJPR
#
# Task:     Descriptive plots of the main DVs over actors and time
#           Figure 2 in the main text
#
# Author:   Christian Rauh
# Date:     22.02.2019
###################################################################

# R version 3.4.4 (2018-03-15)

# Packages
library(ggplot2) # 3.0.0
library(reshape2) # 1.4.3

# Set YOUR working directory here ####
# Root folder of replication archive, relative paths used from hereon
setwd("Your/Working/Directory") 


# Load and prepare the data ###
corpus <- readRDS("./data/RauhBesSchoonvelde_ReplData.Rds") 

# Keep variables of interest
corpus <- corpus[ , c("halfyear", "institution", 
                "sent.eusentences",
                "complscore.eusentences")]

# Drop the few polish observations exceeding the investigation period
corpus <- corpus[corpus$halfyear <= "2015-h2", ]

# Invert reading ease score to interpret it as complexity
corpus$complscore.eusentences <- -corpus$complscore.eusentences

# Name vars meaningfully
names(corpus) <- c("Halfyear", "Actor", 
               "Sentiment", 
               "Complexity")

# Keep only observations in range of complexity (drops out speeches whith scraping errors)
corpus <- corpus[corpus$Complexity >= -100 & corpus$Complexity <=0, ]

# Reshape to long format
long <- melt(corpus, id.vars = c("Halfyear", "Actor"))
long <- long[!is.na(long$Halfyear), ]

# Make labels pretty
long$variable <- as.character(long$variable)
long$variable[long$variable == "Sentiment"] <- "Sentiment\nattached to European integration"
long$variable[long$variable == "Complexity"] <- "Complexity\nattached to European integration"
long$variable <- factor(long$variable, levels = c("Sentiment\nattached to European integration", "Complexity\nattached to European integration"))

long$Actor <- as.character(long$Actor)
long$Actor[long$Actor == "Nat. leader"] <- "National leader"
long$Actor[long$Actor == "EU Commission"] <- "EU Commissioner"
long$Actor <- factor(long$Actor, levels = c("National leader", "EU Commissioner"))


# Plot ####

ggplot(data = long, aes(x = Halfyear, y=value, group=Actor))+
  geom_smooth(aes(linetype=Actor), size = .5, method = "loess", colour = "black", span = .9, na.rm = T)+
  xlab("")+ylab("")+
  scale_linetype_manual(name = "", values = c("solid", "dashed"))+
  facet_wrap(~ variable, scales="free")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,vjust=.5, size = 11),
        strip.text = element_text(face = "bold", size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 11, face = "bold"))

ggsave("./figures/Fig2_DVs_OverTimeAndActors.jpg", width = 30, height = 20, unit = "cm")



