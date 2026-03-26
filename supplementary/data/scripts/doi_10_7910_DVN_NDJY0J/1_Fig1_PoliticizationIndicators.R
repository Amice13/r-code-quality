###################################################################
# Project:  Replication of Rauh/Bes/Schoonvelde 2019 EJPR
#
# Task:     Plot public and partisan Euroscepticism during period
#           Figure 1 in the main text
#
# Author:   Christian Rauh / Martijn Schoonvelde
# Date:     22.02.2019
###################################################################

# R version 3.4.4 (2018-03-15)

# Packages ####
library(tidyverse) # 1.2.1
library(cowplot) # 0.8.0

# Set YOUR working directory here ####
# Root folder of replication archive, relative paths used from hereon
setwd("Your/Working/Directory") 


# Eurobarometer EU image over time ####
# Data from http://ec.europa.eu/commfrontoffice/publicopinion/index.cfm/Chart/index (02.08.2017)

eb.image <- read.table(file = './data/Eurobarometer_Image_Item.csv', sep = ';', header = TRUE, stringsAsFactors = FALSE)

#clean data
names(eb.image) <- c("country", "date", "very.positive", "fairly.positive", "neutral", "fairly negative", "very negative", "dk")

eb.image <- eb.image[eb.image$country != "CY (tcc)", ]

clean <- function(x) {
  temp <- x
  temp <- gsub("%", "", temp)
  temp <- gsub(",", ".", temp)
  temp <- as.numeric(temp)
  return(temp)
}

eb.image <- data.frame(eb.image[1:2], apply(eb.image[3:8], 2, clean))
eb.image$net <- (eb.image$very.positive + eb.image$fairly.positive) - (eb.image$very.negative + eb.image$fairly.negative)
eb.image$hhi <- (eb.image$very.positive^2 + eb.image$fairly.positive^2 + eb.image$very.negative^2 + eb.image$fairly.negative^2)/10000 # https://en.wikipedia.org/wiki/Herfindahl_index
eb.image$date <- as.Date(eb.image$date, format = "%d-%m-%y")

eb.image$year <- str_extract(as.character(eb.image$date), "[0-9]{4}")
eb.image$month <- as.numeric(str_replace_all(str_extract(as.character(eb.image$date), "-[0-9]{2}-"), "-", ""))
eb.image$halfyear <- ifelse(eb.image$month > 6, "h2", "h1")
eb.image$halfyear <- paste(eb.image$year, eb.image$halfyear, sep = "-")


# Establish plot
pol1 <- ggplot(data = eb.image[eb.image$halfyear >= "2007-h1" & eb.image$halfyear <= "2015-h2", ], aes(x=halfyear, y=net))+
  stat_smooth(aes(group = 1), colour = "black")+
  ggtitle("Public EU support")+ xlab("")+ylab("\nNet shares of positive EU image\nin public opinion (Eurobarometer, EU-28 average)\n")+
  theme(plot.title = element_text(size=12, face="bold"),
        panel.grid.major.y=element_line(linetype=2,color="grey"),
        panel.grid.major.x=element_line(linetype=2,color="grey"),
        panel.grid.minor=element_line(color="white"),
        panel.background=element_rect(fill="white"),
        axis.title.y = element_text(size = 12),
        axis.text.x=element_text(angle=90, size=12, vjust = 0.5, colour = "black"),
        axis.text.y=element_text(size=11, colour = "black"))


# Eurosceptic challenger parties ####
# Vote shares of üparties classfied as Eurosceptic challenegers (see researchg design section in main text)
# Country panel that carries their electoral share(s) from the last election forward until the next election (as provided by http://www.parlgov.org/)

load(file = "./data/EurosceptChallengersPanel.Rdata")

challenger.panel$halfyear <- as.character(challenger.panel$halfyear)

# Establish plot
pol2 <- ggplot(data = challenger.panel[challenger.panel$halfyear <= "2015-h2", ], aes(x=halfyear, y=challenger.share))+
  stat_smooth(aes(group = 1), colour = "black")+
  ggtitle("Partisan Euroscepticism")+ xlab("")+ylab("\nVote shares of Eurosceptic parties\nin the latest national election (EU-28 average)\n")+
  theme(plot.title = element_text(size=12, face="bold"),
        panel.grid.major.y=element_line(linetype=2,color="grey"),
        panel.grid.major.x=element_line(linetype=2,color="grey"),
        panel.grid.minor=element_line(color="white"),
        panel.background=element_rect(fill="white"),
        axis.title.y = element_text(size = 12),
        axis.text.x=element_text(angle=90, size=12, vjust = 0.5, colour = "black"),
        axis.text.y=element_text(size=11, colour = "black"))


# Combined plot ####

comb.pl <- plot_grid(pol1, pol2, ncol =2)
save_plot("./figures/Fig1_Politicization.jpg", comb.pl, base_width = 12, base_height = 5)




# Compare EWurobarometer membership and image items in period of overlap ####
# FN 7 in main text

corpus <- readRDS("./data/RauhBesSchoonvelde_ReplData.Rds") 

ggplot(data = corpus, aes(x = eb.image, y = eb.membership)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm",na.rm = TRUE)+
  # geom_smooth(method = "loess")+
  ylab("EB: Net support EU membership")+xlab("EB: Net EU image")+
  theme_bw()
