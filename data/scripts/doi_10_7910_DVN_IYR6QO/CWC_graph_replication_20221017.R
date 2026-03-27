### CONCEPTUALIZING CIVIL WAR COMPLEXITY, GRAPH REPLICATION ####


#install.packages('plyr')               
library(plyr)
#install.packages("readstata13")
library(readstata13)
#install.packages('dplyr')
library(dplyr)
#install.packages('splitstackshape')
library(splitstackshape)
#install.packages('load.Rdata')
#library(load.Rdata)
#install.packages('zoo')
library(zoo)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('tibble')
library(tibble)
#install.packages('ggrepel')
library(ggrepel)
#install.packages('hrbrthemes')
#library(hrbrthemes)
#install.packages('countrycode')
library(countrycode)
library(data.table)
#install.packages('showtext')
library(showtext)

showtext.auto()
font_paths()
font_add(family = "Garamond", regular = "C:/Windows/Fonts/GARA.TTF")

options(ggrepel.max.overlaps = Inf)


## load dataset labeled "dvs2.csv"

dvs2 <- read.csv(file = 'dvs2.csv')



# FIGURE 2A
px1 <- ggplot(dvs2, aes(x=year, y=all_types, size = all_types, color = abc)) +
  geom_point(alpha=0.4) +
  scale_size_continuous(range = c(.1, 10), name="# actors") +
  geom_text_repel(data=subset(dvs2, all_types > 10 ), aes(label=abc, size=10)) +
  ylab("Number of actors (all types)") +
  xlab("year") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size=12,  family="Garamond"))

px1 


# FIGURE 2B

px3 <-ggplot(dvs2, aes(x=year, y=rebel_type, size = rebel_type, color = abc)) +
  geom_point(alpha=0.4) +
  scale_size_continuous(range = c(.1, 10), name="# actors") +
  geom_text_repel(data=subset(dvs2, rebel_type > 6 ), aes(label=abc, size=10)) +
  ylab("Number of rebel actors") +
  xlab("year") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size=12,  family="Garamond"))

px3


# FIGURE 2C

px4 <-ggplot(dvs2, aes(x=year, y=communal_type, size = communal_type, color = abc)) +
  geom_point(alpha=0.4) +
  scale_size_continuous(range = c(.1, 10), name="# actors") +
  geom_text_repel(data=subset(dvs2, communal_type > 5 ), aes(label=abc, size=10)) +
  ylab("Number of actors (communal type)") +
  xlab("year") +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(text=element_text(size=12,  family="Garamond"))
px4


# FIGURE 3

px5 <- ggplot(dvs2, aes(x=year, y=all_orgdyads, size = all_orgdyads, color = abc)) +
  geom_point(alpha=0.4) +
  scale_size_continuous(range = c(.1, 10), name="# dyads") +
  geom_text_repel(data=subset(dvs2, all_orgdyads > 8 ), aes(label=abc, size=10)) +
  ylab("Number of hostile dyads (all types)") +
  xlab("year") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size=12,  family="Garamond"))
px5 


