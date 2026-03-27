######################## Code Summary ##################
#Replication for The Impact of Vote-By-Mail Policy on Turnout and Vote Share in the 2020 Election
#Election Law Journal 
#Amlani & Collitt (2021)
#R Version: 4.1.1
#December 8 2021

#This R script makes the map in figures 1
########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################

######################### Library #####################
library(tidyverse)
library(ggthemes)
library(scales)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(sf)

######################## Upload Data ##################

#*********************** Turnout Data ********************
#Set Working Directory
#This neat snippet sets your working directory to wherever the script is located!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Upload Data
load(file = "Replication Data - Amlani & Collitt 2021.rda"); VBM_Master.1 <- VBM_Master.Pure

#******************* County Map **********************
load(file = "Replication Data - County Level Shapefile - Amlani & Collitt 2021.rda")

######################### Examine Data ##################
head(VBM_Master.1)
head(counties_sf)

colnames(VBM_Master.1)
colnames(counties_sf)

####################### Checks ###################
#Make Sure State And County FIPS Are the Same
table(nchar(counties_sf$county_fips))

names(counties_sf)[names(counties_sf) == 'county_fips'] <- 'state_county_fips'

####################### Data Management #####################

#********************* Subset 2020 Year *********************

VBM_Master.2 <- subset(VBM_Master.1, year == "2020")  

#********************* Capitalize  *********************
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "Mail-in ballots automatically sent"] <- "Automatic Vote-by-Mail"
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "Mail-in ballot applications automatically sent"] <-  "Vote-by-Mail Applications Sent"
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "request a mail-in ballot"] <- "No Excuse-Needed Absentee"
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "need an excuse"] <- "Excuse-Needed Absentee"


#********************* Reorder Levels
VBM_Master.2$Mail_In_Vote <- factor(VBM_Master.2$Mail_In_Vote, levels = c("Excuse-Needed Absentee", "No Excuse-Needed Absentee", 
                                                                          "Vote-by-Mail Applications Sent", "Automatic Vote-by-Mail"))

##################### Merge Data with Map #######################

VBM_Master.3 <- left_join(counties_sf,
                          VBM_Master.2,
                          by = "state_county_fips")

##################### Maps ##################

#**************** Vote By Mail Policies  ********************
P3 <- ggplot(subset(VBM_Master.3, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = Mail_In_Vote),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank()) +
  labs(title = "2020 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_grey("Vote-By-Mail Policy", labels = levels(VBM_Master.3$Mail_In_Vote),  na.value = "darkgrey") +
  theme(legend.position="bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ 
  theme(plot.title=element_text(size=18, hjust=0.5, face="bold", color="black")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title="Vote-By-Mail Policy"))

######################### VBM Laws 2016 ######################### 
VBM_Master.2 <- subset(VBM_Master.1, year == "2016")  

#Capitalize 
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "Mail-in ballots automatically sent"] <- "Automatic Vote-by-Mail"
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "No-excuse absentee"] <- "No Excuse-Needed Absentee"
VBM_Master.2$Mail_In_Vote[VBM_Master.2$Mail_In_Vote == "Absentee with excuse"] <- "Excuse-Needed Absentee"


#Reorder Levels
VBM_Master.2$Mail_In_Vote <- factor(VBM_Master.2$Mail_In_Vote, levels = c("Excuse-Needed Absentee", "No Excuse-Needed Absentee", 
                                                                          "Automatic Vote-by-Mail"))

#********************* Merge Data with Map *********************

VBM_Master.3 <- left_join(counties_sf,
                          VBM_Master.2,
                          by = "state_county_fips")

#******************* 2016 VBM Policies *********************
P7 <- ggplot(subset(VBM_Master.3, state_abbv != "AK")) +
  geom_sf(mapping = aes(fill = Mail_In_Vote),
          color = "white", size = 0.05) +
  coord_sf(datum = NA) +
  theme(panel.background = element_blank()) +
  labs(title = "2016 Presidential Election") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_grey("Vote-By-Mail Policy", labels = levels(VBM_Master.3$Mail_In_Vote),  na.value = "darkgrey") +
  theme(legend.position="bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ 
  theme(plot.title=element_text(size=18, hjust=0.5, face="bold", color="black")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title="Vote-By-Mail Policy")); P7

############################ Arrange Plots ######################
library(ggplot2)
library(patchwork)
library(grid)

#*********************** 2016 and 2020 VBM  ********************
#Submission

tiff(file = "AmlaniCollittFig1.tiff", units="in", width=15, height=20,  res = 300)
grid.arrange(P7,P3, nrow = 2, ncol = 1, top = textGrob(str_wrap("Figure 1: (a) U.S. Counties' Vote-by-Mail Policies in the 2016 Presidental Election and 
                                             (b) U.S. Counties' Vote-by-Mail Policies in the 2020 Presidental Election", 100), 
                                                       gp=gpar(fontsize=20)),
             bottom = textGrob(str_wrap("Note: Alaska is excluded from the figure because Alaska does not use counties, but electoral districts, to administer elections. Therefore, turnout data at the county level is unavailable and we omit it from our analysis.", 100),
                               x = .01,
                               y = 0.5,
                               gp=gpar(fontsize=20),
                               just = "left"))
dev.off()

