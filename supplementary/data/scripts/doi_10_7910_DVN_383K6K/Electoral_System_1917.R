### Socialist Threat: Electoral System Figures (A8 & A9)
### Author: André Walter
### andre.walter[at]unisg.ch

### Load packages

library(openxlsx)
library(tidyverse)
library(xtable)

rm(list=ls())

setwd("C:/Work/Dropbox/Papers/Soc_Threat/Data")

data <- read.xlsx("Data_Ger.xlsx")

data <- data %>% 
  mutate(party_new = ifelse(Party %in% c("K","CS","RP","M","DR","DS","A","BdL","WVg","HBB"), "Conservatives", NA),
         party_new = ifelse(Party %in% c("Z","ELZ"), "Centre Party",party_new),
         party_new = ifelse(Party %in% c("NL","DBB","BB"), "National Liberals",party_new),
         party_new = ifelse(Party %in% c("FoVP","DVg","DVP","FVP","LL/ELV","LL","ELV/LL"), "Left Liberals",party_new),
         party_new = ifelse(Party %in% c("S","USPD"), "Social Democrats",party_new),
         party_new = ifelse(Party %in% c("Minority","P","D","W","ULP"), "Minorities",party_new),
         districtid = Electoral_District_Number) %>% 
  group_by(districtid,Year2,party_new) %>% summarize(parvotes = sum(Votes_Received, na.rm = T), votes = mean(Turnout, na.rm = T)) %>%
  ungroup() %>% mutate(vot_sh = parvotes/votes, mrdis = 1) %>%
  dplyr::select(districtid,party_new,vot_sh,mrdis) %>% 
  filter(party_new %in% c("Conservatives","Centre Party","National Liberals",
                          "Left Liberals","Social Democrats", "Minorities")) %>% drop_na() %>% rename(Party = party_new)

data <- data %>% complete(districtid, nesting(Party)) %>%group_by(districtid) %>% 
  arrange(districtid,vot_sh) %>%
  fill(mrdis) %>% mutate(vot_sh = ifelse(is.na(vot_sh), 0, vot_sh)) 

gerdat <- data %>% mutate(merg = districtid) %>%  filter(merg != 298)

gerdat <- gerdat %>% mutate(pr_dis_mag2 = ifelse(merg %in% c(31:36), 10,
                                                 ifelse(merg %in% c(46),7,
                                                        ifelse(merg %in% c(380:382), 5,
                                                               ifelse(merg %in% c(183),4,
                                                                      ifelse(merg %in% c(296:297),4,
                                                                             ifelse(merg %in% c(91:92),3,
                                                                                    ifelse(merg %in% c(192),2,
                                                                                           ifelse(merg %in% c(237:238),3,
                                                                                                  ifelse(merg %in% c(288:290),3,
                                                                                                         ifelse(merg %in% c(201:202),3,
                                                                                                                ifelse(merg %in% c(210),2,
                                                                                                                       ifelse(merg %in% c(208),2,
                                                                                                                              ifelse(merg %in% c(211),3,
                                                                                                                                     ifelse(merg %in% c(212),3,
                                                                                                                                            ifelse(merg %in% c(158),2,
                                                                                                                                                   ifelse(merg %in% c(308),2,
                                                                                                                                                          ifelse(merg %in% c(42),3,
                                                                                                                                                                 ifelse(merg %in% c(103),2,
                                                                                                                                                                        ifelse(merg %in% c(104),2,
                                                                                                                                                                               ifelse(merg %in% c(147),2,
                                                                                                                                                                                      ifelse(merg %in% c(172),2,
                                                                                                                                                                                             ifelse(merg %in% c(183),4,
                                                                                                                                                                                                    ifelse(merg %in% c(184),3,
                                                                                                                                                                                                           ifelse(merg %in% c(267),2,
                                                                                                                                                                                                                  ifelse(merg %in% c(300),2,
                                                                                                                                                                                                                         ifelse(merg %in% c(235),2,
                                                                                                                                                                                                                                ifelse(merg %in% c(379),2,1))))))))))))))))))))))))))),
                            infl2 = ifelse(Party %in% c("Social Democrats","National Liberals","Left Liberals","Centre Party"),1,0))


disc <- gerdat %>% filter(Party == "Social Democrats") %>% mutate(mmd = ifelse(pr_dis_mag2 >1,1,0))

mmd <- disc %>% filter(mmd == 1) 
smd <- disc %>% filter(mmd == 0) 

### Figure A8a

ggplot(smd, aes(vot_sh)) + geom_histogram(color="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(vot_sh)), color="black", linetype="dashed", size=1)+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", 
                                             x = "Vote Share Social Democrats 1912",
                                             y = "Number of Electoral Districts")

ggsave("D:/Sync/Dropbox/Papers/Soc_Threat/Paper/Revision/SMD.eps", height=7, width=10, units='in',device=cairo_ps)

### Figure A8b

ggplot(mmd, aes(vot_sh)) + geom_histogram(color="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(vot_sh)), color="black", linetype="dashed", size=1)+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", 
                                             x = "Vote Share Social Democrats 1912",
                                             y = "Number of Electoral Districts")
ggsave("D:/Sync/Dropbox/Papers/Soc_Threat/Paper/Revision/MMD.eps", height=7, width=10, units='in',device=cairo_ps)


### Figure A9

ggplot(mmd, aes(vot_sh,pr_dis_mag2)) + geom_point(color="black")+ geom_smooth(method = "lm", se = F, colour = "black")+
  theme_bw()+ theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", 
                                             x = "Vote Share Social Democrats 1912",
                                             y = "District Magnitude")

ggsave("D:/Sync/Dropbox/Papers/Soc_Threat/Paper/Revision/socmmd.eps", height=7, width=10, units='in',device=cairo_ps)
