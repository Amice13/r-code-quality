##-----------------------------------------------------------------------------##
##  Replication Code for: Zvobgo, Kelebogile. Governing Truth: NGOs and the    ##
##                        Politics of Transitional Justice. Oxford University  ##
##                        Press.                                               ##
##                                                                             ##
##-----------------------------------------------------------------------------##


# install.packages("ggplot2")
# install.packages("geodata")
# install.packages("rworldmap")
# install.packages("tidyverse")
# install.packages("maps")
# install.packages("countrycode")
# install.packages("foreign")
# install.packages("survival")
# install.packages("survminer")
# install.packages("stargazer")
# install.packages("haven")
# install.packages("readstata13")
# install.packages("readxl")

library(ggplot2)
library(geodata)
library(rworldmap)
library(tidyverse)
library(maps)
library(countrycode)
library(foreign)
library(survival)
library(survminer)
library(stargazer)
library(haven)
library(readstata13)
library(readxl)


# Set working directory
setwd("/Users/kelebogilezvobgo/Library/CloudStorage/Dropbox/1_Research/1_Publications/29_Governing-Truth/OUP_FINAL/Data Files")


#-------------------------------------#
# Transitional TCs Map
#-------------------------------------#

## Prepping the data ##

tc <- read_excel("TCs and TTCs Around the World kz2025governing.xlsx")

#Map of transitional TCs (total # ever by country)
tc$ccode <- countrycode(tc$country_1, 'country.name', 'cown')
tc_dens <- aggregate(tc$country_1, by = list(tc$ccode), length) #get total number of TCs for each country
trans_dens <- aggregate(tc$country_1[tc$transitional == 1], by = list(tc$ccode[tc$transitional == 1]), length)
names(trans_dens)[names(trans_dens) == 'x'] <- 'trans'

# world map shape data
map.world <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

#merge with TC data
map.world$ccode <- countrycode(map.world$region, 'country.name', 'cown')
map.world$geog <- ifelse(map.world$ccode < 30, "North America", 
                         ifelse(map.world$ccode < 100, "C. America and Caribbean",
                                ifelse(map.world$ccode < 200, "South America",
                                       ifelse(map.world$ccode < 400, "Europe", 
                                              ifelse(map.world$ccode < 600, "Africa",
                                                     ifelse(map.world$ccode < 700, "MENA","Asia"))))))
map.world <- merge(map.world, tc_dens, by.x="ccode", by.y="Group.1", all.x = T)
map.world <- merge(map.world, trans_dens, by.x="ccode", by.y="Group.1", all.x = T)
map.world <- map.world[order(map.world$order),]
map.world$x <- ifelse(is.na(map.world$x), 0, map.world$x)
map.world$trans <- ifelse(is.na(map.world$trans), 0, map.world$trans)


## Making the figures ##

# Figure 3.1: Truth Commissions during Transitions (1970-2018)

setEPS()
postscript("/Users/kelebogilezvobgo/Dropbox/Apps/Overleaf/Governing Truth/trans_TC_map_BW.eps", width=12, height=5)

ggplot(map.world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = as.factor(trans)), color = "black", lwd = 0.1) +
  scale_fill_manual("TCs", values = c('#ffffff','#bdbdbd','#636363','#252525','#252525'), na.translate = F) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(legend.key.size = unit(2.5, "lines")) +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  coord_fixed()

dev.off()





#------------------------------------#
# Transitional TCs Time-Series Plot  #
#------------------------------------#

## Prepping the data ##

#regions
tc$Region <- ifelse(tc$ccode < 200, "Americas", 
                    ifelse(tc$ccode < 400, "Europe", 
                           ifelse(tc$ccode < 600, "Africa",
                                  ifelse(tc$ccode < 700, "MENA", "Asia and Oceania"))))
# decades 
tc$Decade <- tc$`commission start` - (tc$`commission start` %% 10)
tc$any <- 1

#trans_dec <- aggregate(tc[,c(7,16)], by = list(tc$decade, tc$region), sum)
trans_dec <- aggregate(tc[,c(7,11)], by = list(tc$Decade, tc$Region), sum)
names(trans_dec)[1] <- "Decade"
names(trans_dec)[2] <- "Region"

dec <- expand.grid(unique(tc$Decade), unique(tc$Region))
names(dec)[1] <- "Decade"
names(dec)[2] <- "Region"
dec <- merge(dec, trans_dec, by = c("Decade","Region"), all = T)
dec[is.na(dec)] <- 0

tmp <- aggregate(dec[,3:4], by = list(dec$Decade), sum)
tmp$Region <- "Overall"
names(tmp)[1] <- "Decade"
tmp <- tmp[,c(1,4,2,3)]
tmp <- rbind(tmp, dec)

dec$prop <- dec$transitional/dec$any
tmp$prop <- tmp$transitional/tmp$any
dec[is.na(dec)] <- 0
tmp[is.na(tmp)] <- 0


## Making the figures ##

# Figure A1: Truth Commissions during Transitions as a Share of All Truth Commissions by Decade (1970-2018)

setEPS()
postscript("/Users/kelebogilezvobgo/Dropbox/Apps/Overleaf/Governing Truth/trans_TC_proportion_BW.eps", width=11, height=8)

ggplot(data = tmp, aes(x = Decade, y = prop, color = Region)) +
  geom_line(size = 1, aes(linetype=Region)) +
  scale_colour_grey() +
  ylab("Transitional TCs / All TCs") +
  xlab("Decade")+
  theme(axis.text.y = element_text(color="#181818", size=16, margin=margin(0,0,0,20)))+
  theme(axis.text.x = element_text(color="#181818", size=16, margin=margin(0,0,20,0)))+
  theme(axis.title.y = element_text(size=18))+
  theme(axis.title.x = element_text(size=18))+
  theme(legend.key.size = unit(2.5, "lines"))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16))
  
dev.off()



# Figure A2: Truth Commissions during Transitions as a Share of All Truth Commissions by Region (1970-2018)

setEPS()
postscript("/Users/kelebogilezvobgo/Dropbox/Apps/Overleaf/Governing Truth/trans_TC_bar_BW.eps", width=12, height=8)

ggplot(data = tmp, aes(x = Decade, y = prop)) +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.y = element_text(color="#181818", size=16, margin=margin(0,0,0,20)))+
  theme(axis.text.x = element_text(color="#181818", size=16, margin=margin(0,0,20,0)))+
  theme(axis.title = element_text(color="#181818", size=18)) +
  theme(strip.text = element_text(size=16,lineheight=5.0))+
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~Region, ncol = 3)+
  ylab("Transitional TCs / All TCs")+
  xlab("Decade")
  
dev.off()





#-------------------------------------#
# ICTJ TCs Map
#-------------------------------------#

## Prepping the data ##

tc <- read_excel("TCs and ICTJ Involvement kz2025governing.xlsx")

tc$ccode <- countrycode(tc$country_one, 'country.name', 'cown')
tc_dens <- aggregate(tc$ictj, by = list(tc$ccode), sum) #get total number of countries w/o ICTJ and with ICTJ once or mult times.
tc_dens$ictj <- ifelse(tc_dens$x > 0, 2, 1) #if a country has ICTJ advisement give 2, or else give 1. (still universe of comms)

# world map shape data
map.world <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

#merge with TC data
map.world$ccode <- countrycode(map.world$region, 'country.name', 'cown')
map.world <- merge(map.world, tc_dens, by.x="ccode", by.y="Group.1", all.x = T)
map.world <- map.world[order(map.world$order),]
map.world$ictj <- ifelse(is.na(map.world$ictj), 0, map.world$ictj)


## Making the figures ##

# Figure 4.1: The ICTJ and Truth Commissions Around the World (1970-2018)

setEPS()
postscript("/Users/kelebogilezvobgo/Dropbox/Apps/Overleaf/Governing Truth/ictj_TC_map_BW.eps", width=12, height=5)

ggplot(map.world, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = as.factor(ictj)), color = "black", lwd = 0.1) +
  scale_fill_manual("", values = c('#ffffff','#bdbdbd','#636363'), labels = c("No TC", "TC without \nICTJ Involvement", "TC with \nICTJ Involvement")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(legend.key.size = unit(2.5, "lines")) +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  coord_fixed()

dev.off()





#------------------------------------#
# Survival Analysis
#-------------------------------------#

# Table A9: Robustness Check: Truth Commission Adoption, using Single-Observation Survival Analysis

cox_conflict <- read.dta13("cox_conflict.dta") #call in the conflict data
surv_object_conflit <- Surv(time = cox_conflict$t2, event = cox_conflict$tc_0)
fit.coxph_conflict <- coxph(surv_object_conflit ~ v2xcs_ccsi_avg5 + f_media_ai_br_avg5 + ingo_avg5  +
                              latentmean_avg5 +  lji_avg5 + polconiii_avg5 + 
                              region_precedent + yr + igo_avg5 + 
                              gdppc_wdi_avg5 + oda_per_avg5 + pop_wdi_avg5 +
                              cumulative_intensity + duration +
                              cluster(gwno), data = cox_conflict)
summary(fit.coxph_conflict)



cox_killings <- read.dta13("cox_killings.dta") #call in the killings data
surv_object_killings <- Surv(time = cox_killings$t2, event = cox_killings$tc_0)
fit.coxph_killings <- coxph(surv_object_killings ~ v2xcs_ccsi_avg5 + f_media_ai_br_avg5 + ingo_avg5  +
                              latentmean_avg5 +  lji_avg5 + polconiii_avg5 + 
                              region_precedent + yr + igo_avg5 + 
                              gdppc_wdi_avg5 + oda_per_avg5 + pop_wdi_avg5 +
                              best_fatality_estimate +
                              cluster(gwno), data = cox_killings)
summary(fit.coxph_killings)



cox_dem <- read.dta13("cox_dem.dta") #call in the regime data
surv_object_democracy <- Surv(time = cox_dem$t2, event = cox_dem$tc_0)
fit.coxph_democracy <- coxph(surv_object_democracy ~ v2xcs_ccsi_avg5 + f_media_ai_br_avg5 + ingo_avg5  +
                               latentmean_avg5 +  lji_avg5 + polconiii_avg5 + 
                               region_precedent + yr + igo_avg5 + 
                               gdppc_wdi_avg5 + oda_per_avg5 + pop_wdi_avg5 +
                               democracy_breakdowns + 
                               cluster(gwno), data = cox_dem)
summary(fit.coxph_democracy)



coefficients <- c("Strong Domestic Civil Society", "HRO Naming and Shaming", "Network Access (ln)", 
                  "Human Rights Respect", "Judicial Independence",
                  "Veto Players", "Regional Diffusion", "Global Diffusion", "IGO Membership (ln)", 
                  "GDP per capita (ln)", "ODA as % of GDP", "Population (ln)", "Conflict Intensity", 
                  "Conflict Duration", "Civilian Casualties (ln)", "Democracy Breakdowns")

stargazer(fit.coxph_conflict, fit.coxph_killings, fit.coxph_democracy, covariate.labels=coefficients, 
          star.char = c("+", "*", "**"), notes = c("+ p<0.1; * p<0.05; ** p<0.01"), 
          digits =2, single.row=TRUE)