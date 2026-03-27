##HarbridgeYong_Volden_Wiseman_Addtl_Figures.R
##Replication files for figures of bipartisanship over time

##Libraries
##Install libraries if not already installed in R
library(haven)
library(tidyverse)
library(ggplot2)
library(gridExtra)

##Set working directory
setwd("") ##Enter own working directory where data is stored

##Load data
h.les<- read_dta("HarbridgeYong_Volden_Wiseman_House_Replication.dta", .name_repair = "unique") 
s.les<- read_dta("HarbridgeYong_Volden_Wiseman_Senate_Replication.dta", .name_repair = "unique") 

##Directory for output
#setwd("") ##Enter directory for saving figure output if desired

##################################################
##        Create key variables                  ##
##################################################
##correct Senate year
s.les$year_session<- s.les$year+1
summary(s.les$year_session)

##create same name for House
h.les$year_session<- h.les$year
summary(h.les$year_session)

####################
##Prep for figures
##Senate
s.year<- s.les %>%
  select(year_session, mean_prop_cospon_opp_spon_SN, prop_co_bipart, 
         mean_prop_cospon_opp_spon_SN_nc, prop_co_bipart_nc,
         mean_prop_cospon_opp_spon_SN_ss, prop_co_bipart_ss) %>%
  filter(!is.na(year_session)) %>%
  group_by (year_session) %>%
  mutate(senate_mean_bipart_spon = mean(mean_prop_cospon_opp_spon_SN, na.rm=TRUE),
         senate_mean_bipart_cospon = mean(prop_co_bipart, na.rm=TRUE),
         senate_mean_bipart_spon_nc = mean(mean_prop_cospon_opp_spon_SN_nc, na.rm=TRUE),
         senate_mean_bipart_cospon_nc = mean(prop_co_bipart_nc, na.rm=TRUE),
         senate_mean_bipart_spon_ss = mean(mean_prop_cospon_opp_spon_SN_ss, na.rm=TRUE),
         senate_mean_bipart_cospon_ss = mean(prop_co_bipart_ss, na.rm=TRUE)) %>%
  distinct(year_session, .keep_all = T) %>%
  select (year_session, senate_mean_bipart_spon, senate_mean_bipart_cospon,
          senate_mean_bipart_spon_nc, senate_mean_bipart_cospon_nc,
          senate_mean_bipart_spon_ss, senate_mean_bipart_cospon_ss)

##House
h.year<- h.les %>%
  select(year_session, mean_prop_cospon_opp_spon_HR, prop_co_bipart, 
         mean_prop_cospon_opp_spon_HR_nc, prop_co_bipart_nc,
         mean_prop_cospon_opp_spon_HR_ss, prop_co_bipart_ss) %>%
  filter(!is.na(year_session)) %>%
  group_by (year_session) %>%
  mutate(house_mean_bipart_spon = mean(mean_prop_cospon_opp_spon_HR, na.rm=TRUE),
         house_mean_bipart_cospon = mean(prop_co_bipart, na.rm=TRUE),
         house_mean_bipart_spon_nc = mean(mean_prop_cospon_opp_spon_HR_nc, na.rm=TRUE),
         house_mean_bipart_cospon_nc = mean(prop_co_bipart_nc, na.rm=TRUE),
         house_mean_bipart_spon_ss = mean(mean_prop_cospon_opp_spon_HR_ss, na.rm=TRUE),
         house_mean_bipart_cospon_ss = mean(prop_co_bipart_ss, na.rm=TRUE)) %>%
  distinct(year_session, .keep_all = T) %>%
  select (year_session, house_mean_bipart_spon, house_mean_bipart_cospon,
          house_mean_bipart_spon_nc, house_mean_bipart_cospon_nc,
          house_mean_bipart_spon_ss, house_mean_bipart_cospon_ss)

###########################
##Stack H and S year-level data for ggplot
h.year2<- h.year
h.year2$Chamber<- "House"
s.year2<- s.year
s.year2$Chamber<- "Senate"
names(h.year2)<- c("year_session","mean_bipart_spon", "mean_bipart_cospon",
                   "mean_bipart_spon_nc", "mean_bipart_cospon_nc", 
                   "mean_bipart_spon_ss",  "mean_bipart_cospon_ss",
                   "Chamber")
names(s.year2)<- c("year_session","mean_bipart_spon", "mean_bipart_cospon",
                   "mean_bipart_spon_nc", "mean_bipart_cospon_nc", 
                   "mean_bipart_spon_ss",  "mean_bipart_cospon_ss",
                   "Chamber")
data.year.stack<- rbind(h.year2, s.year2)

###################################################
##                    Figures                    ##
###################################################

##Figure A1 - Average proportion bipartisan cosponsors attracted
##All bills
ggplot(data=data.year.stack, aes(x=year_session, y=mean_bipart_spon, group=Chamber)) +
  geom_line(aes(linetype=Chamber))+
  scale_y_continuous(limits=c(0,.6)) + 
  xlab("Year Congress Began") +
  ylab("Proportion Bipartisan \nCosponsors Attracted") +
  ##ggtitle("All Bills")
  ggsave("FigA1_Avg_bipart_spon_year.png", height=6, width=8)

#####################
##Figure A2 - Average proportion bipartisan cosponsors attracted
##S&S bills only
ggplot(data=data.year.stack, aes(x=year_session, y=mean_bipart_spon_ss, group=Chamber)) +
  geom_line(aes(linetype=Chamber))+
  scale_y_continuous(limits=c(0,.6)) + 
  xlab("Year Congress Began") +
  ylab("Proportion Bipartisan \nCosponsors Attracted") +
  ##ggtitle("S&S Bills")
  ggsave("FigA2_Avg_bipart_spon_year_ss.png", height=6, width=8)


############################################
############################################
##Prep for boxplots
##Drop NA from year
h.les.clean<- h.les %>%
  filter(!is.na(year_session))

s.les.clean<- s.les %>%
  filter(!is.na(year_session))

#################
##Figure 1 - BOXPLOT of proportion bipartisan cosponsors attracted
##House, all bills
h.bipart.spon<- ggplot(h.les.clean, aes(x=as.factor(year_session), y=mean_prop_cospon_opp_spon_HR)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme(axis.text=element_text(size=11, angle=90), axis.title.y = element_text(size = 11),
        plot.title = element_text(size=11))+
  xlab("Year Congress Began") +
  ylab("Proportion Bipartisan \nCosponsors Attracted") +
  ggtitle("House")

##Senate, all bills
s.bipart.spon<- ggplot(s.les.clean, aes(x=as.factor(year_session), y=mean_prop_cospon_opp_spon_SN)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme(axis.text=element_text(size=11, angle=90), axis.title.y = element_text(size = 11),
        plot.title = element_text(size=11))+
  xlab("Year Congress Began") +
  ylab("Proportion Bipartisan \nCosponsors Attracted") +
  ggtitle("Senate")

##Combine House and Senate
grid.arrange(h.bipart.spon, s.bipart.spon, nrow=2)
bipart.spon<- arrangeGrob(h.bipart.spon, s.bipart.spon, nrow=2)
ggsave("Fig1_bipart_spon_H_S_combine_year.png", bipart.spon, height=11, width=8)
