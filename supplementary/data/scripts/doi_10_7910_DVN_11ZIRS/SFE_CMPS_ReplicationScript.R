#### SFE CMPS Replication Script ###
#Run using RStudio v. 1.0.136 and R v. 3.2.5.

##Load Packages##
library(ggplot2)        #v. 2.1.0
library(dplyr)          #v. 0.5.0
library(stargazer)      #v. 5.2
library(multiwayvcov)   #v. 1.2.2
library(lmtest)         #v. 0.9-34
library(MASS)           #v. 7.3-45
library(simcf)          #v. 0.2.15
#The best way to install "simcf" is to download directly from Chris Adolph's Github:
#install.packages("devtools")
#library(devtools)
#install_github("chrisadolph/simcf")

###Load Data###
SFE<-read.csv("SFE_CMPS_Replication_Dataset.csv", na.strings=c("-99", "-88", "NA"))

###Produce Figures 1 and 2: Ethnic Manipulation Over Time###
SFE_RFA<-SFE[SFE$DIVISION==1,]

##Figure 1: Officer Corps##

#Propotion Table
OC_PROP<-data.frame(prop.table(table(SFE_RFA$YEAR, SFE_RFA$OFFICER_RELATIVE), margin=1))
colnames(OC_PROP)<-c("Year", "Level", "Frequency")
OC_PROP$Year<-as.numeric(as.character(OC_PROP$Year))

#Plot
Figure1<-ggplot(OC_PROP, aes(x=Year, y=Frequency, fill=Level)) +
  geom_area(position="stack") + 
  scale_fill_grey(guide=guide_legend(reverse=TRUE),name="SFE Relative Level",
                  labels=c("Excluded", "Underrepresented", "Proportionate","Overrepresented","Monopoly"))+ 
  theme_bw()
Figure1
dev.print(png, "Figure1.png", width=600, height=483)
dev.off()

##Figure 2: Rank and File##

#Proportion Table
RF_PROP<-data.frame(prop.table(table(SFE_RFA$YEAR, SFE_RFA$RANK_FILE_RELATIVE), margin=1))
colnames(RF_PROP)<-c("Year", "Level", "Frequency")
RF_PROP$Year<-as.numeric(as.character(OC_PROP$Year))

#Plot
Figure2<-ggplot(RF_PROP, aes(x=Year, y=Frequency, fill=Level)) +
  geom_area(position="stack") + 
  scale_fill_grey(guide=guide_legend(reverse=TRUE),name="SFE Relative Level",
                  labels=c("Excluded", "Underrepresented", "Proportionate","Overrepresented","Monopoly"))+ 
  theme_bw()
Figure2
dev.print(png, "Figure2.png", width=600, height=483)
dev.off()

###Produce underlying data for Tables 1 and 2 -- State-Level Measures###

#Create State-Year dataframe with percent of population belonging to groups underrepresented or excluded in OC and RF
SFE_EPR<-SFE[!is.na(SFE$EPR_GROUP_ID),]#(Only includes EPR groups due to group size data availability)
SFE_STATE<-SFE_EPR%>%
  mutate(UNDERREP_OFF=ifelse(OFFICER_RELATIVE %in% c(1,2), EPR_GROUP_SIZE, 0))%>%
  mutate(UNDERREP_RF=ifelse(RANK_FILE_RELATIVE %in% c(1,2), EPR_GROUP_SIZE, 0))%>%
  group_by(GWID, STATENAME, YEAR)%>%
  summarise(STATE_UNDERREP_OFF=sum(UNDERREP_OFF), STATE_UNDERREP_RF=sum(UNDERREP_RF))

#Summary Statistics as printed in Table 1
summary(SFE_STATE$STATE_UNDERREP_OFF)
sd(SFE_STATE$STATE_UNDERREP_OFF, na.rm=TRUE)
summary(SFE_STATE$STATE_UNDERREP_RF)
sd(SFE_STATE$STATE_UNDERREP_RF, na.rm=TRUE)

#Save State Level as .csv, reload, and produce Latex code for Table 1
#oddly required as stargazer can't read data frames created with dplyr for some reason
write.csv(SFE_STATE, "SFE_State_Level.csv")
SFE_STATE<-read.csv("SFE_State_Level.csv")
stargazer(SFE_STATE[c("STATE_UNDERREP_OFF", "STATE_UNDERREP_RF")],
          covariate.labels = c("Population Underrepresented in Officer Corps",
                               "Population Underrepresented in Rank and File"))

#Ranked Exclusion Levels for 1963 and 2013
STATE_1963<-SFE_STATE[SFE_STATE$YEAR==1963,]
STATE_2013<-SFE_STATE[SFE_STATE$YEAR==2013,]
TABLE2<-merge(STATE_1963, STATE_2013, by="GWID", all.x=TRUE, all.y=TRUE)
TABLE2<-TABLE2[order(-TABLE2$STATE_UNDERREP_OFF.y),]
TABLE2$CHANGE<-TABLE2$STATE_UNDERREP_OFF.y-TABLE2$STATE_UNDERREP_OFF.x
View(TABLE2)


##Figures 3 and 4: Comparing SFE to EPR
SFE_EPR_Figs<-SFE_EPR[!is.na(SFE_EPR$EPR_ORDINAL),]
SFE_EPR_Fig3<-SFE_EPR_Figs[!is.na(SFE_EPR_Figs$OFFICER_RELATIVE),]

Figure3<-ggplot(SFE_EPR_Fig3, aes(x=EPR_ORDINAL, y=OFFICER_RELATIVE)) + geom_point(shape=1, position=position_jitter(width=.3,height=.2)) + geom_smooth(method=lm) +
  scale_x_continuous(name="EPR Ordinal Scale", breaks=c(1,2,3,4,5,6), labels=c("Discriminated","Powerless", "Jr. Partner", "Sr. Partner", "Dominant", "Monopoly")) + 
  scale_y_continuous(name="Officer Representation (Relative)", breaks=(c(1,2,3,4,5)), labels=c("Excluded", "Underrepresented", "Proportionate", "Overrepresented", "Monopoly")) +
  theme_bw()
Figure3
dev.print(png, "Figure3.png", width=600, height=483)
dev.off()

SFE_EPR_Fig4<-SFE_EPR_Figs[!is.na(SFE_EPR_Figs$RANK_FILE_RELATIVE),]

Figure4<-ggplot(SFE_EPR_Fig4, aes(x=EPR_ORDINAL, y=RANK_FILE_RELATIVE)) + geom_point(shape=1, position=position_jitter(width=.3,height=.2)) + geom_smooth(method=lm) +
  scale_x_continuous(name="EPR Ordinal Scale", breaks=c(1,2,3,4,5,6), labels=c("Discriminated","Powerless",  "Jr. Partner", "Sr. Partner", "Dominant", "Monopoly")) + 
  scale_y_continuous(name="Rank and File Representation (Relative)", breaks=(c(1,2,3,4,5)), labels=c("Excluded", "Underrepresented", "Proportionate", "Overrepresented", "Monopoly")) +
  theme_bw()
Figure4
dev.print(png, "Figure4.png", width=600, height=483)
dev.off()

##Tables 3 and 4: Coup Regression Models

#Formulas
Coup_F1<-SPEED_COUP_ATTEMPT ~ EPR_SRPLUS + COUP_SPELL + COUP_SPELL_2 + COUP_SPELL_3
Coup_F2<-SPEED_COUP_ATTEMPT ~ OFFICER_MAJORITY + COUP_SPELL + COUP_SPELL_2 + COUP_SPELL_3
Coup_F3<-SPEED_COUP_ATTEMPT ~ OFFICER_MAJORITY*EPR_SRPLUS + COUP_SPELL + COUP_SPELL_2 + COUP_SPELL_3
Coup_F4<-SPEED_COUP_ATTEMPT ~ OFFICER_MAJORITY*EPR_SRPLUS + EPR_CENTERSEG + EPR_GROUPNUM + EPR_GROUP_SIZE_LOG + 
  COUP_SPELL + COUP_SPELL_2 + COUP_SPELL_3
Coup_F5<-SPEED_COUP_ATTEMPT ~ OFFICER_MAJORITY*EPR_SRPLUS + EPR_CENTERSEG + EPR_GROUPNUM + EPR_GROUP_SIZE_LOG + 
  NMC_CPOP_LOG_LAG_INT + MAD_GDPPC_LOG_LAG_INT + COUP_SPELL + COUP_SPELL_2 + COUP_SPELL_3

#Models
Coup_M1<-glm(Coup_F1, family="binomial", data=SFE_EPR)
Coup_M2<-glm(Coup_F2, family="binomial", data=SFE_EPR)
Coup_M3<-glm(Coup_F3, family="binomial", data=SFE_EPR)
Coup_M4<-glm(Coup_F4, family="binomial", data=SFE_EPR)
Coup_M5<-glm(Coup_F5, family="binomial", data=SFE_EPR)

#Cluster standard errors at the group level.
Coup_M1.gcse <- sqrt(diag(cluster.vcov(Coup_M1, SFE_EPR$EPR_GROUP_ID)))
Coup_M2.gcse <- sqrt(diag(cluster.vcov(Coup_M2, SFE_EPR$EPR_GROUP_ID)))
Coup_M3.gcse <- sqrt(diag(cluster.vcov(Coup_M3, SFE_EPR$EPR_GROUP_ID)))
Coup_M4.gcse <- sqrt(diag(cluster.vcov(Coup_M4, SFE_EPR$EPR_GROUP_ID)))
Coup_M5.gcse <- sqrt(diag(cluster.vcov(Coup_M5, SFE_EPR$EPR_GROUP_ID)))

#Produce Summaries with group-clustered errors
coeftest(Coup_M1, cluster.vcov(Coup_M1, SFE_EPR$EPR_GROUP_ID))
coeftest(Coup_M2, cluster.vcov(Coup_M2, SFE_EPR$EPR_GROUP_ID))
coeftest(Coup_M3, cluster.vcov(Coup_M3, SFE_EPR$EPR_GROUP_ID))
coeftest(Coup_M4, cluster.vcov(Coup_M4, SFE_EPR$EPR_GROUP_ID))
coeftest(Coup_M5, cluster.vcov(Coup_M5, SFE_EPR$EPR_GROUP_ID))



#Produce Latex Code for Table 3
stargazer(Coup_M1, Coup_M2, Coup_M3, Coup_M4, Coup_M5, 
          se = list(Coup_M1.gcse, Coup_M2.gcse, Coup_M3.gcse, Coup_M4.gcse, Coup_M5.gcse),
          title = "Effect of Military and Political Representation on Attempted Coups", 
          font.size = "small",
          align = TRUE, 
          notes = c("Cubic polynomial for time not displayed.",
                    "GDP and population lagged 1 year.",
                    "(Robust standard errors clustered by group)"
          ),
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          order = c(1,7,11),
          omit = c("COUP_SPELL", "COUP_SPELL_2", "COUP_SPELL_3"),
          covariate.labels = c(
            "Senior Partner or Higher (EPR)", 
            "Officer Majority (SFE)", 
            "EPR x SFE",
            "Num. Groups in Government",
            "Num. Groups in Country",
            "ln(Group Size)",
            "ln(Country Population)",
            "ln(GDP Per Capita)"
          ),
          colnames = TRUE)

##Simulated Expected Probabilities for Table 4
Coup_M5_Data<- extractdata(Coup_F5, SFE_EPR, na.rm=TRUE)
set.seed(815)
pe <- Coup_M5$coefficients 
vc <- cluster.vcov(Coup_M5, SFE_EPR$EPR_GROUP_ID)        
sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)
oa_range<-seq(0,1,1) 
nscen=length(oa_range)
Majscen <- Minscen <- cfMake(Coup_F5, Coup_M5_Data, nscen)

#EPR and SFE measures to vary as all other variables held at mean
for(i in 1:nscen){
  Majscen <- cfChange(Majscen, "EPR_SRPLUS", oa_range[i], scen=i)
  Majscen <- cfChange(Majscen, "OFFICER_MAJORITY", x=1, scen=i)
  Minscen <- cfChange(Minscen, "EPR_SRPLUS", oa_range[i], scen=i)
  Minscen  <- cfChange(Minscen, "OFFICER_MAJORITY", x=0, scen=i)}

#simulate expected y's given x's, using point estimates and var-cov matrix specified above
Majsims <- logitsimev(Majscen, simbetas, ci=0.95)
Minsims <- logitsimev(Minscen, simbetas, ci=0.95)
Coup_M5_EVs1<-data.frame(oa_range, Majsims$pe, Majsims$lower, Majsims$upper,
                          Minsims$pe, Minsims$lower, Minsims$upper)
View(Coup_M5_EVs1)

###Table 5 and Figure 5:  MAR Repression Models###

#Abridged Dataframe
SFE_MAR<-SFE[!is.na(SFE$MAR_GROUP_ID),]
SFE_MAR<-SFE_MAR[SFE_MAR$YEAR>1995 & SFE_MAR$YEAR<2007,]

#Formulas

REP_F1<-REPTOTAL ~ RANK_FILE_LOWER + MAR_POLDIS_LAG + MAR_GROUPSIZE_LOG_LAG + MAR_STATEPOP_LOG_LAG + REP_SPELL + REP_SPELL_2 + REP_SPELL_3
REP_F2<-REPTOTAL ~ RANK_FILE_UPPER + MAR_POLDIS_LAG + MAR_GROUPSIZE_LOG_LAG + MAR_STATEPOP_LOG_LAG + REP_SPELL + REP_SPELL_2 + REP_SPELL_3
REP_F3<-REPTOTAL ~ OFFICER_LOWER + MAR_POLDIS_LAG + MAR_GROUPSIZE_LOG_LAG + MAR_STATEPOP_LOG_LAG + REP_SPELL + REP_SPELL_2 + REP_SPELL_3
REP_F4<-REPTOTAL ~ OFFICER_UPPER + MAR_POLDIS_LAG + MAR_GROUPSIZE_LOG_LAG + MAR_STATEPOP_LOG_LAG + REP_SPELL + REP_SPELL_2 + REP_SPELL_3

#Models

REP_M1<-glm(REP_F1, family="binomial", data=SFE_MAR)
REP_M2<-glm(REP_F2, family="binomial", data=SFE_MAR)
REP_M3<-glm(REP_F3, family="binomial", data=SFE_MAR)
REP_M4<-glm(REP_F4, family="binomial", data=SFE_MAR)

#Standard Errors Clustered at the Group Level

REP_M1.gcse<-sqrt(diag(cluster.vcov(REP_M1, SFE_MAR$MAR_GROUP_ID)))
REP_M2.gcse<-sqrt(diag(cluster.vcov(REP_M2, SFE_MAR$MAR_GROUP_ID)))
REP_M3.gcse<-sqrt(diag(cluster.vcov(REP_M3, SFE_MAR$MAR_GROUP_ID)))
REP_M4.gcse<-sqrt(diag(cluster.vcov(REP_M4, SFE_MAR$MAR_GROUP_ID)))

#Print summaries with clustered errors

coeftest(REP_M1, cluster.vcov(REP_M1, SFE_MAR$MAR_GROUP_ID))
coeftest(REP_M2, cluster.vcov(REP_M2, SFE_MAR$MAR_GROUP_ID))
coeftest(REP_M3, cluster.vcov(REP_M3, SFE_MAR$MAR_GROUP_ID))
coeftest(REP_M4, cluster.vcov(REP_M4, SFE_MAR$MAR_GROUP_ID))

##Latex Code to produce Table 5

stargazer(REP_M1, REP_M2, REP_M3, REP_M4,
          se = list(REP_M1.gcse, REP_M2.gcse, REP_M3.gcse, REP_M4.gcse),
          font.size = "small",
          title="Rank and File Representation and Minority Repression", 
          align=TRUE, 
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          omit = c("REP_SPELL", "REP_SPELL_2", "REP_SPELL_3"),
          notes = c("MAR Political Discrimination lagged 1 year", "Group Proportion and Country Size lagged and logged", "Cubic polynomial for time not displayed"),
          covariate.labels = c("Rank and File Lower Estimate", "Rank and File Upper Estimate", "Officer Corps Lower Estimate", "Officer Corps Upper Estimate",
                               "MAR Political Discrimination", "Group Proportion", "Country Size"))


##Simulated Expected Probabilities for Figure 5


#set up models and complete data
pe <- REP_M4$coefficients  # point estimates
vc <- cluster.vcov(REP_M4, SFE_MAR$MAR_GROUP_ID)        # var-cov matrix
sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)
REP_M4_Data<- extractdata(REP_F4, SFE_MAR, na.rm=TRUE)

# set list of x values all at mean
oa_range<-seq(0,90,10) 
xhyp <- cfMake(REP_F4, REP_M4_Data, nscen=length(oa_range))

#set OFFICER_UPPER to vary across range as controls held at means
for(i in 1:length(oa_range)){
  xhyp <- cfChange(xhyp, "OFFICER_UPPER", oa_range[i], scen=i)
}

#simulate expected y's given x's, using point estimates and var-cov matrix specified above
yhyp <- logitsimev(xhyp, simbetas, ci=0.95)
REP_M4_EVs<-data.frame(oa_range, yhyp$pe, yhyp$lower, yhyp$upper)
View(REP_M4_EVs)

#plot with ggplot

Model4.probplot<-ggplot(REP_M4_EVs, aes(x=oa_range))+
  geom_errorbar(aes(ymin=yhyp$lower, ymax=yhyp$upper), width=.1)+
  geom_line(aes(y=yhyp$pe)) +
  labs(y= "PROBABILITY OF REPRESSION") +
  labs(x= "OFFICER SHARE (UPPER BOUND ESTIMATE)")+
  scale_x_continuous(breaks=seq(0, 90, 10), labels=c("0 %", "10 %", "20 %", "30 %", "40%", "50 %", "60 %", "70 %", "80 %", "90 %"))+
  ylim(0,.7)+
  geom_rug(mapping=aes(x=OFFICER_UPPER, y=REPTOTAL), data=REP_M4_Data, sides="b", position="jitter")+
  theme_bw()
Model4.probplot
dev.print(png, "Figure5.png", width=600, height=483)
dev.off()