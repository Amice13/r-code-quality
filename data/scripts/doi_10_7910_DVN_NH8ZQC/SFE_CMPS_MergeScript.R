#### Creating the Full Replication Dataset from Originals ###
#Run using RStudio v. 1.0.136 and R v. 3.2.5.

##Load Packages##
library(dplyr)    #v. 0.5.0
library(zoo)      #v. 1.7-12
library(tidyr)    #v. 0.4.1
library(DAMisc)   #v. 1.3

##Load Data##
SFE<-read.csv("SFE_MENA_v1.csv", na.strings=c("-99", "-88"))
EPR<-read.csv("EPR-2014.csv")
MAR2003 <- read.csv("mardatav02.2005.csv", na.strings="-99")
MAR2006<- read.csv("marupdate_20042006.csv", na.strings="-99")
Maddison<-read.csv("maddison.csv", na.strings="")
NMC<-read.csv("NMC_v4_0.csv")
CoupList<-read.csv("SFE_Couplist.csv")


##Prepare SFE Data##

#Convert Period-Level to Year-Level
SFE$timespan<-SFE$TO-SFE$FROM+1
SFE_YEAR<- SFE[rep(row.names(SFE), SFE$timespan), 1:20]
SFE_YEAR<-SFE_YEAR %>%
  group_by(SFE_GROUP_ID, FROM) %>%
  arrange(SFE_GROUP_ID, FROM) %>%
  mutate(PYEAR = seq_along(FROM)) %>%
  mutate(YEAR = FROM + PYEAR -1) %>%
  select(GWID, YEAR, STATENAME:RANK_FILE_UPPER) %>%
  arrange(GWID, YEAR)

#Create Majority Dummies for SFE
SFE_YEAR<-SFE_YEAR %>%
  mutate(OFFICER_MAJORITY=ifelse(OFFICER_LOWER >49, 1,
                                 ifelse(OFFICER_UPPER < 51, 0, NA)))%>%
  mutate(RANK_FILE_MAJORITY=ifelse(RANK_FILE_LOWER >49, 1,
                                   ifelse(RANK_FILE_UPPER < 51, 0, NA)))

##Prepare State-Level GDP and Population Data

#Prepare GDP: 
#make vertical, create log, interpolate and extrapolate, lag
GDP<-Maddison%>%
  mutate(gdppc2011=NA)%>%
  mutate(gdppc2012=NA)%>%
  mutate(gdppc2013=NA)%>%
  gather(year, MAD_GDPPC, gdppc1946:gdppc2013)%>%
  separate(year, into = c("junk", "YEAR"), sep = "pc")%>%
  mutate(GWID=gwid)%>%
  mutate(MAD_GDPPC=as.numeric(gsub(",", "", MAD_GDPPC)))%>%
  group_by(GWID)%>%
  arrange(GWID, YEAR)%>%
  mutate(MAD_GDPPC_LOG=log(MAD_GDPPC))%>%
  mutate(MAD_GDPPC_LOG_LAG=lag(MAD_GDPPC_LOG))%>%
  mutate(MAD_GDPPC_LOG_LAG_INT=na.fill(MAD_GDPPC_LOG_LAG,c("extend","extend","extend"))) #interpolates in middle, extends last known to left and right
 
#Prepare COW State Population:
NMC_Cut<-NMC%>%
  subset(ccode>599 & ccode <700)%>%
  subset(year>1945)%>%
  filter(!(ccode==678 &year==1990))%>%
  mutate(GWID=ifelse(ccode==679, 678, ccode))%>% #correcting diff btw COW and GWID on Yemens
  mutate(NMC_CPOP=tpop*1000)%>%
  mutate(NMC_CPOP_LOG=log(NMC_CPOP))%>%
  mutate(Duplicate=ifelse(year==2007, 7, 1))
  
POP_Exp<-NMC_Cut[rep(row.names(NMC_Cut), NMC_Cut$Duplicate), 1:15]
POP<-POP_Exp%>%
  group_by(GWID)%>%
  arrange(GWID, year)%>%
  mutate(PYEAR=seq_along(year))%>%
  mutate(YEAR=min(year)+PYEAR-1)%>%
  mutate(NMC_CPOP_LOG_LAG=lag(NMC_CPOP_LOG))%>%
  select(GWID, YEAR, NMC_CPOP, NMC_CPOP_LOG, NMC_CPOP_LOG_LAG)%>%
  mutate(NMC_CPOP_LOG_LAG_INT=na.fill(NMC_CPOP_LOG_LAG,c("extend","extend","extend"))) #interpolates in middle, extends last known to left and right

         

##Prepare EPR Data##

#Convert Period-Level to Year-Level
EPR$timespan<-EPR$to-EPR$from+1
EPR_YEAR<- EPR[rep(row.names(EPR), EPR$timespan), 1:12]
EPR_YEAR<-EPR_YEAR %>%
  subset(gwid>599 & gwid <700)%>%
  group_by(gwgroupid, from) %>%
  arrange(gwgroupid, from) %>%
  mutate(pyear = seq_along(from)) %>%
  mutate(YEAR = from + pyear -1) %>%
  rename(EPR_GROUP_ID=gwgroupid) %>%
  rename(EPR_GROUP=group) %>%
  rename(EPR_GROUP_SIZE=size) %>%
  rename(EPR_STATUS=status) %>%
  arrange(gwid, YEAR)

#Create Variables for EPR 
#(Irrelevant Dummy, Ordinal Status, Included Dummy, Senior Plus Dummy, Number of Groups, and Center Segmentation)
 EPR_YEAR<-EPR_YEAR%>% 
   group_by(gwid, YEAR)%>%
   mutate(EPR_GROUPNUM=n())%>%
   mutate(EPR_ORDINAL=ifelse(EPR_STATUS=="DISCRIMINATED",1,
                            ifelse(EPR_STATUS=="POWERLESS",2,
                                   ifelse (EPR_STATUS=="JUNIOR PARTNER",3,
                                           ifelse (EPR_STATUS=="SENIOR PARTNER",4,
                                                   ifelse(EPR_STATUS=="DOMINANT",5,
                                                     ifelse (EPR_STATUS=="MONOPOLY",6,NA)))))))%>%
   mutate(EPR_INCLUDED=ifelse(EPR_GROUPNUM ==1,1,
                              ifelse(EPR_STATUS=="IRRELEVANT", 0,
                                ifelse(EPR_ORDINAL >2, 1,
                                  ifelse(EPR_ORDINAL < 3, 0, NA)))))%>%
   
    mutate(EPR_SRPLUS=ifelse(EPR_GROUPNUM ==1,1,
                             ifelse(EPR_STATUS=="IRRELEVANT", 0,
                               ifelse(EPR_ORDINAL >3, 1,
                                 ifelse(EPR_ORDINAL < 4 | EPR_STATUS=="IRRELEVANT", 0, NA)))))%>%
   mutate(EPR_CENTERSEG=sum(EPR_INCLUDED))%>%
   mutate(EPR_GROUP_SIZE_LOG=log(EPR_GROUP_SIZE))
  
##Add Coup Data to EPR##

 CoupYear<-CoupList%>%
   subset(!is.na(EPR_Initiator))%>%
   group_by(EPR_Initiator, Year) %>%
   filter(row_number() == 1) %>%
   mutate(EPR_GROUP_ID=EPR_Initiator)%>%
   mutate(YEAR=Year)%>%
   ungroup()%>%
   select(YEAR, EPR_GROUP_ID, SPEED_MIL_ATTEMPT)
 
 EPR_COUPS<-merge(EPR_YEAR, CoupYear, by=c("YEAR", "EPR_GROUP_ID"), all.x=TRUE)
 
 EPR_COUPS<-EPR_COUPS%>%
  mutate(SPEED_COUP_ATTEMPT=ifelse(is.na(SPEED_MIL_ATTEMPT)==TRUE, 0,1))

 #Create Time Series Cubits for SPEED Coup Attempts 
EPR_COUPS_BTSCS<-btscs(EPR_COUPS, "SPEED_COUP_ATTEMPT","YEAR", "EPR_GROUP_ID")

EPR_MERGE<-EPR_COUPS_BTSCS%>%
  mutate(COUP_SPELL=spell)%>%
  mutate(COUP_SPELL_2=spell^2)%>%
  mutate(COUP_SPELL_3=spell^3)%>%
  select(YEAR, EPR_GROUP_ID, EPR_GROUP_SIZE, EPR_STATUS, EPR_GROUPNUM:EPR_GROUP_SIZE_LOG, SPEED_COUP_ATTEMPT, COUP_SPELL:COUP_SPELL_3)%>%
  arrange(EPR_GROUP_ID,YEAR)

  

  
##Prepare MAR Data##

#MAR 2003: load, limit to ME and variables of interest, prepare to bind
MAR2003<-MAR2003 %>%
  subset(ccode>599 & ccode<700) %>%
  select(numcode, group, ccode, country, year, gpop, cpop, poldis, rep18, rep19, rep20, rep21, rep22, rep23) %>%
  mutate(gpop=gpop*1000) %>%
  mutate(cpop=cpop*1000)
MAR2003$repgenciv<-MAR2003$repnviol<-MAR2003$repviol<-NA


#MAR 2004-2006: load, limit to ME and variables of interest, prepare to bind
MAR2006<-MAR2006 %>%
  subset(ccode>599 & ccode <700)%>%
  select(numcode, group=VMAR_Group, ccode, country, year, gpop=GPOP, cpop=CPOP, poldis=POLDIS, repgenciv=REPGENCIV, repnviol=REPNVIOL, repviol=REPVIOL)

MAR2006$cpop[MAR2006$ccode == 663] <- NA #remove clearly wrong population figures for Jordan
MAR2006$gpop[MAR2006$ccode == 663] <- NA #remove clearly wrong population figures for Jordan
MAR2006$row.names<-NULL
MAR2006$rep18<-MAR2006$rep19<-MAR2006$rep20<-MAR2006$rep21<-MAR2006$rep21<-MAR2006$rep22<-MAR2006$rep23<-NA

#Merge new and old MAR data
MAR2003<-MAR2003[!(MAR2003$numcode==65202 & MAR2003$year==2004),] #remove Syrian Kurds 2004 from old MAR as duplicative
MAR2003<-MAR2003[!(MAR2003$numcode==62504 & MAR2003$year==2004),] #remove Darfur Black Muslims 2004 from old MAR as duplicative
MAR<-rbind(MAR2003, MAR2006)

#Linear Interpolation of missing population values

MAR<-MAR[!(MAR$numcode==64006),] # remove Roma in Turkey as no group data available

MAR<-MAR %>%
  group_by(numcode) %>%
  arrange(numcode, year) %>%
  mutate(gpop_int= na.fill(gpop, c("extend","extend","extend")))%>%
  mutate(cpop_int= na.fill(cpop, c("extend","extend","extend")))%>%
  #Create lagged variables for gpop, cpop, and poldis
  mutate(gpop_lag = lag(gpop_int)) %>%
  mutate(cpop_lag = lag(cpop_int)) %>%
  mutate(poldis_lag=lag(poldis))%>%
  #re-Create gpro based on interpolated and lagged data
  mutate(gpro_lag=(gpop_lag/cpop_lag))%>%
  #Natural Log Transformations of lagged gpop, cpop, gpro
  mutate(log_gpop_lag=log(gpop_lag))%>%
  mutate(log_cpop_lag=log(cpop_lag))%>%
  mutate(log_gpro_lag=log(gpro_lag))

#Recoding Repression DV# Creates a dummy DV of 1 if any of 5 MAR variables indicates use of armed repression against ethnic group
MAR<-MAR[!(MAR$year<1996),] #remove years prior to when repression variables are collected
MAR$reptotal<-ifelse(MAR$repnviol %in% c(4, 5), 1,
                     ifelse(MAR$repviol %in% c(4, 5), 1,
                            ifelse(MAR$repgenciv %in% c(4, 5), 1,
                                   ifelse(MAR$rep18 %in% c(1,2,3),1,
                                          ifelse(MAR$rep19 %in% c(1,2,3),1,
                                                 ifelse(MAR$rep20 %in% c(1,2,3),1,
                                                        ifelse(MAR$rep21 %in% c(1,2,3),1,
                                                               ifelse(MAR$rep22 %in% c(1,2,3),1,0))))))))

#Make Time-Series Cubits for Repression of Group
#Saving and reloading to avoid buggy interaction btwn dplyr and btscs
write.csv(MAR, "MAR_Merged.csv")
MAR<-read.csv("MAR_Merged.csv")
  
MAR_BTSCS<-btscs(MAR, "reptotal", "year", "numcode")
MAR_MERGE<-MAR_BTSCS%>%
  mutate(REP_SPELL=spell)%>%
  mutate(REP_SPELL_2=spell^2)%>%
  mutate(REP_SPELL_3=spell^3)%>%
  select(YEAR=year, MAR_GROUP_ID=numcode, MAR_GROUP=group, MAR_POLDIS_LAG=poldis_lag, MAR_GROUPSIZE_LOG_LAG=log_gpro_lag, 
         MAR_STATEPOP_LOG_LAG=log_cpop_lag, REPTOTAL=reptotal, REP_SPELL:REP_SPELL_3)%>%
  arrange(MAR_GROUP_ID, YEAR)



##Merge Into Final Replication Dataset##

#Merge GDP and POP into SFE
SFE_GDP<-merge(SFE_YEAR, GDP, by=c("GWID", "YEAR"), all.x=TRUE)
SFE_GDP_POP<-merge(SFE_GDP, POP, by=c("GWID", "YEAR"), all.x=TRUE)

#Merge EPR with coups into SFE

SFE_EPR<-merge(SFE_GDP_POP, EPR_MERGE, by=c("EPR_GROUP_ID", "YEAR"), all.x=TRUE)

#Merge Mar into SFE with EPR

SFE_EPR_MAR<-merge(SFE_EPR, MAR_MERGE, by=c("MAR_GROUP_ID", "YEAR"), all.x=TRUE)

#Clean Up

SFE_CMPS_REPDATA<-SFE_EPR_MAR%>%
  group_by(SFE_GROUP_ID)%>%
  arrange(SFE_GROUP_ID, YEAR)%>%
  select(YEAR, GWID:DIVISION, EPR_GROUP_ID, MAR_GROUP_ID, OFFICER_RELATIVE:RANK_FILE_MAJORITY, MAD_GDPPC:REP_SPELL_3)%>%
  arrange(GWID, YEAR, SFE_GROUP_ID)

###Export Replication Dataset for CMPS Article###
write.csv(SFE_CMPS_REPDATA, "SFE_CMPS_Replication_Dataset.csv")
