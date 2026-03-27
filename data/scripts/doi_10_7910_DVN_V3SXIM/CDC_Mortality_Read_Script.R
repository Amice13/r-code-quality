#Code to read CDC Mortality data.
#Author: Ernani F. Choma 

#Please change the working directory path on line 15 before running:
#Line 15 -- Working directory
#Also make sure to check following paths for Inputs and Outputs (i.e., Processed Data for Model):
#Inputs:
#Line 36-39 -- CDC Mortality Data (provided with model)
#Line 43 -- STCOU List file (provided with model). STCOU = FIPS State + County code
#Outputs:
#Lines 285-286 -- RData output files


#Please change path in the line below to the working directory
setwd("~/US_OnRoad_HealthImpacts/Preprocessing_of_Model_Input_Data/CDC_HHS_Mortality/")
#This file reads:
#a) the mortality data from CDC wonder
#   5-year death counts for 2006-2010
#   5-year death counts for 2014-2018
#   both were collected for (i) all-cause/all ages; and (ii) Non-accidental, for each GEMM age group
#   We apply 5-year death rates to 2017 and 2008 population
#   We use county-level data for counts >= 50, 
#   County-level counts are smaller than 50 are not listed. We use state-level rates for these.


#b) race-bridge population used in wonder from HHS [[check]]
#   5-year population counts for 2006-2010
#   1-year population count for 2008
#   5-year population counts for 2014-2018
#   1-year population count for 2017
#   Each was collected for (i) all ages and (ii) for each GEMM age group
#   In each case it was collected for all counties as well as for all states

###### 1. Reading data ######
#Please change file paths in the next 4 lines to location of original CDC data (provided with model)
CountyDeaths <- read.csv("Original_Data_Pre_Processing/CountyDeaths.csv")
CountyPopulation <- read.csv("Original_Data_Pre_Processing/CountyPop.csv")
StateDeaths <- read.csv("Original_Data_Pre_Processing/StateDeaths.csv")
StatePopulation <- read.csv("Original_Data_Pre_Processing/StatePop.csv")

#Updated list of counties
#Please change file path in the next line to location of STCOU file (provided with model). STCOU = FIPS State + County Code
STCOU.List <- read.csv("Auxiliary_Data/STCOUList.csv")
STCOU.List <- as.data.frame(STCOU.List)
names(STCOU.List) <- "STCOU"


###### 2. Calculating State-level mortality rates ######
#These will be used in counties/age groups with counts < 50 

#2.1 All-cause
ST.AllCause <- StateDeaths[StateDeaths$Cause=="All-Cause",]
ST.AllCause.17 <- ST.AllCause[ST.AllCause$Year=="2014-2018",]
names(ST.AllCause.17)
ST.AllCause.17 <- subset(ST.AllCause.17, select=c("ST","AgeGroupCode","Deaths"))
ST.AllCause.08 <- ST.AllCause[ST.AllCause$Year=="2006-2010",]
ST.AllCause.08 <- subset(ST.AllCause.08, select=c("ST","AgeGroupCode","Deaths"))

ST.AllCause.Pop <- StatePopulation[StatePopulation$AgeGroupCode==0,]
ST.AllCause.Pop17 <- ST.AllCause.Pop[ST.AllCause.Pop$Year %in% c("2017","2014-2018"),]
names(ST.AllCause.Pop17)
ST.AllCause.Pop17 <- subset(ST.AllCause.Pop17, select=c("ST","AgeGroupCode","Population","Year"))
ST.AllCause.Pop08 <- ST.AllCause.Pop[ST.AllCause.Pop$Year %in% c("2008","2006-2010"),]
ST.AllCause.Pop08 <- subset(ST.AllCause.Pop08, select=c("ST","AgeGroupCode","Population","Year"))

ST.AllCause.Pop17 <- reshape(ST.AllCause.Pop17, timevar="Year", idvar=c("ST","AgeGroupCode"), direction="wide")
names(ST.AllCause.17)
ST.AllCause.17 <- merge(x=ST.AllCause.17, y=ST.AllCause.Pop17, by=c("ST","AgeGroupCode"))
ST.AllCause.17 <- ST.AllCause.17[order(ST.AllCause.17$ST, decreasing=FALSE),]
ST.AllCause.17$DeathRate.2014_2018 <- ST.AllCause.17$Deaths/ST.AllCause.17$`Population.2014-2018`
ST.AllCause.17 <- subset(ST.AllCause.17, select=c("ST","DeathRate.2014_2018"))

ST.AllCause.Pop08 <- reshape(ST.AllCause.Pop08, timevar="Year", idvar=c("ST","AgeGroupCode"), direction="wide")
names(ST.AllCause.08)
ST.AllCause.08 <- merge(x=ST.AllCause.08, y=ST.AllCause.Pop08, by=c("ST","AgeGroupCode"))
ST.AllCause.08 <- ST.AllCause.08[order(ST.AllCause.08$ST, decreasing=FALSE),]
ST.AllCause.08$DeathRate.2006_2010 <- ST.AllCause.08$Deaths/ST.AllCause.08$`Population.2006-2010`
ST.AllCause.08 <- subset(ST.AllCause.08, select=c("ST","DeathRate.2006_2010"))

#2.2 GEMM Age groups
ST.GEMM <- StateDeaths[StateDeaths$Cause=="Non-Accidental",]
ST.GEMM.17 <- ST.GEMM[ST.GEMM$Year=="2014-2018",]
names(ST.GEMM.17)
ST.GEMM.17 <- subset(ST.GEMM.17, select=c("ST","AgeGroupCode","Deaths"))
ST.GEMM.08 <- ST.GEMM[ST.GEMM$Year=="2006-2010",]
ST.GEMM.08 <- subset(ST.GEMM.08, select=c("ST","AgeGroupCode","Deaths"))

ST.GEMM.Pop <- StatePopulation[StatePopulation$AgeGroupCode!=0,]
ST.GEMM.Pop17 <- ST.GEMM.Pop[ST.GEMM.Pop$Year %in% c("2017","2014-2018"),]
names(ST.GEMM.Pop17)
ST.GEMM.Pop17 <- subset(ST.GEMM.Pop17, select=c("ST","AgeGroupCode","Population","Year"))
ST.GEMM.Pop08 <- ST.GEMM.Pop[ST.GEMM.Pop$Year %in% c("2008","2006-2010"),]
ST.GEMM.Pop08 <- subset(ST.GEMM.Pop08, select=c("ST","AgeGroupCode","Population","Year"))

ST.GEMM.Pop17 <- reshape(ST.GEMM.Pop17, timevar="Year", idvar=c("ST","AgeGroupCode"), direction="wide")
names(ST.GEMM.17)
ST.GEMM.17 <- merge(x=ST.GEMM.17, y=ST.GEMM.Pop17, by=c("ST","AgeGroupCode"))
ST.GEMM.17 <- ST.GEMM.17[order(ST.GEMM.17$ST, decreasing=FALSE),]
ST.GEMM.17$DeathRate.2014_2018 <- ST.GEMM.17$Deaths/ST.GEMM.17$`Population.2014-2018`
names(ST.GEMM.17)
ST.GEMM.17 <- subset(ST.GEMM.17, select=c("ST","AgeGroupCode","DeathRate.2014_2018")) 
#ST.GEMM.17 <- reshape(ST.GEMM.17, timevar="AgeGroupCode", idvar=c("ST"), direction="wide")
#ST.GEMM.17 <- ST.GEMM.17[order(ST.GEMM.17$ST, decreasing=FALSE),]
#names(ST.GEMM.17)
#names(ST.GEMM.17) <- c("ST",paste0(c("Deaths.2017_","Population.2017_"),rep(c(27.5+5*0:10,"85"),each=2)))

ST.GEMM.Pop08 <- reshape(ST.GEMM.Pop08, timevar="Year", idvar=c("ST","AgeGroupCode"), direction="wide")
names(ST.GEMM.08)
ST.GEMM.08 <- merge(x=ST.GEMM.08, y=ST.GEMM.Pop08, by=c("ST","AgeGroupCode"))
ST.GEMM.08 <- ST.GEMM.08[order(ST.GEMM.08$ST, decreasing=FALSE),]
ST.GEMM.08$DeathRate.2006_2010 <- ST.GEMM.08$Deaths/ST.GEMM.08$`Population.2006-2010`
names(ST.GEMM.08)
ST.GEMM.08 <- subset(ST.GEMM.08, select=c("ST","AgeGroupCode","DeathRate.2006_2010")) 


###### 3. Calculating County-level mortality counts######
#3.1 All-cause
C.AllCause <- CountyDeaths[CountyDeaths$Cause=="All-Cause",]
C.AllCause.17 <- C.AllCause[C.AllCause$Year=="2014-2018",]
names(C.AllCause.17)
C.AllCause.17 <- subset(C.AllCause.17, select=c("STCOU","Deaths"))

#Fixing County codes
C.AllCause.17$STCOU[C.AllCause.17$STCOU==46113] <- 46102
C.AllCause.17 <- C.AllCause.17[order(C.AllCause.17$STCOU,decreasing=FALSE),]

C.AllCause.Pop <- CountyPopulation[CountyPopulation$AgeGroupCode==0,]
C.AllCause.Pop.17 <- C.AllCause.Pop[C.AllCause.Pop$Year %in% c("2017"),]
C.AllCause.Pop.17 <- subset(C.AllCause.Pop.17, select=c("STCOU","Population"))
names(C.AllCause.Pop.17) <- c("STCOU","Pop.2017")
C.AllCause.Pop.1418 <- C.AllCause.Pop[C.AllCause.Pop$Year %in% c("2014-2018"),]
C.AllCause.Pop.1418 <- subset(C.AllCause.Pop.1418, select=c("STCOU","Population"))
names(C.AllCause.Pop.1418) <- c("STCOU","Pop.2014_2018")

C.AllCause.17 <- merge(x=C.AllCause.17,y=C.AllCause.Pop.17,by="STCOU",all=TRUE)
C.AllCause.17 <- merge(x=C.AllCause.17,y=C.AllCause.Pop.1418,by="STCOU",all=TRUE)
summary(C.AllCause.17)

C.AllCause.17$Deaths.2017 <- C.AllCause.17$Deaths * (C.AllCause.17$Pop.2017/C.AllCause.17$Pop.2014_2018)
sum(is.na(C.AllCause.17$Deaths.2017))

C.AllCause.17$ST <- C.AllCause.17$STCOU %/% 1000
C.AllCause.17 <- merge(x=C.AllCause.17,y=ST.AllCause.17,by="ST",all=TRUE)

sum(na.omit(C.AllCause.17$Deaths.2017))
C.AllCause.17$Deaths.2017[is.na(C.AllCause.17$Deaths.2017)] <- C.AllCause.17$Pop.2017[is.na(C.AllCause.17$Deaths.2017)] * C.AllCause.17$DeathRate.2014_2018[is.na(C.AllCause.17$Deaths.2017)]
sum(na.omit(C.AllCause.17$Deaths.2017))

C.AllCause.17 <- subset(C.AllCause.17,select=c("STCOU","Deaths.2017"))

C.AllCause <- CountyDeaths[CountyDeaths$Cause=="All-Cause",]
C.AllCause.08 <- C.AllCause[C.AllCause$Year=="2006-2010",]
names(C.AllCause.08)
C.AllCause.08 <- subset(C.AllCause.08, select=c("STCOU","Deaths"))

#Fixing County codes
C.AllCause.08$STCOU[C.AllCause.08$STCOU==46113] <- 46102
C.AllCause.08$Deaths[C.AllCause.08$STCOU==51019] <- C.AllCause.08$Deaths[C.AllCause.08$STCOU==51019] + C.AllCause.08$Deaths[C.AllCause.08$STCOU==51515]
C.AllCause.08 <- C.AllCause.08[C.AllCause.08$STCOU != 51515,]
C.AllCause.08 <- C.AllCause.08[order(C.AllCause.08$STCOU,decreasing=FALSE),]

C.AllCause.Pop <- CountyPopulation[CountyPopulation$AgeGroupCode==0,]
C.AllCause.Pop.08 <- C.AllCause.Pop[C.AllCause.Pop$Year %in% c("2008"),]
C.AllCause.Pop.08 <- subset(C.AllCause.Pop.08, select=c("STCOU","Population"))
names(C.AllCause.Pop.08) <- c("STCOU","Pop.2008")

#Fixing County codes
C.AllCause.Pop.08$Pop.2008[C.AllCause.Pop.08$STCOU==51019] <- C.AllCause.Pop.08$Pop.2008[C.AllCause.Pop.08$STCOU==51019] + C.AllCause.Pop.08$Pop.2008[C.AllCause.Pop.08$STCOU==51515]
C.AllCause.Pop.08 <- C.AllCause.Pop.08[C.AllCause.Pop.08$STCOU != 51515,]
C.AllCause.Pop.08 <- C.AllCause.Pop.08[order(C.AllCause.Pop.08$STCOU,decreasing=FALSE),]

C.AllCause.Pop.0610 <- C.AllCause.Pop[C.AllCause.Pop$Year %in% c("2006-2010"),]
C.AllCause.Pop.0610 <- subset(C.AllCause.Pop.0610, select=c("STCOU","Population"))
names(C.AllCause.Pop.0610) <- c("STCOU","Pop.2006_2010")

C.AllCause.Pop.0610$Pop.2006_2010[C.AllCause.Pop.0610$STCOU==51019] <- C.AllCause.Pop.0610$Pop.2006_2010[C.AllCause.Pop.0610$STCOU==51019] + C.AllCause.Pop.0610$Pop.2006_2010[C.AllCause.Pop.0610$STCOU==51515]
C.AllCause.Pop.0610 <- C.AllCause.Pop.0610[C.AllCause.Pop.0610$STCOU != 51515,]
C.AllCause.Pop.0610 <- C.AllCause.Pop.0610[order(C.AllCause.Pop.0610$STCOU,decreasing=FALSE),]

C.AllCause.08 <- merge(x=C.AllCause.08,y=C.AllCause.Pop.08,by="STCOU",all=TRUE)
C.AllCause.08 <- merge(x=C.AllCause.08,y=C.AllCause.Pop.0610,by="STCOU",all=TRUE)
summary(C.AllCause.08)

C.AllCause.08$Deaths.2008 <- C.AllCause.08$Deaths * (C.AllCause.08$Pop.2008/C.AllCause.08$Pop.2006_2010)
sum(is.na(C.AllCause.08$Deaths.2008))

C.AllCause.08$ST <- C.AllCause.08$STCOU %/% 1000
C.AllCause.08 <- merge(x=C.AllCause.08,y=ST.AllCause.08,by="ST",all=TRUE)

sum(na.omit(C.AllCause.08$Deaths.2008))
C.AllCause.08$Deaths.2008[is.na(C.AllCause.08$Deaths.2008)] <- C.AllCause.08$Pop.2008[is.na(C.AllCause.08$Deaths.2008)] * C.AllCause.08$DeathRate.2006_2010[is.na(C.AllCause.08$Deaths.2008)]
sum(na.omit(C.AllCause.08$Deaths.2008))

C.AllCause.08 <- subset(C.AllCause.08,select=c("STCOU","Deaths.2008"))

#3.2 Non-Accidental for GEMM (by age group)
C.GEMM <- CountyDeaths[CountyDeaths$Cause=="Non-Accidental",]
C.GEMM.17 <- C.GEMM[C.GEMM$Year=="2014-2018",]
names(C.GEMM.17)
C.GEMM.17 <- subset(C.GEMM.17, select=c("STCOU","AgeGroupCode","Deaths"))

#Fixing County codes
C.GEMM.17$STCOU[C.GEMM.17$STCOU==46113] <- 46102
C.GEMM.17 <- C.GEMM.17[order(C.GEMM.17$STCOU,decreasing=FALSE),]

C.GEMM.Pop <- CountyPopulation[CountyPopulation$AgeGroupCode!=0,]
C.GEMM.Pop.17 <- C.GEMM.Pop[C.GEMM.Pop$Year %in% c("2017"),]
C.GEMM.Pop.17 <- subset(C.GEMM.Pop.17, select=c("STCOU","AgeGroupCode","Population"))
names(C.GEMM.Pop.17) <- c("STCOU","AgeGroupCode","Pop.2017")

C.GEMM.Pop.1418 <- C.GEMM.Pop[C.GEMM.Pop$Year %in% c("2014-2018"),]
C.GEMM.Pop.1418 <- subset(C.GEMM.Pop.1418, select=c("STCOU","AgeGroupCode","Population"))
names(C.GEMM.Pop.1418) <- c("STCOU","AgeGroupCode","Pop.2014_2018")

C.GEMM.17 <- merge(x=C.GEMM.17,y=C.GEMM.Pop.17,by=c("STCOU","AgeGroupCode"),all=TRUE)
C.GEMM.17 <- merge(x=C.GEMM.17,y=C.GEMM.Pop.1418,by=c("STCOU","AgeGroupCode"),all=TRUE)
summary(C.GEMM.17)

C.GEMM.17$Deaths.2017 <- C.GEMM.17$Deaths * (C.GEMM.17$Pop.2017/C.GEMM.17$Pop.2014_2018)
sum(is.na(C.GEMM.17$Deaths.2017))

C.GEMM.17$ST <- C.GEMM.17$STCOU %/% 1000
C.GEMM.17 <- merge(x=C.GEMM.17,y=ST.GEMM.17,by=c("ST","AgeGroupCode"),all=TRUE)

sum(na.omit(C.GEMM.17$Deaths.2017))
C.GEMM.17$Deaths.2017[is.na(C.GEMM.17$Deaths.2017)] <- C.GEMM.17$Pop.2017[is.na(C.GEMM.17$Deaths.2017)] * C.GEMM.17$DeathRate.2014_2018[is.na(C.GEMM.17$Deaths.2017)]
sum(na.omit(C.GEMM.17$Deaths.2017))

C.GEMM.17 <- subset(C.GEMM.17,select=c("STCOU","AgeGroupCode","Deaths.2017"))
C.GEMM.17 <- reshape(C.GEMM.17, timevar="AgeGroupCode", idvar=c("STCOU"), direction="wide")
C.GEMM.17 <- C.GEMM.17[order(C.GEMM.17$STCOU,decreasing=FALSE),]


C.GEMM <- CountyDeaths[CountyDeaths$Cause=="Non-Accidental",]
C.GEMM.08 <- C.GEMM[C.GEMM$Year=="2006-2010",]
names(C.GEMM.08)
C.GEMM.08 <- subset(C.GEMM.08, select=c("STCOU","AgeGroupCode","Deaths"))

#Fixing County codes
C.GEMM.08$STCOU[C.GEMM.08$STCOU==46113] <- 46102
C.GEMM.08 <- C.GEMM.08[order(C.GEMM.08$STCOU,decreasing=FALSE),]

C.GEMM.Pop <- CountyPopulation[CountyPopulation$AgeGroupCode!=0,]
C.GEMM.Pop.08 <- C.GEMM.Pop[C.GEMM.Pop$Year %in% c("2008"),]
C.GEMM.Pop.08 <- subset(C.GEMM.Pop.08, select=c("STCOU","AgeGroupCode","Population"))
names(C.GEMM.Pop.08) <- c("STCOU","AgeGroupCode","Pop.2008")


C.GEMM.Pop.0610 <- C.GEMM.Pop[C.GEMM.Pop$Year %in% c("2006-2010"),]
C.GEMM.Pop.0610 <- subset(C.GEMM.Pop.0610, select=c("STCOU","AgeGroupCode","Population"))
names(C.GEMM.Pop.0610) <- c("STCOU","AgeGroupCode","Pop.2006_2010")

C.GEMM.08 <- merge(x=C.GEMM.08,y=C.GEMM.Pop.08,by=c("STCOU","AgeGroupCode"),all=TRUE)
C.GEMM.08 <- merge(x=C.GEMM.08,y=C.GEMM.Pop.0610,by=c("STCOU","AgeGroupCode"),all=TRUE)
summary(C.GEMM.08)

C.GEMM.08$Deaths.2008 <- C.GEMM.08$Deaths * (C.GEMM.08$Pop.2008/C.GEMM.08$Pop.2006_2010)
sum(is.na(C.GEMM.08$Deaths.2008))

C.GEMM.08$ST <- C.GEMM.08$STCOU %/% 1000
C.GEMM.08 <- merge(x=C.GEMM.08,y=ST.GEMM.08,by=c("ST","AgeGroupCode"),all=TRUE)

sum(na.omit(C.GEMM.08$Deaths.2008))
C.GEMM.08$Deaths.2008[is.na(C.GEMM.08$Deaths.2008)] <- C.GEMM.08$Pop.2008[is.na(C.GEMM.08$Deaths.2008)] * C.GEMM.08$DeathRate.2006_2010[is.na(C.GEMM.08$Deaths.2008)]
sum(na.omit(C.GEMM.08$Deaths.2008))

C.GEMM.08 <- subset(C.GEMM.08,select=c("STCOU","AgeGroupCode","Deaths.2008"))
C.GEMM.08 <- reshape(C.GEMM.08, timevar="AgeGroupCode", idvar=c("STCOU"), direction="wide")
C.GEMM.08 <- C.GEMM.08[order(C.GEMM.08$STCOU,decreasing=FALSE),]

#Fixing county codes
C.GEMM.08[C.GEMM.08$STCOU==51019,-1] <- C.GEMM.08[C.GEMM.08$STCOU==51019,-1] + C.GEMM.08[C.GEMM.08$STCOU==51515,-1]
sum(C.GEMM.08[,-1])
C.GEMM.08 <- C.GEMM.08[C.GEMM.08$STCOU != 51515,]
sum(C.GEMM.08[,-1])
C.GEMM.08 <- C.GEMM.08[order(C.GEMM.08$STCOU,decreasing=FALSE),]

Deaths.AllCause.2008 <- C.AllCause.08
Deaths.AllCause.2017 <- C.AllCause.17
Deaths.GEMM.2008 <- C.GEMM.08
Deaths.GEMM.2017 <- C.GEMM.17

###### 4. Writing output files to be used in marginal damages model ######
#Please change file path in the next 2 lines according to desired location to save output
setwd("/Users/Ernani/Documents/US_OnRoad_HealthImpacts_May2021/Preprocessing_of_Model_Input_Data/CDC_HHS_Mortality/")
save(list=c("Deaths.AllCause.2008","Deaths.AllCause.2017","Deaths.GEMM.2008","Deaths.GEMM.2017"), 
     file="Processed_Data_For_Model/Mortality.RData")

