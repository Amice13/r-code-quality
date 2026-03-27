#Code to process NEI 2011 emissions
#Author: Ernani F. Choma 

#Please change the working directory path on line 17 before running:
#Line 17 -- Working directory
#Also make sure to check following paths for Inputs and Outputs (i.e., Processed Data for Model):
#Inputs:
#Line 39-43 -- NEI On-Road emissions files (not provided with model/needs to be downloaded)
#Line 48-49 -- NEI On-Road supporting data -- VMT file (not provided with model/needs to be downloaded)
#Line 104 -- STCOU List file (provided with model). STCOU = FIPS State + County code
#Outputs:
#Line 203 -- RData output file



#Please change path in the next line to work directory
setwd("~/US_OnRoad_HealthImpacts/Preprocessing_of_Model_Input_Data/NEI/")
#This file reads the original NEI 2011 data and processes it in the format that is used in the model 
#The Original data is not provided but can be downloaded from the links below

#On Road Data -- Emissions:
#US EPA -- NEI 2011
#https://gaftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_onroad_byregions.zip
#Version: Last Modified 02/01/18
#Accessed 04/30/21

#On Road Supplemental Files: VMT
#US EPA -- NEI 2011
#https://gaftp.epa.gov/air/nei/2011/doc/2011v2_supportingdata/onroad/2011neiv2_supdata_or_VMT.zip
#Version: Last Modified 02/01/18
#This supplemental dataset has many files. The file used was:
#File used: VMT_NEI_v2_2011_no_E85_30sep2014_v1.csv
#Accessed 04/30/21


###### 1. Loading Main NEI 2011 Mobile Source Datasets ######
#US is split in 10 regions, hence numbers 1-10
#Please change file path in the next 5 lines according to location of NEI data (needs to be downloaded -- see above)
OnRoad123 <- read.csv("Original_Data_Pre_Processing/NEI_2011/2011neiv2_onroad_byregions/onroad_123.csv")
OnRoad4 <- read.csv("Original_Data_Pre_Processing/NEI_2011/2011neiv2_onroad_byregions/onroad_4.csv")
OnRoad5 <- read.csv("Original_Data_Pre_Processing/NEI_2011/2011neiv2_onroad_byregions/onroad_5.csv")
OnRoad67 <- read.csv("Original_Data_Pre_Processing/NEI_2011/2011neiv2_onroad_byregions/onroad_67.csv")
OnRoad8910 <- read.csv("Original_Data_Pre_Processing/NEI_2011/2011neiv2_onroad_byregions/onroad_8910.csv")

###### 2. Loading Supplemental Files for VMT ######
#VMT: The data starts on line 34 and the header is on line 29.
#Please change file path in the next 2 lines according to location of NEI supplemental VMT data (needs to be downloaded -- see above)
VMT <- read.csv("Original_Data_Pre_Processing/NEI_2011/VMT_NEI_v2_2011_no_E85_30sep2014_v1.csv",skip=33, header=F)
VMTh <- read.csv("Original_Data_Pre_Processing/NEI_2011/VMT_NEI_v2_2011_no_E85_30sep2014_v1.csv",skip=28, header=F,nrows=1, colClasses=c(rep("character",26)))

names(VMT) <- VMTh

VMT <- subset(VMT, select=c("region_cd","scc","ann_value"))
names(VMT) <- c("STCOU","SCC","VMT")

VMT$VTYPE <- (VMT$SCC %/% 1e4) %% 1e2

###### 3. Filtering and aggregating emissions ######
OnRoad <- rbind(OnRoad123,OnRoad4,OnRoad5,OnRoad67,OnRoad8910)
rm(list=ls()[! ls() %in% c("OnRoad","VMT")])

pollutants <- c("PM25-PRI","SO2","NOX","NH3","VOC","CO2","CH4","N2O")

OnRoad <- OnRoad[OnRoad$pollutant_cd %in% pollutants,]
table(OnRoad$uom) #all TON (i.e. short ton)

OnRoad <- subset(OnRoad, select=c("state_and_county_fips_code",
                                  "scc",
                                  "pollutant_cd",
                                  "total_emissions"))
names(OnRoad) <- c("STCOU","SCC","POLCD","EMIS")
OnRoad$VTYPE <- (OnRoad$SCC %/% 1e4) %% 1e2
table(OnRoad$VTYPE)

#Fixing FIPS state code to match codes for 2017
OnRoad$STCOU[OnRoad$STCOU==46113] <- 46102
OnRoad$STCOU[OnRoad$STCOU==51515] <- 51019

VMT$STCOU[VMT$STCOU==46113] <- 46102
VMT$STCOU[VMT$STCOU==51515] <- 51019


OnRoad <- aggregate(OnRoad$EMIS, by=list(STCOU=OnRoad$STCOU, VTYPE=OnRoad$VTYPE, POLCD=OnRoad$POLCD), FUN=sum)
names(OnRoad)[4] <- "EMIS"

OnRoad <- reshape(OnRoad, timevar="POLCD", idvar=c("STCOU","VTYPE"), direction="wide")
names(OnRoad)[8] <- "EMIS.PM25PRI"

VMT <- aggregate(VMT$VMT, by=list(STCOU=VMT$STCOU, VTYPE=VMT$VTYPE), FUN=sum)
names(VMT)[3] <- "VMT"

#Filtering contiguous U.S., i.e. Removing AK, HI, PR & VI 
VMT <- VMT[!VMT$STCOU %/% 1000 %in% c(2,15,72,78),]
OnRoad <- OnRoad[!OnRoad$STCOU %/% 1000 %in% c(2,15,72,78),]

OnRoad[is.na(OnRoad)] <- 0

NEI2011 <- merge(x=OnRoad, y=VMT, by=c("STCOU","VTYPE"), all=TRUE)
NEI2011[is.na(NEI2011)] <- 0

rm(list=ls()[! ls() %in% c("NEI2011","pollutants")])

#Please change file path in the next line according to location of STCOU (FIPS State+County codes) file (provided with model)
STCOU.List <- read.csv("Auxiliary/STCOUList.csv")
names(STCOU.List) <- "STCOU"

names(NEI2011)
NEI2011 <- subset(NEI2011, select=c("STCOU","VTYPE","VMT",
                                         "EMIS.PM25PRI","EMIS.SO2","EMIS.NOX","EMIS.NH3","EMIS.VOC",
                                         "EMIS.CO2","EMIS.CH4","EMIS.N2O"))

NEI2011.Ref <- NEI2011[NEI2011$VTYPE==0,]
summary(NEI2011.Ref)
NEI2011.Ref <- subset(NEI2011.Ref, select=c("STCOU","EMIS.VOC"))


NEI2011.VTYPE <- array(rep(-1,3108*11*14),dim=c(3108,11,14))
MOVES.VTYPE <- unique(NEI2011$VTYPE)
MOVES.VTYPE <- MOVES.VTYPE[MOVES.VTYPE!=0] #Excluding refueling
MOVES.VTYPE <- MOVES.VTYPE[order(MOVES.VTYPE,decreasing=FALSE)]

pollutants[1] <- "PM25PRI"
ordercols <- c("STCOU","VTYPE","VMT",paste0("EMIS.",pollutants))


for (i in 1:length(MOVES.VTYPE))
{
  NEI.filter <- NEI2011[NEI2011$VTYPE==MOVES.VTYPE[i],]
  NEI.filter <- NEI.filter[order(NEI.filter$STCOU,decreasing=FALSE),]
  NEI.filter <- subset(NEI.filter, select=ordercols)
  NEI.filter <- merge(x=NEI.filter,y=STCOU.List,by="STCOU",all=TRUE)
  print(sum(is.na(NEI.filter)))
  NEI.filter[is.na(NEI.filter)] <- 0
  NEI2011.VTYPE[,,i] <- as.matrix(NEI.filter)
}

NEI.filter <- NEI2011
#NEI.filter <- NEI.filter[NEI.filter$VTYPE != 0,]
NEI.filter$VTYPE <- 99 #All vehicles
NEI.filter <- aggregate(list(VMT=NEI.filter$VMT,
                             EMIS.PM25PRI=NEI.filter$EMIS.PM25PRI,
                             EMIS.SO2=NEI.filter$EMIS.SO2,
                             EMIS.NOX=NEI.filter$EMIS.NOX,
                             EMIS.NH3=NEI.filter$EMIS.NH3,
                             EMIS.VOC=NEI.filter$EMIS.VOC,
                             EMIS.CO2=NEI.filter$EMIS.CO2,
                             EMIS.CH4=NEI.filter$EMIS.CH4,
                             EMIS.N2O=NEI.filter$EMIS.N2O),
                        by=list(STCOU=NEI.filter$STCOU, VTYPE=NEI.filter$VTYPE),
                        FUN=sum)
NEI.filter <- merge(x=NEI.filter,y=STCOU.List,by="STCOU",all=TRUE)
print(sum(is.na(NEI.filter)))
NEI.filter[is.na(NEI.filter)] <- 0
NEI.filter <- NEI.filter[order(NEI.filter$STCOU, decreasing=FALSE),]
names(NEI.filter)
NEI.filter <- subset(NEI.filter, select=ordercols)
NEI2011.VTYPE[,,14] <- as.matrix(NEI.filter)
for (i in 1:14)
{
  NEI2011.VTYPE[,2,i] <- (c(MOVES.VTYPE,99))[i]
}



ShortTon.grams <- 2000*1000*0.45359237
CA.Counties <- which(NEI2011.VTYPE[,1,1] %/% 1000 == 6)

NEI2011.VTYPE.EMIS <- NEI2011.VTYPE
for (vt in 1:14)
{
  CA.Mean.EF <- colSums(NEI2011.VTYPE[CA.Counties,4:11,vt])/sum(NEI2011.VTYPE[CA.Counties,3,vt]) * ShortTon.grams #CA GHGs will be dealt with later
  US.Mean.EF <- colSums(NEI2011.VTYPE[-CA.Counties,4:11,vt])/sum(NEI2011.VTYPE[-CA.Counties,3,vt]) * ShortTon.grams
  NEI2011.VTYPE[,4:11,vt] <- (NEI2011.VTYPE[,4:11,vt]/NEI2011.VTYPE[,3,vt]) * ShortTon.grams
  for (pol in 4:11)
  {
    NEI2011.VTYPE[CA.Counties,pol,vt][NEI2011.VTYPE[CA.Counties,pol,vt]==0] <- CA.Mean.EF[pol-3]
    NEI2011.VTYPE[-CA.Counties,pol,vt][NEI2011.VTYPE[-CA.Counties,pol,vt]==0] <- US.Mean.EF[pol-3]
    NEI2011.VTYPE[CA.Counties,pol,vt][is.na(NEI2011.VTYPE[CA.Counties,pol,vt])] <- CA.Mean.EF[pol-3]
    NEI2011.VTYPE[-CA.Counties,pol,vt][is.na(NEI2011.VTYPE[-CA.Counties,pol,vt])] <- US.Mean.EF[pol-3]
    NEI2011.VTYPE[CA.Counties,pol,vt][NEI2011.VTYPE[CA.Counties,pol,vt]=="Inf"] <- CA.Mean.EF[pol-3]
    NEI2011.VTYPE[-CA.Counties,pol,vt][NEI2011.VTYPE[-CA.Counties,pol,vt]=="Inf"] <- US.Mean.EF[pol-3]
  }
}

aux.VMT <- as.data.frame(NEI2011.VTYPE.EMIS[,c(1,3),14])
names(aux.VMT) <- c("STCOU","VMT")
NEI2011.Ref <- merge(x=NEI2011.Ref,y=aux.VMT,by="STCOU",all.x=TRUE,all.y=FALSE)
NEI2011.Ref$EF.VOC <- (NEI2011.Ref$EMIS.VOC/NEI2011.Ref$VMT) * ShortTon.grams


NEI2011.Ref$VTYPE <- 0

NEI2011.VTYPE.EF <- NEI2011.VTYPE
NEI2011.REF.EMIS <- NEI2011.Ref[,c(1,5,3,2,4)]

sum(NEI2011.REF.EMIS$STCOU != NEI2011.VTYPE.EF[,1,14])
sum(NEI2011.REF.EMIS$STCOU == NEI2011.VTYPE.EF[,1,14])

rm(NEI2011.VTYPE)
rm(NEI2011.Ref)

#Please change file path in the next line according to desired location to save output
save(list=c("NEI2011.VTYPE.EF","NEI2011.VTYPE.EMIS","NEI2011.REF.EMIS"),file="Processed_Data_For_Model/NEI2011.RData")
