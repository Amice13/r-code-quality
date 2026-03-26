#Code to process NEI 2008 emissions
#Author: Ernani F. Choma 

#Please change the working directory path on line 18 before running:
#Line 18 -- Working directory
#Also make sure to check following paths for Inputs and Outputs (i.e., Processed Data for Model):
#Inputs:
#Line 40-44 -- NEI On-Road emissions files (not provided with model/needs to be downloaded)
#Line 49 -- NEI On-Road supporting data -- VMT file (not provided with mode/needs to be downloaded)
#Line 120 -- STCOU List file (provided with model). STCOU = FIPS State + County code
#Line 127 -- EPA SCC Conversion Table file (provided with model)
#Line 192 -- CARB GHG emissions file (provided with model)
#Outputs:
#Line 268 -- RData output file


#Please change path in the next line to work directory
setwd("~/US_OnRoad_HealthImpacts/Preprocessing_of_Model_Input_Data/NEI/")
#This file reads the original NEI 2008 data and processes it in the format that is used in the model
#The Original data is not provided but can be downloaded from the links below

#On Road Data -- Emissions:
#US EPA -- NEI 2008
#https://gaftp.epa.gov/air/nei/2008/data_summaries/2008neiv3_onroad_byregions.zip
#Version: Last Modified 01/31/18
#Accessed 04/30/21

#On Road Supplemental Files: VMT
#US EPA -- NEI 2008
#https://gaftp.epa.gov/air/nei/2008/doc/2008v3_supportingdata/2008nei_supdata_4c.zip
#Version: Last Modified 01/31/18
#This supplemental dataset has many files. The file used was:
#File used: "activity_data_2008.zip"
#Accessed 04/30/21


###### 1. Loading 2008 Emissions ######
#US is split in 10 regions, hence numbers 1-10
#Please change file path in the next 5 lines according to location of NEI data (needs to be downloaded -- see above)
OnRoad123 <- read.csv("Original_Data_Pre_Processing/NEI_2008/2008neiv3_onroad_byregions/2008NEIv3_onroad123.csv")
OnRoad4 <- read.csv("Original_Data_Pre_Processing/NEI_2008/2008neiv3_onroad_byregions/2008NEIv3_onroad4.csv")
OnRoad5 <- read.csv("Original_Data_Pre_Processing/NEI_2008/2008neiv3_onroad_byregions/2008NEIv3_onroad5.csv")
OnRoad67 <- read.csv("Original_Data_Pre_Processing/NEI_2008/2008neiv3_onroad_byregions/2008NEIv3_onroad67.csv")
OnRoad8910 <- read.csv("Original_Data_Pre_Processing/NEI_2008/2008neiv3_onroad_byregions/2008NEIv3_onroad8910.csv")


###### 2. Loading Supplemental Files for VMT  ######
#Please change file path in the next line according to location of NEI supplemental VMT data (needs to be downloaded -- see above)
VMT <- read.table("Original_Data_Pre_Processing/NEI_2008/section4-mobile/onroad/activity_data_2008/VMT_NEI_2008_updated2_18jan2012_v3",sep=",")
#Fips state + county is column 2
#SCC is column 6
#Annual VMT is column 10
VMT <- VMT[,c(2,6,10)]
names(VMT) <- c("STCOU","SCC","VMT")

###### 3. Filtering and aggregating emissions ######
OnRoad <- rbind(OnRoad123,OnRoad4,OnRoad5,OnRoad67,OnRoad8910)
rm(list=ls()[!ls() %in% c("OnRoad","VMT")])

VMT$VTYPE <- VMT$SCC %/% 1e3

pollutants <- c("PM25-PRI","SO2","NOX","NH3","VOC","CO2","CH4","N2O")
OnRoad <- OnRoad[OnRoad$pollutant_cd %in% pollutants,]

table(OnRoad$uom) #Short ton

OnRoad <- subset(OnRoad, select=c("state_and_county_fips_code",
                                  "scc",
                                  "pollutant_cd",
                                  "total_emissions",
                                  "emissions_type_code"))
names(OnRoad) <- c("STCOU","SCC","POLCD","EMIS","ETC")

OnRoad$VTYPE <- OnRoad$SCC %/% 1e3

str(VMT)
sum(is.na(VMT))
str(OnRoad)
sum(is.na(OnRoad))


#Fixing FIPS state code to match codes for 2017
OnRoad$STCOU[OnRoad$STCOU==46113] <- 46102
OnRoad$STCOU[OnRoad$STCOU==51515] <- 51019

VMT$STCOU[VMT$STCOU==46113] <- 46102
VMT$STCOU[VMT$STCOU==51515] <- 51019

#Filtering contiguous U.S., i.e. Removing AK, HI, PR & VI 
VMT <- VMT[!VMT$STCOU %/% 1000 %in% c(2,15,72,78),]
OnRoad <- OnRoad[!OnRoad$STCOU %/% 1000 %in% c(2,15,72,78),]

#Aggregating by county, vehicle type, and pollutant
OnRoad <- aggregate(OnRoad$EMIS, by=list(STCOU=OnRoad$STCOU, VTYPE=OnRoad$VTYPE, POLCD=OnRoad$POLCD), FUN=sum)
names(OnRoad)[4] <- "EMIS"

OnRoad <- reshape(OnRoad, timevar="POLCD", idvar=c("STCOU","VTYPE"), direction="wide")
names(OnRoad)[8] <- "EMIS.PM25PRI"

VMT <- aggregate(VMT$VMT, by=list(STCOU=VMT$STCOU, VTYPE=VMT$VTYPE), FUN=sum)
names(VMT)[3] <- "VMT"

NEI2008 <- merge(x=OnRoad, y=VMT, by=c("STCOU","VTYPE"), all=TRUE)
# 
# sum(is.na(NEI2008))
# sum(is.na(NEI2008$VMT))
# NEI2008[is.na(NEI2008$VMT),]
# summary(NEI2008)

#Assigning 0 to everything that is missing.
NEI2008[is.na(NEI2008)] <- 0

rm(list=ls()[! ls() %in% c("NEI2008","pollutants")])

NEI2008 <- subset(NEI2008, select=c("STCOU","VTYPE","VMT",
                                         "EMIS.PM25PRI","EMIS.SO2","EMIS.NOX","EMIS.NH3","EMIS.VOC",
                                         "EMIS.CO2","EMIS.CH4","EMIS.N2O"))

#Please change file path in the next line according to location of STCOU (FIPS State+County codes) file (provided with model)
STCOU.List <- read.csv("Auxiliary/STCOUList.csv")
names(STCOU.List) <- "STCOU"


#Reading SCC conversion to new MOVES source types

#Please change file path in the next line according to location of SCC Conversion file (provided with model)
SCC.Conv <- read.csv("Original_Data_Pre_Processing/Auxiliary/SCC_Conversion.csv")
#Source: https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100O7PS.pdf (accessed 12/03/20)
#Table 21-2 (page 125)
SCC.Conv <- subset(SCC.Conv, select=c("SCC","MOVES_VTYPE","ALLOC_2011"))

NEI2008.VTYPE <- array(rep(-1,3108*11*14),dim=c(3108,11,14))

MOVES.VTYPE <- unique(SCC.Conv$MOVES_VTYPE)
MOVES.VTYPE <- MOVES.VTYPE[order(MOVES.VTYPE, decreasing=FALSE)]

pollutants[1] <- "PM25PRI"
ordercols <- c("STCOU","VTYPE","VMT",paste0("EMIS.",pollutants))

for (i in 1:length(MOVES.VTYPE))
{
  scc <- SCC.Conv[SCC.Conv$MOVES_VTYPE==MOVES.VTYPE[i],]
  NEI.filter <- NEI2008
  NEI.filter <- merge(x=NEI.filter,y=scc,by.x="VTYPE",by.y="SCC",all.x=FALSE,all.y=TRUE)
  emis <- which(names(NEI.filter) %in% c(paste0("EMIS.",pollutants),"VMT"))
  NEI.filter[,emis] <- NEI.filter[,emis] * NEI.filter$ALLOC_2011
  NEI.filter <- aggregate(list(VMT=NEI.filter$VMT,
                         EMIS.PM25PRI=NEI.filter$EMIS.PM25PRI,
                         EMIS.SO2=NEI.filter$EMIS.SO2,
                         EMIS.NOX=NEI.filter$EMIS.NOX,
                         EMIS.NH3=NEI.filter$EMIS.NH3,
                         EMIS.VOC=NEI.filter$EMIS.VOC,
                         EMIS.CO2=NEI.filter$EMIS.CO2,
                         EMIS.CH4=NEI.filter$EMIS.CH4,
                         EMIS.N2O=NEI.filter$EMIS.N2O),
                    by=list(STCOU=NEI.filter$STCOU, VTYPE=NEI.filter$MOVES_VTYPE),
                    FUN=sum)
  NEI.filter <- merge(x=NEI.filter,y=STCOU.List,by="STCOU",all=TRUE)
  print(sum(is.na(NEI.filter)))
  NEI.filter[is.na(NEI.filter)] <- 0
  NEI.filter <- NEI.filter[order(NEI.filter$STCOU, decreasing=FALSE),]
  NEI.filter <- subset(NEI.filter, select=ordercols)
  NEI2008.VTYPE[,,i] <- as.matrix(NEI.filter)
}

NEI.filter <- NEI2008
NEI.filter$MOVES_VTYPE <- 99 #All vehicles
NEI.filter <- aggregate(list(VMT=NEI.filter$VMT,
                             EMIS.PM25PRI=NEI.filter$EMIS.PM25PRI,
                             EMIS.SO2=NEI.filter$EMIS.SO2,
                             EMIS.NOX=NEI.filter$EMIS.NOX,
                             EMIS.NH3=NEI.filter$EMIS.NH3,
                             EMIS.VOC=NEI.filter$EMIS.VOC,
                             EMIS.CO2=NEI.filter$EMIS.CO2,
                             EMIS.CH4=NEI.filter$EMIS.CH4,
                             EMIS.N2O=NEI.filter$EMIS.N2O),
                        by=list(STCOU=NEI.filter$STCOU, VTYPE=NEI.filter$MOVES_VTYPE),
                        FUN=sum)
NEI.filter <- merge(x=NEI.filter,y=STCOU.List,by="STCOU",all=TRUE)
print(sum(is.na(NEI.filter)))
NEI.filter[is.na(NEI.filter)] <- 0
NEI.filter <- NEI.filter[order(NEI.filter$STCOU, decreasing=FALSE),]
NEI.filter <- subset(NEI.filter, select=ordercols)
NEI2008.VTYPE[,,14] <- as.matrix(NEI.filter)
for (i in 1:14)
{
  NEI2008.VTYPE[,2,i] <- (c(MOVES.VTYPE,99))[i]
}

#Supplementing CA GHG emissions with CARB Data
#Please change file path in the next line according to location of CARB GHG data file (provided with model)
CARBGHG <- read.csv("Original_Data_Pre_Processing/Auxiliary/CARB_GHG_OnRoad.csv", skip=1)

dim(NEI2008.VTYPE)
ShortTon.grams <- 2000*1000*0.45359237
CA.Counties <- which(NEI2008.VTYPE[,1,1] %/% 1000 == 6)
CA.VMT <- NEI2008.VTYPE[CA.Counties,3,1:13]
CA.MOT.VMT <- CA.VMT[,1]
CA.LDV.VMT <- CA.VMT[,2:4]
CA.BUS.VMT <- CA.VMT[,5:7]
CA.HDT.VMT <- CA.VMT[,8:13]

CARBGHG$VClass <- "NA"
CARBGHG$VClass[CARBGHG$Sector.Level.3=="Light-duty Vehicles"] <- "LDV"
CARBGHG$VClass[CARBGHG$Sector.Level.3=="Heavy-duty Vehicles"] <- "HDT"
CARBGHG$VClass[CARBGHG$Sector.Level.4=="Motorcycles"] <- "MOT"
CARBGHG$VClass[CARBGHG$Sector.Level.4=="Buses"] <- "BUS"
CARB.GHG.2008 <- aggregate(CARBGHG$X2008, by=list(CARBGHG$VClass,CARBGHG$GHG),FUN=sum)
names(CARB.GHG.2008) <- c("VClass","GHG","Emissions")
CARB.GHG.2008 <- CARB.GHG.2008[CARB.GHG.2008$VClass != "NA",]
#These are in MT (Tg) of CO2-eq, so converting to Short Tons:
#Emissions * 1e12 [g/Tg] * (1/(2000*1000*0.4536)) [short tons/g]
#Then divide by 298 to get Short tons for N2O, and by 25 to get short tons of CH4
CARB.GHG.2008$Emissions <- CARB.GHG.2008$Emissions * 1e12 * (1/(ShortTon.grams))
CARB.GHG.2008$Emissions[CARB.GHG.2008$GHG=="CH4"] <- CARB.GHG.2008$Emissions[CARB.GHG.2008$GHG=="CH4"] * (1/25)
CARB.GHG.2008$Emissions[CARB.GHG.2008$GHG=="N2O"] <- CARB.GHG.2008$Emissions[CARB.GHG.2008$GHG=="N2O"] * (1/298)
CARB.GHG.2008$EFs <- 0
CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="MOT"] <- CARB.GHG.2008$Emissions[CARB.GHG.2008$VClass=="MOT"]/sum(CA.MOT.VMT)
CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="LDV"] <- CARB.GHG.2008$Emissions[CARB.GHG.2008$VClass=="LDV"]/sum(CA.LDV.VMT)
CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="BUS"] <- CARB.GHG.2008$Emissions[CARB.GHG.2008$VClass=="BUS"]/sum(CA.BUS.VMT)
CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="HDT"] <- CARB.GHG.2008$Emissions[CARB.GHG.2008$VClass=="HDT"]/sum(CA.HDT.VMT)


NEI2008.VTYPE[CA.Counties,9,1] <- CA.MOT.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="MOT" & CARB.GHG.2008$GHG=="CO2"]
NEI2008.VTYPE[CA.Counties,10,1] <- CA.MOT.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="MOT" & CARB.GHG.2008$GHG=="CH4"]
NEI2008.VTYPE[CA.Counties,11,1] <- CA.MOT.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="MOT" & CARB.GHG.2008$GHG=="N2O"]

NEI2008.VTYPE[CA.Counties,9,2:4] <- CA.LDV.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="LDV" & CARB.GHG.2008$GHG=="CO2"]
NEI2008.VTYPE[CA.Counties,10,2:4] <- CA.LDV.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="LDV" & CARB.GHG.2008$GHG=="CH4"]
NEI2008.VTYPE[CA.Counties,11,2:4] <- CA.LDV.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="LDV" & CARB.GHG.2008$GHG=="N2O"]

NEI2008.VTYPE[CA.Counties,9,5:7] <- CA.BUS.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="BUS" & CARB.GHG.2008$GHG=="CO2"]
NEI2008.VTYPE[CA.Counties,10,5:7] <- CA.BUS.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="BUS" & CARB.GHG.2008$GHG=="CH4"]
NEI2008.VTYPE[CA.Counties,11,5:7] <- CA.BUS.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="BUS" & CARB.GHG.2008$GHG=="N2O"]

NEI2008.VTYPE[CA.Counties,9,8:13] <- CA.HDT.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="HDT" & CARB.GHG.2008$GHG=="CO2"]
NEI2008.VTYPE[CA.Counties,10,8:13] <- CA.HDT.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="HDT" & CARB.GHG.2008$GHG=="CH4"]
NEI2008.VTYPE[CA.Counties,11,8:13] <- CA.HDT.VMT * CARB.GHG.2008$EFs[CARB.GHG.2008$VClass=="HDT" & CARB.GHG.2008$GHG=="N2O"]

NEI2008.VTYPE[CA.Counties,9,14] <- rowSums(NEI2008.VTYPE[CA.Counties,9,1:13])
NEI2008.VTYPE[CA.Counties,10,14] <- rowSums(NEI2008.VTYPE[CA.Counties,10,1:13])
NEI2008.VTYPE[CA.Counties,11,14] <- rowSums(NEI2008.VTYPE[CA.Counties,11,1:13])

NEI2008.VTYPE.EMIS <- NEI2008.VTYPE


for (vt in 1:14)
{
  CA.Mean.EF <- colSums(NEI2008.VTYPE[CA.Counties,4:11,vt])/sum(NEI2008.VTYPE[CA.Counties,3,vt]) * ShortTon.grams #CA GHGs will be dealt with later
  US.Mean.EF <- colSums(NEI2008.VTYPE[-CA.Counties,4:11,vt])/sum(NEI2008.VTYPE[-CA.Counties,3,vt]) * ShortTon.grams
  NEI2008.VTYPE[,4:11,vt] <- (NEI2008.VTYPE[,4:11,vt]/NEI2008.VTYPE[,3,vt]) * ShortTon.grams
  for (pol in 4:11)
  {
    NEI2008.VTYPE[CA.Counties,pol,vt][NEI2008.VTYPE[CA.Counties,pol,vt]==0] <- CA.Mean.EF[pol-3]
    NEI2008.VTYPE[-CA.Counties,pol,vt][NEI2008.VTYPE[-CA.Counties,pol,vt]==0] <- US.Mean.EF[pol-3]
    NEI2008.VTYPE[CA.Counties,pol,vt][is.na(NEI2008.VTYPE[CA.Counties,pol,vt])] <- CA.Mean.EF[pol-3]
    NEI2008.VTYPE[-CA.Counties,pol,vt][is.na(NEI2008.VTYPE[-CA.Counties,pol,vt])] <- US.Mean.EF[pol-3]
    NEI2008.VTYPE[CA.Counties,pol,vt][NEI2008.VTYPE[CA.Counties,pol,vt]=="Inf"] <- CA.Mean.EF[pol-3]
    NEI2008.VTYPE[-CA.Counties,pol,vt][NEI2008.VTYPE[-CA.Counties,pol,vt]=="Inf"] <- US.Mean.EF[pol-3]
  }
}

NEI2008.VTYPE.EF <- NEI2008.VTYPE
rm(NEI2008.VTYPE)

#Please change file path in the next line according to desired location to save output
save(list=c("NEI2008.VTYPE.EF","NEI2008.VTYPE.EMIS"),file="Processed_Data_For_Model/NEI2008.RData")
