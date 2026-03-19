#Code to process NEI 2014 emissions
#Author: Ernani F. Choma 

#Please change the working directory path on line 18 before running:
#Line 18 -- Working directory
#Also make sure to check following paths for Inputs and Outputs (i.e., Processed Data for Model):
#Inputs:
#Line 40-44 -- NEI On-Road emissions files (not provided with model/needs to be downloaded)
#Line 53-54 -- NEI On-Road supporting data -- US VMT file (not provided with model/needs to be downloaded)
#Line 60-61 -- NEI On-Road supporting data -- CA VMT file (not provided with model/needs to be downloaded)
#Line 115 -- STCOU List file (provided with model). STCOU = FIPS State + County code
#Line 176 -- CARB GHG emissions file (provided with model)
#Outputs:
#Line 259 -- RData output file


#Please change path in the next line to work directory
setwd("~/US_OnRoad_HealthImpacts/Preprocessing_of_Model_Input_Data/NEI/")
#This file reads the original NEI 2014 data and processes it in the format that is used in the model 
#The Original data is not provided but can be downloaded from the links below

#On Road Data -- Emissions:
#US EPA -- NEI 2014
#https://gaftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_onroad_byregions.zip
#Version: Last Modified 11/19/20
#Accessed 04/30/21

#On Road Supplemental Files: VMT
#US EPA -- NEI 2014
#https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/onroad/2014v2_onroad_activity_final.zip
#Version: Last Modified 05/15/18
#This supplemental dataset has many files. The two files used were:
#(i): 2014v2_onroad_activity_final/inputs/onroad/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v4.csv
#(ii): 2014v2_onroad_activity_final/inputs/onroad/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v4.csv
#Accessed 04/30/21

###### 1. Loading Main NEI 2014 Mobile Source Datasets ######
#US is split in 10 regions, hence numbers 1-10
#Please change file path in the next 5 lines according to location of NEI data (needs to be downloaded -- see above)
OnRoad123 <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014neiv2_onroad_byregions/onroad_123.csv")
OnRoad4 <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014neiv2_onroad_byregions/onroad_4.csv")
OnRoad5 <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014neiv2_onroad_byregions/onroad_5.csv")
OnRoad67 <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014neiv2_onroad_byregions/onroad_67.csv")
OnRoad8910 <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014neiv2_onroad_byregions/onroad_8910.csv")


###### 2. Loading Supplemental Files for VMT & VPOP ######
#California's files are separate. 
#VMT: The data starts on line 21 and the header is on line 11

#US data
#Please change file path in the next 2 lines according to location of NEI supplemental VMT data (needs to be downloaded -- see above)
VMT <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014v2_onroad_activity_final/inputs/onroad/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v4.csv",skip=20, header=F)
VMTh <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014v2_onroad_activity_final/inputs/onroad/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v4.csv",skip=10, header=F,nrows=1, colClasses=c(rep("character",26)))

names(VMT) <- VMTh

#California data
#Please change file path in the next 2 lines according to location of NEI supplemental VMT data (needs to be downloaded -- see above)
VMTCA <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014v2_onroad_activity_final/inputs/onroad_ca_adj/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v5.csv",skip=20, header=F)
VMTCAh <- read.csv("Original_Data_Pre_Processing/NEI_2014/2014v2_onroad_activity_final/inputs/onroad_ca_adj/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v5.csv",skip=10, header=F,nrows=1, colClasses=c(rep("character",26)))

names(VMTCA) <- VMTCAh

###### 3. Filtering and aggregating emissions ######
OnRoad <- rbind(OnRoad123,OnRoad4,OnRoad5,OnRoad67,OnRoad8910)

VMT <- rbind(VMT, VMTCA)

names(VMT)
VMT <- subset(VMT, select=c("region_cd","scc","ann_parm_value"))
names(VMT) <- c("STCOU","SCC","VMT")
VMT$VTYPE <- (VMT$SCC %/% 1e4) %% 1e2

rm(list=ls()[! ls() %in% c("OnRoad","VMT")])

pollutants <- c("PM25-PRI","SO2","NOX","NH3","VOC","CO2","CH4","N2O")

OnRoad <- OnRoad[OnRoad$pollutant_cd %in% pollutants,]

table(OnRoad$uom) #OK...all in TON (i.e. short ton)

OnRoad <- subset(OnRoad, select=c("state_and_county_fips_code",
                                  "scc",
                                  "pollutant_cd",
                                  "total_emissions"))
names(OnRoad) <- c("STCOU","SCC","POLCD","EMIS")
OnRoad$VTYPE <- (OnRoad$SCC %/% 1e4) %% 1e2

#Fixing FIPS state code to match codes for 2017
OnRoad$STCOU[OnRoad$STCOU==46113] <- 46102
VMT$STCOU[VMT$STCOU==46113] <- 46102

OnRoad <- aggregate(OnRoad$EMIS, by=list(STCOU=OnRoad$STCOU, VTYPE=OnRoad$VTYPE, POLCD=OnRoad$POLCD), FUN=sum)
names(OnRoad)[4] <- "EMIS"

OnRoad <- reshape(OnRoad, timevar="POLCD", idvar=c("STCOU","VTYPE"), direction="wide")
names(OnRoad)[8] <- "EMIS.PM25PRI"

VMT <- aggregate(VMT$VMT, by=list(STCOU=VMT$STCOU, VTYPE=VMT$VTYPE), FUN=sum)
sum(VMT$x)
names(VMT)[3] <- "VMT"

#Filtering contiguous U.S., i.e. Removing AK, HI, PR & VI 
OnRoad <- OnRoad[!OnRoad$STCOU %/% 1000 %in% c(2,15,72,78),]
VMT <- VMT[!VMT$STCOU %/% 1000 %in% c(2,15,72,78),]

OnRoad[is.na(OnRoad)] <- 0

NEI2014 <- merge(x=OnRoad, y=VMT, by=c("STCOU","VTYPE"), all=TRUE)
NEI2014[is.na(NEI2014)] <- 0
rm(list=ls()[! ls() %in% c("NEI2014","pollutants")])

#Please change file path in the next line according to location of STCOU (FIPS State+County codes) file (provided with model)
STCOU.List <- read.csv("Auxiliary/STCOUList.csv")
names(STCOU.List) <- "STCOU"

names(NEI2014)
NEI2014 <- subset(NEI2014, select=c("STCOU","VTYPE","VMT",
                                    "EMIS.PM25PRI","EMIS.SO2","EMIS.NOX","EMIS.NH3","EMIS.VOC",
                                    "EMIS.CO2","EMIS.CH4","EMIS.N2O"))

NEI2014.Ref <- NEI2014[NEI2014$VTYPE==0,]
summary(NEI2014.Ref)
NEI2014.Ref <- subset(NEI2014.Ref, select=c("STCOU","EMIS.VOC"))


NEI2014.VTYPE <- array(rep(-1,3108*11*14),dim=c(3108,11,14))
MOVES.VTYPE <- unique(NEI2014$VTYPE)
MOVES.VTYPE <- MOVES.VTYPE[MOVES.VTYPE!=0] #Excluding refueling
MOVES.VTYPE <- MOVES.VTYPE[order(MOVES.VTYPE,decreasing=FALSE)]

pollutants[1] <- "PM25PRI"
ordercols <- c("STCOU","VTYPE","VMT",paste0("EMIS.",pollutants))

for (i in 1:length(MOVES.VTYPE))
{
  NEI.filter <- NEI2014[NEI2014$VTYPE==MOVES.VTYPE[i],]
  NEI.filter <- NEI.filter[order(NEI.filter$STCOU,decreasing=FALSE),]
  NEI.filter <- subset(NEI.filter, select=ordercols)
  NEI.filter <- merge(x=NEI.filter,y=STCOU.List,by="STCOU",all=TRUE)
  print(sum(is.na(NEI.filter)))
  NEI.filter[is.na(NEI.filter)] <- 0
  NEI2014.VTYPE[,,i] <- as.matrix(NEI.filter)
}

NEI.filter <- NEI2014
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
NEI2014.VTYPE[,,14] <- as.matrix(NEI.filter)
for (i in 1:14)
{
  NEI2014.VTYPE[,2,i] <- (c(MOVES.VTYPE,99))[i]
}


#Supplementing CA N2O emissions with CARB Data
#Please change file path in the next line according to location of CARB GHG data file (provided with model)
CARBGHG <- read.csv("Original_Data_Pre_Processing/Auxiliary/CARB_GHG_OnRoad.csv", skip=1)
dim(NEI2014.VTYPE)
CA.Counties <- which(NEI2014.VTYPE[,1,1] %/% 1000 == 6)
ShortTon.grams <- 2000*1000*0.45359237
CA.VMT <- NEI2014.VTYPE[CA.Counties,3,1:13]
CA.MOT.VMT <- CA.VMT[,1]
CA.LDV.VMT <- CA.VMT[,2:4]
CA.BUS.VMT <- CA.VMT[,5:7]
CA.HDT.VMT <- CA.VMT[,8:13]


CARBGHG$VClass <- "NA"
CARBGHG$VClass[CARBGHG$Sector.Level.3=="Light-duty Vehicles"] <- "LDV"
CARBGHG$VClass[CARBGHG$Sector.Level.3=="Heavy-duty Vehicles"] <- "HDT"
CARBGHG$VClass[CARBGHG$Sector.Level.4=="Motorcycles"] <- "MOT"
CARBGHG$VClass[CARBGHG$Sector.Level.4=="Buses"] <- "BUS"
CARB.N2O.2014 <- aggregate(CARBGHG$X2014, by=list(CARBGHG$VClass,CARBGHG$GHG),FUN=sum)
names(CARB.N2O.2014) <- c("VClass","GHG","Emissions")
CARB.N2O.2014 <- CARB.N2O.2014[CARB.N2O.2014$GHG=="N2O",]
CARB.N2O.2014 <- CARB.N2O.2014[CARB.N2O.2014$VClass != "NA",]
#These are in MT (Tg) of CO2-eq, so converting to Short Tons of N2O:
#Emissions * 1e12 [g/Tg] * (1/(2000*1000*0.4536)) [short tons/g] * 1/298 [CO2-eq/N2O]
CARB.N2O.2014$Emissions <- CARB.N2O.2014$Emissions * 1e12 * (1/(ShortTon.grams)) * (1/298)
CARB.N2O.2014$EFs <- 0
CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="MOT"] <- CARB.N2O.2014$Emissions[CARB.N2O.2014$VClass=="MOT"]/sum(CA.MOT.VMT)
CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="LDV"] <- CARB.N2O.2014$Emissions[CARB.N2O.2014$VClass=="LDV"]/sum(CA.LDV.VMT)
CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="BUS"] <- CARB.N2O.2014$Emissions[CARB.N2O.2014$VClass=="BUS"]/sum(CA.BUS.VMT)
CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="HDT"] <- CARB.N2O.2014$Emissions[CARB.N2O.2014$VClass=="HDT"]/sum(CA.HDT.VMT)

CA.MOT.N2O <- CA.MOT.VMT * CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="MOT"]
CA.LDV.N2O <- CA.LDV.VMT * CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="LDV"]
CA.BUS.N2O <- CA.BUS.VMT * CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="BUS"]
CA.HDT.N2O <- CA.HDT.VMT * CARB.N2O.2014$EFs[CARB.N2O.2014$VClass=="HDT"]

NEI2014.VTYPE[CA.Counties,11,1] <- CA.MOT.N2O
NEI2014.VTYPE[CA.Counties,11,2:4] <- CA.LDV.N2O
NEI2014.VTYPE[CA.Counties,11,5:7] <- CA.BUS.N2O
NEI2014.VTYPE[CA.Counties,11,8:13] <- CA.HDT.N2O
NEI2014.VTYPE[CA.Counties,11,14] <- rowSums(NEI2014.VTYPE[CA.Counties,11,1:13])


NEI2014.VTYPE.EMIS <- NEI2014.VTYPE
for (vt in 1:14)
{
  CA.Mean.EF <- colSums(NEI2014.VTYPE[CA.Counties,4:11,vt])/sum(NEI2014.VTYPE[CA.Counties,3,vt]) * ShortTon.grams #CA GHGs will be dealt with later
  US.Mean.EF <- colSums(NEI2014.VTYPE[-CA.Counties,4:11,vt])/sum(NEI2014.VTYPE[-CA.Counties,3,vt]) * ShortTon.grams
  NEI2014.VTYPE[,4:11,vt] <- (NEI2014.VTYPE[,4:11,vt]/NEI2014.VTYPE[,3,vt]) * ShortTon.grams
  for (pol in 4:11)
  {
    NEI2014.VTYPE[CA.Counties,pol,vt][NEI2014.VTYPE[CA.Counties,pol,vt]==0] <- CA.Mean.EF[pol-3]
    NEI2014.VTYPE[-CA.Counties,pol,vt][NEI2014.VTYPE[-CA.Counties,pol,vt]==0] <- US.Mean.EF[pol-3]
    NEI2014.VTYPE[CA.Counties,pol,vt][is.na(NEI2014.VTYPE[CA.Counties,pol,vt])] <- CA.Mean.EF[pol-3]
    NEI2014.VTYPE[-CA.Counties,pol,vt][is.na(NEI2014.VTYPE[-CA.Counties,pol,vt])] <- US.Mean.EF[pol-3]
    NEI2014.VTYPE[CA.Counties,pol,vt][NEI2014.VTYPE[CA.Counties,pol,vt]=="Inf"] <- CA.Mean.EF[pol-3]
    NEI2014.VTYPE[-CA.Counties,pol,vt][NEI2014.VTYPE[-CA.Counties,pol,vt]=="Inf"] <- US.Mean.EF[pol-3]
  }
}

aux.VMT <- as.data.frame(NEI2014.VTYPE.EMIS[,c(1,3),14])
names(aux.VMT) <- c("STCOU","VMT")
aux.Mean.EF <- merge(x=NEI2014.Ref,y=aux.VMT,by="STCOU",all.x=TRUE,all.y=FALSE)
CA.Mean.EF <- sum(aux.Mean.EF$EMIS.VOC[CA.Counties])/sum(aux.Mean.EF$VMT[CA.Counties]) * ShortTon.grams
US.Mean.EF <- sum(aux.Mean.EF$EMIS.VOC[-CA.Counties])/sum(aux.Mean.EF$VMT[-CA.Counties]) * ShortTon.grams

NEI2014.Ref <- merge(x=NEI2014.Ref,y=aux.VMT,by="STCOU",all=TRUE)
aux.missing <- which(is.na(NEI2014.Ref$EMIS.VOC))
aux.CA <- aux.missing[aux.missing %in% CA.Counties]
aux.US <- aux.missing[!aux.missing %in% CA.Counties]
NEI2014.Ref[is.na(NEI2014.Ref)] <- 0

NEI2014.Ref$EF.VOC <- (NEI2014.Ref$EMIS.VOC/NEI2014.Ref$VMT) * ShortTon.grams
NEI2014.Ref$EF.VOC[aux.CA] <- CA.Mean.EF
NEI2014.Ref$EF.VOC[aux.US] <- US.Mean.EF

NEI2014.Ref$VTYPE <- 0

NEI2014.VTYPE.EF <- NEI2014.VTYPE
NEI2014.REF.EMIS <- NEI2014.Ref[,c(1,5,3,2,4)]
rm(NEI2014.VTYPE)
rm(NEI2014.Ref)

#Please change file path in the next line according to desired location to save output
save(list=c("NEI2014.VTYPE.EF","NEI2014.VTYPE.EMIS","NEI2014.REF.EMIS"),file="Processed_Data_For_Model/NEI2014.RData")
