####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com


############INSTRUCTIONS

#Run this script via SupDataDR4C.R

############INSTRUCTIONS



library(gdata)
library(data.table)
library(maps)
library(gridExtra)
library(ggplot2)
library(plyr)
library(dplyr)
library(robis)
library(ggpubr)

theme_set(theme_bw())

####PARAMETERS ----
#Set number of individuals of of taxanomic ranks to retain community
n_phy<-3
n_cla<-4
n_ord<-5
n_fam<-6
n_gen<-7

#Set the geographic bin sizes used for grouping modern collections into datasets (in fractions of degrees: 1 =.01 degrees square, 2 =.02 degrees square, etc.):
ModBinRes <- 10
#Set the size of depth bins used for grouping modern collections into datasets (in meters):
DepthRes <- 10

#Read in OBIS data
EurDat_mstr <- fread("SupDataDR1.csv",header = T, sep = ',',#drop=c("V1"),
                     #nrow=100000,
                     select=c("id","dataset_id","year","minimumdepthinmeters","maximumdepthinmeters","decimallatitude","decimallongitude",
                              "class","family","samplingprotocol","dropped",
                              "phylum","order","genus","habitat","scientificname","kingdom")) 

#Drop likely errors
EurDat<-subset(EurDat_mstr, maximumdepthinmeters>=0 & minimumdepthinmeters>=0)
EurDat<-subset(EurDat, maximumdepthinmeters<=11000 & minimumdepthinmeters<=11000)
EurDat<-subset(EurDat, decimallatitude!=0 & decimallongitude!=0)
EurDat[EurDat==""] <- NA
EurDat <- EurDat[is.na(EurDat$dropped),]
EurDat <- EurDat[EurDat$kingdom %in% "Animalia",]

#Drop species named "Zooplankton"
EurDat <- EurDat[(EurDat$scientificname !=  "Zooplankton"),]

##Drop bad quality sets from "dataset_id"
dataset_id_drop<-c(
  #TOPP Fish 
  "1004a490-c117-4712-9868-a8412420f4c8",
  #TOPP northern elephant seal 
  "9818d42f-87ed-4bea-95bf-dac9226f3748",
  #TOPP Summary of SSM-derived Telemetry 
  "68ed4f02-f04b-46ae-b273-0bcbf8a8f807",
  #TOPP Albatrosses 
  "a02e9289-42b5-4520-b079-fb9b7ac02075",
  #ICES Zooplankton Community  
  "363961a9-9f45-46df-980b-0b076e66a296",
  #ICES Phytoplankton Community 
  "46c52e34-73b7-45e6-a2ac-742e4c2058c5",
  #ICES Zoobenthos Community 
  "8acba7e7-2e50-4490-8328-b78a30472508",
  #ICES Phytobenthos community 
  "9c8a2ac6-f394-4fa1-981e-227fa7b14675",
  #Diveboard 
  "4d2a31a3-bb76-4ffe-a2f9-817e975f6ba9",
  
  #Fish/vert only studies
  "f8c046f0-e8ae-49f5-b94c-f78809a8b7dc",	#Western Australian Museum Fish Collection - Marine records
  "5b6251f6-a7a5-4dc9-994d-c9504b54776f",	#Rockfish Recruitment and Ecosystem Assessment Survey, Catch Data
  "58f229d6-8aad-4d85-b4b6-621d4f25d5ec",	#Museums Victoria Ichthyology Collection
  "c53a1d63-0ca7-4481-8607-0b2d01f9f99a",	#CSIRO, fish surveys by the Courageous, 1978-1979
  "4293224b-d280-4264-ab19-30ab5f78c826",	#CSIRO, fish surveys by the Soela, 1980s
  "c56bd06c-f51e-4506-9239-584938c0c880" #Southeast Area Monitoring and Assessment Program (SEAMAP)
)
EurDat<-EurDat[!EurDat$dataset_id %in% dataset_id_drop, ]


#Drop datasets by number of taxa by column
Alldata_tax_nums<- EurDat %>% group_by(dataset_id) %>%
  mutate(u_phylum = n_distinct(phylum),
         u_class = n_distinct(class),
         u_order = n_distinct(order),
         u_family = n_distinct(family),
         u_genus = n_distinct(genus)
  )
Alldata_tax_nums<-Alldata_tax_nums[,c("dataset_id","u_phylum",
                                      "u_class","u_family",
                                      "u_order","u_genus"
)]
Alldata_tax_nums<-unique(Alldata_tax_nums[,])
Alldata_tax_nums<-subset(Alldata_tax_nums, u_phylum>=n_phy & 
                           u_class>=n_cla & u_family>=n_fam &
                           u_order>=n_ord &  u_genus>=n_gen
)
EurDat<- EurDat[EurDat$dataset_id %in% as.character(Alldata_tax_nums$dataset_id), ]

#Select required columns
Moddata <- subset(EurDat, select=c(id,dataset_id,year,minimumdepthinmeters,maximumdepthinmeters,decimallatitude,decimallongitude,
                                   class,family,samplingprotocol,
                                   phylum,order,genus,habitat))

#Define lat, long, and depth bins for aggregating collections into datasets:  
round_any = function(x, accuracy, f=ceiling){f(x/ accuracy) * accuracy}

Moddata <- Moddata[(Moddata$maximumdepthinmeters > 0),]
Moddata$depth.slice<-round_any(Moddata$maximumdepthinmeters,accuracy=DepthRes)
Moddata$LatBin <- round(Moddata$decimallatitude/ModBinRes,2)*ModBinRes
Moddata$LongBin <- round(Moddata$decimallongitude/ModBinRes,2)*ModBinRes
Moddata$coordinates <- paste(Moddata$LatBin,Moddata$LongBin)
Moddata$time.bin <- ceiling(Moddata$year/1)*1

#Make a collection ID code by concatenating all unique variables:  
Moddata$collection_no <- paste(Moddata$dataset_id,Moddata$year)

#Remove additional taxon names that are generically unresolved:  
Moddata <- Moddata[( Moddata$genus != ""),]
indets <-c("POLYPLACOPHORA","Polyplacophora","PLATYHELMINTHES","Platyhelminthes","Osteichthyes","OSTEICHTHYES","Neoloricata","NEOLORICATA","Brachyura","BRACHYURA","BIVALVE","BIVALVIA","","ACTINIARIA","Actiniaria","Porifera","PORIFERA","SIPUNCULA","Sipuncula","Turbellaria","TURBELLARIA","Echiura","ECHIURA","Echinodermata","Cirripedia","Cephalaspidea","Branchiura","Ascidia","Nemertinea","Cladocera","NEMERTINI","unidentified","CHITON","Annelida")
Moddata <- Moddata[ ! Moddata$genus %in% indets, ]
Moddata$taxon <- drop.levels(gsub("Ampelicsa", "Ampelisca",Moddata$genus))
Moddata$taxon <- drop.levels(gsub("Pisidia", "Phisidia",Moddata$genus))

#Combine coordinates, time bin, depth, substrate to define unique datasets for diversity partitioning:  
Moddata$dataset.par <- paste(Moddata$coordinates,Moddata$time.bin,Moddata$dataset_id,Moddata$depth.slice,Moddata$habitat)

Alldata<-Moddata

#Again, cull communities by number of taxa
Alldata_tax_nums<- Alldata %>% group_by(dataset.par) %>%
  mutate(u_phylum = n_distinct(phylum,na.rm=T),
         u_class = n_distinct(class,na.rm=T),
         u_order = n_distinct(order,na.rm=T),
         u_family = n_distinct(family,na.rm=T),
         u_genus = n_distinct(genus,na.rm=T)
  )
Alldata_tax_nums<-Alldata_tax_nums[,c("dataset.par","dataset_id","u_phylum",
                                      "u_class","u_family",
                                      "u_order","u_genus")]
Alldata_tax_nums<-unique(Alldata_tax_nums[,])
Alldata_tax_nums<-subset(Alldata_tax_nums, u_phylum>=n_phy & 
                           u_class>=n_cla & u_family>=n_fam &
                           u_order>=n_ord &  u_genus>=n_gen)

Alldata<- Alldata[Alldata$dataset.par %in% as.character(Alldata_tax_nums$dataset.par), ]

Alldata$decimallatitude<-NULL
Alldata$decimallongitude<-NULL
Alldata$id<-NULL
Alldata$samplingprotocol<-NULL
Alldata$coordinates<-NULL
Alldata$collection_no<-NULL
Alldata$minimumdepthinmeters<-NULL
Alldata$maximumdepthinmeters<-NULL

#Change blanks to NA
Alldata<-as.data.frame(apply(Alldata, 2, function(x) gsub("^$|^ $", NA, x)))
Alldata<-unique(Alldata[,])

#Confirm that abundances are joined by dataset and genus
Moddata_joined2 <- as.data.frame(Alldata)

#Clean taxon labels
Moddata_joined2$phylum<-stringr::word(Moddata_joined2$phylum, 1)
Moddata_joined2$class<-stringr::word(Moddata_joined2$class, 1)
Moddata_joined2$order<-stringr::word(Moddata_joined2$order, 1)
Moddata_joined2$family<-stringr::word(Moddata_joined2$family, 1)
Moddata_joined2$genus<-stringr::word(Moddata_joined2$genus, 1)
