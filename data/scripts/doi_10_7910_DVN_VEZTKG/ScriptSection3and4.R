#First coding file for waiting times paper
#Main results of sections 3 and 4
#Construction of samples waiting for mental healthcare and control sample not waiting for care
#Time series data on healthcare utilization and labour market outcomes
#Descriptive statistics
#event-study analysis on onset of mental health problems

library(tidyverse)
library(haven)
library(lubridate)
library(xtable)
library(magrittr)
library(data.table)
library(biglm)
library(xlsx)
library(readxl)
library(AER)
library(plm)
library(MatchIt)
library(Matching)
library(dplyr)
library(TAF)

#Create directories for intermediate data files and final output files
dir.create("IntermediateFiles")
dir.create("FinalOutput")

#Load all mental healthcare activities with positive treatment duration
Activiteiten2012 <- read_sav("G:/GezondheidWelzijn/GGZDBCGELEVERDZORGPROFIELTAB/GGZDBCGELEVERDZORGPROFIEL2012TABV1.sav")
Activiteiten2012%<>%
  dplyr::select(RINPERSOON, GGZInstelling, GGZDBCtrajectnummer, GGZGZPZorgactiviteit, GGZGZPZorgactiviteitDatum, GGZGZPDirecttijd, GGZGZPBeroepcode)
Activiteiten2012<-Activiteiten2012[!is.na(Activiteiten2012$GGZGZPDirecttijd),]
Activiteiten2012<-Activiteiten2012[Activiteiten2012$GGZGZPDirecttijd>0,]
Activiteiten2013 <- read_sav("G:/GezondheidWelzijn/GGZDBCGELEVERDZORGPROFIELTAB/GGZDBCGELEVERDZORGPROFIEL2013TABV1.sav")
Activiteiten2013%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummer, GGZGZPZorgactiviteit, GGZGZPZorgactiviteitDatum, GGZGZPDirecttijd, GGZGZPBeroepcode)
Activiteiten2013<-Activiteiten2013[!is.na(Activiteiten2013$GGZGZPDirecttijd),]
Activiteiten2013<-Activiteiten2013[Activiteiten2013$GGZGZPDirecttijd>0,]
Activiteiten2014 <- read_sav("G:/GezondheidWelzijn/GGZDBCGELEVERDZORGPROFIELTAB/GGZDBCGELEVERDZORGPROFIEL2014TABV2.sav")
Activiteiten2014%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummer, GGZGZPZorgactiviteit, GGZGZPZorgactiviteitDatum, GGZGZPDirecttijd, GGZGZPBeroepcode)
Activiteiten2014<-Activiteiten2014[!is.na(Activiteiten2014$GGZGZPDirecttijd),]
Activiteiten2014<-Activiteiten2014[Activiteiten2014$GGZGZPDirecttijd>0,]
Activiteiten2015 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2015TABV1.sav")
Activiteiten2015%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG, GGZGZPBeroepcodeHOOFDDIAG)
names(Activiteiten2015)<-names(Activiteiten2012)
Activiteiten2015<-Activiteiten2015[!is.na(Activiteiten2015$GGZGZPDirecttijd),]
Activiteiten2015<-Activiteiten2015[Activiteiten2015$GGZGZPDirecttijd>0,]
Activiteiten2016 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2016TABV1.sav")
Activiteiten2016%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG, GGZGZPBeroepcodeHOOFDDIAG)
names(Activiteiten2016)<-names(Activiteiten2012)
Activiteiten2016<-Activiteiten2016[!is.na(Activiteiten2016$GGZGZPDirecttijd),]
Activiteiten2016<-Activiteiten2016[Activiteiten2016$GGZGZPDirecttijd>0,]
Activiteiten2017 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2017TABV2.sav")
Activiteiten2017%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG, GGZGZPBeroepcodeHOOFDDIAG)
names(Activiteiten2017)<-names(Activiteiten2012)
Activiteiten2017<-Activiteiten2017[!is.na(Activiteiten2017$GGZGZPDirecttijd),]
Activiteiten2017<-Activiteiten2017[Activiteiten2017$GGZGZPDirecttijd>0,]
Activiteiten2018 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2018TABV2.sav")
Activiteiten2018%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG, GGZGZPBeroepcodeHOOFDDIAG)
names(Activiteiten2018)<-names(Activiteiten2012)
Activiteiten2018<-Activiteiten2018[!is.na(Activiteiten2018$GGZGZPDirecttijd),]
Activiteiten2018<-Activiteiten2018[Activiteiten2018$GGZGZPDirecttijd>0,]
Activiteiten2019 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2019TABV1.sav")
Activiteiten2019%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG, GGZGZPBeroepcodeHOOFDDIAG)
names(Activiteiten2019)<-names(Activiteiten2012)
Activiteiten2019<-Activiteiten2019[!is.na(Activiteiten2019$GGZGZPDirecttijd),]
Activiteiten2019<-Activiteiten2019[Activiteiten2019$GGZGZPDirecttijd>0,]

#load treatment trajectory information with start date and diagnosis
DBC2011 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2011TABV4.sav")
DBC2011%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummer, GGZDBCBegindatum, GGZDBCStartdatumZTR,GGZDBCHoofddiagnoseDSMIV)
DBC2012 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2012TABV3.sav")
DBC2012%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummer, GGZDBCBegindatum, GGZDBCStartdatumZTR,GGZDBCHoofddiagnoseDSMIV)
DBC2013 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2013TABV3.sav")
DBC2013%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummer, GGZDBCBegindatum, GGZDBCStartdatumZTR,GGZDBCHoofddiagnoseDSMIV)
DBC2014 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2014TABV2.sav")
DBC2014%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummer, GGZDBCBegindatum, GGZDBCStartdatumZTR,GGZDBCHoofddiagnoseDSMIV)
DBC2015 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2015TABV2.sav")
DBC2015%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummerHOOFDDIAG, GGZDBCBegindatumHOOFDDIAG, GGZDBCStartdatumZTRHOOFDDIAG,GGZDBCHoofddiagnoseDSMIVHOOFDDIAG)
names(DBC2015)<-names(DBC2012)
DBC2016 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2016TABV2.sav")
DBC2016%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummerHOOFDDIAG, GGZDBCBegindatumHOOFDDIAG, GGZDBCStartdatumZTRHOOFDDIAG,GGZDBCHoofddiagnoseDSMIVHOOFDDIAG)
names(DBC2016)<-names(DBC2012)
DBC2017 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2017TABV2.sav")
DBC2017%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummerHOOFDDIAG, GGZDBCBegindatumHOOFDDIAG, GGZDBCStartdatumZTRHOOFDDIAG,GGZDBCHoofddiagnoseDSMIVHOOFDDIAG)
names(DBC2017)<-names(DBC2012)
DBC2018 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2018TABV2.sav")
DBC2018%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummerHOOFDDIAG, GGZDBCBegindatumHOOFDDIAG, GGZDBCStartdatumZTRHOOFDDIAG,GGZDBCHoofddiagnoseDSMIVHOOFDDIAG)
names(DBC2018)<-names(DBC2012)
DBC2019 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2019TABV3.sav")
DBC2019%<>%
  dplyr::select(RINPERSOON, GGZDBCtrajectnummerHOOFDDIAG, GGZDBCBegindatumHOOFDDIAG, GGZDBCStartdatumZTRHOOFDDIAG,GGZDBCHoofddiagnoseDSMIVHOOFDDIAG)
names(DBC2019)<-names(DBC2012)

#Use minimum of startdatum financing trajectory dbc and startdate care trajectory as first moment of contact
DBC2011$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2011$GGZDBCStartdatumZTR), as.numeric(DBC2011$GGZDBCBegindatum)), na.rm=TRUE)
DBC2012$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2012$GGZDBCStartdatumZTR), as.numeric(DBC2012$GGZDBCBegindatum)), na.rm=TRUE)
DBC2013$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2013$GGZDBCStartdatumZTR), as.numeric(DBC2013$GGZDBCBegindatum)), na.rm=TRUE)
DBC2014$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2014$GGZDBCStartdatumZTR), as.numeric(DBC2014$GGZDBCBegindatum)), na.rm=TRUE)
DBC2015$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2015$GGZDBCStartdatumZTR), as.numeric(DBC2015$GGZDBCBegindatum)), na.rm=TRUE)
DBC2016$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2016$GGZDBCStartdatumZTR), as.numeric(DBC2016$GGZDBCBegindatum)), na.rm=TRUE)
DBC2017$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2017$GGZDBCStartdatumZTR), as.numeric(DBC2017$GGZDBCBegindatum)), na.rm=TRUE)
DBC2018$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2018$GGZDBCStartdatumZTR), as.numeric(DBC2018$GGZDBCBegindatum)), na.rm=TRUE)
DBC2019$GGZDBCBegindatum<-rowMins(as.matrix(as.numeric(DBC2019$GGZDBCStartdatumZTR), as.numeric(DBC2019$GGZDBCBegindatum)), na.rm=TRUE)

#Loop through year files to construct sample of individuals waiting for care. Obtain first moment of contact (Aanmelddatum), waiting time till intake (Aanmeldtijd) , waiting time till treatment (behandeltijd), crisis indicator (Crisis), mental healthcare provider (ggz instelling), and amental healthcare provider type (behandelaar)
#To ensure individuals start new treatment: exclude all individuals already receiving treatment in 2011
#2012
index<-!(Activiteiten2012$RINPERSOON %in% DBC2011$RINPERSOON)
Activiteiten2012Start<-Activiteiten2012[index,]
index<-!(DBC2012$RINPERSOON %in% DBC2011$RINPERSOON)
DBC2012Start<-DBC2012[index,]
DBC2012Start<-DBC2012Start[order(DBC2012Start$RINPERSOON, DBC2012Start$GGZDBCBegindatum),]
DBC2012Start<-distinct(DBC2012Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2012Start<-Activiteiten2012Start[order(Activiteiten2012Start$RINPERSOON, Activiteiten2012Start$GGZGZPZorgactiviteitDatum),]
DBC2012Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2012Start$GGZDBCBegindatum,1,4), "-",substring(DBC2012Start$GGZDBCBegindatum,5,6), "-",substring(DBC2012Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2012Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2012Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2012Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2012Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2012<-as.data.frame(matrix(NA, nrow(DBC2012Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd, fourth crisis indicator, fifth ggz instelling
names(Wachttijd2012)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2012$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2012Start)){
  Wachttijd2012[i,1]<-DBC2012Start$GGZDBCBegindatum[i]
  while(DBC2012Start$RINPERSOON[i]==Activiteiten2012Start$RINPERSOON[j]){
    if(substring(Activiteiten2012Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2012[i,4]<-1
      Wachttijd2012[i,6]<-Activiteiten2012Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2012[i,2])&substring(Activiteiten2012Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2012[i,2]<-Activiteiten2012Start$GGZGZPZorgactiviteitDatum[j]-DBC2012Start$GGZDBCBegindatum[i]
      Wachttijd2012[i,5]<-Activiteiten2012Start$GGZInstelling[j]
      Wachttijd2012[i,6]<-Activiteiten2012Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2012[i,3])){
        if(substring(Activiteiten2012Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2012Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2012Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2012[i,3]<-Activiteiten2012Start$GGZGZPZorgactiviteitDatum[j]-DBC2012Start$GGZDBCBegindatum[i]
          Wachttijd2012[i,5]<-Activiteiten2012Start$GGZInstelling[j]
          Wachttijd2012[i,6]<-Activiteiten2012Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}

#2013
index<-!(Activiteiten2013$RINPERSOON %in% DBC2012$RINPERSOON)
Activiteiten2013Start<-Activiteiten2013[index,]
index<-!(DBC2013$RINPERSOON %in% DBC2012$RINPERSOON)
DBC2013Start<-DBC2013[index,]
DBC2013Start<-DBC2013Start[order(DBC2013Start$RINPERSOON, DBC2013Start$GGZDBCBegindatum),]
DBC2013Start<-distinct(DBC2013Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2013Start<-Activiteiten2013Start[order(Activiteiten2013Start$RINPERSOON, Activiteiten2013Start$GGZGZPZorgactiviteitDatum),]
DBC2013Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2013Start$GGZDBCBegindatum,1,4), "-",substring(DBC2013Start$GGZDBCBegindatum,5,6), "-",substring(DBC2013Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2013Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2013Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2013Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2013Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2013<-as.data.frame(matrix(NA, nrow(DBC2013Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2013)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2013$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2013Start)){
  Wachttijd2013[i,1]<-DBC2013Start$GGZDBCBegindatum[i]
  while(DBC2013Start$RINPERSOON[i]==Activiteiten2013Start$RINPERSOON[j]){
    if(substring(Activiteiten2013Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2013[i,4]<-1
      Wachttijd2013[i,6]<-Activiteiten2013Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2013[i,2])&substring(Activiteiten2013Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2013[i,2]<-Activiteiten2013Start$GGZGZPZorgactiviteitDatum[j]-DBC2013Start$GGZDBCBegindatum[i]
      Wachttijd2013[i,5]<-Activiteiten2013Start$GGZInstelling[j]
      Wachttijd2013[i,6]<-Activiteiten2013Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2013[i,3])){
        if(substring(Activiteiten2013Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2013Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2013Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2013[i,3]<-Activiteiten2013Start$GGZGZPZorgactiviteitDatum[j]-DBC2013Start$GGZDBCBegindatum[i]
          Wachttijd2013[i,5]<-Activiteiten2013Start$GGZInstelling[j]
          Wachttijd2013[i,6]<-Activiteiten2013Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2012, Activiteiten2012, Activiteiten2012Start)
gc()

#2014
index<-!(Activiteiten2014$RINPERSOON %in% DBC2013$RINPERSOON)
Activiteiten2014Start<-Activiteiten2014[index,]
index<-!(DBC2014$RINPERSOON %in% DBC2013$RINPERSOON)
DBC2014Start<-DBC2014[index,]
DBC2014Start<-DBC2014Start[order(DBC2014Start$RINPERSOON, DBC2014Start$GGZDBCBegindatum),]
DBC2014Start<-distinct(DBC2014Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2014Start<-Activiteiten2014Start[order(Activiteiten2014Start$RINPERSOON, Activiteiten2014Start$GGZGZPZorgactiviteitDatum),]
DBC2014Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2014Start$GGZDBCBegindatum,1,4), "-",substring(DBC2014Start$GGZDBCBegindatum,5,6), "-",substring(DBC2014Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2014Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2014Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2014Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2014Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2014<-as.data.frame(matrix(NA, nrow(DBC2014Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2014)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2014$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2014Start)){
  Wachttijd2014[i,1]<-DBC2014Start$GGZDBCBegindatum[i]
  while(DBC2014Start$RINPERSOON[i]==Activiteiten2014Start$RINPERSOON[j]){
    if(substring(Activiteiten2014Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2014[i,4]<-1
      Wachttijd2014[i,6]<-Activiteiten2014Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2014[i,2])&substring(Activiteiten2014Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2014[i,2]<-Activiteiten2014Start$GGZGZPZorgactiviteitDatum[j]-DBC2014Start$GGZDBCBegindatum[i]
      Wachttijd2014[i,5]<-Activiteiten2014Start$GGZInstelling[j]
      Wachttijd2014[i,6]<-Activiteiten2014Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2014[i,3])){
        if(substring(Activiteiten2014Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2014Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2014Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2014[i,3]<-Activiteiten2014Start$GGZGZPZorgactiviteitDatum[j]-DBC2014Start$GGZDBCBegindatum[i]
          Wachttijd2014[i,5]<-Activiteiten2014Start$GGZInstelling[j]
          Wachttijd2014[i,6]<-Activiteiten2014Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2013, Activiteiten2013, Activiteiten2013Start)
gc()

#2015
index<-!(Activiteiten2015$RINPERSOON %in% DBC2014$RINPERSOON)
Activiteiten2015Start<-Activiteiten2015[index,]
index<-!(DBC2015$RINPERSOON %in% DBC2014$RINPERSOON)
DBC2015Start<-DBC2015[index,]
DBC2015Start<-DBC2015Start[order(DBC2015Start$RINPERSOON, DBC2015Start$GGZDBCBegindatum),]
DBC2015Start<-distinct(DBC2015Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2015Start<-Activiteiten2015Start[order(Activiteiten2015Start$RINPERSOON, Activiteiten2015Start$GGZGZPZorgactiviteitDatum),]
DBC2015Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2015Start$GGZDBCBegindatum,1,4), "-",substring(DBC2015Start$GGZDBCBegindatum,5,6), "-",substring(DBC2015Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2015Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2015Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2015Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2015Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2015<-as.data.frame(matrix(NA, nrow(DBC2015Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2015)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2015$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2015Start)){
  Wachttijd2015[i,1]<-DBC2015Start$GGZDBCBegindatum[i]
  while(DBC2015Start$RINPERSOON[i]==Activiteiten2015Start$RINPERSOON[j]){
    if(substring(Activiteiten2015Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2015[i,4]<-1
      Wachttijd2015[i,6]<-Activiteiten2015Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2015[i,2])&substring(Activiteiten2015Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2015[i,2]<-Activiteiten2015Start$GGZGZPZorgactiviteitDatum[j]-DBC2015Start$GGZDBCBegindatum[i]
      Wachttijd2015[i,5]<-Activiteiten2015Start$GGZInstelling[j]
      Wachttijd2015[i,6]<-Activiteiten2015Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2015[i,3])){
        if(substring(Activiteiten2015Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2015Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2015Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2015[i,3]<-Activiteiten2015Start$GGZGZPZorgactiviteitDatum[j]-DBC2015Start$GGZDBCBegindatum[i]
          Wachttijd2015[i,5]<-Activiteiten2015Start$GGZInstelling[j]
          Wachttijd2015[i,6]<-Activiteiten2015Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2014, Activiteiten2014, Activiteiten2014Start)
gc()

#2016
index<-!(Activiteiten2016$RINPERSOON %in% DBC2015$RINPERSOON)
Activiteiten2016Start<-Activiteiten2016[index,]
index<-!(DBC2016$RINPERSOON %in% DBC2015$RINPERSOON)
DBC2016Start<-DBC2016[index,]
DBC2016Start<-DBC2016Start[order(DBC2016Start$RINPERSOON, DBC2016Start$GGZDBCBegindatum),]
DBC2016Start<-distinct(DBC2016Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2016Start<-Activiteiten2016Start[order(Activiteiten2016Start$RINPERSOON, Activiteiten2016Start$GGZGZPZorgactiviteitDatum),]
DBC2016Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2016Start$GGZDBCBegindatum,1,4), "-",substring(DBC2016Start$GGZDBCBegindatum,5,6), "-",substring(DBC2016Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2016Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2016Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2016Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2016Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2016<-as.data.frame(matrix(NA, nrow(DBC2016Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2016)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2016$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2016Start)){
  Wachttijd2016[i,1]<-DBC2016Start$GGZDBCBegindatum[i]
  while(DBC2016Start$RINPERSOON[i]==Activiteiten2016Start$RINPERSOON[j]){
    if(substring(Activiteiten2016Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2016[i,4]<-1
      Wachttijd2016[i,6]<-Activiteiten2016Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2016[i,2])&substring(Activiteiten2016Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2016[i,2]<-Activiteiten2016Start$GGZGZPZorgactiviteitDatum[j]-DBC2016Start$GGZDBCBegindatum[i]
      Wachttijd2016[i,5]<-Activiteiten2016Start$GGZInstelling[j]
      Wachttijd2016[i,6]<-Activiteiten2016Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2016[i,3])){
        if(substring(Activiteiten2016Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2016Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2016Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2016[i,3]<-Activiteiten2016Start$GGZGZPZorgactiviteitDatum[j]-DBC2016Start$GGZDBCBegindatum[i]
          Wachttijd2016[i,5]<-Activiteiten2016Start$GGZInstelling[j]
          Wachttijd2016[i,6]<-Activiteiten2016Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2015, Activiteiten2015, Activiteiten2015Start)
gc()

#2017
index<-!(Activiteiten2017$RINPERSOON %in% DBC2016$RINPERSOON)
Activiteiten2017Start<-Activiteiten2017[index,]
index<-!(DBC2017$RINPERSOON %in% DBC2016$RINPERSOON)
DBC2017Start<-DBC2017[index,]
DBC2017Start<-DBC2017Start[order(DBC2017Start$RINPERSOON, DBC2017Start$GGZDBCBegindatum),]
DBC2017Start<-distinct(DBC2017Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2017Start<-Activiteiten2017Start[order(Activiteiten2017Start$RINPERSOON, Activiteiten2017Start$GGZGZPZorgactiviteitDatum),]
DBC2017Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2017Start$GGZDBCBegindatum,1,4), "-",substring(DBC2017Start$GGZDBCBegindatum,5,6), "-",substring(DBC2017Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2017Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2017Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2017Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2017Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2017<-as.data.frame(matrix(NA, nrow(DBC2017Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2017)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2017$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2017Start)){
  Wachttijd2017[i,1]<-DBC2017Start$GGZDBCBegindatum[i]
  while(DBC2017Start$RINPERSOON[i]==Activiteiten2017Start$RINPERSOON[j]){
    if(substring(Activiteiten2017Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2017[i,4]<-1
      Wachttijd2017[i,6]<-Activiteiten2017Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2017[i,2])&substring(Activiteiten2017Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2017[i,2]<-Activiteiten2017Start$GGZGZPZorgactiviteitDatum[j]-DBC2017Start$GGZDBCBegindatum[i]
      Wachttijd2017[i,5]<-Activiteiten2017Start$GGZInstelling[j]
      Wachttijd2017[i,6]<-Activiteiten2017Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2017[i,3])){
        if(substring(Activiteiten2017Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2017Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2017Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2017[i,3]<-Activiteiten2017Start$GGZGZPZorgactiviteitDatum[j]-DBC2017Start$GGZDBCBegindatum[i]
          Wachttijd2017[i,5]<-Activiteiten2017Start$GGZInstelling[j]
          Wachttijd2017[i,6]<-Activiteiten2017Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2016, Activiteiten2016, Activiteiten2016Start)
gc()

#2018
index<-!(Activiteiten2018$RINPERSOON %in% DBC2017$RINPERSOON)
Activiteiten2018Start<-Activiteiten2018[index,]
index<-!(DBC2018$RINPERSOON %in% DBC2017$RINPERSOON)
DBC2018Start<-DBC2018[index,]
DBC2018Start<-DBC2018Start[order(DBC2018Start$RINPERSOON, DBC2018Start$GGZDBCBegindatum),]
DBC2018Start<-distinct(DBC2018Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2018Start<-Activiteiten2018Start[order(Activiteiten2018Start$RINPERSOON, Activiteiten2018Start$GGZGZPZorgactiviteitDatum),]
DBC2018Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2018Start$GGZDBCBegindatum,1,4), "-",substring(DBC2018Start$GGZDBCBegindatum,5,6), "-",substring(DBC2018Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2018Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2018Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2018Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2018Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2018<-as.data.frame(matrix(NA, nrow(DBC2018Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2018)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2018$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2018Start)){
  Wachttijd2018[i,1]<-DBC2018Start$GGZDBCBegindatum[i]
  while(DBC2018Start$RINPERSOON[i]==Activiteiten2018Start$RINPERSOON[j]){
    if(substring(Activiteiten2018Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2018[i,4]<-1
      Wachttijd2018[i,6]<-Activiteiten2018Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2018[i,2])&substring(Activiteiten2018Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2018[i,2]<-Activiteiten2018Start$GGZGZPZorgactiviteitDatum[j]-DBC2018Start$GGZDBCBegindatum[i]
      Wachttijd2018[i,5]<-Activiteiten2018Start$GGZInstelling[j]
      Wachttijd2018[i,6]<-Activiteiten2018Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2018[i,3])){
        if(substring(Activiteiten2018Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2018Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2018Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2018[i,3]<-Activiteiten2018Start$GGZGZPZorgactiviteitDatum[j]-DBC2018Start$GGZDBCBegindatum[i]
          Wachttijd2018[i,5]<-Activiteiten2018Start$GGZInstelling[j]
          Wachttijd2018[i,6]<-Activiteiten2018Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2017, Activiteiten2017, Activiteiten2017Start)
gc()

#2019
index<-!(Activiteiten2019$RINPERSOON %in% DBC2018$RINPERSOON)
Activiteiten2019Start<-Activiteiten2019[index,]
index<-!(DBC2019$RINPERSOON %in% DBC2018$RINPERSOON)
DBC2019Start<-DBC2019[index,]
DBC2019Start<-DBC2019Start[order(DBC2019Start$RINPERSOON, DBC2019Start$GGZDBCBegindatum),]
DBC2019Start<-distinct(DBC2019Start, RINPERSOON, .keep_all = TRUE)
Activiteiten2019Start<-Activiteiten2019Start[order(Activiteiten2019Start$RINPERSOON, Activiteiten2019Start$GGZGZPZorgactiviteitDatum),]
DBC2019Start$GGZDBCBegindatum<-as.Date(paste(substring(DBC2019Start$GGZDBCBegindatum,1,4), "-",substring(DBC2019Start$GGZDBCBegindatum,5,6), "-",substring(DBC2019Start$GGZDBCBegindatum,7,8), sep=""))
Activiteiten2019Start$GGZGZPZorgactiviteitDatum<-as.Date(paste(substring(Activiteiten2019Start$GGZGZPZorgactiviteitDatum,1,4), "-",substring(Activiteiten2019Start$GGZGZPZorgactiviteitDatum,5,6), "-",substring(Activiteiten2019Start$GGZGZPZorgactiviteitDatum,7,8), sep=""))

Wachttijd2019<-as.data.frame(matrix(NA, nrow(DBC2019Start), 6)) #First column startdatum, second aanmeld wachttijd, third column behandel wachttijd
names(Wachttijd2019)<-c("AanmeldDatum", "Aanmeldtijd", "Behandeltijd", "Crisis", "GGZInstelling", "Behandelaar")
class(Wachttijd2019$AanmeldDatum)<-"Date"
j<-1
for(i in 1:nrow(DBC2019Start)){
  Wachttijd2019[i,1]<-DBC2019Start$GGZDBCBegindatum[i]
  while(DBC2019Start$RINPERSOON[i]==Activiteiten2019Start$RINPERSOON[j]){
    if(substring(Activiteiten2019Start$GGZGZPZorgactiviteit[j],5,5)=="6"){
      Wachttijd2019[i,4]<-1
      Wachttijd2019[i,6]<-Activiteiten2019Start$GGZGZPBeroepcode[j]
    }
    if(is.na(Wachttijd2019[i,2])&substring(Activiteiten2019Start$GGZGZPZorgactiviteit[j],5,5)=="2"){
      Wachttijd2019[i,2]<-Activiteiten2019Start$GGZGZPZorgactiviteitDatum[j]-DBC2019Start$GGZDBCBegindatum[i]
      Wachttijd2019[i,5]<-Activiteiten2019Start$GGZInstelling[j]
      Wachttijd2019[i,6]<-Activiteiten2019Start$GGZGZPBeroepcode[j]
    }
    else{
      if(is.na(Wachttijd2019[i,3])){
        if(substring(Activiteiten2019Start$GGZGZPZorgactiviteit[j],5,5)!="1"&substring(Activiteiten2019Start$GGZGZPZorgactiviteit[j],5,5)!="2"&substring(Activiteiten2019Start$GGZGZPZorgactiviteit[j],5,5)!="7"){
          Wachttijd2019[i,3]<-Activiteiten2019Start$GGZGZPZorgactiviteitDatum[j]-DBC2019Start$GGZDBCBegindatum[i]
          Wachttijd2019[i,5]<-Activiteiten2019Start$GGZInstelling[j]
          Wachttijd2019[i,6]<-Activiteiten2019Start$GGZGZPBeroepcode[j]
        }
      }
    }
    j<-j+1
  }
}
rm(DBC2018, Activiteiten2018, DBC2019, Activiteiten2019, Activiteiten2018Start, Activiteiten2019Start)
save.image("IntermediateFiles/WachttijdData.RData")

#Create time series for individuals waiting for care
#Health cost per year
#employment
#benefit receipt
#add demographic characteristics and educational attainment

Wachttijd2012<-cbind(DBC2012Start, Wachttijd2012)
Wachttijd2013<-cbind(DBC2013Start, Wachttijd2013)
Wachttijd2014<-cbind(DBC2014Start, Wachttijd2014)
Wachttijd2015<-cbind(DBC2015Start, Wachttijd2015)
Wachttijd2016<-cbind(DBC2016Start, Wachttijd2016)
Wachttijd2017<-cbind(DBC2017Start, Wachttijd2017)
Wachttijd2018<-cbind(DBC2018Start, Wachttijd2018)
Wachttijd2019<-cbind(DBC2019Start, Wachttijd2019)
rm(DBC2012Start,DBC2012, Activiteiten2012Start, Activiteiten2012)
rm(DBC2013Start,DBC2013, Activiteiten2013Start, Activiteiten2013)
rm(DBC2014Start,DBC2014, Activiteiten2014Start, Activiteiten2014)
rm(DBC2015Start,DBC2015, Activiteiten2015Start, Activiteiten2015)
rm(DBC2016Start,DBC2016, Activiteiten2016Start, Activiteiten2016)
rm(DBC2017Start,DBC2017, Activiteiten2017Start, Activiteiten2017)
rm(DBC2018Start,DBC2018, Activiteiten2018Start, Activiteiten2018)
rm(DBC2019Start,DBC2019, Activiteiten2019Start, Activiteiten2019)
rm(DBC2011, index)

#Add location information in year in which first contact took place
GBAAdres<- read_sav("G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2021BUSV1.sav")
GBAAdres%<>%
  dplyr::select(RINPERSOON, RINOBJECTNUMMER, GBADATUMAANVANGADRESHOUDING,GBADATUMEINDEADRESHOUDING)
Gebouwen <- read_sav("G:/BouwenWonen/VSLGWBTAB/VSLGWB2022TAB03V2.sav")
#dplyr::select municipality 2012-2019
Gebouwen%<>%
  dplyr::select(RINOBJECTNUMMER, gem2012, gem2013, gem2014, gem2015, gem2016, gem2017, gem2018, gem2019)

index2012<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20120101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20120101
GBAAdres2012<-GBAAdres[index2012,]
GBAAdres2012<-distinct(GBAAdres2012, RINPERSOON,  .keep_all = TRUE)
GBAAdres2012<-merge(GBAAdres2012, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2012<-merge(Wachttijd2012, GBAAdres2012, by="RINPERSOON", all.x=TRUE)
Wachttijd2012$gem2012[is.na(Wachttijd2012$gem2012)]<-"Onbekende woonplaats"
Wachttijd2012%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017, -gem2013, -gem2014, -gem2015, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2012)[12]<-"Gemeente"

index2013<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20130101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20130101
GBAAdres2013<-GBAAdres[index2013,]
GBAAdres2013<-distinct(GBAAdres2013, RINPERSOON,  .keep_all = TRUE)
GBAAdres2013<-merge(GBAAdres2013, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2013<-merge(Wachttijd2013, GBAAdres2013, by="RINPERSOON", all.x=TRUE)
Wachttijd2013$gem2013[is.na(Wachttijd2013$gem2013)]<-"Onbekende woonplaats"
Wachttijd2013%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2012, -gem2014, -gem2015, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2013)[12]<-"Gemeente"

index2014<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20140101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20140101
GBAAdres2014<-GBAAdres[index2014,]
GBAAdres2014<-distinct(GBAAdres2014, RINPERSOON,  .keep_all = TRUE)
GBAAdres2014<-merge(GBAAdres2014, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2014<-merge(Wachttijd2014, GBAAdres2014, by="RINPERSOON", all.x=TRUE)
Wachttijd2014$gem2014[is.na(Wachttijd2014$gem2014)]<-"Onbekende woonplaats"
Wachttijd2014%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2012, -gem2013, -gem2015, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2014)[12]<-"Gemeente"

index2015<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20150101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20150101
GBAAdres2015<-GBAAdres[index2015,]
GBAAdres2015<-distinct(GBAAdres2015, RINPERSOON,  .keep_all = TRUE)
GBAAdres2015<-merge(GBAAdres2015, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2015<-merge(Wachttijd2015, GBAAdres2015, by="RINPERSOON", all.x=TRUE)
Wachttijd2015$gem2015[is.na(Wachttijd2015$gem2015)]<-"Onbekende woonplaats"
Wachttijd2015%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2013, -gem2014, -gem2012, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2015)[12]<-"Gemeente"

index2016<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20160101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20160101
GBAAdres2016<-GBAAdres[index2016,]
GBAAdres2016<-distinct(GBAAdres2016, RINPERSOON,  .keep_all = TRUE)
GBAAdres2016<-merge(GBAAdres2016, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2016<-merge(Wachttijd2016, GBAAdres2016, by="RINPERSOON", all.x=TRUE)
Wachttijd2016$gem2016[is.na(Wachttijd2016$gem2016)]<-"Onbekende woonplaats"
Wachttijd2016%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2016)[12]<-"Gemeente"

index2017<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20170101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20170101
GBAAdres2017<-GBAAdres[index2017,]
GBAAdres2017<-distinct(GBAAdres2017, RINPERSOON,  .keep_all = TRUE)
GBAAdres2017<-merge(GBAAdres2017, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2017<-merge(Wachttijd2017, GBAAdres2017, by="RINPERSOON", all.x=TRUE)
Wachttijd2017$gem2017[is.na(Wachttijd2017$gem2017)]<-"Onbekende woonplaats"
Wachttijd2017%<>%
  dplyr::select(-gem2019, -gem2018, -gem2016,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2017)[12]<-"Gemeente"


index2018<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20180101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20180101
GBAAdres2018<-GBAAdres[index2018,]
GBAAdres2018<-distinct(GBAAdres2018, RINPERSOON,  .keep_all = TRUE)
GBAAdres2018<-merge(GBAAdres2018, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2018<-merge(Wachttijd2018, GBAAdres2018, by="RINPERSOON", all.x=TRUE)
Wachttijd2018$gem2018[is.na(Wachttijd2018$gem2018)]<-"Onbekende woonplaats"
Wachttijd2018%<>%
  dplyr::select(-gem2019, -gem2016, -gem2017,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2018)[12]<-"Gemeente"

index2019<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20190101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20190101
GBAAdres2019<-GBAAdres[index2019,]
GBAAdres2019<-distinct(GBAAdres2019, RINPERSOON,  .keep_all = TRUE)
GBAAdres2019<-merge(GBAAdres2019, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd2019<-merge(Wachttijd2019, GBAAdres2019, by="RINPERSOON", all.x=TRUE)
Wachttijd2019$gem2019[is.na(Wachttijd2019$gem2019)]<-"Onbekende woonplaats"
Wachttijd2019%<>%
  dplyr::select(-gem2016, -gem2018, -gem2017,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd2019)[12]<-"Gemeente"

rm(index2012, index2013, index2014, index2015, index2016, index2017, index2018, index2019)
rm(GBAAdres, GBAAdres2012, GBAAdres2013, GBAAdres2014, GBAAdres2015, GBAAdres2016, Gebouwen, GBAAdres2017, GBAAdres2018, GBAAdres2019)
save.image("IntermediateFiles/Wachttijd.RData")
load("IntermediateFiles/Wachttijd.RData")

#merge files
class(Wachttijd2012$Gemeente)<-"character"
class(Wachttijd2013$Gemeente)<-"character"
class(Wachttijd2014$Gemeente)<-"character"
class(Wachttijd2015$Gemeente)<-"character"
class(Wachttijd2016$Gemeente)<-"character"
class(Wachttijd2017$Gemeente)<-"character"
class(Wachttijd2018$Gemeente)<-"character"
class(Wachttijd2019$Gemeente)<-"character"
class(Wachttijd2012$GGZInstelling)<-"character"
class(Wachttijd2013$GGZInstelling)<-"character"
class(Wachttijd2014$GGZInstelling)<-"character"
class(Wachttijd2015$GGZInstelling)<-"character"
class(Wachttijd2016$GGZInstelling)<-"character"
class(Wachttijd2017$GGZInstelling)<-"character"
class(Wachttijd2018$GGZInstelling)<-"character"
class(Wachttijd2019$GGZInstelling)<-"character"
class(Wachttijd2012$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2013$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2014$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2015$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2016$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2017$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2018$GGZDBCHoofddiagnoseDSMIV)<-"character"
class(Wachttijd2019$GGZDBCHoofddiagnoseDSMIV)<-"character"
Wachttijd<-rbind(Wachttijd2012, Wachttijd2013,Wachttijd2014, Wachttijd2015, Wachttijd2016, Wachttijd2017, Wachttijd2018, Wachttijd2019)
Wachttijd<-distinct(Wachttijd, RINPERSOON, .keep_all = TRUE)
rm(Wachttijd2012, Wachttijd2013, Wachttijd2014, Wachttijd2015, Wachttijd2016, Wachttijd2017, Wachttijd2018, Wachttijd2019)
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]

#Create month index of first moment of contact with mental healthcare provider (indexStartGGZ)
IndexStartGGZ<-(as.numeric(substring(Wachttijd$AanmeldDatum, 1, 4))-2012)*12+as.numeric(substring(Wachttijd$AanmeldDatum, 6, 7))
Wachttijd<-cbind(Wachttijd, IndexStartGGZ)
Wachttijd%<>%
  dplyr::select(-GGZDBCtrajectnummer, -GGZDBCBegindatum, -GGZDBCStartdatumZTR)

#Delete starts prior to 2012: For 2011 it cannot be determined whether people start treatment or continu treatment
index<-Wachttijd$IndexStartGGZ<1
Wachttijd<-Wachttijd[!index,]
rm(Activiteiten2012, Activiteiten2012Start, Activiteiten2013, Activiteiten2013Start, Activiteiten2014, Activiteiten2014Start, Activiteiten2015, Activiteiten2015Start, Activiteiten2016, Activiteiten2016Start)
rm(DBC2011, DBC2012, DBC2012Start, DBC2013, DBC2013Start, DBC2014, DBC2014Start, DBC2015, DBC2015Start, DBC2016, DBC2016Start)
rm(index, IndexStartGGZ)
save.image("IntermediateFiles/WachttijdenAll.RData")
load("IntermediateFiles/WachttijdenAll.RData")

#Sample selection
Tab1SampleSelection<-matrix(NA,1,6)
Tab1SampleSelection[1,1]<-nrow(Wachttijd)
#Delete individuals without behandeltijd
index<-!is.na(Wachttijd$Behandeltijd)
Wachttijd<-Wachttijd[index,]
Tab1SampleSelection[1,2]<-nrow(Wachttijd)
#Delete negative or long behandeltijd
index<-Wachttijd$Behandeltijd<365&Wachttijd$Behandeltijd>0
Wachttijd<-Wachttijd[index,]
Tab1SampleSelection[1,3]<-nrow(Wachttijd)
#Delete no intake time
index<-!is.na(Wachttijd$Aanmeldtijd)
Wachttijd<-Wachttijd[index,]
Tab1SampleSelection[1,4]<-nrow(Wachttijd)

#Add demographics (gender, age, nationality)
#Load 2012 till 2019
start <- 2012
for(k in 0:7){
  y<-start+k
  if(y==2016){
    file = paste("G:/Bevolking/GBAPERSOONTAB/", y, "/GBAPERSOONTAB", y, "V1.sav", sep = "")
  }
  if(y==2018){
    file = paste("G:/Bevolking/GBAPERSOONTAB/", y, "/GBAPERSOON", y, "TABV2.sav",sep = "")
  }
  if(y!=2016&y!=2018){
    file = paste("G:/Bevolking/GBAPERSOONTAB/", y, "/GBAPERSOON", y, "TABV1.sav",sep = "")
  }
  GBAYear<-read_sav(file)
  GBAYear %<>%
    mutate(Man = as.integer(case_when(
      GBAGESLACHT == "1" ~ 1,
      GBAGESLACHT == "2" ~ 0,
      TRUE ~ 9
    ))) %>% 
    mutate(GBAGEBOORTEJAAR = as.numeric(GBAGEBOORTEJAAR)) %>%
    dplyr::select(c("RINPERSOON", "Man", "GBAGEBOORTEJAAR", "GBAGENERATIE", "GBAGEBOORTELAND"))
  
  if(k==0){
    GBA<-GBAYear
  }
  if(k!=0){
    GBA<-rbind(GBA, GBAYear)
  }
  GBA %<>%
    distinct(RINPERSOON, .keep_all = TRUE)
  rm(GBAYear)
  gc()
}

index<-GBA$RINPERSOON %in% Wachttijd$RINPERSOON
GBA<-GBA[index,]
index<-Wachttijd$RINPERSOON %in% GBA$RINPERSOON
Wachttijd<-Wachttijd[index,]
Wachttijd<-merge(Wachttijd, GBA, by="RINPERSOON")
rm(GBA)
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
rm(index)

##### Load education data
Educ <- read_sav("G:/Onderwijs/HOOGSTEOPLTAB/2019/HOOGSTEOPL2019TABV2.sav") # 11.9 mil obs
Wachttijd %<>%
  merge(Educ %<>%
          dplyr::select(RINPERSOON, OPLNIVSOI2021AGG4HBmetNIRWO, OPLNIVSOI2021AGG4HGmetNIRWO),
        by = "RINPERSOON", all.x = TRUE)
rm(Educ)

#Add indicator for year and age
JaarDummy<-substring(Wachttijd$AanmeldDatum,1,4)
Wachttijd<-cbind(Wachttijd, JaarDummy)
rm(JaarDummy)
Leeftijd<-as.numeric(as.character(Wachttijd$JaarDummy))-as.numeric(Wachttijd$GBAGEBOORTEJAAR)
Wachttijd<-cbind(Wachttijd, Leeftijd)
rm(Leeftijd)

#Focus on 18-65 year old
index<-!is.na(Wachttijd$Leeftijd)
Wachttijd<-Wachttijd[index,]
index<-Wachttijd$Leeftijd>17&Wachttijd$Leeftijd<66
Wachttijd<-Wachttijd[index,]
save.image("IntermediateFiles/Wachttijden.RData")
load("IntermediateFiles/Wachttijden.RData")

#Create time series of patients
#### Time series mental, non-mental and pharmaceutical health costs ##############
for(y in 2009:2020){
  if(y==2017|y==2018){
    costs <- read_sav(paste("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/", y, "/ZVWZORGKOSTEN", y,"TABV2.sav", sep = ""))
  }
  else{
    costs <- read_sav(paste("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/", y, "/ZVWZORGKOSTEN", y,"TABV1.sav", sep = ""))
  }
  index<-costs$RINPERSOON %in% Wachttijd$RINPERSOON
  costs<-costs[index,]
  MentalCost<-rowSums(cbind(costs$ZVWKEERSTELIJNSPSYCHO,costs$ZVWKGGZ,costs$ZVWKGENBASGGZ,costs$ZVWKSPECGGZ), na.rm=TRUE)
  PhysicalCost<-rowSums(cbind(costs$ZVWKHUISARTS,costs$ZVWKZIEKENHUIS,costs$ZVWKPARAMEDISCH,costs$ZVWKWYKVERPLEGING), na.rm=TRUE)
  Medicijnen<-costs$ZVWKPARAMEDISCH
  costs<-as.data.frame(cbind(costs$RINPERSOON,MentalCost, PhysicalCost, Medicijnen))
  names(costs)<-c("RINPERSOON", paste("MentalCost",y, sep=""), paste("PhysicalCost",y, sep=""), paste("Medicijnen",y, sep=""))
  Wachttijd<-merge(Wachttijd, costs, by="RINPERSOON", all.x=TRUE)
  rm(costs, MentalCost, PhysicalCost, index)
}
save.image("IntermediateFiles/CostData.RData")
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
rm(Medicijnen)
MentalCost<-Wachttijd[,c(19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52)]
PhysicalCost<-Wachttijd[,c(20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53)]
MedicijnCost<-Wachttijd[,c(21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54)]
for(j in 1:12){
  MentalCost[,j]<-as.numeric(as.character(MentalCost[,j]))
  PhysicalCost[,j]<-as.numeric(as.character(PhysicalCost[,j]))
  MedicijnCost[,j]<-as.numeric(as.character(MedicijnCost[,j]))
}
MentalCost<-as.matrix(MentalCost)
PhysicalCost<-as.matrix(PhysicalCost)
MedicijnCost<-as.matrix(MedicijnCost)

save.image("IntermediateFiles/TijdreeksenCost.RData")
load("IntermediateFiles/TijdreeksenCost.RData")

#####receipt of various types of benefits and employment indicator. Werknemer=employment, WW= unemployment benefits, Bijstand= social assistance, ziekteAO= sickness/disability benefits, SocVerOv=Other social benefits, Scholier=student
rm(list=ls())
load("IntermediateFiles/Wachttijden.RData")
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
Wachttijd%<>%
  dplyr::select(RINPERSOON)
SECMBUSV20191<-read_sav("G:/InkomenBestedingen/SECMBUS/SECMBUS2021V1.sav")
SECMBUSV20191%<>%
  dplyr::select(RINPERSOON,AANVSECM,EINDSECM,XKOPPELWERKNSECM,XKOPPELWERKLUITKSECM,XKOPPELBIJSTANDSECM,XKOPPELSOCVOORZOVSECM,XKOPPELZIEKTEAOSECM,XKOPPELSCHOLSTUDSECM)
index<-SECMBUSV20191$RINPERSOON %in% Wachttijd$RINPERSOON
SECMBUS<-SECMBUSV20191[index,]
rm(SECMBUSV20191)

#dplyr::select entries which end after january 2004
SECMBUS<-subset(SECMBUS, EINDSECM>=20040101)

#select entries with relevant receipt
index<-SECMBUS$XKOPPELWERKNSECM=="1"|SECMBUS$XKOPPELWERKLUITKSECM=="1"|SECMBUS$XKOPPELBIJSTANDSECM=="1"|SECMBUS$XKOPPELSOCVOORZOVSECM=="1"|SECMBUS$XKOPPELZIEKTEAOSECM=="1"|SECMBUS$XKOPPELSCHOLSTUDSECM=="1"
SECMBUS<-SECMBUS[index,]

#order
SECMBUS<-SECMBUS[order(SECMBUS$RINPERSOON),]

#Matrices for various benefits from januari 2004 untill december 2020
Werknemer<-matrix(0, length(Wachttijd$RINPERSOON),216)
WW<-matrix(0, length(Wachttijd$RINPERSOON),216)
Bijstand<-matrix(0, length(Wachttijd$RINPERSOON),216)
SocVerOv<-matrix(0, length(Wachttijd$RINPERSOON),216)
ZiekteAO<-matrix(0, length(Wachttijd$RINPERSOON),216)
Scholier<-matrix(0, length(Wachttijd$RINPERSOON),216)

indexAanvang<-as.data.frame((as.numeric(substr(SECMBUS$AANVSECM,1,4))-2004)*12+as.numeric(substr(SECMBUS$AANVSECM,5,6)))
indexEinde<-as.data.frame((as.numeric(substr(SECMBUS$EINDSECM,1,4))-2004)*12+as.numeric(substr(SECMBUS$EINDSECM,5,6)))
names(indexAanvang)[1]<-"indexAanvang"
names(indexEinde)[1]<-"indexEinde"

indexAanvang$indexAanvang[which(indexAanvang$indexAanvang<1)]<-1

j<-1
for(i in 1:length(Wachttijd$RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while((Wachttijd$RINPERSOON[i]==SECMBUS$RINPERSOON[j])&((j)<nrow(SECMBUS))){
    if(as.character(SECMBUS$XKOPPELWERKNSECM[j])==1){
      Werknemer[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELWERKLUITKSECM[j])==1){
      WW[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELBIJSTANDSECM[j])==1){
      Bijstand[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELSOCVOORZOVSECM[j])==1){
      SocVerOv[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELZIEKTEAOSECM[j])==1){
      ZiekteAO[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELSCHOLSTUDSECM[j])==1){
      Scholier[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    j<-j+1
  }
}

NoIncome<-matrix(0, nrow(Wachttijd), 216)
for(i in 1:nrow(Wachttijd)){
  for(j in 1:216){
    if(Werknemer[i,j]==0&WW[i,j]==0&Bijstand[i,j]==0&SocVerOv[i,j]==0&ZiekteAO[i,j]==0){
      NoIncome[i,j]<-1
    }
  }
}

rm(SECMBUS, indexAanvang, indexEinde)
rm(Wachttijd)
save.image("IntermediateFiles/TijdreeksenSECM.RData")

#ADD EARNINGS
rm(list=ls())
load("IntermediateFiles/Wachttijden.RData")
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
Wachttijd%<>%
  dplyr::select(RINPERSOON)

#Load Polis Data
POLIS2009 <- read_sav(file="G:/Polis/POLISBUS/2009/POLISBUS2009V2.sav", col_select=c(RINPERSOON, AANVBUS, BASISLOON, BASISUREN))
index<-POLIS2009$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2009<-POLIS2009[index,]
gc()
POLIS2010 <- read_sav(file="G:/Spolis/SPOLISBUS/2010/SPOLISBUS2010V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2010$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2010<-POLIS2010[index,]
gc()
POLIS2011 <- read_sav(file="G:/Spolis/SPOLISBUS/2011/SPOLISBUS2011V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2011$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2011<-POLIS2011[index,]
gc()
POLIS2012 <- read_sav(file="G:/Spolis/SPOLISBUS/2012/SPOLISBUS2012V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2012$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2012<-POLIS2012[index,]
gc()
POLIS2013 <- read_sav(file="G:/Spolis/SPOLISBUS/2013/SPOLISBUS2013V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2013$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2013<-POLIS2013[index,]
gc()
POLIS2014 <- read_sav(file="G:/Spolis/SPOLISBUS/2014/SPOLISBUS 2014V1.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2014$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2014<-POLIS2014[index,]
gc()
POLIS2015 <- read_sav(file="G:/Spolis/SPOLISBUS/2015/SPOLISBUS 2015V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2015$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2015<-POLIS2015[index,]
gc()
POLIS2016 <- read_sav(file="G:/Spolis/SPOLISBUS/2016/SPOLISBUS2016V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2016$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2016<-POLIS2016[index,]
gc()
POLIS2017 <- read_sav(file="G:/Spolis/SPOLISBUS/2017/SPOLISBUS2017V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2017$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2017<-POLIS2017[index,]
gc()
POLIS2018 <- read_sav(file="G:/Spolis/SPOLISBUS/2018/SPOLISBUS2018V5.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2018$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2018<-POLIS2018[index,]
gc()
POLIS2019 <- read_sav(file="G:/Spolis/SPOLISBUS/2019/SPOLISBUS2019V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2019$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2019<-POLIS2019[index,]
gc()
POLIS2020 <- read_sav(file="G:/Spolis/SPOLISBUS/2020/SPOLISBUS2020V5.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2020$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2020<-POLIS2020[index,]
gc()
POLIS2021 <- read_sav(file="G:/Spolis/SPOLISBUS/2021/SPOLISBUS2021V5.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2021$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2021<-POLIS2021[index,]
gc()
POLIS2022 <- read_sav(file="G:/Spolis/SPOLISBUS/2022/SPOLISBUS2022V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2022$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2022<-POLIS2022[index,]
gc()

names(POLIS2009)<-names(POLIS2021)
save.image("IntermediateFiles/POLISDataGGZ.RData")
load("IntermediateFiles/POLISDataGGZ.RData")

PolisData<-rbind(POLIS2009, POLIS2010, POLIS2011, POLIS2012, POLIS2013, POLIS2014, POLIS2015, POLIS2016, POLIS2017, POLIS2018, POLIS2019,POLIS2020, POLIS2021, POLIS2022)
PolisData<-PolisData[order(PolisData$RINPERSOON),]
indexAanvang<-as.data.frame((as.numeric(substr(PolisData$SDATUMAANVANGIKO,1,4))-2009)*12+as.numeric(substr(PolisData$SDATUMAANVANGIKO,5,6)))
names(indexAanvang)<-"indexAanvang"
rm(POLIS2009, POLIS2010, POLIS2011, POLIS2012, POLIS2013, POLIS2014, POLIS2015, POLIS2016, POLIS2017, POLIS2018, POLIS2019,POLIS2020, POLIS2021, POLIS2022)

#Uren=Monthly working hours
Earnings<-matrix(0, nrow(Wachttijd), 168)
Uren<-matrix(0, nrow(Wachttijd), 168)

time<-Sys.time()
j<-1
for(i in 1:nrow(Wachttijd)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while((Wachttijd$RINPERSOON[i]==PolisData$RINPERSOON[j])&((j)<nrow(PolisData))){
    Earnings[i,indexAanvang$indexAanvang[j]]<-Earnings[i,indexAanvang$indexAanvang[j]]+PolisData$SBASISLOON[j]
    Uren[i,indexAanvang$indexAanvang[j]]<-Uren[i,indexAanvang$indexAanvang[j]]+PolisData$SBASISUREN[j]
    j<-j+1
  }
}
Duur<-Sys.time()-time
#change last 3 as they are not fully captured in the data yet
Earnings[,166:168]<-NA
Uren[,166:168]<-NA

rm(PolisData, indexAanvang, index)
save.image("IntermediateFiles/TijdreeksenPolis.RData")

#ADD mental healthcare treatment minutes
rm(list=ls())
load("IntermediateFiles/Wachttijden.RData")
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
Wachttijd%<>%
  dplyr::select(RINPERSOON)

Activiteiten2012 <- read_sav("G:/GezondheidWelzijn/GGZDBCGELEVERDZORGPROFIELTAB/GGZDBCGELEVERDZORGPROFIEL2012TABV1.sav")
Activiteiten2012%<>%
  dplyr::select(RINPERSOON, GGZInstelling, GGZDBCtrajectnummer, GGZGZPZorgactiviteit, GGZGZPZorgactiviteitDatum, GGZGZPDirecttijd)
Activiteiten2012<-Activiteiten2012[!is.na(Activiteiten2012$GGZGZPDirecttijd),]
Activiteiten2012<-Activiteiten2012[Activiteiten2012$GGZGZPDirecttijd>0,]
index<-Activiteiten2012$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2012<-Activiteiten2012[index,]
Activiteiten2013 <- read_sav("G:/GezondheidWelzijn/GGZDBCGELEVERDZORGPROFIELTAB/GGZDBCGELEVERDZORGPROFIEL2013TABV1.sav")
Activiteiten2013%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummer, GGZGZPZorgactiviteit, GGZGZPZorgactiviteitDatum, GGZGZPDirecttijd)
Activiteiten2013<-Activiteiten2013[!is.na(Activiteiten2013$GGZGZPDirecttijd),]
Activiteiten2013<-Activiteiten2013[Activiteiten2013$GGZGZPDirecttijd>0,]
index<-Activiteiten2013$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2013<-Activiteiten2013[index,]
Activiteiten2014 <- read_sav("G:/GezondheidWelzijn/GGZDBCGELEVERDZORGPROFIELTAB/GGZDBCGELEVERDZORGPROFIEL2014TABV2.sav")
Activiteiten2014%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummer, GGZGZPZorgactiviteit, GGZGZPZorgactiviteitDatum, GGZGZPDirecttijd)
Activiteiten2014<-Activiteiten2014[!is.na(Activiteiten2014$GGZGZPDirecttijd),]
Activiteiten2014<-Activiteiten2014[Activiteiten2014$GGZGZPDirecttijd>0,]
index<-Activiteiten2014$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2014<-Activiteiten2014[index,]
Activiteiten2015 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2015TABV1.sav")
Activiteiten2015%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG)
names(Activiteiten2015)<-names(Activiteiten2014)
Activiteiten2015<-Activiteiten2015[!is.na(Activiteiten2015$GGZGZPDirecttijd),]
Activiteiten2015<-Activiteiten2015[Activiteiten2015$GGZGZPDirecttijd>0,]
index<-Activiteiten2015$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2015<-Activiteiten2015[index,]
Activiteiten2016 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2016TABV1.sav")
Activiteiten2016%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG)
names(Activiteiten2016)<-names(Activiteiten2014)
Activiteiten2016<-Activiteiten2016[!is.na(Activiteiten2016$GGZGZPDirecttijd),]
Activiteiten2016<-Activiteiten2016[Activiteiten2016$GGZGZPDirecttijd>0,]
index<-Activiteiten2016$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2016<-Activiteiten2016[index,]
Activiteiten2017 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2017TABV2.sav")
Activiteiten2017%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG)
names(Activiteiten2017)<-names(Activiteiten2014)
Activiteiten2017<-Activiteiten2017[!is.na(Activiteiten2017$GGZGZPDirecttijd),]
Activiteiten2017<-Activiteiten2017[Activiteiten2017$GGZGZPDirecttijd>0,]
index<-Activiteiten2017$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2017<-Activiteiten2017[index,]
Activiteiten2018 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2018TABV2.sav")
Activiteiten2018%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG)
names(Activiteiten2018)<-names(Activiteiten2014)
Activiteiten2018<-Activiteiten2018[Activiteiten2018$GGZGZPDirecttijd>0,]
index<-Activiteiten2018$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2018<-Activiteiten2018[index,]
Activiteiten2019 <- read_sav("G:/GezondheidWelzijn/GGZDBCZRGPROFIELHOOFDDIAGTAB/GGZDBCZrgprofielHOOFDDIAG2019TABV1.sav")
Activiteiten2019%<>%
  dplyr::select(RINPERSOON,GGZInstelling, GGZDBCtrajectnummerHOOFDDIAG, GGZGZPZorgactiviteitHOOFDDIAG, GGZGZPZorgactiviteitDatumHOOFDDIAG, GGZGZPDirecttijdHOOFDDIAG)
names(Activiteiten2019)<-names(Activiteiten2014)
Activiteiten2019<-Activiteiten2019[!is.na(Activiteiten2019$GGZGZPDirecttijd),]
Activiteiten2019<-Activiteiten2019[Activiteiten2019$GGZGZPDirecttijd>0,]
index<-Activiteiten2019$RINPERSOON %in% Wachttijd$RINPERSOON
Activiteiten2019<-Activiteiten2019[index,]

GGZActiviteiten<-rbind(Activiteiten2012, Activiteiten2013, Activiteiten2014, Activiteiten2015, Activiteiten2016, Activiteiten2017, Activiteiten2018, Activiteiten2019)

#zoom in on actual treatment: act_3=treatment, act_4=counselling, act_8=in-patient, act_9=out-patient 
index<-substring(GGZActiviteiten$GGZGZPZorgactiviteit, 1,5)=="act_3"|substring(GGZActiviteiten$GGZGZPZorgactiviteit, 1,5)=="act_4"|substring(GGZActiviteiten$GGZGZPZorgactiviteit, 1,5)=="act_8"|substring(GGZActiviteiten$GGZGZPZorgactiviteit, 1,5)=="act_9"
GGZActiviteiten<-GGZActiviteiten[index,]
GGZActiviteiten<-GGZActiviteiten[order(GGZActiviteiten$RINPERSOON),]
rm(Activiteiten2012, Activiteiten2013, Activiteiten2014, Activiteiten2015, Activiteiten2016, Activiteiten2017, Activiteiten2018, Activiteiten2019)

indexAanvang<-as.data.frame((as.numeric(substr(GGZActiviteiten$GGZGZPZorgactiviteitDatum,1,4))-2012)*12+as.numeric(substr(GGZActiviteiten$GGZGZPZorgactiviteitDatum,5,6)))
names(indexAanvang)[1]<-"indexAanvang"

TreatmentMinutes<-matrix(0, nrow(Wachttijd), 96)

j<-1
for(i in 1:nrow(Wachttijd)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while((Wachttijd$RINPERSOON[i]==GGZActiviteiten$RINPERSOON[j])&((j)<nrow(GGZActiviteiten))){
    if(indexAanvang$indexAanvang[j]>0&indexAanvang$indexAanvang[j]<=96){
      TreatmentMinutes[i,indexAanvang$indexAanvang[j]]<-TreatmentMinutes[i,indexAanvang$indexAanvang[j]]+GGZActiviteiten$GGZGZPDirecttijd[j]
    }
    j<-j+1
  }
}

#change outliers to fulltime treatment
rm(GGZActiviteiten, indexAanvang, index)
for(i in 1:nrow(Wachttijd)){
  for(j in 1:60){
    if(TreatmentMinutes[i,j]>14400){
      TreatmentMinutes[i,j]<-14400
    }
  }
}
save.image("IntermediateFiles/TijdreeksenGGZ1.RData")

#Merge timeseries
rm(list=ls())
load("IntermediateFiles/TijdreeksenSECM.RData")
load("IntermediateFiles/TijdreeksenPolis.RData")
load("IntermediateFiles/TijdreeksenGGZ1.RData")
load("IntermediateFiles/TijdreeksenCost.RData")
load("IntermediateFiles/Wachttijden.RData")

#Plots of measures relative to application data
BijstandGGZ<-matrix(NA, nrow(Wachttijd), 396)
ScholierGGZ<-matrix(NA, nrow(Wachttijd), 396)
SocVerOvGGZ<-matrix(NA, nrow(Wachttijd), 396)
WerknemerGGZ<-matrix(NA, nrow(Wachttijd), 396)
WWGGZ<-matrix(NA, nrow(Wachttijd), 396)
ZiekteAOGGZ<-matrix(NA, nrow(Wachttijd), 396)
NoIncomeGGZ<-matrix(NA, nrow(Wachttijd), 396)
MentalCostGGZ<-matrix(NA, nrow(Wachttijd), 24)
PhysicalCostGGZ<-matrix(NA, nrow(Wachttijd), 24)
MedicijnenGGZ<-matrix(NA, nrow(Wachttijd), 24)
TreatmentMinutesGGZ<-matrix(NA, nrow(Wachttijd), 192)
UrenGGZ<-matrix(NA, nrow(Wachttijd), 396)
EarningsGGZ<-matrix(NA, nrow(Wachttijd), 396)

#193 is month of first contact (97 for treatment minutes). 12 is year of first contact for healthcare expenditures
for(i in 1:nrow(Wachttijd)){
  if(!is.na(Wachttijd$IndexStartGGZ[i])){
    if(Wachttijd$IndexStartGGZ[i]>0&Wachttijd$IndexStartGGZ[i]<97){
      BijstandGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-Bijstand[i,]
      ScholierGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-Scholier[i,]
      SocVerOvGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-SocVerOv[i,]
      WerknemerGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-Werknemer[i,]
      WWGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-WW[i,]
      ZiekteAOGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-ZiekteAO[i,]
      NoIncomeGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+96)):(349-(Wachttijd$IndexStartGGZ[i]+96)+60)]<-NoIncome[i,]
      MentalCostGGZ[i,(10-ceiling(Wachttijd$IndexStartGGZ[i]/12)):(21-ceiling(Wachttijd$IndexStartGGZ[i]/12))]<-MentalCost[i,]
      PhysicalCostGGZ[i,(10-ceiling(Wachttijd$IndexStartGGZ[i]/12)):(21-ceiling(Wachttijd$IndexStartGGZ[i]/12))]<-PhysicalCost[i,]
      MedicijnenGGZ[i,(10-ceiling(Wachttijd$IndexStartGGZ[i]/12)):(21-ceiling(Wachttijd$IndexStartGGZ[i]/12))]<-MedicijnCost[i,]
      TreatmentMinutesGGZ[i,(98-(Wachttijd$IndexStartGGZ[i])):(193-(Wachttijd$IndexStartGGZ[i]))]<-TreatmentMinutes[i,]
      UrenGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+36)):(361-(Wachttijd$IndexStartGGZ[i]+36))]<-Uren[i,]
      EarningsGGZ[i,(194-(Wachttijd$IndexStartGGZ[i]+36)):(361-(Wachttijd$IndexStartGGZ[i]+36))]<-Earnings[i,]
      if(i%%1000==0){
        gc()
      }
    }
  }
}

#Select 72 pre-months+24 pre-pre untill 96 post months
BijstandGGZ<-BijstandGGZ[, 97:288]
ScholierGGZ<-ScholierGGZ[, 97:288]
SocVerOvGGZ<-SocVerOvGGZ[, 97:288]
WerknemerGGZ<-WerknemerGGZ[, 97:288]
WWGGZ<-WWGGZ[, 97:288]
ZiekteAOGGZ<-ZiekteAOGGZ[, 97:288]
NoIncomeGGZ<-NoIncomeGGZ[, 97:288]
EarningsGGZ<-EarningsGGZ[, 97:288]
UrenGGZ<-UrenGGZ[, 97:288]

rm(Bijstand, Scholier, SocVerOv, Werknemer, WW, ZiekteAO, NoIncome, MentalCost, PhysicalCost, MedicijnCost, TreatmentMinutes, Uren, Earnings)
save.image("IntermediateFiles/FullDataStep1.RData")
load("IntermediateFiles/FullDataStep1.RData")
#add months of year
GBAPERSOON2014 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2014/GBAPERSOON2014TABV1.sav")
GBAPERSOON2014%<>%
  dplyr::select(RINPERSOON, GBAGEBOORTEMAAND)
Wachttijd<-merge(Wachttijd, GBAPERSOON2014, by="RINPERSOON", all.x=TRUE)
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
rm(GBAPERSOON2014)

#Make age in months in january 2012
MonthBirth<-(2012-as.numeric(as.character(Wachttijd$GBAGEBOORTEJAAR)))*12-(13-as.numeric(as.character(Wachttijd$GBAGEBOORTEMAAND)))
AgeTreatment<-MonthBirth+Wachttijd$IndexStartGGZ
AgeStart<-MonthBirth+Wachttijd$IndexStartGGZ-96
AgeEnd<-MonthBirth+Wachttijd$IndexStartGGZ+96

#Delete if individuals are younger than 18 or older than 65
index<-is.na(AgeTreatment)|(AgeTreatment<216|AgeTreatment>780)
BijstandGGZ<-BijstandGGZ[!index,]
ScholierGGZ<-ScholierGGZ[!index,]
SocVerOvGGZ<-SocVerOvGGZ[!index,]
WerknemerGGZ<-WerknemerGGZ[!index,]
WWGGZ<-WWGGZ[!index,]
ZiekteAOGGZ<-ZiekteAOGGZ[!index,]
NoIncomeGGZ<-NoIncomeGGZ[!index,]
MentalCostGGZ<-MentalCostGGZ[!index,]
PhysicalCostGGZ<-PhysicalCostGGZ[!index,]
MedicijnenGGZ<-MedicijnenGGZ[!index,]
TreatmentMinutesGGZ<-TreatmentMinutesGGZ[!index,]
UrenGGZ<-UrenGGZ[!index,]
EarningsGGZ<-EarningsGGZ[!index,]
Wachttijd<-Wachttijd[!index,]
AgeTreatment<-AgeTreatment[!index]
AgeStart<-AgeStart[!index]
AgeEnd<-AgeEnd[!index]
rm(index)

#Delete employment observations if individuals are younger than 18 or older than 65
for(i in 1:nrow(Wachttijd)){
  if(AgeStart[i]<216){
    WerknemerGGZ[i,1:(216-AgeStart[i])]<-NA
    BijstandGGZ[i,1:(216-AgeStart[i])]<-NA
    EarningsGGZ[i,1:(216-AgeStart[i])]<-NA
    NoIncomeGGZ[i,1:(216-AgeStart[i])]<-NA
    ScholierGGZ[i,1:(216-AgeStart[i])]<-NA
    SocVerOvGGZ[i,1:(216-AgeStart[i])]<-NA
    UrenGGZ[i,1:(216-AgeStart[i])]<-NA
    WWGGZ[i,1:(216-AgeStart[i])]<-NA
    ZiekteAOGGZ[i,1:(216-AgeStart[i])]<-NA
  }
  if(AgeEnd[i]>780){
    WerknemerGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    BijstandGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    EarningsGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    NoIncomeGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    ScholierGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    SocVerOvGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    UrenGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    WWGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    ZiekteAOGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
  }
}

rm(MonthBirth, AgeStart, AgeEnd, AgeTreatment)
Wachttijd%<>%
  dplyr::select(-GBAGEBOORTEMAAND)

#selection on age
Tab1SampleSelection[1,5]<-nrow(Wachttijd)

#Delete individuals with pre-start mental healthcare spending
index<-rowSums(MentalCostGGZ[,9:11], na.rm=TRUE)<=0
BijstandGGZ<-BijstandGGZ[index,]
ScholierGGZ<-ScholierGGZ[index,]
SocVerOvGGZ<-SocVerOvGGZ[index,]
WerknemerGGZ<-WerknemerGGZ[index,]
WWGGZ<-WWGGZ[index,]
ZiekteAOGGZ<-ZiekteAOGGZ[index,]
NoIncomeGGZ<-NoIncomeGGZ[index,]
MentalCostGGZ<-MentalCostGGZ[index,]
PhysicalCostGGZ<-PhysicalCostGGZ[index,]
MedicijnenGGZ<-MedicijnenGGZ[index,]
TreatmentMinutesGGZ<-TreatmentMinutesGGZ[index,]
UrenGGZ<-UrenGGZ[index,]
EarningsGGZ<-EarningsGGZ[index,]
Wachttijd<-Wachttijd[index,]
rm(index)

Tab1SampleSelection[1,6]<-nrow(Wachttijd)
write.xlsx(Tab1SampleSelection, "FinalOutput/Tab1.xlsx")

save.image("IntermediateFiles/FullDataStep1Extra.RData")

#Construct matched sample not receiving mental health treatment as control group
load("IntermediateFiles/FullDataStep1Extra.RData")
rm(BijstandGGZ, ScholierGGZ, SocVerOvGGZ, WerknemerGGZ, WWGGZ, ZiekteAOGGZ, NoIncomeGGZ, MentalCostGGZ, PhysicalCostGGZ, MedicijnenGGZ, TreatmentMinutesGGZ, UrenGGZ, EarningsGGZ)

#Load mental healthcare data
DBC2011 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2011TABV4.sav")
DBC2011%<>%
  dplyr::select(RINPERSOON)
DBC2012 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2012TABV3.sav")
DBC2012%<>%
  dplyr::select(RINPERSOON)
DBC2013 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2013TABV3.sav")
DBC2013%<>%
  dplyr::select(RINPERSOON)
DBC2014 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/GGZDBCtrajecten2014TABV2.sav")
DBC2014%<>%
  dplyr::select(RINPERSOON)
DBC2015 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2015TABV2.sav")
DBC2015%<>%
  dplyr::select(RINPERSOON)
DBC2016 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2016TABV2.sav")
DBC2016%<>%
  dplyr::select(RINPERSOON)
DBC2017 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2017TABV2.sav")
DBC2017%<>%
  dplyr::select(RINPERSOON)
DBC2018 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2018TABV2.sav")
DBC2018%<>%
  dplyr::select(RINPERSOON)
DBC2019 <- read_sav("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/GGZDBCtrajectenHOOFDDIAG2019TABV2.sav")
DBC2019%<>%
  dplyr::select(RINPERSOON)

GGZ20112019<-unique(c(DBC2011$RINPERSOON, DBC2012$RINPERSOON,DBC2013$RINPERSOON,DBC2014$RINPERSOON,DBC2015$RINPERSOON,DBC2016$RINPERSOON,DBC2017$RINPERSOON,DBC2018$RINPERSOON,DBC2019$RINPERSOON))
rm(DBC2011, DBC2012, DBC2013, DBC2014, DBC2015, DBC2016, DBC2017, DBC2018, DBC2019)

#load demographic information
GBAPERSOON2014 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2014/GBAPERSOON2014TABV1.sav")
index<-GBAPERSOON2014$RINPERSOON %in% GGZ20112019
NoTreatmentSample<-GBAPERSOON2014[!index,]
rm(GBAPERSOON2014)
#Focus on adults aged 18+ in 2012 en 65< in 2019
index<-NoTreatmentSample$GBAGEBOORTEJAAR<=2001&NoTreatmentSample$GBAGEBOORTEJAAR>=1947
NoTreatmentSample<-NoTreatmentSample[index,]

#Add education
Educ <- read_sav("G:/Onderwijs/HOOGSTEOPLTAB/2019/HOOGSTEOPL2019TABV2.sav") # 11.9 mil obs
NoTreatmentSample %<>%
  merge(Educ %<>%
          dplyr::select(RINPERSOON, OPLNIVSOI2021AGG4HBmetNIRWO),
        by = "RINPERSOON", all.x = TRUE)
rm(Educ)

#select variables
Wachttijd%<>%
  dplyr::select(RINPERSOON, IndexStartGGZ, Man, GBAGEBOORTEJAAR, GBAGENERATIE, GBAGEBOORTELAND, OPLNIVSOI2021AGG4HBmetNIRWO, GGZDBCHoofddiagnoseDSMIV, Behandelaar)

#Rename correctly
NoTreatmentSample<-as.data.frame(cbind(NoTreatmentSample$RINPERSOON, matrix(NA, nrow(NoTreatmentSample),1), NoTreatmentSample$GBAGESLACHT, NoTreatmentSample$GBAGEBOORTEJAAR, NoTreatmentSample$GBAGENERATIE, NoTreatmentSample$GBAGEBOORTELAND, NoTreatmentSample$OPLNIVSOI2021AGG4HBmetNIRWO,matrix(NA, nrow(NoTreatmentSample),1),matrix(NA, nrow(NoTreatmentSample),1)))
names(NoTreatmentSample)<-names(Wachttijd)
#make correct type
NoTreatmentSample$RINPERSOON<-as.character(NoTreatmentSample$RINPERSOON)
NoTreatmentSample$Man<-as.character(NoTreatmentSample$Man)
NoTreatmentSample$GBAGEBOORTEJAAR<-as.character(NoTreatmentSample$GBAGEBOORTEJAAR)
NoTreatmentSample$GBAGENERATIE<-as.character(NoTreatmentSample$GBAGENERATIE)
NoTreatmentSample$GBAGEBOORTELAND<-as.character(NoTreatmentSample$GBAGEBOORTELAND)
NoTreatmentSample$OPLNIVSOI2021AGG4HBmetNIRWO<-as.character(NoTreatmentSample$OPLNIVSOI2021AGG4HBmetNIRWO)
save.image("IntermediateFiles/PreMatchingSample.RData")

#Perform matching
load("IntermediateFiles/PreMatchingSample.RData")

#Combine files
GGZGebruik<-c(matrix(TRUE, nrow(Wachttijd),1),matrix(FALSE,nrow(NoTreatmentSample),1))
FullSample<-rbind(Wachttijd, NoTreatmentSample)
FullSample<-cbind(FullSample, GGZGebruik)
rm(Wachttijd, NoTreatmentSample, GGZGebruik)

#Change entries
FullSample$Man[FullSample$Man==2]<-0
FullSample$OPLNIVSOI2021AGG4HBmetNIRWO[is.na(FullSample$OPLNIVSOI2021AGG4HBmetNIRWO)]<-"Onbekende opleiding"

#Create extra descriptives for full population
Tab2DescriptivesPart3<-matrix(NA, 10, 1 )
Tab2DescriptivesPart3[1,1]<-2012-mean(as.numeric(FullSample$GBAGEBOORTEJAAR[!FullSample$GGZGebruik]), na.rm=TRUE)
Tab2DescriptivesPart3[2,1]<-mean(as.numeric(FullSample$Man[!FullSample$GGZGebruik]), na.rm=TRUE)
Tab2DescriptivesPart3[3,1]<-mean(as.character(FullSample$GBAGENERATIE[!FullSample$GGZGebruik])=="0", na.rm=TRUE)

#recheck education
FullSample$OPLNIVSOI2021AGG4HBmetNIRWO[FullSample$OPLNIVSOI2021AGG4HBmetNIRWO=="missing"]<-"Onbekende opleiding"
Tab2DescriptivesPart3[4,1]<-mean(as.numeric(FullSample[!FullSample$GGZGebruik,]$OPLNIVSOI2021AGG4HBmetNIRWO=="Onbekende opleiding"),na.rm=TRUE)
Tab2DescriptivesPart3[5,1]<-mean(as.numeric(as.character(FullSample[(!FullSample$GGZGebruik)&FullSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))<2000, na.rm=TRUE)
Tab2DescriptivesPart3[6,1]<-mean(as.numeric(as.character(FullSample[(!FullSample$GGZGebruik)&FullSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))>=2000&as.numeric(as.character(FullSample[(!FullSample$GGZGebruik)&FullSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))<3000, na.rm=TRUE)
Tab2DescriptivesPart3[7,1]<-mean(as.numeric(as.character(FullSample[(!FullSample$GGZGebruik)&FullSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))>=3000, na.rm=TRUE)

#ADd healthcare spending in 2012
costs <- read_sav(paste("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/2012/ZVWZORGKOSTEN2012TABV1.sav", sep = ""))
index<-costs$RINPERSOON %in% FullSample$RINPERSOON[!FullSample$GGZGebruik]
costs<-costs[index,]
Mental<-rowSums(cbind(costs$ZVWKEERSTELIJNSPSYCHO,costs$ZVWKGGZ,costs$ZVWKGENBASGGZ,costs$ZVWKSPECGGZ), na.rm=TRUE)
Physical<-rowSums(cbind(costs$ZVWKHUISARTS,costs$ZVWKZIEKENHUIS,costs$ZVWKWYKVERPLEGING), na.rm=TRUE)
Medicijnen<-costs$ZVWKPARAMEDISCH
Tab2DescriptivesPart3[8,1]<-mean(Mental, na.rm=TRUE)
Tab2DescriptivesPart3[9,1]<-mean(Physical, na.rm=TRUE)
Tab2DescriptivesPart3[10,1]<-mean(Medicijnen, na.rm=TRUE)

write.xlsx(Tab2DescriptivesPart3, "FinalOutput/Tab2DescriptivesPart3.xlsx")

#Discard unnecesary data
rm(GGZ20112019, Mental, Physical, Medicijnen)

#Factorize variables
FullSample$GBAGEBOORTEJAAR<-as.factor(FullSample$GBAGEBOORTEJAAR)
FullSample$GBAGEBOORTELAND<-as.factor(FullSample$GBAGEBOORTELAND)
FullSample$GBAGENERATIE<-as.factor(FullSample$GBAGENERATIE)
FullSample$OPLNIVSOI2021AGG4HBmetNIRWO<-as.factor(FullSample$OPLNIVSOI2021AGG4HBmetNIRWO)

#Estimate propensity score to start mental healthcare treatment
#Randomize for coverage
FullSample<-FullSample[order(FullSample$RINPERSOON),]
Propensity<-biglm(GGZGebruik~Man+GBAGEBOORTEJAAR+GBAGENERATIE+OPLNIVSOI2021AGG4HBmetNIRWO, data=FullSample[1:1000000,])
summary(Propensity)
for(i in 1:11){
  cat(paste(i, " "));flush.console()
  Propensity<-update(Propensity, moredata=FullSample[(1000000*i+1):(1000000*(i+1)),])
  gc()
}
Propensity<-update(Propensity, moredata=FullSample[12000001:nrow(FullSample),])

PredictedPropensity<-matrix(NA, nrow(FullSample),1)
for(i in 1:12){
  PredictedPropensity[(1000000*(i-1)+1):(i*1000000)]<-predict(Propensity, newdata = FullSample[(1000000*(i-1)+1):(i*1000000),])
}
PredictedPropensity[12000001:nrow(FullSample)]<-predict(Propensity, newdata = FullSample[12000001:nrow(FullSample),])

#Propensity score matching
MatchedSample<-Matchby(Y=runif(n = nrow(FullSample), min=0, max=1), X=PredictedPropensity, Tr=FullSample$GGZGebruik, by=FullSample$GBAGEBOORTEJAAR, M=1, replace=FALSE)

TreatmentSample<-FullSample[MatchedSample$index.treated,]
ControlSample<-FullSample[MatchedSample$index.control,]
PropensityTreated<-PredictedPropensity[MatchedSample$index.treated]
PropensityControl<-PredictedPropensity[MatchedSample$index.control]

ControlSample$IndexStartGGZ<-TreatmentSample$IndexStartGGZ
ControlSample$Behandelaar<-TreatmentSample$Behandelaar
ControlSample$GGZDBCHoofddiagnoseDSMIV<-TreatmentSample$GGZDBCHoofddiagnoseDSMIV
save.image("IntermediateFiles/MatchedSample.RData")
load("IntermediateFiles/MatchedSample.RData")

#Add earnings for matched sample
rm(FullSample, PredictedPropensity, Propensity, index, Medicijnen, PropensityControl, PropensityTreated)
MatchedSample<-ControlSample
rm(ControlSample, TreatmentSample)
MatchedSample%<>%
  dplyr::select(RINPERSOON)

#Load Polis Data
gc()
POLIS2009 <- read_sav(file="G:/Polis/POLISBUS/2009/POLISBUS2009V2.sav", col_select=c(RINPERSOON, AANVBUS, BASISLOON, BASISUREN))
index<-POLIS2009$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2009<-POLIS2009[index,]
gc()
POLIS2010 <- read_sav(file="G:/Spolis/SPOLISBUS/2010/SPOLISBUS2010V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2010$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2010<-POLIS2010[index,]
gc()
POLIS2011 <- read_sav(file="G:/Spolis/SPOLISBUS/2011/SPOLISBUS2011V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2011$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2011<-POLIS2011[index,]
gc()
POLIS2012 <- read_sav(file="G:/Spolis/SPOLISBUS/2012/SPOLISBUS2012V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2012$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2012<-POLIS2012[index,]
gc()
POLIS2013 <- read_sav(file="G:/Spolis/SPOLISBUS/2013/SPOLISBUS2013V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2013$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2013<-POLIS2013[index,]
gc()
POLIS2014 <- read_sav(file="G:/Spolis/SPOLISBUS/2014/SPOLISBUS 2014V1.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2014$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2014<-POLIS2014[index,]
gc()
POLIS2015 <- read_sav(file="G:/Spolis/SPOLISBUS/2015/SPOLISBUS 2015V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2015$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2015<-POLIS2015[index,]
gc()
POLIS2016 <- read_sav(file="G:/Spolis/SPOLISBUS/2016/SPOLISBUS2016V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2016$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2016<-POLIS2016[index,]
gc()
POLIS2017 <- read_sav(file="G:/Spolis/SPOLISBUS/2017/SPOLISBUS2017V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2017$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2017<-POLIS2017[index,]
gc()
POLIS2018 <- read_sav(file="G:/Spolis/SPOLISBUS/2018/SPOLISBUS2018V5.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2018$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2018<-POLIS2018[index,]
gc()
POLIS2019 <- read_sav(file="G:/Spolis/SPOLISBUS/2019/SPOLISBUS2019V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2019$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2019<-POLIS2019[index,]
gc()
POLIS2020 <- read_sav(file="G:/Spolis/SPOLISBUS/2020/SPOLISBUS2020V5.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2020$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2020<-POLIS2020[index,]
gc()
POLIS2021 <- read_sav(file="G:/Spolis/SPOLISBUS/2021/SPOLISBUS2021V5.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2021$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2021<-POLIS2021[index,]
names(POLIS2009)<-names(POLIS2021)
gc()
POLIS2022 <- read_sav(file="G:/Spolis/SPOLISBUS/2022/SPOLISBUS2022V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2022$RINPERSOON %in% MatchedSample$RINPERSOON
POLIS2022<-POLIS2022[index,]
names(POLIS2009)<-names(POLIS2022)
gc()
save.image("IntermediateFiles/POLISDataMatchedSample.RData")

#make polis series for control group
load("IntermediateFiles/POLISDataMatchedSample.RData")

MatchedSample$RINPERSOON<-MatchedSample$RINPERSOON[order(MatchedSample$RINPERSOON)]
PolisData<-rbind(POLIS2009, POLIS2010, POLIS2011, POLIS2012, POLIS2013, POLIS2014, POLIS2015, POLIS2016, POLIS2017, POLIS2018, POLIS2019,POLIS2020, POLIS2021, POLIS2022)
PolisData<-PolisData[order(PolisData$RINPERSOON),]
indexAanvang<-as.data.frame((as.numeric(substr(PolisData$SDATUMAANVANGIKO,1,4))-2009)*12+as.numeric(substr(PolisData$SDATUMAANVANGIKO,5,6)))
names(indexAanvang)<-"indexAanvang"
rm(POLIS2009, POLIS2010, POLIS2011, POLIS2012, POLIS2013, POLIS2014, POLIS2015, POLIS2016, POLIS2017, POLIS2018, POLIS2019,POLIS2020, POLIS2021, POLIS2022)

Earnings<-matrix(0, nrow(MatchedSample), 168)
Uren<-matrix(0, nrow(MatchedSample), 168)

time<-Sys.time()
j<-1
for(i in 1:nrow(MatchedSample)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(MatchedSample$RINPERSOON[i]==PolisData$RINPERSOON[j]){
    Earnings[i,indexAanvang$indexAanvang[j]]<-Earnings[i,indexAanvang$indexAanvang[j]]+PolisData$SBASISLOON[j]
    Uren[i,indexAanvang$indexAanvang[j]]<-Uren[i,indexAanvang$indexAanvang[j]]+PolisData$SBASISUREN[j]
    j<-j+1
  }
}
Duur<-Sys.time()-time
#Delete last 3 months
Uren[,166:168]<-NA
Earnings[,166:168]<-NA

rm(PolisData, indexAanvang, index)
save.image("IntermediateFiles/TijdreeksenPolisControl.RData")

#merge Polis Samples
load("IntermediateFiles/Nieuw/TijdreeksenPolisControl.RData")
UrenControl<-Uren
EarningsControl<-Earnings
RINPERSOONControl<-MatchedSample
rm(Uren, Earnings, MatchedSample)
load("IntermediateFiles/TijdreeksenPolis.RData")
RINPERSOONTreatment<-Wachttijd
rm(Wachttijd)
load("IntermediateFiles/MatchedSample.RData")
#delete obs in treatment not used
index<-RINPERSOONTreatment$RINPERSOON %in% TreatmentSample$RINPERSOON
Earnings<-Earnings[index,]
Uren<-Uren[index,]
TreatmentSample<-TreatmentSample[order(TreatmentSample$RINPERSOON),]

MatchedSample<-rbind(TreatmentSample, ControlSample)
Uren<-rbind(Uren, UrenControl)
Earnings<-rbind(Earnings, EarningsControl)
order<-order(MatchedSample$RINPERSOON)
MatchedSample<-MatchedSample[order,]
Uren<-Uren[order,]
Earnings<-Earnings[order,]
rm(EarningsControl, FullSample, PredictedPropensity, Propensity, RINPERSOONControl, RINPERSOONTreatment, UrenControl, index, Medicijnen, order, PropensityControl, PropensityTreated)
save.image("IntermediateFiles/PolisTijdreeksenMatchedSample.RData")

#HealthCostSeries
rm(list=ls())
load("IntermediateFiles//PolisTijdreeksenMatchedSample.RData")
rm(ControlSample, TreatmentSample)
RINPERSOONSample<-as.data.frame(MatchedSample$RINPERSOON)
names(RINPERSOONSample)[1]<-"RINPERSOON"

for(y in 2009:2020){
  cat(paste(y, " "));flush.console()
  if(y==2017|y==2018){
    costs <- read_sav(paste("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/", y, "/ZVWZORGKOSTEN", y,"TABV2.sav", sep = ""))
  }
  else{
    costs <- read_sav(paste("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB/", y, "/ZVWZORGKOSTEN", y,"TABV1.sav", sep = ""))
  }
  index<-costs$RINPERSOON %in% MatchedSample$RINPERSOON
  costs<-costs[index,]
  Mental<-rowSums(cbind(costs$ZVWKEERSTELIJNSPSYCHO,costs$ZVWKGGZ,costs$ZVWKGENBASGGZ,costs$ZVWKSPECGGZ), na.rm=TRUE)
  Physical<-rowSums(cbind(costs$ZVWKHUISARTS,costs$ZVWKZIEKENHUIS,costs$ZVWKWYKVERPLEGING), na.rm=TRUE)
  Medicijnen<-costs$ZVWKPARAMEDISCH
  costs1<-as.data.frame(cbind(costs$RINPERSOON))
  costs1<-cbind(costs1, Mental)
  costs2<-as.data.frame(cbind(costs$RINPERSOON))
  costs2<-cbind(costs2, Physical)
  costs3<-as.data.frame(cbind(costs$RINPERSOON))
  costs3<-cbind(costs3, Medicijnen)
  names(costs1)<-c("RINPERSOON", "MentalCost")
  names(costs2)<-c("RINPERSOON", "PhysicalCost")
  names(costs3)<-c("RINPERSOON", "MedicijnCost")
  if(y==2009){
    MentalCost<-merge(RINPERSOONSample, costs1, by="RINPERSOON", all.x=TRUE)
    PhysicalCost<-merge(RINPERSOONSample, costs2, by="RINPERSOON", all.x=TRUE)
    MedicijnCost<-merge(RINPERSOONSample, costs3, by="RINPERSOON", all.x=TRUE)
  }
  else{
    MentalCost<-merge(MentalCost, costs1, by="RINPERSOON", all.x=TRUE)
    PhysicalCost<-merge(PhysicalCost, costs2, by="RINPERSOON", all.x=TRUE)
    MedicijnCost<-merge(MedicijnCost, costs3, by="RINPERSOON", all.x=TRUE)
  }
}
rm(costs, costs1, costs2, costs3, Mental, Physical, Medicijnen)

#order
MentalCost<-MentalCost[order(MentalCost$RINPERSOON),]
PhysicalCost<-PhysicalCost[order(PhysicalCost$RINPERSOON),]
MedicijnCost<-MedicijnCost[order(MedicijnCost$RINPERSOON),]
MentalCost<-as.matrix(MentalCost[,2:13])
PhysicalCost<-as.matrix(PhysicalCost[,2:13])
MedicijnCost<-as.matrix(MedicijnCost[,2:13])
rm(RINPERSOONSample)
save.image("IntermediateFiles/TijdreeksenKosten.RData")

#Make time series of SECM
load("IntermediateFiles/TijdreeksenKosten.RData")
rm(Earnings, MedicijnCost, MentalCost, PhysicalCost,Uren,index)
MatchedSample%<>%
  dplyr::select(RINPERSOON, IndexStartGGZ)
SECMBUSV20191<-read_sav("G:/InkomenBestedingen/SECMBUS/SECMBUS2021V1.sav", col_select=c(RINPERSOON,AANVSECM,EINDSECM,XKOPPELWERKNSECM,XKOPPELWERKLUITKSECM,XKOPPELBIJSTANDSECM,XKOPPELSOCVOORZOVSECM,XKOPPELZIEKTEAOSECM,XKOPPELSCHOLSTUDSECM))
index<-SECMBUSV20191$RINPERSOON %in% MatchedSample$RINPERSOON
SECMBUS<-SECMBUSV20191[index,]
rm(SECMBUSV20191, index)

#dplyr::select entries which end after january 2004
SECMBUS<-subset(SECMBUS, EINDSECM>=20040101)

#select entries with relevant receipt
index<-SECMBUS$XKOPPELWERKNSECM=="1"|SECMBUS$XKOPPELWERKLUITKSECM=="1"|SECMBUS$XKOPPELBIJSTANDSECM=="1"|SECMBUS$XKOPPELSOCVOORZOVSECM=="1"|SECMBUS$XKOPPELZIEKTEAOSECM=="1"|SECMBUS$XKOPPELSCHOLSTUDSECM=="1"
SECMBUS<-SECMBUS[index,]

#order
SECMBUS<-SECMBUS[order(SECMBUS$RINPERSOON),]
MatchedSample<-MatchedSample[order(MatchedSample$RINPERSOON),]

#Matrices for various benefits from januari 2004 untill december 2021
Werknemer<-matrix(0, length(MatchedSample$RINPERSOON),216)
WW<-matrix(0, length(MatchedSample$RINPERSOON),216)
Bijstand<-matrix(0, length(MatchedSample$RINPERSOON),216)
SocVerOv<-matrix(0, length(MatchedSample$RINPERSOON),216)
ZiekteAO<-matrix(0, length(MatchedSample$RINPERSOON),216)
Scholier<-matrix(0, length(MatchedSample$RINPERSOON),216)

indexAanvang<-as.data.frame((as.numeric(substr(SECMBUS$AANVSECM,1,4))-2004)*12+as.numeric(substr(SECMBUS$AANVSECM,5,6)))
indexEinde<-as.data.frame((as.numeric(substr(SECMBUS$EINDSECM,1,4))-2004)*12+as.numeric(substr(SECMBUS$EINDSECM,5,6)))
names(indexAanvang)[1]<-"indexAanvang"
names(indexEinde)[1]<-"indexEinde"

indexAanvang$indexAanvang[which(indexAanvang$indexAanvang<1)]<-1

j<-1
for(i in 1:length(MatchedSample$RINPERSOON)){
  if(i %% 5000==0){
    cat(paste(i, " "));flush.console()
    gc()
  }
  while((MatchedSample$RINPERSOON[i]==SECMBUS$RINPERSOON[j])&((j)<nrow(SECMBUS))){
    if(as.character(SECMBUS$XKOPPELWERKNSECM[j])==1){
      Werknemer[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELWERKLUITKSECM[j])==1){
      WW[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELBIJSTANDSECM[j])==1){
      Bijstand[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELSOCVOORZOVSECM[j])==1){
      SocVerOv[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELZIEKTEAOSECM[j])==1){
      ZiekteAO[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELSCHOLSTUDSECM[j])==1){
      Scholier[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    j<-j+1
  }
}
endTime<-Sys.time()

rm(SECMBUS, indexAanvang, indexEinde)

NoIncome<-matrix(0, nrow(MatchedSample), 216)
for(i in 1:nrow(MatchedSample)){
  for(j in 1:216){
    if(Werknemer[i,j]==0&WW[i,j]==0&Bijstand[i,j]==0&SocVerOv[i,j]==0&ZiekteAO[i,j]==0){
      NoIncome[i,j]<-1
    }
  }
}
save.image("IntermediateFiles/TijdreeksenSECMBUSStap1.RData")
rm(MatchedSample)
save.image("IntermediateFiles/TijdreeksenSECMBUSStap1a.RData")

#Combine all data sets
load("IntermediateFiles/TijdreeksenSECMBUSStap1.RData")
load("IntermediateFiles/TijdreeksenKosten.RData")
#Change NA entries
MatchedSample$Man[MatchedSample$Man==2]<-0
MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO[is.na(MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO)]<-"Onbekende opleiding"

rm(ZiekteAO, NoIncome, MentalCost, PhysicalCost, MedicijnCost, Uren, Earnings)
#measures relative to first month of contact data
BijstandGGZ<-matrix(NA, nrow(MatchedSample), 396)
ScholierGGZ<-matrix(NA, nrow(MatchedSample), 396)
SocVerOvGGZ<-matrix(NA, nrow(MatchedSample), 396)
WerknemerGGZ<-matrix(NA, nrow(MatchedSample), 396)
WWGGZ<-matrix(NA, nrow(MatchedSample), 396)
MatchedSample$IndexStartGGZ<-as.numeric(MatchedSample$IndexStartGGZ)

#193 is month of first contact (97 voor treatment). 12 is year of first contact
for(i in 1:nrow(MatchedSample)){
  if(!is.na(MatchedSample$IndexStartGGZ[i])){
    if(MatchedSample$IndexStartGGZ[i]>0&MatchedSample$IndexStartGGZ[i]<97){
      BijstandGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-Bijstand[i,]
      ScholierGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-Scholier[i,]
      SocVerOvGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-SocVerOv[i,]
      WerknemerGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-Werknemer[i,]
      WWGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-WW[i,]
      if(i%%1000==0){
        gc()
      }
    }
  }
}

BijstandGGZ<-BijstandGGZ[, 97:288]
ScholierGGZ<-ScholierGGZ[, 97:288]
SocVerOvGGZ<-SocVerOvGGZ[, 97:288]
WerknemerGGZ<-WerknemerGGZ[, 97:288]
WWGGZ<-WWGGZ[, 97:288]
load("IntermediateFiles/TijdreeksenSECMBUSStap1.RData")
load("IntermediateFiles/TijdreeksenKosten.RData")
rm(Bijstand, Scholier, SocVerOv, Werknemer, WW)
gc()
MatchedSample$IndexStartGGZ<-as.numeric(MatchedSample$IndexStartGGZ)

ZiekteAOGGZ<-matrix(NA, nrow(MatchedSample), 396)
NoIncomeGGZ<-matrix(NA, nrow(MatchedSample), 396)
MentalCostGGZ<-matrix(NA, nrow(MatchedSample), 24)
PhysicalCostGGZ<-matrix(NA, nrow(MatchedSample), 24)
MedicijnenGGZ<-matrix(NA, nrow(MatchedSample), 24)
UrenGGZ<-matrix(NA, nrow(MatchedSample), 396)
EarningsGGZ<-matrix(NA, nrow(MatchedSample), 396)
for(i in 1:nrow(MatchedSample)){
  if(!is.na(MatchedSample$IndexStartGGZ[i])){
    if(MatchedSample$IndexStartGGZ[i]>0&MatchedSample$IndexStartGGZ[i]<97){
      ZiekteAOGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-ZiekteAO[i,]
      NoIncomeGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+96)):(349-(MatchedSample$IndexStartGGZ[i]+96)+60)]<-NoIncome[i,]
      MentalCostGGZ[i,(10-ceiling(MatchedSample$IndexStartGGZ[i]/12)):(21-ceiling(MatchedSample$IndexStartGGZ[i]/12))]<-MentalCost[i,]
      PhysicalCostGGZ[i,(10-ceiling(MatchedSample$IndexStartGGZ[i]/12)):(21-ceiling(MatchedSample$IndexStartGGZ[i]/12))]<-PhysicalCost[i,]
      MedicijnenGGZ[i,(10-ceiling(MatchedSample$IndexStartGGZ[i]/12)):(21-ceiling(MatchedSample$IndexStartGGZ[i]/12))]<-MedicijnCost[i,]
      UrenGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+36)):(361-(MatchedSample$IndexStartGGZ[i]+36))]<-Uren[i,]
      EarningsGGZ[i,(194-(MatchedSample$IndexStartGGZ[i]+36)):(361-(MatchedSample$IndexStartGGZ[i]+36))]<-Earnings[i,]
      if(i%%1000==0){
        gc()
      }
    }
  }
}

#Select 72 pre-months+24 pre-pre untill 96 post months
ZiekteAOGGZ<-ZiekteAOGGZ[, 97:288]
NoIncomeGGZ<-NoIncomeGGZ[, 97:288]
EarningsGGZ<-EarningsGGZ[, 97:288]
UrenGGZ<-UrenGGZ[, 97:288]

rm(ZiekteAO, NoIncome, Earnings, Uren, MedicijnCost, MentalCost, PhysicalCost)
save.image("IntermediateFiles/EventStudyDataStep1.RData")
load("IntermediateFiles/EventStudyDataStep1.RData")

#add months of year
GBAPERSOON2014 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2014/GBAPERSOON2014TABV1.sav")
GBAPERSOON2014%<>%
  dplyr::select(RINPERSOON, GBAGEBOORTEMAAND)
MatchedSample<-merge(MatchedSample, GBAPERSOON2014, by="RINPERSOON", all.x=TRUE)
MatchedSample<-MatchedSample[order(MatchedSample$RINPERSOON),]
rm(GBAPERSOON2014)

#Make age in months in january 2012
MonthBirth<-(2012-as.numeric(as.character(MatchedSample$GBAGEBOORTEJAAR)))*12-(13-as.numeric(as.character(MatchedSample$GBAGEBOORTEMAAND)))
AgeTreatment<-MonthBirth+MatchedSample$IndexStartGGZ
AgeStart<-MonthBirth+MatchedSample$IndexStartGGZ-96
AgeEnd<-MonthBirth+MatchedSample$IndexStartGGZ+96

#delete data prior to 18 years old
for(i in 1:nrow(MatchedSample)){
  if(AgeStart[i]<216){
    WerknemerGGZ[i,1:(216-AgeStart[i])]<-NA
    BijstandGGZ[i,1:(216-AgeStart[i])]<-NA
    EarningsGGZ[i,1:(216-AgeStart[i])]<-NA
    NoIncomeGGZ[i,1:(216-AgeStart[i])]<-NA
    ScholierGGZ[i,1:(216-AgeStart[i])]<-NA
    SocVerOvGGZ[i,1:(216-AgeStart[i])]<-NA
    UrenGGZ[i,1:(216-AgeStart[i])]<-NA
    WWGGZ[i,1:(216-AgeStart[i])]<-NA
    ZiekteAOGGZ[i,1:(216-AgeStart[i])]<-NA
  }
  if(AgeEnd[i]>780){
    WerknemerGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    BijstandGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    EarningsGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    NoIncomeGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    ScholierGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    SocVerOvGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    UrenGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    WWGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
    ZiekteAOGGZ[i,(192+(780-AgeEnd[i])):192]<-NA
  }
}

rm(MonthBirth, AgeStart, AgeEnd, AgeTreatment)

#Make descriptives table
Tab2DescriptivesPart1<-matrix(NA, 10,3)
test<-t.test(2014-as.numeric(as.character(MatchedSample[MatchedSample$GGZGebruik,]$GBAGEBOORTEJAAR)), 2014-as.numeric(as.character(MatchedSample[!MatchedSample$GGZGebruik,]$GBAGEBOORTEJAAR)), alternative="two.sided")
Tab2DescriptivesPart1[1,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(as.numeric(MatchedSample[MatchedSample$GGZGebruik,]$Man), as.numeric(MatchedSample[!MatchedSample$GGZGebruik,]$Man), alternative="two.sided")
Tab2DescriptivesPart1[2,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(as.numeric(MatchedSample[MatchedSample$GGZGebruik,]$GBAGENERATIE==0), as.numeric(MatchedSample[!MatchedSample$GGZGebruik,]$GBAGENERATIE==0), alternative="two.sided")
Tab2DescriptivesPart1[3,]<-c(test$estimate[1],test$estimate[2], test$p.value)

#recheck education
MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO[MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO=="missing"]<-"Onbekende opleiding"
test<-t.test(as.numeric(MatchedSample[MatchedSample$GGZGebruik,]$OPLNIVSOI2021AGG4HBmetNIRWO=="Onbekende opleiding"), as.numeric(MatchedSample[!MatchedSample$GGZGebruik,]$OPLNIVSOI2021AGG4HBmetNIRWO=="Onbekende opleiding"), alternative="two.sided")
Tab2DescriptivesPart1[4,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(as.numeric(as.character(MatchedSample[MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))<2000, as.numeric(as.character(MatchedSample[!MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))<2000, alternative="two.sided")
Tab2DescriptivesPart1[5,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(as.numeric(as.character(MatchedSample[MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))>=2000&as.numeric(as.character(MatchedSample[MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))<3000, as.numeric(as.character(MatchedSample[!MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))>=2000&as.numeric(as.character(MatchedSample[!MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))<3000, alternative="two.sided")
Tab2DescriptivesPart1[6,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(as.numeric(as.character(MatchedSample[MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))>=3000, as.numeric(as.character(MatchedSample[!MatchedSample$GGZGebruik&MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO!="Onbekende opleiding",]$OPLNIVSOI2021AGG4HBmetNIRWO))>=3000, alternative="two.sided")
Tab2DescriptivesPart1[7,]<-c(test$estimate[1],test$estimate[2], test$p.value)

test<-t.test(MentalCostGGZ[MatchedSample$GGZGebruik,12],MentalCostGGZ[!MatchedSample$GGZGebruik,12], alternative="two.sided")
Tab2DescriptivesPart1[8,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(PhysicalCostGGZ[MatchedSample$GGZGebruik,12],PhysicalCostGGZ[!MatchedSample$GGZGebruik,12], alternative="two.sided")
Tab2DescriptivesPart1[9,]<-c(test$estimate[1],test$estimate[2], test$p.value)
test<-t.test(MedicijnenGGZ[MatchedSample$GGZGebruik,12],MedicijnenGGZ[!MatchedSample$GGZGebruik,12], alternative="two.sided")
Tab2DescriptivesPart1[10,]<-c(test$estimate[1],test$estimate[2], test$p.value)
write.xlsx(Tab2DescriptivesPart1, "FinalOutput/Tab2DescriptivesPart1.xlsx")

OutputTrends<-matrix(NA, 192, 14)
OutputTrends[,1]<-100*colMeans(WerknemerGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,2]<-100*colMeans(WerknemerGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,3]<-100*colMeans(WWGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,4]<-100*colMeans(WWGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,5]<-100*colMeans(BijstandGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,6]<-100*colMeans(BijstandGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,7]<-100*colMeans(ZiekteAOGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,8]<-100*colMeans(ZiekteAOGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,9]<-100*colMeans(SocVerOvGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,10]<-100*colMeans(SocVerOvGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,11]<-colMeans(UrenGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,12]<-colMeans(UrenGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,13]<-colMeans(EarningsGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE)
OutputTrends[,14]<-colMeans(EarningsGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE)
write.xlsx(OutputTrends, "FinalOutput/OutputTrendsFigB1.xlsx")

#Plot trends of employment outcomes
op<-par(cex=1)
time<-seq(-96,95,1)

setEPS()
postscript("FinalOutput/Fig3a.eps", width=8)
plot(time,100*colMeans(WerknemerGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE), type="l", xlab="Months relative to first moment of contact", ylab="Probability to be employed", ylim=c(50,80), lwd=2, col="black", xaxt="n")
lines(time, 100*colMeans(WerknemerGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE), col="red", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topleft", legend = c("No mental health treatment","Mental health treatment"), col=c("black","red"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

setEPS()
postscript("FinalOutput/FigB1e.eps", width=8)
plot(time,100*colMeans(BijstandGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE), type="l", xlab="Months relative to first moment of contact", ylab="Probability to receive social assistance", ylim=c(2,14), lwd=2, col="black", xaxt="n")
lines(time, 100*colMeans(BijstandGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE), col="red", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topleft", legend = c("No mental health treatment","Mental health treatment"), col=c("black","red"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

setEPS()
postscript("FinalOutput/FigB1c.eps", width=8)
plot(time,100*colMeans(WWGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE), type="l", xlab="Months relative to first moment of contact", ylab="Probability to receive UI benefits", ylim=c(1,8), lwd=2, col="black", xaxt="n")
lines(time, 100*colMeans(WWGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE), col="red", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topleft", legend = c("No mental health treatment","Mental health treatment"), col=c("black","red"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

setEPS()
postscript("FinalOutput/FigB1d.eps", width=8)
plot(time,100*colMeans(ZiekteAOGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE), type="l", xlab="Months relative to first moment of contact", ylab="Probability to receice sickness/DI benefits", ylim=c(3,17), lwd=2, col="black", xaxt="n")
lines(time, 100*colMeans(ZiekteAOGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE), col="red", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topleft", legend = c("No mental health treatment","Mental health treatment"), col=c("black","red"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

setEPS()
postscript("FinalOutput/FigB1b.eps", width=8)
plot(time,colMeans(UrenGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE), type="l", xlab="Months relative to first moment of contact", ylab="Monthly number of working hours", ylim=c(70,90), lwd=2, col="black", xaxt="n")
lines(time, colMeans(UrenGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE), col="red", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topleft", legend = c("No mental health treatment","Mental health treatment"), col=c("black","red"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

setEPS()
postscript("FinalOutput/FigB1a.eps", width=8)
plot(time,colMeans(EarningsGGZ[!MatchedSample$GGZGebruik,],na.rm=TRUE), type="l", xlab="Months relative to first moment of contact", ylab="Monthly labor earnings", ylim=c(800,2000), lwd=2, col="black", xaxt="n")
lines(time, colMeans(EarningsGGZ[MatchedSample$GGZGebruik,],na.rm=TRUE), col="red", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topleft", legend = c("No mental health treatment","Mental health treatment"), col=c("black","red"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

save.image("IntermediateFiles/EventStudyDataStep1Extra.RData")

load("IntermediateFiles/EventStudyDataStep1Extra.RData")
#delete unneccesary data
rm(MedicijnenGGZ, MentalCostGGZ, NoIncomeGGZ, PhysicalCostGGZ, ScholierGGZ, index)
MatchedSample<-MatchedSample[,1:10]

#Re-order data
order<-order(MatchedSample$GGZGebruik)
MatchedSample<-MatchedSample[order,]
BijstandGGZ<-BijstandGGZ[order,]
EarningsGGZ<-EarningsGGZ[order,]
SocVerOvGGZ<-SocVerOvGGZ[order,]
UrenGGZ<-UrenGGZ[order,]
WerknemerGGZ<-WerknemerGGZ[order,]
WWGGZ<-WWGGZ[order,]
ZiekteAOGGZ<-ZiekteAOGGZ[order,]


###Event study with various outcome variables with GGZGebruik as treatment of interest. 8 years pre is baseline
#pre and post time window and donut
x<-96
y<-96
donut<-0

N<-nrow(MatchedSample)
NControl<-length(which(!MatchedSample$GGZGebruik))
NTreatment<-length(which(MatchedSample$GGZGebruik))

#Outcome measures
DiDUren<-matrix(t(UrenGGZ),dimnames=list(t(outer(colnames(t(UrenGGZ)), rownames(t(UrenGGZ)),FUN=paste)),NULL))
DiDEarnings<-matrix(t(EarningsGGZ),dimnames=list(t(outer(colnames(t(EarningsGGZ)), rownames(t(EarningsGGZ)),FUN=paste)),NULL))
DiDIndividual<-cbind(DiDUren,DiDEarnings)
rm(DiDUren, DiDEarnings, UrenGGZ, EarningsGGZ)
DiDIndividual<-as.data.frame(DiDIndividual)
names(DiDIndividual)[1:2]<-c("DiDUren", "DiDEarnings")
DiDIndividual$DiDEmployment<-matrix(t(WerknemerGGZ),dimnames=list(t(outer(colnames(t(WerknemerGGZ)), rownames(t(WerknemerGGZ)),FUN=paste)),NULL))
DiDIndividual$DiDWW<-matrix(t(WWGGZ),dimnames=list(t(outer(colnames(t(WWGGZ)), rownames(t(WWGGZ)),FUN=paste)),NULL))
DiDIndividual$DiDBijstand<-matrix(t(BijstandGGZ),dimnames=list(t(outer(colnames(t(BijstandGGZ)), rownames(t(BijstandGGZ)),FUN=paste)),NULL))
DiDIndividual$DiDSocVerOv<-matrix(t(SocVerOvGGZ),dimnames=list(t(outer(colnames(t(SocVerOvGGZ)), rownames(t(SocVerOvGGZ)),FUN=paste)),NULL))
DiDIndividual$DiDZiekte<-matrix(t(ZiekteAOGGZ),dimnames=list(t(outer(colnames(t(ZiekteAOGGZ)), rownames(t(ZiekteAOGGZ)),FUN=paste)),NULL))
rm(WerknemerGGZ, WWGGZ, BijstandGGZ,SocVerOvGGZ, ZiekteAOGGZ, UrenGGZ, EarningsGGZ)

group<-matrix(0, (x+y)*N)
treatment<-matrix(0, (x+y)*N)
for(i in 1:N){
  treatment[((x+y)*(i-1)+x+1):((x+y)*i),1]<-matrix(1,y,1)
}
DiDIndividual$treatment<-treatment
rm(treatment)

group[(NControl*(x+y)+1):(N*(x+y)),1]<-matrix(1, NTreatment*(x+y),1)
DiDIndividual$group<-group
rm(group)

MonthDummies<-matrix(0, (x+y)*N,1)
for(i in 1:N){
  MonthDummies[((x+y)*(i-1)+1):((x+y)*i),1]<-seq(-x,y-1,1)
}
DiDIndividual$MonthDummies<-MonthDummies
rm(MonthDummies)

#Calculate standard errors clustered on individual level
#Cluster<-matrix(0, (x+y)*N,1)
#for(i in 1:N){
#  Cluster[(1+(x+y)*(i-1)):((x+y)*i),1]<-matrix(i,x+y,1)
#}
#DiDIndividual$Cluster<-Cluster
#rm(group, treatment, MonthDummies, Cluster)
rm(order)
save.image("IntermediateFiles/EventStudyData.RData")
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDEmployment,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)

#Employment
numRep<-2013
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}

#WW
rm(DiDIndividual)
gc()
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDWW,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)
gc()
DiDWWplm<-biglm(DiDWW ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDWWplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDWWplm<-update(DiDWWplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}

#Bijstand
rm(DiDIndividual)
gc()
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDBijstand,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)
gc()
DiDBijstandplm<-biglm(DiDBijstand ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDBijstandplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDBijstandplm<-update(DiDBijstandplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}

#SocVerOv
rm(DiDIndividual)
gc()
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDSocVerOv,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)
gc()
DiDSocVerOvplm<-biglm(DiDSocVerOv ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDSocVerOvplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDSocVerOvplm<-update(DiDSocVerOvplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}

#Ziekte
rm(DiDIndividual)
gc()
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDZiekte,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)
gc()
DiDZiekteplm<-biglm(DiDZiekte ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDZiekteplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDZiekteplm<-update(DiDZiekteplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}

#Uren
rm(DiDIndividual)
gc()
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDUren,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)
gc()
DiDUrenplm<-biglm(DiDUren ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDUrenplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDUrenplm<-update(DiDUrenplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}

#earnings
rm(DiDIndividual)
gc()
load("IntermediateFiles/EventStudyData.RData")
rm(MatchedSample)
DiDIndividual%<>%
  dplyr::select(DiDEarnings,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")
gc()
#Event study
#Reshuffle
RandomOrder<-sample(1:nrow(DiDIndividual), nrow(DiDIndividual), replace=F)
DiDIndividual<-DiDIndividual[RandomOrder,]
rm(RandomOrder)
gc()
DiDEarningsplm<-biglm(DiDEarnings ~ group*MonthDummies, data=DiDIndividual[1:100000,])
summary(DiDEarningsplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEarningsplm<-update(DiDEarningsplm, moredata=DiDIndividual[(100000*i+1):(100000*(i+1)),])
  gc()
}
save.image("IntermediateFiles/ResultsEventStudy.RData")

#make plots
load("IntermediateFiles/ResultsEventStudy.RData")

#Save results to export
EventStudyOutput<-matrix(NA, 168, 14)
EventStudyOutput[,1]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyOutput[,2]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
EventStudyOutput[,3]<-c(0,100*coeftest(DiDWWplm)[170:336,1])
EventStudyOutput[,4]<-c(0,100*coeftest(DiDWWplm)[170:336,2])
EventStudyOutput[,5]<-c(0,100*coeftest(DiDBijstandplm)[170:336,1])
EventStudyOutput[,6]<-c(0,100*coeftest(DiDBijstandplm)[170:336,2])
EventStudyOutput[,7]<-c(0,100*coeftest(DiDZiekteplm)[170:336,1])
EventStudyOutput[,8]<-c(0,100*coeftest(DiDZiekteplm)[170:336,2])
EventStudyOutput[,9]<-c(0,100*coeftest(DiDSocVerOvplm)[170:336,1])
EventStudyOutput[,10]<-c(0,100*coeftest(DiDSocVerOvplm)[170:336,2])
EventStudyOutput[,11]<-c(0,coeftest(DiDUrenplm)[170:336,1])
EventStudyOutput[,12]<-c(0,coeftest(DiDUrenplm)[170:336,2])
EventStudyOutput[,13]<-c(0,coeftest(DiDEarningsplm)[170:336,1])
EventStudyOutput[,14]<-c(0,coeftest(DiDEarningsplm)[170:336,2])
write.xlsx(EventStudyOutput, "FinalOutput/EventStudyOutputFig3.xlsx")

EventStudyEstimates<-c(0,100*coeftest(DiDEmploymentplm)[170:335,1])
EventStudyUpperbound<-c(0,100*coeftest(DiDEmploymentplm)[170:335,1])+2*c(0, 100*coeftest(DiDEmploymentplm)[170:335,2])
EventStudyLowerbound<-c(0,100*coeftest(DiDEmploymentplm)[170:335,1])-2*c(0, 100*coeftest(DiDEmploymentplm)[170:335,2])

op<-par(cex=1)
time<-seq(1,167)

setEPS()
postscript("FinalOutput/Fig3b.eps", width=8)
plot(EventStudyEstimates,type="l", col="black", xlab="Months relative to first moment of contact", ylab="Difference in employment rate", lty=1, xaxt="n", lwd=3, ylim=c(-13, 3))
abline(v=72, col="grey", lty=2, lwd=2 )
abline(h=0, col="grey", lty=2, lwd=2 )
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col="grey", border=NA)
lines(EventStudyEstimates, col="black", lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(0, 24, 48, 72, 96, 120, 144, 168))
legend("topright", legend=c("Event-study estimates", "95% confidence interval"), col=c("black", "grey"), lty=c(1,1), lwd=c(2,5))
dev.off()

EventStudyEstimates<-c(0,100*coeftest(DiDWWplm)[170:335,1])
EventStudyUpperbound<-c(0,100*coeftest(DiDWWplm)[170:335,1])+2*c(0, 100*coeftest(DiDWWplm)[170:335,2])
EventStudyLowerbound<-c(0,100*coeftest(DiDWWplm)[170:335,1])-2*c(0, 100*coeftest(DiDWWplm)[170:335,2])

setEPS()
postscript("FinalOutput/Fig3d.eps", width=8)
plot(EventStudyEstimates,type="l", col="black", xlab="Months relative to first moment of contact", ylab="Difference in probability to receive UI benefits", lty=1, xaxt="n", lwd=3, ylim=c(-1, 3))
abline(v=72, col="grey", lty=2, lwd=2 )
abline(h=0, col="grey", lty=2, lwd=2 )
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col="grey", border=NA)
lines(EventStudyEstimates, col="black", lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(0, 24, 48, 72, 96, 120, 144, 168))
legend("topright", legend=c("Event-study estimates", "95% confidence interval"), col=c("black", "grey"), lty=c(1,1), lwd=c(2,5))
dev.off()

EventStudyEstimates<-c(0,100*coeftest(DiDBijstandplm)[170:335,1])
EventStudyUpperbound<-c(0,100*coeftest(DiDBijstandplm)[170:335,1])+2*c(0, 100*coeftest(DiDBijstandplm)[170:335,2])
EventStudyLowerbound<-c(0,100*coeftest(DiDBijstandplm)[170:335,1])-2*c(0, 100*coeftest(DiDBijstandplm)[170:335,2])

setEPS()
postscript("FinalOutput/Fig3f.eps", width=8)
plot(EventStudyEstimates,type="l", col="black", xlab="Months relative to first moment of contact", ylab="Difference in probability to receive social assistance", lty=1, xaxt="n", lwd=3, ylim=c(0, 7))
abline(v=72, col="grey", lty=2, lwd=2 )
abline(h=0, col="grey", lty=2, lwd=2 )
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col="grey", border=NA)
lines(EventStudyEstimates, col="black", lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(0, 24, 48, 72, 96, 120, 144, 168))
legend("topright", legend=c("Event-study estimates", "95% confidence interval"), col=c("black", "grey"), lty=c(1,1), lwd=c(2,5))
dev.off()

EventStudyEstimates<-c(0,100*coeftest(DiDZiekteplm)[170:335,1])
EventStudyUpperbound<-c(0,100*coeftest(DiDZiekteplm)[170:335,1])+2*c(0, 100*coeftest(DiDZiekteplm)[170:335,2])
EventStudyLowerbound<-c(0,100*coeftest(DiDZiekteplm)[170:335,1])-2*c(0, 100*coeftest(DiDZiekteplm)[170:335,2])

setEPS()
postscript("FinalOutput/Fig3e.eps", width=8)
plot(EventStudyEstimates,type="l", col="black", xlab="Months relative to first moment of contact", ylab="Difference in probability to receive DI benefits", lty=1, xaxt="n", lwd=3, ylim=c(-1, 9))
abline(v=72, col="grey", lty=2, lwd=2 )
abline(h=0, col="grey", lty=2, lwd=2 )
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col="grey", border=NA)
lines(EventStudyEstimates, col="black", lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(0, 24, 48, 72, 96, 120, 144, 168))
legend("topright", legend=c("Event-study estimates", "95% confidence interval"), col=c("black", "grey"), lty=c(1,1), lwd=c(2,5))
dev.off()

EventStudyEstimates<-c(0,coeftest(DiDUrenplm)[170:335,1])
EventStudyUpperbound<-c(0,coeftest(DiDUrenplm)[170:335,1])+2*c(0, coeftest(DiDUrenplm)[170:335,2])
EventStudyLowerbound<-c(0,coeftest(DiDUrenplm)[170:335,1])-2*c(0, coeftest(DiDUrenplm)[170:335,2])

setEPS()
postscript("FinalOutput/Fig3g.eps", width=8)
plot(EventStudyEstimates,type="l", col="black", xlab="Months relative to first moment of contact", ylab="Difference in monthly working hours", lty=1, xaxt="n", lwd=3, ylim=c(-20, 5))
abline(v=72, col="grey", lty=2, lwd=2 )
abline(h=0, col="grey", lty=2, lwd=2 )
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col="grey", border=NA)
lines(EventStudyEstimates, col="black", lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(0, 24, 48, 72, 96, 120, 144, 168))
legend("topright", legend=c("Event-study estimates", "95% confidence interval"), col=c("black", "grey"), lty=c(1,1), lwd=c(2,5))
dev.off()

EventStudyEstimates<-c(0,coeftest(DiDEarningsplm)[170:335,1])
EventStudyUpperbound<-c(0,coeftest(DiDEarningsplm)[170:335,1])+2*c(0, coeftest(DiDEarningsplm)[170:335,2])
EventStudyLowerbound<-c(0,coeftest(DiDEarningsplm)[170:335,1])-2*c(0, coeftest(DiDEarningsplm)[170:335,2])

setEPS()
postscript("FinalOutput/Fig3c.eps", width=8)
time<-seq(1,167)
plot(EventStudyEstimates,type="l", col="black", xlab="Months relative to first moment of contact", ylab="Difference in monthly labor earnings", lty=1, xaxt="n", lwd=3, ylim=c(-400, 150))
abline(v=72, col="grey", lty=2, lwd=2 )
abline(h=0, col="grey", lty=2, lwd=2 )
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col="grey", border=NA)
lines(EventStudyEstimates, col="black", lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(0, 24, 48, 72, 96, 120, 144, 168))
legend("topright", legend=c("Event-study estimates", "95% confidence interval"), col=c("black", "grey"), lty=c(1,1), lwd=c(2,5))
dev.off()

#heterogeneity analysis
#By gender, age, nationality, education, diagnosis and provider
#Employment
rm(list=ls())
gc()
load("IntermediateFiles/EventStudyData.RData")
DiDIndividual%<>%
  dplyr::select(DiDEmployment,group, MonthDummies)
#delete first 24 pre-months
index<-DiDIndividual$MonthDummies>=-72
DiDIndividual<-DiDIndividual[index,]
rm(index)
DiDIndividual$MonthDummies<-as.factor(DiDIndividual$MonthDummies)
DiDIndividual$MonthDummies<-relevel(DiDIndividual$MonthDummies , ref="-72")

#Add age
MatchedSample$Leeftijd<-2012-as.numeric(as.character(MatchedSample$GBAGEBOORTEJAAR))+as.numeric(as.character(MatchedSample$IndexStartGGZ))/12

#Add individual characteristics
x<-72
y<-96
DiDGender<-matrix(NA, N*(x+y),1)
DiDAge<-matrix(NA, N*(x+y),1)
DiDNative<-matrix(NA, N*(x+y),1)
DiDEdu<-matrix(NA, N*(x+y),1)
DiDDiagnose<-matrix(NA, N*(x+y),1)
DiDBehandelaar<-matrix(NA, N*(x+y),1)

for(i in 1:N){
  DiDGender[((x+y)*(i-1)+1):((x+y)*i),1]<-MatchedSample$Man[i]
  DiDAge[((x+y)*(i-1)+1):((x+y)*i),1]<-as.character(MatchedSample$Leeftijd[i])
  DiDNative[((x+y)*(i-1)+1):((x+y)*i),1]<-as.character(MatchedSample$GBAGENERATIE[i])
  DiDEdu[((x+y)*(i-1)+1):((x+y)*i),1]<-as.character(MatchedSample$OPLNIVSOI2021AGG4HBmetNIRWO[i])
  DiDDiagnose[((x+y)*(i-1)+1):((x+y)*i),1]<-MatchedSample$GGZDBCHoofddiagnoseDSMIV[i]
  DiDBehandelaar[((x+y)*(i-1)+1):((x+y)*i),1]<-MatchedSample$Behandelaar[i]
}
rm(MatchedSample)
DiDIndividual<-cbind(DiDIndividual, DiDGender, DiDAge, DiDNative, DiDEdu, DiDDiagnose, DiDBehandelaar)
rm(DiDGender, DiDAge, DiDNative, DiDEdu, DiDDiagnose, DiDBehandelaar)
save.image("IntermediateFiles/HeterogeneityData.RData")

load("IntermediateFiles/HeterogeneityData.RData")
#output:19x2
EventStudyHeterogeneityOutput<-matrix(NA, 168, 38)

#Event-studies
#By diagnosis:
#as1_6=stemming, as1_7=angst, as2_16=persoonlijkheid
DiDIndividual%<>%
  dplyr::select(DiDEmployment, group, MonthDummies, DiDDiagnose)
indexDiag1<-substring(DiDIndividual$DiDDiagnose,1,5)=="as1_6"|substring(DiDIndividual$DiDDiagnose,1,2)=="22"
indexDiag2<-substring(DiDIndividual$DiDDiagnose,1,5)=="as1_7"|substring(DiDIndividual$DiDDiagnose,1,2)=="13"
indexDiag3<-substring(DiDIndividual$DiDDiagnose,1,6)=="as2_16"|substring(DiDIndividual$DiDDiagnose,1,2)=="14"
indexDiag4<-!(substring(DiDIndividual$DiDDiagnose,1,5)=="as1_6")&!(substring(DiDIndividual$DiDDiagnose,1,5)=="as1_7")&!(substring(DiDIndividual$DiDDiagnose,1,6)=="as2_16")&!(substring(DiDIndividual$DiDDiagnose,1,2)=="13")&!(substring(DiDIndividual$DiDDiagnose,1,2)=="14")&!(substring(DiDIndividual$DiDDiagnose,1,2)=="22")
#diag1
DiDHeterogeneity<-DiDIndividual[indexDiag1,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,1]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,2]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#diag2
DiDHeterogeneity<-DiDIndividual[indexDiag2,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,3]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,4]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#diag3
DiDHeterogeneity<-DiDIndividual[indexDiag3,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,5]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,6]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#diag4
DiDHeterogeneity<-DiDIndividual[indexDiag4,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,7]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,8]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
rm(indexDiag1, indexDiag2, indexDiag3, indexDiag4)

#By provider:
rm(DiDIndividual, DiDHeterogeneity) 
gc()
load("IntermediateFiles/HeterogeneityData.RData")
DiDIndividual%<>%
  dplyr::select(DiDEmployment, group, MonthDummies, DiDBehandelaar)
#PB= Psycholoog, PT=psychotherepeut, MB= Psychiater, Other
indexProvider1<-substring(DiDIndividual$DiDBehandelaar,1,2)=="PB"
indexProvider2<-substring(DiDIndividual$DiDBehandelaar,1,2)=="PT"
indexProvider3<-substring(DiDIndividual$DiDBehandelaar,1,2)=="MB"
indexProvider4<-!(substring(DiDIndividual$DiDBehandelaar,1,2)=="PB")&!(substring(DiDIndividual$DiDBehandelaar,1,2)=="PT")&!(substring(DiDIndividual$DiDBehandelaar,1,2)=="MB")
#provider1
DiDHeterogeneity<-DiDIndividual[indexProvider1,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,9]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,10]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#provider 2
DiDHeterogeneity<-DiDIndividual[indexProvider2,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,11]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,12]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#provider3
DiDHeterogeneity<-DiDIndividual[indexProvider3,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,13]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,14]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#provider4
DiDHeterogeneity<-DiDIndividual[indexProvider4,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,15]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,16]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
rm(indexProvider1, indexProvider2, indexProvider3, indexProvider4)

#By Gender: 0=female
rm(DiDIndividual, DiDHeterogeneity) 
gc()
load("IntermediateFiles/HeterogeneityData.RData")
DiDIndividual%<>%
  dplyr::select(DiDEmployment, group, MonthDummies, DiDGender)
indexGender1<-DiDIndividual$DiDGender==0
indexGender2<-DiDIndividual$DiDGender==1
#gender 1
DiDHeterogeneity<-DiDIndividual[indexGender1,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,17]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,18]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#gender2
DiDHeterogeneity<-DiDIndividual[indexGender2,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,19]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,20]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
rm(indexGender1, indexGender2)

#By ethnicity
rm(DiDIndividual, DiDHeterogeneity) 
gc()
load("IntermediateFiles/HeterogeneityData.RData")
DiDIndividual%<>%
  dplyr::select(DiDEmployment, group, MonthDummies, DiDNative)
indexEthnicity1<-DiDIndividual$DiDNative==0
indexEthnicity2<-DiDIndividual$DiDNative==1
indexEthnicity3<-DiDIndividual$DiDNative==2
#ethnicity1
DiDHeterogeneity<-DiDIndividual[indexEthnicity1,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,21]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,22]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#ethnciity2
DiDHeterogeneity<-DiDIndividual[indexEthnicity2,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,23]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,24]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#Ethncity3
DiDHeterogeneity<-DiDIndividual[indexEthnicity3,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,25]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,26]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
rm(indexEthnicity1, indexEthnicity2, indexEthnicity3)

#By education
rm(DiDIndividual, DiDHeterogeneity) 
gc()
load("IntermediateFiles/HeterogeneityData.RData")
DiDIndividual%<>%
  dplyr::select(DiDEmployment, group, MonthDummies, DiDEdu)
#indexEdu1<-DiDIndividual$DiDEdu=="Onbekende opleiding"
indexEdu2<-DiDIndividual$DiDEdu!="Onbekende opleiding"&as.numeric(as.character(DiDIndividual$DiDEdu))<2000
indexEdu3<-DiDIndividual$DiDEdu!="Onbekende opleiding"&as.numeric(as.character(DiDIndividual$DiDEdu))>=2000&as.numeric(as.character(DiDIndividual$DiDEdu))<3000
indexEdu4<-DiDIndividual$DiDEdu!="Onbekende opleiding"&as.numeric(as.character(DiDIndividual$DiDEdu))>=3000
#edu1
DiDHeterogeneity<-DiDIndividual[indexEdu2,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,27]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,28]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#edu2
DiDHeterogeneity<-DiDIndividual[indexEdu3,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,29]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,30]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#edu3
DiDHeterogeneity<-DiDIndividual[indexEdu4,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,31]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,32]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
rm(indexEdu1, indexEdu2, indexEdu3, indexEdu4)

#By age
rm(DiDIndividual, DiDHeterogeneity)
gc()
load("IntermediateFiles/HeterogeneityData.RData")
DiDIndividual%<>%
  dplyr::select(DiDEmployment, group, MonthDummies, DiDAge)
indexAge1<-DiDIndividual$DiDAge<35
indexAge2<-DiDIndividual$DiDAge>=35&DiDIndividual$DiDAge<50
indexAge3<-DiDIndividual$DiDAge>=50
#age1
DiDHeterogeneity<-DiDIndividual[indexAge1,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,33]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,34]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#age2
DiDHeterogeneity<-DiDIndividual[indexAge2,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,35]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,36]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
#Age3
DiDHeterogeneity<-DiDIndividual[indexAge3,]
#randomize order for coverage
Order<-sample(1:nrow(DiDHeterogeneity), nrow(DiDHeterogeneity), replace=F)
DiDHeterogeneity<-DiDHeterogeneity[Order,]
rm(Order)
numRep<-floor(nrow(DiDHeterogeneity)/100000)-1
DiDEmploymentplm<-biglm(DiDEmployment ~ group*MonthDummies, data=DiDHeterogeneity[1:100000,])
#summary(DiDEmploymentplm)
for(i in 1:numRep){
  cat(paste(i, " "));flush.console()
  DiDEmploymentplm<-update(DiDEmploymentplm, moredata=DiDHeterogeneity[(100000*i+1):(100000*(i+1)),])
  gc()
}
EventStudyHeterogeneityOutput[,37]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,1])
EventStudyHeterogeneityOutput[,38]<-c(0,100*coeftest(DiDEmploymentplm)[170:336,2])
rm(indexAge1, indexAge2, indexAge3)
rm(DiDIndividual, DiDHeterogeneity)
save.image("FinalOutput/ResultsHeterogeneityStap1.RData")

write.xlsx(EventStudyHeterogeneityOutput, "FigB5.xlsx")
