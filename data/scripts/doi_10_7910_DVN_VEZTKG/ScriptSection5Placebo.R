#Additional analyses for section 5 using placebo samples of individuals not receiving treatment, or individuals dropping out while on the waitlist

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
library("writexl")
library(labelled)
library(datawizard)

#select control sample of step 1
rm(list=ls())
load("IntermediateFiles/EventStudyDataStep1Extra.RData")
index<-!MatchedSample$GGZGebruik
BijstandGGZ<-BijstandGGZ[index,]
EarningsGGZ<-EarningsGGZ[index,]
MedicijnenGGZ<-MedicijnenGGZ[index,]
MentalCostGGZ<-MentalCostGGZ[index,]
NoIncomeGGZ<-NoIncomeGGZ[index,]
PhysicalCostGGZ<-PhysicalCostGGZ[index,]
ScholierGGZ<-ScholierGGZ[index,]
SocVerOvGGZ<-SocVerOvGGZ[index,]
UrenGGZ<-UrenGGZ[index,]
WerknemerGGZ<-WerknemerGGZ[index,]
WWGGZ<-WWGGZ[index,]
ZiekteAOGGZ<-ZiekteAOGGZ[index,]
Wachttijd<-MatchedSample[index,]
rm(MatchedSample, index)

#Add location information
GBAAdres<- read_sav("G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2021BUSV1.sav")
GBAAdres%<>%
  dplyr::select(RINPERSOON, RINOBJECTNUMMER, GBADATUMAANVANGADRESHOUDING,GBADATUMEINDEADRESHOUDING)
Gebouwen <- read_sav("G:/BouwenWonen/VSLGWBTAB/VSLGWB2023TAB03V1.sav")
#dplyr::select municipality 2012-2019
Gebouwen%<>%
  dplyr::select(RINOBJECTNUMMER, gem2012, gem2013, gem2014, gem2015, gem2016, gem2017, gem2018, gem2019)

index2012<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20120101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20120101
GBAAdres2012<-GBAAdres[index2012,]
GBAAdres2012<-distinct(GBAAdres2012, RINPERSOON,  .keep_all = TRUE)
GBAAdres2012<-merge(GBAAdres2012, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2012, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2012[is.na(Wachttijd$gem2012)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017, -gem2013, -gem2014, -gem2015, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[12]<-"Gemeente12"

index2013<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20130101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20130101
GBAAdres2013<-GBAAdres[index2013,]
GBAAdres2013<-distinct(GBAAdres2013, RINPERSOON,  .keep_all = TRUE)
GBAAdres2013<-merge(GBAAdres2013, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2013, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2013[is.na(Wachttijd$gem2013)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2012, -gem2014, -gem2015, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[13]<-"Gemeente13"

index2014<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20140101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20140101
GBAAdres2014<-GBAAdres[index2014,]
GBAAdres2014<-distinct(GBAAdres2014, RINPERSOON,  .keep_all = TRUE)
GBAAdres2014<-merge(GBAAdres2014, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2014, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2014[is.na(Wachttijd$gem2014)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2012, -gem2013, -gem2015, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[14]<-"Gemeente14"

index2015<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20150101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20150101
GBAAdres2015<-GBAAdres[index2015,]
GBAAdres2015<-distinct(GBAAdres2015, RINPERSOON,  .keep_all = TRUE)
GBAAdres2015<-merge(GBAAdres2015, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2015, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2015[is.na(Wachttijd$gem2015)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2013, -gem2014, -gem2012, -gem2016, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[15]<-"Gemeente15"

index2016<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20160101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20160101
GBAAdres2016<-GBAAdres[index2016,]
GBAAdres2016<-distinct(GBAAdres2016, RINPERSOON,  .keep_all = TRUE)
GBAAdres2016<-merge(GBAAdres2016, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2016, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2016[is.na(Wachttijd$gem2016)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2018, -gem2017,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[16]<-"Gemeente16"

index2017<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20170101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20170101
GBAAdres2017<-GBAAdres[index2017,]
GBAAdres2017<-distinct(GBAAdres2017, RINPERSOON,  .keep_all = TRUE)
GBAAdres2017<-merge(GBAAdres2017, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2017, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2017[is.na(Wachttijd$gem2017)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2018, -gem2016,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[17]<-"Gemeente17"

index2018<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20180101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20180101
GBAAdres2018<-GBAAdres[index2018,]
GBAAdres2018<-distinct(GBAAdres2018, RINPERSOON,  .keep_all = TRUE)
GBAAdres2018<-merge(GBAAdres2018, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2018, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2018[is.na(Wachttijd$gem2018)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2019, -gem2016, -gem2017,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[18]<-"Gemeente18"

index2019<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20190101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20190101
GBAAdres2019<-GBAAdres[index2019,]
GBAAdres2019<-distinct(GBAAdres2019, RINPERSOON,  .keep_all = TRUE)
GBAAdres2019<-merge(GBAAdres2019, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
Wachttijd<-merge(Wachttijd, GBAAdres2019, by="RINPERSOON", all.x=TRUE)
Wachttijd$gem2019[is.na(Wachttijd$gem2019)]<-"Onbekende woonplaats"
Wachttijd%<>%
  dplyr::select(-gem2016, -gem2018, -gem2017,-gem2013, -gem2014, -gem2015, -gem2012, -RINOBJECTNUMMER, -GBADATUMAANVANGADRESHOUDING, -GBADATUMEINDEADRESHOUDING)
names(Wachttijd)[19]<-"Gemeente19"

#select municipality in year of placebo start treatment
Wachttijd$Gemeente<-NA
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ<=12]<-Wachttijd$Gemeente12[Wachttijd$IndexStartGGZ<=12]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>12&Wachttijd$IndexStartGGZ<=24]<-Wachttijd$Gemeente13[Wachttijd$IndexStartGGZ>12&Wachttijd$IndexStartGGZ<=24]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>24&Wachttijd$IndexStartGGZ<=36]<-Wachttijd$Gemeente14[Wachttijd$IndexStartGGZ>24&Wachttijd$IndexStartGGZ<=36]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>36&Wachttijd$IndexStartGGZ<=48]<-Wachttijd$Gemeente15[Wachttijd$IndexStartGGZ>36&Wachttijd$IndexStartGGZ<=48]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>48&Wachttijd$IndexStartGGZ<=60]<-Wachttijd$Gemeente16[Wachttijd$IndexStartGGZ>48&Wachttijd$IndexStartGGZ<=60]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>60&Wachttijd$IndexStartGGZ<=72]<-Wachttijd$Gemeente17[Wachttijd$IndexStartGGZ>60&Wachttijd$IndexStartGGZ<=72]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>72&Wachttijd$IndexStartGGZ<=84]<-Wachttijd$Gemeente18[Wachttijd$IndexStartGGZ>72&Wachttijd$IndexStartGGZ<=84]
Wachttijd$Gemeente[Wachttijd$IndexStartGGZ>84&Wachttijd$IndexStartGGZ<=96]<-Wachttijd$Gemeente19[Wachttijd$IndexStartGGZ>84&Wachttijd$IndexStartGGZ<=96]
rm(index2012, index2013, index2014, index2015, index2016, index2017, index2018, index2019)
rm(GBAAdres, GBAAdres2012, GBAAdres2013, GBAAdres2014, GBAAdres2015, GBAAdres2016, Gebouwen, GBAAdres2017, GBAAdres2018, GBAAdres2019)
Wachttijd<-Wachttijd[,c(1:11, 20)]
save.image("IntermediateFiles/PlaceboSample.RData")

#Link instruments to individuals. Use wachttijden of last 3 months
load("IntermediateFiles/Instrumenten.RData")
load("IntemediateFiles/PlaceboSample.RData")
rm(Activiteiten)

#link to individual data
WachttijdRegioInd<-matrix(NA, nrow(Wachttijd),1)
for(i in 1:nrow(Wachttijd)){
  if(Wachttijd$IndexStartGGZ[i]>=2){
    if(i%%10000==0){
      cat(paste(i, " "));flush.console()
    }
    Regionum<-which(Regions==Wachttijd$Gemeente[i])
    if(length(Regionum)==1){
      if(sum(RegionalPatients[Regionum,max(1,Wachttijd$IndexStartGGZ[i]-2):Wachttijd$IndexStartGGZ[i]], na.rm=TRUE)>10){
        #average waiting time over past 3 months
        WachtRegio<-RegionalWaitingTime[Regionum,Wachttijd$IndexStartGGZ[i]]
        TotalWacht<-c(WachtRegio, RegionalWaitingTime[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)])
        TotalNum<-c(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1, (RegionalPatients[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
        TotalWachtSum<-TotalWacht*TotalNum
        WachttijdRegioInd[i,1]<-sum(TotalWachtSum, na.rm=TRUE)/sum(TotalNum,na.rm=TRUE)
      }
    }
  }
}

Wachttijd<-cbind(Wachttijd, WachttijdRegioInd)
rm(WachttijdRegioInd, WachttijdRegioInd1month, InstroomRegioInd,InstroomRegioInd1month,UitstroomRegioInd, UitstroomRegioInd1month)
rm(RegionalWaitingTime)

#Add time trends, age, region controls
kwb_2012 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2012.xls")
index<-kwb_2012$RECS=="G"
kwb_2012<-kwb_2012[index,]
kwb_2012%<>%
  dplyr::select(GWB_CODE12, BEV_DICHTH, P_WEST_AL, P_MAROKKO, P_TURKIJE, woningwaarde, P_KOOPWON, P_HUURCORP, P_WONT2000, INK_INW2, P_LAAGINKP, P_HOOGINKP, P_SOCMINH, WW_UIT_TOT,AO_UIT_TOT , AANT_INW)

kwb_2013 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2013.xls")
index<-kwb_2013$recs=="Gemeente"
kwb_2013<-kwb_2013[index,]
kwb_2013%<>%
  dplyr::select(gwb_code, bev_dich, p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao , a_inw)

names(kwb_2012)<-names(kwb_2013)
Wachttijd<-merge(Wachttijd, kwb_2012, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
#delete entries of diferent years
Wachttijd[Wachttijd$IndexStartGGZ>12.5,14:28]<-NA

kwb_2013$gwb_code<-substr(kwb_2013$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2013, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>12.5&Wachttijd$IndexStartGGZ<24.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>12&Wachttijd$IndexStartGGZ<25), 3:17]
#Region controls: p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao )

#Add municipality characteristics for those with NA
kwb_2014 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kerncijfers-wijken-en-buurten-2014.xls")
index<-kwb_2014$recs=="Gemeente"
kwb_2014<-kwb_2014[index,]
kwb_2014%<>%
  dplyr::select(gwb_code, bev_dich, p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

kwb_2014$gwb_code<-substr(kwb_2014$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2014, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>24.5&Wachttijd$IndexStartGGZ<36.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>24&Wachttijd$IndexStartGGZ<37), 3:17]

kwb_2015 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2015.xls")
index<-kwb_2015$recs=="Gemeente"
kwb_2015<-kwb_2015[index,]
kwb_2015%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )
kwb_2015$a_marok<-as.numeric(kwb_2015$a_marok)/as.numeric(kwb_2015$a_inw)*100
kwb_2015$a_tur<-as.numeric(kwb_2015$a_tur)/as.numeric(kwb_2015$a_inw)*100
names(kwb_2015)[4:5]<-c("p_marok", "p_tur")

p_w_all<-as.numeric(kwb_2015$a_w_all)/as.numeric(kwb_2015$a_inw)*100
kwb_2015$a_w_all<-p_w_all
names(kwb_2015)[3]<-"p_w_all"
rm(p_w_all)

kwb_2015$gwb_code<-substr(kwb_2015$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2015, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>36.5&Wachttijd$IndexStartGGZ<48.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>36&Wachttijd$IndexStartGGZ<49), 3:17]

kwb_2016<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2016.xls")
index<-kwb_2016$recs=="Gemeente"
kwb_2016<-kwb_2016[index,]
kwb_2016%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2016$a_w_all)/as.numeric(kwb_2016$a_inw)*100
kwb_2016$a_w_all<-p_w_all
names(kwb_2016)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2016$a_marok)/as.numeric(kwb_2016$a_inw)*100
kwb_2016$a_marok<-p_marok
names(kwb_2016)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2016$a_tur)/as.numeric(kwb_2016$a_inw)*100
kwb_2016$a_tur<-p_tur
names(kwb_2016)[5]<-"p_tur"
rm(p_tur)

kwb_2016$gwb_code<-substr(kwb_2016$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2016, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>48.5&Wachttijd$IndexStartGGZ<60.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>48&Wachttijd$IndexStartGGZ<61), 3:17]

kwb_2017<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2017.xls")
index<-kwb_2017$recs=="Gemeente"
kwb_2017<-kwb_2017[index,]
kwb_2017%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2017$a_w_all)/as.numeric(kwb_2017$a_inw)*100
kwb_2017$a_w_all<-p_w_all
names(kwb_2017)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2017$a_marok)/as.numeric(kwb_2017$a_inw)*100
kwb_2017$a_marok<-p_marok
names(kwb_2017)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2017$a_tur)/as.numeric(kwb_2017$a_inw)*100
kwb_2017$a_tur<-p_tur
names(kwb_2017)[5]<-"p_tur"
rm(p_tur)

kwb_2017$gwb_code<-substr(kwb_2017$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2017, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>60.5&Wachttijd$IndexStartGGZ<72.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>60&Wachttijd$IndexStartGGZ<73), 3:17]

kwb_2018<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2018.xls")
index<-kwb_2018$recs=="Gemeente"
kwb_2018<-kwb_2018[index,]
kwb_2018%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2018$a_w_all)/as.numeric(kwb_2018$a_inw)*100
kwb_2018$a_w_all<-p_w_all
names(kwb_2018)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2018$a_marok)/as.numeric(kwb_2018$a_inw)*100
kwb_2018$a_marok<-p_marok
names(kwb_2018)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2018$a_tur)/as.numeric(kwb_2018$a_inw)*100
kwb_2018$a_tur<-p_tur
names(kwb_2018)[5]<-"p_tur"
rm(p_tur)

kwb_2018$gwb_code<-substr(kwb_2018$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2018, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>72.5&Wachttijd$IndexStartGGZ<84.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>72&Wachttijd$IndexStartGGZ<85), 3:17]

kwb_2019<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2019.xlsx")
index<-kwb_2019$recs=="Gemeente"
kwb_2019<-kwb_2019[index,]
kwb_2019%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2019$a_w_all)/as.numeric(kwb_2019$a_inw)*100
kwb_2019$a_w_all<-p_w_all
names(kwb_2019)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2019$a_marok)/as.numeric(kwb_2019$a_inw)*100
kwb_2019$a_marok<-p_marok
names(kwb_2019)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2019$a_tur)/as.numeric(kwb_2019$a_inw)*100
kwb_2019$a_tur<-p_tur
names(kwb_2019)[5]<-"p_tur"
rm(p_tur)

kwb_2019$gwb_code<-substr(kwb_2019$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2019, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>84.5&Wachttijd$IndexStartGGZ<96.5), 14:28]<-Subset[(Wachttijd$IndexStartGGZ>84&Wachttijd$IndexStartGGZ<97), 3:17]

#change ww and ao to percentages
Wachttijd$a_soz_ww<-as.numeric(Wachttijd$a_soz_ww)/as.numeric(Wachttijd$a_inw)
Wachttijd$a_soz_ao<-as.numeric(Wachttijd$a_soz_ao)/as.numeric(Wachttijd$a_inw)

rm(kwb_2013, kwb_2014, kwb_2015, kwb_2016, kwb_2017, kwb_2018, kwb_2019, RegionalPatients, Subset)
save.image("IntermediateFiles/TotalRegressionPlacebo.RData")
load("IntermediateFiles/TotalRegressionPlacebo.RData")

#change g_ink_pi, g_ink_li, g_ink_hi, p_hh_osm
Wachttijd$g_ink_pi<-as.numeric(gsub(",", ".", Wachttijd$g_ink_pi))
Wachttijd$p_ink_li<-as.numeric(gsub(",", ".", Wachttijd$p_ink_li))
Wachttijd$p_ink_hi<-as.numeric(gsub(",", ".", Wachttijd$p_ink_hi))
Wachttijd$p_hh_osm<-as.numeric(gsub(",", ".", Wachttijd$p_hh_osm))

#Change variables in 10 percentile classes
class(Wachttijd$bev_dich)<-"numeric"
class(Wachttijd$p_w_all)<-"numeric"
class(Wachttijd$p_marok)<-"numeric"
class(Wachttijd$p_tur)<-"numeric"
class(Wachttijd$g_woz)<-"numeric"
class(Wachttijd$p_koopw)<-"numeric"
class(Wachttijd$p_wcorpw)<-"numeric"
class(Wachttijd$p_bjj2k)<-"numeric"
class(Wachttijd$g_ink_pi)<-"numeric"
class(Wachttijd$p_ink_li)<-"numeric"
class(Wachttijd$p_ink_hi)<-"numeric"
class(Wachttijd$p_hh_osm)<-"numeric"
class(Wachttijd$a_soz_ww)<-"numeric"
class(Wachttijd$a_soz_ao)<-"numeric"

quant<-quantile(as.numeric(Wachttijd$bev_dich), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$bev_dich, na.rm=TRUE)
Wachttijd %<>%
  mutate(bev_dich = as.factor(case_when(
    dplyr::between(bev_dich, 0, quant[1]) ~ "quantile1",
    dplyr::between(bev_dich, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(bev_dich, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(bev_dich, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(bev_dich, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(bev_dich, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(bev_dich, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(bev_dich, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(bev_dich, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(bev_dich, quant[9], maximum) ~ "quantile10",
    bev_dich== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_w_all), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_w_all, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_w_all = as.factor(case_when(
    dplyr::between(p_w_all, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_w_all, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_w_all, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_w_all, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_w_all, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_w_all, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_w_all, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_w_all, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_w_all, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_w_all, quant[9], maximum) ~ "quantile10",
    p_w_all== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_marok), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_marok, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_marok = as.factor(case_when(
    dplyr::between(p_marok, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_marok, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_marok, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_marok, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_marok, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_marok, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_marok, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_marok, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_marok, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_marok, quant[9], maximum) ~ "quantile10",
    p_marok== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_tur), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_tur, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_tur = as.factor(case_when(
    dplyr::between(p_tur, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_tur, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_tur, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_tur, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_tur, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_tur, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_tur, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_tur, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_tur, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_tur, quant[9], maximum) ~ "quantile10",
    p_tur== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$g_woz), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$g_woz, na.rm=TRUE)
Wachttijd %<>%
  mutate(g_woz = as.factor(case_when(
    dplyr::between(g_woz, 0, quant[1]) ~ "quantile1",
    dplyr::between(g_woz, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(g_woz, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(g_woz, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(g_woz, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(g_woz, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(g_woz, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(g_woz, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(g_woz, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(g_woz, quant[9], maximum) ~ "quantile10",
    g_woz== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_koopw), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_koopw, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_koopw = as.factor(case_when(
    dplyr::between(p_koopw, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_koopw, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_koopw, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_koopw, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_koopw, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_koopw, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_koopw, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_koopw, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_koopw, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_koopw, quant[9], maximum) ~ "quantile10",
    p_koopw== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_wcorpw), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_wcorpw, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_wcorpw = as.factor(case_when(
    dplyr::between(p_wcorpw, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_wcorpw, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_wcorpw, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_wcorpw, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_wcorpw, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_wcorpw, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_wcorpw, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_wcorpw, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_wcorpw, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_wcorpw, quant[9], maximum) ~ "quantile10",
    p_wcorpw== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_bjj2k), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_bjj2k, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_bjj2k = as.factor(case_when(
    dplyr::between(p_bjj2k, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_bjj2k, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_bjj2k, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_bjj2k, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_bjj2k, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_bjj2k, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_bjj2k, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_bjj2k, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_bjj2k, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_bjj2k, quant[9], maximum) ~ "quantile10",
    p_bjj2k== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$g_ink_pi), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$g_ink_pi, na.rm=TRUE)
Wachttijd %<>%
  mutate(g_ink_pi = as.factor(case_when(
    dplyr::between(g_ink_pi, 0, quant[1]) ~ "quantile1",
    dplyr::between(g_ink_pi, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(g_ink_pi, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(g_ink_pi, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(g_ink_pi, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(g_ink_pi, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(g_ink_pi, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(g_ink_pi, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(g_ink_pi, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(g_ink_pi, quant[9], maximum) ~ "quantile10",
    g_ink_pi== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_ink_li), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_ink_li, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_ink_li = as.factor(case_when(
    dplyr::between(p_ink_li, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_ink_li, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_ink_li, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_ink_li, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_ink_li, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_ink_li, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_ink_li, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_ink_li, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_ink_li, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_ink_li, quant[9], maximum) ~ "quantile10",
    p_ink_li== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_ink_hi), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_ink_hi, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_ink_hi = as.factor(case_when(
    dplyr::between(p_ink_hi, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_ink_hi, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_ink_hi, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_ink_hi, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_ink_hi, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_ink_hi, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_ink_hi, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_ink_hi, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_ink_hi, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_ink_hi, quant[9], maximum) ~ "quantile10",
    p_ink_hi== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_hh_osm), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_hh_osm, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_hh_osm = as.factor(case_when(
    dplyr::between(p_hh_osm, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_hh_osm, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_hh_osm, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_hh_osm, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_hh_osm, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_hh_osm, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_hh_osm, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_hh_osm, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_hh_osm, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_hh_osm, quant[9], maximum) ~ "quantile10",
    p_hh_osm== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$a_soz_ww), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$a_soz_ww, na.rm=TRUE)
Wachttijd %<>%
  mutate(a_soz_ww = as.factor(case_when(
    dplyr::between(a_soz_ww, 0, quant[1]) ~ "quantile1",
    dplyr::between(a_soz_ww, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(a_soz_ww, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(a_soz_ww, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(a_soz_ww, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(a_soz_ww, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(a_soz_ww, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(a_soz_ww, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(a_soz_ww, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(a_soz_ww, quant[9], maximum) ~ "quantile10",
    a_soz_ww== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$a_soz_ao), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$a_soz_ao, na.rm=TRUE)
Wachttijd %<>%
  mutate(a_soz_ao = as.factor(case_when(
    dplyr::between(a_soz_ao, 0, quant[1]) ~ "quantile1",
    dplyr::between(a_soz_ao, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(a_soz_ao, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(a_soz_ao, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(a_soz_ao, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(a_soz_ao, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(a_soz_ao, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(a_soz_ao, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(a_soz_ao, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(a_soz_ao, quant[9], maximum) ~ "quantile10",
    a_soz_ao== NA ~ "missing",
    TRUE ~ "missing"
  )))

#Add age
Leeftijd<-2012-as.numeric(as.character(Wachttijd$GBAGEBOORTEJAAR))+(as.numeric(Wachttijd$IndexStartGGZ)%/%12)
Wachttijd<-cbind(Wachttijd, Leeftijd)

#factorize age
Wachttijd %<>%
  mutate(Leeftijd = as.factor(case_when(
    dplyr::between(Leeftijd, 0, 20) ~ "<20",
    dplyr::between(Leeftijd, 20, 25) ~ "20-25",
    dplyr::between(Leeftijd, 25, 30) ~ "25-30",
    dplyr::between(Leeftijd, 30,35) ~ "30-35",
    dplyr::between(Leeftijd, 35,40) ~ "35-40",
    dplyr::between(Leeftijd, 40,45) ~ "40-45",
    dplyr::between(Leeftijd, 45,50) ~ "45-50",
    dplyr::between(Leeftijd, 50,55) ~ "50-55",
    dplyr::between(Leeftijd, 55,60) ~ "55-60",
    dplyr::between(Leeftijd, 60,65) ~ "60-65",
    dplyr::between(Leeftijd, 65,200) ~ ">60",
    Leeftijd== NA ~ "missing",
    TRUE ~ "missing"
  )))

#IV regressions with regional controls or regional dummies. 72 months before start untill 96 months after
#relevel variables
Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO<-as.factor(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO)
Wachttijd$IndexStartGGZ<-as.factor(Wachttijd$IndexStartGGZ)

#Change crisis
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, (120)])
names(Wachttijd)[30]<-"Outcome"

#Add pre controls
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 90], WWGGZ[, 90], BijstandGGZ[, 90], ZiekteAOGGZ[, 90], EarningsGGZ[, 90], UrenGGZ[, 90])
names(Wachttijd)[31:36]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")

#first stage
#delete unused data
rm(hist1, hist2, hist3, HistogramOutput, intake, NoIncomeGGZ, op, RegionalWaitingOutput, SocVerOvGGZ,Terminations, total,RealWaiting)
rm(FNoControls, FIndControls, FControls, FControls1month, FDummies, FControlsUit, FControlsCombined, F1month )
Wachttijd%<>%
  dplyr::select(-Outcome)

#Only use employment for all robustness
rm(MedicijnenGGZ, MentalCostGGZ, PhysicalCostGGZ, TreatmentMinutesGGZ)
#Estimates
EmploymentEst<-matrix(NA, 2,168)
EmploymentSE<-matrix(NA, 2, 168)

#Delete old pre-controls
Wachttijd<-Wachttijd[, 1:29]
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 25])
names(Wachttijd)[30]<-"Outcome"
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 19], WWGGZ[, 19], BijstandGGZ[, 19], ZiekteAOGGZ[, 19], EarningsGGZ[, 19], UrenGGZ[, 19])
names(Wachttijd)[31:36]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")
for(i in 1:168){
  cat(paste(i, " "));flush.console()
  gc()
  Wachttijd$Outcome<-WerknemerGGZ[, (i+24)]
  if(i<72){
    Wachttijd[,31:36]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,31:36]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  #Employment
  ControlsTotal<-lm(Outcome~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  ControlsDummies<-lm(Outcome~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + as.factor(Gemeente), data=Wachttijd)
  
  EmploymentEst[1,i]<-ControlsTotal$coefficients[2]
  EmploymentSE[1,i]<-summary(ControlsTotal)$coefficients[2,2]
  EmploymentEst[2,i]<-ControlsDummies$coefficients[2]
  EmploymentSE[2,i]<-summary(ControlsDummies)$coefficients[2,2]
  
  gc()
  rm(ControlsTotal, ControlsDummies)
}

ResultsPlacebo<-cbind(EmploymentEst, EmploymentSE)
write.xlsx(ResultsPlacebo, "FinalOutput/Fig6a.xlsx")

#Make plot
time<-seq(-72,95,1)
Estimates<-30*100*EmploymentEst[1,]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[1,]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[1,]
EstimatesFE<-30*100*EmploymentEst[2,]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[2,]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[2,]

png("FinalOutput/Fig6a.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-1.5, 1), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()
save.image("IntermediateFiles/PlaceboResults.RData")

#Analysis on drop-out sample
rm(list=ls())
load("IntermediateFiles/WachttijdenAll.RData")

#Add demographics
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

#Time trend, age
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
save.image("IntermediateFiles/DropoutWachttijden.RData")
load("IntermediateFiles/DropoutWachttijden.RData")

#Create time series of patients
#### Time series mental and non-mental health costs ##############
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
save.image("IntermediateFiles/DropOutCostData.RData")
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

save.image("IntermediateFiles/DropOutTijdreeksenCost.RData")
load("IntermediateFiles/DropOutTijdreeksenCost.RData")

#####DI/UI/Bijstand receipt
rm(list=ls())
load("IntermediateFiles/DropOutWachttijden.RData")
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
save.image("IntermediateFiles/DropOutTijdreeksenSECM.RData")

#ADD EARNINGS
rm(list=ls())
load("IntermediateFiles/DropOutWachttijden.RData")
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
Wachttijd%<>%
  dplyr::select(RINPERSOON)

#Load Polis Data
POLIS2009 <- read_sav(file="G:/Polis/POLISBUS/2009/POLISBUS2009V2.sav", col_select=c(RINPERSOON, AANVBUS, BASISLOON, BASISUREN))
index<-POLIS2009$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2009<-POLIS2009[index,]
gc()
POLIS2010 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2010V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2010$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2010<-POLIS2010[index,]
gc()
POLIS2011 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2011V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2011$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2011<-POLIS2011[index,]
gc()
POLIS2012 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2012V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2012$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2012<-POLIS2012[index,]
gc()
POLIS2013 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2013V3.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2013$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2013<-POLIS2013[index,]
gc()
POLIS2014 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2014V2.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2014$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2014<-POLIS2014[index,]
gc()
POLIS2015 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2015V4.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2015$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2015<-POLIS2015[index,]
gc()
POLIS2016 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2016V4.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2016$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2016<-POLIS2016[index,]
gc()
POLIS2017 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2017V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2017$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2017<-POLIS2017[index,]
gc()
POLIS2018 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2018V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2018$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2018<-POLIS2018[index,]
gc()
POLIS2019 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2019V7.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2019$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2019<-POLIS2019[index,]
gc()
POLIS2020 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2020V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2020$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2020<-POLIS2020[index,]
gc()
POLIS2021 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2021V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2021$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2021<-POLIS2021[index,]
gc()
POLIS2022 <- read_sav(file="G:/Spolis/SPOLISBUS/SPOLISBUS2022V6.sav", col_select=c(RINPERSOON, SDATUMAANVANGIKO, SBASISLOON, SBASISUREN))
index<-POLIS2022$RINPERSOON %in% Wachttijd$RINPERSOON
POLIS2022<-POLIS2022[index,]
gc()

names(POLIS2009)<-names(POLIS2021)
save.image("IntermediateFiles/DropOutPOLISDataGGZ.RData")
load("IntermediateFiles/DropOutPOLISDataGGZ.RData")

PolisData<-rbind(POLIS2009, POLIS2010, POLIS2011, POLIS2012, POLIS2013, POLIS2014, POLIS2015, POLIS2016, POLIS2017, POLIS2018, POLIS2019,POLIS2020, POLIS2021, POLIS2022)
PolisData<-PolisData[order(PolisData$RINPERSOON),]
indexAanvang<-as.data.frame((as.numeric(substr(PolisData$SDATUMAANVANGIKO,1,4))-2009)*12+as.numeric(substr(PolisData$SDATUMAANVANGIKO,5,6)))
names(indexAanvang)<-"indexAanvang"
rm(POLIS2009, POLIS2010, POLIS2011, POLIS2012, POLIS2013, POLIS2014, POLIS2015, POLIS2016, POLIS2017, POLIS2018, POLIS2019,POLIS2020, POLIS2021, POLIS2022)

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
#change last 3
Earnings[,166:168]<-NA
Uren[,166:168]<-NA

rm(PolisData, indexAanvang, index)
save.image("IntermediateFiles/DropOutTijdreeksenPolis.RData")

#Merge timeseries
rm(list=ls())
load("IntermediateFiles/DropOutTijdreeksenSECM.RData")
load("IntermediateFiles/DropOutTijdreeksenPolis.RData")
load("IntermediateFiles/DropOutTijdreeksenCost.RData")
load("IntermediateFiles/DropOutWachttijden.RData")

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
save.image("IntermediateFiles/DropOutFullDataStep1.RData")
load("IntermediateFiles/DropOutFullDataStep1.RData")
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

#Delete wrong ages
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
UrenGGZ<-UrenGGZ[!index,]
EarningsGGZ<-EarningsGGZ[!index,]
Wachttijd<-Wachttijd[!index,]
AgeTreatment<-AgeTreatment[!index]
AgeStart<-AgeStart[!index]
AgeEnd<-AgeEnd[!index]
rm(index)

#delete data prior to 18 years old
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
UrenGGZ<-UrenGGZ[index,]
EarningsGGZ<-EarningsGGZ[index,]
Wachttijd<-Wachttijd[index,]
rm(index)

#Delete negative or long behandeltijd
index<-Wachttijd$Behandeltijd<365&Wachttijd$Behandeltijd>0
index[is.na(index)]<-FALSE
index2<-is.na(Wachttijd$Behandeltijd)
index<-index|index2
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
UrenGGZ<-UrenGGZ[index,]
EarningsGGZ<-EarningsGGZ[index,]
Wachttijd<-Wachttijd[index,]
rm(index, index2)

#Construct indicator for drop-out
Wachttijd$DropOut<-is.na(Wachttijd$Behandeltijd)|is.na(Wachttijd$Aanmeldtijd)
save.image("IntermediateFiles/DropoutSample.RData")

#Link instruments to individuals. Use wachttijden of last 3 months
load("IntermediateFiles/Instrumenten.RData")
load("IntermediateFiles/DropoutSample.RData")

#link to individual data
WachttijdRegioInd<-matrix(NA, nrow(Wachttijd),1)
for(i in 1:nrow(Wachttijd)){
  if(Wachttijd$IndexStartGGZ[i]>=2){
    if(i%%10000==0){
      cat(paste(i, " "));flush.console()
    }
    Regionum<-which(Regions==Wachttijd$Gemeente[i])
    if(length(Regionum)==1){
      #For non dropouts
      if(!Wachttijd$DropOut[i]){
        if(sum(RegionalPatients[Regionum,max(1,Wachttijd$IndexStartGGZ[i]-2):Wachttijd$IndexStartGGZ[i]], na.rm=TRUE)>10){
          #average waiting time over past 3 months
          WachtRegio<-(RegionalWaitingTime[Regionum,Wachttijd$IndexStartGGZ[i]]*RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-Wachttijd$Behandeltijd[i])/(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1)
          TotalWacht<-c(WachtRegio, RegionalWaitingTime[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)])
          TotalNum<-c(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1, (RegionalPatients[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
          TotalWachtSum<-TotalWacht*TotalNum
          WachttijdRegioInd[i,1]<-sum(TotalWachtSum, na.rm=TRUE)/sum(TotalNum,na.rm=TRUE)
        }
      }
      #For drop-outs
      if(Wachttijd$DropOut[i]){
        if(sum(RegionalPatients[Regionum,max(1,Wachttijd$IndexStartGGZ[i]-2):Wachttijd$IndexStartGGZ[i]], na.rm=TRUE)>9){
          #average waiting time over past 3 months
          WachtRegio<-RegionalWaitingTime[Regionum,Wachttijd$IndexStartGGZ[i]]
          TotalWacht<-c(WachtRegio, RegionalWaitingTime[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)])
          TotalNum<-c(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]], (RegionalPatients[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
          TotalWachtSum<-TotalWacht*TotalNum
          WachttijdRegioInd[i,1]<-sum(TotalWachtSum, na.rm=TRUE)/sum(TotalNum,na.rm=TRUE)
        }
      }
    }
  }
}

Wachttijd<-cbind(Wachttijd, WachttijdRegioInd)
rm(WachttijdRegioInd, WachttijdRegioInd1month, InstroomRegioInd,InstroomRegioInd1month,UitstroomRegioInd, UitstroomRegioInd1month)
rm(RegionalWaitingTime)
#Delete for unknown municipality of residence
Wachttijd$WachttijdRegioInd[Wachttijd$Gemeente=="Onbekende woonplaats"]<-NA

#Add time trends, age, region controls
kwb_2012 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2012.xls")
index<-kwb_2012$RECS=="G"
kwb_2012<-kwb_2012[index,]
kwb_2012%<>%
  dplyr::select(GWB_CODE12, BEV_DICHTH, P_WEST_AL, P_MAROKKO, P_TURKIJE, woningwaarde, P_KOOPWON, P_HUURCORP, P_WONT2000, INK_INW2, P_LAAGINKP, P_HOOGINKP, P_SOCMINH, WW_UIT_TOT,AO_UIT_TOT , AANT_INW)

kwb_2013 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2013.xls")
index<-kwb_2013$recs=="Gemeente"
kwb_2013<-kwb_2013[index,]
kwb_2013%<>%
  dplyr::select(gwb_code, bev_dich, p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao , a_inw)

names(kwb_2012)<-names(kwb_2013)
Wachttijd<-merge(Wachttijd, kwb_2012, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
#delete entries of diferent years
Wachttijd[Wachttijd$IndexStartGGZ>12.5,21:35]<-NA

kwb_2013$gwb_code<-substr(kwb_2013$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2013, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>12.5&Wachttijd$IndexStartGGZ<24.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>12&Wachttijd$IndexStartGGZ<25), 3:17]
#Region controls: p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao )

#Add municipality characteristics for those with NA
kwb_2014 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2014.xls")
index<-kwb_2014$recs=="Gemeente"
kwb_2014<-kwb_2014[index,]
kwb_2014%<>%
  dplyr::select(gwb_code, bev_dich, p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

kwb_2014$gwb_code<-substr(kwb_2014$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2014, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>24.5&Wachttijd$IndexStartGGZ<36.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>24&Wachttijd$IndexStartGGZ<37), 3:17]

kwb_2015 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2015.xls")
index<-kwb_2015$recs=="Gemeente"
kwb_2015<-kwb_2015[index,]
kwb_2015%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )
kwb_2015$a_marok<-as.numeric(kwb_2015$a_marok)/as.numeric(kwb_2015$a_inw)*100
kwb_2015$a_tur<-as.numeric(kwb_2015$a_tur)/as.numeric(kwb_2015$a_inw)*100
names(kwb_2015)[4:5]<-c("p_marok", "p_tur")

p_w_all<-as.numeric(kwb_2015$a_w_all)/as.numeric(kwb_2015$a_inw)*100
kwb_2015$a_w_all<-p_w_all
names(kwb_2015)[3]<-"p_w_all"
rm(p_w_all)

kwb_2015$gwb_code<-substr(kwb_2015$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2015, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>36.5&Wachttijd$IndexStartGGZ<48.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>36&Wachttijd$IndexStartGGZ<49), 3:17]

kwb_2016<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2016.xls")
index<-kwb_2016$recs=="Gemeente"
kwb_2016<-kwb_2016[index,]
kwb_2016%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2016$a_w_all)/as.numeric(kwb_2016$a_inw)*100
kwb_2016$a_w_all<-p_w_all
names(kwb_2016)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2016$a_marok)/as.numeric(kwb_2016$a_inw)*100
kwb_2016$a_marok<-p_marok
names(kwb_2016)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2016$a_tur)/as.numeric(kwb_2016$a_inw)*100
kwb_2016$a_tur<-p_tur
names(kwb_2016)[5]<-"p_tur"
rm(p_tur)

kwb_2016$gwb_code<-substr(kwb_2016$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2016, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>48.5&Wachttijd$IndexStartGGZ<60.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>48&Wachttijd$IndexStartGGZ<61), 3:17]

kwb_2017<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2017.xls")
index<-kwb_2017$recs=="Gemeente"
kwb_2017<-kwb_2017[index,]
kwb_2017%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2017$a_w_all)/as.numeric(kwb_2017$a_inw)*100
kwb_2017$a_w_all<-p_w_all
names(kwb_2017)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2017$a_marok)/as.numeric(kwb_2017$a_inw)*100
kwb_2017$a_marok<-p_marok
names(kwb_2017)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2017$a_tur)/as.numeric(kwb_2017$a_inw)*100
kwb_2017$a_tur<-p_tur
names(kwb_2017)[5]<-"p_tur"
rm(p_tur)

kwb_2017$gwb_code<-substr(kwb_2017$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2017, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>60.5&Wachttijd$IndexStartGGZ<72.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>60&Wachttijd$IndexStartGGZ<73), 3:17]

kwb_2018<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2018.xls")
index<-kwb_2018$recs=="Gemeente"
kwb_2018<-kwb_2018[index,]
kwb_2018%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2018$a_w_all)/as.numeric(kwb_2018$a_inw)*100
kwb_2018$a_w_all<-p_w_all
names(kwb_2018)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2018$a_marok)/as.numeric(kwb_2018$a_inw)*100
kwb_2018$a_marok<-p_marok
names(kwb_2018)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2018$a_tur)/as.numeric(kwb_2018$a_inw)*100
kwb_2018$a_tur<-p_tur
names(kwb_2018)[5]<-"p_tur"
rm(p_tur)

kwb_2018$gwb_code<-substr(kwb_2018$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2018, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>72.5&Wachttijd$IndexStartGGZ<84.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>72&Wachttijd$IndexStartGGZ<85), 3:17]

kwb_2019<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/KERNCIJFERS wijk en buurt/kwb-2019.xlsx")
index<-kwb_2019$recs=="Gemeente"
kwb_2019<-kwb_2019[index,]
kwb_2019%<>%
  dplyr::select(gwb_code, bev_dich, a_w_all, a_marok, a_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao, a_inw )

p_w_all<-as.numeric(kwb_2019$a_w_all)/as.numeric(kwb_2019$a_inw)*100
kwb_2019$a_w_all<-p_w_all
names(kwb_2019)[3]<-"p_w_all"
rm(p_w_all)

p_marok<-as.numeric(kwb_2019$a_marok)/as.numeric(kwb_2019$a_inw)*100
kwb_2019$a_marok<-p_marok
names(kwb_2019)[4]<-"p_marok"
rm(p_marok)

p_tur<-as.numeric(kwb_2019$a_tur)/as.numeric(kwb_2019$a_inw)*100
kwb_2019$a_tur<-p_tur
names(kwb_2019)[5]<-"p_tur"
rm(p_tur)

kwb_2019$gwb_code<-substr(kwb_2019$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2019, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>84.5&Wachttijd$IndexStartGGZ<96.5), 21:35]<-Subset[(Wachttijd$IndexStartGGZ>84&Wachttijd$IndexStartGGZ<97), 3:17]

#change ww and ao to percentages
Wachttijd$a_soz_ww<-as.numeric(Wachttijd$a_soz_ww)/as.numeric(Wachttijd$a_inw)
Wachttijd$a_soz_ao<-as.numeric(Wachttijd$a_soz_ao)/as.numeric(Wachttijd$a_inw)

rm(kwb_2013, kwb_2014, kwb_2015, kwb_2016, kwb_2017, kwb_2018, kwb_2019, RegionalPatients, Subset)
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
save.image("IntermediateFiles/TotalRegressionDropOut.RData")
load("IntermediateFiles/TotalRegressionDropOut.RData")

#change g_ink_pi, g_ink_li, g_ink_hi, p_hh_osm
Wachttijd$g_ink_pi<-as.numeric(gsub(",", ".", Wachttijd$g_ink_pi))
Wachttijd$p_ink_li<-as.numeric(gsub(",", ".", Wachttijd$p_ink_li))
Wachttijd$p_ink_hi<-as.numeric(gsub(",", ".", Wachttijd$p_ink_hi))
Wachttijd$p_hh_osm<-as.numeric(gsub(",", ".", Wachttijd$p_hh_osm))

#Change variables in 10 percentile classes
class(Wachttijd$bev_dich)<-"numeric"
class(Wachttijd$p_w_all)<-"numeric"
class(Wachttijd$p_marok)<-"numeric"
class(Wachttijd$p_tur)<-"numeric"
class(Wachttijd$g_woz)<-"numeric"
class(Wachttijd$p_koopw)<-"numeric"
class(Wachttijd$p_wcorpw)<-"numeric"
class(Wachttijd$p_bjj2k)<-"numeric"
class(Wachttijd$g_ink_pi)<-"numeric"
class(Wachttijd$p_ink_li)<-"numeric"
class(Wachttijd$p_ink_hi)<-"numeric"
class(Wachttijd$p_hh_osm)<-"numeric"
class(Wachttijd$a_soz_ww)<-"numeric"
class(Wachttijd$a_soz_ao)<-"numeric"

quant<-quantile(as.numeric(Wachttijd$bev_dich), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$bev_dich, na.rm=TRUE)
Wachttijd %<>%
  mutate(bev_dich = as.factor(case_when(
    dplyr::between(bev_dich, 0, quant[1]) ~ "quantile1",
    dplyr::between(bev_dich, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(bev_dich, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(bev_dich, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(bev_dich, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(bev_dich, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(bev_dich, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(bev_dich, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(bev_dich, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(bev_dich, quant[9], maximum) ~ "quantile10",
    bev_dich== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_w_all), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_w_all, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_w_all = as.factor(case_when(
    dplyr::between(p_w_all, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_w_all, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_w_all, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_w_all, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_w_all, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_w_all, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_w_all, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_w_all, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_w_all, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_w_all, quant[9], maximum) ~ "quantile10",
    p_w_all== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_marok), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_marok, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_marok = as.factor(case_when(
    dplyr::between(p_marok, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_marok, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_marok, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_marok, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_marok, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_marok, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_marok, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_marok, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_marok, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_marok, quant[9], maximum) ~ "quantile10",
    p_marok== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_tur), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_tur, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_tur = as.factor(case_when(
    dplyr::between(p_tur, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_tur, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_tur, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_tur, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_tur, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_tur, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_tur, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_tur, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_tur, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_tur, quant[9], maximum) ~ "quantile10",
    p_tur== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$g_woz), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$g_woz, na.rm=TRUE)
Wachttijd %<>%
  mutate(g_woz = as.factor(case_when(
    dplyr::between(g_woz, 0, quant[1]) ~ "quantile1",
    dplyr::between(g_woz, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(g_woz, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(g_woz, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(g_woz, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(g_woz, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(g_woz, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(g_woz, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(g_woz, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(g_woz, quant[9], maximum) ~ "quantile10",
    g_woz== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_koopw), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_koopw, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_koopw = as.factor(case_when(
    dplyr::between(p_koopw, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_koopw, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_koopw, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_koopw, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_koopw, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_koopw, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_koopw, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_koopw, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_koopw, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_koopw, quant[9], maximum) ~ "quantile10",
    p_koopw== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_wcorpw), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_wcorpw, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_wcorpw = as.factor(case_when(
    dplyr::between(p_wcorpw, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_wcorpw, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_wcorpw, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_wcorpw, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_wcorpw, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_wcorpw, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_wcorpw, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_wcorpw, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_wcorpw, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_wcorpw, quant[9], maximum) ~ "quantile10",
    p_wcorpw== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_bjj2k), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_bjj2k, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_bjj2k = as.factor(case_when(
    dplyr::between(p_bjj2k, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_bjj2k, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_bjj2k, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_bjj2k, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_bjj2k, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_bjj2k, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_bjj2k, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_bjj2k, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_bjj2k, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_bjj2k, quant[9], maximum) ~ "quantile10",
    p_bjj2k== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$g_ink_pi), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$g_ink_pi, na.rm=TRUE)
Wachttijd %<>%
  mutate(g_ink_pi = as.factor(case_when(
    dplyr::between(g_ink_pi, 0, quant[1]) ~ "quantile1",
    dplyr::between(g_ink_pi, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(g_ink_pi, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(g_ink_pi, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(g_ink_pi, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(g_ink_pi, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(g_ink_pi, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(g_ink_pi, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(g_ink_pi, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(g_ink_pi, quant[9], maximum) ~ "quantile10",
    g_ink_pi== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_ink_li), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_ink_li, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_ink_li = as.factor(case_when(
    dplyr::between(p_ink_li, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_ink_li, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_ink_li, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_ink_li, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_ink_li, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_ink_li, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_ink_li, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_ink_li, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_ink_li, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_ink_li, quant[9], maximum) ~ "quantile10",
    p_ink_li== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_ink_hi), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_ink_hi, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_ink_hi = as.factor(case_when(
    dplyr::between(p_ink_hi, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_ink_hi, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_ink_hi, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_ink_hi, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_ink_hi, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_ink_hi, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_ink_hi, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_ink_hi, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_ink_hi, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_ink_hi, quant[9], maximum) ~ "quantile10",
    p_ink_hi== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_hh_osm), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_hh_osm, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_hh_osm = as.factor(case_when(
    dplyr::between(p_hh_osm, 0, quant[1]) ~ "quantile1",
    dplyr::between(p_hh_osm, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(p_hh_osm, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(p_hh_osm, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(p_hh_osm, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(p_hh_osm, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(p_hh_osm, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(p_hh_osm, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(p_hh_osm, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(p_hh_osm, quant[9], maximum) ~ "quantile10",
    p_hh_osm== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$a_soz_ww), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$a_soz_ww, na.rm=TRUE)
Wachttijd %<>%
  mutate(a_soz_ww = as.factor(case_when(
    dplyr::between(a_soz_ww, 0, quant[1]) ~ "quantile1",
    dplyr::between(a_soz_ww, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(a_soz_ww, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(a_soz_ww, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(a_soz_ww, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(a_soz_ww, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(a_soz_ww, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(a_soz_ww, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(a_soz_ww, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(a_soz_ww, quant[9], maximum) ~ "quantile10",
    a_soz_ww== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$a_soz_ao), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$a_soz_ao, na.rm=TRUE)
Wachttijd %<>%
  mutate(a_soz_ao = as.factor(case_when(
    dplyr::between(a_soz_ao, 0, quant[1]) ~ "quantile1",
    dplyr::between(a_soz_ao, quant[1], quant[2]) ~ "quantile2",
    dplyr::between(a_soz_ao, quant[2], quant[3]) ~ "quantile3",
    dplyr::between(a_soz_ao, quant[3], quant[4]) ~ "quantile4",
    dplyr::between(a_soz_ao, quant[4], quant[5]) ~ "quantile5",
    dplyr::between(a_soz_ao, quant[5], quant[6]) ~ "quantile6",
    dplyr::between(a_soz_ao, quant[6], quant[7]) ~ "quantile7",
    dplyr::between(a_soz_ao, quant[7], quant[8]) ~ "quantile8",
    dplyr::between(a_soz_ao, quant[8], quant[9]) ~ "quantile9",
    dplyr::between(a_soz_ao, quant[9], maximum) ~ "quantile10",
    a_soz_ao== NA ~ "missing",
    TRUE ~ "missing"
  )))

Wachttijd$LeeftijdLin<-Wachttijd$Leeftijd
#factorize age
Wachttijd %<>%
  mutate(Leeftijd = as.factor(case_when(
    dplyr::between(Leeftijd, 0, 20) ~ "<20",
    dplyr::between(Leeftijd, 20, 25) ~ "20-25",
    dplyr::between(Leeftijd, 25, 30) ~ "25-30",
    dplyr::between(Leeftijd, 30,35) ~ "30-35",
    dplyr::between(Leeftijd, 35,40) ~ "35-40",
    dplyr::between(Leeftijd, 40,45) ~ "40-45",
    dplyr::between(Leeftijd, 45,50) ~ "45-50",
    dplyr::between(Leeftijd, 50,55) ~ "50-55",
    dplyr::between(Leeftijd, 55,60) ~ "55-60",
    dplyr::between(Leeftijd, 60,65) ~ "60-65",
    dplyr::between(Leeftijd, 65,200) ~ ">60",
    Leeftijd== NA ~ "missing",
    TRUE ~ "missing"
  )))

#IV regressions with regional controls or regional dummies. 72 months before start untill 96 months after
#relevel variables
Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO<-as.factor(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO)
Wachttijd$IndexStartGGZ<-as.factor(Wachttijd$IndexStartGGZ)

#Make outcome
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, (120)])
names(Wachttijd)[37]<-"Outcome"

#Add pre controls
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 90], WWGGZ[, 90], BijstandGGZ[, 90], ZiekteAOGGZ[, 90], EarningsGGZ[, 90], UrenGGZ[, 90])
names(Wachttijd)[38:43]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")

save.image("IntermediateFiles/TempComposition.RData")

#Make same control variables as in other compositional regression
Wachttijd$EduCat<-"Middel"
Wachttijd$EduCat[(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))<2000)]<-"Laag"
Wachttijd$EduCat[(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))>=3000)]<-"Hoog"
Wachttijd$EduCat[Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO=="missing"]<-"Onbekend"
Wachttijd$EduCat[is.na(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO)]<-"Onbekend"
Wachttijd$EduCat<-as.factor(Wachttijd$EduCat)
Wachttijd<-within(Wachttijd, EduCat<-relevel(EduCat, ref="Laag"))

#Delete data to avoid NA
Wachttijd<-Wachttijd[!is.na(Wachttijd$WachttijdRegioInd),]
Wachttijd$IndexStartGGZ<-as.factor(Wachttijd$IndexStartGGZ)
Wachttijd$p_w_all<-as.factor(Wachttijd$p_w_all)
Wachttijd$p_marok<-as.factor(Wachttijd$p_marok)
Wachttijd$p_tur<-as.factor(Wachttijd$p_tur)
Wachttijd$g_woz<-as.factor(Wachttijd$g_woz)
Wachttijd$p_koopw<-as.factor(Wachttijd$p_koopw)
Wachttijd$p_wcorpw<-as.factor(Wachttijd$p_wcorpw)
Wachttijd$p_bjj2k<-as.factor(Wachttijd$p_bjj2k)
Wachttijd$g_ink_pi<-as.factor(Wachttijd$g_ink_pi)
Wachttijd$p_ink_li<-as.factor(Wachttijd$p_ink_li)
Wachttijd$p_ink_hi<-as.factor(Wachttijd$p_ink_hi)
Wachttijd$p_hh_osm<-as.factor(Wachttijd$p_hh_osm)
Wachttijd$a_soz_ww<-as.factor(Wachttijd$a_soz_ww)
Wachttijd$a_soz_ao<-as.factor(Wachttijd$a_soz_ao)
Wachttijd$bev_dich<-as.factor(Wachttijd$bev_dich)
#Delete indexstartggz=96 because on NA
Wachttijd<-Wachttijd[as.numeric(as.character(Wachttijd$IndexStartGGZ))!=96,]
Wachttijd$IndexStartGGZ<-as.factor(as.character(Wachttijd$IndexStartGGZ))
#Change crisis NA to zero
Wachttijd$Crisis[is.na(Wachttijd$Crisis)]<-0

#Make Start Treatment indicator
Wachttijd$StartTreatment<-!Wachttijd$DropOut

#Use regional waiting time as outcome to check differences in composition
CompositionDrop<-biglm(WachttijdRegioInd~StartTreatment + Man + LeeftijdLin + GBAGENERATIE + EduCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO + GGZDBCHoofddiagnoseDSMIV  + Behandelaar, data=Wachttijd)
CompositionTotalDrop<-biglm(WachttijdRegioInd~StartTreatment + Man + LeeftijdLin + GBAGENERATIE + EduCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO + as.character(p_w_all) +  as.character(p_marok) + as.character(p_tur) + as.character(g_woz) + as.character(p_koopw) + as.character(p_wcorpw) + as.character(p_bjj2k) + as.character(g_ink_pi) + as.character(p_ink_li) + as.character(p_ink_hi) + as.character(p_hh_osm) + as.character(a_soz_ww) + as.character(a_soz_ao) + as.character(bev_dich) + GGZDBCHoofddiagnoseDSMIV  + Behandelaar, data=Wachttijd)
CompositionTotalDummiesDrop<-biglm(WachttijdRegioInd~StartTreatment + Man + LeeftijdLin + GBAGENERATIE + EduCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + PreWW + PreBijstand + PreZiekteAO + Gemeente + GGZDBCHoofddiagnoseDSMIV  + Behandelaar, data=Wachttijd)

#Merge all Fig7 results
load("IntermediateFiles/Fig7Part2.RData")

#coefficients
CompositionResultsDrop<-matrix(NA, 1, 6)
CompositionResultsDrop[,1:2]<-coeftest(CompositionDrop)[2,1:2]
CompositionResultsDrop[,3:4]<-coeftest(CompositionTotalDrop)[2,1:2]
CompositionResultsDrop[,5:6]<-coeftest(CompositionTotalDummiesDrop)[2,1:2]
write.xlsx(CompositionResultsDrop, "IntermediateFiles/Fig7Part3.xlsx")

CompositionTotalResults<-rbind(CompositionResults, CompositionResultsDrop)
write.xlsx(CompositionTotalResults, "FinalOutput/Fig7.xlsx")

#Make Fig 7
library(dotwhisker)
library(ggplot2)

png("FinalOutput/Fig7Part1.png")
dwplot(list("No controls" = Composition, "Regional controls" = CompositionTotal, "Regional FE" = CompositionTotalDummies), vars_order = c("Man", "LeeftijdLin", "GBAGENERATIE1", "GBAGENERATIE2", "EduCatHoog", "EduCatMiddel", "EduCatOnbekend", "DiagnosisGroupMood", "DiagnosisGroupOther", "DiagnosisGroupPersonality", "BehandelaarCatPsychologist", "BehandelaarCatPsychotherapist", "PreWerknemer", "PreUren", "PreEarnings", "OtherMunicipalityTRUE"), vline = geom_vline(xintercept=0, colour="grey", linetype=2), model_order = c("No controls", "Regional controls", "Regional FE"), dot_args = list(aes(shape = model)))|>
  relabel_predictors(c(Man = "Female", LeeftijdLin = "Age*", GBAGENERATIE1 = "First gen migrant", GBAGENERATIE2 = "Second gen migrant", EduCatHoog = "High Education", EduCatMiddel = "Middle education", EduCatOnbekend = "Unknown education", DiagnosisGroupMood = "Mood diagnosis", DiagnosisGroupOther = "Other diagnosis", DiagnosisGroupPersonality = "Personality diagnosis**", BehandelaarCatPsychologist = "Psychologist", BehandelaarCatPsychotherapist = "Psychotherapist", PreWerknemer = "Pre-treatment employed", PreUren = "Pre-treatment working hours", PreEarnings = "Pre-treatment earnings", OtherMunicipalityTRUE = "Other municipality**"))+
  theme_bw(base_size = 11)+
  xlab("Difference in regional waiting time in days")+
  theme(legend.position=c(0.63,0.7), legend.justification = c(0,0), legend.title = element_blank())+
  scale_colour_grey(start=0.2, end=0.7, name = "Model", labels = c("No controls","Regional controls", "Regional FE"))+
  scale_shape_discrete(name = "Model", breaks = c(0, 0.5, 1), labels = c("No controls","Regional controls", "Regional FE"))+
  guides(shape=guide_legend("Model"), colour = guide_legend("Model")) +
  scale_x_continuous(limits =c(-1.7,1.5))
dev.off()  

png("FinalOutput/Fig7Part2.png")
dwplot(list("No controls" = CompositionDrop, "Regional controls" = CompositionTotalDrop, "Regional FE" = CompositionTotalDummiesDrop), vars_order = c("StartTreatmentTRUE"), vline = geom_vline(xintercept=0, colour="grey", linetype=2), model_order = c( "Regional FE", "No controls", "Regional controls"), dot_args = list(aes(shape = model)), xlim=c(-1.7, 1.5), ci=0.999)|>
  relabel_predictors(c(StartTreatmentTRUE = "                      Start treatment"))+
  theme_bw(base_size = 11)+
  xlab("Difference in regional waiting time in days")+
  theme(legend.position=c(1.63,0.7), legend.justification = c(0,0), legend.title = element_blank())+
  scale_colour_grey(start=0.2, end=0.7, name = "Model", labels = c("No controls","Regional controls", "Regional FE"))+
  scale_shape_discrete(name = "Model", breaks = c(0, 0.5, 1), labels = c("No controls","Regional controls", "Regional FE"))+
  guides(shape=guide_legend("Model"), colour = guide_legend("Model"))+
  scale_x_continuous(limits =c(-1.7,1.5))
dev.off()  

#first stage
rm(list=ls())
load("IntermediateFiles/TempComposition.RData")
#delete unused data
rm(hist1, hist2, hist3, HistogramOutput, intake, NoIncomeGGZ, op, RegionalWaitingOutput, SocVerOvGGZ,Terminations, total,RealWaiting)
rm(FNoControls, FIndControls, FControls, FControls1month, FDummies, FControlsUit, FControlsCombined, F1month )
Wachttijd%<>%
  dplyr::select(-Outcome)

#Only use employment for all robustness
rm(MedicijnenGGZ, MentalCostGGZ, PhysicalCostGGZ, TreatmentMinutesGGZ)
#Estimates
EmploymentEst<-matrix(NA, 4,168)
EmploymentSE<-matrix(NA, 4, 168)

#Delete old pre-controls
Wachttijd<-Wachttijd[, 1:35]
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 25])
names(Wachttijd)[36]<-"Outcome"
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 19], WWGGZ[, 19], BijstandGGZ[, 19], ZiekteAOGGZ[, 19], EarningsGGZ[, 19], UrenGGZ[, 19])
names(Wachttijd)[37:42]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")
for(i in 1:168){
  cat(paste(i, " "));flush.console()
  gc()
  Wachttijd$Outcome<-WerknemerGGZ[, (i+24)]
  if(i<72){
    Wachttijd[,37:42]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,37:42]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  #Employment
  ControlsTotal<-lm(Outcome~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  ControlsDummies<-lm(Outcome~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + as.factor(Gemeente), data=Wachttijd)
  
  EmploymentEst[1,i]<-ControlsTotal$coefficients[2]
  EmploymentSE[1,i]<-summary(ControlsTotal)$coefficients[2,2]
  EmploymentEst[2,i]<-ControlsDummies$coefficients[2]
  EmploymentSE[2,i]<-summary(ControlsDummies)$coefficients[2,2]
  
  #Only drop-outs
  ControlsTotal<-lm(Outcome~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DropOut,])
  ControlsDummies<-lm(Outcome~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + as.factor(Gemeente), data=Wachttijd[Wachttijd$DropOut,])
  
  EmploymentEst[3,i]<-ControlsTotal$coefficients[2]
  EmploymentSE[3,i]<-summary(ControlsTotal)$coefficients[2,2]
  EmploymentEst[4,i]<-ControlsDummies$coefficients[2]
  EmploymentSE[4,i]<-summary(ControlsDummies)$coefficients[2,2]
  
  gc()
  rm(ControlsTotal, ControlsDummies)
}

ResultsPlacebo<-cbind(t(EmploymentEst), t(EmploymentSE))
write.xlsx(ResultsPlacebo, "FinalOutput/Fig6bA1e.xlsx")
save.image("IntermediateFiles/DropOutResults.RData")

#Make plots
#dropouts=fig6
time<-seq(-72,95,1)
Estimates<-30*100*EmploymentEst[3,]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[3,]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[3,]
EstimatesFE<-30*100*EmploymentEst[4,]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[4,]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[4,]

png("FinalOutput/Fig6b.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-1.5, 1), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#dropouts= A1a
Estimates<-30*100*EmploymentEst[1,]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[1,]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[1,]
EstimatesFE<-30*100*EmploymentEst[2,]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[2,]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[2,]

png("FinalOutput/FigA1a.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-1.5, 1), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()