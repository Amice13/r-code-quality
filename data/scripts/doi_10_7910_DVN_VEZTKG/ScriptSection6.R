#Analysis for differences in waiting times of Section 6
#Uses intermediate files computed through "ScriptSection3and4"

#Wachttijden
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

#load data
rm(list=ls())
load("IntermediateFiles/TotalRegressionData.RData")
Wachttijd$Crisis[is.na(Wachttijd$Crisis)]<-0

#Make education groups
EducationLevel<-matrix(NA, nrow(Wachttijd),1)
for(i in 1:nrow(Wachttijd)){
  if(is.na(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[i])){
    EducationLevel[i]<-"missing"
  }
  else{
    if(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[i]!="missing"){
      if(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[i]))>=3000){
        EducationLevel[i]<-"Hoog"
      }
      if(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[i]))<3000){
        EducationLevel[i]<-"Midden"
      }
      if(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[i]))<2000){
        EducationLevel[i]<-"Laag"
      }
    }
  }
}

Wachttijd<-cbind(Wachttijd, EducationLevel)
Wachttijd$EducationLevel<-as.factor(Wachttijd$EducationLevel)
Wachttijd$EducationLevel<-relevel(Wachttijd$EducationLevel, ref="Hoog")

#Pretreatment labor market status
BijstandPre<-BijstandGGZ[,90]
EarningsPre<-EarningsGGZ[,90]
UrenPre<-UrenGGZ[,90]
WerknemerPre<-WerknemerGGZ[,90]
WWPre<-WWGGZ[,90]
ZiekteAOPre<-ZiekteAOGGZ[,90]
rm(BijstandGGZ, ScholierGGZ, SocVerOvGGZ, WerknemerGGZ, WWGGZ, ZiekteAOGGZ, NoIncomeGGZ, MentalCostGGZ, PhysicalCostGGZ, MedicijnenGGZ, TreatmentMinutesGGZ, UrenGGZ, EarningsGGZ)
Wachttijd<-cbind(Wachttijd, BijstandPre, EarningsPre, UrenPre, WerknemerPre, WWPre, ZiekteAOPre)

DifferencesEst<-matrix(NA, 5, 5)
DifferencesSE<-matrix(NA, 5, 5)

#Raw differences
RawDif<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd, data=Wachttijd[Wachttijd$Crisis==0,])
summary(RawDif)
coef<-coeftest(RawDif)
DifferencesEst[1,]<-coef[2:6, 1]
DifferencesSE[1,]<-coef[2:6, 2]

#Spatial
Spatial<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + Gemeente +WachttijdRegioInd, data=Wachttijd[Wachttijd$Crisis==0,])
summary(Spatial)
coef<-coeftest(Spatial)
DifferencesEst[2,]<-coef[2:6, 1]
DifferencesSE[2,]<-coef[2:6, 2]

#laborMarket
LaborMarket<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + Gemeente+ WachttijdRegioInd +BijstandPre + EarningsPre + UrenPre+ WerknemerPre+WWPre+ZiekteAOPre, data=Wachttijd[Wachttijd$Crisis==0,])
summary(LaborMarket)
coef<-coeftest(LaborMarket)
DifferencesEst[3,]<-coef[2:6, 1]
DifferencesSE[3,]<-coef[2:6, 2]

#Severity
Severity<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + WachttijdRegioInd +BijstandPre + EarningsPre + UrenPre+ WerknemerPre+WWPre+ZiekteAOPre+GGZDBCHoofddiagnoseDSMIV+ Gemeente, data=Wachttijd[Wachttijd$Crisis==0,])
summary(Severity)
coef<-coeftest(Severity)
DifferencesEst[4,]<-coef[2:6, 1]
DifferencesSE[4,]<-coef[2:6, 2]

#provider
Provider<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + WachttijdRegioInd +BijstandPre + EarningsPre + UrenPre+ WerknemerPre+WWPre+ZiekteAOPre+GGZDBCHoofddiagnoseDSMIV+ GGZInstelling, data=Wachttijd[Wachttijd$Crisis==0,])
summary(Provider)
coef<-coeftest(Provider)
DifferencesEst[5,]<-coef[2:6, 1]
DifferencesSE[5,]<-coef[2:6, 2]
write.xlsx(DifferencesEst, file= "FinalOutput/Tab4Est.xlsx",sheet="summary", col.names=TRUE, row.names=TRUE)
write.xlsx(DifferencesSE, file= "FinalOutput/Tab4SE.xlsx",sheet="summary", col.names=TRUE, row.names=TRUE)


#Zoom into severity: # as1_6.01.01.01.01.01 = licht, 02=matig, 03/04=ernstig
DifferencesEst<-matrix(NA, 3, 5)
DifferencesSE<-matrix(NA, 3, 5)

index<-Wachttijd$GGZDBCHoofddiagnoseDSMIV=="as1_6.01.01.01.01"
Low<-Wachttijd[index,]
index<-!is.na(Low$GGZDBCHoofddiagnoseDSMIV)
Low<-Low[index,]
index<-Wachttijd$GGZDBCHoofddiagnoseDSMIV=="as1_6.01.01.01.02"
Middle<-Wachttijd[index,]
index<-!is.na(Middle$GGZDBCHoofddiagnoseDSMIV)
Middle<-Middle[index,]
index<-Wachttijd$GGZDBCHoofddiagnoseDSMIV=="as1_6.01.01.01.03"|Wachttijd$GGZDBCHoofddiagnoseDSMIV=="as1_6.01.01.01.04"
High<-Wachttijd[index,]
index<-!is.na(High$GGZDBCHoofddiagnoseDSMIV)
High<-High[index,]

#low
LowSeverity<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + WachttijdRegioInd +BijstandPre + EarningsPre + UrenPre+ WerknemerPre+WWPre+ZiekteAOPre+ GGZInstelling, data=Low)
summary(LowSeverity)
coef<-coeftest(LowSeverity)
DifferencesEst[1,]<-coef[2:6, 1]
DifferencesSE[1,]<-coef[2:6, 2]

#laborMarket: mean =57.3
MiddleSeverity<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + WachttijdRegioInd +BijstandPre + EarningsPre + UrenPre+ WerknemerPre+WWPre+ZiekteAOPre+ GGZInstelling, data=Middle)
summary(MiddleSeverity)
coef<-coeftest(MiddleSeverity)
DifferencesEst[2,]<-coef[2:6, 1]
DifferencesSE[2,]<-coef[2:6, 2]

#laborMarket: mean = 49.4
HighSeverity<-biglm(Behandeltijd ~ Man + GBAGENERATIE + EducationLevel + Leeftijd + WachttijdRegioInd +BijstandPre + EarningsPre + UrenPre+ WerknemerPre+WWPre+ZiekteAOPre+ GGZInstelling, data=High)
summary(HighSeverity)
coef<-coeftest(HighSeverity)
DifferencesEst[3,]<-coef[2:6, 1]
DifferencesSE[3,]<-coef[2:6, 2]

write.xlsx(DifferencesEst, file= "FinalOutput/TabB7Est.xlsx",sheet="summary", col.names=TRUE, row.names=TRUE)
write.xlsx(DifferencesSE, file= "FinalOutput/TabB7SE.xlsx",sheet="summary", col.names=TRUE, row.names=TRUE)
