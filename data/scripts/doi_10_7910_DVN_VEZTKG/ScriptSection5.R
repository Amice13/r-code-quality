#Main analysis for impact of waiting times
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

#select sample of step 1
rm(list=ls())
load("IntermediateFiles/FullDataStep1Extra.RData")

#Make time series of average annual waiting time
AverageWaitingTime<-matrix(NA, 96,1)
for(i in 1:96){
  AverageWaitingTime[i]<-mean(Wachttijd$Behandeltijd[Wachttijd$IndexStartGGZ==i], na.rm=TRUE)
}

setEPS()
postscript("FinalOutput/FigB9.eps", width=8)
plot(AverageWaitingTime, xaxt="n", ylim=c(0, 90), type="l", lwd=2, xlab="Year", ylab="Average waiting time in days")
axis(1, labels = c("2012","2014", "2016", "2018"), at=seq(1,96,24))
dev.off()

#change outliers to fulltime treatment
for(i in 1:nrow(Wachttijd)){
  for(j in 1:192){
    if(!is.na(TreatmentMinutesGGZ[i,j])){
      if(TreatmentMinutesGGZ[i,j]>14400){
        TreatmentMinutesGGZ[i,j]<-14400
      }
    }
  }
}

#Make plots and distinguish based on wachttijd
time<-seq(-72,95,1)

setEPS()
postscript("FinalOutput/FigB6e.eps", width=8)
plot(time,100*colMeans(BijstandGGZ,na.rm=TRUE)[25:192], type="l", xlab="Months relative to first moment of contact", ylab="Probability to receive social assistance", ylim=c(0,25), lwd=2, col="white", xaxt="n")
lines(time, 100*colMeans(BijstandGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192], col="red", lwd=2)
lines(time, 100*colMeans(BijstandGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192], col="blue", lwd=2)
lines(time, 100*colMeans(BijstandGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192], col="green", lwd=2)
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
abline(v=0, col="grey", lty=2, lwd=2 )
dev.off()

setEPS()
postscript("FinalOutput/Fig4.eps", width=8)
plot(time,100*colMeans(WerknemerGGZ,na.rm=TRUE)[25:192], type="l", xlab="Months relative to first moment of contact", ylab="Probability to be employed", ylim=c(45,70), lwd=2, col="white", xaxt="n")
lines(time, 100*colMeans(WerknemerGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192], col="red", lwd=2)
lines(time, 100*colMeans(WerknemerGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192], col="blue", lwd=2)
lines(time, 100*colMeans(WerknemerGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192], col="green", lwd=2)
abline(v=0, col="grey", lty=2, lwd=2 )
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
abline(v=0, col="grey", lty=2, lwd=2 )
dev.off()

setEPS()
postscript("FinalOutput/FigB6c.eps", width=8)
plot(time,100*colMeans(WWGGZ,na.rm=TRUE)[25:192], type="l", xlab="Months relative to first moment of contact", ylab="Probability to receive UI benefits", ylim=c(0,10), lwd=2, col="white", xaxt="n")
lines(time, 100*colMeans(WWGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192], col="red", lwd=2)
lines(time, 100*colMeans(WWGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192], col="blue", lwd=2)
lines(time, 100*colMeans(WWGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192], col="green", lwd=2)
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
abline(v=0, col="grey", lty=2, lwd=2 )
dev.off()

setEPS()
postscript("FinalOutput/FigB6d.eps", width=8)
plot(time,100*colMeans(ZiekteAOGGZ,na.rm=TRUE)[25:192], type="l", xlab="Months relative to first moment of contact", ylab="Probability to receice sickness/DI benefits", ylim=c(0.0,25), lwd=2, col="white", xaxt="n")
lines(time, 100*colMeans(ZiekteAOGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192], col="red", lwd=2)
lines(time, 100*colMeans(ZiekteAOGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192], col="blue", lwd=2)
lines(time, 100*colMeans(ZiekteAOGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192], col="green", lwd=2)
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
abline(v=0, col="grey", lty=2, lwd=2 )
dev.off()

setEPS()
postscript("FinalOutput/FigB6b.eps", width=8)
plot(time,colMeans(UrenGGZ,na.rm=TRUE)[25:192], type="l", xlab="Months relative to first moment of contact", ylab="Monthly number of working hours", ylim=c(60,90), lwd=2, col="white", xaxt="n")
lines(time, colMeans(UrenGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192], col="red", lwd=2)
lines(time, colMeans(UrenGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192], col="blue", lwd=2)
lines(time, colMeans(UrenGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192], col="green", lwd=2)
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
abline(v=0, col="grey", lty=2, lwd=2 )
dev.off()

setEPS()
postscript("FinalOutput/FigB6a.eps", width=8)
plot(time,colMeans(EarningsGGZ,na.rm=TRUE)[25:192], type="l", xlab="Months relative to first moment of contact", ylab="Monthly labor earnings", ylim=c(900,2000), lwd=2, col="white", xaxt="n")
lines(time, colMeans(EarningsGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192], col="red", lwd=2)
lines(time, colMeans(EarningsGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192], col="blue", lwd=2)
lines(time, colMeans(EarningsGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192], col="green", lwd=2)
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
abline(v=0, col="grey", lty=2, lwd=2 )
dev.off()

#MakeOutput
OutputTrendsStap2<-matrix(NA, 168, 21)
OutputTrendsStap2[,1]<-100*colMeans(WerknemerGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,2]<-100*colMeans(WerknemerGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,3]<-100*colMeans(WerknemerGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,4]<-100*colMeans(WWGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,5]<-100*colMeans(WWGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,6]<-100*colMeans(WWGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,7]<-100*colMeans(BijstandGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,8]<-100*colMeans(BijstandGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,9]<-100*colMeans(BijstandGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,10]<-100*colMeans(ZiekteAOGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,11]<-100*colMeans(ZiekteAOGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,12]<-100*colMeans(ZiekteAOGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,13]<-100*colMeans(SocVerOvGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,14]<-100*colMeans(SocVerOvGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,15]<-100*colMeans(SocVerOvGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,16]<-colMeans(UrenGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,17]<-colMeans(UrenGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,18]<-colMeans(UrenGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,19]<-colMeans(EarningsGGZ[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,20]<-colMeans(EarningsGGZ[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[25:192]
OutputTrendsStap2[,21]<-colMeans(EarningsGGZ[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[25:192]
write.xlsx(OutputTrendsStap2, "FinalOutput/OutputTrendsFig4_B6.xlsx")

#Make minutes time series relative to actual start
IndexAlternative<-Wachttijd$Behandeltijd%/%30
TreatmentMinutesStart<-matrix(NA, nrow(Wachttijd), 107)
TreatmentMinutesGGZ<-TreatmentMinutesGGZ[,97:192]

for(i in 1:nrow(Wachttijd)){
  if(!is.na(Wachttijd$IndexStartGGZ[i])){
    if(!is.na(IndexAlternative[i])){
      if(IndexAlternative[i]>0){
        TreatmentMinutesStart[i,((12-IndexAlternative[i]):(107-IndexAlternative[i]))]<-TreatmentMinutesGGZ[i,]
      }
    }
    
  }
}

time<-seq(1,95,1)
plot(time,colMeans(TreatmentMinutesStart,na.rm=TRUE)[12:106], type="l", xlab="Months since the start of treatment", ylab="Monthly number of treatment minutes", ylim=c(0.0,140), lwd=2, col="white", xaxt="n")
lines(time, colMeans(TreatmentMinutesStart[Wachttijd$Behandeltijd>15&Wachttijd$Behandeltijd<60,],na.rm=TRUE)[12:106], col="red", lwd=2)
lines(time, colMeans(TreatmentMinutesStart[Wachttijd$Behandeltijd>60&Wachttijd$Behandeltijd<120,],na.rm=TRUE)[12:106], col="blue", lwd=2)
lines(time, colMeans(TreatmentMinutesStart[Wachttijd$Behandeltijd>120,],na.rm=TRUE)[12:106], col="green", lwd=2)
legend("topright", legend = c("1-2 months","2-4 months", ">4 months"), col=c("red", "blue", "green"), lty=1, lwd=2)
axis(1, labels = c("0","12", "24","36", "48", "60", "72", "84", "96"), at=c(0,12, 24,36, 48, 60, 72, 84, 96))

time<-seq(1,95,1)
plot(time,colMeans(TreatmentMinutesStart,na.rm=TRUE)[12:106], type="l", xlab="Months since the start of treatment", ylab="Monthly number of treatment minutes", ylim=c(0.0,110), lwd=2, col="black", xaxt="n")
abline(v=1, col="red", lwd=2)
abline(v=13, col="red", lwd=2)
abline(v=25, col="red", lwd=2)
axis(1, labels = c("0","12", "24","36", "48", "60", "72", "84", "96"), at=c(0,12, 24,36, 48, 60, 72, 84, 96))
colMeans(TreatmentMinutesStart,na.rm=TRUE)[13]
colMeans(TreatmentMinutesStart,na.rm=TRUE)[25]
colMeans(TreatmentMinutesStart,na.rm=TRUE)[37]

hist(TreatmentMinutesStart[TreatmentMinutesStart[,13]<300&TreatmentMinutesStart[,13]>0,][,13], n=20, xlab="Monthly minutes of treatment", col="grey")
hist(TreatmentMinutesStart[TreatmentMinutesStart[,25]<300&TreatmentMinutesStart[,25]>0,][,25], n=20, xlab="Monthly minutes of treatment", col="grey")
hist(TreatmentMinutesStart[TreatmentMinutesStart[,37]<300&TreatmentMinutesStart[,37]>0,][,37], n=20, xlab="Monthly minutes of treatment", col="grey")

hist1<-hist(TreatmentMinutesStart[TreatmentMinutesStart[,13]<300&TreatmentMinutesStart[,13]>0,][,13], n=20, xlab="Monthly minutes of treatment", col="grey", xlim=c(0, 300))
hist2<-hist(TreatmentMinutesStart[TreatmentMinutesStart[,25]<300&TreatmentMinutesStart[,25]>0,][,25], n=20, xlab="Monthly minutes of treatment", col="grey", xlim=c(0, 300))
hist3<-hist(TreatmentMinutesStart[TreatmentMinutesStart[,37]<300&TreatmentMinutesStart[,37]>0,][,37], n=20, xlab="Monthly minutes of treatment", col="grey", xlim=c(0, 300))

setEPS()
postscript("FinalOutput/FigB3a.eps", width=8)
hist(TreatmentMinutesStart[TreatmentMinutesStart[,13]<300&TreatmentMinutesStart[,13]>0,][,13], n=20, xlab="Monthly minutes of treatment", col="grey", xlim=c(0, 300))
dev.off()

setEPS()
postscript("FinalOutput/FigB3b.eps", width=8)
hist(TreatmentMinutesStart[TreatmentMinutesStart[,13]<300&TreatmentMinutesStart[,13]>0,][,13], n=20, xlab="Monthly minutes of treatment", col="grey", xlim=c(0, 300))
dev.off()

setEPS()
postscript("FinalOutput/FigB3c.eps", width=8)
hist(TreatmentMinutesStart[TreatmentMinutesStart[,13]<300&TreatmentMinutesStart[,13]>0,][,13], n=20, xlab="Monthly minutes of treatment", col="grey", xlim=c(0, 300))
dev.off()

write.xlsx(cbind(hist1$counts, hist2$counts, hist3$counts), "FinalOutput/HistogramTreatmentB3.xlsx")

#Histograms of intake time and total waiting time
setEPS()
postscript("FinalOutput/Fig2a.eps", width=8)
hist(Wachttijd$Aanmeldtijd[Wachttijd$Aanmeldtijd>0&Wachttijd$Aanmeldtijd<150], col="grey", xlab= "Intake waiting time in days", n=90)
abline(v=22, col="red", lwd=3)
dev.off()

length(which(Wachttijd$Aanmeldtijd==0))/nrow(Wachttijd)
length(which(Wachttijd$Aanmeldtijd>28))/nrow(Wachttijd)
mean(Wachttijd$Aanmeldtijd, na.rm=TRUE)

setEPS()
postscript("FinalOutput/Fig2b.eps", width=8)
hist(Wachttijd$Behandeltijd[Wachttijd$Behandeltijd>0&Wachttijd$Behandeltijd<365], col="grey", xlab= "Total waiting time in days", n=90)
dev.off()

mean(Wachttijd$Behandeltijd, na.rm=TRUE)
abline(v=56, col="red", lwd=3)
length(which(Wachttijd$Behandeltijd==0))/nrow(Wachttijd)
length(which(Wachttijd$Behandeltijd>98))/nrow(Wachttijd)

setEPS()
postscript("FinalOutput/FigB2.eps", width=8)
hist(Wachttijd$Behandeltijd[Wachttijd$Aanmeldtijd==0&Wachttijd$Behandeltijd>0&Wachttijd$Behandeltijd<365], col="grey", xlab= "Total waiting time in days", n=90)
dev.off()

#Histograms output
HistogramOutput<-matrix(NA, 90,3)
intake<-hist(Wachttijd$Aanmeldtijd[Wachttijd$Aanmeldtijd>0&Wachttijd$Aanmeldtijd<150], col="grey", xlab= "Intake waiting time in days", n=90)
total<-hist(Wachttijd$Behandeltijd[Wachttijd$Behandeltijd>0&Wachttijd$Behandeltijd<365], col="grey", xlab= "Total waiting time in days", n=90)
HistogramOutput[1:75,1]<-intake$counts
HistogramOutput[1:72,2]<-total$counts
write.xlsx(HistogramOutput, "FinalOutput/HistogramOutputFig2.xlsx")

#Extra descriptive statistics treatment sample
DescriptiveStatistics<-matrix(NA,1,11)
#Diagnosis group: 11, 12 & 1_6 is mood. 13 & 1_7 is anxiety, 14 & 2_16 is personality
Wachttijd$DiagnosisGroup<-NA
Wachttijd$DiagnosisGroup[substring(Wachttijd$GGZDBCHoofddiagnoseDSMIV,1,5)=="as1_6"]<-"Mood"
Wachttijd$DiagnosisGroup[Wachttijd$GGZDBCHoofddiagnoseDSMIV=="11"|Wachttijd$GGZDBCHoofddiagnoseDSMIV=="12"]<-"Mood"
Wachttijd$DiagnosisGroup[substring(Wachttijd$GGZDBCHoofddiagnoseDSMIV,1,5)=="as1_7"]<-"Anxiety"
Wachttijd$DiagnosisGroup[Wachttijd$GGZDBCHoofddiagnoseDSMIV=="13"]<-"Anxiety"
Wachttijd$DiagnosisGroup[substring(Wachttijd$GGZDBCHoofddiagnoseDSMIV,1,6)=="as2_16"]<-"Personality"
Wachttijd$DiagnosisGroup[Wachttijd$GGZDBCHoofddiagnoseDSMIV=="14"]<-"Personality"
Wachttijd$DiagnosisGroup[is.na(Wachttijd$DiagnosisGroup)]<-"Other"
DescriptiveStatistics[1,1]<-table(Wachttijd$DiagnosisGroup)[2]/nrow(Wachttijd)
DescriptiveStatistics[1,2]<-table(Wachttijd$DiagnosisGroup)[1]/nrow(Wachttijd)
DescriptiveStatistics[1,3]<-table(Wachttijd$DiagnosisGroup)[4]/nrow(Wachttijd)
DescriptiveStatistics[1,4]<-table(Wachttijd$DiagnosisGroup)[3]/nrow(Wachttijd)

max(colMeans(TreatmentMinutesStart, na.rm=TRUE), na.rm=TRUE)
DescriptiveStatistics[1,5]<-length(which(substring(Wachttijd$Behandelaar,1,2)=="PB"))/nrow(Wachttijd)
DescriptiveStatistics[1,6]<-length(which(substring(Wachttijd$Behandelaar,1,2)=="PT"))/nrow(Wachttijd)
DescriptiveStatistics[1,7]<-length(which(substring(Wachttijd$Behandelaar,1,2)=="MB"))/nrow(Wachttijd)
DescriptiveStatistics[1,8]<-1-DescriptiveStatistics[1,5]-DescriptiveStatistics[1,6]-DescriptiveStatistics[1,7]
DescriptiveStatistics[1,9]<-length(which(Wachttijd$Crisis==1))/nrow(Wachttijd)
DescriptiveStatistics[1,10]<-mean(TreatmentMinutesStart[,1], na.rm=TRUE)
write.xlsx(DescriptiveStatistics, "FinalOutput/Tab2DescriptivesPart2.xlsx")

#delete unneccesary data
rm(ScholierGGZ, TreatmentMinutesStart, IndexAlternative)

#Ommit crisis from regional waiting time
RealWaiting<-Wachttijd$Behandeltijd
Wachttijd$Behandeltijd[Wachttijd$Crisis==1]<-NA

#Create instrument time series
Regions<-unique(Wachttijd$Gemeente)
Regions<-Regions[order(Regions)]
Wachttijd<-Wachttijd[order(Wachttijd$Gemeente,Wachttijd$IndexStartGGZ),]

#create instrument time series for everyone
RegionalWaitingTime<-matrix(NA, length(Regions), 96)
RegionalPatients<-matrix(NA, length(Regions), 96)

for(j in 1:length(Regions)){
  cat(paste(j, " "));flush.console()
  for(t in 1:96){
    WachttijdGemeente<-Wachttijd[Wachttijd$Gemeente==Regions[j]&Wachttijd$IndexStartGGZ==t&is.na(Wachttijd$Crisis),]
    RegionalWaitingTime[j,t]<-mean(WachttijdGemeente$Behandeltijd, na.rm=TRUE)
    RegionalPatients[j,t]<-length(which(!is.na(WachttijdGemeente$Behandeltijd)))
  }
}

save.image("IntermediateFiles/Instrumenten.RData")

#Link instruments to individuals. Use waittimes of last 3 months
load("IntermediateFiles/Instrumenten.RData")
rm(Activiteiten)

#Change unknown municipality of residence to NA
RegionalWaitingTime[428,]<-NA
RegionalPatients[428,]<-NA
Terminations[428,]<-NA

#link regional waiting time to individual data
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
Wachttijd$Behandeltijd<-RealWaiting
WachttijdRegioInd<-matrix(NA, nrow(Wachttijd),1)
WachttijdRegioInd1month<-matrix(NA, nrow(Wachttijd),1)
InstroomRegioInd<-matrix(NA, nrow(Wachttijd),1)
InstroomRegioInd1month<-matrix(NA, nrow(Wachttijd),1)
UitstroomRegioInd<-matrix(NA, nrow(Wachttijd),1)
UitstroomRegioInd1month<-matrix(NA, nrow(Wachttijd),1)

for(i in 1:nrow(Wachttijd)){
  if(Wachttijd$IndexStartGGZ[i]>=2){
    if(i%%10000==0){
      cat(paste(i, " "));flush.console()
    }
    Regionum<-which(Regions==Wachttijd$Gemeente[i])
    if(length(Regionum)==1){
      if(sum(RegionalPatients[Regionum,max(1,Wachttijd$IndexStartGGZ[i]-2):Wachttijd$IndexStartGGZ[i]], na.rm=TRUE)>10){
        #average waiting time over past 3 months
        WachtRegio<-(RegionalWaitingTime[Regionum,Wachttijd$IndexStartGGZ[i]]*RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-Wachttijd$Behandeltijd[i])/(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1)
        TotalWacht<-c(WachtRegio, RegionalWaitingTime[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)])
        TotalNum<-c(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1, (RegionalPatients[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
        TotalWachtSum<-TotalWacht*TotalNum
        WachttijdRegioInd[i,1]<-sum(TotalWachtSum, na.rm=TRUE)/sum(TotalNum,na.rm=TRUE)
        #average waiting time 1 month
        if(TotalNum[1]>10){
          WachttijdRegioInd1month[i,1]<-WachtRegio
        }
        # inflow 2 months ago
        InstroomRegioInd[i,1]<-sum(TotalNum)
        InstroomRegioInd1month[i,1]<-TotalNum[1]
        
        #outflow
        TotalOutflow<-c(Terminations[Regionum,Wachttijd$IndexStartGGZ[i]], (Terminations[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
        UitstroomRegioInd[i,1]<-sum(TotalOutflow)
        UitstroomRegioInd1month[i,1]<-TotalOutflow[1]
      }
    }
  }
}

Wachttijd<-cbind(Wachttijd, WachttijdRegioInd, WachttijdRegioInd1month, InstroomRegioInd,InstroomRegioInd1month, UitstroomRegioInd, UitstroomRegioInd1month)
rm(WachttijdRegioInd, WachttijdRegioInd1month, InstroomRegioInd,InstroomRegioInd1month,UitstroomRegioInd, UitstroomRegioInd1month)
rm(RegionalWaitingTime)
save.image("IntermediateFiles/FullDataStep2.RData")
load("IntermediateFiles/FullDataStep2.RData")

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
Wachttijd[Wachttijd$IndexStartGGZ>12.5,26:40]<-NA

kwb_2013$gwb_code<-substr(kwb_2013$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2013, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>12.5&Wachttijd$IndexStartGGZ<24.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>12&Wachttijd$IndexStartGGZ<25), 3:17]
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
Wachttijd[(Wachttijd$IndexStartGGZ>24.5&Wachttijd$IndexStartGGZ<36.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>24&Wachttijd$IndexStartGGZ<37), 3:17]

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
Wachttijd[(Wachttijd$IndexStartGGZ>36.5&Wachttijd$IndexStartGGZ<48.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>36&Wachttijd$IndexStartGGZ<49), 3:17]

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
Wachttijd[(Wachttijd$IndexStartGGZ>48.5&Wachttijd$IndexStartGGZ<60.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>48&Wachttijd$IndexStartGGZ<61), 3:17]

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
Wachttijd[(Wachttijd$IndexStartGGZ>60.5&Wachttijd$IndexStartGGZ<72.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>60&Wachttijd$IndexStartGGZ<73), 3:17]

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
Wachttijd[(Wachttijd$IndexStartGGZ>72.5&Wachttijd$IndexStartGGZ<84.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>72&Wachttijd$IndexStartGGZ<85), 3:17]

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
Wachttijd[(Wachttijd$IndexStartGGZ>84.5&Wachttijd$IndexStartGGZ<96.5), 26:40]<-Subset[(Wachttijd$IndexStartGGZ>84&Wachttijd$IndexStartGGZ<97), 3:17]

#change ww and ao to percentages
Wachttijd$a_soz_ww<-as.numeric(Wachttijd$a_soz_ww)/as.numeric(Wachttijd$a_inw)
Wachttijd$a_soz_ao<-as.numeric(Wachttijd$a_soz_ao)/as.numeric(Wachttijd$a_inw)

rm(kwb_2013, kwb_2014, kwb_2015, kwb_2016, kwb_2017, kwb_2018, kwb_2019, RegionalPatients, Subset)

#add lagged and future ww and ao
kwb_2011 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2011.xls")
index<-kwb_2011$RECS=="Gemeente"
kwb_2011<-kwb_2011[index,]
kwb_2011%<>%
  dplyr::select(GWB_CODE11, WW_UIT_TOT,AO_UIT_TOT)

kwb_2012 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2012.xls")
index<-kwb_2012$RECS=="G"
kwb_2012<-kwb_2012[index,]
kwb_2012%<>%
  dplyr::select(GWB_CODE12, WW_UIT_TOT,AO_UIT_TOT )

kwb_2013 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2013.xls")
index<-kwb_2013$recs=="Gemeente"
kwb_2013<-kwb_2013[index,]
kwb_2013%<>%
  dplyr::select(gwb_code, a_soz_ww,a_soz_ao)

names(kwb_2013)[2:3]<-c("ww_lagged", "ao_lagged")
names(kwb_2011)<-names(kwb_2013)
names(kwb_2012)<-names(kwb_2013)
Wachttijd<-merge(Wachttijd, kwb_2011, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
#delete entries of diferent years
Wachttijd[Wachttijd$IndexStartGGZ>12.5,41:42]<-NA

Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2012, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>12.5&Wachttijd$IndexStartGGZ<24.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>12&Wachttijd$IndexStartGGZ<25), 3:4]

kwb_2013$gwb_code<-substr(kwb_2013$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2013, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>24.5&Wachttijd$IndexStartGGZ<36.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>24&Wachttijd$IndexStartGGZ<37), 3:4]
#Region controls: p_w_all, p_marok, p_tur, g_woz, p_koopw, p_wcorpw, p_bjj2k, g_ink_pi, p_ink_li, p_ink_hi, p_hh_osm, a_soz_ww,a_soz_ao )

kwb_2014 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kerncijfers-wijken-en-buurten-2014.xls")
index<-kwb_2014$recs=="Gemeente"
kwb_2014<-kwb_2014[index,]
kwb_2014%<>%
  dplyr::select(gwb_code, a_soz_ww,a_soz_ao)

kwb_2014$gwb_code<-substr(kwb_2014$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2014, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>36.5&Wachttijd$IndexStartGGZ<48.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>36&Wachttijd$IndexStartGGZ<49), 3:4]

kwb_2015 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2015.xls")
index<-kwb_2015$recs=="Gemeente"
kwb_2015<-kwb_2015[index,]
kwb_2015%<>%
  dplyr::select(gwb_code, a_soz_ww,a_soz_ao)

kwb_2015$gwb_code<-substr(kwb_2015$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2015, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>48.5&Wachttijd$IndexStartGGZ<60.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>48&Wachttijd$IndexStartGGZ<61), 3:4]

kwb_2016 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2016.xls")
index<-kwb_2016$recs=="Gemeente"
kwb_2016<-kwb_2016[index,]
kwb_2016%<>%
  dplyr::select(gwb_code, a_soz_ww,a_soz_ao)

kwb_2016$gwb_code<-substr(kwb_2016$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2016, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>60.5&Wachttijd$IndexStartGGZ<72.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>60&Wachttijd$IndexStartGGZ<73), 3:4]

kwb_2017 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2017.xls")
index<-kwb_2017$recs=="Gemeente"
kwb_2017<-kwb_2017[index,]
kwb_2017%<>%
  dplyr::select(gwb_code, a_soz_ww,a_soz_ao)

kwb_2017$gwb_code<-substr(kwb_2017$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2017, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>72.5&Wachttijd$IndexStartGGZ<84.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>72&Wachttijd$IndexStartGGZ<85), 3:4]

kwb_2018 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2018.xls")
index<-kwb_2018$recs=="Gemeente"
kwb_2018<-kwb_2018[index,]
kwb_2018%<>%
  dplyr::select(gwb_code, a_soz_ww,a_soz_ao)

kwb_2018$gwb_code<-substr(kwb_2018$gwb_code,3,6)
Subset<-Wachttijd[,1:2]
Subset<-merge(Subset, kwb_2018, by.x="Gemeente", by.y="gwb_code", all.x=TRUE)
Wachttijd[(Wachttijd$IndexStartGGZ>84.5&Wachttijd$IndexStartGGZ<96.5), 41:42]<-Subset[(Wachttijd$IndexStartGGZ>84&Wachttijd$IndexStartGGZ<97), 3:4]

#change ww and ao to percentages
Wachttijd$ww_lagged<-as.numeric(Wachttijd$ww_lagged)/as.numeric(Wachttijd$a_inw)
Wachttijd$ao_lagged<-as.numeric(Wachttijd$ao_lagged)/as.numeric(Wachttijd$a_inw)

rm(kwb_2013, kwb_2014, kwb_2015, kwb_2016, kwb_2017, kwb_2018, AfstandenUni, RegionalPatients, Subset, Gemeentecode,Hoofddiagnose, index)
rm(kwb_2011, kwb_2012, kwb_2017, WachttijdGemeente, WaitedAverage, IndexAlternative, test)
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]

#Add change in ao and WW
Wachttijd$ao_change<-(Wachttijd$a_soz_ao-Wachttijd$ao_lagged)/Wachttijd$ao_lagged
Wachttijd$ww_change<-(Wachttijd$a_soz_ww-Wachttijd$ww_lagged)/Wachttijd$ww_lagged

save.image("IntermediateFiles/TotalRegression.RData")
load("IntermediateFiles/TotalRegression.RData")

#hist of regional waiting time
setEPS()
postscript("FinalOutput/FigB8.eps", width=8)
hist(Wachttijd$WachttijdRegioInd[Wachttijd$WachttijdRegioInd<150],n=100, xlab="Average regional waiting time in days", col="grey")
dev.off()

mean(Wachttijd$WachttijdRegioInd, na.rm=TRUE)
sd(Wachttijd$WachttijdRegioInd, na.rm=TRUE)
abline(v=64, col="red", lwd=4)
Regio<-hist(Wachttijd$WachttijdRegioInd[Wachttijd$WachttijdRegioInd<150],n=90, xlab="Average regional waiting time in days", col="grey")
write.xlsx(Regio$counts, "FinalOutput/HistogramOutputFigB8.xlsx")

mean(Wachttijd$Behandeltijd, na.rm=TRUE)
sd(Wachttijd$Behandeltijd, na.rm=TRUE)
mean(Wachttijd$Aanmeldtijd[Wachttijd$Aanmeldtijd>0], na.rm=TRUE)
sd(Wachttijd$Aanmeldtijd[Wachttijd$Aanmeldtijd>0], na.rm=TRUE)
length(which(Wachttijd$Aanmeldtijd==0))/nrow(Wachttijd)
mean(Wachttijd$WachttijdRegioInd, na.rm=TRUE)
sd(Wachttijd$WachttijdRegioInd, na.rm=TRUE)

#copy number of inhabitants and age
Wachttijd$inwoners<-Wachttijd$a_inw
Wachttijd$LeeftijdLin<-Wachttijd$Leeftijd

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
class(Wachttijd$ao_lagged)<-"numeric"
class(Wachttijd$ww_lagged)<-"numeric"
class(Wachttijd$ww_change)<-"numeric"
class(Wachttijd$ao_change)<-"numeric"

quant<-quantile(as.numeric(Wachttijd$bev_dich), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$bev_dich, na.rm=TRUE)
Wachttijd %<>%
  mutate(bev_dich = as.factor(case_when(
    between(bev_dich, 0, quant[1]) ~ "quantile1",
    between(bev_dich, quant[1], quant[2]) ~ "quantile2",
    between(bev_dich, quant[2], quant[3]) ~ "quantile3",
    between(bev_dich, quant[3], quant[4]) ~ "quantile4",
    between(bev_dich, quant[4], quant[5]) ~ "quantile5",
    between(bev_dich, quant[5], quant[6]) ~ "quantile6",
    between(bev_dich, quant[6], quant[7]) ~ "quantile7",
    between(bev_dich, quant[7], quant[8]) ~ "quantile8",
    between(bev_dich, quant[8], quant[9]) ~ "quantile9",
    between(bev_dich, quant[9], maximum) ~ "quantile10",
    bev_dich== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_w_all), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_w_all, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_w_all = as.factor(case_when(
    between(p_w_all, 0, quant[1]) ~ "quantile1",
    between(p_w_all, quant[1], quant[2]) ~ "quantile2",
    between(p_w_all, quant[2], quant[3]) ~ "quantile3",
    between(p_w_all, quant[3], quant[4]) ~ "quantile4",
    between(p_w_all, quant[4], quant[5]) ~ "quantile5",
    between(p_w_all, quant[5], quant[6]) ~ "quantile6",
    between(p_w_all, quant[6], quant[7]) ~ "quantile7",
    between(p_w_all, quant[7], quant[8]) ~ "quantile8",
    between(p_w_all, quant[8], quant[9]) ~ "quantile9",
    between(p_w_all, quant[9], maximum) ~ "quantile10",
    p_w_all== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_marok), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_marok, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_marok = as.factor(case_when(
    between(p_marok, 0, quant[1]) ~ "quantile1",
    between(p_marok, quant[1], quant[2]) ~ "quantile2",
    between(p_marok, quant[2], quant[3]) ~ "quantile3",
    between(p_marok, quant[3], quant[4]) ~ "quantile4",
    between(p_marok, quant[4], quant[5]) ~ "quantile5",
    between(p_marok, quant[5], quant[6]) ~ "quantile6",
    between(p_marok, quant[6], quant[7]) ~ "quantile7",
    between(p_marok, quant[7], quant[8]) ~ "quantile8",
    between(p_marok, quant[8], quant[9]) ~ "quantile9",
    between(p_marok, quant[9], maximum) ~ "quantile10",
    p_marok== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_tur), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_tur, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_tur = as.factor(case_when(
    between(p_tur, 0, quant[1]) ~ "quantile1",
    between(p_tur, quant[1], quant[2]) ~ "quantile2",
    between(p_tur, quant[2], quant[3]) ~ "quantile3",
    between(p_tur, quant[3], quant[4]) ~ "quantile4",
    between(p_tur, quant[4], quant[5]) ~ "quantile5",
    between(p_tur, quant[5], quant[6]) ~ "quantile6",
    between(p_tur, quant[6], quant[7]) ~ "quantile7",
    between(p_tur, quant[7], quant[8]) ~ "quantile8",
    between(p_tur, quant[8], quant[9]) ~ "quantile9",
    between(p_tur, quant[9], maximum) ~ "quantile10",
    p_tur== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$g_woz), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$g_woz, na.rm=TRUE)
Wachttijd %<>%
  mutate(g_woz = as.factor(case_when(
    between(g_woz, 0, quant[1]) ~ "quantile1",
    between(g_woz, quant[1], quant[2]) ~ "quantile2",
    between(g_woz, quant[2], quant[3]) ~ "quantile3",
    between(g_woz, quant[3], quant[4]) ~ "quantile4",
    between(g_woz, quant[4], quant[5]) ~ "quantile5",
    between(g_woz, quant[5], quant[6]) ~ "quantile6",
    between(g_woz, quant[6], quant[7]) ~ "quantile7",
    between(g_woz, quant[7], quant[8]) ~ "quantile8",
    between(g_woz, quant[8], quant[9]) ~ "quantile9",
    between(g_woz, quant[9], maximum) ~ "quantile10",
    g_woz== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_koopw), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_koopw, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_koopw = as.factor(case_when(
    between(p_koopw, 0, quant[1]) ~ "quantile1",
    between(p_koopw, quant[1], quant[2]) ~ "quantile2",
    between(p_koopw, quant[2], quant[3]) ~ "quantile3",
    between(p_koopw, quant[3], quant[4]) ~ "quantile4",
    between(p_koopw, quant[4], quant[5]) ~ "quantile5",
    between(p_koopw, quant[5], quant[6]) ~ "quantile6",
    between(p_koopw, quant[6], quant[7]) ~ "quantile7",
    between(p_koopw, quant[7], quant[8]) ~ "quantile8",
    between(p_koopw, quant[8], quant[9]) ~ "quantile9",
    between(p_koopw, quant[9], maximum) ~ "quantile10",
    p_koopw== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_wcorpw), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_wcorpw, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_wcorpw = as.factor(case_when(
    between(p_wcorpw, 0, quant[1]) ~ "quantile1",
    between(p_wcorpw, quant[1], quant[2]) ~ "quantile2",
    between(p_wcorpw, quant[2], quant[3]) ~ "quantile3",
    between(p_wcorpw, quant[3], quant[4]) ~ "quantile4",
    between(p_wcorpw, quant[4], quant[5]) ~ "quantile5",
    between(p_wcorpw, quant[5], quant[6]) ~ "quantile6",
    between(p_wcorpw, quant[6], quant[7]) ~ "quantile7",
    between(p_wcorpw, quant[7], quant[8]) ~ "quantile8",
    between(p_wcorpw, quant[8], quant[9]) ~ "quantile9",
    between(p_wcorpw, quant[9], maximum) ~ "quantile10",
    p_wcorpw== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_bjj2k), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_bjj2k, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_bjj2k = as.factor(case_when(
    between(p_bjj2k, 0, quant[1]) ~ "quantile1",
    between(p_bjj2k, quant[1], quant[2]) ~ "quantile2",
    between(p_bjj2k, quant[2], quant[3]) ~ "quantile3",
    between(p_bjj2k, quant[3], quant[4]) ~ "quantile4",
    between(p_bjj2k, quant[4], quant[5]) ~ "quantile5",
    between(p_bjj2k, quant[5], quant[6]) ~ "quantile6",
    between(p_bjj2k, quant[6], quant[7]) ~ "quantile7",
    between(p_bjj2k, quant[7], quant[8]) ~ "quantile8",
    between(p_bjj2k, quant[8], quant[9]) ~ "quantile9",
    between(p_bjj2k, quant[9], maximum) ~ "quantile10",
    p_bjj2k== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$g_ink_pi), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$g_ink_pi, na.rm=TRUE)
Wachttijd %<>%
  mutate(g_ink_pi = as.factor(case_when(
    between(g_ink_pi, 0, quant[1]) ~ "quantile1",
    between(g_ink_pi, quant[1], quant[2]) ~ "quantile2",
    between(g_ink_pi, quant[2], quant[3]) ~ "quantile3",
    between(g_ink_pi, quant[3], quant[4]) ~ "quantile4",
    between(g_ink_pi, quant[4], quant[5]) ~ "quantile5",
    between(g_ink_pi, quant[5], quant[6]) ~ "quantile6",
    between(g_ink_pi, quant[6], quant[7]) ~ "quantile7",
    between(g_ink_pi, quant[7], quant[8]) ~ "quantile8",
    between(g_ink_pi, quant[8], quant[9]) ~ "quantile9",
    between(g_ink_pi, quant[9], maximum) ~ "quantile10",
    g_ink_pi== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_ink_li), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_ink_li, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_ink_li = as.factor(case_when(
    between(p_ink_li, 0, quant[1]) ~ "quantile1",
    between(p_ink_li, quant[1], quant[2]) ~ "quantile2",
    between(p_ink_li, quant[2], quant[3]) ~ "quantile3",
    between(p_ink_li, quant[3], quant[4]) ~ "quantile4",
    between(p_ink_li, quant[4], quant[5]) ~ "quantile5",
    between(p_ink_li, quant[5], quant[6]) ~ "quantile6",
    between(p_ink_li, quant[6], quant[7]) ~ "quantile7",
    between(p_ink_li, quant[7], quant[8]) ~ "quantile8",
    between(p_ink_li, quant[8], quant[9]) ~ "quantile9",
    between(p_ink_li, quant[9], maximum) ~ "quantile10",
    p_ink_li== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_ink_hi), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_ink_hi, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_ink_hi = as.factor(case_when(
    between(p_ink_hi, 0, quant[1]) ~ "quantile1",
    between(p_ink_hi, quant[1], quant[2]) ~ "quantile2",
    between(p_ink_hi, quant[2], quant[3]) ~ "quantile3",
    between(p_ink_hi, quant[3], quant[4]) ~ "quantile4",
    between(p_ink_hi, quant[4], quant[5]) ~ "quantile5",
    between(p_ink_hi, quant[5], quant[6]) ~ "quantile6",
    between(p_ink_hi, quant[6], quant[7]) ~ "quantile7",
    between(p_ink_hi, quant[7], quant[8]) ~ "quantile8",
    between(p_ink_hi, quant[8], quant[9]) ~ "quantile9",
    between(p_ink_hi, quant[9], maximum) ~ "quantile10",
    p_ink_hi== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$p_hh_osm), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$p_hh_osm, na.rm=TRUE)
Wachttijd %<>%
  mutate(p_hh_osm = as.factor(case_when(
    between(p_hh_osm, 0, quant[1]) ~ "quantile1",
    between(p_hh_osm, quant[1], quant[2]) ~ "quantile2",
    between(p_hh_osm, quant[2], quant[3]) ~ "quantile3",
    between(p_hh_osm, quant[3], quant[4]) ~ "quantile4",
    between(p_hh_osm, quant[4], quant[5]) ~ "quantile5",
    between(p_hh_osm, quant[5], quant[6]) ~ "quantile6",
    between(p_hh_osm, quant[6], quant[7]) ~ "quantile7",
    between(p_hh_osm, quant[7], quant[8]) ~ "quantile8",
    between(p_hh_osm, quant[8], quant[9]) ~ "quantile9",
    between(p_hh_osm, quant[9], maximum) ~ "quantile10",
    p_hh_osm== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$a_soz_ww), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$a_soz_ww, na.rm=TRUE)
Wachttijd %<>%
  mutate(a_soz_ww = as.factor(case_when(
    between(a_soz_ww, 0, quant[1]) ~ "quantile1",
    between(a_soz_ww, quant[1], quant[2]) ~ "quantile2",
    between(a_soz_ww, quant[2], quant[3]) ~ "quantile3",
    between(a_soz_ww, quant[3], quant[4]) ~ "quantile4",
    between(a_soz_ww, quant[4], quant[5]) ~ "quantile5",
    between(a_soz_ww, quant[5], quant[6]) ~ "quantile6",
    between(a_soz_ww, quant[6], quant[7]) ~ "quantile7",
    between(a_soz_ww, quant[7], quant[8]) ~ "quantile8",
    between(a_soz_ww, quant[8], quant[9]) ~ "quantile9",
    between(a_soz_ww, quant[9], maximum) ~ "quantile10",
    a_soz_ww== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$a_soz_ao), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$a_soz_ao, na.rm=TRUE)
Wachttijd %<>%
  mutate(a_soz_ao = as.factor(case_when(
    between(a_soz_ao, 0, quant[1]) ~ "quantile1",
    between(a_soz_ao, quant[1], quant[2]) ~ "quantile2",
    between(a_soz_ao, quant[2], quant[3]) ~ "quantile3",
    between(a_soz_ao, quant[3], quant[4]) ~ "quantile4",
    between(a_soz_ao, quant[4], quant[5]) ~ "quantile5",
    between(a_soz_ao, quant[5], quant[6]) ~ "quantile6",
    between(a_soz_ao, quant[6], quant[7]) ~ "quantile7",
    between(a_soz_ao, quant[7], quant[8]) ~ "quantile8",
    between(a_soz_ao, quant[8], quant[9]) ~ "quantile9",
    between(a_soz_ao, quant[9], maximum) ~ "quantile10",
    a_soz_ao== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$ww_lagged), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$ww_lagged, na.rm=TRUE)
Wachttijd %<>%
  mutate(ww_lagged = as.factor(case_when(
    between(ww_lagged, 0, quant[1]) ~ "quantile1",
    between(ww_lagged, quant[1], quant[2]) ~ "quantile2",
    between(ww_lagged, quant[2], quant[3]) ~ "quantile3",
    between(ww_lagged, quant[3], quant[4]) ~ "quantile4",
    between(ww_lagged, quant[4], quant[5]) ~ "quantile5",
    between(ww_lagged, quant[5], quant[6]) ~ "quantile6",
    between(ww_lagged, quant[6], quant[7]) ~ "quantile7",
    between(ww_lagged, quant[7], quant[8]) ~ "quantile8",
    between(ww_lagged, quant[8], quant[9]) ~ "quantile9",
    between(ww_lagged, quant[9], maximum) ~ "quantile10",
    ww_lagged== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$ao_lagged), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$ao_lagged, na.rm=TRUE)
Wachttijd %<>%
  mutate(ao_lagged = as.factor(case_when(
    between(ao_lagged, 0, quant[1]) ~ "quantile1",
    between(ao_lagged, quant[1], quant[2]) ~ "quantile2",
    between(ao_lagged, quant[2], quant[3]) ~ "quantile3",
    between(ao_lagged, quant[3], quant[4]) ~ "quantile4",
    between(ao_lagged, quant[4], quant[5]) ~ "quantile5",
    between(ao_lagged, quant[5], quant[6]) ~ "quantile6",
    between(ao_lagged, quant[6], quant[7]) ~ "quantile7",
    between(ao_lagged, quant[7], quant[8]) ~ "quantile8",
    between(ao_lagged, quant[8], quant[9]) ~ "quantile9",
    between(ao_lagged, quant[9], maximum) ~ "quantile10",
    ao_lagged== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$ao_change), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$ao_change, na.rm=TRUE)
Wachttijd %<>%
  mutate(ao_change = as.factor(case_when(
    between(ao_change, 0, quant[1]) ~ "quantile1",
    between(ao_change, quant[1], quant[2]) ~ "quantile2",
    between(ao_change, quant[2], quant[3]) ~ "quantile3",
    between(ao_change, quant[3], quant[4]) ~ "quantile4",
    between(ao_change, quant[4], quant[5]) ~ "quantile5",
    between(ao_change, quant[5], quant[6]) ~ "quantile6",
    between(ao_change, quant[6], quant[7]) ~ "quantile7",
    between(ao_change, quant[7], quant[8]) ~ "quantile8",
    between(ao_change, quant[8], quant[9]) ~ "quantile9",
    between(ao_change, quant[9], maximum) ~ "quantile10",
    ao_change== NA ~ "missing",
    TRUE ~ "missing"
  )))

quant<-quantile(as.numeric(Wachttijd$ww_change), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm=TRUE)
maximum<-max(Wachttijd$ww_change, na.rm=TRUE)
Wachttijd %<>%
  mutate(ww_change = as.factor(case_when(
    between(ww_change, 0, quant[1]) ~ "quantile1",
    between(ww_change, quant[1], quant[2]) ~ "quantile2",
    between(ww_change, quant[2], quant[3]) ~ "quantile3",
    between(ww_change, quant[3], quant[4]) ~ "quantile4",
    between(ww_change, quant[4], quant[5]) ~ "quantile5",
    between(ww_change, quant[5], quant[6]) ~ "quantile6",
    between(ww_change, quant[6], quant[7]) ~ "quantile7",
    between(ww_change, quant[7], quant[8]) ~ "quantile8",
    between(ww_change, quant[8], quant[9]) ~ "quantile9",
    between(ww_change, quant[9], maximum) ~ "quantile10",
    ww_change== NA ~ "missing",
    TRUE ~ "missing"
  )))


#factorize age
Wachttijd %<>%
  mutate(Leeftijd = as.factor(case_when(
    between(Leeftijd, 0, 20) ~ "<20",
    between(Leeftijd, 20, 25) ~ "20-25",
    between(Leeftijd, 25, 30) ~ "25-30",
    between(Leeftijd, 30,35) ~ "30-35",
    between(Leeftijd, 35,40) ~ "35-40",
    between(Leeftijd, 40,45) ~ "40-45",
    between(Leeftijd, 45,50) ~ "45-50",
    between(Leeftijd, 50,55) ~ "50-55",
    between(Leeftijd, 55,60) ~ "55-60",
    between(Leeftijd, 60,65) ~ "60-65",
    between(Leeftijd, 65,200) ~ ">60",
    Leeftijd== NA ~ "missing",
    TRUE ~ "missing"
  )))


#Change education
Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[is.na(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO)]<-"missing"

#IV regressions with regional controls or regional dummies. 72 months before start untill 96 months after
#relevel variables
Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO<-as.factor(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO)
Wachttijd$IndexStartGGZ<-as.factor(Wachttijd$IndexStartGGZ)

#Change crisis
Wachttijd$Crisis[is.na(Wachttijd$Crisis)]<-0
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, (120)])
names(Wachttijd)[51]<-"Outcome"

#Add pre controls
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 90], WWGGZ[, 90], BijstandGGZ[, 90], ZiekteAOGGZ[, 90], EarningsGGZ[, 90], UrenGGZ[, 90])
names(Wachttijd)[52:57]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")

save.image("IntermediateFiles/TotalRegressionData.RData")
load("IntermediateFiles/TotalRegressionData.RData")

#Make low/middle/high edu for composition analysis
Wachttijd$EduCat<-"Middel"
Wachttijd$EduCat[(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))<2000)]<-"Laag"
Wachttijd$EduCat[(as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))>=3000)]<-"Hoog"
Wachttijd$EduCat[Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO=="missing"]<-"Onbekend"
Wachttijd$EduCat[is.na(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO)]<-"Onbekend"
Wachttijd$EduCat<-as.factor(Wachttijd$EduCat)
Wachttijd<-within(Wachttijd, EduCat<-relevel(EduCat, ref="Laag"))

#Make provider category
Wachttijd$BehandelaarCat<-"Other"
Wachttijd$BehandelaarCat[substring(Wachttijd$Behandelaar,1,2)=="PB"]<-"Psychologist"
Wachttijd$BehandelaarCat[substring(Wachttijd$Behandelaar,1,2)=="PT"]<-"Psychotherapist"
Wachttijd$BehandelaarCat<-as.factor(Wachttijd$BehandelaarCat)
Wachttijd<-within(Wachttijd, BehandelaarCat<-relevel(BehandelaarCat, ref="Other"))

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

#Use regional waiting time as outcome to check differences in composition
Composition<-biglm(WachttijdRegioInd~Man + LeeftijdLin + GBAGENERATIE + EduCat+ DiagnosisGroup + BehandelaarCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO, data=Wachttijd)
CompositionTotal<-biglm(WachttijdRegioInd~Man + LeeftijdLin + GBAGENERATIE + EduCat+ DiagnosisGroup + BehandelaarCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO + as.character(p_w_all) +  as.character(p_marok) + as.character(p_tur) + as.character(g_woz) + as.character(p_koopw) + as.character(p_wcorpw) + as.character(p_bjj2k) + as.character(g_ink_pi) + as.character(p_ink_li) + as.character(p_ink_hi) + as.character(p_hh_osm) + as.character(a_soz_ww) + as.character(a_soz_ao) + as.character(bev_dich), data=Wachttijd)
CompositionTotalDummies<-biglm(WachttijdRegioInd~Man + LeeftijdLin + GBAGENERATIE + EduCat+ DiagnosisGroup + BehandelaarCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO + Gemeente, data=Wachttijd)

#coefficients
CompositionResults<-matrix(NA, 13, 6)
CompositionResults[,1:2]<-coeftest(Composition)[2:14,1:2]
CompositionResults[,3:4]<-coeftest(CompositionTotal)[2:14,1:2]
CompositionResults[,5:6]<-coeftest(CompositionTotalDummies)[2:14,1:2]
write.xlsx(CompositionResults, "IntermediateFiles/Fig7.xlsx")

#F-tests on compositional
linearHypothesis(CompositionTotal, names(coef(CompositionTotal))[2:14], singular.ok = TRUE)
linearHypothesis(CompositionTotalDummies, names(coef(CompositionTotal))[2:14], singular.ok = TRUE)
save.image("IntermediateFiles/CompositionData.RData")

rm(list=ls())
load("IntermediateFiles/TotalRegressionData.RData")
#first stage
FirstStageResults<-matrix(NA,3,2)
FirstStageAverageControls<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
FirstStageAverageDummies<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
FirstStageResults[1:2, 1]<-coeftest(FirstStageAverageControls)[2,1:2]
FirstStageResults[1:2, 2]<-coeftest(FirstStageAverageDummies)[2,1:2]
#compute F-statistics
FControls<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
FDummies<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)

FirstStageResults[3, 1]<-summary(FControls, diagnostics=TRUE)$diagnostics[1,3]
FirstStageResults[3, 2]<-summary(FDummies, diagnostics=TRUE)$diagnostics[1,3]
write.xlsx(FirstStageResults, "FinalOutput/Tab3.xlsx")

save.image("IntermediateFiles/FirstStage.RData")
load("IntermediateFiles/FirstStage.RData")
rm(FirstStageAverageControls,FirstStageAverage1monthControls,FirstStageAverageDummies,FirstStageAverage1monthDummies,SecondStageAverageControls,SecondStageAverage1monthControls, SecondStageAveragDummies,SecondStageAverage1monthDummies)
rm(CompositionDiag1, CompositionDiag2, CompositionDiag3, CompositionDiag4, CompositionEdu1, CompositionEdu2, CompositionEdu3, CompositionGeneratie, CompositionLeeftijd, CompositionMan, CompositionStartggz)
rm(FirstStageAverageControls1month, FirstStageAverageControlsIn, FirstStageAverageControlsUit, FirstStageAverageIndControls, FirstStageAverageIndControls1month, FirstStageAverageControlsCombined, FirstStageAverageNoControls, FirstStageAverage1Month)
#delete unused data
rm(hist1, hist2, hist3, HistogramOutput, intake, NoIncomeGGZ, op, RegionalWaitingOutput, SocVerOvGGZ,Terminations, total,RealWaiting)
rm(FNoControls, FIndControls, FControls, FControls1month, FDummies, FControlsUit, FControlsCombined, F1month )
Wachttijd%<>%
  dplyr::select(-Outcome)

#Check sample sizes at yearly intervals
TabB5<-matrix(NA,1,8)
for(i in 1:8){
  TabB5[1,i]<-(length(which(!is.na(WerknemerGGZ[,((24*i))]))))
}
write.xlsx(TabB5, "FinalOutput/TabB5.xlsx")

#Only use employment for all robustness
rm(MedicijnenGGZ, MentalCostGGZ, PhysicalCostGGZ, TreatmentMinutesGGZ)
#Estimates
EmploymentEst<-matrix(NA, 12,168)
EmploymentSE<-matrix(NA, 12,168)

#Delete old pre-controls
Wachttijd<-Wachttijd[, 1:50]
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 25])
names(Wachttijd)[51]<-"Outcome"
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 19], WWGGZ[, 19], BijstandGGZ[, 19], ZiekteAOGGZ[, 19], EarningsGGZ[, 19], UrenGGZ[, 19])
names(Wachttijd)[52:57]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")
for(i in 1:168){
  cat(paste(i, " "));flush.console()
  gc()
  Wachttijd$Outcome<-WerknemerGGZ[, (i+24)]
  if(i<72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  #Employment
  OLSTotal<-biglm(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  NoControlsTotal<-ivreg(Outcome~Behandeltijd|WachttijdRegioInd, data=Wachttijd)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  ControlsTotal1month<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd1month + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  ControlsTotalIntake<-ivreg(Outcome~Aanmeldtijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  gc()
  DummiesTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
  DummiesTotal1month<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd1month + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
  ControlsTotalNoZero<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$Aanmeldtijd!=0,])
  if(i<132){
    ControlsTotalBalanced<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[(as.numeric(as.character(Wachttijd$IndexStartGGZ))<=48)&(Wachttijd$LeeftijdLin>24)&(Wachttijd$LeeftijdLin<60),])
    EmploymentEst[12,i]<-ControlsTotalBalanced$coefficients[2]
    EmploymentSE[12,i]<-summary(ControlsTotalBalanced)$coefficients[2,2]
    rm(ControlsTotalBalanced)
  }
  
  
  EmploymentEst[1,i]<-ControlsTotal$coefficients[2]
  EmploymentSE[1,i]<-summary(ControlsTotal)$coefficients[2,2]
  EmploymentEst[2,i]<-summary(OLSTotal)$mat[2,2]
  EmploymentSE[2,i]<-summary(OLSTotal)$mat[2,4]
  EmploymentEst[3,i]<-NoControlsTotal$coefficients[2]
  EmploymentSE[3,i]<-summary(NoControlsTotal)$coefficients[2,2]
  EmploymentEst[7,i]<-ControlsTotalIntake$coefficients[2]
  EmploymentSE[7,i]<-summary(ControlsTotalIntake)$coefficients[2,2]
  EmploymentEst[8,i]<-DummiesTotal$coefficients[2]
  EmploymentSE[8,i]<-summary(DummiesTotal)$coefficients[2,2]
  EmploymentEst[10,i]<-ControlsTotalNoZero$coefficients[2]
  EmploymentSE[10,i]<-summary(ControlsTotalNoZero)$coefficients[2,2]
  EmploymentEst[11,i]<-DummiesTotal1month$coefficients[2]
  EmploymentSE[11,i]<-summary(DummiesTotal1month)$coefficients[2,2]
  
  gc()
}

WWEst<-matrix(NA, 1,168)
WWSE<-matrix(NA, 1,168)
BijstandEst<-matrix(NA, 1,168)
BijstandSE<-matrix(NA, 1,168)
ZiekteAOEst<-matrix(NA, 1,168)
ZiekteAOSE<-matrix(NA, 1,168)
EarningsEst<-matrix(NA, 1,168)
EarningsSE<-matrix(NA, 1,168)
UrenEst<-matrix(NA, 1,168)
UrenSE<-matrix(NA, 1,168)

for(i in 1:168){
  cat(paste(i, " "));flush.console()
  gc()
  if(i<72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  #Receipt of benefits
  #WW
  Wachttijd$Outcome<-WWGGZ[, (i+24)]
  WWTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  WWEst[1,i]<-WWTotal$coefficients[2]
  WWSE[1,i]<-summary(WWTotal)$coefficients[2,2]
  rm(WWTotal)
  
  #Bijstand
  Wachttijd$Outcome<-BijstandGGZ[, (i+24)]
  BijstandTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  BijstandEst[1,i]<-BijstandTotal$coefficients[2]
  BijstandSE[1,i]<-summary(BijstandTotal)$coefficients[2,2]
  rm(BijstandTotal)
  
  #ZiekteAO
  Wachttijd$Outcome<-ZiekteAOGGZ[, (i+24)]
  ZiekteAOTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  ZiekteAOEst[1,i]<-ZiekteAOTotal$coefficients[2]
  ZiekteAOSE[1,i]<-summary(ZiekteAOTotal)$coefficients[2,2]
  rm(ZiekteAOTotal)
  
  #Earnings
  Wachttijd$Outcome<-EarningsGGZ[, (i+24)]
  EarningsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  EarningsEst[1,i]<-EarningsTotal$coefficients[2]
  EarningsSE[1,i]<-summary(EarningsTotal)$coefficients[2,2]
  rm(EarningsTotal)
  
  #Uren
  Wachttijd$Outcome<-UrenGGZ[, (i+24)]
  UrenTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  UrenEst[1,i]<-UrenTotal$coefficients[2]
  UrenSE[1,i]<-summary(UrenTotal)$coefficients[2,2]
  rm(UrenTotal)
}

save.image("IntermediateFiles/ResultsEmploymentOutcomes.RData")

#Health outcomes
rm(list=ls())
load("IntermediateFiles/FirstStage.RData")
rm(FirstStageAverageControls,FirstStageAverage1monthControls,FirstStageAverageDummies,FirstStageAverage1monthDummies,SecondStageAverageControls,SecondStageAverage1monthControls, SecondStageAveragDummies,SecondStageAverage1monthDummies)
rm(CompositionDiag1, CompositionDiag2, CompositionDiag3, CompositionDiag4, CompositionEdu1, CompositionEdu2, CompositionEdu3, CompositionGeneratie, CompositionLeeftijd, CompositionMan, CompositionStartggz)
rm(FirstStageAverageControls1month, FirstStageAverageControlsIn, FirstStageAverageControlsUit, FirstStageAverageIndControls, FirstStageAverageIndControls1month, FirstStageAverageControlsCombined, FirstStageAverageNoControls)
#delete unused data
rm(hist1, hist2, hist3, HistogramOutput, intake, NoIncomeGGZ, op, RegionalWaitingOutput, SocVerOvGGZ,Terminations, total,RealWaiting)
rm(FNoControls, FIndControls, FControls, FControls1month, FDummies, FControlsUit, FControlsCombined )
Wachttijd%<>%
  dplyr::select(-Outcome)

TreatmentEst<-matrix(NA, 2,96)
TreatmentSE<-matrix(NA, 2,96)
MentalCostEst<-matrix(NA, 2,14)
MentalCostSE<-matrix(NA, 2,14)
PhysicalCostEst<-matrix(NA, 2,14)
PhysicalCostSE<-matrix(NA, 2,14)
MedicijnCostEst<-matrix(NA, 2,14)
MedicijnCostSE<-matrix(NA, 2,14)

#Treatment minutes
Wachttijd<-cbind(Wachttijd, TreatmentMinutesGGZ[, 1])
names(Wachttijd)[51]<-"Outcome"
Wachttijd[,52:57]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
names(Wachttijd)[52:57]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")
for(i in 1:96){
  cat(paste(i, " "));flush.console()
  gc()
  Wachttijd$Outcome<-TreatmentMinutesGGZ[, i]
  
  TreatmentTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  TreatmentEst[1,i]<-TreatmentTotal$coefficients[2]
  TreatmentSE[1,i]<-summary(TreatmentTotal)$coefficients[2,2]
  TreatmentTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
  TreatmentEst[2,i]<-TreatmentTotal$coefficients[2]
  TreatmentSE[2,i]<-summary(TreatmentTotal)$coefficients[2,2]
  rm(TreatmentTotal)
}

#costs
for(i in c(12, 24, 36,48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168)){
  cat(paste(i, " "));flush.console()
  gc()
  if(i<72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  #Medicijn
  Wachttijd$Outcome<-MedicijnenGGZ[, ((i%/%12)+5)]
  MedicijnTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  MedicijnCostEst[1,(i%/%12)]<-MedicijnTotal$coefficients[2]
  MedicijnCostSE[1,(i%/%12)]<-summary(MedicijnTotal)$coefficients[2,2]
  MedicijnTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
  MedicijnCostEst[2,(i%/%12)]<-MedicijnTotal$coefficients[2]
  MedicijnCostSE[2,(i%/%12)]<-summary(MedicijnTotal)$coefficients[2,2]
  rm(MedicijnTotal)
  
  #physical
  Wachttijd$Outcome<-PhysicalCostGGZ[,  ((i%/%12)+5)]
  PhysicalTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PhysicalCostEst[1,(i%/%12)]<-PhysicalTotal$coefficients[2]
  PhysicalCostSE[1,(i%/%12)]<-summary(PhysicalTotal)$coefficients[2,2]
  PhysicalTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
  PhysicalCostEst[2,(i%/%12)]<-PhysicalTotal$coefficients[2]
  PhysicalCostSE[2,(i%/%12)]<-summary(PhysicalTotal)$coefficients[2,2]
  rm(PhysicalTotal)
  
  #mental
  if(i>72){
    Wachttijd$Outcome<-MentalCostGGZ[,  ((i%/%12)+5)]
    MentalTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
    MentalCostEst[1,(i%/%12)]<-MentalTotal$coefficients[2]
    MentalCostSE[1,(i%/%12)]<-summary(MentalTotal)$coefficients[2,2]
    MentalTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + Gemeente, data=Wachttijd)
    MentalCostEst[2,(i%/%12)]<-MentalTotal$coefficients[2]
    MentalCostSE[2,(i%/%12)]<-summary(MentalTotal)$coefficients[2,2]
    rm(MentalTotal)
  }
}

#Impact on sum of treatment minutes and sum of expenditures
SumTreatment<-rowSums(TreatmentMinutesGGZ,na.rm=TRUE)
SumMental<-rowSums(MentalCostGGZ[,12:20],na.rm=TRUE)
SumPhysical<-rowSums(PhysicalCostGGZ[,12:20],na.rm=TRUE)
SumMedicijn<-rowSums(MedicijnenGGZ[,12:20],na.rm=TRUE)
#Delete outliers
SumTreatment[SumTreatment>40000]<-NA
SumMental[SumMental>50000]<-NA
SumPhysical[SumPhysical>50000]<-NA
SumMedicijn[SumMedicijn>50000]<-NA
#mean values
mean(SumTreatment, na.rm=TRUE)
mean(SumMental, na.rm=TRUE)
mean(SumPhysical, na.rm=TRUE)
mean(SumMedicijn, na.rm=TRUE)

Wachttijd<-cbind(Wachttijd, SumTreatment,SumMental,SumPhysical, SumMedicijn)
TotalTreatmentEffect<-ivreg(SumTreatment~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
TotalMentalEffect<-ivreg(SumMental~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
TotalPhysicalEffect<-ivreg(SumPhysical~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
TotalMedicijnEffect<-ivreg(SumMedicijn~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)

Output<-cbind(TreatmentEst[2,], TreatmentSE[2,], c(MentalCostEst[2,], rep(NA,82)), c(MentalCostSE[2,], rep(NA,82)), c(PhysicalCostEst[2,], rep(NA,82)), c(PhysicalCostSE[2,], rep(NA,82)))
write.xlsx(Output, "FinalOutput/FigA2OutputHealth.xlsx")

save.image("IntermediateFiles/ResultsHealthOutcomesAlt.RData")

#Add pharmaceutical utilisation
load("IntermediateFiles/FirstStage.RData")
rm(FirstStageAverageControls,FirstStageAverage1monthControls,FirstStageAverageDummies,FirstStageAverage1monthDummies,SecondStageAverageControls,SecondStageAverage1monthControls, SecondStageAveragDummies,SecondStageAverage1monthDummies)
rm(CompositionDiag1, CompositionDiag2, CompositionDiag3, CompositionDiag4, CompositionEdu1, CompositionEdu2, CompositionEdu3, CompositionGeneratie, CompositionLeeftijd, CompositionMan, CompositionStartggz)
rm(FirstStageAverageControls1month, FirstStageAverageControlsIn, FirstStageAverageControlsUit, FirstStageAverageIndControls, FirstStageAverageIndControls1month, FirstStageAverageControlsCombined, FirstStageAverageNoControls, FirstStageAverage1Month)
#delete unused data
rm(hist1, hist2, hist3, HistogramOutput, intake, NoIncomeGGZ, op, RegionalWaitingOutput, SocVerOvGGZ,Terminations, total,RealWaiting)
rm(FNoControls, FIndControls, FControls, FControls1month, FDummies, FControlsUit, FControlsCombined, F1month )
Wachttijd%<>%
  dplyr::select(-Outcome)

#Load pharmaceutical utilization and make time series
#Load data
Addictive<-read.csv("L:/9821_Lancaster_N07B_2006-2022.csv", sep=";")
Antidepressants<-read.csv("L:/9821_Lancaster_N06A_2006-2022.csv", sep=";")
ADHD<-read.csv("L:/9821_Lancaster_N06B_2006-2022.csv", sep=";")
Antipsychotics<-read.csv("L:/9821_Lancaster_N05A_2006-2022.csv", sep=";")
Antixiolytics<-read.csv("L:/9821_Lancaster_N05B_2006-2022.csv", sep=";")
Hypnotics<-read.csv("L:/9821_Lancaster_N05C_2006-2022.csv", sep=";")
Opioids<-read.csv("L:/9821_Lancaster_N02A_2006-2022.csv", sep=";")

#Select variables
Addictive%<>%
  dplyr::select(RINPERSOON, Datumaflevering)
Antidepressants%<>%
  dplyr::select(RINPERSOON, Datumaflevering)
ADHD%<>%
  dplyr::select(RINPERSOON, Datumaflevering)
Antipsychotics%<>%
  dplyr::select(RINPERSOON, Datumaflevering)
Antixiolytics%<>%
  dplyr::select(RINPERSOON, Datumaflevering)
Hypnotics%<>%
  dplyr::select(RINPERSOON, Datumaflevering)
Opioids%<>%
  dplyr::select(RINPERSOON, Datumaflevering)

#Create sample Addictive
RINPERSOON<-unique(Addictive$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
Addictive<-Addictive[order(Addictive$RINPERSOON),]
AddictiveTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(Addictive$Datumaflevering,1,4))-2006)*12+as.numeric(substr(Addictive$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==Addictive$RINPERSOON[j]&j<nrow(Addictive)){
    AddictiveTime[i,indexAanvang$indexAanvang[j]]<-AddictiveTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

AddictiveTime<-cbind(RINPERSOON, AddictiveTime)

#Create sample ADHD
RINPERSOON<-unique(ADHD$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
ADHD<-ADHD[order(ADHD$RINPERSOON),]
ADHDTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(ADHD$Datumaflevering,1,4))-2006)*12+as.numeric(substr(ADHD$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==ADHD$RINPERSOON[j]&j<nrow(ADHD)){
    ADHDTime[i,indexAanvang$indexAanvang[j]]<-ADHDTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

ADHDTime<-cbind(RINPERSOON, ADHDTime)
#Create sample Antidepressants
RINPERSOON<-unique(Antidepressants$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
Antidepressants<-Antidepressants[order(Antidepressants$RINPERSOON),]
AntidepressantsTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(Antidepressants$Datumaflevering,1,4))-2006)*12+as.numeric(substr(Antidepressants$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==Antidepressants$RINPERSOON[j]&j<nrow(Antidepressants)){
    AntidepressantsTime[i,indexAanvang$indexAanvang[j]]<-AntidepressantsTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

AntidepressantsTime<-cbind(RINPERSOON, AntidepressantsTime)
#Create sample Antipsychotics
RINPERSOON<-unique(Antipsychotics$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
Antipsychotics<-Antipsychotics[order(Antipsychotics$RINPERSOON),]
AntipsychoticsTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(Antipsychotics$Datumaflevering,1,4))-2006)*12+as.numeric(substr(Antipsychotics$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==Antipsychotics$RINPERSOON[j]&j<nrow(Antipsychotics)){
    AntipsychoticsTime[i,indexAanvang$indexAanvang[j]]<-AntipsychoticsTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

AntipsychoticsTime<-cbind(RINPERSOON, AntipsychoticsTime)
#Create sample Antixiolytics
RINPERSOON<-unique(Antixiolytics$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
Antixiolytics<-Antixiolytics[order(Antixiolytics$RINPERSOON),]
AntixiolyticsTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(Antixiolytics$Datumaflevering,1,4))-2006)*12+as.numeric(substr(Antixiolytics$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==Antixiolytics$RINPERSOON[j]&j<nrow(Antixiolytics)){
    AntixiolyticsTime[i,indexAanvang$indexAanvang[j]]<-AntixiolyticsTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

AntixiolyticsTime<-cbind(RINPERSOON, AntixiolyticsTime)
#Create sample Hypnotics
RINPERSOON<-unique(Hypnotics$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
Hypnotics<-Hypnotics[order(Hypnotics$RINPERSOON),]
HypnoticsTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(Hypnotics$Datumaflevering,1,4))-2006)*12+as.numeric(substr(Hypnotics$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==Hypnotics$RINPERSOON[j]&j<nrow(Hypnotics)){
    HypnoticsTime[i,indexAanvang$indexAanvang[j]]<-HypnoticsTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

HypnoticsTime<-cbind(RINPERSOON, HypnoticsTime)

#Create sample Opioids
RINPERSOON<-unique(Opioids$RINPERSOON)
RINPERSOON<-RINPERSOON[order(RINPERSOON)]
Opioids<-Opioids[order(Opioids$RINPERSOON),]
OpioidsTime<-matrix(0, length(RINPERSOON),204 )
indexAanvang<-as.data.frame((as.numeric(substr(Opioids$Datumaflevering,1,4))-2006)*12+as.numeric(substr(Opioids$Datumaflevering,5,6)))
names(indexAanvang)<-"indexAanvang"

#Make time series: 2006-2022
time<-Sys.time()
j<-1
for(i in 1:length(RINPERSOON)){
  if(i %% 6000==0){
    cat(paste(i, " "));flush.console()
  }
  while(RINPERSOON[i]==Opioids$RINPERSOON[j]&j<nrow(Opioids)){
    OpioidsTime[i,indexAanvang$indexAanvang[j]]<-OpioidsTime[i,indexAanvang$indexAanvang[j]]+1
    j<-j+1
  }
}

OpioidsTime<-cbind(RINPERSOON, OpioidsTime)

rm(Addictive, ADHD, Antidepressants, Antipsychotics, Antixiolytics, Hypnotics, Opioids, RINPERSOON, indexAanvang)

#Match pharmaceuticals
RINPERSOONSample<-as.data.frame(as.numeric(Wachttijd$RINPERSOON))
names(RINPERSOONSample)<-"RINPERSOON"
AddictiveTime<-merge(RINPERSOONSample, AddictiveTime, by="RINPERSOON", all.x=TRUE)
AddictiveTime<-as.matrix(AddictiveTime)
AddictiveTime[is.na(AddictiveTime)]<-0

ADHDTime<-merge(RINPERSOONSample, ADHDTime, by="RINPERSOON", all.x=TRUE)
ADHDTime<-as.matrix(ADHDTime)
ADHDTime[is.na(ADHDTime)]<-0

AntidepressantsTime<-merge(RINPERSOONSample, AntidepressantsTime, by="RINPERSOON", all.x=TRUE)
AntidepressantsTime<-as.matrix(AntidepressantsTime)
AntidepressantsTime[is.na(AntidepressantsTime)]<-0

AntipsychoticsTime<-merge(RINPERSOONSample, AntipsychoticsTime, by="RINPERSOON", all.x=TRUE)
AntipsychoticsTime<-as.matrix(AntipsychoticsTime)
AntipsychoticsTime[is.na(AntipsychoticsTime)]<-0

AntixiolyticsTime<-merge(RINPERSOONSample, AntixiolyticsTime, by="RINPERSOON", all.x=TRUE)
AntixiolyticsTime<-as.matrix(AntixiolyticsTime)
AntixiolyticsTime[is.na(AntixiolyticsTime)]<-0

HypnoticsTime<-merge(RINPERSOONSample, HypnoticsTime, by="RINPERSOON", all.x=TRUE)
HypnoticsTime<-as.matrix(HypnoticsTime)
HypnoticsTime[is.na(HypnoticsTime)]<-0

OpioidsTime<-merge(RINPERSOONSample, OpioidsTime, by="RINPERSOON", all.x=TRUE)
OpioidsTime<-as.matrix(OpioidsTime)
OpioidsTime[is.na(OpioidsTime)]<-0

#Relative to start GGZ: 168 pre, 132 post
Wachttijd$IndexStartGGZ<-as.numeric(as.character(Wachttijd$IndexStartGGZ))
AddictiveGGZ<-matrix(NA,nrow(Wachttijd), 300)
ADHDGGZ<-matrix(NA,nrow(Wachttijd), 300)
AntidepressantsGGZ<-matrix(NA,nrow(Wachttijd), 300)
AntipsychoticsGGZ<-matrix(NA,nrow(Wachttijd), 300)
AntixiolyticsGGZ<-matrix(NA,nrow(Wachttijd), 300)
HypnoticsGGZ<-matrix(NA,nrow(Wachttijd), 300)
OpioidsGGZ<-matrix(NA,nrow(Wachttijd), 300)

#169 is maand van aanmelding
for(i in 1:nrow(Wachttijd)){
  if(!is.na(Wachttijd$IndexStartGGZ[i])){
    if(Wachttijd$IndexStartGGZ[i]>0&Wachttijd$IndexStartGGZ[i]<97){
      AddictiveGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-AddictiveTime[i,2:205]
      ADHDGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-ADHDTime[i,2:205]
      AntidepressantsGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-AntidepressantsTime[i,2:205]
      AntipsychoticsGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-AntipsychoticsTime[i,2:205]
      AntixiolyticsGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-AntixiolyticsTime[i,2:205]
      HypnoticsGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-HypnoticsTime[i,2:205]
      OpioidsGGZ[i,(97-(Wachttijd$IndexStartGGZ[i])):(300-(Wachttijd$IndexStartGGZ[i]))]<-OpioidsTime[i,2:205]      
      if(i%%1000==0){
        gc()
      }
    }
  }
}
rm(AddictiveTime, ADHDTime, AntidepressantsTime, AntipsychoticsTime, AntixiolyticsTime, HypnoticsTime, OpioidsTime)

#Select 72 pre-months+24 pre-pre untill 96 post months
AddictiveGGZ<-AddictiveGGZ[, 73:288]
ADHDGGZ<-ADHDGGZ[, 73:288]
AntidepressantsGGZ<-AntidepressantsGGZ[, 73:288]
AntipsychoticsGGZ<-AntipsychoticsGGZ[, 73:288]
AntixiolyticsGGZ<-AntixiolyticsGGZ[, 73:288]
HypnoticsGGZ<-HypnoticsGGZ[, 73:288]
OpioidsGGZ<-OpioidsGGZ[, 73:288]
AnyGGZ<-AddictiveGGZ+ADHDGGZ+AntidepressantsGGZ+AntipsychoticsGGZ+AntixiolyticsGGZ+HypnoticsGGZ+OpioidsGGZ

#make plots of pharma utilisation
setEPS()
postscript("FinalOutput/FigB4.eps", width=8)
plot(colMeans(AnyGGZ>0, na.rm=TRUE)[25:192]*100, ylim=c(0, 25), xlab="Months relative to first moment of contact", ylab="Percentage with prescription", lwd=2)
dev.off()

rm(MedicijnenGGZ, MentalCostGGZ, PhysicalCostGGZ, TreatmentMinutesGGZ)
#Estimates
PharmaceuticalEst<-matrix(NA, 10,168)
PharmaceuticalSE<-matrix(NA, 10,168)

#Delete old pre-controls
Wachttijd$IndexStartGGZ<-as.factor(Wachttijd$IndexStartGGZ)
Wachttijd<-Wachttijd[, 1:50]
Wachttijd<-cbind(Wachttijd, AddictiveGGZ[, 25])
names(Wachttijd)[51]<-"Outcome"
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 19], WWGGZ[, 19], BijstandGGZ[, 19], ZiekteAOGGZ[, 19], EarningsGGZ[, 19], UrenGGZ[, 19], AnyGGZ[,19])
names(Wachttijd)[52:58]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren", "PrePharma")
for(i in 84:168){
  cat(paste(i, " "));flush.console()
  gc()
  if(i<72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  Wachttijd$PrePharma<-AddictiveGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-AddictiveGGZ[, 90]>0
  }
  Wachttijd$Outcome<-AddictiveGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[1,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[1,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-ADHDGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-ADHDGGZ[, 90]>0
  }
  Wachttijd$Outcome<-ADHDGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[2,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[2,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-AntidepressantsGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-AntidepressantsGGZ[, 90]>0
  }
  Wachttijd$Outcome<-AntidepressantsGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[3,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[3,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-AntipsychoticsGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-AntipsychoticsGGZ[, 90]>0
  }
  Wachttijd$Outcome<-AntipsychoticsGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[4,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[4,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-AntixiolyticsGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-AntixiolyticsGGZ[, 90]>0
  }
  Wachttijd$Outcome<-AntixiolyticsGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[5,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[5,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-HypnoticsGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-HypnoticsGGZ[, 90]>0
  }
  Wachttijd$Outcome<-HypnoticsGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[6,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[6,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-OpioidsGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-OpioidsGGZ[, 90]>0
  }
  Wachttijd$Outcome<-OpioidsGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[7,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[7,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  Wachttijd$PrePharma<-AnyGGZ[, 18+i]>0
  if(i>=72){
    Wachttijd$PrePharma<-AnyGGZ[, 90]>0
  }
  Wachttijd$Outcome<-AnyGGZ[, 24+i]>0
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd)
  PharmaceuticalEst[8,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[8,i]<-summary(ControlsTotal)$coefficients[2,2]
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + PrePharma + Gemeente|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren+ PrePharma + Gemeente, data=Wachttijd)
  PharmaceuticalEst[9,i]<-ControlsTotal$coefficients[2]
  PharmaceuticalSE[9,i]<-summary(ControlsTotal)$coefficients[2,2]
  
  
  gc()
}

save.image("IntermediateFiles/PharmaceuticalResults.RData")
write.xlsx(cbind(t(PharmaceuticalEst), t(PharmaceuticalSE)), "FinalOutput/FigA2bResultsPharma.xlsx")

#Heterogeniteit
load("IntermediateFiles/FirstStage.RData")
rm(FirstStageAverageControls,FirstStageAverage1monthControls,FirstStageAverageDummies,FirstStageAverage1monthDummies,SecondStageAverageControls,SecondStageAverage1monthControls, SecondStageAveragDummies,SecondStageAverage1monthDummies)
rm(CompositionDiag1, CompositionDiag2, CompositionDiag3, CompositionDiag4, CompositionEdu1, CompositionEdu2, CompositionEdu3, CompositionGeneratie, CompositionLeeftijd, CompositionMan, CompositionStartggz)
rm(FirstStageAverageControls1month, FirstStageAverageControlsIn, FirstStageAverageControlsUit, FirstStageAverageIndControls, FirstStageAverageIndControls1month, FirstStageAverageControlsCombined, FirstStageAverageNoControls)
#delete unused data
rm(hist1, hist2, hist3, HistogramOutput, intake, NoIncomeGGZ, op, RegionalWaitingOutput, SocVerOvGGZ,Terminations, total,RealWaiting)
rm(FNoControls, FIndControls, FControls, FControls1month, FDummies, FControlsUit, FControlsCombined )
Wachttijd%<>%
  dplyr::select(-Outcome)

#Only use employment for all heterogeneity
rm(MedicijnenGGZ, MentalCostGGZ, PhysicalCostGGZ, TreatmentMinutesGGZ)
#Estimates
HeterogeneityEst<-matrix(NA, 27,168)
HeterogeneitySE<-matrix(NA, 27,168)

Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 25])
names(Wachttijd)[51]<-"Outcome"
Wachttijd<-cbind(Wachttijd, WerknemerGGZ[, 19], WWGGZ[, 19], BijstandGGZ[, 19], ZiekteAOGGZ[, 19], EarningsGGZ[, 19], UrenGGZ[, 19])
names(Wachttijd)[52:57]<-c("PreWerknemer", "PreWW", "PreBijstand", "PreZiekteAO", "PreEarnings", "PreUren")
for(i in 1:168){
  cat(paste(i, " "));flush.console()
  gc()
  Wachttijd$Outcome<-WerknemerGGZ[, (i+24)]
  if(i<72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[, 18+i], WWGGZ[, 18+i], BijstandGGZ[, 18+i], ZiekteAOGGZ[, 18+i], EarningsGGZ[, 18+i], UrenGGZ[, 18+i])
  }
  if(i>=72){
    Wachttijd[,52:57]<-cbind(WerknemerGGZ[,90], WWGGZ[,90], BijstandGGZ[,90], ZiekteAOGGZ[,90], EarningsGGZ[,90], UrenGGZ[,90])
  }
  
  #Gender (female, male)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$Man==0,])
  HeterogeneityEst[1,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[1,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$Man==1,])
  HeterogeneityEst[2,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[2,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  #age (<35,  35-50, 50+)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$LeeftijdLin<35,])
  HeterogeneityEst[3,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[3,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$LeeftijdLin>=35& Wachttijd$LeeftijdLin<50,])
  HeterogeneityEst[4,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[4,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$LeeftijdLin>=50,])
  HeterogeneityEst[5,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[5,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  
  #Migration background
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$GBAGENERATIE=="0",])
  HeterogeneityEst[6,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[6,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$GBAGENERATIE=="1",])
  HeterogeneityEst[7,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[7,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$GBAGENERATIE=="2",])
  HeterogeneityEst[8,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[8,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  
  #Education level
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))<2000,])
  HeterogeneityEst[9,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[9,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))>=2000&as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))<3000,])
  HeterogeneityEst[10,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[10,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))>3000,])
  HeterogeneityEst[11,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[11,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  
  #Diagnosis
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Anxiety",])
  HeterogeneityEst[12,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[12,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Mood",])
  HeterogeneityEst[13,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[13,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Personality",])
  HeterogeneityEst[14,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[14,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Other",])
  HeterogeneityEst[15,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[15,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  
  #Provider
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[substring(Wachttijd$Behandelaar,1,2)=="PB",])
  HeterogeneityEst[16,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[16,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[substring(Wachttijd$Behandelaar,1,2)=="PT",])
  HeterogeneityEst[17,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[17,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[substring(Wachttijd$Behandelaar,1,2)=="MB",])
  HeterogeneityEst[18,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[18,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  ControlsTotal<-ivreg(Outcome~Behandeltijd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich|WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[!(substring(Wachttijd$Behandelaar,1,2)=="PB")&!(substring(Wachttijd$Behandelaar,1,2)=="PT")&!(substring(Wachttijd$Behandelaar,1,2)=="MB"),])
  HeterogeneityEst[19,i]<-ControlsTotal$coefficients[2]
  HeterogeneitySE[19,i]<-summary(ControlsTotal)$coefficients[2,2]
  rm(ControlsTotal)
  gc()
}

write.xlsx(c(HeterogeneityEst, HeterogeneitySE), "FinalOutput/FigB12.xlsx")

#Heterogeneity first stage
FirstStage<-matrix(NA, 19, 2)

ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$Man==0,])
FirstStage[1,1]<-ControlsTotal$coefficients[2]
FirstStage[1,2]<-summary(ControlsTotal)$coefficients[2,2]
rm(ControlsTotal)
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$Man==1,])
FirstStage[2,1]<-ControlsTotal$coefficients[2]
FirstStage[2,2]<-summary(ControlsTotal)$coefficients[2,2]
rm(ControlsTotal)
#age (<35,  35-50, 50+)
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$LeeftijdLin<35,])
FirstStage[3,1]<-ControlsTotal$coefficients[2]
FirstStage[3,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$LeeftijdLin>=35& Wachttijd$LeeftijdLin<50,])
FirstStage[4,1]<-ControlsTotal$coefficients[2]
FirstStage[4,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$LeeftijdLin>=50,])
FirstStage[5,1]<-ControlsTotal$coefficients[2]
FirstStage[5,2]<-summary(ControlsTotal)$coefficients[2,2]

#Migration background
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$GBAGENERATIE=="0",])
FirstStage[6,1]<-ControlsTotal$coefficients[2]
FirstStage[6,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$GBAGENERATIE=="1",])
FirstStage[7,1]<-ControlsTotal$coefficients[2]
FirstStage[7,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$GBAGENERATIE=="2",])
FirstStage[8,1]<-ControlsTotal$coefficients[2]
FirstStage[8,2]<-summary(ControlsTotal)$coefficients[2,2]

#Education level
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))<2000,])
FirstStage[9,1]<-ControlsTotal$coefficients[2]
FirstStage[9,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))>=2000&as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))<3000,])
FirstStage[10,1]<-ControlsTotal$coefficients[2]
FirstStage[10,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[as.numeric(as.character(Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO))>3000,])
FirstStage[11,1]<-ControlsTotal$coefficients[2]
FirstStage[11,2]<-summary(ControlsTotal)$coefficients[2,2]

#Diagnosis
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Anxiety",])
FirstStage[12,1]<-ControlsTotal$coefficients[2]
FirstStage[12,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Mood",])
FirstStage[13,1]<-ControlsTotal$coefficients[2]
FirstStage[13,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Personality",])
FirstStage[14,1]<-ControlsTotal$coefficients[2]
FirstStage[14,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[Wachttijd$DiagnosisGroup=="Other",])
FirstStage[15,1]<-ControlsTotal$coefficients[2]
FirstStage[15,2]<-summary(ControlsTotal)$coefficients[2,2]

#Provider
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[substring(Wachttijd$Behandelaar,1,2)=="PB",])
FirstStage[16,1]<-ControlsTotal$coefficients[2]
FirstStage[16,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[substring(Wachttijd$Behandelaar,1,2)=="PT",])
FirstStage[17,1]<-ControlsTotal$coefficients[2]
FirstStage[17,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[substring(Wachttijd$Behandelaar,1,2)=="MB",])
FirstStage[18,1]<-ControlsTotal$coefficients[2]
FirstStage[18,2]<-summary(ControlsTotal)$coefficients[2,2]
ControlsTotal<-lm(Behandeltijd~WachttijdRegioInd + Man + Leeftijd + GBAGENERATIE + OPLNIVSOI2021AGG4HBmetNIRWO+IndexStartGGZ+ PreWerknemer + PreWW + PreBijstand + PreZiekteAO + PreEarnings + PreUren + p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich, data=Wachttijd[!(substring(Wachttijd$Behandelaar,1,2)=="PB")&!(substring(Wachttijd$Behandelaar,1,2)=="PT")&!(substring(Wachttijd$Behandelaar,1,2)=="MB"),])
FirstStage[19,1]<-ControlsTotal$coefficients[2]
FirstStage[19,2]<-summary(ControlsTotal)$coefficients[2,2]

write.xlsx(FirstStage, "FinalOutput/TabB6.xlsx")
save.image("IntermediateFiles/HeterogeniteitsResults.RData")

#Event study with continuous treatment
rm(list=ls())
load("IntermediateFiles/ResultsEmploymentOutcomes.RData")

x<-72
y<-96
donut<-0
N<-nrow(Wachttijd)

MonthDummies<-matrix(0, (x+y)*N,1)
for(i in 1:N){
  MonthDummies[((x+y)*(i-1)+1):((x+y)*i),1]<-seq(-x,y-1,1)
}

DiDIndividual<-as.data.frame(factor(MonthDummies))
names(DiDIndividual)<-c("MonthDummies")
rm(MonthDummies)

#Outcome measures and pre values
DiDEmployment<-matrix(NA, N*(x+y),1)
for(i in 1:N){
  DiDEmployment[((x+y)*(i-1)+1):((x+y)*i),1]<-WerknemerGGZ[i,25:192]
}

#Pre values (fix at 90)
WerknemerGGZ[,90:168]<-WerknemerGGZ[,90]
BijstandGGZ[,90:168]<-BijstandGGZ[,90]
EarningsGGZ[,90:168]<-EarningsGGZ[,90]
UrenGGZ[,90:168]<-UrenGGZ[,90]
WWGGZ[,90:168]<-WWGGZ[,90]
ZiekteAOGGZ[,90:168]<-ZiekteAOGGZ[,90]

#Outcome measures and pre values
DiDPreEmployment<-matrix(NA, N*(x+y),1)
DiDPreBijstand<-matrix(NA, N*(x+y),1)
DiDPreEarnings<-matrix(NA, N*(x+y),1)
DiDPreUren<-matrix(NA, N*(x+y),1)
DiDPreWW<-matrix(NA, N*(x+y),1)
DiDPreZiekte<-matrix(NA, N*(x+y),1)
for(i in 1:N){
  DiDPreEmployment[((x+y)*(i-1)+1):((x+y)*i),1]<-WerknemerGGZ[i,1:168]
  DiDPreBijstand[((x+y)*(i-1)+1):((x+y)*i),1]<-BijstandGGZ[i,1:168]
  DiDPreEarnings[((x+y)*(i-1)+1):((x+y)*i),1]<-EarningsGGZ[i,1:168]
  DiDPreUren[((x+y)*(i-1)+1):((x+y)*i),1]<-UrenGGZ[i,1:168]
  DiDPreWW[((x+y)*(i-1)+1):((x+y)*i),1]<-WWGGZ[i,1:168]
  DiDPreZiekte[((x+y)*(i-1)+1):((x+y)*i),1]<-ZiekteAOGGZ[i,1:168]
}
DiDIndividual<-cbind(DiDIndividual, DiDEmployment, DiDPreEmployment, DiDPreBijstand, DiDPreEarnings, DiDPreUren, DiDPreWW, DiDPreZiekte)
rm(DiDEmployment, WerknemerGGZ, DiDPreEmployment, DiDPreBijstand, DiDPreEarnings, DiDPreUren, DiDPreWW, DiDPreZiekte)
rm(BijstandGGZ, EarningsGGZ, UrenGGZ, WWGGZ, ZiekteAOGGZ)

#individual and regional controls
IndividualGender<-matrix(0, (x+y)*N,1)
IndividualLeeftijd<-matrix(0, (x+y)*N,1)
IndividualGeneratie<-matrix(0, (x+y)*N,1)
IndividualOpleiding<-matrix(0, (x+y)*N,1)
IndividualIndexStart<-matrix(0, (x+y)*N,1)
p_w_all<-matrix(0, (x+y)*N,1)
p_marok<-matrix(0, (x+y)*N,1)
p_tur<-matrix(0, (x+y)*N,1)
g_woz<-matrix(0, (x+y)*N,1)
p_koopw<-matrix(0, (x+y)*N,1)
p_wcorpw<-matrix(0, (x+y)*N,1)
p_bjj2k<-matrix(0, (x+y)*N,1)
g_ink_pi<-matrix(0, (x+y)*N,1)
p_ink_li<-matrix(0, (x+y)*N,1)
p_ink_hi<-matrix(0, (x+y)*N,1)
p_hh_osm<-matrix(0, (x+y)*N,1)
a_soz_ww<-matrix(0, (x+y)*N,1)
a_soz_ao<-matrix(0, (x+y)*N,1)
bev_dich<-matrix(0, (x+y)*N,1)
for(i in 1:N){
  IndividualGender[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$Man[i]
  IndividualLeeftijd[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$Leeftijd[i]
  IndividualGeneratie[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$GBAGENERATIE[i]
  IndividualOpleiding[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$OPLNIVSOI2021AGG4HBmetNIRWO[i]
  IndividualIndexStart[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$IndexStartGGZ[i]
  p_w_all[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_w_all[i]
  p_marok[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_marok[i]
  p_tur[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_tur[i]
  g_woz[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$g_woz[i]
  p_koopw[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_koopw[i]
  p_wcorpw[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_wcorpw[i]
  p_bjj2k[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_bjj2k[i]
  g_ink_pi[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$g_ink_pi[i]
  p_ink_li[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_ink_li[i]
  p_ink_hi[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_ink_hi[i]
  p_hh_osm[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$p_hh_osm[i]
  a_soz_ww[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$a_soz_ww[i]
  a_soz_ao[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$a_soz_ao[i]
  bev_dich[((x+y)*(i-1)+1):((x+y)*i),1]<-Wachttijd$bev_dich[i]
}
DiDIndividual<-cbind(DiDIndividual, IndividualGender, IndividualLeeftijd, IndividualGeneratie, IndividualOpleiding, IndividualIndexStart,p_w_all ,  p_marok , p_tur , g_woz , p_koopw , p_wcorpw , p_bjj2k , g_ink_pi , p_ink_li , p_ink_hi , p_hh_osm , a_soz_ww , a_soz_ao , bev_dich)
rm(IndividualGender, IndividualLeeftijd, IndividualGeneratie, IndividualOpleiding, IndividualIndexStart, p_w_all ,  p_marok , p_tur , g_woz , p_koopw , p_wcorpw , p_bjj2k , g_ink_pi , p_ink_li , p_ink_hi , p_hh_osm , a_soz_ww , a_soz_ao , bev_dich)


#Add regional waiting time
DiDRW<-matrix(NA, N*(x+y),1)
for(i in 1:N){
  DiDRW[((x+y)*(i-1)+1):((x+y)*i),1]<-rep(Wachttijd$WachttijdRegioInd[i], (x+y))
}
DiDIndividual<-cbind(DiDIndividual, DiDRW)
rm(DiDRW)

#Factorize
DiDIndividual$MonthDummies<-factor(DiDIndividual$MonthDummies)
DiDIndividual$IndividualGender<-factor(DiDIndividual$IndividualGender)
DiDIndividual$IndividualLeeftijd<-factor(DiDIndividual$IndividualLeeftijd)
DiDIndividual$IndividualGeneratie<-factor(DiDIndividual$IndividualGeneratie)
DiDIndividual$IndividualOpleiding<-factor(DiDIndividual$IndividualOpleiding)
DiDIndividual$IndividualIndexStart<-factor(DiDIndividual$IndividualIndexStart)
DiDIndividual$p_w_all<-factor(DiDIndividual$p_w_all)
DiDIndividual$p_marok<-factor(DiDIndividual$p_marok)
DiDIndividual$p_tur<-factor(DiDIndividual$p_tur)
DiDIndividual$g_woz<-factor(DiDIndividual$g_woz)
DiDIndividual$p_koopw<-factor(DiDIndividual$p_koopw)
DiDIndividual$p_wcorpw<-factor(DiDIndividual$p_wcorpw)
DiDIndividual$p_bjj2k<-factor(DiDIndividual$p_bjj2k)
DiDIndividual$g_ink_pi<-factor(DiDIndividual$g_ink_pi)
DiDIndividual$p_ink_li<-factor(DiDIndividual$p_ink_li)
DiDIndividual$p_ink_hi<-factor(DiDIndividual$p_ink_hi)
DiDIndividual$p_hh_osm<-factor(DiDIndividual$p_hh_osm)
DiDIndividual$a_soz_ww<-factor(DiDIndividual$a_soz_ww)
DiDIndividual$a_soz_ao<-factor(DiDIndividual$a_soz_ao)
DiDIndividual$bev_dich<-factor(DiDIndividual$bev_dich)
rm(DiDRW, Wachttijd, TreatmentMinutesStart, TotTreatment)

#Iterative comparison of month -71 and l, for computational feasability.
EventStudy<-matrix(NA, 167, 4)
for(l in -71:95){
  EventStudyEstimates<-biglm(DiDEmployment~(MonthDummies!="-72")*(DiDRW + IndividualGender + IndividualLeeftijd + IndividualGeneratie + IndividualOpleiding +IndividualIndexStart +  p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich), data=DiDIndividual[DiDIndividual$MonthDummies=="-72"|DiDIndividual$MonthDummies==l,])
  EventStudy[(l+72),c(1,2)]<-summary(EventStudyEstimates)$mat[269,c(1,4)]
  EventStudyEstimates<-biglm(DiDEmployment~(MonthDummies!="-72")*((DiDRW>63) + IndividualGender + IndividualLeeftijd + IndividualGeneratie + IndividualOpleiding +IndividualIndexStart +  p_w_all +  p_marok + p_tur + g_woz + p_koopw + p_wcorpw + p_bjj2k + g_ink_pi + p_ink_li + p_ink_hi + p_hh_osm + a_soz_ww + a_soz_ao + bev_dich), data=DiDIndividual[DiDIndividual$MonthDummies=="-72"|DiDIndividual$MonthDummies==l,])
  EventStudy[(l+72),c(3,4)]<-summary(EventStudyEstimates)$mat[269,c(1,4)]
}

save.image("IntermediateFiles/EventStudyEstimates.RData")

time<-seq(-72,95,1)
EventStudyEstimates<-c(0,30*100*EventStudy[,1])
EventStudyUpperbound<-EventStudyEstimates+1.96*c(0,30*100*EventStudy[,2])
EventStudyLowerbound<-EventStudyEstimates-1.96*c(0,30*100*EventStudy[,2])

png("FinalOutput/FigB10.png", width=700)
plot(time, (EventStudyEstimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-1.5, 1), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col=adjustcolor("blue", alpha.f=0.5), border=NA)
dev.off()

EventStudyEstimates<-c(0,100*EventStudy[,3])
EventStudyUpperbound<-EventStudyEstimates+1.96*c(0,100*EventStudy[,4])
EventStudyLowerbound<-EventStudyEstimates-1.96*c(0,100*EventStudy[,4])

png("FinalOutput/FigB11.png", width=700)
plot(time, (EventStudyEstimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-1.5, 1), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(EventStudyUpperbound, rev(EventStudyLowerbound)), col=adjustcolor("blue", alpha.f=0.5), border=NA)
dev.off()

write.xlsx(EventStudy, "FinalOutput/FigB10B11.xlsx")

#Non-parametric estimates
#Convert to weekly
load("IntermediateFiles/ResultsEmploymentOutcomes.RData")
BehandeltijdWeek<-matrix(NA, nrow(Wachttijd),1)
for(i in 1:110){
  for(j in 1:nrow(Wachttijd)){
    if(Wachttijd$Behandeltijd[j]<=7*i & Wachttijd$Behandeltijd[j]>7*(i-1)){
      BehandeltijdWeek[j]<-i
    }
  }
}
Wachttijd<-cbind(Wachttijd, BehandeltijdWeek)
Month24<-WerknemerGGZ[,120]
Wachttijd<-cbind(Wachttijd, Month24)
OLSEmploymentNonParametric1<-lm(Month24~as.factor(BehandeltijdWeek), data=Wachttijd)
time<-seq(1,51,1)
op<-par(cex=0.85)

png("FinalOutput/FigB7.png", width=700)
plot(OLSEmploymentNonParametric1$coefficients[2:52], type="l", xaxt="n", xlab= "Waiting time in days", ylab="Effect on employment")
Lower<-OLSEmploymentNonParametric1$coefficients[2:52]-2*summary(OLSEmploymentNonParametric1)$coefficients[2:52,2]
Upper<-OLSEmploymentNonParametric1$coefficients[2:52]+2*summary(OLSEmploymentNonParametric1)$coefficients[2:52,2]
polygon(c(time, rev(time)), c(Upper, rev(Lower)), col="grey", border=NA)
axis(1, label=seq(7, 350, 7), at = seq(1, 50, 1))
lines(OLSEmploymentNonParametric1$coefficients[2:52], type="l", lwd=2)
dev.off()

write.xlsx(cbind(OLSEmploymentNonParametric1$coefficients[2:52],summary(OLSEmploymentNonParametric1)$coefficients[2:52,2]), "FinalOutput/FigB7.xlsx")

#save estimates
rm(list=ls())
load("IntermediateFiles/ResultsEmploymentOutcomes.RData")
load("IntermediateFiles/ResultsHealthOutcomesAlt.RData")
load("IntermediateFiles/HeterogeniteitsResults.RData")
load("IntermediateFiles/ResultsEventStudyStap2.RData")
load("IntermediateFiles/PharmaceuticalResults.RData")

#Save output to excell
OutputIVEstimates<-matrix(NA, 46, 168)
OutputIVSE<-matrix(NA, 46,168)
OutputIVEstimates[1:10,]<-30*EmploymentEst[1:10,]*100
OutputIVSE[1:10,]<-30*EmploymentSE[1:10,]*100
OutputIVEstimates[11,]<-30*WWEst[1,]*100
OutputIVSE[11,]<-30*WWSE[1,]*100
OutputIVEstimates[12,]<-30*BijstandEst[1,]*100
OutputIVSE[12,]<-30*BijstandSE[1,]*100
OutputIVEstimates[13,]<-30*ZiekteAOEst[1,]*100
OutputIVSE[13,]<-30*ZiekteAOSE[1,]*100
OutputIVEstimates[14,]<-30*UrenEst[1,]
OutputIVSE[14,]<-30*UrenSE[1,]
OutputIVEstimates[15,]<-30*EarningsEst[1,]
OutputIVSE[15,]<-30*EarningsSE[1,]

#HealthOutcomes
OutputIVEstimates[16,1:94]<-30*TreatmentEst[1,1:94]
OutputIVSE[16,1:94]<-30*TreatmentSE[1,1:94]
OutputIVEstimates[17,1:8]<-30*MentalCostEst[1,7:14]
OutputIVSE[17,1:8]<-30*MentalCostSE[1,7:14]
OutputIVEstimates[18,1:14]<-30*PhysicalCostEst[1,1:14]
OutputIVSE[18,1:14]<-30*PhysicalCostSE[1,1:14]
OutputIVEstimates[19,1:14]<-30*MedicijnCostEst[1,1:14]
OutputIVSE[19,1:14]<-30*MedicijnCostSE[1,1:14]

#Save plots
Estimates<-30*100*EmploymentEst[1,]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[1,]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[1,]
EstimatesFE<-30*100*EmploymentEst[8,]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[8,]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[8,]

time<-seq(-72, 95,1)
png("FinalOutput/Fig8a.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-5, 3), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Earnings
Estimates<-30*EarningsEst[1,]
Upperbound<-Estimates+1.96*30*EarningsSE[1,]
Lowerbound<-Estimates-1.96*30*EarningsSE[1,]

png("FinalOutput/Fig8b.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on monthly labor earnings", ylim=c(-200, 50), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
lines(time, Estimates, lty=1, lwd=4, col="black")
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#UI
Estimates<-30*100*WWEst[1,]
Upperbound<-Estimates+1.96*30*100*WWSE[1,]
Lowerbound<-Estimates-1.96*30*100*WWSE[1,]

png("FinalOutput/Fig8c.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on percentage on UI benefits", ylim=c(-2, 2), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
lines(time, Estimates, lty=1, lwd=4, col="black")
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#DI
Estimates<-30*100*ZiekteAOEst[1,]
Upperbound<-Estimates+1.96*30*100*ZiekteAOSE[1,]
Lowerbound<-Estimates-1.96*30*100*ZiekteAOSE[1,]

png("FinalOutput/Fig8d.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on percentage on DI benefits", ylim=c(-2, 3), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
lines(time, Estimates, lty=1, lwd=4, col="black")
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#social assistance
Estimates<-30*100*BijstandEst[1,]
Upperbound<-Estimates+1.96*30*100*BijstandSE[1,]
Lowerbound<-Estimates-1.96*30*100*BijstandSE[1,]

png("FinalOutput/Fig8e.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on percentage on social assistance", ylim=c(-2, 3), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
lines(time, Estimates, lty=1, lwd=4, col="black")
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Robustness
#OLS
Estimates<-30*100*EmploymentEst[1,]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[1,]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[1,]
EstimatesFE<-30*100*EmploymentEst[2,]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[2,]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[2,]

png("FinalOutput/FigA1a.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-5, 3), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("IV","OLS"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Balanced
time<-seq(-72, 47,1)
Estimates<-30*100*EmploymentEst[1,1:120]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[1,1:120]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[1,1:120]
EstimatesFE<-30*100*EmploymentEst[12,1:120]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[12,1:120]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[12,1:120]

png("FinalOutput/FigA1b.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-5, 3), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Full sample","Balanced panel"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Time till intake
time<-seq(-72, 95,1)
Estimates<-30*100*EmploymentEst[1,]
Upperbound<-Estimates+1.96*30*100*EmploymentSE[1,]
Lowerbound<-Estimates-1.96*30*100*EmploymentSE[1,]
EstimatesFE<-30*100*EmploymentEst[7,]
UpperboundFE<-EstimatesFE+1.96*30*100*EmploymentSE[7,]
LowerboundFE<-EstimatesFE-1.96*30*100*EmploymentSE[7,]

png("FinalOutput/FigA1d.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on probability to be employed", ylim=c(-8, 3), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Total waiting time","Time till intake"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Treatment minutes
Estimates<-30*TreatmentEst[1,1:94]
Upperbound<-Estimates+1.96*30*TreatmentSE[1,1:94]
Lowerbound<-Estimates-1.96*30*TreatmentSE[1,1:94]
EstimatesFE<-30*100*TreatmentEst[2,1:94]
UpperboundFE<-EstimatesFE+1.96*30*TreatmentSE[2,1:94]
LowerboundFE<-EstimatesFE-1.96*30*TreatmentSE[2,1:94]

time<-seq(1, 94,1)
png("FinalOutput/FigA2a.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on monthly treatment minutes", ylim=c(-25, 25), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Pharmaceutical utilization
time<-seq(-72, 95,1)
Estimates<-30*100*PharmaceuticalEst[1,]
Upperbound<-Estimates+1.96*30*100*PharmaceuticalSE[1,]
Lowerbound<-Estimates-1.96*30*100*PharmaceuticalSE[1,]
EstimatesFE<-30*100*PharmaceuticalEst[2,]
UpperboundFE<-EstimatesFE+1.96*30*100*PharmaceuticalSE[2,]
LowerboundFE<-EstimatesFE-1.96*30*100*PharmaceuticalSE[2,]

png("FinalOutput/FigA2b.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on pharmaceutical utilization", ylim=c(-3, 5), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=c(-72,-48, -24,0, 24, 48, 72, 96))
dev.off()

#Mental expenditure
time<-seq(1, 8,1)
Estimates<-30*MentalCostEst[1,7:14]
Upperbound<-Estimates+1.96*30*MentalCostSE[1,7:14]
Lowerbound<-Estimates-1.96*30*MentalCostSE[1,7:14]
EstimatesFE<-30*MentalCostEst[2,7:14]
UpperboundFE<-EstimatesFE+1.96*30*MentalCostSE[2,7:14]
LowerboundFE<-EstimatesFE-1.96*30*MentalCostSE[2,7:14]

png("FinalOutput/FigA2c.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on mental healthcare expenditure", ylim=c(-100, 1000), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-72","-48", "-24", "0", "24", "48", "72", "96"), at=seq(1,8))
dev.off()

#non-Mental expenditure
time<-seq(-5, 8,1)
Estimates<-30*PhysicalCostEst[1,]
Upperbound<-Estimates+1.96*30*PhysicalCostSE[1,]
Lowerbound<-Estimates-1.96*30*PhysicalCostSE[1,]
EstimatesFE<-30*PhysicalCostEst[2,]
UpperboundFE<-EstimatesFE+1.96*30*PhysicalCostSE[2,]
LowerboundFE<-EstimatesFE-1.96*30*PhysicalCostSE[2,]

png("FinalOutput/FigA2d.png", width=700)
plot(time, (Estimates), type="l", xlab="Months relative to first moment of contact", ylab="Effect on Physical healthcare expenditure", ylim=c(-300, 300), xaxt="n", lwd=4)
polygon(c(time, rev(time)), c(Upperbound, rev(Lowerbound)), col="grey50", border=NA)
polygon(c(time, rev(time)), c(UpperboundFE, rev(LowerboundFE)), col=adjustcolor("grey", alpha.f=0.5), border=NA)
lines(time, EstimatesFE, lty=2, lwd=4, col="grey40")
lines(time, Estimates, lty=1, lwd=4, col="black")
legend("topright", legend = c("Regional controls","Regional fixed-effects"), col=c("black", "grey50"), lty=c(1,2), lwd=4)
axis(1, labels = c("-48", "-24", "0", "24", "48", "72", "96"), at=seq(-4,8,2))
dev.off()

#heterogeniteit
OutputIVEstimates[20:46,]<-30*HeterogeneityEst[1:27,]*100
OutputIVSE[20:46,]<-30*HeterogeneitySE[1:27,]*100

write.xlsx(OutputIVEstimates, "FinalOutput/Fig8FigA1OutputIVEstimates.xlsx")
write.xlsx(OutputIVSE, "FinalOutput/Fig8FigA1OutputIVSE.xlsx")

#Save heterogeneity seperately
OutputIVEstimates<-30*HeterogeneityEst[1:27,]*100
OutputIVSE<-30*HeterogeneitySE[1:27,]*100
write.xlsx(OutputIVEstimates, "FinalOutput/FigB12IVEstimates.xlsx")
write.xlsx(OutputIVSE, "FinalOutput/FigB12IVSE.xlsx")

#compute correlation regional (un)employment rate and regional waiting time
#Regional (un)employment rates
rm(list=ls())
#Load SECM data, make time series for all individuals, merge to regional level
SECMBUS<-read_sav("G:/InkomenBestedingen/SECMBUS/SECMBUS2021V1.sav", col_select=c(RINPERSOON,AANVSECM,EINDSECM,XKOPPELWERKNSECM,XKOPPELWERKLUITKSECM))

#time series between 2012 and 2019
SECMBUS<-subset(SECMBUS, EINDSECM>=20120101)
SECMBUS<-subset(SECMBUS, AANVSECM<20200101)
#entries with employment or UI
index<-SECMBUS$XKOPPELWERKNSECM=="1"|SECMBUS$XKOPPELWERKLUITKSECM=="1"
SECMBUS<-SECMBUS[index,]
rm(index)
#order
SECMBUS<-SECMBUS[order(SECMBUS$RINPERSOON),]
RINPERSOON<-as.data.frame(as.character(unique(SECMBUS$RINPERSOON)))
names(RINPERSOON)<-"RINPERSOON"
RINPERSOON$RINPERSOON<-as.character(RINPERSOON$RINPERSOON)

#Matrices for various benefits from januari 2012 untill december 2019
Werknemer<-matrix(0, length(RINPERSOON$RINPERSOON),96)
WW<-matrix(0, length(RINPERSOON$RINPERSOON),96)

indexAanvang<-as.data.frame((as.numeric(substr(SECMBUS$AANVSECM,1,4))-2012)*12+as.numeric(substr(SECMBUS$AANVSECM,5,6)))
indexEinde<-as.data.frame((as.numeric(substr(SECMBUS$EINDSECM,1,4))-2012)*12+as.numeric(substr(SECMBUS$EINDSECM,5,6)))
names(indexAanvang)[1]<-"indexAanvang"
names(indexEinde)[1]<-"indexEinde"
SECMBUS%<>%
  dplyr::select(-AANVSECM, -EINDSECM)

indexAanvang$indexAanvang[indexAanvang$indexAanvang<1]<-1
indexEinde$indexEinde[indexEinde$indexEinde>=96]<-96
j<-1
for(i in 1:length(RINPERSOON$RINPERSOON)){
  if(i %% 5000==0){
    cat(paste(i, " "));flush.console()
    gc()
  }
  while((RINPERSOON$RINPERSOON[i]==SECMBUS$RINPERSOON[j])&((j)<nrow(SECMBUS))){
    if(as.character(SECMBUS$XKOPPELWERKNSECM[j])==1){
      Werknemer[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    if(as.character(SECMBUS$XKOPPELWERKLUITKSECM[j])==1){
      WW[i,indexAanvang$indexAanvang[j]:indexEinde$indexEinde[j]]<-1
    }
    j<-j+1
  }
}
endTime<-Sys.time()

rm(SECMBUS, indexAanvang, indexEinde)

GBAPERSOON2012 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2012/GBAPERSOON2012TABV1.sav", col_select = "RINPERSOON")
GBAPERSOON2013 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2013/GBAPERSOON2013TABV1.sav", col_select = "RINPERSOON")
GBAPERSOON2014 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2014/GBAPERSOON2014TABV1.sav", col_select = "RINPERSOON")
GBAPERSOON2015 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2015/GBAPERSOON2015TABV1.sav", col_select = "RINPERSOON")
GBAPERSOON2016 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2016/GBAPERSOONTAB2016V1.sav", col_select = "RINPERSOON")
GBAPERSOON2017 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2017/GBAPERSOON2017TABV1.sav", col_select = "RINPERSOON")
GBAPERSOON2018 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2018/GBAPERSOON2018TABV2.sav", col_select = "RINPERSOON")
GBAPERSOON2019 <- read_sav("G:/Bevolking/GBAPERSOONTAB/2019/GBAPERSOON2019TABV1.sav", col_select = "RINPERSOON")
GBA<-rbind(GBAPERSOON2012, GBAPERSOON2013, GBAPERSOON2014, GBAPERSOON2015, GBAPERSOON2016, GBAPERSOON2017, GBAPERSOON2018, GBAPERSOON2019)
rm(GBAPERSOON2012, GBAPERSOON2013, GBAPERSOON2014, GBAPERSOON2015, GBAPERSOON2016, GBAPERSOON2017, GBAPERSOON2018, GBAPERSOON2019)
GBA<-unique(GBA$RINPERSOON)
GBA<-as.data.frame(GBA)
names(GBA)<-"RINPERSOON"

GBAAdres<- read_sav("G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2021BUSV1.sav", col_select = c("RINPERSOON", "RINOBJECTNUMMER", "GBADATUMAANVANGADRESHOUDING","GBADATUMEINDEADRESHOUDING"))
Gebouwen <- read_sav("G:/BouwenWonen/VSLGWBTAB/VSLGWB2022TAB03V2.sav", col_select = c("RINOBJECTNUMMER", "gem2012", "gem2013", "gem2014", "gem2015", "gem2016", "gem2017", "gem2018", "gem2019"))

index2012<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20120101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20120101
GBAAdres2012<-GBAAdres[index2012,]
GBAAdres2012<-distinct(GBAAdres2012, RINPERSOON,  .keep_all = TRUE)
GBAAdres2012<-merge(GBAAdres2012, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2012%<>%
  dplyr::select(RINPERSOON, gem2012)
GBA<-merge(GBA, GBAAdres2012, by="RINPERSOON", all.x=TRUE)
GBA$gem2012[is.na(GBA$gem2012)]<-"Onbekende woonplaats"
rm(index2012, GBAAdres2012)
gc()

index2013<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20130101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20130101
GBAAdres2013<-GBAAdres[index2013,]
GBAAdres2013<-distinct(GBAAdres2013, RINPERSOON,  .keep_all = TRUE)
GBAAdres2013<-merge(GBAAdres2013, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2013%<>%
  dplyr::select(RINPERSOON, gem2013)
GBA<-merge(GBA, GBAAdres2013, by="RINPERSOON", all.x=TRUE)
GBA$gem2013[is.na(GBA$gem2013)]<-"Onbekende woonplaats"
rm(index2013, GBAAdres2013)
gc()

index2014<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20140101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20140101
GBAAdres2014<-GBAAdres[index2014,]
GBAAdres2014<-distinct(GBAAdres2014, RINPERSOON,  .keep_all = TRUE)
GBAAdres2014<-merge(GBAAdres2014, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2014%<>%
  dplyr::select(RINPERSOON, gem2014)
GBA<-merge(GBA, GBAAdres2014, by="RINPERSOON", all.x=TRUE)
GBA$gem2014[is.na(GBA$gem2014)]<-"Onbekende woonplaats"

index2015<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20150101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20150101
GBAAdres2015<-GBAAdres[index2015,]
GBAAdres2015<-distinct(GBAAdres2015, RINPERSOON,  .keep_all = TRUE)
GBAAdres2015<-merge(GBAAdres2015, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2015%<>%
  dplyr::select(RINPERSOON, gem2015)
GBA<-merge(GBA, GBAAdres2015, by="RINPERSOON", all.x=TRUE)
GBA$gem2015[is.na(GBA$gem2015)]<-"Onbekende woonplaats"

index2016<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20160101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20160101
GBAAdres2016<-GBAAdres[index2016,]
GBAAdres2016<-distinct(GBAAdres2016, RINPERSOON,  .keep_all = TRUE)
GBAAdres2016<-merge(GBAAdres2016, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2016%<>%
  dplyr::select(RINPERSOON, gem2016)
GBA<-merge(GBA, GBAAdres2016, by="RINPERSOON", all.x=TRUE)
GBA$gem2016[is.na(GBA$gem2016)]<-"Onbekende woonplaats"

index2017<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20170101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20170101
GBAAdres2017<-GBAAdres[index2017,]
GBAAdres2017<-distinct(GBAAdres2017, RINPERSOON,  .keep_all = TRUE)
GBAAdres2017<-merge(GBAAdres2017, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2017%<>%
  dplyr::select(RINPERSOON, gem2017)
GBA<-merge(GBA, GBAAdres2017, by="RINPERSOON", all.x=TRUE)
GBA$gem2017[is.na(GBA$gem2017)]<-"Onbekende woonplaats"

index2018<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20180101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20180101
GBAAdres2018<-GBAAdres[index2018,]
GBAAdres2018<-distinct(GBAAdres2018, RINPERSOON,  .keep_all = TRUE)
GBAAdres2018<-merge(GBAAdres2018, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2018%<>%
  dplyr::select(RINPERSOON, gem2018)
GBA<-merge(GBA, GBAAdres2018, by="RINPERSOON", all.x=TRUE)
GBA$gem2018[is.na(GBA$gem2018)]<-"Onbekende woonplaats"

index2019<-GBAAdres$GBADATUMAANVANGADRESHOUDING<20190101 & GBAAdres$GBADATUMEINDEADRESHOUDING>=20190101
GBAAdres2019<-GBAAdres[index2019,]
GBAAdres2019<-distinct(GBAAdres2019, RINPERSOON,  .keep_all = TRUE)
GBAAdres2019<-merge(GBAAdres2019, Gebouwen, by="RINOBJECTNUMMER", all.x=TRUE)
GBAAdres2019%<>%
  dplyr::select(RINPERSOON, gem2019)
GBA<-merge(GBA, GBAAdres2019, by="RINPERSOON", all.x=TRUE)
GBA$gem2019[is.na(GBA$gem2019)]<-"Onbekende woonplaats"

rm(index2012, index2013, index2014, index2015, index2016, index2017, index2018, index2019)
rm(GBAAdres, GBAAdres2012, GBAAdres2013, GBAAdres2014, GBAAdres2015, GBAAdres2016,GBAAdres2017,GBAAdres2018,GBAAdres2019, Gebouwen)
save.image("IntermediateFiles/temp.RData")

#Include regions
#Regions
rm(list=ls())
load("IntermediateFiles/Instrumenten.RData")
rm(list=setdiff(ls(), c("Regions", "RegionalWaitingTime")))

load("IntermediateFiles/temp.RData")
#Delete unused individuals
index<-GBA$RINPERSOON %in% RINPERSOON$RINPERSOON
GBA<-GBA[index,]
index<-RINPERSOON$RINPERSOON %in% GBA$RINPERSOON
RINPERSOON<-RINPERSOON[index,]
Werknemer<-Werknemer[index,]
WW<-WW[index,]
#Delete double GBA
GBA<-distinct(GBA, RINPERSOON, .keep_all = TRUE)

#Compute regional employment and unemployment rate
RegionalEmployment<-matrix(NA, length(Regions), 96)
RegionalUnemployment<-matrix(NA, length(Regions), 96)

for(i in 1:length(Regions)){
  cat(paste(i, " "));flush.console()
  #Compute per year
  for(j in 1:8){
    indexRegion<-GBA[,j+1]==Regions[i]
    if(length(which(indexRegion))>1){
      RegionalEmployment[i,(1+12*(j-1)):(12*j) ]<-colSums(Werknemer[indexRegion, (1+12*(j-1)):(12*j)])
      RegionalUnemployment[i,(1+12*(j-1)):(12*j) ]<-colSums(WW[indexRegion, (1+12*(j-1)):(12*j)])
    }
  }
}

rm(Werknemer, WW)

#Divide by number of inhabitants to get rates
#load inhabitant numbers
kwb_2012 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2012.xls")
index<-kwb_2012$RECS=="G"
kwb_2012<-kwb_2012[index,]
kwb_2012%<>%
  dplyr::select(GWB_CODE12, AANT_INW)

kwb_2013 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2013.xls")
index<-kwb_2013$recs=="Gemeente"
kwb_2013<-kwb_2013[index,]
kwb_2013%<>%
  dplyr::select(gwb_code, a_inw)

names(kwb_2012)<-names(kwb_2013)

kwb_2014 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kerncijfers-wijken-en-buurten-2014.xls")
index<-kwb_2014$recs=="Gemeente"
kwb_2014<-kwb_2014[index,]
kwb_2014%<>%
  dplyr::select(gwb_code, a_inw)

kwb_2015 <- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2015.xls")
index<-kwb_2015$recs=="Gemeente"
kwb_2015<-kwb_2015[index,]
kwb_2015%<>%
  dplyr::select(gwb_code, a_inw)

kwb_2016<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2016.xls")
index<-kwb_2016$recs=="Gemeente"
kwb_2016<-kwb_2016[index,]
kwb_2016%<>%
  dplyr::select(gwb_code, a_inw)

kwb_2017<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2017.xls")
index<-kwb_2017$recs=="Gemeente"
kwb_2017<-kwb_2017[index,]
kwb_2017%<>%
  dplyr::select(gwb_code, a_inw)

kwb_2018<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2018.xls")
index<-kwb_2018$recs=="Gemeente"
kwb_2018<-kwb_2018[index,]
kwb_2018%<>%
  dplyr::select(gwb_code, a_inw)

kwb_2019<- read_excel("K:/Utilities/Wijk_en_Buurtstatistieken/kwb-2019.xlsx")
index<-kwb_2019$recs=="Gemeente"
kwb_2019<-kwb_2019[index,]
kwb_2019%<>%
  dplyr::select(gwb_code, a_inw)

Regions<-as.data.frame(Regions)
names(Regions)<-"Regions"
Regions$Regions<-as.character(Regions$Regions)

kwb_2013$gwb_code<-substring(kwb_2013$gwb_code, 3, 6)
kwb_2014$gwb_code<-substring(kwb_2014$gwb_code, 3, 6)
kwb_2015$gwb_code<-substring(kwb_2015$gwb_code, 3, 6)
kwb_2016$gwb_code<-substring(kwb_2016$gwb_code, 3, 6)
kwb_2017$gwb_code<-substring(kwb_2017$gwb_code, 3, 6)
kwb_2018$gwb_code<-substring(kwb_2018$gwb_code, 3, 6)
kwb_2019$gwb_code<-substring(kwb_2019$gwb_code, 3, 6)

Regions<-merge(Regions, kwb_2012, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[2]<-"a_inw2012"
Regions<-merge(Regions, kwb_2013, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[3]<-"a_inw2013"
Regions<-merge(Regions, kwb_2014, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[4]<-"a_inw2014"
Regions<-merge(Regions, kwb_2015, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[5]<-"a_inw2015"
Regions<-merge(Regions, kwb_2016, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[6]<-"a_inw2016"
Regions<-merge(Regions, kwb_2017, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[7]<-"a_inw2017"
Regions<-merge(Regions, kwb_2018, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[8]<-"a_inw2018"
Regions<-merge(Regions, kwb_2019, by.x="Regions", by.y="gwb_code", all.x=TRUE )
names(Regions)[9]<-"a_inw2019"

RegionalEmploymentRate<-matrix(NA, nrow(Regions), 96)
RegionalUnemploymentRate<-matrix(NA, nrow(Regions), 96)

for(i in 1:nrow(Regions)){
  for(j in 1:8){
    if(!is.na(Regions[i,j+1])){
      RegionalEmploymentRate[i,(12*(j-1)+1):(12*j)]<-RegionalEmployment[i,(12*(j-1)+1):(12*j)]/as.numeric(Regions[i,(j+1)])
      RegionalUnemploymentRate[i,(12*(j-1)+1):(12*j)]<-RegionalUnemployment[i,(12*(j-1)+1):(12*j)]/as.numeric(Regions[i,(j+1)])
    }
  }
}

#Make laggs of regional unemployment
RegionalEmploymentRate1<-matrix(NA, nrow(Regions), 96)
RegionalUnemploymentRate1<-matrix(NA, nrow(Regions), 96)
RegionalEmploymentRate2<-matrix(NA, nrow(Regions), 96)
RegionalUnemploymentRate2<-matrix(NA, nrow(Regions), 96)
RegionalEmploymentRate3<-matrix(NA, nrow(Regions), 96)
RegionalUnemploymentRate3<-matrix(NA, nrow(Regions), 96)
RegionalEmploymentRate1[,2:96]<-RegionalEmploymentRate[,1:95]
RegionalUnemploymentRate1[,2:96]<-RegionalUnemploymentRate[,1:95]
RegionalEmploymentRate2[,3:96]<-RegionalEmploymentRate[,1:94]
RegionalUnemploymentRate2[,3:96]<-RegionalUnemploymentRate[,1:94]
RegionalEmploymentRate3[,4:96]<-RegionalEmploymentRate[,1:93]
RegionalUnemploymentRate3[,4:96]<-RegionalUnemploymentRate[,1:93]

RegionalMatrix<-as.data.frame(cbind(c(t(RegionalWaitingTime)), c(t(RegionalEmploymentRate)), c(t(RegionalEmploymentRate1)),c(t(RegionalEmploymentRate2)),c(t(RegionalEmploymentRate3)), c(t(RegionalUnemploymentRate)),c(t(RegionalUnemploymentRate1)),c(t(RegionalUnemploymentRate2)),c(t(RegionalUnemploymentRate3))))
names(RegionalMatrix)<-c("RegionalWaitingTime", "Employment", "Employment1", "Employment2", "Employment3", "Unemployment", "Unemployment1", "Unemployment2", "Unemployment3")
Region<-matrix(NA, nrow(RegionalMatrix),1)
for(i in 1:nrow(Regions)){
  Region[(1+(96*(i-1))):(96*i),1]<-rep(Regions$Regions[i],96)
}
RegionalMatrix<-cbind(RegionalMatrix, Region)

#Add month dummies
MonthDummies<-matrix(NA, nrow(RegionalMatrix),1)
for(i in 1:nrow(Regions)){
  MonthDummies[(1+(96*(i-1))):(96*i),1]<-seq(1,96)
}
RegionalMatrix<-cbind(RegionalMatrix, MonthDummies)

#Change rates to percentages
RegionalMatrix[,2:9]<-RegionalMatrix[,2:9]*100

#compute average waiting time in 3 month period
AverageWaiting<-matrix(NA, nrow(RegionalMatrix),1)
for(i in 3:nrow(RegionalMatrix)){
  AverageWaiting[i]<-mean(RegionalMatrix$RegionalWaitingTime[(i-2):i], na.rm=TRUE)
}
AverageWaiting[RegionalMatrix$MonthDummies=="1"|RegionalMatrix$MonthDummies=="2"]<-NA
RegionalMatrix<-cbind(AverageWaiting, RegionalMatrix)
rm(AverageWaiting)

Test1<-coeftest(lm(RegionalWaitingTime~Employment+Unemployment +as.factor(MonthDummies), data=RegionalMatrix))[1:3,]
Test2<-coeftest(lm(RegionalWaitingTime~Employment+Employment1+Unemployment+Unemployment1+ as.factor(MonthDummies), data=RegionalMatrix))[1:5,]
Test3<-coeftest(lm(RegionalWaitingTime~Employment+Employment1+Employment2+Unemployment+Unemployment1+Unemployment2+ as.factor(MonthDummies), data=RegionalMatrix))[1:7,]
Test4<-coeftest(lm(RegionalWaitingTime~Employment+Employment1+Employment2+Employment3+Unemployment+Unemployment1+Unemployment2+Unemployment3+ as.factor(MonthDummies), data=RegionalMatrix))[1:9,]

TabA1<-Test4[2:9,1:2]
write.xlsx(TabA1, "FinalOutput/TabA1.xlsx")
save.image("IntermediateFiles/RegionalUnemploymentRates.RData")

#Check P(Start intake) and P(Treatment)
rm(list=ls())
#Link instruments to individuals. Use wachttijden of last 3 months
load("IntermediateFiles/Instrumenten.RData")
load("IntermediateFiles/WachttijdenAll.RData")
rm(list=setdiff(ls(), c("Wachttijd", "Regions", "RegionalPatients", "RegionalWaitingTime")))

#Change onbekende woonplaats to NA
RegionalWaitingTime[428,]<-NA

#Delete starts afetr 96
index<-Wachttijd$IndexStartGGZ<97
Wachttijd<-Wachttijd[index,]

#link to individual data
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
WachttijdRegioInd<-matrix(NA, nrow(Wachttijd),1)

for(i in 1:nrow(Wachttijd)){
  if(Wachttijd$IndexStartGGZ[i]>=2){
    if(i%%10000==0){
      cat(paste(i, " "));flush.console()
    }
    Regionum<-which(Regions==Wachttijd$Gemeente[i])
    if(length(Regionum)==1){
      if(sum(RegionalPatients[Regionum,max(1,Wachttijd$IndexStartGGZ[i]-2):Wachttijd$IndexStartGGZ[i]], na.rm=TRUE)>10){
        #patients without waiting time
        if(is.na(Wachttijd$Behandeltijd[i])){
          WachtRegio<-RegionalWaitingTime[Regionum,Wachttijd$IndexStartGGZ[i]]
          TotalWacht<-c(WachtRegio, RegionalWaitingTime[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)])
          TotalNum<-c(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1, (RegionalPatients[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
          TotalWachtSum<-TotalWacht*TotalNum
          WachttijdRegioInd[i,1]<-sum(TotalWachtSum, na.rm=TRUE)/sum(TotalNum,na.rm=TRUE)
        }
        #patients with waiting time
        if(!is.na(Wachttijd$Behandeltijd[i])){
          WachtRegio<-(RegionalWaitingTime[Regionum,Wachttijd$IndexStartGGZ[i]]*RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-Wachttijd$Behandeltijd[i])/(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1)
          TotalWacht<-c(WachtRegio, RegionalWaitingTime[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)])
          TotalNum<-c(RegionalPatients[Regionum,Wachttijd$IndexStartGGZ[i]]-1, (RegionalPatients[Regionum,(Wachttijd$IndexStartGGZ[i]-1):max(1,Wachttijd$IndexStartGGZ[i]-2)]))
          TotalWachtSum<-TotalWacht*TotalNum
          WachttijdRegioInd[i,1]<-sum(TotalWachtSum, na.rm=TRUE)/sum(TotalNum,na.rm=TRUE)
        }
      }
    }
  }
}
WachttijdRegioInd[WachttijdRegioInd<=0]<-NA
Wachttijd<-cbind(Wachttijd, WachttijdRegioInd)
rm(WachttijdRegioInd)
rm(RegionalWaitingTime)

Wachttijd$HaveIntake<-!is.na(Wachttijd$Aanmeldtijd)
Wachttijd$HaveTreatment<-!is.na(Wachttijd$Behandeltijd)

#Add diagnosis group and crisis
Wachttijd$DiagnosisGroup<-NA
Wachttijd$DiagnosisGroup[substring(Wachttijd$GGZDBCHoofddiagnoseDSMIV,1,5)=="as1_6"]<-"Mood"
Wachttijd$DiagnosisGroup[Wachttijd$GGZDBCHoofddiagnoseDSMIV=="11"|Wachttijd$GGZDBCHoofddiagnoseDSMIV=="12"]<-"Mood"
Wachttijd$DiagnosisGroup[substring(Wachttijd$GGZDBCHoofddiagnoseDSMIV,1,5)=="as1_7"]<-"Anxiety"
Wachttijd$DiagnosisGroup[Wachttijd$GGZDBCHoofddiagnoseDSMIV=="13"]<-"Anxiety"
Wachttijd$DiagnosisGroup[substring(Wachttijd$GGZDBCHoofddiagnoseDSMIV,1,6)=="as2_16"]<-"Personality"
Wachttijd$DiagnosisGroup[Wachttijd$GGZDBCHoofddiagnoseDSMIV=="14"]<-"Personality"
Wachttijd$DiagnosisGroup[is.na(Wachttijd$DiagnosisGroup)]<-"Other"
Wachttijd$Crisis[is.na(Wachttijd$Crisis)]<-0

#Add individual demographic controls
GBA <- read_sav("G:/Bevolking/GBAPERSOONTAB/2019/GBAPERSOON2019TABV1.sav")
GBA%<>%
  select(RINPERSOON, GBAGESLACHT, GBAGEBOORTEJAAR, GBAGENERATIE)
Wachttijd<-merge(Wachttijd, GBA, by="RINPERSOON", all.x=TRUE)
rm(GBA)

#Delete long or negative behandeltijd
index<-is.na(Wachttijd$Behandeltijd)|(Wachttijd$Behandeltijd>0&Wachttijd$Behandeltijd<360)
Wachttijd<-Wachttijd[index,]

#Focus on 18-65 year olds
JaarDummy<-substring(Wachttijd$AanmeldDatum,1,4)
Wachttijd<-cbind(Wachttijd, JaarDummy)
rm(JaarDummy)
Leeftijd<-as.numeric(as.character(Wachttijd$JaarDummy))-as.numeric(Wachttijd$GBAGEBOORTEJAAR)
Wachttijd<-cbind(Wachttijd, Leeftijd)
index<-!is.na(Wachttijd$Leeftijd)
Wachttijd<-Wachttijd[index,]
index<-Wachttijd$Leeftijd>17&Wachttijd$Leeftijd<66
Wachttijd<-Wachttijd[index,]

#Delete individual with pre-treatment spending
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
Wachttijd<-Wachttijd[order(Wachttijd$RINPERSOON),]
rm(Medicijnen)
MentalCost<-Wachttijd[,c(20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53)]
PhysicalCost<-Wachttijd[,c(21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54)]
MedicijnCost<-Wachttijd[,c(22, 25, 28, 31, 34, 37, 40, 43, 46, 49, 52, 55)]
for(j in 1:12){
  MentalCost[,j]<-as.numeric(as.character(MentalCost[,j]))
  PhysicalCost[,j]<-as.numeric(as.character(PhysicalCost[,j]))
  MedicijnCost[,j]<-as.numeric(as.character(MedicijnCost[,j]))
}
MentalCost<-as.matrix(MentalCost)
PhysicalCost<-as.matrix(PhysicalCost)
MedicijnCost<-as.matrix(MedicijnCost)
Wachttijd<-Wachttijd[,1:19]

#Plots of measures relative to application data
MentalCostGGZ<-matrix(NA, nrow(Wachttijd), 24)
PhysicalCostGGZ<-matrix(NA, nrow(Wachttijd), 24)
MedicijnenGGZ<-matrix(NA, nrow(Wachttijd), 24)

#12 is jaar van aanmelding
for(i in 1:nrow(Wachttijd)){
  if(!is.na(Wachttijd$IndexStartGGZ[i])){
    if(Wachttijd$IndexStartGGZ[i]>0&Wachttijd$IndexStartGGZ[i]<97){
      MentalCostGGZ[i,(10-ceiling(Wachttijd$IndexStartGGZ[i]/12)):(21-ceiling(Wachttijd$IndexStartGGZ[i]/12))]<-MentalCost[i,]
      PhysicalCostGGZ[i,(10-ceiling(Wachttijd$IndexStartGGZ[i]/12)):(21-ceiling(Wachttijd$IndexStartGGZ[i]/12))]<-PhysicalCost[i,]
      MedicijnenGGZ[i,(10-ceiling(Wachttijd$IndexStartGGZ[i]/12)):(21-ceiling(Wachttijd$IndexStartGGZ[i]/12))]<-MedicijnCost[i,]
      if(i%%1000==0){
        gc()
      }
    }
  }
}
#Delete individuals with pre-start mental healthcare spending
index<-rowSums(MentalCostGGZ[,9:11], na.rm=TRUE)<=0
Wachttijd<-Wachttijd[index,]

coeftest(lm(HaveIntake~WachttijdRegioInd + Gemeente + as.factor(GBAGESLACHT) + as.factor(GBAGEBOORTEJAAR) + as.factor(GBAGENERATIE), data=Wachttijd))[1:2,]
coeftest(lm(HaveTreatment~WachttijdRegioInd + Gemeente + as.factor(GBAGESLACHT) + as.factor(GBAGEBOORTEJAAR) + as.factor(GBAGENERATIE), data=Wachttijd))[1:2,]

#Check correlation between RW and P(other region)
rm(list=ls())
load("IntermediateFiles/CompositionData.RData")
rm(list=setdiff(ls(), c("Wachttijd")))

#Get institutions
Instelling<-unique(Wachttijd$GGZInstelling)
RegionInstelling<-matrix(NA, length(Instelling),1)
for(i in 1:length(Instelling)){
  RegionalPatients<-Wachttijd[Wachttijd$GGZInstelling==Instelling[i],]
  RegionInstelling[i]<-names(which.max(table(RegionalPatients$Gemeente)))
}
Instelling<-as.data.frame(cbind(Instelling, RegionInstelling))
rm(RegionInstelling)
names(Instelling)[2]<-"RegionInstelling"

#merge with individual data
Wachttijd<-merge(Wachttijd, Instelling, by.x="GGZInstelling", by.y="Instelling", all.x=TRUE)

#Indicator for treatment in other municipality
Wachttijd$OtherMunicipality<-Wachttijd$Gemeente==Wachttijd$RegionInstelling

Composition<-biglm(WachttijdRegioInd~OtherMunicipality + Man + LeeftijdLin + GBAGENERATIE + EduCat+ DiagnosisGroup + BehandelaarCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO, data=Wachttijd)
CompositionTotal<-biglm(WachttijdRegioInd~OtherMunicipality + Man + LeeftijdLin + GBAGENERATIE + EduCat+ DiagnosisGroup + BehandelaarCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO + as.character(p_w_all) +  as.character(p_marok) + as.character(p_tur) + as.character(g_woz) + as.character(p_koopw) + as.character(p_wcorpw) + as.character(p_bjj2k) + as.character(g_ink_pi) + as.character(p_ink_li) + as.character(p_ink_hi) + as.character(p_hh_osm) + as.character(a_soz_ww) + as.character(a_soz_ao) + as.character(bev_dich), data=Wachttijd)
CompositionTotalDummies<-biglm(WachttijdRegioInd~OtherMunicipality + Man + LeeftijdLin + GBAGENERATIE + EduCat+ DiagnosisGroup + BehandelaarCat + PreWerknemer + PreUren + PreEarnings +IndexStartGGZ + Crisis + PreWW + PreBijstand + PreZiekteAO + Gemeente, data=Wachttijd)

#Estimate impact RW on seeking treatment elsewhere
CompositionResults<-matrix(NA, 14, 6)
CompositionResults[,1:2]<-coeftest(Composition)[2:15,1:2]
CompositionResults[,3:4]<-coeftest(CompositionTotal)[2:15,1:2]
CompositionResults[,5:6]<-coeftest(CompositionTotalDummies)[2:15,1:2]

write.xlsx(CompositionResults, "IntermediateFiles/Fig7Part2.xlsx")
save.image("IntermediateFiles/Fig7Part2.RData")
