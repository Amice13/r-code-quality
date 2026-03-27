
## Effects of Artisanal Gold Mining on Biological Trace Element Concentrations
## in Two Amazonian Headwater Streams

## Alexander R. Pelletier, University of Saskatchewan Toxicology Centre


## Working Directory and Importing Packages

setwd("C:/Users/Owner/Desktop/Effects of Artisanal Gold Mning on Biological Trace Element Concentrations in Two Amazonian Headwater Streams")
library(readxl)
library(ggplot2)
library(lme4)
library(tidyverse)
library(stargazer)
library(patchwork)
library(reporter)
library(magrittr)
library(factoextra)
library(NbClust)
library(sm)
library(car)
par(family="sans", las=1, bg=NA)



###### Importing and Subsetting Data ######


#### Cumulative Individuals Dataset

## This dataset lists each individual biological specimen collected
## from all seven sites: five reference sites and two mining sites.
## Many rows (i.e., specimens) will not be included in analysis because
## the determination of trace element concentrations in one biological sample
## is very expensive, so we could only measure trace element concentrations 
## in a subset of samples from each site. Only samples with both d15N and
## trace element concentration data will be analyzed.

IndividualsData <- read_excel("Mining Individual Data Condensed.xlsx")
attach(IndividualsData)

## Site information
IndividualsData$SiteCategory <- factor(IndividualsData$SiteCategory,
                                       levels=c("Reference", "Mining"))
IndividualsData$SiteID <- factor(IndividualsData$SiteID)
IndividualsData$SiteName <- factor(IndividualsData$SiteName)


## Sample information (Biological data)
IndividualsData$Taxon <- factor(IndividualsData$Taxon)
IndividualsData$Cohort <- factor(IndividualsData$Cohort)
IndividualsData$Sp.Code <- factor(IndividualsData$Sp.Code)
IndividualsData$SampleType <- factor(IndividualsData$SampleType)
IndividualsData$TotalLength <- as.numeric(IndividualsData$TotalLength)
IndividualsData$d15N <- as.numeric(IndividualsData$d15N)
IndividualsData$d13C <- as.numeric(IndividualsData$d13C)


## Chemical concentrations
IndividualsData$Element <- factor(IndividualsData$Element,
      levels=c("Aluminum", "Arsenic", "Barium", "Cadmium", "Cobalt",
               "Copper", "Iron", "Lead", "Manganese", "Mercury",
               "Molybdenum", "Nickel", "Selenium", "Strontium", "Thallium",
               "Titanium", "Uranium", "Vanadium", "Zinc"))
IndividualsData$ConcentrationPPM <- as.numeric(IndividualsData$ConcentrationPPM)
IndividualsData$TraceType <- factor(IndividualsData$TraceType,
                                    levels = c("Trace", "UltraTrace"))


#### Cumulative Sites Dataset

## This dataset includes data about the sites rather than the individual
## biological specimens. It's a small dataset with only seven rows
## (seven sites). It contains environmental and ecological data as well as
## the trophic magnification slopes (TMSs) that we will define and
## calculate below.

SitesData <- read_excel("Mining Site Data.xlsx")
attach(SitesData)

## Physical environmental data
SitesData$SiteID <- factor(SitesData$SiteID)
SitesData$SiteCategory <- factor(SitesData$SiteCategory,
                                 levels=c("Reference", "Mining"))
SitesData$SiteName <- factor(SitesData$SiteName)
SitesData$DateSampled <- factor(SitesData$DateSampled)
SitesData$Latitude <- as.numeric(SitesData$Latitude)
SitesData$Longitude <- as.numeric(SitesData$Longitude)
SitesData$Elevation <- as.numeric(SitesData$Elevation)
SitesData$WettedWidth <- as.numeric(SitesData$WettedWidth)
SitesData$MeanWettedDepth <- as.numeric(SitesData$MeanWettedDepth)
SitesData$WettedCrossSection <- as.numeric(SitesData$WettedCrossSection)
SitesData$MeanVelocity <- as.numeric(SitesData$MeanVelocity)
SitesData$Discharge <- as.numeric(SitesData$Discharge)
SitesData$Log10Discharge <- as.numeric(SitesData$Log10Discharge)
SitesData$BankfullWidth <- as.numeric(SitesData$BankfullWidth)
SitesData$MeanBankfullDepth <- as.numeric(SitesData$MeanBankfullDepth)
SitesData$BankfullCrossSection <- as.numeric(SitesData$BankfullCrossSection)
SitesData$CanopyCover <- as.numeric(SitesData$CanopyCover)
SitesData$PfankuchRaw <- as.numeric(SitesData$PfankuchRaw)
SitesData$PfankuchClass <- factor(SitesData$PfankuchClass)
## Pfankuch scores measure the degree of disturance at a site, with larger
## numbers representing greater disturbance. "Raw" is the numeric score,
## "Class" is a subjective categorical (qualitative) metric like "good" and "fair"

## Water chemistry
SitesData$pH <- as.numeric(SitesData$pH)
SitesData$Temp <- as.numeric(SitesData$Temp)
SitesData$Conductivity <- as.numeric(SitesData$Conductivity)
SitesData$TN <- as.numeric(SitesData$TN) ## Total nitrogen
SitesData$TP <- as.numeric(SitesData$TP) ## Total phosphorus
SitesData$PhosphorusTSI <- as.numeric(SitesData$PhosphorusTSI) ## Trophic State Index
SitesData$TRA_TN_Discharge <- as.numeric(SitesData$TRA_TN_Discharge)
SitesData$TRA_TP_Discharge <- as.numeric(SitesData$TRA_TP_Discharge)
## TRA_TN and TRA_TP are the Total Resource Availability of nitrogen and phosphorus,
## respectively. When TN and TP are multiplied by discharge, it measures the
## mass of nitrogen and phosphorus that flow past a point in the stream every 1 second
## (because me measured discharge in units of volume per second)
SitesData$d13C_Min <- as.numeric(SitesData$d13C_Min)
SitesData$d13C_Max <- as.numeric(SitesData$d13C_Max)
SitesData$d13C_Range <- as.numeric(SitesData$d13C_Range)
SitesData$d15N_Min <- as.numeric(SitesData$d15N_Min)
SitesData$d15N_Max <- as.numeric(SitesData$d15N_Max)
SitesData$d15N_Range <- as.numeric(SitesData$d15N_Range)

## Trophic magnification slopes (TMSs). These were calculated below, then
## added to this datasheet afterwards.

SitesData$TMS_Al <- as.numeric(SitesData$TMS_Al)
SitesData$TMS_Ti <- as.numeric(SitesData$TMS_Ti)
SitesData$TMS_V <- as.numeric(SitesData$TMS_V)
SitesData$TMS_Mn <- as.numeric(SitesData$TMS_Mn)
SitesData$TMS_Fe <- as.numeric(SitesData$TMS_Fe)
SitesData$TMS_Co <- as.numeric(SitesData$TMS_Co)
SitesData$TMS_Ni <- as.numeric(SitesData$TMS_Ni)
SitesData$TMS_Cu <- as.numeric(SitesData$TMS_Cu)
SitesData$TMS_Zn <- as.numeric(SitesData$TMS_Zn)
SitesData$TMS_As <- as.numeric(SitesData$TMS_As)
SitesData$TMS_Se <- as.numeric(SitesData$TMS_Se)
SitesData$TMS_Sr <- as.numeric(SitesData$TMS_Sr)
SitesData$TMS_Cd <- as.numeric(SitesData$TMS_Cd)
SitesData$TMS_Ba <- as.numeric(SitesData$TMS_Ba)
SitesData$TMS_Hg <- as.numeric(SitesData$TMS_Hg)
SitesData$TMS_Tl <- as.numeric(SitesData$TMS_Tl)
SitesData$TMS_Pb <- as.numeric(SitesData$TMS_Pb)
SitesData$TMS_U <- as.numeric(SitesData$TMS_U)
SitesData$TMS_Mo <- as.numeric(SitesData$TMS_Mo)

## Trophic magnification factors (TMFs). These are calculated as
## TMF = 10^(3.4*TMS), where 3.4 is the mean permil change in d15N per trophic
## level. A trace element's TMF at a given location therefore represents
## the average factor by which the biological concentration of that trace
## element either increases (TMF > 1.0) or decreases (TMF < 1.0) per
## trophic level. The TMF is more ecologically relevant and meaningful,
## but the TMS data are better suited for statistical testing because they
## meet the assumptions of parametric testing (e.g., normality of residuals,
## homogeneity of variances between reference and mining sites).

SitesData$TMF_Al <- as.numeric(SitesData$TMF_Al)
SitesData$TMF_Ti <- as.numeric(SitesData$TMF_Ti)
SitesData$TMF_V <- as.numeric(SitesData$TMF_V)
SitesData$TMF_Mn <- as.numeric(SitesData$TMF_Mn)
SitesData$TMF_Fe <- as.numeric(SitesData$TMF_Fe)
SitesData$TMF_Co <- as.numeric(SitesData$TMF_Co)
SitesData$TMF_Ni <- as.numeric(SitesData$TMF_Ni)
SitesData$TMF_Cu <- as.numeric(SitesData$TMF_Cu)
SitesData$TMF_Zn <- as.numeric(SitesData$TMF_Zn)
SitesData$TMF_As <- as.numeric(SitesData$TMF_As)
SitesData$TMF_Se <- as.numeric(SitesData$TMF_Se)
SitesData$TMF_Sr <- as.numeric(SitesData$TMF_Sr)
SitesData$TMF_Cd <- as.numeric(SitesData$TMF_Cd)
SitesData$TMF_Ba <- as.numeric(SitesData$TMF_Ba)
SitesData$TMF_Hg <- as.numeric(SitesData$TMF_Hg)
SitesData$TMF_Tl <- as.numeric(SitesData$TMF_Tl)
SitesData$TMF_Pb <- as.numeric(SitesData$TMF_Pb)
SitesData$TMF_U <- as.numeric(SitesData$TMF_U)
SitesData$TMF_Mo <- as.numeric(SitesData$TMF_Mo)

## Concentrations of trace elements in water

SitesData$Water_Al <- as.numeric(SitesData$Water_Al)
SitesData$Water_V <- as.numeric(SitesData$Water_V)
SitesData$Water_Mn <- as.numeric(SitesData$Water_Mn)
SitesData$Water_Fe <- as.numeric(SitesData$Water_Fe)
SitesData$Water_Co <- as.numeric(SitesData$Water_Co)
SitesData$Water_Ni <- as.numeric(SitesData$Water_Ni)
SitesData$Water_Cu <- as.numeric(SitesData$Water_Cu)
SitesData$Water_Zn <- as.numeric(SitesData$Water_Zn)
SitesData$Water_As <- as.numeric(SitesData$Water_As)
SitesData$Water_Se <- as.numeric(SitesData$Water_Se)
SitesData$Water_Sr <- as.numeric(SitesData$Water_Sr)
SitesData$Water_Cd <- as.numeric(SitesData$Water_Cd)
SitesData$Water_Ba <- as.numeric(SitesData$Water_Ba)
SitesData$Water_Tl <- as.numeric(SitesData$Water_Tl)
SitesData$Water_Pb <- as.numeric(SitesData$Water_Pb)
SitesData$Water_U <- as.numeric(SitesData$Water_U)

## Water column concentrations could not be quantified for
## titanium, molybdenum, and mercury



###### Subsetting Data

ReferencesData <- subset(SitesData, SiteCategory == "Reference")
attach(ReferencesData)

MiningsData <- subset(SitesData, SiteCategory == "Mining")
attach(MiningsData)

ReferenceData <- subset(IndividualsData, SiteCategory == "Reference")
attach(ReferenceData)

MiningData <- subset(IndividualsData, SiteCategory == "Mining")
attach(MiningData)

## Reference Sites R1, R2, R3, R4, and R5

R1Data <- subset(IndividualsData, SiteID == "R1")
attach(R1Data)

R2Data <- subset(IndividualsData, SiteID == "R2")
attach(R2Data)

R3Data <- subset(IndividualsData, SiteID == "R3")
attach(R3Data)

R4Data <- subset(IndividualsData, SiteID == "R4")
attach(R4Data)

R5Data <- subset(IndividualsData, SiteID == "R5")
attach(R5Data)

## Mining Sites M1 and M2

M1Data <- subset(IndividualsData, SiteID == "M1")
attach(M1Data)

M2Data <- subset(IndividualsData, SiteID == "M2")
attach(M2Data)


## Fish vs Invertebrates

FishData <- subset(IndividualsData, SampleType == "Fish")
attach(FishData)

InvertData <- subset(IndividualsData, SampleType == "Invertebrate")
attach(InvertData)


## Elemental Subsets

AluminumData <- subset(IndividualsData, Element == "Aluminum")
attach(AluminumData)

ArsenicData <- subset(IndividualsData, Element == "Arsenic")
attach(ArsenicData)

BariumData <- subset(IndividualsData, Element == "Barium")
attach(BariumData)

CadmiumData <- subset(IndividualsData, Element == "Cadmium")
attach(CadmiumData)

CobaltData <- subset(IndividualsData, Element == "Cobalt")
attach(CobaltData)

CopperData <- subset(IndividualsData, Element == "Copper")
attach(CopperData)

IronData <- subset(IndividualsData, Element == "Iron")
attach(IronData)

LeadData <- subset(IndividualsData, Element == "Lead")
attach(LeadData)

ManganeseData <- subset(IndividualsData, Element == "Manganese")
attach(ManganeseData)

MercuryData <- subset(IndividualsData, Element == "Mercury")
attach(MercuryData)

MolybdenumData <- subset(IndividualsData, Element == "Molybdenum")
attach(MolybdenumData)

NickelData <- subset(IndividualsData, Element == "Nickel")
attach(NickelData)

SeleniumData <- subset(IndividualsData, Element == "Selenium")
attach(SeleniumData)

StrontiumData <- subset(IndividualsData, Element == "Strontium")
attach(StrontiumData)

ThalliumData <- subset(IndividualsData, Element == "Thallium")
attach(ThalliumData)

TitaniumData <- subset(IndividualsData, Element == "Titanium")
attach(TitaniumData)

UraniumData <- subset(IndividualsData, Element == "Uranium")
attach(UraniumData)

VanadiumData <- subset(IndividualsData, Element == "Vanadium")
attach(VanadiumData)

ZincData <- subset(IndividualsData, Element == "Zinc")
attach(ZincData)


## Trace vs. UltraTrace Elements

TraceData <- subset(IndividualsData, TraceType == "Trace")
attach(TraceData)

UltraTraceData <- subset(IndividualsData, TraceType == "UltraTrace")
attach(UltraTraceData)


## Trophic Guilds

OmnivoreData <- subset(IndividualsData, Guild == "Omnivore")
PredatorData <- subset(IndividualsData, Guild == "Predator")


## Sites Data organized for making figures

SFDataFull <- read_excel("Mining Site Figure Data Full.xlsx")
attach(SFDataFull)

SFDataFull$SiteID <- factor(SFDataFull$SiteID,
                      levels=c("R1", "R2", "R3", "R4", "R5", "M1", "M2"))
SFDataFull$Element <- factor(SFDataFull$Element)
SFDataFull$TMS <- as.numeric(SFDataFull$TMS)
SFDataFull$TMF <- as.numeric(SFDataFull$TMF)
SFDataFull$LogOmniConc <- as.numeric(SFDataFull$LogOmniConc)
SFDataFull$LogOmniSD <- as.numeric(SFDataFull$LogOmniSD)
SFDataFull$LogAqueous <- as.numeric(SFDataFull$LogAqueous)


SFDataTrimmed <- read_excel("Mining Site Figure Data Trimmed.xlsx")
attach(SFDataTrimmed)

SFDataTrimmed$SiteID <- factor(SFDataTrimmed$SiteID,
                                 levels=c("R1", "R2", "R3", "R4", "R5", "M1", "M2"))
SFDataTrimmed$Element <- factor(SFDataTrimmed$Element)
SFDataTrimmed$TMS <- as.numeric(SFDataTrimmed$TMS)
SFDataTrimmed$TMF <- as.numeric(SFDataTrimmed$TMF)
SFDataTrimmed$LogOmniConc <- as.numeric(SFDataTrimmed$LogOmniConc)
SFDataTrimmed$LogOmniSD <- as.numeric(SFDataTrimmed$LogOmniSD)
SFDataTrimmed$LogAqueous <- as.numeric(SFDataTrimmed$LogAqueous)


SFDataNoM2 <- read_excel("Mining Site Figure Data No M2.xlsx")
attach(SFDataNoM2)

SFDataNoM2$SiteID <- factor(SFDataNoM2$SiteID,
                                 levels=c("R1", "R2", "R3", "R4", "R5", "M1"))
SFDataNoM2$Element <- factor(SFDataNoM2$Element)
SFDataNoM2$TMS <- as.numeric(SFDataNoM2$TMS)
SFDataNoM2$TMF <- as.numeric(SFDataNoM2$TMF)
SFDataNoM2$LogOmniConc <- as.numeric(SFDataNoM2$LogOmniConc)
SFDataNoM2$LogOmniSD <- as.numeric(SFDataNoM2$LogOmniSD)
SFDataNoM2$LogAqueous <- as.numeric(SFDataNoM2$LogAqueous)


SFDataTrimmedNoM2 <- read_excel("Mining Site Figure Data Trimmed No M2.xlsx")
attach(SFDataTrimmedNoM2)

SFDataTrimmedNoM2$SiteID <- factor(SFDataTrimmedNoM2$SiteID,
                            levels=c("R1", "R2", "R3", "R4", "R5", "M1"))
SFDataTrimmedNoM2$Element <- factor(SFDataTrimmedNoM2$Element)
SFDataTrimmedNoM2$TMS <- as.numeric(SFDataTrimmedNoM2$TMS)
SFDataTrimmedNoM2$TMF <- as.numeric(SFDataTrimmedNoM2$TMF)
SFDataTrimmedNoM2$LogOmniConc <- as.numeric(SFDataTrimmedNoM2$LogOmniConc)
SFDataTrimmedNoM2$LogOmniSD <- as.numeric(SFDataTrimmedNoM2$LogOmniSD)
SFDataTrimmedNoM2$LogAqueous <- as.numeric(SFDataTrimmedNoM2$LogAqueous)



###### Calculating Trophic Magnification Slopes (TMSs)


## To get the TMS of an element at a site, run the corresponding 15-line
## block of code below. For example, if I want to calculate the TMS of aluminum
## at the R1 site, I will run lines 343 - 357. The TMS will appear when the
## "summary()" line of code (line 357) is run. Under the "Coefficients:"
## heading, go to the "R1AluminumData$d15N" row and across to the "Estimate" column.
## That number (in this case, -0.1687) is the TMS. Then, rinse and repeat!

## Calculating TMF's will not be shown here, but TMF's can be calculated
## via the equation TMF = 10^(3.4*TMS), wherein 3.4 is the mean permil
## change in d15N per trophic level. Rather than do that here, it's easier
## to transfer all the TMS's to an Excel sheet (as we did in the 
## "Sites Data" datasheet) and use an Excel formula to generate TMF's.
## All TMS's calculated below, along with their corresponding TMF's, were
## manually transfered to SitesData for further analyses.



###### R1 (Rio Pashimbi Chico)


## Aluminum

R1AluminumData <- subset(R1Data, Element=="Aluminum")
plot(log10(R1AluminumData$ConcentrationPPM) ~ R1AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1AluminumData$ConcentrationPPM) ~ R1AluminumData$d15N), col="blue")

R1TMS_Al <- lm(log10(R1AluminumData$ConcentrationPPM) ~ R1AluminumData$d15N)
summary(R1TMS_Al)
plot(R1TMS_Al$residuals)


## Arsenic

R1ArsenicData <- subset(R1Data, Element=="Arsenic")
plot(log10(R1ArsenicData$ConcentrationPPM) ~ R1ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1ArsenicData$ConcentrationPPM) ~ R1ArsenicData$d15N), col="blue")

R1TMS_As <- lm(log10(R1ArsenicData$ConcentrationPPM) ~ R1ArsenicData$d15N)
summary(R1TMS_As)
plot(R1TMS_As$residuals)


## Barium

R1BariumData <- subset(R1Data, Element=="Barium")
plot(log10(R1BariumData$ConcentrationPPM) ~ R1BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1BariumData$ConcentrationPPM) ~ R1BariumData$d15N), col="blue")

R1TMS_Ba <- lm(log10(R1BariumData$ConcentrationPPM) ~ R1BariumData$d15N)
summary(R1TMS_Ba)
plot(R1TMS_Ba$residuals)


## Cadmium

R1CadmiumData <- subset(R1Data, Element=="Cadmium")
plot(log10(R1CadmiumData$ConcentrationPPM) ~ R1CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1CadmiumData$ConcentrationPPM) ~ R1CadmiumData$d15N), col="blue")

R1TMS_Cd <- lm(log10(R1CadmiumData$ConcentrationPPM) ~ R1CadmiumData$d15N)
summary(R1TMS_Cd)
plot(R1TMS_Cd$residuals)


## Cobalt

R1CobaltData <- subset(R1Data, Element=="Cobalt")
plot(log10(R1CobaltData$ConcentrationPPM) ~ R1CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1CobaltData$ConcentrationPPM) ~ R1CobaltData$d15N), col="blue")

R1TMS_Co <- lm(log10(R1CobaltData$ConcentrationPPM) ~ R1CobaltData$d15N)
summary(R1TMS_Co)
plot(R1TMS_Co$residuals)


## Copper

R1CopperData <- subset(R1Data, Element=="Copper")
plot(log10(R1CopperData$ConcentrationPPM) ~ R1CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1CopperData$ConcentrationPPM) ~ R1CopperData$d15N), col="blue")

R1TMS_Cu <- lm(log10(R1CopperData$ConcentrationPPM) ~ R1CopperData$d15N)
summary(R1TMS_Cu)
plot(R1TMS_Cu$residuals)


## Iron

R1IronData <- subset(R1Data, Element=="Iron")
plot(log10(R1IronData$ConcentrationPPM) ~ R1IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1IronData$ConcentrationPPM) ~ R1IronData$d15N), col="blue")

R1TMS_Fe <- lm(log10(R1IronData$ConcentrationPPM) ~ R1IronData$d15N)
summary(R1TMS_Fe)
plot(R1TMS_Fe$residuals)


## Lead

R1LeadData <- subset(R1Data, Element=="Lead")
plot(log10(R1LeadData$ConcentrationPPM) ~ R1LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1LeadData$ConcentrationPPM) ~ R1LeadData$d15N), col="blue")

R1TMS_Pb <- lm(log10(R1LeadData$ConcentrationPPM) ~ R1LeadData$d15N)
summary(R1TMS_Pb)
plot(R1TMS_Pb$residuals)


## Manganese

R1ManganeseData <- subset(R1Data, Element=="Manganese")
plot(log10(R1ManganeseData$ConcentrationPPM) ~ R1ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1ManganeseData$ConcentrationPPM) ~ R1ManganeseData$d15N), col="blue")

R1TMS_Mn <- lm(log10(R1ManganeseData$ConcentrationPPM) ~ R1ManganeseData$d15N)
summary(R1TMS_Mn)
plot(R1TMS_Mn$residuals)


## Mercury

R1MercuryData <- subset(R1Data, Element=="Mercury")
plot(log10(R1MercuryData$ConcentrationPPM) ~ R1MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1MercuryData$ConcentrationPPM) ~ R1MercuryData$d15N), col="blue")

R1TMS_Hg <- lm(log10(R1MercuryData$ConcentrationPPM) ~ R1MercuryData$d15N)
summary(R1TMS_Hg)
plot(R1TMS_Hg$residuals)


## Molybdenum

R1MolybdenumData <- subset(R1Data, Element=="Molybdenum")
plot(log10(R1MolybdenumData$ConcentrationPPM) ~ R1MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1MolybdenumData$ConcentrationPPM) ~ R1MolybdenumData$d15N), col="blue")

R1TMS_Mo <- lm(log10(R1MolybdenumData$ConcentrationPPM) ~ R1MolybdenumData$d15N)
summary(R1TMS_Mo)
plot(R1TMS_Mo$residuals)


## Nickel

R1NickelData <- subset(R1Data, Element=="Nickel")
plot(log10(R1NickelData$ConcentrationPPM) ~ R1NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1NickelData$ConcentrationPPM) ~ R1NickelData$d15N), col="blue")

R1TMS_Ni <- lm(log10(R1NickelData$ConcentrationPPM) ~ R1NickelData$d15N)
summary(R1TMS_Ni)
plot(R1TMS_Ni$residuals)


## Selenium

R1SeleniumData <- subset(R1Data, Element=="Selenium")
plot(log10(R1SeleniumData$ConcentrationPPM) ~ R1SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1SeleniumData$ConcentrationPPM) ~ R1SeleniumData$d15N), col="blue")

R1TMS_Se <- lm(log10(R1SeleniumData$ConcentrationPPM) ~ R1SeleniumData$d15N)
summary(R1TMS_Se)
plot(R1TMS_Se$residuals)


## Strontium

R1StrontiumData <- subset(R1Data, Element=="Strontium")
plot(log10(R1StrontiumData$ConcentrationPPM) ~ R1StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1StrontiumData$ConcentrationPPM) ~ R1StrontiumData$d15N), col="blue")

R1TMS_Sr <- lm(log10(R1StrontiumData$ConcentrationPPM) ~ R1StrontiumData$d15N)
summary(R1TMS_Sr)
plot(R1TMS_Sr$residuals)


## Thallium

R1ThalliumData <- subset(R1Data, Element=="Thallium")
plot(log10(R1ThalliumData$ConcentrationPPM) ~ R1ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1ThalliumData$ConcentrationPPM) ~ R1ThalliumData$d15N), col="blue")

R1TMS_Tl <- lm(log10(R1ThalliumData$ConcentrationPPM) ~ R1ThalliumData$d15N)
summary(R1TMS_Tl)
plot(R1TMS_Tl$residuals)


## Titanium

R1TitaniumData <- subset(R1Data, Element=="Titanium")
plot(log10(R1TitaniumData$ConcentrationPPM) ~ R1TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1TitaniumData$ConcentrationPPM) ~ R1TitaniumData$d15N), col="blue")

R1TMS_Ti <- lm(log10(R1TitaniumData$ConcentrationPPM) ~ R1TitaniumData$d15N)
summary(R1TMS_Ti)
plot(R1TMS_Ti$residuals)


## Uranium

R1UraniumData <- subset(R1Data, Element=="Uranium")
plot(log10(R1UraniumData$ConcentrationPPM) ~ R1UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1UraniumData$ConcentrationPPM) ~ R1UraniumData$d15N), col="blue")

R1TMS_U <- lm(log10(R1UraniumData$ConcentrationPPM) ~ R1UraniumData$d15N)
summary(R1TMS_U)
plot(R1TMS_U$residuals)


## Vanadium

R1VanadiumData <- subset(R1Data, Element=="Vanadium")
plot(log10(R1VanadiumData$ConcentrationPPM) ~ R1VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1VanadiumData$ConcentrationPPM) ~ R1VanadiumData$d15N), col="blue")

R1TMS_V <- lm(log10(R1VanadiumData$ConcentrationPPM) ~ R1VanadiumData$d15N)
summary(R1TMS_V)
plot(R1TMS_V$residuals)


## Zinc

R1ZincData <- subset(R1Data, Element=="Zinc")
plot(log10(R1ZincData$ConcentrationPPM) ~ R1ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in R1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R1ZincData$ConcentrationPPM) ~ R1ZincData$d15N), col="blue")

R1TMS_Zn <- lm(log10(R1ZincData$ConcentrationPPM) ~ R1ZincData$d15N)
summary(R1TMS_Zn)
plot(R1TMS_Zn$residuals)



###### R2 (Rio Pashimbi Grande)


## Aluminum

R2AluminumData <- subset(R2Data, Element=="Aluminum")
plot(log10(R2AluminumData$ConcentrationPPM) ~ R2AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2AluminumData$ConcentrationPPM) ~ R2AluminumData$d15N), col="blue")

R2TMS_Al <- lm(log10(R2AluminumData$ConcentrationPPM) ~ R2AluminumData$d15N)
summary(R2TMS_Al)
plot(R2TMS_Al$residuals)


## Arsenic

R2ArsenicData <- subset(R2Data, Element=="Arsenic")
plot(log10(R2ArsenicData$ConcentrationPPM) ~ R2ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2ArsenicData$ConcentrationPPM) ~ R2ArsenicData$d15N), col="blue")

R2TMS_As <- lm(log10(R2ArsenicData$ConcentrationPPM) ~ R2ArsenicData$d15N)
summary(R2TMS_As)
plot(R2TMS_As$residuals)


## Barium

R2BariumData <- subset(R2Data, Element=="Barium")
plot(log10(R2BariumData$ConcentrationPPM) ~ R2BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2BariumData$ConcentrationPPM) ~ R2BariumData$d15N), col="blue")

R2TMS_Ba <- lm(log10(R2BariumData$ConcentrationPPM) ~ R2BariumData$d15N)
summary(R2TMS_Ba)
plot(R2TMS_Ba$residuals)


## Cadmium

R2CadmiumData <- subset(R2Data, Element=="Cadmium")
plot(log10(R2CadmiumData$ConcentrationPPM) ~ R2CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2CadmiumData$ConcentrationPPM) ~ R2CadmiumData$d15N), col="blue")

R2TMS_Cd <- lm(log10(R2CadmiumData$ConcentrationPPM) ~ R2CadmiumData$d15N)
summary(R2TMS_Cd)
plot(R2TMS_Cd$residuals)


## Cobalt

R2CobaltData <- subset(R2Data, Element=="Cobalt")
plot(log10(R2CobaltData$ConcentrationPPM) ~ R2CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2CobaltData$ConcentrationPPM) ~ R2CobaltData$d15N), col="blue")

R2TMS_Co <- lm(log10(R2CobaltData$ConcentrationPPM) ~ R2CobaltData$d15N)
summary(R2TMS_Co)
plot(R2TMS_Co$residuals)


## Copper

R2CopperData <- subset(R2Data, Element=="Copper")
plot(log10(R2CopperData$ConcentrationPPM) ~ R2CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2CopperData$ConcentrationPPM) ~ R2CopperData$d15N), col="blue")

R2TMS_Cu <- lm(log10(R2CopperData$ConcentrationPPM) ~ R2CopperData$d15N)
summary(R2TMS_Cu)
plot(R2TMS_Cu$residuals)


## Iron

R2IronData <- subset(R2Data, Element=="Iron")
plot(log10(R2IronData$ConcentrationPPM) ~ R2IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2IronData$ConcentrationPPM) ~ R2IronData$d15N), col="blue")

R2TMS_Fe <- lm(log10(R2IronData$ConcentrationPPM) ~ R2IronData$d15N)
summary(R2TMS_Fe)
plot(R2TMS_Fe$residuals)


## Lead

R2LeadData <- subset(R2Data, Element=="Lead")
plot(log10(R2LeadData$ConcentrationPPM) ~ R2LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2LeadData$ConcentrationPPM) ~ R2LeadData$d15N), col="blue")

R2TMS_Pb <- lm(log10(R2LeadData$ConcentrationPPM) ~ R2LeadData$d15N)
summary(R2TMS_Pb)
plot(R2TMS_Pb$residuals)


## Manganese

R2ManganeseData <- subset(R2Data, Element=="Manganese")
plot(log10(R2ManganeseData$ConcentrationPPM) ~ R2ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2ManganeseData$ConcentrationPPM) ~ R2ManganeseData$d15N), col="blue")

R2TMS_Mn <- lm(log10(R2ManganeseData$ConcentrationPPM) ~ R2ManganeseData$d15N)
summary(R2TMS_Mn)
plot(R2TMS_Mn$residuals)


## Mercury

R2MercuryData <- subset(R2Data, Element=="Mercury")
plot(log10(R2MercuryData$ConcentrationPPM) ~ R2MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2MercuryData$ConcentrationPPM) ~ R2MercuryData$d15N), col="blue")

R2TMS_Hg <- lm(log10(R2MercuryData$ConcentrationPPM) ~ R2MercuryData$d15N)
summary(R2TMS_Hg)
plot(R2TMS_Hg$residuals)


## Molybdenum

R2MolybdenumData <- subset(R2Data, Element=="Molybdenum")
plot(log10(R2MolybdenumData$ConcentrationPPM) ~ R2MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2MolybdenumData$ConcentrationPPM) ~ R2MolybdenumData$d15N), col="blue")

R2TMS_Mo <- lm(log10(R2MolybdenumData$ConcentrationPPM) ~ R2MolybdenumData$d15N)
summary(R2TMS_Mo)
plot(R2TMS_Mo$residuals)


## Nickel

R2NickelData <- subset(R2Data, Element=="Nickel")
plot(log10(R2NickelData$ConcentrationPPM) ~ R2NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2NickelData$ConcentrationPPM) ~ R2NickelData$d15N), col="blue")

R2TMS_Ni <- lm(log10(R2NickelData$ConcentrationPPM) ~ R2NickelData$d15N)
summary(R2TMS_Ni)
plot(R2TMS_Ni$residuals)


## Selenium

R2SeleniumData <- subset(R2Data, Element=="Selenium")
plot(log10(R2SeleniumData$ConcentrationPPM) ~ R2SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2SeleniumData$ConcentrationPPM) ~ R2SeleniumData$d15N), col="blue")

R2TMS_Se <- lm(log10(R2SeleniumData$ConcentrationPPM) ~ R2SeleniumData$d15N)
summary(R2TMS_Se)
plot(R2TMS_Se$residuals)


## Strontium

R2StrontiumData <- subset(R2Data, Element=="Strontium")
plot(log10(R2StrontiumData$ConcentrationPPM) ~ R2StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2StrontiumData$ConcentrationPPM) ~ R2StrontiumData$d15N), col="blue")

R2TMS_Sr <- lm(log10(R2StrontiumData$ConcentrationPPM) ~ R2StrontiumData$d15N)
summary(R2TMS_Sr)
plot(R2TMS_Sr$residuals)


## Thallium

R2ThalliumData <- subset(R2Data, Element=="Thallium")
plot(log10(R2ThalliumData$ConcentrationPPM) ~ R2ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2ThalliumData$ConcentrationPPM) ~ R2ThalliumData$d15N), col="blue")

R2TMS_Tl <- lm(log10(R2ThalliumData$ConcentrationPPM) ~ R2ThalliumData$d15N)
summary(R2TMS_Tl)
plot(R2TMS_Tl$residuals)


## Titanium

R2TitaniumData <- subset(R2Data, Element=="Titanium")
plot(log10(R2TitaniumData$ConcentrationPPM) ~ R2TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2TitaniumData$ConcentrationPPM) ~ R2TitaniumData$d15N), col="blue")

R2TMS_Ti <- lm(log10(R2TitaniumData$ConcentrationPPM) ~ R2TitaniumData$d15N)
summary(R2TMS_Ti)
plot(R2TMS_Ti$residuals)


## Uranium

R2UraniumData <- subset(R2Data, Element=="Uranium")
plot(log10(R2UraniumData$ConcentrationPPM) ~ R2UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2UraniumData$ConcentrationPPM) ~ R2UraniumData$d15N), col="blue")

R2TMS_U <- lm(log10(R2UraniumData$ConcentrationPPM) ~ R2UraniumData$d15N)
summary(R2TMS_U)
plot(R2TMS_U$residuals)


## Vanadium

R2VanadiumData <- subset(R2Data, Element=="Vanadium")
plot(log10(R2VanadiumData$ConcentrationPPM) ~ R2VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2VanadiumData$ConcentrationPPM) ~ R2VanadiumData$d15N), col="blue")

R2TMS_V <- lm(log10(R2VanadiumData$ConcentrationPPM) ~ R2VanadiumData$d15N)
summary(R2TMS_V)
plot(R2TMS_V$residuals)


## Zinc

R2ZincData <- subset(R2Data, Element=="Zinc")
plot(log10(R2ZincData$ConcentrationPPM) ~ R2ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in R2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R2ZincData$ConcentrationPPM) ~ R2ZincData$d15N), col="blue")

R2TMS_Zn <- lm(log10(R2ZincData$ConcentrationPPM) ~ R2ZincData$d15N)
summary(R2TMS_Zn)
plot(R2TMS_Zn$residuals)



###### R3 (Rio Colonsito)


## Aluminum

R3AluminumData <- subset(R3Data, Element=="Aluminum")
plot(log10(R3AluminumData$ConcentrationPPM) ~ R3AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3AluminumData$ConcentrationPPM) ~ R3AluminumData$d15N), col="blue")

R3TMS_Al <- lm(log10(R3AluminumData$ConcentrationPPM) ~ R3AluminumData$d15N)
summary(R3TMS_Al)
plot(R3TMS_Al$residuals)


## Arsenic

R3ArsenicData <- subset(R3Data, Element=="Arsenic")
plot(log10(R3ArsenicData$ConcentrationPPM) ~ R3ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3ArsenicData$ConcentrationPPM) ~ R3ArsenicData$d15N), col="blue")

R3TMS_As <- lm(log10(R3ArsenicData$ConcentrationPPM) ~ R3ArsenicData$d15N)
summary(R3TMS_As)
plot(R3TMS_As$residuals)


## Barium

R3BariumData <- subset(R3Data, Element=="Barium")
plot(log10(R3BariumData$ConcentrationPPM) ~ R3BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3BariumData$ConcentrationPPM) ~ R3BariumData$d15N), col="blue")

R3TMS_Ba <- lm(log10(R3BariumData$ConcentrationPPM) ~ R3BariumData$d15N)
summary(R3TMS_Ba)
plot(R3TMS_Ba$residuals)


## Cadmium

R3CadmiumData <- subset(R3Data, Element=="Cadmium")
plot(log10(R3CadmiumData$ConcentrationPPM) ~ R3CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3CadmiumData$ConcentrationPPM) ~ R3CadmiumData$d15N), col="blue")

R3TMS_Cd <- lm(log10(R3CadmiumData$ConcentrationPPM) ~ R3CadmiumData$d15N)
summary(R3TMS_Cd)
plot(R3TMS_Cd$residuals)


## Cobalt

R3CobaltData <- subset(R3Data, Element=="Cobalt")
plot(log10(R3CobaltData$ConcentrationPPM) ~ R3CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3CobaltData$ConcentrationPPM) ~ R3CobaltData$d15N), col="blue")

R3TMS_Co <- lm(log10(R3CobaltData$ConcentrationPPM) ~ R3CobaltData$d15N)
summary(R3TMS_Co)
plot(R3TMS_Co$residuals)


## Copper

R3CopperData <- subset(R3Data, Element=="Copper")
plot(log10(R3CopperData$ConcentrationPPM) ~ R3CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3CopperData$ConcentrationPPM) ~ R3CopperData$d15N), col="blue")

R3TMS_Cu <- lm(log10(R3CopperData$ConcentrationPPM) ~ R3CopperData$d15N)
summary(R3TMS_Cu)
plot(R3TMS_Cu$residuals)


## Iron

R3IronData <- subset(R3Data, Element=="Iron")
plot(log10(R3IronData$ConcentrationPPM) ~ R3IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3IronData$ConcentrationPPM) ~ R3IronData$d15N), col="blue")

R3TMS_Fe <- lm(log10(R3IronData$ConcentrationPPM) ~ R3IronData$d15N)
summary(R3TMS_Fe)
plot(R3TMS_Fe$residuals)


## Lead

R3LeadData <- subset(R3Data, Element=="Lead")
plot(log10(R3LeadData$ConcentrationPPM) ~ R3LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3LeadData$ConcentrationPPM) ~ R3LeadData$d15N), col="blue")

R3TMS_Pb <- lm(log10(R3LeadData$ConcentrationPPM) ~ R3LeadData$d15N)
summary(R3TMS_Pb)
plot(R3TMS_Pb$residuals)


## Manganese

R3ManganeseData <- subset(R3Data, Element=="Manganese")
plot(log10(R3ManganeseData$ConcentrationPPM) ~ R3ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3ManganeseData$ConcentrationPPM) ~ R3ManganeseData$d15N), col="blue")

R3TMS_Mn <- lm(log10(R3ManganeseData$ConcentrationPPM) ~ R3ManganeseData$d15N)
summary(R3TMS_Mn)
plot(R3TMS_Mn$residuals)


## Mercury

R3MercuryData <- subset(R3Data, Element=="Mercury")
plot(log10(R3MercuryData$ConcentrationPPM) ~ R3MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3MercuryData$ConcentrationPPM) ~ R3MercuryData$d15N), col="blue")

R3TMS_Hg <- lm(log10(R3MercuryData$ConcentrationPPM) ~ R3MercuryData$d15N)
summary(R3TMS_Hg)
plot(R3TMS_Hg$residuals)


## Molybdenum

R3MolybdenumData <- subset(R3Data, Element=="Molybdenum")
plot(log10(R3MolybdenumData$ConcentrationPPM) ~ R3MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3MolybdenumData$ConcentrationPPM) ~ R3MolybdenumData$d15N), col="blue")

R3TMS_Mo <- lm(log10(R3MolybdenumData$ConcentrationPPM) ~ R3MolybdenumData$d15N)
summary(R3TMS_Mo)
plot(R3TMS_Mo$residuals)


## Nickel

R3NickelData <- subset(R3Data, Element=="Nickel")
plot(log10(R3NickelData$ConcentrationPPM) ~ R3NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3NickelData$ConcentrationPPM) ~ R3NickelData$d15N), col="blue")

R3TMS_Ni <- lm(log10(R3NickelData$ConcentrationPPM) ~ R3NickelData$d15N)
summary(R3TMS_Ni)
plot(R3TMS_Ni$residuals)


## Selenium

R3SeleniumData <- subset(R3Data, Element=="Selenium")
plot(log10(R3SeleniumData$ConcentrationPPM) ~ R3SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3SeleniumData$ConcentrationPPM) ~ R3SeleniumData$d15N), col="blue")

R3TMS_Se <- lm(log10(R3SeleniumData$ConcentrationPPM) ~ R3SeleniumData$d15N)
summary(R3TMS_Se)
plot(R3TMS_Se$residuals)


## Strontium

R3StrontiumData <- subset(R3Data, Element=="Strontium")
plot(log10(R3StrontiumData$ConcentrationPPM) ~ R3StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3StrontiumData$ConcentrationPPM) ~ R3StrontiumData$d15N), col="blue")

R3TMS_Sr <- lm(log10(R3StrontiumData$ConcentrationPPM) ~ R3StrontiumData$d15N)
summary(R3TMS_Sr)
plot(R3TMS_Sr$residuals)


## Thallium

R3ThalliumData <- subset(R3Data, Element=="Thallium")
plot(log10(R3ThalliumData$ConcentrationPPM) ~ R3ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3ThalliumData$ConcentrationPPM) ~ R3ThalliumData$d15N), col="blue")

R3TMS_Tl <- lm(log10(R3ThalliumData$ConcentrationPPM) ~ R3ThalliumData$d15N)
summary(R3TMS_Tl)
plot(R3TMS_Tl$residuals)


## Titanium

R3TitaniumData <- subset(R3Data, Element=="Titanium")
plot(log10(R3TitaniumData$ConcentrationPPM) ~ R3TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3TitaniumData$ConcentrationPPM) ~ R3TitaniumData$d15N), col="blue")

R3TMS_Ti <- lm(log10(R3TitaniumData$ConcentrationPPM) ~ R3TitaniumData$d15N)
summary(R3TMS_Ti)
plot(R3TMS_Ti$residuals)


## Uranium

R3UraniumData <- subset(R3Data, Element=="Uranium")
plot(log10(R3UraniumData$ConcentrationPPM) ~ R3UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3UraniumData$ConcentrationPPM) ~ R3UraniumData$d15N), col="blue")

R3TMS_U <- lm(log10(R3UraniumData$ConcentrationPPM) ~ R3UraniumData$d15N)
summary(R3TMS_U)
plot(R3TMS_U$residuals)


## Vanadium

R3VanadiumData <- subset(R3Data, Element=="Vanadium")
plot(log10(R3VanadiumData$ConcentrationPPM) ~ R3VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3VanadiumData$ConcentrationPPM) ~ R3VanadiumData$d15N), col="blue")

R3TMS_V <- lm(log10(R3VanadiumData$ConcentrationPPM) ~ R3VanadiumData$d15N)
summary(R3TMS_V)
plot(R3TMS_V$residuals)


## Zinc

R3ZincData <- subset(R3Data, Element=="Zinc")
plot(log10(R3ZincData$ConcentrationPPM) ~ R3ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in R3")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R3ZincData$ConcentrationPPM) ~ R3ZincData$d15N), col="blue")

R3TMS_Zn <- lm(log10(R3ZincData$ConcentrationPPM) ~ R3ZincData$d15N)
summary(R3TMS_Zn)
plot(R3TMS_Zn$residuals)



###### R4 (Unnamed Reference Tributary of Rio Yutzupino)


## Aluminum

R4AluminumData <- subset(R4Data, Element=="Aluminum")
plot(log10(R4AluminumData$ConcentrationPPM) ~ R4AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4AluminumData$ConcentrationPPM) ~ R4AluminumData$d15N), col="blue")

R4TMS_Al <- lm(log10(R4AluminumData$ConcentrationPPM) ~ R4AluminumData$d15N)
summary(R4TMS_Al)
plot(R4TMS_Al$residuals)


## Arsenic

R4ArsenicData <- subset(R4Data, Element=="Arsenic")
plot(log10(R4ArsenicData$ConcentrationPPM) ~ R4ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4ArsenicData$ConcentrationPPM) ~ R4ArsenicData$d15N), col="blue")

R4TMS_As <- lm(log10(R4ArsenicData$ConcentrationPPM) ~ R4ArsenicData$d15N)
summary(R4TMS_As)
plot(R4TMS_As$residuals)


## Barium

R4BariumData <- subset(R4Data, Element=="Barium")
plot(log10(R4BariumData$ConcentrationPPM) ~ R4BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4BariumData$ConcentrationPPM) ~ R4BariumData$d15N), col="blue")

R4TMS_Ba <- lm(log10(R4BariumData$ConcentrationPPM) ~ R4BariumData$d15N)
summary(R4TMS_Ba)
plot(R4TMS_Ba$residuals)


## Cadmium

R4CadmiumData <- subset(R4Data, Element=="Cadmium")
plot(log10(R4CadmiumData$ConcentrationPPM) ~ R4CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4CadmiumData$ConcentrationPPM) ~ R4CadmiumData$d15N), col="blue")

R4TMS_Cd <- lm(log10(R4CadmiumData$ConcentrationPPM) ~ R4CadmiumData$d15N)
summary(R4TMS_Cd)
plot(R4TMS_Cd$residuals)


## Cobalt

R4CobaltData <- subset(R4Data, Element=="Cobalt")
plot(log10(R4CobaltData$ConcentrationPPM) ~ R4CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4CobaltData$ConcentrationPPM) ~ R4CobaltData$d15N), col="blue")

R4TMS_Co <- lm(log10(R4CobaltData$ConcentrationPPM) ~ R4CobaltData$d15N)
summary(R4TMS_Co)
plot(R4TMS_Co$residuals)


## Copper

R4CopperData <- subset(R4Data, Element=="Copper")
plot(log10(R4CopperData$ConcentrationPPM) ~ R4CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4CopperData$ConcentrationPPM) ~ R4CopperData$d15N), col="blue")

R4TMS_Cu <- lm(log10(R4CopperData$ConcentrationPPM) ~ R4CopperData$d15N)
summary(R4TMS_Cu)
plot(R4TMS_Cu$residuals)


## Iron

R4IronData <- subset(R4Data, Element=="Iron")
plot(log10(R4IronData$ConcentrationPPM) ~ R4IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4IronData$ConcentrationPPM) ~ R4IronData$d15N), col="blue")

R4TMS_Fe <- lm(log10(R4IronData$ConcentrationPPM) ~ R4IronData$d15N)
summary(R4TMS_Fe)
plot(R4TMS_Fe$residuals)


## Lead

R4LeadData <- subset(R4Data, Element=="Lead")
plot(log10(R4LeadData$ConcentrationPPM) ~ R4LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4LeadData$ConcentrationPPM) ~ R4LeadData$d15N), col="blue")

R4TMS_Pb <- lm(log10(R4LeadData$ConcentrationPPM) ~ R4LeadData$d15N)
summary(R4TMS_Pb)
plot(R4TMS_Pb$residuals)


## Manganese

R4ManganeseData <- subset(R4Data, Element=="Manganese")
plot(log10(R4ManganeseData$ConcentrationPPM) ~ R4ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4ManganeseData$ConcentrationPPM) ~ R4ManganeseData$d15N), col="blue")

R4TMS_Mn <- lm(log10(R4ManganeseData$ConcentrationPPM) ~ R4ManganeseData$d15N)
summary(R4TMS_Mn)
plot(R4TMS_Mn$residuals)


## Mercury

R4MercuryData <- subset(R4Data, Element=="Mercury")
plot(log10(R4MercuryData$ConcentrationPPM) ~ R4MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4MercuryData$ConcentrationPPM) ~ R4MercuryData$d15N), col="blue")

R4TMS_Hg <- lm(log10(R4MercuryData$ConcentrationPPM) ~ R4MercuryData$d15N)
summary(R4TMS_Hg)
plot(R4TMS_Hg$residuals)


## Molybdenum

R4MolybdenumData <- subset(R4Data, Element=="Molybdenum")
plot(log10(R4MolybdenumData$ConcentrationPPM) ~ R4MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4MolybdenumData$ConcentrationPPM) ~ R4MolybdenumData$d15N), col="blue")

R4TMS_Mo <- lm(log10(R4MolybdenumData$ConcentrationPPM) ~ R4MolybdenumData$d15N)
summary(R4TMS_Mo)
plot(R4TMS_Mo$residuals)


## Nickel

R4NickelData <- subset(R4Data, Element=="Nickel")
plot(log10(R4NickelData$ConcentrationPPM) ~ R4NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4NickelData$ConcentrationPPM) ~ R4NickelData$d15N), col="blue")

R4TMS_Ni <- lm(log10(R4NickelData$ConcentrationPPM) ~ R4NickelData$d15N)
summary(R4TMS_Ni)
plot(R4TMS_Ni$residuals)


## Selenium

R4SeleniumData <- subset(R4Data, Element=="Selenium")
plot(log10(R4SeleniumData$ConcentrationPPM) ~ R4SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4SeleniumData$ConcentrationPPM) ~ R4SeleniumData$d15N), col="blue")

R4TMS_Se <- lm(log10(R4SeleniumData$ConcentrationPPM) ~ R4SeleniumData$d15N)
summary(R4TMS_Se)
plot(R4TMS_Se$residuals)


## Strontium

R4StrontiumData <- subset(R4Data, Element=="Strontium")
plot(log10(R4StrontiumData$ConcentrationPPM) ~ R4StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4StrontiumData$ConcentrationPPM) ~ R4StrontiumData$d15N), col="blue")

R4TMS_Sr <- lm(log10(R4StrontiumData$ConcentrationPPM) ~ R4StrontiumData$d15N)
summary(R4TMS_Sr)
plot(R4TMS_Sr$residuals)


## Thallium

R4ThalliumData <- subset(R4Data, Element=="Thallium")
plot(log10(R4ThalliumData$ConcentrationPPM) ~ R4ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4ThalliumData$ConcentrationPPM) ~ R4ThalliumData$d15N), col="blue")

R4TMS_Tl <- lm(log10(R4ThalliumData$ConcentrationPPM) ~ R4ThalliumData$d15N)
summary(R4TMS_Tl)
plot(R4TMS_Tl$residuals)


## Titanium

R4TitaniumData <- subset(R4Data, Element=="Titanium")
plot(log10(R4TitaniumData$ConcentrationPPM) ~ R4TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4TitaniumData$ConcentrationPPM) ~ R4TitaniumData$d15N), col="blue")

R4TMS_Ti <- lm(log10(R4TitaniumData$ConcentrationPPM) ~ R4TitaniumData$d15N)
summary(R4TMS_Ti)
plot(R4TMS_Ti$residuals)


## Uranium

R4UraniumData <- subset(R4Data, Element=="Uranium")
plot(log10(R4UraniumData$ConcentrationPPM) ~ R4UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4UraniumData$ConcentrationPPM) ~ R4UraniumData$d15N), col="blue")

R4TMS_U <- lm(log10(R4UraniumData$ConcentrationPPM) ~ R4UraniumData$d15N)
summary(R4TMS_U)
plot(R4TMS_U$residuals)


## Vanadium

R4VanadiumData <- subset(R4Data, Element=="Vanadium")
plot(log10(R4VanadiumData$ConcentrationPPM) ~ R4VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4VanadiumData$ConcentrationPPM) ~ R4VanadiumData$d15N), col="blue")

R4TMS_V <- lm(log10(R4VanadiumData$ConcentrationPPM) ~ R4VanadiumData$d15N)
summary(R4TMS_V)
plot(R4TMS_V$residuals)


## Zinc

R4ZincData <- subset(R4Data, Element=="Zinc")
plot(log10(R4ZincData$ConcentrationPPM) ~ R4ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in R4")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R4ZincData$ConcentrationPPM) ~ R4ZincData$d15N), col="blue")

R4TMS_Zn <- lm(log10(R4ZincData$ConcentrationPPM) ~ R4ZincData$d15N)
summary(R4TMS_Zn)
plot(R4TMS_Zn$residuals)



###### R5 (Rio Vuano)


## Aluminum

R5AluminumData <- subset(R5Data, Element=="Aluminum")
plot(log10(R5AluminumData$ConcentrationPPM) ~ R5AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5AluminumData$ConcentrationPPM) ~ R5AluminumData$d15N), col="blue")

R5TMS_Al <- lm(log10(R5AluminumData$ConcentrationPPM) ~ R5AluminumData$d15N)
summary(R5TMS_Al)
plot(R5TMS_Al$residuals)


## Arsenic

R5ArsenicData <- subset(R5Data, Element=="Arsenic")
plot(log10(R5ArsenicData$ConcentrationPPM) ~ R5ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5ArsenicData$ConcentrationPPM) ~ R5ArsenicData$d15N), col="blue")

R5TMS_As <- lm(log10(R5ArsenicData$ConcentrationPPM) ~ R5ArsenicData$d15N)
summary(R5TMS_As)
plot(R5TMS_As$residuals)


## Barium

R5BariumData <- subset(R5Data, Element=="Barium")
plot(log10(R5BariumData$ConcentrationPPM) ~ R5BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5BariumData$ConcentrationPPM) ~ R5BariumData$d15N), col="blue")

R5TMS_Ba <- lm(log10(R5BariumData$ConcentrationPPM) ~ R5BariumData$d15N)
summary(R5TMS_Ba)
plot(R5TMS_Ba$residuals)


## Cadmium

R5CadmiumData <- subset(R5Data, Element=="Cadmium")
plot(log10(R5CadmiumData$ConcentrationPPM) ~ R5CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5CadmiumData$ConcentrationPPM) ~ R5CadmiumData$d15N), col="blue")

R5TMS_Cd <- lm(log10(R5CadmiumData$ConcentrationPPM) ~ R5CadmiumData$d15N)
summary(R5TMS_Cd)
plot(R5TMS_Cd$residuals)


## Cobalt

R5CobaltData <- subset(R5Data, Element=="Cobalt")
plot(log10(R5CobaltData$ConcentrationPPM) ~ R5CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5CobaltData$ConcentrationPPM) ~ R5CobaltData$d15N), col="blue")

R5TMS_Co <- lm(log10(R5CobaltData$ConcentrationPPM) ~ R5CobaltData$d15N)
summary(R5TMS_Co)
plot(R5TMS_Co$residuals)


## Copper

R5CopperData <- subset(R5Data, Element=="Copper")
plot(log10(R5CopperData$ConcentrationPPM) ~ R5CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5CopperData$ConcentrationPPM) ~ R5CopperData$d15N), col="blue")

R5TMS_Cu <- lm(log10(R5CopperData$ConcentrationPPM) ~ R5CopperData$d15N)
summary(R5TMS_Cu)
plot(R5TMS_Cu$residuals)


## Iron

R5IronData <- subset(R5Data, Element=="Iron")
plot(log10(R5IronData$ConcentrationPPM) ~ R5IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5IronData$ConcentrationPPM) ~ R5IronData$d15N), col="blue")

R5TMS_Fe <- lm(log10(R5IronData$ConcentrationPPM) ~ R5IronData$d15N)
summary(R5TMS_Fe)
plot(R5TMS_Fe$residuals)


## Lead

R5LeadData <- subset(R5Data, Element=="Lead")
plot(log10(R5LeadData$ConcentrationPPM) ~ R5LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5LeadData$ConcentrationPPM) ~ R5LeadData$d15N), col="blue")

R5TMS_Pb <- lm(log10(R5LeadData$ConcentrationPPM) ~ R5LeadData$d15N)
summary(R5TMS_Pb)
plot(R5TMS_Pb$residuals)


## Manganese

R5ManganeseData <- subset(R5Data, Element=="Manganese")
plot(log10(R5ManganeseData$ConcentrationPPM) ~ R5ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5ManganeseData$ConcentrationPPM) ~ R5ManganeseData$d15N), col="blue")

R5TMS_Mn <- lm(log10(R5ManganeseData$ConcentrationPPM) ~ R5ManganeseData$d15N)
summary(R5TMS_Mn)
plot(R5TMS_Mn$residuals)


## Mercury

R5MercuryData <- subset(R5Data, Element=="Mercury")
plot(log10(R5MercuryData$ConcentrationPPM) ~ R5MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5MercuryData$ConcentrationPPM) ~ R5MercuryData$d15N), col="blue")

R5TMS_Hg <- lm(log10(R5MercuryData$ConcentrationPPM) ~ R5MercuryData$d15N)
summary(R5TMS_Hg)
plot(R5TMS_Hg$residuals)


## Molybdenum

R5MolybdenumData <- subset(R5Data, Element=="Molybdenum")
plot(log10(R5MolybdenumData$ConcentrationPPM) ~ R5MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5MolybdenumData$ConcentrationPPM) ~ R5MolybdenumData$d15N), col="blue")

R5TMS_Mo <- lm(log10(R5MolybdenumData$ConcentrationPPM) ~ R5MolybdenumData$d15N)
summary(R5TMS_Mo)
plot(R5TMS_Mo$residuals)


## Nickel

R5NickelData <- subset(R5Data, Element=="Nickel")
plot(log10(R5NickelData$ConcentrationPPM) ~ R5NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5NickelData$ConcentrationPPM) ~ R5NickelData$d15N), col="blue")

R5TMS_Ni <- lm(log10(R5NickelData$ConcentrationPPM) ~ R5NickelData$d15N)
summary(R5TMS_Ni)
plot(R5TMS_Ni$residuals)


## Selenium

R5SeleniumData <- subset(R5Data, Element=="Selenium")
plot(log10(R5SeleniumData$ConcentrationPPM) ~ R5SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5SeleniumData$ConcentrationPPM) ~ R5SeleniumData$d15N), col="blue")

R5TMS_Se <- lm(log10(R5SeleniumData$ConcentrationPPM) ~ R5SeleniumData$d15N)
summary(R5TMS_Se)
plot(R5TMS_Se$residuals)


## Strontium

R5StrontiumData <- subset(R5Data, Element=="Strontium")
plot(log10(R5StrontiumData$ConcentrationPPM) ~ R5StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5StrontiumData$ConcentrationPPM) ~ R5StrontiumData$d15N), col="blue")

R5TMS_Sr <- lm(log10(R5StrontiumData$ConcentrationPPM) ~ R5StrontiumData$d15N)
summary(R5TMS_Sr)
plot(R5TMS_Sr$residuals)


## Thallium

R5ThalliumData <- subset(R5Data, Element=="Thallium")
plot(log10(R5ThalliumData$ConcentrationPPM) ~ R5ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5ThalliumData$ConcentrationPPM) ~ R5ThalliumData$d15N), col="blue")

R5TMS_Tl <- lm(log10(R5ThalliumData$ConcentrationPPM) ~ R5ThalliumData$d15N)
summary(R5TMS_Tl)
plot(R5TMS_Tl$residuals)


## Titanium

R5TitaniumData <- subset(R5Data, Element=="Titanium")
plot(log10(R5TitaniumData$ConcentrationPPM) ~ R5TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5TitaniumData$ConcentrationPPM) ~ R5TitaniumData$d15N), col="blue")

R5TMS_Ti <- lm(log10(R5TitaniumData$ConcentrationPPM) ~ R5TitaniumData$d15N)
summary(R5TMS_Ti)
plot(R5TMS_Ti$residuals)


## Uranium

R5UraniumData <- subset(R5Data, Element=="Uranium")
plot(log10(R5UraniumData$ConcentrationPPM) ~ R5UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5UraniumData$ConcentrationPPM) ~ R5UraniumData$d15N), col="blue")

R5TMS_U <- lm(log10(R5UraniumData$ConcentrationPPM) ~ R5UraniumData$d15N)
summary(R5TMS_U)
plot(R5TMS_U$residuals)


## Vanadium

R5VanadiumData <- subset(R5Data, Element=="Vanadium")
plot(log10(R5VanadiumData$ConcentrationPPM) ~ R5VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5VanadiumData$ConcentrationPPM) ~ R5VanadiumData$d15N), col="blue")

R5TMS_V <- lm(log10(R5VanadiumData$ConcentrationPPM) ~ R5VanadiumData$d15N)
summary(R5TMS_V)
plot(R5TMS_V$residuals)


## Zinc

R5ZincData <- subset(R5Data, Element=="Zinc")
plot(log10(R5ZincData$ConcentrationPPM) ~ R5ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in R5")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(R5ZincData$ConcentrationPPM) ~ R5ZincData$d15N), col="blue")

R5TMS_Zn <- lm(log10(R5ZincData$ConcentrationPPM) ~ R5ZincData$d15N)
summary(R5TMS_Zn)
plot(R5TMS_Zn$residuals)



###### M1 (Rio Yutzupino)


## Aluminum

M1AluminumData <- subset(M1Data, Element=="Aluminum")
plot(log10(M1AluminumData$ConcentrationPPM) ~ M1AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1AluminumData$ConcentrationPPM) ~ M1AluminumData$d15N), col="blue")

M1TMS_Al <- lm(log10(M1AluminumData$ConcentrationPPM) ~ M1AluminumData$d15N)
summary(M1TMS_Al)
plot(M1TMS_Al$residuals)


## Arsenic

M1ArsenicData <- subset(M1Data, Element=="Arsenic")
plot(log10(M1ArsenicData$ConcentrationPPM) ~ M1ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1ArsenicData$ConcentrationPPM) ~ M1ArsenicData$d15N), col="blue")

M1TMS_As <- lm(log10(M1ArsenicData$ConcentrationPPM) ~ M1ArsenicData$d15N)
summary(M1TMS_As)
plot(M1TMS_As$residuals)


## Barium

M1BariumData <- subset(M1Data, Element=="Barium")
plot(log10(M1BariumData$ConcentrationPPM) ~ M1BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1BariumData$ConcentrationPPM) ~ M1BariumData$d15N), col="blue")

M1TMS_Ba <- lm(log10(M1BariumData$ConcentrationPPM) ~ M1BariumData$d15N)
summary(M1TMS_Ba)
plot(M1TMS_Ba$residuals)


## Cadmium

M1CadmiumData <- subset(M1Data, Element=="Cadmium")
plot(log10(M1CadmiumData$ConcentrationPPM) ~ M1CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1CadmiumData$ConcentrationPPM) ~ M1CadmiumData$d15N), col="blue")

M1TMS_Cd <- lm(log10(M1CadmiumData$ConcentrationPPM) ~ M1CadmiumData$d15N)
summary(M1TMS_Cd)
plot(M1TMS_Cd$residuals)


## Cobalt

M1CobaltData <- subset(M1Data, Element=="Cobalt")
plot(log10(M1CobaltData$ConcentrationPPM) ~ M1CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1CobaltData$ConcentrationPPM) ~ M1CobaltData$d15N), col="blue")

M1TMS_Co <- lm(log10(M1CobaltData$ConcentrationPPM) ~ M1CobaltData$d15N)
summary(M1TMS_Co)
plot(M1TMS_Co$residuals)


## Copper

M1CopperData <- subset(M1Data, Element=="Copper")
plot(log10(M1CopperData$ConcentrationPPM) ~ M1CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1CopperData$ConcentrationPPM) ~ M1CopperData$d15N), col="blue")

M1TMS_Cu <- lm(log10(M1CopperData$ConcentrationPPM) ~ M1CopperData$d15N)
summary(M1TMS_Cu)
plot(M1TMS_Cu$residuals)


## Iron

M1IronData <- subset(M1Data, Element=="Iron")
plot(log10(M1IronData$ConcentrationPPM) ~ M1IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1IronData$ConcentrationPPM) ~ M1IronData$d15N), col="blue")

M1TMS_Fe <- lm(log10(M1IronData$ConcentrationPPM) ~ M1IronData$d15N)
summary(M1TMS_Fe)
plot(M1TMS_Fe$residuals)


## Lead

M1LeadData <- subset(M1Data, Element=="Lead")
plot(log10(M1LeadData$ConcentrationPPM) ~ M1LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1LeadData$ConcentrationPPM) ~ M1LeadData$d15N), col="blue")

M1TMS_Pb <- lm(log10(M1LeadData$ConcentrationPPM) ~ M1LeadData$d15N)
summary(M1TMS_Pb)
plot(M1TMS_Pb$residuals)


## Manganese

M1ManganeseData <- subset(M1Data, Element=="Manganese")
plot(log10(M1ManganeseData$ConcentrationPPM) ~ M1ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1ManganeseData$ConcentrationPPM) ~ M1ManganeseData$d15N), col="blue")

M1TMS_Mn <- lm(log10(M1ManganeseData$ConcentrationPPM) ~ M1ManganeseData$d15N)
summary(M1TMS_Mn)
plot(M1TMS_Mn$residuals)


## Mercury

M1MercuryData <- subset(M1Data, Element=="Mercury")
plot(log10(M1MercuryData$ConcentrationPPM) ~ M1MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1MercuryData$ConcentrationPPM) ~ M1MercuryData$d15N), col="blue")

M1TMS_Hg <- lm(log10(M1MercuryData$ConcentrationPPM) ~ M1MercuryData$d15N)
summary(M1TMS_Hg)
plot(M1TMS_Hg$residuals)


## Molybdenum

M1MolybdenumData <- subset(M1Data, Element=="Molybdenum")
plot(log10(M1MolybdenumData$ConcentrationPPM) ~ M1MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1MolybdenumData$ConcentrationPPM) ~ M1MolybdenumData$d15N), col="blue")

M1TMS_Mo <- lm(log10(M1MolybdenumData$ConcentrationPPM) ~ M1MolybdenumData$d15N)
summary(M1TMS_Mo)
plot(M1TMS_Mo$residuals)


## Nickel

M1NickelData <- subset(M1Data, Element=="Nickel")
plot(log10(M1NickelData$ConcentrationPPM) ~ M1NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1NickelData$ConcentrationPPM) ~ M1NickelData$d15N), col="blue")

M1TMS_Ni <- lm(log10(M1NickelData$ConcentrationPPM) ~ M1NickelData$d15N)
summary(M1TMS_Ni)
plot(M1TMS_Ni$residuals)


## Selenium

M1SeleniumData <- subset(M1Data, Element=="Selenium")
plot(log10(M1SeleniumData$ConcentrationPPM) ~ M1SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1SeleniumData$ConcentrationPPM) ~ M1SeleniumData$d15N), col="blue")

M1TMS_Se <- lm(log10(M1SeleniumData$ConcentrationPPM) ~ M1SeleniumData$d15N)
summary(M1TMS_Se)
plot(M1TMS_Se$residuals)


## Strontium

M1StrontiumData <- subset(M1Data, Element=="Strontium")
plot(log10(M1StrontiumData$ConcentrationPPM) ~ M1StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1StrontiumData$ConcentrationPPM) ~ M1StrontiumData$d15N), col="blue")

M1TMS_Sr <- lm(log10(M1StrontiumData$ConcentrationPPM) ~ M1StrontiumData$d15N)
summary(M1TMS_Sr)
plot(M1TMS_Sr$residuals)


## Thallium

M1ThalliumData <- subset(M1Data, Element=="Thallium")
plot(log10(M1ThalliumData$ConcentrationPPM) ~ M1ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1ThalliumData$ConcentrationPPM) ~ M1ThalliumData$d15N), col="blue")

M1TMS_Tl <- lm(log10(M1ThalliumData$ConcentrationPPM) ~ M1ThalliumData$d15N)
summary(M1TMS_Tl)
plot(M1TMS_Tl$residuals)


## Titanium

M1TitaniumData <- subset(M1Data, Element=="Titanium")
plot(log10(M1TitaniumData$ConcentrationPPM) ~ M1TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1TitaniumData$ConcentrationPPM) ~ M1TitaniumData$d15N), col="blue")

M1TMS_Ti <- lm(log10(M1TitaniumData$ConcentrationPPM) ~ M1TitaniumData$d15N)
summary(M1TMS_Ti)
plot(M1TMS_Ti$residuals)


## Uranium

M1UraniumData <- subset(M1Data, Element=="Uranium")
plot(log10(M1UraniumData$ConcentrationPPM) ~ M1UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1UraniumData$ConcentrationPPM) ~ M1UraniumData$d15N), col="blue")

M1TMS_U <- lm(log10(M1UraniumData$ConcentrationPPM) ~ M1UraniumData$d15N)
summary(M1TMS_U)
plot(M1TMS_U$residuals)


## Vanadium

M1VanadiumData <- subset(M1Data, Element=="Vanadium")
plot(log10(M1VanadiumData$ConcentrationPPM) ~ M1VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1VanadiumData$ConcentrationPPM) ~ M1VanadiumData$d15N), col="blue")

M1TMS_V <- lm(log10(M1VanadiumData$ConcentrationPPM) ~ M1VanadiumData$d15N)
summary(M1TMS_V)
plot(M1TMS_V$residuals)


## Zinc

M1ZincData <- subset(M1Data, Element=="Zinc")
plot(log10(M1ZincData$ConcentrationPPM) ~ M1ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in M1")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M1ZincData$ConcentrationPPM) ~ M1ZincData$d15N), col="blue")

M1TMS_Zn <- lm(log10(M1ZincData$ConcentrationPPM) ~ M1ZincData$d15N)
summary(M1TMS_Zn)
plot(M1TMS_Zn$residuals)



###### M2 (Rio Chimbiyacu)


## Aluminum

M2AluminumData <- subset(M2Data, Element=="Aluminum")
plot(log10(M2AluminumData$ConcentrationPPM) ~ M2AluminumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Al]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Aluminum in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2AluminumData$ConcentrationPPM) ~ M2AluminumData$d15N), col="blue")

M2TMS_Al <- lm(log10(M2AluminumData$ConcentrationPPM) ~ M2AluminumData$d15N)
summary(M2TMS_Al)
plot(M2TMS_Al$residuals)


## Arsenic

M2ArsenicData <- subset(M2Data, Element=="Arsenic")
plot(log10(M2ArsenicData$ConcentrationPPM) ~ M2ArsenicData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[As]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Arsenic in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2ArsenicData$ConcentrationPPM) ~ M2ArsenicData$d15N), col="blue")

M2TMS_As <- lm(log10(M2ArsenicData$ConcentrationPPM) ~ M2ArsenicData$d15N)
summary(M2TMS_As)
plot(M2TMS_As$residuals)


## Barium

M2BariumData <- subset(M2Data, Element=="Barium")
plot(log10(M2BariumData$ConcentrationPPM) ~ M2BariumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ba]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Barium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2BariumData$ConcentrationPPM) ~ M2BariumData$d15N), col="blue")

M2TMS_Ba <- lm(log10(M2BariumData$ConcentrationPPM) ~ M2BariumData$d15N)
summary(M2TMS_Ba)
plot(M2TMS_Ba$residuals)


## Cadmium

M2CadmiumData <- subset(M2Data, Element=="Cadmium")
plot(log10(M2CadmiumData$ConcentrationPPM) ~ M2CadmiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cd]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cadmium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2CadmiumData$ConcentrationPPM) ~ M2CadmiumData$d15N), col="blue")

M2TMS_Cd <- lm(log10(M2CadmiumData$ConcentrationPPM) ~ M2CadmiumData$d15N)
summary(M2TMS_Cd)
plot(M2TMS_Cd$residuals)


## Cobalt

M2CobaltData <- subset(M2Data, Element=="Cobalt")
plot(log10(M2CobaltData$ConcentrationPPM) ~ M2CobaltData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Co]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Cobalt in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2CobaltData$ConcentrationPPM) ~ M2CobaltData$d15N), col="blue")

M2TMS_Co <- lm(log10(M2CobaltData$ConcentrationPPM) ~ M2CobaltData$d15N)
summary(M2TMS_Co)
plot(M2TMS_Co$residuals)


## Copper

M2CopperData <- subset(M2Data, Element=="Copper")
plot(log10(M2CopperData$ConcentrationPPM) ~ M2CopperData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Cu]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Copper in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2CopperData$ConcentrationPPM) ~ M2CopperData$d15N), col="blue")

M2TMS_Cu <- lm(log10(M2CopperData$ConcentrationPPM) ~ M2CopperData$d15N)
summary(M2TMS_Cu)
plot(M2TMS_Cu$residuals)


## Iron

M2IronData <- subset(M2Data, Element=="Iron")
plot(log10(M2IronData$ConcentrationPPM) ~ M2IronData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Fe]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Iron in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2IronData$ConcentrationPPM) ~ M2IronData$d15N), col="blue")

M2TMS_Fe <- lm(log10(M2IronData$ConcentrationPPM) ~ M2IronData$d15N)
summary(M2TMS_Fe)
plot(M2TMS_Fe$residuals)


## Lead

M2LeadData <- subset(M2Data, Element=="Lead")
plot(log10(M2LeadData$ConcentrationPPM) ~ M2LeadData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Pb]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Lead in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2LeadData$ConcentrationPPM) ~ M2LeadData$d15N), col="blue")

M2TMS_Pb <- lm(log10(M2LeadData$ConcentrationPPM) ~ M2LeadData$d15N)
summary(M2TMS_Pb)
plot(M2TMS_Pb$residuals)


## Manganese

M2ManganeseData <- subset(M2Data, Element=="Manganese")
plot(log10(M2ManganeseData$ConcentrationPPM) ~ M2ManganeseData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Manganese in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2ManganeseData$ConcentrationPPM) ~ M2ManganeseData$d15N), col="blue")

M2TMS_Mn <- lm(log10(M2ManganeseData$ConcentrationPPM) ~ M2ManganeseData$d15N)
summary(M2TMS_Mn)
plot(M2TMS_Mn$residuals)


## Mercury

M2MercuryData <- subset(M2Data, Element=="Mercury")
plot(log10(M2MercuryData$ConcentrationPPM) ~ M2MercuryData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Hg]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Mercury in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2MercuryData$ConcentrationPPM) ~ M2MercuryData$d15N), col="blue")

M2TMS_Hg <- lm(log10(M2MercuryData$ConcentrationPPM) ~ M2MercuryData$d15N)
summary(M2TMS_Hg)
plot(M2TMS_Hg$residuals)


## Molybdenum

M2MolybdenumData <- subset(M2Data, Element=="Molybdenum")
plot(log10(M2MolybdenumData$ConcentrationPPM) ~ M2MolybdenumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Mo]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Molybdenum in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2MolybdenumData$ConcentrationPPM) ~ M2MolybdenumData$d15N), col="blue")

M2TMS_Mo <- lm(log10(M2MolybdenumData$ConcentrationPPM) ~ M2MolybdenumData$d15N)
summary(M2TMS_Mo)
plot(M2TMS_Mo$residuals)


## Nickel

M2NickelData <- subset(M2Data, Element=="Nickel")
plot(log10(M2NickelData$ConcentrationPPM) ~ M2NickelData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ni]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Nickel in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2NickelData$ConcentrationPPM) ~ M2NickelData$d15N), col="blue")

M2TMS_Ni <- lm(log10(M2NickelData$ConcentrationPPM) ~ M2NickelData$d15N)
summary(M2TMS_Ni)
plot(M2TMS_Ni$residuals)


## Selenium

M2SeleniumData <- subset(M2Data, Element=="Selenium")
plot(log10(M2SeleniumData$ConcentrationPPM) ~ M2SeleniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Se]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Selenium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2SeleniumData$ConcentrationPPM) ~ M2SeleniumData$d15N), col="blue")

M2TMS_Se <- lm(log10(M2SeleniumData$ConcentrationPPM) ~ M2SeleniumData$d15N)
summary(M2TMS_Se)
plot(M2TMS_Se$residuals)


## Strontium

M2StrontiumData <- subset(M2Data, Element=="Strontium")
plot(log10(M2StrontiumData$ConcentrationPPM) ~ M2StrontiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Sr]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Strontium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2StrontiumData$ConcentrationPPM) ~ M2StrontiumData$d15N), col="blue")

M2TMS_Sr <- lm(log10(M2StrontiumData$ConcentrationPPM) ~ M2StrontiumData$d15N)
summary(M2TMS_Sr)
plot(M2TMS_Sr$residuals)


## Thallium

M2ThalliumData <- subset(M2Data, Element=="Thallium")
plot(log10(M2ThalliumData$ConcentrationPPM) ~ M2ThalliumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Tl]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Thallium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2ThalliumData$ConcentrationPPM) ~ M2ThalliumData$d15N), col="blue")

M2TMS_Tl <- lm(log10(M2ThalliumData$ConcentrationPPM) ~ M2ThalliumData$d15N)
summary(M2TMS_Tl)
plot(M2TMS_Tl$residuals)


## Titanium

M2TitaniumData <- subset(M2Data, Element=="Titanium")
plot(log10(M2TitaniumData$ConcentrationPPM) ~ M2TitaniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Ti]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Titanium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2TitaniumData$ConcentrationPPM) ~ M2TitaniumData$d15N), col="blue")

M2TMS_Ti <- lm(log10(M2TitaniumData$ConcentrationPPM) ~ M2TitaniumData$d15N)
summary(M2TMS_Ti)
plot(M2TMS_Ti$residuals)


## Uranium

M2UraniumData <- subset(M2Data, Element=="Uranium")
plot(log10(M2UraniumData$ConcentrationPPM) ~ M2UraniumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[U]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Uranium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2UraniumData$ConcentrationPPM) ~ M2UraniumData$d15N), col="blue")

M2TMS_U <- lm(log10(M2UraniumData$ConcentrationPPM) ~ M2UraniumData$d15N)
summary(M2TMS_U)
plot(M2TMS_U$residuals)


## Vanadium

M2VanadiumData <- subset(M2Data, Element=="Vanadium")
plot(log10(M2VanadiumData$ConcentrationPPM) ~ M2VanadiumData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[V]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Vanadium in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2VanadiumData$ConcentrationPPM) ~ M2VanadiumData$d15N), col="blue")

M2TMS_V <- lm(log10(M2VanadiumData$ConcentrationPPM) ~ M2VanadiumData$d15N)
summary(M2TMS_V)
plot(M2TMS_V$residuals)


## Zinc

M2ZincData <- subset(M2Data, Element=="Zinc")
plot(log10(M2ZincData$ConcentrationPPM) ~ M2ZincData$d15N,
     xlim=c(1.8, 11.2), ylim=c(-5.2, 5.3), type="p", pch=16, col="gray35",
     xlab="d15N", ylab="[Zn]", cex.lab=1.0, cex.axis=1.0,
     cex=0.8, lwd=1, las=1, bty="o", yaxt="n", xaxt="n",
     main="Zinc in M2")
axis(1, at=c(2:11),
     labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))
axis(2, at=c(-5:5),
     labels=c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1",
              "10", "100", "1,000", "10,000", "100,000"))
abline(lm(log10(M2ZincData$ConcentrationPPM) ~ M2ZincData$d15N), col="blue")

M2TMS_Zn <- lm(log10(M2ZincData$ConcentrationPPM) ~ M2ZincData$d15N)
summary(M2TMS_Zn)
plot(M2TMS_Zn$residuals)




###### Graphs


#### Element-wise TMS lines ([Element] x d15N), all sites per element


FullAxisMarks  <- c(0.000001, 0.0001, 0.01, 1, 100, 10000)
FullAxisLabels <- c("0.000001", "0.0001", "0.01", "1", "100", "10000")

IndividualsData %>%
  ggplot(aes(d15N, ConcentrationPPM, colour = SiteCategory)) +
  geom_smooth(method = lm, se = TRUE) +
  geom_point(size = 2, alpha = 0.5) +
  facet_wrap(~Element, nrow = 4, ncol = 5) +
  labs(x = expression(paste(delta, ""^"15", "N")),
       y = "Concentration (ppm)") +
  scale_color_manual(values = c("gray70", "black")) +
  scale_y_log10(limits = c(0.000001, 10000), 
                breaks = FullAxisMarks,
                 label = FullAxisLabels) +
  theme_bw()



## Subsetting based on Trace (median biological concentration > 1.0 ppm)
##            vs Ultra-Trace (median biological concentration < 1.0 ppm)


tapply(IndividualsData$ConcentrationPPM, IndividualsData$Element, median)

## Trace = Aluminum, Barium, Copper, Iron, Manganese, Strontium, Titanium, Zinc
## Ultra Trace = Arsenic, Cadmium, Cobalt, Lead, Mercury,
## Molybdenum, Nickel, Selenium, Thallium, Uranium, Vanadium

min(TraceData$ConcentrationPPM)
max(TraceData$ConcentrationPPM)

TraceAxisMarks  <- c(0.000001, 0.0001, 0.01, 1, 100, 10000)
TraceAxisLabels <- c("0.000001", "0.0001", "0.01", "1", "100", "10000")

TraceData %>%
  ggplot(aes(d15N, ConcentrationPPM, colour = SiteCategory)) +
  geom_smooth(method = lm, se = TRUE) +
  geom_point(size = 2, alpha = 0.5) +
  facet_wrap(~Element, nrow = 3, ncol = 3) +
  labs(x = expression(paste(delta, ""^"15", "N")),
       y = "Concentration (ppm)") +
  scale_color_manual(values = c("gray70", "black")) +
  scale_y_log10(limits = c(0.000001, 10000), 
                breaks = TraceAxisMarks,
                 label = TraceAxisLabels) +
  theme_bw()



min(UltraTraceData$ConcentrationPPM)
max(UltraTraceData$ConcentrationPPM)

UltraTraceAxisMarks  <- c(0.000001, 0.0001, 0.01, 1, 100, 10000)
UltraTraceAxisLabels <- c("0.000001", "0.0001", "0.01", "1", "100", "10000")

UltraTraceData %>%
  ggplot(aes(d15N, ConcentrationPPM, colour = SiteCategory)) +
  geom_smooth(method = lm, se = TRUE) +
  geom_point(size = 2, alpha = 0.5) +
  facet_wrap(~Element, nrow = 3, ncol = 4) +
  labs(x = expression(paste(delta, ""^"15", "N")),
       y = "Concentration (ppm)") +
  scale_color_manual(values = c("gray70", "black")) +
  scale_y_log10(limits = c(0.000001, 10000), 
                breaks = UltraTraceAxisMarks,
                 label = UltraTraceAxisLabels) +
  theme_bw()




## Stacked Boxplots

## tapply(IndividualsData$Metric, IndividualsData$Category, mean)
## tapply(SitesData$Metric, list(SitesData$Metal, SitesData$SiteCategory), mean)



#### Aqueous vs Biological Figures

R1OmnivoreData <- subset(OmnivoreData, SiteID == "R1")
R2OmnivoreData <- subset(OmnivoreData, SiteID == "R2")
R3OmnivoreData <- subset(OmnivoreData, SiteID == "R3")
R4OmnivoreData <- subset(OmnivoreData, SiteID == "R4")
R5OmnivoreData <- subset(OmnivoreData, SiteID == "R5")
M1OmnivoreData <- subset(OmnivoreData, SiteID == "M1")
M2OmnivoreData <- subset(OmnivoreData, SiteID == "M2")

tapply(log10(R1OmnivoreData$ConcentrationPPM), R1OmnivoreData$Element, mean)
tapply(log10(R2OmnivoreData$ConcentrationPPM), R2OmnivoreData$Element, mean)
tapply(log10(R3OmnivoreData$ConcentrationPPM), R3OmnivoreData$Element, mean)
tapply(log10(R4OmnivoreData$ConcentrationPPM), R4OmnivoreData$Element, mean)
tapply(log10(R5OmnivoreData$ConcentrationPPM), R5OmnivoreData$Element, mean)
tapply(log10(M1OmnivoreData$ConcentrationPPM), M1OmnivoreData$Element, mean)
tapply(log10(M2OmnivoreData$ConcentrationPPM), M2OmnivoreData$Element, mean)

tapply(log10(R1OmnivoreData$ConcentrationPPM), R1OmnivoreData$Element, sd)
tapply(log10(R2OmnivoreData$ConcentrationPPM), R2OmnivoreData$Element, sd)
tapply(log10(R3OmnivoreData$ConcentrationPPM), R3OmnivoreData$Element, sd)
tapply(log10(R4OmnivoreData$ConcentrationPPM), R4OmnivoreData$Element, sd)
tapply(log10(R5OmnivoreData$ConcentrationPPM), R5OmnivoreData$Element, sd)
tapply(log10(M1OmnivoreData$ConcentrationPPM), M1OmnivoreData$Element, sd)
tapply(log10(M2OmnivoreData$ConcentrationPPM), M2OmnivoreData$Element, sd)


## TMS vs Aqueous


SFDataTrimmedNoM2 %>%
  ggplot(aes(LogAqueous, TMS, label = SiteID)) +
  geom_smooth(method = lm, se = TRUE, color = "turquoise3") +
  geom_text(size = 3, alpha = 1, color = "black") +
  facet_wrap(~Element, nrow = 4, ncol = 4, scales = "free") +
  labs(x = " ",
       y = " ") +
  geom_abline(slope = 0, intercept = 0, color = "gray50") +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  theme(axis.text = element_text(color = "black")) +
  theme_bw()


## Omnivore Concentrations vs Aqueous


SFDataTrimmed %>%
  ggplot(aes(LogAqueous, LogOmniConc, label = SiteID)) +
  geom_smooth(method = lm, se = TRUE, color = "turquoise3") +
  geom_text(size = 3, alpha = 1, color = "black") +
  facet_wrap(~Element, nrow = 4, ncol = 4, scales = "free") +
  geom_errorbar(aes(ymin = LogOmniConc-LogOmniSD, ymax = LogOmniConc+LogOmniSD),
                width = 0.05, color = "gray60") +
  labs(x = " ",
       y = " ") +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  theme(axis.text = element_text(color = "black")) +
  theme_bw()


## Comparing reference concentrations between us and Capparelli

CappAl <- c(0.10, 0, 0.10, 1.10)
OurAl <- c(0.75, 1.67, 0.44, 4.32, 6.60)

wilcox.test(CappAl, OurAl)

CappBa <- c(23.6, 24.5, 16.4, 30.2)
OurBa <- c(1.81, 2.76, 4.27, 5.63, 3.81)

wilcox.test(CappBa, OurBa)

CappFe <- c(0.2, 0.1, 0.10, 1.0)
OurFe <- c(11.98, 9.34, 7.14, 22.37, 16.76)

wilcox.test(CappFe, OurFe)

CappMn <- c(18.5, 5.40, 7.70, 47)
OurMn <- c(0.58, 0.66, 0.65, 1.37, 3.26)

wilcox.test(CappMn, OurMn)

CappZn <- c(31.2, 3.2, 10.9, 3.2)
OurZn <- c(0.35, 3.44, 0.40, 1.68, 0.77)

wilcox.test(CappZn, OurZn)

CappCd <- c(3.1, 2, 2.8)
OurCd <- c(0.03, 0.02, 0.01, 0.02, 0.02)

wilcox.test(CappCd, OurCd)

CappPb <- c(45.9, 1.1, 32.7)
OurPb <- c(0.44, 0.21, 0.04, 0.56, 0.18)

wilcox.test(CappPb, OurPb)




###### Testing Omnivore TE Concentrations


## Subsetting

OmnivoreAlData <- subset(OmnivoreData, Element == "Aluminum")
OmnivoreAsData <- subset(OmnivoreData, Element == "Arsenic")
OmnivoreBaData <- subset(OmnivoreData, Element == "Barium")
OmnivoreCdData <- subset(OmnivoreData, Element == "Cadmium")
OmnivoreCoData <- subset(OmnivoreData, Element == "Cobalt")
OmnivoreCuData <- subset(OmnivoreData, Element == "Copper")
OmnivoreFeData <- subset(OmnivoreData, Element == "Iron")
OmnivoreHgData <- subset(OmnivoreData, Element == "Mercury")
OmnivoreMnData <- subset(OmnivoreData, Element == "Manganese")
OmnivoreMoData <- subset(OmnivoreData, Element == "Molybdenum")
OmnivoreNiData <- subset(OmnivoreData, Element == "Nickel")
OmnivorePbData <- subset(OmnivoreData, Element == "Lead")
OmnivoreSeData <- subset(OmnivoreData, Element == "Selenium")
OmnivoreSrData <- subset(OmnivoreData, Element == "Strontium")
OmnivoreTiData <- subset(OmnivoreData, Element == "Titanium")
OmnivoreTlData <- subset(OmnivoreData, Element == "Thallium")
OmnivoreUData <- subset(OmnivoreData, Element == "Uranium")
OmnivoreVData <- subset(OmnivoreData, Element == "Vanadium")
OmnivoreZnData <- subset(OmnivoreData, Element == "Zinc")


## Testing parametric assumptions: Log-Adjusted Data

hist(log10(OmnivoreAlData$ConcentrationPPM))
qqnorm(log10(OmnivoreAlData$ConcentrationPPM))
qqline(log10(OmnivoreAlData$ConcentrationPPM))
shapiro.test(log10(OmnivoreAlData$ConcentrationPPM))
bartlett.test(log10(OmnivoreAlData$ConcentrationPPM), OmnivoreAlData$SiteCategory)

## Conclusion: Full pass

hist(log10(OmnivoreAsData$ConcentrationPPM))
qqnorm(log10(OmnivoreAsData$ConcentrationPPM))
qqline(log10(OmnivoreAsData$ConcentrationPPM))
shapiro.test(log10(OmnivoreAsData$ConcentrationPPM))
bartlett.test(log10(OmnivoreAsData$ConcentrationPPM), OmnivoreAsData$SiteCategory)

## Conclusion: Fails Shapiro and Bartlett

hist(log10(OmnivoreBaData$ConcentrationPPM))
qqnorm(log10(OmnivoreBaData$ConcentrationPPM))
qqline(log10(OmnivoreBaData$ConcentrationPPM))
shapiro.test(log10(OmnivoreBaData$ConcentrationPPM))
bartlett.test(log10(OmnivoreBaData$ConcentrationPPM), OmnivoreBaData$SiteCategory)

## Conclusion: Fails Shapiro

hist(log10(OmnivoreCdData$ConcentrationPPM))
qqnorm(log10(OmnivoreCdData$ConcentrationPPM))
qqline(log10(OmnivoreCdData$ConcentrationPPM))
shapiro.test(log10(OmnivoreCdData$ConcentrationPPM))
bartlett.test(log10(OmnivoreCdData$ConcentrationPPM), OmnivoreCdData$SiteCategory)

## Conclusion: Full pass

hist(log10(OmnivoreCoData$ConcentrationPPM))
qqnorm(log10(OmnivoreCoData$ConcentrationPPM))
qqline(log10(OmnivoreCoData$ConcentrationPPM))
shapiro.test(log10(OmnivoreCoData$ConcentrationPPM))
bartlett.test(log10(OmnivoreCoData$ConcentrationPPM), OmnivoreCoData$SiteCategory)

## Conclusion: Full pass

hist(log10(OmnivoreCuData$ConcentrationPPM))
qqnorm(log10(OmnivoreCuData$ConcentrationPPM))
qqline(log10(OmnivoreCuData$ConcentrationPPM))
shapiro.test(log10(OmnivoreCuData$ConcentrationPPM))
bartlett.test(log10(OmnivoreCuData$ConcentrationPPM), OmnivoreCuData$SiteCategory)

## Conclusion: Fails Shapiro and Bartlett

hist(log10(OmnivoreFeData$ConcentrationPPM))
qqnorm(log10(OmnivoreFeData$ConcentrationPPM))
qqline(log10(OmnivoreFeData$ConcentrationPPM))
shapiro.test(log10(OmnivoreFeData$ConcentrationPPM))
bartlett.test(log10(OmnivoreFeData$ConcentrationPPM), OmnivoreFeData$SiteCategory)

## Conclusion: Full pass

hist(log10(OmnivoreHgData$ConcentrationPPM))
qqnorm(log10(OmnivoreHgData$ConcentrationPPM))
qqline(log10(OmnivoreHgData$ConcentrationPPM))
shapiro.test(log10(OmnivoreHgData$ConcentrationPPM))
bartlett.test(log10(OmnivoreHgData$ConcentrationPPM), OmnivoreHgData$SiteCategory)

## Conclusion: Full pass

hist(log10(OmnivoreMnData$ConcentrationPPM))
qqnorm(log10(OmnivoreMnData$ConcentrationPPM))
qqline(log10(OmnivoreMnData$ConcentrationPPM))
shapiro.test(log10(OmnivoreMnData$ConcentrationPPM))
bartlett.test(log10(OmnivoreMnData$ConcentrationPPM), OmnivoreMnData$SiteCategory)

## Conclusion: Fails Shapiro

hist(log10(OmnivoreMoData$ConcentrationPPM))
qqnorm(log10(OmnivoreMoData$ConcentrationPPM))
qqline(log10(OmnivoreMoData$ConcentrationPPM))
shapiro.test(log10(OmnivoreMoData$ConcentrationPPM))
bartlett.test(log10(OmnivoreMoData$ConcentrationPPM), OmnivoreMoData$SiteCategory)

## Conclusion: Fails Shapiro

hist(log10(OmnivoreNiData$ConcentrationPPM))
qqnorm(log10(OmnivoreNiData$ConcentrationPPM))
qqline(log10(OmnivoreNiData$ConcentrationPPM))
shapiro.test(log10(OmnivoreNiData$ConcentrationPPM))
bartlett.test(log10(OmnivoreNiData$ConcentrationPPM), OmnivoreNiData$SiteCategory)

## Conclusion: Fails Shapiro

hist(log10(OmnivorePbData$ConcentrationPPM))
qqnorm(log10(OmnivorePbData$ConcentrationPPM))
qqline(log10(OmnivorePbData$ConcentrationPPM))
shapiro.test(log10(OmnivorePbData$ConcentrationPPM))
bartlett.test(log10(OmnivorePbData$ConcentrationPPM), OmnivorePbData$SiteCategory)

## Conclusion: Fails Shapiro

hist(log10(OmnivoreSeData$ConcentrationPPM))
qqnorm(log10(OmnivoreSeData$ConcentrationPPM))
qqline(log10(OmnivoreSeData$ConcentrationPPM))
shapiro.test(log10(OmnivoreSeData$ConcentrationPPM))
bartlett.test(log10(OmnivoreSeData$ConcentrationPPM), OmnivoreSeData$SiteCategory)

## Conclusion: Full pass

hist(log10(OmnivoreSrData$ConcentrationPPM))
qqnorm(log10(OmnivoreSrData$ConcentrationPPM))
qqline(log10(OmnivoreSrData$ConcentrationPPM))
shapiro.test(log10(OmnivoreSrData$ConcentrationPPM))
bartlett.test(log10(OmnivoreSrData$ConcentrationPPM), OmnivoreSrData$SiteCategory)

## Conclusion: Fails Bartlett

hist(log10(OmnivoreTiData$ConcentrationPPM))
qqnorm(log10(OmnivoreTiData$ConcentrationPPM))
qqline(log10(OmnivoreTiData$ConcentrationPPM))
shapiro.test(log10(OmnivoreTiData$ConcentrationPPM))
bartlett.test(log10(OmnivoreTiData$ConcentrationPPM), OmnivoreTiData$SiteCategory)

## Conclusion: Full pass, but barely

hist(log10(OmnivoreTlData$ConcentrationPPM))
qqnorm(log10(OmnivoreTlData$ConcentrationPPM))
qqline(log10(OmnivoreTlData$ConcentrationPPM))
shapiro.test(log10(OmnivoreTlData$ConcentrationPPM))
bartlett.test(log10(OmnivoreTlData$ConcentrationPPM), OmnivoreTlData$SiteCategory)

## Conclusion: Fails Shapiro and Bartlett

hist(log10(OmnivoreUData$ConcentrationPPM))
qqnorm(log10(OmnivoreUData$ConcentrationPPM))
qqline(log10(OmnivoreUData$ConcentrationPPM))
shapiro.test(log10(OmnivoreUData$ConcentrationPPM))
bartlett.test(log10(OmnivoreUData$ConcentrationPPM), OmnivoreUData$SiteCategory)

## Conclusion: Fails Shapiro

hist(log10(OmnivoreVData$ConcentrationPPM))
qqnorm(log10(OmnivoreVData$ConcentrationPPM))
qqline(log10(OmnivoreVData$ConcentrationPPM))
shapiro.test(log10(OmnivoreVData$ConcentrationPPM))
bartlett.test(log10(OmnivoreVData$ConcentrationPPM), OmnivoreVData$SiteCategory)

## Conclusion: Fails Shapiro and Bartlett

hist(log10(OmnivoreZnData$ConcentrationPPM))
qqnorm(log10(OmnivoreZnData$ConcentrationPPM))
qqline(log10(OmnivoreZnData$ConcentrationPPM))
shapiro.test(log10(OmnivoreZnData$ConcentrationPPM))
bartlett.test(log10(OmnivoreZnData$ConcentrationPPM), OmnivoreZnData$SiteCategory)

## Conclusion: Full pass



## More subsetting

RefOmniAlData <- subset(OmnivoreAlData, SiteCategory == "Reference")
RefOmniAsData <- subset(OmnivoreAsData, SiteCategory == "Reference")
RefOmniBaData <- subset(OmnivoreBaData, SiteCategory == "Reference")
RefOmniCdData <- subset(OmnivoreCdData, SiteCategory == "Reference")
RefOmniCoData <- subset(OmnivoreCoData, SiteCategory == "Reference")
RefOmniCuData <- subset(OmnivoreCuData, SiteCategory == "Reference")
RefOmniFeData <- subset(OmnivoreFeData, SiteCategory == "Reference")
RefOmniHgData <- subset(OmnivoreHgData, SiteCategory == "Reference")
RefOmniMnData <- subset(OmnivoreMnData, SiteCategory == "Reference")
RefOmniMoData <- subset(OmnivoreMoData, SiteCategory == "Reference")
RefOmniNiData <- subset(OmnivoreNiData, SiteCategory == "Reference")
RefOmniPbData <- subset(OmnivorePbData, SiteCategory == "Reference")
RefOmniSeData <- subset(OmnivoreSeData, SiteCategory == "Reference")
RefOmniSrData <- subset(OmnivoreSrData, SiteCategory == "Reference")
RefOmniTiData <- subset(OmnivoreTiData, SiteCategory == "Reference")
RefOmniTlData <- subset(OmnivoreTlData, SiteCategory == "Reference")
RefOmniUData <- subset(OmnivoreUData, SiteCategory == "Reference")
RefOmniVData <- subset(OmnivoreVData, SiteCategory == "Reference")
RefOmniZnData <- subset(OmnivoreZnData, SiteCategory == "Reference")

MineOmniAlData <- subset(OmnivoreAlData, SiteCategory == "Mining")
MineOmniAsData <- subset(OmnivoreAsData, SiteCategory == "Mining")
MineOmniBaData <- subset(OmnivoreBaData, SiteCategory == "Mining")
MineOmniCdData <- subset(OmnivoreCdData, SiteCategory == "Mining")
MineOmniCoData <- subset(OmnivoreCoData, SiteCategory == "Mining")
MineOmniCuData <- subset(OmnivoreCuData, SiteCategory == "Mining")
MineOmniFeData <- subset(OmnivoreFeData, SiteCategory == "Mining")
MineOmniHgData <- subset(OmnivoreHgData, SiteCategory == "Mining")
MineOmniMnData <- subset(OmnivoreMnData, SiteCategory == "Mining")
MineOmniMoData <- subset(OmnivoreMoData, SiteCategory == "Mining")
MineOmniNiData <- subset(OmnivoreNiData, SiteCategory == "Mining")
MineOmniPbData <- subset(OmnivorePbData, SiteCategory == "Mining")
MineOmniSeData <- subset(OmnivoreSeData, SiteCategory == "Mining")
MineOmniSrData <- subset(OmnivoreSrData, SiteCategory == "Mining")
MineOmniTiData <- subset(OmnivoreTiData, SiteCategory == "Mining")
MineOmniTlData <- subset(OmnivoreTlData, SiteCategory == "Mining")
MineOmniUData <- subset(OmnivoreUData, SiteCategory == "Mining")
MineOmniVData <- subset(OmnivoreVData, SiteCategory == "Mining")
MineOmniZnData <- subset(OmnivoreZnData, SiteCategory == "Mining")




## Wilcoxon Rank Sum Tests: Omnivore [TE]s in reference vs mining streams


wilcox.test(RefOmniAlData$ConcentrationPPM, MineOmniAlData$ConcentrationPPM)
mean(RefOmniAlData$ConcentrationPPM)
mean(MineOmniAlData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniAsData$ConcentrationPPM, MineOmniAsData$ConcentrationPPM)
mean(RefOmniAsData$ConcentrationPPM)
mean(MineOmniAsData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniBaData$ConcentrationPPM, MineOmniBaData$ConcentrationPPM)
mean(RefOmniBaData$ConcentrationPPM)
mean(MineOmniBaData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniCdData$ConcentrationPPM, MineOmniCdData$ConcentrationPPM)
mean(RefOmniCdData$ConcentrationPPM)
mean(MineOmniCdData$ConcentrationPPM)
sd(RefOmniCdData$ConcentrationPPM)
sd(MineOmniCdData$ConcentrationPPM)

## Mining sites (mean = 0.25) higher than reference (mean = 0.12)

wilcox.test(RefOmniCoData$ConcentrationPPM, MineOmniCoData$ConcentrationPPM)
mean(RefOmniCoData$ConcentrationPPM)
mean(MineOmniCoData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniCuData$ConcentrationPPM, MineOmniCuData$ConcentrationPPM)
mean(RefOmniCuData$ConcentrationPPM)
mean(MineOmniCuData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniFeData$ConcentrationPPM, MineOmniFeData$ConcentrationPPM)
mean(RefOmniFeData$ConcentrationPPM)
mean(MineOmniFeData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniHgData$ConcentrationPPM, MineOmniHgData$ConcentrationPPM)
mean(RefOmniHgData$ConcentrationPPM)
mean(MineOmniHgData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniMnData$ConcentrationPPM, MineOmniMnData$ConcentrationPPM)
mean(RefOmniMnData$ConcentrationPPM)
mean(MineOmniMnData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniMoData$ConcentrationPPM, MineOmniMoData$ConcentrationPPM)
mean(RefOmniMoData$ConcentrationPPM)
mean(MineOmniMoData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniNiData$ConcentrationPPM, MineOmniNiData$ConcentrationPPM)
mean(RefOmniNiData$ConcentrationPPM)
mean(MineOmniNiData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniPbData$ConcentrationPPM, MineOmniPbData$ConcentrationPPM)
mean(RefOmniPbData$ConcentrationPPM)
mean(MineOmniPbData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniSeData$ConcentrationPPM, MineOmniSeData$ConcentrationPPM)
mean(RefOmniSeData$ConcentrationPPM)
mean(MineOmniSeData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniSrData$ConcentrationPPM, MineOmniSrData$ConcentrationPPM)
mean(RefOmniSrData$ConcentrationPPM)
mean(MineOmniSrData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniTiData$ConcentrationPPM, MineOmniTiData$ConcentrationPPM)
mean(RefOmniTiData$ConcentrationPPM)
mean(MineOmniTiData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniTlData$ConcentrationPPM, MineOmniTlData$ConcentrationPPM)
mean(RefOmniTlData$ConcentrationPPM)
mean(MineOmniTlData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniUData$ConcentrationPPM, MineOmniUData$ConcentrationPPM)
mean(RefOmniUData$ConcentrationPPM)
mean(MineOmniUData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniVData$ConcentrationPPM, MineOmniVData$ConcentrationPPM)
mean(RefOmniVData$ConcentrationPPM)
mean(MineOmniVData$ConcentrationPPM)

## No difference

wilcox.test(RefOmniZnData$ConcentrationPPM, MineOmniZnData$ConcentrationPPM)
mean(RefOmniZnData$ConcentrationPPM)
mean(MineOmniZnData$ConcentrationPPM)

## No difference



## Now let's see whether Hierarchical Cluster Analysis (HCA) and 
## Principal Component Analysis (PCA) can tell us why we didn't see much:

## First, HCA:

AmbientPCAData2 <- read_excel("Mining HCA and PCA Data Aqueous.xlsx")
OmnivorePCAData2 <- read_excel("Mining HCA and PCA Data Omnivores.xlsx")
TMSPCAData2 <- read_excel("Mining HCA and PCA Data TMS.xlsx")

AmbientPCAData <- AmbientPCAData2[3:18]
OmnivorePCAData <- OmnivorePCAData2[3:21]
TMSPCAData <- TMSPCAData2[3:21]

## z-standardize data (i.e., set mean to 0 and sd = 1 for all x-variables)

AmbientPCAData_Scaled <- scale(AmbientPCAData)
OmnivorePCAData_Scaled <- scale(OmnivorePCAData)
TMSPCAData_Scaled <- scale(TMSPCAData)

## Turn Euclidean distances among variables into an R object

AmbientDist <- dist(AmbientPCAData_Scaled)
OmnivoreDist <- dist(OmnivorePCAData_Scaled)
TMSDist <- dist(TMSPCAData_Scaled)

## HCAs:

AmbientHCA <- hclust(AmbientDist, method = "complete")
OmnivoreHCA <- hclust(OmnivoreDist, method = "complete")
TMSHCA <- hclust(TMSDist, method = "complete")

AmbientHCA
OmnivoreHCA
TMSHCA

## Plotting

plot(AmbientHCA)

## Determining best number of clusters

set.seed(1)
Ambientk <- NbClust(data = AmbientPCAData_Scaled, distance = "euclidean",
        min.nc = 2, max.nc = 6, method = "complete", index = "gap")
Ambientk$All.index
Ambientk$All.CriticalValues
Ambientk$Best.nc
Ambientk$Best.partition
rect.hclust(AmbientHCA, k = 2, border=2:6)

plot(OmnivoreHCA)
Omnivorek <- NbClust(data = OmnivorePCAData_Scaled, distance = "euclidean",
        min.nc = 2, max.nc = 6, method = "complete", index = "gap")
Omnivorek$All.index
Omnivorek$All.CriticalValues
Omnivorek$Best.nc
Omnivorek$Best.partition
rect.hclust(OmnivoreHCA, k = 2, border=2:5)

plot(TMSHCA)
TMSk <- NbClust(data = TMSPCAData_Scaled, distance = "euclidean",
        min.nc = 2, max.nc = 5, method = "complete", index = "gap")
TMSk$All.index
TMSk$All.CriticalValues
TMSk$Best.nc
TMSk$Best.partition
rect.hclust(TMSHCA, k = 2, border=2:5)



## Now it is time for PCA

AmbientPCA <- prcomp(AmbientPCAData, scale = TRUE)
OmnivorePCA <- prcomp(OmnivorePCAData, scale = TRUE)
TMSPCA <- prcomp(TMSPCAData, scale = TRUE)

summary(AmbientPCA)
## Looks like at least 2 PCs, probably 3
summary(OmnivorePCA)
## Looks like 3 PCs
summary(TMSPCA)
## Looks like at least 2 PCs, probably 3


## Standard deviation of components (this is in the previous summary output anyway)
## These are the Eigenvalues

AmbientPCA$sdev
OmnivorePCA$sdev
TMSPCA$sdev


## Eigenvectors of first three PCs. These are the factor loadings
## for each trace element on each of the first three PCs

AmbientPCA$rotation[,1:3]
OmnivorePCA$rotation[,1:3]
TMSPCA$rotation[,1:3]


## Principal component scores. These are essentially the new coordinate values
## of each data point (in our case, sites R1, R2, R3, R4, R5, M1, and M2)
## in the three-dimensional space wherein each PC represents an axis. For example,
## since we are keeping the first three PCs in all analyses, the principal
## component "scores" represent the coordinates along the new axes PC1, PC2, and PC3.

AmbientPCA$x[,1:3]
OmnivorePCA$x[,1:3]
TMSPCA$x[,1:3]


## Scree plots of variances

fviz_eig(AmbientPCA, addlabels = TRUE)
fviz_eig(OmnivorePCA, addlabels = TRUE)
fviz_eig(TMSPCA, addlabels = TRUE)


## Biplots

fviz_pca_biplot(AmbientPCA, label = "var", habillage = AmbientPCAData2$SiteCategory,
                mean.point = FALSE, col.var = "black",
                pointshape = 16, pointsize = 2, ggtheme = theme_bw()) +
                scale_color_manual(values = c("red4", "blue3"))
fviz_pca_biplot(OmnivorePCA, label = "var", habillage = OmnivorePCAData2$SiteCategory,
                mean.point = FALSE, col.var = "black",
                pointshape = 16, pointsize = 2, ggtheme = theme_bw()) +
                scale_color_manual(values = c("red4", "blue3"))
fviz_pca_biplot(TMSPCA, label = "var", habillage = TMSPCAData2$SiteCategory,
                mean.point = FALSE, col.var = "black", ylim=c(-3.6, 4),
                pointshape = 16, pointsize = 2, ggtheme = theme_bw()) +
                scale_color_manual(values = c("red4", "blue3"))


min(R1MercuryData$d15N) - max(R1MercuryData$d15N)
min(R2MercuryData$d15N) - max(R2MercuryData$d15N)
min(R3MercuryData$d15N) - max(R3MercuryData$d15N)
min(R4MercuryData$d15N) - max(R4MercuryData$d15N)
min(R5MercuryData$d15N) - max(R5MercuryData$d15N)
min(M1MercuryData$d15N) - max(M1MercuryData$d15N)

min(R1AluminumData$d15N) - max(R1AluminumData$d15N)
min(R2AluminumData$d15N) - max(R2AluminumData$d15N)
min(R3AluminumData$d15N) - max(R3AluminumData$d15N)
min(R4AluminumData$d15N) - max(R4AluminumData$d15N)
min(R5AluminumData$d15N) - max(R5AluminumData$d15N)
min(M1AluminumData$d15N) - max(M1AluminumData$d15N)


## Testing whether pooling sites by site category
## (Mining vs Reference) leads to different TMSs

AlAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = AluminumData)
summary(AlAncova, type = "III")


AsAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = ArsenicData)
summary(AsAncova, type = "III")


BaAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = BariumData)
summary(BaAncova, type = "III")


CdAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = CadmiumData)
summary(CdAncova, type = "III")


CoAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = CobaltData)
summary(CoAncova, type = "III")


CuAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = CopperData)
summary(CuAncova, type = "III")


FeAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = IronData)
summary(FeAncova, type = "III")


HgAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = MercuryData)
summary(HgAncova, type = "III")


MnAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = ManganeseData)
summary(MnAncova, type = "III")


MoAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = MolybdenumData)
summary(MoAncova, type = "III")


NiAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = NickelData)
summary(NiAncova, type = "III")


PbAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = LeadData)
summary(PbAncova, type = "III")


SeAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = SeleniumData)
summary(SeAncova, type = "III")


SrAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = StrontiumData)
summary(SrAncova, type = "III")


TiAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = TitaniumData)
summary(TiAncova, type = "III")


TlAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = ThalliumData)
summary(TlAncova, type = "III")


UAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = UraniumData)
summary(UAncova, type = "III")


VAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = VanadiumData)
summary(VAncova, type = "III")


ZnAncova <- lm(log10(ConcentrationPPM) ~
          SiteCategory + d15N + SiteCategory:d15N, data = ZincData)
summary(ZnAncova, type = "III")