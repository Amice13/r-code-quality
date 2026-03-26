
# Environmental Determinants of Trace Element Trophic Transfer Rates in
# Freshwater Food Webs


# Alexander Pelletier, Francisco Villamarin, Joao Campos-Silva,
# Andressa Scabin, Lorne Doig, and Tim Jardine


# Code written by Alex Pelletier



## Working Directory and Importing Packages

## Don't forget to set working directory to your own personal computer before continuing.

library(readxl)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(tidyverse)
library(stargazer)
library(patchwork)
library(reporter)
library(magrittr)
library(devtools)
library(lme4)
library(lavaan)
library(semPlot)
library(DHARMa)
library(car)
library(dunn.test)
par(family="sans", las=1, bg=NA, mar=c(5,6,4,1)+.1)



###### Importing and Subsetting Data ######


TMSData <- read_excel("Ecological TMSs Data.xlsx")
attach(TMSData)


## Site Info

TMSData$Region <- factor(TMSData$Region,
                           levels=c("Canada", "Tropics"))
TMSData$HabitatType <- factor(TMSData$HabitatType,
                                levels=c("Oxbow Lake", "Stream"))
TMSData$SiteCategory <- factor(TMSData$SiteCategory,
                                 levels=c("Canadian Lakes", "Canadian Streams",
                                          "Tropical Lakes", "Tropical Streams"))
TMSData$SiteID <- factor(TMSData$SiteID)
TMSData$SiteName <- factor(TMSData$SiteName)

TMSData$Latitude <- as.numeric(TMSData$Latitude)
TMSData$Longitude <- as.numeric(TMSData$Longitude)
TMSData$Elevation <- as.numeric(TMSData$Elevation)

TMSData$ScaledSize <- as.numeric(TMSData$ScaledSize)
TMSData$MeanTSI <- as.numeric(TMSData$MeanTSI)
TMSData$MeanTRA <- as.numeric(TMSData$MeanTRA)

TMSData$CRange <- as.numeric(TMSData$CRange)
TMSData$NRange <- as.numeric(TMSData$NRange)
TMSData$SizeStructureSlope <- as.numeric(TMSData$SizeStructureSlope)
TMSData$CNA <- as.numeric(TMSData$CNA)

TMSData$Element <- factor(TMSData$Element)
TMSData$TMS <- as.numeric(TMSData$TMS)
TMSData$Ambient <- as.numeric(TMSData$Ambient)



#### Site-Specific Subsets

## Canada Oxbow Lakes (CLs)

CL1Data <- subset(TMSData, SiteID == "CL1")
attach(CL1Data)
CL2Data <- subset(TMSData, SiteID == "CL2")
attach(CL2Data)
CL3Data <- subset(TMSData, SiteID == "CL3")
attach(CL3Data)
CL4Data <- subset(TMSData, SiteID == "CL4")
attach(CL4Data)
CL5Data <- subset(TMSData, SiteID == "CL5")
attach(CL5Data)
CL6Data <- subset(TMSData, SiteID == "CL6")
attach(CL6Data)
CL7Data <- subset(TMSData, SiteID == "CL7")
attach(CL7Data)


## Canada Headwater Streams (CSs)

CS1Data <- subset(TMSData, SiteID == "CS1")
attach(CS1Data)
CS2Data <- subset(TMSData, SiteID == "CS2")
attach(CS2Data)
CS3Data <- subset(TMSData, SiteID == "CS3")
attach(CS3Data)
CS4Data <- subset(TMSData, SiteID == "CS4")
attach(CS4Data)
CS5Data <- subset(TMSData, SiteID == "CS5")
attach(CS5Data)
CS6Data <- subset(TMSData, SiteID == "CS6")
attach(CS6Data)
CS7Data <- subset(TMSData, SiteID == "CS7")
attach(CS7Data)
CS8Data <- subset(TMSData, SiteID == "CS8")
attach(CS8Data)


## Tropics Oxbow Lakes (TLs)

TL1Data <- subset(TMSData, SiteID == "TL1")
attach(TL1Data)
TL2Data <- subset(TMSData, SiteID == "TL2")
attach(TL2Data)
TL3Data <- subset(TMSData, SiteID == "TL3")
attach(TL3Data)
TL4Data <- subset(TMSData, SiteID == "TL4")
attach(TL4Data)
TL5Data <- subset(TMSData, SiteID == "TL5")
attach(TL5Data)
TL6Data <- subset(TMSData, SiteID == "TL6")
attach(TL6Data)
TL7Data <- subset(TMSData, SiteID == "TL7")
attach(TL7Data)
TL8Data <- subset(TMSData, SiteID == "TL8")
attach(TL8Data)


## Tropics Headwater Streams (TH's)

TS1Data <- subset(TMSData, SiteID == "TS1")
attach(TS1Data)
TS2Data <- subset(TMSData, SiteID == "TS2")
attach(TS2Data)
TS3Data <- subset(TMSData, SiteID == "TS3")
attach(TS3Data)
TS4Data <- subset(TMSData, SiteID == "TS4")
attach(TS4Data)
TS5Data <- subset(TMSData, SiteID == "TS5")
attach(TS5Data)
TS6Data <- subset(TMSData, SiteID == "TS6")
attach(TS6Data)
TS7Data <- subset(TMSData, SiteID == "TS7")
attach(TS7Data)


## Habitat Types

LakesData <- subset(TMSData, HabitatType == "Oxbow Lake")
attach(LakesData)

StreamsData <- subset(TMSData, HabitatType == "Stream")
attach(StreamsData)


## Regions

CanadaData <- subset(TMSData, Region == "Canada")
attach(CanadaData)

TropicsData <- subset(TMSData, Region == "Tropics")
attach(TropicsData)


#### Region x Habitat Combinations (n = 4)

## Canada Oxbow Lakes

CLsData <- subset(CanadaData, HabitatType == "Oxbow Lake")
attach(CLsData)

## Canada Headwater Streams

CSsData <- subset(CanadaData, HabitatType == "Stream")
attach(CSsData)

## Tropics Oxbow Lakes

TLsData <- subset(TropicsData, HabitatType == "Oxbow Lake")
attach(TLsData)

## Tropics Headwater Streams

TSsData <- subset(TropicsData, HabitatType == "Stream")
attach(TSsData)




## Elemental Subsets

AlData <- subset(TMSData, Element == "Al")
attach(AlData)

AsData <- subset(TMSData, Element == "As")
attach(AsData)

BaData <- subset(TMSData, Element == "Ba")
attach(BaData)

CdData <- subset(TMSData, Element == "Cd")
attach(CdData)

CoData <- subset(TMSData, Element == "Co")
attach(CoData)

CuData <- subset(TMSData, Element == "Cu")
attach(CuData)

FeData <- subset(TMSData, Element == "Fe")
attach(FeData)

HgData <- subset(TMSData, Element == "Hg")
attach(HgData)

MnData <- subset(TMSData, Element == "Mn")
attach(MnData)

NiData <- subset(TMSData, Element == "Ni")
attach(NiData)

PbData <- subset(TMSData, Element == "Pb")
attach(PbData)

SeData <- subset(TMSData, Element == "Se")
attach(SeData)

SrData <- subset(TMSData, Element == "Sr")
attach(SrData)

TiData <- subset(TMSData, Element == "Ti")
attach(TiData)

TlData <- subset(TMSData, Element == "Tl")
attach(TlData)

UData <- subset(TMSData, Element == "U")
attach(UData)

VData <- subset(TMSData, Element == "V")
attach(VData)

ZnData <- subset(TMSData, Element == "Zn")
attach(ZnData)




## Note that TMS data already appear in the spreadsheets loaded above.
## I removed most of the code I used to calculate these because the code was
## extremely long, but here is a snippet of code that you can use to re-calculate
## TMSs for individual elements at individual sites. Simply change the site code 
## and element type as needed, and create new site subsets as needed:

IndivData <- read_excel("Ecological TMSs Individual Data February 2026.xlsx")
attach(IndivData)

CL1Data_Indiv <- subset(IndivData, SiteID == "CL1")


summary(lm(log10(CL1Data_Indiv$AluminumPPM) ~ CL1Data_Indiv$d15N))

## The TMS of Al at Site CL1 in the summary output table is -0.3850 (p = 0.010066)
## The TMF of Al at Site CL1 is 10^(3.4 * -0.3850), which is 0.049. The 3.4 in
## this equation is the assumed trophic discrimination factor of d15N, which we
## assumed to be 3.4 permil. Given that this is not a great assumption, and given
## that TMSs had less skew and fewer outliers, we used TMSs for all statistical analyses.

plot(log10(CL1Data_Indiv$AluminumPPM) ~ CL1Data_Indiv$d15N)
abline(lm(log10(CL1Data_Indiv$AluminumPPM) ~ CL1Data_Indiv$d15N))



#### Multicollinearity Testing



cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  
  return(p.mat)
}



#### Correlation Plots


## First, let's look at how well ambient aqueous trace element concentrations
## correlated with each other:

WaterCorData <- read_excel("Water Correlation Data.xlsx")
attach(WaterCorData)

pairs(WaterCorData)

Waterpmatrix <- cor.mtest(WaterCorData)
Waterpmatrix <- as.matrix(Waterpmatrix)

WaterCor <- cor(WaterCorData, method = "spearman", use = "complete.obs")
corrplot(WaterCor, method = "square", type="lower",
         p.mat = Waterpmatrix, diag=F, tl.pos="ld",
         ## sig.level = 0.00345,
         sig.level = 1,
         insig="blank", mar=c(1.5,1.5,1.5,1.5),
         tl.srt=0, tl.offset = 0.8, tl.col = "black", tl.cex=0.8)




## Now let's do the same thing to see how well trace element TMSs correlated
## with each other:


TMSCorData_d15N <- read_excel("TMS Correlation Data d15N.xlsx")
attach(TMSCorData_d15N)

TMSpmatrix_d15N <- cor.mtest(TMSCorData_d15N)
TMSpmatrix_d15N <- as.matrix(TMSpmatrix_d15N)

pairs(TMSCorData_d15N)

TMScor_d15N <- cor(TMSCorData_d15N, method = "pearson", use = "complete.obs")
corrplot(TMScor_d15N, method = "circle", type="upper",
         p.mat = TMSpmatrix_d15N, diag=F,
         ## tl.pos="n",
         ## sig.level = 0.002858,
         sig.level = 1,
         insig="blank", mar=c(1.5,1.5,1.5,1.5),
         tl.srt=0, tl.offset = 0.8, tl.col = "black", tl.cex=0.8)



## Now let's investigate how trace element TMSs varied among regions and habitat types.


RegionalCorData_d15N <- read_excel("Regional Correlation Data d15N.xlsx")
attach(RegionalCorData_d15N)
head(RegionalCorData_d15N)

## Values in the table below represent habitat type-specific or region-specific
## mean (or standard deviation) TMS values for each element.


ElementList <- c("Al", "As", "Ba", "Cd", "Co", "Cu", "Fe", "Hg", "Mn",
                 "Ni", "Pb", "Se", "Sr", "Ti", "Tl", "U", "V", "Zn")


StreamTMS_d15N <- RegionalCorData_d15N$StreamTMS
LakeTMS_d15N <- RegionalCorData_d15N$LakeTMS
CanadaTMS_d15N <- RegionalCorData_d15N$CanadaTMS
TropicsTMS_d15N <- RegionalCorData_d15N$TropicsTMS
StreamSD_d15N <- RegionalCorData_d15N$StreamSD
LakeSD_d15N <- RegionalCorData_d15N$LakeSD
CanadaSD_d15N <- RegionalCorData_d15N$CanadaSD
TropicsSD_d15N <- RegionalCorData_d15N$TropicsSD
TotalTMS_d15N <- RegionalCorData_d15N$TotalTMS
TotalSD_d15N <- RegionalCorData_d15N$TotalSD



plot(StreamTMS_d15N, LakeTMS_d15N, type="n",
     xlim=c(-0.68, 0.20), ylim=c(-0.48, 0.28), xaxt="n", yaxt="n",
     xlab="TMS in Streams", ylab="TMS in Lakes")
axis(1, at=seq(-0.7, 0.3, 0.1),
     labels=c("-0.7", "-0.6", "-0.5", "-0.4", "-0.3", "-0.2",
              "-0.1",  "0.0",  "0.1",  "0.2",  "0.3"))
axis(2, at=seq(-0.7, 0.3, 0.1),
     labels=c("-0.7", "-0.6", "-0.5", "-0.4", "-0.3", "-0.2",
              "-0.1",  "0.0",  "0.1",  "0.2",  "0.3"))
text(StreamTMS_d15N, LakeTMS_d15N, labels=ElementList, cex=1)
arrows(StreamTMS_d15N-0.023, LakeTMS_d15N, StreamTMS_d15N-StreamSD_d15N, LakeTMS_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
arrows(StreamTMS_d15N+0.02, LakeTMS_d15N, StreamTMS_d15N+StreamSD_d15N, LakeTMS_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
arrows(StreamTMS_d15N, LakeTMS_d15N-0.023, StreamTMS_d15N, LakeTMS_d15N-LakeSD_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
arrows(StreamTMS_d15N, LakeTMS_d15N+0.02, StreamTMS_d15N, LakeTMS_d15N+LakeSD_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
abline(0, 1, lty=1, col="dodgerblue", lwd=1.7)
abline(h=0, lwd=1, lty=2, col="black")
abline(v=0, lwd=1, lty=2, col="black")
## legend(-0.661, 0.253, expression({delta}^15*N~'- derived'), bty="n", cex=1.1)



plot(CanadaTMS_d15N, TropicsTMS_d15N, type="n",
     xlim=c(-0.68, 0.20), ylim=c(-0.68, 0.28), xaxt="n", yaxt="n",
     xlab="TMS in Western Canada", ylab="TMS in Amazonia")
axis(1, at=seq(-0.7, 0.3, 0.1),
     labels=c("-0.7", "-0.6", "-0.5", "-0.4", "-0.3", "-0.2",
              "-0.1",  "0.0",  "0.1",  "0.2",  "0.3"))
axis(2, at=seq(-0.7, 0.3, 0.1),
     labels=c("-0.7", "-0.6", "-0.5", "-0.4", "-0.3", "-0.2",
              "-0.1",  "0.0",  "0.1",  "0.2",  "0.3"))
text(CanadaTMS_d15N, TropicsTMS_d15N, labels=ElementList, cex=1)
arrows(CanadaTMS_d15N-0.023, TropicsTMS_d15N, CanadaTMS_d15N-CanadaSD_d15N, TropicsTMS_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
arrows(CanadaTMS_d15N+0.02, TropicsTMS_d15N, CanadaTMS_d15N+CanadaSD_d15N, TropicsTMS_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
arrows(CanadaTMS_d15N, TropicsTMS_d15N-0.023, CanadaTMS_d15N, TropicsTMS_d15N-TropicsSD_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
arrows(CanadaTMS_d15N, TropicsTMS_d15N+0.02, CanadaTMS_d15N, TropicsTMS_d15N+TropicsSD_d15N,
       lty=2, lwd=0.2, angle=90, length=0.05, col="gray72")
abline(0, 1, lty=1, col="dodgerblue", lwd=1.7)
abline(h=0, lwd=1, lty=2, col="black")
abline(v=0, lwd=1, lty=2, col="black")
## legend(-0.661, 0.253, expression({delta}^15*N~'- derived'), bty="n", cex=1.1)




#### Regional Elemental TMS Plot ####

ElementTMSPlot <-
  ggplot(TMSData, aes(x = Element, y = TMS, fill=SiteCategory)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("dodgerblue3", "deepskyblue",
                               "green4", "green2")) +
  geom_hline(slope=0, yintercept=0) +
  scale_y_continuous(limits=c(-1.2, 0.6)) +
  labs(title = "",
       x = "Element",
       y = "TMS",
       fill = "Site Category") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0))

ElementTMSPlot



## Alternative View: Let's facet wrap it

ElementTMSPlot_facet <-
  ggplot(TMSData, aes(x = SiteCategory, y = TMS)) +
  geom_hline(yintercept = 0.25, col="gray85") +
  geom_hline(yintercept = 0, col="black", lwd=0.5) +
  geom_hline(yintercept = -0.25, col="gray85") +
  geom_hline(yintercept = -0.5, col="gray75") +
  geom_hline(yintercept = -0.75, col="gray85") +
  geom_hline(yintercept = -1, col="gray75") +
  geom_boxplot(position = position_dodge(width = 0.4),
               width=0.6,
               fill=rep(c("dodgerblue3", "deepskyblue",
                          "green4", "green2"), times=18)) +
  facet_wrap(~ Element, nrow = 3, ncol = 6) +
  scale_y_continuous(limits=c(-1.18, 0.42), n.breaks=8) +
  labs(title = "",
       x = "",
       y = "",
       fill = "Site Category") +
  theme_bw()

ElementTMSPlot_facet





## Now we can start statistical testing for differences among sites
## and in relation to environmental and ecological parameters



#### Assumptions Testing for TMSs: Can we use parametric testing, or do
## we need non-parametric measures?



## Aluminum (Al)

hist(AlData$TMS)
shapiro.test(AlData$TMS)
qqnorm(AlData$TMS)
qqline(AlData$TMS)
## Passes Shapiro (p = 0.819) and visually appears normal.
bartlett.test(AlData$TMS, AlData$Region)
## Passes Bartlett (p = 0.425)
tapply(AlData$TMS, AlData$Region, mean)
tapply(AlData$TMS, AlData$Region, sd)
t.test(AlData$TMS ~ AlData$Region)
## TMSs are higher in Amazonia (-0.150 +/- 0.195) than in Canada (-0.310 +/- 0.157)
## (t(26.8) = -2.47, p = 0.020)
bartlett.test(AlData$TMS, AlData$HabitatType)
## Barely passes Bartlett (p = 0.051)
tapply(AlData$TMS, AlData$HabitatType, mean)
tapply(AlData$TMS, AlData$HabitatType, sd)
t.test(AlData$TMS ~ AlData$HabitatType)
## TMSs are similar between oxbow lakes (-0.184 +/- 0.135) and streams (-0.276 +/- 0.232)
## (t(22.5) = 1.33, p = 0.197)

tapply(AlData$TMS, list(AlData$Region, AlData$HabitatType), mean)
tapply(AlData$TMS, list(AlData$Region, AlData$HabitatType), sd)
kruskal.test(AlData$TMS ~ AlData$SiteCategory)
dunn.test(AlData$TMS, AlData$SiteCategory)
## TMSs are higher in tropical lakes than Canadian streams



## Arsenic (As)

hist(AsData$TMS)
shapiro.test(AsData$TMS)
qqnorm(AsData$TMS)
qqline(AsData$TMS)
## Shapiro (p = 0.002) and QQ plot suggests not normal.
bartlett.test(AsData$TMS, AsData$Region)
## Passes Bartlett (p = 0.413)
tapply(AsData$TMS, AsData$Region, mean)
tapply(AsData$TMS, AsData$Region, sd)
t.test(AsData$TMS ~ AsData$Region)
## TMSs are similar in Amazonia (-0.202 +/- 0.143) and Canada (-0.155 +/- 0.114)
## (t(26.7) = 0.98, p = 0.336)
bartlett.test(AsData$TMS, AsData$HabitatType)
## Passes Bartlett (p = 0.171)
tapply(AsData$TMS, AsData$HabitatType, mean)
tapply(AsData$TMS, AsData$HabitatType, sd)
t.test(AsData$TMS ~ AsData$HabitatType)
## TMSs are similar between oxbow lakes (-0.143 +/- 0.101) and streams (-0.214 +/- 0.147)
## (t(24.8) = 1.55, p = 0.134)

tapply(AsData$TMS, list(AsData$Region, AsData$HabitatType), mean)
tapply(AsData$TMS, list(AsData$Region, AsData$HabitatType), sd)
kruskal.test(AsData$TMS ~ AsData$SiteCategory)
dunn.test(AsData$TMS, AsData$SiteCategory)
## No differences among site categories



## Barium (Ba)

hist(BaData$TMS)
shapiro.test(BaData$TMS)
qqnorm(BaData$TMS)
qqline(BaData$TMS)
## Passes Shapiro (p = 0.834) and visually appears normal.
bartlett.test(BaData$TMS, BaData$Region)
## Passes Bartlett (p = 0.229)
tapply(BaData$TMS, BaData$Region, mean)
tapply(BaData$TMS, BaData$Region, sd)
t.test(BaData$TMS ~ BaData$Region)
## TMSs are similar in Amazonia (-0.238 +/- 0.216) and Canada (-0.305 +/- 0.155)
## (t(25.4) = -0.97, p = 0.342)
bartlett.test(BaData$TMS, BaData$HabitatType)
## Passes Bartlett (p = 0.996)
tapply(BaData$TMS, BaData$HabitatType, mean)
tapply(BaData$TMS, BaData$HabitatType, sd)
t.test(BaData$TMS ~ BaData$HabitatType)
## TMSs are similar between oxbow lakes (-0.266 +/- 0.191) and streams (-0.277 +/- 0.191)
## (t(28.0) = 0.16, p = 0.876)

tapply(BaData$TMS, list(BaData$Region, BaData$HabitatType), mean)
tapply(BaData$TMS, list(BaData$Region, BaData$HabitatType), sd)
kruskal.test(BaData$TMS ~ BaData$SiteCategory)
dunn.test(BaData$TMS, BaData$SiteCategory)
## No differences



## Cadmium (Cd)

hist(CdData$TMS)
shapiro.test(CdData$TMS)
qqnorm(CdData$TMS)
qqline(CdData$TMS)
## Passes Shapiro (p = 0.154) and visually appears mostly normal.
bartlett.test(CdData$TMS, CdData$Region)
## Passes Bartlett (p = 0.180)
tapply(CdData$TMS, CdData$Region, mean)
tapply(CdData$TMS, CdData$Region, sd)
t.test(CdData$TMS ~ CdData$Region)
## TMSs are similar in Amazonia (-0.279 +/- 0.188) and Canada (-0.209 +/- 0.130)
## (t(24.9) = 1.19, p = 0.246)
bartlett.test(CdData$TMS, CdData$HabitatType)
## Passes Bartlett (p = 0.527)
tapply(CdData$TMS, CdData$HabitatType, mean)
tapply(CdData$TMS, CdData$HabitatType, sd)
t.test(CdData$TMS ~ CdData$HabitatType)
## TMSs are similar between oxbow lakes (-0.263 +/- 0.150) and streams (-0.224 +/- 0.178)
## (t(27.2) = -0.64, p = 0.529)

tapply(CdData$TMS, list(CdData$Region, CdData$HabitatType), mean)
tapply(CdData$TMS, list(CdData$Region, CdData$HabitatType), sd)
kruskal.test(CdData$TMS ~ CdData$SiteCategory)
dunn.test(CdData$TMS, CdData$SiteCategory)
## No differences



## Cobalt (Co)

hist(CoData$TMS)
shapiro.test(CoData$TMS)
qqnorm(CoData$TMS)
qqline(CoData$TMS)
## Fails Shapiro (p < 0.001) and does not appear normal.
bartlett.test(CoData$TMS, CoData$Region)
## Passes Bartlett (p = 0.535)
tapply(CoData$TMS, CoData$Region, mean)
tapply(CoData$TMS, CoData$Region, sd)
t.test(CoData$TMS ~ CoData$Region)
## TMSs are similar in Amazonia (-0.192 +/- 0.133) and Canada (-0.174 +/- 0.157)
## (t(27.2) = 0.33, p = 0.741)
bartlett.test(CoData$TMS, CoData$HabitatType)
## Passes Bartlett (p = 0.292)
tapply(CoData$TMS, CoData$HabitatType, mean)
tapply(CoData$TMS, CoData$HabitatType, sd)
t.test(CoData$TMS ~ CoData$HabitatType)
## TMSs are similar between oxbow lakes (-0.193 +/- 0.165) and streams (-0.174 +/- 0.123)
## (t(26.0) = -0.36, p = 0.725)

tapply(CoData$TMS, list(CoData$Region, CoData$HabitatType), mean)
tapply(CoData$TMS, list(CoData$Region, CoData$HabitatType), sd)
kruskal.test(CoData$TMS ~ CoData$SiteCategory)
dunn.test(CoData$TMS, CoData$SiteCategory)
## No differences



## Copper (Cu)

hist(CuData$TMS)
shapiro.test(CuData$TMS)
qqnorm(CuData$TMS)
qqline(CuData$TMS)
## Fails Shapiro (p = 0.011) but visually appears normal other than one outlier.
bartlett.test(CuData$TMS, CuData$Region)
## Fails Bartlett (p = 0.003)
tapply(CuData$TMS, CuData$Region, mean)
tapply(CuData$TMS, CuData$Region, sd)
wilcox.test(CuData$TMS ~ CuData$Region)
## TMSs are similar in Amazonia (-0.315 +/- 0.280) and Canada (-0.144 +/- 0.121)
## (W = 150, p = 0.126)
bartlett.test(CuData$TMS, CuData$HabitatType)
## Passes Bartlett (p = 0.196)
tapply(CuData$TMS, CuData$HabitatType, mean)
tapply(CuData$TMS, CuData$HabitatType, sd)
t.test(CuData$TMS ~ CuData$HabitatType)
## TMSs are similar between oxbow lakes (-0.208 +/- 0.189) and streams (-0.251 +/- 0.269)
## (t(25.1) = 0.50, p = 0.620)

tapply(CuData$TMS, list(CuData$Region, CuData$HabitatType), mean)
tapply(CuData$TMS, list(CuData$Region, CuData$HabitatType), sd)
kruskal.test(CuData$TMS ~ CuData$SiteCategory)
dunn.test(CuData$TMS, CuData$SiteCategory)
## No differences



## Iron (Fe)

hist(FeData$TMS)
shapiro.test(FeData$TMS)
qqnorm(FeData$TMS)
qqline(FeData$TMS)
## Passes Shapiro (p = 0.683) and visually appears normal.
bartlett.test(FeData$TMS, FeData$Region)
## Passes Bartlett (p = 0.475)
tapply(FeData$TMS, FeData$Region, mean)
tapply(FeData$TMS, FeData$Region, sd)
t.test(FeData$TMS ~ FeData$Region)
## TMSs are similar in Amazonia (-0.186 +/- 0.149) and Canada (-0.229 +/- 0.122)
## (t(27.0) = -0.86, p = 0.396)
bartlett.test(FeData$TMS, FeData$HabitatType)
## Passes Bartlett (p = 0.719)
tapply(FeData$TMS, FeData$HabitatType, mean)
tapply(FeData$TMS, FeData$HabitatType, sd)
t.test(FeData$TMS ~ FeData$HabitatType)
## TMSs are similar between oxbow lakes (-0.187 +/- 0.143) and streams (-0.228 +/- 0.130)
## (t(27.7) = 0.82, p = 0.418)

tapply(FeData$TMS, list(FeData$Region, FeData$HabitatType), mean)
tapply(FeData$TMS, list(FeData$Region, FeData$HabitatType), sd)
kruskal.test(FeData$TMS ~ FeData$SiteCategory)
dunn.test(FeData$TMS, FeData$SiteCategory)
## No differences



## Mercury (Hg)

hist(HgData$TMS)
shapiro.test(HgData$TMS)
qqnorm(HgData$TMS)
qqline(HgData$TMS)
## Barely fails Shapiro (p = 0.046) and mostly seems normal, but very close.
bartlett.test(HgData$TMS, HgData$Region)
## Passes Bartlett (p = 0.491)
tapply(HgData$TMS, HgData$Region, mean)
tapply(HgData$TMS, HgData$Region, sd)
t.test(HgData$TMS ~ HgData$Region)
## TMSs are higher in Amazonia (0.206 +/- 0.056) than in Canada (0.137 +/- 0.067)
## (t(27.1) = -3.04, p = 0.005)
bartlett.test(HgData$TMS, HgData$HabitatType)
## Passes Bartlett (p = 0.293)
tapply(HgData$TMS, HgData$HabitatType, mean)
tapply(HgData$TMS, HgData$HabitatType, sd)
t.test(HgData$TMS ~ HgData$HabitatType)
## TMSs are greater in oxbow lakes (0.204 +/- 0.053) than in streams (0.139 +/- 0.071)
## (t(26.0) = 2.81, p = 0.009)

tapply(HgData$TMS, list(HgData$Region, HgData$HabitatType), mean)
tapply(HgData$TMS, list(HgData$Region, HgData$HabitatType), sd)
kruskal.test(HgData$TMS ~ HgData$SiteCategory)
dunn.test(HgData$TMS, HgData$SiteCategory)
## TMSs are higher in tropical lakes than in all other site types



## Manganese (Mn)

hist(MnData$TMS)
shapiro.test(MnData$TMS)
qqnorm(MnData$TMS)
qqline(MnData$TMS)
## Passes Shapiro (p = 0.653) and visually appears normal.
bartlett.test(MnData$TMS, MnData$Region)
## Passes Bartlett (p = 0.825)
tapply(MnData$TMS, MnData$Region, mean)
tapply(MnData$TMS, MnData$Region, sd)
t.test(MnData$TMS ~ MnData$Region)
## TMSs are similar in Amazonia (-0.222 +/- 0.162) and Canada (-0.281 +/- 0.153)
## (t(27.9) = -1.03, p = 0.311)
bartlett.test(MnData$TMS, MnData$HabitatType)
## Passes Bartlett (p = 0.719)
tapply(MnData$TMS, MnData$HabitatType, mean)
tapply(MnData$TMS, MnData$HabitatType, sd)
t.test(MnData$TMS ~ MnData$HabitatType)
## TMSs are similar between oxbow lakes (-0.219 +/- 0.149) and streams (-0.285 +/- 0.164)
## (t(27.7) = 1.15, p = 0.258)

tapply(MnData$TMS, list(MnData$Region, MnData$HabitatType), mean)
tapply(MnData$TMS, list(MnData$Region, MnData$HabitatType), sd)
kruskal.test(MnData$TMS ~ MnData$SiteCategory)
dunn.test(MnData$TMS, MnData$SiteCategory)
## No differences



## Nickel (Ni)

hist(NiData$TMS)
shapiro.test(NiData$TMS)
qqnorm(NiData$TMS)
qqline(NiData$TMS)
## Passes Shapiro (p = 0.232) and visually appears mostly normal.
bartlett.test(NiData$TMS, NiData$Region)
## Passes Bartlett (p = 0.484)
tapply(NiData$TMS, NiData$Region, mean)
tapply(NiData$TMS, NiData$Region, sd)
t.test(NiData$TMS ~ NiData$Region)
## TMSs are similar in Amazonia (-0.172 +/- 0.177) and Canada (-0.285 +/- 0.146)
## (t(27.0) = -1.89, p = 0.069)
bartlett.test(NiData$TMS, NiData$HabitatType)
## Passes Bartlett (p = 0.531)
tapply(NiData$TMS, NiData$HabitatType, mean)
tapply(NiData$TMS, NiData$HabitatType, sd)
t.test(NiData$TMS ~ NiData$HabitatType)
## TMSs are similar between oxbow lakes (-0.223 +/- 0.187) and streams (-0.234 +/- 0.157)
## (t(27.2) = 0.18, p = 0.857)

tapply(NiData$TMS, list(NiData$Region, NiData$HabitatType), mean)
tapply(NiData$TMS, list(NiData$Region, NiData$HabitatType), sd)
kruskal.test(NiData$TMS ~ NiData$SiteCategory)
dunn.test(NiData$TMS, NiData$SiteCategory)
## No differences



## Lead (Pb)

hist(PbData$TMS)
shapiro.test(PbData$TMS)
qqnorm(PbData$TMS)
qqline(PbData$TMS)
## Passes Shapiro (p = 0.791) and visually appears normal.
bartlett.test(PbData$TMS, PbData$Region)
## Passes Bartlett (p = 0.412)
tapply(PbData$TMS, PbData$Region, mean)
tapply(PbData$TMS, PbData$Region, sd)
t.test(PbData$TMS ~ PbData$Region)
## TMSs are higher in Amazonia (-0.172 +/- 0.223) than in Canada (-0.391 +/- 0.279)
## (t(26.7) = -2.38, p = 0.025)
bartlett.test(PbData$TMS, PbData$HabitatType)
## Passes Bartlett (p = 0.920)
tapply(PbData$TMS, PbData$HabitatType, mean)
tapply(PbData$TMS, PbData$HabitatType, sd)
t.test(PbData$TMS ~ PbData$HabitatType)
## TMSs are similar between oxbow lakes (-0.198 +/- 0.260) and streams (-0.365 +/- 0.267)
## (t(28.0) = 1.74, p = 0.094)

tapply(PbData$TMS, list(PbData$Region, PbData$HabitatType), mean)
tapply(PbData$TMS, list(PbData$Region, PbData$HabitatType), sd)
kruskal.test(PbData$TMS ~ PbData$SiteCategory)
dunn.test(PbData$TMS, PbData$SiteCategory)
## TMSs in Canadian streams were lower than tropical sites



## Selenium (Se)

hist(SeData$TMS)
shapiro.test(SeData$TMS)
qqnorm(SeData$TMS)
qqline(SeData$TMS)
## Passes Shapiro (p = 0.227) and visually appears mostly normal.
bartlett.test(SeData$TMS, SeData$Region)
## Passes Bartlett (p = 0.413)
tapply(SeData$TMS, SeData$Region, mean)
tapply(SeData$TMS, SeData$Region, sd)
t.test(SeData$TMS ~ SeData$Region)
## TMSs are similar in Amazonia (0.013 +/- 0.115) and Canada (-0.017 +/- 0.092)
## (t(26.7) = -0.79, p = 0.439)
bartlett.test(SeData$TMS, SeData$HabitatType)
## Passes Bartlett (p = 0.236)
tapply(SeData$TMS, SeData$HabitatType, mean)
tapply(SeData$TMS, SeData$HabitatType, sd)
t.test(SeData$TMS ~ SeData$HabitatType)
## TMSs are similar between oxbow lakes (0.022 +/- 0.085) and streams (-0.027 +/- 0.117)
## (t(25.5) = 1.32, p = 0.200)

tapply(SeData$TMS, list(SeData$Region, SeData$HabitatType), mean)
tapply(SeData$TMS, list(SeData$Region, SeData$HabitatType), sd)
kruskal.test(SeData$TMS ~ SeData$SiteCategory)
dunn.test(SeData$TMS, SeData$SiteCategory)
## No differences



## Strontium (Sr)

hist(SrData$TMS)
shapiro.test(SrData$TMS)
qqnorm(SrData$TMS)
qqline(SrData$TMS)
## Passes Shapiro (p = 0.602) and visually appears normal.
bartlett.test(SrData$TMS, SrData$Region)
## Passes Bartlett (p = 0.277)
tapply(SrData$TMS, SrData$Region, mean)
tapply(SrData$TMS, SrData$Region, sd)
t.test(SrData$TMS ~ SrData$Region)
## TMSs are higher in Amazonia (-0.053 +/- 0.187) than in Canada (-0.186 +/- 0.139)
## (t(25.8) = -2.21, p = 0.036)
bartlett.test(SrData$TMS, SrData$HabitatType)
## Passes Bartlett (p = 0.826)
tapply(SrData$TMS, SrData$HabitatType, mean)
tapply(SrData$TMS, SrData$HabitatType, sd)
t.test(SrData$TMS ~ SrData$HabitatType)
## TMSs are similar between oxbow lakes (-0.171 +/- 0.165) and streams (-0.068 +/- 0.176)
## (t(27.9) = -1.67, p = 0.107)

tapply(SrData$TMS, list(SrData$Region, SrData$HabitatType), mean)
tapply(SrData$TMS, list(SrData$Region, SrData$HabitatType), sd)
kruskal.test(SrData$TMS ~ SrData$SiteCategory)
dunn.test(SrData$TMS, SrData$SiteCategory)
## TMSs in tropical streams are higher than Canadian sites



## Titanium (Ti)

hist(TiData$TMS)
shapiro.test(TiData$TMS)
qqnorm(TiData$TMS)
qqline(TiData$TMS)
## Passes Shapiro (p = 0.110) and visually appears mostly normal.
bartlett.test(TiData$TMS, TiData$Region)
## Passes Bartlett (p = 0.209)
tapply(TiData$TMS, TiData$Region, mean)
tapply(TiData$TMS, TiData$Region, sd)
t.test(TiData$TMS ~ TiData$Region)
## TMSs are higher in Amazonia (-0.066 +/- 0.114) than in Canada (-0.195 +/- 0.161)
## (t(25.2) = -2.53, p = 0.018)
bartlett.test(TiData$TMS, TiData$HabitatType)
## Fails Bartlett (p = 0.009)
tapply(TiData$TMS, TiData$HabitatType, mean)
tapply(TiData$TMS, TiData$HabitatType, sd)
wilcox.test(TiData$TMS ~ TiData$HabitatType)
## TMSs are similar between oxbow lakes (-0.089 +/- 0.091) and streams (-0.171 +/- 0.190)
## (W = 144, p = 0.202)

tapply(TiData$TMS, list(TiData$Region, TiData$HabitatType), mean)
tapply(TiData$TMS, list(TiData$Region, TiData$HabitatType), sd)
kruskal.test(TiData$TMS ~ TiData$SiteCategory)
dunn.test(TiData$TMS, TiData$SiteCategory)
## TMSs are higher in tropical lakes than Canadian streams



## Thallium (Tl)

hist(TlData$TMS)
shapiro.test(TlData$TMS)
qqnorm(TlData$TMS)
qqline(TlData$TMS)
## Barely passes Shapiro (p = 0.058) and visually appears normal other than one outlier.
bartlett.test(TlData$TMS, TlData$Region)
## Passes Bartlett (p = 0.202)
tapply(TlData$TMS, TlData$Region, mean)
tapply(TlData$TMS, TlData$Region, sd)
t.test(TlData$TMS ~ TlData$Region)
## TMSs are similar in Amazonia (-0.028 +/- 0.173) and Canada (-0.113 +/- 0.122)
## (t(25.1) = -1.55, p = 0.134)
bartlett.test(TlData$TMS, TlData$HabitatType)
## Passes Bartlett (p = 0.529)
tapply(TlData$TMS, TlData$HabitatType, mean)
tapply(TlData$TMS, TlData$HabitatType, sd)
t.test(TlData$TMS ~ TlData$HabitatType)
## TMSs are similar between oxbow lakes (-0.069 +/- 0.142) and streams (-0.072 +/- 0.169)
## (t(27.2) = 0.06, p = 0.953)

tapply(TlData$TMS, list(TlData$Region, TlData$HabitatType), mean)
tapply(TlData$TMS, list(TlData$Region, TlData$HabitatType), sd)
kruskal.test(TlData$TMS ~ TlData$SiteCategory)
dunn.test(TlData$TMS, TlData$SiteCategory)
## No differences



## Uranium

hist(UData$TMS)
shapiro.test(UData$TMS)
qqnorm(UData$TMS)
qqline(UData$TMS)
## Passes Shapiro (p = 0.272) and visually appears mostly normal.
bartlett.test(UData$TMS, UData$Region)
## Barely passes Bartlett (p = 0.054)
tapply(UData$TMS, UData$Region, mean)
tapply(UData$TMS, UData$Region, sd)
t.test(UData$TMS ~ UData$Region)
## TMSs are similar in Amazonia (-0.353 +/- 0.312) and Canada (-0.342 +/- 0.182)
## (t(22.6) = 0.12, p = 0.902)
bartlett.test(UData$TMS, UData$HabitatType)
## Passes Bartlett (p = 0.733)
tapply(UData$TMS, UData$HabitatType, mean)
tapply(UData$TMS, UData$HabitatType, sd)
t.test(UData$TMS ~ UData$HabitatType)
## TMSs are higher in oxbow lakes (-0.244 +/- 0.221) than in streams (-0.451 +/- 0.243)
## (t(27.8) = 2.43, p = 0.022)

tapply(UData$TMS, list(UData$Region, UData$HabitatType), mean)
tapply(UData$TMS, list(UData$Region, UData$HabitatType), sd)
kruskal.test(UData$TMS ~ UData$SiteCategory)
dunn.test(UData$TMS, UData$SiteCategory)
## TMSs are higher in tropical lakes than tropical streams



## Vanadium

hist(VData$TMS)
shapiro.test(VData$TMS)
qqnorm(VData$TMS)
qqline(VData$TMS)
## Fails Shapiro (p < 0.001) and doesn't really appear normal.
bartlett.test(VData$TMS, VData$Region)
## Fails Bartlett (p = 0.001)
tapply(VData$TMS, VData$Region, mean)
tapply(VData$TMS, VData$Region, sd)
wilcox.test(VData$TMS ~ VData$Region)
## TMSs are similar in Amazonia (-0.286 +/- 0.310) and Canada (-0.247 +/- 0.123)
## (W = 95.5, p = 0.494)
bartlett.test(VData$TMS, VData$HabitatType)
## Fails Bartlett (p = 0.007)
tapply(VData$TMS, VData$HabitatType, mean)
tapply(VData$TMS, VData$HabitatType, sd)
wilcox.test(VData$TMS ~ VData$HabitatType)
## TMSs are greater in oxbow lakes (-0.182 +/- 0.132) than in streams (-0.351 +/- 0.282)
## (W = 162, p = 0.042)

tapply(VData$TMS, list(VData$Region, VData$HabitatType), mean)
tapply(VData$TMS, list(VData$Region, VData$HabitatType), sd)
kruskal.test(VData$TMS ~ VData$SiteCategory)
dunn.test(VData$TMS, VData$SiteCategory)
## TMSs are higher in Canadian lakes than Canadian streams



## Zinc

hist(ZnData$TMS)
shapiro.test(ZnData$TMS)
qqnorm(ZnData$TMS)
qqline(ZnData$TMS)
## Passes Shapiro (p = 0.134) and visually appears mostly normal.
bartlett.test(ZnData$TMS, ZnData$Region)
## Passes Bartlett (p = 0.523)
tapply(ZnData$TMS, ZnData$Region, mean)
tapply(ZnData$TMS, ZnData$Region, sd)
t.test(ZnData$TMS ~ ZnData$Region)
## TMSs are similar in Amazonia (-0.081 +/- 0.093) and Canada (-0.056 +/- 0.111)
## (t(27.2) = 0.68, p = 0.505)
bartlett.test(ZnData$TMS, ZnData$HabitatType)
## Passes Bartlett (p = 0.180)
tapply(ZnData$TMS, ZnData$HabitatType, mean)
tapply(ZnData$TMS, ZnData$HabitatType, sd)
t.test(ZnData$TMS ~ ZnData$HabitatType)
## TMSs are similar between oxbow lakes (-0.055 +/- 0.119) and streams (-0.082 +/- 0.082)
## (t(24.9) = 0.72, p = 0.477)

tapply(ZnData$TMS, list(ZnData$Region, ZnData$HabitatType), mean)
tapply(ZnData$TMS, list(ZnData$Region, ZnData$HabitatType), sd)
kruskal.test(ZnData$TMS ~ ZnData$SiteCategory)
dunn.test(ZnData$TMS, ZnData$SiteCategory)
## No differences






## Exploratory Linear Regressions to Construct SEM


## In all regressions below, only the statistically significant ones are commented.



## Aluminum

summary(lm(AlData$TMS ~ AlData$ScaledSize))
summary(lm(AlData$TMS ~ AlData$MeanTSI))
## Positive relationship with MeanTSI (p = 0.016)
summary(lm(AlData$TMS ~ AlData$MeanTRA))
summary(lm(AlData$TMS ~ log10(AlData$Ambient)))

summary(lm(AlData$TMS ~ AlData$NRange))
summary(lm(AlData$TMS ~ AlData$CRange))
summary(lm(AlData$TMS ~ AlData$CNA))
summary(lm(AlData$TMS ~ AlData$SizeStructureSlope))
## Negative relationship with Size Structure (p = 0.027)



## Arsenic

summary(lm(AsData$TMS ~ AsData$ScaledSize))
summary(lm(AsData$TMS ~ AsData$MeanTSI))
summary(lm(AsData$TMS ~ AsData$MeanTRA))
summary(lm(AsData$TMS ~ log10(AsData$Ambient)))

summary(lm(AsData$TMS ~ AsData$NRange))
summary(lm(AsData$TMS ~ AsData$CRange))
summary(lm(AsData$TMS ~ AsData$CNA))
summary(lm(AsData$TMS ~ AsData$SizeStructureSlope))



## Barium

summary(lm(BaData$TMS ~ BaData$ScaledSize))
summary(lm(BaData$TMS ~ BaData$MeanTSI))
summary(lm(BaData$TMS ~ BaData$MeanTRA))
summary(lm(BaData$TMS ~ log10(BaData$Ambient)))

summary(lm(BaData$TMS ~ BaData$NRange))
summary(lm(BaData$TMS ~ BaData$CRange))
summary(lm(BaData$TMS ~ BaData$CNA))
summary(lm(BaData$TMS ~ BaData$SizeStructureSlope))



## Cadmium

summary(lm(CdData$TMS ~ CdData$ScaledSize))
summary(lm(CdData$TMS ~ CdData$MeanTSI))
## Negative relationship with MeanTSI (p = 0.046)
summary(lm(CdData$TMS ~ CdData$MeanTRA))
summary(lm(CdData$TMS ~ log10(CdData$Ambient)))

summary(lm(CdData$TMS ~ CdData$NRange))
summary(lm(CdData$TMS ~ CdData$CRange))
summary(lm(CdData$TMS ~ CdData$CNA))
summary(lm(CdData$TMS ~ CdData$SizeStructureSlope))
## Positive relationship with SSS (p = 0.017)



## Cobalt

summary(lm(CoData$TMS ~ CoData$ScaledSize))
summary(lm(CoData$TMS ~ CoData$MeanTSI))
summary(lm(CoData$TMS ~ CoData$MeanTRA))
summary(lm(CoData$TMS ~ log10(CoData$Ambient)))

summary(lm(CoData$TMS ~ CoData$NRange))
summary(lm(CoData$TMS ~ CoData$CRange))
summary(lm(CoData$TMS ~ CoData$CNA))
summary(lm(CoData$TMS ~ CoData$SizeStructureSlope))



## Copper

summary(lm(CuData$TMS ~ CuData$ScaledSize))
summary(lm(CuData$TMS ~ CuData$MeanTSI))
summary(lm(CuData$TMS ~ CuData$MeanTRA))
## Negative relationship with MeanTRA (p = 0.030)
summary(lm(CuData$TMS ~ log10(CuData$Ambient)))

summary(lm(CuData$TMS ~ CuData$NRange))
summary(lm(CuData$TMS ~ CuData$CRange))
summary(lm(CuData$TMS ~ CuData$CNA))
summary(lm(CuData$TMS ~ CuData$SizeStructureSlope))
## Positive relationship with SSS (p = 0.007)



## Iron

summary(lm(FeData$TMS ~ FeData$ScaledSize))
summary(lm(FeData$TMS ~ FeData$MeanTSI))
summary(lm(FeData$TMS ~ FeData$MeanTRA))
summary(lm(FeData$TMS ~ log10(FeData$Ambient)))

summary(lm(FeData$TMS ~ FeData$NRange))
summary(lm(FeData$TMS ~ FeData$CRange))
summary(lm(FeData$TMS ~ FeData$CNA))
summary(lm(FeData$TMS ~ FeData$SizeStructureSlope))



## Mercury

summary(lm(HgData$TMS ~ HgData$ScaledSize))
summary(lm(HgData$TMS ~ HgData$MeanTSI))
## Positive relationship with MeanTSI (p < 0.001)
summary(lm(HgData$TMS ~ HgData$MeanTRA))
summary(lm(HgData$TMS ~ log10(HgData$Ambient)))

summary(lm(HgData$TMS ~ HgData$NRange))
summary(lm(HgData$TMS ~ HgData$CRange))
summary(lm(HgData$TMS ~ HgData$CNA))
summary(lm(HgData$TMS ~ HgData$SizeStructureSlope))



## Manganese

summary(lm(MnData$TMS ~ MnData$ScaledSize))
summary(lm(MnData$TMS ~ MnData$MeanTSI))
summary(lm(MnData$TMS ~ MnData$MeanTRA))
summary(lm(MnData$TMS ~ log10(MnData$Ambient)))

summary(lm(MnData$TMS ~ MnData$NRange))
summary(lm(MnData$TMS ~ MnData$CRange))
summary(lm(MnData$TMS ~ MnData$CNA))
summary(lm(MnData$TMS ~ MnData$SizeStructureSlope))



## Nickel

summary(lm(NiData$TMS ~ NiData$ScaledSize))
summary(lm(NiData$TMS ~ NiData$MeanTSI))
summary(lm(NiData$TMS ~ NiData$MeanTRA))
summary(lm(NiData$TMS ~ log10(NiData$Ambient)))

summary(lm(NiData$TMS ~ NiData$NRange))
summary(lm(NiData$TMS ~ NiData$CRange))
## Positive relationship with CRange (p = 0.049)
summary(lm(NiData$TMS ~ NiData$CNA))
## Positive relationship with CNA (p = 0.023)
summary(lm(NiData$TMS ~ NiData$SizeStructureSlope))



## Lead

summary(lm(PbData$TMS ~ PbData$ScaledSize))
summary(lm(PbData$TMS ~ PbData$MeanTSI))
## Positive relationship with MeanTSI (p = 0.028)
summary(lm(PbData$TMS ~ PbData$MeanTRA))
summary(lm(PbData$TMS ~ log10(PbData$Ambient)))

summary(lm(PbData$TMS ~ PbData$NRange))
summary(lm(PbData$TMS ~ PbData$CRange))
summary(lm(PbData$TMS ~ PbData$CNA))
summary(lm(PbData$TMS ~ PbData$SizeStructureSlope))



## Selenium

summary(lm(SeData$TMS ~ SeData$ScaledSize))
summary(lm(SeData$TMS ~ SeData$MeanTSI))
summary(lm(SeData$TMS ~ SeData$MeanTRA))
summary(lm(SeData$TMS ~ log10(SeData$Ambient)))

summary(lm(SeData$TMS ~ SeData$NRange))
summary(lm(SeData$TMS ~ SeData$CRange))
summary(lm(SeData$TMS ~ SeData$CNA))
summary(lm(SeData$TMS ~ SeData$SizeStructureSlope))



## Strontium

summary(lm(SrData$TMS ~ SrData$ScaledSize))
summary(lm(SrData$TMS ~ SrData$MeanTSI))
summary(lm(SrData$TMS ~ SrData$MeanTRA))
summary(lm(SrData$TMS ~ log10(SrData$Ambient)))
## Negative relationship with Ambient (p = 0.025)

summary(lm(SrData$TMS ~ SrData$NRange))
summary(lm(SrData$TMS ~ SrData$CRange))
summary(lm(SrData$TMS ~ SrData$CNA))
summary(lm(SrData$TMS ~ SrData$SizeStructureSlope))



## Titanium

summary(lm(TiData$TMS ~ TiData$ScaledSize))
summary(lm(TiData$TMS ~ TiData$MeanTSI))
## Positive relationship with MeanTSI (p = 0.009)
summary(lm(TiData$TMS ~ TiData$MeanTRA))
summary(lm(TiData$TMS ~ log10(TiData$Ambient)))
## Significant positive relationship, but ambient concentrations are only
## available at 11 of the 30 sites (all in Canada)

summary(lm(TiData$TMS ~ TiData$NRange))
summary(lm(TiData$TMS ~ TiData$CRange))
summary(lm(TiData$TMS ~ TiData$CNA))
summary(lm(TiData$TMS ~ TiData$SizeStructureSlope))



## Thallium

summary(lm(TlData$TMS ~ TlData$ScaledSize))
summary(lm(TlData$TMS ~ TlData$MeanTSI))
summary(lm(TlData$TMS ~ TlData$MeanTRA))
summary(lm(TlData$TMS ~ log10(TlData$Ambient)))

summary(lm(TlData$TMS ~ TlData$NRange))
summary(lm(TlData$TMS ~ TlData$CRange))
summary(lm(TlData$TMS ~ TlData$CNA))
summary(lm(TlData$TMS ~ TlData$SizeStructureSlope))



## Uranium

summary(lm(UData$TMS ~ UData$ScaledSize))
summary(lm(UData$TMS ~ UData$MeanTSI))
summary(lm(UData$TMS ~ UData$MeanTRA))
summary(lm(UData$TMS ~ log10(UData$Ambient)))

summary(lm(UData$TMS ~ UData$NRange))
summary(lm(UData$TMS ~ UData$CRange))
summary(lm(UData$TMS ~ UData$CNA))
summary(lm(UData$TMS ~ UData$SizeStructureSlope))



## Vanadium

summary(lm(VData$TMS ~ VData$ScaledSize))
summary(lm(VData$TMS ~ VData$MeanTSI))
summary(lm(VData$TMS ~ VData$MeanTRA))
summary(lm(VData$TMS ~ log10(VData$Ambient)))

summary(lm(VData$TMS ~ VData$NRange))
summary(lm(VData$TMS ~ VData$CRange))
summary(lm(VData$TMS ~ VData$CNA))
summary(lm(VData$TMS ~ VData$SizeStructureSlope))



## Zinc

summary(lm(ZnData$TMS ~ ZnData$ScaledSize))
summary(lm(ZnData$TMS ~ ZnData$MeanTSI))
summary(lm(ZnData$TMS ~ ZnData$MeanTRA))
summary(lm(ZnData$TMS ~ log10(ZnData$Ambient)))

summary(lm(ZnData$TMS ~ ZnData$NRange))
summary(lm(ZnData$TMS ~ ZnData$CRange))
summary(lm(ZnData$TMS ~ ZnData$CNA))
summary(lm(ZnData$TMS ~ ZnData$SizeStructureSlope))




## Now let's look at ambient trace element concentrations


AmbientColors <- c("dodgerblue3", "deepskyblue", "green4", "green2",
                   "dodgerblue3", "deepskyblue",           "green2",
                   "dodgerblue3", "deepskyblue", "green4", "green2",
                                                           "green2",
                   "dodgerblue3",                          "green2",
                   "dodgerblue3", "deepskyblue",           "green2",
                   "dodgerblue3", "deepskyblue", "green4", "green2",
                   "dodgerblue3", "deepskyblue", "green4", "green2",
                   "dodgerblue3", "deepskyblue", "green4", "green2",
                   "dodgerblue3",                "green4", "green2",
                   "dodgerblue3", "deepskyblue",           "green2",
                   "dodgerblue3", "deepskyblue",           "green2",
                   "dodgerblue3", "deepskyblue",
                                                           "green2",
                   "dodgerblue3", "deepskyblue",           "green2",
                   "dodgerblue3", "deepskyblue",           "green2",
                   "dodgerblue3", "deepskyblue",           "green2")


ElementAmbientPlot <-
  ggplot(TMSData, aes(x = SiteCategory, y = log10(Ambient), fill=SiteCategory)) +
  ## geom_hline(yintercept = 0.25, col="gray85") +
  ## geom_hline(yintercept = 0, col="black", lwd=0.5) +
  ## geom_hline(yintercept = -0.25, col="gray85") +
  ## geom_hline(yintercept = -0.5, col="gray75") +
  ## geom_hline(yintercept = -0.75, col="gray85") +
  ## geom_hline(yintercept = -1, col="gray75") +
  geom_boxplot(position = position_dodge(width = 0.4),
               # fill=AmbientColors
               width=0.6) +
  facet_wrap(~ Element, nrow = 3, ncol = 6) +
  scale_y_continuous(limits=c(-2.5, 3.5), n.breaks=6) +
  labs(title = "",
       x = "",
       y = "",
       fill = "Site Category") +
  theme_bw()

ElementAmbientPlot




#### Now let's test the assumptions of parametric testing for ambient 
## trace element concentration data, and compare concentrations among sites


shapiro.test(log10(AlData$Ambient))
bartlett.test(log10(AlData$Ambient), AlData$SiteCategory)
## Passes both!
kruskal.test(log10(AlData$Ambient) ~ AlData$SiteCategory)
dunn.test(log10(AlData$Ambient), AlData$SiteCategory)


shapiro.test(log10(AsData$Ambient))
bartlett.test(log10(AsData$Ambient), AsData$SiteCategory)
## Passes both!
kruskal.test(log10(AsData$Ambient) ~ AsData$SiteCategory)
dunn.test(log10(AsData$Ambient), AsData$SiteCategory)


shapiro.test(log10(BaData$Ambient))
bartlett.test(log10(BaData$Ambient), BaData$SiteCategory)
## Passes both!
kruskal.test(log10(BaData$Ambient) ~ BaData$SiteCategory)
dunn.test(log10(BaData$Ambient), BaData$SiteCategory)


shapiro.test(log10(CoData$Ambient))
bartlett.test(log10(CoData$Ambient), CoData$SiteCategory)
## Passes both!
kruskal.test(log10(CoData$Ambient) ~ CoData$SiteCategory)
dunn.test(log10(CoData$Ambient), CoData$SiteCategory)


shapiro.test(log10(CuData$Ambient))
bartlett.test(log10(CuData$Ambient), CuData$SiteCategory)
## Passes both!
kruskal.test(log10(CuData$Ambient) ~ CuData$SiteCategory)
dunn.test(log10(CuData$Ambient), CuData$SiteCategory)


shapiro.test(log10(FeData$Ambient))
bartlett.test(log10(FeData$Ambient), FeData$SiteCategory)
## Passes both!
kruskal.test(log10(FeData$Ambient) ~ FeData$SiteCategory)
dunn.test(log10(FeData$Ambient), FeData$SiteCategory)


shapiro.test(log10(MnData$Ambient))
bartlett.test(log10(MnData$Ambient), MnData$SiteCategory)
## Fails Bartlett (p = 0.002)
kruskal.test(log10(MnData$Ambient) ~ MnData$SiteCategory)
dunn.test(log10(MnData$Ambient), MnData$SiteCategory)


shapiro.test(log10(NiData$Ambient))
bartlett.test(log10(NiData$Ambient), NiData$SiteCategory)
## Passes both!
kruskal.test(log10(NiData$Ambient) ~ NiData$SiteCategory)
dunn.test(log10(NiData$Ambient), NiData$SiteCategory)


shapiro.test(log10(PbData$Ambient))
bartlett.test(log10(PbData$Ambient), PbData$SiteCategory)
## Passes both!
kruskal.test(log10(PbData$Ambient) ~ PbData$SiteCategory)
dunn.test(log10(PbData$Ambient), PbData$SiteCategory)


shapiro.test(log10(SeData$Ambient))
bartlett.test(log10(SeData$Ambient), SeData$SiteCategory)
## Passes both!
kruskal.test(log10(SeData$Ambient) ~ SeData$SiteCategory)
dunn.test(log10(SeData$Ambient), SeData$SiteCategory)


shapiro.test(log10(SrData$Ambient))
bartlett.test(log10(SrData$Ambient), SrData$SiteCategory)
## Fails both (p's = 0.003 and 0.021)
kruskal.test(log10(SrData$Ambient) ~ SrData$SiteCategory)
dunn.test(log10(SrData$Ambient), SrData$SiteCategory)


shapiro.test(log10(TiData$Ambient))
bartlett.test(log10(TiData$Ambient), TiData$SiteCategory)
## Fails Shapiro (p = 0.023)
wilcox.test(log10(TiData$Ambient) ~ TiData$SiteCategory)


shapiro.test(log10(UData$Ambient))
bartlett.test(log10(UData$Ambient), UData$SiteCategory)
## Passes both!
kruskal.test(log10(UData$Ambient) ~ UData$SiteCategory)
dunn.test(log10(UData$Ambient), UData$SiteCategory)


shapiro.test(log10(VData$Ambient))
bartlett.test(log10(VData$Ambient), VData$SiteCategory)
## Fails Bartlett (p = 0.018)
kruskal.test(log10(VData$Ambient) ~ VData$SiteCategory)
dunn.test(log10(VData$Ambient), VData$SiteCategory)


shapiro.test(log10(ZnData$Ambient))
bartlett.test(log10(ZnData$Ambient), ZnData$SiteCategory)
## Fails Bartlett (p = 0.016)
kruskal.test(log10(ZnData$Ambient) ~ ZnData$SiteCategory)
dunn.test(log10(ZnData$Ambient), ZnData$SiteCategory)





###### Path Analysis Models


## Because we have ambient concentrations of Al, Fe, and Ni but not Cd, Hg, and Se,
## only models excluding ambient concentrations will be tested for the latter TEs.



# Define the path models


Model1 <- '
  # Direct effects
  TMS               ~ 1
'

Model2 <- '
  # Direct effects
  TMS               ~ Ambient
'

Model3 <- '
  # Direct effects
  TMS               ~ SizeStructureSlope
'

Model4 <- '
  # Direct effects
  TMS               ~ MeanTSI
'

Model5 <- '
  # Direct effects
  TMS               ~ SizeStructureSlope + Ambient
'

Model6 <- '
  # Direct effects
  TMS               ~ MeanTSI + Ambient
'

Model7 <- '
  # Direct effects
  TMS               ~ SizeStructureSlope + MeanTSI
'

Model8 <- '
  # Direct effects
  TMS               ~ SizeStructureSlope + MeanTSI + Ambient
'

Model9 <- '
  # Direct effects
  SizeStructureSlope ~ a*MeanTSI
  TMS               ~ b*SizeStructureSlope

# Indirect effects
indirect := a*b
'

Model10 <- '
  # Direct effects
  SizeStructureSlope ~ a*MeanTSI
  TMS               ~ b*SizeStructureSlope + Ambient
  
# Indirect effects
indirect := a*b
'


Model11 <- '
  # Direct effects
  SizeStructureSlope ~ a*MeanTSI
  TMS               ~ b*SizeStructureSlope + c*MeanTSI
 
  # Indirect and total effects
indirect := a*b
total := c + (a*b)
'

Model12 <- '
  # Direct effects
  SizeStructureSlope ~ a*MeanTSI
  TMS               ~ b*SizeStructureSlope + c*MeanTSI + Ambient
 
  # Indirect and total effects
indirect := a*b
total := c + (a*b)
'




## Now let's fit the path analysis models. To keep the amount of code down, I
## am showing the code for just one trace element, but feel free to copy and
## paste the code and replace the Aluminum subdata set with any other element.


fit_1 <- sem(
  model = Model1,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_2 <- sem(
  model = Model2,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_3 <- sem(
  model = Model3,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_4 <- sem(
  model = Model4,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_5 <- sem(
  model = Model5,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_6 <- sem(
  model = Model6,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_7 <- sem(
  model = Model7,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_8 <- sem(
  model = Model8,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_9 <- sem(
  model = Model9,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_10 <- sem(
  model = Model10,  
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)

fit_11 <- sem(
  model = Model11,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)


fit_12 <- sem(
  model = Model12,      
  data  = AlData,       # Change dataset for each element
  missing = "fiml"      # handle missing data via FIML
)



# Collect fitted models into a list
fits <- list(
  fit_1, fit_2, fit_3, fit_4, fit_5, fit_6,
  fit_7, fit_8, fit_9, fit_10, fit_11, fit_12)


model_names <- c("Model1", "Model2", "Model3", "Model4",
                 "Model5", "Model6", "Model7", "Model8",
                 "Model9", "Model10","Model11","Model12")


## Use the following model_names and fits when using TEs whose ambient
## concentrations were not measured, as this model_names and fits
## excludes models with ambient concentrations

fits <- list(fit_1, fit_3, fit_4, fit_7, fit_9, fit_11)

model_names <- c("Model1", "Model3", "Model4",
                 "Model7", "Model9", "Model11")



# Function to compute adjusted R² for a given dependent variable
adj_r2 <- function(fit, dv) {
  # R² for the DV
  r2 <- inspect(fit, "r2")[dv]
  
  # Sample size
  n <- lavInspect(fit, "nobs")
  
  # Number of predictors for the DV
  pe <- parameterEstimates(fit)
  p <- sum(pe$lhs == dv & pe$op == "~")
  
  # Adjusted R²
  1 - (1 - r2) * (n - 1) / (n - p - 1)
}


# Build comparison table
model_table <- data.frame(
  Model = model_names,
  AIC = sapply(fits, AIC),
  R2_TMS = sapply(fits, function(x) inspect(x, "r2")["TMS"]),
  AdjR2_TMS = sapply(fits, function(x) adj_r2(x, "TMS"))
)


# Add ΔAIC and AIC weights
model_table$Delta_AIC <- model_table$AIC - min(model_table$AIC)

model_table$AIC_weight <- exp(-0.5 * model_table$Delta_AIC) /
  sum(exp(-0.5 * model_table$Delta_AIC))

# Rank models by AIC
model_table <- model_table[order(model_table$AIC), ]

# Print table
print(model_table, row.names = FALSE)




# Check model outputs for a specific model:


summary(fit_7, fit.measures=TRUE, rsquare=TRUE, stand=TRUE)


## Finally, because sem() doesn't allow transformations of 
## independent variables, then if an optimal model contains
## "Ambient", then we should double-check it:

summary(lm(SrData$TMS ~ log10(SrData$Ambient)))



# Path diagram (Optional)
semPaths(
  fit_12,                # Change model name as necessary
  what = "stand",        # standardized coefficients
  style = "ram",         # arrows for regression paths
  residuals = TRUE,      # show residuals for each endogenous variable
  intercepts = FALSE,
  nCharNodes = 0,
  edge.label.cex = 0.9,
  layout = "tree",
  rotation = 2,
  sizeMan = 8)


## Just for bookkeeping, this was the original SEM code before
## switching to path analysis due to the observed variables
## inadequately representing the latent variables:


MedModel <- '
  # Measurement model
  ENV =~ ScaledSize + MeanTSI + MeanTRA
  FWS =~ NRange + CRange + CNA + SizeStructureSlope

  # Structural model
  FWS ~ a*ENV
  TMS ~ b*FWS + c*ENV + Ambient

  # Indirect and total effects
  indirect := a*b
  total := c + (a*b)
'
fit_med <- lavaan::sem(
  model = MedModel,
  data  = AlData,
  missing = "fiml",
  std.lv = TRUE
)
summary(
  fit_med,
  standardized = TRUE,
  fit.measures = TRUE,
  rsquare = TRUE
)



###### Do biological Hg concentrations vary with biological Se concentrations
## at tropical lake sites, and could this explain the anomalous Hg concentrations there?

TLData <- subset(IndivData, SiteCategory == "Tropical Lakes")

plot(log10(TLData$SeleniumPPM)+3, log10(TLData$HgPPB),
     xaxt="n", yaxt="n", xlim=c(1.7, 3.3), ylim=c(1.3, 4),
     xlab = "Selenium concentration (ppb)",
     ylab = "Mercury concentration (ppb)")
xlist = c(50, 100, 200, 500, 1000, 2000)
ylist = c(20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
axis(1, at=log10(xlist),
     labels=c("50", "100", "200", "500", "1,000", "2,000"))
axis(2, at=log10(ylist),
     labels=c("20", "50", "100", "200", "500", "1,000", "2,000", "5,000", "10,000"))
summary(lm(log10(TLData$HgPPB) ~ log10(TLData$SeleniumPPM)))
abline(lm(log10(TLData$HgPPB) ~
            log10(TLData$SeleniumPPM)), col="blue")

## No




###### Do Hg TMSs vary with ambient Se concentrations at all sites?

HgSeData <- read_excel("HgSe Data.xlsx")
attach(HgSeData)


plot(log10(HgSeData$Se), HgSeData$HgTMS,
     xaxt="n", yaxt="n", xlim=c(-1.7, 0.15), ylim=c(-0.05, 0.35),
     xlab = "Selenium concentration (ppb)",
     ylab = "Mercury TMS")
xlist = c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2)
axis(1, at=log10(xlist),
     labels=c("0.02", "0.05", "0.1", "0.2", "0.5", "1", "2"))
axis(2, at=c(-0.05, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35), las=1)
summary(lm(HgSeData$HgTMS ~ log10(HgSeData$Se)))
abline(lm(HgSeData$HgTMS ~ log10(HgSeData$Se)), col="blue")

## Yes



###### Do Hg TMSs correlate with Se TMSs at all sites?

summary(lm(HgData$TMS ~ SeData$TMS))

## No