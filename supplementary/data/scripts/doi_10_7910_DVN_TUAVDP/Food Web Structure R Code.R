
# Environmental Variation in Freshwater Community Structure


## Don't forget to set your working directory first!


library(car)
library(corrplot)
library(devtools)
library(dplyr)
library(dunn.test)
library(ggcorrplot)
library(ggplot2)
library(lattice)
library(lavaan)
library(mgcv)
library(nlme)
library(patchwork)
library(readxl)
library(reporter)
library(SIBER)
library(spdep)
library(stargazer)
library(tidyverse)
library(units)
par(family="sans", las=1, bg=NA, mar=c(5,6,4,1)+.1)



###### Importing and Subsetting Data ######


#### Cumulative Analysis Dataset

AnalysisData2 <- read_excel("Community Structure Analysis Data.xlsx")
AnalysisData <- AnalysisData2[c(1:30),]
attach(AnalysisData)

## Site Info

AnalysisData$Region <- factor(AnalysisData$Region,
                                 levels=c("Canada", "Tropics"))
AnalysisData$HabitatType <- factor(AnalysisData$HabitatType,
                                      levels=c("Oxbow Lake", "Stream"))
AnalysisData$SiteCategory <- factor(AnalysisData$SiteCategory,
                                       levels=c("Canadian Lakes", "Canadian Streams",
                                                "Tropical Lakes", "Tropical Streams"))
AnalysisData$Latitude <- as.numeric(AnalysisData$Latitude)
AnalysisData$Longitude <- as.numeric(AnalysisData$Longitude)
AnalysisData$ScaledSize <- as.numeric(AnalysisData$ScaledSize)
AnalysisData$PfankuchRaw <- as.numeric(AnalysisData$PfankuchRaw)
AnalysisData$MeanTSI <- as.numeric(AnalysisData$MeanTSI)
AnalysisData$MeanTRA <- as.numeric(AnalysisData$MeanTRA)
AnalysisData$d13C_Range <- as.numeric(AnalysisData$d13C_Range)
AnalysisData$d15N_Range <- as.numeric(AnalysisData$d15N_Range)
AnalysisData$SizeStructureSlope_d15N <- as.numeric(AnalysisData$SizeStructureSlope_d15N)
AnalysisData$SizeStructureSlope_TP <- as.numeric(AnalysisData$SizeStructureSlope_TP)
AnalysisData$FCLMax_Final <- as.numeric(AnalysisData$FCLMax_Final)
AnalysisData$Isotope.SE.Area.Lower <- as.numeric(AnalysisData$Isotope.SE.Area.Lower)
AnalysisData$Isotope.SE.Area <- as.numeric(AnalysisData$Isotope.SE.Area)
AnalysisData$Isotope.SE.Area.Upper <- as.numeric(AnalysisData$Isotope.SE.Area.Upper)
AnalysisData$TP.SE.Area.Lower <- as.numeric(AnalysisData$TP.SE.Area.Lower)
AnalysisData$TP.SE.Area <- as.numeric(AnalysisData$TP.SE.Area)
AnalysisData$TP.SE.Area.Upper <- as.numeric(AnalysisData$TP.SE.Area.Upper)

## Data Subsets

LakeAnalysisData <- subset(AnalysisData, HabitatType == "Oxbow Lake")
StreamAnalysisData <- subset(AnalysisData, HabitatType == "Stream")
CanadaAnalysisData <- subset(AnalysisData, Region == "Canada")
TropicsAnalysisData <- subset(AnalysisData, Region == "Tropics")

CLAnalysisData <- subset(AnalysisData, SiteCategory == "Canadian Lakes")
CSAnalysisData <- subset(AnalysisData, SiteCategory == "Canadian Streams")
TLAnalysisData <- subset(AnalysisData, SiteCategory == "Tropical Lakes")
TSAnalysisData <- subset(AnalysisData, SiteCategory == "Tropical Streams")



######## Figures ########



#### NRange



NRange.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=d15N_Range,
                        col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(3.5, 12)) +
  xlab("Scaled Ecosystem Size") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.EcoSize


NRange.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=d15N_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(3.5, 12)) +
  xlab("Mean Trophic State Index") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.TSI


NRange.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=d15N_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(3.5, 12)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.TRA.Linear


NRange.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=d15N_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(3.5, 12)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.TRA.Polynomial


NRange.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=d15N_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(3.5, 12)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.TRA.Loess


NRange.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=d15N_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(3.5, 12)) +
  xlab("Pfankuch Disturbance Score") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.Pfankuch



#### CRange



CRange.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=d13C_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20)) +
  xlab("Scaled Ecosystem Size") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.EcoSize


CRange.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=d13C_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 20)) +
  xlab("Mean Trophic State Index") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.TSI


CRange.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=d13C_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 20)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.TRA.Linear


CRange.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=d13C_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 20)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.TRA.Polynomial


CRange.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=d13C_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 20)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.TRA.Loess


CRange.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=d13C_Range,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(0, 20)) +
  xlab("Pfankuch Disturbance Score") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.Pfankuch



#### Isotopic SEAc



IsoSEAc.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Scaled Ecosystem Size") +
  ylab(expression('Isotopic CNA (\u2030  )'))

IsoSEAc.EcoSize


IsoSEAc.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Mean Trophic State Index") +
  ylab(expression('Isotopic CNA (\u2030  )'))

IsoSEAc.TSI


IsoSEAc.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression('Isotopic CNA (\u2030  )'))

IsoSEAc.TRA.Linear


IsoSEAc.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression('Isotopic CNA (\u2030  )'))

IsoSEAc.TRA.Polynomial


IsoSEAc.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression('Isotopic CNA (\u2030  )'))

IsoSEAc.TRA.Loess


IsoSEAc.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(0, 30)) +
  xlab("Pfankuch Disturbance Score") +
  ylab(expression('Isotopic CNA (\u2030  )'))

IsoSEAc.Pfankuch



#### Isotopic SSS



IsoSSS.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  xlab("Scaled Ecosystem Size") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)'))

IsoSSS.EcoSize


IsoSSS.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  xlab("Mean Trophic State Index") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)'))

IsoSSS.TSI


IsoSSS.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)'))

IsoSSS.TRA.Linear


IsoSSS.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)'))

IsoSSS.TRA.Polynomial


IsoSSS.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  xlab("Mean Total Resource Abundance") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)'))

IsoSSS.TRA.Loess


IsoSSS.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  xlab("Pfankuch Disturbance Score") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)'))

IsoSSS.Pfankuch



#### FCLMax



FCL.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(2, 6)) +
  xlab("Scaled Ecosystem Size") +
  ylab("Max Food Chain Length")

FCL.EcoSize


FCL.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(2, 6)) +
  xlab("Mean Trophic State Index") +
  ylab("Max Food Chain Length")

FCL.TSI


FCL.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(2, 6)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Max Food Chain Length")

FCL.TRA.Linear


FCL.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(2, 6)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Max Food Chain Length")

FCL.TRA.Polynomial


FCL.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(2, 6)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Max Food Chain Length")

FCL.TRA.Loess


FCL.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(2, 6)) +
  xlab("Pfankuch Disturbance Score") +
  ylab("Max Food Chain Length")

FCL.Pfankuch



#### Ecological SEAc



EcoSEAc.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  xlab("Scaled Ecosystem Size") +
  ylab("Ecological CNA (TP %)")

EcoSEAc.EcoSize


EcoSEAc.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  xlab("Mean Trophic State Index") +
  ylab("Ecological CNA (TP %)")

EcoSEAc.TSI


EcoSEAc.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 100)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Ecological CNA (TP %)")

EcoSEAc.TRA.Linear


EcoSEAc.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 100)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Ecological CNA (TP %)")

EcoSEAc.TRA.Polynomial


EcoSEAc.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(0, 100)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Ecological CNA (TP %)")

EcoSEAc.TRA.Loess


EcoSEAc.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(0, 100)) +
  xlab("Pfankuch Disturbance Score") +
  ylab("Ecological CNA (TP %)")

EcoSEAc.Pfankuch



#### Ecological SSS



EcoSSS.EcoSize <-
  ggplot(AnalysisData, aes(x=ScaledSize, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-1, 1)) +
  xlab("Scaled Ecosystem Size") +
  ylab("Ecological SSS (TP / 100 mm)")

EcoSSS.EcoSize


EcoSSS.TSI <-
  ggplot(AnalysisData, aes(x=MeanTSI, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-1, 1)) +
  xlab("Mean Trophic State Index") +
  ylab("Ecological SSS (TP / 100 mm)")

EcoSSS.TSI


EcoSSS.TRA.Linear <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(-1, 1)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Ecological SSS (TP / 100 mm)")

EcoSSS.TRA.Linear


EcoSSS.TRA.Polynomial <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 2),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(-1, 1)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Ecological SSS (TP / 100 mm)")

EcoSSS.TRA.Polynomial


EcoSSS.TRA.Loess <-
  ggplot(AnalysisData, aes(x=MeanTRA, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "loess",
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(0, 7000)) +
  scale_y_continuous(limits = c(-1, 1)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Ecological SSS (TP / 100 mm)")

EcoSSS.TRA.Loess


EcoSSS.Pfankuch <-
  ggplot(AnalysisData, aes(x=PfankuchRaw, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  ## geom_smooth(method = "lm", formula = y ~ poly(x, 1),
  ##             group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 120)) +
  scale_y_continuous(limits = c(-1, 1)) +
  xlab("Pfankuch Disturbance Score") +
  ylab("Ecological SSS (TP / 100 mm)")

EcoSSS.Pfankuch



## Investigating Spatial Autocorrelation in our FWS metrics


coords <- cbind(AnalysisData$Longitude, AnalysisData$Latitude)


## Some response variables have missing values. One at a time, for each response variable
## with missing values, create new data frames and new coordinate lists:

TP.SE.Data <- AnalysisData[complete.cases(AnalysisData[, c("TP.SE.Area", "Latitude", "Longitude")]), ]
coords.TP.SE <- cbind(TP.SE.Data$Longitude, TP.SE.Data$Latitude)

SSS.Iso.Data <- AnalysisData[complete.cases(AnalysisData[, c("SizeStructureSlope_d15N", "Latitude", "Longitude")]), ]
coords.SSS.Iso <- cbind(SSS.Iso.Data$Longitude, SSS.Iso.Data$Latitude)

SSS.Eco.Data <- AnalysisData[complete.cases(AnalysisData[, c("SizeStructureSlope_TP", "Latitude", "Longitude")]), ]
coords.SSS.Eco <- cbind(SSS.Eco.Data$Longitude, SSS.Eco.Data$Latitude)


## Build k-nearest-neighbor structure (k = 4 is common)

knn <- knearneigh(coords, k = 4)
nb  <- knn2nb(knn)
lw  <- nb2listw(nb, style = "W")


## As above, if any response variables have missing values, then those variables need new weights:

knn.TP.SE <- knearneigh(coords.TP.SE, k = 4)
nb.TP.SE  <- knn2nb(knn.TP.SE)
lw.TP.SE  <- nb2listw(nb.TP.SE, style = "W")

knn.SSS.Iso <- knearneigh(coords.SSS.Iso, k = 4)
nb.SSS.Iso  <- knn2nb(knn.SSS.Iso)
lw.SSS.Iso  <- nb2listw(nb.SSS.Iso, style = "W")

knn.SSS.Eco <- knearneigh(coords.SSS.Eco, k = 4)
nb.SSS.Eco  <- knn2nb(knn.SSS.Eco)
lw.SSS.Eco  <- nb2listw(nb.SSS.Eco, style = "W")


## Global Moran's I Tests

NRange_Moran_Test <- moran.test(AnalysisData$d15N_Range, lw)
NRange_Moran_Test

## I = 0.133, Expectation = -0.034, Variance = 0.011, Standard deviate = 1.562, p = 0.059

CRange_Moran_Test <- moran.test(AnalysisData$d13C_Range, lw)
CRange_Moran_Test

## I = -0.004, Expectation = -0.034, Variance = 0.011, Standard deviate = 0.289, p = 0.387

FCL_Moran_Test <- moran.test(AnalysisData$FCLMax_Final, lw)
FCL_Moran_Test

## I = 0.060, Expectation = -0.034, Variance = 0.011, Standard deviate = 0.895, p = 0.185

CNAiso_Moran_Test <- moran.test(AnalysisData$Isotope.SE.Area, lw)
CNAiso_Moran_Test

## I = 0.063, Expectation = -0.034, Variance = 0.011, Standard deviate = 0.922, p = 0.178

CNAeco_Moran_Test <- moran.test(TP.SE.Data$TP.SE.Area, lw.TP.SE)
CNAeco_Moran_Test

## I = -0.091, Expectation = -0.038, Variance = 0.012, Standard deviate = -0.484, p = 0.686

SSSiso_Moran_Test <- moran.test(SSS.Iso.Data$SizeStructureSlope_d15N, lw.SSS.Iso)
SSSiso_Moran_Test

## I = -0.180, Expectation = -0.036, Variance = 0.011, Standard deviate = -1.393, p = 0.918

SSSeco_Moran_Test <- moran.test(SSS.Eco.Data$SizeStructureSlope_TP, lw.SSS.Eco)
SSSeco_Moran_Test

## I = -0.156, Expectation = -0.036, Variance = 0.011, Standard deviate = -1.132, p = 0.871


## Overall conclusion: No spatial autocorrelation detected.





#### Continuing with multiple linear regression and model selection procedure



## FCL Max

Null_FCL <- glm(FCLMax_Final ~ 1, data = AnalysisData)

Maximal_FCL <- glm(FCLMax_Final ~ ScaledSize + MeanTSI + poly(MeanTRA, 2),
                  data = AnalysisData)
summary(Maximal_FCL)

anova(Maximal_FCL, Null_FCL, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.935)

drop1(Maximal_FCL)
FCL_2 <- glm(FCLMax_Final ~ ScaledSize + MeanTSI + MeanTRA, data = AnalysisData)
summary(FCL_2)
anova(Maximal_FCL, FCL_2, test="F")

drop1(FCL_2)
FCL_3 <- glm(FCLMax_Final ~ ScaledSize + MeanTSI, data = AnalysisData)
summary(FCL_3)
anova(FCL_2, FCL_3, test="F")

drop1(FCL_3)
FCL_4 <- glm(FCLMax_Final ~ MeanTSI, data = AnalysisData)
summary(FCL_4)
anova(FCL_3, FCL_4, test="F")

## Nothin' doin'.


## NRange

Null_NRange <- glm(d15N_Range ~ 1, data = AnalysisData)

Maximal_NRange <- glm(d15N_Range ~ ScaledSize + MeanTSI + poly(MeanTRA, 2),
                   data = AnalysisData)
summary(Maximal_NRange)

anova(Maximal_NRange, Null_NRange, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.460)

drop1(Maximal_NRange)
NRange_2 <- glm(d15N_Range ~ ScaledSize + MeanTSI + MeanTRA, data = AnalysisData)
summary(NRange_2)
anova(Maximal_NRange, NRange_2, test="F")

drop1(NRange_2)
NRange_3 <- glm(d15N_Range ~ ScaledSize + MeanTRA, data = AnalysisData)
summary(NRange_3)
anova(NRange_2, NRange_3, test="F")

drop1(NRange_3)
NRange_4 <- glm(d15N_Range ~ ScaledSize, data = AnalysisData)
summary(NRange_4)
anova(NRange_3, NRange_4, test="F")

## Nothin' doin'.


## CRange

Null_CRange <- glm(d13C_Range ~ 1, data = AnalysisData)

Maximal_CRange <- glm(d13C_Range ~ ScaledSize + MeanTSI + MeanTRA,
                   data = AnalysisData)
summary(Maximal_CRange)

anova(Maximal_CRange, Null_CRange, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.323).
## However, all three explanatory variables appear marginally significant.

drop1(Maximal_CRange)
CRange_2 <- glm(d13C_Range ~ ScaledSize + MeanTSI, data = AnalysisData)
summary(CRange_2)
anova(CRange_2, Maximal_CRange, test="F")

## Now model is worse than it was with all three explanatory variables.

drop1(CRange_2)
CRange_3 <- glm(d13C_Range ~ MeanTSI, data = AnalysisData)
summary(CRange_3)
anova(CRange_2, CRange_3, test="F")

## Nothin'.


## Isotopic CNA

Null_CNAiso <- glm(Isotope.SE.Area ~ 1, data = AnalysisData)

Maximal_CNAiso <- glm(Isotope.SE.Area ~ ScaledSize + MeanTSI + MeanTRA,
                      data = AnalysisData)
summary(Maximal_CNAiso)

anova(Maximal_CNAiso, Null_CNAiso, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.700)

drop1(Maximal_CNAiso)
CNAiso_2 <- glm(Isotope.SE.Area ~ MeanTRA + MeanTSI, data = AnalysisData)
summary(CNAiso_2)
anova(CNAiso_2, Maximal_CNAiso, test="F")

## Now model is worse than it was with all three explanatory variables.

drop1(CNAiso_2)
CNAiso_3 <- glm(Isotope.SE.Area ~ MeanTSI, data = AnalysisData)
summary(CNAiso_3)
anova(CNAiso_2, CNAiso_3, test="F")

## Nothin'.


## Ecological CNA

Null_CNAeco <- glm(TP.SE.Area ~ 1, data = AnalysisData)

Maximal_CNAeco <- glm(TP.SE.Area ~ ScaledSize + MeanTSI + MeanTRA,
                      data = AnalysisData)
summary(Maximal_CNAeco)

anova(Maximal_CNAeco, Null_CNAeco, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.658)

drop1(Maximal_CNAeco)
CNAeco_2 <- glm(TP.SE.Area ~ MeanTRA + ScaledSize, data = AnalysisData)
summary(CNAeco_2)
anova(CNAeco_2, Maximal_CNAeco, test="F")

drop1(CNAeco_2)
CNAeco_3 <- glm(TP.SE.Area ~ ScaledSize, data = AnalysisData)
summary(CNAeco_3)
anova(CNAeco_2, CNAeco_3, test="F")

## Nothin'.


## Isotopic SSS

Null_SSSiso <- glm(SizeStructureSlope_d15N ~ 1, data = AnalysisData)

Maximal_SSSiso <- glm(SizeStructureSlope_d15N ~ ScaledSize + MeanTSI + MeanTRA,
                      data = AnalysisData)
summary(Maximal_SSSiso)

anova(Maximal_SSSiso, Null_SSSiso, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.441)

drop1(Maximal_SSSiso)
SSSiso_2 <- glm(SizeStructureSlope_d15N ~ MeanTSI + ScaledSize, data = AnalysisData)
summary(SSSiso_2)
anova(SSSiso_2, Maximal_SSSiso, test="F")

drop1(SSSiso_2)
SSSiso_3 <- glm(SizeStructureSlope_d15N ~ MeanTSI, data = AnalysisData)
summary(SSSiso_3)
anova(SSSiso_2, SSSiso_3, test="F")

## Nothin'.


## Ecological SSS

Null_SSSeco <- glm(SizeStructureSlope_TP ~ 1, data = AnalysisData)

Maximal_SSSeco <- glm(SizeStructureSlope_TP ~ ScaledSize + MeanTSI + MeanTRA,
                      data = AnalysisData)
summary(Maximal_SSSeco)

anova(Maximal_SSSeco, Null_SSSeco, test="F")

## F test reveals that the explanatory power of the maximal model
## does not differ from the explanatory power of the null model (p = 0.907)

drop1(Maximal_SSSeco)
SSSeco_2 <- glm(SizeStructureSlope_TP ~ MeanTSI + ScaledSize, data = AnalysisData)
summary(SSSeco_2)
anova(SSSeco_2, Maximal_SSSeco, test="F")

drop1(SSSeco_2)
SSSeco_3 <- glm(SizeStructureSlope_TP ~ MeanTSI, data = AnalysisData)
summary(SSSeco_3)
anova(SSSeco_2, SSSeco_3, test="F")

## Nothin'




#### Regional Plots

SiteCat.NRange <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=d15N_Range,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(2, 12), breaks = c(2, 4, 6, 8, 10, 12)) +
  xlab(" ") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

SiteCat.NRange


SiteCat.CRange <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=d13C_Range,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(2, 18), breaks = c(3, 6, 9, 12, 15, 18)) +
  xlab(" ") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

SiteCat.CRange


SiteCat.IsoSSS <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=SizeStructureSlope_d15N,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  xlab(" ") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)')) +
  geom_abline(slope = 0, intercept = 0, col = "gray40")

SiteCat.IsoSSS


SiteCat.IsoCNA <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=Isotope.SE.Area,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  xlab(" ") +
  ylab(expression('Isotopic CNA (\u2030  )'))

SiteCat.IsoCNA


SiteCat.FCL <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(2.5, 5.5), breaks = c(2.5, 3, 3.5, 4, 4.5, 5, 5.5)) +
  xlab(" ") +
  ylab("Max Food Chain Length")

SiteCat.FCL


SiteCat.EcoSSS <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=SizeStructureSlope_TP,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
  xlab(" ") +
  ylab("Ecological SSS (TP / 100 mm)") +
  geom_abline(slope = 0, intercept = 0, col = "gray40")

SiteCat.EcoSSS


SiteCat.EcoCNA <-
  ggplot(AnalysisData, aes(x=SiteCategory, y=TP.SE.Area,
                           col=SiteCategory)) +
  geom_boxplot(alpha=1, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3",
                                "deepskyblue",
                                "green4",
                                "green2")) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_discrete(labels = c("", "", "", "")) +
  scale_y_continuous(limits = c(-5, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  xlab(" ") +
  ylab("Ecological CNA (TP %)")

SiteCat.EcoCNA



#### Regional, Habitat, and Site Category Comparisons


## NRange

tapply(AnalysisData$d15N_Range, AnalysisData$Region, mean)
tapply(AnalysisData$d15N_Range, AnalysisData$Region, sd)
wilcox.test(CanadaAnalysisData$d15N_Range, TropicsAnalysisData$d15N_Range)

tapply(AnalysisData$d15N_Range, AnalysisData$HabitatType, mean)
tapply(AnalysisData$d15N_Range, AnalysisData$HabitatType, sd)
wilcox.test(LakeAnalysisData$d15N_Range, StreamAnalysisData$d15N_Range)

tapply(AnalysisData$d15N_Range, AnalysisData$SiteCategory, mean)
tapply(AnalysisData$d15N_Range, AnalysisData$SiteCategory, sd)
kruskal.test(AnalysisData$d15N_Range, AnalysisData$SiteCategory)
dunn.test(AnalysisData$d15N_Range, AnalysisData$SiteCategory)


## CRange

tapply(AnalysisData$d13C_Range, AnalysisData$Region, mean)
tapply(AnalysisData$d13C_Range, AnalysisData$Region, sd)
wilcox.test(CanadaAnalysisData$d13C_Range, TropicsAnalysisData$d13C_Range)

tapply(AnalysisData$d13C_Range, AnalysisData$HabitatType, mean)
tapply(AnalysisData$d13C_Range, AnalysisData$HabitatType, sd)
wilcox.test(LakeAnalysisData$d13C_Range, StreamAnalysisData$d13C_Range)

tapply(AnalysisData$d13C_Range, AnalysisData$SiteCategory, mean)
tapply(AnalysisData$d13C_Range, AnalysisData$SiteCategory, sd)
kruskal.test(AnalysisData$d13C_Range, AnalysisData$SiteCategory)
dunn.test(AnalysisData$d13C_Range, AnalysisData$SiteCategory)


## IsoSSS

## First, remove NAs

SSSData <- AnalysisData[-5,]
SSSCanadaData <- CanadaAnalysisData[-5,]
SSSLakeData <- LakeAnalysisData[-5,]

tapply(SSSData$SizeStructureSlope_d15N, SSSData$Region, mean)
tapply(SSSData$SizeStructureSlope_d15N, SSSData$Region, sd)
wilcox.test(SSSCanadaData$SizeStructureSlope_d15N, TropicsAnalysisData$SizeStructureSlope_d15N)

tapply(SSSData$SizeStructureSlope_d15N, SSSData$HabitatType, mean)
tapply(SSSData$SizeStructureSlope_d15N, SSSData$HabitatType, sd)
wilcox.test(SSSLakeData$SizeStructureSlope_d15N, StreamAnalysisData$SizeStructureSlope_d15N)

tapply(SSSData$SizeStructureSlope_d15N, SSSData$SiteCategory, mean)
tapply(SSSData$SizeStructureSlope_d15N, SSSData$SiteCategory, sd)
kruskal.test(SSSData$SizeStructureSlope_d15N, SSSData$SiteCategory)
dunn.test(SSSData$SizeStructureSlope_d15N, SSSData$SiteCategory)


## IsoCNA

tapply(AnalysisData$Isotope.SE.Area, AnalysisData$Region, mean)
tapply(AnalysisData$Isotope.SE.Area, AnalysisData$Region, sd)
wilcox.test(CanadaAnalysisData$Isotope.SE.Area, TropicsAnalysisData$Isotope.SE.Area)

tapply(AnalysisData$Isotope.SE.Area, AnalysisData$HabitatType, mean)
tapply(AnalysisData$Isotope.SE.Area, AnalysisData$HabitatType, sd)
wilcox.test(LakeAnalysisData$Isotope.SE.Area, StreamAnalysisData$Isotope.SE.Area)

tapply(AnalysisData$Isotope.SE.Area, AnalysisData$SiteCategory, mean)
tapply(AnalysisData$Isotope.SE.Area, AnalysisData$SiteCategory, sd)
kruskal.test(AnalysisData$Isotope.SE.Area, AnalysisData$SiteCategory)
dunn.test(AnalysisData$Isotope.SE.Area, AnalysisData$SiteCategory)


## FCL Max

tapply(AnalysisData$FCLMax_Final, AnalysisData$Region, mean)
tapply(AnalysisData$FCLMax_Final, AnalysisData$Region, sd)
wilcox.test(CanadaAnalysisData$FCLMax_Final, TropicsAnalysisData$FCLMax_Final)

tapply(AnalysisData$FCLMax_Final, AnalysisData$HabitatType, mean)
tapply(AnalysisData$FCLMax_Final, AnalysisData$HabitatType, sd)
wilcox.test(LakeAnalysisData$FCLMax_Final, StreamAnalysisData$FCLMax_Final)

tapply(AnalysisData$FCLMax_Final, AnalysisData$SiteCategory, mean)
tapply(AnalysisData$FCLMax_Final, AnalysisData$SiteCategory, sd)
kruskal.test(AnalysisData$FCLMax_Final, AnalysisData$SiteCategory)
dunn.test(AnalysisData$FCLMax_Final, AnalysisData$SiteCategory)


## EcoSSS

tapply(SSSData$SizeStructureSlope_TP, SSSData$Region, mean)
tapply(SSSData$SizeStructureSlope_TP, SSSData$Region, sd)
wilcox.test(SSSCanadaData$SizeStructureSlope_TP, TropicsAnalysisData$SizeStructureSlope_TP)

tapply(SSSData$SizeStructureSlope_TP, SSSData$HabitatType, mean)
tapply(SSSData$SizeStructureSlope_TP, SSSData$HabitatType, sd)
wilcox.test(SSSLakeData$SizeStructureSlope_TP, StreamAnalysisData$SizeStructureSlope_TP)

tapply(SSSData$SizeStructureSlope_TP, SSSData$SiteCategory, mean)
tapply(SSSData$SizeStructureSlope_TP, SSSData$SiteCategory, sd)
kruskal.test(SSSData$SizeStructureSlope_TP, SSSData$SiteCategory)
dunn.test(SSSData$SizeStructureSlope_TP, SSSData$SiteCategory)


## EcoCNA

## First, remove NAs

EcoCNAData <- AnalysisData[-c(24, 25, 29),]
EcoCNATropicsData <-TropicsAnalysisData[-c(9,10,14),]
EcoCNAStreamData <- StreamAnalysisData[-c(9,10,14),]

tapply(EcoCNAData$TP.SE.Area, EcoCNAData$Region, mean)
tapply(EcoCNAData$TP.SE.Area, EcoCNAData$Region, sd)
wilcox.test(CanadaAnalysisData$TP.SE.Area, EcoCNATropicsData$TP.SE.Area)

tapply(EcoCNAData$TP.SE.Area, EcoCNAData$HabitatType, mean)
tapply(EcoCNAData$TP.SE.Area, EcoCNAData$HabitatType, sd)
wilcox.test(LakeAnalysisData$TP.SE.Area, EcoCNAStreamData$TP.SE.Area)

tapply(EcoCNAData$TP.SE.Area, EcoCNAData$SiteCategory, mean)
tapply(EcoCNAData$TP.SE.Area, EcoCNAData$SiteCategory, sd)
kruskal.test(EcoCNAData$TP.SE.Area, EcoCNAData$SiteCategory)
dunn.test(EcoCNAData$TP.SE.Area, EcoCNAData$SiteCategory)




###### Plotting Specific Regional / Habitat Relationships


summary(lm(StreamAnalysisData$d15N_Range ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$d15N_Range ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$d15N_Range ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$d15N_Range ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$d15N_Range ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$d15N_Range ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$d15N_Range ~ StreamAnalysisData$PfankuchRaw))


## NRange was negatively affected by MeanTSI in Streams (R2 = 0.46, p = 0.003).


summary(lm(StreamAnalysisData$FCLMax_Final ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$FCLMax_Final ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$FCLMax_Final ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$FCLMax_Final ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$FCLMax_Final ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$FCLMax_Final ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$FCLMax_Final ~ StreamAnalysisData$PfankuchRaw))


## FCLmax was negatively affected by TRA in Streams (R2 = 0.32, p = 0.016)


summary(lm(StreamAnalysisData$d13C_Range ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$d13C_Range ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$d13C_Range ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$d13C_Range ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$d13C_Range ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$d13C_Range ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$d13C_Range ~ StreamAnalysisData$PfankuchRaw))


## CRange was negatively affected by TSI in Streams (R2 = 0.16, p = 0.078)
## and positively affected by TSI in Lakes (R2 = 0.33, p = 0.014)


summary(lm(StreamAnalysisData$Isotope.SE.Area ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$Isotope.SE.Area ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$Isotope.SE.Area ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$Isotope.SE.Area ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$Isotope.SE.Area ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$Isotope.SE.Area ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$Isotope.SE.Area ~ StreamAnalysisData$PfankuchRaw))


## CNAiso was negatively affected by TSI in Streams (R2 = 0.24, p = 0.038). This is
## unsurprising given that NRange and CRange were both negatively affected by TSI in Streams.


summary(lm(StreamAnalysisData$TP.SE.Area ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$TP.SE.Area ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$TP.SE.Area ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$TP.SE.Area ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$TP.SE.Area ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$TP.SE.Area ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$TP.SE.Area ~ StreamAnalysisData$PfankuchRaw))


## Nothing


summary(lm(StreamAnalysisData$SizeStructureSlope_d15N ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$SizeStructureSlope_d15N ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$SizeStructureSlope_d15N ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$SizeStructureSlope_d15N ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$SizeStructureSlope_d15N ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$SizeStructureSlope_d15N ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$SizeStructureSlope_d15N ~ StreamAnalysisData$PfankuchRaw))


## IsoSSS was negatively affected by TSI in Streams (R2 = 0.23, p = 0.041)


summary(lm(StreamAnalysisData$SizeStructureSlope_TP ~ StreamAnalysisData$MeanTSI))
summary(lm(LakeAnalysisData$SizeStructureSlope_TP ~ LakeAnalysisData$MeanTSI))

summary(lm(StreamAnalysisData$SizeStructureSlope_TP ~ StreamAnalysisData$ScaledSize))
summary(lm(LakeAnalysisData$SizeStructureSlope_TP ~ LakeAnalysisData$ScaledSize))

summary(lm(StreamAnalysisData$SizeStructureSlope_TP ~ StreamAnalysisData$MeanTRA))
summary(lm(LakeAnalysisData$SizeStructureSlope_TP ~ LakeAnalysisData$MeanTRA))

summary(lm(StreamAnalysisData$SizeStructureSlope_TP ~ StreamAnalysisData$PfankuchRaw))


### EcoSSS was negatively affected by TSI in Streams (R2 = 0.22, p = 0.043)



NRange.TSI.Streams <-
  ggplot(StreamAnalysisData, aes(x=MeanTSI, y=d15N_Range,
                                 col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("deepskyblue",
                                "green2")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(10, 73), breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  scale_y_continuous(limits = c(3, 11), breaks = c(3, 5, 7, 9, 11)) +
  xlab("Mean Trophic State Index") +
  ylab(expression({delta}^15*N~'Range (\u2030)'))

NRange.TSI.Streams


CRange.TSI.Streams <-
  ggplot(StreamAnalysisData, aes(x=MeanTSI, y=d13C_Range,
                                 col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("deepskyblue",
                                "green2")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(10, 73), breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  scale_y_continuous(limits = c(2, 20), breaks = c(3, 6, 9, 12, 15, 18)) +
  xlab("Mean Trophic State Index") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.TSI.Streams


CNAiso.TSI.Streams <-
  ggplot(StreamAnalysisData, aes(x=MeanTSI, y=Isotope.SE.Area,
                                 col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("deepskyblue",
                                "green2")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(10, 73), breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  scale_y_continuous(limits = c(0, 30), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  xlab("Mean Trophic State Index") +
  ylab(expression('Isotopic CNA (\u2030  )'))

CNAiso.TSI.Streams


SSSiso.TSI.Streams <-
  ggplot(StreamAnalysisData, aes(x=MeanTSI, y=SizeStructureSlope_d15N,
                                 col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("deepskyblue",
                                "green2")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(10, 73), breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  xlab("Mean Trophic State Index") +
  ylab(expression('Isotopic SSS (\u2030 / 100 mm)')) +
  geom_abline(slope = 0, intercept = 0, col = "gray40")

SSSiso.TSI.Streams


SSSeco.TSI.Streams <-
  ggplot(StreamAnalysisData, aes(x=MeanTSI, y=SizeStructureSlope_TP,
                                 col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("deepskyblue",
                                "green2")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(10, 73), breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  scale_y_continuous(limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
  xlab("Mean Trophic State Index") +
  ylab("Ecological SSS (TP / 100 mm)") +
  geom_abline(slope = 0, intercept = 0, col = "gray40")

SSSeco.TSI.Streams


CRange.TSI.Lakes <-
  ggplot(LakeAnalysisData, aes(x=MeanTSI, y=d13C_Range,
                                 col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue2",
                                "green4")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(40, 96), breaks = c(40, 50, 60, 70, 80, 90)) +
  scale_y_continuous(limits = c(4, 16), breaks = c(4, 6, 8, 10, 12, 14, 16)) +
  xlab("Mean Trophic State Index") +
  ylab(expression({delta}^13*C~'Range (\u2030)'))

CRange.TSI.Lakes


FCL.TRA.Streams <-
  ggplot(StreamAnalysisData, aes(x=MeanTRA, y=FCLMax_Final,
                           col=SiteCategory)) +
  geom_point(alpha=1, size=2, show.legend = FALSE) +
  scale_color_manual(values = c("deepskyblue",
                                "green2")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              group=1, col="black") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 11),
        axis.text.y = element_text(colour = "black", size = 11)) +
  theme(axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11)) +
  scale_x_continuous(limits = c(-300, 4950)) +
  scale_y_continuous(limits = c(2.75, 4.5)) +
  xlab("Mean Total Resource Abundance") +
  ylab("Max Food Chain Length")

FCL.TRA.Streams