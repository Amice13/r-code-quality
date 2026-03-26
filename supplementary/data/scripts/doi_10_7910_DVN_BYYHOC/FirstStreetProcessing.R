#Code for processing first street data
library(rspatial)
library(PerformanceAnalytics)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(sf)
library(sp)
library(tidyverse)
library(tidycensus)
library(tmap)
library(spdep)
library(tigris)
library(rmapshaper)
library(broom)
library(car)
library(spatialreg)
library(knitr)
library(stargazer)
library(RColorBrewer)
library(stats)
library(ape)
library(GWmodel)
library(lctools)
library(rspatial)
library(rgdal)
library(MASS)
library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(spdep)
library(tigris)
library(rmapshaper)
library(broom)
library(car)
library(spatialreg)
library(knitr)
library(stargazer)
library(MuMIn)

#Setting Times New Roman as font
windowsFonts()

options(promt="R >", digits = 4, scipen = 7)


#Individual city data processing

##Phoenix, AZ
setwd("C:/Users/J/Documents/FirstStreet/DataSharing/RCode")

###CBG, whole city

phoenixCBG <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PHX_CBG.csv")

phoenixCBG_spearman <- phoenixCBG[,c(21, 22, 4, 6, 3, 10, 8, 9, 12, 20, 7, 14, 23, 16, 15, 17, 18, 19)]
phoenixCBG_spearman <- phoenixCBG_spearman[complete.cases(phoenixCBG_spearman),]

####Dropping change in risk as a factor that contributes
phoenixCBG_spearman_1 <- phoenixCBG_spearman[,-2]

####Dropping 0 values from FF
phoenixCBG_spearman_1 <- phoenixCBG_spearman_1[which(phoenixCBG_spearman_1$FF > 0), ]

model1 <- lm(FF ~ ., data = phoenixCBG_spearman_1)
vif(model1)

###Dropping PopLatino because the VIF is the highest
phoenixCBG_spearman_1 <- phoenixCBG_spearman_1[,-13]



model1 <- lm(FF ~ ., data = phoenixCBG_spearman_1, na.action = na.fail)
vif(model1)



####Dredge allows you to hunt for the models with the lowest AIC; then I subset 
####for those where delta < 2, which is a threshold necessary to cross before the 
####models are significantly different from one another. This ultimately tells 
####you which variables should be used or dropped by which ones are included here.
dd_1 <- dredge(model1)
subset(dd_1, delta < 2)
summary(model.avg(dd, subset = delta <= 2))
####Same but for delta risk
phoenixCBG_spearman_2 <- phoenixCBG_spearman[which(phoenixCBG_spearman$FF > 0),]
####Gets rid of FF variable
phoenixCBG_spearman_2 <- phoenixCBG_spearman_2[, -1]


model2 <- lm(DeltaRisk ~ ., data = phoenixCBG_spearman_2, na.action = na.fail)
vif(model2)
####Need to get rid of PopLatino because VIF > 5
phoenixCBG_spearman_2 <- phoenixCBG_spearman_2[, -13]
model2 <- lm(DeltaRisk ~ ., data = phoenixCBG_spearman_2, na.action = na.fail)
vif(model2)

dd_2 <- dredge(model2)
subset(dd_2, delta < 2)
summary(model.avg(dd_2, subset = delta <= 2))


####Spearman's Rank for CBG
colorsForPlot <- colorRampPalette(c("#88bbbe", "white", "#E24E3C"))

phoenixCBG_spearman_2 <- rcorr(as.matrix(phoenixCBG_spearman), type = "spearman")

###CBGs in HOLC, for Spearman's Rank
phoenixCBG_HOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PHX_CBG_HOLC.csv")
phoenixCBG_HOLC <- phoenixCBG_HOLC[,c(21, 22, 25, 26, 27)]

phoenixCBG_HOLC$Redline <- phoenixCBG_HOLC$sum_holc_d/phoenixCBG_HOLC$HOLCParcels

####Testing sensitivity of redlining analysis. First version is for parcels where at least 25% of parcels are in HOLC areas
phoenixCBG_HOLC_25 <- phoenixCBG_HOLC[which(phoenixCBG_HOLC$PropInHOLC >= 0.25),]
phoenixCBG_HOLC_25_spearman <- phoenixCBG_HOLC_25[complete.cases(phoenixCBG_HOLC_25),]
phoenixCBG_HOLC_25_spearman <- phoenixCBG_HOLC_25[,c(1, 2, 6)]
phoenixCBG_HOLC_25_spearman_2 <- rcorr(as.matrix(phoenixCBG_HOLC_25_spearman))

corrplot(phoenixCBG_HOLC_25_spearman_2$r,  method = "color", type = "lower", order = "alphabet",
         p.mat = phoenixCBG_HOLC_25_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))

####Second version is for parcels where at least 50% of parcels are in HOLC
phoenixCBG_HOLC_50 <- phoenixCBG_HOLC[which(phoenixCBG_HOLC$PropInHOLC >= 0.50),]
phoenixCBG_HOLC_50_spearman <- phoenixCBG_HOLC_50[complete.cases(phoenixCBG_HOLC_50),]
phoenixCBG_HOLC_50_spearman <- phoenixCBG_HOLC_50[,c(1, 2, 6)]
phoenixCBG_HOLC_50_spearman_2 <- rcorr(as.matrix(phoenixCBG_HOLC_50_spearman))

corrplot(phoenixCBG_HOLC_50_spearman_2$r,  method = "color", type = "lower", order = "alphabet",
         p.mat = phoenixCBG_HOLC_50_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))

###Same analysis for parcels

phoenixParcels <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PHX_Parcels.csv")
phoenixParcels_spearman <- phoenixParcels[,c(2, 6, 3, 7, 4)]
colnames(phoenixParcels_spearman) <- c("Flood factor", "Change in flood risk", "Building value", "Building age", "Green cover") 
phoenixParcels_spearman <- phoenixParcels_spearman[complete.cases(phoenixParcels_spearman),]
phoenixParcels_spearman_2 <-rcorr(as.matrix(phoenixParcels_spearman), type = "spearman")

phoenixParcels_inHOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PHX_Parcels_HOLC.csv")
phoenixParcels_inHOLC_spearman <- phoenixParcels_inHOLC[,c(2, 6, 8)]
colnames(phoenixParcels_inHOLC_spearman) <- c("Flood factor", "Change in chance", "Redline") 
phoenixParcels_inHOLC_spearman <- phoenixParcels_inHOLC_spearman[complete.cases(phoenixParcels_inHOLC_spearman),]
phoenixParcels_inHOLC_spearman_2 <- rcorr(as.matrix(phoenixParcels_inHOLC_spearman), type = "spearman")

corrplot(phoenixParcels_inHOLC_spearman_2$r,  method = "color", type = "lower", order = "alphabet",
         p.mat = phoenixParcels_inHOLC_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))




##Plotting Spearman's Rank
jpeg("phoenixCBGSpearmanMatrix_07112022.jpg", width = 14, height = 14, unit = "in", res = 300)
corrplot(phoenixCBG_spearman_2$r,  method = "color", type = "lower", order = "alphabet",
         p.mat = phoenixCBG_spearman_2$P, sig.level = 0.01, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))
dev.off()

jpeg("phoenixParcelsSpearmanMatrix_07112022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(phoenixParcels_spearman_2$r,  method = "color", type = "lower", order = "alphabet",
         p.mat = phoenixParcels_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1), addCoef.col = "black")
dev.off()

jpeg("phoenixParcelsSpearmanMatrix_06102022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(phoenixParcels_spearman_2$r,  method = "color", type = "lower", order = "alphabet",
         p.mat = phoenixParcels_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1), addCoef.col = "black")
dev.off()


#Portland
##Analysis of CBGs
PDX_CBG <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PDX_CBG.csv")


portlandCBG_spearman <- PDX_CBG[,c(22, 23, 4, 3, 6, 17, 15, 16, 19, 20, 14, 8, 21, 10, 9, 11, 12, 13)]

portlandCBG_spearman <- portlandCBG_spearman[complete.cases(portlandCBG_spearman),]


###Dropping delta risk so I can anlyze for FF
portlandCBG_spearman_1 <- portlandCBG_spearman[,-2]

model1 <- lm(FF ~ ., data = portlandCBG_spearman_1, na.action = na.fail)
vif(model1)

####Dredge allows you to hunt for the models with the lowest AIC; then I subset 
####for those where delta < 2, which is a threshold necessary to cross before the 
####models are significantly different from one another. This ultimately tells 
##you which variables should be used or dropped by which ones are included here.
dd_1 <- dredge(model1)
summary(model.avg(dd_1, subset = delta <= 2))


###Doing the same thing but for the change in chance
portlandCBG_spearman_2 <- portlandCBG_spearman[,-1]

model2 <- lm(FF ~ ., data = portlandCBG_spearman_2, na.action = na.fail)
vif(model2)

dd_2 <- dredge(model2)
summary(model.avg(dd_2, subset = delta <= 2))

portlandCBG_spearman_2 <-rcorr(as.matrix(portlandCBG_spearman), type = "spearman")


##CBGs in HOLC
portlandCBG_HOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PDX_CBG_HOLC.csv")
portlandCBG_HOLC_spearman <- portlandCBG_HOLC[,c(23, 24, 26, 27)]
portlandCBG_HOLC_spearman$Redline <- portlandCBG_HOLC_spearman$sum_holc_grade_d / portlandCBG_HOLC_spearman$InHOLC_Count
portlandCBG_HOLC_spearman <- portlandCBG_HOLC_spearman[complete.cases(portlandCBG_HOLC_spearman),]

###Using only CBGs for which >= 25% are in HOLC areas
portlandCBG_HOLC_25_spearman <- portlandCBG_HOLC_spearman[which(portlandCBG_HOLC_spearman$PropInHOLC >=0.25),]
portlandCBG_HOLC_25_spearman <- portlandCBG_HOLC_25_spearman[,c(1, 2, 5)]
colnames(portlandCBG_HOLC_25_spearman) <- c("Flood factor", "Change in flood risk", "Redline") 
portlandCBG_HOLC_25_spearman_2 <-rcorr(as.matrix(portlandCBG_HOLC_25_spearman), type = "spearman")

colorsForPlot <- colorRampPalette(c("#88bbbe", "white", "#E24E3C"))

corrplot(portlandCBG_HOLC_25_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = portlandCBG_HOLC_25_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")


###Using only CBGs for which >= 50% are in HOLC areas
portlandCBG_HOLC_50_spearman <- portlandCBG_HOLC_spearman[which(portlandCBG_HOLC_spearman$PropInHOLC >=0.50),]
portlandCBG_HOLC_50_spearman <- portlandCBG_HOLC_50_spearman[,c(1, 2, 5)]
colnames(portlandCBG_HOLC_50_spearman) <- c("Flood factor", "Change in flood risk", "Redline") 
portlandCBG_HOLC_50_spearman_2 <-rcorr(as.matrix(portlandCBG_HOLC_50_spearman), type = "spearman")

colorsForPlot <- colorRampPalette(c("#88bbbe", "white", "#E24E3C"))

corrplot(portlandCBG_HOLC_50_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = portlandCBG_HOLC_50_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")



##Analysis of parcels
PDX_Parcels <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PDX_Parcels.csv")
portlandParcels_spearman <- PDX_Parcels[,c(9, 3, 7, 2, 10)]

portlandParcels_spearman <- portlandParcels_spearman[complete.cases(portlandParcels_spearman),]
portlandParcels_spearman_2 <-rcorr(as.matrix(portlandParcels_spearman), type = "spearman")

##Analysis of parcels in HOLC
PDX_Parcels_HOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/PDX_Parcels_HOLC.csv")
portlandParcels_HOLC_spearman <- portlandParcels_HOLC[,c(11, 5, 14)]
colnames(portlandParcels_HOLC_spearman) <- c("Flood factor", "Change in flood risk", "Redline") 
portlandParcels_HOLC_spearman <- portlandParcels_HOLC_spearman[complete.cases(portlandParcels_HOLC_spearman),]
portlandParcels_HOLC_spearman_2 <-rcorr(as.matrix(portlandParcels_HOLC_spearman), type = "spearman")

###Change the sig.level field to figure out the level of significance of the correlations
corrplot(portlandParcels_HOLC_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = portlandParcels_HOLC_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")






colorsForPlot <- colorRampPalette(c("#88bbbe", "white", "#E24E3C"))

jpeg("portlandCBGSpearmanMatrix_07122022.jpg", width = 14, height = 14, unit = "in", res = 300)
corrplot(portlandCBG_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = portlandCBG_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")
dev.off()


jpeg("portlandParcelsSpearmanMatrix_07122022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(portlandParcels_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = portlandParcels_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1), addCoef.col = "black")
dev.off()

jpeg("portlandParcels_redliningOnly_SpearmanMatrix_05062022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(portlandParcels_redliningOnly_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = portlandParcels_redliningOnly_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1), addCoef.col = "black")
dev.off()


jpeg("portlandCBGChartCorr_11082021.jpg", width = 8, height = 8, unit = "in", res = 300)
chart.Correlation(portlandCBG_spearman, histogram = TRUE, pch = 19, order = "alphabet")
dev.off()

#Baltimore
setwd("C:/Users/J/Documents/FirstStreet/DataSharing/RCode")

##Baltimore CBG
BMORE_CBG <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/BMORE_CBG.csv")


baltimoreCBG_spearman <- BMORE_CBG[,c(24, 25, 6, 3, 4, 8, 10, 22, 11, 12, 9, 16, 23, 18, 17, 19, 20, 21)]
baltimoreCBG_spearman <- baltimoreCBG_spearman[complete.cases(baltimoreCBG_spearman),]

corrplot(baltimoreCBG_spearman$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreCBG_spearman$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")




model2 <- lm(FF ~ ., data = baltimoreCBG_spearman_2, na.action = na.fail)

summary(model2)

dd_2 <- dredge(model2)
summary(model.avg(dd_2, subset = delta <= 2))

###Removing AmInd and AK Nat from matrix because of very very low numbers
baltimoreCBG_spearman_2 <- baltimoreCBG_spearman_2[,-16]
baltimoreCBG_spearman_2 <- baltimoreCBG_spearman_2[,-16]

vif(model2)


baltimoreCBG_spearman_2 <-rcorr(as.matrix(baltimoreCBG_spearman), type = "spearman")

###Doing HOLC analysis for CBG
baltimoreCBG_inHOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/BMORE_CBG_HOLC.csv")
baltimoreCBG_inHOLC_spearman <- baltimoreCBG_inHOLC[,c(24, 25, 29, 30)]
baltimoreCBG_inHOLC_spearman <- baltimoreCBG_inHOLC_spearman[complete.cases(baltimoreCBG_inHOLC_spearman),]

###Selecting CBGs for which >=25% of parcels are in HOLC areas
baltimoreCBG_inHOLC_25_spearman <- baltimoreCBG_inHOLC_spearman[which(baltimoreCBG_inHOLC_spearman$PropInHOLC >= 0.25),]
baltimoreCBG_inHOLC_25_spearman <- baltimoreCBG_inHOLC_25_spearman[,c(1, 2, 4)]
baltimoreCBG_inHOLC_25_spearman_2 <-rcorr(as.matrix(baltimoreCBG_inHOLC_25_spearman), type = "spearman")

corrplot(baltimoreCBG_inHOLC_25_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreCBG_inHOLC_25_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")

###Selecting CBGs for which >=50% of parcels are in HOLC areas
baltimoreCBG_inHOLC_50_spearman <- baltimoreCBG_inHOLC_spearman[which(baltimoreCBG_inHOLC_spearman$PropInHOLC >= 0.50),]
baltimoreCBG_inHOLC_50_spearman <- baltimoreCBG_inHOLC_50_spearman[,c(1, 2, 4)]
baltimoreCBG_inHOLC_50_spearman_2 <-rcorr(as.matrix(baltimoreCBG_inHOLC_50_spearman), type = "spearman")

corrplot(baltimoreCBG_inHOLC_50_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreCBG_inHOLC_50_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")

colorsForPlot <- colorRampPalette(c("#88bbbe", "white", "#E24E3C"))

corrplot(baltimoreCBG_HOLC_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreCBG_HOLC_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")



##Baltimore analysis for Parcels
BMORE_Parcels <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/BMORE_Parcels.csv")
baltimoreParcels_spearman <- BMORE_Parcels[,c(7, 10, 9, 3, 4)]


baltimoreParcels_spearman <- baltimoreParcels_spearman[complete.cases(baltimoreParcels_spearman),]
baltimoreParcels_spearman_2 <-rcorr(as.matrix(baltimoreParcels_spearman), type = "spearman")


##Baltimore analysis for parcels in HOLC
baltimoreParcels_inHOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/BMORE_Parcels_HOLC.csv")
baltimoreParcels_inHOLC_spearman <- baltimoreParcels_inHOLC[,c(7, 10, 11)]
 
baltimoreParcels_inHOLC_spearman <- baltimoreParcels_inHOLC_spearman[complete.cases(baltimoreParcels_inHOLC_spearman),]
baltimoreParcels_inHOLC_spearman_2 <-rcorr(as.matrix(baltimoreParcels_inHOLC_spearman), type = "spearman")


jpeg("baltimoreCBGSpearmanMatrix_07112022.jpg", width = 14, height = 14, unit = "in", res = 300)
corrplot(baltimoreCBG_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreCBG_spearman_2$P, sig.level = 0.01, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif")
dev.off()


jpeg("baltimoreParcelsSpearmanMatrix_07112022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(baltimoreParcels_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreParcels_spearman_2$P, sig.level = 0.01, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1),  addCoef.col = "black")
dev.off()

jpeg("baltimoreParcelsSpearmanMatrix_inHOLC_05062022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(baltimoreParcels_inHOLC_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = baltimoreParcels_inHOLC_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1),  addCoef.col = "black")
dev.off()

#Atlanta
setwd("C:/Users/J/Documents/FirstStreet/DataSharing/RCode")


ATL_CBG <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/ATL_CBG.csv")
atlantaCBG_spearman <- ATL_CBG[,c(24, 25, 4, 3, 6, 7, 8, 18, 19, 20, 12, 23, 14, 13, 15, 16, 17)]

atlantaCBG_spearman <- atlantaCBG_spearman[complete.cases(atlantaCBG_spearman),]
atlantaCBG_spearman_2 <- rcorr(as.matrix(atlantaCBG_spearman))

##Getting rid of delta risk to analyze for FF
atlantaCBG_spearman_1 <- atlantaCBG_spearman[,-2]

model1 <- lm(FF ~ ., data = atlantaCBG_spearman_1, na.action = na.fail)

summary(model1)
vif(model2)


dd_1 <- dredge(model1)
summary(model.avg(dd_1, subset = delta <= 2))



atlantaCBG_spearman_2 <-rcorr(as.matrix(atlantaCBG_spearman), type = "spearman")


##Analyzing HOLC data for CBGs

ATL_CBG_HOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/ATL_CBG_HOLC.csv")
atlantaCBG_HOLC_spearman <- ATL_CBG_HOLC[,c(21, 22, 23, 24, 25, 26)]
atlantaCBG_HOLC_spearman <- atlantaCBG_HOLC_spearman[complete.cases(atlantaCBG_HOLC_spearman), ]
atlantaCBG_HOLC_spearman$Redline <- atlantaCBG_HOLC_spearman$sum_holc_d/atlantaCBG_HOLC_spearman$HOLCParcels_Count


##Using only CBGs for which the proportion of parcels in the HOLC are 25% or greater
atlantaCBG_HOLC_25_spearman <- atlantaCBG_HOLC_spearman[which(atlantaCBG_HOLC_spearman$ParcelsInHOLC_prop >= 0.25),]
atlantaCBG_HOLC_25_spearman <- atlantaCBG_HOLC_25_spearman[,c(1, 2, 7)]
atlantaCBG_HOLC_25_spearman_2 <- rcorr(as.matrix(atlantaCBG_HOLC_25_spearman), type = "spearman")

corrplot(atlantaCBG_HOLC_25_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = atlantaCBG_HOLC_25_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))

##Using only CBGs for which the proportion of parcels in the HOLC are 50% or greater
atlantaCBG_HOLC_50_spearman <- atlantaCBG_HOLC_spearman[which(atlantaCBG_HOLC_spearman$ParcelsInHOLC_prop >= 0.50),]
atlantaCBG_HOLC_50_spearman <- atlantaCBG_HOLC_50_spearman[,c(1, 2, 7)]
atlantaCBG_HOLC_50_spearman_2 <- rcorr(as.matrix(atlantaCBG_HOLC_50_spearman), type = "spearman")

corrplot(atlantaCBG_HOLC_50_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = atlantaCBG_HOLC_50_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))



##Atlanta parcels analysis
ATL_Parcels <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/ATL_Parcels.csv")
atlantaParcels_spearman <- ATL_Parcels[,c(7, 9, 5, 8)]
colnames(atlantaParcels_spearman) <- c("Flood factor", "Change in flood risk", "Green cover", "Building age") 
atlantaParcels_spearman <- atlantaParcels_spearman[complete.cases(atlantaParcels_spearman),]
atlantaParcels_spearman_2 <-rcorr(as.matrix(atlantaParcels_spearman), type = "spearman")


##Atlanta parcels in HOLC analysis
ATL_Parcels_HOLC <- read.csv("C:/Users/J/Documents/FirstStreet/DataSharing/RCode/ATL_Parcels_HOLC.csv")
atlantaParcels_inHOLC <- ATL_Parcels_HOLC[,c(8, 10, 11)]

atlantaParcels_inHOLC_spearman <- atlantaParcels_inHOLC[complete.cases(atlantaParcels_inHOLC),]
atlantaParcels_inHOLC_spearman_2 <-rcorr(as.matrix(atlantaParcels_inHOLC_spearman), type = "spearman")



colorsForPlot <- colorRampPalette(c("#88bbbe", "white", "#E24E3C"))


jpeg("AtlantaCBGSpearmanMatrix_07112022.jpg", width = 14, height = 14, unit = "in", res = 300)
corrplot(atlantaCBG_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = atlantaCBG_spearman_2$P, sig.level = 0.05, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1))
dev.off()

jpeg("AtlantaParcelsSpearmanMatrix_07112022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(atlantaParcels_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = atlantaParcels_spearman_2$P, sig.level = 0.01, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1), addCoef.col = "black")
dev.off()

jpeg("AtlantaParcelsSpearmanMatrix_inHOLC_05062022.jpg", width = 8, height = 8, unit = "in", res = 300)
corrplot(atlantaParcels_inHOLC_spearman_2$r, method = "color", type = "lower", order = "alphabet",
         p.mat = atlantaParcels_inHOLC_spearman_2$P, sig.level = 0.001, insig = "blank", col = colorsForPlot(10), 
         diag = FALSE, tl.srt = 45, tl.col = "black", family = "serif", cl.cex = 1.2, tl.cex = 1.6, mar = c(0.1, 0.1, 0.1, 0.1), addCoef.col = "black")
dev.off()