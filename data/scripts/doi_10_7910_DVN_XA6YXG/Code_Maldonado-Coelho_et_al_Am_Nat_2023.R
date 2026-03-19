#code for analyses in Maldonado-Coelho et al. 2023. 
#Evolutionary and ecological processes underlying geographic variation in innate bird songs. 
#The American Naturalist
##########################################################################################################
#to see numbers without +e
options("scipen"=999, "digits"=4)
##########################################################################################################
#load packages

#GAM/BAM models and Figures
library(gratia)# Author: Gavin L. Simpson (2021)
library(mgcv)# Author: Simon Wood (2017)
library(ggplot2)# Author: Hadley Wickham et al. (2020)
library(scam)# Author: Natalya Pya (2021)
library(cowplot)# Author: Claus O. Wilke (2020)
library(tidyr)# Author: Hadley Wickham et al. (2021)

#contrasting GAMs with and without spatial dependence
library(visreg)# Author: Patrick Breheny and Woodrow Burchett (2020)
library(lmerTest)# Author: Alexandra Kuznetsova et al. (2020)
library(nlme)# Author: José Pinheiro et al. (2021)    # gamm for lme() in nlme package
library(emmeans)# Author: Paul Buerkner et al. (2021)
library(MuMIn)# Author: Kamil Barton (2020)
library(itsadug)#Author: Jacolien van Rij et al. (2020)

#Discrimination Function Analyses - Comparing songs across the range
library(MASS)#Author: Brian Ripley et al (2021)
library(biotools)#Author: Anderson Rodrigo da Silva (2021)
library(caret)#Author: Max Kuhn et al. (2021)
library(e1071)#Author: David Meyer et al. (2021)

#Calculating Cohen´s Kappa
library("vcd")# Author: David Meyer (2021)

#Variance F-ratio tests
#loading package to perform F-ratio through permutations (randomly sampled without replacement)
#p-value estimated as the proportion of random values equal to or larger than 
#the observed F-ratio value. The ratio under the H0 is 1.
#F-ratio = larger variance in the numerator/smaller variance in the denominator
library(RVAideMemoire)# Author: Maxime Hervé (2021)

#Testing the role of song predictors using Generalized Additive Models (GAMs)
library(gratia)# Author: Gavin L. Simpson (2021)
library(mgcv)# Author: Simon Wood (2017)
library(ggplot2)# Author: Hadley Wickham et al. (2020)
library(scam)# Author: Natalya Pya (2021)
library(cowplot)# Author: Claus O. Wilke (2020)
library(tidyr)# Author: Hadley Wickham et al. (2021)
library(dplyr)# Author: Hadley Wickham et al. (2021)
library(car):# Author: John Fox et al. (2020)
library(caret)# Author: Max Kuhn et al. (2021)
#Installation instructions for mgcv.helper from Github (this package is not available on CRAN)
#https://github.com/samclifford/mgcv.helper
library(devtools)
devtools::install_github("samclifford/mgcv.helper")
library(mgcv.helper)#Author: Sam Clifford (2021)

#contrasting GAMs with and without spatial dependence
library(gratia)# Author: Gavin L. Simpson (2021)
library(plyr)# Author: Hadley Wickham (2020)
library(dplyr)# Author: Hadley Wickham et al. (2021)
library(rgdal)# Author: Roger Bivand et al. (2021)
library(magrittr)# Author: Stefan Bache et al. (2020)
library(gstat)# Author: Edzer Pebesma and Benedikt Graeler (2021)
library(MASS)# Author: Brian Ripley et al. (2021)
library(visreg)# Author: Patrick Breheny and Woodrow Burchett (2020)
library(lmerTest)# Author: Per", "Bruun Brockhoff et al. (2020)
library(nlme)# Author: José Pinheiro et al. (2021)    # gamm for lme() in nlme package
library(emmeans)# Author: Russell V. Lent et al (2021)
library(MuMIn)#Author: Kamil Barton (2020)

#MMRR analyses
library(vegan)# Author: Jari Oksanen et al. (2020)
library(geosphere)# Author: Robert J. Hijmans et al. (2019)
library(MASS)# Author: Brian Ripley et al. (2021)
library(ade4)# Author: Stéphane Dray et al. (2021) 
# Read the matrices from files.
# The read.matrix function requires {tseries} package to be installed and loaded.
# If the files have a row as a header (e.g. column names), then specify 'header=TRUE', default is 'header=FALSE'.
library(tseries)# Author: Adrian Trapletti et al. (2020)

#Figures
library(ggplot2)# Author: Hadley Wickham et al. (2020)
library(scam)# Author: Natalya Pya (2021)
library(cowplot)# Author: Claus O. Wilke (2020)
library(tidyr)# Author: Hadley Wickham et al. (2021)

#########################################################################################
##########################################################################################################
#Script for GAM/BAM models and Figures
# In Fig3 and Table S4, We compared model with spatial dependence and non-dependence with Auto Correlation Function Plots (ACF) and AIC. 
#Models with and without spatial dependence were were essentially the same under AICc
#loading file for figure 3
song=read.csv2("maleleucopteraatra_means.csv", head=TRUE)

#Models and script for Figure 3 and Table S4
song$SiteName<-as.factor(song$SiteName)
typeof(song$SiteName)

#fitting GAM without spatial dependence
#Song PC1
songfitpc1 <- bam(PC1 ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = song, method = "fREML")

#Song PC2
songfitpc2 <- bam(PC2 ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = song, method = "fREML")

#Note frequency
songfitHiFreqMean <- bam(HiFreqMean ~ s(Latsouth, k = 60) + s(SiteName, bs ="re"), data = song, method = "fREML")

#Note length
songfitNoteLenMean <- bam(NoteLenMean ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = song, method = "fREML")

#Note interval
songfitSpaceLenMean <- bam(SpaceLenMean ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = song, method = "fREML")

#Note number
songfitPaceNotes <- bam(PaceNotes ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = song, Family = quasipoisson(link = "log"), method = "fREML")

summary(songfitpc1)
summary(songfitpc2)
summary(songfitHiFreqMean)
summary(songfitNoteLenMean)
summary(songfitSpaceLenMean)
summary(songfitPaceNotes)

gam.check(songfitpc1)
gam.check(songfitpc2)
gam.check(songfitHiFreqMean)
gam.check(songfitNoteLenMean)
gam.check(songfitSpaceLenMean)
gam.check(songfitPaceNotes)

appraise(songfitpc1)
appraise(songfitpc2)
appraise(songfitHiFreqMean)
appraise(songfitNoteLenMean)
appraise(songfitSpaceLenMean)
appraise(songfitPaceNotes)

draw(songfitpc1)
draw(songfitpc2)
draw(songfitHiFreqMean)
draw(songfitNoteLenMean)
draw(songfitSpaceLenMean)
draw(songfitPaceNotes)


#acf plots to check residual autocorrelation
# Getting a rough estimate of the correlation between adjacent errors. 
# We expect the residuals of the model to be independent (ie not autocorrelated)
acf(resid(songfitpc1), lag.max = 496, main = "ACF")
acf(resid(songfitpc2), lag.max = 496, main = "ACF")
acf(resid(songfitHiFreqMean), lag.max = 496, main = "ACF")
acf(resid(songfitNoteLenMean), lag.max = 496, main = "ACF")
acf(resid(songfitSpaceLenMean), lag.max = 496, main = "ACF")
acf(resid(songfitPaceNotes), lag.max = 496, main = "ACF")


songdat <- start_event(song, column="CutNumber", event=c("Latsouth", "Numberac"), label.event="Event")
head(songdat)
tail(songdat)

#obtaing the rho/AR(1) parameter
r1songfitpc1 <- start_value_rho(songfitpc1)
r1songfitpc1
r1songfitpc2 <- start_value_rho(songfitpc2)
r1songfitpc2
r1songfitHiFreqMean <- start_value_rho(songfitHiFreqMean)
r1songfitHiFreqMean
r1songfitNoteLenMean <- start_value_rho(songfitNoteLenMean)
r1songfitNoteLenMean
r1songfitSpaceLenMean <- start_value_rho(songfitSpaceLenMean)
r1songfitSpaceLenMean
r1songfitPaceNotes <- start_value_rho(songfitPaceNotes)
r1songfitPaceNotes

#fitting BAM with spatial dependence
#In Bam, generalized models do not go together with AR! parameter
songfitAR1pc1 <- bam(PC1 ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = songdat, method = "fREML", 
                     rho=r1songfitpc1, AR.start=songdat$start.event)
songfitAR1pc2 <- bam(PC2 ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = songdat, method = "fREML", 
                     rho=r1songfitpc2, AR.start=songdat$start.event)
songfitHiFreqMeanAR1 <- bam(HiFreqMean ~ s(Latsouth, k=60) + s(SiteName, bs ="re"), data = songdat, method = "fREML", 
                            rho=r1songfitHiFreqMean, AR.start=songdat$start.event)
songfitNoteLenMeanAR1 <- bam(NoteLenMean ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = songdat, method = "fREML", 
                             rho=r1songfitNoteLenMean, AR.start=songdat$start.event)
songfitSpaceLenMeanAR1 <- bam(SpaceLenMean ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = songdat, method = "fREML", 
                              rho=r1songfitSpaceLenMean, AR.start=songdat$start.event)
songfitPaceNotesAR1 <- bam(PaceNotes ~ s(Latsouth, k=40) + s(SiteName, bs ="re"), data = songdat, Family=quasipoisson(link="log"), method = "fREML", 
                           rho=r1songfitPaceNotes, AR.start=songdat$start.event)

#Song PC1
r1 <- start_value_rho(songfitAR1pc1, plot=TRUE)
#Uncorrected versus corrected residuals:
par(mfrow=c(1,2), cex=1.1)
acf_resid(songfitpc1)
acf_resid(songfitAR1pc1)

c(AICc(songfitpc1), AICc(songfitAR1pc1))
AICc(songfitpc1,songfitAR1pc1)

#Song PC2
summary(songfitAR1pc2)
gam.check(songfitAR1pc2)
appraise(songfitAR1pc2)
draw(songfitAR1pc2)
r1 <- start_value_rho(songfitAR1pc2, plot=TRUE)
#Uncorrected versus corrected residuals:
par(mfrow=c(1,2), cex=1.1)
acf_resid(songfitpc2)
acf_resid(songfitAR1pc2)

c(AICc(songfitpc2), AICc(songfitAR1pc2))
AICc(songfitpc2,songfitAR1pc2)

#Note frequency
summary(songfitHiFreqMeanAR1)
gam.check(songfitHiFreqMeanAR1)
appraise(songfitHiFreqMeanAR1)
draw(songfitHiFreqMeanAR1)
r1 <- start_value_rho(songfitHiFreqMeanAR1, plot=TRUE)
#Uncorrected versus corrected residuals:
par(mfrow=c(1,2), cex=1.1)
acf_resid(songfitHiFreqMean)
acf_resid(songfitHiFreqMeanAR1)
AIC(songfitHiFreqMean,songfitHiFreqMeanAR1)
summary(songfitNoteLenMeanAR1)
gam.check(songfitNoteLenMeanAR1)
appraise(songfitNoteLenMeanAR1)
draw(songfitNoteLenMeanAR1)

#Note length
r1 <- start_value_rho(songfitNoteLenMeanAR1, plot=TRUE)
#Uncorrected versus corrected residuals:
par(mfrow=c(1,2), cex=1.1)
acf_resid(songfitNoteLenMean)
acf_resid(songfitNoteLenMeanAR1)

c(AICc(songfitNoteLenMean), AICc(songfitNoteLenMeanAR1))
AICc(songfitNoteLenMean,songfitNoteLenMeanAR1)

#Note interval
r1 <- start_value_rho(songfitSpaceLenMeanAR1, plot=TRUE)
#Uncorrected versus corrected residuals:
par(mfrow=c(1,2), cex=1.1)
acf_resid(songfitSpaceLenMean)
acf_resid(songfitSpaceLenMeanAR1)

c(AICc(songfitSpaceLenMean), AICc(songfitSpaceLenMeanAR1))
AICc(songfitSpaceLenMean,songfitSpaceLenMeanAR1)

#Number of notes
r1 <- start_value_rho(songfitPaceNotesAR1, plot=TRUE)
#Uncorrected versus corrected residuals:
par(mfrow=c(1,2), cex=1.1)
acf_resid(songfitPaceNotes)
acf_resid(songfitPaceNotesAR1)

c(AICc(songfitPaceNotes), AICc(songfitPaceNotesAR1))
AICc(songfitPaceNotes,songfitPaceNotesAR1)

###############################################################################################
#Plotting the song variables from GAM/BAM as the panel in Figure 3
#code modified from Gavin Simpson tutorial (https://fromthebottomoftheheap.net/)
###############################################################################################
#plot for song PC1 - model without spatial dependence was best fit
# graph of fitted values for Song PC1 vs Latitude
N <- 496 # number of points at which to evaluate the smooth

newsong <- with(song, data.frame(Latsouth = seq(min(Latsouth), max(Latsouth),
                                                length.out = 496),SiteName = c(1:496)))

## Predict from the fitted model
newsong <- cbind(newsong,
                 data.frame(predict(songfitpc1, newsong, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(songfitpc1))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
# scatter plot malesPC1
small_fittedmalespc1=ggplot(newsong,series = Latsouth, aes(x = Latsouth, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Latsouth), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(17, 16, 16)) +  scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(2, 2,2))+labs(y="Song PC2",x="Latitude (S)", face="bold", color="black")+theme(axis.text = element_text(size = 18, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=18), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),plot.margin = margin(1,0.5,0.5,0.5, "cm"),   axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=18, face="bold"),
                                                                                                                                                                                                                                                                         axis.text.y=element_text(size=18, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_x_continuous(breaks = seq(10,30,2))+ scale_y_continuous(breaks = seq(-2,10,1))+
  geom_point(data = song, mapping = aes(x = Latsouth, y = PC1, shape=TaxonName, color=TaxonName, size=TaxonName),
             inherit.aes = FALSE) +
  geom_line() +
  labs(x="Latitude (S)",y="Song PC1", face="bold")
small_fittedmalespc1

##########################################################################################
#plot for song PC2 - model with spatial dependence was best fit
# graph of fitted values for Song PC2 vs Latitude
N <- 496 # number of points at which to evaluate the smooth
newsong <- with(songdat, data.frame(Latsouth = seq(min(Latsouth), max(Latsouth),
                                                   length.out = 496),SiteName = c(1:496)))


## Predict from the fitted model
newsong <- cbind(newsong,
                 data.frame(predict(songfitAR1pc2, newsong, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(songfitAR1pc2))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
# scatter plot malesPC2
small_fittedmalespc2=ggplot(newsong,series = Latsouth, aes(x = Latsouth, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Latsouth), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(17, 16, 16)) +  scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(2, 2,2))+labs(y="Song PC2",x="Latitude (S)", face="bold", color="black")+theme(axis.text = element_text(size = 18, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=18), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  plot.margin = margin(1,0.5,0.5,0.5, "cm"), axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=18, face="bold"),
                                                                                                                                                                                                                                                                         axis.text.y=element_text(size=18, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_x_continuous(breaks = seq(10,30,2))+ scale_y_continuous(breaks = seq(-4,5,1))+
  geom_point(data = songdat, mapping = aes(x = Latsouth, y = PC2, shape=TaxonName, color=TaxonName, size=TaxonName),
             inherit.aes = FALSE) +
  geom_line() +
  labs(x="Latitude (S)",y="Song PC2", face="bold")
small_fittedmalespc2

##########################################################################################
#plot for Note frequency - model with spatial dependence was best fit
# graph of fitted values for Highfreq vs Latitude
N <- 496 # number of points at which to evaluate the smooth
newsong <- with(songdat, data.frame(Latsouth = seq(min(Latsouth), max(Latsouth),
                                                   length.out = 496),SiteName = c(1:496)))


## Predict from the fitted model
newsong <- cbind(newsong,
                 data.frame(predict(songfitHiFreqMeanAR1, newsong, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(songfitHiFreqMeanAR1))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
# scatter plot malesPC2
small_fittedmaleshighfreq=ggplot(newsong,series = Latsouth, aes(x = Latsouth, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Latsouth), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(17, 16, 16)) +  scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(2, 2,2))+labs(y="Note maximum frequency (kHz)",x="Latitude (S)", face="bold", color="black")+theme(axis.text = element_text(size = 18, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=18), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"), plot.margin = margin(1,0.5,0.5,0.5, "cm"), axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=18, face="bold"),
                                                                                                                                                                                                                                                                                             axis.text.y=element_text(size=18, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_x_continuous(breaks = seq(10,30,2))+ scale_y_continuous(breaks = seq(2000,3500,200))+
  geom_point(data = songdat, mapping = aes(x = Latsouth, y =  HiFreqMean, shape=TaxonName, color=TaxonName, size=TaxonName),
             inherit.aes = FALSE) +
  geom_line() +
  labs(x="Latitude (S)",y="Note maximum frequency (kHz)", face="bold")
small_fittedmaleshighfreq

################################################################################################
#plot for Note Length - model without spatial dependence was best fit
# graph of fitted values for Note Length vs Latitude
N <- 496 # number of points at which to evaluate the smooth
newsong <- with(song, data.frame(Latsouth = seq(min(Latsouth), max(Latsouth),
                                                length.out = 496),SiteName = c(1:496)))


## Predict from the fitted model
newsong <- cbind(newsong,
                 data.frame(predict(songfitNoteLenMean, newsong, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(songfitNoteLenMean))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
# scatter plot malesNoteLenMean
small_fittedmalesNoteLenMean=ggplot(newsong,series = Latsouth, aes(x = Latsouth, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Latsouth), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(17, 16, 16)) +  scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(2, 2,2))+labs(y="Note length (ms)",x="Latitude (S)", face="bold", color="black")+theme(axis.text = element_text(size = 18, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=18), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"), plot.margin = margin(1,0.5,0.5,0.5, "cm"), axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=18, face="bold"),
                                                                                                                                                                                                                                                                                 axis.text.y=element_text(size=18, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_x_continuous(breaks = seq(10,30,2))+ scale_y_continuous(breaks = seq(0,300,30))+
  geom_point(data = song, mapping = aes(x = Latsouth, y =  NoteLenMean, shape=TaxonName, color=TaxonName, size=TaxonName),
             inherit.aes = FALSE) +
  geom_line() +
  labs(x="Latitude (S)",y="Note length (ms)", face="bold")
small_fittedmalesNoteLenMean

################################################################################################
#plot for Note interval - model without spatial dependence was best fit
# graph of fitted values for Note Space vs Latitude
N <- 496 # number of points at which to evaluate the smooth
newsong <- with(song, data.frame(Latsouth = seq(min(Latsouth), max(Latsouth),
                                                length.out = 496),SiteName = c(1:496)))


## Predict from the fitted model
newsong <- cbind(newsong,
                 data.frame(predict(songfitSpaceLenMean, newsong, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(songfitSpaceLenMean))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
# scatter plot malesNotespaceMean
small_fittedmalesSpaceLenMean=ggplot(newsong,series = Latsouth, aes(x = Latsouth, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Latsouth), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(17, 16, 16)) +  scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(2, 2,2))+labs(y="Note interval (ms)",x="Latitude (S)", face="bold", color="black")+theme(axis.text = element_text(size = 18, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=18), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"), plot.margin = margin(1,0.5,0.5,0.5, "cm"), axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=18, face="bold"),
                                                                                                                                                                                                                                                                                   axis.text.y=element_text(size=18, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_x_continuous(breaks = seq(10,30,2))+ scale_y_continuous(breaks = seq(0,300,30))+
  geom_point(data = song, mapping = aes(x = Latsouth, y =  SpaceLenMean, shape=TaxonName, color=TaxonName, size=TaxonName),
             inherit.aes = FALSE) +
  geom_line() +
  labs(x="Latitude (S)",y="Note interval (ms)", face="bold")
small_fittedmalesSpaceLenMean

################################################################################################
#plot for Note Number - model without spatial dependence was best fit
# graph of fitted values for Note Numbe vs Latitude
N <- 496 # number of points at which to evaluate the smooth
newsong <- with(song, data.frame(Latsouth = seq(min(Latsouth), max(Latsouth),
                                                length.out = 496),SiteName = c(1:496)))


## Predict from the fitted model
newsong <- cbind(newsong,
                 data.frame(predict(songfitPaceNotes, newsong, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(songfitPaceNotes))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
# scatter plot malesNotenumber
small_fittedmalesPaceNotes=ggplot(newsong,series = Latsouth, aes(x = Latsouth, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Latsouth), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(17, 16, 16)) +  scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(2, 2,2))+labs(y="Number of notes",x="Latitude (S)", face="bold", color="black")+theme(axis.text = element_text(size = 18, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=18), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"), plot.margin = margin(1,0.5,0.5,0.5, "cm"), axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=18, face="bold"),
                                                                                                                                                                                                                                                                                axis.text.y=element_text(size=18, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +scale_x_continuous(breaks = seq(10,30,2))+ scale_y_continuous(breaks = seq(0,15,3))+
  geom_point(data = song, mapping = aes(x = Latsouth, y =  PaceNotes, shape=TaxonName, color=TaxonName, size=TaxonName),
             inherit.aes = FALSE) +
  geom_line() +
  labs(x="Latitude (S)",y="Number of notes", face="bold")
small_fittedmalesPaceNotes

#****************************************************************************************
#Function for a panel with many plots

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggpubr")
ipak(packages)

#****************************************************************************************
# ggarrange 
#****************************************************************************************
#If you need to arrange many panels, can use ggarrange 
#jpeg and dev.off() go together

jpeg("Figure3_December_2022.jpeg", width = 10, height = 20, units = 'in', res=600) #set size and resolution
ggarrange(small_fittedmaleshighfreq,
          small_fittedmalesPaceNotes,
          small_fittedmalesNoteLenMean,
          small_fittedmalesSpaceLenMean,
          small_fittedmalespc1,
          small_fittedmalespc2,
          #ggplot objects
          labels = c("A", "B", "c", "D", "E", "F"), 
          font.label = list(size=32),#if you want to label panels
          ncol = 2, nrow = 3) #specify dimensions
dev.off()
####################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#loading file for other figures
malesong=read.csv2("maleleucoptera_indivfinal_2022.csv", head=TRUE, fileEncoding="latin1")

songprob=read.csv2("maleleucopteraatra_meansforDFA-allinfo.csv", head=TRUE)

songprobqvaluestwospecies=read.csv2("PosteriorprobvsQ_atra_leucop_November2021.csv", head=TRUE)

songvar=read.csv2("Freq_variance_males.csv", head=TRUE)
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure 5
#Figure 5A
#songpostprob vs geo distance
plota=ggplot(songprob, aes(DistrefHZ, PleucopteraposterprobR)) +
  geom_point(mapping = aes(x = DistrefHZ, y = PleucopteraposterprobR, shape=Species,color=Species,size=Species)) + 
  scale_shape_manual(values = c(17, 16, 16)) + 
  scale_color_manual(values = c("mediumblue", "red", "black")) + 
  scale_size_manual(values = c(3, 2,2))+labs(y="Posterior probability",x="Distance from the hybrid zone (KM)", face="bold", color="black")+
  theme(axis.text = element_text(size = 20, color='black'), 
        axis.title = element_text(family = "Times", color="black",face="bold", size=20), 
        legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), 
        panel.border = element_rect(fill = NA, color = "black"), plot.margin = margin(1,0.5,0.5,0.5, "cm"),  
        axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), 
        axis.text.x=element_text(size=20, face="bold"), axis.text.y=element_text(size=20, face="bold")) + 
  coord_cartesian(ylim = c(-0.002, 1.06)) + 
  scale_x_continuous(breaks = seq(-500, 2000,250))+ 
  scale_y_continuous(breaks = seq(0.0,1,0.25))+
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 6), color="black")
plota

#Figure 5B
#songpostprob vs average q-values(for both species)
plotx=ggplot(songprobqvaluestwospecies, aes(Q_atra_k4log, PleucopteraposterprobR)) +
  geom_point(mapping = aes(x = Q_atra_k4log, y = PleucopteraposterprobR, shape=Species,color=Species,size=Species)) + 
  scale_shape_manual(values = c(17, 16, 16)) + scale_color_manual(values = c("mediumblue", "red", "black")) + 
  scale_size_manual(values = c(3, 2,2))+ 
  labs(y="Posterior probability",x="Coefficient of admixture (log)", face="bold", color="black")+
  theme(axis.text = element_text(size = 20, color='black'), 
        axis.title = element_text(family = "Times", color="black",face="bold", size=20), 
        legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), 
        panel.border = element_rect(fill = NA, color = "black"),  plot.margin = margin(1,0.5,0.5,0.5, "cm"), 
        axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), 
        axis.text.x=element_text(size=20, face="bold"),axis.text.y=element_text(size=20, face="bold")) + 
  coord_cartesian(ylim = c(-0.002, 1.06)) + scale_x_continuous(breaks = seq(-6, 0,1))+ 
  scale_y_continuous(breaks = seq(0.0,1,0.25)) +  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 6), color="black")
plotx

#Figure 5C
#songpostprob vs introgression
Fullpostprobmodel<-gam(PleucopteraposterprobR ~ s(Q_atra_k4log),family=betar(link='logit'),
                       data=malesong, method = "REML")

summary(Fullpostprobmodel)
gam.check(Fullpostprobmodel)

#Code below modified from Gavin Simpson tutorial (https://fromthebottomoftheheap.net/)
# graph of fitted values for Posterior probability vs geo distances to HZ
N <- 196 # number of points at which to evaluate the smooth
newsong <- with(malesong, data.frame(Q_atra_k4log = seq(min(Q_atra_k4log), max(Q_atra_k4log),
                                                        length.out = 196),SiteName = c(1:196)))


## Predict from the fitted model; note we predict from the $gam part
newsong <- cbind(newsong,
                 data.frame(predict(Fullpostprobmodel, newsong, se.fit = TRUE, type = "response")))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(Fullpostprobmodel))
newsong <- transform(newsong,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
plotz <- ggplot(newsong,series =Q_atra_k4log, aes(x = Q_atra_k4log, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = Q_atra_k4log), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") + scale_shape_manual(values = c(16)) +  
  scale_size_manual(values = c(3))+labs(y="Posterior probability",x="Coefficient of ancestry", face="bold", color="black")+
  theme(axis.text = element_text(size = 20, color='black'), 
        axis.title = element_text(family = "Times", color="black",face="bold", size=20), 
        legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), 
        panel.border = element_rect(fill = NA, color = "black"), plot.margin = margin(1,0.5,0.5,0.5, "cm"),  
        axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), 
        axis.text.x=element_text(size=20, face="bold"), axis.text.y=element_text(size=20, face="bold")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(values = c("black", "black", "black"))+
  scale_x_continuous(breaks = seq(-6,0,1))+ scale_y_continuous(breaks = seq(0,1,0.2))+
  geom_point(data = malesong, mapping = aes(x = Q_atra_k4log, y = PleucopteraposterprobR),inherit.aes = FALSE) +
  geom_line() +
  labs(x="Coefficient of admixture (log)",y="Posterior probability", face="bold")
plotz

#Figure 5D
#code for variance note freq vs Latitude
plotb=ggplot(songvar, aes(Lat, HiFreqMean.var)) +
  geom_point(mapping = aes(x = Lat, y = HiFreqMean.var, shape=Species,color=Species,size=Species)) + 
  scale_shape_manual(values = c(16, 16, 16, 16, 17)) + 
  scale_color_manual(values = c("red", "red", "black", "black", "mediumblue")) + 
  scale_size_manual(values = c(2,2,2,2,3))+
  labs(y="Note maximum frequency (kHz) variance",x="Latitude (S)", face="bold", color="black")+
  theme(axis.text = element_text(size = 20, color='black'), 
        axis.title = element_text(family = "Times", color="black",face="bold", size=20), 
        legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), 
        panel.border = element_rect(fill = NA, color = "black"),  plot.margin = margin(1,0.5,0.5,0.5, "cm"), 
        axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), 
        axis.text.x=element_text(size=20, face="bold"),axis.text.y=element_text(size=20, face="bold")) + 
  scale_y_continuous(breaks = seq(0,100,10)) + scale_x_continuous(breaks = seq(10, 30,2))

plotb

jpeg("Figure5biglegendaz_December2021.jpeg", width = 16, height = 14, units = 'in', res=600) #set size and resolution
ggarrange(plota,plotx,plotz,plotb, #ggplot objects
          labels = c("A", "B", "C", "D"), 
          font.label = list(size=32), #if you want to label panels
          ncol = 2, nrow = 2) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure 6
# code for high freq vs body size- males
plot6a=ggplot(malesong, aes(x = Pcwingtarsus , y = HiFreq) ) +
  geom_point(mapping = aes(x = Pcwingtarsus, y = HiFreq)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note maximum frequency (Hz)",x="Body size PC", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                            axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-1.0, 1, 0.5))+ scale_y_continuous(breaks = seq(2100, 3200, 200))+ geom_smooth(method = "lm", color="black")

plot6a

# code for Note Length vs NDVI - males
plot6b=ggplot(malesong, aes(x = NDVI, y = NoteLen) ) +
  geom_point(mapping = aes(x = NDVI, y = NoteLen)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note length (ms)",x="Forest cover", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                          axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 1, 0.1))+ scale_y_continuous(breaks = seq(80, 220, 20))+ geom_smooth(method = "lm", color="black")
plot6b

# code for Note Length vs introgression - males
plot6c=ggplot(malesong, aes(x = Q_atra_k4log, y = NoteLen) ) +
  geom_point(mapping = aes(x = Q_atra_k4log, y = NoteLen)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note length (ms)",x="Coefficient of admixture (log)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                    axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-6, 0, 1))+ scale_y_continuous(breaks = seq(80, 220, 20)) +  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), color="black")

plot6c

# code for vocal PC1 vs introgression- males
plot6d=ggplot(malesong, aes(x = Q_atra_k4log, y = PC1) ) +
  geom_point(mapping = aes(x = Q_atra_k4log, y = PC1)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC1",x="Coefficient of admixture (log)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                   axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-6, 0, 1))+ scale_y_continuous(breaks = seq(-3, 2, 0.5)) +  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), color="black")

plot6d

# code for Vocal PC2 vs NDVI - males
plot6e=ggplot(malesong, aes(x = NDVI, y = PC2) ) +
  geom_point(mapping = aes(x = NDVI, y = PC2)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC2",x="Forest cover", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                               axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 1, 0.1))+ scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5))+ geom_smooth(method = "lm", color="black")

plot6e

# code for Vocal PC2 vs introgression - males
plot6f=ggplot(malesong, aes(x = Q_atra_k4log, y = PC2) ) +
  geom_point(mapping = aes(x = Q_atra_k4log, y = PC2)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC2",x="Coefficient of admixture (log)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                         axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-6, 0, 1))+ scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5)) +  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), color="black")

plot6f

jpeg("Figure6biglegend_November_2022.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot6a, plot6b, plot6c, plot6d, plot6e, plot6f,  #ggplot objects
          labels = c("A", "B", "C", "D", "E", "F"), #if you want to label panels
          ncol = 3, nrow = 2) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S2
malesqvaldist=read.csv2("Q_values_allindiv_vsGeodistHZ_December2021.csv", head=TRUE)

dim(malesqvaldist)
summary(malesqvaldist)

Qvalueatralog<- log(malesqvaldist$Qatra_K4)

hist(Qvalueatralog)
Qvalueatralog

write.csv2(Qvalueatralog, file="Q_atra_log_natural.csv")

jpeg("Q-valuevsgeodistHZ_March2022.jpeg", width = 9, height = 9, units = 'in', res = 300)
ggplot(malesqvaldist, aes(DistHZ, Qatra_K4)) +
  geom_point(mapping = aes(x = DistHZ, y = Qatra_K4)) + scale_shape_manual(values = c(16)) + scale_color_manual(values = c("black")) + scale_size_manual(values = c(3))+labs(y="Coefficient of admixture",x="Distance from the hybrid zone (KM)", face="bold", color="black")+theme(axis.text = element_text(size = 20, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=20), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=20, face="bold"),
                                                                                                                                                                                                                                                                                    axis.text.y=element_text(size=20, face="bold")) + coord_cartesian(ylim = c(-0.002, 1.06)) + scale_x_continuous(breaks = seq(0, 2000,250))+ scale_y_continuous(breaks = seq(0,1,0.2)) 
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S3
# code for high freq vs geo distance - males
plot1a=ggplot(malesong, aes(x = GeodistanceNorthleucoptera, y = HiFreq) ) +
  geom_point(mapping = aes(x = GeodistanceNorthleucoptera, y = HiFreq)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note maximum frequency (Hz)",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                                      axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 2000, 400))+ scale_y_continuous(breaks = seq(0, 3500, 200))+ geom_smooth(method = "lm", color="black")

plot1a

# code for Note Length vs geo distance - males
plot2a=ggplot(malesong, aes(x = GeodistanceNorthleucoptera, y = NoteLen) ) +
  geom_point(mapping = aes(x = GeodistanceNorthleucoptera, y = NoteLen)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note length (ms)",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                            axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 2000, 400))+ scale_y_continuous(breaks = seq(80, 220, 20))+ geom_smooth(method = "lm", color="black")

plot2a

# code for note interval vs geo distance - males
plot3a=ggplot(malesong, aes(x = GeodistanceNorthleucoptera, y = SpaceLen) ) +
  geom_point(mapping = aes(x = GeodistanceNorthleucoptera, y = SpaceLen)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note interval (ms)",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                               axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 2000, 400))+ scale_y_continuous(breaks = seq(50, 200, 20))+ geom_smooth(method = "lm", color="black")

plot3a
# code for note number vs geo distance - males
plot4a=ggplot(malesong, aes(x = GeodistanceNorthleucoptera, y = PaceNotes) ) +
  geom_point(mapping = aes(x = GeodistanceNorthleucoptera, y = PaceNotes)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Number of notes",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                             axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 2000, 400))+ scale_y_continuous(breaks = seq(0, 12, 2))+ geom_smooth(method = "lm", color="black")

plot4a
# code for vocal pc1 vs geo distance - males
plot5a=ggplot(malesong, aes(x = GeodistanceNorthleucoptera, y = PC1) ) +
  geom_point(mapping = aes(x = GeodistanceNorthleucoptera, y = PC1)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC1",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                 axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 2000, 400))+ scale_y_continuous(breaks = seq(-3, 2, 0.5))+ geom_smooth(method = "lm", color="black")

plot5a
# code for Vocal PC2 vs geo distance - males
plot6a=ggplot(malesong, aes(x = GeodistanceNorthleucoptera, y = PC2) ) +
  geom_point(mapping = aes(x = GeodistanceNorthleucoptera, y = PC2)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC2",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                 axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 2000, 400))+ scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5))+ geom_smooth(method = "lm", color="black")

plot6a
#Example of ggarrange
jpeg("Figure_SM5_IBD_2022.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot1a, plot2a, plot3a, plot4a, plot5a, plot6a, #ggplot objects
          labels = c("A", "B", "C", "D", "E", "F"), #if you want to label panels
          ncol = 3, nrow = 2) #specify dimensions
dev.off()


###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S4
songallop=read.csv2("lda_scores_posterior_indivDFA-allopatric_only_March2022.csv", head=TRUE)

head (songallop)
summary(songallop)

# Basic box plot
p <- ggplot(songallop, aes(x=Species, y=LD1)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(songallop, aes(x=Species, y=LD1)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(songallop, aes(x=Species, y=LD1)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

p + stat_summary(fun=mean, geom="point", shape=18, size=6)

plotS3 = p + geom_jitter(shape=16, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Species", y = "Canonical Axis 1",face="bold", color="black")+
  theme(axis.text = element_text(size = 14, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=14), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=12, face="bold"),
        axis.text.y=element_text(size=12, face="bold")) + scale_y_continuous(breaks = seq(-5,5,1))
plotS3
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S5
malevariance<-read.csv2("variance_allosvscontact_boxplots_2022.csv", head=TRUE)

dim (malevariance)
summary(malevariance)


# Number of Notes
p <- ggplot(malevariance, aes(x=Group, y=PaceNotes)) + 
  geom_boxplot()
p
# Rotate the box plot
#p + coord_flip()
# Notched box plot
ggplot(malevariance, aes(x=Group, y=PaceNotes)) + 
  geom_boxplot(notch=FALSE)
# Change outlier, color, shape and size
ggplot(malevariance, aes(x=Group, y=PaceNotes)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

plot1 = p + geom_jitter(shape=16, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Group", y = "Number of notes",face="bold", color="black")+
  theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24, face="bold")) + scale_y_continuous(breaks = seq(0,12,2))
plot1


# Note length
p <- ggplot(malevariance, aes(x=Group, y=NoteLenMean)) + 
  geom_boxplot()
p
# Rotate the box plot
#p + coord_flip()
# Notched box plot
ggplot(malevariance, aes(x=Group, y=NoteLenMean)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(malevariance, aes(x=Group, y=NoteLenMean)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

plot2 = p + geom_jitter(shape=24, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Group", y = "Note length (ms)",face="bold", color="black")+
  theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24, face="bold")) + scale_y_continuous(breaks = seq(80,220,20))
plot2

# Note interval
p <- ggplot(malevariance, aes(x=Group, y=SpaceLenMean)) + 
  geom_boxplot()
p
# Rotate the box plot
#p + coord_flip()
# Notched box plot
ggplot(malevariance, aes(x=Group, y=SpaceLenMean)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(malevariance, aes(x=Group, y=SpaceLenMean)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

plot3 = p + geom_jitter(shape=24, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Group", y = "Note interval (ms)",face="bold", color="black")+
  theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24, face="bold")) + scale_y_continuous(breaks = seq(60,220,20))
plot3


# Note frequency
p <- ggplot(malevariance, aes(x=Group, y=HiFreqMean)) + 
  geom_boxplot()
p
# Rotate the box plot
#p + coord_flip()
# Notched box plot
ggplot(malevariance, aes(x=Group, y=HiFreqMean)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(malevariance, aes(x=Group, y=HiFreqMean)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

plot4 = p + geom_jitter(shape=24, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Group", y = "Note frequency (kHz)",face="bold", color="black")+
  theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24, face="bold")) + scale_y_continuous(breaks = seq(2000,3400,200))
plot4

# PC1
p <- ggplot(malevariance, aes(x=Group, y=PC1)) + 
  geom_boxplot()
p
# Rotate the box plot
#p + coord_flip()
# Notched box plot
ggplot(malevariance, aes(x=Group, y=PC1)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(malevariance, aes(x=Group, y=PC1)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

plot5 = p + geom_jitter(shape=24, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Group", y = "Song PC1",face="bold", color="black")+
  theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24, face="bold")) + scale_y_continuous(breaks = seq(-4,3,0.5))
plot5


# PC2
p <- ggplot(malevariance, aes(x=Group, y=PC2)) + 
  geom_boxplot()
p
# Rotate the box plot
#p + coord_flip()
# Notched box plot
ggplot(malevariance, aes(x=Group, y=PC2)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(malevariance, aes(x=Group, y=PC2)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=18, size=6)

# Box plot with jittered points
# 0.2 : degree of jitter in x direction

plot6 = p + geom_jitter(shape=24, position=position_jitter(0.2)) + geom_boxplot(colour="black",alpha=0)+ labs(x="Group", y = "Song PC2",face="bold", color="black")+
  theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'),
        panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
        axis.text.y=element_text(size=24, face="bold")) + scale_y_continuous(breaks = seq(-4,4,0.5))
plot6

#****************************************************************************************
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggpubr")
ipak(packages)

#****************************************************************************************
# ggarrange or multiplot ####
#****************************************************************************************
#If you need to arrange many panels, use ggarrange or multiplot
#If you have only one panel, use just the plot function with no multiplot or ggarrange
#jpeg and dev.off() go together

#Example of ggarrange. Works only for ggplot objects, I think
jpeg("variance_allovscontact.jpeg", width = 18, height = 24, units = 'in', res=600) #set size and resolution
ggarrange(plot1,plot2,plot3,plot4,plot5,plot6, #ggplot objects
          labels = c("A", "B","C","D","E","F"), #if you want to label panels
          ncol = 2, nrow = 3) #specify dimensions
dev.off()


##################################################################################################
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S6
# code for high freq vs temperature- males
plot1c=ggplot(malesong, aes(x = Temperature, y = HiFreq) ) +
  geom_point(mapping = aes(x = Temperature, y = HiFreq)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note maximum frequency (Hz)",x="Temperature °C", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                             axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 30, 2))+ scale_y_continuous(breaks = seq(2100, 3200, 200))+ geom_smooth(method = "lm", color="black")

plot1c

# code for high freq vs bill length- males
plot1d=ggplot(malesong, aes(x = Billlength, y = HiFreq) ) +
  geom_point(mapping = aes(x = Billlength, y = HiFreq)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note maximum frequency (Hz)",x="Bill length (mm)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                              axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(9, 12, 0.5))+ scale_y_continuous(breaks = seq(2100, 3200, 200))+ geom_smooth(method = "lm", color="black")

plot1d

# code for high freq vs NDVI- males
plot1e=ggplot(malesong, aes(x = NDVI, y = HiFreq) ) +
  geom_point(mapping = aes(x = NDVI, y = HiFreq)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note maximum frequency (Hz)",x="Forest cover", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                    axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 1, 0.1))+ scale_y_continuous(breaks = seq(2100, 3200, 200))+ geom_smooth(method = "lm", color="black")

plot1e

# code for high freq vs introgression- males
plot1f=ggplot(malesong, aes(x = Q_atra_k4log, y = HiFreq) ) +
  geom_point(mapping = aes(x = Q_atra_k4log, y = HiFreq)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note maximum frequency (Hz)",x="Coefficient of admixture", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                        axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-6, 0, 1))+ scale_y_continuous(breaks = seq(2100, 3200, 200)) +  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), color="black")

plot1f


#Example of ggarrange. Works only for ggplot objects, I think
jpeg("Figure_SM6_November_2022.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot1c, plot1d, plot1e, plot1f, #ggplot objects
          labels = c("A", "B", "C", "D"), #if you want to label panels
          ncol = 2, nrow = 2) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S7

# code for note interval vs forest cover - males
plot3b=ggplot(malesong, aes(x = NDVI, y = SpaceLen) ) +
  geom_point(mapping = aes(x = NDVI, y = SpaceLen)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note interval (ms)",x="Forest cover", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                             axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 1, 0.1))+ scale_y_continuous(breaks = seq(50, 200, 20))+ geom_smooth(method = "lm", color="black")

plot3b

# code for note interval vs introgression - males

plot3c=ggplot(malesong, aes(x = Q_atra_k4log, y = SpaceLen) ) +
  geom_point(mapping = aes(x =  Q_atra_k4log, y = SpaceLen)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Note interval (ms)",x="Coefficient of admixture", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                                  axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-6, 0, 1))+ scale_y_continuous(breaks = seq(50, 200, 20)) +  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3), color="black")

plot3c

#Example of ggarrange. Works only for ggplot objects, I think
jpeg("Figure_SM7_november_2022.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot3b, plot3c, #ggplot objects
          labels = c("A", "B"), #if you want to label panels
          ncol = 2, nrow = 1) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S8

plot4b=ggplot(malesong, aes(x = Q_atra_k4log, y = PaceNotes) ) +
  geom_point(mapping = aes(x = Q_atra_k4log, y = PaceNotes)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Number of notes",x="Coefficient of admixture", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                                               axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-6, 0, 1))+ scale_y_continuous(breaks = seq(0, 12, 2))+ geom_smooth(method = "lm", color="black")

plot4b
#Example of ggarrange. Works only for ggplot objects, I think
jpeg("Figure_SM8_December_2021.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot4b, #ggplot objects
          #if you want to label panels
          ncol = 1, nrow = 1) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S9
# code for vocal PC1 vs temperature- males
plot5c=ggplot(malesong, aes(x = Temperature, y = PC1) ) +
  geom_point(mapping = aes(x = Temperature, y = PC1)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC1",x="Temperature (°C)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                          axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 30, 2))+ scale_y_continuous(breaks = seq(-3, 2, 0.5))+ geom_smooth(method = "lm", color="black")

plot5c

# code for vocal PC1 vs bill length- males
plot5d=ggplot(malesong, aes(x = Billlength, y = PC1) ) +
  geom_point(mapping = aes(x = Billlength, y = PC1)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC1",x="Bill length (mm)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                         axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(9, 12, 0.5))+ scale_y_continuous(breaks = seq(-3, 2, 0.5))+ geom_smooth(method = "lm", color="black")

plot5d

# code for vocal PC1 vs NDVI- males
plot5e=ggplot(malesong, aes(x = NDVI, y = PC1)) +
  geom_point(mapping = aes(x = NDVI, y = PC1)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC1",x="Forest cover", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                               axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 1, 0.1))+ scale_y_continuous(breaks = seq(-3, 2, 0.5))+ geom_smooth(method = "lm", color="black")

plot5e

# code for vocal PC1 vs body size- males
plot5f=ggplot(malesong, aes(x = Pcwingtarsus, y = PC1) ) +
  geom_point(mapping = aes(x = Pcwingtarsus, y = PC1)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC1",x="Body size PC", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                       axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-1.0, 1, 0.5))+ scale_y_continuous(breaks = seq(-3, 2, 0.5))+ geom_smooth(method = "lm", color="black")

plot5f

#Example of ggarrange. Works only for ggplot objects, I think
jpeg("Figure_SM9_November_2022.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot5c,plot5d,plot5e,plot5f, #ggplot objects
          labels = c("A", "B","C","D"), #if you want to label panels
          ncol = 2, nrow = 2) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Figure S10
# code for vocal PC2 vs temperature- males
plot6d=ggplot(malesong, aes(x = Temperature, y = PC2) ) +
  geom_point(mapping = aes(x = Temperature, y = PC2)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC2",x="Temperature (°C)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                          axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(0, 30, 2))+ scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5))+ geom_smooth(method = "lm", color="black")

plot6d

# code for vocal PC2 vs bill length- males
plot6e=ggplot(malesong, aes(x = Billlength, y = PC2) ) +
  geom_point(mapping = aes(x = Billlength, y = PC2)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC2",x="Bill length (mm)", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                         axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(9, 12, 0.5))+ scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5))+ geom_smooth(method = "lm", color="black")

plot6e


# code for vocal PC2 vs body size- males
plot6f=ggplot(malesong, aes(x = Pcwingtarsus, y = PC2) ) +
  geom_point(mapping = aes(x = Pcwingtarsus, y = PC2)) + scale_shape_manual(values = c(24)) +  scale_size_manual(values = c(2))+labs(y="Vocal PC2",x="Body size PC", face="bold", color="black")+theme(axis.text = element_text(size = 24, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=24), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=24, face="bold"),
                                                                                                                                                                                                       axis.text.y=element_text(size=24, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(breaks = seq(-1.0, 1, 0.5))+ scale_y_continuous(breaks = seq(-2.5, 2.5, 0.5)) +  stat_smooth(method = lm, color="black")

plot6f


jpeg("Figure_SM10_2022.jpeg", width = 16, height = 14, units = 'in', res = 600) #set size and resolution
ggarrange(plot6d,plot6e,plot6f, #ggplot objects
          labels = c("A", "B","C"), #if you want to label panels
          ncol = 2, nrow = 2) #specify dimensions
dev.off()
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Discriminat Function Analysis
###Comparing songs across the range###
song=read.csv2("allrange_DFA-Zscores_2022.csv", head=TRUE)
dim(song)
head(song,3)
length(song)
##############################################################################################################
##Box's M-test for Homogeneity of Covariance Matrices. 
#If significant, reject H0(ie. var-covar matrices are homogeneous). If H0 rejected/p-value significant, use QDA instead. 
#from package biotools
boxM(song[,2:5],song$Species)
##############################################################################################
#Call to qda 
#The . means that all the other variables are used as covariates
#The prior defines the prior probabilities of class membership. 
#The prior 2 is because we have two species.
song$Species <- as.factor(song$Species)
af <- qda(formula = Species ~ .,data = song,prior = c(1,1)/2)
table(predict(af, type="class")$class, song$Species)
predictions.af<-predict(af, song)
prediction.probabilities.af<- predictions.af$posterior[,2]
predicted.classes.af<-predict(af, type="class")$class
observed.classes.af<-song$Species
confusionMatrix(predicted.classes.af, observed.classes.af)
#to generate individual values of posterior probability
qda.values=predict(af)
af
qda.values
write.csv2(qda.values, file="scores&posterior_indivDFA-allsamplesSong.csv")

##########DFA using Jacknife##############
jackknife <- qda(Species ~ ., data=song,prior = c(1,1)/2, CV=TRUE)
accJack <- table(song$Species, jackknife$class)
accJack
sum(accJack[row(accJack) == col(accJack)]) / sum(accJack)
#to generate individual values of posterior probability
head(jackknife$posterior, 10)
jackknife$posterior
write.csv2(jackknife$posterior, file="postprob_allsamples-jacknife_March_2022.csv")

##########DFA using Training sets##############
train <- sample(1:87, 87)
#above, we use the allopatric songs as a training set to classify songs elsewhere in the range
r3 <- qda(Species ~ ., data=song, prior = c(1,1)/2, subset = train) #training model
pred = predict(r3,song)
names(pred)
acc1 <-table(song$Species,pred$class)
acc1
sum(acc1[row(acc1) == col(acc1)]) / sum(acc1)
#to generate individual values of posterior probability
pqda = predict(object = r3,newdata = song[-train, ])
head(pqda$posterior, 10) 
pqda$posterior
write.csv2(pqda$posterior, file="results_DFA-Zscores-trainingvstesting-posteriorp-March2022.csv")
#end
##############################################################################################################
###Comparing songs in allopatry and in the contact zone
songallocontact=read.csv2("DFA_Zscores_allopatric_contactzone.csv", header=T)
##############################################################################################################
##  Box's M-test for Homogeneity of Covariance Matrices. 
boxM(songallocontact[,2:5],songallocontact$Species)
##############################################################################################################
songallocontact
head(songallocontact,3)
songallocontact$Species <- as.factor(songallocontact$Species)
af2 <- qda(formula = Species ~ .,data = songallocontact,prior = c(1,1)/2)
table(predict(af2, type="class")$class, songallocontact$Species)
predictions.af2<-predict(af2, songallocontact)
prediction.probabilities.af2<- predictions.af2$posterior[,2]
predicted.classes.af2<-predict(af2, type="class")$class
observed.classes.af2<-songallocontact$Species
confusionMatrix(predicted.classes.af2, observed.classes.af2)
#to generate individual values of posterior probability
qda.values2=predict(af2)
af2
qda.values2
write.csv2(qda.values2, file="scores&posterior_indivDFA-allo_contact_March2022.csv")
##########DFA using Jacknife##############
jackknife2 <- qda(Species ~ ., data=songallocontact,prior = c(1,1)/2, CV=TRUE)
head(jackknife2)
accJack2 <- table(songallocontact$Species, jackknife2$class)
accJack2
sum(accJack2[row(accJack2) == col(accJack2)]) / sum(accJack2)
#to generate individual values of posterior probability
jackknife2$posterior
write.csv2(jackknife2$posterior, file="postprob_allopatricvscontact-jacknife_March_2022.csv")
##########DFA using Training sets##############
train2 <- sample(1:84, 84)
#above, we use the allopatric songs as a training set to classify songs elsewhere in the range
r6 <- qda(Species ~ ., data=songallocontact, prior = c(1,1)/2, subset = train2) #training model
pqda = predict(object = r6,newdata = songallocontact[-train2, ]) # predictions
pred = predict(r6,songallocontact)
names(pred)
acc3 <-table(songallocontact$Species,pred$class)
acc3
sum(acc3[row(acc3) == col(acc3)]) / sum(acc3)
#to generate individual values of posterior probability
pqda2 = predict(object = r6,newdata = songallocontact[-train, ]) # predictions
head(pqda2$posterior, 10) # posterior prob.
pqda2$posterior
head(pqda2$posterior, 10) # posterior prob.
write.csv2(pqda2$posterior, file="results_DFA-Zscores-trainingvstesting-posteriorp-March2022.csv")
#end
#############################################################################################################
###Comparing songs in allopatry###
songallo=read.csv2("DFA_Zscores_allopatric-only-March_2022.csv", header=T)
#############################################################################################################
##  Box's M-test for Homogeneity of Covariance Matrices. 
boxM(songallo[,2:5],songallo$Species)
###################################################
songallo
head(songallo,3)
songallocontact
head(songallocontact,3)
songallo$Species <- as.factor(songallo$Species)
r7 <- qda(formula = Species ~ .,data = songallo,prior = c(1,1)/2)
table(predict(r7, type="class")$class, songallo$Species)
predictions.r7<-predict(r7, songallo)
prediction.probabilities.r7<- predictions.r7$posterior[,2]
predicted.classes.r7<-predict(r7, type="class")$class
observed.classes.r7<-songallo$Species
confusionMatrix(predicted.classes.r7, observed.classes.r7)
#to generate individual values of posterior probability
qda.values3=predict(r7)
r7
qda.values3
write.csv2(qda.values3, file="scores&posterior_indivDFA-allopatric_only_March2022.csv")

#to show graphically the scores we run a lda (qda cannot be used for visualization)
r8 <- lda(formula = Species ~ .,data = songallo,prior = c(1,1)/2)
qda.values4=predict(r8)
r8
qda.values4
write.csv2(qda.values4, file="lda_scores&posterior_indivDFA-allopatric_only_March2022.csv")
##########DFA using Jacknife##############
jackknife3 <- qda(Species ~ ., data=songallo,prior = c(1,1)/2, CV=TRUE)
head(jackknife3)
accJack3 <- table(songallo$Species, jackknife3$class)
accJack3
sum(accJack3[row(accJack3) == col(accJack3)]) / sum(accJack3)
#to generate individual values of posterior probability
jackknife3$posterior
write.csv2(jackknife3$posterior, file="postprob_allopatric-only-jacknife_March_2022.csv")
#end
#############################################################################################################
#############################################################################################################
#Calculating Cohen´s Kappa
#Estimating whether the number of corrected classifications from DFAs was different from random
#The function kappa was used to estimate Cohen´s Kappa, which is the unweighted option

#allopatric songs and contact zone songs
class1 <- as.table(rbind(c(63, 55), c(8, 84)))
# Compute kapa
res.k1 <- Kappa(class1)
res.k1
# Confidence intervals
confint(res.k1)

#all songs across the range
class2 <- as.table(rbind(c(120, 116), c(24, 236)))
# Compute kapa
res.k2 <- Kappa(class2)
res.k2
# Confidence intervals
confint(res.k2)
#end
#############################################################################################################
#############################################################################################################
#Estimating if the percentage of incorrect classifications from the DFAs differed between the species
#We want to know whether the proportions of incorrectly classified songs are the same in the two species

#allopatric songs and contact zone songs
res <- prop.test(x = c(55, 8), n = c(139, 71))
res 

#all songs across the range
res <- prop.test(x = c(116, 24), n = c(352, 144))
res
#end
#############################################################################################################
#############################################################################################################
#Variance F-ratio tests

#Loading files
#southern fire-eye species allopatric and contact zone song variables
leuco_fratioallop<-read.csv2("maleleucoptera_Fratio_allopat_2022.csv", head=TRUE)
leuco_fratiocontactzone<-read.csv2("maleleucoptera_Fratio_contactzone_2022.csv", head=TRUE)

#northern fire-eye species allopatric and contact zone song variables
atra_fratioallop<-read.csv2("maleatra_testeofvariance_final_march2022_allop.csv", head=TRUE)
atra_fratiocontactzone<-read.csv2("maleatra_testeofvariance_final_march2022_contactzone.csv", head=TRUE)


head(leuco_fratioallop)
summary(leuco_fratioallop)

head(leuco_fratiocontactzone)
summary(leuco_fratiocontactzone)

head(atra_fratioallop)
summary(atra_fratioallop)

head(atra_fratiocontactzone)
summary(atra_fratiocontactzone)

#estimating the variance of song variables for each song group
#southern fire-eye species allopatric group
#number of notes
var(leuco_fratioallop$PaceNotes)
#note length
var(leuco_fratioallop$NoteLen)
#note frequency
var(leuco_fratioallop$HiFreq)
#note interval
var(leuco_fratioallop$SpaceLen)
#song PC1
var(leuco_fratioallop$PC1)
#song PC2
var(leuco_fratioallop$PC2)

#southern fire-eye species contact zone group
#number of notes
var(leuco_fratiocontactzone$PaceNotes)
#note length
var(leuco_fratiocontactzone$NoteLen)
#note frequency
var(leuco_fratiocontactzone$HiFreq)
#note interval
var(leuco_fratiocontactzone$SpaceLen)
#song PC1
var(leuco_fratiocontactzone$PC1)
#song PC2
var(leuco_fratiocontactzone$PC2)

#northern fire-eye species allopatric group
#number of notes
var(atra_fratioallop$PaceNotes)
#note length
var(atra_fratioallop$NoteLen)
#note frequency
var(atra_fratioallop$HiFreq)
#note interval
var(atra_fratioallop$SpaceLen)
#song PC1
var(atra_fratioallop$PC1)
#song PC2
var(atra_fratioallop$PC2)

#northern fire-eye species contact zone group
#number of notes
var(atra_fratiocontactzone$PaceNotes)
#note length
var(atra_fratiocontactzone$NoteLen)
#note frequency
var(atra_fratiocontactzone$HiFreq)
#note interval
var(atra_fratiocontactzone$SpaceLen)
#song PC1
var(atra_fratiocontactzone$PC1)
#song PC2
var(atra_fratiocontactzone$PC2)

#these tests below are one-tail tests ("greater" in the code below) because we had a priori expectation 
#that song variance would be higher at the contact zone (see article Introduction for details). 

#southern fire-eye species: comparing song variance between allopatric and contact zone groups
#number of notes
perm.var.test(leuco_fratiocontactzone$PaceNotes, leuco_fratioallop$PaceNotes,  
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#note length
perm.var.test(leuco_fratiocontactzone$NoteLen,leuco_fratioallop$NoteLen,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#note interval
perm.var.test(leuco_fratioallop$SpaceLen,leuco_fratiocontactzone$SpaceLen,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#note frequency
perm.var.test(leuco_fratiocontactzone$HiFreq,leuco_fratioallop$HiFreq,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#song PC1
perm.var.test(leuco_fratiocontactzone$PC1,leuco_fratioallop$PC1,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#song PC2
perm.var.test(leuco_fratiocontactzone$PC2,leuco_fratioallop$PC2,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)


#northern fire-eye species: comparing song variance between allopatric and contact zone groups
#number of notes
perm.var.test(atra_fratioallop$PaceNotes,atra_fratiocontactzone$PaceNotes,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#note length
perm.var.test(atra_fratiocontactzone$NoteLen,atra_fratioallop$NoteLen,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#note interval
perm.var.test(atra_fratiocontactzone$SpaceLen,atra_fratioallop$SpaceLen,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#note frequency
perm.var.test(atra_fratiocontactzone$HiFreq,atra_fratioallop$HiFreq,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#song PC1
perm.var.test(atra_fratiocontactzone$PC1,atra_fratioallop$PC1,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)
#song PC2
perm.var.test(atra_fratiocontactzone$PC2,atra_fratioallop$PC2,   
              alternative = c("greater"), nperm = 10000, progress = TRUE)

#end
#############################################################################################################
###################################################################################################
#testing the role of song predictors
#Load data
malesong=read.csv2("maleleucoptera_indivfinal_2022.csv", head=TRUE, fileEncoding="latin1")
head(malesong)
summary(malesong)
dim(malesong)
#########################################################################################
#obtaining z-scores 
# Assuming variables to be transformed are in the columns 46-47
preObj <- preProcess(malesong[, 17:20], method=c("center", "scale"))
newData <- predict(preObj,malesong[, 17:20])
newData
write.csv2(newData, file="lat_long-zscores.csv")
#########################################################################################
#assessing the nature of variation of the coefficient of ancestry (introgression) from 
#Structure and its association with geographic distance from the hybrid zone
hist(malesong$Q_atra_k4)
cor.test(malesong$GeoDistanceHZ, malesong$Q_atra_k4log, method=c("spearman"))
plot(malesong$GeoDistanceHZ, malesong$Q_atra_k4)
##########################################################################################
#natural log transform of the ancestry coefficient
Qvalueatralog<- log(malesong$Q_atra_k4)
hist(Qvalueatralog)
write.csv2(Qvalueatralog, file="Q_atra_log_natural.csv")
###############################################################################
#Generalized additive models - GAMs

#song variable: Note Frequency
#exploratory visualization of associations
plot(malesong$Billlength,malesong$HiFreq)
plot(malesong$Pcwingtarsus,malesong$HiFreq)
plot(malesong$NDVI,malesong$HiFreq)
plot(malesong$Temperature,malesong$HiFreq)
plot(malesong$LatS,malesong$HiFreq)
plot(malesong$Q_atra_k4logzscores,malesong$HiFreq)

#fitting a gam
highfreq.model = gam(HiFreqzscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                       NDVIzscores + Temperaturezscores + 
                       s(Q_atra_k4logzscores, k=20), 
                     data=malesong, 
                     method = "REML")

#checking colinearity among parametric/linear predictors
mgcv.helper::vif.gam(highfreq.model)

#obtaining model output, checking model assumptions, diagnostics/residuals, etc. 
appraise(highfreq.model)
summary(highfreq.model)
gam.check(highfreq.model)
coef(highfreq.model)
plot.gam(highfreq.model, residuals = TRUE, pch = 1, cex = 1, pages = 1)
draw (highfreq.model)

#########################################################################################
# song variable: Note Length
#exploratory visualization of associations
plot(malesong$LatS, malesong$NoteLen)
plot(malesong$NDVI, malesong$NoteLen)
plot(malesong$Q_atra_k4logzscores, malesong$NoteLen)

#fitting a gam
notelength.model = gam(NoteLenzscores ~  LatSzscoresinv  + NDVIzscores + 
                         s(Q_atra_k4logzscores, k=20),
                       data=malesong, method = "REML")
#checking colinearity among parametric/linear predictors
mgcv.helper::vif.gam(notelength.model)


#obtaining model output, checking model assumptions, diagnostics/residuals, etc. 
appraise(notelength.model)
summary(notelength.model)
gam.check(notelength.model)
coef(notelength.model)
plot.gam(notelength.model, residuals = TRUE, pch = 1, cex = 1, pages = 1)
draw(notelength.model)

#########################################################################################
#song variable: Note interval
#exploratory visualization of associations
plot(malesong$LatS, malesong$SpaceLen)
plot(malesong$NDVI, malesong$SpaceLen)
plot(malesong$Q_atra_k4logzscores, malesong$SpaceLen)

#fitting a gam
spacelength.model = gam(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores 
                         + s(Q_atra_k4logzscores, k=20), 
                          data=malesong, method = "REML")

#checking colinearity among parametric/linear predictors
mgcv.helper::vif.gam(spacelength.model)

#obtaining model output, checking model assumptions, diagnostics/residuals, etc. 
appraise(spacelength.model)
summary(spacelength.model)
gam.check(spacelength.model)
coef(spacelength.model)
plot.gam(spacelength.model, residuals = TRUE, pch = 1, cex = 1, pages = 1)
draw(spacelength.model)
#########################################################################################
#song variable: Number of notes
#exploratory visualization of associations 
plot(malesong$LatS, malesong$PaceNotes)
plot(malesong$Q_atra_k4logzscores_positive, malesong$PaceNotes)

#fitting a gam
nnotes.model = gam(PaceNoteszscorespositive ~ LatSzscoresinv + 
                     s(Q_atra_k4logzscores_positive, k=20),family=quasipoisson(link="log"), 
                   data=malesong, method = "REML")

#mgcv.helper::vif.gam(nnotes.model)#does not work for generalized models
#we only have one linear predictor, so no problem

#obtaining model output, checking model assumptions, diagnostics/residuals, etc. 
appraise(nnotes.model)
summary(nnotes.model)
gam.check(nnotes.model)
coef(nnotes.model)
plot.gam(nnotes.model, residuals = TRUE, pch = 1, cex = 1, pages = 1)
draw (nnotes.model)

#########################################################################################
#song variable: PC1 scores
#exploratory visualization of associations
plot (malesong$LatS, malesong$PC1)
plot (malesong$Q_atra_k4logzscores, malesong$PC1)
plot (malesong$Billlength, malesong$PC1)
plot (malesong$Pcwingtarsus, malesong$PC1)
plot (malesong$NDVI, malesong$PC1)
plot (malesong$Temperature, malesong$PC1)

##fitting a gam
pc1.model = gam(PC1zscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                  Temperaturezscores + NDVIzscores + s(Q_atra_k4logzscores, k=20), 
                data=malesong, method = "REML")

#checking colinearity among parametric/linear predictors
mgcv.helper::vif.gam(pc1.model)

#obtaining model output, checking model assumptions, diagnostics/residuals, etc. 
appraise(pc1.model)
summary(pc1.model)
gam.check(pc1.model)
coef(pc1.model)
plot.gam(pc1.model, residuals = TRUE, pch = 1, cex = 1, pages = 1)
draw(pc1.model)
#########################################################################################
#song variable: PC2 scores
#exploratory visualization of associations 
plot (malesong$LatS, malesong$PC2)
plot (malesong$Q_atra_k4logzscores, malesong$PC2)
plot (malesong$Billlength, malesong$PC2)
plot (malesong$Pcwingtarsus, malesong$PC2)
plot (malesong$NDVI, malesong$PC2)
plot (malesong$Temperature, malesong$PC2)

##fitting a gam
pc2.model = gam(PC2zscores ~ Billlengthzscores + 
                  Pcwingtarsuszscores + 
                  NDVIzscores  + Temperaturezscores + 
                  s(Q_atra_k4logzscores, k=20), 
                data=malesong, method = "REML")

#checking colinearity among parametric/linear predictors
mgcv.helper::vif.gam(pc2.model)

#obtaining model output, checking model assumptions, diagnostics/residuals, etc. 
appraise(pc2.model)
summary(pc2.model)
gam.check(pc2.model)
coef(pc2.model)
plot.gam(pc2.model, residuals = TRUE, pch = 1, cex = 1, pages = 1)
draw(pc2.model)
#end
#############################################################################################
######################################################################################################
#########################################################################################################
#Contrasting GAMs with and without spatial dependence
#Load data
malesong=read.csv2("maleleucoptera_indivfinal_2022.csv", head=TRUE)
head(malesong)
summary(malesong)
dim(malesong)

#song variable: note frequency
highfreq.model = gam(HiFreqzscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                       NDVIzscores + Temperaturezscores + 
                       s(Q_atra_k4logzscores), 
                     data=malesong)

#estimating a variogram
malesong$resid <- residuals(highfreq.model)
var.dat_resid <- variogram(resid~1, loc= ~Longsouthcor + Latsouthcor, data=malesong)
plot(var.dat_resid)

#testing spatial autocorrelation using gamm
highfreq.model1 <- gamm(HiFreqzscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                          NDVIzscores + Temperaturezscores + 
                          s(Q_atra_k4logzscores),data=malesong, 
                        method = "REML", correlation=corExp(form = ~ Latsouthcor + Longsouthcor))
highfreq.model2 <- gamm(HiFreqzscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                          NDVIzscores + Temperaturezscores + 
                          s(Q_atra_k4logzscores),data=malesong, 
                        method = "REML", correlation=corGaus(form = ~ Latsouthcor + Longsouthcor))
highfreq.model3 <- gamm(HiFreqzscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                          NDVIzscores + Temperaturezscores + 
                          s(Q_atra_k4logzscores),data=malesong, 
                        method = "REML", correlation=corLin(form = ~ Latsouthcor + Longsouthcor))
highfreq.model4 <- gamm(HiFreqzscores ~  LatSzscoresinv +  
                          Billlengthzscores + Pcwingtarsuszscores + 
                          NDVIzscores + Temperaturezscores + 
                          s(Q_atra_k4logzscores),data=malesong, 
                        method = "REML", correlation=corSpher(form = ~ Latsouthcor + Longsouthcor))
highfreq.model5 <- gamm(HiFreqzscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + 
                          NDVIzscores + Temperaturezscores + 
                          s(Q_atra_k4logzscores),data=malesong, 
                        method = "REML", correlation=corRatio(form = ~ Latsouthcor + Longsouthcor))

AIC (highfreq.model,highfreq.model1$lme,highfreq.model2$lme,highfreq.model3$lme,highfreq.model4$lme,highfreq.model5$lme)

#AICc
c(AICc(highfreq.model), AICc(highfreq.model1$lme), AICc(highfreq.model2$lme), AICc(highfreq.model3$lme), AICc(highfreq.model4$lme),AICc(highfreq.model5$lme)) # vector of AIC values
AICc(highfreq.model,highfreq.model1$lme,highfreq.model2$lme,highfreq.model3$lme,highfreq.model4$lme,highfreq.model5$lme)

#Below, the model with the smallest AIC will have a difference of zero.
x <- c(AICc(highfreq.model), AICc(highfreq.model1$lme), AICc(highfreq.model2$lme), AICc(highfreq.model3$lme), AICc(highfreq.model4$lme),AICc(highfreq.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)                # AIC differences
delta 

#estimating Akaike weights
x <- c(AICc(highfreq.model), AICc(highfreq.model1$lme), AICc(highfreq.model2$lme), AICc(highfreq.model3$lme), AICc(highfreq.model4$lme),AICc(highfreq.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # relative likelihoods of models
L
w <- L/sum(L)                     # Akaike weights
w

####################################################################################
#song variable: note length
notelength.model = gam(NoteLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores),
                       data=malesong, method = "REML")

#estimating a variogram
malesong$resid <- residuals(notelength.model)
var.dat_resid <- variogram(resid~1, loc= ~Longsouthcor + Latsouthcor, data=malesong)
plot(var.dat_resid)

#testing spatial autocorrelation using gamm
notelength.model1 = gamm(NoteLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores),
                         data=malesong, method = "REML", correlation=corExp(form = ~ Latsouthcor + Longsouthcor))

notelength.model2 = gamm(NoteLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores),
                         data=malesong, method = "REML", correlation=corGaus(form = ~ Latsouthcor + Longsouthcor))
notelength.model3 = gamm(NoteLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores),
                         data=malesong, method = "REML", correlation=corLin(form = ~ Latsouthcor + Longsouthcor))
notelength.model4 = gamm(NoteLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores),
                         data=malesong, method = "REML", correlation=corSpher(form = ~ Latsouthcor + Longsouthcor))
notelength.model5 = gamm(NoteLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores),
                         data=malesong, method = "REML", correlation=corRatio(form = ~ Latsouthcor + Longsouthcor))


#AICc
c(AICc(notelength.model), AICc(notelength.model1$lme), AICc(notelength.model2$lme), AICc(notelength.model3$lme), AICc(notelength.model4$lme),AICc(notelength.model5$lme)) # vector of AIC values

AICc(notelength.model,notelength.model1$lme,notelength.model2$lme,notelength.model3$lme,notelength.model4$lme,notelength.model5$lme)

#The model with the smallest AIC will have a difference of zero.

x <- c(AICc(notelength.model), AICc(notelength.model1$lme), AICc(notelength.model2$lme), AICc(notelength.model3$lme), AICc(notelength.model4$lme),AICc(notelength.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)                # AIC differences
delta 

#estimating Akaike weights
x <- c(AICc(notelength.model), AICc(notelength.model1$lme), AICc(notelength.model2$lme), AICc(notelength.model3$lme), AICc(notelength.model4$lme),AICc(notelength.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # relative likelihoods of models
L
w <- L/sum(L)                     # Akaike weights
w


#########################################################################################
#song variable: note interval
spacelength.model = gam(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores), 
                        data=malesong, method = "REML")

#estimating a variogram
malesong$resid <- residuals(spacelength.model)
var.dat_resid <- variogram(resid~1, loc= ~Longsouthcor + Latsouthcor, data=malesong)
plot(var.dat_resid)

#testing spatial autocorrelation using gamm
spacelength.model1 = gamm(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores), 
                          data=malesong, method = "REML", correlation=corExp(form = ~ Latsouthcor + Longsouthcor))
spacelength.model2 = gamm(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores), 
                          data=malesong, method = "REML", correlation=corGaus(form = ~ Latsouthcor + Longsouthcor))
spacelength.model3 = gamm(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores), 
                          data=malesong, method = "REML", correlation=corLin(form = ~ Latsouthcor + Longsouthcor))
spacelength.model4 = gamm(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores), 
                          data=malesong, method = "REML", correlation=corSpher(form = ~ Latsouthcor + Longsouthcor))
spacelength.model5 = gamm(SpaceLenzscores ~  LatSzscoresinv + NDVIzscores + s(Q_atra_k4logzscores), 
                          data=malesong, method = "REML", correlation=corRatio(form = ~ Latsouthcor + Longsouthcor))

#AICc
AICc(spacelength.model,spacelength.model1$lme,spacelength.model2$lme,spacelength.model3$lme,spacelength.model4$lme,spacelength.model5$lme)


#The model with the smallest AIC will have a difference of zero.
x <- c(AICc(spacelength.model), AICc(spacelength.model1$lme), AICc(spacelength.model2$lme), AICc(spacelength.model3$lme), AICc(spacelength.model4$lme),AICc(spacelength.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)                # AIC differences
delta

#estimating Akaike weights
x <- c(AICc(spacelength.model), AICc(spacelength.model1$lme), AICc(spacelength.model2$lme), AICc(spacelength.model3$lme), AICc(spacelength.model4$lme),AICc(spacelength.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # relative likelihoods of models
L
w <- L/sum(L)                     # Akaike weights
w
##########################################################################################################
#song variable: number of notes
nnotes.model <- gamm(PaceNoteszscorespositive ~ LatSzscoresinv  
                     + s(Q_atra_k4logzscores_positive),family=quasipoisson(link="log"),data=malesong, method = "REML")

#estimating a variogram was not possible for this model

#testing spatial autocorrelation using gamm
nnotes.model1 <- gamm(PaceNoteszscorespositive ~ LatSzscoresinv  
                      + s(Q_atra_k4logzscores_positive),family=quasipoisson(link="log"),data=malesong, method = "REML", 
                      correlation=corExp(form = ~ Latsouthcor + Longsouthcor))

nnotes.model2 <- gamm(PaceNoteszscorespositive ~ LatSzscoresinv  
                      + s(Q_atra_k4logzscores_positive),family=quasipoisson(link="log"),data=malesong, method = "REML", 
                      correlation=corGaus(form = ~ Latsouthcor + Longsouthcor))
nnotes.model3 <- gamm(PaceNoteszscorespositive ~ LatSzscoresinv  
                      + s(Q_atra_k4logzscores_positive),family=quasipoisson(link="log"),data=malesong, method = "REML", 
                      correlation=corLin(form = ~ Latsouthcor + Longsouthcor))
nnotes.model4 <- gamm(PaceNoteszscorespositive ~ LatSzscoresinv  
                      + s(Q_atra_k4logzscores_positive),family=quasipoisson(link="log"),data=malesong, method = "REML", 
                      correlation=corSpher(form = ~ Latsouthcor + Longsouthcor))
nnotes.model5 <- gamm(PaceNoteszscorespositive ~ LatSzscoresinv  
                      + s(Q_atra_k4logzscores_positive),family=quasipoisson(link="log"),data=malesong, method = "REML", 
                      correlation=corRatio(form = ~ Latsouthcor + Longsouthcor))

#AICc
AICc(nnotes.model,nnotes.model1$lme,nnotes.model2$lme,nnotes.model3$lme,nnotes.model4$lme,nnotes.model5$lme)

#The model with the smallest AIC will have a difference of zero.
x <- c(AICc(nnotes.model), AICc(nnotes.model1$lme), AICc(nnotes.model2$lme), AICc(nnotes.model3$lme), AICc(nnotes.model4$lme),AICc(nnotes.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)                # AIC differences
delta 

#estimating Akaike weights
x <- c(AICc(nnotes.model), AICc(nnotes.model1$lme), AICc(nnotes.model2$lme), AICc(nnotes.model3$lme), AICc(nnotes.model4$lme),AICc(nnotes.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # relative likelihoods of models
L
w <- L/sum(L)                     
w
##########################################################################################################
#song variable: PC1
pc1.model = gam(PC1zscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + Temperaturezscores + 
                  NDVIzscores + s(Q_atra_k4logzscores), 
                data=malesong, method = "REML")

#estimating a variogram
malesong$resid <- residuals(pc1.model)
var.dat_resid <- variogram(resid~1, loc= ~Longsouthcor + Latsouthcor, data=malesong)
plot(var.dat_resid)

#testing spatial autocorrelation using gamm
pc1.model1 = gamm(PC1zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corExp(form = ~ Latsouthcor + Longsouthcor))

pc1.model2 = gamm(PC1zscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corGaus(form = ~ Latsouthcor + Longsouthcor))

pc1.model3 = gamm(PC1zscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corLin(form = ~ Latsouthcor + Longsouthcor))

pc1.model4 = gamm(PC1zscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corSpher(form = ~ Latsouthcor + Longsouthcor))

pc1.model5 = gamm(PC1zscores ~  LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corRatio(form = ~ Latsouthcor + Longsouthcor))

#AIC
AICc(pc1.model,pc1.model1$lme,pc1.model2$lme,pc1.model3$lme,pc1.model4$lme,pc1.model5$lme)

#The model with the smallest AIC will have a difference of zero.
x <- c(AICc(pc1.model), AICc(pc1.model1$lme), AICc(pc1.model2$lme), AICc(pc1.model3$lme), AICc(pc1.model4$lme),AICc(pc1.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)                # AIC differences
delta 

#estimating Akaike weights
x <- c(AICc(pc1.model), AICc(pc1.model1$lme), AICc(pc1.model2$lme), AICc(pc1.model3$lme), AICc(pc1.model4$lme),AICc(pc1.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # relative likelihoods of models
L
w <- L/sum(L)                     
w
##########################################################################################################
#song variable: PC2
pc2.model = gam(PC2zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                  Temperaturezscores + s(Q_atra_k4logzscores), 
                data=malesong, method = "REML")

#estimating a variogram
malesong$resid <- residuals(pc2.model)
var.dat_resid <- variogram(resid~1, loc= ~Longsouthcor + Latsouthcor, data=malesong)
plot(var.dat_resid)

#testing spatial autocorrelation using gamm
pc2.model1 = gamm(PC2zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corExp(form = ~ Latsouthcor + Longsouthcor))
pc2.model2 = gamm(PC2zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corGaus(form = ~ Latsouthcor + Longsouthcor))
pc2.model3 = gamm(PC2zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corLin(form = ~ Latsouthcor + Longsouthcor))
pc2.model4 = gamm(PC2zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corSpher(form = ~ Latsouthcor + Longsouthcor))
pc2.model5 = gamm(PC2zscores ~ LatSzscoresinv + Billlengthzscores + Pcwingtarsuszscores + NDVIzscores  + 
                    Temperaturezscores + s(Q_atra_k4logzscores), 
                  data=malesong, method = "REML", correlation=corRatio(form = ~ Latsouthcor + Longsouthcor))

#AICc
AICc(pc2.model, pc2.model1$lme,pc2.model2$lme,pc2.model3$lme,pc2.model4$lme,pc2.model5$lme)

#The model with the smallest AIC will have a difference of zero.
x <- c(AICc(pc2.model), AICc(pc2.model1$lme), AICc(pc2.model2$lme), AICc(pc2.model3$lme), AICc(pc2.model4$lme),AICc(pc2.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)                # AIC differences
delta 

#estimating Akaike weights
x <- c(AICc(pc2.model), AICc(pc2.model1$lme), AICc(pc2.model2$lme), AICc(pc2.model3$lme), AICc(pc2.model4$lme),AICc(pc2.model5$lme)) # vector of AIC values
# stores AIC values in a vector
delta <- x - min(x)               # AIC differences
L <- exp(-0.5 * delta)            # relative likelihoods of models
L
w <- L/sum(L)                  
w

####################################################################################################
###############################################################################################################################################################################
#code for MMRR analyses
####################################################################################################
malesong=read.csv2("maleleucoptera_indivfinal_2022.csv",head=T)
head(malesong)
dim(malesong)

#########################################################################################
####################################################################################################
#males High Frequency means euclidian distance
HiFreq<-vegdist(malesong$HiFreqzscores,method="euclidean",binary=F)
HiFreq
hfreq=write.matrix(HiFreq, file = "highfrezscoresmales.txt")

####################################################################################
####################################################################################
#males Pace Notes means euclidian distance
Pacenotes<-vegdist(malesong$PaceNoteszscores,method="euclidean",binary=F)
Pacenotes
write.matrix(Pacenotes, file = "pacenoteszscoresmales.txt")
#####################################################################################
####################################################################################
#males Note length means euclidian distance
NoteLength<-vegdist(malesong$NoteLenzscores,method="euclidean",binary=F)
NoteLength
write.matrix(NoteLength, file = "notelengthzscoresmales.txt")
####################################################################################
#####################################################################################
#Note Space Length means euclidian distance
Spacelength<-vegdist(malesong$SpaceLenzscores,method="euclidean",binary=F)
Spacelength
write.matrix(Spacelength, file = "spacelengthzscoresmales.txt")
#####################################################################################
####################################################################################
#Note PC1 means euclidian distance
PC1<-vegdist(malesong$PC1zscores,method="euclidean",binary=F)
PC1
write.matrix(PC1, file = "PC1zscoresmales.txt")
#####################################################################################
####################################################################################
#Note PC2 means euclidian distance
PC2<-vegdist(malesong$PC2zscores,method="euclidean",binary=F)
PC2
write.matrix(PC2, file = "PC2zscoresmales.txt")
#####################################################################################
####################################################################################
#NDVI means euclidian distance
NDVI<-vegdist(malesong$NDVIzscores,method="euclidean",binary=F)
NDVI
write.matrix(NDVI, file = "NDVIzscoresmales.txt")
#####################################################################################
####################################################################################
#Temperature means euclidian distance
Temperature<-vegdist(malesong$Temperaturezscores,method="euclidean",binary=F)
Temperature
write.matrix(Temperature, file = "Temperaturezscoresmales.txt")
#####################################################################################
####################################################################################
#PCwingtarsus means euclidian distance
PCwingtarsus<-vegdist(malesong$Pcwingtarsuszscores,method="euclidean",binary=F)
PCwingtarsus
write.matrix(PCwingtarsus, file = "PCwingtarsuszscoresmales.txt")
#####################################################################################
####################################################################################
#Q-atra_k4 means euclidian distance
Qvalueatra<-vegdist(malesong$Q_atra_k4logzscores,method="euclidean",binary=F)
Qvalueatra
write.matrix(Qvalueatra, file = "Qvalueatrazscoresmales.txt")
#####################################################################################
####################################################################################
#Billlength means euclidian distance
Billlength<-vegdist(malesong$Billlengthzscores,method="euclidean",binary=F)
Billlength
write.matrix(Billlength, file = "Billengthzscoresmales.txt") 
#####################################################################################
####################################################################################
#geographic distance as the crow flies -straigth lines 
#geographic<-vegdist(cbind(malesong$Lat, malesong$Long), method = "euclidean",binary=F)
#geographic
#longitude and latitude 
geo = data.frame(malesong$Lat, malesong$Long)
#geographic data frame - haversine distance (distance between two points on a sphere) 
d.geo = distm(geo, fun = distHaversine)
dist.geo = as.dist(d.geo)
dist.geo
write.matrix(dist.geo, file = "geodistmales.txt")
#####################################################################################
####################################################################################

############## MMRR: we need to run the function below for the analysis#########
# Function code from Wang,I (2013). Examining the full effects of landscape heterogeneity on 
#spatial genetic variation: a multiple matrix regression approach for quantifying geographic and 
#ecological isolation. Evolution: 67-12: 3403-3411.
# MMRR performs Multiple Matrix Regression with Randomization analysis
# Y is a dependent distance matrix
# X is a list of independent distance matrices (with optional names)

MMRR<-function(Y,X,nperm=999){
  #compute regression coefficients and test statistics
  nrowsY<-nrow(Y)
  y<-unfold(Y)
  if(is.null(names(X)))names(X)<-paste("X",1:length(X),sep="")
  Xmats<-sapply(X,unfold)
  fit<-lm(y~Xmats)
  coeffs<-fit$coefficients
  summ<-summary(fit)
  r.squared<-summ$r.squared
  tstat<-summ$coefficients[,"t value"]
  Fstat<-summ$fstatistic[1]
  tprob<-rep(1,length(tstat))
  Fprob<-1
  
  #perform permutations
  for(i in 1:nperm){
    rand<-sample(1:nrowsY)
    Yperm<-Y[rand,rand]
    yperm<-unfold(Yperm)
    fit<-lm(yperm~Xmats)
    summ<-summary(fit)
    Fprob<-Fprob+as.numeric(summ$fstatistic[1]>=Fstat)
    tprob<-tprob+as.numeric(abs(summ$coefficients[,"t value"])>=abs(tstat))
  }
  
  #return values
  tp<-tprob/(nperm+1)
  Fp<-Fprob/(nperm+1)
  names(r.squared)<-"r.squared"
  names(coeffs)<-c("Intercept",names(X))
  names(tstat)<-paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp)<-paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat)<-"F-statistic"
  names(Fp)<-"F p-value"
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp))
}

# unfold converts the lower diagonal elements of a matrix into a vector
# unfold is called by MMRR

unfold<-function(X){
  x<-vector()
  for(i in 2:nrow(X)) x<-c(x,X[i,1:i-1])
  x<-scale(x, center=TRUE, scale=TRUE)  # Comment this line out if you wish to perform the analysis without standardizing the distance matrices! 
  return(x)
}


####################################################################################################
####################################################################################################
#the files below were generated with the functions above, except for the genetic distances, which are provided 

#males Pace Notes euclidian distance
pacenotesmat<- read.matrix("pacenoteszscoresmales.txt", header=TRUE) 
genemat<- read.matrix("individual_distances_August_2022.txt")
geomat<- read.matrix("geodistmales.txt", header=TRUE)
Qvalueatramat<- read.matrix("Qvalueatrazscoresmales.txt", header=TRUE)
# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.

Xmatsgeo <- list(geography=geomat)
Xmatsgenet <- list(genetic=genemat)
Xmats <- list(geography=geomat,genetic=genemat)
Xmatsqvalue<- list(introgression=Qvalueatramat)
Xmats <- list(geography=geomat,genetic=genemat,introgression=Qvalueatramat)

# Run MMRR function using pacenotesmat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRR(pacenotesmat,Xmats,nperm=10000)

####################################################################################################
####################################################################################################
highfreqmat<- read.matrix("highfrezscoresmales.txt", header=TRUE)   
bodysizeMat <- read.matrix("PCwingtarsuszscoresmales.txt",header=TRUE)
billmat<- read.matrix("Billengthzscoresmales.txt", header=TRUE)
ndvimat<- read.matrix("NDVIzscoresmales.txt", header=TRUE)
temperaturemat<- read.matrix("Temperaturezscoresmales.txt", header=TRUE)
genemat<- read.matrix("individual_distances_August_2022.txt")
geomat<- read.matrix("geodistmales.txt", header=TRUE)
Qvalueatramat<- read.matrix("Qvalueatrazscoresmales.txt", header=TRUE)


# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.
Xmatsbody <- list(bodysize=bodysizeMat)
Xmatsbill <- list(beak=billmat)
Xmatsforest <- list(forestcover=ndvimat)
Xmatstemp <- list(temp=temperaturemat)
Xmatsgeo <- list(geography=geomat)
Xmatsgenet <- list(genetic=genemat)
Xmatsqvalue<- list(introgression=Qvalueatramat)

Xmats <- list(bodysize=bodysizeMat,beak=billmat,forestcover=ndvimat,temp=temperaturemat,geography=geomat,genetic=genemat,introgression=Qvalueatramat)


# Run MMRR function using highfreqmat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRR(highfreqmat,Xmats,nperm=10000)


####################################################################################################
####################################################################################################
#males Space length means
spacelengthmat<- read.matrix("spacelengthzscoresmales.txt", header=TRUE)   
ndvimat<- read.matrix("NDVIzscoresmales.txt", header=TRUE)
genemat<- read.matrix("individual_distances_August_2022.txt")
geomat<- read.matrix("geodistmales.txt", header=TRUE)
Qvalueatramat<- read.matrix("Qvalueatrazscoresmales.txt", header=TRUE)
# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.

Xmatsforest <- list(forestcover=ndvimat)
Xmatsgeo <- list(geography=geomat)
Xmatsgenet <- list(genetic=genemat)
Xmatsqvalue<- list(introgression=Qvalueatramat)

Xmats <- list(forestcover=ndvimat,genetic=genemat,geography=geomat,introgression=Qvalueatramat)

# Run MMRR function using spacelengthmat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRR(spacelengthmat,Xmats,nperm=10000)

####################################################################################################
####################################################################################################
#males Note length means
Notelengthmat<- read.matrix("notelengthzscoresmales.txt", header=TRUE)   
ndvimat<- read.matrix("NDVIzscoresmales.txt", header=TRUE)
genemat<- read.matrix("individual_distances_August_2022.txt")
geomat<- read.matrix("geodistmales.txt", header=TRUE)
Qvalueatramat<- read.matrix("Qvalueatrazscoresmales.txt", header=TRUE)

# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.
Xmatsforest <- list(forestcover=ndvimat)
Xmatsgeo <- list(geography=geomat)
Xmatsgenet <- list(genetic=genemat)
Xmatsqvalue<- list(introgression=Qvalueatramat)

Xmats <- list(forestcover=ndvimat,genetic=genemat,geography=geomat,introgression=Qvalueatramat)

# Run MMRR function using Notelengthmat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRR(Notelengthmat,Xmats,nperm=10000)


####################################################################################################
####################################################################################################
#males PC1 means
PC1mat<- read.matrix("PC1zscoresmales.txt", header=TRUE) 
bodysizeMat <- read.matrix("PCwingtarsuszscoresmales.txt",header=TRUE) 
billmat<- read.matrix("Billengthzscoresmales.txt", header=TRUE)
ndvimat<- read.matrix("NDVIzscoresmales.txt", header=TRUE)
temperaturemat<- read.matrix("Temperaturezscoresmales.txt", header=TRUE)
genemat<- read.matrix("individual_distances_August_2022.txt")
geomat<- read.matrix("geodistmales.txt", header=TRUE)
Qvalueatramat<- read.matrix("Qvalueatrazscoresmales.txt", header=TRUE)


Xmatsbody <- list(bodysize=bodysizeMat)
Xmatsbill <- list(beak=billmat)
Xmatsforest <- list(forestcover=ndvimat)
Xmatstemp <- list(temp=temperaturemat)
Xmatsgeo <- list(geography=geomat)
Xmatsgenet <- list(genetic=genemat)
Xmatsqvalue<- list(introgression=Qvalueatramat)

# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.
Xmats <- list(bodysize=bodysizeMat,beak=billmat,forestcover=ndvimat,temp=temperaturemat,geography=geomat,genetic=genemat,introgression=Qvalueatramat)


# Run MMRR function using PC1mat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRR(PC1mat,Xmats,nperm=10000)

####################################################################################################
####################################################################################################
#males PC2 means
PC2mat<- read.matrix("PC2zscoresmales.txt", header=TRUE)   
bodysizeMat <- read.matrix("PCwingtarsuszscoresmales.txt",header=TRUE)
billmat<- read.matrix("Billengthzscoresmales.txt", header=TRUE)
ndvimat<- read.matrix("NDVIzscoresmales.txt", header=TRUE)
temperaturemat<- read.matrix("Temperaturezscoresmales.txt", header=TRUE)
genemat<- read.matrix("individual_distances_August_2022.txt")
geomat<- read.matrix("geodistmales.txt", header=TRUE)
Qvalueatramat<- read.matrix("Qvalueatrazscoresmales.txt", header=TRUE)


Xmatsbody <- list(bodysize=bodysizeMat)
Xmatsbill <- list(beak=billmat)
Xmatsforest <- list(forestcover=ndvimat)
Xmatstemp <- list(temp=temperaturemat)
Xmatsgeo <- list(geography=geomat)
Xmatsgenet <- list(genetic=genemat)
Xmatsqvalue<- list(introgression=Qvalueatramat)

# Make a list of the explanatory (X) matrices.
# Names are optional.  Order doesn't matter.
# Can include more than two matrices, if desired.
Xmats <- list(bodysize=bodysizeMat,beak=billmat,forestcover=ndvimat,temp=temperaturemat,geography=geomat,genetic=genemat,introgression=Qvalueatramat)

# Run MMRR function using PC2mat as the response variable and Xmats as the explanatory variables.
# nperm does not need to be specified, default is nperm=999)
MMRR(PC2mat,Xmats,nperm=10000)
###############################################################################################################################################
############Figures S11-S18######################################
songvar=read.csv2("Variables_dist_colum.csv", head=TRUE)

summary(songvar)
head(songvar)
tail(songvar)
dim(songvar)
cor(songvar[, 8:10], method = c("spearman"))


#Fig S11 - highfreq
#highfreqvsgeodist
FigS11a= ggplot(songvar, aes(Geo_distance_km, Highfreq_distance) ) +
  geom_point(mapping = aes(x = Geo_distance_km, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                
                                                                                                                                                                                                                axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,2000,400))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11a

#highfreqvsbodysizePC
FigS11b= ggplot(songvar, aes(Pcwingtarsus_distance, Highfreq_distance) ) +
  geom_point(mapping = aes(x = Pcwingtarsus_distance, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Body size PC distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                        
                                                                                                                                                                                                                        axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11b

#highfreqvsgen_distance
FigS11c= ggplot(songvar, aes(gen_distance, Highfreq_distance) ) +
  geom_point(mapping = aes(x = gen_distance, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Genetic distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                          
                                                                                                                                                                                                          axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,0.01,0.002))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11c

#highfreqvsndvi_distance
FigS11d= ggplot(songvar, aes(NDVI_distance, Highfreq_distance) ) +
  geom_point(mapping = aes(x = NDVI_distance, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Forest cover distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                
                                                                                                                                                                                                                axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11d

#highfreqvstemperature_distance
FigS11e= ggplot(songvar, aes(temperature_distance, Highfreq_distance) ) +
  geom_point(mapping = aes(x = temperature_distance, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Temperature distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                      
                                                                                                                                                                                                                      axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11e

#highfreqvsbilllength_distance
FigS11f= ggplot(songvar, aes(billlength_distance, Highfreq_distance) ) +
  geom_point(mapping = aes(x = billlength_distance, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Bill length distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                     
                                                                                                                                                                                                                     axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11f

#highfreqvsintrogression_distance
FigS11g= ggplot(songvar, aes(Qvalueatra_distance, Highfreq_distance) ) +
  geom_point(mapping = aes(x = Qvalueatra_distance, y = Highfreq_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Maximum frequency distance",x="Introgression distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                       
                                                                                                                                                                                                                       axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS11g



#Note length
#notelength_distancevsgeo_distance
#highfreqvsgeodist
FigS12a= ggplot(songvar, aes(Geo_distance_km, notelength_distance) ) +
  geom_point(mapping = aes(x = Geo_distance_km, y = notelength_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note length distance",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                            
                                                                                                                                                                                                            axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,2000,400))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS12a

#Note length distancevsgen_distance
FigS12b= ggplot(songvar, aes(gen_distance, notelength_distance) ) +
  geom_point(mapping = aes(x = gen_distance, y = notelength_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note length distance",x="Genetic distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                      
                                                                                                                                                                                                      axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,0.01,0.002))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS12b

#notelength_distancevsndvi_distance
FigS12c= ggplot(songvar, aes(NDVI_distance, notelength_distance) ) +
  geom_point(mapping = aes(x = NDVI_distance, y = notelength_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note length distance",x="Forest cover distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                            
                                                                                                                                                                                                            axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS12c

#notelengthdistancevsintrogression_distance
FigS12d= ggplot(songvar, aes(Qvalueatra_distance, notelength_distance) ) +
  geom_point(mapping = aes(x = Qvalueatra_distance, y = notelength_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note length distance",x="Introgression distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                   
                                                                                                                                                                                                                   axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS12d

#noteinterval_distancevsgeo_distance
#highfreqvsgeodist
FigS13a= ggplot(songvar, aes(Geo_distance_km, Spacelengthdistance) ) +
  geom_point(mapping = aes(x = Geo_distance_km, y = Spacelengthdistance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note interval distance",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                              
                                                                                                                                                                                                              axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,2000,400))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS13a

#noteintervaldistancevsintrogression_distance
FigS13b= ggplot(songvar, aes(Qvalueatra_distance, Spacelengthdistance) ) +
  geom_point(mapping = aes(x = Qvalueatra_distance, y = Spacelengthdistance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note interval distance",x="Introgression distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                     
                                                                                                                                                                                                                     axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS13b

#noteinterval distancevsgen_distance
FigS13c= ggplot(songvar, aes(gen_distance, Spacelengthdistance) ) +
  geom_point(mapping = aes(x = gen_distance, y = Spacelengthdistance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note interval distance",x="Genetic distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                        
                                                                                                                                                                                                        axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,0.01,0.002))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS13c

#noteinterval_distancevsndvi_distance
FigS13d= ggplot(songvar, aes(NDVI_distance, Spacelengthdistance) ) +
  geom_point(mapping = aes(x = NDVI_distance, y = Spacelengthdistance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Note interval distance",x="Forest cover distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                              
                                                                                                                                                                                                              axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS13d


#numberofnotes_distancevsgeo_distance
FigS14a= ggplot(songvar, aes(Geo_distance_km, pacenote_distance) ) +
  geom_point(mapping = aes(x = Geo_distance_km, y = pacenote_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Number of notes distance",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                              
                                                                                                                                                                                                              axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,2000,400))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS14a

#numberofnotesdistancevsintrogression_distance
FigS14b= ggplot(songvar, aes(Qvalueatra_distance, pacenote_distance) ) +
  geom_point(mapping = aes(x = Qvalueatra_distance, y = pacenote_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Number of notes distance",x="Introgression distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                     
                                                                                                                                                                                                                     axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS14b

#numberofnotes distancevsgen_distance
FigS14c= ggplot(songvar, aes(gen_distance, pacenote_distance) ) +
  geom_point(mapping = aes(x = gen_distance, y = pacenote_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Number of notes distance",x="Genetic distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                        
                                                                                                                                                                                                        axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,0.01,0.002))+ scale_y_continuous(breaks = seq(0,6,1)) + stat_smooth(method = lm)

FigS14c

#Fig S15 - PC1
#PC1vsgeodist
FigS15a= ggplot(songvar, aes(Geo_distance_km, PC1_distance) ) +
  geom_point(mapping = aes(x = Geo_distance_km, y = PC1_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                   
                                                                                                                                                                                                   axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,2000,400))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS15a

#PC1_distance vsbodysizePC
FigS15b= ggplot(songvar, aes(Pcwingtarsus_distance, PC1_distance ) ) +
  geom_point(mapping = aes(x = Pcwingtarsus_distance, y = PC1_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Body size PC distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                           
                                                                                                                                                                                                           axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS15b

#PC1_distancevsbilllength_distance
FigS15c= ggplot(songvar, aes(billlength_distance, PC1_distance) ) +
  geom_point(mapping = aes(x = billlength_distance, y = PC1_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Bill length distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                        
                                                                                                                                                                                                        axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS15c

#PC1_distance vsgen_distance
FigS15d= ggplot(songvar, aes(gen_distance, PC1_distance) ) +
  geom_point(mapping = aes(x = gen_distance, y = PC1_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Genetic distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                             
                                                                                                                                                                                             axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,0.01,0.002))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS15d

#PC1_distance vsndvi_distance
FigS15e= ggplot(songvar, aes(NDVI_distance, PC1_distance ) ) +
  geom_point(mapping = aes(x = NDVI_distance, y = PC1_distance )) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Forest cover distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                    
                                                                                                                                                                                                    axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS15e

#PC1_distance vstemperature_distance
FigS15f= ggplot(songvar, aes(temperature_distance, PC1_distance) ) +
  geom_point(mapping = aes(x = temperature_distance, y = PC1_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Temperature distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                         
                                                                                                                                                                                                         axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)
FigS15f


#PC1_distancevsintrogression_distance
FigS15g= ggplot(songvar, aes(Qvalueatra_distance, PC1_distance) ) +
  geom_point(mapping = aes(x = Qvalueatra_distance, y = PC1_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC1 distance",x="Introgression distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                          
                                                                                                                                                                                                          axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS15g

#Fig S16 - Vocal PC2
#PC2_distancevsgeodist
FigS16a= ggplot(songvar, aes(Geo_distance_km, PC2_distance) ) +
  geom_point(mapping = aes(x = Geo_distance_km, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Geographic distance (km)", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                   
                                                                                                                                                                                                   axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,2000,400))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16a

#PC2_distancevsbodysizePC
FigS16b= ggplot(songvar, aes(Pcwingtarsus_distance, PC2_distance) ) +
  geom_point(mapping = aes(x = Pcwingtarsus_distance, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Body size PC distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                           
                                                                                                                                                                                                           axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16b

#PC2_distancevsgen_distance
FigS16c= ggplot(songvar, aes(gen_distance, PC2_distance) ) +
  geom_point(mapping = aes(x = gen_distance, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Genetic distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                             
                                                                                                                                                                                             axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,0.01,0.002))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16c

#PC2_distancevsndvi_distance
FigS16d= ggplot(songvar, aes(NDVI_distance, PC2_distance) ) +
  geom_point(mapping = aes(x = NDVI_distance, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Forest cover distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                   
                                                                                                                                                                                                   axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16d

#PC2_distancevstemperature_distance
FigS16e= ggplot(songvar, aes(temperature_distance, PC2_distance) ) +
  geom_point(mapping = aes(x = temperature_distance, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Temperature distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                         
                                                                                                                                                                                                         axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16e

#PC2_distancevsbilllength_distance
FigS16f= ggplot(songvar, aes(billlength_distance, PC2_distance) ) +
  geom_point(mapping = aes(x = billlength_distance, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Bill length distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                        
                                                                                                                                                                                                        axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16f

#PC2_distancevsintrogression_distance
FigS16g= ggplot(songvar, aes(Qvalueatra_distance, PC2_distance) ) +
  geom_point(mapping = aes(x = Qvalueatra_distance, y = PC2_distance)) +  scale_size_manual(values = c(5, 5,5))+labs(y="Vocal PC2 distance",x="Introgression distance", face="bold", color="black")+theme(axis.text = element_text(size = 16, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                          
                                                                                                                                                                                                          axis.text.y=element_text(size=16, face="bold")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_color_manual(values = c("black", "black", "black"))+scale_x_continuous(breaks = seq(0,5,1))+ scale_y_continuous(breaks = seq(0,5,1)) + stat_smooth(method = lm)

FigS16g

###############################################################################################################################################
############Figures S17-S19#############
#Figure S17
ndvidist=read.csv2("maleleucopteraatra_meansforDFA-allinfo.csv", head=TRUE)
summary(ndvidist)

jpeg("NDVIvsGEOdistHZ_December_2022.jpeg", width = 9, height = 9, units = 'in', res = 300)

plota=ggplot(ndvidist, aes(DistrefHZ, NDVI)) +
  geom_point(mapping = aes(x = DistrefHZ, y = NDVI, shape=Species,color=Species,size=Species)) + scale_shape_manual(values = c(16, 16, 16)) + scale_color_manual(values = c("mediumblue", "red", "black")) + scale_size_manual(values = c(5, 5,5))+labs(y="Forest cover",x="Distance from the hybrid zone (KM)", face="bold", color="black")+theme(axis.text = element_text(size = 20, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=16), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=16, face="bold"),
                                                                                                                                                                                                                                                                                                                                              axis.text.y=element_text(size=16, face="bold")) + coord_cartesian(ylim = c(0.4, 0.9)) + scale_x_continuous(breaks = seq(-500, 2000,250))+ scale_y_continuous(breaks = seq(0.0,1,0.1))
plota
dev.off()


#Figure S18
notelength_leuco_dist=read.csv2("leucoptera_notelength_vs_distHZ.csv", head=TRUE)
summary(notelength_leuco_dist)

jpeg("NotelegthvsGEOdistHZ_leuco_2022.jpeg", width = 9, height = 9, units = 'in', res = 300)
ggplot(notelength_leuco_dist, aes(DistrefHZnegat, NoteLenMean)) +
  geom_point(mapping = aes(x = DistrefHZnegat, y = NoteLenMean)) + scale_shape_manual(values = c(16)) + scale_color_manual(values = c("black")) + scale_size_manual(values = c(3))+labs(y="Note length (ms)",x="Distance to the hybrid zone (KM)", face="bold", color="black")+theme(axis.text = element_text(size = 20, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=20), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=20, face="bold"),
                                                                                                                                                                                                                                                                                     axis.text.y=element_text(size=20, face="bold")) + coord_cartesian(ylim = c(80, 230)) + scale_x_continuous(breaks = seq(-2000,0, 250))+ scale_y_continuous(breaks = seq(80,220,20))

dev.off()


#Figure S19
notelength_atra_dist=read.csv2("atra_notelength_vs_distHZ.csv", head=TRUE)
summary(notelength_atra_dist)

jpeg("NotelegthvsGEOdistHZ_atra_2022.jpeg", width = 9, height = 9, units = 'in', res = 300)
ggplot(notelength_atra_dist, aes(DistrefHZ, NoteLenMean)) +
  geom_point(mapping = aes(x = DistrefHZ, y = NoteLenMean)) + scale_shape_manual(values = c(16)) + scale_color_manual(values = c("black")) + scale_size_manual(values = c(3))+labs(y="Note length (ms)",x="Distance to the hybrid zone (KM)", face="bold", color="black")+theme(axis.text = element_text(size = 20, color='black'), axis.title = element_text(family = "Times", color="black",face="bold", size=20), legend.position="none", panel.background = element_rect(fill = "white", size = 2, linetype = 1, color='black'), panel.border = element_rect(fill = NA, color = "black"),  axis.ticks = element_line(color = "black",size = 1, linetype = 'solid'), axis.text.x=element_text(size=20, face="bold"),
                                                                                                                                                                                                                                                                                axis.text.y=element_text(size=20, face="bold")) + coord_cartesian(ylim = c(110, 210)) + scale_x_continuous(breaks = seq(-500,0, 50))+ scale_y_continuous(breaks = seq(110,210,20))

dev.off()

##################end##########################

