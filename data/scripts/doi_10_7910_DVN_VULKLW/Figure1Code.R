####### METANALYSIS SUMMARY TABLE SYNTAX
library(foreign)
library(tidyverse)
library(readxl)
library(plotrix)
library(shape)
library(ggplot2)

setwd("") ##set working directory to preferred location

################################
#Figure 1
################################

## loading data
un<- read_excel("Figure1Data.xlsx") ##unadjusted D (d1)


######### Subsetting by DVs
obsbu<-subset(un, IV == "Obj/Sub")
issueu<- subset(un, IV == "System 2")
PIDu<- subset(un, IV =="System 1")






###########################################
################ Unadjusted Delta Plots
###########################################




############## Info Search Plot
png("MetaFigure1A.png", width = 5, height = 8, unit = "in", res = 300)
par(mfrow=c(3,1),   mar = c(5, 7, 4, 4))
#pdf("figure1.pdf")
plotCI(x=c(1:3), y=obsbu$Dunadj, 
       ui=obsbu$ULunadj, li=obsbu$LLunadj,
       main = "Emotions and Information Search",
       font.main = 1,
       xlab="", xaxt = "n",
       yaxt="n", ylab="Unadjusted D",
       col.lab = "black",
       pch = 16,
       xlim =c(.5,3.5),
       ylim= c(-.23, .23))
abline(h=0, col="#b2182b",lty=5)
axis(1, c(1:3), c("Anxiety", "Enthusiasm", "Anger"), las = 1, tick = FALSE)
axis(2, cex.axis=.75)



############## PID Decision Making Plot

plotCI(x=c(1:3), y=PIDu$Dunadj, 
       ui=PIDu$ULunadj, li=PIDu$LLunadj,
       main = "Emotions x PID: System 1 Processing",
       font.main = 1,
       xlab="", xaxt = "n",
       yaxt ="n", ylab="Unadjusted D",
       col.lab = "black",
       pch = 16,
       xlim =c(.5,3.5),
       ylim= c(-.23, .23))
abline(h=0, col="#b2182b",lty=5)
axis(1, c(1:3), c("Anxiety", "Enthusiasm", "Anger"), las = 1, tick = FALSE)
axis(2, cex.axis=.75)



############### Decision Plot Issues

plotCI(x=c(1:3), y=issueu$Dunadj, 
       ui=issueu$ULunadj, li=issueu$LLunadj,
       main = "Emotions x Issue, System 2 Processing",
       font.main = 1,
       xlab="", xaxt = "n",
       yaxt = "n", ylab="Unadjusted D",
       col.lab = "black",
       pch = 16,
       xlim =c(.5,3.5),
       ylim= c(-.23, .23))
abline(h=0, col="#b2182b",lty=5)
axis(1, c(1:3), c("Anxiety", "Enthusiasm", "Anger"), las = 1, tick = FALSE)
axis(2, cex.axis=.75)

dev.off()