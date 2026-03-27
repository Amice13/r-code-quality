library(ggplot2)
library(foreign)

#The files used in this R script (marginal effects and CIs) are created using Stata: BCFvU_JOP_repdata.do


setwd(" ")


df <- read.csv("Full.csv", sep=";")


par(mfrow = c(1, 1))

    par(mar=c(3.1,8.1,4.1,0.3))

    plot(df$avch, 1:length(df$avch), cex=1.1, type = "p", axes = F,  ylab = "", xlab="",
         pch = 19, ylim=c(-0.01, length(df$avch)+0.003), xlim=c(-0.005,0.02),
         main="Full Sample", cex.main = 1.2)
   # abline(v=0,col="gray95",lwd=1100)
    abline(v=0, lty=2)
    points(df$avch, 1:length(df$avch), pch = 21, cex = 0.8, bg = "white" ) 
    segments(df$lower, 1:length(df$avch), df$upper, 1:length(df$avch), lwd =  3,   col="black")
    
    #segments(lower, 1:length(avch), upper, 1:length(avch), lwd =  2)
    points(df$avch, 1:length(df$avch), pch = 21, cex = 0.8, bg = "white" ) 
    
    
    axis(1, at = seq(-0.005,0.02, by=.005), tick = T,
         cex.axis = 1.2, mgp = c(5,.7,0))   
    
    axis(2, at = 1:length(df$avch), label = df$names2, adj=0, las=1, tick= T,
         mgp = c(2,.4,0), cex.axis= 1.1) #mpg determines how far they are from the tixs 
    
    #axis(2, at = 1:length(df$avch), label = df$names2, adj=0, las=1, tick= T,
       #  mgp = c(2,.4,0), cex.axis= 1.6)

    par(mfrow = c(1, 2))
    
    
par(mar=c(3.1,0.7,4.1,0.5))
    

df <- read.csv("Low_dev.csv", sep=";")

plot(df$avch, 1:length(df$avch), cex=1.1, type = "p", axes = F,  ylab = "", xlab="",
     pch = 19, ylim=c(-0.01, length(df$avch)+0.004),xlim=c(-0.005,0.02),
     main="Low Development",  cex.main = 1.2)
#abline(v=0,col="skyblue3",lwd=1100)
abline(v=0, lty=2)
points(df$avch, 1:length(df$avch), pch = 21, cex = 0.8, bg = "white" ) 

segments(df$lower, 1:length(df$avch), df$upper, 1:length(df$avch), lwd =  3, col=("black"))

points(df$avch, 1:length(df$avch), pch = 21, cex = 0.8, bg = "white") 


axis(1, at = seq(-0.005,0.02, by=.005), tick = T,
     cex.axis = 1.2, mgp = c(5,.7,0))  

#axis(2, at = 1:length(df$avch), label = df$names2, adj=0, las=1, tick= T,
     #mgp = c(2,.6,0), cex.axis= .9)

par(mar=c(3.1,0.7,4.1,0.5))

df <- read.csv("Ag_dep.csv", sep=";")

plot(df$avch, 1:length(df$avch), cex=1.1, type = "p", axes = F,  ylab = "", xlab="",
     pch = 19, ylim=c(-0.01, length(df$avch)+0.004),  xlim=c(-0.005,0.02),
     main="High Agric. Employment",  cex.main = 1.2)
#abline(v=0, col="skyblue1",lwd=1100)
abline(v=0, lty=2)
points(df$avch, 1:length(df$avch), pch = 21, cex = 0.8, bg = "white" ) 

segments(df$lower, 1:length(df$avch), df$upper, 1:length(df$avch), lwd =  3, col=("black"))

#segments(lower, 1:length(avch), upper, 1:length(avch), lwd =  2)
points(df$avch, 1:length(df$avch), pch = 21, cex = 0.8, bg = "white" ) 


axis(1, at = seq(-0.005,0.02, by=.005), tick = T,
     cex.axis = 1.2, mgp = c(5,.7,0))  

#axis(2, at = 1:length(df$avch), label = df$names2, adj=0, las=1, tick= T,
#mgp = c(2,.6,0), cex.axis= .9)


par(mar=c(3.1,2.1,4.1,2.1))




