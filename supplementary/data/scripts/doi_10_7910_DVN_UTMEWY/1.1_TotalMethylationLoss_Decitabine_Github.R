########################To plot total methylation loss from decitabine experiment###############

################################################################################################

#First install dplyr, only needs to be completed once 
install.packages("dplyr")
#load dplyr package to allow select function
library(dplyr)

setwd("~/Dropbox/DemethylationKinetics_Scripts/Oscar/Datasets/")

#Read in "all" calls and methylation scores
All.calls <- read.csv("V65_Dec_non_CGI_calls.csv", row.names = 1)
All.perc <- read.csv("V65_Dec_non_CGI_meth.csv", row.names = 1)

#Calculate the numbers of methylated calls per motif
Methyl.calls.motif <- All.perc/100*All.calls

#Sum motif methylation calls and totals for each sample
Total.perc <- colSums(Methyl.calls.motif)/colSums(All.calls)*100
data <- rbind(All.perc,Total.perc)

zero.h <- t(select(data[257,], matches("V65_Dec_0h_")))
six.h <- t(select(data[257,], matches("V65_Dec_6h_")))
twelve.h <- t(select(data[257,], matches("V65_Dec_12h_")))
eighteen.h <- t(select(data[257,], matches("V65_Dec_18h_")))
twenty4.h <- t(select(data[257,], matches("V65_Dec_24h_")))
thirty.h <- t(select(data[257,], matches("V65_Dec_30h_")))
thirty6.h <- t(select(data[257,], matches("V65_Dec_36h_")))
forty2.h <- t(select(data[257,], matches("V65_Dec_42h_")))
forty8.h <- t(select(data[257,], matches("V65_Dec_48h_")))

zero.h.neg <- t(select(data[257,], matches("V65_Ctr_0h_")))
six.h.neg <- t(select(data[257,], matches("V65_Ctr_6h_")))
twelve.h.neg <- t(select(data[257,], matches("V65_Ctr_12h")))
eighteen.h.ng <- t(select(data[257,], matches("V65_Ctr_18h")))
twenty4.h.neg <- t(select(data[257,], matches("V65_Ctr_24h")))
thirty.h.neg <- t(select(data[257,], matches("V65_Ctr_30h")))
thirty6.h.neg <- t(select(data[257,], matches("V65_Ctr_36h")))
forty2.h.neg <- t(select(data[257,], matches("V65_Ctr_42h")))
forty8.h.neg <- t(select(data[257,], matches("V65_Ctr_48h")))

#global.loss is a table showing global trends (i.e. all motifs)
global.loss.pos <- cbind(zero.h, six.h, twelve.h, eighteen.h, twenty4.h, thirty.h, thirty6.h, forty2.h, forty8.h)
global.loss.neg <- cbind(zero.h, six.h.neg, twelve.h.neg, eighteen.h.ng, twenty4.h.neg, thirty.h.neg, thirty6.h.neg, forty2.h.neg, forty8.h.neg)

#plot the slope data
layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))

#For main text (to be in red)
h <- c(0,0,0,6,6,6,12,12,12,18,18,18,24,24,24,30,30,30,36,36,36,42,42,42,48,48,48)
plot(x=h, y=global.loss.pos, col="red", ylim=c(0,80), pch=20, frame.plot = F, las=1, ylab="CG methylation (%)", cex=0.4)
h <- c(0,6,12,18,24,30,36,42,48)
mean.data.pos = colMeans(global.loss.pos, na.rm=T)
lines(x=h, y=mean.data.pos, type = "l", col='red')


#hr <- c(0,0,0,6,6,6,12,12,12,18,18,18,24,24,24,30,30,30,36,36,36,42,42,42,48,48,48)
hr <- c(0,0,0,6,6,6,12,12,12,18,18,18,24,24,24,30,30,30,36,36,36,42,42,42,48,48,48)
plot(x=hr, y=global.loss.pos, col="red", ylim=c(20,80), xlim = c(0,50), pch=20, frame.plot = F, las=1, ylab="CG methylation (%)", cex=0.4)
points(x=hr, y=global.loss.neg, col="black", pch=20, cex=0.4)

hr <- c(0,6,12,18,24,30,36,42,48)
mean.data.pos = colMeans(global.loss.pos, na.rm=T)
mean.data.neg = colMeans(global.loss.neg, na.rm=T)
lines(x=hr, y=mean.data.pos, type = "l", col='red')
lines(x=hr, y=mean.data.neg, type = "l", col='black')
