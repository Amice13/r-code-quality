#############################To plot total methylation loss from TET-TKO experiment##############


#################################################################################################
#First install dplyr, only needs to be completed once 
#install.packages("dplyr")
library(dplyr)

setwd("~/Dropbox/DemethylationKinetics_Scripts/Oscar/Datasets")

#Read in "all" calls and methylation scores
All.calls <- read.csv("./ESC_TET_TKO_TET3_calls.csv", row.names = 1)
All.perc <- read.csv("./ESC_TET_TKO_TET3_meth.csv", row.names = 1)


#Calculate the numbers of methylated calls per motif
Methyl.calls.motif <- All.perc/100*All.calls

#Sum motif methylation calls and totals for each sample
Total.perc <- colSums(Methyl.calls.motif)/colSums(All.calls)*100
data <- rbind(All.perc,Total.perc)

zero.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_0h_._Perc_met")))
six.h <- t(select(data[257,], matches ("ESC_TET_TKO_TET3_Dox_6h_._Perc_met")))
twelve.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_12h_._Perc_met")))
eighteen.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_18h_._Perc_met")))
twenty4.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_24h_._Perc_met")))
thirty.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_30h_._Perc_met")))
thirty6.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_36h_._Perc_met")))
forty2.h<- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_42h_._Perc_met")))
length(forty2.h) =3
forty8.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_48h_._Perc_met")))
fifty4.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_54h_._Perc_met")))
sixty.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_60h_._Perc_met")))
sixty6.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_66h_._Perc_met")))
seventy2.h <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Dox_72h_._Perc_met")))

zero.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_0h_._Perc_met")))
six.h.neg <- t(select(data[257,], matches ("ESC_TET_TKO_TET3_Ctr_6h_._Perc_met")))
twelve.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_12h_._Perc_met")))
eighteen.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_18h_._Perc_met")))
length(eighteen.h.neg) =3
twenty4.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_24h_._Perc_met")))
thirty.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_30h_._Perc_met")))
thirty6.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_36h_._Perc_met")))
length(thirty6.h.neg) =3
forty2.h.neg<- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_42h_._Perc_met")))
forty8.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_48h_._Perc_met")))
fifty4.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_54h_._Perc_met")))
sixty.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_60h_._Perc_met")))
sixty6.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_66h_._Perc_met")))
seventy2.h.neg <- t(select(data[257,], matches("ESC_TET_TKO_TET3_Ctr_72h_._Perc_met")))

#global.loss is a table with all motifs together
global.loss.pos <- cbind(zero.h, six.h, twelve.h, eighteen.h, twenty4.h, thirty.h, thirty6.h, forty2.h, forty8.h, fifty4.h, sixty.h, sixty6.h, seventy2.h)
global.loss.neg <- cbind(zero.h.neg, six.h.neg, twelve.h.neg, eighteen.h.neg, twenty4.h.neg, thirty.h.neg, thirty6.h.neg, forty2.h.neg, forty8.h.neg, fifty4.h.neg, sixty.h.neg, sixty6.h.neg, seventy2.h.neg)

#plot the slope data
layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))

#For main text (to be in red)
h <- c(0,0,0,6,6,6,12,12,12,18,18,18,24,24,24,30,30,30,36,36,36,42,42,42,48,48,48,54,54,54,60,60,60,66,66,66,72,72,72)
plot(x=h, y=global.loss.pos, col="red", ylim=c(55,80), pch=20, frame.plot = F, las=1, ylab="CG methylation (%)", cex=0.4)
h <- c(0,6,12,18,24,30,36,42,48,54,60,66,72)
mean.data.pos = colMeans(global.loss.pos, na.rm=T)
lines(x=h, y=mean.data.pos, type = "l", col='red')

#For supplementary
h <- c(0,0,0,6,6,6,12,12,12,18,18,18,24,24,24,30,30,30,36,36,36,42,42,42,48,48,48,54,54,54,60,60,60,66,66,66,72,72,72)
plot(x=h, y=global.loss.neg, col="black", ylim=c(0,80), pch=20, frame.plot = F, las=1, ylab="CG methylation (%)", cex=0.4)
points(x=h, y=global.loss.pos, col="red", ylim=c(60,80), pch=20, frame.plot = F, las=1, ylab="CG methylation (%)", cex=0.4)
h <- c(0,6,12,18,24,30,36,42,48,54,60,66,72)
mean.data.neg = colMeans(global.loss.neg, na.rm=T)
lines(x=h, y=mean.data.neg, type = "l", col='black')
mean.data.pos = colMeans(global.loss.pos, na.rm=T)
lines(x=h, y=mean.data.pos, type = "l", col='red')


########end###################



