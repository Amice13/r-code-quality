###############################################################################################
### REPLICATION CODE FOR TIME IN OFFICE AND THE CHANGING GENDER GAP  -- FIGURE 2 #######
###############################################################################################

rm(list = ls())
setwd("") #Set to the folder the data is saved in

library(Hmisc)
library(readstata13)

dta<-read.dta13("Time_in_Office_Chaudhuri_Jensenius_Iversen_Maitra.dta")
dim(dta)
attach(dta)

#Figure 2
jpeg("Fig2_die_6.jpeg", width=7, height=4, units= "in", res=900, quality=100)
par(mfrow=c(1,2))#, ps=9, family = 'garamond')
par(mar=c(4, 0.2, 1.5,0.2))
plot(density(die_6[!is.na(primsample) & female=="Female" & primsample==0]), xlab="Number of reported sixes", main="Inexperienced politicians", yaxt="n", xaxt="n", xlim=c(0,30), ylim=c(0,0.2))
mgp.axis(1, mgp = c(3, 0.8, 0), las=1)
polygon(density(die_6[!is.na(primsample) & female=="Female" & primsample==0]), col="#4D4D4D90")
text(x=mean(die_6[!is.na(primsample) & female=="Female" & primsample==0])-0.5, y=0.14,"Distribution for women", pos=4)

par(new=T)
plot(density(die_6[!is.na(primsample) & female=="Male" & primsample==0]), xlab="", main="", yaxt="n", xaxt="n", xlim=c(0,30), ylim=c(0,0.2))
polygon(density(die_6[!is.na(primsample) & female=="Male" & primsample==0]), col="#B3B3B390")
text(x=mean(die_6[!is.na(primsample) & female=="Male" & primsample==0])+4, y=0.05,"Distribution for men", pos=4)

par(mar=c(4, 0.2, 1.5,0.2))
plot(density(die_6[!is.na(primsample) & female=="Female" & primsample==1]), xlab="Number of reported sixes", main="Experienced politicians", yaxt="n", xaxt="n", xlim=c(0,30), ylim=c(0,0.2))
mgp.axis(1, mgp = c(3, 0.8, 0), las=1)
polygon(density(die_6[!is.na(primsample) & female=="Female" & primsample==1]), col="#4D4D4D90")
text(x=mean(die_6[!is.na(primsample) & female=="Female" & primsample==1])+4, y=0.04,"Distribution for women", pos=4)

par(new=T)
plot(density(die_6[!is.na(primsample) & female=="Male" & primsample==1]), xlab="", main="", yaxt="n", xaxt="n", xlim=c(0,30), ylim=c(0,0.2))
polygon(density(die_6[!is.na(primsample) & female=="Male" & primsample==1]), col="#B3B3B390")
text(x=mean(die_6[!is.na(primsample) & female=="Male" & primsample==1]), y=0.13,"Distribution for men", pos=4)
dev.off()
