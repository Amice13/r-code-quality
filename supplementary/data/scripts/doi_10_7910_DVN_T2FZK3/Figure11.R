#############################################################################################
#############################################################################################
#############################################################################################

#Paper: Illiberal Communication and Election Intervention During the Refugee Crisis in Germany

#Ashrakat Elshehawy, Konstantin Gavras, Nikolay Marinov, Federico Nanni, Harald Schoen

#Perspectives on Politics

#Replication for figure: 11


#############################################################################################
#############################################################################################
#############################################################################################

#clean environment
rm(list=ls())


#load packages, please install these packages if you have not already done so
library(readr)
library(ggplot2)

#set working directory, please change your directory accordingly
setwd("/Users/ashrakatelshehawy/Dropbox/Working Papers and Submissions/Illiberal Communication and Election Intervention During the Refugee Crisis in Germany/Replication/All replication files to be uploaded/Figure 11 Data and R Code")


#load data
overlayeddata_consp_wiki <- read_csv("PopulismConspiracyColRevolt-ablationdata_v2.csv")


#code russian dummy
overlayeddata_consp_wiki$rus <- ifelse(grepl("sputnik" , overlayeddata_consp_wiki$url), "1",  
                                       ifelse(grepl("deutsch.rt", overlayeddata_consp_wiki$url), "1",
                                              ifelse(grepl("rtdeutsch", overlayeddata_consp_wiki$url),"1","0")))


#change dummy to factor
overlayeddata_consp_wiki$rus<-as.factor(overlayeddata_consp_wiki$rus)


#check variable
summary(overlayeddata_consp_wiki$rus)

tapply(overlayeddata_consp_wiki$overall, overlayeddata_consp_wiki$rus, summary)






#######################conspiracy plot  ########################figure 11



plot (density(overlayeddata_consp_wiki$`- open society`), col="darkorange4",
      xlab="Conspiracy", ylim=c(0, 16),xlim=c(0.5, 0.9), cex.lab=1.5, cex.axis=1.3, main="")
lines (density(overlayeddata_consp_wiki$`- verheimlichen`),col="red", lty=2,lwd=1)
lines (density(overlayeddata_consp_wiki$`- orange revolution`),col="darkmagenta", lty=3,lwd=1)
lines (density(overlayeddata_consp_wiki$`- einflussnahme`),col="deeppink", lty=4,lwd=1)
lines (density(overlayeddata_consp_wiki$`- euromaidan`),col="mediumaquamarine",lty=5,lwd=1)
lines (density(overlayeddata_consp_wiki$`- geheimdokumente`),col="slateblue1", lty=6,lwd=1)
lines (density(overlayeddata_consp_wiki$`-geopolitisch`), col="royalblue2",  lty=1,lwd=1)
lines (density(overlayeddata_consp_wiki$`- marionette`),col="lightblue2",lty=2,lwd=1)
lines (density(overlayeddata_consp_wiki$overall),col="black", lty=1,lwd=1)
lines (density(overlayeddata_consp_wiki$`- soros`),col="springgreen", lty=3,lwd=1)
lines (density(overlayeddata_consp_wiki$`- verschwörung`),col="lawngreen", lty=4,lwd=1)
legend(0.5, 16,bg="transparent", legend=c("- Open Society","- verheimlichen","- Orange Revolution",
                                          "- Einflussnahme","- Euromaidan","- Geheimdokumente",
                                          "- geopolitisch","- Marionette"," overall",
                                          "- Soros","- Verschwörung"
),
col=c("darkorange4","red","darkmagenta","deeppink","mediumaquamarine","slateblue1","royalblue2",
      "lightblue2","black","springgreen","lawngreen"), lty = c(1, 2, 3,4,5,6,1,2,1,3,4), cex=0.5,
box.lty=0)


