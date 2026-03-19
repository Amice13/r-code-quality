#  Normalize_Cytokines.r
#  Cory White
#  Systems Biology Group
#  Merck Cambridge Exploratory Science Center
#  Script to normalize cytokine data for sepsis project using median value
#  All data with "<" symbol will be set to half the min value present in the dataset for that cytokine
#  All data with ">" symbol will be set to twice the max value present in the dataset for that cytokine

source("Normalize_Function.r")

setwd("../data/")

Mili<-Normalize("Patient 1 to 27-millipore kit.csv",Outname="Millipore")
RandD1<-Normalize("RandD_1.csv",Outname="RandD_1")
RandD2<-Normalize("RandD_2.csv",Outname="RandD_2")

#  Merge data together for final output
RandDout<-rbind(RandD2,RandD1)
write.csv(RandDout,file="lg2_Normalized_Cytokines_RandD_Merged.csv",row.names=FALSE)
