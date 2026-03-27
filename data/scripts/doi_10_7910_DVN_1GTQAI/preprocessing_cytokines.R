rm(list=ls())
library(tidyverse)
library(Hmisc) #impute


cytokines_file<-"lg2_Normalized_Cytokines_RandD_Merged.csv"
#cytokines_file<-"lg2_Normalized_Cytokines_Millipore.csv"

DataNorm <- read.csv(cytokines_file,row.names = 1,header=TRUE)
DataNorm <- as.matrix(DataNorm)
Y=rep(0,ncol(DataNorm))
Y[which(grepl("sepsis",colnames(DataNorm)))]=1
#save(Y,file='Sepsis_D0_milipore.RData')


#imputing missing values
tmp<-apply(DataNorm,1, function(x) sum(is.na(x)))
if(max(tmp)>0){
  DataNorm_impute_sepsis<-apply(DataNorm[,grepl("sepsis",colnames(DataNorm))],1, function(x) impute(x))
  DataNorm_impute_control<-apply(DataNorm[,grepl("control",colnames(DataNorm))],1, function(x) impute(x))
  
  DataNorm_impute<-t(rbind(DataNorm_impute_sepsis,DataNorm_impute_control))
} else {
  DataNorm_impute<-DataNorm
}


SD<-apply(DataNorm_impute,1,sd)
std_datanorm<-(DataNorm_impute-rowMeans(DataNorm_impute))/SD
##save(std_datanorm,file= "./STD_log2_transformed_Sepsis_D0_RandD.RData")
##save(std_datanorm,file= "./STD_log2_transformed_Sepsis_D0_Millipore.RData")



    