rm(list=ls())
library(tidyverse)
library(readxl)
library(limma)

metab_file<-"Sepsis_20Sep19 clean2.xlsx"
metab<-read_excel(metab_file)

con_ind<-which(metab[,2]$...2=='control')
sep_ind<-which(metab[,2]$...2=='sepsis' & metab[,3]$...3==0)

C=c()
for(index in con_ind){
  #index<-25
  Rec1<-(as.numeric(unlist(metab[index,-c(1,2,3,4)])))
  Rec1[which(is.na(Rec1))]=0
  Rec2<-(as.numeric(unlist(metab[index+1,-c(1,2,3,4)])))
  Rec2[which(is.na(Rec2))]=0
  tmp=cbind(as.matrix(Rec1),as.matrix(Rec2))
  tmp2<-as.matrix(rowMeans(tmp))
  for(i in seq(1,nrow(tmp2))){
    if(min(tmp[i,1],tmp[i,2])==0){
      tmp2[i,1]<-max(tmp[i,1],tmp[i,2])
    } 
  }
  colnames(tmp2)<-unlist(metab[index,1])
  C<-cbind(C,tmp2)
}

S=c()
for(index in sep_ind){
  #index<-25
  Rec1<-(as.numeric(unlist(metab[index,-c(1,2,3,4)])))
  Rec1[which(is.na(Rec1))]=0
  Rec2<-(as.numeric(unlist(metab[index+1,-c(1,2,3,4)])))
  Rec2[which(is.na(Rec2))]=0
  tmp=cbind(as.matrix(Rec1),as.matrix(Rec2))
  tmp2<-as.matrix(rowMeans(tmp))
  for(i in seq(1,nrow(tmp2))){
    if(min(tmp[i,1],tmp[i,2])==0){
      tmp2[i,1]<-max(tmp[i,1],tmp[i,2])
    } 
  }
  colnames(tmp2)<-unlist(metab[index,1])
  S<-cbind(S,tmp2)
}

data<-cbind(C,S)
rownames(data)<-unlist(metab[1,-c(1:4)])
Y<-c(rep(0,ncol(C)),rep(1,ncol(S)))
RData<-list(data,Y)
save(RData,file='Sepsis_D0_2.RData')


DataNorm<-normalizeVSN(data)
variance<-apply(DataNorm,1,var)
mean_val<-rowMeans(DataNorm)

# Standardised Data
SD<-apply(DataNorm,1,sd)
std_datanorm<-(DataNorm-rowMeans(DataNorm))/SD



save(std_datanorm,file='STD_VST_Normalized_Sepsis_D0_2.RData')
write.csv(x=std_datanorm, file='STD_VST_Normalized_Sepsis_D0_2.csv')



