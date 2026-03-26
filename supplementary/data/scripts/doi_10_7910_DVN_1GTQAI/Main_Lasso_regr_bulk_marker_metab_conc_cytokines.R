remove(list = ls())
library('rlang')
library('randomForest')
library("tidyverse")
library('umap')
library('pROC')
library('glmnet')

source('compute_metric.R')
source('Perform_Lasso_integration.R')

# ###Metabolities Data
load("../Metabolites/STD_VST_Normalized_Sepsis_D0_2.RData")

Pdata<-std_datanorm

colnames(Pdata)<-str_remove(colnames(Pdata)," ")

ext1 <- function(x) {  
  ( unlist(strsplit(x,"_"))[3])}
N1<- sapply(colnames(Pdata)[1:11], ext1)
N1<-paste0("C",N1)
ext2 <- function(x) {  
  ( unlist(strsplit(x,"_"))[2])}
N2<- sapply(colnames(Pdata)[12:25], ext2)
N2<-paste0("S",N2)
N2[4]<-"S13"
N2[9]<-"S4"
N2[12]<-"S7"
colnames(Pdata)<-c(N1,N2)

meta<-Pdata
Name_meta<-colnames(Pdata)

load("../Cytokines/STD_log2_transformed_Sepsis_D0_Millipore.RData")

cyto_mili<-std_datanorm
tmp<-str_replace(colnames(cyto_mili),"sepsis.","S")
tmp<-str_replace(tmp,"control.","C")
tmp<-str_replace(tmp,"\\.0","")
Name_sepsiscyto<-tmp
colnames(cyto_mili)<-tmp

#Cytokine
load("../Cytokines/STD_log2_transformed_Sepsis_D0_RandD.RData")
cyto_RD<-std_datanorm
tmp<-str_replace(colnames(cyto_RD),"sepsis.","S")
tmp<-str_replace(tmp,"control.","C")
tmp<-str_replace(tmp,"\\.0","")

Name_sepsiscyto_RD<-tmp
colnames(cyto_RD)<-tmp

##Bulk ow data
load("../Flow_Data/STD_tanh_transformed_Sep_control.RData")
std_flow=t(std_flow)
Name_flow=colnames(std_flow)

####common
tmp<-intersect(Name_meta,intersect(intersect(Name_sepsiscyto_RD,Name_sepsiscyto),Name_flow))
req_meta<-Pdata[,match(tmp,Name_meta)]
req_cyto_RD<-cyto_RD[,match(tmp,Name_sepsiscyto_RD)]
req_cyto_mili<-cyto_mili[,match(tmp,Name_sepsiscyto)]
req_std_flow<-std_flow[,match(tmp,Name_flow)]
################


#wt_m<-mean(Met)/(mean(Met)+mean(Cyt)+mean(Flo))
wt_f<-mean(apply(req_std_flow,1,IQR))/(mean(apply(req_std_flow,1,IQR))+ mean(apply(req_cyto_RD,1,IQR))
	+ mean(apply(req_cyto_mili,1,IQR)) + mean(apply(req_meta,1,IQR)))
wt_m<-mean(apply(req_meta,1,IQR))/(mean(apply(req_std_flow,1,IQR))+ mean(apply(req_cyto_RD,1,IQR))
	+ mean(apply(req_cyto_mili,1,IQR)) + mean(apply(req_meta,1,IQR)))
wt_c_RD<-mean(apply(req_cyto_RD,1,IQR))/(mean(apply(req_std_flow,1,IQR))+ mean(apply(req_cyto_RD,1,IQR))
	+ mean(apply(req_cyto_mili,1,IQR)) + mean(apply(req_meta,1,IQR)))
wt_c_mili<-mean(apply(req_cyto_mili,1,IQR))/(mean(apply(req_std_flow,1,IQR))+ mean(apply(req_cyto_RD,1,IQR))
	+ mean(apply(req_cyto_mili,1,IQR)) + mean(apply(req_meta,1,IQR)))
penality<-c(rep(wt_f,ncol(req_std_flow)),rep(wt_m,nrow(req_meta)),rep(wt_c_mili,nrow(req_cyto_mili)),rep(wt_c_RD,nrow(req_cyto_RD)))


data<-rbind(req_std_flow,req_meta,req_cyto_mili,req_cyto_RD)
dim_data=c(nrow(req_std_flow),nrow(req_meta),nrow(req_cyto_mili),nrow(req_cyto_RD))




Y=c(rep(0,9),rep(1,12))

n_folds=5

i=1
C<-c()
perf<-c()
while(i<1000){
  print(i)
  try(tmp<-Perform_Lasso_integration(data,Y,n_folds,1,penality))
  perf<-rbind(perf,tmp[[1]])
  C<-cbind(C,tmp[[2]])
  i<-i+1
}

tmp_met_0_01<-names(which(sort(abs(rowMeans(C[-1,])))>0.01))
types=rep(1,length(tmp_met_0_01))
types[match(rownames(req_meta),tmp_met_0_01)]=2
types[match(rownames(req_cyto_mili),tmp_met_0_01)]=3
types[match(rownames(req_cyto_RD),tmp_met_0_01)]=4

which((abs(rowMeans(C[-1,])))>0.01)
var(perf[,2])
colMeans(perf)

req_met_0_1<-names(which(sort(abs(rowMeans(C[-1,])))>0.1))
types=rep(1,length(tmp_met_0_01))
types[match(rownames(req_meta),tmp_met_0_01)]=2
types[match(rownames(req_cyto_mili),tmp_met_0_01)]=3
types[match(rownames(req_cyto_RD),tmp_met_0_01)]=4

save.image('meta_all_bulk_Flow_Cyto_RD_mili_integration_Lasso.RData')


