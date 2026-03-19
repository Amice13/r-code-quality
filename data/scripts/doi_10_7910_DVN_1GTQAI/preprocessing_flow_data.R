remove(list = ls())
library(tidyverse)
library(limma)


flow_file<-"sepsis_neutrophils_bulk_data.csv"
flow<-read.csv(flow_file,header = TRUE,row.names=1)


data<-flow
variance<-apply(data,1,var)
mean_val<-rowMeans(data)

data<-apply(data,2, function(x) (1*tanh(0.01*((x-mean(x))/sd(x) +1))))
                                 
SD<-apply(data,1,sd)
std_flow<-(data-rowMeans(data))/SD

std_flow<-as.matrix(std_flow)


save(std_flow,Y,file= "./STD_tanh_transformed_Sep_control.RData")


