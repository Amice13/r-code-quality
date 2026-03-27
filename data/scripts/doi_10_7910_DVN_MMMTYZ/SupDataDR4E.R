####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com


############INSTRUCTIONS

#Use this code to generate genus duration estimates for OBIS taxa
#Code requires that you run SupDataDR4C.R section "FINAL FAUNAL LIST" in order to generate list of OBIS taxa

############INSTRUCTIONS


library(data.table)
library(dplyr)
library(ggplot2)

source("SupDataDR6_ABM_code.R")

#Pass list of OBIS genus names
gen_nams<-unique(finalgen8$genus)

pbcmr<-read.csv(SupDataDR3A.csv)

genlist<-pbcmr %>%  filter(genus!="" & !is.na(genus) & genus!="NO_GENUS_SPECIFIED") %>% filter(genus %in% gens_nams)

tax_durs<-c()
#Number of subsamples to draw
samp_nums<-150
runs<-1000

library(R.utils)
library(foreach)
library(doParallel)

close_rar<-c()
print(Sys.time())
myCluster <- parallel::makeCluster(detectCores()/2, setup_timeout = 0.5)
registerDoParallel(myCluster)
close_rar<-foreach(i=unique(genlist$genus),.combine=rbind,.packages=c("dplyr","tidyverse")) %dopar% {

  tax_durs<-c()
  
  sel_tax<-genlist %>% filter(genus==i)
  sel_tax$mid_ma<-(sel_tax$max_ma+sel_tax$min_ma)/2
  sel_dat<-na.omit(sort(as.numeric(sel_tax$mid_ma),decreasing=T))
  
  sel_dat <- as.numeric(as.character(sel_dat[!sel_dat %in% 0]))
  
  
  if(length(sel_dat)<=5){
    
    tax_durs<-rbind(tax_durs,cbind(genus=i,ogFAD=max(sel_dat),infFAD=max(sel_dat),method="lowsamp",timerange=diff(range(sel_dat_samp)),run=j,quant=length(sel_dat)))
    
    
  }else if(length(sel_dat)<100){
    
    og_dat<-abm38(sel_dat,distance=F,ext=F,base=0,conf=.9,PLOT=0)
    tax_durs<-rbind(tax_durs,cbind(genus=i,ogFAD=max(sel_dat),infFAD=og_dat[1,c("th-hat")],method="noerror",timerange=diff(range(sel_dat_samp)),run=j,quant=length(sel_dat)))
    
  }else{
    
    tryCatch({
      
      for(j in 1:runs){
        #Sample all but the oldest occurrence and then include the oldest
        sel_dat_samp <- sort(c(sample(sel_dat[2:length(sel_dat)], min(samp_nums-1,length(sel_dat)-1)),sel_dat[1]),decreasing = TRUE)
        og_dat<-abm38(sel_dat_samp,distance=F,ext=F,base=0,conf=.9,PLOT=0)
        tax_durs<-rbind(tax_durs,cbind(genus=i,ogFAD=max(sel_dat),infFAD=og_dat[1,c("th-hat")],method="sampled",timerange=diff(range(sel_dat_samp)),run=j,quant=length(sel_dat_samp)))
      }
      
      sel_dat_low <- sel_dat[1:min(samp_nums,length(sel_dat))]
      og_dat<-abm38(sel_dat_low,distance=F,ext=F,base=0,conf=.9,PLOT=0)
      tax_durs<-rbind(tax_durs,cbind(genus=i,ogFAD=max(sel_dat),infFAD=og_dat[1,c("th-hat")],method="lowest",timerange=diff(range(sel_dat_low)),run=0,quant=length(sel_dat_low)))
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  }
  
  as.data.frame(tax_durs)
  
}

stopCluster(myCluster)
closeAllConnections()
registerDoSEQ()
print(Sys.time())


tax_durs<-as.data.frame(close_rar)
tax_durs$quant<-as.numeric(as.character(tax_durs$quant))
tax_durs$ogFAD<-as.numeric(as.character(tax_durs$ogFAD))
tax_durs$infFAD<-as.numeric(as.character(tax_durs$infFAD))
tax_durs$difFAD<-tax_durs$ogFAD-tax_durs$infFAD
tax_durs$lowsamps<-ifelse(tax_durs$quant<100,"raw","sampled")

ggplot(tax_durs)+geom_point(aes(x=ogFAD,y=infFAD,color=method),alpha=0.5)
ggplot(tax_durs)+geom_point(aes(x=ogFAD,y=infFAD,color=method),alpha=0.5)+facet_grid(.~lowsamps)

ggplot(tax_durs)+geom_point(aes(x=genus,y=ogFAD-infFAD,color=method))

tax_durs2<-tax_durs %>%
  group_by(method,ogFAD,genus) %>%
  summarise(meanFAD=mean(infFAD,na.rm=T))
tax_durs2$difFAD<-tax_durs2$meanFAD-tax_durs2$ogFAD

ggplot(tax_durs2)+geom_point(aes(x=genus,y=difFAD,color=method))
ggplot(tax_durs2)+geom_point(aes(x=ogFAD,y=meanFAD,color=method))

#Retain a single age for each taxon
tax_durs3 <- tax_durs2 %>% filter(method!="lowest")

write.csv(tax_durs3, "ABM_OBIS_ages.csv")
