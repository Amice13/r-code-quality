
#remove(list=ls())
source("code/build.R")

#suppressMessages({
#Suppress warnings due to the dummies package, which works but triggers warnings for any R version since R 3.6.
#Suppress warnings of mass points detected in the running variable in rdd. We discuss this on p. 28 of the article. 

  suppressWarnings({

data$labour_party<-ifelse(data[,"pOrg"] %in% c("BLLoY","LP","Labour Party", "LP & LLoY","LPSA"),1,0 )


data_no_comm<-data[data$comm==0 | data$other_left==1, ]
data_only_part<-data[data$comm==1 | data$other_left==1, ]
data_no_extreme<-data[(data$comm==0 & data$other_left==0)   | data$labour_party==1, ]
data_only_comm_lab<-data[data$comm==1 | data$labour_party==1, ]


#data<-data[data_no_comm$arrival_date_run> -355,]


a<-rdrobust(y=data$other_left,x=data$arrival_date_run,cluster=data$country, masspoints = "adjust")
b<-rdrobust(y=data_no_comm$other_left,x=data_no_comm$arrival_date_run,cluster=data_no_comm$country, masspoints = "adjust")
c<-rdrobust(y=data_only_part$other_left,x=data_only_part$arrival_date_run,cluster=data_only_part$country, masspoints = "adjust")


d<-rdrobust(y=data$labour_party,x=data$arrival_date_run,cluster=data$country, masspoints = "adjust")
e<-rdrobust(y=data_no_extreme$labour_party,x=data_no_extreme$arrival_date_run,cluster=data_no_extreme$country, masspoints = "adjust")
f<-rdrobust(y=data_only_comm_lab$labour_party,x=data_only_comm_lab$arrival_date_run,cluster=data_only_comm_lab$country, masspoints = "adjust")








out_rd<-data.frame(matrix("&",8,12))

out_cols<-seq(1,12,by=2)

rd_mods<-list(a,b,c,d,e,f)

for(i in 1:length(rd_mods)){
  
  
  
  out_rd[1,out_cols[i]] <-  round(rd_mods[[i]]$coef[1,1],3)
  out_rd[5,out_cols[i]] <-  round(rd_mods[[i]]$coef[3,1],3)
  out_rd[2,out_cols[i]] <-  paste(paste("(", round(rd_mods[[i]]$se[1,1],3),sep="" ),")",sep="")
  out_rd[6,out_cols[i]] <-  paste(paste("(", round(rd_mods[[i]]$se[3,1],3),sep="" ),")",sep="")
  
  
  out_rd[3,out_cols[i]]<- round(rd_mods[[i]]$bws[1,1],3)
  out_rd[4,out_cols[i]]<- paste(round(rd_mods[[i]]$N_h[1],3), round(rd_mods[[i]]$N_h[2],3),sep="/")
  
  out_rd[7,out_cols[i]]<- round(rd_mods[[i]]$bws[2,1],3)
  out_rd[8,out_cols[i]]<- paste(round(rd_mods[[i]]$N_b[1],3), round(rd_mods[[i]]$N_b[2],3),sep="/")
}





out_rd[,12]<-"\\"

out_rd<-cbind( c("Conventional","", "Bandwith","N (L/R)", "Robust", "","Bandwith","N (L/R)"),"&", out_rd)
names(out_rd)[1]<-""

print(out_rd,row.names=F)

  })
#})