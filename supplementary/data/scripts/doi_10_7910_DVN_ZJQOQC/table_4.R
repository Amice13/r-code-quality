#remove(list=ls())
source("code/build.R")

#suppressMessages({
#Suppress warnings due to the dummies package, which works but triggers warnings for any R version since R 3.6.
#Suppress warnings of mass points detected in the running variable in rdd. We discuss this on p. 28 of the article.
#Suppress warnings of dropping redundant fixed effect variables due to multicollinearity.
  suppressWarnings({
    

country_dums<-dummy(data$country)
age_dums<-dummy(data$age)
class_dums<-dummy(data$Class)
occ_dums<-dummy(data$Occupation)



a<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country, masspoints = "adjust")
a2<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country, masspoints = "adjust",b=a$bws[2,]/2,h = a$bws[1,]/2)
a3<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country, masspoints = "adjust",b=a$bws[2,]*2,h = a$bws[1,]*2)
a4<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36]), masspoints = "adjust")
a5<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36]), masspoints = "adjust",b=a4$bws[2,]/2,h = a4$bws[1,]/2)
a6<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36]), masspoints = "adjust",b=a4$bws[2,]*2,h = a4$bws[1,]*2)


a7<-feols(comm~arrival_date_run*ifelse(arrival_date_run >=0,1,0)
          +I(arrival_date_run^2)*ifelse(arrival_date_run >=0,1,0)
          +I(arrival_date_run^3)*ifelse(arrival_date_run >=0,1,0)
          ,data,cluster~country)


a8<-feols(comm~arrival_date_run*ifelse(arrival_date_run >=0,1,0)
          +I(arrival_date_run^2)*ifelse(arrival_date_run >=0,1,0)
          +I(arrival_date_run^3)*ifelse(arrival_date_run >=0,1,0) | 
            age+woman+country  
          ,data,cluster~country)

#####



a41<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:ncol(country_dums)],class_dums[,2:5]), masspoints = "adjust")
a42<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36],occ_dums[,2:ncol(occ_dums)]), masspoints = "adjust")





#########


rd_contrl<-list(a41,a42)

out_a<-data.frame(matrix("&",4,8))

cols1<-c(1,3)
cols2<-c(5,7)

for(i in 1:length(rd_contrl)){
  
  out_a[1,cols1[i]] <-  round(rd_contrl[[i]]$coef[1,1],3)
  out_a[1,cols2[i]] <-  round(rd_contrl[[i]]$coef[3,1],3)
  
  out_a[2,cols1[i]] <-  paste(paste("(", round(rd_contrl[[i]]$se[1,1],3),sep="" ),")",sep="")
  out_a[2,cols2[i]] <-  paste(paste("(", round(rd_contrl[[i]]$se[3,1],3),sep="" ),")",sep="")
  
  
  out_a[3,cols1[i]]<- round(rd_contrl[[i]]$bws[1,1],3)
  out_a[4,cols1[i]]<- paste(round(rd_contrl[[i]]$N_h[1],3), round(rd_contrl[[i]]$N_h[2],3),sep="/")
  
  out_a[3,cols2[i]]<- round(rd_contrl[[i]]$bws[2,1],3)
  out_a[4,cols2[i]]<- paste(round(rd_contrl[[i]]$N_b[1],3), round(rd_contrl[[i]]$N_b[2],3),sep="/")
  
}


out_a[,8]<-"\\"





#########



out_rd<-data.frame(matrix("&",8,16))

out_cols<-seq(1,16,by=2)

rd_mods<-list(a,a2,a3,a4,a5,a6,a7,a8)

for(i in 1:length(rd_mods)){
  
  
  if(i < 7){
    
    out_rd[1,out_cols[i]] <-  round(rd_mods[[i]]$coef[1,1],3)
    out_rd[5,out_cols[i]] <-  round(rd_mods[[i]]$coef[3,1],3)
    out_rd[2,out_cols[i]] <-  paste(paste("(", round(rd_mods[[i]]$se[1,1],3),sep="" ),")",sep="")
    out_rd[6,out_cols[i]] <-  paste(paste("(", round(rd_mods[[i]]$se[3,1],3),sep="" ),")",sep="")
    
    
    out_rd[3,out_cols[i]]<- round(rd_mods[[i]]$bws[1,1],3)
    out_rd[4,out_cols[i]]<- paste(round(rd_mods[[i]]$N_h[1],3), round(rd_mods[[i]]$N_h[2],3),sep="/")
    
    out_rd[7,out_cols[i]]<- round(rd_mods[[i]]$bws[2,1],3)
    out_rd[8,out_cols[i]]<- paste(round(rd_mods[[i]]$N_b[1],3), round(rd_mods[[i]]$N_b[2],3),sep="/")
  }
  
  
  
  if(i > 6){
    
    
    out_rd[1,out_cols[i]] <-round(coef(rd_mods[[i]])[names(coef(rd_mods[[i]])) == "ifelse(arrival_date_run >= 0, 1, 0)"],3)
    out_rd[2,out_cols[i]] <-  paste(paste("(", round(rd_mods[[i]]$se[names(coef(rd_mods[[i]])) == "ifelse(arrival_date_run >= 0, 1, 0)"],3),sep="" ),")",sep="")
    out_rd[4,out_cols[i]]  <- rd_mods[[i]]$nobs
    
  }
  
  
  
}


out_rd[,16]<-"\\"

out_rd<-cbind( c("Conventional","", "Bandwith","N (L/R)", "Robust", "","Bandwith","N (L/R)"),"&", out_rd)

names(out_rd)<-c("","&","Optimal Bandwith","&" ,"1/2 Optimal Bandwith","&","2 x Optimal Bandwith","&", "Optimal Bandwith","&", "1/2 Optimal Bandwith","&","2 x Optimal Bandwith", "&","3rd Order Polynomial","&","3rd Order Polynomial","\\" )




out_rd<-rbind(out_rd,
              c("Covariates","&", "No", "&", "No", "&", "No","&","Yes", "&", "Yes", "&", "Yes","&","No","&","Yes","\\" ))


out_rd[c(4,8),ncol(out_rd)]<-"\\ \\"


print(out_rd,row.names = F)

  })

#})
