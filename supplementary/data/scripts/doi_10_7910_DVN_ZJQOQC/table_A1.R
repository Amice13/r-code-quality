#remove(list=ls())
source("code/build.R")

#suppressMessages({

    

covs<-list(ifelse(data$Occupation!="", data$Class,NA),ifelse(data$Occupation!="", data$Ordered_Occ+2,NA),data$ISCO,data$Weekly.Wage,data$comm,
           data$age,data$woman,as.numeric(data$arrival_date_run),data$leave_duration,data$killed,data$deserter)

descripts<-data.frame(matrix("&",length(covs),5))
rws<-seq(1,length(descripts),2)

for(i in 1:length(covs)){
  descripts[i,1]<-round(mean(covs[[i]],na.rm=T),3)
  descripts[i,3]<-round(sd(covs[[i]],na.rm=T),3)
  descripts[i,5]<-sum(is.na(covs[[i]])==F)
}

descript<-cbind(cbind(c("Routh Class 1-5","Routh Occupational Status 1-9","Class ISCO 1-10","Weekly Wage (Pounds)", "CP Party Member",
                        "Age in 1936","Woman","Arrival Date (Relative to FEA)","Days in Spain","KIA","Deserter"),"&"),descripts,"\\")
names(descript)<-c("","","Mean","","SD","","Complete Obs","")

tb1<-c(0,table(data$Ordered_Occ))
names(tb1)<-seq(1,10)

print(descript,row.names=F)


#})
  