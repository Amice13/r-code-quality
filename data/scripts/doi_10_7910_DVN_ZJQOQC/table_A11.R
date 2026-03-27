
#remove(list=ls())
source("code/build.R")

#suppressMessages({
#Suppress warnings due to the dummies package, which works but triggers warnings for any R version since R 3.6.
#Suppress warnings of mass points detected in the running variable in rdd. We discuss this on p. 28 of the article.
#Suppress warnings of dropping redundant fixed effect variables due to multicollinearity.

suppressWarnings({
    

des7<-feols(deserter~comm+killed|country+age+woman+arrival+Class,data, cluster= ~country )
des8<-feols(deserter~comm+killed|country+age+woman+arrival+Occupation,data, cluster= ~country )
kia7<-feols(killed~comm+deserter|country+age+woman+arrival+Class,data, cluster= ~country )
kia8<-feols(killed~comm+deserter|country+age+woman+arrival+Occupation,data, cluster= ~country )


print(esttex(list(des7,des8,kia7,kia8),digits=3,digits.stats=3,signif.code=NA))





country_dums<-dummy(data$country)
age_dums<-dummy(data$age)
class_dums<-dummy(data$Class)
occ_dums<-dummy(data$Occupation)



a4_occ<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36],occ_dums[,2:ncol(occ_dums)]), masspoints = "adjust")
a4_class<-rdrobust(y=data$comm,x=data$arrival_date_run,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36],occ_dums[,2:ncol(class_dums)]), masspoints = "adjust")



out_rd<-data.frame(matrix("&",4,8))

out_cols<-seq(1,8,by=2)


out_rd[1,out_cols] <-  c( round(a4_class$coef[1,1],3), round(a4_occ$coef[1,1],3), round(a4_class$coef[2,1],3), round(a4_occ$coef[2,1],3))
out_rd[2,out_cols] <- paste( paste("(",c( round(a4_class$se[1,1],3), round(a4_occ$se[1,1],3), round(a4_class$se[3,1],3), round(a4_occ$se[3,1],3)),sep=""),")",sep="")
out_rd[3,out_cols] <- c( round(a4_class$bws[1,1],3), round(a4_occ$bws[1,1],3), round(a4_class$bws[1,1],3), round(a4_occ$bws[1,1],3))
out_rd[4,out_cols] <- c( paste(a4_class$N_h[1],a4_class$N_h[2],sep="/"),  paste(a4_occ$N_h[1],a4_occ$N_h[2],sep="/"),  paste(a4_class$N_h[1],a4_class$N_h[2],sep="/"), paste(a4_occ$N_h[1],a4_occ$N_h[2],sep="/"))

out_rd[,8]<-"\\"


print(out_rd)


  })
#})