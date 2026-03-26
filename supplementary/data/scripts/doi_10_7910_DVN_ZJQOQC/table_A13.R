
#remove(list=ls())
source("code/build.R")

#suppressMessages({
#  suppressWarnings({
    

data$labour_party<-ifelse(data[,"pOrg"] %in% c("BLLoY","LP","Labour Party", "LP & LLoY","LPSA"),1,0 )


kia_other<-feols(killed~comm+other_left+deserter|country+age+woman+arrival,data, cluster= ~country )
kF_other<-linearHypothesis(kia_other,"other_left=comm",test="F")

kia_labour<-feols(killed~comm+labour_party+deserter|country+age+woman+arrival,data, cluster= ~country )
kF_labour<-linearHypothesis(kia_labour,"labour_party=comm",test="F")


des_other<-feols(deserter~comm+other_left+killed|country+age+woman+arrival,data, cluster= ~country )
dF_other<-linearHypothesis(des_other,"other_left=comm",test="F")

des_labour<-feols(deserter~comm+labour_party+killed|country+age+woman+arrival,data, cluster= ~country )
dF_labour<-linearHypothesis(des_labour,"labour_party=comm",test="F")


print(esttex(list(kia_other,kia_labour, des_other,des_labour),digits=3,digits.stats=3,signif.code=NA))



kF<-list(kF_other,kF_labour,dF_other,dF_labour)

kO<-data.frame(matrix("&",2,8))
rws<-seq(1,8,by=2)

for(i in 1:4){
  kO[1,rws[i]] <-round(kF[[i]][[3]][2],3)
  kO[2,rws[i]] <-paste(paste("(", round(kF[[i]][[4]][2],3),sep=""),")",sep="")
}

kO<-cbind("&",kO)
kO[,ncol(kO)]<-"\\"

print(kO)

#  })
#})
