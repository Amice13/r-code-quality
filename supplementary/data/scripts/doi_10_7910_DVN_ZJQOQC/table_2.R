#remove(list=ls())
source("code/build.R")


#suppressMessages({

des1<-feols(deserter~comm,data, cluster= ~country )
des2<-feols(deserter~comm|arrival,data, cluster= ~country )
des3<-feols(deserter~comm|country+age+woman+arrival,data, cluster= ~country )
des4<-feols(deserter~comm+killed|country+age+woman+arrival,data, cluster= ~country )
des5<-feglm(deserter~comm+killed|country+age+woman+arrival,data,family=binomial(link="logit"), cluster= ~country )

print(esttex(list(des1,des2,des3,des4,des5),digits=3,digits.stats=3,signif.code=NA))

#})