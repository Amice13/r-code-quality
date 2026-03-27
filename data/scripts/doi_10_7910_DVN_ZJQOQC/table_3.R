#remove(list=ls())
source("code/build.R")


#suppressMessages({

kia1<-feols(killed~comm,data, cluster= ~country )
kia2<-feols(killed~comm|arrival,data, cluster= ~country )
kia3<-feols(killed~comm|country+age+woman+arrival,data, cluster= ~country )
kia4<-feols(killed~comm+deserter|country+age+woman+arrival,data, cluster= ~country )
kia5<-feglm(killed~comm+deserter|country+age+woman+arrival,data, cluster= ~country,family=binomial(link="logit") )


print(esttex(list(kia1,kia2,kia3,kia4,kia5),digits=3,digits.stats=3,signif.code=NA))


#})