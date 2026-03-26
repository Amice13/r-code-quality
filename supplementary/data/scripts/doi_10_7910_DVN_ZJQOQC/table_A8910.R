
#remove(list=ls())
source("code/build.R")

#suppressMessages({


### Mean Communinsts at a given time

kia3_meancomm<-feols(killed~comm+I(100*mean_comms)|country+age+woman+arrival,data, cluster= ~country )
kia4_meancomm<-feols(killed~comm+I(100*mean_comms)+deserter|country+age+woman+arrival,data, cluster= ~country )

des3_meancomm<-feols(deserter~comm+I(100*mean_comms)|country+age+woman+arrival,data, cluster= ~country )
des4_meancomm<-feols(deserter~comm+I(100*mean_comms)+killed|country+age+woman+arrival,data, cluster= ~country )


print(esttex(list(des3_meancomm,des4_meancomm,kia3_meancomm,kia4_meancomm),digits=3,digits.stats=3, signif.code=NA))


### Mean Communists by country of origin

kia3_meancount<-feols(killed~comm+I(100*mean_country)|age+woman+arrival,data, cluster= ~country )
kia4_meancount<-feols(killed~comm+I(100*mean_country)+deserter|age+woman+arrival,data, cluster= ~country )

des3_meancount<-feols(deserter~comm+I(100*mean_country)|age+woman+arrival,data, cluster= ~country )
des4_meancount<-feols(deserter~comm+I(100*mean_country)+killed|age+woman+arrival,data, cluster= ~country )


print(esttex(list(des3_meancount,des4_meancount,kia3_meancount,kia4_meancount),digits=3,digits.stats=3, signif.code=NA))



### Arrival Date


kia3_arriv<-feols(killed~comm:I(arrival_date_run/365)+comm|country+age+woman+arrival,data, cluster= ~country )
kia4_arriv<-feols(killed~comm:I(arrival_date_run/365)+comm+deserter|country+age+woman+arrival,data, cluster= ~country )

des3_arriv<-feols(deserter~comm:I(arrival_date_run/365)+comm|country+age+woman+arrival,data, cluster= ~country )
des4_arriv<-feols(deserter~comm:I(arrival_date_run/365)+comm+killed|country+age+woman+arrival,data, cluster= ~country )


print(esttex(list(des3_arriv,des4_arriv,kia3_arriv,kia4_arriv),digits=3,digits.stats=3, signif.code=NA))



#})  