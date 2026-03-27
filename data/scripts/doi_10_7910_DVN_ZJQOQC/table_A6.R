#remove(list=ls())
source("code/build.R")

data2<-data[data$birthYear > 1901,]

#suppressMessages({
  
  
  ft3<-summary(feols(comm~Class|arrival +age +country+woman,data=data2[data2$Occupation!="",], cluster = ~country))
  des3<-feols(deserter~comm|country+age+woman+arrival,data2, cluster= ~country )
  kia3<-feols(killed~comm|country+age+woman+arrival,data2, cluster= ~country )
  
#})

print(esttex(list(ft3,des3,kia3),digits=3,digits.stats=3,signif.code=NA))

