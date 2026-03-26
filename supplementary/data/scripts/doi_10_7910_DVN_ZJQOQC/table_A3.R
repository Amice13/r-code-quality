#remove(list=ls())
source("code/build.R")


########### Results ## Student Fighters #################

#suppressMessages({


## Omit Students

st1<-summary(feols(comm~Class,data[data$Occupation!="" & data$student==0,], cluster = ~country))
st2<-summary(feols(comm~Class|arrival,data=data[data$Occupation!=""& data$student==0,], cluster = ~country))
st3<-summary(feols(comm~Class|arrival +age +country+woman,data=data[data$Occupation!=""& data$student==0,], cluster = ~country))

## Assign students lowest value

st4<-summary(feols(comm~ifelse(student==1,5,Class),data[data$Occupation!="",], cluster = ~country))
st5<-summary(feols(comm~ifelse(student==1,5,Class)|arrival,data=data[data$Occupation!="",], cluster = ~country))
st6<-summary(feols(comm~ifelse(student==1,5,Class)|arrival +age +country+woman,data=data[data$Occupation!="",], cluster = ~country))



print(esttex(list(st1,st2,st3,st4,st5,st6),digits=3,digits.stats=3,signif.code=NA))



#})