#remove(list=ls())
source("code/build.R")


#suppressMessages({

ft1<-summary(feols(comm~Class,data[data$Occupation!="",], cluster = ~country))
ft2<-summary(feols(comm~Class|arrival,data=data[data$Occupation!="",], cluster = ~country))
ft3<-summary(feols(comm~Class|arrival +age +country+woman,data=data[data$Occupation!="",], cluster = ~country))
ft4<-summary(feols(comm~ifelse(Class >= 3,1,0)|arrival +age +country+woman,data=data[data$Occupation!="",], cluster = ~country))
ft5<-summary(feols(comm~Ordered_Occ|arrival +age +country+woman,data=data[data$Occupation!="",], cluster = ~country))
ft6<-summary(feols(comm~I(log(Weekly.Wage))|arrival +age +country+woman,data=data[data$Occupation!="",], cluster = ~country))
ft7<-summary(feols(comm~ISCO|arrival +age +country+woman,data=data[data$Occupation!="",], cluster = ~country))
ft8<-summary(feglm(comm~Class|arrival +age +country+woman,data=data[data$Occupation!="",],  family = binomial(link="logit"),cluster = ~country))



print(esttex(list(ft1,ft2,ft3,ft4,ft5,ft6,ft7,ft8),digits=3,digits.stats=3,signif.code=NA))

#})
