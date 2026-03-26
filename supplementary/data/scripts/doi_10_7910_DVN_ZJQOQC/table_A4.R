#remove(list=ls())
source("code/build.R")

#suppressMessages({
  


data$Class_noc_2<-ifelse(data$Occupation=="",2,data$Class)
data$Class_noc_3<-ifelse(data$Occupation=="",3,data$Class)
data$Class_noc_4<-ifelse(data$Occupation=="",4,data$Class)
data$Class_noc_5<-ifelse(data$Occupation=="",max(data$Class),data$Class)

ft_alt_class5<-feols(comm~Class_noc_5|arrival +age +country+woman,data=data, cluster = ~country)
ft_alt_class4<-feols(comm~Class_noc_4|arrival +age +country+woman,data=data, cluster = ~country)
ft_alt_class3<-feols(comm~Class_noc_3|arrival +age +country+woman,data=data, cluster = ~country)
ft_alt_class2<-feols(comm~Class_noc_2|arrival +age +country+woman,data=data, cluster = ~country)

data$no_occ<-ifelse(data$Occupation=="",1,0)

ft_alt_no_occ<-feols(comm~no_occ+Class|arrival +age +country+woman,data=data, cluster = ~country)


print(esttex(list(ft_alt_class5,ft_alt_class4,ft_alt_class3,ft_alt_class2,ft_alt_no_occ),digits=3,digits.stats=3))

#})


