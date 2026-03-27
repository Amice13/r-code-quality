
#remove(list=ls())
source("code/build.R")

#suppressMessages({
  

pop_39_a<-7317000+7665000+6736400+5712200+4729700+2967300+1042000+134900
pop_39_f<-7317000+7665000+6736400+5712200+4729700+2967300

comm_39_a<-17756

pfight_39_a<-nrow(data)/pop_39_f

pcomm_fight_a<-mean(data$comm)

pcom_39_a<-comm_39_a/pop_39_a

attrsk_39_a<-pfight_39_a*(pcomm_fight_a-pcom_39_a)/(pcom_39_a*(1-pcom_39_a))



####################################################


pop_39_am<-3586400+3724200+3175500+2620900+2180800+1330800+405900+42900
pop_39_mf<-3586400+3724200+3175500+2620900+2180800+1330800


women<-data[data$woman==1,]
men<-data[data$woman==0,]



comm_39_am<-17756-sum(women$comm)

pfight_39_am<-nrow(men)/pop_39_mf

pcomm_fight_m<-mean(men$comm)

pcom_39_m<-comm_39_am/pop_39_am

attrsk_39_am<-pfight_39_am*(pcomm_fight_m-pcom_39_m)/(pcom_39_m*(1-pcom_39_m))



######################################################


comm_39_am2<-17756/2

pfight_39_am<-nrow(men)/pop_39_mf

pcomm_fight_m<-mean(men$comm)

pcom_39_m<-comm_39_am2/pop_39_am

attrsk_39_am2<-pfight_39_am*(pcomm_fight_m-pcom_39_m)/(pcom_39_m*(1-pcom_39_m))



###############################################################3


pop_37_a<-7454000+7620200+6580600+5650800+4652400+2775700+973200+121700
pop_37_f<-7454000+7620200+6580600+5650800+4652400+2775700


comm_37_a<-12500

pfight_37_a<-nrow(data)/pop_37_f

pcomm_fight_a<-mean(data$comm)

pcom_37_a<-comm_37_a/pop_37_a

attrsk_37_a<-pfight_37_a*(pcomm_fight_a-pcom_37_a)/(pcom_37_a*(1-pcom_37_a))



print(c(attrsk_39_a, attrsk_39_am, attrsk_39_am2,attrsk_37_a))


#})


