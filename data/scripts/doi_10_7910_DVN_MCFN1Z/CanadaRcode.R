#####cantralia Multi-variate for Mplus
library(plyr)
library(haven)
library(expss)
library(questionr)
library(tidyverse)
library(survey)
library(car)

can<-read_dta("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/Fordataverse/Canadacut.dta")
 
can<- can %>% filter(wave==2)


###Rename as before 
can$id<-can$caseid

can$chaos1<-can$nfchaos_1
can$chaos2<-can$nfchaos_2
can$chaos3<-can$nfchaos_3
can$chaos4<-can$nfchaos_4
can$chaos5<-can$nfchaos_5
can$chaos6<-can$nfchaos_6
can$chaos7<-can$nfchaos_7
can$chaos8<-can$nfchaos_8


  


can$dark1<-can$dktetrad_5
can$dark2<-can$dktetrad_1
can$dark3<-can$dktetrad_7
can$dark4<-can$dktetrad_10
can$dark5<-can$dktetrad_6
can$dark6<-can$dktetrad_12
can$dark7<-can$dktetrad_11
can$dark8<-can$dktetrad_3
can$dark9<-can$dktetrad_8
can$dark10<-can$dktetrad_2
can$dark11<-can$dktetrad_4
can$dark12<-can$dktetrad_9



can$dark1<-mapvalues(can$dark1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark2<-mapvalues(can$dark2, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark3<-mapvalues(can$dark3, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark4<-mapvalues(can$dark4, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark5<-mapvalues(can$dark5, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark6<-mapvalues(can$dark6, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark7<-mapvalues(can$dark7, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark8<-mapvalues(can$dark8, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark10<-mapvalues(can$dark10, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark11<-mapvalues(can$dark11, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
can$dark12<-mapvalues(can$dark12, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))


#  id canada specific for multigroup
can$idmg<-can$id+70000
can$group<-3

justchaoscan<-c("idmg","weight", "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", 
                "chaos7", "chaos8", "dark1", "dark2", "dark3", "dark4", "dark5", "dark6",
                "dark7", "dark8", "dark10", "dark11", "dark12",
                "group")
canmplus<-can[justchaoscan]

 


#This is for exporting for MGA
#write.csv(canmplus,"C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/can.csv", row.names = TRUE)



can$extra1<-can$personality_1
 can$agree1<-can$personality_2
can$cons1<-can$personality_3 
 can$neurot1<-can$personality_4
 can$imagin1<-can$personality_5
 can$extra2<-can$personality_6
 can$agree2<-can$personality_7
 can$cons2<-can$personality_8
 can$neurot2<-can$personality_9
 can$imagin2<-can$personality_10
 can$extra3<-can$personality_11
 can$agree3<-can$personality_12
 can$cons3<-can$personality_13
 can$neurot3<-can$personality_14
 can$imagin3<-can$personality_15
can$extra4<-can$personality_16 
 can$agree4<-can$personality_17
 can$cons4<-can$personality_18
 can$neurot4<-can$personality_19
 can$imagin4<-can$personality_20
 
#gender
 
can$gender<-can$sex-1
 
 
#  Race  
 




can$aborig<-0
can$aborig[can$anc_1==6 |can$anc_2==6]<-1

can$immg<-0
can$immg[can$born==2]<-1

can$french<-0
can$french[can$survey_lang=='FR']<-2

#  Ba or higher

can$edudum<-ifelse(can$edu>8,1,0)

#  Generation

 

can$silentg<-ifelse(can$year_born<1946,1,0)
can$millen<-ifelse(can$year_born>1980,1,0)
can$boomer<-ifelse(can$year_born>1945 & can$year_born<1965,1,0)
can$genx<-ifelse(can$year_born>1964 & can$year_born<1981,1,0)



#ideology
 


can$farleft<-ifelse(can$ideol_1==0 | can$ideol_1==1, 1,0)
can$left<-ifelse(can$ideol_1==2 | can$ideol_1==3, 1,0)
can$centre<-ifelse(can$ideol_1==4 | can$ideol_1==5 | can$ideol_1==6, 1,0)
can$right<-ifelse(can$ideol_1==7 | can$ideol_1==8, 1,0)
can$farright<-ifelse(can$ideol_1==9 | can$ideol_1==10, 1,0)


#  Education, Generation Interactions

can$edgenx<-can$edudum*can$genx
can$edmill<-can$edudum*can$millen
can$edsilent<-can$edudum*can$silentg



mergecanadamain<-c("id","weight", "gender", "farleft","left","centre", "right", "farright", "edudum",
            "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", "chaos7", "chaos8",
            "imagin1", "imagin2", "imagin3", "imagin4", "cons1", "cons2", "cons3", "cons4", 
            "extra1", "extra2", "extra3","extra4","agree1", "agree2", 
            "agree3", "agree4",
            "neurot1", "neurot2", "neurot3", "neurot4", "silentg", "millen", "boomer", "genx",
            "immg", "aborig", "french", "edgenx", "edmill", "edsilent")
canadarest<-can[mergecanadamain]



                 



###This is to merge with the single country likely profile from mplus most likely 
canwithlat <- read.csv("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/New_Runs_for_R&R/Canada/canadacprob.csv", na.strings="-999")
canmulti <- merge(canadarest,canwithlat,by="id")

canada<-canmulti


#  personality

canadapersonality<-data.frame(canada$id, canada$imagin2, canada$imagin3, canada$imagin4,
                              canada$cons2, canada$cons4,
                              canada$agree1, canada$agree2, canada$agree4,
                              canada$neurot1, canada$neurot3,
                              canada$extra2, canada$extra3, canada$extra4)
delete.na <- function(canadapersonality, n=13) {
        canadapersonality[rowSums(is.na(canadapersonality)) <= n,]
}
canadapersonalitychk<-delete.na(canadapersonality)


canadapersonality<-canadapersonalitychk

###reverse coding

canadapersonality$canada.extra3r=recode(canadapersonality$canada.extra3, 
                                         '1=5; 2=4; 3=3; 4=2; 5=1')
canadapersonality$canada.agree1r=recode(canadapersonality$canada.agree1, 
                                        '1=5; 2=4; 3=3; 4=2; 5=1')

canadapersonality$consm <- rowMeans(subset(canadapersonality, select = c(canada.cons2, canada.cons4)), 
                                 na.rm = TRUE)

canadapersonality$imaginm <- rowMeans(subset(canadapersonality, select = 
                                                     c(canada.imagin2, canada.imagin3,
                                                     canada.imagin4)), na.rm = TRUE)

canadapersonality$neurotm <- rowMeans(subset(canadapersonality, select = c(canada.neurot1, canada.neurot3)), 
                                    na.rm = TRUE)

canadapersonality$agreem<-rowMeans(subset(canadapersonality, select=
                                                  c(canada.agree1r, canada.agree2,
                                                    canada.agree4)), na.rm=TRUE)


canadapersonality$extram<-rowMeans(subset(canadapersonality, select=
                                                  c(canada.extra2, canada.extra3r,
                                                    canada.extra4)), na.rm=TRUE)


canadapersonalitym<-data.frame(canadapersonality$canada.id, canadapersonality$consm, 
                               canadapersonality$imaginm,
                                canadapersonality$agreem, canadapersonality$neurotm, 
                                canadapersonality$extram)

canadapersonalitym$id<-canadapersonality$canada.id


canadamulti <- merge(canada,canadapersonalitym,by="id")

library(foreign)
library(nnet)
library(stargazer)
library(plyr)

table(canadamulti$Chaos_cl)
canadamulti$Chaos_cl<-as.factor(canadamulti$Chaos_cl)
canadamulti$Chaos_cl<-mapvalues(canadamulti$Chaos_cl, from = c("1", "2", "3", "4"), to 
                           = c("Low", "Medium", "Rebuild", "High"))

canada$Chaos_cl = relevel(canada$Chaos_cl, ref = "Low")


persmulti = multinom(Chaos_cl ~ canadapersonality.consm + canadapersonality.imaginm + 
                             canadapersonality.agreem+ canadapersonality.neurotm+ 
                             canadapersonality.extram, data=canadamulti, weights=weight)


persmultifull = multinom(Chaos_cl ~ canadapersonality.consm + canadapersonality.imaginm + 
                             canadapersonality.agreem+ canadapersonality.neurotm+ 
                             canadapersonality.extram+french+aborig+immg+
                                 genx+millen+silentg+farleft+left+right+farright+
                                 gender+edudum,
                              data=canadamulti, weights=weight)

persmultifullint = multinom(Chaos_cl ~ canadapersonality.consm + canadapersonality.imaginm + 
                                 canadapersonality.agreem+ canadapersonality.neurotm+ 
                                 canadapersonality.extram+french+aborig+immg+
                                 genx+millen+silentg+farleft+left+right+farright+
                                 gender+edudum+edsilent+edmill+edgenx,
                         data=canadamulti, weights=weight)

lowedu<- data.frame(canadapersonality.consm=3.684839, canadapersonality.imaginm=3.524721, 
                    canadapersonality.agreem=3.671132, 
                    canadapersonality.neurotm=3.335885,  canadapersonality.extram=2.947368, edudum=0, farleft=0, left=0, right=0,
                    farright=0, gender=0, silentg=1, millen=0, genx=0, french=0, aborig=0, immg=0, edgenx=0, edmill=0, edsilent=0)

round(predict(persmultifullint, type="probs", newdata=lowedu),2)*100


highedu<- data.frame(canadapersonality.consm=3.684839, canadapersonality.imaginm=3.524721, 
                    canadapersonality.agreem=3.671132, 
                    canadapersonality.neurotm=3.335885,  canadapersonality.extram=2.947368, edudum=1, farleft=0, left=0, right=0,
                    farright=0, gender=0, silentg=1, millen=0, genx=0, french=0, aborig=0, immg=0, edgenx=0, edmill=0, edsilent=1)

round(predict(persmultifullint, type="probs", newdata=highedu),2)*100

####This replicates far right ideology

farright<- data.frame(canadapersonality.consm=3.684839, canadapersonality.imaginm=3.524721, 
                    canadapersonality.agreem=3.671132, 
                    canadapersonality.neurotm=3.335885,  canadapersonality.extram=2.947368, edudum=0, farleft=0, left=0, right=0,
                    farright=1, gender=0, silentg=0, millen=0, genx=0, french=0, aborig=0, immg=0, edgenx=0, edmill=0, edsilent=0)

round(predict(persmultifullint, type="probs", newdata=farright),2)*100



stargazer(persmulti, type="latex", out="canadajustpers.tex")
stargazer(persmultifull, type="latex", out="canadaall.tex")
stargazer(persmultifullint, type="latex", out="canadaallint.tex")




