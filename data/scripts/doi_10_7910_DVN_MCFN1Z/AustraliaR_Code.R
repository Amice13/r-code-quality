#####Australia Multi-variate for Mplus
library(plyr)
library(haven)
library(expss)
library(questionr)
library(tidyverse)
library(survey)

aus<-read_dta("****/Australiacut.dta")
 
aus<- aus %>% filter(wave==2)


###Rename as before 
aus$id<-aus$caseid

aus$chaos1<-aus$nfchaos_1
aus$chaos2<-aus$nfchaos_2
aus$chaos3<-aus$nfchaos_3
aus$chaos4<-aus$nfchaos_4
aus$chaos5<-aus$nfchaos_5
aus$chaos6<-aus$nfchaos_6
aus$chaos7<-aus$nfchaos_7
aus$chaos8<-aus$nfchaos_8






aus$dark1<-aus$dktetrad_5
aus$dark2<-aus$dktetrad_1
aus$dark3<-aus$dktetrad_7
aus$dark4<-aus$dktetrad_10
aus$dark5<-aus$dktetrad_6
aus$dark6<-aus$dktetrad_12
aus$dark7<-aus$dktetrad_11
aus$dark8<-aus$dktetrad_3
aus$dark9<-aus$dktetrad_8
aus$dark10<-aus$dktetrad_2
aus$dark11<-aus$dktetrad_4
aus$dark12<-aus$dktetrad_9



aus$dark1<-mapvalues(aus$dark1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark2<-mapvalues(aus$dark2, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark3<-mapvalues(aus$dark3, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark4<-mapvalues(aus$dark4, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark5<-mapvalues(aus$dark5, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark6<-mapvalues(aus$dark6, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark7<-mapvalues(aus$dark7, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark8<-mapvalues(aus$dark8, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark10<-mapvalues(aus$dark10, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark11<-mapvalues(aus$dark11, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
aus$dark12<-mapvalues(aus$dark12, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))


aus$idmg<-aus$id+170000
aus$group<-4


justchaosaus<-c("idmg","weight", "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", 
                 "chaos7", "chaos8", "dark1", "dark2", "dark3", "dark4", "dark5", "dark6",
                 "dark7", "dark8", "dark10", "dark11", "dark12",
                 "group")
ausmplus<-aus[justchaosaus]

#write.csv(ausmplus,"C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/aus.csv", row.names = TRUE)



aus$extra1<-aus$personality_1
 aus$agree1<-aus$personality_2
aus$cons1<-aus$personality_3 
 aus$neurot1<-aus$personality_4
 aus$imagin1<-aus$personality_5
 aus$extra2<-aus$personality_6
 aus$agree2<-aus$personality_7
 aus$cons2<-aus$personality_8
 aus$neurot2<-aus$personality_9
 aus$imagin2<-aus$personality_10
 aus$extra3<-aus$personality_11
 aus$agree3<-aus$personality_12
 aus$cons3<-aus$personality_13
 aus$neurot3<-aus$personality_14
 aus$imagin3<-aus$personality_15
aus$extra4<-aus$personality_16 
 aus$agree4<-aus$personality_17
 aus$cons4<-aus$personality_18
 aus$neurot4<-aus$personality_19
 aus$imagin4<-aus$personality_20
 
#gender
 
aus$gender<-aus$sex-1
 
 
#  Race  
 

aus$aborig<-0
aus$aborig[aus$abor==1]<-1

aus$immg<-0
aus$immg[aus$born==2]<-1

#  Ba or higher

aus$edudum<-ifelse(aus$edu>9, 1, 0)
#  Generation

 
aus$generation<-cut(aus$year_born,
                          breaks=c(-Inf, 1945.9, 1964.9, 1980.9, Inf),
                          labels=c("Silent", "Boomer", "X", "Millennial")) 




aus$silentg<-ifelse(aus$year_born<1946,1,0)
aus$millen<-ifelse(aus$year_born>1980,1,0)
aus$boomer<-ifelse(aus$year_born>1945 & aus$year_born<1965,1,0)
aus$genx<-ifelse(aus$year_born>1964 & aus$year_born<1981,1,0)



#ideology



aus$farleft<-ifelse(aus$ideol_1==0 | aus$ideol_1==1, 1,0)
aus$left<-ifelse(aus$ideol_1==2 | aus$ideol_1==3, 1,0)
aus$centre<-ifelse(aus$ideol_1==4 | aus$ideol_1==5 | aus$ideol_1==6, 1,0)
aus$right<-ifelse(aus$ideol_1==7 | aus$ideol_1==8, 1,0)
aus$farright<-ifelse(aus$ideol_1==9 | aus$ideol_1==10, 1,0)


#  Education, Generation Interactions

aus$edgenx<-aus$edudum*aus$genx
aus$edmill<-aus$edudum*aus$millen
aus$edsilent<-aus$edudum*aus$silentg




formerge<-c("id","weight", "gender", "farleft","left","centre", "right", "farright", "edudum",
            "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", "chaos7", "chaos8",
            "imagin1", "imagin2", "imagin3", "imagin4", "cons1", "cons2", "cons3", "cons4", 
            "extra1", "extra2", "extra3","extra4","agree1", "agree2", 
            "agree3", "agree4",
            "neurot1", "neurot2", "neurot3", "neurot4", "silentg", "millen", "boomer", "genx",
            "immg", "aborig", "edgenx", "edmill", "edsilent")
australiarest<-aus[formerge]





                 


###########this merges  data waith latent profiles with variables reneeded for remaining analyses

auswithlat <- read.csv("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/New_Runs_for_R&R/Australia/auslpa.csv", na.strings="-999")
ausmulti<-merge(australiarest,auswithlat,by="id")


australiapersonality<-data.frame(aus$id, aus$imagin2, aus$imagin3, aus$imagin4,
                                 aus$cons2, aus$cons4,
                                 aus$agree1, aus$agree2, aus$agree4,
                                 aus$neurot1, aus$neurot3,
                                 aus$extra2, aus$extra3, aus$extra4)

delete.na <- function(australiapersonality, n=13) {
        australiapersonality[rowSums(is.na(australiapersonality)) <= n,]
}
australiapersonalitychk<-delete.na(australiapersonality)


australiapersonality<-australiapersonalitychk

library(car)



australiapersonality$aus.extra3r=recode(australiapersonality$aus.extra3, 
                                        '1=5; 2=4; 3=3; 4=2; 5=1')
australiapersonality$aus.agree1r=recode(australiapersonality$aus.agree1, 
                                        '1=5; 2=4; 3=3; 4=2; 5=1')


####This is getting an index of each personality trait



australiapersonality$consm <- rowMeans(subset(australiapersonality, select = c(aus.cons2, aus.cons4)), 
                                       na.rm = TRUE)

australiapersonality$imaginm <- rowMeans(subset(australiapersonality, select = 
                                                        c(aus.imagin2, aus.imagin3,
                                                          aus.imagin4)), na.rm = TRUE)

australiapersonality$neurotm <- rowMeans(subset(australiapersonality, select = c(aus.neurot1, aus.neurot3)), 
                                         na.rm = TRUE)

australiapersonality$agreem<-rowMeans(subset(australiapersonality, select=
                                                     c(aus.agree1r, aus.agree2,
                                                       aus.agree4)), na.rm=TRUE)


australiapersonality$extram<-rowMeans(subset(australiapersonality, select=
                                                     c(aus.extra2, aus.extra3r,
                                                       aus.extra4)), na.rm=TRUE)


australiapersonalitym<-data.frame(australiapersonality$aus.id, australiapersonality$consm, 
                                  australiapersonality$imaginm,
                                  australiapersonality$agreem, australiapersonality$neurotm, 
                                  australiapersonality$extram)

australiapersonalitym$id<-australiapersonality$aus.id

australiamulti <- merge(ausmulti,australiapersonalitym,by="id")
australia<-australiamulti

library(foreign)
library(nnet)
library(stargazer)
library(plyr)


table(australia$Chaos_cl)
australia$Chaos_cl<-as.factor(australia$Chaos_cl)
australia$Chaos_cl<-mapvalues(australia$Chaos_cl, from = c("1", "2", "3", "4"), to 
                              = c("Rebuild", "Medium", "High", "Low"))


australia$Chaos_cl = relevel(australia$Chaos_cl, ref = "Low")

###These are the multinomial logits presented in Table 3 of the Appendix!


persmulti = multinom(Chaos_cl ~ australiapersonality.consm + australiapersonality.imaginm + 
                             australiapersonality.agreem+ australiapersonality.neurotm+ 
                             australiapersonality.extram, data=australia, weights=weight)


persmultifull = multinom(Chaos_cl ~ australiapersonality.consm + australiapersonality.imaginm + 
                                 australiapersonality.agreem+ australiapersonality.neurotm+ 
                                 australiapersonality.extram+aborig+immg+
                                 genx+millen+silentg+farleft+left+right+farright+
                                 gender+edudum,
                         data=australia, weights=weight)


persmultifullint = multinom(Chaos_cl ~ australiapersonality.consm + australiapersonality.imaginm + 
                                    australiapersonality.agreem+ australiapersonality.neurotm+ 
                                    australiapersonality.extram+aborig+immg+
                                    genx+millen+silentg+farleft+left+right+farright+
                                    gender+edudum+edsilent+edmill+edgenx,
                            data=australia, weights=weight)

###These generate the predicted probabilities presented in Table 3 of the Text

lowedu<- data.frame(australiapersonality.consm=3.52, australiapersonality.imaginm=3.400606, 
                    australiapersonality.agreem=3.618788, 
                    australiapersonality.neurotm=3.187727,  australiapersonality.extram=2.854242, edudum=0, farleft=0, left=0, right=0,
                    farright=0, gender=0, silentg=0, millen=1, genx=0, aborig=0, immg=0, edgenx=0, edmill=0, edsilent=0)

round(predict(persmultifullint, type="probs", newdata=lowedu),2)*100


highedu<- data.frame(australiapersonality.consm=3.52, australiapersonality.imaginm=3.400606, 
                     australiapersonality.agreem=3.618788, 
                     australiapersonality.neurotm=3.187727,  australiapersonality.extram=2.854242, edudum=1, farleft=0, left=0, right=0,
                     farright=0, gender=0, silentg=1, millen=0, genx=0, aborig=0, immg=0, edgenx=0, edmill=0, edsilent=1)

round(predict(persmultifullint, type="probs", newdata=highedu),2)*100

###############

farrights<- data.frame(australiapersonality.consm=3.52, australiapersonality.imaginm=3.400606, 
                    australiapersonality.agreem=3.618788, 
                    australiapersonality.neurotm=3.187727,  australiapersonality.extram=2.854242, edudum=0, farleft=0, left=1, right=0,
                    farright=0, gender=0, silentg=0, millen=0, genx=0, aborig=0, immg=0, edgenx=0, edmill=0, edsilent=0)

round(predict(persmultifullint, type="probs", newdata=farrights),2)*100


stargazer(persmulti, type="latex", out="ausjustpers.tex")
stargazer(persmultifull, type="latex", out="ausall.tex")
stargazer(persmultifullint, type="latex", out="ausallint.tex")