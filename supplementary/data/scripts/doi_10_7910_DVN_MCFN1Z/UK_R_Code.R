#####UK Multi-variate for Mplus
library(plyr)
library(haven)
library(expss)
library(questionr)
library(tidyverse)
library(survey)


uk<-read_dta("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/Fordataverse/ukcut.dta")
uk<- uk %>% filter(!is.na(W8wave12))

### gender use us$gender  ###

###Rename as before 
uk$weight<-uk$W8wave12
uk$chaos1<-uk$w12chaos1
uk$chaos2<-uk$w12chaos2
uk$chaos3<-uk$w12chaos3
uk$chaos4<-uk$w12chaos4
uk$chaos5<-uk$w12chaos5
uk$chaos6<-uk$w12chaos6
uk$chaos7<-uk$w12chaos7
uk$chaos8<-uk$w12chaos8

uk$w12dark1<-mapvalues(uk$w12dark1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark2<-mapvalues(uk$w12dark2, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark3<-mapvalues(uk$w12dark3, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark4<-mapvalues(uk$w12dark4, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark5<-mapvalues(uk$w12dark5, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark6<-mapvalues(uk$w12dark6, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark7<-mapvalues(uk$w12dark7, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark8<-mapvalues(uk$w12dark8, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark10<-mapvalues(uk$w12dark10, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark11<-mapvalues(uk$w12dark11, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12dark12<-mapvalues(uk$w12dark12, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))


uk$dark1<-uk$w12dark1
uk$dark2<-uk$w12dark2
uk$dark3<-uk$w12dark3
uk$dark4<-uk$w12dark4
uk$dark5<-uk$w12dark5
uk$dark6<-uk$w12dark6
uk$dark7<-uk$w12dark7
uk$dark8<-uk$w12dark8
uk$dark10<-uk$w12dark10
uk$dark11<-uk$w12dark11
uk$dark12<-uk$w12dark12



####Variables for Multi-group Analysis
###This gets to a dataset for mplus

#uk$group<-2
#uk$idmg<-uk$id+2000000


#ukmplusready<-c("id", "idmg","weight", "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", 
#                 "chaos7", "chaos8", "dark1", "dark2", "dark3", "dark4", "dark5", "dark6",
#                 "dark7", "dark8", "dark10", "dark11", "dark12",
#                 "group")
#ukready<-uk[ukmplusready]

#write.csv(ukready,"C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/uk.csv", row.names = TRUE)

###read in latent profile from mplus
ukwithlatents <- read.csv("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/New_Runs_for_R&R/UK/ukwithlatents.csv", na.strings="-999")
#ukmulti <- merge(ukready,ukwithlatents,by="id")

#Personality Recodes

uk$open1<-uk$w4open1
uk$open1 <- ifelse(is.na(uk$w4open1) & !is.na(uk$w5open1), uk$w5open1, uk$open1)
uk$open1[uk$open1==6]<-NA


uk$open3<-uk$w4open3
uk$open3 <- ifelse(is.na(uk$w4open3) & !is.na(uk$w5open3), uk$w5open3, uk$open3)
uk$open3[uk$open3==6]<-NA


uk$open4<-uk$w4open4
uk$open4 <- ifelse(is.na(uk$w4open4) & !is.na(uk$w5open4), uk$w5open4, uk$open4)
uk$open4[uk$open4==6]<-NA

uk$consc1<-uk$w4consc1
uk$consc1<-ifelse(is.na(uk$w4consc1) & !is.na(uk$w5consc1), uk$w5consc1, uk$consc1)
uk$consc1[uk$consc1==6]<-NA

uk$consc3<-uk$w4consc3
uk$consc3<-ifelse(is.na(uk$w4consc3) & !is.na(uk$w5consc3), uk$w5consc3, uk$consc3)
uk$consc3[uk$consc3==6]<-NA

uk$consc4<-uk$w4consc4
uk$consc4<-ifelse(is.na(uk$w4consc4) & !is.na(uk$w5consc4), uk$w5consc4, uk$consc4)
uk$consc4[uk$consc4==6]<-NA

uk$extra1<-uk$w4extra1
uk$extra1<-ifelse(is.na(uk$w4extra1) & !is.na(uk$w5extra1), uk$w5extra1, uk$extra1)
uk$extra1[uk$extra1==6]<-NA

uk$extra2<-uk$w4extra2
uk$extra2<-ifelse(is.na(uk$w4extra2) & !is.na(uk$w5extra2), uk$w5extra2, uk$extra2)
uk$extra2[uk$extra2==6]<-NA

uk$extra4<-uk$w4extra4
uk$extra4<-ifelse(is.na(uk$w4extra4) & !is.na(uk$w5extra4), uk$w5extra4, uk$extra4)
uk$extra4[uk$extra4==6]<-NA

uk$agree2<-uk$w4agree2
uk$agree2<-ifelse(is.na(uk$w4agree2) & !is.na(uk$w5agree2), uk$w5agree2, uk$agree2)
uk$agree2[uk$agree2==6]<-NA

uk$agree3<-uk$w4agree3
uk$agree3<-ifelse(is.na(uk$w4agree3) & !is.na(uk$w5agree3), uk$w5agree3, uk$agree3)
uk$agree3[uk$agree3==6]<-NA

uk$agree4<-uk$w4agree4
uk$agree4<-ifelse(is.na(uk$w4agree4) & !is.na(uk$w5agree4), uk$w5agree4, uk$agree4)
uk$agree4[uk$agree4==6]<-NA

uk$neurot1<-uk$w4neurot1
uk$neurot1<-ifelse(is.na(uk$w4neurot1) & !is.na(uk$w5neurot1), uk$w5neurot1, uk$neurot1)
uk$neurot1[uk$neurot1==6]<-NA

uk$neurot2<-uk$w4neurot2
uk$neurot2<-ifelse(is.na(uk$w4neurot2) & !is.na(uk$w5neurot2), uk$w5neurot2, uk$neurot2)
uk$neurot2[uk$neurot2==6]<-NA

uk$neurot4<-uk$w4neurot4
uk$neurot4<-ifelse(is.na(uk$w4neurot4) & !is.na(uk$w5neurot4), uk$w5neurot4, uk$neurot4)
uk$neurot4[uk$neurot4==6]<-NA

###Control Variables

#  Race  

uk$BME<-0
uk$BME[uk$ethnicity_new>4 & uk$ethnicity_new<19]<-1

#  Ba or higher


uk$edudum<-ifelse(uk$profile_education_level>15 & uk$profile_education_level<19, 1, 0)

#  Generation

uk$birthyr<-2018-uk$age
uk$generation<-cut(uk$birthyr,
                         breaks=c(-Inf, 1945.9, 1964.9, 1980.9, Inf),
                         labels=c("Silent", "Boomer", "X", "Millennial")) 


uk$silentg<-ifelse(uk$birthyr<1946,1,0)
uk$millen<-ifelse(uk$birthyr>1980,1,0)
uk$boomer<-ifelse(uk$birthyr>1945 & uk$birthyr<1965,1,0)
uk$genx<-ifelse(uk$birthyr>1964 & uk$birthyr<1981,1,0)



#ideology

uk$farleft<-ifelse(uk$w12wing1a==0 | uk$w12wing1a==1, 1,0)
uk$left<-ifelse(uk$w12wing1a==2 | uk$w12wing1a==3, 1,0)
uk$centre<-ifelse(uk$w12wing1a==4 | uk$w12wing1a==5 | uk$w12wing1a==6, 1,0)
uk$right<-ifelse(uk$w12wing1a==7 | uk$w12wing1a==8, 1,0)
uk$farright<-ifelse(uk$w12wing1a==9 | uk$w12wing1a==10, 1,0)

###  Interactions 
uk$edgenx<-uk$edudum*uk$genx
uk$edmil<-uk$edudum*uk$millen
uk$edsilent<-uk$edudum*uk$silentg

 











#  personality clean

#deleting cases where all missing on personality
ukpersonality<-data.frame(uk$id, uk$consc1, uk$consc3,
                          uk$open1, uk$open3, uk$open4,
                          uk$agree2, uk$agree3, uk$agree4,
                          uk$neurot1, uk$neurot2,
                          uk$extra1, uk$extra2, uk$extra4)
delete.na <- function(ukpersonality, n=13) {
  ukpersonality[rowSums(is.na(ukpersonality)) <= n,]
}
ukpersonalitychk<-delete.na(ukpersonality)


ukpersonality<-ukpersonalitychk


ukpersonality$conscm <- rowMeans(subset(ukpersonality, select = c(uk.consc1, uk.consc3)), na.rm = TRUE)
ukpersonality$openm <- rowMeans(subset(ukpersonality, select = c(uk.open1, uk.open3, uk.open4)), na.rm = TRUE)
ukpersonality$agreem <- rowMeans(subset(ukpersonality, select = c(uk.agree2, uk.agree3, uk.agree4)), na.rm = TRUE)
ukpersonality$neurotm <- rowMeans(subset(ukpersonality, select = c(uk.neurot1, uk.neurot2)), na.rm = TRUE)
ukpersonality$extram <- rowMeans(subset(ukpersonality, select = c(uk.extra1, uk.extra2, uk.extra4)), na.rm = TRUE)


ukpersonalitym<-data.frame(ukpersonality$uk.id, ukpersonality$conscm, ukpersonality$openm,
                           ukpersonality$agreem, ukpersonality$neurotm, ukpersonality$extram)
ukpersonalitym$id<-ukpersonality$uk.id

uk <- merge(uk,ukpersonalitym,by="id")
uk<-merge(uk,ukwithlatents,by="id") 
library(foreign)
library(nnet)
library(stargazer)
library(plyr)


table(uk$Chaos_cl)
uk$Chaos_class<-as.factor(uk$Chaos_cl)
uk$Chaos_class<-mapvalues(uk$Chaos_class, from = c("1", "2", "3", "4"), to = c("Low", "Medium",
                                                                               "High", "Rebuild"))

uk$Chaos_class = relevel(uk$Chaos_class, ref = "Low")


#  Just personality

persmulti = multinom(Chaos_class ~ ukpersonality.conscm + ukpersonality.openm + 
                       ukpersonality.agreem+ ukpersonality.neurotm+ 
                       ukpersonality.extram, data=uk, weights=weight)



perswithalluk=multinom(Chaos_class ~ ukpersonality.conscm + ukpersonality.openm + 
                         ukpersonality.agreem+ ukpersonality.neurotm+ 
                         ukpersonality.extram+edudum+farleft+left+right+farright+
                         gender+silentg+millen+genx+BME, data=uk, weights=weight)

perswithallukint=multinom(Chaos_class ~ ukpersonality.conscm + ukpersonality.openm + 
                         ukpersonality.agreem+ ukpersonality.neurotm+ 
                         ukpersonality.extram+edudum+farleft+left+right+farright+
                         gender+silentg+millen+genx+BME+
                           edgenx+edmil+edsilent, data=uk, weights=weight)

###Sample code to get predicted probabilities for Table 3

noedu<- data.frame(ukpersonality.conscm=4.160503, ukpersonality.openm=3.52624, ukpersonality.agreem=3.680692, 
                   ukpersonality.neurotm=3.239045,  ukpersonality.extram=3.30356, edudum=0, farleft=0, left=0, right=0,
                   farright=0, gender=1, silentg=0, millen=0, genx=0, BME=0, edgenx=0, edmil=0, edsilent=0)



round(predict(perswithallukint, type="probs", newdata=noedu),2)*100

highedu<- data.frame(ukpersonality.conscm=4.160503, ukpersonality.openm=3.52624, ukpersonality.agreem=3.680692, 
                     ukpersonality.neurotm=3.239045,  ukpersonality.extram=3.30356, edudum=1, farleft=0, left=0, right=0,
                     farright=0, gender=1, silentg=0, millen=1, genx=0, BME=0, edgenx=0, edmil=1, edsilent=0)

round(predict(perswithallukint, type="probs", newdata=highedu),2)*100


###  Sample Code to get High Chaos by Predicted ideology 

farright<- data.frame(ukpersonality.conscm=4.160503, ukpersonality.openm=3.52624, ukpersonality.agreem=3.680692, 
                   ukpersonality.neurotm=3.239045,  ukpersonality.extram=3.30356, edudum=0, farleft=0, left=0, right=0,
                   farright=1, gender=1, silentg=0, millen=0, genx=0, BME=0, edgenx=0, edmil=0, edsilent=0)



round(predict(perswithallukint, type="probs", newdata=farright),2)*100


stargazer(persmulti, type="latex", out="ukjustpers.tex")
stargazer(perswithalluk, type="latex", out="ukall.tex")
stargazer(perswithallukint, type="latex", out="ukallint.tex")





###Participation

#participation




uk<-uk%>% mutate_at(vars(w12partic1aa, w12partic2aa, w12partic7aa, w12partic8aa, w12partic9aa, w12partic10aa, w12partic13aa,
                         w2partic20aa, w2partic21aa)
                    , ~ifelse(.==999, NA, .))

partnames<-c("Display Badge", "Vote in National", "Work for PCG", "Lawful Protest", "Illegal Protest",
             "Local Election", "Give Money", "Social Media Politics Post", "Comment on Family Post")

uk$badge<-uk$w12partic1aa
uk$voten<-uk$w12partic2aa
uk$partywk<-uk$w12partic7aa
uk$legp<-uk$w12partic8aa
uk$illegp<-uk$w12partic9aa
uk$votel<-uk$w12partic10aa
uk$givemon<-uk$w12partic13aa
uk$smpost<-uk$w2partic20aa
uk$smcomm<-uk$w2partic21aa

###social values

library(plyr)
uk$w12eupan1r<-mapvalues(uk$w12eupan1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12eupan3r<-mapvalues(uk$w12eupan3, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12deathp1r<-mapvalues(uk$w12deathp1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA, NA))
uk$w12fremkt1r<-mapvalues(uk$w12fremkt1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12welfable1r<-mapvalues(uk$w12welfable1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
uk$w12newlife1r<-mapvalues(uk$w12newlife1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))

uk$islam<-uk$w12eupan1r
uk$immhlt<-uk$w12eupan3r
uk$deathp<-uk$w12deathp1r
uk$fremkt<-uk$w12fremkt1r
uk$welfare<-uk$w12welfable1r
uk$newlife<-uk$w12newlife1r                 








library('fastDummies')
uk <- dummy_cols(uk, select_columns = 'Chaos_class')

baderreg<-lm(badge ~ ukpersonality.conscm + ukpersonality.openm + 
               ukpersonality.agreem+ ukpersonality.neurotm+ 
               ukpersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+BME+
               edgenx+edmil+edsilent+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

votenreg<-lm(voten ~ ukpersonality.conscm + ukpersonality.openm + 
               ukpersonality.agreem+ ukpersonality.neurotm+ 
               ukpersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+BME+
               edgenx+edmil+edsilent+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

ptywk1<-lm(partywk ~ ukpersonality.conscm + ukpersonality.openm + 
             ukpersonality.agreem+ ukpersonality.neurotm+ 
             ukpersonality.extram+edudum+farleft+left+right+farright+
             gender+silentg+millen+genx+BME+
             edgenx+edmil+edsilent+Chaos_class_Rebuild+
             Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

legpreg<-lm(legp ~ ukpersonality.conscm + ukpersonality.openm + 
              ukpersonality.agreem+ ukpersonality.neurotm+ 
              ukpersonality.extram+edudum+farleft+left+right+farright+
              gender+silentg+millen+genx+BME+
              edgenx+edmil+edsilent+Chaos_class_Rebuild+
              Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)


illegpreguk<-lm(illegp ~ ukpersonality.conscm + ukpersonality.openm + 
                  ukpersonality.agreem+ ukpersonality.neurotm+ 
                  ukpersonality.extram+edudum+farleft+left+right+farright+
                  gender+silentg+millen+genx+BME+
                  edgenx+edmil+edsilent+Chaos_class_Rebuild+
                  Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

votelreg<-lm(votel ~ ukpersonality.conscm + ukpersonality.openm + 
               ukpersonality.agreem+ ukpersonality.neurotm+ 
               ukpersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+BME+
               edgenx+edmil+edsilent+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

givemonreg<-lm(givemon ~ ukpersonality.conscm + ukpersonality.openm + 
                 ukpersonality.agreem+ ukpersonality.neurotm+ 
                 ukpersonality.extram+edudum+farleft+left+right+farright+
                 gender+silentg+millen+genx+BME+
                 edgenx+edmil+edsilent+Chaos_class_Rebuild+
                 Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

smpostreg<-lm(smpost ~ ukpersonality.conscm + ukpersonality.openm + 
                ukpersonality.agreem+ ukpersonality.neurotm+ 
                ukpersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+BME+
                edgenx+edmil+edsilent+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

smcommreg<-lm(smcomm ~ ukpersonality.conscm + ukpersonality.openm + 
                ukpersonality.agreem+ ukpersonality.neurotm+ 
                ukpersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+BME+
                edgenx+edmil+edsilent+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

#regressions on policies

islam1<-lm(islam ~ ukpersonality.conscm + ukpersonality.openm + 
             ukpersonality.agreem+ ukpersonality.neurotm+ 
             ukpersonality.extram+edudum+farleft+left+right+farright+
             gender+silentg+millen+genx+BME+
             edgenx+edmil+edsilent+Chaos_class_Rebuild+
             Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)


immhlt1uk<-lm(immhlt ~ ukpersonality.conscm + ukpersonality.openm + 
                ukpersonality.agreem+ ukpersonality.neurotm+ 
                ukpersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+BME+
                edgenx+edmil+edsilent+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

deathp1<-lm(deathp ~ ukpersonality.conscm + ukpersonality.openm + 
              ukpersonality.agreem+ ukpersonality.neurotm+ 
              ukpersonality.extram+edudum+farleft+left+right+farright+
              gender+silentg+millen+genx+BME+
              edgenx+edmil+edsilent+Chaos_class_Rebuild+
              Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

fremkt1<-lm(fremkt ~ ukpersonality.conscm + ukpersonality.openm + 
              ukpersonality.agreem+ ukpersonality.neurotm+ 
              ukpersonality.extram+edudum+farleft+left+right+farright+
              gender+silentg+millen+genx+BME+
              edgenx+edmil+edsilent+Chaos_class_Rebuild+
              Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)

welfare1<-lm(welfare ~ ukpersonality.conscm + ukpersonality.openm + 
               ukpersonality.agreem+ ukpersonality.neurotm+ 
               ukpersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+BME+
               edgenx+edmil+edsilent+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)


newlife1<-lm(newlife ~ ukpersonality.conscm + ukpersonality.openm + 
               ukpersonality.agreem+ ukpersonality.neurotm+ 
               ukpersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+BME+
               edgenx+edmil+edsilent+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=uk, weights=weight)



####variables used in analyses

ukstandard<-c("id","weight", "gender", "farleft","left","centre", "right", "farright", "edudum",
              "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", "chaos7", "chaos8",
              "open1", "open3", "open4", "consc1", "consc3", "consc4", 
              "extra1", "extra2", "extra4", "agree2", "agree3", "agree4",
              "neurot1", "neurot2", "neurot4", "silentg", "millen", "boomer", "genx",
              "BME", "edgenx", "edmil", "edsilent", "w12partic1aa", "w12partic2aa", "w12partic7aa", 
              "w12partic8aa", "w12partic9aa", "w12partic10aa", "w12partic13aa",
              "w2partic20aa", "w2partic21aa", "w12eupan1", "w12eupan3",
              "w12deathp1", "w12fremkt1", "w12welfable1", "w12newlife1", "Chaos_cl")
ukready<-uk[ukstandard]

