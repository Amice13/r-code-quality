##################  This may not run fully--combines two r scripts---see breakpoint



#####USA Multi-variate for Mplus
library(plyr)
library(haven)
#library(expss)
library(questionr)

library(survey)

library(tidyverse)

usa<-read_dta( "C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/Fordataverse/uscut.dta")
usa<- usa %>% filter(!is.na(weight_w12))

### gender use us$gender  ###

###Rename as before 
usa$weight<-usa$weight_w12
usa$chaos1<-usa$w12chaos1
usa$chaos2<-usa$w12chaos2
usa$chaos3<-usa$w12chaos3
usa$chaos4<-usa$w12chaos4
usa$chaos5<-usa$w12chaos5
usa$chaos6<-usa$w12chaos6
usa$chaos7<-usa$w12chaos7
usa$chaos8<-usa$w12chaos8


usa$w12dark1<-mapvalues(usa$w12dark1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark2<-mapvalues(usa$w12dark2, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark3<-mapvalues(usa$w12dark3, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark4<-mapvalues(usa$w12dark4, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark5<-mapvalues(usa$w12dark5, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark6<-mapvalues(usa$w12dark6, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark7<-mapvalues(usa$w12dark7, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark8<-mapvalues(usa$w12dark8, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark10<-mapvalues(usa$w12dark10, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark11<-mapvalues(usa$w12dark11, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12dark12<-mapvalues(usa$w12dark12, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))

usa$dark1<-usa$w12dark1
usa$dark2<-usa$w12dark2
usa$dark3<-usa$w12dark3
usa$dark4<-usa$w12dark4
usa$dark5<-usa$w12dark5
usa$dark6<-usa$w12dark6
usa$dark7<-usa$w12dark7
usa$dark8<-usa$w12dark8
usa$dark10<-usa$w12dark10
usa$dark11<-usa$w12dark11
usa$dark12<-usa$w12dark12

####Variables for Multi-group Analysis
#usa$group<-1
#usa$idmg<-usa$id


#usamplusready<-c("idmg","weight", "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", 
#                "chaos7", "chaos8", "dark1", "dark2", "dark3", "dark4", "dark5", "dark6",
#                "dark7", "dark8", "dark10", "dark11", "dark12",
#                "group")
#usaready<-usa[usamplusready]

###  Write file that contains chaos and dark triad variables for export into mplus

#
#write.csv(usaready,"C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/usa.csv", row.names = TRUE)


usa$open1<-usa$w4open1
usa$open1 <- ifelse(is.na(usa$w4open1) & !is.na(usa$w5open1), usa$w5open1, usa$open1)
usa$open1[usa$open1==6]<-NA


usa$open3<-usa$w4open3
usa$open3 <- ifelse(is.na(usa$w4open3) & !is.na(usa$w5open3), usa$w5open3, usa$open3)
usa$open3[usa$open3==6]<-NA


usa$open4<-usa$w4open4
usa$open4 <- ifelse(is.na(usa$w4open4) & !is.na(usa$w5open4), usa$w5open4, usa$open4)
usa$open4[usa$open4==6]<-NA

usa$consc1<-usa$w4consc1
usa$consc1<-ifelse(is.na(usa$w4consc1) & !is.na(usa$w5consc1), usa$w5consc1, usa$consc1)
usa$consc1[usa$consc1==6]<-NA

usa$consc3<-usa$w4consc3
usa$consc3<-ifelse(is.na(usa$w4consc3) & !is.na(usa$w5consc3), usa$w5consc3, usa$consc3)
usa$consc3[usa$consc3==6]<-NA

usa$consc4<-usa$w4consc4
usa$consc4<-ifelse(is.na(usa$w4consc4) & !is.na(usa$w5consc4), usa$w5consc4, usa$consc4)
usa$consc4[usa$consc4==6]<-NA

usa$extra1<-usa$w4extra1
usa$extra1<-ifelse(is.na(usa$w4extra1) & !is.na(usa$w5extra1), usa$w5extra1, usa$extra1)
usa$extra1[usa$extra1==6]<-NA

usa$extra2<-usa$w4extra2
usa$extra2<-ifelse(is.na(usa$w4extra2) & !is.na(usa$w5extra2), usa$w5extra2, usa$extra2)
usa$extra2[usa$extra2==6]<-NA

usa$extra4<-usa$w4extra4
usa$extra4<-ifelse(is.na(usa$w4extra4) & !is.na(usa$w5extra4), usa$w5extra4, usa$extra4)
usa$extra4[usa$extra4==6]<-NA

usa$agree2<-usa$w4agree2
usa$agree2<-ifelse(is.na(usa$w4agree2) & !is.na(usa$w5agree2), usa$w5agree2, usa$agree2)
usa$agree2[usa$agree2==6]<-NA

usa$agree3<-usa$w4agree3
usa$agree3<-ifelse(is.na(usa$w4agree3) & !is.na(usa$w5agree3), usa$w5agree3, usa$agree3)
usa$agree3[usa$agree3==6]<-NA

usa$agree4<-usa$w4agree4
usa$agree4<-ifelse(is.na(usa$w4agree4) & !is.na(usa$w5agree4), usa$w5agree4, usa$agree4)
usa$agree4[usa$agree4==6]<-NA

usa$neurot1<-usa$w4neurot1
usa$neurot1<-ifelse(is.na(usa$w4neurot1) & !is.na(usa$w5neurot1), usa$w5neurot1, usa$neurot1)
usa$neurot1[usa$neurot1==6]<-NA

usa$neurot2<-usa$w4neurot2
usa$neurot2<-ifelse(is.na(usa$w4neurot2) & !is.na(usa$w5neurot2), usa$w5neurot2, usa$neurot2)
usa$neurot2[usa$neurot2==6]<-NA

usa$neurot4<-usa$w4neurot4
usa$neurot4<-ifelse(is.na(usa$w4neurot4) & !is.na(usa$w5neurot4), usa$w5neurot4, usa$neurot4)
usa$neurot4[usa$neurot4==6]<-NA

#  Race  

usa$black<-0
usa$black[usa$race==2]<-1

usa$hisp<-0
usa$hisp[usa$race==3]<-1

usa$asian<-0
usa$asian[usa$race==4]<-1

usa$orace<-0
usa$orace[usa$race>4]<-1


#  Ba or higher

usa$edudum<-ifelse(usa$educ>4, 1, 0) 

#  Generation

usa$generation<-cut(usa$birthyr,
                         breaks=c(-Inf, 1945.9, 1964.9, 1980.9, Inf),
                         labels=c("Silent", "Boomer", "X", "Millennial")) 


usa$silentg<-ifelse(usa$birthyr<1946,1,0)
usa$millen<-ifelse(usa$birthyr>1980,1,0)
usa$boomer<-ifelse(usa$birthyr>1945 & usa$birthyr<1965,1,0)
usa$genx<-ifelse(usa$birthyr>1964 & usa$birthyr<1981,1,0)



#ideology

usa$farleft<-ifelse(usa$w12wing1a==0 | usa$w12wing1a==1, 1,0)
usa$left<-ifelse(usa$w12wing1a==2 | usa$w12wing1a==3, 1,0)
usa$centre<-ifelse(usa$w12wing1a==4 | usa$w12wing1a==5 | usa$w12wing1a==6, 1,0)
usa$right<-ifelse(usa$w12wing1a==7 | usa$w12wing1a==8, 1,0)
usa$farright<-ifelse(usa$w12wing1a==9 | usa$w12wing1a==10, 1,0)

#participation

usa<-usa%>% mutate_at(vars(w12partic1aa, w12partic2aa, w12partic7aa, w12partic8aa, w12partic9aa, w12partic10aa, w12partic13aa,
                         w2partic20aa, w2partic21aa)
                    , ~ifelse(.==32767, NA, .))

partnames<-c("Display Badge", "Vote in National", "Work for PCG", "Lawful Protest", "Illegal Protest",
             "Local Election", "Give Money", "Social Media Politics Post", "Comment on Family Post")

usa$badge<-usa$w12partic1aa
usa$voten<-usa$w12partic2aa
usa$partywk<-usa$w12partic7aa
usa$legp<-usa$w12partic8aa
usa$illegp<-usa$w12partic9aa
usa$votel<-usa$w12partic10aa
usa$givemon<-usa$w12partic13aa
usa$smpost<-usa$w2partic20aa
usa$smcomm<-usa$w2partic21aa


library(plyr)
usa$w12eupan1r<-mapvalues(usa$w12eupan1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12eupan3r<-mapvalues(usa$w12eupan3, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12deathp1r<-mapvalues(usa$w12deathp1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA, NA))
usa$w12fremkt1r<-mapvalues(usa$w12fremkt1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12welfable1r<-mapvalues(usa$w12welfable1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))
usa$w12newlife1r<-mapvalues(usa$w12newlife1, c(1,2,3,4,5,6, NA), c(5,4,3,2,1,NA,NA))

usa$islam<-usa$w12eupan1r
usa$immhlt<-usa$w12eupan3r
usa$deathp<-usa$w12deathp1r
usa$fremkt<-usa$w12fremkt1r
usa$welfare<-usa$w12welfable1r
usa$newlife<-usa$w12newlife1r



forrandrmnl<-c("id","weight", "gender", "farleft","left","centre", "right", "farright", "edudum",
               "chaos1", "chaos2", "chaos3", "chaos4", "chaos5", "chaos6", "chaos7", "chaos8",
               "open1", "open3", "open4", "consc1", "consc3", "consc4", 
               "extra1", "extra2", "extra4", "agree2", "agree3", "agree4",
               "neurot1", "neurot2", "neurot4", "silentg", "millen", "boomer", "genx",
               "black", "hisp", "asian", "orace", 
               "badge", "voten", "partywk","legp", "illegp", "votel", "givemon",
               "smpost", "smcomm", "islam", "immhlt", "deathp", "fremkt", "welfare", "newlife")
mnlready<-usa[forrandrmnl]





#usaformplus <- read.csv("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/New_Runs_for_R&R/USA/usaformplus.csv", na.strings="-999")

withlatents <- read.csv("C:/Users/ts139c/Dropbox/Grants and Applications/ESRC/Bang_Peterson/New_Runs_for_R&R/USA/withlatents.csv", na.strings="-999")
usamulti <- merge(mnlready,withlatents,by="id")

us<-usamulti

#deleting cases where all missing on personality
uspersonality<-data.frame(us$id, us$consc1, us$consc3, us$consc4,
                          us$open1, us$open3, us$open4,
                          us$agree2, us$agree3, us$agree4,
                          us$neurot1, us$neurot2, us$neurot4,
                          us$extra1, us$extra2, us$extra4)
delete.na <- function(uspersonality, n=14) {
  uspersonality[rowSums(is.na(uspersonality)) <= n,]
}
uspersonalitychk<-delete.na(uspersonality)


uspersonality<-uspersonalitychk

###creating average dimension scores

uspersonality$conscm <- rowMeans(subset(uspersonality, select = c(us.consc1, us.consc3)), na.rm = TRUE)
uspersonality$openm <- rowMeans(subset(uspersonality, select = c(us.open1, us.open3, us.open4)), na.rm = TRUE)
uspersonality$agreem <- rowMeans(subset(uspersonality, select = c(us.agree2, us.agree3, us.agree4)), na.rm = TRUE)
uspersonality$neurotm <- rowMeans(subset(uspersonality, select = c(us.neurot1, us.neurot2)), na.rm = TRUE)
uspersonality$extram <- rowMeans(subset(uspersonality, select = c(us.extra1, us.extra2, us.extra4)), na.rm = TRUE)

uspersonalitym<-data.frame(uspersonality$us.id, uspersonality$conscm, uspersonality$openm,
                           uspersonality$agreem, uspersonality$neurotm, uspersonality$extram)
uspersonalitym$id<-uspersonality$us.id

###combining data with only those answering personality and average personality scores

usamulti <- merge(usamulti,uspersonalitym,by="id")
us<-usamulti

library(foreign)
library(nnet)
library(stargazer)


table(us$Chaos_class)
us$Chaos_class<-as.factor(us$Chaos_class)
us$Chaos_class<-mapvalues(us$Chaos_class, from = c("1", "2", "3", "4"), to = c("Rebuild", "Low",
                                                                               "Medium", "High"))

us$Chaos_class = relevel(us$Chaos_class, ref = "Low")

persmulti = multinom(Chaos_class ~ uspersonality.conscm + uspersonality.openm + 
                       uspersonality.agreem+ uspersonality.neurotm+ 
                       uspersonality.extram, data=us, weights=weight)


perswithallus=multinom(Chaos_class ~ uspersonality.conscm + uspersonality.openm + 
                         uspersonality.agreem+ uspersonality.neurotm+ 
                         uspersonality.extram+edudum+farleft+left+right+farright+
                         gender+silentg+millen+genx+black+hisp+asian+orace, data=us, weights=weight)

###Edu Interactions
us$edusilent<-us$edudum*us$silentg
us$edumillenial<-us$edudum*us$millen
us$edugenx<-us$edudum*us$genx


perswithallusint=multinom(Chaos_class ~ uspersonality.conscm + uspersonality.openm + 
                            uspersonality.agreem+ uspersonality.neurotm+ 
                            uspersonality.extram+edudum+farleft+left+right+farright+
                            gender+silentg+millen+genx+black+hisp+asian+orace+
                            edugenx+edusilent+edumillenial, data=us, weights=weight)


###  Table 3 Replications  

noedu<- data.frame(uspersonality.conscm=4.292273, uspersonality.openm=3.686854, uspersonality.agreem=3.890083, 
uspersonality.neurotm=2.938939,  uspersonality.extram=3.355583, edudum=0, farleft=0, left=0, right=0,
                   farright=0, gender=1, silentg=0, millen=1, genx=0, black=0, hisp=0, asian=0, orace=0, edugenx=0, 
                   edumillenial=0, edusilent=0)

round(predict(perswithallusint, type="probs", newdata=noedu),2)*100

highedu<- data.frame(uspersonality.conscm=4.292273, uspersonality.openm=3.686854, uspersonality.agreem=3.890083, 
                   uspersonality.neurotm=2.938939,  uspersonality.extram=3.355583, edudum=1, farleft=0, left=0, right=0,
            farright=0, gender=1, silentg=0, millen=0, genx=0, black=0, hisp=0, asian=0, orace=0, edugenx=0, 
                   edumillenial=0, edusilent=0)

round(predict(perswithallusint, type="probs", newdata=highedu),2)*100

###getting ideology predictions for tables 4

rightw<- data.frame(uspersonality.conscm=4.292273, uspersonality.openm=3.686854, uspersonality.agreem=3.890083, 
                   uspersonality.neurotm=2.938939,  uspersonality.extram=3.355583, edudum=0, farleft=1, left=0, right=0,
                   farright=0, gender=0, silentg=0, millen=0, genx=0, black=0, hisp=0, asian=0, orace=0, edugenx=0, 
                   edumillenial=0, edusilent=0)

round(predict(perswithallusint, type="probs", newdata=rightw),2)*100


####tables for participation  #

#  dummies for chaos classes as indies

library('fastDummies')
us <- dummy_cols(us, select_columns = 'Chaos_class')

#simple factor analysis participation

badgereg<-lm(badge~uspersonality.conscm+ uspersonality.openm + 
               uspersonality.agreem+ uspersonality.neurotm+ 
               uspersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+black+hisp+asian+orace+
               edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

votereg<-lm(voten~uspersonality.conscm+ uspersonality.openm + 
               uspersonality.agreem+ uspersonality.neurotm+ 
               uspersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+black+hisp+asian+orace+edugenx+edusilent+
               edumillenial+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

partywkreg<-lm(partywk~uspersonality.conscm+ uspersonality.openm + 
              uspersonality.agreem+ uspersonality.neurotm+ 
              uspersonality.extram+edudum+farleft+left+right+farright+
              gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
              Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)
 
legpreg<-lm(legp~uspersonality.conscm+ uspersonality.openm + 
                 uspersonality.agreem+ uspersonality.neurotm+ 
                 uspersonality.extram+edudum+farleft+left+right+farright+
                 gender+silentg+millen+genx+black+hisp+asian+orace+
                 edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                 Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

illegpregus<-lm(illegp~uspersonality.conscm+ uspersonality.openm + 
              uspersonality.agreem+ uspersonality.neurotm+ 
              uspersonality.extram+edudum+farleft+left+right+farright+
              gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
              Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)


votelpreg<-lm(votel~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

givemonreg<-lm(givemon~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
                 edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

smpostreg<-lm(smpost~uspersonality.conscm+ uspersonality.openm + 
                 uspersonality.agreem+ uspersonality.neurotm+ 
                 uspersonality.extram+edudum+farleft+left+right+farright+
                 gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                 Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

smcommreg<-lm(smcomm~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)



#####policy positions
islamreg<-lm(islam~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
               edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

immhltreg<-lm(immhlt~uspersonality.conscm+ uspersonality.openm + 
               uspersonality.agreem+ uspersonality.neurotm+ 
               uspersonality.extram+edudum+farleft+left+right+farright+
               gender+silentg+millen+genx+black+hisp+asian+orace+
               edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
               Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

deathpreg<-lm(deathp~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

fremktreg<-lm(fremkt~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

welfarereg<-lm(welfare~uspersonality.conscm+ uspersonality.openm + 
                uspersonality.agreem+ uspersonality.neurotm+ 
                uspersonality.extram+edudum+farleft+left+right+farright+
                gender+silentg+millen+genx+black+hisp+asian+orace+
                edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)

newlifereg<-lm(newlife~uspersonality.conscm+ uspersonality.openm + 
                 uspersonality.agreem+ uspersonality.neurotm+ 
                 uspersonality.extram+edudum+farleft+left+right+farright+
                 gender+silentg+millen+genx+black+hisp+asian+orace+
                 edugenx+edusilent+edumillenial+Chaos_class_Rebuild+
                 Chaos_class_Medium+Chaos_class_High, data=us, weights=weight)
#immhltus<-immhltreg
#illegprotus<-illegpreg
#stargazer(immhltus, type="latex", out="ustjs1.tex")
#stargazer(illegpreg, immhltreg, type="latex", out="ustjs1.tex")

 