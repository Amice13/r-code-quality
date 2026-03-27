# Load Necessary Packages
library(dplyr)

# Load Data
setwd("~/Dropbox/Projects/What is Woke")
coding <- read.csv("Woke Coding Initial.csv",sep=",",header=T)

# Combined Ratings
coding$party_s <- ifelse(coding$liberal._s==1,1,ifelse(coding$conservative_s==1,-1,0))
coding$party_j <- ifelse(coding$liberal_j==1,1,ifelse(coding$conservative_j==1,-1,0))
coding$race_s <- ifelse(coding$race_prog_s==1,1,ifelse(coding$race_con_s==1,-1,0))
coding$race_j <- ifelse(coding$race_prog_j==1,1,ifelse(coding$race_con_j==1,-1,0))
coding$gender_s <- ifelse(coding$gender_prog_s==1,1,ifelse(coding$gender_con_s==1,-1,0))
coding$gender_j <- ifelse(coding$gender_prog_j==1,1,ifelse(coding$gender_con_j==1,-1,0))

# Correlations
## Raw
cor.test(coding[,2],coding[,3]) # Democratic: r=.66, p<.001
cor.test(coding[,4],coding[,5]) # Republican: r=.75, p<.001
cor.test(coding[,6],coding[,7]) # Racial Progressivism: r=.41, p<.001
cor.test(coding[,8],coding[,9]) # Racial Conservatism: r=.57, p<.001
cor.test(coding[,10],coding[,11]) # Gender Progressivism: r=.69, p<.001
cor.test(coding[,12],coding[,13]) # Gender Conservatism: r=.92, p<.001
## Combined
cor.test(coding$party_s,coding$party_j) # r=.78, p<.001
cor.test(coding$race_s,coding$race_j) # r=.54, p<.001
cor.test(coding$gender_s,coding$gender_j) # r=.85, p<.001

# Cohen's Kappa
## Party
summary(coding$party_s==1 & coding$party_j==1) # 34 codes
summary(coding$party_s==0 & coding$party_j==1) # 4 codes
summary(coding$party_s==-1 & coding$party_j==1) # 2 codes
summary(coding$party_s==1 & coding$party_j==0) # 11 codes
summary(coding$party_s==0 & coding$party_j==0) # 15 codes
summary(coding$party_s==-1 & coding$party_j==0) # 7 codes
summary(coding$party_s==1 & coding$party_j==-1) # 0 codes
summary(coding$party_s==0 & coding$party_j==-1) # 2 codes
summary(coding$party_s==-1 & coding$party_j==-1) # 25 codes
kappa.party <- data.frame(label=factor(c("Lib","Null","Con"),levels=c("Lib","Null","Con")),
                              lib=c(34,4,2),
                              null=c(11,15,7),
                              con=c(0,2,25))
sum.agree <- kappa.party[1,2]+kappa.party[2,3]+kappa.party[3,4]
sum.expfreq <- sum(c((sum(kappa.party[1:3,2])*sum(kappa.party[1,2:4]))/(sum(kappa.party[1:3,2]+kappa.party[1:3,3]+kappa.party[1:3,4])),
                     (sum(kappa.party[1:3,3])*sum(kappa.party[2,2:4]))/(sum(kappa.party[1:3,2]+kappa.party[1:3,3]+kappa.party[1:3,4])),
                     (sum(kappa.party[1:3,4])*sum(kappa.party[3,2:4]))/(sum(kappa.party[1:3,2]+kappa.party[1:3,3]+kappa.party[1:3,4]))))
kappa <- (sum.agree-sum.expfreq)/(sum(kappa.party[1:3,2]+kappa.party[1:3,3]+kappa.party[1:3,4])-sum.expfreq) # kappa=.605
## Democratic
summary(coding$liberal._s==1 & coding$liberal_j==1) # 34 cases
summary(coding$liberal._s==0 & coding$liberal_j==1) # 6 cases
summary(coding$liberal._s==1 & coding$liberal_j==0) # 11 cases
summary(coding$liberal._s==0 & coding$liberal_j==0) # 49 cases
kappa.party.dem <- data.frame(label=factor(c("Yes","No"),levels=c("Yes","No")),
                              yes=c(34,6),
                              no=c(11,49))
agree <- (kappa.party.dem[1,2]+kappa.party.dem[2,3])/(kappa.party.dem[1,2]+kappa.party.dem[2,3]+kappa.party.dem[1,3]+kappa.party.dem[2,2])
chance <- (((kappa.party.dem[1,2]+kappa.party.dem[2,2])/(kappa.party.dem[1,2]+kappa.party.dem[2,3]+kappa.party.dem[1,3]+kappa.party.dem[2,2]))*
  ((kappa.party.dem[1,2]+kappa.party.dem[1,3])/(kappa.party.dem[1,2]+kappa.party.dem[2,3]+kappa.party.dem[1,3]+kappa.party.dem[2,2])))+
  (((kappa.party.dem[1,3]+kappa.party.dem[2,3])/(kappa.party.dem[1,2]+kappa.party.dem[2,3]+kappa.party.dem[1,3]+kappa.party.dem[2,2]))*
     ((kappa.party.dem[2,2]+kappa.party.dem[2,3])/(kappa.party.dem[1,2]+kappa.party.dem[2,3]+kappa.party.dem[1,3]+kappa.party.dem[2,2])))
(agree-chance)/(1-chance) # Kappa=.653
## Republican
summary(coding$conservative_s==1 & coding$conservative_j==1) # 25 cases
summary(coding$conservative_s==0 & coding$conservative_j==1) # 2 cases
summary(coding$conservative_s==1 & coding$conservative_j==0) # 9 cases
summary(coding$conservative_s==0 & coding$conservative_j==0) # 64 cases
kappa.party.rep <- data.frame(label=factor(c("Yes","No"),levels=c("Yes","No")),
                              yes=c(25,2),
                              no=c(9,64))
agree <- (kappa.party.rep[1,2]+kappa.party.rep[2,3])/(kappa.party.rep[1,2]+kappa.party.rep[2,3]+kappa.party.rep[1,3]+kappa.party.rep[2,2])
chance <- (((kappa.party.rep[1,2]+kappa.party.rep[2,2])/(kappa.party.rep[1,2]+kappa.party.rep[2,3]+kappa.party.rep[1,3]+kappa.party.rep[2,2]))*
             ((kappa.party.rep[1,2]+kappa.party.rep[1,3])/(kappa.party.rep[1,2]+kappa.party.rep[2,3]+kappa.party.rep[1,3]+kappa.party.rep[2,2])))+
  (((kappa.party.rep[1,3]+kappa.party.rep[2,3])/(kappa.party.rep[1,2]+kappa.party.rep[2,3]+kappa.party.rep[1,3]+kappa.party.rep[2,2]))*
     ((kappa.party.rep[2,2]+kappa.party.rep[2,3])/(kappa.party.rep[1,2]+kappa.party.rep[2,3]+kappa.party.rep[1,3]+kappa.party.rep[2,2])))
(agree-chance)/(1-chance) # Kappa=.742
## Race
summary(coding$race_s==1 & coding$race_j==1) # 14 codes
summary(coding$race_s==0 & coding$race_j==1) # 3 codes
summary(coding$race_s==-1 & coding$race_j==1) # 1 codes
summary(coding$race_s==1 & coding$race_j==0) # 21 codes
summary(coding$race_s==0 & coding$race_j==0) # 35 codes
summary(coding$race_s==-1 & coding$race_j==0) # 12 codes
summary(coding$race_s==1 & coding$race_j==-1) # 1 codes
summary(coding$race_s==0 & coding$race_j==-1) # 1 codes
summary(coding$race_s==-1 & coding$race_j==-1) # 12 codes
kappa.race <- data.frame(label=factor(c("Lib","Null","Con"),levels=c("Lib","Null","Con")),
                          lib=c(14,3,1),
                          null=c(21,35,12),
                          con=c(1,1,12))
sum.agree <- kappa.race[1,2]+kappa.race[2,3]+kappa.race[3,4]
sum.expfreq <- sum(c((sum(kappa.race[1:3,2])*sum(kappa.race[1,2:4]))/(sum(kappa.race[1:3,2]+kappa.race[1:3,3]+kappa.race[1:3,4])),
                     (sum(kappa.race[1:3,3])*sum(kappa.race[2,2:4]))/(sum(kappa.race[1:3,2]+kappa.race[1:3,3]+kappa.race[1:3,4])),
                     (sum(kappa.race[1:3,4])*sum(kappa.race[3,2:4]))/(sum(kappa.race[1:3,2]+kappa.race[1:3,3]+kappa.race[1:3,4]))))
kappa <- (sum.agree-sum.expfreq)/(sum(kappa.race[1:3,2]+kappa.race[1:3,3]+kappa.race[1:3,4])-sum.expfreq) # kappa=.386

## Racial Progressivism
summary(coding$race_prog_s==1 & coding$race_prog_j==1) # 14 cases
summary(coding$race_prog_s==0 & coding$race_prog_j==1) # 4 cases
summary(coding$race_prog_s==1 & coding$race_prog_j==0) # 22 cases
summary(coding$race_prog_s==0 & coding$race_prog_j==0) # 60 cases
kappa.party.raceprog <- data.frame(label=factor(c("Yes","No"),levels=c("Yes","No")),
                              yes=c(14,4),
                              no=c(22,60))
agree <- (kappa.party.raceprog[1,2]+kappa.party.raceprog[2,3])/(kappa.party.raceprog[1,2]+kappa.party.raceprog[2,3]+kappa.party.raceprog[1,3]+kappa.party.raceprog[2,2])
chance <- (((kappa.party.raceprog[1,2]+kappa.party.raceprog[2,2])/(kappa.party.raceprog[1,2]+kappa.party.raceprog[2,3]+kappa.party.raceprog[1,3]+kappa.party.raceprog[2,2]))*
             ((kappa.party.raceprog[1,2]+kappa.party.raceprog[1,3])/(kappa.party.raceprog[1,2]+kappa.party.raceprog[2,3]+kappa.party.raceprog[1,3]+kappa.party.raceprog[2,2])))+
  (((kappa.party.raceprog[1,3]+kappa.party.raceprog[2,3])/(kappa.party.raceprog[1,2]+kappa.party.raceprog[2,3]+kappa.party.raceprog[1,3]+kappa.party.raceprog[2,2]))*
     ((kappa.party.raceprog[2,2]+kappa.party.raceprog[2,3])/(kappa.party.raceprog[1,2]+kappa.party.raceprog[2,3]+kappa.party.raceprog[1,3]+kappa.party.raceprog[2,2])))
(agree-chance)/(1-chance) # Kappa=.36

## Racial Conservatism
summary(coding$race_con_s==1 & coding$race_con_j==1) # 12 cases
summary(coding$race_con_s==0 & coding$race_con_j==1) # 2 cases
summary(coding$race_con_s==1 & coding$race_con_j==0) # 13 cases
summary(coding$race_con_s==0 & coding$race_con_j==0) # 73 cases
kappa.party.racecon <- data.frame(label=factor(c("Yes","No"),levels=c("Yes","No")),
                                  yes=c(12,2),
                                  no=c(13,73))
agree <- (kappa.party.racecon[1,2]+kappa.party.racecon[2,3])/(kappa.party.racecon[1,2]+kappa.party.racecon[2,3]+kappa.party.racecon[1,3]+kappa.party.racecon[2,2])
chance <- (((kappa.party.racecon[1,2]+kappa.party.racecon[2,2])/(kappa.party.racecon[1,2]+kappa.party.racecon[2,3]+kappa.party.racecon[1,3]+kappa.party.racecon[2,2]))*
             ((kappa.party.racecon[1,2]+kappa.party.racecon[1,3])/(kappa.party.racecon[1,2]+kappa.party.racecon[2,3]+kappa.party.racecon[1,3]+kappa.party.racecon[2,2])))+
  (((kappa.party.racecon[1,3]+kappa.party.racecon[2,3])/(kappa.party.racecon[1,2]+kappa.party.racecon[2,3]+kappa.party.racecon[1,3]+kappa.party.racecon[2,2]))*
     ((kappa.party.racecon[2,2]+kappa.party.racecon[2,3])/(kappa.party.racecon[1,2]+kappa.party.racecon[2,3]+kappa.party.racecon[1,3]+kappa.party.racecon[2,2])))
(agree-chance)/(1-chance) # Kappa=.531



## Gender
summary(coding$gender_s==1 & coding$gender_j==1) # 20 codes
summary(coding$gender_s==0 & coding$gender_j==1) # 4 codes
summary(coding$gender_s==-1 & coding$gender_j==1) # 0 codes
summary(coding$gender_s==1 & coding$gender_j==0) # 8 codes
summary(coding$gender_s==0 & coding$gender_j==0) # 43 codes
summary(coding$gender_s==-1 & coding$gender_j==0) # 3 codes
summary(coding$gender_s==1 & coding$gender_j==-1) # 0 codes
summary(coding$gender_s==0 & coding$gender_j==-1) # 0 codes
summary(coding$gender_s==-1 & coding$gender_j==-1) # 22 codes
kappa.gender <- data.frame(label=factor(c("Lib","Null","Con"),levels=c("Lib","Null","Con")),
                           lib=c(20,4,0),
                           null=c(8,43,3),
                           con=c(0,0,22))
sum.agree <- kappa.gender[1,2]+kappa.gender[2,3]+kappa.gender[3,4]
sum.expfreq <- sum(c((sum(kappa.gender[1:3,2])*sum(kappa.gender[1,2:4]))/(sum(kappa.gender[1:3,2]+kappa.gender[1:3,3]+kappa.gender[1:3,4])),
                     (sum(kappa.gender[1:3,3])*sum(kappa.gender[2,2:4]))/(sum(kappa.gender[1:3,2]+kappa.gender[1:3,3]+kappa.gender[1:3,4])),
                     (sum(kappa.gender[1:3,4])*sum(kappa.gender[3,2:4]))/(sum(kappa.gender[1:3,2]+kappa.gender[1:3,3]+kappa.gender[1:3,4]))))
kappa <- (sum.agree-sum.expfreq)/(sum(kappa.gender[1:3,2]+kappa.gender[1:3,3]+kappa.gender[1:3,4])-sum.expfreq) # kappa=.760

## Gender Progressivism
summary(coding$gender_prog_s==1 & coding$gender_prog_j==1) # 20 cases
summary(coding$gender_prog_s==0 & coding$gender_prog_j==1) # 4 cases
summary(coding$gender_prog_s==1 & coding$gender_prog_j==0) # 8 cases
summary(coding$gender_prog_s==0 & coding$gender_prog_j==0) # 68 cases
kappa.party.genderprog <- data.frame(label=factor(c("Yes","No"),levels=c("Yes","No")),
                                     yes=c(20,4),
                                     no=c(8,68))
agree <- (kappa.party.genderprog[1,2]+kappa.party.genderprog[2,3])/(kappa.party.genderprog[1,2]+kappa.party.genderprog[2,3]+kappa.party.genderprog[1,3]+kappa.party.genderprog[2,2])
chance <- (((kappa.party.genderprog[1,2]+kappa.party.genderprog[2,2])/(kappa.party.genderprog[1,2]+kappa.party.genderprog[2,3]+kappa.party.genderprog[1,3]+kappa.party.genderprog[2,2]))*
             ((kappa.party.genderprog[1,2]+kappa.party.genderprog[1,3])/(kappa.party.genderprog[1,2]+kappa.party.genderprog[2,3]+kappa.party.genderprog[1,3]+kappa.party.genderprog[2,2])))+
  (((kappa.party.genderprog[1,3]+kappa.party.genderprog[2,3])/(kappa.party.genderprog[1,2]+kappa.party.genderprog[2,3]+kappa.party.genderprog[1,3]+kappa.party.genderprog[2,2]))*
     ((kappa.party.genderprog[2,2]+kappa.party.genderprog[2,3])/(kappa.party.genderprog[1,2]+kappa.party.genderprog[2,3]+kappa.party.genderprog[1,3]+kappa.party.genderprog[2,2])))
(agree-chance)/(1-chance) # Kappa=.689

## Gender Conservatism
summary(coding$gender_con_s==1 & coding$gender_con_j==1) # 22 cases
summary(coding$gender_con_s==0 & coding$gender_con_j==1) # 0 cases
summary(coding$gender_con_s==1 & coding$gender_con_j==0) # 3 cases
summary(coding$gender_con_s==0 & coding$gender_con_j==0) # 75 cases
kappa.party.gendercon <- data.frame(label=factor(c("Yes","No"),levels=c("Yes","No")),
                                    yes=c(22,0),
                                    no=c(3,75))
agree <- (kappa.party.gendercon[1,2]+kappa.party.gendercon[2,3])/(kappa.party.gendercon[1,2]+kappa.party.gendercon[2,3]+kappa.party.gendercon[1,3]+kappa.party.gendercon[2,2])
chance <- (((kappa.party.gendercon[1,2]+kappa.party.gendercon[2,2])/(kappa.party.gendercon[1,2]+kappa.party.gendercon[2,3]+kappa.party.gendercon[1,3]+kappa.party.gendercon[2,2]))*
             ((kappa.party.gendercon[1,2]+kappa.party.gendercon[1,3])/(kappa.party.gendercon[1,2]+kappa.party.gendercon[2,3]+kappa.party.gendercon[1,3]+kappa.party.gendercon[2,2])))+
  (((kappa.party.gendercon[1,3]+kappa.party.gendercon[2,3])/(kappa.party.gendercon[1,2]+kappa.party.gendercon[2,3]+kappa.party.gendercon[1,3]+kappa.party.gendercon[2,2]))*
     ((kappa.party.gendercon[2,2]+kappa.party.gendercon[2,3])/(kappa.party.gendercon[1,2]+kappa.party.gendercon[2,3]+kappa.party.gendercon[1,3]+kappa.party.gendercon[2,2])))
(agree-chance)/(1-chance) # Kappa=.917

# Agreement by Category
coding$category <- factor(c(rep("Party",3),rep("Race",5),rep("Religion",7),rep("Sexuality",6),rep("Gender",3,),rep("Product",19),rep("Politician",8),rep("Event",8),rep("Job",12),rep("University",9),rep("Movement",10),rep("Policy",10)),levels=c("Party","Race","Religion","Sexuality","Gender","Product","Politician","Event","Job","University","Movement","Policy"))
coding$agreement_party <- ifelse(coding$party_s==coding$party_j,1,0)
coding$agreement_race <- ifelse(coding$race_s==coding$race_j,1,0)
coding$agreement_gender <- ifelse(coding$gender_s==coding$gender_j,1,0)
coding.long <- data.frame(id=c(rep(1:100,3)),category=rep(coding$category,3),agreement=c(coding$agreement_party,coding$agreement_race,coding$agreement_gender),subject=factor(c(rep("Party",100),rep("Race",100),rep("Gender",100)),levels=c("Party","Race","Gender")))
dissection <- aov(agreement~(category*subject),data=coding.long)
## Coding: F(11,264)=3.255, p<.001
## Subject: F(2,264)=8.531, p<.001
## Interaction: F(22,264)=1.359, p=.134
TukeyHSD(dissection)$category # Politician-Religion (Diff=0.476, p=.007); Job-Politician (Diff=-.417, p=.008); University-Politician (Diff=-.444, p=.008)
TukeyHSD(dissection)$subject # Gender-Race (Diff=0.24, p<.001)
