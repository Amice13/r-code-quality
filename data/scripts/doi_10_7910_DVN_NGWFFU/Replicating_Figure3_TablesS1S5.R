####################################################################################################
# Replication Code for
# Bruder et al. "Cross-cultural awareness of and attitudes toward threatened animal species"
# Frontier in Psychology
# Code summary: Replicating Figure 3 and Tables S1-S5
# Code author: Taeyong Park (taeyongp@andrew.cmu.edu)
####################################################################################################

library(MASS); library(Amelia); library(apsrtable)

rm(list=ls())

dataRef = read.csv("survey.csv")
data = dataRef[-(1:2),]

colnames(data)[c(5,11:49)]=c(
  "DurationSec",
  "consent_above18",
  "consent_residingQatar",
  "consent_literacy",
  "consent_understandInf",
  "consent_wantToParticipate",
  "vacationWild",
  "thinkEnvironment",
  "connectReligion",
  "noticeWildLife",
  "natureImportant",
  "connectedEarth",
  "protectAnimals",
  "treatFairly",
  "reportIllegal",
  "obeyAnimalLaw",
  "disgustedGarbage",
  "godWill",
  "ableProtect",
  "familiar_Dugong",
  "like_Dugong",
  "govProtect_Dugong",
  "familiar_WhaleShark",
  "like_WhaleShark",
  "govProtect_WhaleShark",
  "familiar_Turtle",
  "like_Turtle",
  "govProtect_Turtle",
  "familiar_Agama",
  "like_Agama",
  "govProtect_Agama",
  "familiar_Falcon",
  "like_Falcon",
  "govProtect_Falcon",
  "age",
  "gender",
  "nationality",
  "religion",
  "education",
  "howLongQatar"
)

###################
#
# DATA CLEAN-UP
#
###################

data$familiar_Dugong[data$familiar_Dugong==5]="4"
data$familiar_WhaleShark[data$familiar_WhaleShark==8]="4"
data$familiar_Turtle[data$familiar_Turtle==6]="4"

dataR = data.frame(apply(data[,16:49], 2, FUN=as.numeric))

###################
# GENDER
###################
dataR$female = rep(0, nrow(dataR))
dataR$female[dataR$gender==1]=1
dataR$female[which(is.na(dataR$gender))] = NA

dataR$male = rep(0, nrow(dataR))
dataR$male[dataR$gender==2]=1
dataR$male[which(is.na(dataR$gender))] = NA



###################
# NATIONALITY
###################
# WEIRD: Western, educated, industrialized, rich, and democratic societies
dataR$nationWeird = rep(0, nrow(dataR))
dataR$nationWeird[dataR$nationality==4 # American
                  | dataR$nationality==10 # Australian
                  | dataR$nationality==11 # Austrian 
                  | dataR$nationality==26 # British
                  | dataR$nationality==35 # Canadian 
                  | dataR$nationality==64 # Dutch
                  | dataR$nationality==69 # English 
                  | dataR$nationality==78 # French
                  | dataR$nationality==82 # German
                  | dataR$nationality==101 # Irish
                  | dataR$nationality==103 # Italian 
                  | dataR$nationality==147 # New Zealander
                  | dataR$nationality==154 # Norwegian 
                  | dataR$nationality==165 # Portuguese
                  | dataR$nationality==177 # Scottish 
                  | dataR$nationality==189 # Spanish
                  | dataR$nationality==198 # Swiss
                  | dataR$nationality==222 # Welsh
] = 1
dataR$nationWeird[which(is.na(dataR$nationality))] = NA


dataR$nationGulf = rep(0, nrow(dataR))
dataR$nationGulf[dataR$nationality==14 # Bahraini
                 | dataR$nationality==68 # Emirati
                 | dataR$nationality==112 # Kuwaiti 
                 | dataR$nationality==155 # Omani
                 | dataR$nationality==168 # Qatari 
                 | dataR$nationality==176 # Saudi
] = 1
dataR$nationGulf[which(is.na(dataR$nationality))] = NA

# MENA excluding GCC
dataR$nationMena = rep(0, nrow(dataR))
dataR$nationMena[dataR$nationality==3 # Algerian
                 | dataR$nationality==67 # Egyptian 
                 | dataR$nationality==99 # Iranian
                 | dataR$nationality==100 # Iraqi 
                 | dataR$nationality==107 # Jordanian
                 | dataR$nationality==116 # Lebanese
                 | dataR$nationality==118 # Libyan
                 | dataR$nationality==141 # Moroccan
                 | dataR$nationality==158 # Palestinian
                 | dataR$nationality==194 # Sudanese
                 | dataR$nationality==199 # Syrian
                 | dataR$nationality==208 # Tunisian
                 | dataR$nationality==209 # Turkish 
                 | dataR$nationality==223 # Yemeni
] = 1
dataR$nationMena[which(is.na(dataR$nationality))] = NA

dataR$nationSouthAsia = rep(0, nrow(dataR))
dataR$nationSouthAsia[dataR$nationality==1 # Afghan
                      | dataR$nationality==15 # Bagladeshi
                      | dataR$nationality==97 # Indian
                      | dataR$nationality==146 # Nepalese
                      | dataR$nationality==156 # Pakistani
                      | dataR$nationality==190 # Sri Lankan
] = 1
dataR$nationSouthAsia[which(is.na(dataR$nationality))] = NA

dataR$nationEastAsia = rep(0, nrow(dataR))
dataR$nationEastAsia[dataR$nationality==31 # Burmese
                     | dataR$nationality==76 # Filipino
                     | dataR$nationality==98 # Indonesian
                     | dataR$nationality==126 # Malaysian
                     | dataR$nationality==181 # Singaporean
] = 1
dataR$nationEastAsia[which(is.na(dataR$nationality))] = NA

# Africa excluding MENA
dataR$nationAfrica = rep(0, nrow(dataR))
dataR$nationAfrica[dataR$nationality==30 # Burkinan
                   | dataR$nationality==36 # Cape Verdean
                   | dataR$nationality==71 # Eritrean
                   | dataR$nationality==73 # Ethiopian
                   | dataR$nationality==83 # Ghanaian
                   | dataR$nationality==109 # Kenyan
                   | dataR$nationality==132 # Mauritanian
                   | dataR$nationality==133 # Mauritian
                   | dataR$nationality==143 # Mozambican
                   | dataR$nationality==149 # Nigerian
                   | dataR$nationality==185 # Somali
                   | dataR$nationality==186 # South African
                   | dataR$nationality==202 # Tanzanian
                   | dataR$nationality==213 # Ugandan
                   | dataR$nationality==225 # Zimbabwean
] = 1
dataR$nationAfrica[which(is.na(dataR$nationality))] = NA

dataR = dataR[-which(is.na(dataR$nationality)),]

# The number of observations belonging to Other
other = unique(dataR$nationality[dataR$nationAfrica==0 & dataR$nationEastAsia==0
                                 & dataR$nationGulf==0 & dataR$nationMena==0
                                 & dataR$nationWeird==0 & dataR$nationSouthAsia==0])

result = NULL
for(i in 1:length(other)){
  result[i] = length(which(dataR$nationality==other[i]))
}
cbind(other, result)

dataR$nationOther = rep(0, nrow(dataR))
dataR$nationOther[dataR$nationAfrica==0 & dataR$nationEastAsia==0 & dataR$nationGulf==0 & dataR$nationMena==0 & dataR$nationWeird==0 & dataR$nationSouthAsia==0] = 1



dataRDugong = dataR[-which(is.na(dataR$govProtect_Dugong)),]
dataRWhaleShark = dataR[-which(is.na(dataR$govProtect_WhaleShark)),]
dataRTurtle = dataR[-which(is.na(dataR$govProtect_Turtle)),]
dataRFalcon = dataR[-which(is.na(dataR$govProtect_Falcon)),]
dataRAgama = dataR[-which(is.na(dataR$govProtect_Agama)),]


dataRDugong$nr6=(dataRDugong$vacationWild+ dataRDugong$thinkEnvironment+dataRDugong$connectReligion
                 +dataRDugong$noticeWildLife+dataRDugong$natureImportant+dataRDugong$connectedEarth)/6
dataRWhaleShark$nr6=(dataRWhaleShark$vacationWild+ dataRWhaleShark$thinkEnvironment+dataRWhaleShark$connectReligion
                     +dataRWhaleShark$noticeWildLife+dataRWhaleShark$natureImportant+dataRWhaleShark$connectedEarth)/6
dataRTurtle$nr6=(dataRTurtle$vacationWild+ dataRTurtle$thinkEnvironment+dataRTurtle$connectReligion
                 +dataRTurtle$noticeWildLife+dataRTurtle$natureImportant+dataRTurtle$connectedEarth)/6
dataRFalcon$nr6=(dataRFalcon$vacationWild+ dataRFalcon$thinkEnvironment+dataRFalcon$connectReligion
                 +dataRFalcon$noticeWildLife+dataRFalcon$natureImportant+dataRFalcon$connectedEarth)/6
dataRAgama$nr6=(dataRAgama$vacationWild+ dataRAgama$thinkEnvironment+dataRAgama$connectReligion
                +dataRAgama$noticeWildLife+dataRAgama$natureImportant+dataRAgama$connectedEarth)/6



##############################
#
#  overall data
#
##############################

# Dugong
modelD_1 = lm(govProtect_Dugong ~ familiar_Dugong + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRDugong)
modelD_2 = lm(govProtect_Dugong ~ like_Dugong + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRDugong)
modelD_3 = lm(govProtect_Dugong ~ familiar_Dugong + like_Dugong + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRDugong)

# WhaleShark
modelW_1 = lm(govProtect_WhaleShark ~ familiar_WhaleShark + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRWhaleShark)
modelW_2 = lm(govProtect_WhaleShark ~ like_WhaleShark + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRWhaleShark)
modelW_3 = lm(govProtect_WhaleShark ~ familiar_WhaleShark + like_WhaleShark + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRWhaleShark)

# Turtle
modelT_1 = lm(govProtect_Turtle ~ familiar_Turtle + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRTurtle)
modelT_2 = lm(govProtect_Turtle ~ like_Turtle + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRTurtle)
modelT_3 = lm(govProtect_Turtle ~ familiar_Turtle + like_Turtle + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRTurtle)

# Falcon
modelF_1 = lm(govProtect_Falcon ~ familiar_Falcon + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRFalcon)
modelF_2 = lm(govProtect_Falcon ~ like_Falcon + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRFalcon)
modelF_3 = lm(govProtect_Falcon ~ familiar_Falcon + like_Falcon + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRFalcon)

# Agama
modelA_1 = lm(govProtect_Agama ~ familiar_Agama + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRAgama)
modelA_2 = lm(govProtect_Agama ~ like_Agama + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRAgama)
modelA_3 = lm(govProtect_Agama ~ familiar_Agama + like_Agama + nr6
              + protectAnimals + treatFairly + reportIllegal
              + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
              + age + education + female + howLongQatar 
              + nationGulf + nationMena + nationAfrica 
              + nationEastAsia + nationSouthAsia + nationOther, data = dataRAgama)


####################
# Plot the results
####################
ruler=4:1
nRuler=length(ruler)
color=c("black","blue","seagreen","seagreen")

coefDugong = c(coef(modelD_1)[2], coef(modelD_2)[2], 
               coef(modelD_3)[2:3])
lowerDugong = c(confint(modelD_1)[2,1], confint(modelD_2)[2,1],
                confint(modelD_3)[2:3,1])
upperDugong = c(confint(modelD_1)[2,2], confint(modelD_2)[2,2],
                confint(modelD_3)[2:3,2])

coefWhaleShark = c(coef(modelW_1)[2], coef(modelW_2)[2], 
                   coef(modelW_3)[2:3])
lowerWhaleShark = c(confint(modelW_1)[2,1], confint(modelW_2)[2,1],
                    confint(modelW_3)[2:3,1])
upperWhaleShark = c(confint(modelW_1)[2,2], confint(modelW_2)[2,2],
                    confint(modelW_3)[2:3,2])

coefTurtle = c(coef(modelT_1)[2], coef(modelT_2)[2], 
               coef(modelT_3)[2:3])
lowerTurtle = c(confint(modelT_1)[2,1], confint(modelT_2)[2,1],
                confint(modelT_3)[2:3,1])
upperTurtle = c(confint(modelT_1)[2,2], confint(modelT_2)[2,2],
                confint(modelT_3)[2:3,2])

coefFalcon = c(coef(modelF_1)[2], coef(modelF_2)[2], 
               coef(modelF_3)[2:3])
lowerFalcon = c(confint(modelF_1)[2,1], confint(modelF_2)[2,1],
                confint(modelF_3)[2:3,1])
upperFalcon = c(confint(modelF_1)[2,2], confint(modelF_2)[2,2],
                confint(modelF_3)[2:3,2])

coefAgama = c(coef(modelA_1)[2], coef(modelA_2)[2], 
              coef(modelA_3)[2:3])
lowerAgama = c(confint(modelA_1)[2,1], confint(modelA_2)[2,1],
               confint(modelA_3)[2:3,1])
upperAgama = c(confint(modelA_1)[2,2], confint(modelA_2)[2,2],
               confint(modelA_3)[2:3,2])


tiff("Figure3.tiff",
     width=900, height=450)
par(mfrow=c(2,3), 
    mar=c(1.5, 1, 1.5, .5), 
    oma = c(2, 4.5, .5, 0), cex=1.2)
plot(x=coefDugong, y=ruler, 
     xlab="",
     ylab="",
     main="Dugong",
     xlim=c(min(c(0,min(lowerDugong))), 
            max(c(0,max(upperDugong)))), 
     ylim=c(0.75, 4.25),
     pch=c(15,16), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerDugong[i],
           x1=upperDugong[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerDugong)))*1.1, 1), 
                to=round(max(c(0,max(upperDugong)))*1.1, 1),
                by=0.05))
axis(2, at=ruler, 
     labels=c("Familiarity \n Model1","Liking \n Model2","Familiarity \n Model3","Liking \n Model3"),
     las=2)
abline(v=0, lty=2, col="red")

plot(x=coefWhaleShark, y=ruler, 
     xlab="",
     ylab="",
     main="Whale Shark",
     xlim=c(min(c(0,min(lowerWhaleShark))), 
            max(c(0,max(upperWhaleShark)))), 
     ylim=c(0.75, 4.25),
     pch=c(15,16), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerWhaleShark[i],
           x1=upperWhaleShark[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerWhaleShark)))*1.1, 1), 
                to=round(max(c(0,max(upperWhaleShark)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")

plot(x=coefTurtle, y=ruler, 
     xlab="",
     ylab="",
     main="H.S. Turtle",
     xlim=c(min(c(0,min(lowerTurtle))), 
            max(c(0,max(upperTurtle)))), 
     ylim=c(0.75, 4.25),
     pch=c(15,16), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerTurtle[i],
           x1=upperTurtle[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerTurtle)))*1.1, 1), 
                to=round(max(c(0,max(upperTurtle)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")

plot(x=coefFalcon, y=ruler, 
     xlab="",
     ylab="",
     main="S. Falcon",
     xlim=c(min(c(0,min(lowerFalcon))), 
            max(c(0,max(upperFalcon)))), 
     ylim=c(0.75, 4.25),
     pch=c(15,16), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerFalcon[i],
           x1=upperFalcon[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerFalcon)))*1.1, 1), 
                to=round(max(c(0,max(upperFalcon)))*1.1, 1),
                by=0.05))
axis(2, at=ruler, 
     labels=c("Familiarity \n Model1","Liking \n Model2","Familiarity \n Model3","Liking \n Model3"),
     las=2)
abline(v=0, lty=2, col="red")

plot(x=coefAgama, y=ruler, 
     xlab="",
     ylab="",
     main="S. T. Agama",
     xlim=c(min(c(0,min(lowerAgama))), 
            max(c(0,max(upperAgama)))), 
     ylim=c(0.75, 4.25),
     pch=c(15,16), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerAgama[i],
           x1=upperAgama[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerAgama)))*1.1, 1), 
                to=round(max(c(0,max(upperAgama)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")

plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Model1 with only the Familiarity variable. \n', 
                           'Model2 with only the Liking variable. \n',  
                           'Model3 with both the Liking',
                           'and Familiarity variables. \n',
                           'All models include control variables.'), bty='n', cex=.9)

dev.off()

####################
# Tables S1-S5
####################
apsrtable(modelD_1, modelD_2, modelD_3)
apsrtable(modelW_1, modelW_2, modelW_3)
apsrtable(modelT_1, modelT_2, modelT_3)
apsrtable(modelF_1, modelF_2, modelF_3)
apsrtable(modelA_1, modelA_2, modelA_3)
