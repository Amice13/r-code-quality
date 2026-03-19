####################################################################################################
# Replication Code for
# Bruder et al. "Cross-cultural awareness of and attitudes toward threatened animal species"
# Frontier in Psychology
# Code summary: Replicating Figures 5 and S3
# Code author: Taeyong Park (taeyongp@andrew.cmu.edu)
####################################################################################################


library(MASS); library(mediation); library(Amelia)

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
# Mediation analysis
#
##############################

# Dugong

b = lm(like_Dugong ~ familiar_Dugong + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRDugong)
c = lm(govProtect_Dugong ~ like_Dugong + familiar_Dugong + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRDugong)


modelD_1 = mediate(b, c, boot=TRUE, sims=500, treat="familiar_Dugong", mediator="like_Dugong")
summary(modelD_1)


# WhaleShark

b = lm(like_WhaleShark ~ familiar_WhaleShark + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRWhaleShark)
c = lm(govProtect_WhaleShark ~ like_WhaleShark + familiar_WhaleShark + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRWhaleShark)


modelW_1 = mediate(b, c, boot=TRUE, sims=500, treat="familiar_WhaleShark", mediator="like_WhaleShark")
summary(modelW_1)


# Turtle

b = lm(like_Turtle ~ familiar_Turtle + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRTurtle)
c = lm(govProtect_Turtle ~ like_Turtle + familiar_Turtle + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRTurtle)


modelT_1 = mediate(b, c, boot=TRUE, sims=500, treat="familiar_Turtle", mediator="like_Turtle")
summary(modelT_1)


# Falcon

b = lm(like_Falcon ~ familiar_Falcon + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRFalcon)
c = lm(govProtect_Falcon ~ like_Falcon + familiar_Falcon + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRFalcon)


modelF_1 = mediate(b, c, boot=TRUE, sims=500, treat="familiar_Falcon", mediator="like_Falcon")
summary(modelF_1)


# Agama

b = lm(like_Agama ~ familiar_Agama + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRAgama)
c = lm(govProtect_Agama ~ like_Agama + familiar_Agama + nr6
       + protectAnimals + treatFairly + reportIllegal
       + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
       + age + education + female + howLongQatar 
       + nationGulf + nationMena + nationAfrica 
       + nationEastAsia + nationSouthAsia + nationOther, data = dataRAgama)


modelA_1 = mediate(b, c, boot=TRUE, sims=500, treat="familiar_Agama", mediator="like_Agama")
summary(modelA_1)


####################
#
# FIGURE 5
#
####################
ruler=3:1
nRuler=length(ruler)
color=c("black","blue","seagreen")

coefD_1=c(modelD_1$d.avg,modelD_1$z.avg,modelD_1$tau.coef)
lowerD_1=c(modelD_1$d0.ci[1],modelD_1$z0.ci[1],modelD_1$tau.ci[1])
upperD_1=c(modelD_1$d0.ci[2],modelD_1$z0.ci[2],modelD_1$tau.ci[2])

coefW_1=c(modelW_1$d.avg,modelW_1$z.avg,modelW_1$tau.coef)
lowerW_1=c(modelW_1$d0.ci[1],modelW_1$z0.ci[1],modelW_1$tau.ci[1])
upperW_1=c(modelW_1$d0.ci[2],modelW_1$z0.ci[2],modelW_1$tau.ci[2])

coefT_1=c(modelT_1$d.avg,modelT_1$z.avg,modelT_1$tau.coef)
lowerT_1=c(modelT_1$d0.ci[1],modelT_1$z0.ci[1],modelT_1$tau.ci[1])
upperT_1=c(modelT_1$d0.ci[2],modelT_1$z0.ci[2],modelT_1$tau.ci[2])

coefF_1=c(modelF_1$d.avg,modelF_1$z.avg,modelF_1$tau.coef)
lowerF_1=c(modelF_1$d0.ci[1],modelF_1$z0.ci[1],modelF_1$tau.ci[1])
upperF_1=c(modelF_1$d0.ci[2],modelF_1$z0.ci[2],modelF_1$tau.ci[2])

coefA_1=c(modelA_1$d.avg,modelA_1$z.avg,modelA_1$tau.coef)
lowerA_1=c(modelA_1$d0.ci[1],modelA_1$z0.ci[1],modelA_1$tau.ci[1])
upperA_1=c(modelA_1$d0.ci[2],modelA_1$z0.ci[2],modelA_1$tau.ci[2])


tiff("Figure5.tiff",
     width=900, height=300)
par(mfrow=c(1,5), 
    mar=c(0, .5, 1.5, .5), 
    oma = c(2, 2, 0, 1), cex=1.3)
plot(x=coefD_1, y=ruler, 
     xlab="",
     ylab="",
     main="Dugong",
     xlim=c(min(c(0,min(lowerD_1))), 
            max(c(0,max(upperD_1)))), 
     ylim=c(0.75, 3.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerD_1[i],
           x1=upperD_1[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerD_1)))*1.1, 1), 
                to=round(max(c(0,max(upperD_1)))*1.1, 1),
                by=0.05))
axis(2, at=ruler, labels=c("Mediation","Direct","Total"))
abline(v=0, lty=2, col="red")

plot(x=coefW_1, y=ruler, 
     xlab="",
     ylab="",
     main="Whale Shark",
     xlim=c(min(c(0,min(lowerW_1))), 
            max(c(0,max(upperW_1)))), 
     ylim=c(0.75, 3.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerW_1[i],
           x1=upperW_1[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerW_1)))*1.1, 1), 
                to=round(max(c(0,max(upperW_1)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")

plot(x=coefT_1, y=ruler, 
     xlab="",
     ylab="",
     main="H.S. Turtle",
     xlim=c(min(c(0,min(lowerT_1))), 
            max(c(0,max(upperT_1)))), 
     ylim=c(0.75, 3.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerT_1[i],
           x1=upperT_1[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerT_1)))*1.1, 1), 
                to=round(max(c(0,max(upperT_1)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")

plot(x=coefF_1, y=ruler, 
     xlab="",
     ylab="",
     main="S. Falcon",
     xlim=c(min(c(0,min(lowerF_1))), 
            max(c(0,max(upperF_1)))), 
     ylim=c(0.75, 3.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerF_1[i],
           x1=upperF_1[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerF_1)))*1.1, 1), 
                to=round(max(c(0,max(upperF_1)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")

plot(x=coefA_1, y=ruler, 
     xlab="",
     ylab="",
     main="S. T. Agama",
     xlim=c(min(c(0,min(lowerA_1))), 
            max(c(0,max(upperA_1)))), 
     ylim=c(0.75, 3.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerA_1[i],
           x1=upperA_1[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(lowerA_1)))*1.1, 1), 
                to=round(max(c(0,max(upperA_1)))*1.1, 1),
                by=0.05))
abline(v=0, lty=2, col="red")
dev.off()


########################################
#
# FIGURE S3: senstivity analysis
#
########################################
sensD_1 = medsens(modelD_1, rho.by=.1, effect.type="indirect")
sensW_1 = medsens(modelW_1, rho.by=.1, effect.type="indirect")
sensT_1 = medsens(modelT_1, rho.by=.1, effect.type="indirect")
sensF_1 = medsens(modelF_1, rho.by=.1, effect.type="indirect")
sensA_1 = medsens(modelA_1, rho.by=.1, effect.type="indirect")


tiff("FigureS3.tiff",
     width=900, height=500)
par(mfrow=c(2,3), 
    mar=c(3.5, 3.5, 1.5, 1), 
    oma = c(1, 1, 1, 1), cex=.9)
plot(sensD_1, main="Dugong", ylim=c(-.3, .3))
plot(sensW_1, main="Whale Shark", ylim=c(-.3, .3))
plot(sensT_1, main="H.S. Turtle", ylim=c(-.3, .3))
plot(sensF_1, main="S. Falcon", ylim=c(-.3, .3))
plot(sensA_1, main="S. T. Agama", ylim=c(-.3, .3))
dev.off()

