####################################################################################################
# Replication Code for
# Bruder et al. "Cross-cultural awareness of and attitudes toward threatened animal species"
# Frontier in Psychology
# Code summary: Replicating Figure 2
# Code author: Taeyong Park (taeyongp@andrew.cmu.edu)
####################################################################################################


library(MASS); library(Amelia)

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


###########################
# MULTIPLE IMPUTATION
###########################
iv = c("vacationWild" , "thinkEnvironment" , "connectReligion" , 
       "noticeWildLife" , "natureImportant" , "connectedEarth" , 
       "protectAnimals" , "treatFairly" , "reportIllegal" , 
       "obeyAnimalLaw" , "disgustedGarbage" , "godWill" , "ableProtect" , 
       "age" , "education" , "female" , "howLongQatar", "nationGulf" , "nationOther" , 
       "nationMena" , "nationSouthAsia" , "nationEastAsia" , "nationAfrica")


dataR_FamiliarDugong = dataR[,c("familiar_Dugong", iv)]
dataR_FamiliarWhaleShark = dataR[,c("familiar_WhaleShark", iv)]
dataR_FamiliarTurtle = dataR[,c("familiar_Turtle", iv)]
dataR_FamiliarFalcon = dataR[,c("familiar_Falcon", iv)]
dataR_FamiliarAgama = dataR[,c("familiar_Agama", iv)]

dataR_LikeDugong = dataR[,c("like_Dugong", iv)]
dataR_LikeWhaleShark = dataR[,c("like_WhaleShark", iv)]
dataR_LikeTurtle = dataR[,c("like_Turtle", iv)]
dataR_LikeFalcon = dataR[,c("like_Falcon", iv)]
dataR_LikeAgama = dataR[,c("like_Agama", iv)]

dataR_GovProtectDugong = dataR[,c("govProtect_Dugong", iv)]
dataR_GovProtectWhaleShark = dataR[,c("govProtect_WhaleShark", iv)]
dataR_GovProtectTurtle = dataR[,c("govProtect_Turtle", iv)]
dataR_GovProtectFalcon = dataR[,c("govProtect_Falcon", iv)]
dataR_GovProtectAgama = dataR[,c("govProtect_Agama", iv)]


df = list(dataR_FamiliarDugong, dataR_FamiliarWhaleShark, dataR_FamiliarTurtle, dataR_FamiliarFalcon, dataR_FamiliarAgama,
          dataR_LikeDugong, dataR_LikeWhaleShark, dataR_LikeTurtle, dataR_LikeFalcon, dataR_LikeAgama,
          dataR_GovProtectDugong, dataR_GovProtectWhaleShark, dataR_GovProtectTurtle, dataR_GovProtectFalcon, dataR_GovProtectAgama)
dfName = c("dataR_FamiliarDugong", "dataR_FamiliarWhaleShark", "dataR_FamiliarTurtle", "dataR_FamiliarFalcon", "dataR_FamiliarAgama",
           "dataR_LikeDugong", "dataR_LikeWhaleShark", "dataR_LikeTurtle", "dataR_LikeFalcon", "dataR_LikeAgama",
           "dataR_GovProtectDugong", "dataR_GovProtectWhaleShark", "dataR_GovProtectTurtle", "dataR_GovProtectFalcon", "dataR_GovProtectAgama")

output = list()
for(i in 1:15){
  impute = df[[i]]
  if (length(which(is.na(impute[,1])))==0) {
    impute=impute
  } else {
    impute=impute[-(which(is.na(impute[,1]))),]
  }
  set.seed(1)
  imputed = amelia(impute, m=1)
  output[[i]] = imputed$imputations$imp1
}

for(i in 1:15){
  output[[i]]$nr6=(output[[i]]$vacationWild+ output[[i]]$thinkEnvironment+output[[i]]$connectReligion
                   +output[[i]]$noticeWildLife+output[[i]]$natureImportant+output[[i]]$connectedEarth)/6
}

dataR_FamiliarDugong = output[[1]]
dataR_FamiliarWhaleShark = output[[2]]
dataR_FamiliarTurtle = output[[3]]
dataR_FamiliarFalcon = output[[4]]
dataR_FamiliarAgama = output[[5]]

dataR_LikeDugong = output[[6]]
dataR_LikeWhaleShark = output[[7]]
dataR_LikeTurtle = output[[8]]
dataR_LikeFalcon = output[[9]]
dataR_LikeAgama = output[[10]]

dataR_GovProtectDugong = output[[11]]
dataR_GovProtectWhaleShark = output[[12]]
dataR_GovProtectTurtle = output[[13]]
dataR_GovProtectFalcon = output[[14]]
dataR_GovProtectAgama = output[[15]]


################
# familiar
################

# Dugong
modelD = lm(familiar_Dugong ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_FamiliarDugong)


# WhaleShark
modelW = lm(familiar_WhaleShark ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_FamiliarWhaleShark)


# Turtle
modelT = lm(familiar_Turtle ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_FamiliarTurtle)


# Falcon
modelF = lm(familiar_Falcon ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_FamiliarFalcon)


# Agama
modelA = lm(familiar_Agama ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_FamiliarAgama)


####################
# Plot the results
####################
ruler=5:1
nRuler=length(ruler)
color=c("black","blue","seagreen","purple","tomato")

tiff("Figure2A.tiff",
     width=900, height=340)
par(mfrow=c(1,5), 
    mar=c(0, .5, 1.5, .5), 
    oma = c(2, 2, 0, 1), cex=1.1)
plot(x=coef(modelD)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="Dugong",
     xlim=c(min(c(0,min(confint(modelD)[14:18,][,1]))), 
            max(c(0,max(confint(modelD)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelD)[14:18,][i,1],
           x1=confint(modelD)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelD)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelD)[14:18,][,2])))*1.1, 1),
                by=0.3))
axis(2, at=ruler, labels=c("Gulf","Mena","Africa","S.Asia","S.E.Asia"))
abline(v=0, lty=2, col="red")

plot(x=coef(modelW)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="Whale Shark",
     xlim=c(min(c(0,min(confint(modelW)[14:18,][,1]))), 
            max(c(0,max(confint(modelW)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelW)[14:18,][i,1],
           x1=confint(modelW)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelW)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelW)[14:18,][,2])))*1.1, 1),
                by=0.2))

abline(v=0, lty=2, col="red")

plot(x=coef(modelT)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="H.S. Turtle",
     xlim=c(min(c(0,min(confint(modelT)[14:18,][,1]))), 
            max(c(0,max(confint(modelT)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelT)[14:18,][i,1],
           x1=confint(modelT)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelT)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelT)[14:18,][,2])))*1.1, 1),
                by=0.2))

abline(v=0, lty=2, col="red")

plot(x=coef(modelF)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="S. Falcon",
     xlim=c(min(c(0,min(confint(modelF)[14:18,][,1]))), 
            max(c(0,max(c(0,max(c(0,max(confint(modelF)[14:18,][,2])))))))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelF)[14:18,][i,1],
           x1=confint(modelF)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelF)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(c(0,max(c(0,max(confint(modelF)[14:18,][,2])))))))*1.1, 1),
                by=0.2))

abline(v=0, lty=2, col="red")

plot(x=coef(modelA)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="S. T. Agama",
     xlim=c(min(c(0,min(confint(modelA)[14:18,][,1]))), 
            max(c(0,max(confint(modelA)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelA)[14:18,][i,1],
           x1=confint(modelA)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelA)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelA)[14:18,][,2])))*1.1, 1),
                by=0.5))

abline(v=0, lty=2, col="red")
dev.off()

################
# like
################

# Dugong
modelD = lm(like_Dugong ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_LikeDugong)


# WhaleShark
modelW = lm(like_WhaleShark ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_LikeWhaleShark)


# Turtle
modelT = lm(like_Turtle ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_LikeTurtle)


# Falcon
modelF = lm(like_Falcon ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_LikeFalcon)


# Agama
modelA = lm(like_Agama ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_LikeAgama)

####################
# Plot the results
####################
ruler=5:1
nRuler=length(ruler)

tiff("Figure2B.tiff",
     width=900, height=340.5)
par(mfrow=c(1,5), 
    mar=c(0, .5, 1.5, .5), 
    oma = c(2, 2, 0, 1), cex=1.1)
plot(x=coef(modelD)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="Dugong",
     xlim=c(min(c(0,min(confint(modelD)[14:18,][,1]))), 
            max(c(0,max(confint(modelD)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelD)[14:18,][i,1],
           x1=confint(modelD)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelD)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelD)[14:18,][,2])))*1.1, 1),
                by=0.4))
axis(2, at=ruler, labels=c("Gulf","Mena","Africa","S.Asia","S.E.Asia"))
abline(v=0, lty=2, col="red")

plot(x=coef(modelW)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="Whale Shark",
     xlim=c(min(c(0,min(confint(modelW)[14:18,][,1]))), 
            max(c(0,max(confint(modelW)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelW)[14:18,][i,1],
           x1=confint(modelW)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelW)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelW)[14:18,][,2])))*1.1, 1),
                by=0.4))

abline(v=0, lty=2, col="red")

plot(x=coef(modelT)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="H.S. Turtle",
     xlim=c(min(c(0,min(confint(modelT)[14:18,][,1]))), 
            max(c(0,max(confint(modelT)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelT)[14:18,][i,1],
           x1=confint(modelT)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelT)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelT)[14:18,][,2])))*1.1, 1),
                by=0.3))

abline(v=0, lty=2, col="red")

plot(x=coef(modelF)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="S. Falcon",
     xlim=c(min(c(0,min(confint(modelF)[14:18,][,1]))), 
            max(c(0,max(c(0,max(confint(modelF)[14:18,][,2])))))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelF)[14:18,][i,1],
           x1=confint(modelF)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelF)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(c(0,max(confint(modelF)[14:18,][,2])))))*1.1, 1),
                by=0.3))

abline(v=0, lty=2, col="red")

plot(x=coef(modelA)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="S. T. Agama",
     xlim=c(min(c(0,min(confint(modelA)[14:18,][,1]))), 
            max(c(0,max(c(0,max(confint(modelA)[14:18,][,2])))))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelA)[14:18,][i,1],
           x1=confint(modelA)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelA)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelA)[14:18,][,2])))*1.1, 1),
                by=0.4))

abline(v=0, lty=2, col="red")
dev.off()

################
# govProtect
################

# Dugong
modelD = lm(govProtect_Dugong ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_GovProtectDugong)


# WhaleShark
modelW = lm(govProtect_WhaleShark ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_GovProtectWhaleShark)


# Turtle
modelT = lm(govProtect_Turtle ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_GovProtectTurtle)


# Falcon
modelF = lm(govProtect_Falcon ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_GovProtectFalcon)


# Agama
modelA = lm(govProtect_Agama ~ nr6
            + protectAnimals + treatFairly + reportIllegal
            + obeyAnimalLaw + disgustedGarbage + godWill + ableProtect
            + age + education + female + howLongQatar 
            + nationGulf + nationMena + nationAfrica 
            + nationSouthAsia + nationEastAsia + nationOther, data = dataR_GovProtectAgama)


####################
# Plot the results
####################
ruler=5:1
nRuler=length(ruler)

tiff("Figure2C.tiff",
     width=900, height=340.5)
par(mfrow=c(1,5), 
    mar=c(0, .5, 1.5, .5), 
    oma = c(2, 2, 0, 1), cex=1.1)
plot(x=coef(modelD)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="Dugong",
     xlim=c(min(c(0,min(confint(modelD)[14:18,][,1]))), 
            max(c(0,max(confint(modelD)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelD)[14:18,][i,1],
           x1=confint(modelD)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelD)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelD)[14:18,][,2])))*1.1, 1),
                by=0.2))
axis(2, at=ruler, labels=c("Gulf","Mena","Africa","S.Asia","S.E.Asia"))
abline(v=0, lty=2, col="red")

plot(x=coef(modelW)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="Whale Shark",
     xlim=c(min(c(0,min(confint(modelW)[14:18,][,1]))), 
            max(c(0,max(confint(modelW)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelW)[14:18,][i,1],
           x1=confint(modelW)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelW)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelW)[14:18,][,2])))*1.1, 1),
                by=0.2))

abline(v=0, lty=2, col="red")

plot(x=coef(modelT)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="H.S. Turtle",
     xlim=c(min(c(0,min(confint(modelT)[14:18,][,1]))), 
            max(c(0,max(confint(modelT)[14:18,][,2])))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelT)[14:18,][i,1],
           x1=confint(modelT)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelT)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelT)[14:18,][,2])))*1.1, 1),
                by=0.2))

abline(v=0, lty=2, col="red")

plot(x=coef(modelF)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="S. Falcon",
     xlim=c(min(c(0,min(confint(modelF)[14:18,][,1]))), 
            max(c(0,max(c(0,max(confint(modelF)[14:18,][,2])))))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelF)[14:18,][i,1],
           x1=confint(modelF)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelF)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(c(0,max(confint(modelF)[14:18,][,2])))))*1.1, 1),
                by=0.2))

abline(v=0, lty=2, col="red")

plot(x=coef(modelA)[14:18], y=ruler, 
     xlab="",
     ylab="",
     main="S. T. Agama",
     xlim=c(min(c(0,min(confint(modelA)[14:18,][,1]))), 
            max(c(0,max(c(0,max(confint(modelA)[14:18,][,2])))))), 
     ylim=c(0.75, 5.25),
     pch=c(15:17, 8, 9, 13), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=confint(modelA)[14:18,][i,1],
           x1=confint(modelA)[14:18,][i,2],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=round(min(c(0,min(confint(modelA)[14:18,][,1])))*1.1, 1), 
                to=round(max(c(0,max(confint(modelA)[14:18,][,2])))*1.1, 1),
                by=0.4))

abline(v=0, lty=2, col="red")
dev.off()
