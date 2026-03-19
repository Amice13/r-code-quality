####################################################################################################
# Replication Code for
# Bruder et al. "Cross-cultural awareness of and attitudes toward threatened animal species"
# Frontier in Psychology
# Code summary: Replicating Tables 1-2; Figure 1; Figure S1 
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

# The number of observations belonging to Other, a.k.a Mixed
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


#######################################
#
# REPLICATING Table 1
#
#######################################
table(dataR$gender)
round(prop.table(table(dataR$gender))*100,2)

table(dataR$age)
round(prop.table(table(dataR$age))*100,2)

table(dataR$education)
round(prop.table(table(dataR$education))*100,2)

table(dataR$howLongQatar)
round(prop.table(table(dataR$howLongQatar))*100,2)


#######################################
#
# REPLICATING Table 2
#
#######################################
# age
round(median(na.omit(dataR$age[dataR$nationWeird==1])), 2)
round(median(na.omit(dataR$age[dataR$nationGulf==1])), 2)
round(median(na.omit(dataR$age[dataR$nationMena==1])), 2)
round(median(na.omit(dataR$age[dataR$nationAfrica==1])), 2)
round(median(na.omit(dataR$age[dataR$nationEastAsia==1])), 2)
round(median(na.omit(dataR$age[dataR$nationSouthAsia==1])), 2)
round(median(na.omit(dataR$age[dataR$nationOther==1])), 2)

# female
round(table(dataR$female[dataR$nationWeird==1])[2]/sum(table(dataR$female[dataR$nationWeird==1])),2)
round(table(dataR$female[dataR$nationGulf==1])[2]/sum(table(dataR$female[dataR$nationGulf==1])),2)
round(table(dataR$female[dataR$nationMena==1])[2]/sum(table(dataR$female[dataR$nationMena==1])),2)
round(table(dataR$female[dataR$nationAfrica==1])[2]/sum(table(dataR$female[dataR$nationAfrica==1])),2)
round(table(dataR$female[dataR$nationEastAsia==1])[2]/sum(table(dataR$female[dataR$nationEastAsia==1])),2)
round(table(dataR$female[dataR$nationSouthAsia==1])[2]/sum(table(dataR$female[dataR$nationSouthAsia==1])),2)
round(table(dataR$female[dataR$nationOther==1])[2]/sum(table(dataR$female[dataR$nationOther==1])),2)

# education
round(median(na.omit(dataR$education[dataR$nationWeird==1])), 2)
round(median(na.omit(dataR$education[dataR$nationGulf==1])), 2)
round(median(na.omit(dataR$education[dataR$nationMena==1])), 2)
round(median(na.omit(dataR$education[dataR$nationAfrica==1])), 2)
round(median(na.omit(dataR$education[dataR$nationEastAsia==1])), 2)
round(median(na.omit(dataR$education[dataR$nationSouthAsia==1])), 2)
round(median(na.omit(dataR$education[dataR$nationOther==1])), 2)

# howLongQatar
round(median(na.omit(dataR$howLongQatar[dataR$nationWeird==1])), 2)
round(median(na.omit(dataR$howLongQatar[dataR$nationGulf==1])), 2)
round(median(na.omit(dataR$howLongQatar[dataR$nationMena==1])), 2)
round(median(na.omit(dataR$howLongQatar[dataR$nationAfrica==1])), 2)
round(median(na.omit(dataR$howLongQatar[dataR$nationEastAsia==1])), 2)
round(median(na.omit(dataR$howLongQatar[dataR$nationSouthAsia==1])), 2)
round(median(na.omit(dataR$howLongQatar[dataR$nationOther==1])), 2)


#######################################
#
# REPLICATING Figure 1 and Figure S1
#
#######################################

###########################
# MULTIPLE IMPUTATION
###########################
iv = c("vacationWild" , "thinkEnvironment" , "connectReligion" , 
       "noticeWildLife" , "natureImportant" , "connectedEarth" , 
       "protectAnimals" , "treatFairly" , "reportIllegal" , 
       "obeyAnimalLaw" , "disgustedGarbage" , "godWill" , "ableProtect" , 
       "age" , "education" , "female" , "nationGulf" , "nationOther" , 
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


####################################
# Order in Liking by nationality
####################################
## Mean ##
# Weird
meanDugongWeird = mean(na.omit(dataR$like_Dugong[dataR$nationWeird==1]))
meanWhaleSharkWeird = mean(na.omit(dataR$like_WhaleShark[dataR$nationWeird==1]))
meanTurtleWeird = mean(na.omit(dataR$like_Turtle[dataR$nationWeird==1]))
meanFalconWeird = mean(na.omit(dataR$like_Falcon[dataR$nationWeird==1]))
meanAgamaWeird = mean(na.omit(dataR$like_Agama[dataR$nationWeird==1]))

meanWeirdSorted = sort(c(meanDugongWeird,
                         meanWhaleSharkWeird,
                         meanTurtleWeird,
                         meanFalconWeird,
                         meanAgamaWeird), decreasing=T)
# Gulf
meanDugongGulf = mean(na.omit(dataR$like_Dugong[dataR$nationGulf==1]))
meanWhaleSharkGulf = mean(na.omit(dataR$like_WhaleShark[dataR$nationGulf==1]))
meanTurtleGulf = mean(na.omit(dataR$like_Turtle[dataR$nationGulf==1]))
meanFalconGulf = mean(na.omit(dataR$like_Falcon[dataR$nationGulf==1]))
meanAgamaGulf = mean(na.omit(dataR$like_Agama[dataR$nationGulf==1]))

meanGulfSorted = sort(c(meanDugongGulf,
                         meanWhaleSharkGulf,
                         meanTurtleGulf,
                         meanFalconGulf,
                         meanAgamaGulf), decreasing=T)

# Mena
meanDugongMena = mean(na.omit(dataR$like_Dugong[dataR$nationMena==1]))
meanWhaleSharkMena = mean(na.omit(dataR$like_WhaleShark[dataR$nationMena==1]))
meanTurtleMena = mean(na.omit(dataR$like_Turtle[dataR$nationMena==1]))
meanFalconMena = mean(na.omit(dataR$like_Falcon[dataR$nationMena==1]))
meanAgamaMena = mean(na.omit(dataR$like_Agama[dataR$nationMena==1]))

meanMenaSorted = sort(c(meanDugongMena,
                         meanWhaleSharkMena,
                         meanTurtleMena,
                         meanFalconMena,
                         meanAgamaMena), decreasing=T)

# Africa
meanDugongAfrica = mean(na.omit(dataR$like_Dugong[dataR$nationAfrica==1]))
meanWhaleSharkAfrica = mean(na.omit(dataR$like_WhaleShark[dataR$nationAfrica==1]))
meanTurtleAfrica = mean(na.omit(dataR$like_Turtle[dataR$nationAfrica==1]))
meanFalconAfrica = mean(na.omit(dataR$like_Falcon[dataR$nationAfrica==1]))
meanAgamaAfrica = mean(na.omit(dataR$like_Agama[dataR$nationAfrica==1]))

meanAfricaSorted = sort(c(meanDugongAfrica,
                         meanWhaleSharkAfrica,
                         meanTurtleAfrica,
                         meanFalconAfrica,
                         meanAgamaAfrica), decreasing=T)

# EastAsia
meanDugongEastAsia = mean(na.omit(dataR$like_Dugong[dataR$nationEastAsia==1]))
meanWhaleSharkEastAsia = mean(na.omit(dataR$like_WhaleShark[dataR$nationEastAsia==1]))
meanTurtleEastAsia = mean(na.omit(dataR$like_Turtle[dataR$nationEastAsia==1]))
meanFalconEastAsia = mean(na.omit(dataR$like_Falcon[dataR$nationEastAsia==1]))
meanAgamaEastAsia = mean(na.omit(dataR$like_Agama[dataR$nationEastAsia==1]))

meanEastAsiaSorted = sort(c(meanDugongEastAsia,
                         meanWhaleSharkEastAsia,
                         meanTurtleEastAsia,
                         meanFalconEastAsia,
                         meanAgamaEastAsia), decreasing=T)

# SouthAsia
meanDugongSouthAsia = mean(na.omit(dataR$like_Dugong[dataR$nationSouthAsia==1]))
meanWhaleSharkSouthAsia = mean(na.omit(dataR$like_WhaleShark[dataR$nationSouthAsia==1]))
meanTurtleSouthAsia = mean(na.omit(dataR$like_Turtle[dataR$nationSouthAsia==1]))
meanFalconSouthAsia = mean(na.omit(dataR$like_Falcon[dataR$nationSouthAsia==1]))
meanAgamaSouthAsia = mean(na.omit(dataR$like_Agama[dataR$nationSouthAsia==1]))

meanSouthAsiaSorted = sort(c(meanDugongSouthAsia,
                         meanWhaleSharkSouthAsia,
                         meanTurtleSouthAsia,
                         meanFalconSouthAsia,
                         meanAgamaSouthAsia), decreasing=T)

# Other (Mixed)
meanDugongOther = mean(na.omit(dataR$like_Dugong[dataR$nationOther==1]))
meanWhaleSharkOther = mean(na.omit(dataR$like_WhaleShark[dataR$nationOther==1]))
meanTurtleOther = mean(na.omit(dataR$like_Turtle[dataR$nationOther==1]))
meanFalconOther = mean(na.omit(dataR$like_Falcon[dataR$nationOther==1]))
meanAgamaOther = mean(na.omit(dataR$like_Agama[dataR$nationOther==1]))

meanOtherSorted = sort(c(meanDugongOther,
                         meanWhaleSharkOther,
                         meanTurtleOther,
                         meanFalconOther,
                         meanAgamaOther), decreasing=T)

###################################
# Mean Diff. Tests 
###################################

# Weird
TurtleWhaleSharkWeird = t.test(na.omit(dataR$like_Turtle[dataR$nationWeird==1]), na.omit(dataR$like_WhaleShark[dataR$nationWeird==1]))
TurtleWhaleSharkWeird95 = TurtleWhaleSharkWeird$conf.int[1:2]
TurtleWhaleSharkWeirdDiff = TurtleWhaleSharkWeird$estimate[1]-TurtleWhaleSharkWeird$estimate[2]
WhaleSharkDugongWeird = t.test(na.omit(dataR$like_WhaleShark[dataR$nationWeird==1]), na.omit(dataR$like_Dugong[dataR$nationWeird==1]))
WhaleSharkDugongWeird95 = WhaleSharkDugongWeird$conf.int[1:2]
WhaleSharkDugongWeirdDiff = WhaleSharkDugongWeird$estimate[1]-WhaleSharkDugongWeird$estimate[2]
WhaleSharkFalconWeird = t.test(na.omit(dataR$like_WhaleShark[dataR$nationWeird==1]), na.omit(dataR$like_Falcon[dataR$nationWeird==1]))
DugongFalconWeird = t.test(na.omit(dataR$like_Dugong[dataR$nationWeird==1]), na.omit(dataR$like_Falcon[dataR$nationWeird==1]))
DugongFalconWeird95 = DugongFalconWeird$conf.int[1:2]
DugongFalconWeirdDiff = DugongFalconWeird$estimate[1]-DugongFalconWeird$estimate[2]
DugongAgamaWeird = t.test(na.omit(dataR$like_Dugong[dataR$nationWeird==1]), na.omit(dataR$like_Agama[dataR$nationWeird==1]))
FalconAgamaWeird = t.test(na.omit(dataR$like_Falcon[dataR$nationWeird==1]), na.omit(dataR$like_Agama[dataR$nationWeird==1]))
FalconAgamaWeird95 = FalconAgamaWeird$conf.int[1:2]
FalconAgamaWeirdDiff = FalconAgamaWeird$estimate[1]-FalconAgamaWeird$estimate[2]


# EastAsia
TurtleDugongEastAsia = t.test(na.omit(dataR$like_Turtle[dataR$nationEastAsia==1]), na.omit(dataR$like_Dugong[dataR$nationEastAsia==1]))
TurtleDugongEastAsia95 = TurtleDugongEastAsia$conf.int[1:2]
TurtleDugongEastAsiaDiff = TurtleDugongEastAsia$estimate[1]-TurtleDugongEastAsia$estimate[2]
DugongWhaleSharkEastAsia = t.test(na.omit(dataR$like_Dugong[dataR$nationEastAsia==1]), na.omit(dataR$like_WhaleShark[dataR$nationEastAsia==1]))
DugongWhaleSharkEastAsia95 = DugongWhaleSharkEastAsia$conf.int[1:2]
DugongWhaleSharkEastAsiaDiff = DugongWhaleSharkEastAsia$estimate[1]-DugongWhaleSharkEastAsia$estimate[2]
DugongFalconEastAsia = t.test(na.omit(dataR$like_Dugong[dataR$nationEastAsia==1]), na.omit(dataR$like_Falcon[dataR$nationEastAsia==1]))
WhaleSharkFalconEastAsia = t.test(na.omit(dataR$like_WhaleShark[dataR$nationEastAsia==1]), na.omit(dataR$like_Falcon[dataR$nationEastAsia==1]))
WhaleSharkFalconEastAsia95 = WhaleSharkFalconEastAsia$conf.int[1:2]
WhaleSharkFalconEastAsiaDiff = WhaleSharkFalconEastAsia$estimate[1]-WhaleSharkFalconEastAsia$estimate[2]
FalconAgamaEastAsia = t.test(na.omit(dataR$like_Falcon[dataR$nationEastAsia==1]), na.omit(dataR$like_Agama[dataR$nationEastAsia==1]))
FalconAgamaEastAsia95 = FalconAgamaEastAsia$conf.int[1:2]
FalconAgamaEastAsiaDiff = FalconAgamaEastAsia$estimate[1]-FalconAgamaEastAsia$estimate[2]

# SouthAsia
FalconTurtleSouthAsia = t.test(na.omit(dataR$like_Falcon[dataR$nationSouthAsia==1]), na.omit(dataR$like_Turtle[dataR$nationSouthAsia==1]))
FalconTurtleSouthAsia95 = FalconTurtleSouthAsia$conf.int[1:2]
FalconTurtleSouthAsiaDiff = FalconTurtleSouthAsia$estimate[1]-FalconTurtleSouthAsia$estimate[2]
TurtleWhaleSharkSouthAsia = t.test(na.omit(dataR$like_Turtle[dataR$nationSouthAsia==1]), na.omit(dataR$like_WhaleShark[dataR$nationSouthAsia==1]))
TurtleWhaleSharkSouthAsia95 = TurtleWhaleSharkSouthAsia$conf.int[1:2]
TurtleWhaleSharkSouthAsiaDiff = TurtleWhaleSharkSouthAsia$estimate[1]-TurtleWhaleSharkSouthAsia$estimate[2]
WhaleSharkDugongSouthAsia = t.test(na.omit(dataR$like_WhaleShark[dataR$nationSouthAsia==1]), na.omit(dataR$like_Dugong[dataR$nationSouthAsia==1]))
WhaleSharkDugongSouthAsia95 = WhaleSharkDugongSouthAsia$conf.int[1:2]
WhaleSharkDugongSouthAsiaDiff = WhaleSharkDugongSouthAsia$estimate[1]-WhaleSharkDugongSouthAsia$estimate[2]
DugongAgamaSouthAsia = t.test(na.omit(dataR$like_Dugong[dataR$nationSouthAsia==1]), na.omit(dataR$like_Agama[dataR$nationSouthAsia==1]))
DugongAgamaSouthAsia95 = DugongAgamaSouthAsia$conf.int[1:2]
DugongAgamaSouthAsiaDiff = DugongAgamaSouthAsia$estimate[1]-DugongAgamaSouthAsia$estimate[2]

# Gulf
TurtleFalconGulf = t.test(na.omit(dataR$like_Turtle[dataR$nationGulf==1]), na.omit(dataR$like_Falcon[dataR$nationGulf==1]))
TurtleFalconGulf95 = TurtleFalconGulf$conf.int[1:2]
TurtleFalconGulfDiff = TurtleFalconGulf$estimate[1]-TurtleFalconGulf$estimate[2]
FalconWhaleSharkGulf = t.test(na.omit(dataR$like_Falcon[dataR$nationGulf==1]), na.omit(dataR$like_WhaleShark[dataR$nationGulf==1]))
FalconWhaleSharkGulf95 = FalconWhaleSharkGulf$conf.int[1:2]
FalconWhaleSharkGulfDiff = FalconWhaleSharkGulf$estimate[1]-FalconWhaleSharkGulf$estimate[2]
WhaleSharkDugongGulf = t.test(na.omit(dataR$like_WhaleShark[dataR$nationGulf==1]), na.omit(dataR$like_Dugong[dataR$nationGulf==1]))
WhaleSharkDugongGulf95 = WhaleSharkDugongGulf$conf.int[1:2]
WhaleSharkDugongGulfDiff = WhaleSharkDugongGulf$estimate[1]-WhaleSharkDugongGulf$estimate[2]
DugongAgamaGulf = t.test(na.omit(dataR$like_Dugong[dataR$nationGulf==1]), na.omit(dataR$like_Agama[dataR$nationGulf==1]))
DugongAgamaGulf95 = DugongAgamaGulf$conf.int[1:2]
DugongAgamaGulfDiff = DugongAgamaGulf$estimate[1]-DugongAgamaGulf$estimate[2]

# Mena
FalconTurtleMena = t.test(na.omit(dataR$like_Falcon[dataR$nationMena==1]), na.omit(dataR$like_Turtle[dataR$nationMena==1]))
FalconTurtleMena95 = FalconTurtleMena$conf.int[1:2]
FalconTurtleMenaDiff = FalconTurtleMena$estimate[1]-FalconTurtleMena$estimate[2]
TurtleDugongMena = t.test(na.omit(dataR$like_Turtle[dataR$nationMena==1]), na.omit(dataR$like_Dugong[dataR$nationMena==1]))
TurtleDugongMena95 = TurtleDugongMena$conf.int[1:2]
TurtleDugongMenaDiff = TurtleDugongMena$estimate[1]-TurtleDugongMena$estimate[2]
DugongWhaleSharkMena = t.test(na.omit(dataR$like_Dugong[dataR$nationMena==1]), na.omit(dataR$like_WhaleShark[dataR$nationMena==1]))
DugongWhaleSharkMena95 = DugongWhaleSharkMena$conf.int[1:2]
DugongWhaleSharkMenaDiff = DugongWhaleSharkMena$estimate[1]-DugongWhaleSharkMena$estimate[2]
WhaleSharkAgamaMena = t.test(na.omit(dataR$like_WhaleShark[dataR$nationMena==1]), na.omit(dataR$like_Agama[dataR$nationMena==1]))
WhaleSharkAgamaMena95 = WhaleSharkAgamaMena$conf.int[1:2]
WhaleSharkAgamaMenaDiff = WhaleSharkAgamaMena$estimate[1]-WhaleSharkAgamaMena$estimate[2]

# Africa
TurtleFalconAfrica = t.test(na.omit(dataR$like_Turtle[dataR$nationAfrica==1]), na.omit(dataR$like_Falcon[dataR$nationAfrica==1]))
TurtleFalconAfrica95 = TurtleFalconAfrica$conf.int[1:2]
TurtleFalconAfricaDiff = TurtleFalconAfrica$estimate[1]-TurtleFalconAfrica$estimate[2]
TurtleWhaleSharkAfrica = t.test(na.omit(dataR$like_Turtle[dataR$nationAfrica==1]), na.omit(dataR$like_WhaleShark[dataR$nationAfrica==1]))
FalconWhaleSharkAfrica = t.test(na.omit(dataR$like_Falcon[dataR$nationAfrica==1]), na.omit(dataR$like_WhaleShark[dataR$nationAfrica==1]))
FalconWhaleSharkAfrica95 = FalconWhaleSharkAfrica$conf.int[1:2]
FalconWhaleSharkAfricaDiff = FalconWhaleSharkAfrica$estimate[1]-FalconWhaleSharkAfrica$estimate[2]
TurtleDugongAfrica = t.test(na.omit(dataR$like_Turtle[dataR$nationAfrica==1]), na.omit(dataR$like_Dugong[dataR$nationAfrica==1]))
FalconDugongAfrica = t.test(na.omit(dataR$like_Falcon[dataR$nationAfrica==1]), na.omit(dataR$like_Dugong[dataR$nationAfrica==1]))
WhaleSharkDugongAfrica = t.test(na.omit(dataR$like_WhaleShark[dataR$nationAfrica==1]), na.omit(dataR$like_Dugong[dataR$nationAfrica==1]))
WhaleSharkDugongAfrica95 = WhaleSharkDugongAfrica$conf.int[1:2]
WhaleSharkDugongAfricaDiff = WhaleSharkDugongAfrica$estimate[1]-WhaleSharkDugongAfrica$estimate[2]
WhaleSharkAgamaAfrica = t.test(na.omit(dataR$like_WhaleShark[dataR$nationAfrica==1]), na.omit(dataR$like_Agama[dataR$nationAfrica==1]))
DugongAgamaAfrica = t.test(na.omit(dataR$like_Dugong[dataR$nationAfrica==1]), na.omit(dataR$like_Agama[dataR$nationAfrica==1]))
DugongAgamaAfrica95 = DugongAgamaAfrica$conf.int[1:2]
DugongAgamaAfricaDiff = DugongAgamaAfrica$estimate[1]-DugongAgamaAfrica$estimate[2]


# Other
TurtleFalconOther = t.test(na.omit(dataR$like_Turtle[dataR$nationOther==1]), na.omit(dataR$like_Falcon[dataR$nationOther==1]))
TurtleFalconOther95 = TurtleFalconOther$conf.int[1:2]
TurtleFalconOtherDiff = TurtleFalconOther$estimate[1]-TurtleFalconOther$estimate[2]
FalconDugongOther = t.test(na.omit(dataR$like_Falcon[dataR$nationOther==1]), na.omit(dataR$like_Dugong[dataR$nationOther==1]))
FalconDugongOther95 = FalconDugongOther$conf.int[1:2]
FalconDugongOtherDiff = FalconDugongOther$estimate[1]-FalconDugongOther$estimate[2]
DugongWhaleSharkOther = t.test(na.omit(dataR$like_Dugong[dataR$nationOther==1]), na.omit(dataR$like_WhaleShark[dataR$nationOther==1]))
DugongWhaleSharkOther95 = DugongWhaleSharkOther$conf.int[1:2]
DugongWhaleSharkOtherDiff = DugongWhaleSharkOther$estimate[1]-DugongWhaleSharkOther$estimate[2]
WhaleSharkAgamaOther = t.test(na.omit(dataR$like_WhaleShark[dataR$nationOther==1]), na.omit(dataR$like_Agama[dataR$nationOther==1]))
WhaleSharkAgamaOther95 = WhaleSharkAgamaOther$conf.int[1:2]
WhaleSharkAgamaOtherDiff = WhaleSharkAgamaOther$estimate[1]-WhaleSharkAgamaOther$estimate[2]


############################
# FIGURE 1 in the main text
############################

tiff("Figure1.tiff",
     width=700, height=900)
par(mfrow=c(3,2), 
    mar=c(2, 1, 2, 1), 
    oma = c(1.5, 1.5, 1, 1), cex=1.5)
c(meanDugongGulf,
  meanWhaleSharkGulf,
  meanTurtleGulf,
  meanFalconGulf,
  meanAgamaGulf)
nameGulf=c("T", "F", "W", "D", "A")
colorGulf=c("turquoise4","firebrick3","wheat4","darksalmon","aquamarine")
barplot(meanGulfSorted,
        col=colorGulf, ylim=c(0, 4), main="Arabian Gulf",
        ylab="Liking Level", xlab="Animal", names=nameGulf)
text(1.4,3.7,"paired t-test \n p=0.088",col="purple",cex=.6)
text(2.5,3.1,"p=0.001",col="purple",cex=.6)
text(3.8,3.0,"p=0.378",col="purple",cex=.6)
text(5,2.2,"p=0.000",col="purple",cex=.6)

c(meanDugongMena,
  meanWhaleSharkMena,
  meanTurtleMena,
  meanFalconMena,
  meanAgamaMena)
nameMena=c("F", "T", "D", "W", "A")
colorMena=c("firebrick3","turquoise4","darksalmon","wheat4","aquamarine")
barplot(meanMenaSorted,
        col=colorMena, ylim=c(0, 4), main="MENA",
        ylab="Liking Level", xlab="Animal", names=nameMena)
# legend("topright", legend =c('D = Dugong','W = Whale shark','T = H.S. Turtle', 
#                              'F = S. Falcon', 'A = S.T. Agama'), 
#        bty='n', cex=.75)
text(1.4,3.5,"p=0.723",col="purple",cex=.6)
text(2.5,3.1,"p=0.000",col="purple",cex=.6)
text(3.8,3.1,"p=0.593",col="purple",cex=.6)
text(5,2.2,"p=0.000",col="purple",cex=.6)

c(meanDugongAfrica,
  meanWhaleSharkAfrica,
  meanTurtleAfrica,
  meanFalconAfrica,
  meanAgamaAfrica)
nameAfrica=c("T", "F", "W", "D", "A")
colorAfrica=c("turquoise4","firebrick3","wheat4","darksalmon","aquamarine")
barplot(meanAfricaSorted,
        col=colorAfrica, ylim=c(0, 4), main="Africa",
        ylab="Liking Level", xlab="Animal", names=nameAfrica)
text(1.4,3.4,"p=0.900",col="purple",cex=.6)
text(2.5,3.2,"p=0.071",col="purple",cex=.6)
text(3.8,2.9,"p=0.063",col="purple",cex=.6)
text(5,2.7,"p=0.078",col="purple",cex=.6)

c(meanDugongSouthAsia,
  meanWhaleSharkSouthAsia,
  meanTurtleSouthAsia,
  meanFalconSouthAsia,
  meanAgamaSouthAsia)
nameSouthAsia=c("F", "T", "W", "D", "A")
colorSouthAsia=c("firebrick3","turquoise4","wheat4","darksalmon","aquamarine")
barplot(meanSouthAsiaSorted,
        col=colorSouthAsia, ylim=c(0, 4), main="South Asia",
        ylab="Liking Level", xlab="Animal", names=nameSouthAsia)
text(1.4,3.5,"p=0.003",col="purple",cex=.6)
text(2.5,3.2,"p=0.000",col="purple",cex=.6)
text(3.8,3.1,"p=0.000",col="purple",cex=.6)
text(5,2.7,"p=0.000",col="purple",cex=.6)

c(meanDugongEastAsia,
  meanWhaleSharkEastAsia,
  meanTurtleEastAsia,
  meanFalconEastAsia,
  meanAgamaEastAsia)
nameEastAsia=c("T", "D", "W", "F", "A")
colorEastAsia=c("turquoise4","darksalmon","wheat4","firebrick3","aquamarine")
barplot(meanEastAsiaSorted,
        col=colorEastAsia, ylim=c(0, 4), main="S. East Asia",
        ylab="Liking Level", xlab="Animal", names=nameEastAsia)
text(1.4,3.5,"p=0.009",col="purple",cex=.6)
text(2.5,3.25,"p=0.107",col="purple",cex=.6)
text(3.8,3.15,"p=0.360",col="purple",cex=.6)
text(5,2.8,"p=0.001",col="purple",cex=.6)


c(meanDugongWeird,
  meanWhaleSharkWeird,
  meanTurtleWeird,
  meanFalconWeird,
  meanAgamaWeird)
nameWeird=c("T", "W", "D", "F", "A")
colorWeird=c("turquoise4","wheat4","darksalmon","firebrick3","aquamarine")
barplot(meanWeirdSorted,
        col=colorWeird, ylim=c(0, 4), main="WEIRD",
        ylab="Liking Level", xlab="Animal", names=nameWeird)
text(1.4,3.8,"p=0.002",col="purple",cex=.6)
text(2.5,3.6,"p=0.799",col="purple",cex=.6)
text(3.8,3.45,"p=0.160",col="purple",cex=.6)
text(5,3.3,"p=0.064",col="purple",cex=.6)

dev.off()


##########################################
# FIGURE S1 in the supplementary document
##########################################
ruler=4:1
nRuler=length(ruler)
color=c("black", "blue","seagreen","purple")

coefWeird = c(TurtleWhaleSharkWeirdDiff,
              WhaleSharkDugongWeirdDiff,
              DugongFalconWeirdDiff,
              FalconAgamaWeirdDiff)
lowerWeird = c(TurtleWhaleSharkWeird95[1],
               WhaleSharkDugongWeird95[1],
               DugongFalconWeird95[1],
               FalconAgamaWeird95[1])
upperWeird = c(TurtleWhaleSharkWeird95[2],
               WhaleSharkDugongWeird95[2],
               DugongFalconWeird95[2],
               FalconAgamaWeird95[2])

coefEastAsia = c(TurtleDugongEastAsiaDiff,
                 DugongWhaleSharkEastAsiaDiff,
                 WhaleSharkFalconEastAsiaDiff,
                 FalconAgamaEastAsiaDiff)
lowerEastAsia = c(TurtleDugongEastAsia95[1],
                  DugongWhaleSharkEastAsia95[1],
                  WhaleSharkFalconEastAsia95[1],
                  FalconAgamaEastAsia95[1])
upperEastAsia = c(TurtleDugongEastAsia95[2],
                  DugongWhaleSharkEastAsia95[2],
                  WhaleSharkFalconEastAsia95[2],
                  FalconAgamaEastAsia95[2])

coefSouthAsia = c(FalconTurtleSouthAsiaDiff,
                  TurtleWhaleSharkSouthAsiaDiff,
                  WhaleSharkDugongSouthAsiaDiff,
                  DugongAgamaSouthAsiaDiff)
lowerSouthAsia = c(FalconTurtleSouthAsia95[1],
                   TurtleWhaleSharkSouthAsia95[1],
                   WhaleSharkDugongSouthAsia95[1],
                   DugongAgamaSouthAsia95[1])
upperSouthAsia = c(FalconTurtleSouthAsia95[2],
                   TurtleWhaleSharkSouthAsia95[2],
                   WhaleSharkDugongSouthAsia95[2],
                   DugongAgamaSouthAsia95[2])

coefGulf = c(TurtleFalconGulfDiff,
             FalconWhaleSharkGulfDiff,
             WhaleSharkDugongGulfDiff,
             DugongAgamaGulfDiff)
lowerGulf = c(TurtleFalconGulf95[1],
              FalconWhaleSharkGulf95[1],
              WhaleSharkDugongGulf95[1],
              DugongAgamaGulf95[1])
upperGulf = c(TurtleFalconGulf95[2],
              FalconWhaleSharkGulf95[2],
              WhaleSharkDugongGulf95[2],
              DugongAgamaGulf95[2])

coefMena = c(FalconTurtleMenaDiff,
             TurtleDugongMenaDiff,
             DugongWhaleSharkMenaDiff,
             WhaleSharkAgamaMenaDiff)
lowerMena = c(FalconTurtleMena95[1],
              TurtleDugongMena95[1],
              DugongWhaleSharkMena95[1],
              WhaleSharkAgamaMena95[1])
upperMena = c(FalconTurtleMena95[2],
              TurtleDugongMena95[2],
              DugongWhaleSharkMena95[2],
              WhaleSharkAgamaMena95[2])

coefAfrica = c(TurtleFalconAfricaDiff,
               FalconWhaleSharkAfricaDiff,
               WhaleSharkDugongAfricaDiff,
               DugongAgamaAfricaDiff)
lowerAfrica = c(TurtleFalconAfrica95[1],
                FalconWhaleSharkAfrica95[1],
                WhaleSharkDugongAfrica95[1],
                DugongAgamaAfrica95[1])
upperAfrica = c(TurtleFalconAfrica95[2],
                FalconWhaleSharkAfrica95[2],
                WhaleSharkDugongAfrica95[2],
                DugongAgamaAfrica95[2])

coefOther = c(TurtleFalconOtherDiff,
              FalconDugongOtherDiff,
              DugongWhaleSharkOtherDiff,
              WhaleSharkAgamaOtherDiff)
lowerOther = c(TurtleFalconOther95[1],
               FalconDugongOther95[1],
               DugongWhaleSharkOther95[1],
               WhaleSharkAgamaOther95[1])
upperOther = c(TurtleFalconOther95[2],
               FalconDugongOther95[2],
               DugongWhaleSharkOther95[2],
               WhaleSharkAgamaOther95[2])

tiff("FigureS1.tiff",
     width=700, height=900)
par(mfrow=c(3,2), 
    mar=c(2, .5, 1.5, .5), 
    oma = c(2, 2.5, 0, 2.5), cex=1.5)

plot(x=coefGulf, y=ruler, 
     xlab="",
     ylab="",
     main="Arabian Gulf",
     xlim=c(-0.2, 1), 
     ylim=c(0.75, 4.25),
     pch=c(15:17, 8, 9, 13, 25, 21, 14), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerGulf[i],
           x1=upperGulf[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=-0.2, 
                to=1.0,
                by=0.2))
axis(2, at=ruler, las=2,
     labels=c("T-F","F-W","W-D","D-A"))
abline(v=0, lty=2, col="red")

plot(x=coefMena, y=ruler, 
     xlab="",
     ylab="",
     main="MENA",
     xlim=c(-0.2, 1), 
     ylim=c(0.75, 4.25),
     pch=c(15:17, 8, 9, 13, 25, 21, 14), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerMena[i],
           x1=upperMena[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=-0.2, 
                to=1.0,
                by=0.2))
axis(4, at=ruler, las=2,
     labels=c("F-T","T-D","D-W","W-A"))
abline(v=0, lty=2, col="red")

plot(x=coefAfrica, y=ruler, 
     xlab="",
     ylab="",
     main="Africa",
     xlim=c(-0.2, 1), 
     ylim=c(0.75, 4.25),
     pch=c(15:17, 8, 9, 13, 25, 21, 14), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerAfrica[i],
           x1=upperAfrica[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=-0.2, 
                to=1.0,
                by=0.2))
axis(2, at=ruler, las=2,
     labels=c("T-F","F-W","W-D","D-A"))
abline(v=0, lty=2, col="red")

plot(x=coefSouthAsia, y=ruler, 
     xlab="",
     ylab="",
     main="South Asia",
     xlim=c(-0.2, 1), 
     ylim=c(0.75, 4.25),
     pch=c(15:17, 8, 9, 13, 25, 21, 14), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerSouthAsia[i],
           x1=upperSouthAsia[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=-0.2, 
                to=1.0,
                by=0.2))
axis(4, at=ruler, las=2,
     labels=c("F-T","T-W","W-D","D-A"))
abline(v=0, lty=2, col="red")

plot(x=coefEastAsia, y=ruler, 
     xlab="",
     ylab="",
     main="S. East Asia",
     xlim=c(-0.2, 1), 
     ylim=c(0.75, 4.25),
     pch=c(15:17, 8, 9, 13, 25, 21, 14), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerEastAsia[i],
           x1=upperEastAsia[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=-0.2, 
                to=1.0,
                by=0.2))
axis(2, at=ruler, las=2,
     labels=c("T-D","D-W","W-F","F-A"))
abline(v=0, lty=2, col="red")

plot(x=coefWeird, y=ruler, 
     xlab="",
     ylab="",
     main="WEIRD",
     xlim=c(-0.2, 1),
     ylim=c(0.75, 4.25),
     pch=c(15:17, 8, 9, 13, 25, 21, 14), col=color, yaxt="n", xaxt="n")
for(i in 1:nRuler){
  segments(x0=lowerWeird[i],
           x1=upperWeird[i],
           y0=ruler[i],
           y1=ruler[i], col=color[i])
}
axis(1,  at=seq(from=-0.2, 
                to=1.0,
                by=0.2))
axis(4, at=ruler, las=2,
     labels=c("T-W","W-D","D-F","F-A"))
abline(v=0, lty=2, col="red")
dev.off()


