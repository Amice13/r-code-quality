
rm(list = ls())
library(tidyverse)
library(stargazer)
library(dagitty)
library(dplyr)
library(ggdag)
library(car)
library(tinytex)
library(dotwhisker)
library(plm)
library(readr)
library(readxl)
library(stats)
library(lmtest)
library(writexl)
library(aod)
library(aods3)
library(ggplot2)




#Regular Season 2018-2019

ANA <- filter(read_excel("Original Data/2018-2019 Season/ANA2019.xlsx"),HomeTeam == "ANA") 
ARI <- filter(read_excel("Original Data/2018-2019 Season/ARI.xlsx"),HomeTeam == "ARI") 
BOS <- filter(read_excel("Original Data/2018-2019 Season/BOS.xlsx"),HomeTeam == "BOS") 
BUF <- filter(read_excel("Original Data/2018-2019 Season/BUF.xlsx"),HomeTeam == "BUF") 
CAR <- filter(read_excel("Original Data/2018-2019 Season/CAR.xlsx"),HomeTeam == "CAR") 
CBJ <- filter(read_excel("Original Data/2018-2019 Season/CBJ.xlsx"),HomeTeam == "CBJ") 
CGY <- filter(read_excel("Original Data/2018-2019 Season/CGY.xlsx"),HomeTeam == "CGY") 
CHI <- filter(read_excel("Original Data/2018-2019 Season/CHI.xlsx"),HomeTeam == "CHI") 
COL <- filter(read_excel("Original Data/2018-2019 Season/COL.xlsx"),HomeTeam == "COL") 
DAL <- filter(read_excel("Original Data/2018-2019 Season/DAL.xlsx"),HomeTeam == "DAL") 
DET<- filter(read_excel("Original Data/2018-2019 Season/DET.xlsx"),HomeTeam == "DET")  
EDM <- filter(read_excel("Original Data/2018-2019 Season/EDM.xlsx"),HomeTeam == "EDM") 
FLA <- filter(read_excel("Original Data/2018-2019 Season/FLA.xlsx"),HomeTeam == "FLA") 
LA <- filter(read_excel("Original Data/2018-2019 Season/LA.xlsx"),HomeTeam == "LA") 
MIN <- filter(read_excel("Original Data/2018-2019 Season/MIN.xlsx"),HomeTeam == "MIN") 
MON <- filter(read_excel("Original Data/2018-2019 Season/MON.xlsx"),HomeTeam == "MON") 
NAS <- filter(read_excel("Original Data/2018-2019 Season/NAS.xlsx"),HomeTeam == "NAS") 
NJ <- filter(read_excel("Original Data/2018-2019 Season/NJ.xlsx"),HomeTeam == "NJ") 
NYI <- filter(read_excel("Original Data/2018-2019 Season/NYI.xlsx"),HomeTeam == "NYI") 
NYR <- filter(read_excel("Original Data/2018-2019 Season/NYR.xlsx"),HomeTeam == "NYR") 
OTT<- filter(read_excel("Original Data/2018-2019 Season/OTT.xlsx"),HomeTeam == "OTT") 
PHI <- filter(read_excel("Original Data/2018-2019 Season/PHI.xlsx"),HomeTeam == "PHI") 
PIT <- filter(read_excel("Original Data/2018-2019 Season/PIT.xlsx"),HomeTeam == "PIT") 
SJ <- filter(read_excel("Original Data/2018-2019 Season/SJ.xlsx"),HomeTeam == "SJ") 
STL <- filter(read_excel("Original Data/2018-2019 Season/STL.xlsx"),HomeTeam == "STL") 
TB <- filter(read_excel("Original Data/2018-2019 Season/TB.xlsx"),HomeTeam == "TB") 
TOR <- filter(read_excel("Original Data/2018-2019 Season/TOR.xlsx"),HomeTeam == "TOR") 
VAN <- filter(read_excel("Original Data/2018-2019 Season/VAN.xlsx"),HomeTeam == "VAN") 
VEG <- filter(read_excel("Original Data/2018-2019 Season/VEG.xlsx"),HomeTeam == "VEG") 
WAS <- filter(read_excel("Original Data/2018-2019 Season/WAS.xlsx"),HomeTeam == "WAS") 
WPG <- filter(read_excel("Original Data/2018-2019 Season/WPG.xlsx"),HomeTeam == "WPG") 




RS20182019 <- ANA 
RS20182019 <- rbind(RS20182019, ARI)
RS20182019 <- rbind(RS20182019, BOS)
RS20182019 <- rbind(RS20182019, BUF)
RS20182019 <- rbind(RS20182019, CAR)
RS20182019 <- rbind(RS20182019, CBJ)
RS20182019 <- rbind(RS20182019, CGY)
RS20182019 <- rbind(RS20182019, CHI)
RS20182019 <- rbind(RS20182019, COL)
RS20182019 <- rbind(RS20182019, DAL)
RS20182019 <- rbind(RS20182019, DET)
RS20182019 <- rbind(RS20182019, EDM)
RS20182019 <- rbind(RS20182019, FLA)
RS20182019 <- rbind(RS20182019, LA)
RS20182019 <- rbind(RS20182019, MIN)
RS20182019 <- rbind(RS20182019, MON)
RS20182019 <- rbind(RS20182019, NAS)
RS20182019 <- rbind(RS20182019, NJ)
RS20182019 <- rbind(RS20182019, NYI)
RS20182019 <- rbind(RS20182019, NYR)
RS20182019 <- rbind(RS20182019, OTT)
RS20182019 <- rbind(RS20182019, PHI)
RS20182019 <- rbind(RS20182019, PIT)
RS20182019 <- rbind(RS20182019, SJ)
RS20182019 <- rbind(RS20182019, STL)
RS20182019 <- rbind(RS20182019, TB)
RS20182019 <- rbind(RS20182019, TOR)
RS20182019 <- rbind(RS20182019, VAN)
RS20182019 <- rbind(RS20182019, VEG)
RS20182019 <- rbind(RS20182019, WAS)
RS20182019 <- rbind(RS20182019, WPG)


#Regular Season 2019-2020

ANA <- filter(read_excel("Original Data/2019-2020 Season/ANA.xlsx"),HomeTeam == "ANA") 
ARI <- filter(read_excel("Original Data/2019-2020 Season/ARI.xlsx"),HomeTeam == "ARI") 
BOS <- filter(read_excel("Original Data/2019-2020 Season/BOS.xlsx"),HomeTeam == "BOS") 
BUF <- filter(read_excel("Original Data/2019-2020 Season/BUF.xlsx"),HomeTeam == "BUF") 
CAR <- filter(read_excel("Original Data/2019-2020 Season/CAR.xlsx"),HomeTeam == "CAR") 
CBJ <- filter(read_excel("Original Data/2019-2020 Season/CBJ.xlsx"),HomeTeam == "CBJ") 
CGY <- filter(read_excel("Original Data/2019-2020 Season/CGY.xlsx"),HomeTeam == "CGY") 
CHI <- filter(read_excel("Original Data/2019-2020 Season/CHI.xlsx"),HomeTeam == "CHI") 
COL <- filter(read_excel("Original Data/2019-2020 Season/COL.xlsx"),HomeTeam == "COL") 
DAL <- filter(read_excel("Original Data/2019-2020 Season/DAL.xlsx"),HomeTeam == "DAL") 
DET<- filter(read_excel("Original Data/2019-2020 Season/DET.xlsx"),HomeTeam == "DET")  
EDM <- filter(read_excel("Original Data/2019-2020 Season/EDM.xlsx"),HomeTeam == "EDM") 
FLA <- filter(read_excel("Original Data/2019-2020 Season/FLA.xlsx"),HomeTeam == "FLA") 
LA <- filter(read_excel("Original Data/2019-2020 Season/LA.xlsx"),HomeTeam == "LA") 
MIN <- filter(read_excel("Original Data/2019-2020 Season/MIN.xlsx"),HomeTeam == "MIN") 
MON <- filter(read_excel("Original Data/2019-2020 Season/MON.xlsx"),HomeTeam == "MON") 
NAS <- filter(read_excel("Original Data/2019-2020 Season/NAS.xlsx"),HomeTeam == "NAS") 
NJ <- filter(read_excel("Original Data/2019-2020 Season/NJ.xlsx"),HomeTeam == "NJ") 
NYI <- filter(read_excel("Original Data/2019-2020 Season/NYI.xlsx"),HomeTeam == "NYI") 
NYR <- filter(read_excel("Original Data/2019-2020 Season/NYR.xlsx"),HomeTeam == "NYR") 
OTT<- filter(read_excel("Original Data/2019-2020 Season/OTT.xlsx"),HomeTeam == "OTT") 
PHI <- filter(read_excel("Original Data/2019-2020 Season/PHI.xlsx"),HomeTeam == "PHI") 
PIT <- filter(read_excel("Original Data/2019-2020 Season/PIT.xlsx"),HomeTeam == "PIT") 
SJ <- filter(read_excel("Original Data/2019-2020 Season/SJ.xlsx"),HomeTeam == "SJ") 
STL <- filter(read_excel("Original Data/2019-2020 Season/STL.xlsx"),HomeTeam == "STL") 
TB <- filter(read_excel("Original Data/2019-2020 Season/TB.xlsx"),HomeTeam == "TB") 
TOR <- filter(read_excel("Original Data/2019-2020 Season/TOR.xlsx"),HomeTeam == "TOR") 
VAN <- filter(read_excel("Original Data/2019-2020 Season/VAN.xlsx"),HomeTeam == "VAN") 
VEG <- filter(read_excel("Original Data/2019-2020 Season/VEG.xlsx"),HomeTeam == "VEG") 
WAS <- filter(read_excel("Original Data/2019-2020 Season/WAS.xlsx"),HomeTeam == "WAS") 
WPG <- filter(read_excel("Original Data/2019-2020 Season/WPG.xlsx"),HomeTeam == "WPG") 





RS20192020 <- ANA 
RS20192020 <- rbind(RS20192020, ARI)
RS20192020 <- rbind(RS20192020, BOS)
RS20192020 <- rbind(RS20192020, BUF)
RS20192020 <- rbind(RS20192020, CAR)
RS20192020 <- rbind(RS20192020, CBJ)
RS20192020 <- rbind(RS20192020, CGY)
RS20192020 <- rbind(RS20192020, CHI)
RS20192020 <- rbind(RS20192020, COL)
RS20192020 <- rbind(RS20192020, DAL)
RS20192020 <- rbind(RS20192020, DET)
RS20192020 <- rbind(RS20192020, EDM)
RS20192020 <- rbind(RS20192020, FLA)
RS20192020 <- rbind(RS20192020, LA)
RS20192020 <- rbind(RS20192020, MIN)
RS20192020 <- rbind(RS20192020, MON)
RS20192020 <- rbind(RS20192020, NAS)
RS20192020 <- rbind(RS20192020, NJ)
RS20192020 <- rbind(RS20192020, NYI)
RS20192020 <- rbind(RS20192020, NYR)
RS20192020 <- rbind(RS20192020, OTT)
RS20192020 <- rbind(RS20192020, PHI)
RS20192020 <- rbind(RS20192020, PIT)
RS20192020 <- rbind(RS20192020, SJ)
RS20192020 <- rbind(RS20192020, STL)
RS20192020 <- rbind(RS20192020, TB)
RS20192020 <- rbind(RS20192020, TOR)
RS20192020 <- rbind(RS20192020, VAN)
RS20192020 <- rbind(RS20192020, VEG)
RS20192020 <- rbind(RS20192020, WAS)
RS20192020 <- rbind(RS20192020, WPG)



#Regular Season 2020-2021

ANA <- filter(read_excel("Original Data/2020-2021 Season/ANA.xlsx"),HomeTeam == "ANA") 
ARI <- filter(read_excel("Original Data/2020-2021 Season/ARI.xlsx"),HomeTeam == "ARI") 
BOS <- filter(read_excel("Original Data/2020-2021 Season/BOS.xlsx"),HomeTeam == "BOS") 
BUF <- filter(read_excel("Original Data/2020-2021 Season/BUF.xlsx"),HomeTeam == "BUF") 
CAR <- filter(read_excel("Original Data/2020-2021 Season/CAR.xlsx"),HomeTeam == "CAR") 
CBJ <- filter(read_excel("Original Data/2020-2021 Season/CBJ.xlsx"),HomeTeam == "CBJ") 
CGY <- filter(read_excel("Original Data/2020-2021 Season/CGY.xlsx"),HomeTeam == "CGY") 
CHI <- filter(read_excel("Original Data/2020-2021 Season/CHI.xlsx"),HomeTeam == "CHI") 
COL <- filter(read_excel("Original Data/2020-2021 Season/COL.xlsx"),HomeTeam == "COL") 
DAL <- filter(read_excel("Original Data/2020-2021 Season/DAL.xlsx"),HomeTeam == "DAL") 
DET<- filter(read_excel("Original Data/2020-2021 Season/DET.xlsx"),HomeTeam == "DET")  
EDM <- filter(read_excel("Original Data/2020-2021 Season/EDM.xlsx"),HomeTeam == "EDM") 
FLA <- filter(read_excel("Original Data/2020-2021 Season/FLA.xlsx"),HomeTeam == "FLA") 
LA <- filter(read_excel("Original Data/2020-2021 Season/LA.xlsx"),HomeTeam == "LA") 
MIN <- filter(read_excel("Original Data/2020-2021 Season/MIN.xlsx"),HomeTeam == "MIN") 
MON <- filter(read_excel("Original Data/2020-2021 Season/MON.xlsx"),HomeTeam == "MON") 
NAS <- filter(read_excel("Original Data/2020-2021 Season/NAS.xlsx"),HomeTeam == "NAS") 
NJ <- filter(read_excel("Original Data/2020-2021 Season/NJ.xlsx"),HomeTeam == "NJ") 
NYI <- filter(read_excel("Original Data/2020-2021 Season/NYI.xlsx"),HomeTeam == "NYI") 
NYR <- filter(read_excel("Original Data/2020-2021 Season/NYR.xlsx"),HomeTeam == "NYR") 
OTT<- filter(read_excel("Original Data/2020-2021 Season/OTT.xlsx"),HomeTeam == "OTT") 
PHI <- filter(read_excel("Original Data/2020-2021 Season/PHI.xlsx"),HomeTeam == "PHI") 
PIT <- filter(read_excel("Original Data/2020-2021 Season/PIT.xlsx"),HomeTeam == "PIT") 
SJ <- filter(read_excel("Original Data/2020-2021 Season/SJ.xlsx"),HomeTeam == "SJ") 
STL <- filter(read_excel("Original Data/2020-2021 Season/STL.xlsx"),HomeTeam == "STL") 
TB <- filter(read_excel("Original Data/2020-2021 Season/TB.xlsx"),HomeTeam == "TB") 
TOR <- filter(read_excel("Original Data/2020-2021 Season/TOR.xlsx"),HomeTeam == "TOR") 
VAN <- filter(read_excel("Original Data/2020-2021 Season/VAN.xlsx"),HomeTeam == "VAN") 
VEG <- filter(read_excel("Original Data/2020-2021 Season/VEG.xlsx"),HomeTeam == "VEG") 
WAS <- filter(read_excel("Original Data/2020-2021 Season/WAS.xlsx"),HomeTeam == "WAS") 
WPG <- filter(read_excel("Original Data/2020-2021 Season/WPG.xlsx"),HomeTeam == "WPG") 




RS20202021<- ANA 
RS20202021 <- rbind(RS20202021, ARI)
RS20202021<- rbind(RS20202021, BOS)
RS20202021 <- rbind(RS20202021, BUF)
RS20202021<- rbind(RS20202021, CAR)
RS20202021 <- rbind(RS20202021, CBJ)
RS20202021 <- rbind(RS20202021, CGY)
RS20202021 <- rbind(RS20202021, CHI)
RS20202021 <- rbind(RS20202021, COL)
RS20202021 <- rbind(RS20202021, DAL)
RS20202021 <- rbind(RS20202021, DET)
RS20202021 <- rbind(RS20202021, EDM)
RS20202021 <- rbind(RS20202021, FLA)
RS20202021 <- rbind(RS20202021, LA)
RS20202021<- rbind(RS20202021, MIN)
RS20202021 <- rbind(RS20202021, MON)
RS20202021 <- rbind(RS20202021, NAS)
RS20202021 <- rbind(RS20202021, NJ)
RS20202021 <- rbind(RS20202021, NYI)
RS20202021 <- rbind(RS20202021, NYR)
RS20202021 <- rbind(RS20202021, OTT)
RS20202021 <- rbind(RS20202021, PHI)
RS20202021 <- rbind(RS20202021, PIT)
RS20202021 <- rbind(RS20202021, SJ)
RS20202021 <- rbind(RS20202021, STL)
RS20202021 <- rbind(RS20202021, TB)
RS20202021 <- rbind(RS20202021, TOR)
RS20202021 <- rbind(RS20202021, VAN)
RS20202021 <- rbind(RS20202021, VEG)
RS20202021 <- rbind(RS20202021, WAS)
RS20202021 <- rbind(RS20202021, WPG)
```

#Regular Season 2021-2022

ANA <- filter(read_excel("Original Data/2021-2022 Season/ANA.xlsx"),HomeTeam == "ANA") 
ARI <- filter(read_excel("Original Data/2021-2022 Season/ARI.xlsx"),HomeTeam == "ARI") 
BOS <- filter(read_excel("Original Data/2021-2022 Season/BOS.xlsx"),HomeTeam == "BOS") 
BUF <- filter(read_excel("Original Data/2021-2022 Season/BUF.xlsx"),HomeTeam == "BUF") 
CAR <- filter(read_excel("Original Data/2021-2022 Season/CAR.xlsx"),HomeTeam == "CAR") 
CBJ <- filter(read_excel("Original Data/2021-2022 Season/CBJ.xlsx"),HomeTeam == "CBJ") 
CGY <- filter(read_excel("Original Data/2021-2022 Season/CGY.xlsx"),HomeTeam == "CGY") 
CHI <- filter(read_excel("Original Data/2021-2022 Season/CHI.xlsx"),HomeTeam == "CHI") 
COL <- filter(read_excel("Original Data/2021-2022 Season/COL.xlsx"),HomeTeam == "COL") 
DAL <- filter(read_excel("Original Data/2021-2022 Season/DAL.xlsx"),HomeTeam == "DAL") 
DET<- filter(read_excel("Original Data/2021-2022 Season/DET.xlsx"),HomeTeam == "DET")  
EDM <- filter(read_excel("Original Data/2021-2022 Season/EDM.xlsx"),HomeTeam == "EDM") 
FLA <- filter(read_excel("Original Data/2021-2022 Season/FLA.xlsx"),HomeTeam == "FLA") 
LA <- filter(read_excel("Original Data/2021-2022 Season/LA.xlsx"),HomeTeam == "LA") 
MIN <- filter(read_excel("Original Data/2021-2022 Season/MIN.xlsx"),HomeTeam == "MIN") 
MON <- filter(read_excel("Original Data/2021-2022 Season/MON.xlsx"),HomeTeam == "MON") 
NAS <- filter(read_excel("Original Data/2021-2022 Season/NAS.xlsx"),HomeTeam == "NAS") 
NJ <- filter(read_excel("Original Data/2021-2022 Season/NJ.xlsx"),HomeTeam == "NJ") 
NYI <- filter(read_excel("Original Data/2021-2022 Season/NYI.xlsx"),HomeTeam == "NYI") 
NYR <- filter(read_excel("Original Data/2021-2022 Season/NYR.xlsx"),HomeTeam == "NYR") 
OTT<- filter(read_excel("Original Data/2021-2022 Season/OTT.xlsx"),HomeTeam == "OTT") 
PHI <- filter(read_excel("Original Data/2021-2022 Season/PHI.xlsx"),HomeTeam == "PHI") 
PIT <- filter(read_excel("Original Data/2021-2022 Season/PIT.xlsx"),HomeTeam == "PIT") 
SEA <- filter(read_excel("Original Data/2021-2022 Season/SEA.xlsx"),HomeTeam == "SEA") 
SJ <- filter(read_excel("Original Data/2021-2022 Season/SJ.xlsx"),HomeTeam == "SJ") 
STL <- filter(read_excel("Original Data/2021-2022 Season/STL.xlsx"),HomeTeam == "STL") 
TB <- filter(read_excel("Original Data/2021-2022 Season/TB.xlsx"),HomeTeam == "TB") 
TOR <- filter(read_excel("Original Data/2021-2022 Season/TOR.xlsx"),HomeTeam == "TOR") 
VAN <- filter(read_excel("Original Data/2021-2022 Season/VAN.xlsx"),HomeTeam == "VAN") 
VEG <- filter(read_excel("Original Data/2021-2022 Season/VEG.xlsx"),HomeTeam == "VEG") 
WAS <- filter(read_excel("Original Data/2021-2022 Season/WAS.xlsx"),HomeTeam == "WAS") 
WPG <- filter(read_excel("Original Data/2021-2022 Season/WPG.xlsx"),HomeTeam == "WPG") 



RS20212022<- ANA 
RS20212022 <- rbind(RS20212022, ARI)
RS20212022<- rbind(RS20212022, BOS)
RS20212022 <- rbind(RS20212022, BUF)
RS20212022<- rbind(RS20212022, CAR)
RS20212022 <- rbind(RS20212022, CBJ)
RS20212022 <- rbind(RS20212022, CGY)
RS20212022 <- rbind(RS20212022, CHI)
RS20212022 <- rbind(RS20212022, COL)
RS20212022 <- rbind(RS20212022, DAL)
RS20212022 <- rbind(RS20212022, DET)
RS20212022 <- rbind(RS20212022, EDM)
RS20212022 <- rbind(RS20212022, FLA)
RS20212022 <- rbind(RS20212022, LA)
RS20212022<- rbind(RS20212022, MIN)
RS20212022 <- rbind(RS20212022, MON)
RS20212022 <- rbind(RS20212022, NAS)
RS20212022 <- rbind(RS20212022, NJ)
RS20212022 <- rbind(RS20212022, NYI)
RS20212022 <- rbind(RS20212022, NYR)
RS20212022 <- rbind(RS20212022, OTT)
RS20212022 <- rbind(RS20212022, PHI)
RS20212022 <- rbind(RS20212022, PIT)
RS20212022 <- rbind(RS20212022, SEA)
RS20212022 <- rbind(RS20212022, SJ)
RS20212022 <- rbind(RS20212022, STL)
RS20212022 <- rbind(RS20212022, TB)
RS20212022 <- rbind(RS20212022, TOR)
RS20212022 <- rbind(RS20212022, VAN)
RS20212022 <- rbind(RS20212022, VEG)
RS20212022 <- rbind(RS20212022, WAS)
RS20212022 <- rbind(RS20212022, WPG)
```

#Combine All DATA

AllData <- rbind(RS20182019, RS20192020, RS20202021, RS20212022)





names(AllData) <- gsub("_888", "", names(AllData))



#Deviation Variables 

AllGames<- AllData %>% 
  mutate(DKDifHome = ifelse(DraftKingsHomeMoneyLine<0, DraftKingsHomeMoneyLine+100, DraftKingsHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         DKDifAway= ifelse(DraftKingsAwayMoneyLine<0, DraftKingsAwayMoneyLine+100, DraftKingsAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         FanDuelDifHome = ifelse(FanDuelHomeMoneyLine<0, FanDuelHomeMoneyLine+100, FanDuelHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         FanDuelDifAway= ifelse(FanDuelAwayMoneyLine<0, FanDuelAwayMoneyLine+100, FanDuelAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         SugarDifHome = ifelse(SugarHousePAHomeMoneyLine<0, SugarHousePAHomeMoneyLine+100, SugarHousePAHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         SugarDifAway= ifelse(SugarHousePAAwayMoneyLine<0, SugarHousePAAwayMoneyLine+100, SugarHousePAAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         ParxDifHome = ifelse(ParxHomeMoneyLine<0, ParxHomeMoneyLine+100, ParxHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         ParxDifAway= ifelse(ParxAwayMoneyLine<0, ParxAwayMoneyLine+100, ParxAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         SportNJDifHome = ifelse(SportNJHomeMoneyLine<0, SportNJHomeMoneyLine+100, SportNJHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         SportNJDifAway= ifelse(SportNJAwayMoneyLine<0, SportNJAwayMoneyLine+100, SportNJAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         RiversDifHome = ifelse(RiversCasinoPAHomeMoneyLine<0, RiversCasinoPAHomeMoneyLine+100, RiversCasinoPAHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         RiversDifAway= ifelse(RiversCasinoPAAwayMoneyLine<0, RiversCasinoPAAwayMoneyLine+100, RiversCasinoPAAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         PointsBetDifHome = ifelse(PointsBetHomeMoneyLine<0, PointsBetHomeMoneyLine+100, PointsBetHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         PointsBetDifAway= ifelse(PointsBetAwayMoneyLine<0, PointsBetAwayMoneyLine+100, PointsBetAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         BetMGMDifHome = ifelse(BetMGMHomeMoneyLine<0, BetMGMHomeMoneyLine+100, BetMGMHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         BetMGMDifAway= ifelse(BetMGMAwayMoneyLine<0, BetMGMAwayMoneyLine+100, BetMGMAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100),
         UnibetDifHome = ifelse(UnibetHomeMoneyLine<0, UnibetHomeMoneyLine+100, UnibetHomeMoneyLine-100)-
           ifelse(ConsensusHomeMoneyLine<0, ConsensusHomeMoneyLine+100, ConsensusHomeMoneyLine-100),
         UnibetDifAway= ifelse(UnibetAwayMoneyLine<0, UnibetAwayMoneyLine+100, UnibetAwayMoneyLine-100)-
           ifelse(ConsensusAwayMoneyLine<0, ConsensusAwayMoneyLine+100, ConsensusAwayMoneyLine-100)
  )






BookData <- AllGames %>% 
  select("Season", "AwayTeamScore", "AwayTeamHasWon",  "HomeTeamScore", "HomeTeamHasWon", "ConsensusHomeMoneyLine", "ConsensusAwayMoneyLine", "DraftKingsHomeMoneyLine", "DraftKingsAwayMoneyLine", "FanDuelHomeMoneyLine", "FanDuelAwayMoneyLine", "SugarHousePAHomeMoneyLine", "SugarHousePAAwayMoneyLine", "ParxHomeMoneyLine", "ParxAwayMoneyLine",  "SportNJHomeMoneyLine", "SportNJAwayMoneyLine",  "RiversCasinoPAHomeMoneyLine", "RiversCasinoPAAwayMoneyLine", "PointsBetHomeMoneyLine", "PointsBetAwayMoneyLine", "BetMGMHomeMoneyLine","BetMGMAwayMoneyLine","UnibetHomeMoneyLine", "UnibetAwayMoneyLine", "DKDifHome", "DKDifAway", "FanDuelDifHome", "FanDuelDifAway", "SugarDifHome",  "SugarDifAway", "ParxDifHome", "ParxDifAway", "SportNJDifHome", "SportNJDifAway", "RiversDifHome", "RiversDifAway", "PointsBetDifHome","PointsBetDifAway", "BetMGMDifHome", "BetMGMDifAway", "UnibetDifHome", "UnibetDifAway")


stargazer(as.data.frame(BookData), header = FALSE, type = "text", keep.stat = c("n", "rsq"),
          omit.summary.stat = c("p25", "p75"))




#MoneyLines to ODDS:
  
  #Convert MoneyLines into Odds: 
 # Money Lines to Odds 
#(favorite): 100/ |ML|
  
 # (underdog): ML/ 100

#first one is +172 --> 172/100=1.72
#second one is -136--> 100/ |-136|= .735

#Fractional Odds

Odds_Data<- AllGames %>% 
  mutate(DKFracoddsHome = ifelse(DraftKingsHomeMoneyLine<0, #favorite
                                 (100/abs(DraftKingsHomeMoneyLine)),
                                 ifelse(DraftKingsHomeMoneyLine > 0, #underdog
                                        (DraftKingsHomeMoneyLine/100) ,
                                        ifelse(DraftKingsHomeMoneyLine == 100, #equal 100
                                               1, NA))),
  )

Odds_Data <- Odds_Data %>%
  mutate(DKFracoddsAway = ifelse(DraftKingsAwayMoneyLine<0, #favorite
                                 100/abs(DraftKingsAwayMoneyLine),
                                 ifelse(DraftKingsAwayMoneyLine > 0, #underdog
                                        DraftKingsAwayMoneyLine/100,
                                        ifelse(DraftKingsAwayMoneyLine == 100, #equal 100
                                               1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(FanDuelFracoddsHome = ifelse(FanDuelHomeMoneyLine<0, #favorite
                                      100/abs(FanDuelHomeMoneyLine),
                                      ifelse(FanDuelHomeMoneyLine > 0, #underdog
                                             FanDuelHomeMoneyLine/100,
                                             ifelse(FanDuelHomeMoneyLine == 100, #equal 100
                                                    1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(FanDuelFracoddsAway = ifelse(FanDuelAwayMoneyLine<0, #favorite
                                      100/abs(FanDuelAwayMoneyLine),
                                      ifelse(FanDuelAwayMoneyLine > 0, #underdog
                                             FanDuelAwayMoneyLine/100,
                                             ifelse(FanDuelAwayMoneyLine == 100, #equal 100
                                                    1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(SugarHousePAFracoddsHome = ifelse(SugarHousePAHomeMoneyLine<0, #favorite
                                           100/abs(SugarHousePAHomeMoneyLine),
                                           ifelse(SugarHousePAHomeMoneyLine > 0, #underdog
                                                  SugarHousePAHomeMoneyLine/100,
                                                  ifelse(SugarHousePAHomeMoneyLine == 100, #equal 100
                                                         1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(SugarHousePAFracoddsAway = ifelse(SugarHousePAAwayMoneyLine<0, #favorite
                                           100/abs(SugarHousePAAwayMoneyLine),
                                           ifelse(SugarHousePAAwayMoneyLine > 0, #underdog
                                                  SugarHousePAAwayMoneyLine/100,
                                                  ifelse(SugarHousePAAwayMoneyLine == 100, #equal 100
                                                         1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(ParxFracoddsHome = ifelse(ParxHomeMoneyLine<0, #favorite
                                   100/abs(ParxHomeMoneyLine),
                                   ifelse(ParxHomeMoneyLine > 0, #underdog
                                          ParxHomeMoneyLine/100,
                                          ifelse(ParxHomeMoneyLine == 100, #equal 100
                                                 1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(ParxFracoddsAway = ifelse(ParxAwayMoneyLine<0, #favorite
                                   100/abs(ParxAwayMoneyLine),
                                   ifelse(ParxAwayMoneyLine > 0, #underdog
                                          ParxAwayMoneyLine/100,
                                          ifelse(ParxAwayMoneyLine == 100, #equal 100
                                                 1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(SportNJFracoddsHome = ifelse(SportNJHomeMoneyLine<0, #favorite
                                      100/abs(SportNJHomeMoneyLine),
                                      ifelse(SportNJHomeMoneyLine > 0, #underdog
                                             SportNJHomeMoneyLine/100,
                                             ifelse(SportNJHomeMoneyLine == 100, #equal 100
                                                    1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(SportNJFracoddsAway = ifelse(SportNJAwayMoneyLine<0, #favorite
                                      100/abs(SportNJAwayMoneyLine),
                                      ifelse(SportNJAwayMoneyLine > 0, #underdog
                                             SportNJAwayMoneyLine/100,
                                             ifelse(SportNJAwayMoneyLine == 100, #equal 100
                                                    1, NA)))
  )
Odds_Data <- Odds_Data %>% 
  mutate(RiversCasinoPAFracoddsHome = ifelse(RiversCasinoPAHomeMoneyLine<0, #favorite
                                             100/abs(RiversCasinoPAHomeMoneyLine),
                                             ifelse(RiversCasinoPAHomeMoneyLine > 0, #underdog
                                                    RiversCasinoPAHomeMoneyLine/100,
                                                    ifelse(RiversCasinoPAHomeMoneyLine == 100, #equal 100
                                                           1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(RiversCasinoPAFracoddsAway = ifelse(RiversCasinoPAAwayMoneyLine<0, #favorite
                                             100/abs(RiversCasinoPAAwayMoneyLine),
                                             ifelse(RiversCasinoPAAwayMoneyLine > 0, #underdog
                                                    RiversCasinoPAAwayMoneyLine/100,
                                                    ifelse(RiversCasinoPAAwayMoneyLine == 100, #equal 100
                                                           1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(CaesarsFracoddsHome = ifelse(CaesarsHomeMoneyLine<0, #favorite
                                      100/abs(CaesarsHomeMoneyLine),
                                      ifelse(CaesarsHomeMoneyLine > 0, #underdog
                                             CaesarsHomeMoneyLine/100,
                                             ifelse(CaesarsHomeMoneyLine == 100, #equal 100
                                                    1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(CaesarsFracoddsAway = ifelse(CaesarsAwayMoneyLine<0, #favorite
                                      100/abs(CaesarsAwayMoneyLine),
                                      ifelse(CaesarsAwayMoneyLine > 0, #underdog
                                             CaesarsAwayMoneyLine/100,
                                             ifelse(CaesarsAwayMoneyLine == 100, #equal 100
                                                    1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(PointsBetFracoddsHome = ifelse(PointsBetHomeMoneyLine<0, #favorite
                                        100/abs(PointsBetHomeMoneyLine),
                                        ifelse(PointsBetHomeMoneyLine > 0, #underdog
                                               PointsBetHomeMoneyLine/100,
                                               ifelse(PointsBetHomeMoneyLine == 100, #equal 100
                                                      1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(PointsBetFracoddsAway = ifelse(PointsBetAwayMoneyLine<0, #favorite
                                        100/abs(PointsBetAwayMoneyLine),
                                        ifelse(PointsBetAwayMoneyLine > 0, #underdog
                                               PointsBetAwayMoneyLine/100,
                                               ifelse(PointsBetAwayMoneyLine == 100, #equal 100
                                                      1, NA)))
  )
Odds_Data <- Odds_Data %>% 
  mutate(BetMGMFracoddsHome = ifelse(BetMGMHomeMoneyLine<0, #favorite
                                     100/abs(BetMGMHomeMoneyLine),
                                     ifelse(BetMGMHomeMoneyLine > 0, #underdog
                                            BetMGMHomeMoneyLine/100,
                                            ifelse(BetMGMHomeMoneyLine == 100, #equal 100
                                                   1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(BetMGMFracoddsAway = ifelse(BetMGMAwayMoneyLine<0, #favorite
                                     100/abs(BetMGMAwayMoneyLine),
                                     ifelse(BetMGMAwayMoneyLine > 0, #underdog
                                            BetMGMAwayMoneyLine/100,
                                            ifelse(BetMGMAwayMoneyLine == 100, #equal 100
                                                   1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(UnibetFracoddsHome = ifelse(UnibetHomeMoneyLine<0, #favorite
                                     100/abs(UnibetHomeMoneyLine),
                                     ifelse(UnibetHomeMoneyLine > 0, #underdog
                                            UnibetHomeMoneyLine/100,
                                            ifelse(UnibetHomeMoneyLine == 100, #equal 100
                                                   1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(UnibetFracoddsAway = ifelse(UnibetAwayMoneyLine<0, #favorite
                                     100/abs(UnibetAwayMoneyLine),
                                     ifelse(UnibetAwayMoneyLine > 0, #underdog
                                            UnibetAwayMoneyLine/100,
                                            ifelse(UnibetAwayMoneyLine == 100, #equal 100
                                                   1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(BarstoolFracoddsHome = ifelse(BarstoolHomeMoneyLine<0, #favorite
                                       100/abs(BarstoolHomeMoneyLine),
                                       ifelse(BarstoolHomeMoneyLine > 0, #underdog
                                              BarstoolHomeMoneyLine/100,
                                              ifelse(BarstoolHomeMoneyLine == 100, #equal 100
                                                     1, NA)))
  )

Odds_Data <- Odds_Data %>%
  mutate(BarstoolFracoddsAway = ifelse(BarstoolAwayMoneyLine<0, #favorite
                                       100/abs(BarstoolAwayMoneyLine),
                                       ifelse(BarstoolAwayMoneyLine > 0, #underdog
                                              BarstoolAwayMoneyLine/100,
                                              ifelse(BarstoolAwayMoneyLine == 100, #equal 100
                                                     1, NA)))
  )

Odds_Data <- Odds_Data %>% 
  mutate(ConsensusFracoddsHome = ifelse(ConsensusHomeMoneyLine<0, #favorite
                                        100/abs(ConsensusHomeMoneyLine),
                                        ifelse(ConsensusHomeMoneyLine > 0, #underdog
                                               ConsensusHomeMoneyLine/100,
                                               ifelse(ConsensusHomeMoneyLine == 100, #equal 100
                                                      1, NA)))
  )
Odds_Data <- Odds_Data %>%
  mutate(ConsensusFracoddsAway = ifelse(ConsensusAwayMoneyLine<0, #favorite
                                        100/abs(ConsensusAwayMoneyLine),
                                        ifelse(ConsensusAwayMoneyLine > 0, #underdog
                                               ConsensusAwayMoneyLine/100,
                                               ifelse(ConsensusAwayMoneyLine == 100, #equal 100
                                                      1, NA)))
  )




#JUST DK
##Frac to Euro
Odds_Data<- Odds_Data %>% 
  mutate(DKoddsHomeWin = DKFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(DKoddsAwayWin = DKFracoddsAway+1)

## Create Lambda
Odds_Data<- Odds_Data %>% 
  mutate(LambdaDK= ((1/DKoddsHomeWin)+(1/DKoddsAwayWin)))

## Scale the Odds
Odds_Data<- Odds_Data %>% 
  mutate(DKScaledOddsHome= (LambdaDK*DKoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(DKScaledOddsAway= (LambdaDK*DKoddsAwayWin)-1)

## Log Sclaed Odds
Odds_Data<- Odds_Data %>% 
  mutate(DKLogOddsHome= log(DKScaledOddsHome))

Odds_Data<- Odds_Data %>% 
  mutate(DKLogOddsAway= log(DKScaledOddsAway))           


## Home Away variables
Odds_Data <- Odds_Data %>%
  mutate(HomeWin = ifelse(HomeTeamHasWon == TRUE,1,0))
Odds_Data <- Odds_Data %>%
  mutate(AwayWin = ifelse(AwayTeamHasWon== TRUE,1,0))

## Logit 
HomeDK<- glm(HomeWin~DKLogOddsHome, data = Odds_Data, family= binomial(link=logit))
AwayDK<- glm(AwayWin~DKLogOddsAway, data = Odds_Data, family= binomial(link=logit))

## Prepare Wald
Odds_Data <- Odds_Data %>%
  mutate(lDKoddsHome = log(DKScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lDKoddsHome2 = log(DKScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lDKoddsAway = log(DKScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lDKoddsAway2 = log(DKScaledOddsAway)^2
  )

## Wald Test
DKHome_model <- glm(HomeWin~lDKoddsHome, data = Odds_Data, family= binomial(link=logit))

## Wald test needs either a Terms arg if you are testing single constraints
DKHomewald.model <- wald.test(b = coef(DKHome_model), varb = vcov(DKHome_model), Terms = 1:2, H0 = c(0, -1))
DKHomewald.model$result$chi2

DKAway_model <- glm(AwayWin~lDKoddsAway, data = Odds_Data, family= binomial(link=logit))
DKAwaywald.model <- wald.test(b = coef(DKAway_model), varb = vcov(DKAway_model), Terms = 1:2, H0 = c(0, -1))
DKAwaywald.model$result$chi2





#Add One back in 

Odds_Data<- Odds_Data %>% 
  mutate(DKoddsHomeWin = DKFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(DKoddsAwayWin = DKFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(FanDueloddsHomeWin = FanDuelFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(FanDueloddsAwayWin = FanDuelFracoddsAway+1)


Odds_Data <- Odds_Data %>% 
  mutate(SugarHousePAoddsHomeWin = SugarHousePAFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(SugarHousePAoddsAwayWin = SugarHousePAFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(ParxoddsHomeWin = ParxFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(ParxoddsAwayWin = ParxFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(SportNJoddsHomeWin = SportNJFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(SportNJoddsAwayWin = SportNJFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(RiversCasinoPAoddsHomeWin = RiversCasinoPAFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(RiversCasinoPAoddsAwayWin = RiversCasinoPAFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(CaesarsoddsHomeWin = CaesarsFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(CaesarsoddsAwayWin = CaesarsFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(PointsBetoddsHomeWin = PointsBetFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(PointsBetoddsAwayWin = PointsBetFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(BetMGModdsHomeWin = BetMGMFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(BetMGModdsAwayWin = BetMGMFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(UnibetoddsHomeWin = UnibetFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(UnibetoddsAwayWin = UnibetFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(BarstooloddsHomeWin = BarstoolFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(BarstooloddsAwayWin = BarstoolFracoddsAway+1)

Odds_Data <- Odds_Data %>% 
  mutate(ConsensusoddsHomeWin = ConsensusFracoddsHome+1)

Odds_Data <- Odds_Data %>%
  mutate(ConsensusoddsAwayWin = ConsensusFracoddsAway+1)



#Lambda

Odds_Data<- Odds_Data %>% 
  mutate(LambdaDK= (1/DKoddsHomeWin) +(1/DKoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaFanDuel= (1/FanDueloddsHomeWin) +(1/FanDueloddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaSugarHousePA= (1/SugarHousePAoddsHomeWin) +(1/SugarHousePAoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaParx= (1/ParxoddsHomeWin) +(1/ParxoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaSportNJ= (1/SportNJoddsHomeWin) +(1/SportNJoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaRiversCasinoPA= (1/RiversCasinoPAoddsHomeWin) +(1/RiversCasinoPAoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaCaesars= (1/CaesarsoddsHomeWin) +(1/CaesarsoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaPointsBet= (1/PointsBetoddsHomeWin) +(1/PointsBetoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaBetMGM= (1/BetMGModdsHomeWin) +(1/BetMGModdsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaUnibet= (1/UnibetoddsHomeWin) +(1/UnibetoddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaBarstool= (1/BarstooloddsHomeWin) +(1/BarstooloddsAwayWin))

Odds_Data<- Odds_Data %>% 
  mutate(LambdaConsensus= (1/ConsensusoddsHomeWin) +(1/ConsensusoddsAwayWin))




#Scaled Odds

Odds_Data<- Odds_Data %>% 
  mutate(DKScaledOddsHome= (LambdaDK*DKoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(DKScaledOddsAway= (LambdaDK*DKoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(FanDuelScaledOddsHome= (LambdaFanDuel*FanDueloddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(FanDuelScaledOddsAway= (LambdaFanDuel*FanDueloddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(SugarHousePAScaledOddsHome= (LambdaSugarHousePA*SugarHousePAoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(SugarHousePAScaledOddsAway= (LambdaSugarHousePA*SugarHousePAoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(ParxScaledOddsHome= (LambdaParx*ParxoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(ParxScaledOddsAway= (LambdaParx*ParxoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(SportNJScaledOddsHome= (LambdaSportNJ*SportNJoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(SportNJScaledOddsAway= (LambdaSportNJ*SportNJoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(RiversCasinoPAScaledOddsHome= (LambdaRiversCasinoPA*RiversCasinoPAoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(RiversCasinoPAScaledOddsAway= (LambdaRiversCasinoPA*RiversCasinoPAoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(CaesarsScaledOddsHome= (LambdaCaesars*CaesarsoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(CaesarsScaledOddsAway= (LambdaCaesars*CaesarsoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(PointsBetScaledOddsHome= (LambdaPointsBet*PointsBetoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(PointsBetScaledOddsAway= (LambdaPointsBet*PointsBetoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(BetMGMScaledOddsHome= (LambdaBetMGM*BetMGModdsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(BetMGMScaledOddsAway= (LambdaBetMGM*BetMGModdsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(UnibetScaledOddsHome= (LambdaUnibet*UnibetoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(UnibetScaledOddsAway= (LambdaUnibet*UnibetoddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(BarstoolScaledOddsHome= (LambdaBarstool*BarstooloddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(BarstoolScaledOddsAway= (LambdaBarstool*BarstooloddsAwayWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(ConsensusScaledOddsHome= (LambdaConsensus*ConsensusoddsHomeWin)-1)

Odds_Data<- Odds_Data %>% 
  mutate(ConsensusScaledOddsAway= (LambdaConsensus*ConsensusoddsAwayWin)-1)



Odds_Data<- Odds_Data %>% 
  mutate(DKLogOddsHome= log(DKScaledOddsHome),DKLogOddsAway=log(DKScaledOddsAway),FanDuelLogOddsHome=log(FanDuelScaledOddsHome),FanDuelLogOddsAway=log(FanDuelScaledOddsAway),
         SugarHousePALogOddsHome=log(SugarHousePAScaledOddsHome),SugarHousePALogOddsAway=log(SugarHousePAScaledOddsAway),
         ParxLogOddsHome=log(ParxScaledOddsHome),ParxLogOddsAway=log(ParxScaledOddsAway),
         SportNJLogOddsHome=log(SportNJScaledOddsHome),SportNJLogOddsAway=log(SportNJScaledOddsAway),
         RiversCasinoPALogOddsHome=log(RiversCasinoPAScaledOddsHome),RiversCasinoPALogOddsAway=log(RiversCasinoPAScaledOddsAway),
         CaesarsLogOddsHome=log(CaesarsScaledOddsHome),CaesarsLogOddsAway=log(CaesarsScaledOddsAway),
         PointsBetLogOddsHome=log(PointsBetScaledOddsHome),PointsBetLogOddsAway=log(PointsBetScaledOddsAway),
         BetMGMLogOddsHome=log(BetMGMScaledOddsHome),BetMGMLogOddsAway=log(BetMGMScaledOddsAway),
         UnibetLogOddsHome=log(UnibetScaledOddsHome),UnibetLogOddsAway=log(UnibetScaledOddsAway),
         BarstoolLogOddsHome=log(BarstoolScaledOddsHome),BarstoolLogOddsAway=log(BarstoolScaledOddsAway),
         ConsensusLogOddsHome=log(ConsensusScaledOddsHome),ConsensusLogOddsAway=log(ConsensusScaledOddsAway))



Odds_Data <- Odds_Data %>%
  mutate(HomeWin = ifelse(HomeTeamHasWon == TRUE,1,0))
Odds_Data <- Odds_Data %>%
  mutate(AwayWin = ifelse(AwayTeamHasWon== TRUE,1,0))




#Run the Joint Test:


## Make some new variables
## DK
Odds_Data <- Odds_Data %>%
  mutate(lDKoddsHome = log(DKScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lDKoddsHome2 = log(DKScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lDKoddsAway = log(DKScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lDKoddsAway2 = log(DKScaledOddsAway)^2
  )

## FanDuel
Odds_Data <- Odds_Data %>%
  mutate(lFanDueloddsHome = log(FanDuelScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lFanDueloddsHome2 = log(FanDuelScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lFanDueloddsAway = log(FanDuelScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lFanDueloddsAway2 = log(FanDuelScaledOddsAway)^2
  )

## SugarHousePa
Odds_Data <- Odds_Data %>%
  mutate(lSugarHousePAoddsHome = log(SugarHousePAScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lSugarHousePAoddsHome2 = log(SugarHousePAScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lSugarHousePAoddsAway = log(SugarHousePAScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lSugarHousePAoddsAway2 = log(SugarHousePAScaledOddsAway)^2
  )

## Parx
Odds_Data <- Odds_Data %>%
  mutate(lParxoddsHome = log(ParxScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lParxoddsHome2 = log(ParxScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lParxoddsAway = log(ParxScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lParxoddsAway2 = log(ParxScaledOddsAway)^2
  )

## SportNJ
Odds_Data <- Odds_Data %>%
  mutate(lSportNJoddsHome = log(SportNJScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lSportNJoddsHome2 = log(SportNJScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lSportNJoddsAway = log(SportNJScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lSportNJoddsAway2 = log(SportNJScaledOddsAway)^2
  )

## RiversCasinoPA
Odds_Data <- Odds_Data %>%
  mutate(lRiversCasinoPAoddsHome = log(RiversCasinoPAScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lRiversCasinoPAoddsHome2 = log(RiversCasinoPAScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lRiversCasinoPAoddsAway = log(RiversCasinoPAScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lRiversCasinoPAoddsAway2 = log(RiversCasinoPAScaledOddsAway)^2
  )

## Caesars
Odds_Data <- Odds_Data %>%
  mutate(lCaesarsoddsHome = log(CaesarsScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lCaesarsoddsHome2 = log(CaesarsScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lCaesarsoddsAway = log(CaesarsScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lCaesarsoddsAway2 = log(CaesarsScaledOddsAway)^2
  )

## PointsBet
Odds_Data <- Odds_Data %>%
  mutate(lPointsBetoddsHome = log(PointsBetScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lPointsBetoddsHome2 = log(PointsBetScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lPointsBetoddsAway = log(PointsBetScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lPointsBetoddsAway2 = log(PointsBetScaledOddsAway)^2
  )

## BetMGM
Odds_Data <- Odds_Data %>%
  mutate(lBetMGModdsHome = log(BetMGMScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lBetMGModdsHome2 = log(BetMGMScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lBetMGModdsAway = log(BetMGMScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lBetMGModdsAway2 = log(BetMGMScaledOddsAway)^2
  )

## Unibet
Odds_Data <- Odds_Data %>%
  mutate(lUnibetoddsHome = log(UnibetScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lUnibetoddsHome2 = log(UnibetScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lUnibetoddsAway = log(UnibetScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lUnibetoddsAway2 = log(UnibetScaledOddsAway)^2
  )

## Barstool
Odds_Data <- Odds_Data %>%
  mutate(lBarstooloddsHome = log(BarstoolScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lBarstooloddsHome2 = log(BarstoolScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lBarstooloddsAway = log(BarstoolScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lBarstooloddsAway2 = log(BarstoolScaledOddsAway)^2
  )

## Consensus
Odds_Data <- Odds_Data %>%
  mutate(lConsensusoddsHome = log(ConsensusScaledOddsHome),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lConsensusoddsHome2 = log(ConsensusScaledOddsHome)^2
  )
Odds_Data <- Odds_Data %>%
  mutate(lConsensusoddsAway = log(ConsensusScaledOddsAway),
         S22 = ifelse(Season == 2022,1,0),
         S19 = ifelse(Season == 2019,1,0),
         S20 = ifelse(Season == 2020,1,0),
         S21 = ifelse(Season == 2021,1,0),
         lConsensusoddsAway2 = log(ConsensusScaledOddsAway)^2
  )




#Wald Test


## Run the glm model first
## DK Model
DKHome_model <- glm(HomeWin~lDKoddsHome, data = Odds_Data, family= binomial(link=logit))
estimates <- (DKHome_model$coefficients)
estimates <-cbind(t(DKHome_model$coefficients),summary(DKHome_model)$coefficients[1,4])
## construct p for B1
tstats <- cbind((summary(DKHome_model)$coefficients[2,1] + 1)/summary(DKHome_model)$coefficients[2,2],
                summary(DKHome_model)$df[2])
## Wald test needs either a Terms arg if you are testing single constraints
DKHomewald.model <- wald.test(b = coef(DKHome_model), varb = vcov(DKHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <-DKHomewald.model$result$chi2[3]


## FanDuel Model
FanDuelHome_model <- glm(HomeWin~lFanDueloddsHome, data = Odds_Data, family= binomial(link=logit))
FDestimates <-cbind(t(FanDuelHome_model$coefficients),summary(FanDuelHome_model)$coefficients[1,4])
estimates <- rbind(estimates, FDestimates)
## construct p for B1
tstats <- rbind(tstats, cbind((summary(FanDuelHome_model)$coefficients[2,1] + 1)/summary(FanDuelHome_model)$coefficients[2,2],
                summary(FanDuelHome_model)$df[2]))

FanDuelHomewald.model <- wald.test(b = coef(FanDuelHome_model), varb = vcov(FanDuelHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, FanDuelHomewald.model$result$chi2[3])

## SugarHousePa Model
SugarHousePAHome_model <- glm(HomeWin~lSugarHousePAoddsHome, data = Odds_Data, family= binomial(link=logit))
SugarHousePAestimates <-cbind(t(SugarHousePAHome_model$coefficients),summary(SugarHousePAHome_model)$coefficients[1,4])
estimates <- rbind(estimates, SugarHousePAestimates)

#tstats <- rbind(tstats, (summary(SugarHousePAHome_model)$coefficients[2,1] + 1)/summary(SugarHousePAHome_model)$coefficients[2,2])

tstats <- rbind(tstats,cbind((summary(SugarHousePAHome_model)$coefficients[2,1] + 1)/summary(SugarHousePAHome_model)$coefficients[2,2],
                summary(SugarHousePAHome_model)$df[2]))

SugarHousePAHomewald.model <- wald.test(b = coef(SugarHousePAHome_model), varb = vcov(SugarHousePAHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, SugarHousePAHomewald.model$result$chi2[3])


## Parx Model
ParxHome_model <- glm(HomeWin~lParxoddsHome, data = Odds_Data, family= binomial(link=logit))
Parxestimates <-cbind(t(ParxHome_model$coefficients),summary(ParxHome_model)$coefficients[1,4])
estimates <- rbind(estimates, Parxestimates)

#tstats <- rbind(tstats, (summary(ParxHome_model)$coefficients[2,1] + 1)/summary(ParxHome_model)$coefficients[2,2])
tstats <- rbind(tstats,cbind((summary(ParxHome_model)$coefficients[2,1] + 1)/summary(ParxHome_model)$coefficients[2,2],
                summary(ParxHome_model)$df[2]))
ParxHomewald.model <- wald.test(b = coef(ParxHome_model), varb = vcov(ParxHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, ParxHomewald.model$result$chi2[3])


## SportNJ Model
SportNJHome_model <- glm(HomeWin~lSportNJoddsHome, data = Odds_Data, family= binomial(link=logit))
SportNJestimates <-cbind(t(SportNJHome_model$coefficients),summary(SportNJHome_model)$coefficients[1,4])
estimates <- rbind(estimates, SportNJestimates)

#tstats <- rbind(tstats, (summary(SportNJHome_model)$coefficients[2,1] + 1)/summary(SportNJHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(SportNJHome_model)$coefficients[2,1] + 1)/summary(SportNJHome_model)$coefficients[2,2],
                summary(SportNJHome_model)$df[2]))
SportNJHomewald.model <- wald.test(b = coef(SportNJHome_model), varb = vcov(SportNJHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, SportNJHomewald.model$result$chi2[3])


## RiversCasino Model
RiversCasinoPAHome_model <- glm(HomeWin~lRiversCasinoPAoddsHome, data = Odds_Data, family= binomial(link=logit))
RiversCasinoPAestimates <-cbind(t(RiversCasinoPAHome_model$coefficients),summary(RiversCasinoPAHome_model)$coefficients[1,4])
estimates <- rbind(estimates, RiversCasinoPAestimates)
#tstats <- rbind(tstats, (summary(RiversCasinoPAHome_model)$coefficients[2,1] + 1)/summary(RiversCasinoPAHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(DKHome_model)$coefficients[2,1] + 1)/summary(DKHome_model)$coefficients[2,2],
                summary(DKHome_model)$df[2]))
RiversCasinoPAHomewald.model <- wald.test(b = coef(RiversCasinoPAHome_model), varb = vcov(RiversCasinoPAHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, RiversCasinoPAHomewald.model$result$chi2[3])

## Caesars Model
CaesarsHome_model <- glm(HomeWin~lCaesarsoddsHome, data = Odds_Data, family= binomial(link=logit))
Caesarsestimates <-cbind(t(CaesarsHome_model$coefficients),summary(CaesarsHome_model)$coefficients[1,4])
estimates <- rbind(estimates, Caesarsestimates)
#tstats <- rbind(tstats, (summary(CaesarsHome_model)$coefficients[2,1] + 1)/summary(CaesarsHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(CaesarsHome_model)$coefficients[2,1] + 1)/summary(CaesarsHome_model)$coefficients[2,2],
                summary(CaesarsHome_model)$df[2]))
CaesarsHomewald.model <- wald.test(b = coef(CaesarsHome_model), varb = vcov(CaesarsHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, CaesarsHomewald.model$result$chi2[3])

## PointsBet Model
PointsBetHome_model <- glm(HomeWin~lPointsBetoddsHome, data = Odds_Data, family= binomial(link=logit))
PointsBetestimates <-cbind(t(PointsBetHome_model$coefficients),summary(PointsBetHome_model)$coefficients[1,4])
estimates <- rbind(estimates, PointsBetestimates)
#tstats <- rbind(tstats, (summary(PointsBetHome_model)$coefficients[2,1] + 1)/summary(PointsBetHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(PointsBetHome_model)$coefficients[2,1] + 1)/summary(PointsBetHome_model)$coefficients[2,2],
                summary(PointsBetHome_model)$df[2]))
PointsBetHomewald.model <- wald.test(b = coef(PointsBetHome_model), varb = vcov(PointsBetHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, PointsBetHomewald.model$result$chi2[3])


## BetMGM Model
BetMGMHome_model <- glm(HomeWin~lBetMGModdsHome, data = Odds_Data, family= binomial(link=logit))
BetMGMestimates <-cbind(t(BetMGMHome_model$coefficients),summary(BetMGMHome_model)$coefficients[1,4])
estimates <- rbind(estimates, BetMGMestimates)
#tstats <- rbind(tstats, (summary(BetMGMHome_model)$coefficients[2,1] + 1)/summary(BetMGMHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(BetMGMHome_model)$coefficients[2,1] + 1)/summary(BetMGMHome_model)$coefficients[2,2],
                summary(BetMGMHome_model)$df[2]))
BetMGMHomewald.model <- wald.test(b = coef(BetMGMHome_model), varb = vcov(BetMGMHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, BetMGMHomewald.model$result$chi2[3])

## Unibet Model
UnibetHome_model <- glm(HomeWin~lUnibetoddsHome, data = Odds_Data, family= binomial(link=logit))
Unibetestimates <-cbind(t(UnibetHome_model$coefficients),summary(UnibetHome_model)$coefficients[1,4])
estimates <- rbind(estimates, Unibetestimates)
#tstats <- rbind(tstats, (summary(UnibetHome_model)$coefficients[2,1] + 1)/summary(UnibetHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(UnibetHome_model)$coefficients[2,1] + 1)/summary(UnibetHome_model)$coefficients[2,2],
                summary(UnibetHome_model)$df[2]))
UnibetHomewald.model <- wald.test(b = coef(UnibetHome_model), varb = vcov(UnibetHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, UnibetHomewald.model$result$chi2[3])


## Barstool Model
BarstoolHome_model <- glm(HomeWin~lBarstooloddsHome, data = Odds_Data, family= binomial(link=logit))
Barstoolestimates <-cbind(t(BarstoolHome_model$coefficients),summary(BarstoolHome_model)$coefficients[1,4])
estimates <- rbind(estimates, Barstoolestimates)
#tstats <- rbind(tstats, (summary(BarstoolHome_model)$coefficients[2,1] + 1)/summary(BarstoolHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(BarstoolHome_model)$coefficients[2,1] + 1)/summary(BarstoolHome_model)$coefficients[2,2],
                summary(BarstoolHome_model)$df[2]))
BarstoolHomewald.model <- wald.test(b = coef(BarstoolHome_model), varb = vcov(BarstoolHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, BarstoolHomewald.model$result$chi2[3])


## Consensus Model
ConsensusHome_model <- glm(HomeWin~lConsensusoddsHome, data = Odds_Data, family= binomial(link=logit))
Consensusestimates <-cbind(t(ConsensusHome_model$coefficients),summary(ConsensusHome_model)$coefficients[1,4])
estimates <- rbind(estimates, Consensusestimates)
#tstats <- rbind(tstats, (summary(ConsensusHome_model)$coefficients[2,1] + 1)/summary(ConsensusHome_model)$coefficients[2,2])
tstats <- rbind(tstats, cbind((summary(ConsensusHome_model)$coefficients[2,1] + 1)/summary(ConsensusHome_model)$coefficients[2,2],
                summary(ConsensusHome_model)$df[2]))
ConsensusHomewald.model <- wald.test(b = coef(ConsensusHome_model), varb = vcov(ConsensusHome_model), Terms = 1:2, H0 = c(0, -1))
pwald <- rbind(pwald, ConsensusHomewald.model$result$chi2[3])




#Separate By Year

#Efficient Books:

## DK
DK_model19 <- glm(HomeWin~lDKoddsHome, data = Odds_Data, subset=Odds_Data$S19==1, family= binomial(link=logit))
DK_model20 <- glm(HomeWin~lDKoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
DK_model21 <- glm(HomeWin~lDKoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
DK_model22 <- glm(HomeWin~lDKoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

DKwald.model19 <- wald.test(b = coef(DK_model19), varb = vcov(DK_model19), Terms = 1:2, H0 = c(0, -1))
DKwald.model19$result$chi2
DKwald.model20 <- wald.test(b = coef(DK_model20), varb = vcov(DK_model20), Terms = 1:2, H0 = c(0, -1))
DKwald.model20$result$chi2
DKwald.model21 <- wald.test(b = coef(DK_model21), varb = vcov(DK_model21), Terms = 1:2, H0 = c(0, -1))
DKwald.model21$result$chi2
DKwald.model22 <- wald.test(b = coef(DK_model22), varb = vcov(DK_model22), Terms = 1:2, H0 = c(0, -1))
DKwald.model22$result$chi2[3]
pvalueWald22 <- DKwald.model22$result$chi2[3]

##FanDuel
FanDuel_model19 <- glm(HomeWin~lFanDueloddsHome, data = Odds_Data, subset=Odds_Data$S19==1, family= binomial(link=logit))
FanDuel_model20 <- glm(HomeWin~lFanDueloddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
FanDuel_model21 <- glm(HomeWin~lFanDueloddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
FanDuel_model22 <- glm(HomeWin~lFanDueloddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

FanDuelwald.model19 <- wald.test(b = coef(FanDuel_model19), varb = vcov(FanDuel_model19), Terms = 1:2, H0 = c(0, -1))
FanDuelwald.model19$result$chi2
FanDuelwald.model20 <- wald.test(b = coef(FanDuel_model20), varb = vcov(FanDuel_model20), Terms = 1:2, H0 = c(0, -1))
FanDuelwald.model20$result$chi2
FanDuelwald.model21 <- wald.test(b = coef(FanDuel_model21), varb = vcov(FanDuel_model21), Terms = 1:2, H0 = c(0, -1))
FanDuelwald.model21$result$chi2
FanDuelwald.model22 <- wald.test(b = coef(FanDuel_model22), varb = vcov(FanDuel_model22), Terms = 1:2, H0 = c(0, -1))
FanDuelwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, FanDuelwald.model22$result$chi2[3])

##SugarHousePA

SugarHousePA_model20 <- glm(HomeWin~lSugarHousePAoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
SugarHousePA_model21 <- glm(HomeWin~lSugarHousePAoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
SugarHousePA_model22 <- glm(HomeWin~lSugarHousePAoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))


SugarHousePAwald.model20 <- wald.test(b = coef(SugarHousePA_model20), varb = vcov(SugarHousePA_model20), Terms = 1:2, H0 = c(0, -1))
SugarHousePAwald.model20$result$chi2
SugarHousePAwald.model21 <- wald.test(b = coef(SugarHousePA_model21), varb = vcov(SugarHousePA_model21), Terms = 1:2, H0 = c(0, -1))
SugarHousePAwald.model21$result$chi2
SugarHousePAwald.model22 <- wald.test(b = coef(SugarHousePA_model22), varb = vcov(SugarHousePA_model22), Terms = 1:2, H0 = c(0, -1))
SugarHousePAwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, SugarHousePAwald.model22$result$chi2[3])

## Parx
Parx_model20 <- glm(HomeWin~lParxoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
Parx_model21 <- glm(HomeWin~lParxoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
Parx_model22 <- glm(HomeWin~lParxoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

Parxwald.model20 <- wald.test(b = coef(Parx_model20), varb = vcov(Parx_model20), Terms = 1:2, H0 = c(0, -1))
Parxwald.model20$result$chi2
Parxwald.model21 <- wald.test(b = coef(Parx_model21), varb = vcov(Parx_model21), Terms = 1:2, H0 = c(0, -1))
Parxwald.model21$result$chi2
Parxwald.model22 <- wald.test(b = coef(Parx_model22), varb = vcov(Parx_model22), Terms = 1:2, H0 = c(0, -1))
Parxwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, Parxwald.model22$result$chi2[3])

##SportNJ
SportNJ_model20 <- glm(HomeWin~lSportNJoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
SportNJ_model21 <- glm(HomeWin~lSportNJoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
SportNJ_model22 <- glm(HomeWin~lSportNJoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

SportNJwald.model20 <- wald.test(b = coef(SportNJ_model20), varb = vcov(SportNJ_model20), Terms = 1:2, H0 = c(0, -1))
SportNJwald.model20$result$chi2
SportNJwald.model21 <- wald.test(b = coef(SportNJ_model21), varb = vcov(SportNJ_model21), Terms = 1:2, H0 = c(0, -1))
SportNJwald.model21$result$chi2
SportNJwald.model22 <- wald.test(b = coef(SportNJ_model22), varb = vcov(SportNJ_model22), Terms = 1:2, H0 = c(0, -1))
SportNJwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, SportNJwald.model22$result$chi2[3])

## RiversCasino
RiversCasino_model20 <- glm(HomeWin~lRiversCasinoPAoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
RiversCasino_model21 <- glm(HomeWin~lRiversCasinoPAoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
RiversCasino_model22 <- glm(HomeWin~lRiversCasinoPAoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

RiversCasinowald.model20 <- wald.test(b = coef(RiversCasino_model20), varb = vcov(RiversCasino_model20), Terms = 1:2, H0 = c(0, -1))
RiversCasinowald.model20$result$chi2
RiversCasinowald.model21 <- wald.test(b = coef(RiversCasino_model21), varb = vcov(RiversCasino_model21), Terms = 1:2, H0 = c(0, -1))
RiversCasinowald.model21$result$chi2
RiversCasinowald.model22 <- wald.test(b = coef(RiversCasino_model22), varb = vcov(RiversCasino_model22), Terms = 1:2, H0 = c(0, -1))
RiversCasinowald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, RiversCasinowald.model22$result$chi2[3])

## PointsBet Model
PointsBet_model20 <- glm(HomeWin~lPointsBetoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
PointsBet_model21 <- glm(HomeWin~lPointsBetoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
PointsBet_model22 <- glm(HomeWin~lPointsBetoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

PointsBetwald.model20 <- wald.test(b = coef(PointsBet_model20), varb = vcov(PointsBet_model20), Terms = 1:2, H0 = c(0, -1))
PointsBetwald.model20$result$chi2
PointsBetwald.model21 <- wald.test(b = coef(PointsBet_model21), varb = vcov(PointsBet_model21), Terms = 1:2, H0 = c(0, -1))
PointsBetwald.model21$result$chi2
PointsBetwald.model22 <- wald.test(b = coef(PointsBet_model22), varb = vcov(PointsBet_model22), Terms = 1:2, H0 = c(0, -1))
PointsBetwald.model22$result$chi2
pvalueWald22 <- rbind(pvalueWald22, PointsBetwald.model22$result$chi2[3])

## Barstool Model

Barstool_model22 <- glm(HomeWin~lBarstooloddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))


Barstoolwald.model22 <- wald.test(b = coef(Barstool_model22), varb = vcov(Barstool_model22), Terms = 1:2, H0 = c(0, -1))
Barstoolwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, Barstoolwald.model22$result$chi2[3])

## Consensus Model
Consensus_model19 <- glm(HomeWin~lConsensusoddsHome, data = Odds_Data, subset=Odds_Data$S19==1, family= binomial(link=logit))
Consensus_model20 <- glm(HomeWin~lConsensusoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
Consensus_model21 <- glm(HomeWin~lConsensusoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
Consensus_model22 <- glm(HomeWin~lConsensusoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))

Consensuswald.model19 <- wald.test(b = coef(Consensus_model19), varb = vcov(Consensus_model19), Terms = 1:2, H0 = c(0, -1))
Consensuswald.model19$result$chi2
Consensuswald.model20 <- wald.test(b = coef(Consensus_model20), varb = vcov(Consensus_model20), Terms = 1:2, H0 = c(0, -1))
Consensuswald.model20$result$chi2
Consensuswald.model21 <- wald.test(b = coef(Consensus_model21), varb = vcov(Consensus_model21), Terms = 1:2, H0 = c(0, -1))
Consensuswald.model21$result$chi2
Consensuswald.model22 <- wald.test(b = coef(Consensus_model22), varb = vcov(Consensus_model22), Terms = 1:2, H0 = c(0, -1))
Consensuswald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, Consensuswald.model22$result$chi2[3])


#InEfficient Books:
  

## Caesars Model

Caesars_model21 <- glm(HomeWin~lCaesarsoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
Caesars_model22 <- glm(HomeWin~lCaesarsoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))


Caesarswald.model21 <- wald.test(b = coef(Caesars_model21), varb = vcov(Caesars_model21), Terms = 1:2, H0 = c(0, -1))
Caesarswald.model21$result$chi2
Caesarswald.model22 <- wald.test(b = coef(Caesars_model22), varb = vcov(Caesars_model22), Terms = 1:2, H0 = c(0, -1))
Caesarswald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, Caesarswald.model22$result$chi2[3])

##BetMGM
BetMGM_model20 <- glm(HomeWin~lBetMGModdsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
BetMGM_model21 <- glm(HomeWin~lBetMGModdsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
BetMGM_model22 <- glm(HomeWin~lBetMGModdsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))


BetMGMwald.model20 <- wald.test(b = coef(BetMGM_model20), varb = vcov(BetMGM_model20), Terms = 1:2, H0 = c(0, -1))
BetMGMwald.model20$result$chi2
BetMGMwald.model21 <- wald.test(b = coef(BetMGM_model21), varb = vcov(BetMGM_model21), Terms = 1:2, H0 = c(0, -1))
BetMGMwald.model21$result$chi2
BetMGMwald.model22 <- wald.test(b = coef(BetMGM_model22), varb = vcov(BetMGM_model22), Terms = 1:2, H0 = c(0, -1))
BetMGMwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, BetMGMwald.model22$result$chi2[3])

##Unibet
Unibet_model20 <- glm(HomeWin~lUnibetoddsHome, data = Odds_Data, subset=Odds_Data$S20==1, family= binomial(link=logit))
Unibet_model21 <- glm(HomeWin~lUnibetoddsHome, data = Odds_Data, subset=Odds_Data$S21==1, family= binomial(link=logit))
Unibet_model22 <- glm(HomeWin~lUnibetoddsHome, data = Odds_Data, subset=Odds_Data$S22==1, family= binomial(link=logit))


Unibetwald.model20 <- wald.test(b = coef(Unibet_model20), varb = vcov(Unibet_model20), Terms = 1:2, H0 = c(0, -1))
Unibetwald.model20$result$chi2
Unibetwald.model21 <- wald.test(b = coef(Unibet_model21), varb = vcov(Unibet_model21), Terms = 1:2, H0 = c(0, -1))
Unibetwald.model21$result$chi2
Unibetwald.model22 <- wald.test(b = coef(Unibet_model22), varb = vcov(Unibet_model22), Terms = 1:2, H0 = c(0, -1))
Unibetwald.model22$result$chi2[3]
pvalueWald22 <- rbind(pvalueWald22, Unibetwald.model22$result$chi2[3])



#(Beta1 - 1)/se(Beta1)

## DK
-.116/0.07500
##FanDuel
-.121/(0.074) 
## SugarHousePA
-.133/(0.080)  
##Parx
-.145/(0.081)
##SportNJ
-.075/(0.102) 
##RiversCasinoPA
-.133/(0.080)
##PointsBet
-.156/(0.082)
##BetMGM
-.237/(0.093)
##UniBet
-.196/(0.093) 
##Barstool
-.181/(0.138)
##Caesars
-.229/ (0.103)    
##Consensus
-.124/(0.074)


#Create Data Frame with these variables

## Data Frame name Beta1_Data
tstats <- c("-1.547", "-1.635","-1.6625", "-1.7901", "-0.7353", "-1.6625", "-1.9024", "-2.5484", "-2.1075", "-1.3116", "-2.2233", "-1.6757")
df <- c("3731", "3916", "3303", "3303","2446", "3303", "3176", "2378", "2418", "765", "1632", "3959")

Beta1_Data <- data.frame(tstats, df)




pvals <- function(x){
  x <- as.numeric(x)
  #print(x)
  pt(x[1], df = x[2])
}
apply(Beta1_Data, 1, FUN = pvals)




pt(-1.547, df = 3303)



#pvaluesB1 <- pvals(tstats)
pvaluesB1 <- pvals(tstats)



books= c("Draft Kings", "FanDuel", "SugarHousePA","Parx", "SportNJ", "RiversCasino", "Caesars", "PointsBet", "BetMGM", "Unibet", "Barstool", "Consensus")
#est.df<- data.frame (Book=books,
#beta_0= estimates[,1],
# pvalue_beta_0 = estimates[,3],
#beta_1= estimates [,2],
# pvalue_beta_1 = pvaluesB1,
# pvalue_wald= pwald
# )
est.df<- data.frame (Book=books,
                     beta_0= estimates[,1],
                     pvalue_beta_0 = estimates[,3],
                     beta_1= estimates [,2],
                     pvalue_beta_1= pvaluesB1,
                     pvalue_wald= pwald
)




#Chart Data

## Data Frame name Chart_Data
book <- c("DraftKings", "FanDuel", "SugarHousePA", "Parx", "SportNJ", "RiversCasino", "PointsBet", "BetMGM", "Unibet", "Barstool", "Caesars", "Consensus")
beta_0 <- c("-0.0350", "-0.0340","-0.028", "-0.028", "-0.005", "-0.028", "-0.031", "-0.004", "-0.003", "-0.103", "-0.037", "-0.035")
pvalue_beta_0 <- c("(0.322)", "(0.333)", "(0.461)", "(0.453)", "(0.91)", "(0.462)", "(0.425)", "(0.934)", "(0.952)", "(0.192)","(0.498)", "(0.311)")

##tstats
beta_1 <- c("-1.116", "-1.121","-1.133", "-1.145", "-1.075", "-1.133", "-1.156", "-1.237", "-1.196", "-1.181", "-1.229", "-1.125")
pvalue_beta_1 <- c("(0.0610)", "(0.0511)", "(0.0483)", "(0.0368)", "(0.2311)", "(0.0483)", "(0.0286)", "(0.0054)", "(0.0176)", "(0.0950)","(0.0132)", "(0.0469)")


pvalue_wald <- c("(0.2630)","(0.2333)", "(0.2445)", "(0.1988)", "(0.7574)", "(0.2461)", "(0.1570)", "(0.0341)", "(0.0902)", "(0.2603)", "(0.0835)", "(0.2212)")

Chart_Data <- data.frame(book, beta_0, pvalue_beta_0, beta_1, pvalue_beta_1, pvalue_wald)

Chart_data1 <- unite(Chart_Data, c(2,3), col = "Beta0 (p-value)", sep = "    ") %>% 
  unite(., c(3,4), col = "Beta1 (p-value)", sep = "      ")
knitr::kable(Chart_data1 , caption = "Figure 1. Results")


#knitr::kable(data.frame(col1 = est.df[1], col2 = linebreak(est.df[2]), col3 = linebreak(est.df[3]), col4 = est.df[4]), caption = "Figure 1. Results")




book <- c("DraftKings", "FanDuel", "SugarHousePA", "Parx", "SportNJ", "RiversCasino", "PointsBet", "BetMGM", "Unibet", "Barstool", "Caesars", "Consensus")

pvalue_wald <- c("(0.061)","(0.092)", "(0.061)", "(0.045)", "(0.230)", "(0.062)", "(0.088)", "(0.047)", "(0.108)", "(0.260)", "(0.080)", "(0.059)")

Year_Data <- data.frame(book, pvalue_wald)

knitr::kable(Year_Data , caption = "2021-2022 Results")





#Test efficiency without 21-22

Yearly_Data <- Odds_Data %>%
  filter(S22 == 0)





DKHome_modelYear <- glm(HomeWin~lDKoddsHome, data = Yearly_Data, family= binomial(link=logit))

## Wald test needs either a Terms arg if you are testing single constraints
DKHomewald.modelYear <- wald.test(b = coef(DKHome_modelYear), varb = vcov(DKHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <-DKHomewald.modelYear$result$chi2[3]


## FanDuel Model
FanDuelHome_modelYear <- glm(HomeWin~lFanDueloddsHome, data = Yearly_Data, family= binomial(link=logit))
FanDuelHomewald.modelYear <- wald.test(b = coef(FanDuelHome_modelYear), varb = vcov(FanDuelHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, FanDuelHomewald.modelYear$result$chi2[3])

## SugarHousePa Model
SugarHousePAHome_modelYear <- glm(HomeWin~lSugarHousePAoddsHome, data = Yearly_Data, family= binomial(link=logit))
SugarHousePAHomewald.modelYear <- wald.test(b = coef(SugarHousePAHome_modelYear), varb = vcov(SugarHousePAHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, SugarHousePAHomewald.modelYear$result$chi2[3])
#SugarHousePAHomewald.modelYear$result$chi2


## Parx Model
ParxHome_modelYear <- glm(HomeWin~lParxoddsHome, data = Yearly_Data, family= binomial(link=logit))
ParxHomewald.modelYear <- wald.test(b = coef(ParxHome_modelYear), varb = vcov(ParxHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, ParxHomewald.modelYear$result$chi2[3])


## SportNJ Model
SportNJHome_modelYear <- glm(HomeWin~lSportNJoddsHome, data = Yearly_Data, family= binomial(link=logit))
SportNJHomewald.modelYear <- wald.test(b = coef(SportNJHome_modelYear), varb = vcov(SportNJHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, SportNJHomewald.modelYear$result$chi2[3])


## RiversCasino Model
RiversCasinoPAHome_modelYear <- glm(HomeWin~lRiversCasinoPAoddsHome, data = Yearly_Data, family= binomial(link=logit))
RiversCasinoPAHomewald.modelYear <- wald.test(b = coef(RiversCasinoPAHome_modelYear), varb = vcov(RiversCasinoPAHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, RiversCasinoPAHomewald.modelYear$result$chi2[3])


## Caesars Model
CaesarsHome_modelYear <- glm(HomeWin~lCaesarsoddsHome, data = Yearly_Data, family= binomial(link=logit))
CaesarsHomewald.modelYear <- wald.test(b = coef(CaesarsHome_modelYear), varb = vcov(CaesarsHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, CaesarsHomewald.modelYear$result$chi2[3])


## PointsBet Model
PointsBetHome_modelYear <- glm(HomeWin~lPointsBetoddsHome, data = Yearly_Data, family= binomial(link=logit))
PointsBetHomewald.modelYear <- wald.test(b = coef(PointsBetHome_modelYear), varb = vcov(PointsBetHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, PointsBetHomewald.modelYear$result$chi2[3])

## BetMGM Model
BetMGMHome_modelYear <- glm(HomeWin~lBetMGModdsHome, data = Yearly_Data, family= binomial(link=logit))
BetMGMHomewald.modelYear <- wald.test(b = coef(BetMGMHome_modelYear), varb = vcov(BetMGMHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, BetMGMHomewald.modelYear$result$chi2[3])


## Unibet Model
UnibetHome_modelYear <- glm(HomeWin~lUnibetoddsHome, data = Yearly_Data, family= binomial(link=logit))
UnibetHomewald.modelYear <- wald.test(b = coef(UnibetHome_modelYear), varb = vcov(UnibetHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, UnibetHomewald.modelYear$result$chi2[3])


## Consensus Model
ConsensusHome_modelYear <- glm(HomeWin~lConsensusoddsHome, data = Yearly_Data, family= binomial(link=logit))
ConsensusHomewald.modelYear <- wald.test(b = coef(ConsensusHome_modelYear), varb = vcov(ConsensusHome_modelYear), Terms = 1:2, H0 = c(0, -1))
pwaldYear <- rbind(pwaldYear, ConsensusHomewald.modelYear$result$chi2[3])




books= c("Draft Kings", "FanDuel", "SugarHousePA","Parx", "SportNJ", "RiversCasino", "Caesars", "PointsBet", "BetMGM", "Unibet", "Consensus")

year.df<- data.frame (Book=books,
                      pvalue_wald= pwaldYear
)



books= c("Draft Kings", "FanDuel", "SugarHousePA","Parx", "SportNJ", "RiversCasino", "PointsBet", "Barstool", "Consensus", "Caesars", "BetMGM", "Unibet")

year22.df<- data.frame (Book=books,
                        pvalue_wald= pvalueWald22
)




book <- c("DraftKings", "FanDuel", "SugarHousePA", "Parx", "SportNJ", "RiversCasino", "Caesars", "PointsBet", "BetMGM", "Unibet",  "Consensus")

pvalue_wald <- c("(0.8875)","(0.9060)", "(0.9903)", "(0.9898)", "(0.9896)", "(0.9904)", "(0.8034)", "(0.8839)", "(0.4235)", "(0.6404)", "(0.8850)")

Season_Data <- data.frame(book, pvalue_wald)

knitr::kable(Season_Data , caption = "Results w/o 2021/2022 Season")



#Make Graph

graph.data <- data.frame(Odds = seq(.1,5,by = .1))

graph.data <- graph.data %>%
  mutate(efficient = 1/(1 + exp(-1 * (0 + -1 * log(Odds)))),
         Caesars = 1/(1 + exp(-1 * (CaesarsHome_model$coefficients[1] + CaesarsHome_model$coefficients[2] * log(Odds))))
  )

graph.data <- graph.data %>%
  mutate(efficient = 1/(1 + exp(-1 * (0 + -1 * log(Odds)))),
         BetMGM = 1/(1 + exp(-1 * (BetMGMHome_model$coefficients[1] + BetMGMHome_model$coefficients[2] * log(Odds))))
  )

graph.data <- graph.data %>%
  mutate(efficient = 1/(1 + exp(-1 * (0 + -1 * log(Odds)))),
         Unibet = 1/(1 + exp(-1 * (UnibetHome_model$coefficients[1] + UnibetHome_model$coefficients[2] * log(Odds))))
  )

graph.data <- graph.data %>%
  gather(key = "Book", value = "Probability", efficient, Caesars, BetMGM, Unibet)





ggplot(graph.data, aes(x = Odds, y = Probability)) + 
  geom_line(aes(color = Book))


