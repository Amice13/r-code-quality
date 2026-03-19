### Chapter 4
rm(list=ls(all=TRUE))

### TASK 1: The Estimation of the Multilevel Models with the BLSs (Main Analysis)
### TASK 2: The Estimation of the Multilevel Models with the APEP (Post-Hoc Analysis-A)
###       : The Estimation of Interaction Effects of Controlling Variables on Equal Shares and Equal Changes (Post-Hoc Analysis-B)

library(MASS)

### TASK 1: The Estimation of the Multilevel Models with the BLSs (Main Analysis)
### Operationalizing Variables
MyData <- subset(bls7_released_v01, select = c(wave,                                                                                                        #Year
                                               party_elected,                                                                                               #Party affiliation
                                               region,                                                                                                      #Region (Clustering Unit)
                                               fidelit, believe, efforts, needmps_all, ffaaint, ppswitch_all,                                               #Variables on political issues
                                               econlmr,                                                                                                     #Variable on economic issues
                                               family, vereador, depest, senador, depfed, prefvice, govvice, ministro, secest, clients_all, ppvsreg,        #Variables on traditional issues
                                               lrclass                                                                                                      #Self-placement on the left-right scale
                                               ))

colnames(MyData) <- c("YEAR",
                   "PARTY",
                   "REGION",
                   "FIDELITY", "BELIEVE", "EFFORTS", "DECREE", "MILITARY", "SWITCH",
                   "ECONOMY",
                   "FAMILY", "COUNCILLOR", "STATEDEPUTY", "SENATOR", "FEDERALDEPUTY", "MAYOR", "GOVERNOR", "MINISTER", "SECRETARY", "CLIENTELISM", "LOCALINTERESTS",
                   "LEFTRIGHT")

MyData$PARTY[ MyData$PARTY == "99"] <- NA
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "99"] <- NA
MyData$FIDELITY[ MyData$FIDELITY == "9"] <- NA
MyData$DECREE[ MyData$DECREE == "9"] <- NA
MyData$BELIEVE[ MyData$BELIEVE == "9"] <- NA
MyData$BELIEVE[ MyData$BELIEVE == "8"] <- NA
MyData$EFFORTS[ MyData$EFFORTS == "9"] <- NA
MyData$EFFORTS[ MyData$EFFORTS == "8"] <- NA
MyData$MILITARY[ MyData$SWITCH == "9"] <- NA
MyData$MILITARY[ MyData$MILITARY == "9"] <- NA
MyData$ECONOMY[ MyData$ECONOMY == "9"] <- NA
MyData$CLIENTELISM[ MyData$CLIENTELISM == "9"] <- NA
MyData$LOCALINTERESTS[ MyData$LOCALINTERESTS == "3"] <- NA
MyData$LOCALINTERESTS[ MyData$LOCALINTERESTS == "9"] <- NA
MyData$FAMILY[ MyData$FAMILY == "9"] <- NA
MyData$COUNCILLOR[ MyData$COUNCILLOR == "9"] <- NA
MyData$STATEDEPUTY[ MyData$STATEDEPUTY == "9"] <- NA
MyData$SENATOR[ MyData$SENATOR == "9"] <- NA
MyData$FEDERALDEPUTY[ MyData$FEDERALDEPUTY == "9"] <- NA
MyData$MAYOR[ MyData$MAYOR == "9"] <- NA
MyData$GOVERNOR[ MyData$GOVERNOR == "9"] <- NA
MyData$MINISTER[ MyData$MINISTER == "9"] <- NA
MyData$SECRETARY[ MyData$SECRETARY == "9"] <- NA
MyData$REGION[ MyData$REGION == "9"] <- NA
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "3.5"] <- 3
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "5.5"] <- 5

# Coding the degree of party nationalization
unique(sort(MyData$PARTY))
#11 <- PP2003 4.304939 (Equal Share) and 27.0367 (Equal Change)
#12 <- PDT 8.897054 (Equal Share) and 17.1212 (Equal Change)
#13 <- PT 20.74353 (Equal Share) and 11.9215 (Equal Change)
#14 <- PTB 2.724552 (Equal Share) and 11.20572 (Equal Change)
#15 <- PMDB 36.88386 (Equal Share) and 35.67332 (Equal Change)
#20 <- PSC 0.3628846 (Equal Share) and 5.347685 (Equal Change)
#22 <- PR 3.930946 (Equal Share) and 9.936525 (Equal Change)
#23 <- PPS 5.38e-22 (Equal Share) and 1.063595 (Equal Change)
#25 <- PFL-DEM 47.35181 (Equal Share) and 43.30902 (Equal Change)
#33 <- PMN 0.4869283 (Equal Share) and 1.646082 (Equal Change)
#40 <- PSB 14.99037 (Equal Share) and 9.719556 (Equal Change)
#43 <- PV 1.61627 (Equal Share) and 1.994438 (Equal Change)
#45 <- PSDB 25.88173 (Equal Share) and 29.34851 (Equal Change)
#50 <- PSOL 1.182162 (Equal Share) and 1.009469 (Equal Change)
#65 <- PCdoB 0.8503498 (Equal Share) and 2.863987 (Equal Change)

MyData$ES <- NA
MyData$ES[ MyData$PARTY == "11"] <- 4.304939
MyData$ES[ MyData$PARTY == "13"] <- 20.74353 
MyData$ES[ MyData$PARTY == "15"] <- 36.88386
MyData$ES[ MyData$PARTY == "22"] <- 3.930946
MyData$ES[ MyData$PARTY == "40"] <- 14.99037
MyData$ES[ MyData$PARTY == "45"] <- 25.88173

MyData$EC <- NA
MyData$EC[ MyData$PARTY == "11"] <- 27.0367
MyData$EC[ MyData$PARTY == "13"] <- 11.9215 
MyData$EC[ MyData$PARTY == "15"] <- 35.67332
MyData$EC[ MyData$PARTY == "22"] <- 9.936525
MyData$EC[ MyData$PARTY == "40"] <- 9.719556
MyData$EC[ MyData$PARTY == "45"] <- 29.34851

MyData2 <- na.omit(MyData)

# Coding the regional variance of district magnitude
MyData2$DM <- NA
MyData2$DM[ MyData2$REGION == "1"] <- 9.918367347 #North
MyData2$DM[ MyData2$REGION == "2"] <- 97.0617284  #Northeast
MyData2$DM[ MyData2$REGION == "3"] <- 478.6875    #Southeast
MyData2$DM[ MyData2$REGION == "4"] <- 46.88888889 #South
MyData2$DM[ MyData2$REGION == "5"] <- 15.1875     #Centre-West


# Coding the regional variance of district numbers
MyData2$ED <- NA
MyData2$ED[ MyData2$REGION == "1"] <- 7 #North
MyData2$ED[ MyData2$REGION == "2"] <- 9 #Northeast
MyData2$ED[ MyData2$REGION == "3"] <- 4 #Southeast
MyData2$ED[ MyData2$REGION == "4"] <- 3 #South
MyData2$ED[ MyData2$REGION == "5"] <- 4 #Centre-West


# Coding the regional authority index
MyData2$RAI <- NA
MyData2$RAI[ MyData2$YEAR == "1997"] <- 21.5
MyData2$RAI[ MyData2$YEAR == "2001"] <- 19.5
MyData2$RAI[ MyData2$YEAR == "2005"] <- 19.5
MyData2$RAI[ MyData2$YEAR == "2009"] <- 19.5


# Coding the effective number of electoral parties
North <- MyData2[ which(MyData2$REGION == "1"),]
Northeast <- MyData2[ which(MyData2$REGION == "2"),]
Southeast <- MyData2[ which(MyData2$REGION == "3"),]
South <- MyData2[ which(MyData2$REGION == "4"),]
CentreWest <- MyData2[ which(MyData2$REGION == "5"),]

North$ENP<- NA
Northeast$ENP<- NA
Southeast$ENP<- NA
South$ENP<- NA
CentreWest$ENP<- NA

# - 1998 -
North$ENP[ North$YEAR == "1997"] <- 3.083383
Northeast$ENP[ Northeast$YEAR == "1997"] <- 3.965065
Southeast$ENP[ Southeast$YEAR == "1997"] <- 0.4797318
South$ENP[ South$YEAR == "1997"] <- 0.3673527
CentreWest$ENP[ CentreWest$YEAR == "1997"] <- 1.347972

# - 2002 -
North$ENP[ North$YEAR == "2001"] <- 1.727305
Northeast$ENP[ Northeast$YEAR == "2001"] <- 3.042864
Southeast$ENP[ Southeast$YEAR == "2001"] <- 1.571398
South$ENP[ South$YEAR == "2001"] <- 1.06751
CentreWest$ENP[ CentreWest$YEAR == "2001"] <- 1.571398

# - 2006 -
North$ENP[ North$YEAR == "2005"] <- 3.052043
Northeast$ENP[ Northeast$YEAR == "2005"] <- 2.112666
Southeast$ENP[ Southeast$YEAR == "2005"] <- 4.287599
South$ENP[ South$YEAR == "2005"] <- 0.9168819
CentreWest$ENP[ CentreWest$YEAR == "2005"] <- 0.7901674

# - 2010 -
North$ENP[ North$YEAR == "2009"] <- 2.080644
Northeast$ENP[ Northeast$YEAR == "2009"] <- 0.8554433
Southeast$ENP[ Southeast$YEAR == "2009"] <- 1.993163
South$ENP[ South$YEAR == "2009"] <- 2.802145
CentreWest$ENP[ CentreWest$YEAR == "2009"] <- 0.9314278

# - 2014 -
North$ENP[ North$YEAR == "2013"] <- 9.804636
Northeast$ENP[ Northeast$YEAR == "2013"] <- 4.643176
Southeast$ENP[ Southeast$YEAR == "2013"] <- 0.4860608
South$ENP[ South$YEAR == "2013"] <- 7.136502
CentreWest$ENP[ CentreWest$YEAR == "2013"] <- 7.949261


# Coding ethnic fractionalization
North$EthnicFractionalization <- NA
Northeast$EthnicFractionalization <- NA
Southeast$EthnicFractionalization <- NA
South$EthnicFractionalization <- NA
CentreWest$EthnicFractionalization <- NA

# -1998 - 
North$EthnicFractionalization[ North$YEAR == "1997"] <- 0.451659027
Northeast$EthnicFractionalization[ Northeast$YEAR == "1997"] <- 0.494894512
Southeast$EthnicFractionalization[ Southeast$YEAR == "1997"] <- 0.508943
South$EthnicFractionalization[ South$YEAR == "1997"] <- 0.293391
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "1997"] <- 0.545466

# -2002 - 
North$EthnicFractionalization[ North$YEAR == "2001"] <- 0.472878789
Northeast$EthnicFractionalization[ Northeast$YEAR == "2001"] <- 0.472878789
Southeast$EthnicFractionalization[ Southeast$YEAR == "2001"] <- 0.510037
South$EthnicFractionalization[ South$YEAR == "2001"] <- 0.297078
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2001"] <- 0.548522

# -2006 - 
North$EthnicFractionalization[ North$YEAR == "2005"] <- 0.460495074
Northeast$EthnicFractionalization[ Northeast$YEAR == "2005"] <- 0.460495074
Southeast$EthnicFractionalization[ Southeast$YEAR == "2005"] <- 0.542604
South$EthnicFractionalization[ South$YEAR == "2005"] <- 0.339256
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2005"] <- 0.556918

# -2010 - 
North$EthnicFractionalization[ North$YEAR == "2009"] <- 0.479708422
Northeast$EthnicFractionalization[ Northeast$YEAR == "2009"] <- 0.479708422
Southeast$EthnicFractionalization[ Southeast$YEAR == "2009"] <- 0.560931
South$EthnicFractionalization[ South$YEAR == "2009"] <- 0.362381
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2009"] <- 0.576055

# -2014 - 
North$EthnicFractionalization[ North$YEAR == "2013"] <- 0.465401922
Northeast$EthnicFractionalization[ Northeast$YEAR == "2013"] <- 0.465401922
Southeast$EthnicFractionalization[ Southeast$YEAR == "2013"] <- 0.575059
South$EthnicFractionalization[ South$YEAR == "2013"] <- 0.384956
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2013"] <- 0.572509


# Coding class fractionalization
North$ClassFractionalization <- NA
Northeast$ClassFractionalization <- NA
Southeast$ClassFractionalization <- NA
South$ClassFractionalization <- NA
CentreWest$ClassFractionalization <- NA

# -1998-
North$ClassFractionalization[ North$YEAR == "1997"] <- 0.864635546
Northeast$ClassFractionalization[ Northeast$YEAR == "1997"] <- 0.786392218
Southeast$ClassFractionalization[ Southeast$YEAR == "1997"] <- 0.868567191
South$ClassFractionalization[ South$YEAR == "1997"] <- 0.848399786
CentreWest$ClassFractionalization[ CentreWest$YEAR == "1997"] <- 0.855468597

# -2002-
North$ClassFractionalization[ North$YEAR == "2001"] <- 0.872388514
Northeast$ClassFractionalization[ Northeast$YEAR == "2001"] <- 0.808797479
Southeast$ClassFractionalization[ Southeast$YEAR == "2001"] <- 0.876767117
South$ClassFractionalization[ South$YEAR == "2001"] <- 0.839780885
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2001"] <- 0.884436044

# -2006-
North$ClassFractionalization[ North$YEAR == "2005"] <- 0.853243036
Northeast$ClassFractionalization[ Northeast$YEAR == "2005"] <- 0.825329965
Southeast$ClassFractionalization[ Southeast$YEAR == "2005"] <- 0.876845282
South$ClassFractionalization[ South$YEAR == "2005"] <- 0.844659801
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2005"] <- 0.887962334

# -2010-
North$ClassFractionalization[ North$YEAR == "2009"] <- 0.864250639
Northeast$ClassFractionalization[ Northeast$YEAR == "2009"] <- 0.856123458
Southeast$ClassFractionalization[ Southeast$YEAR == "2009"] <- 0.89520095
South$ClassFractionalization[ South$YEAR == "2009"] <- 0.859166596
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2009"] <- 0.900787258

# -2014-
North$ClassFractionalization[ North$YEAR == "2013"] <- 0.874565552
Northeast$ClassFractionalization[ Northeast$YEAR == "2013"] <- 0.865528143
Southeast$ClassFractionalization[ Southeast$YEAR == "2013"] <- 0.896487334
South$ClassFractionalization[ South$YEAR == "2013"] <- 0.859421454
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2013"] <- 0.898026609


# Binding regional sebsets
MD <- rbind(North, Northeast, Southeast, South, CentreWest)
MD2 <- na.omit(MD)

# Descriptive Statistics of the explanatory dataset
library(psych)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

describe(MD2)

getmode(MD2$FIDELITY)
getmode(MD2$DECREE)
getmode(MD2$BELIEVE)
getmode(MD2$EFFORTS)
getmode(MD2$SWITCH)
getmode(MD2$MILITARY)
getmode(MD2$ECONOMY)
getmode(MD2$CLIENTELISM)
getmode(MD2$`LOCAL INTERESTS`)
getmode(MD2$COUNCILLOR)
getmode(MD2$`STATE DEPUTY`)
getmode(MD2$SENATOR)
getmode(MD2$`FEDERAL DEPUTY`)
getmode(MD2$MAYOR)
getmode(MD2$GOVERNOR)
getmode(MD2$MINISTER)
getmode(MD2$SECRETARY)
getmode(MD2$FAMILY)
getmode(MD2$`LEFT RIGHT`)

# Estimating the explanatory model - Full Model
library(MASS)
library(rcompanion)
library(nlme)
library(r2glmm)
library(ggpubr)

EqualShare <- lme(ES ~ FIDELITY + DECREE + BELIEVE + EFFORTS + SWITCH + MILITARY +
                    ECONOMY +
                    CLIENTELISM + LOCALINTERESTS + COUNCILLOR + STATEDEPUTY + SENATOR + FEDERALDEPUTY + MAYOR + GOVERNOR + MINISTER + SECRETARY + FAMILY +
                    LEFTRIGHT + 
                    DM + ED + ENP + RAI + EthnicFractionalization + ClassFractionalization,
                  random = ~ 1 | REGION, data = MD2, method = "ML")
summary(EqualShare)
R2ES <- r2beta(EqualShare, partial = TRUE, method = "nsj", data = MD2)

EqualChange <- lme(EC ~ FIDELITY + DECREE + BELIEVE + EFFORTS + SWITCH + MILITARY +
                     ECONOMY +
                     CLIENTELISM + LOCALINTERESTS + COUNCILLOR + STATEDEPUTY + SENATOR + FEDERALDEPUTY + MAYOR + GOVERNOR + MINISTER + SECRETARY + FAMILY +
                     LEFTRIGHT + 
                     DM + ED + ENP + RAI + EthnicFractionalization + ClassFractionalization,
                   random = ~ FIDELITY + DECREE + BELIEVE + EFFORTS + LOCALINTERESTS | REGION, data = MD2, method = "ML")
summary(EqualChange)
R2EC <- r2beta(EqualChange, partial = TRUE, method = "nsj", data = MD2)

ModelES <- plot(x = R2ES) +
  theme_classic() +
  xlab("") +
  ggtitle("R-squared for Equal Shares Model") +
  ylab("R-squared (0 - 1)") +
  theme(text=element_text(size=12, 
                          #       family="Comic Sans MS"))
                          #       family="CM Roman"))
                          #       family="TT Times New Roman"))
                          #       family="Sans"))
                          family="serif"))

ModelEC <- plot(x = R2EC) +
  theme_classic() +
  xlab("") +
  ggtitle("R-squared for Equal Changes Model") +
  ylab("R-squared (0 - 1)") +
  theme(text=element_text(size=12, 
                          #       family="Comic Sans MS"))
                          #       family="CM Roman"))
                          #       family="TT Times New Roman"))
                          #       family="Sans"))
                          family="serif"))

ggarrange(ModelES, ModelEC,
          ncol = 1, nrow = 2) ### Plotting estimated R-squared for both equal shares and equal changes models (Figure 4.1)



### TASK 2: The Estimation of the Multilevel Models with the APEP (Post-Hoc Analysis-A)
### Operationalizing Variables

rm(list=ls(all=TRUE))

MyData <- subset(BD_CIS00931, select = c(id,                                           #Case ID
                                         Q35,                                          #Cities,
                                         Q71,                                          #Parties
                                         #Q19, Q20, Q21, Q22, Q24, Q28, Q29, Q30, Q34, #Traditional Dimension
                                         Q89, Q90,                                     #Political Dimension (Q89 = Is democracy the best form of government? Q90 = Do you think Brazil is democratic?)
                                         Q126, Q127,                                   #Economic Dimension (Q126 = Must capitalism accepted as the best model? Q127 = Are you in favour of economic liberalism?)
                                         Q99))                                         #Left-Right Dimension

colnames(MyData) <- c("ID",
                      "CITY",
                      "PARTY",
                      #"Vice-Mayor", "Mayor", "Vice-Governor", "Governor", "State-Secretary", "State-Deputy", "Federal-Deputy", "Senator", "Councillor",
                      "DEMOCRACYI", "DEMOCRACYII",
                      "ECONOMYI", "ECONOMYII",
                      "LEFTRIGHT")

MyData$DEMOCRACYI[ MyData$DEMOCRACYI == "8"] <- NA
MyData$DEMOCRACYI[ MyData$DEMOCRACYI == "9"] <- NA
MyData$ECONOMYI[ MyData$ECONOMYI == "8"] <- NA
MyData$ECONOMYI[ MyData$ECONOMYI == "9"] <- NA
MyData$ECONOMYII[ MyData$ECONOMYII == "8"] <- NA
MyData$ECONOMYII[ MyData$ECONOMYII == "9"] <- NA
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "9"] <- NA

# Coding the degree of party nationalization
#11 <- PP2003 3.316435 (Equal Share) and 31.11282 (Equal Change)
#12 <- PDT 15.00359 (Equal Share) and 19.64228 (Equal Change)
#13 <- PT 24.24111 (Equal Share) and 9.582666 (Equal Change)
#14 <- PTB 3.888116 (Equal Share) and 11.84074 (Equal Change)
#15 <- PMDB 41.34489 (Equal Share) and 35.47119 (Equal Change)
#17 <- PSL .0423648 (Equal Share) and .2544926 (Equal Change)
#20 <- PSC .4188363 (Equal Share) and 4.906114 (Equal Change)
#22 <- PR 2.806297 (Equal Share) and 9.384319 (Equal Change)
#23 <- PPS 1.82e-18 (Equal Share) and 1.015457 (Equal Change)
#25 <- PFL 83.44956 (Equal Share) and 42.11689 (Equal Change)
#31 <- PHS
#40 <- PSB 14.08471 (Equal Share) and 9.340875 (Equal Change)
#45 <- PSDB 36.27292 (Equal Share) and 24.55521 (Equal Change)
#75 <- PTdoB .0820772 (Equal Share) and .1009837 (Equal Change)

MyData$ES <- NA
MyData$ES[ MyData$PARTY == "11"] <- "3.316435"
MyData$ES[ MyData$PARTY == "13"] <- "24.24111"
MyData$ES[ MyData$PARTY == "15"] <- "41.34489"
MyData$ES[ MyData$PARTY == "22"] <- "2.806297"
MyData$ES[ MyData$PARTY == "40"] <- "14.08471"
MyData$ES[ MyData$PARTY == "45"] <- "36.27292"

MyData$EC <- NA
MyData$EC[ MyData$PARTY == "11"] <- "31.11282"
MyData$EC[ MyData$PARTY == "13"] <- "9.582666"
MyData$EC[ MyData$PARTY == "15"] <- "35.47119"
MyData$EC[ MyData$PARTY == "22"] <- "9.384319"
MyData$EC[ MyData$PARTY == "40"] <- "9.340875"
MyData$EC[ MyData$PARTY == "45"] <- "24.55521"

# Estimating the explanatory model - Full Model
library(MASS)
library(rcompanion)
library(nlme)
library(lme4)
library(r2glmm)
library(ggpubr)

MyData2 <- na.omit(MyData)
MyData2$ES <- as.numeric(MyData2$ES)
MyData2$EC <- as.numeric(MyData2$EC)

EqualShare <- lme(ES ~ DEMOCRACYI + DEMOCRACYII + ECONOMYI + ECONOMYII + LEFTRIGHT,
                  random = ~ 1 | CITY, data = MyData2, method = "ML")
summary(EqualShare)
R2ES <- r2beta(EqualShare, partial = TRUE, method = "nsj", data = MyData2)

EqualChange <- lme(EC ~ DEMOCRACYI + DEMOCRACYII + ECONOMYI + ECONOMYII + LEFTRIGHT,
                  random = ~ DEMOCRACYI + ECONOMYI + LEFTRIGHT | CITY, data = MyData2, method = "ML")
summary(EqualChange)
R2EC <- r2beta(EqualChange, partial = TRUE, method = "nsj", data = MyData2)



###TASK 3: The Estimation of Interaction Effects of Controlling Variables on Equal Shares and Equal Changes (Post-Hoc Analysis-B)
rm(list=ls(all=TRUE))

### Operationalizing Variables
MyData <- subset(bls7_released_v01, select = c(wave,                                                                                                        #Year
                                               party_elected,                                                                                               #Party affiliation
                                               region,                                                                                                      #Region (Clustering Unit)
                                               fidelit, believe, efforts, needmps_all, ffaaint, ppswitch_all,                                               #Variables on political issues
                                               econlmr,                                                                                                     #Variable on economic issues
                                               family, vereador, depest, senador, depfed, prefvice, govvice, ministro, secest, clients_all, ppvsreg,        #Variables on traditional issues
                                               lrclass                                                                                                      #Self-placement on the left-right scale
))

colnames(MyData) <- c("YEAR",
                      "PARTY",
                      "REGION",
                      "FIDELITY", "BELIEVE", "EFFORTS", "DECREE", "MILITARY", "SWITCH",
                      "ECONOMY",
                      "FAMILY", "COUNCILLOR", "STATEDEPUTY", "SENATOR", "FEDERALDEPUTY", "MAYOR", "GOVERNOR", "MINISTER", "SECRETARY", "CLIENTELISM", "LOCALINTERESTS",
                      "LEFTRIGHT")

MyData$PARTY[ MyData$PARTY == "99"] <- NA
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "99"] <- NA
MyData$FIDELITY[ MyData$FIDELITY == "9"] <- NA
MyData$DECREE[ MyData$DECREE == "9"] <- NA
MyData$BELIEVE[ MyData$BELIEVE == "9"] <- NA
MyData$BELIEVE[ MyData$BELIEVE == "8"] <- NA
MyData$EFFORTS[ MyData$EFFORTS == "9"] <- NA
MyData$EFFORTS[ MyData$EFFORTS == "8"] <- NA
MyData$MILITARY[ MyData$SWITCH == "9"] <- NA
MyData$MILITARY[ MyData$MILITARY == "9"] <- NA
MyData$ECONOMY[ MyData$ECONOMY == "9"] <- NA
MyData$CLIENTELISM[ MyData$CLIENTELISM == "9"] <- NA
MyData$LOCALINTERESTS[ MyData$LOCALINTERESTS == "3"] <- NA
MyData$LOCALINTERESTS[ MyData$LOCALINTERESTS == "9"] <- NA
MyData$FAMILY[ MyData$FAMILY == "9"] <- NA
MyData$COUNCILLOR[ MyData$COUNCILLOR == "9"] <- NA
MyData$STATEDEPUTY[ MyData$STATEDEPUTY == "9"] <- NA
MyData$SENATOR[ MyData$SENATOR == "9"] <- NA
MyData$FEDERALDEPUTY[ MyData$FEDERALDEPUTY == "9"] <- NA
MyData$MAYOR[ MyData$MAYOR == "9"] <- NA
MyData$GOVERNOR[ MyData$GOVERNOR == "9"] <- NA
MyData$MINISTER[ MyData$MINISTER == "9"] <- NA
MyData$SECRETARY[ MyData$SECRETARY == "9"] <- NA
MyData$REGION[ MyData$REGION == "9"] <- NA
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "3.5"] <- 3
MyData$LEFTRIGHT[ MyData$LEFTRIGHT == "5.5"] <- 5

# Coding the degree of party nationalization
unique(sort(MyData$PARTY))
#11 <- PP2003 4.304939 (Equal Share) and 27.0367 (Equal Change)
#12 <- PDT 8.897054 (Equal Share) and 17.1212 (Equal Change)
#13 <- PT 20.74353 (Equal Share) and 11.9215 (Equal Change)
#14 <- PTB 2.724552 (Equal Share) and 11.20572 (Equal Change)
#15 <- PMDB 36.88386 (Equal Share) and 35.67332 (Equal Change)
#20 <- PSC 0.3628846 (Equal Share) and 5.347685 (Equal Change)
#22 <- PR 3.930946 (Equal Share) and 9.936525 (Equal Change)
#23 <- PPS 5.38e-22 (Equal Share) and 1.063595 (Equal Change)
#25 <- PFL-DEM 47.35181 (Equal Share) and 43.30902 (Equal Change)
#33 <- PMN 0.4869283 (Equal Share) and 1.646082 (Equal Change)
#40 <- PSB 14.99037 (Equal Share) and 9.719556 (Equal Change)
#43 <- PV 1.61627 (Equal Share) and 1.994438 (Equal Change)
#45 <- PSDB 25.88173 (Equal Share) and 29.34851 (Equal Change)
#50 <- PSOL 1.182162 (Equal Share) and 1.009469 (Equal Change)
#65 <- PCdoB 0.8503498 (Equal Share) and 2.863987 (Equal Change)

MyData$ES <- NA
MyData$ES[ MyData$PARTY == "11"] <- 4.304939
MyData$ES[ MyData$PARTY == "13"] <- 20.74353 
MyData$ES[ MyData$PARTY == "15"] <- 36.88386
MyData$ES[ MyData$PARTY == "22"] <- 3.930946
MyData$ES[ MyData$PARTY == "40"] <- 14.99037
MyData$ES[ MyData$PARTY == "45"] <- 25.88173

MyData$EC <- NA
MyData$EC[ MyData$PARTY == "11"] <- 27.0367
MyData$EC[ MyData$PARTY == "13"] <- 11.9215 
MyData$EC[ MyData$PARTY == "15"] <- 35.67332
MyData$EC[ MyData$PARTY == "22"] <- 9.936525
MyData$EC[ MyData$PARTY == "40"] <- 9.719556
MyData$EC[ MyData$PARTY == "45"] <- 29.34851

MyData2 <- na.omit(MyData)

# Coding the regional variance of district magnitude
MyData2$DM <- NA
MyData2$DM[ MyData2$REGION == "1"] <- 9.918367 #North
MyData2$DM[ MyData2$REGION == "2"] <- 97.06173 #Northeast
MyData2$DM[ MyData2$REGION == "3"] <- 478.6875 #Southeast
MyData2$DM[ MyData2$REGION == "4"] <- 46.88889 #South
MyData2$DM[ MyData2$REGION == "5"] <- 15.1875  #Centre-West


# Coding the regional variance of district numbers
MyData2$ED <- NA
MyData2$ED[ MyData2$REGION == "1"] <- 7 #North
MyData2$ED[ MyData2$REGION == "2"] <- 9 #Northeast
MyData2$ED[ MyData2$REGION == "3"] <- 4 #Southeast
MyData2$ED[ MyData2$REGION == "4"] <- 3 #South
MyData2$ED[ MyData2$REGION == "5"] <- 4 #Centre-West


# Coding the regional authority index
MyData2$RAI <- NA
MyData2$RAI[ MyData2$YEAR == "1997"] <- 21.5
MyData2$RAI[ MyData2$YEAR == "2001"] <- 19.5
MyData2$RAI[ MyData2$YEAR == "2005"] <- 19.5
MyData2$RAI[ MyData2$YEAR == "2009"] <- 19.5


# Coding the effective number of electoral parties
North <- MyData2[ which(MyData2$REGION == "1"),]
Northeast <- MyData2[ which(MyData2$REGION == "2"),]
Southeast <- MyData2[ which(MyData2$REGION == "3"),]
South <- MyData2[ which(MyData2$REGION == "4"),]
CentreWest <- MyData2[ which(MyData2$REGION == "5"),]

North$ENP<- NA
Northeast$ENP<- NA
Southeast$ENP<- NA
South$ENP<- NA
CentreWest$ENP<- NA

# - 1998 -
North$ENP[ North$YEAR == "1997"] <- 3.083383
Northeast$ENP[ Northeast$YEAR == "1997"] <- 3.965065
Southeast$ENP[ Southeast$YEAR == "1997"] <- 0.4797318
South$ENP[ South$YEAR == "1997"] <- 0.3673527
CentreWest$ENP[ CentreWest$YEAR == "1997"] <- 1.347972

# - 2002 -
North$ENP[ North$YEAR == "2001"] <- 1.727305
Northeast$ENP[ Northeast$YEAR == "2001"] <- 3.042864
Southeast$ENP[ Southeast$YEAR == "2001"] <- 1.571398
South$ENP[ South$YEAR == "2001"] <- 1.06751
CentreWest$ENP[ CentreWest$YEAR == "2001"] <- 1.571398

# - 2006 -
North$ENP[ North$YEAR == "2005"] <- 3.052043
Northeast$ENP[ Northeast$YEAR == "2005"] <- 2.112666
Southeast$ENP[ Southeast$YEAR == "2005"] <- 4.287599
South$ENP[ South$YEAR == "2005"] <- 0.9168819
CentreWest$ENP[ CentreWest$YEAR == "2005"] <- 0.7901674

# - 2010 -
North$ENP[ North$YEAR == "2009"] <- 2.080644
Northeast$ENP[ Northeast$YEAR == "2009"] <- 0.8554433
Southeast$ENP[ Southeast$YEAR == "2009"] <- 1.993163
South$ENP[ South$YEAR == "2009"] <- 2.802145
CentreWest$ENP[ CentreWest$YEAR == "2009"] <- 0.9314278

# - 2014 -
North$ENP[ North$YEAR == "2013"] <- 9.804636
Northeast$ENP[ Northeast$YEAR == "2013"] <- 4.643176
Southeast$ENP[ Southeast$YEAR == "2013"] <- 0.4860608
South$ENP[ South$YEAR == "2013"] <- 7.136502
CentreWest$ENP[ CentreWest$YEAR == "2013"] <- 7.949261


# Coding ethnic fractionalization
North$EthnicFractionalization <- NA
Northeast$EthnicFractionalization <- NA
Southeast$EthnicFractionalization <- NA
South$EthnicFractionalization <- NA
CentreWest$EthnicFractionalization <- NA

# -1998 - 
North$EthnicFractionalization[ North$YEAR == "1997"] <- 0.451659027
Northeast$EthnicFractionalization[ Northeast$YEAR == "1997"] <- 0.494894512
Southeast$EthnicFractionalization[ Southeast$YEAR == "1997"] <- 0.508943
South$EthnicFractionalization[ South$YEAR == "1997"] <- 0.293391
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "1997"] <- 0.545466

# -2002 - 
North$EthnicFractionalization[ North$YEAR == "2001"] <- 0.472878789
Northeast$EthnicFractionalization[ Northeast$YEAR == "2001"] <- 0.472878789
Southeast$EthnicFractionalization[ Southeast$YEAR == "2001"] <- 0.510037
South$EthnicFractionalization[ South$YEAR == "2001"] <- 0.297078
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2001"] <- 0.548522

# -2006 - 
North$EthnicFractionalization[ North$YEAR == "2005"] <- 0.460495074
Northeast$EthnicFractionalization[ Northeast$YEAR == "2005"] <- 0.460495074
Southeast$EthnicFractionalization[ Southeast$YEAR == "2005"] <- 0.542604
South$EthnicFractionalization[ South$YEAR == "2005"] <- 0.339256
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2005"] <- 0.556918

# -2010 - 
North$EthnicFractionalization[ North$YEAR == "2009"] <- 0.479708422
Northeast$EthnicFractionalization[ Northeast$YEAR == "2009"] <- 0.479708422
Southeast$EthnicFractionalization[ Southeast$YEAR == "2009"] <- 0.560931
South$EthnicFractionalization[ South$YEAR == "2009"] <- 0.362381
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2009"] <- 0.576055

# -2014 - 
North$EthnicFractionalization[ North$YEAR == "2013"] <- 0.465401922
Northeast$EthnicFractionalization[ Northeast$YEAR == "2013"] <- 0.465401922
Southeast$EthnicFractionalization[ Southeast$YEAR == "2013"] <- 0.575059
South$EthnicFractionalization[ South$YEAR == "2013"] <- 0.384956
CentreWest$EthnicFractionalization[ CentreWest$YEAR == "2013"] <- 0.572509


# Coding class fractionalization
North$ClassFractionalization <- NA
Northeast$ClassFractionalization <- NA
Southeast$ClassFractionalization <- NA
South$ClassFractionalization <- NA
CentreWest$ClassFractionalization <- NA

# -1998-
North$ClassFractionalization[ North$YEAR == "1997"] <- 0.864635546
Northeast$ClassFractionalization[ Northeast$YEAR == "1997"] <- 0.786392218
Southeast$ClassFractionalization[ Southeast$YEAR == "1997"] <- 0.868567191
South$ClassFractionalization[ South$YEAR == "1997"] <- 0.848399786
CentreWest$ClassFractionalization[ CentreWest$YEAR == "1997"] <- 0.855468597

# -2002-
North$ClassFractionalization[ North$YEAR == "2001"] <- 0.872388514
Northeast$ClassFractionalization[ Northeast$YEAR == "2001"] <- 0.808797479
Southeast$ClassFractionalization[ Southeast$YEAR == "2001"] <- 0.876767117
South$ClassFractionalization[ South$YEAR == "2001"] <- 0.839780885
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2001"] <- 0.884436044

# -2006-
North$ClassFractionalization[ North$YEAR == "2005"] <- 0.853243036
Northeast$ClassFractionalization[ Northeast$YEAR == "2005"] <- 0.825329965
Southeast$ClassFractionalization[ Southeast$YEAR == "2005"] <- 0.876845282
South$ClassFractionalization[ South$YEAR == "2005"] <- 0.844659801
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2005"] <- 0.887962334

# -2010-
North$ClassFractionalization[ North$YEAR == "2009"] <- 0.864250639
Northeast$ClassFractionalization[ Northeast$YEAR == "2009"] <- 0.856123458
Southeast$ClassFractionalization[ Southeast$YEAR == "2009"] <- 0.89520095
South$ClassFractionalization[ South$YEAR == "2009"] <- 0.859166596
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2009"] <- 0.900787258

# -2014-
North$ClassFractionalization[ North$YEAR == "2013"] <- 0.874565552
Northeast$ClassFractionalization[ Northeast$YEAR == "2013"] <- 0.865528143
Southeast$ClassFractionalization[ Southeast$YEAR == "2013"] <- 0.896487334
South$ClassFractionalization[ South$YEAR == "2013"] <- 0.859421454
CentreWest$ClassFractionalization[ CentreWest$YEAR == "2013"] <- 0.898026609


# Binding regional sebsets
MD <- rbind(North, Northeast, Southeast, South, CentreWest)
MD2 <- na.omit(MD)

# Specifying interactive models
library(lme4)
library(nlme)
library(interplot)
library(ggplot2)

CLIENTELISM1 <- lme(ES ~ CLIENTELISM + DM + CLIENTELISM*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM1)
R2ES <- r2beta(CLIENTELISM1, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM2 <- lme(ES ~ CLIENTELISM + ED + CLIENTELISM*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM2)
R2ES <- r2beta(CLIENTELISM2, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM3 <- lme(ES ~ CLIENTELISM + ENP + CLIENTELISM*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM3)
R2ES <- r2beta(CLIENTELISM3, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM4 <- lme(ES ~ CLIENTELISM + RAI + CLIENTELISM*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM4)
R2ES <- r2beta(CLIENTELISM4, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM5 <- lme(ES ~ CLIENTELISM + EthnicFractionalization + CLIENTELISM*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM5)
R2ES <- r2beta(CLIENTELISM5, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM6 <- lme(ES ~ CLIENTELISM + ClassFractionalization + CLIENTELISM*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM6)
R2ES <- r2beta(CLIENTELISM6, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR1 <- lme(ES ~ COUNCILLOR + DM + COUNCILLOR*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR1)
R2ES <- r2beta(COUNCILLOR1, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR2 <- lme(ES ~ COUNCILLOR + ED + COUNCILLOR*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR2)
R2ES <- r2beta(COUNCILLOR2, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR3 <- lme(ES ~ COUNCILLOR + ENP + COUNCILLOR*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR3)
R2ES <- r2beta(COUNCILLOR3, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR4 <- lme(ES ~ COUNCILLOR + RAI + COUNCILLOR*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR4)
R2ES <- r2beta(COUNCILLOR4, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR5 <- lme(ES ~ COUNCILLOR + EthnicFractionalization + COUNCILLOR*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR5)
R2ES <- r2beta(COUNCILLOR5, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR6 <- lme(ES ~ COUNCILLOR + ClassFractionalization + COUNCILLOR*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR6)
R2ES <- r2beta(COUNCILLOR6, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY1 <- lme(ES ~ FEDERALDEPUTY + DM + FEDERALDEPUTY*DM, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FEDERALDEPUTY1)
R2ES <- r2beta(FEDERALDEPUTY1, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY2 <- lme(ES ~ FEDERALDEPUTY + ED + FEDERALDEPUTY*ED, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FEDERALDEPUTY2)
R2ES <- r2beta(FEDERALDEPUTY2, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY3 <- lme(ES ~ FEDERALDEPUTY + ENP + FEDERALDEPUTY*ENP, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FEDERALDEPUTY3)
R2ES <- r2beta(FEDERALDEPUTY3, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY4 <- lme(ES ~ FEDERALDEPUTY + RAI + FEDERALDEPUTY*RAI, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FEDERALDEPUTY4)
R2ES <- r2beta(FEDERALDEPUTY4, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY5 <- lme(ES ~ FEDERALDEPUTY + EthnicFractionalization + FEDERALDEPUTY*EthnicFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FEDERALDEPUTY5)
R2ES <- r2beta(FEDERALDEPUTY5, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY6 <- lme(ES ~ FEDERALDEPUTY + ClassFractionalization + FEDERALDEPUTY*ClassFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FEDERALDEPUTY6)
R2ES <- r2beta(FEDERALDEPUTY6, partial = TRUE, method = "nsj", data = MD2)

MINISTER1 <- lme(ES ~ MINISTER + DM + MINISTER*DM, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(MINISTER1)
R2ES <- r2beta(MINISTER1, partial = TRUE, method = "nsj", data = MD2)

MINISTER2 <- lme(ES ~ MINISTER + ED + MINISTER*ED, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(MINISTER2)
R2ES <- r2beta(MINISTER2, partial = TRUE, method = "nsj", data = MD2)

MINISTER3 <- lme(ES ~ MINISTER + ENP + MINISTER*ENP, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(MINISTER3)
R2ES <- r2beta(MINISTER3, partial = TRUE, method = "nsj", data = MD2)

MINISTER4 <- lme(ES ~ MINISTER + RAI + MINISTER*RAI, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(MINISTER4)
R2ES <- r2beta(MINISTER4, partial = TRUE, method = "nsj", data = MD2)

MINISTER5 <- lme(ES ~ MINISTER + EthnicFractionalization + MINISTER*EthnicFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(MINISTER5)
R2ES <- r2beta(MINISTER5, partial = TRUE, method = "nsj", data = MD2)

MINISTER6 <- lme(ES ~ MINISTER + ClassFractionalization + MINISTER*ClassFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(MINISTER6)
R2ES <- r2beta(MINISTER6, partial = TRUE, method = "nsj", data = MD2)

SWITCH1 <- lme(EC ~ SWITCH + DM + SWITCH*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(SWITCH1)
R2EC <- r2beta(SWITCH1, partial = TRUE, method = "nsj", data = MD2)

SWITCH2 <- lme(EC ~ SWITCH + ED + SWITCH*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(SWITCH2)
R2EC <- r2beta(SWITCH2, partial = TRUE, method = "nsj", data = MD2)

SWITCH3 <- lme(EC ~ SWITCH + ENP + SWITCH*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(SWITCH3)
R2EC <- r2beta(SWITCH3, partial = TRUE, method = "nsj", data = MD2)

SWITCH4 <- lme(EC ~ SWITCH + RAI + SWITCH*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(SWITCH4)
R2EC <- r2beta(SWITCH4, partial = TRUE, method = "nsj", data = MD2)

SWITCH5 <- lme(EC ~ SWITCH + EthnicFractionalization + SWITCH*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(SWITCH5)
R2EC <- r2beta(SWITCH5, partial = TRUE, method = "nsj", data = MD2)

SWITCH6 <- lme(EC ~ SWITCH + ClassFractionalization + SWITCH*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(SWITCH6)
R2EC <- r2beta(SWITCH6, partial = TRUE, method = "nsj", data = MD2)

ECONOMY1 <- lme(EC ~ ECONOMY + DM + ECONOMY*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(ECONOMY1)
R2EC <- r2beta(ECONOMY1, partial = TRUE, method = "nsj", data = MD2)

ECONOMY2 <- lme(EC ~ ECONOMY + ED + ECONOMY*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(ECONOMY2)
R2EC <- r2beta(ECONOMY2, partial = TRUE, method = "nsj", data = MD2)

ECONOMY3 <- lme(EC ~ ECONOMY + ENP + ECONOMY*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(ECONOMY3)
R2EC <- r2beta(ECONOMY3, partial = TRUE, method = "nsj", data = MD2)

ECONOMY4 <- lme(EC ~ ECONOMY + RAI + ECONOMY*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(ECONOMY4)
R2EC <- r2beta(ECONOMY4, partial = TRUE, method = "nsj", data = MD2)

ECONOMY5 <- lme(EC ~ ECONOMY + EthnicFractionalization + ECONOMY*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(ECONOMY5)
R2EC <- r2beta(ECONOMY5, partial = TRUE, method = "nsj", data = MD2)

ECONOMY6 <- lme(EC ~ ECONOMY + ClassFractionalization + ECONOMY*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(ECONOMY6)
R2EC <- r2beta(ECONOMY6, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM10 <- lme(EC ~ CLIENTELISM + DM + CLIENTELISM*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM10)
R2EC <- r2beta(CLIENTELISM10, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM20 <- lme(EC ~ CLIENTELISM + ED + CLIENTELISM*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM20)
R2EC <- r2beta(CLIENTELISM20, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM30 <- lme(EC ~ CLIENTELISM + ENP + CLIENTELISM*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM30)
R2EC <- r2beta(CLIENTELISM30, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM40 <- lme(EC ~ CLIENTELISM + RAI + CLIENTELISM*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM40)
R2EC <- r2beta(CLIENTELISM40, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM50 <- lme(EC ~ CLIENTELISM + EthnicFractionalization + CLIENTELISM*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM50)
R2EC <- r2beta(CLIENTELISM50, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM60 <- lme(EC ~ CLIENTELISM + ClassFractionalization + CLIENTELISM*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(CLIENTELISM60)
R2EC <- r2beta(CLIENTELISM60, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS1 <- lme(EC ~ LOCALINTERESTS + DM + LOCALINTERESTS*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(LOCALINTERESTS1)
R2EC <- r2beta(LOCALINTERESTS1, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS2 <- lme(EC ~ LOCALINTERESTS + ED + LOCALINTERESTS*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(LOCALINTERESTS2)
R2EC <- r2beta(LOCALINTERESTS2, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS3 <- lme(EC ~ LOCALINTERESTS + ENP + LOCALINTERESTS*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(LOCALINTERESTS3)
R2EC <- r2beta(LOCALINTERESTS3, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS4 <- lme(EC ~ LOCALINTERESTS + RAI + LOCALINTERESTS*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(LOCALINTERESTS4)
R2EC <- r2beta(LOCALINTERESTS4, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS5 <- lme(EC ~ LOCALINTERESTS + EthnicFractionalization + LOCALINTERESTS*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(LOCALINTERESTS5)
R2EC <- r2beta(LOCALINTERESTS5, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS6 <- lme(EC ~ LOCALINTERESTS + ClassFractionalization + LOCALINTERESTS*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(LOCALINTERESTS6)
R2EC <- r2beta(LOCALINTERESTS6, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR10 <- lme(EC ~ COUNCILLOR + DM + COUNCILLOR*DM, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR10)
R2EC <- r2beta(COUNCILLOR10, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR20 <- lme(EC ~ COUNCILLOR + ED + COUNCILLOR*ED, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR20)
R2EC <- r2beta(COUNCILLOR20, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR30 <- lme(EC ~ COUNCILLOR + ENP + COUNCILLOR*ENP, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR30)
R2EC <- r2beta(COUNCILLOR30, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR40 <- lme(EC ~ COUNCILLOR + RAI + COUNCILLOR*RAI, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR40)
R2EC <- r2beta(COUNCILLOR40, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR50 <- lme(EC ~ COUNCILLOR + EthnicFractionalization + COUNCILLOR*EthnicFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR50)
R2EC <- r2beta(COUNCILLOR50, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR60 <- lme(EC ~ COUNCILLOR + ClassFractionalization + COUNCILLOR*ClassFractionalization, random = ~ 1 | REGION, data = MD2, method = "ML")
summary(COUNCILLOR60)
R2EC <- r2beta(COUNCILLOR60, partial = TRUE, method = "nsj", data = MD2)

SECRETARY1 <- lme(EC ~ SECRETARY + DM + SECRETARY*DM, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(SECRETARY1)
R2EC <- r2beta(SECRETARY1, partial = TRUE, method = "nsj", data = MD2)

SECRETARY2 <- lme(EC ~ SECRETARY + ED + SECRETARY*ED, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(SECRETARY2)
R2EC <- r2beta(SECRETARY2, partial = TRUE, method = "nsj", data = MD2)

SECRETARY3 <- lme(EC ~ SECRETARY + RAI + SECRETARY*RAI, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(SECRETARY3)
R2EC <- r2beta(SECRETARY3, partial = TRUE, method = "nsj", data = MD2)

SECRETARY4 <- lme(EC ~ SECRETARY + ENP + SECRETARY*ENP, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(SECRETARY4)
R2EC <- r2beta(SECRETARY4, partial = TRUE, method = "nsj", data = MD2)

SECRETARY5 <- lme(EC ~ SECRETARY + EthnicFractionalization + SECRETARY*EthnicFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(SECRETARY5)
R2EC <- r2beta(SECRETARY5, partial = TRUE, method = "nsj", data = MD2)

SECRETARY6 <- lme(EC ~ SECRETARY + ClassFractionalization + SECRETARY*ClassFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(SECRETARY6)
R2EC <- r2beta(SECRETARY6, partial = TRUE, method = "nsj", data = MD2)

FAMILY1 <- lme(EC ~ FAMILY + DM + FAMILY*DM, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FAMILY1)
R2EC <- r2beta(FAMILY1, partial = TRUE, method = "nsj", data = MD2)

FAMILY2 <- lme(EC ~ FAMILY + ED + FAMILY*ED, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FAMILY2)
R2EC <- r2beta(FAMILY2, partial = TRUE, method = "nsj", data = MD2)

FAMILY3 <- lme(EC ~ FAMILY + RAI + FAMILY*RAI, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FAMILY3)
R2EC <- r2beta(FAMILY3, partial = TRUE, method = "nsj", data = MD2)

FAMILY4 <- lme(EC ~ FAMILY + ENP + FAMILY*ENP, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FAMILY4)
R2EC <- r2beta(FAMILY4, partial = TRUE, method = "nsj", data = MD2)

FAMILY5 <- lme(EC ~ FAMILY + EthnicFractionalization + FAMILY*EthnicFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FAMILY5)
R2EC <- r2beta(FAMILY5, partial = TRUE, method = "nsj", data = MD2)

FAMILY6 <- lme(EC ~ FAMILY + ClassFractionalization + FAMILY*ClassFractionalization, random =  ~ 1 | REGION, data = MD2, method = "ML")
summary(FAMILY6)
R2EC <- r2beta(FAMILY6, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT1 <- lme(EC ~ LEFTRIGHT + DM + LEFTRIGHT*DM, random = ~ LEFTRIGHT | REGION, data = MD2, method = "ML")
summary(LEFTRIGHT1)
R2EC <- r2beta(LEFTRIGHT1, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT2 <- lme(EC ~ LEFTRIGHT + ED + LEFTRIGHT*ED, random = ~ LEFTRIGHT | REGION, data = MD2, method = "ML")
summary(LEFTRIGHT2)
R2EC <- r2beta(LEFTRIGHT2, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT3 <- lme(EC ~ LEFTRIGHT + RAI + LEFTRIGHT*RAI, random = ~ LEFTRIGHT | REGION, data = MD2, method = "ML")
summary(LEFTRIGHT3)
R2EC <- r2beta(LEFTRIGHT3, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT4 <- lme(EC ~ LEFTRIGHT + ENP + LEFTRIGHT*ENP, random = ~ LEFTRIGHT | REGION, data = MD2, method = "ML")
summary(LEFTRIGHT4)
R2EC <- r2beta(LEFTRIGHT4, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT5 <- lme(EC ~ LEFTRIGHT + EthnicFractionalization + LEFTRIGHT*EthnicFractionalization, random = ~ LEFTRIGHT | REGION, data = MD2, method = "ML")
summary(LEFTRIGHT5)
R2EC <- r2beta(LEFTRIGHT5, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT6 <- lme(EC ~ LEFTRIGHT + ClassFractionalization + LEFTRIGHT*ClassFractionalization, random = ~ LEFTRIGHT | REGION, data = MD2, method = "ML")
summary(LEFTRIGHT6)
R2EC <- r2beta(LEFTRIGHT6, partial = TRUE, method = "nsj", data = MD2)


# Models for plotting
CLIENTELISM1 <- lmer(ES ~ CLIENTELISM + DM + CLIENTELISM*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM1)
R2ES <- r2beta(CLIENTELISM1, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM2 <- lmer(ES ~ CLIENTELISM + ED + CLIENTELISM*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM2)
R2ES <- r2beta(CLIENTELISM2, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM3 <- lmer(ES ~ CLIENTELISM + RAI + CLIENTELISM*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM3)
R2ES <- r2beta(CLIENTELISM3, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM4 <- lmer(ES ~ CLIENTELISM + ENP + CLIENTELISM*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM4)
R2ES <- r2beta(CLIENTELISM4, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM5 <- lmer(ES ~ CLIENTELISM + EthnicFractionalization + CLIENTELISM*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM5)
R2ES <- r2beta(CLIENTELISM5, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM6 <- lmer(ES ~ CLIENTELISM + ClassFractionalization + CLIENTELISM*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM6)
R2ES <- r2beta(CLIENTELISM6, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR1 <- lmer(ES ~ COUNCILLOR + DM + COUNCILLOR*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR1)
R2ES <- r2beta(COUNCILLOR1, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR2 <- lmer(ES ~ COUNCILLOR + ED + COUNCILLOR*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR2)
R2ES <- r2beta(COUNCILLOR2, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR3 <- lmer(ES ~ COUNCILLOR + RAI + COUNCILLOR*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR3)
R2ES <- r2beta(COUNCILLOR3, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR4 <- lmer(ES ~ COUNCILLOR + ENP + COUNCILLOR*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR4)
R2ES <- r2beta(COUNCILLOR4, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR5 <- lmer(ES ~ COUNCILLOR + EthnicFractionalization + COUNCILLOR*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR5)
R2ES <- r2beta(COUNCILLOR5, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR6 <- lmer(ES ~ COUNCILLOR + ClassFractionalization + COUNCILLOR*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR6)
R2ES <- r2beta(COUNCILLOR6, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY1 <- lmer(ES ~ FEDERALDEPUTY + DM + FEDERALDEPUTY*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(FEDERALDEPUTY1)
R2ES <- r2beta(FEDERALDEPUTY1, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY2 <- lmer(ES ~ FEDERALDEPUTY + ED + FEDERALDEPUTY*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(FEDERALDEPUTY2)
R2ES <- r2beta(FEDERALDEPUTY2, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY3 <- lmer(ES ~ FEDERALDEPUTY + RAI + FEDERALDEPUTY*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(FEDERALDEPUTY3)
R2ES <- r2beta(FEDERALDEPUTY3, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY4 <- lmer(ES ~ FEDERALDEPUTY + ENP + FEDERALDEPUTY*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(FEDERALDEPUTY4)
R2ES <- r2beta(FEDERALDEPUTY4, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY5 <- lmer(ES ~ FEDERALDEPUTY + EthnicFractionalization + FEDERALDEPUTY*EthnicFractionalization + (1 | REGION), data = MD2,REML = FALSE)
summary(FEDERALDEPUTY5)
R2ES <- r2beta(FEDERALDEPUTY5, partial = TRUE, method = "nsj", data = MD2)

FEDERALDEPUTY6 <- lmer(ES ~ FEDERALDEPUTY + ClassFractionalization + FEDERALDEPUTY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(FEDERALDEPUTY6)
R2ES <- r2beta(FEDERALDEPUTY6, partial = TRUE, method = "nsj", data = MD2)

MINISTER1 <- lmer(ES ~ MINISTER + DM + MINISTER*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(MINISTER1)
R2ES <- r2beta(MINISTER1, partial = TRUE, method = "nsj", data = MD2)

MINISTER2 <- lmer(ES ~ MINISTER + ED + MINISTER*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(MINISTER2)
R2ES <- r2beta(MINISTER2, partial = TRUE, method = "nsj", data = MD2)

MINISTER3 <- lmer(ES ~ MINISTER + RAI + MINISTER*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(MINISTER3)
R2ES <- r2beta(MINISTER3, partial = TRUE, method = "nsj", data = MD2)

MINISTER4 <- lmer(ES ~ MINISTER + ENP + MINISTER*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(MINISTER4)
R2ES <- r2beta(MINISTER4, partial = TRUE, method = "nsj", data = MD2)

MINISTER5 <- lmer(ES ~ MINISTER + EthnicFractionalization + MINISTER*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(MINISTER5)
R2ES <- r2beta(MINISTER5, partial = TRUE, method = "nsj", data = MD2)

MINISTER6 <- lmer(ES ~ MINISTER + ClassFractionalization + MINISTER*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(MINISTER6)
R2ES <- r2beta(MINISTER6, partial = TRUE, method = "nsj", data = MD2)

SWITCH1 <- lmer(EC ~ SWITCH + DM + SWITCH*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(SWITCH1)
R2EC <- r2beta(SWITCH1, partial = TRUE, method = "nsj", data = MD2)

SWITCH2 <- lmer(EC ~ SWITCH + ED + SWITCH*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(SWITCH2)
R2EC <- r2beta(SWITCH2, partial = TRUE, method = "nsj", data = MD2)

SWITCH3 <- lmer(EC ~ SWITCH + RAI + SWITCH*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(SWITCH3)
R2EC <- r2beta(SWITCH3, partial = TRUE, method = "nsj", data = MD2)

SWITCH4 <- lmer(EC ~ SWITCH + ENP + SWITCH*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(SWITCH4)
R2EC <- r2beta(SWITCH4, partial = TRUE, method = "nsj", data = MD2)

SWITCH5 <- lmer(EC ~ SWITCH + EthnicFractionalization + SWITCH*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(SWITCH5)
R2EC <- r2beta(SWITCH5, partial = TRUE, method = "nsj", data = MD2)

SWITCH6 <- lmer(EC ~ SWITCH + ClassFractionalization + SWITCH*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(SWITCH6)
R2EC <- r2beta(SWITCH6, partial = TRUE, method = "nsj", data = MD2)

ECONOMY1 <- lmer(EC ~ ECONOMY + DM + ECONOMY*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(ECONOMY1)
R2EC <- r2beta(ECONOMY1, partial = TRUE, method = "nsj", data = MD2)

ECONOMY2 <- lmer(EC ~ ECONOMY + ED + ECONOMY*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(ECONOMY2)
R2EC <- r2beta(ECONOMY2, partial = TRUE, method = "nsj", data = MD2)

ECONOMY3 <- lmer(EC ~ ECONOMY + RAI + ECONOMY*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(ECONOMY3)
R2EC <- r2beta(ECONOMY3, partial = TRUE, method = "nsj", data = MD2)

ECONOMY4 <- lmer(EC ~ ECONOMY + ENP + ECONOMY*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(ECONOMY4)
R2EC <- r2beta(ECONOMY4, partial = TRUE, method = "nsj", data = MD2)

ECONOMY5 <- lmer(EC ~ ECONOMY + EthnicFractionalization + ECONOMY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(ECONOMY5)
R2EC <- r2beta(ECONOMY5, partial = TRUE, method = "nsj", data = MD2)

ECONOMY6 <- lmer(EC ~ ECONOMY + ClassFractionalization + ECONOMY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(ECONOMY6)
R2EC <- r2beta(ECONOMY6, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM10 <- lmer(EC ~ CLIENTELISM + DM + CLIENTELISM*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM10)
R2EC <- r2beta(CLIENTELISM10, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM20 <- lmer(EC ~ CLIENTELISM + ED + CLIENTELISM*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM20)
R2EC <- r2beta(CLIENTELISM20, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM30 <- lmer(EC ~ CLIENTELISM + RAI + CLIENTELISM*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM30)
R2EC <- r2beta(CLIENTELISM30, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM40 <- lmer(EC ~ CLIENTELISM + ENP + CLIENTELISM*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM40)
R2EC <- r2beta(CLIENTELISM40, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM50 <- lmer(EC ~ CLIENTELISM + EthnicFractionalization + CLIENTELISM*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM50)
R2EC <- r2beta(CLIENTELISM50, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM60 <- lmer(EC ~ CLIENTELISM + ClassFractionalization + CLIENTELISM*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(CLIENTELISM60)
R2EC <- r2beta(CLIENTELISM60, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS1 <- lmer(EC ~ LOCALINTERESTS + DM + LOCALINTERESTS*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(LOCALINTERESTS1)
R2EC <- r2beta(LOCALINTERESTS1, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS2 <- lmer(EC ~ LOCALINTERESTS + ED + LOCALINTERESTS*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(LOCALINTERESTS2)
R2EC <- r2beta(LOCALINTERESTS2, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS3 <- lmer(EC ~ LOCALINTERESTS + RAI + LOCALINTERESTS*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(LOCALINTERESTS3)
R2EC <- r2beta(LOCALINTERESTS3, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS4 <- lmer(EC ~ LOCALINTERESTS + ENP + LOCALINTERESTS*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(LOCALINTERESTS4)
R2EC <- r2beta(LOCALINTERESTS4, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS5 <- lmer(EC ~ LOCALINTERESTS + EthnicFractionalization + LOCALINTERESTS*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(LOCALINTERESTS5)
R2EC <- r2beta(LOCALINTERESTS5, partial = TRUE, method = "nsj", data = MD2)

LOCALINTERESTS6 <- lmer(EC ~ LOCALINTERESTS + ClassFractionalization + LOCALINTERESTS*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(LOCALINTERESTS6)
R2EC <- r2beta(LOCALINTERESTS6, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR10 <- lmer(EC ~ COUNCILLOR + DM + COUNCILLOR*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR10)
R2EC <- r2beta(COUNCILLOR10, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR20 <- lmer(EC ~ COUNCILLOR + ED + COUNCILLOR*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR20)
R2EC <- r2beta(COUNCILLOR20, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR30 <- lmer(EC ~ COUNCILLOR + RAI + COUNCILLOR*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR30)
R2EC <- r2beta(COUNCILLOR30, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR40 <- lmer(EC ~ COUNCILLOR + ENP + COUNCILLOR*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR40)
R2EC <- r2beta(COUNCILLOR40, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR50 <- lmer(EC ~ COUNCILLOR + EthnicFractionalization + COUNCILLOR*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR50)
R2EC <- r2beta(COUNCILLOR50, partial = TRUE, method = "nsj", data = MD2)

COUNCILLOR60 <- lmer(EC ~ COUNCILLOR + ClassFractionalization + COUNCILLOR*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(COUNCILLOR60)
R2EC <- r2beta(COUNCILLOR60, partial = TRUE, method = "nsj", data = MD2)

SECRETARY1 <- lmer(EC ~ SECRETARY + DM + SECRETARY*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(SECRETARY1)
R2EC <- r2beta(SECRETARY1, partial = TRUE, method = "nsj", data = MD2)

SECRETARY2 <- lmer(EC ~ SECRETARY + ED + SECRETARY*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(SECRETARY2)
R2EC <- r2beta(SECRETARY2, partial = TRUE, method = "nsj", data = MD2)

SECRETARY3 <- lmer(EC ~ SECRETARY + ENP + SECRETARY*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(SECRETARY3)
R2EC <- r2beta(SECRETARY3, partial = TRUE, method = "nsj", data = MD2)

SECRETARY4 <- lmer(EC ~ SECRETARY + RAI + SECRETARY*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(SECRETARY4)
R2EC <- r2beta(SECRETARY4, partial = TRUE, method = "nsj", data = MD2)

SECRETARY5 <- lmer(EC ~ SECRETARY + EthnicFractionalization + SECRETARY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(SECRETARY5)
R2EC <- r2beta(SECRETARY5, partial = TRUE, method = "nsj", data = MD2)

SECRETARY6 <- lmer(EC ~ SECRETARY + ClassFractionalization + SECRETARY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(SECRETARY6)
R2EC <- r2beta(SECRETARY6, partial = TRUE, method = "nsj", data = MD2)

FAMILY1 <- lmer(EC ~ FAMILY + DM + FAMILY*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(FAMILY1)
R2EC <- r2beta(FAMILY1, partial = TRUE, method = "nsj", data = MD2)

FAMILY2 <- lmer(EC ~ FAMILY + ED + FAMILY*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(FAMILY2)
R2EC <- r2beta(FAMILY2, partial = TRUE, method = "nsj", data = MD2)

FAMILY3 <- lmer(EC ~ FAMILY + ENP + FAMILY*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(FAMILY3)
R2EC <- r2beta(FAMILY3, partial = TRUE, method = "nsj", data = MD2)

FAMILY4 <- lmer(EC ~ FAMILY + RAI + FAMILY*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(FAMILY4)
R2EC <- r2beta(FAMILY4, partial = TRUE, method = "nsj", data = MD2)

FAMILY5 <- lmer(EC ~ FAMILY + EthnicFractionalization + FAMILY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(FAMILY5)
R2EC <- r2beta(FAMILY5, partial = TRUE, method = "nsj", data = MD2)

FAMILY6 <- lmer(EC ~ FAMILY + ClassFractionalization + FAMILY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(FAMILY6)
R2EC <- r2beta(FAMILY6, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT1 <- lmer(EC ~ LEFTRIGHT + DM + LEFTRIGHT*DM + (1 | REGION), data = MD2, REML = FALSE)
summary(LEFTRIGHT1)
R2EC <- r2beta(LEFTRIGHT1, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT2 <- lmer(EC ~ LEFTRIGHT + ED + LEFTRIGHT*ED + (1 | REGION), data = MD2, REML = FALSE)
summary(LEFTRIGHT2)
R2EC <- r2beta(LEFTRIGHT2, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT3 <- lmer(EC ~ LEFTRIGHT + ENP + LEFTRIGHT*ENP + (1 | REGION), data = MD2, REML = FALSE)
summary(LEFTRIGHT3)
R2EC <- r2beta(LEFTRIGHT3, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT4 <- lmer(EC ~ LEFTRIGHT + RAI + LEFTRIGHT*RAI + (1 | REGION), data = MD2, REML = FALSE)
summary(LEFTRIGHT4)
R2EC <- r2beta(LEFTRIGHT4, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT5 <- lmer(EC ~ LEFTRIGHT + EthnicFractionalization + LEFTRIGHT*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(LEFTRIGHT5)
R2EC <- r2beta(LEFTRIGHT5, partial = TRUE, method = "nsj", data = MD2)

LEFTRIGHT6 <- lmer(EC ~ LEFTRIGHT + ClassFractionalization + LEFTRIGHT*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
summary(LEFTRIGHT6)
R2EC <- r2beta(LEFTRIGHT6, partial = TRUE, method = "nsj", data = MD2)

CLIENTELISM1 <- lmer(ES ~ CLIENTELISM + DM + CLIENTELISM*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = CLIENTELISM1, var1 = "CLIENTELISM", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM2 <- lmer(ES ~ CLIENTELISM + ED + CLIENTELISM*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = CLIENTELISM2, var1 = "CLIENTELISM", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of District Number") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM3 <- lmer(ES ~ CLIENTELISM + RAI + CLIENTELISM*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = CLIENTELISM3, var1 = "CLIENTELISM", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM4 <- lmer(ES ~ CLIENTELISM + ENP + CLIENTELISM*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = CLIENTELISM4, var1 = "CLIENTELISM", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM5 <- lmer(ES ~ CLIENTELISM + EthnicFractionalization + CLIENTELISM*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = CLIENTELISM5, var1 = "CLIENTELISM", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM6 <- lmer(ES ~ CLIENTELISM + ClassFractionalization + CLIENTELISM*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = CLIENTELISM6, var1 = "CLIENTELISM", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure 4.2

CLIENTELISM10 <- lmer(EC ~ CLIENTELISM + DM + CLIENTELISM*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = CLIENTELISM10, var1 = "CLIENTELISM", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance ofDistrict Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM20 <- lmer(EC ~ CLIENTELISM + ED + CLIENTELISM*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = CLIENTELISM20, var1 = "CLIENTELISM", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of District Number") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM30 <- lmer(EC ~ CLIENTELISM + RAI + CLIENTELISM*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = CLIENTELISM30, var1 = "CLIENTELISM", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM40 <- lmer(EC ~ CLIENTELISM + ENP + CLIENTELISM*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = CLIENTELISM40, var1 = "CLIENTELISM", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM50 <- lmer(EC ~ CLIENTELISM + EthnicFractionalization + CLIENTELISM*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = CLIENTELISM50, var1 = "CLIENTELISM", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

CLIENTELISM60 <- lmer(EC ~ CLIENTELISM + ClassFractionalization + CLIENTELISM*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = CLIENTELISM60, var1 = "CLIENTELISM", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for CLIENTELISM") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of CLIENTELISM \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure 4.3

COUNCILLOR1 <- lmer(ES ~ COUNCILLOR + DM + COUNCILLOR*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = COUNCILLOR1, var1 = "COUNCILLOR", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on ES \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR2 <- lmer(ES ~ COUNCILLOR + ED + COUNCILLOR*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = COUNCILLOR2, var1 = "COUNCILLOR", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on ES \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR3 <- lmer(ES ~ COUNCILLOR + RAI + COUNCILLOR*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = COUNCILLOR3, var1 = "COUNCILLOR", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on ES \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR4 <- lmer(ES ~ COUNCILLOR + ENP + COUNCILLOR*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = COUNCILLOR4, var1 = "COUNCILLOR", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on ES \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR5 <- lmer(ES ~ COUNCILLOR + EthnicFractionalization + COUNCILLOR*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = COUNCILLOR5, var1 = "COUNCILLOR", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on ES \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR6 <- lmer(ES ~ COUNCILLOR + ClassFractionalization + COUNCILLOR*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = COUNCILLOR6, var1 = "COUNCILLOR", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on ES \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.1

FEDERALDEPUTY1 <- lmer(ES ~ FEDERALDEPUTY + DM + FEDERALDEPUTY*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = FEDERALDEPUTY1, var1 = "FEDERALDEPUTY", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for FEDERAL DEPUTY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FEDERAL DEPUTY on ES \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FEDERALDEPUTY2 <- lmer(ES ~ FEDERALDEPUTY + ED + FEDERALDEPUTY*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = FEDERALDEPUTY2, var1 = "FEDERALDEPUTY", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for FEDERAL DEPUTY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FEDERAL DEPUTY on ES \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FEDERALDEPUTY3 <- lmer(ES ~ FEDERALDEPUTY + RAI + FEDERALDEPUTY*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = FEDERALDEPUTY3, var1 = "FEDERALDEPUTY", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for FEDERAL DEPUTY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FEDERAL DEPUTY on ES \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FEDERALDEPUTY4 <- lmer(ES ~ FEDERALDEPUTY + ENP + FEDERALDEPUTY*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = FEDERALDEPUTY4, var1 = "FEDERALDEPUTY", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for FEDERAL DEPUTY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FEDERAL DEPUTY on ES \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FEDERALDEPUTY5 <- lmer(ES ~ FEDERALDEPUTY + EthnicFractionalization + FEDERALDEPUTY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = FEDERALDEPUTY5, var1 = "FEDERALDEPUTY", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for FEDERAL DEPUTY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FEDERAL DEPUTY on ES \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FEDERALDEPUTY6 <- lmer(ES ~ FEDERALDEPUTY + ClassFractionalization + FEDERALDEPUTY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = FEDERALDEPUTY6, var1 = "FEDERALDEPUTY", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for FEDERAL DEPUTY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FEDERAL DEPUTY on ES \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.1

MINISTER1 <- lmer(ES ~ MINISTER + DM + MINISTER*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = MINISTER1, var1 = "MINISTER", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for MINISTER") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of MINISTER on ES \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

MINISTER2 <- lmer(ES ~ MINISTER + ED + MINISTER*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = MINISTER2, var1 = "MINISTER", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for MINISTER") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of MINISTER on ES \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

MINISTER3 <- lmer(ES ~ MINISTER + RAI + MINISTER*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = MINISTER3, var1 = "MINISTER", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for MINISTER") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of MINISTER on ES \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

MINISTER4 <- lmer(ES ~ MINISTER + ENP + MINISTER*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = MINISTER4, var1 = "MINISTER", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for MINISTER") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of MINISTER on ES \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

MINISTER5 <- lmer(ES ~ MINISTER + EthnicFractionalization + MINISTER*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = FEDERALDEPUTY5, var1 = "FEDERALDEPUTY", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for MINISTER") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of MINISTER on ES \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

MINISTER6 <- lmer(ES ~ MINISTER + ClassFractionalization + MINISTER*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = MINISTER6, var1 = "MINISTER", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for MINISTER") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of MINISTER on ES \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.1

SWITCH1 <- lmer(EC ~ SWITCH + DM + SWITCH*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = SWITCH1, var1 = "SWITCH", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for SWITCH") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SWITCH on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SWITCH2 <- lmer(EC ~ SWITCH + ED + SWITCH*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = SWITCH2, var1 = "SWITCH", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for SWITCH") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SWITCH on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SWITCH3 <- lmer(EC ~ SWITCH + RAI + SWITCH*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = SWITCH3, var1 = "SWITCH", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for SWITCH") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SWITCH on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SWITCH4 <- lmer(EC ~ SWITCH + ENP + SWITCH*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = SWITCH4, var1 = "SWITCH", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for SWITCH") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SWITCH on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SWITCH5 <- lmer(EC ~ SWITCH + EthnicFractionalization + SWITCH*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = SWITCH5, var1 = "SWITCH", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for SWITCH") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SWITCH on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SWITCH6 <- lmer(EC ~ SWITCH + ClassFractionalization + SWITCH*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = SWITCH6, var1 = "SWITCH", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for SWITCH") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SWITCH on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.2

ECONOMY1 <- lmer(EC ~ ECONOMY + DM + ECONOMY*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = ECONOMY1, var1 = "ECONOMY", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for ECONOMY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of ECONOMY on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ECONOMY2 <- lmer(EC ~ ECONOMY + ED + ECONOMY*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = ECONOMY2, var1 = "ECONOMY", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for ECONOMY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of ECONOMY on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ECONOMY3 <- lmer(EC ~ ECONOMY + RAI + ECONOMY*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = ECONOMY3, var1 = "ECONOMY", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for ECONOMY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of ECONOMY on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ECONOMY4 <- lmer(EC ~ ECONOMY + ENP + ECONOMY*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = ECONOMY4, var1 = "ECONOMY", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for ECONOMY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of ECONOMY on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ECONOMY5 <- lmer(EC ~ ECONOMY + EthnicFractionalization + ECONOMY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = ECONOMY5, var1 = "ECONOMY", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for ECONOMY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of ECONOMY on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ECONOMY6 <- lmer(EC ~ ECONOMY + ClassFractionalization + ECONOMY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = ECONOMY6, var1 = "ECONOMY", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for ECONOMY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of ECONOMY on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.2

LOCALINTERESTS1 <- lmer(EC ~ LOCALINTERESTS + DM + LOCALINTERESTS*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = LOCALINTERESTS1, var1 = "LOCALINTERESTS", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for LOCAL INTERESTS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LOCAL INTERESTS on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LOCALINTERESTS2 <- lmer(EC ~ LOCALINTERESTS + ED + LOCALINTERESTS*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = LOCALINTERESTS2, var1 = "LOCALINTERESTS", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for LOCAL INTERESTS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LOCAL INTERESTS on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LOCALINTERESTS3 <- lmer(EC ~ LOCALINTERESTS + RAI + LOCALINTERESTS*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = LOCALINTERESTS3, var1 = "LOCALINTERESTS", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for LOCAL INTERESTS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LOCAL INTERESTS on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LOCALINTERESTS4 <- lmer(EC ~ LOCALINTERESTS + ENP + LOCALINTERESTS*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = LOCALINTERESTS4, var1 = "LOCALINTERESTS", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for LOCAL INTERESTS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LOCAL INTERESTS on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LOCALINTERESTS5 <- lmer(EC ~ LOCALINTERESTS + EthnicFractionalization + LOCALINTERESTS*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = LOCALINTERESTS5, var1 = "LOCALINTERESTS", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for LOCAL INTERESTS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LOCAL INTERESTS on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LOCALINTERESTS6 <- lmer(EC ~ LOCALINTERESTS + ClassFractionalization + LOCALINTERESTS*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = LOCALINTERESTS6, var1 = "LOCALINTERESTS", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for LOCAL INTERESTS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LOCAL INTERESTS on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.2

COUNCILLOR1 <- lmer(EC ~ COUNCILLOR + DM + COUNCILLOR*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = COUNCILLOR1, var1 = "COUNCILLOR", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR2 <- lmer(EC ~ COUNCILLOR + ED + COUNCILLOR*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = COUNCILLOR2, var1 = "COUNCILLOR", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for COUNCILLORS") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR3 <- lmer(EC ~ COUNCILLOR + RAI + COUNCILLOR*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = COUNCILLOR3, var1 = "COUNCILLOR", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR4 <- lmer(EC ~ COUNCILLOR + ENP + COUNCILLOR*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = COUNCILLOR4, var1 = "COUNCILLOR", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR5 <- lmer(EC ~ COUNCILLOR + EthnicFractionalization + COUNCILLOR*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = COUNCILLOR5, var1 = "COUNCILLOR", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

COUNCILLOR6 <- lmer(EC ~ COUNCILLOR + ClassFractionalization + COUNCILLOR*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = COUNCILLOR6, var1 = "COUNCILLOR", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for COUNCILLOR") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of COUNCILLOR on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.2

SECRETARY1 <- lmer(EC ~ SECRETARY + DM + SECRETARY*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = SECRETARY1, var1 = "SECRETARY", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for SECRETARY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SECRETARY on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SECRETARY2 <- lmer(EC ~ SECRETARY + ED + SECRETARY*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = SECRETARY2, var1 = "SECRETARY", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for SECRETARY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SECRETARY on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SECRETARY3 <- lmer(EC ~ SECRETARY + RAI + SECRETARY*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = SECRETARY3, var1 = "SECRETARY", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for SECRETARY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SECRETARY on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SECRETARY4 <- lmer(EC ~ SECRETARY + ENP + SECRETARY*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = SECRETARY4, var1 = "SECRETARY", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for SECRETARY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SECRETARY on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SECRETARY5 <- lmer(EC ~ SECRETARY + EthnicFractionalization + SECRETARY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = SECRETARY5, var1 = "SECRETARY", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for SECRETARY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SECRETARY on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

SECRETARY6 <- lmer(EC ~ SECRETARY + ClassFractionalization + SECRETARY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = SECRETARY6, var1 = "SECRETARY", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for SECRETARY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of SECRETARY on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.2

FAMILY1 <- lmer(EC ~ FAMILY + DM + FAMILY*DM + (1 | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = FAMILY1, var1 = "FAMILY", var2 = "DM", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for FAMILY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FAMILY on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FAMILY2 <- lmer(EC ~ FAMILY + ED + FAMILY*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = FAMILY2, var1 = "FAMILY", var2 = "ED", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for FAMILY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FAMILY on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FAMILY3 <- lmer(EC ~ FAMILY + RAI + FAMILY*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = FAMILY3, var1 = "FAMILY", var2 = "RAI", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for FAMILY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FAMILY on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FAMILY4 <- lmer(EC ~ FAMILY + ENP + FAMILY*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = FAMILY4, var1 = "FAMILY", var2 = "ENP", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for FAMILY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FAMILY on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FAMILY5 <- lmer(EC ~ FAMILY + EthnicFractionalization + FAMILY*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = FAMILY5, var1 = "FAMILY", var2 = "EthnicFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for FAMILY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FAMILY on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

FAMILY6 <- lmer(EC ~ FAMILY + ClassFractionalization + FAMILY*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = FAMILY6, var1 = "FAMILY", var2 = "ClassFractionalization", hist = TRUE) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for FAMILY") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of FAMILY on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 10), axis.title.y = element_text(size = 10), legend.position = "none", text = element_text(size = 10, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure C.1.2

LEFTRIGHT1 <- lmer(EC ~ LEFTRIGHT + DM + LEFTRIGHT*DM + (LEFTRIGHT | REGION), data = MD2, REML = FALSE)
C1 <- interplot(m = LEFTRIGHT1, var1 = "LEFTRIGHT", var2 = "DM", hist = F) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Magnitude") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT on EC \nby Regional Variance of District Magnitude") +
  theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 12), legend.position = "none", text = element_text(size = 12, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT2 <- lmer(EC ~ LEFTRIGHT + ED + LEFTRIGHT*ED + (1 | REGION), data = MD2, REML = FALSE)
C2 <- interplot(m = LEFTRIGHT2, var1 = "LEFTRIGHT", var2 = "ED", hist = F) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of District Numbers") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT on EC \nby Regional Variance of District Numbers") +
  theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 12), legend.position = "none", text = element_text(size = 12, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT3 <- lmer(EC ~ LEFTRIGHT + RAI + LEFTRIGHT*RAI + (1 | REGION), data = MD2, REML = FALSE)
C3 <- interplot(m = LEFTRIGHT3, var1 = "LEFTRIGHT", var2 = "RAI", hist = F) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Variance of Regional Authority Index") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT on EC \nby Variance of Regional Authority Index") +
  theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 12), legend.position = "none", text = element_text(size = 12, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT4 <- lmer(EC ~ LEFTRIGHT + ENP + LEFTRIGHT*ENP + (1 | REGION), data = MD2, REML = FALSE)
C4 <- interplot(m = LEFTRIGHT4, var1 = "LEFTRIGHT", var2 = "ENP", hist = F) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Effective Number of Parties") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT on EC \nby Regional Variance of Effective Number of Parties") +
  theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 12), legend.position = "none", text = element_text(size = 12, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT5 <- lmer(EC ~ LEFTRIGHT + EthnicFractionalization + LEFTRIGHT*EthnicFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C5 <- interplot(m = LEFTRIGHT5, var1 = "LEFTRIGHT", var2 = "EthnicFractionalization", hist = F) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Ethnic Fractionalization") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT on EC \nby Regional Variance of Ethnic Fractionalization") +
  theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 12), legend.position = "none", text = element_text(size = 12, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

LEFTRIGHT6 <- lmer(EC ~ LEFTRIGHT + ClassFractionalization + LEFTRIGHT*ClassFractionalization + (1 | REGION), data = MD2, REML = FALSE)
C6 <- interplot(m = LEFTRIGHT6, var1 = "LEFTRIGHT", var2 = "ClassFractionalization", hist = F) + 
  # Add labels for X and Y axes
  aes(color = "red") +
  xlab("Regional Variance of Class Fractionalization") +
  ylab("Estimated Coefficient for LEFT RIGHT") +
  # Change the background
  theme_classic() +
  # Add the title
  ggtitle("Estimated Coefficient of LEFT RIGHT on EC \nby Regional Variance of Class Fractionalization") +
  theme(plot.title = element_text(size = 12), axis.title.y = element_text(size = 12), legend.position = "none", text = element_text(size = 12, family = "serif")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(C1, C2, C3, C4, C5, C6,
          ncol = 2, nrow = 3) # Figure 4.2



