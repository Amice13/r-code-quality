# OLS Models for 2010 Data #

library(foreign)

# Load 2010 TESS data
TESS <- read.dta(file.choose())

# Create Race Vars
TESS$White <- ifelse(TESS$PPETHM == "White, Non-Hispanic", 1, 0)
table(TESS$White)

TESS$Black <- ifelse(TESS$PPETHM == "Black, Non-Hispanic", 1, 0)
table(TESS$Black)

TESS$Hispanic <- ifelse(TESS$PPETHM == "Hispanic", 1, 0)
table(TESS$Hispanic)

# Data set includes whites, Af. Ams., Latinos
TESS1 <- TESS[which(TESS$White == 1 | TESS$Black == 1 | TESS$Hispanic == 1), ]

# Use only rankings group
TESS2 <- TESS1[which(TESS1$rankings == 1), ]

# Median Center Value Rankings
TESS2$RankEqMC <- TESS2$RankEq - median(TESS2$RankEq)
TESS2$RankESMC <- TESS2$RankES - median(TESS2$RankES)
TESS2$RankMTMC <- TESS2$RankMT - median(TESS2$RankMT)
TESS2$RankFreeMC <- TESS2$RankFree - median(TESS2$RankFree)
TESS2$RankLOMC <- TESS2$RankLO - median(TESS2$RankLO)

### 2010 IDEOLOGY ###

# Big Model (reported)
OLSIdeol1 <- lm(ideol ~ age7 + female + educ4 + hhinc +
                  Hispanic*(RankEqMC) + Hispanic*(RankESMC) +
                  Hispanic*(RankMTMC) + Black*(RankEqMC) + 
                  Black*(RankESMC) +Black*(RankMTMC), 
                data = TESS2)
summary(OLSIdeol1)

OLSIdeol1Small <- lm(ideol ~ age7 + female + educ4 + hhinc +
                       Hispanic + Black + RankEqMC + RankESMC + RankMTMC, 
                     data = TESS2)
summary(OLSIdeol1Small)

# F-stat
anova(OLSIdeol1, OLSIdeol1Small)

### 2010 PARTY ###

# Big Model (reported)
OLSParty1 <- lm(party ~ age7 + female + educ4 + hhinc + 
                  Hispanic*(RankEqMC) + Hispanic*(RankESMC) +
                  Hispanic*(RankMTMC) + Black*(RankEqMC) + 
                  Black*(RankESMC) +Black*(RankMTMC), 
                data = TESS2)
summary(OLSParty1)

OLSParty1Small <- lm(party ~ age7 + female + educ4 + hhinc +
                       Hispanic + Black + RankEqMC + RankESMC + RankMTMC, 
                     data = TESS2)
summary(OLSParty1Small)

# F-stat
anova(OLSParty1, OLSParty1Small)


# 2002 Data #

# Load 2002 TESS Data
TESS2002 <- read.dta(file.choose())

# Create Race Vars
TESS2002$Black <- NA
TESS2002$Black[TESS2002$PPETH == "Black, Non-Hispanic"] <- 1
TESS2002$Black[TESS2002$PPETH == "Hispanic"] <- 0
TESS2002$Black[TESS2002$PPETH == "White, Non-Hispanic"] <- 0

TESS2002$Hispanic <- NA
TESS2002$Hispanic[TESS2002$PPETH == "Black, Non-Hispanic"] <- 0
TESS2002$Hispanic[TESS2002$PPETH == "Hispanic"] <- 1
TESS2002$Hispanic[TESS2002$PPETH == "White, Non-Hispanic"] <- 0

TESS2002$White <- NA
TESS2002$White[TESS2002$PPETH == "Black, Non-Hispanic"] <- 0
TESS2002$White[TESS2002$PPETH == "Hispanic"] <- 0
TESS2002$White[TESS2002$PPETH == "White, Non-Hispanic"] <- 1

# Creating Value Rankings
vars <- c("RankEq6", "RankFree6", "RankMT6", "RankES6", "RankLO6")

vals <- TESS2002[vars]

newdata <- data.frame(vals, t(apply(vals, 1, rank, ties.method="average")))

newdata$RankFreeN <- newdata$RankFree6.1
newdata$RankEqN <- newdata$RankEq6.1
newdata$RankESN <- newdata$RankES6.1
newdata$RankLON <- newdata$RankLO6.1
newdata$RankMTN <- newdata$RankMT6.1

vars2 <- c("RankFreeN", "RankEqN", "RankESN", "RankLON", "RankMTN")

TESS2002.2 <- data.frame(cbind(TESS2002, newdata[vars2]))

# Median Center Value Rankings
TESS2002.2$RankEqMC <- TESS2002.2$RankEqN - median(TESS2002.2$RankEqN)
TESS2002.2$RankESMC <- TESS2002.2$RankESN - median(TESS2002.2$RankESN)
TESS2002.2$RankMTMC <- TESS2002.2$RankMTN - median(TESS2002.2$RankMTN)

TESS2002.3 <- TESS2002.2[which(TESS2002.2$White == 1 | TESS2002.2$Black == 1 | 
                                 TESS2002.2$Hispanic == 1), ]

# Ideology #

# Big Model (reported)
OLSIdeol1 <- lm(ideol ~ age7 + female + educ4 + hhinc +
                  Hispanic*(RankEqMC) + Hispanic*(RankESMC) +
                  Hispanic*(RankMTMC) + Black*(RankEqMC) + 
                  Black*(RankESMC) +Black*(RankMTMC), 
                data = TESS2002.3)
summary(OLSIdeol1)

OLSIdeol1Small <- lm(ideol ~ age7 + female + educ4 + hhinc +
                       Hispanic + Black + RankEqMC + RankESMC + RankMTMC, 
                     data = TESS2002.3)
summary(OLSIdeol1Small)

# F-stat
anova(OLSIdeol1, OLSIdeol1Small)

# Party #

# Big Model (reported)
OLSParty1 <- lm(party ~ age7 + female + educ4 + hhinc + 
                  Hispanic*(RankEqMC) + Hispanic*(RankESMC) +
                  Hispanic*(RankMTMC) + Black*(RankEqMC) + 
                  Black*(RankESMC) +Black*(RankMTMC), 
                data = TESS2002.3)
summary(OLSParty1)

OLSParty1Small <- lm(party ~ age7 + female + educ4 + hhinc +
                       Hispanic + Black + RankEqMC + RankESMC + RankMTMC, 
                     data = TESS2002.3)
summary(OLSParty1Small)

# F-stat
anova(OLSParty1, OLSParty1Small)





