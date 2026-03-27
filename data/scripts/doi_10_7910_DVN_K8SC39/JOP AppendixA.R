

library(foreign)
library(car)
library(xtable)
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(effects)


setwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')

appA <- read.csv('jop_appendixA.csv')

#create ID for cutoffs
appA$grp <- 0
appA$grp[appA$margin < 0.5] <- 1
appA$grp[appA$margin < 1 & appA$margin >= 0.5] <- 2
#appA$grp[appA$margin < 2.5 & appA$margin >= 1] <- 3

#remove cases with margin greater than 1
appA <- subset(appA, grp != 0)

#subset by election timing
nov <- subset(appA, nov == 1)
pre <- subset(appA, prenov == 1)
post <- subset(appA, postnov == 1)

out <- array(NA, dim = c(9, 2))
se <- array(NA, dim = c(9,2))

#estimate balance test for lagged congressional vote, lagged candidate quality, and lagged incumbency
for(i in 1:2){

t1 <- subset(nov, grp <= i)
t2 <- subset(pre, grp <= i)
t3 <- subset(post, grp <= i)

#balance test for Nov. elections
b1 <- lm(dvp_c ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t1)
b2 <- lm(demqa_t1 ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t1)
b3 <- lm(inc_t1 ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t1)

#balance test for Pre-Nov. elections
b4 <- lm(dvp_c ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t2)
b5 <- lm(demqa_t1 ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t2)
b6 <- lm(inc_t1 ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t2)

#balance test for Pre-Nov. elections
b7 <- lm(dvp_c ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t3)
b8 <- lm(demqa_t1 ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t3)
b9 <- lm(inc_t1 ~ I(dpres_c)*dpres_win + I(dpres_c^2)*dpres_win , t3)

#extract coefficients and SEs

out[1, i] <- coeftest(b1, vcov = hccm(b1))[3,1] 
out[2, i] <- coeftest(b2, vcov = hccm(b2))[3,1] 
out[3, i] <- coeftest(b3, vcov = hccm(b3))[3,1] 

out[4, i] <- coeftest(b4, vcov = hccm(b4))[3,1] 
out[5, i] <- coeftest(b5, vcov = hccm(b5))[3,1] 
out[6, i] <- coeftest(b6, vcov = hccm(b6))[3,1] 

out[7, i] <- coeftest(b7, vcov = hccm(b7))[3,1] 
out[8, i] <- coeftest(b8, vcov = hccm(b8))[3,1] 
out[9, i] <- coeftest(b9, vcov = hccm(b9))[3,1] 

se[1, i] <- coeftest(b1, vcov = hccm(b1))[3,2] 
se[2, i] <- coeftest(b2, vcov = hccm(b2))[3,2] 
se[3, i] <- coeftest(b3, vcov = hccm(b3))[3,2] 

se[4, i] <- coeftest(b4, vcov = hccm(b4))[3,2] 
se[5, i] <- coeftest(b5, vcov = hccm(b5))[3,2] 
se[6, i] <- coeftest(b6, vcov = hccm(b6))[3,2] 

se[7, i] <- coeftest(b7, vcov = hccm(b7))[3,2] 
se[8, i] <- coeftest(b8, vcov = hccm(b8))[3,2] 
se[9, i] <- coeftest(b9, vcov = hccm(b9))[3,2] 

}

tab <- cbind(out[,1], se[,1], out[,2], se[,2])


