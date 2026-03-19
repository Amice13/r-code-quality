# Read relevant packages
library(mokken)
library(readr)

# Read data file
france_new <- read_csv("~<PATH>France_myBal.csv")

# Scales that emerge from 20 of the 21 common items  (see Table 16 in Appendix)
france1 <- data.frame(na.omit(france_new[,c("q12","q13","q15","q16")]))
summary(check.monotonicity(france1,minsize=50))
coefH(france1)

france1 <- data.frame(na.omit(france_new[,c("q1","q3","q4_rev","q5_rev","q6","q21","q25_rev")]))
summary(check.monotonicity(france1,minsize=50))
coefH(france1)

##########
# Scales that emerge from 21 items (including AD1, see Table 17 in Appendix))
france1 <- data.frame(na.omit(france_new[,c("q12","q13","q15","q16")]))
summary(check.monotonicity(france1,minsize=100))
coefH(france1)

france1 <- data.frame(na.omit(france_new[,c("q1","q3","q4_rev","q5_rev","q6","q21","q25_rev","q28")]))
summary(check.monotonicity(france1,minsize=100))
coefH(france1)


# Dimension analysis on 18 pre-defined items (see Table 2 in Appendix)
france1 <- data.frame(na.omit(france_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(france1)

france1 <- data.frame(na.omit(france_new[,c("q12","q13","q14_rev","q15","q16")]))
coefH(france1)

france1 <- data.frame(na.omit(france_new[,c("q21","q22","q23","q24_rev","q25_rev","q26_rev","q27_rev")]))
coefH(france1)


# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(france_new)
xpvec <- (q12 + q13 + q14_rev + q15 + q16)/20; ypvec <- (q21 + q22 + q23 + q24_rev + q25_rev + q26_rev + q27_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
france_new1 <- cbind(france_new,xpvec,ypvec,zpvec)
fr <- france_new1[!is.na(france_new1$xpvec)&!is.na(france_new1$ypvec),]
cor(fr$xpvec,fr$ypvec)
fr <- france_new1[!is.na(france_new1$xpvec)&!is.na(france_new1$zpvec),]
cor(fr$xpvec,fr$zpvec)
fr <- france_new1[!is.na(france_new1$ypvec)&!is.na(france_new1$zpvec),]
cor(fr$ypvec,fr$zpvec)
detach(france_new)
