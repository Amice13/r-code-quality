# Read relevant packages
library(mokken)
library(readr)

# Read data file
denmark_new <- read_csv("~<PATH>Denmark_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 8 in Appendix)
denmark1 <- data.frame(na.omit(denmark_new[,c("q14","q15","q16","q17_rev","q18")]))
summary(check.monotonicity(denmark1,minsize=100))
coefH(denmark1)

denmark1 <- data.frame(na.omit(denmark_new[,c("q1","q2","q3","q4_rev","q6")]))
summary(check.monotonicity(denmark1,minsize=100))
coefH(denmark1)

##########
# Scales that emerge from 22 items (including AD1, see Table 9 in Appendix))
denmark1 <- data.frame(na.omit(denmark_new[,c("q14","q15","q16","q17_rev","q18")]))
summary(check.monotonicity(denmark1,minsize=100))
coefH(denmark1)

denmark1 <- data.frame(na.omit(denmark_new[,c("q1","q2","q3","q6","q23","q30")]))
summary(check.monotonicity(denmark1,minsize=100))
coefH(denmark1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
denmark1 <- data.frame(na.omit(denmark_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(denmark1)

denmark1 <- data.frame(na.omit(denmark_new[,c("q14","q15","q16","q17_rev","q18","q19")]))
coefH(denmark1)

denmark1 <- data.frame(na.omit(denmark_new[,c("q23","q24","q25","q26_rev","q27_rev","q28_rev","q29_rev")]))
coefH(denmark1)


# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(denmark_new)
xpvec <- (q14 + q15 + q16 + q17_rev + q18 + q19)/24; ypvec <- (q23 + q24 + q25 + q26_rev + q27_rev + q28_rev + q29_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
denmark_new1 <- cbind(denmark_new,xpvec,ypvec,zpvec)
dk <- denmark_new1[!is.na(denmark_new1$xpvec)&!is.na(denmark_new1$ypvec),]
cor(dk$xpvec,dk$ypvec)
dk <- denmark_new1[!is.na(denmark_new1$xpvec)&!is.na(denmark_new1$zpvec),]
cor(dk$xpvec,dk$zpvec)
dk <- denmark_new1[!is.na(denmark_new1$ypvec)&!is.na(denmark_new1$zpvec),]
cor(dk$ypvec,dk$zpvec)
detach(denmark_new)