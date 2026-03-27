# Read relevant packages
library(mokken)
library(readr)

# Read data file
germany_new <- read_csv("~<PATH>Germany_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 18 in Appendix)
germany1 <- data.frame(na.omit(germany_new[,c("q13","q15","q18")]))
summary(check.monotonicity(germany1,minsize=100))
coefH(germany1)

germany1 <- data.frame(na.omit(germany_new[,c("q1","q3","q6","q22","q26_rev")]))
summary(check.monotonicity(germany1,minsize=100))
coefH(germany1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
germany1 <- data.frame(na.omit(germany_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(germany1)

germany1 <- data.frame(na.omit(germany_new[,c("q13","q14","q15","q16_rev","q17","q18")]))
coefH(germany1)

germany1 <- data.frame(na.omit(germany_new[,c("q22","q23","q24","q25_rev","q26_rev","q27_rev","q28_rev")]))
coefH(germany1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(germany_new)
xpvec <- (q13 + q14 + q15 + q16_rev + q17 + q18)/24; ypvec <- (q22 + q23 + q24 + q25_rev + q26_rev + q27_rev + q28_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
germany_new1 <- cbind(germany_new,xpvec,ypvec,zpvec)
ger <- germany_new1[!is.na(germany_new1$xpvec)&!is.na(germany_new1$ypvec),]
cor(ger$xpvec,ger$ypvec)
ger <- germany_new1[!is.na(germany_new1$xpvec)&!is.na(germany_new1$zpvec),]
cor(ger$xpvec,ger$zpvec)
ger <- germany_new1[!is.na(germany_new1$ypvec)&!is.na(germany_new1$zpvec),]
cor(ger$ypvec,ger$zpvec)
detach(germany_new)