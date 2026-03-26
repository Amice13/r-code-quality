# Read relevant packages
library(mokken)
library(readr)

# Read data file
ireland_new <- read_csv("~<PATH>Ireland_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 22 in Appendix)
ireland1 <- data.frame(na.omit(ireland_new[,c("q1","q2","q6","q16_rev")]))
summary(check.monotonicity(ireland1,minsize=100))
coefH(ireland1)

ireland1 <- data.frame(na.omit(ireland_new[,c("q27","q28","q29")]))
summary(check.monotonicity(ireland1,minsize=100))
coefH(ireland1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
ireland1 <- data.frame(na.omit(ireland_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(ireland1)

ireland1 <- data.frame(na.omit(ireland_new[,c("q10","q11","q12","q13_rev","q14","q15")]))
coefH(ireland1)

ireland1 <- data.frame(na.omit(ireland_new[,c("q23","q24","q25","q26_rev","q27_rev","q28_rev","q29_rev")]))
coefH(ireland1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(ireland_new)
xpvec <- (q10 + q11 + q12 + q13_rev + q14 + q15)/24; ypvec <- (q23 + q24 + q25 + q26_rev + q27_rev + q28_rev + q29_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
ireland_new1 <- cbind(ireland_new,xpvec,ypvec,zpvec)
ire <- ireland_new1[!is.na(ireland_new1$xpvec)&!is.na(ireland_new1$ypvec),]
cor(ire$xpvec,ire$ypvec)
ire <- ireland_new1[!is.na(ireland_new1$xpvec)&!is.na(ireland_new1$zpvec),]
cor(ire$xpvec,ire$zpvec)
ire <- ireland_new1[!is.na(ireland_new1$ypvec)&!is.na(ireland_new1$zpvec),]
cor(ire$ypvec,ire$zpvec)
detach(ireland_new)
