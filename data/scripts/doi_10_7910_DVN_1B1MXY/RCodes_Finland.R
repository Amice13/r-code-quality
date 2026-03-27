# Read relevant packages
library(mokken)
library(readr)

# Read data file
finland_new <- read_csv("~<PATH>Finland_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 15 in Appendix)
finland1 <- data.frame(na.omit(finland_new[,c("q1","q2","q3","q6","q22","q26_rev","q27_rev")]))
summary(check.monotonicity(finland1,minsize=50))
coefH(finland1)

finland1 <- data.frame(na.omit(finland_new[,c("q12","q13","q14","q15_rev","q16","q17")]))
summary(check.monotonicity(finland1,minsize=50))
coefH(finland1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
finland1 <- data.frame(na.omit(finland_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(finland1)

finland1 <- data.frame(na.omit(finland_new[,c("q12","q13","q14","q15_rev","q16","q17")]))
coefH(finland1)

finland1 <- data.frame(na.omit(finland_new[,c("q22","q23","q24","q25_rev","q26_rev","q27_rev","q28_rev")]))
coefH(finland1)


# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(finland_new)
xpvec <- (q12 + q13 + q14 + q15_rev + q16 + q17)/24; ypvec <- (q22 + q23 + q24 + q25_rev + q26_rev + q27_rev + q28_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
finland_new1 <- cbind(finland_new,xpvec,ypvec,zpvec)
fi <- finland_new1[!is.na(finland_new1$xpvec)&!is.na(finland_new1$ypvec),]
cor(fi$xpvec,fi$ypvec)
fi <- finland_new1[!is.na(finland_new1$xpvec)&!is.na(finland_new1$zpvec),]
cor(fi$xpvec,fi$zpvec)
fi <- finland_new1[!is.na(finland_new1$ypvec)&!is.na(finland_new1$zpvec),]
cor(fi$ypvec,fi$zpvec)
detach(finland_new)