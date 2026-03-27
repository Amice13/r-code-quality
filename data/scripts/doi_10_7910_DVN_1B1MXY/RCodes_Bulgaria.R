# Read relevant packages
library(mokken)
library(readr)

# Read data file
bulgaria_new <- read_csv("~<PATH>Bulgaria_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 5 in Appendix)
bulgaria1 <- data.frame(na.omit(bulgaria_new[,c("q1_rev","q2_rev","q3_rev","q4","q6_rev")]))
coefH(bulgaria1)

# Use subset of unique observations to generate "Crit" values (see Appendix)
bulgaria_new1 <- unique(bulgaria_new)

bulgaria1 <- data.frame(na.omit(bulgaria_new1[,c("q1_rev","q2_rev","q3_rev","q4","q6_rev")]))
summary(check.monotonicity(bulgaria1),minsize=25)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
bulgaria1 <- data.frame(na.omit(bulgaria_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(bulgaria1)

bulgaria1 <- data.frame(na.omit(bulgaria_new[,c("q10","q11","q12","q13_rev","q14","q15")]))
coefH(bulgaria1)

bulgaria1 <- data.frame(na.omit(bulgaria_new[,c("q21","q22","q23","q24_rev","q25_rev","q26_rev","q27_rev")]))
coefH(bulgaria1)


# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(bulgaria_new)
xpvec <- (q10 + q11 + q12 + q13_rev + q14 + q15)/24; ypvec <- (q21 + q22 + q23 + q24_rev + q25_rev + q26_rev + q27_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
bulgaria_new1 <- cbind(bulgaria_new,xpvec,ypvec,zpvec)
bg <- bulgaria_new1[!is.na(bulgaria_new1$xpvec)&!is.na(bulgaria_new1$ypvec),]
cor(bg$xpvec,bg$ypvec)
bg <- bulgaria_new1[!is.na(bulgaria_new1$xpvec)&!is.na(bulgaria_new1$zpvec),]
cor(bg$xpvec,bg$zpvec)
bg <- bulgaria_new1[!is.na(bulgaria_new1$ypvec)&!is.na(bulgaria_new1$zpvec),]
cor(bg$ypvec,bg$zpvec)
detach(bulgaria_new)