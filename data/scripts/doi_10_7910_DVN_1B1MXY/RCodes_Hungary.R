# Read relevant packages
library(mokken)
library(readr)

# Read data file
hungary_new <- read_csv("~<PATH>Hungary_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 21 in Appendix)
hungary1 <- data.frame(na.omit(hungary_new[,c("q1_rev","q2_rev","q3_rev","q4","q6_rev","q12","q14","q18","q26","q28")]))
coefH(hungary1)

# Use subset of unique observations to generate "Crit" values (see Appendix)
hungary_new1 <- unique(hungary_new)

hungary1 <- data.frame(na.omit(hungary_new1[,c("q1_rev","q2_rev","q3_rev","q4","q6_rev","q12","q14","q18","q26","q28")]))
summary(check.monotonicity(hungary1),minsize=25)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
hungary1 <- data.frame(na.omit(hungary_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(hungary1)

hungary1 <- data.frame(na.omit(hungary_new[,c("q12","q13","q14","q15_rev","q16","q17")]))
coefH(hungary1)

hungary1 <- data.frame(na.omit(hungary_new[,c("q22","q23","q24","q25_rev","q26_rev","q27_rev","q28_rev")]))
coefH(hungary1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(hungary_new)
xpvec <- (q12 + q13 + q14 + q15_rev + q16 + q17)/24; ypvec <- (q22 + q23 + q24 + q25_rev + q26_rev + q27_rev + q28_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
hungary_new1 <- cbind(hungary_new,xpvec,ypvec,zpvec)
hu <- hungary_new1[!is.na(hungary_new1$xpvec)&!is.na(hungary_new1$ypvec),]
cor(hu$xpvec,hu$ypvec)
hu <- hungary_new1[!is.na(hungary_new1$xpvec)&!is.na(hungary_new1$zpvec),]
cor(hu$xpvec,hu$zpvec)
hu <- hungary_new1[!is.na(hungary_new1$ypvec)&!is.na(hungary_new1$zpvec),]
cor(hu$ypvec,hu$zpvec)
detach(hungary_new)
