# Read relevant packages
library(mokken)
library(readr)

# Read data file
cyprus_new <- read_csv("~<PATH>Cyprus_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 6 in Appendix)
cyprus1 <- data.frame(na.omit(cyprus_new[,c("q1_rev","q5","q7_rev","q8","q9","q14","q15","q22")]))
coefH(cyprus1)

cyprus1 <- data.frame(na.omit(cyprus_new[,c("q4","q24_rev","q25_rev")]))
coefH(cyprus1)

# Use subset of unique observations to generate "Crit" values (see Appendix)
cyprus_new1 <- unique(cyprus_new)

cyprus1 <- data.frame(na.omit(cyprus_new1[,c("q1_rev","q5","q7_rev","q8","q9","q14","q15","q22")]))
summary(check.monotonicity(cyprus1,minsize=25))

cyprus1 <- data.frame(na.omit(cyprus_new1[,c("q4","q24_rev","q25_rev")]))
summary(check.monotonicity(cyprus1,minsize=25))


# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
cyprus1 <- data.frame(na.omit(cyprus_new[,c("q1_rev","q3_rev","q4_rev","q5","q6","q7_rev")]))
coefH(cyprus1)

cyprus1 <- data.frame(na.omit(cyprus_new[,c("q9","q10","q11","q12_rev","q13","q14")]))
coefH(cyprus1)

cyprus1 <- data.frame(na.omit(cyprus_new[,c("q20","q21","q22","q23_rev","q24_rev","q25_rev","q26_rev")]))
coefH(cyprus1)


# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(cyprus_new)
xpvec <- (q9 + q10 + q11 + q12_rev + q13 + q14)/24; ypvec <- (q20 + q21 + q22 + q23_rev + q24_rev + q25_rev + q26_rev)/28; zpvec <- (q1_rev + q3_rev + q4_rev + q5 + q6 + q7_rev)/24
cyprus_new1 <- cbind(cyprus_new,xpvec,ypvec,zpvec)
cyp <- cyprus_new1[!is.na(cyprus_new1$xpvec)&!is.na(cyprus_new1$ypvec),]
cor(cyp$xpvec,cyp$ypvec)
cyp <- cyprus_new1[!is.na(cyprus_new1$xpvec)&!is.na(cyprus_new1$zpvec),]
cor(cyp$xpvec,cyp$zpvec)
cyp <- cyprus_new1[!is.na(cyprus_new1$ypvec)&!is.na(cyprus_new1$zpvec),]
cor(cyp$ypvec,cyp$zpvec)
detach(cyprus_new)
