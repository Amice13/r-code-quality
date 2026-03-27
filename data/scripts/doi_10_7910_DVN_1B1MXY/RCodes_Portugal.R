# Read relevant packages
library(mokken)
library(readr)

# Read data file
portugal_new <- read_csv("~<PATH>Portugal_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 26 in Appendix)
portugal1 <- data.frame(na.omit(portugal_new[,c("q9","q10","q13","q14")]))
summary(check.monotonicity(portugal1,minsize=100))
coefH(portugal1)

portugal1 <- data.frame(na.omit(portugal_new[,c("q26_rev","q27_rev","q28_rev")]))
summary(check.monotonicity(portugal1,minsize=100))
coefH(portugal1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
portugal1 <- data.frame(na.omit(portugal_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(portugal1)

portugal1 <- data.frame(na.omit(portugal_new[,c("q9","q10","q11","q12_rev","q13","q14")]))
coefH(portugal1)

portugal1 <- data.frame(na.omit(portugal_new[,c("q22","q23","q24","q25_rev","q26_rev","q27_rev","q28_rev")]))
coefH(portugal1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(portugal_new)
xpvec <- (q9 + q10 + q11 + q12_rev + q13 + q14)/24; ypvec <- (q22 + q23 + q24 + q25_rev + q26_rev + q27_rev + q28_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
portugal_new1 <- cbind(portugal_new,xpvec,ypvec,zpvec)
por <- portugal_new1[!is.na(portugal_new1$xpvec)&!is.na(portugal_new1$ypvec),]
cor(por$xpvec,por$ypvec)
por <- portugal_new1[!is.na(portugal_new1$xpvec)&!is.na(portugal_new1$zpvec),]
cor(por$xpvec,por$zpvec)
por <- portugal_new1[!is.na(portugal_new1$ypvec)&!is.na(portugal_new1$zpvec),]
cor(por$ypvec,por$zpvec)
detach(portugal_new)