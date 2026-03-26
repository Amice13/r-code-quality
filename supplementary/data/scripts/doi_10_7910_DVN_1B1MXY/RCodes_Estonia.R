# Read relevant packages
library(mokken)
library(readr)

# Read data file
estonia_new <- read_csv("~<PATH>Estonia_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 14 in Appendix)
estonia1 <- data.frame(na.omit(estonia_new[,c("q1_rev","q2_rev","q4","q6_rev","q7")]))
summary(check.monotonicity(estonia1,minsize=50))
coefH(estonia1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
estonia1 <- data.frame(na.omit(estonia_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(estonia1)

estonia1 <- data.frame(na.omit(estonia_new[,c("q10","q11","q12","q13_rev","q14","q15")]))
coefH(estonia1)

estonia1 <- data.frame(na.omit(estonia_new[,c("q20","q21","q22","q23_rev","q24_rev","q25_rev","q26_rev")]))
coefH(estonia1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(estonia_new)
xpvec <- (q10 + q11 + q12 + q13_rev + q14 + q15)/24; ypvec <- (q20 + q21 + q22 + q23_rev + q24_rev + q25_rev + q26_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
estonia_new <- cbind(estonia_new,xpvec,ypvec,zpvec)
ee <- estonia_new[!is.na(estonia_new$xpvec)&!is.na(estonia_new$ypvec),]
cor(ee$xpvec,ee$ypvec)
ee <- estonia_new[!is.na(estonia_new$xpvec)&!is.na(estonia_new$zpvec),]
cor(ee$xpvec,ee$zpvec)
ee <- estonia_new[!is.na(estonia_new$ypvec)&!is.na(estonia_new$zpvec),]
cor(ee$ypvec,ee$zpvec)
detach(estonia_new)