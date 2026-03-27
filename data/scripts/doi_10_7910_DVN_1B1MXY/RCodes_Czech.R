# Read relevant packages
library(mokken)
library(readr)

# Read data file
czech_new <- czech_new <- read_csv("~<PATH>Czech_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 5 in Appendix)
czech1 <- data.frame(na.omit(czech_new[,c("q12","q13","q16")]))
summary(check.monotonicity(czech1,minsize=100))
coefH(czech1)

czech1 <- data.frame(na.omit(czech_new[,c("q1","q2","q3","q6")]))
summary(check.monotonicity(czech1,minsize=100))
coefH(czech1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
czech1 <- data.frame(na.omit(czech_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(czech1)

czech1 <- data.frame(na.omit(czech_new[,c("q11","q12","q13","q14_rev","q15","q16")]))
coefH(czech1)

czech1 <- data.frame(na.omit(czech_new[,c("q20","q21","q22","q23_rev","q24_rev","q25_rev","q26_rev")]))
coefH(czech1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(czech_new)
xpvec <- (q11 + q12 + q13 + q14_rev + q15 + q16)/24; ypvec <- (q20 + q21 + q22 + q23_rev + q24_rev + q25_rev + q26_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
czech_new1 <- cbind(czech_new,xpvec,ypvec,zpvec)
cz <- czech_new1[!is.na(czech_new1$xpvec)&!is.na(czech_new1$ypvec),]
cor(cz$xpvec,cz$ypvec)
cz <- czech_new1[!is.na(czech_new1$xpvec)&!is.na(czech_new1$zpvec),]
cor(cz$xpvec,cz$zpvec)
cz <- czech_new1[!is.na(czech_new1$ypvec)&!is.na(czech_new1$zpvec),]
cor(cz$ypvec,cz$zpvec)
detach(czech_new)