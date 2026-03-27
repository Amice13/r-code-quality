# Read relevant packages
library(mokken)
library(readr)

# Read data file
austria_new <- read_csv("~<PATH>Austria_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 3 in Appendix)
austria1 <- data.frame(na.omit(austria_new[,c("q12","q14","q16")]))
summary(check.monotonicity(austria1,minsize=50))
coefH(austria1)

austria1 <- data.frame(na.omit(austria_new[,c("q1","q2","q3","q4_rev","q5_rev","q6","q7_rev","q21")]))
summary(check.monotonicity(austria1,minsize=50))
coefH(austria1)

##########
# Scales that emerge from 22 items (including AD1, see Table 4 in Appendix))
austria1 <- data.frame(na.omit(austria_new[,c("q12","q14","q16")]))
summary(check.monotonicity(austria1,minsize=50))
coefH(austria1)

austria1 <- data.frame(na.omit(austria_new[,c("q1","q2","q3","q4_rev","q5_rev","q6","q7_rev","q21","q29")]))
summary(check.monotonicity(austria1,minsize=50))
coefH(austria1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
austria1 <- data.frame(na.omit(austria_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(austria1)

austria1 <- data.frame(na.omit(austria_new[,c("q12","q13","q14","q15_rev","q16","q17")]))
coefH(austria1)

austria1 <- data.frame(na.omit(austria_new[,c("q21","q22","q23","q24_rev","q25_rev","q26_rev","q27_rev")]))
coefH(austria1)


# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(austria_new)
xpvec <- (q12 + q13 + q14 + q15_rev + q16 + q17)/24; ypvec <- (q21 + q22 + q23 + q24_rev + q25_rev + q26_rev + q27_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
austria_new1 <- cbind(austria_new,xpvec,ypvec,zpvec)
at <- austria_new1[!is.na(austria_new1$xpvec)&!is.na(austria_new1$ypvec),]
cor(at$xpvec,at$ypvec)
at <- austria_new1[!is.na(austria_new1$xpvec)&!is.na(austria_new1$zpvec),]
cor(at$xpvec,at$zpvec)
at <- austria_new1[!is.na(austria_new1$ypvec)&!is.na(austria_new1$zpvec),]
cor(at$ypvec,at$zpvec)
detach(austria_new)