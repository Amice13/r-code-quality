# Read relevant packages
library(mokken)
library(readr)

# Read data file
poland_new <- read_csv("~<PATH>Poland_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 25 in Appendix)
poland1 <- data.frame(na.omit(poland_new[,c("q1_rev","q4","q6_rev","q21","q22")]))
summary(check.monotonicity(poland1,minsize=100))
coefH(poland1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
poland1 <- data.frame(na.omit(poland_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(poland1)

poland1 <- data.frame(na.omit(poland_new[,c("q9","q10","q11","q12_rev","q13","q14")]))
coefH(poland1)

poland1 <- data.frame(na.omit(poland_new[,c("q17","q18","q19","q20_rev","q21_rev","q22_rev","q23_rev")]))
coefH(poland1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(poland_new)
xpvec <- (q9 + q10 + q11 + q12_rev + q13 + q14)/24; ypvec <- (q17 + q18 + q19 + q20_rev + q21_rev + q22_rev + q23_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
poland_new1 <- cbind(poland_new,xpvec,ypvec,zpvec)
pol <- poland_new1[!is.na(poland_new1$xpvec)&!is.na(poland_new1$ypvec),]
cor(pol$xpvec,pol$ypvec)
pol <- poland_new1[!is.na(poland_new1$xpvec)&!is.na(poland_new1$zpvec),]
cor(pol$xpvec,pol$zpvec)
pol <- poland_new1[!is.na(poland_new1$ypvec)&!is.na(poland_new1$zpvec),]
cor(pol$ypvec,pol$zpvec)
detach(poland_new)