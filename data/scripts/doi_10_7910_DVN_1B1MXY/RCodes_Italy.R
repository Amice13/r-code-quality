# Read relevant packages
library(mokken)
library(readr)

# Read data file
italy_new <- read_csv("~<PATH>Italy_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 23 in Appendix)
italy1 <- data.frame(na.omit(italy_new[,c("q1","q3","q4_rev","q6","q19")]))
summary(check.monotonicity(italy1,minsize=50))
coefH(italy1)

italy1 <- data.frame(na.omit(italy_new[,c("q8","q9","q12")]))
summary(check.monotonicity(italy1,minsize=50))
coefH(italy1)

##########
# Scales that emerge from 23 items (including AD1 and AD2, see Table 24 in Appendix))
italy1 <- data.frame(na.omit(italy_new[,c("q1","q3","q6","q19","q23_rev","q26")]))
summary(check.monotonicity(italy1,minsize=50))
coefH(italy1)

italy1 <- data.frame(na.omit(italy_new[,c("q8","q9","q12")]))
summary(check.monotonicity(italy1,minsize=50))
coefH(italy1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
italy1 <- data.frame(na.omit(italy_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(italy1)

italy1 <- data.frame(na.omit(italy_new[,c("q8","q9","q10","q11_rev","q12","q13")]))
coefH(italy1)

italy1 <- data.frame(na.omit(italy_new[,c("q19","q20","q21","q22_rev","q23_rev","q24_rev","q25_rev")]))
coefH(italy1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(italy_new)
xpvec <- (q8 + q9 + q10 + q11_rev + q12 + q13)/24; ypvec <- (q19 + q20 + q21 + q22_rev + q23_rev + q24_rev + q25_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
italy_new1 <- cbind(italy_new,xpvec,ypvec,zpvec)
it <- italy_new1[!is.na(italy_new1$xpvec)&!is.na(italy_new1$ypvec),]
cor(it$xpvec,it$ypvec)
it <- italy_new1[!is.na(italy_new1$xpvec)&!is.na(italy_new1$zpvec),]
cor(it$xpvec,it$zpvec)
it <- italy_new1[!is.na(italy_new1$ypvec)&!is.na(italy_new1$zpvec),]
cor(it$ypvec,it$zpvec)
detach(italy_new)
