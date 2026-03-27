# Read relevant packages
library(mokken)
library(readr)

# Read data file
greece_new <- read_csv("~<PATH>Greece_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 19 in Appendix)
greece1 <- data.frame(na.omit(greece_new[,c("q1_rev","q4","q6_rev","q7","q11","q16","q17","q23")]))
summary(check.monotonicity(greece1,minsize=100))
coefH(greece1)

##########
# Scales that emerge from 22 items (including AD1, see Table 20 in Appendix))
greece1 <- data.frame(na.omit(greece_new[,c("q1_rev","q4","q6_rev","q7","q11","q16","q17","q23")]))
summary(check.monotonicity(greece1,minsize=100))
coefH(greece1)

greece1 <- data.frame(na.omit(greece_new[,c("q3","q25_rev","q29")]))
summary(check.monotonicity(greece1,minsize=100))
coefH(greece1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
greece1 <- data.frame(na.omit(greece_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(greece1)

greece1 <- data.frame(na.omit(greece_new[,c("q11","q12","q13","q14_rev","q15","q16")]))
coefH(greece1)

greece1 <- data.frame(na.omit(greece_new[,c("q21","q22","q23","q24_rev","q25_rev","q26_rev","q27_rev")]))
coefH(greece1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(greece_new)
xpvec <- (q11 + q12 + q13 + q14_rev + q15 + q16)/24; ypvec <- (q21 + q22 + q23 + q24_rev + q25_rev + q26_rev + q27_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
greece_new1 <- cbind(greece_new,xpvec,ypvec,zpvec)
gr <- greece_new1[!is.na(greece_new1$xpvec)&!is.na(greece_new1$ypvec),]
cor(gr$xpvec,gr$ypvec)
gr <- greece_new1[!is.na(greece_new1$xpvec)&!is.na(greece_new1$zpvec),]
cor(gr$xpvec,gr$zpvec)
gr <- greece_new1[!is.na(greece_new1$ypvec)&!is.na(greece_new1$zpvec),]
cor(gr$ypvec,gr$zpvec)
detach(greece_new)