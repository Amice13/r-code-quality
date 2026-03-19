# Read relevant packages
library(mokken)
library(readr)

# Read data file
slovakia_new <- read_csv("~<PATH>Slovakia_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 27 in Appendix)
slovakia1 <- data.frame(na.omit(slovakia_new[,c("q13","q14","q15","q18")]))
coefH(slovakia1)

slovakia1 <- data.frame(na.omit(slovakia_new[,c("q26","q27","q28")]))
coefH(slovakia1)

# Use subset of unique observations to generate "Crit" values (see Appendix)
slovakia_new1 <- unique(slovakia_new)

slovakia1 <- data.frame(na.omit(slovakia_new1[,c("q13","q14","q15","q18")]))
summary(check.monotonicity(slovakia1),minsize=25)

slovakia1 <- data.frame(na.omit(slovakia_new1[,c("q26","q27","q28")]))
summary(check.monotonicity(slovakia1),minsize=25)


# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
slovakia1 <- data.frame(na.omit(slovakia_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(slovakia1)

slovakia1 <- data.frame(na.omit(slovakia_new[,c("q13","q14","q15","q16_rev","q17","q18")]))
coefH(slovakia1)

slovakia1 <- data.frame(na.omit(slovakia_new[,c("q22","q23","q24","q25_rev","q26_rev","q27_rev","q28_rev")]))
coefH(slovakia1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(slovakia_new)
xpvec <- (q13 + q14 + q15 + q16_rev + q17 + q18)/24; ypvec <- (q22 + q23 + q24 + q25_rev + q26_rev + q27_rev + q28_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
slovakia_new1 <- cbind(slovakia_new,xpvec,ypvec,zpvec)
ska <- slovakia_new1[!is.na(slovakia_new1$xpvec)&!is.na(slovakia_new1$ypvec),]
cor(ska$xpvec,ska$ypvec)
ska <- slovakia_new1[!is.na(slovakia_new1$xpvec)&!is.na(slovakia_new1$zpvec),]
cor(ska$xpvec,ska$zpvec)
ska <- slovakia_new1[!is.na(slovakia_new1$ypvec)&!is.na(slovakia_new1$zpvec),]
cor(ska$ypvec,ska$zpvec)
detach(slovakia_new)
