# Read relevant packages
library(mokken)
library(readr)
slovenia_new <- read_csv("~<PATH>Slovenia_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 28 in Appendix)
slovenia1 <- data.frame(na.omit(slovenia_new[,c("q1_rev","q6_rev","q9","q10","q11","q12_rev","q13","q14")]))
coefH(slovenia1)

slovenia1 <- data.frame(na.omit(slovenia_new[,c("q23_rev","q24_rev","q25_rev")]))
coefH(slovenia1)

# Use subset of unique observations to generate "Crit" values (see Appendix)
slovenia_new1 <- unique(slovenia_new)

slovenia1 <- data.frame(na.omit(slovenia_new1[,c("q1_rev","q6_rev","q9","q10","q11","q12_rev","q13","q14")]))
summary(check.monotonicity(slovenia1),minsize=25)

slovenia1 <- data.frame(na.omit(slovenia_new1[,c("q23_rev","q24_rev","q25_rev")]))
summary(check.monotonicity(slovenia1),minsize=25)


##########
# Scales that emerge from 22 items (including AD1, see Table 29 in Appendix))
slovenia1 <- data.frame(na.omit(slovenia_new[,c("q1_rev","q6_rev","q9","q10","q11","q12_rev","q13","q14")]))
coefH(slovenia1)

slovenia1 <- data.frame(na.omit(slovenia_new[,c("q19","q23_rev","q24_rev","q25_rev","q26")]))
coefH(slovenia1)

# Use subset of unique observations to generate "Crit" values (see Appendix)
slovenia1 <- data.frame(na.omit(slovenia_new1[,c("q1_rev","q6_rev","q9","q10","q11","q12_rev","q13","q14")]))
summary(check.monotonicity(slovenia1),minsize=25)

slovenia1 <- data.frame(na.omit(slovenia_new1[,c("q19","q23_rev","q24_rev","q25_rev","q26")]))
summary(check.monotonicity(slovenia1),minsize=25)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
slovenia1 <- data.frame(na.omit(slovenia_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(slovenia1)

slovenia1 <- data.frame(na.omit(slovenia_new[,c("q9","q10","q11","q12_rev","q13","q14")]))
coefH(slovenia1)

slovenia1 <- data.frame(na.omit(slovenia_new[,c("q19","q20","q21","q22_rev","q23_rev","q24_rev","q25_rev")]))
coefH(slovenia1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(slovenia_new)
xpvec <- (q9 + q10 + q11 + q12_rev + q13 + q14)/24; ypvec <- (q19 + q20 + q21 + q22_rev + q23_rev + q24_rev + q25_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
slovenia_new1 <- cbind(slovenia_new,xpvec,ypvec,zpvec)
slov <- slovenia_new1[!is.na(slovenia_new1$xpvec)&!is.na(slovenia_new1$ypvec),]
cor(slov$xpvec,slov$ypvec)
slov <- slovenia_new1[!is.na(slovenia_new1$xpvec)&!is.na(slovenia_new1$zpvec),]
cor(slov$xpvec,slov$zpvec)
slov <- slovenia_new1[!is.na(slovenia_new1$ypvec)&!is.na(slovenia_new1$zpvec),]
cor(slov$ypvec,slov$zpvec)
detach(slovenia_new)
