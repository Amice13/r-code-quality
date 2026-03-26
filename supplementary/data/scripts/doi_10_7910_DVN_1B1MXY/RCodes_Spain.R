# Read relevant packages
library(mokken)
library(readr)

# Read data file
spain_new <- read_csv("~<PATH>Spain_myBal.csv")

# Scales that emerge from the 21 common items  (see Table 30 in Appendix)
spain1 <- data.frame(na.omit(spain_new[,c("q9","q10","q13","q14","q17","q18","q19","q21_rev","q22_rev","q23_rev")]))
summary(check.monotonicity(spain1,minsize=100))
coefH(spain1)

spain1 <- data.frame(na.omit(spain_new[,c("q1","q2","q6")]))
summary(check.monotonicity(spain1,minsize=100))
coefH(spain1)

##########
# Scales that emerge from 23 items (including AD1 and AD2, see Table 31 in Appendix))
spain1 <- data.frame(na.omit(spain_new[,c("q9","q10","q13","q14","q17","q18","q19","q21_rev","q22_rev","q23_rev","q24")]))
summary(check.monotonicity(spain1,minsize=100))
coefH(spain1)

spain1 <- data.frame(na.omit(spain_new[,c("q1","q2","q6")]))
summary(check.monotonicity(spain1,minsize=100))
coefH(spain1)

# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
spain1 <- data.frame(na.omit(spain_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(spain1)

spain1 <- data.frame(na.omit(spain_new[,c("q9","q10","q11","q12_rev","q13","q14")]))
coefH(spain1)

spain1 <- data.frame(na.omit(spain_new[,c("q17","q18","q19","q20_rev","q21_rev","q22_rev","q23_rev")]))
coefH(spain1)

# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (2nd variant)
attach(spain_new)
xpvec <- (q9 + q10 + q11 + q12_rev + q13 + q14)/24; ypvec <- (q17 + q18 + q19 + q20_rev + q21_rev + q22_rev + q23_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
spain_new1 <- cbind(spain_new,xpvec,ypvec,zpvec)
esp <- spain_new1[!is.na(spain_new1$xpvec)&!is.na(spain_new1$ypvec),]
cor(esp$xpvec,esp$ypvec)
esp <- spain_new1[!is.na(spain_new1$xpvec)&!is.na(spain_new1$zpvec),]
cor(esp$xpvec,esp$zpvec)
esp <- spain_new1[!is.na(spain_new1$ypvec)&!is.na(spain_new1$zpvec),]
cor(esp$ypvec,esp$zpvec)
detach(spain_new)
