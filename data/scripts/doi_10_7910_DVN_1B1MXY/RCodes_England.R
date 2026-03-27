# Read relevant packages
library(mokken)
library(readr)

# Read data file
england_new <- read_csv("~<PATH>England_myBal.csv")

# Scales that emerge from the 21 common items

# 1d solution  (see Table 10 in Appendix)
england1 <- data.frame(na.omit(england_new[,c("q1","q2","q3","q5_rev","q6","q12","q13","q14_rev","q15","q16","q21","q24_rev","q25_rev")]))
summary(check.monotonicity(england1,minsize=100))
coefH(england1)

# 2d solution  (see Table 12 in Appendix)
england1 <- data.frame(na.omit(england_new[,c("q1","q2","q3","q5_rev","q6","q21","q25_rev")]))
summary(check.monotonicity(england1,minsize=100))
coefH(england1)

england1 <- data.frame(na.omit(england_new[,c("q12","q13","q15")]))
summary(check.monotonicity(england1,minsize=100))
coefH(england1)



##########
# Scales that emerge from 23 items (including AD1)

# 1d solution (see Table 11 in the Appendix)
england1 <- data.frame(na.omit(england_new[,c("q1","q2","q3","q5_rev","q6","q12","q13","q14_rev","q15","q16","q21","q24_rev","q25_rev","q28")]))
summary(check.monotonicity(england1,minsize=100))
coefH(england1)

# 2d solution (see Table 13 in the Appendix)
england1 <- data.frame(na.omit(england_new[,c("q1","q2","q3","q5_rev","q6","q21","q25_rev","q28")]))
summary(check.monotonicity(england1,minsize=100))
coefH(england1)

england1 <- data.frame(na.omit(england_new[,c("q12","q13","q15")]))
summary(check.monotonicity(england1,minsize=100))
coefH(england1)


# Dimension analysis on 19 pre-defined items (see Table 2 in Appendix)
england1 <- data.frame(na.omit(england_new[,c("q1_rev","q2_rev","q3_rev","q4","q5","q6_rev")]))
coefH(england1)

england1 <- data.frame(na.omit(england_new[,c("q11","q12","q13","q14_rev","q15","q16")]))
coefH(england1)

england1 <- data.frame(na.omit(england_new[,c("q21","q22","q23","q24_rev","q25_rev","q26_rev","q27_rev")]))
coefH(england1)



# See to what extent the predefined economic, cultural and EU dimensions correlate amongst users (see Table 3 in main manuscript)
attach(england_new)
xpvec <- (q11 + q12 + q13 + q14_rev + q15 + q16)/24; ypvec <- (q21 + q22 + q23 + q24_rev + q25_rev + q26_rev + q27_rev)/28; zpvec <- (q1_rev + q2_rev + q3_rev + q4 + q5 + q6_rev)/24
england_new1 <- cbind(england_new,xpvec,ypvec,zpvec)
eng <- england_new1[!is.na(england_new1$xpvec)&!is.na(england_new1$ypvec),]
cor(eng$xpvec,eng$ypvec)
eng <- england_new1[!is.na(england_new1$xpvec)&!is.na(england_new1$zpvec),]
cor(eng$xpvec,eng$zpvec)
eng <- england_new1[!is.na(england_new1$ypvec)&!is.na(england_new1$zpvec),]
cor(eng$ypvec,eng$zpvec)
detach(england_new)