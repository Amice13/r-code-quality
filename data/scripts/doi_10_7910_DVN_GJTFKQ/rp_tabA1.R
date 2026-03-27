
library(foreign)
library(MASS)
library(lmtest)
library(car)
library(survey)



d <- read.csv('rp_data_rr.csv')
s <- svydesign(id = ~0, data = d, weight = ~weight)
#d <- subset(d, chamber_know != 'NA')

##############
#pooled model#
##############

#OLS
m1 <- lm(veto_app ~ pres_cong_diff  + dem + rep + ideol + col_grad + news_int, weights = weight, d)
o1 <- coeftest(m1, vcov = hccm(m1))

#ordered logit
m1_ord <- ologit.reg(veto_app ~ pres_cong_diff    + dem + rep + ideol + col_grad + news_int, d, weights = weight, robust = TRUE)

###########################
#do not know party control#
###########################

#ols
m2 <- lm(veto_app ~ pres_cong_diff  + dem + rep  + ideol + col_grad + news_int, weights = weight, subset(d, chamber_know < 2))
o2 <- coeftest(m2, vcov = hccm(m2))

#ordered logit
m2_ord <- ologit.reg(veto_app ~ pres_cong_diff    + dem + rep + ideol + col_grad + news_int, subset(d, chamber_know < 2), weights = weight, robust = TRUE)

####################
#know party control#
####################

#ols
m3 <- lm(veto_app ~ pres_cong_diff  + dem + rep + ideol + col_grad + news_int , weights = weight, subset(d, chamber_know == 2))
o3 <- coeftest(m3, vcov = hccm(m3))

#ordered logit
m3_ord <- ologit.reg(veto_app ~ pres_cong_diff    + dem + rep + ideol + col_grad + news_int, subset(d, chamber_know == 2), weights = weight, robust = TRUE)

