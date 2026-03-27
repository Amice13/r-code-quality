library(foreign)
library(car)
library(multiwayvcov)
library(lmtest)
library(sandwich)

setwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')

t1 <- read.csv('jop_tab1.csv')

#subset for incumbent-contested races
#cluster.vcov does not work if model has fewer obs than the datset
t1a <- subset(t1, inc_t != 0)

#subset for open seat races
t1b <- subset(t1, inc_t == 0)

#Table1, Column1
m1 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1)
coeftest(m1, vcov = cluster.vcov(m1, t1$cdID) )

#Table1, Column2
m2 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1a)
coeftest(m2, cluster.vcov(m2, t1a$cdID) )

#Table1, Column3
m3 <- lm(dv ~ dvp + demqa + dpres + south + as.factor(year), t1b)
coeftest(m3, vcov = cluster.vcov(m3, t1b$cdID) )



