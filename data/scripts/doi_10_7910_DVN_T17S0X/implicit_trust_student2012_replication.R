# 2012 student sample replication file

# load library
library(ggplot2)

# load student data, fyi: change file location
datast <-read.csv("/student2012data.csv")

# change all variables to numeric
sapply(datast, as.numeric)

# gender table
table(datast$gender)

# party id table
table(datast$partyid)

# ideology
table(datast$ideology)

# trust index
meantrustindex <- mean(datast$trustindex)

# plot density trust index
den <- density(datast$trustindex)
plot(den, main=NA, xlab=NA)
polygon(den, col="gray", border="black") 
abline(v = meantrustindex, lty=2, lwd=1.5)
text(40, 0.018, "<- Mean", cex=0.75)
title(main = "A. Density of ANES Trust Index", adj=0)

# mean student good/bad iat
mean(datast$goodiat, na.rm=TRUE)

# t-test of student good/bad iat
t.test(datast$goodiat, mu=0)

# standard deviation student good/bad iat
sd(datast$goodiat, na.rm=TRUE)

# mean student trust iat
meantrustiat <- mean(datast$trustiat, na.rm=TRUE)

# plot density trust iat
den1 <- density(datast$trustiat, na.rm=TRUE)
plot(den1, main=NA, xlab=NA) 
polygon(den1, col="gray", border="black") 
abline(v = meantrustiat, lty=2, lwd=1.5)
text(0.3,0.5, "<- Mean", cex=0.75)
title(main="B. Density of Trust ST-IAT D-Scores", adj=0)

# plot of trust index and trust iat
scat <- ggplot(datast, aes(x=datast$trustindex, y=datast$trustiat)) +  geom_jitter(position = position_jitter(width = .5))   + geom_smooth(method=loess, span=.5, level=.95) + theme_bw() + labs(x = "Explicit Trust", y = "Implicit Trust D-Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())  + geom_rug(col=rgb(.7,0,.7,alpha=.15), sides="l") + ylim(-1.7, 1.7) + theme(plot.margin = unit(c(2,2,2,2), "mm"))
scat

# t-test of student trust iat
t.test(datast$trustiat, mu=0)

# standard deviation student trust iat
sd(datast$trustiat, na.rm=TRUE)

# correlation between good/bad iat and trust iat
cor.test(datast$trustiat, datast$goodiat)

# subset data into high/low implicit/explicit trust
datasta <- subset(datast, datast$trustindex>49 & datast$trustiat>0)
datastb <- subset(datast, datast$trustindex<50 & datast$trustiat<0)
datastc <- subset(datast, datast$trustindex>49 & datast$trustiat<0)
datastd <- subset(datast, datast$trustindex<50 & datast$trustiat>0)

# number of people in each category
length(datasta$gender)
length(datastb$gender)
length(datastc$gender)
length(datastd$gender)

# total sample
7 + 22 + 3 + 32

# percentage in each category
7/64    # 11% high both
22/64   # 34% low both
3/64    # 5% high explicit, low implicit
32/64   # 50% low explicit, high implicit










