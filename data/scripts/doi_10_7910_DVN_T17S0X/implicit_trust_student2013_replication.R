# 2013 student sample replication file

# load libraries
library(ggplot2)
library(MASS)

# load student data, fyi: change file location 
datast2 <-read.csv("/student2013data.csv")

# change all variables to numeric
sapply(datast2, as.numeric)

# gender table
table(datast2$gender)

# party id table
table(datast2$partyid)

# ideology
table(datast2$ideology)

# trust index
meantrustindex <- mean(datast2$trustindex, na.rm=TRUE)

# plot density trust index
den <- density(datast2$trustindex, na.rm=TRUE)
plot(den, main=NA, xlab=NA)
polygon(den, col="gray", border="black") 
abline(v = meantrustindex, lty=2, lwd=1.5)
text(38, 0.020, "<- Mean", cex=0.75)
title(main = "A. Density of ANES Trust Index", adj=0)

# mean student trust iat
meantrustiat <- mean(datast2$trustiat, na.rm=TRUE)

# plot density trust iat
den1 <- density(datast2$trustiat, na.rm=TRUE)
plot(den1, main=NA, xlab=NA) 
polygon(den1, col="gray", border="black") 
abline(v = meantrustiat, lty=2, lwd=1.5)
text(0.5,0.5, "<- Mean", cex=0.75)
title(main="B. Density of Trust ST-IAT D-Scores", adj=0)

# plot of trust index and trust iat
scat <- ggplot(datast2, aes(x=datast2$trustindex, y=datast2$trustiat)) +  geom_jitter(position = position_jitter(width = .5))   + geom_smooth(method=loess, span=.5, level=.95) + theme_bw() + labs(x = "Explicit Trust", y = "Implicit Trust D-Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())  + geom_rug(col=rgb(.7,0,.7,alpha=.15), sides="l") + ylim(-1.7, 1.7) + theme(plot.margin = unit(c(2,2,2,2), "mm"))
scat

# subset data into high/low implicit/explicit trust
datast2a <- subset(datast2, datast2$trustindex>49 & datast2$trustiat>0, na.rm=TRUE)
datast2b <- subset(datast2, datast2$trustindex<50 & datast2$trustiat<0, na.rm=TRUE)
datast2c <- subset(datast2, datast2$trustindex>49 & datast2$trustiat<0, na.rm=TRUE)
datast2d <- subset(datast2, datast2$trustindex<50 & datast2$trustiat>0, na.rm=TRUE)

# number of people in each category
length(datast2a$gender)
length(datast2b$gender)
length(datast2c$gender)
length(datast2d$gender)

# total sample
9 + 29 + 3 + 63

# percentage in each category
9/104    # 9% high both
29/104   # 28% low both
3/104    # 3% high explicit, low implicit
63/104   # 61% low explicit, high implicit

# recode party id with dem=1, ind=2, rep=3
datast2$pid3[datast2$partyid==1] <- 3
datast2$pid3[datast2$partyid==2] <- 1
datast2$pid3[datast2$partyid==3] <- 2

# change pid3 to numeric
datast2$pid3 <-as.numeric(datast2$pid3)

# combine ethnicity in one column
datast2$ethnic[datast2$black==1] <- 2
datast2$ethnic[datast2$asian==1] <- 4
datast2$ethnic[datast2$nativeamerican==1] <- 4
datast2$ethnic[datast2$hispanic==1] <- 3
datast2$ethnic[datast2$white==1] <- 1
datast2$ethnic[datast2$otherace==1] <- 4

# change ethnic to factor
datast2$ethnic <-as.factor(datast2$ethnic)

# subset data with ideology other/don't knows taken out
datast2new <-subset(datast2, ideology<=7)

# change govt spending to factor
datast2new$govtspend <-as.factor(datast2new$govtspend)

# govt spending probit model 
modelspend <- polr(govtspend ~ trustiat + trustindex + pid3 + ideology + ethnic + gender, data=datast2new, method="probit", Hess=TRUE)
summary(modelspend)

# obtain p-values in combined table
s <- coef(summary(modelspend))
p <- pnorm(abs(s[, "t value"]), lower.tail = FALSE) * 2
stable <- cbind(s, "p value" = p)
stable

# recode welfare spending, 1=increase, 2=decrease, 3=same 
datast2new$welfspendb[datast2new$welfspend=="1"] <- "1"
datast2new$welfspendb[datast2new$welfspend=="2"] <- "0"
datast2new$welfspendb[datast2new$welfspend=="3"] <- "1"

# change welfspendb to factor
datast2new$welfspendb <-as.factor(datast2new$welfspendb)

# probit model welfare spending
modelwelf <-glm(welfspendb ~ trustiat + trustindex + pid3 + ideology + ethnic + gender, family=binomial(link="probit"), data=datast2new)
summary(modelwelf)

# recode security spending, 1=increase, 2=decrease, 3=same 
datast2new$secspendb[datast2new$secspend=="1"] <- "1"
datast2new$secspendb[datast2new$secspend=="2"] <- "0"
datast2new$secspendb[datast2new$secspend=="3"] <- "1"

# change secspendb to factor
datast2new$secspendb <-as.factor(datast2new$secspendb)

# probit model security spending
modelsec <-glm(secspendb ~ trustiat + trustindex + pid3 + ideology + ethnic + gender, family=binomial(link="probit"), data=datast2new)
summary(modelsec)
