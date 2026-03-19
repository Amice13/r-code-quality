##############################################################################
#
# Purpose:
# Use pilot project data to calculate power of a full study through simulation
#
# Parts:
# (0) - Setup
# (1) - Get the pilot data and clean it
# (2) - Run the model on the pilot data and extract effects
# (3) - Set up and run the simulation
# ====> Set variables at the arrows <====
#
##############################################################################
rm(list=ls())
set.seed(424242) 

library(readr)
library(ggplot2)

# (1) - Get the pilot data and clean it
#source('~/Research/tor_wikipedia_edits/handcoded_edits/inter_coder_reliability_ns0.R')
#source ('/data/users/mgaughan/kkex_data_110823_3')

data1 <- read_csv('mmt_data_final.csv',show_col_types = FALSE)
data2 <- read_csv('inst_all_packages_full_results.csv')
#d$nd <- to_logical(d$not.damaging, custom_true=c("Y")) 
#levels(d$source) <- c("IP-based Editors", "New Editors", "Registered Editors", "Tor-based Editors")
python_labeled <- as.numeric(data2$up.fac.mean[match(paste('python',tolower(data1$pkg), sep = "-"), data2$pkg)])
same_labeled <- as.numeric(data2$up.fac.mean[match(tolower(data1$pkg), data2$pkg)])
data1$up.fac.mean <- pmin(python_labeled, same_labeled, na.rm=TRUE)
data1$old_milestones <- data1$milestones
data1$new_milestones <- as.numeric(data1$milestones > 0) + 1
# (2) - Run the model on the pilot data
data1$formal.score <- data1$mmt / (data1$old_milestones/data1$age)
table(data1$formal.score)
hist(data1$old_mmt, prob=TRUE) #inequality of participation
hist(data1$formal.score)
hist(data1$age/365)
data1$new_mmt <- data1$mmt - 1
hist(data1$new_mmt, prob=TRUE)

data3 <- subset(data1, data1$old_milestones > 0 )
data3$formal.score <- data3$mmt / (data3$old_milestones/data3$age)

data1$new.age <- as.numeric(cut(data1$age/365, breaks=c(0,9,12,15,17), labels=c(1,2,3,4)))
data1$new.formal.score <- data1$mmt / (data1$new_milestones/data1$new.age)
hist(as.numeric(data1$new.age))
table(data1$new.age)
hist(data1$new.formal.score)
data5 <- subset(data1, is.finite(data1$formal.score))
mmtmodel1 <- lm(up.fac.mean ~ mmt + as.factor(new.age), data=data1)
summary(mmtmodel1)
agemodel1 <- lm(up.fac.mean ~ new.age, data=data1)
summary(agemodel1)
msmodel1 <- lm(up.fac.mean ~ old_milestones + as.factor(new.age), data=data1)
summary(msmodel1)
msmodel2 <- lm(up.fac.mean ~ new_milestones, data=data1)
summary(msmodel2)
fsmodel1 <- lm(up.fac.mean ~ formal.score, data=data5)
summary(fsmodel1)
t.test(data3$formal.score)
fsmodel2 <- lm(up.fac.mean ~ new.formal.score, data=data1)
summary(fsmodel2)
hist(data1$formal.score)
cor.test(data1$formal.score, data1$up.fac.mean)
cor.test(data1$mmt, data1$up.fac.mean)
cor.test(data1$milestones, data1$up.fac.mean)
cor.test(data1$age, data1$up.fac.mean)



g <- ggplot(data1, aes(x=mmt, y=up.fac.mean)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  xlab("MMT") +
  ylab("Underproduction Factor") +
  theme_bw() 
g
g

data2 <- subset(data1, (data1$age / 365) < 14 )
hist(floor(data2$age))
g <- ggplot(data2, aes(x=mmt, y=up.fac.mean)) +
  geom_point() +
  geom_smooth() +
  xlab("MMT") +
  ylab("Underproduction Factor") +
  theme_bw()
g

data2$yearsOld <- floor(data2$age / 365)

kmodel2 <- lm(up.fac.mean ~ mmt + milestones + age, data=data1)
kmodel5 <- lm(up.fac.mean ~ mmt + milestones, data=data1)
kmodel4 <- lm(up.fac.mean ~ mmt + age, data=data1)
kmodel3 <- lm(up.fac.mean ~ formal.score, data=data1)
summary(kmodel2)
summary(kmodel3)
summary(kmodel4)
summary(kmodel5)

#pilotM <- glm(up.fac.mean ~ ((mmt) / (milestones/age)),   # give the anticipated regression a try
#                  family=gaussian(link='identity'), data=data1)
summary(pilotM) #we expect effect sizes on this order

pilot.b0 <- coef(summary(kmodel2))[1,1] 
pilot.b1 <- coef(summary(kmodel2))[2,1]
pilot.b2 <- coef(summary(kmodel2))[3,1]
pilot.b3 <- coef(summary(kmodel2))[4,1] 


summary(pilot.b3)

qqline(data1$up.fac.mean)

sd(data1$up.fac.mean)
# (3) - Set up and run the simulation
qqline(data1$mmt)

source('powerAnalysis.R') #my little "lib"

#====>
nSims <- 5000 #how many simulations to run
n <- 100 #a guess for necessary sample size (per group)
#makeData(10) #DEBUGGING CODE -- you can uncomment this if you want to see it work
#<====

texreg(list(fsmodel2, mmtmodel1, msmodel1), stars=NULL, digits=2,
       custom.model.names=c( 'M1: augm. formality','M2: MMT', 'M3: milestones'  ), 
       custom.coef.names=c('(Intercept)', 'Augmented formality', 'MMT', 'Age-2', 'Age-3', 'Age-4', 'Milestones'), 
       use.packages=FALSE, table=FALSE, ci.force = TRUE)

coef(fsmodel1)
#print("Levels are:")
#print(levels(d$source))
powerCheck(n, nSims)
#powerCheck2(n, nSims) like doesn't really work

#Sample values
powerCheck(300, 1000) 
powerCheck(275, 1000) 
powerCheck(7000, 1000)

powerCheck2(50, 1000) 
powerCheck2(75, 1000) 
powerCheck2(900, 1000)
