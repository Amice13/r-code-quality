# Replication File for 
# Rebecca Weitz-Shapiro and Matthew S. Winters
# "Can Citizens Discern? Information Credibility, Political Sophistication, 
 # and the Punishment of Corruption in Brazil"
# Journal of Politics

# This file calculates the tests of statistical significance across different conditional average
 # treatment effect estimates.
# For the main analyses and definition of variables, see the Stata file
 # "WeitzShapiroWinters_JOP_Credibility_Replication.do"

library(foreign)

brazil <- read.dta("Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta")
brazil$vinheta <- as.factor(brazil$vinheta)

# Set Number of Simulations for CATEs Analysis
n.sims <- 1000

# Set Randomization Seed
set.seed(19790412)

###########
# TABLE 3 #
###########

# Estimated Average Treatment Effect for Credible Accusations versus Less Credible Accusations
ate.hat.cred.v.less <- mean(brazil$voteintent[brazil$cred_vs_less==1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0], na.rm=T)

#######################################################
# Does Main Treatment Effect Differ by Education?

# Estimated CATEs  
cate.hat.lowedu <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$educ<4], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$educ<4], na.rm=T)
cate.hat.highedu <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$educ==4], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$educ==4], na.rm=T)
diff.cates.edu <- cate.hat.highedu - cate.hat.lowedu

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$educ))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "educ")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where education question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$educ)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.educ.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==4]) - mean(y.i.1[t==0 & educ==4])
  cate.low <- mean(y.i.0[t==1 & educ < 4]) - mean(y.i.1[t==0 & educ < 4])
  diff.cates.educ.sims[i] <- cate.high - cate.low
}

 # Test
mean(abs(diff.cates.educ.sims) >= abs(diff.cates.edu))

detach(data.for.sims)

#######################################################
# Does Main Treatment Effect Differ by Knowledge?

# Estimated CATEs  
cate.hat.lowknow <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$polknow < 2], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$polknow < 2], na.rm=T)
cate.hat.highknow <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$polknow==2], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$polknow==2], na.rm=T)

diff.cates.know <- cate.hat.highknow - cate.hat.lowknow

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$polknow))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "polknow")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where polknow question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$polknow)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.know.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & polknow==2]) - mean(y.i.1[t==0 & polknow==2])
  cate.low <- mean(y.i.0[t==1 & polknow < 2]) - mean(y.i.1[t==0 & polknow < 2])
  diff.cates.know.sims[i] <- cate.high - cate.low
}

 # Test
mean(abs(diff.cates.know.sims) >= abs(diff.cates.know))

detach(data.for.sims)

#######################################################
# Does Main Treatment Effect Differ by Discussion?

# Estimated CATEs  
cate.hat.lowtalk <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$talkpol < 3], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$talkpol < 3], na.rm=T)
cate.hat.hightalk <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$talkpol==3], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$talkpol==3], na.rm=T)

diff.cates.talk <- cate.hat.hightalk - cate.hat.lowtalk

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$talkpol))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "talkpol")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where talkpolitics question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$talkpol)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.talk.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & talkpol==3]) - mean(y.i.1[t==0 & talkpol==3])
  cate.low <- mean(y.i.0[t==1 & talkpol < 3]) - mean(y.i.1[t==0 & talkpol < 3])
  diff.cates.talk.sims[i] <- cate.high - cate.low
}

 # Test
mean(abs(diff.cates.talk.sims) >= abs(diff.cates.talk))

detach(data.for.sims)



###################
# ONLINE APPENDIX #
###################

#################
# Balance Plots #
#################

# Function for Standard Errors
se <- function(x){
 sqrt(var(x, na.rm=T)/length(x[is.na(x)==F]))
}

par(mfrow=c(4,3))
par(mar=c(3,3,2,1)+0.1)
par(mgp=c(2,1,0))
par(cex.main=0.75)
par(cex.lab=0.75)
par(cex.axis=0.75)

 ### Male
var.temp <- brazil$male
means.temp <- tapply(var.temp, brazil$vinheta, mean)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Male")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Age
var.temp <- brazil$idade
means.temp <- tapply(var.temp, brazil$vinheta, mean)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Age")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Education 
var.temp <- as.numeric(brazil$inst)
means.temp <- tapply(var.temp, brazil$vinheta, mean)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Education (10-Category Scale)")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Social Class 
var.temp <- as.numeric(brazil$classif)
means.temp <- tapply(var.temp, brazil$vinheta, mean)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Social Class (8-Category Scale)")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Income
var.temp <- brazil$income
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Income (4-Category Scale)")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Catholic
var.temp <- brazil$catholic
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Catholic")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Talk about Politics
var.temp <- brazil$talkpol
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Talk about Politics (3-Category Scale)")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Read the News
var.temp <- brazil$news
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Frequency of News (3-Category Scale)")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### Political Knowledge
var.temp <- brazil$polknowledge
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="Political Knowledge (3-Category Scale)")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### PT Identifier
var.temp <- brazil$ptid
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="PT Identifier")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### PSDB Identifier
var.temp <- brazil$psdbid
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="PSDB Identifier")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

 ### PMDB Identifier
var.temp <- brazil$pmdbid
means.temp <- tapply(var.temp, brazil$vinheta, mean, na.rm=T)
se.temp <- tapply(var.temp, brazil$vinheta, se)
high.temp <- means.temp + 1.96*se.temp
low.temp <- means.temp - 1.96*se.temp

plot(1:7, means.temp, ylim=c(min(low.temp)*0.90, max(high.temp)*1.10), xlab="Treatment", ylab="Mean",
     main ="PMDB Identifier")
segments(1:7, low.temp, 1:7, high.temp)
abline(h=mean(var.temp, na.rm=T), lty="dashed")

############################################
# t-Tests for Comparisons Against the Mean #
############################################

p.vals.all <- matrix(NA, nrow=12, ncol=6, dimnames=list(c("Male", "Age", "Edu", "Class", "Inc", 
 "Cath", "Talk", "News", "Know", "PT", "PSDB", "PMDB"), 1:6))

for (j in 1:6){
  p.vals.all["Male",j] <- round(t.test(brazil$male[as.numeric(brazil$vinheta)==j],
                         brazil$male)$p.value, 2)
  p.vals.all["Age",j] <- round(t.test(brazil$idade[as.numeric(brazil$vinheta)==j],
                         brazil$idade)$p.value, 2)
  p.vals.all["Edu",j] <- round(t.test(as.numeric(brazil$inst[as.numeric(brazil$vinheta)==j]),
                         as.numeric(brazil$inst))$p.value, 2)
  p.vals.all["Class",j] <- round(t.test(as.numeric(brazil$classif[as.numeric(brazil$vinheta)==j]),
                         as.numeric(brazil$classif))$p.value, 2)
  p.vals.all["Inc",j] <- round(t.test(brazil$inc[as.numeric(brazil$vinheta)==j],
                         brazil$inc)$p.value, 2)
  p.vals.all["Cath",j] <- round(t.test(brazil$catholic[as.numeric(brazil$vinheta)==j],
                         brazil$catholic)$p.value, 2)
  p.vals.all["Talk",j] <- round(t.test(brazil$talkpol[as.numeric(brazil$vinheta)==j],
                         brazil$talkpol)$p.value, 2)
  p.vals.all["News",j] <- round(t.test(brazil$news[as.numeric(brazil$vinheta)==j],
                         brazil$news)$p.value, 2)
  p.vals.all["Know",j] <- round(t.test(brazil$polknowledge[as.numeric(brazil$vinheta)==j],
                         brazil$polknowledge)$p.value, 2)
  p.vals.all["PT",j] <- round(t.test(brazil$ptid[as.numeric(brazil$vinheta)==j],
                         brazil$ptid)$p.value, 2)
  p.vals.all["PSDB",j] <- round(t.test(brazil$psdbid[as.numeric(brazil$vinheta)==j],
                         brazil$psdbid)$p.value, 2)
  p.vals.all["PMDB",j] <- round(t.test(brazil$pmdbid[as.numeric(brazil$vinheta)==j],
                         brazil$pmdbid)$p.value, 2)
}

p.vals.male <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.male[i,j] <- round(t.test(brazil$male[as.numeric(brazil$vinheta)==i],
                         brazil$male[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.age <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.age[i,j] <- round(t.test(brazil$idade[as.numeric(brazil$vinheta)==i],
                         brazil$idade[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.edu <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.edu[i,j] <- round(t.test(as.numeric(brazil$inst[as.numeric(brazil$vinheta)==i]),
                         as.numeric(brazil$inst[as.numeric(brazil$vinheta)==(8-j)]))$p.value, 2) } }

p.vals.class <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.class[i,j] <- round(t.test(as.numeric(brazil$classif[as.numeric(brazil$vinheta)==i]),
                         as.numeric(brazil$classif[as.numeric(brazil$vinheta)==(8-j)]))$p.value, 2) } }

p.vals.inc <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.inc[i,j] <- round(t.test(brazil$inc[as.numeric(brazil$vinheta)==i],
                         brazil$inc[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.cath <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.cath[i,j] <- round(t.test(brazil$catholic[as.numeric(brazil$vinheta)==i],
                         brazil$catholic[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.talk <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.talk[i,j] <- round(t.test(brazil$talkpol[as.numeric(brazil$vinheta)==i],
                         brazil$talkpol[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.news <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.news[i,j] <- round(t.test(brazil$news[as.numeric(brazil$vinheta)==i],
                         brazil$news[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.know <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.know[i,j] <- round(t.test(brazil$polknowledge[as.numeric(brazil$vinheta)==i],
                         brazil$polknowledge[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.pt <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.pt[i,j] <- round(t.test(brazil$ptid[as.numeric(brazil$vinheta)==i],
                         brazil$ptid[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.psdb <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.psdb[i,j] <- round(t.test(brazil$psdbid[as.numeric(brazil$vinheta)==i],
                         brazil$psdbid[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }

p.vals.pmdb <- matrix(NA, nrow=6, ncol=6, dimnames=list(1:6, 7:2))
for (i in 1:6){  for (j in 1:(7-i)){
   p.vals.pmdb[i,j] <- round(t.test(brazil$pmdbid[as.numeric(brazil$vinheta)==i],
                         brazil$pmdbid[as.numeric(brazil$vinheta)==(8-j)])$p.value, 2) } }


#######################################################
# REPLICATION OF TABLE 3 WITH SPECIFIC VIGNETTES ONLY #
#######################################################

# Create Dataset for Use Only in This Section
data.specific <- subset(brazil, specific==1)

# Estimated Average Treatment Effect for Credible Accusations versus Less Credible Accusations
ate.hat.cred.v.less.spec <- mean(data.specific$voteintent[data.specific$cred_vs_less==1], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0], na.rm=T)

#######################################################
# Does Main Treatment Effect Differ by Education?

# Estimated CATEs  
cate.hat.lowedu <- mean(data.specific$voteintent[data.specific$cred_vs_less==1 & data.specific$educ<4], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0 & data.specific$educ<4], na.rm=T)
cate.hat.highedu <- mean(data.specific$voteintent[data.specific$cred_vs_less==1 & data.specific$educ==4], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0 & data.specific$educ==4], na.rm=T)

diff.cates.edu <- cate.hat.highedu - cate.hat.lowedu

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(data.specific$cred_vs_less==0, data.specific$voteintent, data.specific$voteintent - ate.hat.cred.v.less.spec)
y.i.1 <- ifelse(data.specific$cred_vs_less==1, data.specific$voteintent, data.specific$voteintent + ate.hat.cred.v.less.spec)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, data.specific$educ))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "educ")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where ptid question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$educ)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.educ.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==4]) - mean(y.i.1[t==0 & educ==4])
  cate.low <- mean(y.i.0[t==1 & educ < 4]) - mean(y.i.1[t==0 & educ < 4])
  diff.cates.educ.sims[i] <- cate.high - cate.low
}

 # Test
mean(abs(diff.cates.educ.sims) >= abs(diff.cates.edu))

detach(data.for.sims)

#######################################################
# Does Main Treatment Effect Differ by Knowledge?

# Estimated CATEs  
cate.hat.lowknow <- mean(data.specific$voteintent[data.specific$cred_vs_less==1 & data.specific$polknow < 2], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0 & data.specific$polknow < 2], na.rm=T)
cate.hat.highknow <- mean(data.specific$voteintent[data.specific$cred_vs_less==1 & data.specific$polknow==2], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0 & data.specific$polknow==2], na.rm=T)

diff.cates.know <- cate.hat.highknow - cate.hat.lowknow

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(data.specific$cred_vs_less==0, data.specific$voteintent, data.specific$voteintent - ate.hat.cred.v.less.spec)
y.i.1 <- ifelse(data.specific$cred_vs_less==1, data.specific$voteintent, data.specific$voteintent + ate.hat.cred.v.less.spec)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, data.specific$polknow))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "polknow")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where ptid question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$polknow)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.know.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & polknow==2]) - mean(y.i.1[t==0 & polknow==2])
  cate.low <- mean(y.i.0[t==1 & polknow < 2]) - mean(y.i.1[t==0 & polknow < 2])
  diff.cates.know.sims[i] <- cate.high - cate.low
}

 # Test
mean(abs(diff.cates.know.sims) >= abs(diff.cates.know))

detach(data.for.sims)

#######################################################
# Does Main Treatment Effect Differ by Discussion?

# Estimaed CATEs  
cate.hat.lowtalk <- mean(data.specific$voteintent[data.specific$cred_vs_less==1 & data.specific$talkpol < 3], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0 & data.specific$talkpol < 3], na.rm=T)
cate.hat.hightalk <- mean(data.specific$voteintent[data.specific$cred_vs_less==1 & data.specific$talkpol==3], na.rm=T) - 
  mean(data.specific$voteintent[data.specific$cred_vs_less==0 & data.specific$talkpol==3], na.rm=T)

diff.cates.talk <- cate.hat.hightalk - cate.hat.lowtalk

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(data.specific$cred_vs_less==0, data.specific$voteintent, data.specific$voteintent - ate.hat.cred.v.less.spec)
y.i.1 <- ifelse(data.specific$cred_vs_less==1, data.specific$voteintent, data.specific$voteintent + ate.hat.cred.v.less.spec)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, data.specific$talkpol))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "talkpol")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where ptid question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$talkpol)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.talk.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & talkpol==3]) - mean(y.i.1[t==0 & talkpol==3])
  cate.low <- mean(y.i.0[t==1 & talkpol < 3]) - mean(y.i.1[t==0 & talkpol < 3])
  diff.cates.talk.sims[i] <- cate.high - cate.low
}

 # Test
mean(abs(diff.cates.talk.sims) >= abs(diff.cates.talk))

detach(data.for.sims)


#####################################
# CATEs FOR PT PARTISANS AND OTHERS #
#####################################

# Estimaed CATEs  
cate.hat.pt <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$ptid==1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$ptid==1], na.rm=T)
cate.hat.nonpt <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$ptid==0], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$ptid==0], na.rm=T)

diff.cates.pt <- cate.hat.pt - cate.hat.nonpt

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$ptid))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "ptid")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where ptid question was NA -- does not apply
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$pt)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.pt.sims <- rep(NA, n.sims)

 # Run Loop
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.pt <- mean(y.i.0[t==1 & ptid==1]) - mean(y.i.1[t==0 & ptid==1])
  cate.nonpt <- mean(y.i.0[t==1 & ptid==0]) - mean(y.i.1[t==0 & ptid==0])
  diff.cates.pt.sims[i] <- cate.pt - cate.nonpt
}

 # Test
mean(abs(diff.cates.pt.sims) >= abs(diff.cates.pt))

detach(data.for.sims)


#######################################################
# CATEs VARYING BY RELATIONSHIP WITH REAL WORLD MAYOR #
#######################################################

# Three Groups: Partisans who Match with Mayor; Partisans who Do Not Match with Mayor; Non-Partisans

# Estimaed CATEs  
cate.hat.copartisan <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$partymatch==1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$partymatch==1], na.rm=T)
cate.hat.otherpartisan <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$diffparty==1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$diffparty==1], na.rm=T)
cate.hat.nonpartisan <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$noid==1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$noid==1], na.rm=T)

diff.cates.co.other <- cate.hat.copartisan - cate.hat.otherpartisan
diff.cates.co.non <- cate.hat.copartisan - cate.hat.nonpartisan
diff.cates.other.non <- cate.hat.otherpartisan - cate.hat.nonpartisan

# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$partymatch, brazil$diffparty, brazil$noid))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "partymatch", "diffparty", "noid")
 # Get rid of observations that were not involved in the credibility treatments
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
 # Get rid of observations where partyid question was NA
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$partymatch)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

 # Create Collection Vectors
diff.cates.co.other.sims <- rep(NA, n.sims)
diff.cates.co.non.sims <- rep(NA, n.sims)
diff.cates.other.non.sims <- rep(NA, n.sims)

 # Run Loops
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.co <- mean(y.i.0[t==1 & partymatch==1]) - mean(y.i.1[t==0 & partymatch==1])
  cate.other <- mean(y.i.0[t==1 & diffparty==1]) - mean(y.i.1[t==0 & diffparty==1])
  diff.cates.co.other.sims[i] <- cate.co - cate.other
}

for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.co <- mean(y.i.0[t==1 & partymatch==1]) - mean(y.i.1[t==0 & partymatch==1])
  cate.non <- mean(y.i.0[t==1 & noid==1]) - mean(y.i.1[t==0 & noid==1])
  diff.cates.co.non.sims[i] <- cate.co - cate.non
}

for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.other <- mean(y.i.0[t==1 & diffparty==1]) - mean(y.i.1[t==0 & diffparty==1])
  cate.non <- mean(y.i.0[t==1 & noid==1]) - mean(y.i.1[t==0 & noid==1])
  diff.cates.other.non.sims[i] <- cate.other - cate.non
}

 # Tests
mean(abs(diff.cates.co.other.sims) >= abs(diff.cates.co.other))
mean(abs(diff.cates.co.non.sims) >= abs(diff.cates.co.non))
mean(abs(diff.cates.other.non.sims) >= abs(diff.cates.other.non))

detach(data.for.sims)

#####################################
# CATEs FOR ALL VALUES OF EDUCATION #
#####################################

# Estimate CATEs for Different Levels of Education 
cate.edu0 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$edu == 0], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$edu == 0], na.rm=T)
cate.edu1 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$edu == 1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$edu == 1], na.rm=T)
cate.edu2 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$edu == 2], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$edu == 2], na.rm=T)
cate.edu3 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$edu == 3], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$edu == 3], na.rm=T)
cate.edu4 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$edu == 4], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$edu == 4], na.rm=T)

diff.cate.4.0 <- cate.edu4 - cate.edu0
diff.cate.3.0 <- cate.edu3 - cate.edu0
diff.cate.2.0 <- cate.edu2 - cate.edu0
diff.cate.1.0 <- cate.edu1 - cate.edu0

y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$educ))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "educ")
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$educ)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

# Educ==4 versus Educ==0 
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==4]) - mean(y.i.1[t==0 & educ==4])
  cate.low <- mean(y.i.0[t==1 & educ==0]) - mean(y.i.1[t==0 & educ==0])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.4.0))

# Repeat for Educ==3 versus Educ==0 
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==3]) - mean(y.i.1[t==0 & educ==3])
  cate.low <- mean(y.i.0[t==1 & educ==0]) - mean(y.i.1[t==0 & educ==0])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.3.0))

# Repeat for Educ==2 versus Educ==0 
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==2]) - mean(y.i.1[t==0 & educ==2])
  cate.low <- mean(y.i.0[t==1 & educ==0]) - mean(y.i.1[t==0 & educ==0])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.2.0))

# Repeat for Educ==1 versus Educ==0 
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==1]) - mean(y.i.1[t==0 & educ==1])
  cate.low <- mean(y.i.0[t==1 & educ==0]) - mean(y.i.1[t==0 & educ==0])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.1.0))

detach(data.for.sims)

#####################################
# CATEs FOR ALL VALUES OF KNOWLEDGE #
#####################################

# Estimate CATEs for Different Levels of Knowledge
cate.know0 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$polknowledge==0], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$polknowledge== 0], na.rm=T)
cate.know1 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$polknowledge == 1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$polknowledge == 1], na.rm=T)
cate.know2 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$polknowledge == 2], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$polknowledge == 2], na.rm=T)

diff.cate.know.2.0 <- cate.know2 - cate.know0
diff.cate.know.1.0 <- cate.know1 - cate.know0

y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$polknowledge))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "polknowledge")
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$polknowledge)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

# Polknowledge==2 versus ==0 
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & polknowledge==2]) - mean(y.i.1[t==0 & polknowledge==2])
  cate.low <- mean(y.i.0[t==1 & polknowledge==0]) - mean(y.i.1[t==0 & polknowledge==0])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.know.2.0))

# Polknowledge==1 versus ==0 
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & polknowledge==1]) - mean(y.i.1[t==0 & polknowledge==1])
  cate.low <- mean(y.i.0[t==1 & polknowledge==0]) - mean(y.i.1[t==0 & polknowledge==0])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.know.1.0))

detach(data.for.sims)

######################################
# CATEs FOR ALL VALUES OF DISCUSSION #
######################################

# Estimate CATEs for Different Levels of Discussion
cate.talk1 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$talkpol==1], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$talkpol==1], na.rm=T)
cate.talk2 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$talkpol== 2], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$talkpol==2], na.rm=T)
cate.talk3 <- mean(brazil$voteintent[brazil$cred_vs_less==1 & brazil$talkpol==3], na.rm=T) - 
  mean(brazil$voteintent[brazil$cred_vs_less==0 & brazil$talkpol==3], na.rm=T)

diff.cate.talk.3.1 <- cate.talk3 - cate.talk1
diff.cate.talk.2.1 <- cate.talk2 - cate.talk1

y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$voteintent, brazil$voteintent - ate.hat.cred.v.less)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$voteintent, brazil$voteintent + ate.hat.cred.v.less)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$talkpol))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "talkpol")
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$talkpol)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

# Talkpol==3 versus ==1
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & talkpol==3]) - mean(y.i.1[t==0 & talkpol==3])
  cate.low <- mean(y.i.0[t==1 & talkpol==1]) - mean(y.i.1[t==0 & talkpol==1])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.talk.3.1))

# Talkpol==2 versus ==1
diff.cates.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & talkpol==2]) - mean(y.i.1[t==0 & talkpol==2])
  cate.low <- mean(y.i.0[t==1 & talkpol==1]) - mean(y.i.1[t==0 & talkpol==1])
  diff.cates.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.sims) >= abs(diff.cate.talk.2.1))

detach(data.for.sims)

############################################
# TABLE 3 WITH FEELING THERMOMETER OUTCOME #
############################################

# Estimated Average Treatment Effect for Credible Accusations versus Less Credible Accusations 
ate.hat.cred.v.less.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0], na.rm=T)
# Create the Unobserved Potential Outcomes Based on the estimated ATE
y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$feelingtherm, brazil$feelingtherm - ate.hat.cred.v.less.therm)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$feelingtherm, brazil$feelingtherm + ate.hat.cred.v.less.therm)

# Education

cate.hat.highedu.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1 & brazil$educ==4], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0 & brazil$educ==4], na.rm=T)
cate.hat.lowedu.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1 & brazil$educ<4], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0 & brazil$educ<4], na.rm=T)

diff.cates.edu.therm <- cate.hat.highedu.therm - cate.hat.lowedu.therm

y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$feelingtherm, brazil$feelingtherm - ate.hat.cred.v.less.therm)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$feelingtherm, brazil$feelingtherm + ate.hat.cred.v.less.therm)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$educ))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "educ")
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$educ)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

diff.cates.educ.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & educ==4]) - mean(y.i.1[t==0 & educ==4])
  cate.low <- mean(y.i.0[t==1 & educ < 4]) - mean(y.i.1[t==0 & educ < 4])
  diff.cates.educ.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.educ.sims) >= abs(diff.cates.edu.therm))

detach(data.for.sims)

# Knowledge

cate.hat.highknow.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1 & brazil$highknow==1], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0 & brazil$highknow==1], na.rm=T)
cate.hat.lowknow.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1 & brazil$highknow==0], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0 & brazil$highknow==0], na.rm=T)

diff.cates.know.therm <- cate.hat.highknow.therm - cate.hat.lowknow.therm

y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$feelingtherm, brazil$feelingtherm - ate.hat.cred.v.less.therm)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$feelingtherm, brazil$feelingtherm + ate.hat.cred.v.less.therm)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$highknow))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "highknow")
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$highknow)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

diff.cates.know.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & highknow==1]) - mean(y.i.1[t==0 & highknow==1])
  cate.low <- mean(y.i.0[t==1 & highknow==0]) - mean(y.i.1[t==0 & highknow==0])
  diff.cates.know.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.know.sims) >= abs(diff.cates.know.therm))

detach(data.for.sims)

# Discussion

cate.hat.hightalk.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1 & brazil$hightalk==1], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0 & brazil$hightalk==1], na.rm=T)
cate.hat.lowtalk.therm <- mean(brazil$feelingtherm[brazil$cred_vs_less==1 & brazil$hightalk==0], na.rm=T) - 
  mean(brazil$feelingtherm[brazil$cred_vs_less==0 & brazil$hightalk==0], na.rm=T)

diff.cates.talk.therm <- cate.hat.hightalk.therm - cate.hat.lowtalk.therm

y.i.0 <- ifelse(brazil$cred_vs_less==0, brazil$feelingtherm, brazil$feelingtherm - ate.hat.cred.v.less.therm)
y.i.1 <- ifelse(brazil$cred_vs_less==1, brazil$feelingtherm, brazil$feelingtherm + ate.hat.cred.v.less.therm)

data.for.sims <- data.frame(cbind(y.i.0, y.i.1, brazil$hightalk))
colnames(data.for.sims) <- c("y.i.0", "y.i.1", "hightalk")
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$y.i.0)==0)
data.for.sims <- subset(data.for.sims, is.na(data.for.sims$hightalk)==0)

rm(y.i.0)
rm(y.i.1)

attach(data.for.sims)

diff.cates.talk.sims <- rep(NA, n.sims)
for (i in 1:n.sims) {
  t <- rbinom(n=length(y.i.0), size=1, prob=0.5)
  cate.high <- mean(y.i.0[t==1 & hightalk==1]) - mean(y.i.1[t==0 & hightalk==1])
  cate.low <- mean(y.i.0[t==1 & hightalk==0]) - mean(y.i.1[t==0 & hightalk==0])
  diff.cates.talk.sims[i] <- cate.high - cate.low
}
mean(abs(diff.cates.talk.sims) >= abs(diff.cates.talk.therm))

detach(data.for.sims)


# END OF FILE 