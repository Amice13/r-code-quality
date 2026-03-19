# SSI data replication file

# load libraries
library(plyr)
library(ggplot2)
library(gridExtra)
library(QuantPsyc)
library(psych)
library(MASS)
library(Hmisc)
library(foreign)
library(Zelig)
library(ZeligChoice)
library(nnet)
library(survey)

# load data, fyi: change file location
data <-read.csv("/ssidata.csv")

# subset data with fluent english
data <- subset(data, data$language==1)

# summarize data
summary(data)

# change all variables to numeric
sapply(data, as.numeric)

# age percentages reported in text
prop.table(table(data$age<35))
prop.table(tabdata$age>34 & data$age<55))
prop.table(table(data$age>54))

# gender percentages reported in text, 1=male, 2=female
prop.table(table(data$gender))

# education percentages reported in text 
prop.table(table(data$educ<3)) # high school degree or less
prop.table(table(data$educ>2 & data$educ<5)) # some college 
prop.table(table(data$educ==5)) # college graduate

# party id percentage reported in text, 1=democrat, 2=republican
prop.table(table(data$partyid==1))
prop.table(table(data$partyid==2))

# ideology percentage reported in text
prop.table(table(data$ideology<4))
prop.table(table(data$ideology==4))
prop.table(table(data$ideology>4 & data$ideology<8))

# note: trust index is coded with whole numbers, percentages are presented as decimals in the paper
# create new column with recoded govtrust to 0-100 scale
data$govtrustnew[data$govtrust==4] <- 0
data$govtrustnew[data$govtrust==3] <- 33
data$govtrustnew[data$govtrust==2] <- 67
data$govtrustnew[data$govtrust==1] <- 100

# change govtrustnew to numeric
data$govtrustnew <-as.numeric(data$govtrustnew)

# create new column with recoded interests to 0-100 scale
data$interestsnew[data$interests==2] <- 100
data$interestsnew[data$interests==1] <- 0

# change interestsnew to numeric
data$interestsnew <-as.numeric(data$interestsnew)

# create new column with recoded waste to 0-100 scale
data$wastenew[data$waste==3] <- 100
data$wastenew[data$waste==2] <- 50
data$wastenew[data$waste==1] <- 0

# change wastenew to numeric
data$wastenew <-as.numeric(data$wastenew)

# create new column with recoded crooked to 0-100 scale
data$crookednew[data$crooked==3] <- 100
data$crookednew[data$crooked==2] <- 50
data$crookednew[data$crooked==1] <- 0

# change crookednew to numeric
data$crookednew <-as.numeric(data$crookednew)

# add trust items together
data$trustadd <- data$govtrustnew + data$interestsnew + data$wastenew + data$crookednew

# create trust index
data$trustindex <- data$trustadd/4

# mean trust index
meantrustindex <- mean(data$trustindex)
meantrustindex

# standard deviation trust index
sdtrustindex <- sd(data$trustindex)
sdtrustindex

# mean trust iat
meantrustiat <- mean(data$trustiat)
meantrustiat

# standard deviation trust iat
sdtrustiat <- sd(data$trustiat)
sdtrustiat

# t-test of trust iat
ttest <- t.test(data$trustiat, mu=0)
ttest

# standardize trust index for gap measure
data$trustindex_n <- scale(data$trustindex, center=TRUE)

# standardize trust iat for gap measure
data$trustiat_n <- scale(data$trustiat, center=TRUE)

# create gap measure for appendix f
data$gap <- (data$trustindex_n - data$trustiat_n)

# correlation between trust index and trust iat
cortest <- cor.test(data$trustiat, data$trustindex, alternative=c("t"), method=c("pearson"), conf.level=0.95)
cortest

# plot density trust index, figure 1a 
den2 <- density(data$trustindex)
plot(den2, main=NA, xlab=NA)
polygon(den2, col="gray", border="black") 
abline(v = meantrustindex, lty=2, lwd=1.5)
text(27,0.025, "<- Mean", cex=0.75)
title(main = "A. Density of ANES Trust Index", adj=0)

# plot density trust iat, figure 1b
den1 <- density(data$trustiat)
plot(den1, main=NA, xlab=NA) 
polygon(den1, col="gray", border="black") 
abline(v = meantrustiat, lty=2, lwd=1.5)
text(0.2,0.6, "<- Mean", cex=0.75)
title(main="B. Density of Trust ST-IAT D-Scores", adj=0)

# plot of trust index and trust iat, figure 2
scat <- ggplot(data, aes(x=data$trustindex, y=data$trustiat)) +  geom_jitter(position = position_jitter(width = .5))   + geom_smooth(method=loess, span=.5, level=.95) + theme_bw() + labs(x = "Explicit Trust", y = "Implicit Trust D-Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())  + geom_rug(col=rgb(.7,0,.7,alpha=.15), sides="l") + ylim(-1.7, 1.7) + theme(plot.margin = unit(c(2,2,2,2), "mm"))
scat

# subset data into high/low implicit/explicit trust
data1 <- subset(data, data$trustindex>49 & data$trustiat>0) # high explicit/high implicit
data2 <- subset(data, data$trustindex<50 & data$trustiat<0) # low explicit/low implicit
data3 <- subset(data, data$trustindex>49 & data$trustiat<0) # high explicit/low implicit
data4 <- subset(data, data$trustindex<50 & data$trustiat>0) #low explicit/high implicit

# number of people in each category of high/low implicit/explicit
length(data1$gender)
length(data2$gender)
length(data3$gender)
length(data4$gender)

# total sample
79 + 345 + 43 + 569   

# percentage in each category
79/1036  # 8% high on both
345/1036 # 33% low on both
43/1036  # 4% high explicit, low implicit
569/1036 # 55% low explicit, high implcit

# mean trust index for democrats
meantrustindexdem <- mean(data$trustindex[data$partyid==1])
meantrustindexdem

# mean trust index for republicans
meantrustindexrep <- mean(data$trustindex[data$partyid==2])
meantrustindexrep

# mean trust iat for democrats
meantrustiatdem <- mean(data$trustiat[data$partyid==1])
meantrustiatdem

# standard deviation trust iat for democrats
sdtrustiatdem <- sd(data$trustiat[data$partyid==1])
sdtrustiatdem

# t-test of trust iat for democrats
ttestdem <- t.test(data$trustiat[data$partyid==1], mu=0)
ttestdem

# mean trust iat for republicans
meantrustiatrep <- mean(data$trustiat[data$partyid==2])
meantrustiatrep

# standard deviation trust iat for republicans
sdtrustiatrep <- sd(data$trustiat[data$partyid==2])
sdtrustiatrep

# t-test of trust iat for republicans
ttestrep <- t.test(data$trustiat[data$partyid==2], mu=0)
ttestrep

# t-test of trust iat democrats vs. republicans
ttestparty <- t.test(data$trustiat[data$partyid==1], data$trustiat[data$partyid==2])
ttestparty

# mean trust trust index for liberal
meantrustindexlib <- mean(data$trustindex[data$ideology<4])
meantrustindexlib

# mean trust index for conservatives
meantrustindexcon <- mean(data$trustindex[data$ideology>4 & data$ideology<8])
meantrustindexcon

# mean trust index for moderates
meantrustindexmod <- mean(data$trustindex[data$ideology==4])
meantrustindexmod

# mean trust iat for liberals
meantrustiatlib <- mean(data$trustiat[data$ideology<4])
meantrustiatlib

# standard deviation trust iat for liberals
sdtrustiatlib <- sd(data$trustiat[data$ideology<4])
sdtrustiatlib

# t-test of trust iat for liberals
ttestlib <- t.test(data$trustiat[data$ideology<4], mu=0)
ttestlib

# mean trust iat for conservatives
meantrustiatcon <- mean(data$trustiat[data$ideology>4 & data$ideology<8])
meantrustiatcon

# standard deviation trust iat for conservatives
sdtrustiatcon <- sd(data$trustiat[data$ideology>4 & data$ideology<8])
sdtrustiatcon

# t-test of trust iat for conservatives
ttestcon <- t.test(data$trustiat[data$ideology>4 & data$ideology<8], mu=0)
ttestcon

# mean trust iat for moderates
meantrustiatmod <- mean(data$trustiat[data$ideology==4])
meantrustiatmod

# standard deviation trust iat for moderates
sdtrustiatmod <- sd(data$trustiat[data$ideology==4])
sdtrustiatmod

# t-test of trust iat for moderates
ttestmod <- t.test(data$trustiat[data$ideology==4], mu=0)
ttestmod

# recode system justification items so higher numbers means higher system justification
# note: reverse coded items were pre-coded in survey
data$just1n <- abs(data$just1- 10)
data$just2n <- abs(data$just2- 10)
data$just3n <- abs(data$just3- 10)
data$just4n <- abs(data$just4- 10)
data$just5n <- abs(data$just5- 10)
data$just6n <- abs(data$just6- 10)
data$just7n <- abs(data$just7- 10)
data$just8n <- abs(data$just8- 10)

# add together system justification items
data$justadd <- data$just1n + data$just2n + data$just3n + data$just4n + data$just5n + data$just6n + data$just7n + data$just8n

# create system justification index
data$justindex <- data$justadd/8

# mean system justification
justmean <-mean(data$justindex)
justmean

# standard deviation system justification
justsd <-sd(data$justindex)
justsd

# extract system justification item columns              
justdata <- data[c("just1n", "just2n", "just3n", "just4n", "just5n","just6n", "just7n", "just8n")]

# cronbach's alpha system justification
justalpha <- alpha(justdata)
justalpha

# subset data with demographic dont's knows, other, and younger than 18 taken out for models
datanew <-subset(data, income<=11 & ideology<=7 & partyid<=3 & age>17)

# change all variables in datanew to numeric
sapply(datanew, as.numeric)

# recode ethnic variable to white, black, latino, and non-white other
datanew$ethnicnew[datanew$ethnic==1] <- 1
datanew$ethnicnew[datanew$ethnic==2] <- 2
datanew$ethnicnew[datanew$ethnic==3] <- 3
datanew$ethnicnew[datanew$ethnic==4] <- 4
datanew$ethnicnew[datanew$ethnic==5] <- 4
datanew$ethnicnew[datanew$ethnic==6] <- 4
datanew$ethnicnew[datanew$ethnic==7] <- 4
datanew$ethnicnew[datanew$ethnic==8] <- 4

# change ethnicnew to factor
datanew$ethnicnew <- factor(datanew$ethnicnew)

# recode party id to 7 point
datanew$pid7[datanew$repst==1] <- 7
datanew$pid7[datanew$repst==2] <- 6
datanew$pid7[datanew$lean==2] <- 5
datanew$pid7[datanew$lean==3] <- 4
datanew$pid7[datanew$lean==1] <- 3
datanew$pid7[datanew$demst==2] <- 2
datanew$pid7[datanew$demst==1] <- 1

# change party id to numeric
datanew$pid7 <-as.numeric(datanew$pid7)

# change gender to 0/1 coding
datanew$gendernew[datanew$gender==1] <- 0
datanew$gendernew[datanew$gender==2] <- 1

# turn off scientific notation display
options(scipen=999)

# rounding function
rnd <- function(x) trunc(x+sign(x)*0.5)

# round system justification to whole number
datanew$justindex <-rnd(datanew$justindex)

# change system justification to factor
datanew$justindex <-as.factor(datanew$justindex)

# system justification probit model with trust iat only, table 1, model 1
modeljust0 <- polr(justindex ~ trustiat, data=datanew, method="probit", Hess=TRUE)
summary(modeljust0)

# obtain p-values in combined table
j0 <- coef(summary(modeljust0))
p <- pnorm(abs(j0[, "t value"]), lower.tail = FALSE) * 2
j0table <- cbind(j0, "p value" = p)
j0table

# system justification probit model with trust index, table 1, model 2
modeljust <- polr(justindex ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modeljust)

# obtain p-values in combined table
j <- coef(summary(modeljust))
p <- pnorm(abs(j[, "t value"]), lower.tail = FALSE) * 2
jtable <- cbind(j, "p value" = p)
jtable

# simulated predicted probabilities for system justification model

# change gender to factor
datanew$gendernew <-as.factor(datanew$gendernew)

# estimate model
modeljustz <- zelig(justindex ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, model = "oprobit", data = datanew)
summary(modeljustz)

# set variables to mean, ethnicnew to white, gendernew to female, iat to min and max
just_iatmin <- setx(modeljustz, trustiat = -2, trustindex = mean(datanew$trustindex), pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
               ethnicnew =  1, gendernew = 1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))

               
just_iatmax <- setx(modeljustz, trustiat = 2, trustindex = mean(datanew$trustindex), pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
              ethnicnew =  1, gendernew =  1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))
                                             
iatdif <- sim(modeljustz, x = just_iatmin, x1 = just_iatmax)
iatdif # predicted probablities for trust iat

# set variables to mean, ethnicnew to white, gendernew to female, trust index to min and max
just_trustmin <- setx(modeljustz, trustiat = mean(datanew$trustiat), trustindex = 0, pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                    ethnicnew =  1, gendernew = 1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))


just_trustmax <- setx(modeljustz, trustiat = mean(datanew$trustiat), trustindex = 100, pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                    ethnicnew =  1, gendernew =  1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))

trustdif <- sim(modeljustz, x = just_trustmin, x1 = just_trustmax)
trustdif # predicted probabilities for trust index, indicated in footnote 

# system justification probit model without trust index, table 1, model 3
modeljust2 <- polr(justindex ~ trustiat + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modeljust2)

# obtain p-values in combined table
j <- coef(summary(modeljust2))
p <- pnorm(abs(j[, "t value"]), lower.tail = FALSE) * 2
j2table <- cbind(j, "p value" = p)
j2table

# system justification probit model with gap, appendix f, table a, model 2
modeljust3 <- polr(justindex ~ gap + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modeljust3)

# obtain p-values in combined table
j <- coef(summary(modeljust3))
p <- pnorm(abs(j[, "t value"]), lower.tail = FALSE) * 2
j3table <- cbind(j, "p value" = p)
j3table

# system justification probit model with gap only, appendix f, table a, model 1
modeljust4 <- polr(justindex ~ gap, data=datanew, method="probit", Hess=TRUE)
summary(modeljust4)

# obtain p-values in combined table
j <- coef(summary(modeljust4))
p <- pnorm(abs(j[, "t value"]), lower.tail = FALSE) * 2
j4table <- cbind(j, "p value" = p)
j4table

# change govt spending to factor
datanew$govtspend <-as.factor(datanew$govtspend)

# govt spending probit model, table 2, increase government spending 
modelspend <- polr(govtspend ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modelspend)

# obtain p-values in combined table
s <- coef(summary(modelspend))
p <- pnorm(abs(s[, "t value"]), lower.tail = FALSE) * 2
stable <- cbind(s, "p value" = p)
stable

# govt spending model with gap, appendix f, table 2, model increase government spending
modelspend2 <- polr(govtspend ~ gap + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modelspend2)

# obtain p-values in combined table
s <- coef(summary(modelspend2))
p <- pnorm(abs(s[, "t value"]), lower.tail = FALSE) * 2
s2table <- cbind(s, "p value" = p)
s2table

# recode welfare spending 
datanew$welfspendb[datanew$welfspend=="1"] <- "1"
datanew$welfspendb[datanew$welfspend=="2"] <- "1"
datanew$welfspendb[datanew$welfspend=="3"] <- "0"

# change welfspendb to factor
datanew$welfspendb <-as.factor(datanew$welfspendb)

# probit model welfare spending, table 2, model increase welfare spending
modelwelf <-glm(welfspendb ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, family=binomial(link="probit"), data=datanew)
summary(modelwelf)

# welfare spending model with gap, appendix f, table b, model increase welfare spending
modelwelf2 <-glm(welfspendb ~ gap + pid7 + ideology + ethnicnew + gendernew + income + education + age, family=binomial(link="probit"), data=datanew)
summary(modelwelf2)

# recode homeland security spending 
datanew$secspendb[datanew$secspend=="1"] <- "1"
datanew$secspendb[datanew$secspend=="2"] <- "1"
datanew$secspendb[datanew$secspend=="3"] <- "0"

# change secspendb to factor
datanew$secspendb <-as.factor(datanew$secspendb)

# probit model security spending, table 2, increase homeland security spending
modelsec <- glm(secspendb ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, family=binomial(link="probit"), data=datanew)
summary(modelsec)

# security spending model with gap, appendix f, table b, model increase homeland security spending
modelsec2 <- glm(secspendb ~ gap + pid7 + ideology + ethnicnew + gendernew + income + education + age, family=binomial(link="probit"), data=datanew)
summary(modelsec2)

# mean disaster
disastermean <-mean(data$disaster)
disastermean

# mean attack
attackmean <-mean(data$attack)
attackmean

# change disaster to factor
datanew$disaster <-as.factor(datanew$disaster)

# disaster probit model, table 3, disaster model 1
modeldisaster <- polr(disaster ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modeldisaster)

# obtain p-values in combined table
d <- coef(summary(modeldisaster))
p <- pnorm(abs(d[, "t value"]), lower.tail = FALSE) * 2
dtable <- cbind(d, "p value" = p)
dtable

# simulated predicted probabilities for disaster model
# estimate model
modeldisasterz <- zelig(disaster ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, model = "oprobit", data = datanew)
summary(modeldisasterz)

# set variables to mean, ethnicnew to white, gendernew to female, iat to min and max
disaster_iatmin <- setx(modeldisasterz, trustiat = -2, trustindex = mean(datanew$trustindex), pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                      ethnicnew =  1, gendernew = 1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))


disaster_iatmax <- setx(modeldisasterz, trustiat = 2, trustindex = mean(datanew$trustindex), pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                      ethnicnew =  1, gendernew =  1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))

iatdif <- sim(modeldisasterz, x = disaster_iatmin, x1 = disaster_iatmax)
iatdif # predicted probabilities for trust iat

# probabilities from output
disaster_probs <- c(-0.139239288, -0.135118178,  -0.097312017, -0.009428482, 0.140876332, 0.118995678, 0.121225956)
mean(abs(disaster_probs))

# set variables to mean, ethnicnew to white, gendernew to female, trust index to min and max
disaster_trustmin <- setx(modeldisasterz, trustiat = mean(datanew$trustiat), trustindex = 0, pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                        ethnicnew =  1, gendernew = 1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))


disaster_trustmax <- setx(modeldisasterz, trustiat = mean(datanew$trustiat), trustindex = 100, pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                        ethnicnew =  1, gendernew =  1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))

trustdif <- sim(modeldisasterz, x = disaster_trustmin, x1 = disaster_trustmax)
trustdif # predicted probabilities for trust index

# probabilities from output
disaster_probs2 <- c(-0.13607151, -0.17984108, -0.19115030,  -0.19619709,  0.01932398,  0.18069783, 0.50323817)
mean(abs(disaster_probs2))

# disaster probit model without trust index, table 3, model 3
modeldisaster2 <- polr(disaster ~ trustiat + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modeldisaster2)

# obtain p-values in combined table
d <- coef(summary(modeldisaster2))
p <- pnorm(abs(d[, "t value"]), lower.tail = FALSE) * 2
d2table <- cbind(d, "p value" = p)
d2table

# disaster probit model with gap, appendix f, table c, model 1
modeldisaster3 <- polr(disaster ~ gap + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modeldisaster3)

# obtain p-values in combined table
d <- coef(summary(modeldisaster3))
p <- pnorm(abs(d[, "t value"]), lower.tail = FALSE) * 2
d3table <- cbind(d, "p value" = p)
d3table

# change attack to factor
datanew$attack <-as.factor(datanew$attack)

# attack probit model, table 3, model 2 
modelattack <- polr(attack ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modelattack)

# obtain p-values in combined table
a <- coef(summary(modelattack))
p <- pnorm(abs(a[, "t value"]), lower.tail = FALSE) * 2
atable <- cbind(a, "p value" = p)
atable

# simulated predicted probabilities for attack model
# estimate model
modelattackz <- zelig(attack ~ trustiat + trustindex + pid7 + ideology + ethnicnew + gendernew + income + education + age, model = "oprobit", data = datanew)
summary(modelattackz)

# set variables to mean, ethnicnew to white, gendernew to female, iat to min and max
attack_iatmin <- setx(modelattackz, trustiat = -2, trustindex = mean(datanew$trustindex), pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                    ethnicnew =  1, gendernew = 1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))


attack_iatmax <- setx(modelattackz, trustiat = 2, trustindex = mean(datanew$trustindex), pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                    ethnicnew =  1, gendernew =  1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))

iatdif <- sim(modelattackz, x = attack_iatmin, x1 = attack_iatmax)
iatdif # predicted probabilities for trust iat

# probabilities from output
attack_probs <- c(-0.12117958, -0.09566880,  -0.08799772,  -0.03981975, 0.06697935,  0.12095292, 0.15673358)
mean(abs(attack_probs))

# set variables to mean, ethnicnew to white, gendernew to female, trust index to min and max
attack_trustmin <- setx(modelattackz, trustiat = mean(datanew$trustiat), trustindex = 0, pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                      ethnicnew =  1, gendernew = 1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))


attack_trustmax <- setx(modelattackz, trustiat = mean(datanew$trustiat), trustindex = 100, pid7 = mean(datanew$pid7), ideology = mean(datanew$ideology), 
                      ethnicnew =  1, gendernew =  1, income = mean(datanew$income), education = mean(datanew$education), age = mean(datanew$age))

trustdif <- sim(modelattackz, x = attack_trustmin, x1 = attack_trustmax)
trustdif # predicted probabilities for trust index

# probabilities from output
attack_probs2 <- c(-0.1137321, -0.1255871,  -0.1592346, -0.1603972, -0.0400540,  0.1463454,  0.4526596)
mean(abs(attack_probs2))

# attack probit model without trust index, table 3, model 4
modelattack2 <- polr(attack ~ trustiat + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modelattack2)

# obtain p-values in combined table
a <- coef(summary(modelattack2))
p <- pnorm(abs(a[, "t value"]), lower.tail = FALSE) * 2
a2table <- cbind(a, "p value" = p)
a2table

# attack probit model with gap, appendix f, table c, model attack
modelattack3 <- polr(attack ~ gap + pid7 + ideology + ethnicnew + gendernew + income + education + age, data=datanew, method="probit", Hess=TRUE)
summary(modelattack3)

# obtain p-values in combined table
a <- coef(summary(modelattack3))
p <- pnorm(abs(a[, "t value"]), lower.tail = FALSE) * 2
a3table <- cbind(a, "p value" = p)
a3table

# analysis for appendix b

# trust iat summary
mean(data$trustiat)
median(data$trustiat)
sd(data$trustiat)

# trust index summary
mean(data$trustindex)
median(data$trustindex)
sd(data$trustindex)

# system justification summary
mean(data$justindex)
median(data$justindex)
sd(data$justindex)

# government spending summary
mean(data$govtspend)
median(data$govtspend)
sd(data$govtspend)

# disaster summary
mean(data$disaster)
median(data$disaster)
sd(data$disaster)

# attack summary
mean(data$attack)
median(data$attack)
sd(data$attack)

# welfare spending percentages, 1=increase, 2=stay the same, 3=decrease
prop.table(table(data$welfspend))

# homeland security spending percentages, 1=increase, 2=stay the same, 3=decrease
prop.table(table(data$secspend))

# analysis for appendix c

# mean trust index for 35 and younger
meantrustindexyoung <- mean(data$trustindex[data$age<36 & data$age>17])
meantrustindexyoung

# mean trust index for 36 and older
meantrustindexold <- mean(data$trustindex[data$age>35])
meantrustindexold

# mean trust iat for 35 and younger
meantrustiatyoung <- mean(data$trustiat[data$age<36 & data$age>17])
meantrustiatyoung

# standard deviation trust iat for 35 and younger
sdtrustiatyoung <- sd(data$trustiat[data$age<36 & data$age>17])
sdtrustiatyoung

# t-test of trust iat for 35 and younger
ttestyoung <- t.test(data$trustiat[data$age<36 & data$age>17], mu=0)
ttestyoung

# mean trust iat for 36 and older
meantrustiatold <- mean(data$trustiat[data$age>35])
meantrustiatold

# standard deviation trust iat for 36 and older
sdtrustiatold <- sd(data$trustiat[data$age>35])
sdtrustiatold

# t-test of trust iat for 36 and older
ttestold <- t.test(data$trustiat[data$age>35], mu=0)
ttestold

# mean trust trust index for males
meantrustindexmale <- mean(data$trustindex[data$gender==1])
meantrustindexmale

# mean trust trust index for females
meantrustindexfemale <- mean(data$trustindex[data$gender==2])
meantrustindexfemale

# mean trust iat for males
meantrustiatmale <- mean(data$trustiat[data$gender==1])
meantrustiatmale

# standard deviation trust iat for males
sdtrustiatmale <- sd(data$trustiat[data$gender==1])
sdtrustiatmale

# t-test of trust iat for males
ttestmale <- t.test(data$trustiat[data$gender==1], mu=0)
ttestmale

# mean trust iat for females
meantrustiatfemale <- mean(data$trustiat[data$gender==2])
meantrustiatfemale

# standard deviation trust iat for females
sdtrustiatfemale <- sd(data$trustiat[data$gender==2])
sdtrustiatfemale

# t-test of trust iat for females
ttestfemale <- t.test(data$trustiat[data$gender==2], mu=0)
ttestfemale

# calculate weights 
49/32 # population/sample male
51/68 # population/sample female

# create column with weights
data$weight[data$gender==1] <- 1.53125
data$weight[data$gender==2] <- 0.75

# weighted mean
weightedmean <- sum(data$trustiat*data$weight)/sum(data$weight)
weightedmean

# mean trust trust index for college and above
meantrustindexcol <- mean(data$trustindex[data$educ>4])
meantrustindexcol

# mean trust trust index for no college and below
meantrustindexnocol <- mean(data$trustindex[data$educ<5])
meantrustindexnocol

# mean trust iat for college and above
meantrustiatcol <- mean(data$trustiat[data$educ>4])
meantrustiatcol

# standard deviation trust iat for college and above
sdtrustiatcol <- sd(data$trustiat[data$educ>4])
sdtrustiatcol

# t-test of trust iat for college and above
ttestcol <- t.test(data$trustiat[data$educ>4], mu=0)
ttestcol

# mean trust iat for no college and below
meantrustiatnocol <- mean(data$trustiat[data$educ<5])
meantrustiatnocol

# standard deviation trust iat for no college and below
sdtrustiatnocol <- sd(data$trustiat[data$educ<5])
sdtrustiatnocol

# t-test of trust iat for no college and below
ttestnocol <- t.test(data$trustiat[data$educ<5], mu=0)
ttestnocol

# mean trust trust index 50K+
meantrustindexrich <- mean(data$trustindex[data$income>5 & data$income<12])
meantrustindexrich

# mean trust trust index lower 50k
meantrustindexpoor <- mean(data$trustindex[data$income<6])
meantrustindexpoor

# mean trust iat for 50K+
meantrustiatrich <- mean(data$trustiat[data$income>5 & data$income<12])
meantrustiatrich

# standard deviation trust iat for 50K+
sdtrustiatrich <- sd(data$trustiat[data$income>5 & data$income<12])
sdtrustiatrich

# t-test of trust iat for 50K+
ttestrich <- t.test(data$trustiat[data$income>5 & data$income<12], mu=0)
ttestrich

# mean trust iat for lower 50K
meantrustiatpoor <- mean(data$trustiat[data$income<6])
meantrustiatpoor

# standard deviation trust iat for lower 50K
sdtrustiatpoor <- sd(data$trustiat[data$income<6])
sdtrustiatpoor

# t-test of trust iat for lower 50K
ttestpoor <- t.test(data$trustiat[data$income<6], mu=0)
ttestpoor

# race/ethnicity percentages, 1=white, 2=black, 3=latino
prop.table(table(data$ethnic))

# mean trust index for whites
meantrustindexwhite <- mean(data$trustindex[data$ethnic==1])
meantrustindexwhite

# mean trust index for blacks
meantrustindexblack <- mean(data$trustindex[data$ethnic==2])
meantrustindexblack

# mean trust index for latino
meantrustindexlat <- mean(data$trustindex[data$ethnic==3])
meantrustindexlat

# mean trust iat for whites
meantrustiatwhite <- mean(data$trustiat[data$ethnic==1])
meantrustiatwhite

# standard deviation trust iat for whites
sdtrustiatwhite <- sd(data$trustiat[data$ethnic==1])
sdtrustiatwhite

# t-test of trust iat for whites
ttestwhite<- t.test(data$trustiat[data$ethnic==1], mu=0)
ttestwhite

# mean trust iat for blacks
meantrustiatblack <- mean(data$trustiat[data$ethnic==2])
meantrustiatblack

# standard deviation trust iat for blacks
sdtrustiatblack <- sd(data$trustiat[data$ethnic==2])
sdtrustiatblack

# t-test of trust iat for blacks
ttestblack<- t.test(data$trustiat[data$ethnic==2], mu=0)
ttestblack

# mean trust iat for latinos
meantrustiatlat <- mean(data$trustiat[data$ethnic==3])
meantrustiatlat

# standard deviation trust iat for latinos
sdtrustiatlat <- sd(data$trustiat[data$ethnic==3])
sdtrustiatlat

# t-test of trust iat for latinos
ttestlat <- t.test(data$trustiat[data$ethnic==3], mu=0)
ttestlat







