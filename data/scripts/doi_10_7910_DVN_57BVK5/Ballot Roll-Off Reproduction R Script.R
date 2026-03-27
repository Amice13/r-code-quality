## Ryan Carpenter, Daniel J. Mallinson, and Laura Bucci
## Reproduction Script for "Off to the Polls and then Down the Ballot: Are Union Members Less Likely to Roll-Off?"
## Published in Labor Studies Journal

install.packages("haven", "marginaleffects", "ggplot2", "psych")

#library(foreign)
library(haven)
library(marginaleffects)
library(ggplot2)
library(psych)
library(gridExtra)
library(grid)
library(lattice)
library(tidyr)
library(mfx)
"%notin%" <- Negate("%in%")

all.data <- read_dta("cumulative_2006-2021.dta")

data <- all.data

data <- data[c("year", "case_id", "weight_cumulative", "state", "cong", "cd", "race",
               "gender", "age", "citizen", "educ", "ideo5", "pid3", "pid7", 
               "faminc", "union", "union_hh", "employ", "newsint", "vv_turnout_gvm",
               "voted_rep", "voted_sen", "voted_gov")]

data <- data[which(data$vv_turnout_gvm==1),]
data <- data[which(data$year %in% c(2012, 2016, 2020)),]

####################### Data Preparation #############################
#Create Race Voting Indicators
data$house_vote <- 1
data$house_vote[data$voted_rep %in% c(7, 11, 12)] <- 0
data$house_vote[is.na(data$voted_rep)] <- 0
#data$house_vote[is.na(data$voted_rep)] <- NA

data$senate_vote <- 1
data$senate_vote[data$voted_sen %in% c(7)] <- 0
data$senate_vote[is.na(data$voted_sen)] <- 0
#data$senate_vote[is.na(data$voted_sen)] <- NA
#Return to NA for respondents with no Senate election available
data$senate_vote[data$year==2012 & data$state %notin% c(4,6,9,10,12,15,18,23,24,25,26,27,28,29,30,31,32,34,35,36,38,39,42,44,47,48,49,50,51,53,54,55,56)] <- NA
data$senate_vote[data$year==2016 & data$state %notin% c(1,2,4,5,6,8,9,12,13,16,17,18,19,20,21,22,24,29,32,33,36,37,38,39,40,41,42,45,46,49,50,53,55)] <- NA
data$senate_vote[data$year==2020 & data$state %notin% c(1,2,4,5,8,10,13,16,17,19,20,21,22,23,25,26,27,28,30,31,33,34,35,37,40,41,44,45,46,47,48,56)] <- NA

data$gov_vote <- 1
data$gov_vote[data$voted_gov %in% c(4,5,7)] <- 0
data$gov_vote[is.na(data$voted_gov)]  <- 0
#data$gov_vote[is.na(data$voted_gov)]  <- NA
# Return to NA for respondents with no Gov election available
data$gov_vote[data$year==2012 & data$state %notin% c(10,18,29,30,33,37,38,49,50,53,54)] <- NA
data$gov_vote[data$year==2016 & data$state %notin% c(10,18,29,30,33,37,38,41,49,50,53,54)] <- NA
data$gov_vote[data$year==2020 & data$state %notin% c(10,18,29,30,33,37,38,41,49,50,53,54)] <- NA

##Add Any DOWNVOTE DV
data$downvote <- 0
data$downvote[data$voted_gov | data$voted_rep | data$voted_sen ==1] <- 1

## Add Total Downvote DV
votes <- cbind(data$house_vote, data$senate_vote, data$gov_vote)
votes <- replace_na(votes, 0)
votes_total <- rowSums(votes)
data <- cbind(data, votes_total)

#Recode Union
data$union[data$union %in% c(2)] <- 1
data$union[data$union %in% c(3)] <- 0

#Recode Union Household
data$union_hh[data$union_hh %in% c(2)] <- 1
data$union_hh[data$union_hh %in% c(3,4)] <- 0

#Race
data$white <- 0
data$white[data$race==1] <- 1

data$black <-0
data$black[data$race==2] <- 1

#Recode gender
data$gender[data$gender==2] <- 0

#Newsint
data$newsint[data$newsint==7] <- NA

#No Competition Election
nocomp <- read.csv("no_major_candidate.csv")
nocomp$nocomp <- 1
data <- merge(data, nocomp, by=c("year", "cd"), all.x=TRUE)
data$nocomp[is.na(data$nocomp)] <- 0

#Retired
data$retired <- 0
data$retired[data$employ==5] <- 1

#Employed
data$employed <- 0
data$employed[data$employ %in% c(1,2)] <- 1

## Alternative combined union variable
data$unionall <- 0
data$unionall[data$union == 1 | data$union_hh ==1] <- 1

## Pull out retired union
data$unionnoret <- 0
data$unionnoret[data$unionall == 1 & data$retired==0] <- 1

## Create Partisan Strength variable
data$partisanstrength <- NA
data$partisanstrength[data$pid7 == 4] <- 0 #Independent
data$partisanstrength[data$pid7 == 3] <- 1 #Lean Dem
data$partisanstrength[data$pid7 == 5] <- 1 #Lean Rep
data$partisanstrength[data$pid7 == 2] <- 2 #Not Very Strong Dem
data$partisanstrength[data$pid7 == 6] <- 2 #Not Very Strong Rep
data$partisanstrength[data$pid7 == 1] <- 3 #Strong Dem
data$partisanstrength[data$pid7 == 7] <- 3 #Strong Rep

#Create dummies for whether a state had a senate or gubernatorial election
gov <- read.csv("governor_elections.csv")
senate <- read.csv("senate_elections.csv")

gov$gov_election <- 1
senate$senate_election <- 1

data <- merge(data, gov, by=c("year", "state"), all.x=TRUE)
data <- merge(data, senate, by=c("year", "state"), all.x=TRUE)

data$gov_election <- replace_na(data$gov_election, 0)
data$senate_election <- replace_na(data$senate_election, 0)

## Descriptives Table - Table 1
describe(data, skew=FALSE)


#######################  Data Analysis ###############################
###### Tables 2 and A.1 #####
## Any Down Ballot Voting Models
down.union.glm <- glm(downvote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + nocomp +  unionnoret + union_hh + gov_election + senate_election + factor(year) + factor(state), data=data, 
                       family=binomial(link="logit"))
nobs(down.union.glm)
X <- summary(down.union.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)

## House Models

house.union.glm <- glm(house_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + nocomp + unionnoret + union_hh + factor(year) + factor(state), data=data, 
                 family=binomial(link="logit"))
nobs(house.union.glm)
X <- summary(house.union.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)

## Senate Models

senate.union.glm <- glm(senate_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + factor(year) + factor(state), data=data, 
                       family=binomial(link="logit"))
nobs(senate.union.glm)
X <- summary(senate.union.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)

## Governor Models
gov.union.glm <- glm(gov_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + factor(year) + factor(state), data=data, 
                       family=binomial(link="logit"))
nobs(gov.union.glm)
X <- summary(gov.union.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)

## Total Down Ballot Vote Models

total.union.lm <- lm(votes_total ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + gov_election + senate_election + factor(year) + factor(state), data=data)
nobs(total.union.lm)
print(summary(total.union.lm), digits=2)

total.union.poi.raw <- glm(votes_total ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + gov_election + senate_election + factor(year) + factor(state),
                           data = data, family="poisson")
summary(total.union.poi.raw)
nobs(total.union.poi.raw)

total.union.pois <- poissonirr(votes_total ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + gov_election + senate_election + factor(year) + factor(state),
           data = data, robust=TRUE)
total.union.pois

############################ Interaction Models ##################################
###### News Interest
#Any Down Ballot Vote
png("figure1.png", height=8, width=8, units="in", res=600)
down.int.glm <- glm(downvote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + nocomp + newsint + unionnoret + union_hh + unionnoret*newsint  + gov_election + senate_election + factor(year) + factor(state), data=data, 
                      family=binomial(link="logit"))
nobs(down.int.glm)
summary(down.int.glm)
#png("figure0.png", height=6, width=8, units="in", res=600)
a <- plot_predictions(down.int.glm, condition=c("unionnoret", "newsint")) +
  labs(x="Union", y="Probability of Voting", title="(a) Any Down Vote") +
  scale_colour_discrete(name="News Interest",
                        breaks=c("1", "2", "4"),
                        labels=c("Most of the Time", "Some of the Time", "Hardly at All"))

X <- summary(down.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

#dev.off()

#House
house.int.glm <- glm(house_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + nocomp + newsint + unionnoret + union_hh + unionnoret*newsint + factor(year) + factor(state), data=data, 
                       family=binomial(link="logit"))
nobs(house.int.glm)
summary(house.int.glm)
#png("figure1.png", height=6, width=8, units="in", res=600)
b<- plot_predictions(house.int.glm, condition=c("unionnoret", "newsint")) +
  labs(x="Union", y="Probability of Voting", title="(b) House Vote") +
  scale_colour_discrete(name="News Interest",
                           breaks=c("1", "2", "4"),
                           labels=c("Most of the Time", "Some of the Time", "Hardly at All"))

X <- summary(house.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

#dev.off()

#Senate
senate.int.glm <- glm(senate_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + unionnoret*newsint + factor(year) + factor(state), data=data, 
                     family=binomial(link="logit"))
nobs(senate.int.glm)
summary(senate.int.glm)
#png("figure2.png", height=6, width=8, units="in", res=600)
c <- plot_predictions(senate.int.glm, condition=c("unionnoret", "newsint")) +
  labs(x="Union", y="Probability of Voting", title="(c) Senate Vote") +
  scale_colour_discrete(name="News Interest",
                        breaks=c("1", "2", "4"),
                        labels=c("Most of the Time", "Some of the Time", "Hardly at All"))

X <- summary(senate.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

#dev.off()

#Governor
gov.int.glm <- glm(gov_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + unionnoret*newsint + factor(year) + factor(state), data=data, 
                      family=binomial(link="logit"))
nobs(gov.int.glm)
summary(gov.int.glm)
#png("figure3.png", height=6, width=8, units="in", res=600)
d <- plot_predictions(gov.int.glm, condition=c("unionnoret", "newsint")) +
  labs(x="Union", y="Probability of Voting", title="(d) Governor Vote")+
  scale_colour_discrete(name="News Interest",
                        breaks=c("1", "2", "4"),
                        labels=c("Most of the Time", "Some of the Time", "Hardly at All"))

X <- summary(gov.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)


#dev.off()

grid.arrange(a,b,c,d, ncol=2)

dev.off()

######## Race

#Any Down Ballot Vote Union and Race
png("figureanyunionrace.png", height=8, width=8, units="in", res=600)
down.int.glm <- glm(downvote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + nocomp + newsint + unionnoret + union_hh + unionnoret*white + gov_election + senate_election + factor(year) + factor(state), data=data, 
                    family=binomial(link="logit"))
nobs(down.int.glm)
summary(down.int.glm)

plot_predictions(down.int.glm, condition=c("unionnoret", "white")) +
  labs(x="Union", y="Probability of Voting", title="") +
  scale_colour_discrete(name="white",
                        breaks=c("0", "1"),
                        labels=c("Non-white", "White"))

X <- summary(down.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

dev.off()

#House Ballot Vote Union and Race
png("figurehouseunionrace.png", height=8, width=8, units="in", res=600)
house.int.glm <- glm(house_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + nocomp + newsint + unionnoret + union_hh + unionnoret*white + factor(year) + factor(state), data=data, 
                    family=binomial(link="logit"))
nobs(house.int.glm)
summary(house.int.glm)

png("figure2.png", height=8, width=8, units="in", res=600)
plot_predictions(house.int.glm, condition=c("unionnoret", "white")) +
  labs(x="Union", y="Probability of Voting", title="") +
  scale_colour_discrete(name="white",
                        breaks=c("0", "1"),
                        labels=c("Non-white", "White"))

X <- summary(house.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

dev.off()

#Senate Ballot Vote Union and Race
png("figuresenateunionrace.png", height=8, width=8, units="in", res=600)
senate.int.glm <- glm(senate_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + unionnoret*white + factor(year) + factor(state), data=data, 
                     family=binomial(link="logit"))
nobs(senate.int.glm)
summary(senate.int.glm)

plot_predictions(senate.int.glm, condition=c("unionnoret", "white")) +
  labs(x="Union", y="Probability of Voting", title="") +
  scale_colour_discrete(name="white",
                        breaks=c("0", "1"),
                        labels=c("Non-white", "White"))

X <- summary(senate.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

dev.off()

#Governor Ballot Vote Union and Race
#png("figure2.png", height=8, width=8, units="in", res=600)
gov.int.glm <- glm(gov_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed +  newsint + unionnoret + union_hh + unionnoret*white + factor(year) + factor(state), data=data, 
                     family=binomial(link="logit"))
nobs(gov.int.glm)
summary(gov.int.glm)

plot_predictions(gov.int.glm, condition=c("unionnoret", "white")) +
  labs(x="Union", y="Probability of Voting", title="") +
  scale_colour_discrete(name="white",
                        breaks=c("0", "1"),
                        labels=c("Non-white", "White"))

X <- summary(gov.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)

#dev.off()


###### Education
#Any Down Ballot Vote
png("figuresallunioneduc.png", height=8, width=8, units="in", res=600)
down.int.glm <- glm(downvote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + nocomp + newsint + unionnoret + union_hh + unionnoret*educ + gov_election + senate_election + factor(year) + factor(state), data=data, 
                    family=binomial(link="logit"))
nobs(down.int.glm)
summary(down.int.glm)

plot_predictions(down.int.glm, condition=c("unionnoret", "educ")) +
  labs(x="Union", y="Probability of Voting", title="(a) Any Down Vote") +
  scale_colour_discrete(name="Education",
                        breaks=c("1", "2", "3", "4", "5", "6"),
                        labels=c("No HS", "HS", "Some College", "2-Year", "4-Year", "Post-Grad"))

X <- summary(down.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

dev.off()

#House
house.int.glm <- glm(house_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + nocomp + newsint + unionnoret + union_hh + unionnoret*educ + factor(year) + factor(state), data=data, 
                     family=binomial(link="logit"))
nobs(house.int.glm)
summary(house.int.glm)

png("figure3.png", height=6, width=8, units="in", res=600)
plot_predictions(house.int.glm, condition=c("unionnoret", "educ")) +
  labs(x="Union", y="Probability of Voting", title="(b) House Vote") +
  scale_colour_discrete(name="Education",
                        breaks=c("1", "2", "3", "4", "5", "6"),
                        labels=c("No HS", "HS", "Some College", "2-Year", "4-Year", "Post-Grad"))

X <- summary(house.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

dev.off()

#Senate
senate.int.glm <- glm(senate_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + unionnoret*educ + factor(year) + factor(state), data=data, 
                      family=binomial(link="logit"))
nobs(senate.int.glm)
summary(senate.int.glm)

png("figuressenateunioneduc.png", height=6, width=8, units="in", res=600)
plot_predictions(senate.int.glm, condition=c("unionnoret", "educ")) +
  labs(x="Union", y="Probability of Voting", title="(c) Senate Vote") +
  scale_colour_discrete(name="Education",
                        breaks=c("1", "2", "3", "4", "5", "6"),
                        labels=c("No HS", "HS", "Some College", "2-Year", "4-Year", "Post-Grad"))

X <- summary(senate.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
or

dev.off()

#Governor
gov.int.glm <- glm(gov_vote ~ gender + age + educ + white + ideo5 + partisanstrength + faminc + employed + newsint + unionnoret + union_hh + unionnoret*educ + factor(year) + factor(state), data=data, 
                   family=binomial(link="logit"))
nobs(gov.int.glm)
summary(gov.int.glm)

#png("figure3.png", height=6, width=8, units="in", res=600)
plot_predictions(gov.int.glm, condition=c("unionnoret", "educ")) +
  labs(x="Union", y="Probability of Voting", title="")+
  scale_colour_discrete(name="Education",
                        breaks=c(1,2,3,4,5,6),
                        labels=c("No HS", "HS", "Some College", "2-Year", "4-Year", "Post-Grad"))

X <- summary(gov.int.glm)$coef

or <- as.data.frame(X[,1:2])
or$odds <- exp(or[,1])
or$odds.lowerci <- exp(or[,1] - (1.96*or[,2]))
or$odds.upperci <- exp(or[,1] + (1.96*or[,2]))
round(or,2)

dev.off()