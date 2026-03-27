

## This replication code creates the Tables and Figures used in:
## Sharan Grewal, Amaney Jamal, Tarek Masoud, and Elizabeth Nugent. "Poverty and 
## Divine Rewards: The Electoral Advantage of Islamist Political Parties." American 
## Journal of Political Science, Forthcoming.


## Load packages
library(effects)
library(CBPS)
library(MatchIt)
library(optmatch)
library(ggplot2)
library(scales)
library(stargazer)
library(mediation)
library(stats)
library(sampleSelection)



##############
## Figure 1 ##
##############

ab <- read.csv("ab.csv")

tun <- lm(trust~muslim+age+gender+marry+income, data=ab[ab$wave==2 & ab$country=="Tunisia",], weights=wt)
eg <- lm(trust~muslim+age+gender+marry+income, data=ab[ab$wave==2 & ab$country=="Egypt",], weights=wt)
all <- lm(trust~muslim+age+gender+marry+income+country, data=ab[ab$country!="Yemen" & ab$country!="Libya"& ab$country!="Iraq",], weights=wt)


## Tunisia 2011 (a)
plot(effect("income", tun, xlevels=list(income=c(0,0.2,0.4,0.6,0.8,1))), 
                ylim=c(0.2,4), rug=FALSE,colors=c("black","grey"),
                xlab="Income (0-1)", ylab="Trust in Islamists (1-4)",
                main="Income and Trust in Islamists (Tunisia 2011)")

## Egypt 2011 (b)
plot(effect("income", eg, xlevels=list(income=c(0,0.2,0.4,0.6,0.8,1))), 
               ylim=c(0.2,4), rug=FALSE, colors=c("black","grey"),
               xlab="Income (0-1)", ylab="Trust in Islamists (1-4)",
               main="Income and Trust in Islamists (Egypt 2011)")

## All (c)
plot(effect("income", all, xlevels=list(income=c(0,0.2,0.4,0.6,0.8,1))), 
    rug=FALSE, xlab="Income (0-1)", ylab="Trust in Islamists (1-4)", 
    main="Income and Trust in Islamists (2010-2014)", colors=c("black","grey"))





##############
## Figure 2 ##
##############

elex <- read.csv("elex.csv")

pos <- ifelse(elex$Delegation=="Sidi Bou Zid" | elex$Delegation=="Kairouan" | 
                elex$Delegation=="Medenine" | elex$Delegation=="Tozeur" |
                elex$Delegation=="Jendouba" | elex$Delegation=="Nabeul 1", 3,
              ifelse(elex$Delegation=="Gafsa" | 
                       elex$Delegation=="Ariana" | elex$Delegation=="Beja", 2,
                     ifelse(elex$Delegation=="Zaghouan" | elex$Delegation=="Monastir" | 
                              elex$Delegation=="Tunis 1" | elex$Delegation=="Tozeur" |
                              elex$Delegation=="Ben Arous" | elex$Delegation=="Sousse", 4, 1)))
plot(y=elex$Percentage, x=elex$Unemp2010, main="Ennahda Vote Share and Unemployment\nby Governorate, 2011", ylab="Vote Share", xlab="Unemployment Rate")
abline(lm(Percentage~Unemp2010, data=elex), col="red")
text(Percentage~Unemp2010, labels=Delegation, cex=0.75, pos=pos, data=elex)

plot(y=elex$Percentage.1, x=elex$Unemployment.2014, main="Ennahda Vote Share and Unemployment\nby Governorate, 2014", ylab="Vote Share", xlab="Unemployment Rate", ylim=c(15,65))
abline(lm(Percentage.1~Unemployment.2014, data=elex), col="red")
text(Percentage.1~Unemployment.2014, labels=Delegation, cex=0.75, pos=pos, data=elex)


pos <- ifelse(elex$Delegation=="Zaghouan", 2,
              ifelse(elex$Delegation=="Nabeul 2" | elex$Delegation=="Kef" | 
                       elex$Delegation=="Sfax 2" | elex$Delegation=="Sousse" |
                       elex$Delegation=="Gafsa" | elex$Delegation=="Tozeur", 3, 1))
plot(y=elex$Vote.Change.from.2011, x=elex$unempchange, main="Change in Ennahda Vote Share and\nUnemployment Rates by Governorate, 2011-2014", ylab="Change in Vote Share", 
     xlab="Change in Unemployment Rate")
abline(lm(Vote.Change.from.2011~unempchange, data=elex), col="red")
text(Vote.Change.from.2011~unempchange, labels=Delegation, cex=0.75, pos=pos, data=elex)
abline(h=0,lty=3)







##################
## Experiment 1 ##
##################

exp1 <- read.csv("exp1.csv", row.names=1)

## Matching full sample
cbps.out <- CBPS(treatment~age+female+own+inc+edu+urb+marry+enumfemale+emp+stud+unemp, 
                 data=exp1, ATT = TRUE)
m.out <- matchit(treatment ~ fitted(cbps.out), data=exp1, method="full", replace = TRUE)
m.data <- match.data(m.out)

## Matching just poor
test <- exp1[exp1$inc<11,]
cbps.out2 <- CBPS(treatment~age+female+own+inc+edu+urb+marry+enumfemale+emp+stud+unemp, 
                  data=test, ATT=TRUE)
m.out2 <- matchit(treatment~fitted(cbps.out2), data=test, method="full", replace = TRUE)
m.data2 <- match.data(m.out2)



## Figure 3: Trust in Ennahda
data.summary <- as.data.frame(rbind(
  summary(lm(trusten~treatment, data=m.data, weight=weights))$coef[2,1:2],
  summary(lm(trusten~treatment, data=m.data2, weight=weights))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(m.data$trusten[m.data$treatment==1]),
                    length(m.data2$trusten[m.data2$treatment==1]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Full Sample\n(N=203)", "Poor\n(N=97)")
data.summary$name <- factor(data.summary$name, levels=c("Poor\n(N=97)","Full Sample\n(N=203)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  ggtitle("Trust in Ennahda (1-4)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.2, 1.2),breaks=seq(-0.2, 1.2, by=0.2)) +
  theme(text = element_text(size=17))




## Figure 4: Reliance on Divine Rewards
data.summary <- as.data.frame(rbind(
  summary(lm(fightrev~treatment, data=m.data, weight=weights))$coef[2,1:2],
  summary(lm(fightrev~treatment, data=m.data2, weight=weights))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(m.data$fightrev[m.data$treatment==1]),
                    length(m.data2$fightrev[m.data2$treatment==1]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Full Sample\n(N=203)", "Poor\n(N=97)")
data.summary$name <- factor(data.summary$name, levels=c("Poor\n(N=97)","Full Sample\n(N=203)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  ggtitle("Reliance on Divine Rewards (1-5)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.5, 2),breaks=seq(-0.5, 2, by=0.5)) +
  theme(text = element_text(size=17))



## Figure 5: Moderator
data.summary <- as.data.frame(rbind(
  summary(lm(trusten~treatment, data=m.data2[m.data2$fightrev<3,], weight=weights))$coef[2,1:2],
  summary(lm(trusten~treatment, data=m.data2[m.data2$fightrev>2,], weight=weights))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(m.data2$trusten[m.data2$treatment==1 & m.data2$fightrev<3]),
                    length(m.data2$trusten[m.data2$treatment==1 & m.data2$fightrev>2]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Poor,\nNot Reliant\n(N=45)", "Poor,\nReliant\n(N=52)")
data.summary$name <- factor(data.summary$name, levels=c("Poor,\nReliant\n(N=52)", "Poor,\nNot Reliant\n(N=45)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  ggtitle("Trust in Ennahda (1-4)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.7, 1.6),breaks=seq(-0.5, 1.6, by=0.5)) +
  theme(text = element_text(size=17))















##################
## Experiment 2 ##
##################

data <- read.csv("exp2.csv")


## Figure 6: Voting for Ennahda
data.summary <- as.data.frame(rbind(
  summary(lm(votenahda~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(votenahda~treat, data=data[data$quran==0 & data$inc<7,]))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(data$votenahda[data$treat==1 & data$quran==0]),
                    length(data$votenahda[data$treat==1 & data$quran==0 & data$inc<7]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Full Sample\n(N=201)", "Poor\n(N=105)")
data.summary$name <- factor(data.summary$name, levels=c("Poor\n(N=105)","Full Sample\n(N=201)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  ggtitle("Likelihood of Voting For Ennahda") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.1, 0.4),breaks=seq(-0.1, 0.4, by=0.1)) +
  theme(text = element_text(size=17))


## Figure 7: Reliance on Divine Rewards

data.summary <- as.data.frame(rbind(
  summary(lm(relrev~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(relrev~treat, data=data[data$quran==0 & data$inc<7,]))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(data$relrev[data$treat==1 & data$quran==0]),
                    length(data$relrev[data$treat==1 & data$quran==0 & data$inc<7]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Full Sample\n(N=201)", "Poor\n(N=105)")
data.summary$name <- factor(data.summary$name, levels=c("Poor\n(N=105)","Full Sample\n(N=201)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  ggtitle("Reliance on Divine Rewards (1-5)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.2, 1.3),breaks=seq(-0.2, 1.3, by=0.2)) +
  theme(text = element_text(size=17))






## Table 4: Quran v. no Quran
one <- lm(votenahda~treat, data=data[data$quran==0 & data$inc<7,])
two <- lm(votenahda~treat, data=data[data$quran==1 & data$inc<7,])
three <- lm(relrev~treat, data=data[data$quran==0 & data$inc<7,])
four <- lm(relrev~treat, data=data[data$quran==1 & data$inc<7,])
stargazer(one, two, three, four)




## Figure 8: Parallel Design Mediation

summary(multimed(outcome="votenahda", med.main="relrev", treat="treat", experiment="quran", 
        data=data[data$inc<7,], design="parallel", sims=100, R2.by=0.01, conf.level=0.95))


data.summary <- as.data.frame(cbind(c(0.2291,-0.0474,0.1815),c(0.017,-0.1848,0.0358),c(0.44,0.09,0.31)))
colnames(data.summary) <- c("mean", "lower", "upper")
data.summary$name <- c("ACME", "ADE", "Total")
data.summary$name <- factor(data.summary$name, levels=c("Total", "ADE", "ACME"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=lower, xmax=upper, height=0.1), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Parallel Design Causal Mediation Analysis") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Likelihood of Voting for Ennahda") +
  ylab("") +
  scale_x_continuous(limits=c(-0.2, 0.5),breaks=seq(-0.2, 0.5, by=0.2)) +
  theme(text = element_text(size=17))





## Robustness Check (p. 35)
summary(lm(votenidaa~treat, data=data[data$quran==0 & data$inc<7,]))
summary(lm(votejabha~treat, data=data[data$quran==0 & data$inc<7,]))
summary(lm(voteirada~treat, data=data[data$quran==0 & data$inc<7,]))

summary(lm(votenidaa~relrev, data=data[data$quran==0 & data$inc<7,]))
summary(lm(votejabha~relrev, data=data[data$quran==0 & data$inc<7,]))
summary(lm(voteirada~relrev, data=data[data$quran==0 & data$inc<7,]))




## In-text results, p. 36
mean(data$voteAllah3[data$votenahda==1 & data$inc<7], na.rm=TRUE)
mean(data$voteAllah3[data$votenidaa==1 & data$inc<7], na.rm=TRUE)
mean(data$voteAllah3[data$votejabha==1 & data$inc<7], na.rm=TRUE)
mean(data$voteAllah3[data$voteirada==1 & data$inc<7], na.rm=TRUE)
summary(lm(voteAllah3~votenahda+votejabha+voteirada, data=data[data$inc<7 & data$votenahda==1 |
        data$inc<7 & data$votenidaa==1 | data$inc<7 & data$votejabha==1 | 
        data$inc<7 & data$voteirada==1,]))






##############
## APPENDIX ##
##############

## Table 3: Income and Islamism, Arab Barometer (OLS)
stargazer(eg, tun, all)

## Table 4: Ennahda Vote Share by Unemployment Rate
stargazer(lm(Percentage~Unemp2010, data=elex), 
          lm(Percentage.1~Unemployment.2014, data=elex),
          lm(Vote.Change.from.2011~unempchange, data=elex))

## Fig. S6: Covariate Balance, Experiment 1 (Unmatched)
balance <- as.data.frame(rbind(
  summary(lm((age/10)~treatment, data=exp1))$coef[2,1:2],
  summary(lm(female~treatment, data=exp1))$coef[2,1:2],
  summary(lm(own~treatment, data=exp1))$coef[2,1:2],
  summary(lm(inc~treatment, data=exp1))$coef[2,1:2],
  summary(lm(edu~treatment, data=exp1))$coef[2,1:2],
  summary(lm(urb~treatment, data=exp1))$coef[2,1:2],
  summary(lm(unemp~treatment, data=exp1))$coef[2,1:2],
  summary(lm(marry~treatment, data=exp1))$coef[2,1:2],
  summary(lm(emp~treatment, data=exp1))$coef[2,1:2],
  summary(lm(stud~treatment, data=exp1))$coef[2,1:2],
  summary(lm(enumfemale~treatment, data=exp1))$coef[2,1:2]
))
colnames(balance) <- c("mean", "se")
balance$name <- c("Age/10", "Female (0-1)", "Ownership (1-3)", "Income (1-12)",
                  "Education (1-6)", "Urban (0-1)", "Unemployed (0-1)", 
                  "Never Married (0-1)", "Employed (0-1)", "Student (0-1)",
                  "Female Enum. (0-1)")
ggplot(balance,
       aes(x=balance$mean, y=balance$name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=balance$mean-1.96*balance$se, 
                     xmax=balance$mean+1.96*balance$se, height=0.5), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Balance Plot (Unmatched)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Lose - Win") +
  ylab("") +
  scale_x_continuous(limits=c(-0.6, 1.25),breaks=seq(-0.6, 1.2, by=0.6)) +
  theme(text = element_text(size=17))


## Fig. S7. Covariate Balance among poor who rely on God (a) and do not rely on God (b),
## Experiment 1 (Matched then Sub-setted)
balance <- as.data.frame(rbind(
  summary(lm((age/10)~treatment,data=m.data2[m.data2$fightrev>2,],weights=weights))$coef[2,1:2],
  summary(lm(female~treatment, data=m.data2[m.data2$fightrev>2,],weights=weights))$coef[2,1:2],
  summary(lm(own~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2],
  summary(lm(inc~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2],
  summary(lm(edu~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2],
  summary(lm(urb~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2],
  summary(lm(unemp~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2],
  summary(lm(marry~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2],
  summary(lm(enumfemale~treatment, data=m.data2[m.data2$fightrev>2,], weights=weights))$coef[2,1:2]))
colnames(balance) <- c("mean", "se")
balance$name <- c("Age/10", "Female (0-1)", "Ownership (1-3)", "Income (1-10)",
                  "Education (1-6)", "Urban (0-1)", "Unemployed (0-1)", 
                  "Married (0-1)", "Female Enum. (0-1)")

ggplot(balance,
       aes(x=balance$mean, y=balance$name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=balance$mean-1.96*balance$se, 
                     xmax=balance$mean+1.96*balance$se, height=0.5), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Poor & Reliant (Matched)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Lose - Win") +
  ylab("") +
  scale_x_continuous(limits=c(-2, 1.5),breaks=seq(-2, 1.5, by=0.5)) +
  theme(text = element_text(size=17))

balance <- as.data.frame(rbind(
  summary(lm((age/10)~treatment,data=m.data2[m.data2$fightrev<3,],weights=weights))$coef[2,1:2],
  summary(lm(female~treatment, data=m.data2[m.data2$fightrev<3,],weights=weights))$coef[2,1:2],
  summary(lm(own~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2],
  summary(lm(inc~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2],
  summary(lm(edu~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2],
  summary(lm(urb~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2],
  summary(lm(unemp~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2],
  summary(lm(marry~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2],
  summary(lm(enumfemale~treatment, data=m.data2[m.data2$fightrev<3,], weights=weights))$coef[2,1:2]))
colnames(balance) <- c("mean", "se")
balance$name <- c("Age/10", "Female (0-1)", "Ownership (1-3)", "Income (1-10)",
                  "Education (1-6)", "Urban (0-1)", "Unemployed (0-1)", 
                  "Married (0-1)", "Female Enum. (0-1)")

ggplot(balance,
       aes(x=balance$mean, y=balance$name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=balance$mean-1.96*balance$se, 
                     xmax=balance$mean+1.96*balance$se, height=0.5), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Poor & Not Reliant (Matched)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Lose - Win") +
  ylab("") +
  scale_x_continuous(limits=c(-2, 2),breaks=seq(-2, 2, by=0.5)) +
  theme(text = element_text(size=17))


## Table S7: Effect of Losing (v. Winning) the Coordination Game among the poor
stargazer(lm(trusten~treatment, data=m.data2, weight=weights), 
          lm(fightrev~treatment, data=m.data2, weight=weights),
          lm(trusten~treatment, data=m.data2[m.data2$fightrev>2,], weight=weights))


## Table S8: Multiple Comparisons Corrections, Experiment 1
p <- rbind(summary(lm(trusten~treatment, data=m.data2, weight=weights))$coef[2,4],
           summary(lm(fightrev~treatment, data=m.data2, weight=weights))$coef[2,4],
           summary(lm(trusten~treatment, data=m.data2[m.data2$fightrev>2,], weight=weights))$coef[2,4])

p.adjust(p, method="BH", n=length(p))
p.adjust(p, method="holm", n=length(p))
p.adjust(p, method="bonferroni", n=length(p))



## Fig. S12. Covariate Balance, Experiment 2 (Unmatched). Hard v. easy scenarios in the 
## no Quran condition (a); hard v. easy in the Quran condition (b); no Quran v. Quran (c).

## No Quran (a)
balance <- as.data.frame(rbind(
  summary(lm((age/10)~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(female~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(own~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(inc~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(edu~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(urb~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(unemp~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(marriage~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(enumfemale~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(stud~treat, data=data[data$quran==0,]))$coef[2,1:2]
))
balance$name <- c("Age/10", "Female (0-1)", "Ownership (0-2)", "Income (1-10)",
                  "Education (1-6)", "Urban (0-1)", "Unemployment (0-1)", 
                  "Never Married (0-1)", "Female Enum. (0-1)", "Student (0-1)")
colnames(balance) <- c("mean", "se", "name")

ggplot(balance,
       aes(x=balance$mean, y=balance$name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=balance$mean-1.96*balance$se, 
                     xmax=balance$mean+1.96*balance$se, height=0.5), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Balance Plot (No Quran)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hard - Easy") +
  ylab("") +
  scale_x_continuous(limits=c(-.8, .8),breaks=seq(-.8, .8, by=0.4)) +
  theme(text = element_text(size=17))


## Quran condition (b)
balance <- as.data.frame(rbind(
  summary(lm((age/10)~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(female~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(own~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(inc~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(edu~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(urb~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(unemp~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(marriage~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(enumfemale~treat, data=data[data$quran==1,]))$coef[2,1:2],
  summary(lm(stud~treat, data=data[data$quran==1,]))$coef[2,1:2]
))
balance$name <- c("Age/10", "Female (0-1)", "Ownership (0-2)", "Income (1-10)",
                  "Education (1-6)", "Urban (0-1)", "Unemployment (0-1)", 
                  "Never Married (0-1)", "Female Enum. (0-1)", "Student (0-1)")
colnames(balance) <- c("mean", "se", "name")

ggplot(balance,
       aes(x=balance$mean, y=balance$name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=balance$mean-1.96*balance$se, 
                     xmax=balance$mean+1.96*balance$se, height=0.5), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Balance Plot (Quran)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Hard - Easy") +
  ylab("") +
  scale_x_continuous(limits=c(-1, 1),breaks=seq(-1, 1, by=0.5)) +
  theme(text = element_text(size=17))


## no Quran v. Quran (c)
balance <- as.data.frame(rbind(
  summary(lm((age/10)~quran, data=data))$coef[2,1:2],
  summary(lm(female~quran, data=data))$coef[2,1:2],
  summary(lm(own~quran, data=data))$coef[2,1:2],
  summary(lm(inc~quran, data=data))$coef[2,1:2],
  summary(lm(edu~quran, data=data))$coef[2,1:2],
  summary(lm(urb~quran, data=data))$coef[2,1:2],
  summary(lm(unemp~quran, data=data))$coef[2,1:2],
  summary(lm(marriage~quran, data=data))$coef[2,1:2],
  summary(lm(enumfemale~quran, data=data))$coef[2,1:2],
  summary(lm(stud~quran, data=data))$coef[2,1:2]
))
balance$name <- c("Age/10", "Female (0-1)", "Ownership (0-2)", "Income (1-10)",
                  "Education (1-6)", "Urban (0-1)", "Unemployment (0-1)", 
                  "Never Married (0-1)", "Female Enum. (0-1)", "Student (0-1)")
colnames(balance) <- c("mean", "se", "name")

ggplot(balance,
       aes(x=balance$mean, y=balance$name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=balance$mean-1.96*balance$se, 
                     xmax=balance$mean+1.96*balance$se, height=0.5), size=0.2) +
  geom_vline(aes(xintercept=0)) +
  ggtitle("Balance Plot (Unmatched)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Quran - No Quran") +
  ylab("") +
  scale_x_continuous(limits=c(-.6, .6),breaks=seq(-.6, .6, by=0.3)) +
  theme(text = element_text(size=17))



## Table S11: Effect of Hard (v. Easy) Scenarios among the poor, no Quran (OLS)
stargazer(lm(votenahda~treat, data=data[data$inc<7 & data$quran==0,]), 
          lm(relrev~treat, data=data[data$inc<7 & data$quran==0,]),
          lm(stress~treat, data=data[data$inc<7 & data$quran==0 & data$relrev<3,]))


## Figure S13: Effect of the Hard Scenarios on Self-Reported Stress (Experiment 2)
data.summary <- as.data.frame(rbind(
  summary(lm(stress~treat, data=data[data$quran==0 & data$inc<7 & data$relrev<5,]))$coef[2,1:2],
  summary(lm(stress~treat, data=data[data$quran==0 & data$inc<7 & data$relrev==5,]))$coef[2,1:2],
  summary(lm(stress~treat, data=data[data$quran==1 & data$inc<7,]))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(data$stress[data$treat==1 & data$quran==0 & data$inc<7 & data$relrev<5]),
                    length(data$stress[data$treat==1 & data$quran==0 & data$inc<7 & data$relrev==5]),
                    length(data$stress[data$treat==1 & data$quran==1 & data$inc<7]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Poor,\nNo Quran\nNot Reliant\n(N=51)", "Poor,\nNo Quran\nReliant\n(N=52)", "Poor,\nQuran\n(N=96)")
data.summary$name <- factor(data.summary$name, levels=c("Poor,\nQuran\n(N=96)", "Poor,\nNo Quran\nReliant\n(N=52)", "Poor,\nNo Quran\nNot Reliant\n(N=51)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  ggtitle("Level of Stress (1-4)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.8, 1.2),breaks=seq(-0.8, 1.2, by=0.4)) +
  theme(text = element_text(size=17))


## Figure S14: Effect of Reliance on Divine Rewards on Voting for Ennahda (Experiment 2)
data.summary <- as.data.frame(rbind(
  summary(lm(votenahda~relrev, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(votenahda~relrev, data=data[data$quran==0 & data$inc<7,]))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(data$votenahda[data$relrev>1 & data$quran==0]),
                    length(data$votenahda[data$relrev>1 & data$quran==0 & data$inc<7]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Full Sample\n(N=201)", "Poor\n(N=105)")
data.summary$name <- factor(data.summary$name, levels=c("Poor\n(N=105)","Full Sample\n(N=201)"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  geom_hline(aes(yintercept=1.5), linetype=2) +
  ggtitle("Likelihood of Voting for Ennahda") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Effect of Divine Rewards (1-5)") +
  ylab("") +
  scale_x_continuous(limits=c(-0.02, 0.1),breaks=seq(-0.02, 0.1, by=0.02)) +
  theme(text = element_text(size=17))


## Figure S15: Null Effects for Redistributive Explanations (Experiment 2)
## care for the poor (a)
data.summary <- as.data.frame(rbind(
  summary(lm(votepoor~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(votepoor~votenahda, data=data[data$quran==0,]))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(data$votepoor[data$treat==1 & data$quran==0]),
                    length(data$votepoor[data$votenahda==1 & data$quran==0]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Treatment", "Voting Nahda")
data.summary$name <- factor(data.summary$name, levels=c("Voting Nahda","Treatment"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  geom_hline(aes(yintercept=1.5), linetype=2) +
  ggtitle("My party will care for the poor") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.6, 0.6),breaks=seq(-0.6, 0.6, by=0.6)) +
  theme(text = element_text(size=17))

## Provide goods (b)
data.summary <- as.data.frame(rbind(
  summary(lm(votegoods~treat, data=data[data$quran==0,]))$coef[2,1:2],
  summary(lm(votegoods~votenahda, data=data[data$quran==0,]))$coef[2,1:2]))
colnames(data.summary) <- c("mean", "sem")
data.summary$n <- c(length(data$votepoor[data$treat==1 & data$quran==0]),
                    length(data$votepoor[data$votenahda==1 & data$quran==0]))
data.summary$me <- qt(1-.05/2, df=data.summary$n)*data.summary$se
data.summary$me90 <- qt(1-.1/2, df=data.summary$n)*data.summary$se
data.summary$name <- c("Treatment", "Voting Nahda")
data.summary$name <- factor(data.summary$name, levels=c("Voting Nahda","Treatment"))

ggplot(data.summary,
       aes(x=mean, y=name)) +
  geom_point(stat="identity", position="identity") +
  geom_errorbarh(aes(xmin=mean-me, xmax=mean+me, height=0.1), size=0.2) +
  geom_errorbarh(aes(xmin=mean-me90, xmax=mean+me90, height=0), size=1) + 
  geom_vline(aes(xintercept=0)) +
  geom_hline(aes(yintercept=1.5), linetype=2) +
  ggtitle("My party will provide goods & services") +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Treatment Effect") +
  ylab("") +
  scale_x_continuous(limits=c(-0.7, 0.7),breaks=seq(-0.7, 0.7, by=0.7)) +
  theme(text = element_text(size=17))




## Fig. S17. Parallel Design Causal Mediation Analysis accounting for alternative 
## religious mechanisms.
summary(multimed(outcome="votenahda", med.main="relrev", med.alt=c("sharia","pray"), 
                 treat="treat", experiment="quran", data=data[data$inc<7,], 
                 design="parallel", sims=100, R2.by=0.01, conf.level=0.95))


## Table S12: Multiple Comparisons Corrections, Experiment 2

p <- rbind(summary(lm(votenahda~treat, data=data[data$inc<7 & data$quran==0,]))$coef[2,4],
           summary(lm(relrev~treat, data=data[data$inc<7 & data$quran==0,]))$coef[2,4],
           summary(lm(stress~treat, data=data[data$inc<7 & data$quran==0 & data$relrev<3,]))$coef[2,4],
           exp((-0.717*2.123102)-(0.416*2.123102^2)))

p.adjust(p, method="BH", n=length(p))
p.adjust(p, method="holm", n=length(p))
p.adjust(p, method="bonferroni", n=length(p))



## Table S13: Voting to Please Allah (among poor)
one <- lm(voteAllah3~votenahda+votejabha+voteirada, data=data[data$inc<7 & data$votenahda==1 |
           data$inc<7 & data$votenidaa==1 | data$inc<7 & data$votejabha==1 | 
           data$inc<7 & data$voteirada==1,])

two <- selection(voteAllah2~votenahda+votejabha+treat+quran, 
                  voteAllahrank3~votenahda+votenidaa, 
                  data=data[data$inc<7 & data$votenahda==1 |
                            data$inc<7 & data$votenidaa==1 | 
                            data$inc<7 & data$votejabha==1 | 
                            data$inc<7 & data$voteirada==1,], method="2step")
summary(two)
stargazer(one, two) # manually enter first stage


