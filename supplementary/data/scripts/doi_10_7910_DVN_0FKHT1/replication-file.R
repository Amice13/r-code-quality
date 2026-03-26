#Replication file for Anson, Ian G. and Forestiere, Carolyn,
#Sleeping Giant or Herd of Cats? An Experimental Investigation
#of Nonreligious Americans’ Responsiveness to Issue- and
#Group-Based Political Cues. Public Opinion Quarterly. 2025.


#required packages
require(tidyverse)
require(ggthemes)
require(car)
require(stargazer)

#readin
dat <- read.csv("data.csv")


#modeling (corresponds to Fig. 1 in main text)

mod1 <- lm(data=dat, Q53~as.factor(treatment))
mod2 <- lm(data=dat, Q53_1~as.factor(treatment))
mod3 <- lm(data=dat, Q53_2~as.factor(treatment))
mod4 <- lm(data=dat, Q53_3~as.factor(treatment))
mod5 <- lm(data=dat, Q54~as.factor(treatment))
mod6 <- lm(data=dat, Q54_1~as.factor(treatment))
mod7 <- lm(data=dat, Q54_2~as.factor(treatment))
mod8 <- lm(data=dat, Q54_3~as.factor(treatment))

#main graphics: FIG 1. Treatment Effects, Full Sample

modelLabels <- c(rep(c("Issue Cue (T2)",
                       "Issue + Identity Cue (T3)"),8))

modelCoefs <- c(mod1$coefficients[2],
                mod1$coefficients[3],
                mod2$coefficients[2],
                mod2$coefficients[3],
                mod3$coefficients[2],
                mod3$coefficients[3],
                mod4$coefficients[2],
                mod4$coefficients[3],
                mod5$coefficients[2],
                mod5$coefficients[3],
                mod6$coefficients[2],
                mod6$coefficients[3],
                mod7$coefficients[2],
                mod7$coefficients[3],
                mod8$coefficients[2],
                mod8$coefficients[3])

modelLow95 <- c(confint(mod1,level=0.95)[2,1],
                confint(mod1,level=0.95)[3,1],
                confint(mod2,level=0.95)[2,1],
                confint(mod2,level=0.95)[3,1],
                confint(mod3,level=0.95)[2,1],
                confint(mod3,level=0.95)[3,1],
                confint(mod4,level=0.95)[2,1],
                confint(mod4,level=0.95)[3,1],
                confint(mod5,level=0.95)[2,1],
                confint(mod5,level=0.95)[3,1],
                confint(mod6,level=0.95)[2,1],
                confint(mod6,level=0.95)[3,1],
                confint(mod7,level=0.95)[2,1],
                confint(mod7,level=0.95)[3,1],
                confint(mod8,level=0.95)[2,1],
                confint(mod8,level=0.95)[3,1]
)

modelLow90 <- c(confint(mod1,level=0.90)[2,1],
                confint(mod1,level=0.90)[3,1],
                confint(mod2,level=0.90)[2,1],
                confint(mod2,level=0.90)[3,1],
                confint(mod3,level=0.90)[2,1],
                confint(mod3,level=0.90)[3,1],
                confint(mod4,level=0.90)[2,1],
                confint(mod4,level=0.90)[3,1],
                confint(mod5,level=0.90)[2,1],
                confint(mod5,level=0.90)[3,1],
                confint(mod6,level=0.90)[2,1],
                confint(mod6,level=0.90)[3,1],
                confint(mod7,level=0.90)[2,1],
                confint(mod7,level=0.90)[3,1],
                confint(mod8,level=0.90)[2,1],
                confint(mod8,level=0.90)[3,1]
)


modelHi90 <-  c(confint(mod1,level=0.90)[2,2],
                confint(mod1,level=0.90)[3,2],
                confint(mod2,level=0.90)[2,2],
                confint(mod2,level=0.90)[3,2],
                confint(mod3,level=0.90)[2,2],
                confint(mod3,level=0.90)[3,2],
                confint(mod4,level=0.90)[2,2],
                confint(mod4,level=0.90)[3,2],
                confint(mod5,level=0.90)[2,2],
                confint(mod5,level=0.90)[3,2],
                confint(mod6,level=0.90)[2,2],
                confint(mod6,level=0.90)[3,2],
                confint(mod7,level=0.90)[2,2],
                confint(mod7,level=0.90)[3,2],
                confint(mod8,level=0.90)[2,2],
                confint(mod8,level=0.90)[3,2]
)

modelHi95 <-  c(confint(mod1,level=0.95)[2,2],
                confint(mod1,level=0.95)[3,2],
                confint(mod2,level=0.95)[2,2],
                confint(mod2,level=0.95)[3,2],
                confint(mod3,level=0.95)[2,2],
                confint(mod3,level=0.95)[3,2],
                confint(mod4,level=0.95)[2,2],
                confint(mod4,level=0.95)[3,2],
                confint(mod5,level=0.95)[2,2],
                confint(mod5,level=0.95)[3,2],
                confint(mod6,level=0.95)[2,2],
                confint(mod6,level=0.95)[3,2],
                confint(mod7,level=0.95)[2,2],
                confint(mod7,level=0.95)[3,2],
                confint(mod8,level=0.95)[2,2],
                confint(mod8,level=0.95)[3,2]
)

grafMat <- data.frame(modelLabels,modelCoefs,modelHi90,
                      modelHi95,modelLow90,modelLow95)

grafMat$row <- as.factor(c("Additive Index: Non Religious Identity",
                           "Additive Index: Non Religious Identity",
                           "Group Membership",
                           "Group Membership",
                           "Group Consciousness",
                           "Group Consciousness",
                           "Linked Fate",
                           "Linked Fate",
                           "Additive Index: Political Engagement",
                           "Additive Index: Political Engagement",
                           "Contact State Reps.",
                           "Contact State Reps.",
                           "Encourage Peers to Vote",
                           "Encourage Peers to Vote",
                           "Vote in Next Election",
                           "Vote in Next Election"))

grafMat$row <- fct_relevel(grafMat$row, "Additive Index: Non Religious Identity",
                           "Group Membership",
                           "Group Consciousness",
                           "Linked Fate",
                           "Additive Index: Political Engagement",
                           "Contact State Reps.",
                           "Encourage Peers to Vote",
                           "Vote in Next Election")

grafMat$Item <- c(rep("Group Consciousness Item",8),
                  rep("Political Engagement Item",8))
# 
# grafMat$pid <- c(rep("All Respondents",3),
#                  rep("Republicans Only",3),
#                  rep("Democrats Only",3))




graf1 <- grafMat %>%
  ggplot() +
  geom_point(aes(y=modelLabels,x=modelCoefs,shape = Item),size=2)+
  geom_errorbarh(aes(y=modelLabels,xmax=modelHi90,xmin=modelLow90),height=0.0,size=1.1)+
  geom_errorbarh(aes(y=modelLabels,xmax=modelHi95,xmin=modelLow95),height=0.1)+
  geom_vline(aes(xintercept=0),linetype=2)+
  #ggtitle("Experimental Treatments vs. Control, \n Six DV Items and their Additive Indices")+
  xlab("Outcome (0:1)")+
  ylab("")+
  #scale_x_continuous(limits=c(0,0.28),breaks=c(0,0.1,0.2))+
  theme_few()+
  theme(panel.background = element_rect(fill='grey95'))+
  theme(text=element_text(size=9))+
  theme(plot.title = element_text(hjust = 0.5,size=10))+
  xlim(-0.11,0.11)+
  #scale_color_manual(values = c("black","#0072B2","#FF3333"))+
  #theme(legend.position="none")+
  facet_wrap(~row,ncol=1)

windows(8,6)
graf1




####Appendix matter

#Table A.1 demographics
# Age 

dat2 <- dat %>%
  select(age,male,nonwhite,inc75,college)

stargazer(data.frame(dat2), type="html", out="tablea1.html",
          summary=T)


#Table A.2 main text model results

stargazer(mod1,mod2,mod3,mod4, type="html",out="figa2.html")

stargazer(mod5,mod6,mod7,mod8,type="html",out="figa3.html")

#Tables A.4 and A.5, CACE effects models

mod1 <- lm(data=dat[dat$cace==1,], Q53~as.factor(treatment))
mod2 <- lm(data=dat[dat$cace==1,], Q53_1~as.factor(treatment))
mod3 <- lm(data=dat[dat$cace==1,], Q53_2~as.factor(treatment))
mod4 <- lm(data=dat[dat$cace==1,], Q53_3~as.factor(treatment))
mod5 <- lm(data=dat[dat$cace==1,], Q54~as.factor(treatment))
mod6 <- lm(data=dat[dat$cace==1,], Q54_1~as.factor(treatment))
mod7 <- lm(data=dat[dat$cace==1,], Q54_2~as.factor(treatment))
mod8 <- lm(data=dat[dat$cace==1,], Q54_3~as.factor(treatment))


stargazer(mod1,mod2,mod3,mod4, type="html",out="figa4.html")

stargazer(mod5,mod6,mod7,mod8,type="html",out="figa5.html")


#Table A6. Manipulation checks

mod1 <- glm(data=dat,mcT1~as.factor(treatment),
            family=binomial(link="logit"))
mod2 <- glm(data=dat,mcT2b~as.factor(treatment),
            family=binomial(link="logit"))

stargazer(mod1,mod2, out="tablea6.html", type='html')


#factor analyzing the outcome measures

fa1 <- dat %>%
  select(Q53_1, Q53_2, Q53_3) %>%
  na.omit() %>%
  prcomp(scale = TRUE)

fav1 = fa1$sdev^2 / sum(fa1$sdev^2)


qplot(c(1:3), fav1) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Fig. A.1. Scree Plot, Group Consciousness Measures") +
  ylim(0, 1) +
  theme_few()

fa2 <- dat %>%
  select(Q54_1, Q54_2, Q54_3) %>%
  na.omit() %>%
  prcomp(scale = TRUE)

fav2 = fa2$sdev^2 / sum(fa2$sdev^2)


qplot(c(1:3), fav2) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Fig. A.2. Scree Plot, Civic Engagement Measures") +
  ylim(0, 1) +
  theme_few()


#cor matrix

cormat <- dat %>% select(Q53,Q53_1,Q53_2,Q53_3,
                         Q54,Q54_1,Q54_2,Q54_3) %>%
  na.omit() %>%
  cor()

stargazer(cormat,out="cormat.html",type="html",
          summary = F)


#subsamples

mod1 <- lm(data=dat[dat$dem==1,], Q53~as.factor(treatment))
mod2 <- lm(data=dat[dat$dem==1,], Q53_1~as.factor(treatment))
mod3 <- lm(data=dat[dat$dem==1,], Q53_2~as.factor(treatment))
mod4 <- lm(data=dat[dat$dem==1,], Q53_3~as.factor(treatment))
mod5 <- lm(data=dat[dat$dem==1,], Q54~as.factor(treatment))
mod6 <- lm(data=dat[dat$dem==1,], Q54_1~as.factor(treatment))
mod7 <- lm(data=dat[dat$dem==1,], Q54_2~as.factor(treatment))
mod8 <- lm(data=dat[dat$dem==1,], Q54_3~as.factor(treatment))

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,
          out="tablea8.html",type="html")

#interest in education

mod1 <- lm(data=dat[dat$eduint==1,], Q53~as.factor(treatment))
mod2 <- lm(data=dat[dat$eduint==1,], Q53_1~as.factor(treatment))
mod3 <- lm(data=dat[dat$eduint==1,], Q53_2~as.factor(treatment))
mod4 <- lm(data=dat[dat$eduint==1,], Q53_3~as.factor(treatment))
mod5 <- lm(data=dat[dat$eduint==1,], Q54~as.factor(treatment))
mod6 <- lm(data=dat[dat$eduint==1,], Q54_1~as.factor(treatment))
mod7 <- lm(data=dat[dat$eduint==1,], Q54_2~as.factor(treatment))
mod8 <- lm(data=dat[dat$eduint==1,], Q54_3~as.factor(treatment))

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,
          out="tablea9.html",type="html")


#atheist

mod1 <- lm(data=dat[dat$ath==1,], Q53~as.factor(treatment))
mod2 <- lm(data=dat[dat$ath==1,], Q53_1~as.factor(treatment))
mod3 <- lm(data=dat[dat$ath==1,], Q53_2~as.factor(treatment))
mod4 <- lm(data=dat[dat$ath==1,], Q53_3~as.factor(treatment))
mod5 <- lm(data=dat[dat$ath==1,], Q54~as.factor(treatment))
mod6 <- lm(data=dat[dat$ath==1,], Q54_1~as.factor(treatment))
mod7 <- lm(data=dat[dat$ath==1,], Q54_2~as.factor(treatment))
mod8 <- lm(data=dat[dat$ath==1,], Q54_3~as.factor(treatment))

stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,
          out="tablea10.html",type="html")


#Atheist only figure

#modeling

mod1 <- lm(data=dat[dat$ath==1,], Q53~as.factor(treatment))
mod2 <- lm(data=dat[dat$ath==1,], Q53_1~as.factor(treatment))
mod3 <- lm(data=dat[dat$ath==1,], Q53_2~as.factor(treatment))
mod4 <- lm(data=dat[dat$ath==1,], Q53_3~as.factor(treatment))
mod5 <- lm(data=dat[dat$ath==1,], Q54~as.factor(treatment))
mod6 <- lm(data=dat[dat$ath==1,], Q54_1~as.factor(treatment))
mod7 <- lm(data=dat[dat$ath==1,], Q54_2~as.factor(treatment))
mod8 <- lm(data=dat[dat$ath==1,], Q54_3~as.factor(treatment))

#main graphics

#Fig. 2: treatment effects

modelLabels <- c(rep(c("Issue Cue (T2)",
                       "Issue + Identity Cue (T3)"),8))

modelCoefs <- c(mod1$coefficients[2],
                mod1$coefficients[3],
                mod2$coefficients[2],
                mod2$coefficients[3],
                mod3$coefficients[2],
                mod3$coefficients[3],
                mod4$coefficients[2],
                mod4$coefficients[3],
                mod5$coefficients[2],
                mod5$coefficients[3],
                mod6$coefficients[2],
                mod6$coefficients[3],
                mod7$coefficients[2],
                mod7$coefficients[3],
                mod8$coefficients[2],
                mod8$coefficients[3])

modelLow95 <- c(confint(mod1,level=0.95)[2,1],
                confint(mod1,level=0.95)[3,1],
                confint(mod2,level=0.95)[2,1],
                confint(mod2,level=0.95)[3,1],
                confint(mod3,level=0.95)[2,1],
                confint(mod3,level=0.95)[3,1],
                confint(mod4,level=0.95)[2,1],
                confint(mod4,level=0.95)[3,1],
                confint(mod5,level=0.95)[2,1],
                confint(mod5,level=0.95)[3,1],
                confint(mod6,level=0.95)[2,1],
                confint(mod6,level=0.95)[3,1],
                confint(mod7,level=0.95)[2,1],
                confint(mod7,level=0.95)[3,1],
                confint(mod8,level=0.95)[2,1],
                confint(mod8,level=0.95)[3,1]
)

modelLow90 <- c(confint(mod1,level=0.90)[2,1],
                confint(mod1,level=0.90)[3,1],
                confint(mod2,level=0.90)[2,1],
                confint(mod2,level=0.90)[3,1],
                confint(mod3,level=0.90)[2,1],
                confint(mod3,level=0.90)[3,1],
                confint(mod4,level=0.90)[2,1],
                confint(mod4,level=0.90)[3,1],
                confint(mod5,level=0.90)[2,1],
                confint(mod5,level=0.90)[3,1],
                confint(mod6,level=0.90)[2,1],
                confint(mod6,level=0.90)[3,1],
                confint(mod7,level=0.90)[2,1],
                confint(mod7,level=0.90)[3,1],
                confint(mod8,level=0.90)[2,1],
                confint(mod8,level=0.90)[3,1]
)


modelHi90 <-  c(confint(mod1,level=0.90)[2,2],
                confint(mod1,level=0.90)[3,2],
                confint(mod2,level=0.90)[2,2],
                confint(mod2,level=0.90)[3,2],
                confint(mod3,level=0.90)[2,2],
                confint(mod3,level=0.90)[3,2],
                confint(mod4,level=0.90)[2,2],
                confint(mod4,level=0.90)[3,2],
                confint(mod5,level=0.90)[2,2],
                confint(mod5,level=0.90)[3,2],
                confint(mod6,level=0.90)[2,2],
                confint(mod6,level=0.90)[3,2],
                confint(mod7,level=0.90)[2,2],
                confint(mod7,level=0.90)[3,2],
                confint(mod8,level=0.90)[2,2],
                confint(mod8,level=0.90)[3,2]
)

modelHi95 <-  c(confint(mod1,level=0.95)[2,2],
                confint(mod1,level=0.95)[3,2],
                confint(mod2,level=0.95)[2,2],
                confint(mod2,level=0.95)[3,2],
                confint(mod3,level=0.95)[2,2],
                confint(mod3,level=0.95)[3,2],
                confint(mod4,level=0.95)[2,2],
                confint(mod4,level=0.95)[3,2],
                confint(mod5,level=0.95)[2,2],
                confint(mod5,level=0.95)[3,2],
                confint(mod6,level=0.95)[2,2],
                confint(mod6,level=0.95)[3,2],
                confint(mod7,level=0.95)[2,2],
                confint(mod7,level=0.95)[3,2],
                confint(mod8,level=0.95)[2,2],
                confint(mod8,level=0.95)[3,2]
)

grafMat <- data.frame(modelLabels,modelCoefs,modelHi90,
                      modelHi95,modelLow90,modelLow95)

grafMat$row <- as.factor(c("Additive Index: Non Religious Identity",
                           "Additive Index: Non Religious Identity",
                           "Group Membership",
                           "Group Membership",
                           "Group Consciousness",
                           "Group Consciousness",
                           "Linked Fate",
                           "Linked Fate",
                           "Additive Index: Political Engagement",
                           "Additive Index: Political Engagement",
                           "Contact State Reps.",
                           "Contact State Reps.",
                           "Encourage Peers to Vote",
                           "Encourage Peers to Vote",
                           "Vote in Next Election",
                           "Vote in Next Election"))

grafMat$row <- fct_relevel(grafMat$row, "Additive Index: Non Religious Identity",
                           "Group Membership",
                           "Group Consciousness",
                           "Linked Fate",
                           "Additive Index: Political Engagement",
                           "Contact State Reps.",
                           "Encourage Peers to Vote",
                           "Vote in Next Election")

grafMat$Item <- c(rep("Group Consciousness Item",8),
                  rep("Political Engagement Item",8))
# 
# grafMat$pid <- c(rep("All Respondents",3),
#                  rep("Republicans Only",3),
#                  rep("Democrats Only",3))




graf1 <- grafMat %>%
  ggplot() +
  geom_point(aes(y=modelLabels,x=modelCoefs,shape = Item),size=2)+
  geom_errorbarh(aes(y=modelLabels,xmax=modelHi90,xmin=modelLow90),height=0.0,size=1.1)+
  geom_errorbarh(aes(y=modelLabels,xmax=modelHi95,xmin=modelLow95),height=0.1)+
  geom_vline(aes(xintercept=0),linetype=2)+
  #ggtitle("Experimental Treatments vs. Control, \n Six DV Items and their Additive Indices")+
  xlab("Outcome (0:1)")+
  ylab("")+
  #scale_x_continuous(limits=c(0,0.28),breaks=c(0,0.1,0.2))+
  theme_few()+
  theme(panel.background = element_rect(fill='grey95'))+
  theme(text=element_text(size=9))+
  theme(plot.title = element_text(hjust = 0.5,size=10))+
  xlim(-0.2,0.2)+
  #scale_color_manual(values = c("black","#0072B2","#FF3333"))+
  #theme(legend.position="none")+
  facet_wrap(~row,ncol=1)

windows(8,6)
graf1


