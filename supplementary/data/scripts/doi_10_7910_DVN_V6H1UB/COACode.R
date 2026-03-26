###################################
####### Replication Code for
####### "The Curious Case of the Migrating Seat:
####### Understanding the Role of Senate Norms in Confirming
####### Judges on the U.S. Courts of Appeals"
#######
####### Boston, Rickert, Spriggs, Waterbury
###################################

#setwd()
library(lme4)
library(survival)
library(coxme)
library(eha)
library(stargazer)
library(gridExtra)
library(ggplot2)
library(survminer)
library(mfx)

### Read in Full Dataset
data<-read.csv('AllCOANom.csv')
#Remove Reassigned Judges
data.minus.RA<-data[which(data$Reassigned != 1),]
#Data without Hoover (No NOMINATE Score)
Main.Text.Data<-data.minus.RA[which(data.minus.RA$PresNo != 31),]

######################################################################################
######################################################################################
### MAIN TEXT
######################################################################################
######################################################################################

#### Figure 1A
#See 'decadeData.csv' in supplemental materials

#### Figure 1B
plot(data.minus.RA$Congress, data.minus.RA$NomToFinalAction, 
     xlab = 'Congress', ylab = 'Days to Final Action')


#### Figure 2
#See 'decadeData.csv' in supplemental materials


#### Table 1
logit.mod<-glm(Successful~HomeStateChange*PrevStateDistance+
             AnyHSOpp+
             PolarizationMea+
             DividedGovernment+
             BalancedBench+
             ElectionYear,
            data = Main.Text.Data,
            family = binomial(link='logit'))
summary(logit.mod)
logit.or <- exp(coef(logit.mod))
logit.or

#### Figure 3A
# Create dataset
HS.pred.probs<-data.frame(AnyHSOpp<-c(1, 0),
                          PolarizationMea<-rep(mean(Main.Text.Data$PolarizationMea),2),
                          DividedGovernment<-rep(mean(Main.Text.Data$DividedGovernment),2),
                          BalancedBench<-rep(mean(Main.Text.Data$BalancedBench),2),
                          ElectionYear<-rep(mean(Main.Text.Data$ElectionYear),2),
                          HomeStateChange<-rep(mean(Main.Text.Data$HomeStateChange),2),
                          PrevStateDistance<-rep(mean(Main.Text.Data$PrevStateDistance),2))
HS.pred.probs2 <- cbind(HS.pred.probs,predict(logit.mod, newdata=HS.pred.probs, type="response", se.fit=TRUE))
# Renaming "fit" and "se.fit" columns
names(HS.pred.probs2)[names(HS.pred.probs2)=="fit"] = "prob"
names(HS.pred.probs2)[names(HS.pred.probs2)=="se.fit"] = "se.prob"
# Estimating confidence intervals
HS.pred.probs2$ll = HS.pred.probs2$prob - 1.96*HS.pred.probs2$se.prob
HS.pred.probs2$ul = HS.pred.probs2$prob + 1.96*HS.pred.probs2$se.prob
HS.pred.probs2 <- cbind(HS.pred.probs2,AnyHSOppCat<-c("Opposition", "Support"))
#Plot
ggplot(HS.pred.probs2, aes(x=AnyHSOppCat, y = prob)) +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0.07, lty=1, lwd=0.7, col="black") +
  geom_point(shape=16, size=3, fill="black") +
  geom_point(x=2, y=0.3918159,shape=16, size=3, fill="black")+
  geom_errorbar(aes(x=2,ymin=0.2644,ymax=0.519218), width = 0.07, lty=1, lwd=0.7, col="black")+
  theme_bw()+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_discrete(limits= c("Support", "Oppose"))+
  labs( x="\nHome State Senator Position", y="Probability of Confirmation\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#### Figure 3b
# Create dataset
InteractionSequence<-rep(seq(from=0, to=1.1045, by=0.07363333),2)
InteractionRep<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
interaction.pred.probs<-data.frame(PrevStateDistance<-InteractionSequence,
                                                      HomeStateChange<-InteractionRep,
                                                      AnyHSOpp<-rep(mean(Main.Text.Data$AnyHSOpp),32),
                                                      PolarizationMea<-rep(mean(Main.Text.Data$PolarizationMea),32),
                                                      DividedGovernment<-rep(mean(Main.Text.Data$DividedGovernment),32),
                                                      BalancedBench<-rep(mean(Main.Text.Data$BalancedBench),32),
                                                      ElectionYear<-rep(mean(Main.Text.Data$ElectionYear),32))
                                                     
interaction.pred.probs <- cbind(interaction.pred.probs,predict(logit.mod, newdata=interaction.pred.probs, type="response", se.fit=TRUE))
write.csv(interaction.pred.probs, "interactionPredProbs.csv")

#Re-format dataframe in .csv
pred.prob.data<-read.csv('interactionPredProbs.csv')

# Estimating confidence intervals
pred.prob.data$ll1 = pred.prob.data$Fit.0 - 1.96*pred.prob.data$se.fit.0
pred.prob.data$ul1 = pred.prob.data$Fit.0 + 1.96*pred.prob.data$se.fit.0
pred.prob.data$ll2 = pred.prob.data$Fit.1 - 1.96*pred.prob.data$se.fit.1
pred.prob.data$ul2 = pred.prob.data$Fit.1 + 1.96*pred.prob.data$se.fit.1

#Plot
ggplot(pred.prob.data, aes(x=pred.prob.data$PrevStateDistance....InteractionSequence, ymin=0.2, ymax=1, xmin=0, xmax=1.1045)) +
  geom_point(aes(y=Fit.0,shape="Same H.S.")) +
  geom_line(aes(y=Fit.0), linetype='dashed') +
  geom_ribbon(aes(ymin=ll1, ymax=ul1), linetype=2, alpha=0.2) +
  geom_point(aes(y=Fit.1,shape="Change in H.S.")) +
  geom_line(aes(y=Fit.1)) +
  geom_ribbon(aes(ymin=ll2, ymax=ul2), linetype=2, alpha=0.1) +
  scale_shape_manual(name="",values = c('Same H.S.' = 2, 'Change in H.S.' = 16))+
  theme_bw() +
  theme(panel.background = element_rect(color = "black"), axis.line = element_line(color = "black")) +
  labs(x="\nDistance Between Pres. and a Seat's Previous H.S. Senator", 
       y="Probability of Confirmation\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10), legend.position="right")

#### Table 2
cox.mod<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                 AnyHSOpp+
                 PolarizationMea+
                 DividedGovernment+
                 BalancedBench+
                 ElectionYear, 
               cluster = VacancyID,
               data = Main.Text.Data)
summary(cox.mod)


######################################################################################
######################################################################################
### APPENDIX
######################################################################################
######################################################################################
#Data Post 1946 subset - Must be used if "Well Qualified" is in the model
data.post1946<-Main.Text.Data[which(Main.Text.Data$Year>1946),]
#Data Post Bork
data.postBork<-Main.Text.Data[which(Main.Text.Data$PostBork == 1),]
#Data Post Filibuster Reform
data.post60<-Main.Text.Data[which(Main.Text.Data$Year > 1975),]


#### Table A1
summary(Main.Text.Data$Successful)
sd(Main.Text.Data$Successful)
summary(Main.Text.Data$NomToFinalAction)
sd(Main.Text.Data$NomToFinalAction)
summary(Main.Text.Data$HomeStateChange)
sd(Main.Text.Data$HomeStateChange)
summary(Main.Text.Data$PrevStateDistance)
sd(Main.Text.Data$PrevStateDistance)
summary(Main.Text.Data$AnyHSOpp)
sd(Main.Text.Data$AnyHSOpp)
summary(Main.Text.Data$PolarizationMea)
sd(Main.Text.Data$PolarizationMea)
summary(Main.Text.Data$DividedGovernment)
sd(Main.Text.Data$DividedGovernment)
summary(Main.Text.Data$BalancedBench)
sd(Main.Text.Data$BalancedBench)
summary(Main.Text.Data$ElectionYear)
sd(Main.Text.Data$ElectionYear)

#### Table A2
table(data.minus.RA$PresPartyCat, data.minus.RA$HomeStateChange)
table(data.minus.RA$PresPartyCat, data.minus.RA$HomeStateChange, data.minus.RA$PrevStateUnfriendly)
table(data.minus.RA$PresPartyCat, data.minus.RA$HomeStateChange, data.minus.RA$PrevStateFriendly)

#### Table A3
cox.zph(cox.mod, transform = "rank")

#### Table A4
cox.mod.corrected<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                 AnyHSOpp+
                 PolarizationMea+
                 strata(DividedGovernment)+
                 BalancedBench+
                 strata(ElectionYear), 
               cluster = VacancyID,
               data = Main.Text.Data)
summary(cox.mod.corrected)

#### Table A5
View(HS.pred.probs2)
View(pred.prob.data)

#### Table A6
logit.mod2<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  WellQualifiedNom,  
                data = data.post1946,
                family = binomial(link='logit'))
summary(logit.mod2)
logit.mod3<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  as.factor(JudChairNum),
                data = data.minus.RA,
                family = binomial(link='logit'))
summary(logit.mod3)
logit.mod4<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  WellQualifiedNom+  
                  as.factor(JudChairNum),
                data = data.post1946,
                family = binomial(link='logit'))
summary(logit.mod4)

#### Table A7
logit.mod5<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PresSenateDistance+
                  PresJudChairDistance+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  UnconfirmedNoms+
                  TimeLeftSession,   
                data = data.minus.RA,
                family = binomial(link='logit'))
summary(logit.mod5)
logit.mod6<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PresSenateDistance+
                  PresJudChairDistance+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  TimeLeftSession+
                  UnconfirmedNoms+
                  WellQualifiedNom,
                #PostBork+
                #PostGingrich,
                data = data.post1946,
                family = binomial(link='logit'))
summary(logit.mod6)
logit.mod7<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PresSenateDistance+
                  PresJudChairDistance+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  UnconfirmedNoms+
                  TimeLeftSession+
                  #PostBork+
                  #PostGingrich+
                  as.factor(JudChairNum),
                data = data.minus.RA,
                family = binomial(link='logit'))
summary(logit.mod7)
logit.mod8<-glm(Successful~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PresSenateDistance+
                  PresJudChairDistance+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  UnconfirmedNoms+
                  TimeLeftSession+
                  WellQualifiedNom+
                  as.factor(JudChairNum),
                data = data.post1946,
                family = binomial(link='logit'))
summary(logit.mod8)

#### Table A8
cox.mod2<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear,
                cluster = VacancyID,
                data = data.minus.RA)
summary(cox.mod2)
cox.mod3<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  as.factor(JudChairNum),
                cluster = VacancyID,
                data = data.minus.RA)
summary(cox.mod3)
cox.mod4<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  WellQualifiedNom+
                  as.factor(JudChairNum),
                cluster = VacancyID,
                data = data.post1946)
summary(cox.mod4)
stargazer(cox.mod2, cox.mod3, cox.mod4)

#### Table A9
cox.mod5<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  PresSenateDistance+
                  UnconfirmedNoms+
                  PresJudChairDistance+
                  TimeLeftSession,
                cluster = VacancyID,
                data = data.minus.RA)
summary(cox.mod5)
cox.mod6<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  WellQualifiedNom+
                  PresSenateDistance+
                  UnconfirmedNoms+
                  PresJudChairDistance+
                  TimeLeftSession,
                cluster = VacancyID,
                data = data.post1946)
summary(cox.mod6)
cox.mod7<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  PresSenateDistance+
                  UnconfirmedNoms+
                  PresJudChairDistance+
                  TimeLeftSession+
                  as.factor(JudChairNum),
                cluster = VacancyID,
                data = data.minus.RA)
summary(cox.mod7)
cox.mod8<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                  AnyHSOpp+
                  PolarizationMea+
                  DividedGovernment+
                  BalancedBench+
                  ElectionYear+
                  PresSenateDistance+
                  WellQualifiedNom+
                  UnconfirmedNoms+
                  PresJudChairDistance+
                  TimeLeftSession+
                  as.factor(JudChairNum),
                cluster = VacancyID,
                data = data.minus.RA)
summary(cox.mod8)

#### Table A10
fe.mod1<-glm(Successful~HomeStateChange*PrevStateDistance+
                 AnyHSOpp+
                 PolarizationMea+
                 DividedGovernment+
                 BalancedBench+
                 ElectionYear+
                 as.factor(JudChairNum),
               data = Main.Text.Data,
               family = binomial(link='logit'))
summary(fe.mod1)
fe.mod2<-glm(Successful~HomeStateChange*PrevStateDistance+
               AnyHSOpp+
               PolarizationMea+
               DividedGovernment+
               BalancedBench+
               ElectionYear+
               as.factor(PresNo),
             data = Main.Text.Data,
             family = binomial(link='logit'))
summary(fe.mod2)
fe.mod3<-glm(Successful~HomeStateChange*PrevStateDistance+
               AnyHSOpp+
               PolarizationMea+
               DividedGovernment+
               BalancedBench+
               ElectionYear+
               as.factor(Decade),
             data = Main.Text.Data,
             family = binomial(link='logit'))
summary(fe.mod3)
fe.mod4<-glm(Successful~HomeStateChange*PrevStateDistance+
               AnyHSOpp+
               PolarizationMea+
               DividedGovernment+
               BalancedBench+
               ElectionYear+
               as.factor(Circuit),
             data = Main.Text.Data,
             family = binomial(link='logit'))
summary(fe.mod4)

#### Table A11
bork.mod1<-glm(Successful~HomeStateChange*PrevStateDistance+
               AnyHSOpp+
               PolarizationMea+
               DividedGovernment+
               BalancedBench+
               ElectionYear,
             data = data.postBork,
             family = binomial(link='logit'))
summary(bork.mod1)
bork.mod2<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                            AnyHSOpp+
                            PolarizationMea+
                            DividedGovernment+
                            BalancedBench+
                            ElectionYear, 
                          cluster = VacancyID,
                          data = data.postBork)
summary(bork.mod2)
filibuster.mod1<-glm(Successful~HomeStateChange*PrevStateDistance+
                 AnyHSOpp+
                 PolarizationMea+
                 DividedGovernment+
                 BalancedBench+
                 ElectionYear,
               data = data.post60,
               family = binomial(link='logit'))
summary(filibuster.mod1)
filibuster.mod2<-coxph(Surv(NomToFinalAction, Successful)~HomeStateChange*PrevStateDistance+
                   AnyHSOpp+
                   PolarizationMea+
                   DividedGovernment+
                   BalancedBench+
                   ElectionYear, 
                 cluster = VacancyID,
                 data = data.post60)
summary(filibuster.mod2)




