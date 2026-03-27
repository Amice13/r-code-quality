# Load Necessary Packages
library(dplyr)
library(car)
library(lmtest)
library(sandwich)
library(fabricatr)
library(sjPlot)
library(lme4)
library(stargazer)
library(margins)
library(multcomp)

### Import Clean Data ###
# SET YOUR WORKING DIRECTORY
PV<-read.csv("Double-Profile Experiment.csv")

# Get Profile pair IDs
seven <- data.frame(seven=c(1:19341,1:19341))
seven <- arrange(seven,seven)
PV$profile <- seven$seven

# Content Analysis
summary(as.factor(PV$Responded)) # 55.6% responded
summary(as.factor(PV$Insisted_OnlyAct)) # 48.7% insisted only act mattered or only mentioned act, 13.6% mentioned mostly act, 12.2% mentioned act in conjunction with other things
table(as.factor(PV$Insisted_OnlyAct),as.factor(PV$Mentioned_Perp))
summary(as.factor(PV$Resented_Choice)) # 7.1% generally mentioned wanting to throw everyone in jail, extra 1.3% mentioned it for some choices, only 4 people wanted to free multiple people, 1 mentioned wanting to do so for some people
summary(as.factor(PV$Mentioned_Perp)) # 11.9% mentioned characteristics of the perpetrator, 5.8% as secondary/tie-breaker (these varied)

### Setting Reference Categories ###
PV$profile_target<-factor(PV$profile_target, c("Counter-protester", "Government", "Police", 
                                               "Small business"))
PV$profile_severity<-factor(PV$profile_severity, c("Low", "Medium", "High"))
PV$profile_race<-factor(PV$profile_race, c("White", "Black", "Hispanic", "Asian", "Middle Eastern",
                                           "American Indian"))
PV$profile_marital<-factor(PV$profile_marital, c("Single", "Cohabitating with partner", "Married", 
                                                 "Divorced", "Widowed"))
PV$profile_age<-factor(PV$profile_age, c("Mid twenties", "Mid thirties", "Mid forties", 
                                         "Mid fifties"))

PV$Race.Ethnicity<-factor(PV$Race.Ethnicity, levels= c("White", "Black", "Asian", "Native American",
                                                       "Native Hawaiian or Pacific Islander","Other"))
PV$Gender<-factor(PV$Gender, levels= c("Male", "Female"))
PV$hispanic<-factor(PV$hispanic, levels= c("No", "Yes"))
PV$native<-factor(PV$native, levels= c("No", "Yes"))
PV$interest<-factor(PV$interest, levels= c("Not at all interested", "Not very interested", "Fairly interested",
                                           "Very interested"))
PV$education<-factor(PV$education, levels= c("High School Grad", "Less than High School", "Some College",
                                             "Associates", "Bachelors", "Post-Grad"))
PV$employment<-factor(PV$employment, levels= c("Full time employee", "Homemaker", "Part time employee",
                                               "Unemployed seeking work", "Sick/Disabled", 
                                               "Temporary Sick/Disabled", "Student", "Retired", "None"))
PV$PID_Congruent <- NA
PV$PID_Congruent[(PV$Party7<4 & PV$profile_party=="Democrat") | (PV$Party7>4 & PV$profile_party=="Republican")] <- "Congruent"
PV$PID_Congruent[PV$profile_party=="Independent"] <- "Independent"
PV$PID_Congruent[(PV$Party7<4 & PV$profile_party=="Republican") | (PV$Party7>4 & PV$profile_party=="Democrat")] <- "Incongruent"
PV$PID_Congruent <- factor(PV$PID_Congruent,levels=c("Congruent","Independent","Incongruent"))
PV$polariz <- ifelse(PV$Party7<4,PV$Dems_ft-PV$Repubs_ft,ifelse(PV$Party7>4,PV$Repubs_ft-PV$Dems_ft,NA))
PV$race <- ifelse(PV$hispanic=="Yes","Hispanic",PV$Race.Ethnicity)
PV$race[PV$race=="1"] <- "White"
PV$race[PV$race=="2"] <- "Black"
PV$race[PV$race=="3"] <- "Asian"
PV$race[PV$race=="4"] <- "Native American"
PV$race[PV$race=="5"] <- NA
PV$race[PV$race=="6"] <- NA
PV$race <- factor(PV$race,levels=c("White","Black","Hispanic","Asian","Native American"))
PV$profile_act1 <- PV$profile_act
PV$profile_act <- factor(PV$profile_act)
PV$act <- factor(ifelse(as.numeric(PV$profile_act)==1,NA,ifelse(as.numeric(PV$profile_act)==10,"Threatened Counter-Protester",ifelse(as.numeric(PV$profile_act)==8,"Punched Counter-Protester",ifelse(as.numeric(PV$profile_act)==12,"Injured Counter-Protester",ifelse(as.numeric(PV$profile_act)==11,"Threatened Police",ifelse(as.numeric(PV$profile_act)==9,"Punched Police",ifelse(as.numeric(PV$profile_act)==13,"Injured Police",ifelse(as.numeric(PV$profile_act)==6,"Graffitied Government Building",ifelse(as.numeric(PV$profile_act)==4,"Damaged Government Building",ifelse(as.numeric(PV$profile_act)==2,"Burned Down Government Building",ifelse(as.numeric(PV$profile_act)==7,"Graffitied Small Business",ifelse(as.numeric(PV$profile_act)==5,"Damaged Small Business","Burned Down Small Business")))))))))))),levels=c("Threatened Counter-Protester","Punched Counter-Protester","Injured Counter-Protester","Threatened Police","Punched Police","Injured Police","Graffitied Government Building","Damaged Government Building","Burned Down Government Building","Graffitied Small Business","Damaged Small Business","Burned Down Small Business"))
PV$Party3_1 <- ifelse(PV$Party3=="Independent",NA,PV$Party3)

### Creating separate datasets for Democrats and Republicans Respondents ###

Dems<-PV%>%
  filter(Party7<4)

Reps<-PV%>%
  filter(Party7>4)

# Table 1 - Sample Characteristics
summary(as.factor(PV$Age))/14
summary(as.factor(PV$Gender))/14
table(PV$Race.Ethnicity,PV$hispanic)/14 # % White can include both Hispanic and non-Hispanic

# Content Analysis of Reasons for Answers
summary(as.factor(PV$Responded)) # 55.6% responded
summary(as.factor(PV$Resented_Choice)) # 7.1% did not want to free anyone (=1), 1.3% mentioned this for some choices but not others (=0.5, 0.3% wanted to free both (=-1)
summary(as.factor(PV$Insisted_OnlyAct)) # 48.7% insisted only act mattered (=1), 19.3% mentioned it in conjunction with others (=0.5), 68.0% total
table(PV$Insisted_OnlyAct,PV$Mentioned_Perp) # 12.1% mentioned act along with other concerns (=0.5 or 1 for mentioning perp and =0.5 for mentioning act), 7.2% mentioned act was one of many concerns without specifying specific ones (=0.5 for mentioning act, =0 for mentioning perp)
summary(as.factor(PV$Mentioned_Perp))

# Table A2
mlm_reg <- lmer(decision~profile_target+ profile_severity+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV)

# Figures 1 and 2/Table A4
## Regressions
mlm_reg_act <- lmer(decision~act+PID_Congruent+ profile_race+
                      profile_gender+ profile_children+ profile_marital+ profile_age+
                      profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV) # Model 1

mlm_reg_act_party <- lmer(decision~(act*Party3_1)+PID_Congruent+ profile_race+
                            profile_gender+ profile_children+ profile_marital+ profile_age+
                            profile_occupation_group+ profile_residence_group+(1 | profile)+(1 | ResponseId),data=PV) # Model 2
## Figure 1
df.act <- data.frame(act=c("Threatened Counter-Protester","Punched Counter-Protester","Injured Counter-Protester","Threatened Police","Punched Police","Injured Police","Graffitied Government Building","Damaged Government Building","Burned Down Government Building","Graffitied Small Business","Damaged Small Business","Burned Down Small Business"),
                     PID_Congruent="Independent",
                     profile_race="Asian",
                     profile_gender="Woman",
                     profile_residence_group="Urban",
                     profile_marital="Cohabitating with partner",
                     profile_children="None",
                     profile_occupation_group="Blue Collar",
                     profile_age="Mid twenties")

fig.act.data <- as.data.frame(predict(mlm_reg_act,newdata=df.act,re.form=NA))
set.seed(3)
fig.act.data <- bootMer(mlm_reg_act,
                           nsim = 1000,
                           FUN = function(x) { predict(x, newdata = df.act, re.form = NA) })
fig.act.data
fig.act.data <- data.frame(prob=c(0.6528579,0.5443734,0.3158967,0.5466774,0.4542914,0.2820129,0.6635033,0.4796486,0.3556624,0.6612595,0.4954664,0.3641832),
                           se=c(0.01470914,0.01446174,0.01463921,0.01500514,0.01470901,0.01498349,0.01459296,0.01496970,0.01494027,0.01494424,0.01470812,0.01460288))
fig.act.data$lci <- fig.act.data$prob-(1.96*fig.act.data$se)
fig.act.data$uci <- fig.act.data$prob+(1.96*fig.act.data$se)
fig.act.data$act <- factor(c("Threatened","Punched","Injured","Threatened","Punched","Injured","Graffitied","Damaged","Burned","Graffitied","Damaged","Burned"),levels=c("Burned","Damaged","Graffitied","Injured","Punched","Threatened"))
fig.act.data$Target <- factor(c("Protester","Protester","Protester","Police","Police","Police","Government","Government","Government","Business","Business","Business"),levels=c("Protester","Police","Government","Business"))
fig.act.data$Type <- factor(c("People","People","People","People","People","People","Property","Property","Property","Property","Property","Property"),levels=c("People","Property"))

fig.act <- ggplot(fig.act.data,aes(x=act,y=prob,color=Target)) + geom_point(position=position_dodge(width = .5)) + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(width = .5)) + theme_classic() + coord_flip() + scale_y_continuous(name="Leniency",limits=c(0,1)) + xlab("") + geom_hline(yintercept=0.5,linetype="dashed") + facet_wrap(~Type,nrow=2,scale="free") + theme(legend.position="bottom") + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Double-Profile")
ggsave("Double Profile Act.png",fig.act,device="png",width=8,height=6,units="in")
## Figure 2
df.actparty <- data.frame(act=c(rep(c("Threatened Counter-Protester","Punched Counter-Protester","Injured Counter-Protester","Threatened Police","Punched Police","Injured Police","Graffitied Government Building","Damaged Government Building","Burned Down Government Building","Graffitied Small Business","Damaged Small Business","Burned Down Small Business"),2)),
                          Party3_1=c(rep("Democrat",12,),rep("Republican",12)),
                          PID_Congruent="Independent",
                          profile_race="Asian",
                          profile_gender="Woman",
                          profile_residence_group="Urban",
                          profile_marital="Cohabitating with partner",
                          profile_children="None",
                          profile_occupation_group="Blue Collar",
                          profile_age="Mid twenties")
fig.actparty.data <- as.data.frame(predict(mlm_reg_act_party,newdata=df.actparty,re.form=NA))
set.seed(3)
fig.actparty.data <- bootMer(mlm_reg_act_party,
                           nsim = 1000,
                           FUN = function(x) { predict(x, newdata = df.actparty, re.form = NA) })
fig.actparty.data
fig.actparty.data <- data.frame(prob=c(.6425804,.5305087,.3134715,.5423794,.4553462,.3065196,.6619350,.4684143,.3558437,.6704322,.5115589,.3729479,.6658989,.5626356,.3217128,.5492507,.4591218,.2469431,.6562464,.4957574,.3483782,.6568521,.4813697,.3513233),
                                se=c(.01646047,.01621868,.01623663,.01669200,.01676417,.01629124,.01681774,.01743319,.01689423,.01605811,.01735321,.01678552,.01800610,.01787552,.01829335,.01786061,.01779413,.01838394,.01778743,.01763311,.01766898,.01841822,.01826826,.01814848))
fig.actparty.data$lci <- fig.actparty.data$prob-(1.96*fig.actparty.data$se)
fig.actparty.data$uci <- fig.actparty.data$prob+(1.96*fig.actparty.data$se)
fig.actparty.data$act <- factor(c("Threatened","Punched","Injured","Threatened","Punched","Injured","Graffitied","Damaged","Burned","Graffitied","Damaged","Burned","Threatened","Punched","Injured","Threatened","Punched","Injured","Graffitied","Damaged","Burned","Graffitied","Damaged","Burned"),levels=c("Threatened","Punched","Injured","Graffitied","Damaged","Burned"))
fig.actparty.data$target <- factor(c("Protester","Protester","Protester","Police","Police","Police","Government","Government","Government","Small Business","Small Business","Small Business","Protester","Protester","Protester","Police","Police","Police","Government","Government","Government","Small Business","Small Business","Small Business"),levels=c("Protester","Police","Government","Small Business"))
fig.actparty.data$Party <- factor(df.actparty$Party3,levels=c("Democrat","Republican"))
fig.actparty.data$sig <- ifelse(fig.actparty.data$lci>0.5 & fig.actparty.data$uci>0.5 | fig.actparty.data$lci<0.5 & fig.actparty.data$uci<0.5,T,F)
fig.actparty <- ggplot(fig.actparty.data,aes(x=act,y=prob,group=Party,color=Party)) + geom_point(aes(alpha=sig),position=position_dodge(width=.5)) + geom_errorbar(aes(ymin=lci,ymax=uci,alpha=sig),size=0.5,width=0,position=position_dodge(width=.5)) + theme_classic() + coord_flip() + scale_y_continuous(name="Leniency",limits=c(0.2,0.72)) + xlab("Act") + geom_hline(yintercept=0.5,linetype="dashed") + scale_color_manual(name="Respondent Party",values=c("Blue","Red")) + facet_wrap(~target,nrow=2,scale="free") + scale_alpha_manual(values=c(0.5,1),guide="none") + theme(legend.position="bottom") + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Double-Profile")
ggsave("Double Profile Act Respondent Party.png",fig.actparty,device="png",width=6,height=4,units="in")

# Figure 3/Table A6
## Regressions
mlm_reg_party <- lmer(decision~PID_Congruent+(1 | profile)+(1 | ResponseId),data=PV) # Model 1
mlm_reg_party_act <- lmer(decision~PID_Congruent+act+(PID_Congruent*act) + (1 | profile)+(1 | ResponseId),data=PV) # Model 2
mlm_reg_party_ap <- lmer(decision~PID_Congruent+polariz+(PID_Congruent*polariz)+(1 | profile)+(1 | ResponseId),data=PV) # Model 3
## Figure
df.party <- data.frame(PID_Congruent=c("Congruent","Independent","Incongruent"))
fig.party.data <- as.data.frame(predict(mlm_reg_party,newdata=df.party,re.form=NA))
set.seed(3)
fig.party.data <- bootMer(mlm_reg_party,
                        nsim = 1000,
                        FUN = function(x) { predict(x, newdata = df.party, re.form = NA) })
fig.party.data
fig.party.data <- data.frame(prob=c(0.5339744,0.5005802,0.4660994),
                             se=c(0.004671474,0.004521912,0.004919432))
fig.party.data$lci <- fig.party.data$prob-(1.96*fig.party.data$se)
fig.party.data$uci <- fig.party.data$prob+(1.96*fig.party.data$se)
fig.party.data$Perpetrator <- factor(c("Congruent","Independent","Incongruent"),levels=c("Congruent","Independent","Incongruent"))
fig.party.data$sig <- c(T,F,T)

fig.party <- ggplot(fig.party.data,aes(x=Perpetrator,y=prob)) + geom_point() + geom_errorbar(aes(ymin=lci,ymax=uci,alpha=sig),size=0.5,width=0) + theme_classic() + coord_flip() + scale_y_continuous(name="Leniency") + theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept=0.5,linetype="dashed") + xlab("") + ggtitle("Double-Profile") + scale_alpha_manual(values=c(0.5,1),guide="none")
ggsave("Double Profile Party.png",fig.party,device="png",width=6,height=4,units="in")

# Figure 4 (Model from Model 3 in Table A6)
df.party.ap <- data.frame(polariz=c(0:100,0:100,0:100),
                          PID_Congruent=c(rep("Congruent",101),rep("Independent",101),rep("Incongruent",101)))
fig.party.data.ap <- as.data.frame(predict(mlm_reg_party_ap,newdata=df.party.ap,re.form=NA))
set.seed(3)
fig.party.data.ap <- bootMer(mlm_reg_party_ap,
                          nsim = 1000,
                          FUN = function(x) { predict(x, newdata = df.party.ap, re.form = NA) })
fig.party.data.ap
fig.party.data.ap <- read.csv("Polarization by Party Double Profile.csv",header=T,sep=",")
fig.party.data.ap$polariz <- c(0:100,0:100,0:100)
fig.party.data.ap$Perpetrator <- factor(c(rep("Congruent",101),rep("Independent",101),rep("Incongruent",101)),levels=c("Congruent","Independent","Incongruent"))

fig.party.ap <- ggplot(fig.party.data.ap,aes(x=polariz,y=pred,group=Perpetrator,fill=Perpetrator)) + geom_line() + theme_classic() + geom_ribbon(aes(ymin=lci,ymax=uci),alpha=0.5) + geom_hline(yintercept=0.5,linetype="dashed") + xlab("Affective Polarization") + ylab("Leniency")
ggsave("Double Profile Party Polariz.png",fig.party.ap,device="png",width=6,height=4,units="in")

# Pairwise Comparisons
leniency.act.pairwise <- summary(glht(mlm_reg_act, mcp(act = "Tukey")))
leniency.pid.pairwise <- summary(glht(mlm_reg_act, mcp(PID_Congruent = "Tukey")))
leniency.race.pairwise <- summary(glht(mlm_reg_act, mcp(profile_race = "Tukey")))
leniency.gender.pairwise <- summary(glht(mlm_reg_act, mcp(profile_gender = "Tukey")))
leniency.residence.pairwise <- summary(glht(mlm_reg_act, mcp(profile_residence_group = "Tukey")))
leniency.marital.pairwise <- summary(glht(mlm_reg_act, mcp(profile_marital = "Tukey")))
leniency.children.pairwise <- summary(glht(mlm_reg_act, mcp(profile_children = "Tukey")))
leniency.occupation.pairwise <- summary(glht(mlm_reg_act, mcp(profile_occupation_group = "Tukey")))
leniency.age.pairwise <- summary(glht(mlm_reg_act, mcp(profile_age = "Tukey")))
summary(leniency.act.pairwise$test$pvalues<.05) # 56/66 pairwise comparisons (84.4%)
summary(leniency.pid.pairwise$test$pvalues<.05) # 3/3 pairwise comparisons
summary(leniency.race.pairwise$test$pvalues<.05) # 1/15 pairwise comparisons
summary(leniency.gender.pairwise$test$pvalues<.05) # 1/1 pairwise comparisons
summary(leniency.residence.pairwise$test$pvalues<.05) # 2/3 pairwise comparisons
summary(leniency.marital.pairwise$test$pvalues<.05) # 0/10 pairwise comparisons
summary(leniency.children.pairwise$test$pvalues<.05) # 2/3 pairwise comparisons
summary(leniency.occupation.pairwise$test$pvalues<.05) # 1/1 pairwise comparisons
summary(leniency.age.pairwise$test$pvalues<.05) # 0/6 pairwise comparisons; Identity: 10/42 (23.8%)

# Effects of identity holding act constant
leniency.threat_protester <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==1)
leniency.punch_protester <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==2)
leniency.injure_protester <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==3)
leniency.threat_police <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==4)
leniency.punch_police <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==5)
leniency.injure_police <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==6)
leniency.graffiti_govt <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==7)
leniency.damage_govt <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==8)
leniency.burn_govt <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==9)
leniency.graffiti_business <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==10)
leniency.damage_business <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==11)
leniency.burn_business <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$act)==12)

leniency.pid1 <- summary(glht(leniency.threat_protester, mcp(PID_Congruent = "Tukey")))
leniency.race1 <- summary(glht(leniency.threat_protester, mcp(profile_race = "Tukey")))
leniency.gender1 <- summary(glht(leniency.threat_protester, mcp(profile_gender = "Tukey")))
leniency.residence1 <- summary(glht(leniency.threat_protester, mcp(profile_residence_group = "Tukey")))
leniency.marital1 <- summary(glht(leniency.threat_protester, mcp(profile_marital = "Tukey")))
leniency.children1 <- summary(glht(leniency.threat_protester, mcp(profile_children = "Tukey")))
leniency.occupation1 <- summary(glht(leniency.threat_protester, mcp(profile_occupation_group = "Tukey")))
leniency.age1 <- summary(glht(leniency.threat_protester, mcp(profile_age = "Tukey")))
leniency.pid2 <- summary(glht(leniency.punch_protester, mcp(PID_Congruent = "Tukey")))
leniency.race2 <- summary(glht(leniency.punch_protester, mcp(profile_race = "Tukey")))
leniency.gender2 <- summary(glht(leniency.punch_protester, mcp(profile_gender = "Tukey")))
leniency.residence2 <- summary(glht(leniency.punch_protester, mcp(profile_residence_group = "Tukey")))
leniency.marital2 <- summary(glht(leniency.punch_protester, mcp(profile_marital = "Tukey")))
leniency.children2 <- summary(glht(leniency.punch_protester, mcp(profile_children = "Tukey")))
leniency.occupation2 <- summary(glht(leniency.punch_protester, mcp(profile_occupation_group = "Tukey")))
leniency.age2 <- summary(glht(leniency.punch_protester, mcp(profile_age = "Tukey")))
leniency.pid3 <- summary(glht(leniency.injure_protester, mcp(PID_Congruent = "Tukey")))
leniency.race3 <- summary(glht(leniency.injure_protester, mcp(profile_race = "Tukey")))
leniency.gender3 <- summary(glht(leniency.injure_protester, mcp(profile_gender = "Tukey")))
leniency.residence3 <- summary(glht(leniency.injure_protester, mcp(profile_residence_group = "Tukey")))
leniency.marital3 <- summary(glht(leniency.injure_protester, mcp(profile_marital = "Tukey")))
leniency.children3 <- summary(glht(leniency.injure_protester, mcp(profile_children = "Tukey")))
leniency.occupation3 <- summary(glht(leniency.injure_protester, mcp(profile_occupation_group = "Tukey")))
leniency.age3 <- summary(glht(leniency.injure_protester, mcp(profile_age = "Tukey")))
leniency.pid4 <- summary(glht(leniency.threat_police, mcp(PID_Congruent = "Tukey")))
leniency.race4 <- summary(glht(leniency.threat_police, mcp(profile_race = "Tukey")))
leniency.gender4 <- summary(glht(leniency.threat_police, mcp(profile_gender = "Tukey")))
leniency.residence4 <- summary(glht(leniency.threat_police, mcp(profile_residence_group = "Tukey")))
leniency.marital4 <- summary(glht(leniency.threat_police, mcp(profile_marital = "Tukey")))
leniency.children4 <- summary(glht(leniency.threat_police, mcp(profile_children = "Tukey")))
leniency.occupation4 <- summary(glht(leniency.threat_police, mcp(profile_occupation_group = "Tukey")))
leniency.age4 <- summary(glht(leniency.threat_police, mcp(profile_age = "Tukey")))
leniency.pid5 <- summary(glht(leniency.punch_police, mcp(PID_Congruent = "Tukey")))
leniency.race5 <- summary(glht(leniency.punch_police, mcp(profile_race = "Tukey")))
leniency.gender5 <- summary(glht(leniency.punch_police, mcp(profile_gender = "Tukey")))
leniency.residence5 <- summary(glht(leniency.punch_police, mcp(profile_residence_group = "Tukey")))
leniency.marital5 <- summary(glht(leniency.punch_police, mcp(profile_marital = "Tukey")))
leniency.children5 <- summary(glht(leniency.punch_police, mcp(profile_children = "Tukey")))
leniency.occupation5 <- summary(glht(leniency.punch_police, mcp(profile_occupation_group = "Tukey")))
leniency.age5 <- summary(glht(leniency.punch_police, mcp(profile_age = "Tukey")))
leniency.pid6 <- summary(glht(leniency.injure_police, mcp(PID_Congruent = "Tukey")))
leniency.race6 <- summary(glht(leniency.injure_police, mcp(profile_race = "Tukey")))
leniency.gender6 <- summary(glht(leniency.injure_police, mcp(profile_gender = "Tukey")))
leniency.residence6 <- summary(glht(leniency.injure_police, mcp(profile_residence_group = "Tukey")))
leniency.marital6 <- summary(glht(leniency.injure_police, mcp(profile_marital = "Tukey")))
leniency.children6 <- summary(glht(leniency.injure_police, mcp(profile_children = "Tukey")))
leniency.occupation6 <- summary(glht(leniency.injure_police, mcp(profile_occupation_group = "Tukey")))
leniency.age6 <- summary(glht(leniency.injure_police, mcp(profile_age = "Tukey")))
leniency.pid7 <- summary(glht(leniency.graffiti_govt, mcp(PID_Congruent = "Tukey")))
leniency.race7 <- summary(glht(leniency.graffiti_govt, mcp(profile_race = "Tukey")))
leniency.gender7 <- summary(glht(leniency.graffiti_govt, mcp(profile_gender = "Tukey")))
leniency.residence7 <- summary(glht(leniency.graffiti_govt, mcp(profile_residence_group = "Tukey")))
leniency.marital7 <- summary(glht(leniency.graffiti_govt, mcp(profile_marital = "Tukey")))
leniency.children7 <- summary(glht(leniency.graffiti_govt, mcp(profile_children = "Tukey")))
leniency.occupation7 <- summary(glht(leniency.graffiti_govt, mcp(profile_occupation_group = "Tukey")))
leniency.age7 <- summary(glht(leniency.graffiti_govt, mcp(profile_age = "Tukey")))
leniency.pid8 <- summary(glht(leniency.damage_govt, mcp(PID_Congruent = "Tukey")))
leniency.race8 <- summary(glht(leniency.damage_govt, mcp(profile_race = "Tukey")))
leniency.gender8 <- summary(glht(leniency.damage_govt, mcp(profile_gender = "Tukey")))
leniency.residence8 <- summary(glht(leniency.damage_govt, mcp(profile_residence_group = "Tukey")))
leniency.marital8 <- summary(glht(leniency.damage_govt, mcp(profile_marital = "Tukey")))
leniency.children8 <- summary(glht(leniency.damage_govt, mcp(profile_children = "Tukey")))
leniency.occupation8 <- summary(glht(leniency.damage_govt, mcp(profile_occupation_group = "Tukey")))
leniency.age8 <- summary(glht(leniency.damage_govt, mcp(profile_age = "Tukey")))
leniency.pid9 <- summary(glht(leniency.burn_govt, mcp(PID_Congruent = "Tukey")))
leniency.race9 <- summary(glht(leniency.burn_govt, mcp(profile_race = "Tukey")))
leniency.gender9 <- summary(glht(leniency.burn_govt, mcp(profile_gender = "Tukey")))
leniency.residence9 <- summary(glht(leniency.burn_govt, mcp(profile_residence_group = "Tukey")))
leniency.marital9 <- summary(glht(leniency.burn_govt, mcp(profile_marital = "Tukey")))
leniency.children9 <- summary(glht(leniency.burn_govt, mcp(profile_children = "Tukey")))
leniency.occupation9 <- summary(glht(leniency.burn_govt, mcp(profile_occupation_group = "Tukey")))
leniency.age9 <- summary(glht(leniency.burn_govt, mcp(profile_age = "Tukey")))
leniency.pid10 <- summary(glht(leniency.graffiti_business, mcp(PID_Congruent = "Tukey")))
leniency.race10 <- summary(glht(leniency.graffiti_business, mcp(profile_race = "Tukey")))
leniency.gender10 <- summary(glht(leniency.graffiti_business, mcp(profile_gender = "Tukey")))
leniency.residence10 <- summary(glht(leniency.graffiti_business, mcp(profile_residence_group = "Tukey")))
leniency.marital10 <- summary(glht(leniency.graffiti_business, mcp(profile_marital = "Tukey")))
leniency.children10 <- summary(glht(leniency.graffiti_business, mcp(profile_children = "Tukey")))
leniency.occupation10 <- summary(glht(leniency.graffiti_business, mcp(profile_occupation_group = "Tukey")))
leniency.age10 <- summary(glht(leniency.graffiti_business, mcp(profile_age = "Tukey")))
leniency.pid11 <- summary(glht(leniency.damage_business, mcp(PID_Congruent = "Tukey")))
leniency.race11 <- summary(glht(leniency.damage_business, mcp(profile_race = "Tukey")))
leniency.gender11 <- summary(glht(leniency.damage_business, mcp(profile_gender = "Tukey")))
leniency.residence11 <- summary(glht(leniency.damage_business, mcp(profile_residence_group = "Tukey")))
leniency.marital11 <- summary(glht(leniency.damage_business, mcp(profile_marital = "Tukey")))
leniency.children11 <- summary(glht(leniency.damage_business, mcp(profile_children = "Tukey")))
leniency.occupation11 <- summary(glht(leniency.damage_business, mcp(profile_occupation_group = "Tukey")))
leniency.age11 <- summary(glht(leniency.damage_business, mcp(profile_age = "Tukey")))
leniency.pid12 <- summary(glht(leniency.burn_business, mcp(PID_Congruent = "Tukey")))
leniency.race12 <- summary(glht(leniency.burn_business, mcp(profile_race = "Tukey")))
leniency.gender12 <- summary(glht(leniency.burn_business, mcp(profile_gender = "Tukey")))
leniency.residence12 <- summary(glht(leniency.burn_business, mcp(profile_residence_group = "Tukey")))
leniency.marital12 <- summary(glht(leniency.burn_business, mcp(profile_marital = "Tukey")))
leniency.children12 <- summary(glht(leniency.burn_business, mcp(profile_children = "Tukey")))
leniency.occupation12 <- summary(glht(leniency.burn_business, mcp(profile_occupation_group = "Tukey")))
leniency.age12 <- summary(glht(leniency.burn_business, mcp(profile_age = "Tukey")))

summary(leniency.pid1$test$pvalues<.05) # 1/3
summary(leniency.race1$test$pvalues<.05) # 0/15
summary(leniency.gender1$test$pvalues<.05) # 0/1
summary(leniency.residence1$test$pvalues<.05) # 0/3
summary(leniency.marital1$test$pvalues<.05) # 0/10
summary(leniency.children1$test$pvalues<.05) # 0/3
summary(leniency.occupation1$test$pvalues<.05) # 0/1
summary(leniency.age1$test$pvalues<.05) # 0/6
summary(leniency.pid2$test$pvalues<.05) # 2/3
summary(leniency.race2$test$pvalues<.05) # 0/15
summary(leniency.gender2$test$pvalues<.05) # 0/1
summary(leniency.residence2$test$pvalues<.05) # 0/3
summary(leniency.marital2$test$pvalues<.05) # 0/10
summary(leniency.children2$test$pvalues<.05) # 0/3
summary(leniency.occupation2$test$pvalues<.05) # 0/1
summary(leniency.age2$test$pvalues<.05) # 0/6
summary(leniency.pid3$test$pvalues<.05) # 2/3
summary(leniency.race3$test$pvalues<.05) # 0/15
summary(leniency.gender3$test$pvalues<.05) # 0/1
summary(leniency.residence3$test$pvalues<.05) # 0/3
summary(leniency.marital3$test$pvalues<.05) # 0/10
summary(leniency.children3$test$pvalues<.05) # 0/3
summary(leniency.occupation3$test$pvalues<.05) # 0/1
summary(leniency.age3$test$pvalues<.05) # 0/6
summary(leniency.pid4$test$pvalues<.05) # 1/3
summary(leniency.race4$test$pvalues<.05) # 0/15
summary(leniency.gender4$test$pvalues<.05) # 0/1
summary(leniency.residence4$test$pvalues<.05) # 1/3
summary(leniency.marital4$test$pvalues<.05) # 0/10
summary(leniency.children4$test$pvalues<.05) # 0/3
summary(leniency.occupation4$test$pvalues<.05) # 0/1
summary(leniency.age4$test$pvalues<.05) # 0/6
summary(leniency.pid5$test$pvalues<.05) # 2/3
summary(leniency.race5$test$pvalues<.05) # 0/15
summary(leniency.gender5$test$pvalues<.05) # 0/1
summary(leniency.residence5$test$pvalues<.05) # 0/3
summary(leniency.marital5$test$pvalues<.05) # 0/10
summary(leniency.children5$test$pvalues<.05) # 0/3
summary(leniency.occupation5$test$pvalues<.05) # 0/1
summary(leniency.age5$test$pvalues<.05) # 0/6
summary(leniency.pid6$test$pvalues<.05) # 0/3
summary(leniency.race6$test$pvalues<.05) # 0/15
summary(leniency.gender6$test$pvalues<.05) # 0/1
summary(leniency.residence6$test$pvalues<.05) # 0/3
summary(leniency.marital6$test$pvalues<.05) # 0/10
summary(leniency.children6$test$pvalues<.05) # 0/3
summary(leniency.occupation6$test$pvalues<.05) # 0/1
summary(leniency.age6$test$pvalues<.05) # 0/6
summary(leniency.pid7$test$pvalues<.05) # 2/3
summary(leniency.race7$test$pvalues<.05) # 0/15
summary(leniency.gender7$test$pvalues<.05) # 1/1
summary(leniency.residence7$test$pvalues<.05) # 1/3
summary(leniency.marital7$test$pvalues<.05) # 0/10
summary(leniency.children7$test$pvalues<.05) # 0/3
summary(leniency.occupation7$test$pvalues<.05) # 0/1
summary(leniency.age7$test$pvalues<.05) # 0/6
summary(leniency.pid8$test$pvalues<.05) # 0/3
summary(leniency.race8$test$pvalues<.05) # 0/15
summary(leniency.gender8$test$pvalues<.05) # 0/1
summary(leniency.residence8$test$pvalues<.05) # 1/3
summary(leniency.marital8$test$pvalues<.05) # 0/10
summary(leniency.children8$test$pvalues<.05) # 1/3
summary(leniency.occupation8$test$pvalues<.05) # 0/1
summary(leniency.age8$test$pvalues<.05) # 0/6
summary(leniency.pid9$test$pvalues<.05) # 2/3
summary(leniency.race9$test$pvalues<.05) # 0/15
summary(leniency.gender9$test$pvalues<.05) # 0/1
summary(leniency.residence9$test$pvalues<.05) # 0/3
summary(leniency.marital9$test$pvalues<.05) # 0/10
summary(leniency.children9$test$pvalues<.05) # 0/3
summary(leniency.occupation9$test$pvalues<.05) # 0/1
summary(leniency.age9$test$pvalues<.05) # 0/6
summary(leniency.pid10$test$pvalues<.05) # 1/3
summary(leniency.race10$test$pvalues<.05) # 0/15
summary(leniency.gender10$test$pvalues<.05) # 0/1
summary(leniency.residence10$test$pvalues<.05) # 0/3
summary(leniency.marital10$test$pvalues<.05) # 0/10
summary(leniency.children10$test$pvalues<.05) # 0/3
summary(leniency.occupation10$test$pvalues<.05) # 0/1
summary(leniency.age10$test$pvalues<.05) # 0/6
summary(leniency.pid11$test$pvalues<.05) # 2/3
summary(leniency.race11$test$pvalues<.05) # 0/15
summary(leniency.gender11$test$pvalues<.05) # 0/1
summary(leniency.residence11$test$pvalues<.05) # 0/3
summary(leniency.marital11$test$pvalues<.05) # 0/10
summary(leniency.children11$test$pvalues<.05) # 0/3
summary(leniency.occupation11$test$pvalues<.05) # 0/1
summary(leniency.age11$test$pvalues<.05) # 0/6
summary(leniency.pid12$test$pvalues<.05) # 0/3
summary(leniency.race12$test$pvalues<.05) # 0/15
summary(leniency.gender12$test$pvalues<.05) # 0/1
summary(leniency.residence12$test$pvalues<.05) # 1/3
summary(leniency.marital12$test$pvalues<.05) # 0/10
summary(leniency.children12$test$pvalues<.05) # 0/3
summary(leniency.occupation12$test$pvalues<.05) # 0/1
summary(leniency.age12$test$pvalues<.05) # 0/6
# Total: 21/504 (4.2%)

# Effects of identity holding target constant
leniency.protester <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_target)==1)
leniency.police <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_target)==2)
leniency.government <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_target)==3)
leniency.business <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_target)==4)

leniency.pidprotester <- summary(glht(leniency.protester, mcp(PID_Congruent = "Tukey")))
leniency.raceprotester <- summary(glht(leniency.protester, mcp(profile_race = "Tukey")))
leniency.genderprotester <- summary(glht(leniency.protester, mcp(profile_gender = "Tukey")))
leniency.residenceprotester <- summary(glht(leniency.protester, mcp(profile_residence_group = "Tukey")))
leniency.maritalprotester <- summary(glht(leniency.protester, mcp(profile_marital = "Tukey")))
leniency.childrenprotester <- summary(glht(leniency.protester, mcp(profile_children = "Tukey")))
leniency.occupationprotester <- summary(glht(leniency.protester, mcp(profile_occupation_group = "Tukey")))
leniency.ageprotester <- summary(glht(leniency.protester, mcp(profile_age = "Tukey")))
leniency.pidpolice <- summary(glht(leniency.police, mcp(PID_Congruent = "Tukey")))
leniency.racepolice <- summary(glht(leniency.police, mcp(profile_race = "Tukey")))
leniency.genderpolice <- summary(glht(leniency.police, mcp(profile_gender = "Tukey")))
leniency.residencepolice <- summary(glht(leniency.police, mcp(profile_residence_group = "Tukey")))
leniency.maritalpolice <- summary(glht(leniency.police, mcp(profile_marital = "Tukey")))
leniency.childrenpolice <- summary(glht(leniency.police, mcp(profile_children = "Tukey")))
leniency.occupationpolice <- summary(glht(leniency.police, mcp(profile_occupation_group = "Tukey")))
leniency.agepolice <- summary(glht(leniency.police, mcp(profile_age = "Tukey")))
leniency.pidgovernment <- summary(glht(leniency.government, mcp(PID_Congruent = "Tukey")))
leniency.racegovernment <- summary(glht(leniency.government, mcp(profile_race = "Tukey")))
leniency.gendergovernment <- summary(glht(leniency.government, mcp(profile_gender = "Tukey")))
leniency.residencegovernment <- summary(glht(leniency.government, mcp(profile_residence_group = "Tukey")))
leniency.maritalgovernment <- summary(glht(leniency.government, mcp(profile_marital = "Tukey")))
leniency.childrengovernment <- summary(glht(leniency.government, mcp(profile_children = "Tukey")))
leniency.occupationgovernment <- summary(glht(leniency.government, mcp(profile_occupation_group = "Tukey")))
leniency.agegovernment <- summary(glht(leniency.government, mcp(profile_age = "Tukey")))
leniency.pidbusiness <- summary(glht(leniency.business, mcp(PID_Congruent = "Tukey")))
leniency.racebusiness <- summary(glht(leniency.business, mcp(profile_race = "Tukey")))
leniency.genderbusiness <- summary(glht(leniency.business, mcp(profile_gender = "Tukey")))
leniency.residencebusiness <- summary(glht(leniency.business, mcp(profile_residence_group = "Tukey")))
leniency.maritalbusiness <- summary(glht(leniency.business, mcp(profile_marital = "Tukey")))
leniency.childrenbusiness <- summary(glht(leniency.business, mcp(profile_children = "Tukey")))
leniency.occupationbusiness <- summary(glht(leniency.business, mcp(profile_occupation_group = "Tukey")))
leniency.agebusiness <- summary(glht(leniency.business, mcp(profile_age = "Tukey")))

summary(leniency.pidprotester$test$pvalues<.05) # 3/3
summary(leniency.raceprotester$test$pvalues<.05) # 0/15
summary(leniency.genderprotester$test$pvalues<.05) # 0/1
summary(leniency.residenceprotester$test$pvalues<.05) # 0/3
summary(leniency.maritalprotester$test$pvalues<.05) # 0/10
summary(leniency.childrenprotester$test$pvalues<.05) # 0/3
summary(leniency.occupationprotester$test$pvalues<.05) # 0/1
summary(leniency.ageprotester$test$pvalues<.05) # 0/6
summary(leniency.pidpolice$test$pvalues<.05) # 2/3
summary(leniency.racepolice$test$pvalues<.05) # 0/15
summary(leniency.genderpolice$test$pvalues<.05) # 1/1
summary(leniency.residencepolice$test$pvalues<.05) # 2/3
summary(leniency.maritalpolice$test$pvalues<.05) # 0/10
summary(leniency.childrenpolice$test$pvalues<.05) # 0/3
summary(leniency.occupationpolice$test$pvalues<.05) # 0/1
summary(leniency.agepolice$test$pvalues<.05) # 0/6
summary(leniency.pidgovernment$test$pvalues<.05) # 3/3
summary(leniency.racegovernment$test$pvalues<.05) # 0/15
summary(leniency.gendergovernment$test$pvalues<.05) # 0/1
summary(leniency.residencegovernment$test$pvalues<.05) # 1/3
summary(leniency.maritalgovernment$test$pvalues<.05) # 0/10
summary(leniency.childrengovernment$test$pvalues<.05) # 0/3
summary(leniency.occupationgovernment$test$pvalues<.05) # 0/1
summary(leniency.agegovernment$test$pvalues<.05) # 0/6
summary(leniency.pidbusiness$test$pvalues<.05) # 2/3
summary(leniency.racebusiness$test$pvalues<.05) # 0/15
summary(leniency.genderbusiness$test$pvalues<.05) # 0/1
summary(leniency.residencebusiness$test$pvalues<.05) # 0/3
summary(leniency.maritalbusiness$test$pvalues<.05) # 0/10
summary(leniency.childrenbusiness$test$pvalues<.05) # 1/3
summary(leniency.occupationbusiness$test$pvalues<.05) # 0/1
summary(leniency.agebusiness$test$pvalues<.05) # 0/6
# Total: 15/168 (8.9%)

# Effects of identity holding severity constant
leniency.mild <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_severity)==1)
leniency.moderate <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_severity)==2)
leniency.severe <- lmer(decision~PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_severity)==3)

leniency.pidmild <- summary(glht(leniency.mild, mcp(PID_Congruent = "Tukey")))
leniency.racemild <- summary(glht(leniency.mild, mcp(profile_race = "Tukey")))
leniency.gendermild <- summary(glht(leniency.mild, mcp(profile_gender = "Tukey")))
leniency.residencemild <- summary(glht(leniency.mild, mcp(profile_residence_group = "Tukey")))
leniency.maritalmild <- summary(glht(leniency.mild, mcp(profile_marital = "Tukey")))
leniency.childrenmild <- summary(glht(leniency.mild, mcp(profile_children = "Tukey")))
leniency.occupationmild <- summary(glht(leniency.mild, mcp(profile_occupation_group = "Tukey")))
leniency.agemild <- summary(glht(leniency.mild, mcp(profile_age = "Tukey")))
leniency.pidmoderate <- summary(glht(leniency.moderate, mcp(PID_Congruent = "Tukey")))
leniency.racemoderate <- summary(glht(leniency.moderate, mcp(profile_race = "Tukey")))
leniency.gendermoderate <- summary(glht(leniency.moderate, mcp(profile_gender = "Tukey")))
leniency.residencemoderate <- summary(glht(leniency.moderate, mcp(profile_residence_group = "Tukey")))
leniency.maritalmoderate <- summary(glht(leniency.moderate, mcp(profile_marital = "Tukey")))
leniency.childrenmoderate <- summary(glht(leniency.moderate, mcp(profile_children = "Tukey")))
leniency.occupationmoderate <- summary(glht(leniency.moderate, mcp(profile_occupation_group = "Tukey")))
leniency.agemoderate <- summary(glht(leniency.moderate, mcp(profile_age = "Tukey")))
leniency.pidsevere <- summary(glht(leniency.severe, mcp(PID_Congruent = "Tukey")))
leniency.racesevere <- summary(glht(leniency.severe, mcp(profile_race = "Tukey")))
leniency.gendersevere <- summary(glht(leniency.severe, mcp(profile_gender = "Tukey")))
leniency.residencesevere <- summary(glht(leniency.severe, mcp(profile_residence_group = "Tukey")))
leniency.maritalsevere <- summary(glht(leniency.severe, mcp(profile_marital = "Tukey")))
leniency.childrensevere <- summary(glht(leniency.severe, mcp(profile_children = "Tukey")))
leniency.occupationsevere <- summary(glht(leniency.severe, mcp(profile_occupation_group = "Tukey")))
leniency.agesevere <- summary(glht(leniency.severe, mcp(profile_age = "Tukey")))

summary(leniency.pidmild$test$pvalues<.05) # 3/3
summary(leniency.racemild$test$pvalues<.05) # 1/15
summary(leniency.gendermild$test$pvalues<.05) # 1/1
summary(leniency.residencemild$test$pvalues<.05) # 1/3
summary(leniency.maritalmild$test$pvalues<.05) # 0/10
summary(leniency.childrenmild$test$pvalues<.05) # 0/3
summary(leniency.occupationmild$test$pvalues<.05) # 0/1
summary(leniency.agemild$test$pvalues<.05) # 0/6
summary(leniency.pidmoderate$test$pvalues<.05) # 3/3
summary(leniency.racemoderate$test$pvalues<.05) # 0/15
summary(leniency.gendermoderate$test$pvalues<.05) # 1/1
summary(leniency.residencemoderate$test$pvalues<.05) # 2/3
summary(leniency.maritalmoderate$test$pvalues<.05) # 0/10
summary(leniency.childrenmoderate$test$pvalues<.05) # 1/3
summary(leniency.occupationmoderate$test$pvalues<.05) # 0/1
summary(leniency.agemoderate$test$pvalues<.05) # 0/6
summary(leniency.pidsevere$test$pvalues<.05) # 2/3
summary(leniency.racesevere$test$pvalues<.05) # 0/15
summary(leniency.gendersevere$test$pvalues<.05) # 0/1
summary(leniency.residencesevere$test$pvalues<.05) # 1/3
summary(leniency.maritalsevere$test$pvalues<.05) # 0/10
summary(leniency.childrensevere$test$pvalues<.05) # 1/3
summary(leniency.occupationsevere$test$pvalues<.05) # 0/1
summary(leniency.agesevere$test$pvalues<.05) # 0/6
# Total: 17/126 (13.5%)

# Effects of act holding identity constant
### Hold constant party
leniency.partyconstant1 <- lmer(decision~act+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$PID_Congruent)==1)
leniency.partyconstant2 <- lmer(decision~act+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$PID_Congruent)==2)
leniency.partyconstant3 <- lmer(decision~act+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$PID_Congruent)==3)
pairwise.partyconstant1 <- summary(glht(leniency.partyconstant1, mcp(act = "Tukey")))
pairwise.partyconstant2 <- summary(glht(leniency.partyconstant2, mcp(act = "Tukey")))
pairwise.partyconstant3 <- summary(glht(leniency.partyconstant3, mcp(act = "Tukey")))
summary(pairwise.partyconstant1$test$pvalues<.05) # 53/66
summary(pairwise.partyconstant2$test$pvalues<.05) # 52/66
summary(pairwise.partyconstant3$test$pvalues<.05) # 48/66

### Hold constant race
leniency.raceconstant1 <- lmer(decision~act+ PID_Congruent+ profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_race)==1)
leniency.raceconstant2 <- lmer(decision~act+ PID_Congruent+ profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_race)==2)
leniency.raceconstant3 <- lmer(decision~act+ PID_Congruent+ profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_race)==3)
leniency.raceconstant4 <- lmer(decision~act+ PID_Congruent+ profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_race)==4)
leniency.raceconstant5 <- lmer(decision~act+ PID_Congruent+ profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_race)==5)
leniency.raceconstant6 <- lmer(decision~act+ PID_Congruent+ profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_race)==6)
pairwise.raceconstant1 <- summary(glht(leniency.raceconstant1, mcp(act = "Tukey")))
pairwise.raceconstant2 <- summary(glht(leniency.raceconstant2, mcp(act = "Tukey")))
pairwise.raceconstant3 <- summary(glht(leniency.raceconstant3, mcp(act = "Tukey")))
pairwise.raceconstant4 <- summary(glht(leniency.raceconstant4, mcp(act = "Tukey")))
pairwise.raceconstant5 <- summary(glht(leniency.raceconstant5, mcp(act = "Tukey")))
pairwise.raceconstant6 <- summary(glht(leniency.raceconstant6, mcp(act = "Tukey")))
summary(pairwise.raceconstant1$test$pvalues<.05) # 41/66
summary(pairwise.raceconstant2$test$pvalues<.05) # 46/66
summary(pairwise.raceconstant3$test$pvalues<.05) # 45/66
summary(pairwise.raceconstant4$test$pvalues<.05) # 45/66
summary(pairwise.raceconstant5$test$pvalues<.05) # 46/66
summary(pairwise.raceconstant6$test$pvalues<.05) # 44/66

### Hold constant gender
leniency.genderconstant1 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_gender))==1)
leniency.genderconstant2 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_children+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_gender))==2)
pairwise.genderconstant1 <- summary(glht(leniency.genderconstant1, mcp(act = "Tukey")))
pairwise.genderconstant2 <- summary(glht(leniency.genderconstant2, mcp(act = "Tukey")))
summary(pairwise.genderconstant1$test$pvalues<.05) # 54/66
summary(pairwise.genderconstant2$test$pvalues<.05) # 51/66

### Hold constant residence
leniency.residenceconstant1 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ (1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_residence_group))==1)
leniency.residenceconstant2 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ (1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_residence_group))==2)
leniency.residenceconstant3 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+profile_occupation_group+ (1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_residence_group))==3)
pairwise.residenceconstant1 <- summary(glht(leniency.residenceconstant1, mcp(act = "Tukey")))
pairwise.residenceconstant2 <- summary(glht(leniency.residenceconstant2, mcp(act = "Tukey")))
pairwise.residenceconstant3 <- summary(glht(leniency.residenceconstant3, mcp(act = "Tukey")))
summary(pairwise.residenceconstant1$test$pvalues<.05) # 52/66
summary(pairwise.residenceconstant2$test$pvalues<.05) # 51/66
summary(pairwise.residenceconstant3$test$pvalues<.05) # 48/66

### Hold constant marital status
leniency.maritalconstant1 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_marital)==1)
leniency.maritalconstant2 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_marital)==2)
leniency.maritalconstant3 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_marital)==3)
leniency.maritalconstant4 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_marital)==4)
leniency.maritalconstant5 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_marital)==5)
pairwise.maritalconstant1 <- summary(glht(leniency.maritalconstant1, mcp(act = "Tukey")))
pairwise.maritalconstant2 <- summary(glht(leniency.maritalconstant2, mcp(act = "Tukey")))
pairwise.maritalconstant3 <- summary(glht(leniency.maritalconstant3, mcp(act = "Tukey")))
pairwise.maritalconstant4 <- summary(glht(leniency.maritalconstant4, mcp(act = "Tukey")))
pairwise.maritalconstant5 <- summary(glht(leniency.maritalconstant5, mcp(act = "Tukey")))
summary(pairwise.maritalconstant1$test$pvalues<.05) # 46/66
summary(pairwise.maritalconstant2$test$pvalues<.05) # 46/66
summary(pairwise.maritalconstant3$test$pvalues<.05) # 48/66
summary(pairwise.maritalconstant4$test$pvalues<.05) # 43/66
summary(pairwise.maritalconstant5$test$pvalues<.05) # 46/66

### Hold constant # of children
leniency.childrenconstant1 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_children))==1)
leniency.childrenconstant2 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_children))==2)
leniency.childrenconstant3 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_marital+ profile_age+profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_children))==3)
pairwise.childrenconstant1 <- summary(glht(leniency.childrenconstant1, mcp(act = "Tukey")))
pairwise.childrenconstant2 <- summary(glht(leniency.childrenconstant2, mcp(act = "Tukey")))
pairwise.childrenconstant3 <- summary(glht(leniency.childrenconstant3, mcp(act = "Tukey")))
summary(pairwise.childrenconstant1$test$pvalues<.05) # 51/66
summary(pairwise.childrenconstant2$test$pvalues<.05) # 51/66
summary(pairwise.childrenconstant3$test$pvalues<.05) # 49/66

### Hold constant occupation
leniency.classconstant1 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_occupation_group))==1)
leniency.classconstant2 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_age+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(as.factor(PV$profile_occupation_group))==2)
pairwise.classconstant1 <- summary(glht(leniency.classconstant1, mcp(act = "Tukey")))
pairwise.classconstant2 <- summary(glht(leniency.classconstant2, mcp(act = "Tukey")))
summary(pairwise.classconstant1$test$pvalues<.05) # 54/66
summary(pairwise.classconstant2$test$pvalues<.05) # 51/66

### Hold constant age
leniency.ageconstant1 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_age)==1)
leniency.ageconstant2 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_age)==2)
leniency.ageconstant3 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_age)==3)
leniency.ageconstant4 <- lmer(decision~act+ PID_Congruent+ profile_race+profile_gender+ profile_children+ profile_marital+ profile_occupation_group+ profile_residence_group+(1 | ResponseId),data=PV,subset=as.numeric(PV$profile_age)==4)
pairwise.ageconstant1 <- summary(glht(leniency.ageconstant1, mcp(act = "Tukey")))
pairwise.ageconstant2 <- summary(glht(leniency.ageconstant2, mcp(act = "Tukey")))
pairwise.ageconstant3 <- summary(glht(leniency.ageconstant3, mcp(act = "Tukey")))
pairwise.ageconstant4 <- summary(glht(leniency.ageconstant4, mcp(act = "Tukey")))
summary(pairwise.ageconstant1$test$pvalues<.05) # 51/66
summary(pairwise.ageconstant2$test$pvalues<.05) # 47/66
summary(pairwise.ageconstant3$test$pvalues<.05) # 48/66
summary(pairwise.ageconstant4$test$pvalues<.05) # 50/66

# Total: 1357/1650 (82.2%)
