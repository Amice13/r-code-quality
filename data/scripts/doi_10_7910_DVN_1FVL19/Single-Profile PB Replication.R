# Load Necessary Packages
library(lme4)
library(ggplot2)
library(sjPlot)
library(estimatr)
library(plotrix)
library(dplyr)
library(ggpubr)
library(stargazer)
library(egg)
library(multcomp)
library(rmcorr)

# Load Necessary Packages
# SET YOUR WORKING DIRECTORY
data <- read.csv("Single-Profile Experiment.csv",header=T,sep=",")

# Code Data
## Outcome
data$sentence_ordinal <- ((data$choice_num+1)*12)-11
data$sentence_ordinal_factor <- as.factor(data$sentence_ordinal)
data$sentence_jail <- ifelse(data$sentence_ordinal_factor=="1" | data$sentence_ordinal_factor=="2",0,1)
data$sentence_years <- ifelse(data$sentence_ordinal_factor=="1",0,ifelse(data$sentence_ordinal_factor=="2",0,ifelse(data$sentence_ordinal_factor=="3",1/365,ifelse(data$sentence_ordinal_factor=="4",4/365,ifelse(data$sentence_ordinal_factor=="5",2/12,ifelse(data$sentence_ordinal_factor=="6",1/3,ifelse(data$sentence_ordinal_factor=="7",7/12,ifelse(data$sentence_ordinal_factor=="8",2,ifelse(data$sentence_ordinal_factor=="9",6,ifelse(data$sentence_ordinal_factor=="10",11,ifelse(data$sentence_ordinal_factor=="11",16,ifelse(data$sentence_ordinal_factor=="12",20,30))))))))))))
## Target
data$target <- factor(data$profile_target,levels=c("Counter-protester","Government","Police","Small business"))
## Severity
data$severity <- factor(data$profile_severity,levels=c("Low","Medium","High"))
## General Act
data$act <- factor(data$profile_act,levels=c(" Threatened to injure a counter-protester ", " Punched a counter-protester "," Used a weapon to severely injure a counter protester "," Threatened to injure a police officer "," Punched a police officer "," Used a weapon to severely injure a police officer "," Graffitied a nearby government building "," Caused significant damage to a nearby government building "," Burned down a nearby government building "," Graffitied a nearby small business "," Caused significant damage to a nearby small business "," Burned down a nearby small business "))
## Age
data$p_age <- factor(data$profile_age,levels=c("Mid twenties","Mid thirties","Mid forties","Mid fifties"))
## Children
data$children <- factor(data$profile_children,levels=c("None","One","Two"))
## Gender
data$p_gender <- factor(data$profile_gender,levels=c("Man","Woman"))
## Race
data$p_race <- factor(data$profile_race,levels=c("White","Black","Hispanic","Asian","Middle Eastern"))
## Marital
data$marital <- factor(data$profile_marital,levels=c("Single","Cohabitating with partner","Married","Divorced","Widowed"))
## Occupation Group
data$occupation_group <- factor(data$profile_occupation_group,levels=c("Blue Collar","White Collar"))
## Residence Group
data$residence_group <- factor(data$profile_residence_group,levels=c("Urban","Rural","Out of State"))
## Party Congruency
data$PID_congruency <- factor(ifelse(data$Party3=="Democrat" & data$profile_party=="Democrat","Congruent",ifelse(data$Party3=="Republican" & data$profile_party=="Republican","Congruent",ifelse(data$Party3=="Democrat" & data$profile_party=="Independent","Independent",ifelse(data$Party3=="Republican" & data$profile_party=="Independent","Independent",ifelse(data$Party3=="Democrat" & data$profile_party=="Republican","Incongruent",ifelse(data$Party3=="Republican" & data$profile_party=="Democrat","Incongruent",NA)))))),levels=c("Congruent","Independent","Incongruent"))
## Respondent Race
data$race <- factor(ifelse(data$hispanic=="Yes","Hispanic",data$Race.Ethnicity),levels=c("White","Black","Hispanic","Asian","Native American"))

# Table 1: Sample Characteristics, Single-Profile Experiment
summary(as.factor(data$Age))/5
summary(as.factor(data$Gender))/5
table(data$Race.Ethnicity,data$hispanic)/5 # Recovers % Hispanic and % White Non-Hispanic, Black/Asian/Native American/Native Hawaiian and Pacific Islander percentages include Hispanic identifiers
summary(as.factor(data$Party3))/5

# Footnote 4, Page 15
summary(as.factor(data$p_race))

# Description of Sanctions
summary(data$sentence_jail==1) # 75.7% of sanctions involve at least one day of jail
summary(data$sentence_years>=1) # 29.9% of sanctions involve at least one year of jail
mean(data$sentence_years,na.rm=T) # Mean sentence was 2.1 years
median(data$sentence_years,na.rm=T) # Median sentence was around 4 months

# Table A1
## Regressions
mlm_reg2.1 <- lmer(sentence_jail~target+severity+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data) # Model 1
mlm_reg3 <- lmer(sentence_years~target+severity+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data) # Model 2

# Percentage Jailed and Length of Sentence By Act
data %>% group_by(severity) %>% summarise(jail=mean(sentence_jail,na.rm=T)) # 55.4% of least severe acts get jail time, 25.1pp more likely for moderate acts, 36.8pp more likely for severe acts
data %>% group_by(severity) %>% summarise(years=mean(sentence_years,na.rm=T)) # Mild acts carried slightly less than 1 year in jail, moderately severe acts 1.4 years, and severe acts 4.1 years

# Figures 1 and 2, Top Panels/Table A3
## Regressions
mlm_reg2.1.act <- lmer(sentence_jail~act+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data) # Model 1
mlm_reg2.1.act.party <- lmer(sentence_jail~(act*Party3)+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data) # Model 2
mlm_reg3.act <- lmer(sentence_years~act+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data) # Model 3
mlm_reg3.act.party <- lmer(sentence_years~(act*Party3)+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data) # Model 4
## Figure 1
df.act <- data.frame(act=c(" Threatened to injure a counter-protester ", " Punched a counter-protester "," Used a weapon to severely injure a counter protester "," Threatened to injure a police officer "," Punched a police officer "," Used a weapon to severely injure a police officer "," Graffitied a nearby government building "," Caused significant damage to a nearby government building "," Burned down a nearby government building "," Graffitied a nearby small business "," Caused significant damage to a nearby small business "," Burned down a nearby small business "),
                     PID_congruency="Independent",
                     p_race="White",
                     p_gender="Man",
                     residence_group="Urban",
                     marital="Single",
                     children="Two",
                     occupation_group="Blue Collar",
                     p_age="Mid thirties")

## Likelihood of Jail
fig1.data.jail <- as.data.frame(predict(mlm_reg2.1.act,newdata=df.act,re.form= NA))
set.seed(3)
fig1.data.jail <- bootMer(mlm_reg2.1.act,
                          nsim = 1000,
                          FUN = function(x) { predict(x, newdata = df.act, re.form = NA) })
fig1.data.jail
fig1.data.jail <- data.frame(prob=c(0.6024148,0.7450451,0.9536100,0.7130345,0.8822374,0.9562773,0.5556295,0.8681024,0.9638613,0.4957398,0.8717568,0.9627600),
                             se=c(0.01604548,0.01574052,0.0168482,0.01606512,0.01671965,0.01657245,0.01641423,0.01618701,0.01685733,0.01611205,0.01628135,0.01625591))
fig1.data.jail$lci <- fig1.data.jail$prob-(1.96*fig1.data.jail$se)
fig1.data.jail$uci <- fig1.data.jail$prob+(1.96*fig1.data.jail$se)
fig1.data.jail$act <- factor(c("Threatened","Punched","Injured","Threatened","Punched","Injured","Graffitied","Damaged","Burned","Graffitied","Damaged","Burned"),levels=c("Burned","Damaged", "Graffitied","Injured","Punched","Threatened"))
fig1.data.jail$Target <- factor(c("Counter-Protester","Counter-Protester","Counter-Protester","Police","Police","Police","Government Building","Government Building","Government Building","Small Business","Small Business","Small Business"),levels=c("Counter-Protester","Police","Government Building","Small Business"))
fig1.data.jail$Type <- factor(c("People","People","People","People","People","People","Property","Property","Property","Property","Property","Property"),levels=c("People","Property"))
## Years in Jail
fig1.data.years <- as.data.frame(predict(mlm_reg3.act,newdata=df.act,re.form=NA))
set.seed(3)
fig1.data.years <- bootMer(mlm_reg3.act,
                           nsim = 1000,
                           FUN = function(x) { predict(x, newdata = df.act, re.form = NA) })
fig1.data.years
fig1.data.years <- data.frame(prob=c(0.8145792,0.9644340,3.6785821,1.3337105,1.5706547,5.3203910,0.7521958,1.8595690,4.3243460,0.7031262,1.4097900,3.4384957),
                              se=c(0.1848395,0.1814283,0.1939817,0.1851580,0.1920392,0.1905531,0.1891618,0.1862306,0.1939186,0.1855143,0.1869029,0.1871438))
fig1.data.years$lci <- fig1.data.years$prob-(1.96*fig1.data.years$se)
fig1.data.years$uci <- fig1.data.years$prob+(1.96*fig1.data.years$se)
fig1.data.years$act <- factor(c("Threatened","Punched","Injured","Threatened","Punched","Injured","Graffitied","Damaged","Burned","Graffitied","Damaged","Burned"),levels=c("Burned","Damaged", "Graffitied","Injured","Punched","Threatened"))
fig1.data.years$Target <- factor(c("Counter-Protester","Counter-Protester","Counter-Protester","Police","Police","Police","Government Building","Government Building","Government Building","Small Business","Small Business","Small Business"),levels=c("Counter-Protester","Police","Government Building","Small Business"))
fig1.data.years$Type <- factor(c("People","People","People","People","People","People","Property","Property","Property","Property","Property","Property"),levels=c("People","Property"))
## Plots
fig1.jail <- ggplot(fig1.data.jail,aes(x=act,y=prob,color=Target)) + geom_point(position=position_dodge(width = .5)) + theme_classic() + coord_flip() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(width = .5)) + xlab("") + facet_wrap(~Type,nrow=2,scale="free") + ylab("Probability of Jail") + ylim(0,1) + geom_hline(yintercept=0.5,linetype="dashed") + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
fig1.years <- ggplot(fig1.data.years,aes(x=act,y=prob,color=Target)) + geom_point(position=position_dodge(width = .5)) + theme_classic() + coord_flip() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(width = .5)) + xlab("") + facet_wrap(~Type,nrow=2,scale="free") + scale_y_continuous(name="Years in Jail",breaks=c(0,1,2,3,4,5,6),labels=c(0,1,2,3,4,5,6),limits=c(0,6)) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
fig1 <- ggpubr::ggarrange(fig1.jail,fig1.years,nrow=1,common.legend=T,legend="bottom")
ggsave("Single-Profile Act.png",fig1,device="png",width=8,height=6,units="in")
## Figure 2
### Likelihood of Jail
df.partyact <- data.frame(act=c(rep(c(" Threatened to injure a counter-protester ", " Punched a counter-protester "," Used a weapon to severely injure a counter protester "," Threatened to injure a police officer "," Punched a police officer "," Used a weapon to severely injure a police officer "," Graffitied a nearby government building "," Caused significant damage to a nearby government building "," Burned down a nearby government building "," Graffitied a nearby small business "," Caused significant damage to a nearby small business "," Burned down a nearby small business "),3)),
                          PID_congruency=c(rep("Congruent",12),rep("Independent",12),rep("Incongruent",12)))
fig3.data.jail <- as.data.frame(predict(mlm_reg2.1.party.act,newdata=df.partyact,re.form=NA))
set.seed(3)
#fig3.data.jail <- bootMer(mlm_reg2.1.party.act,
#                          nsim = 1000,
#                          FUN = function(x) { predict(x, newdata = df.partyact, re.form = NA) })
#fig3.data.jail
fig3.data.jail <- data.frame(prob=c(.5900366,.7114045,.9266711,.6684751,.8568287,.9141007,.5109392,.8178693,.9422505,.4609136,.8409207,.9214316,.5350884,.6896140,.9417862,.6684950,.8638513,.9321092,.5191829,.8444193,.9393604,.4311602,.8559823,.9605258,.6065141,.7601254,.9155040,.7285403,.8525001,.9427482,.5654359,.8649350,.9344189,.5157210,.8415902,.9252134),
                             se=c(.01858991,.01881838,.01815350,.01819415,.01768375,.01785471,.01771466,.01790417,.01799154,.01693780,.01802370,.01866984,.01881135,.01744372,.01834641,.01799691,.01756654,.01802899,.01779994,.01881212,.01726674,.01777293,.01873058,.01723165,.01900040,.01804668,.01793034,.01913172,.01768259,.01794068,.01843863,.01794382,.01834895,.01868221,0.01827848,.01863911))
fig3.data.jail$lci <- fig3.data.jail$prob-(1.96*fig3.data.jail$se)
fig3.data.jail$uci <- fig3.data.jail$prob+(1.96*fig3.data.jail$se)
fig3.data.jail$act <- factor(c(rep(c("Threatened Counter-Protester","Punched Counter-Protester","Injured Counter-Protester","Threatened Police","Punched Police","Injured Police","Graffitied Gov't","Damaged Gov't","Burned Gov't","Graffitied Business","Damaged Business","Burned Business"),3)),levels=c("Burned Business","Burned Gov't","Damaged Business","Damaged Gov't","Graffitied Business","Graffitied Gov't","Injured Counter-Protester","Injured Police","Punched Counter-Protester","Punched Police","Threatened Counter-Protester","Threatened Police"))
fig3.data.jail$Perpetrator <- factor(df.partyact$PID_congruency,levels=c("Congruent","Independent","Incongruent"))
### Years in Jail
fig3.data.years <- as.data.frame(predict(mlm_reg3.party.act,newdata=df.partyact,re.form=NA))
set.seed(3)
#fig3.data.years <- bootMer(mlm_reg3.party.act,
#                          nsim = 1000,
#                          FUN = function(x) { predict(x, newdata = df.partyact, re.form = NA) })
#fig3.data.years
fig3.data.years <- data.frame(prob=c(0.9820820,0.6473808,3.1777835,1.0913228,1.5829778,5.2327002,0.7535442,1.5435682,3.8128260,0.4846628,1.3361408,3.2185081,0.6333915,1.0997300,4.0514777,1.1708610,1.3877387,4.8094402,0.6032549,1.9216889,4.1259801,0.5835256,1.4708690,3.0952998,0.6539143,0.9533168,3.6757518,1.5799485,1.5930705,5.7532701,0.7704530,1.9643421,4.85843470,0.8646119,1.2649443,3.8882968),
                              se=c(0.2107621,0.2096413,0.2059607,0.2171308,0.2059576,0.2068319,0.2136990,0.2056404,0.2110343,0.2020531,0.1972464,0.2042956,0.2109916,0.1983839,0.2131319,0.2091411,0.2028581,0.1996783,0.2186981,0.2148852,0.2124998,0.2100903,0.2096884,0.2001820,0.2167740,0.2081039,0.2075474,0.2226654,0.1997466,0.2078510,0.2017316,0.2065535,0.2115140,0.2111491,0.2096163,0.2073058))
fig3.data.years$lci <- fig3.data.years$prob-(1.96*fig3.data.years$se)
fig3.data.years$uci <- fig3.data.years$prob+(1.96*fig3.data.years$se)
fig3.data.years$act <- factor(c(rep(c("Threatened Counter-Protester","Punched Counter-Protester","Injured Counter-Protester","Threatened Police","Punched Police","Injured Police","Graffitied Gov't","Damaged Gov't","Burned Gov't","Graffitied Business","Damaged Business","Burned Business"),3)),levels=c("Burned Business","Burned Gov't","Damaged Business","Damaged Gov't","Graffitied Business","Graffitied Gov't","Injured Counter-Protester","Injured Police","Punched Counter-Protester","Punched Police","Threatened Counter-Protester","Threatened Police"))
fig3.data.years$Perpetrator <- factor(df.partyact$PID_congruency,levels=c("Congruent","Independent","Incongruent"))
### Plots
fig3.jail <- ggplot(fig3.data.jail,aes(x=act,y=prob,fill=Perpetrator,group=Perpetrator)) + geom_bar(stat="identity",position="dodge") + theme_classic() + coord_flip() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0.5,position=position_dodge(0.9)) + xlab("Act") + ylab("Probability of Jail") + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
fig3.years <- ggplot(fig3.data.years,aes(x=act,y=prob,fill=Perpetrator,group=Perpetrator)) + geom_bar(stat="identity",position="dodge") + theme_classic() + coord_flip() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0.5,position=position_dodge(0.9)) + xlab("Act") + ylab("Years in Jail") + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
fig3 <- ggpubr::ggarrange(fig3.jail,fig3.years,ncol=2,common.legend=T,legend="bottom")
ggsave("Single-Profile Party Act.png",fig3,device="png",width=8,height=6,units="in")

figure3.data <- data.frame(prob=c(.01648,.03224,-.02764,.04359,-.02081,.01217,.03802,.03059,-.02431,.03833,-.01581,-.0127),
                           se_prob=c(.02551,.03567,.03539,.03647,.03552,.03558,.03569,.03554,.03604,.03543,.03561,.03605),
                           years=c(-.32817,.6341,.82614,.81679,.33826,.84874,.34508,.74894,1.37378,.70812,.25697,.99796),
                           se_years=c(.28792,.40263,.39941,.41175,.40089,.40179,.40294,.40123,.40673,.39988,.40195,.40691),
                           act=factor(c("Threatened","Punched","Injured","Threatened","Punched","Injured","Graffitied","Damaged","Burned","Graffitied","Damaged","Burned")),levels=c("Burned","Damaged","Graffitied","Injured","Punched","Threatened"),
                           Target=factor(c("Counter-Protester","Counter-Protester","Counter-Protester","Police","Police","Police","Government Building","Government Building","Government Building","Small Business","Small Business","Small Business"),levels=c("Counter-Protester","Police","Government Building","Small Business")),
                           type=factor(c("People","People","People","People","People","People","Property","Property","Property","Property","Property","Property"),levels=c("People","Property")))
figure3.data$lci_prob <- figure3.data$prob - (1.96*figure3.data$se_prob)
figure3.data$uci_prob <- figure3.data$prob + (1.96*figure3.data$se_prob)
figure3.data$lci_years <- figure3.data$years - (1.96*figure3.data$se_years)
figure3.data$uci_years <- figure3.data$years + (1.96*figure3.data$se_years)
figure3.data$sig_prob <- ifelse((figure3.data$lci_prob>0 & figure3.data$uci_prob>0) | (figure3.data$lci_prob<0 & figure3.data$uci_prob<0),TRUE,FALSE)
figure3.data$sig_years <- ifelse((figure3.data$lci_years>0 & figure3.data$uci_years>0) | (figure3.data$lci_years<0 & figure3.data$uci_years<0),TRUE,FALSE)
figure3.data$point_alpha <- 1

fig3a.jail <- ggplot(figure3.data,aes(x=act,y=prob,color=Target)) + geom_point(position=position_dodge(width = .5)) + theme_classic() + coord_flip() + geom_errorbar(aes(ymin=lci_prob,ymax=uci_prob,alpha=sig_prob),size=0.5,width=0,position=position_dodge(width = .5)) + xlab("") + facet_wrap(~type,nrow=2,scale="free") + ylab("Probability of Jail") + geom_hline(yintercept=0,linetype="dashed") + scale_alpha_manual(values=c(0.5,1),guide="none") + ylim(-0.1,0.12) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
fig3a.years <- ggplot(figure3.data,aes(x=act,y=years,color=Target)) + geom_point(position=position_dodge(width = .5)) + theme_classic() + coord_flip() + geom_errorbar(aes(ymin=lci_years,ymax=uci_years,alpha=sig_years),size=0.5,width=0,position=position_dodge(width = .5)) + xlab("") + facet_wrap(~type,nrow=2,scale="free") + ylab("Years in Jail") + geom_hline(yintercept=0,linetype="dashed") + scale_alpha_manual(values=c(0.5,1),guide="none") + ylim(-0.9,2.2) + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
figure3 <- ggpubr::ggarrange(fig3a.jail,fig3a.years,nrow=1,common.legend=T,legend="bottom")
ggsave("Single-Profile Party Act Coef.png",figure3,device="png",width=8,height=6,units="in")

# Table A5/Figure 3 (Top)
## Regressions
mlm_reg2.1.party1 <- lmer(sentence_jail~PID_congruency_1 + (1 | ResponseId),data=data) # Model 1
mlm_reg2.1.party.act <- lmer(sentence_jail~PID_congruency + act + (PID_congruency*act) + (1 | ResponseId),data=data) # Model 2
mlm_reg3.party1 <- lmer(sentence_years~PID_congruency_1 + (1 | ResponseId),data=data) # Model 3
mlm_reg3.party.act <- lmer(sentence_years~PID_congruency + act + (PID_congruency*act) + (1 | ResponseId),data=data) # Model 4
## Figure
### Likelihood of Jail
df.party <- data.frame(PID_congruency=c("Congruent","Independent","Incongruent"))
fig2.data.jail <- as.data.frame(predict(mlm_reg2.1.party,newdata=df.party,re.form=NA))
set.seed(3)
fig2.data.jail <- bootMer(mlm_reg2.1.party,
                          nsim = 1000,
                          FUN = function(x) { predict(x, newdata = df.party, re.form = NA) })
fig2.data.jail
fig2.data.jail <- data.frame(prob=c(.7620994,.7674079,.7894198),
                             se=c(.007287949,.007073221,.007355498))
fig2.data.jail$lci <- fig2.data.jail$prob-(1.96*fig2.data.jail$se)
fig2.data.jail$uci <- fig2.data.jail$prob+(1.96*fig2.data.jail$se)
fig2.data.jail$Perpetrator <- factor(c("Congruent","Independent","Incongruent"),levels=c("Congruent","Independent","Incongruent"))
### Years in Jail
fig2.data.years <- as.data.frame(predict(mlm_reg3.party,newdata=df.party,re.form=NA))
set.seed(3)
fig2.data.years <- bootMer(mlm_reg3.party,
                           nsim = 1000,
                           FUN = function(x) { predict(x, newdata = df.party, re.form = NA) })
fig2.data.years
fig2.data.years <- data.frame(prob=c(1.985056,2.090851,2.320213),
                              se=c(0.08467173,0.08657778,0.08550400))
fig2.data.years$lci <- fig2.data.years$prob-(1.96*fig2.data.years$se)
fig2.data.years$uci <- fig2.data.years$prob+(1.96*fig2.data.years$se)
fig2.data.years$Perpetrator <- factor(c("Congruent","Independent","Incongruent"),levels=c("Congruent","Independent","Incongruent"))
### Plots
fig2.jail <- ggplot(fig2.data.jail,aes(x=Perpetrator,y=prob)) + coord_flip() + geom_point() + theme_classic() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0) + xlab("") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Probability of Jail") + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Single-Profile")
fig2.years <- ggplot(fig2.data.years,aes(x=Perpetrator,y=prob)) + coord_flip() + geom_point() + theme_classic() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0) + xlab("") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Years in Jail") + theme(plot.title = element_text(hjust = 0.5))
fig2 <- ggpubr::ggarrange(fig2.jail,fig2.years,nrow=2)
ggsave("Single-Profile Party.png",fig2,device="png",width=6,height=4,units="in")

# Figure A1
## Underlying Regressions
mlm_reg2.1.race <- lmer(sentence_jail~p_race + race + (race*p_race) + (1 | ResponseId),data=data) # Likelihood of Jail
mlm_reg3.race <- lmer(sentence_years~p_race + race + (race*p_race) + (1 | ResponseId),data=data) # Years in Jail

# Pairwise Comparisons
## General Test
likelihood.act.pairwise <- summary(glht(mlm_reg2.1.act, mcp(act = "Tukey")))
likelihood.pid.pairwise <- summary(glht(mlm_reg2.1.act, mcp(PID_congruency = "Tukey")))
likelihood.race.pairwise <- summary(glht(mlm_reg2.1.act, mcp(p_race = "Tukey")))
likelihood.gender.pairwise <- summary(glht(mlm_reg2.1.act, mcp(p_gender = "Tukey")))
likelihood.residence.pairwise <- summary(glht(mlm_reg2.1.act, mcp(residence_group = "Tukey")))
likelihood.marital.pairwise <- summary(glht(mlm_reg2.1.act, mcp(marital = "Tukey")))
likelihood.children.pairwise <- summary(glht(mlm_reg2.1.act, mcp(children = "Tukey")))
likelihood.occupation.pairwise <- summary(glht(mlm_reg2.1.act, mcp(occupation_group = "Tukey")))
likelihood.age.pairwise <- summary(glht(mlm_reg2.1.act, mcp(p_age = "Tukey")))
summary(likelihood.act.pairwise$test$pvalues<.05) # 55/66 pairwise comparisons (83.3%)
summary(likelihood.pid.pairwise$test$pvalues<.05) # 2/3 pairwise comparisons
summary(likelihood.race.pairwise$test$pvalues<.05) # 3/10 pairwise comparisons
summary(likelihood.gender.pairwise$test$pvalues<.05) # 0/1 pairwise comparisons
summary(likelihood.residence.pairwise$test$pvalues<.05) # 0/3 pairwise comparisons
summary(likelihood.marital.pairwise$test$pvalues<.05) # 0/10 pairwise comparisons
summary(likelihood.children.pairwise$test$pvalues<.05) # 0/3 pairwise comparisons
summary(likelihood.occupation.pairwise$test$pvalues<.05) # 0/1 pairwise comparisons
summary(likelihood.age.pairwise$test$pvalues<.05) # 0/6 pairwise comparisons; Identity: 5/37 (13.5%)

years.act.pairwise <- summary(glht(mlm_reg3.act, mcp(act = "Tukey")))
years.pid.pairwise <- summary(glht(mlm_reg3.act, mcp(PID_congruency = "Tukey")))
years.race.pairwise <- summary(glht(mlm_reg3.act, mcp(p_race = "Tukey")))
years.gender.pairwise <- summary(glht(mlm_reg3.act, mcp(p_gender = "Tukey")))
years.residence.pairwise <- summary(glht(mlm_reg3.act, mcp(residence_group = "Tukey")))
years.marital.pairwise <- summary(glht(mlm_reg3.act, mcp(marital = "Tukey")))
years.children.pairwise <- summary(glht(mlm_reg3.act, mcp(children = "Tukey")))
years.occupation.pairwise <- summary(glht(mlm_reg3.act, mcp(occupation_group = "Tukey")))
years.age.pairwise <- summary(glht(mlm_reg3.act, mcp(p_age = "Tukey")))
summary(years.act.pairwise$test$pvalues<.05) # 50/66 pairwise comparisons (75.8%)
summary(years.pid.pairwise$test$pvalues<.05) # 2/3 pairwise comparisons
summary(years.race.pairwise$test$pvalues<.05) # 1/10 pairwise comparisons
summary(years.gender.pairwise$test$pvalues<.05) # 1/1 pairwise comparisons
summary(years.residence.pairwise$test$pvalues<.05) # 0/3 pairwise comparisons
summary(years.marital.pairwise$test$pvalues<.05) # 0/10 pairwise comparisons
summary(years.children.pairwise$test$pvalues<.05) # 0/3 pairwise comparisons
summary(years.occupation.pairwise$test$pvalues<.05) # 0/1 pairwise comparisons
summary(years.age.pairwise$test$pvalues<.05) # 0/6 pairwise comparisons; Identity: 4/37 (10.8%)
## Test holding each attribute constant
### Hold constant act
likelihood.threat_protester <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==1)
likelihood.punch_protester <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==2)
likelihood.injure_protester <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==3)
likelihood.threat_police <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==4)
likelihood.punch_police <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==5)
likelihood.injure_police <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==6)
likelihood.graffiti_govt <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==7)
likelihood.damage_govt <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==8)
likelihood.burn_govt <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==9)
likelihood.graffiti_business <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==10)
likelihood.damage_business <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==11)
likelihood.burn_business <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==12)
years.threat_protester <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==1)
years.punch_protester <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==2)
years.injure_protester <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==3)
years.threat_police <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==4)
years.punch_police <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==5)
years.injure_police <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==6)
years.graffiti_govt <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==7)
years.damage_govt <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==8)
years.burn_govt <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==9)
years.graffiti_business <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==10)
years.damage_business <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==11)
years.burn_business <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$act)==12)

likelihood.pid1 <- summary(glht(likelihood.threat_protester, mcp(PID_congruency = "Tukey")))
likelihood.race1 <- summary(glht(likelihood.threat_protester, mcp(p_race = "Tukey")))
likelihood.gender1 <- summary(glht(likelihood.threat_protester, mcp(p_gender = "Tukey")))
likelihood.residence1 <- summary(glht(likelihood.threat_protester, mcp(residence_group = "Tukey")))
likelihood.marital1 <- summary(glht(likelihood.threat_protester, mcp(marital = "Tukey")))
likelihood.children1 <- summary(glht(likelihood.threat_protester, mcp(children = "Tukey")))
likelihood.occupation1 <- summary(glht(likelihood.threat_protester, mcp(occupation_group = "Tukey")))
likelihood.age1 <- summary(glht(likelihood.threat_protester, mcp(p_age = "Tukey")))
likelihood.pid2 <- summary(glht(likelihood.punch_protester, mcp(PID_congruency = "Tukey")))
likelihood.race2 <- summary(glht(likelihood.punch_protester, mcp(p_race = "Tukey")))
likelihood.gender2 <- summary(glht(likelihood.punch_protester, mcp(p_gender = "Tukey")))
likelihood.residence2 <- summary(glht(likelihood.punch_protester, mcp(residence_group = "Tukey")))
likelihood.marital2 <- summary(glht(likelihood.punch_protester, mcp(marital = "Tukey")))
likelihood.children2 <- summary(glht(likelihood.punch_protester, mcp(children = "Tukey")))
likelihood.occupation2 <- summary(glht(likelihood.punch_protester, mcp(occupation_group = "Tukey")))
likelihood.age2 <- summary(glht(likelihood.punch_protester, mcp(p_age = "Tukey")))
likelihood.pid3 <- summary(glht(likelihood.injure_protester, mcp(PID_congruency = "Tukey")))
likelihood.race3 <- summary(glht(likelihood.injure_protester, mcp(p_race = "Tukey")))
likelihood.gender3 <- summary(glht(likelihood.injure_protester, mcp(p_gender = "Tukey")))
likelihood.residence3 <- summary(glht(likelihood.injure_protester, mcp(residence_group = "Tukey")))
likelihood.marital3 <- summary(glht(likelihood.injure_protester, mcp(marital = "Tukey")))
likelihood.children3 <- summary(glht(likelihood.injure_protester, mcp(children = "Tukey")))
likelihood.occupation3 <- summary(glht(likelihood.injure_protester, mcp(occupation_group = "Tukey")))
likelihood.age3 <- summary(glht(likelihood.injure_protester, mcp(p_age = "Tukey")))
likelihood.pid4 <- summary(glht(likelihood.threat_police, mcp(PID_congruency = "Tukey")))
likelihood.race4 <- summary(glht(likelihood.threat_police, mcp(p_race = "Tukey")))
likelihood.gender4 <- summary(glht(likelihood.threat_police, mcp(p_gender = "Tukey")))
likelihood.residence4 <- summary(glht(likelihood.threat_police, mcp(residence_group = "Tukey")))
likelihood.marital4 <- summary(glht(likelihood.threat_police, mcp(marital = "Tukey")))
likelihood.children4 <- summary(glht(likelihood.threat_police, mcp(children = "Tukey")))
likelihood.occupation4 <- summary(glht(likelihood.threat_police, mcp(occupation_group = "Tukey")))
likelihood.age4 <- summary(glht(likelihood.threat_police, mcp(p_age = "Tukey")))
likelihood.pid5 <- summary(glht(likelihood.punch_police, mcp(PID_congruency = "Tukey")))
likelihood.race5 <- summary(glht(likelihood.punch_police, mcp(p_race = "Tukey")))
likelihood.gender5 <- summary(glht(likelihood.punch_police, mcp(p_gender = "Tukey")))
likelihood.residence5 <- summary(glht(likelihood.punch_police, mcp(residence_group = "Tukey")))
likelihood.marital5 <- summary(glht(likelihood.punch_police, mcp(marital = "Tukey")))
likelihood.children5 <- summary(glht(likelihood.punch_police, mcp(children = "Tukey")))
likelihood.occupation5 <- summary(glht(likelihood.punch_police, mcp(occupation_group = "Tukey")))
likelihood.age5 <- summary(glht(likelihood.punch_police, mcp(p_age = "Tukey")))
likelihood.pid6 <- summary(glht(likelihood.injure_police, mcp(PID_congruency = "Tukey")))
likelihood.race6 <- summary(glht(likelihood.injure_police, mcp(p_race = "Tukey")))
likelihood.gender6 <- summary(glht(likelihood.injure_police, mcp(p_gender = "Tukey")))
likelihood.residence6 <- summary(glht(likelihood.injure_police, mcp(residence_group = "Tukey")))
likelihood.marital6 <- summary(glht(likelihood.injure_police, mcp(marital = "Tukey")))
likelihood.children6 <- summary(glht(likelihood.injure_police, mcp(children = "Tukey")))
likelihood.occupation6 <- summary(glht(likelihood.injure_police, mcp(occupation_group = "Tukey")))
likelihood.age6 <- summary(glht(likelihood.injure_police, mcp(p_age = "Tukey")))
likelihood.pid7 <- summary(glht(likelihood.graffiti_govt, mcp(PID_congruency = "Tukey")))
likelihood.race7 <- summary(glht(likelihood.graffiti_govt, mcp(p_race = "Tukey")))
likelihood.gender7 <- summary(glht(likelihood.graffiti_govt, mcp(p_gender = "Tukey")))
likelihood.residence7 <- summary(glht(likelihood.graffiti_govt, mcp(residence_group = "Tukey")))
likelihood.marital7 <- summary(glht(likelihood.graffiti_govt, mcp(marital = "Tukey")))
likelihood.children7 <- summary(glht(likelihood.graffiti_govt, mcp(children = "Tukey")))
likelihood.occupation7 <- summary(glht(likelihood.graffiti_govt, mcp(occupation_group = "Tukey")))
likelihood.age7 <- summary(glht(likelihood.graffiti_govt, mcp(p_age = "Tukey")))
likelihood.pid8 <- summary(glht(likelihood.damage_govt, mcp(PID_congruency = "Tukey")))
likelihood.race8 <- summary(glht(likelihood.damage_govt, mcp(p_race = "Tukey")))
likelihood.gender8 <- summary(glht(likelihood.damage_govt, mcp(p_gender = "Tukey")))
likelihood.residence8 <- summary(glht(likelihood.damage_govt, mcp(residence_group = "Tukey")))
likelihood.marital8 <- summary(glht(likelihood.damage_govt, mcp(marital = "Tukey")))
likelihood.children8 <- summary(glht(likelihood.damage_govt, mcp(children = "Tukey")))
likelihood.occupation8 <- summary(glht(likelihood.damage_govt, mcp(occupation_group = "Tukey")))
likelihood.age8 <- summary(glht(likelihood.damage_govt, mcp(p_age = "Tukey")))
likelihood.pid9 <- summary(glht(likelihood.burn_govt, mcp(PID_congruency = "Tukey")))
likelihood.race9 <- summary(glht(likelihood.burn_govt, mcp(p_race = "Tukey")))
likelihood.gender9 <- summary(glht(likelihood.burn_govt, mcp(p_gender = "Tukey")))
likelihood.residence9 <- summary(glht(likelihood.burn_govt, mcp(residence_group = "Tukey")))
likelihood.marital9 <- summary(glht(likelihood.burn_govt, mcp(marital = "Tukey")))
likelihood.children9 <- summary(glht(likelihood.burn_govt, mcp(children = "Tukey")))
likelihood.occupation9 <- summary(glht(likelihood.burn_govt, mcp(occupation_group = "Tukey")))
likelihood.age9 <- summary(glht(likelihood.burn_govt, mcp(p_age = "Tukey")))
likelihood.pid10 <- summary(glht(likelihood.graffiti_business, mcp(PID_congruency = "Tukey")))
likelihood.race10 <- summary(glht(likelihood.graffiti_business, mcp(p_race = "Tukey")))
likelihood.gender10 <- summary(glht(likelihood.graffiti_business, mcp(p_gender = "Tukey")))
likelihood.residence10 <- summary(glht(likelihood.graffiti_business, mcp(residence_group = "Tukey")))
likelihood.marital10 <- summary(glht(likelihood.graffiti_business, mcp(marital = "Tukey")))
likelihood.children10 <- summary(glht(likelihood.graffiti_business, mcp(children = "Tukey")))
likelihood.occupation10 <- summary(glht(likelihood.graffiti_business, mcp(occupation_group = "Tukey")))
likelihood.age10 <- summary(glht(likelihood.graffiti_business, mcp(p_age = "Tukey")))
likelihood.pid11 <- summary(glht(likelihood.damage_business, mcp(PID_congruency = "Tukey")))
likelihood.race11 <- summary(glht(likelihood.damage_business, mcp(p_race = "Tukey")))
likelihood.gender11 <- summary(glht(likelihood.damage_business, mcp(p_gender = "Tukey")))
likelihood.residence11 <- summary(glht(likelihood.damage_business, mcp(residence_group = "Tukey")))
likelihood.marital11 <- summary(glht(likelihood.damage_business, mcp(marital = "Tukey")))
likelihood.children11 <- summary(glht(likelihood.damage_business, mcp(children = "Tukey")))
likelihood.occupation11 <- summary(glht(likelihood.damage_business, mcp(occupation_group = "Tukey")))
likelihood.age11 <- summary(glht(likelihood.damage_business, mcp(p_age = "Tukey")))
likelihood.pid12 <- summary(glht(likelihood.burn_business, mcp(PID_congruency = "Tukey")))
likelihood.race12 <- summary(glht(likelihood.burn_business, mcp(p_race = "Tukey")))
likelihood.gender12 <- summary(glht(likelihood.burn_business, mcp(p_gender = "Tukey")))
likelihood.residence12 <- summary(glht(likelihood.burn_business, mcp(residence_group = "Tukey")))
likelihood.marital12 <- summary(glht(likelihood.burn_business, mcp(marital = "Tukey")))
likelihood.children12 <- summary(glht(likelihood.burn_business, mcp(children = "Tukey")))
likelihood.occupation12 <- summary(glht(likelihood.burn_business, mcp(occupation_group = "Tukey")))
likelihood.age12 <- summary(glht(likelihood.burn_business, mcp(p_age = "Tukey")))
years.pid1 <- summary(glht(years.threat_protester, mcp(PID_congruency = "Tukey")))
years.race1 <- summary(glht(years.threat_protester, mcp(p_race = "Tukey")))
years.gender1 <- summary(glht(years.threat_protester, mcp(p_gender = "Tukey")))
years.residence1 <- summary(glht(years.threat_protester, mcp(residence_group = "Tukey")))
years.marital1 <- summary(glht(years.threat_protester, mcp(marital = "Tukey")))
years.children1 <- summary(glht(years.threat_protester, mcp(children = "Tukey")))
years.occupation1 <- summary(glht(years.threat_protester, mcp(occupation_group = "Tukey")))
years.age1 <- summary(glht(years.threat_protester, mcp(p_age = "Tukey")))
years.pid2 <- summary(glht(years.punch_protester, mcp(PID_congruency = "Tukey")))
years.race2 <- summary(glht(years.punch_protester, mcp(p_race = "Tukey")))
years.gender2 <- summary(glht(years.punch_protester, mcp(p_gender = "Tukey")))
years.residence2 <- summary(glht(years.punch_protester, mcp(residence_group = "Tukey")))
years.marital2 <- summary(glht(years.punch_protester, mcp(marital = "Tukey")))
years.children2 <- summary(glht(years.punch_protester, mcp(children = "Tukey")))
years.occupation2 <- summary(glht(years.punch_protester, mcp(occupation_group = "Tukey")))
years.age2 <- summary(glht(years.punch_protester, mcp(p_age = "Tukey")))
years.pid3 <- summary(glht(years.injure_protester, mcp(PID_congruency = "Tukey")))
years.race3 <- summary(glht(years.injure_protester, mcp(p_race = "Tukey")))
years.gender3 <- summary(glht(years.injure_protester, mcp(p_gender = "Tukey")))
years.residence3 <- summary(glht(years.injure_protester, mcp(residence_group = "Tukey")))
years.marital3 <- summary(glht(years.injure_protester, mcp(marital = "Tukey")))
years.children3 <- summary(glht(years.injure_protester, mcp(children = "Tukey")))
years.occupation3 <- summary(glht(years.injure_protester, mcp(occupation_group = "Tukey")))
years.age3 <- summary(glht(years.injure_protester, mcp(p_age = "Tukey")))
years.pid4 <- summary(glht(years.threat_police, mcp(PID_congruency = "Tukey")))
years.race4 <- summary(glht(years.threat_police, mcp(p_race = "Tukey")))
years.gender4 <- summary(glht(years.threat_police, mcp(p_gender = "Tukey")))
years.residence4 <- summary(glht(years.threat_police, mcp(residence_group = "Tukey")))
years.marital4 <- summary(glht(years.threat_police, mcp(marital = "Tukey")))
years.children4 <- summary(glht(years.threat_police, mcp(children = "Tukey")))
years.occupation4 <- summary(glht(years.threat_police, mcp(occupation_group = "Tukey")))
years.age4 <- summary(glht(years.threat_police, mcp(p_age = "Tukey")))
years.pid5 <- summary(glht(years.punch_police, mcp(PID_congruency = "Tukey")))
years.race5 <- summary(glht(years.punch_police, mcp(p_race = "Tukey")))
years.gender5 <- summary(glht(years.punch_police, mcp(p_gender = "Tukey")))
years.residence5 <- summary(glht(years.punch_police, mcp(residence_group = "Tukey")))
years.marital5 <- summary(glht(years.punch_police, mcp(marital = "Tukey")))
years.children5 <- summary(glht(years.punch_police, mcp(children = "Tukey")))
years.occupation5 <- summary(glht(years.punch_police, mcp(occupation_group = "Tukey")))
years.age5 <- summary(glht(years.punch_police, mcp(p_age = "Tukey")))
years.pid6 <- summary(glht(years.injure_police, mcp(PID_congruency = "Tukey")))
years.race6 <- summary(glht(years.injure_police, mcp(p_race = "Tukey")))
years.gender6 <- summary(glht(years.injure_police, mcp(p_gender = "Tukey")))
years.residence6 <- summary(glht(years.injure_police, mcp(residence_group = "Tukey")))
years.marital6 <- summary(glht(years.injure_police, mcp(marital = "Tukey")))
years.children6 <- summary(glht(years.injure_police, mcp(children = "Tukey")))
years.occupation6 <- summary(glht(years.injure_police, mcp(occupation_group = "Tukey")))
years.age6 <- summary(glht(years.injure_police, mcp(p_age = "Tukey")))
years.pid7 <- summary(glht(years.graffiti_govt, mcp(PID_congruency = "Tukey")))
years.race7 <- summary(glht(years.graffiti_govt, mcp(p_race = "Tukey")))
years.gender7 <- summary(glht(years.graffiti_govt, mcp(p_gender = "Tukey")))
years.residence7 <- summary(glht(years.graffiti_govt, mcp(residence_group = "Tukey")))
years.marital7 <- summary(glht(years.graffiti_govt, mcp(marital = "Tukey")))
years.children7 <- summary(glht(years.graffiti_govt, mcp(children = "Tukey")))
years.occupation7 <- summary(glht(years.graffiti_govt, mcp(occupation_group = "Tukey")))
years.age7 <- summary(glht(years.graffiti_govt, mcp(p_age = "Tukey")))
years.pid8 <- summary(glht(years.damage_govt, mcp(PID_congruency = "Tukey")))
years.race8 <- summary(glht(years.damage_govt, mcp(p_race = "Tukey")))
years.gender8 <- summary(glht(years.damage_govt, mcp(p_gender = "Tukey")))
years.residence8 <- summary(glht(years.damage_govt, mcp(residence_group = "Tukey")))
years.marital8 <- summary(glht(years.damage_govt, mcp(marital = "Tukey")))
years.children8 <- summary(glht(years.damage_govt, mcp(children = "Tukey")))
years.occupation8 <- summary(glht(years.damage_govt, mcp(occupation_group = "Tukey")))
years.age8 <- summary(glht(years.damage_govt, mcp(p_age = "Tukey")))
years.pid9 <- summary(glht(years.burn_govt, mcp(PID_congruency = "Tukey")))
years.race9 <- summary(glht(years.burn_govt, mcp(p_race = "Tukey")))
years.gender9 <- summary(glht(years.burn_govt, mcp(p_gender = "Tukey")))
years.residence9 <- summary(glht(years.burn_govt, mcp(residence_group = "Tukey")))
years.marital9 <- summary(glht(years.burn_govt, mcp(marital = "Tukey")))
years.children9 <- summary(glht(years.burn_govt, mcp(children = "Tukey")))
years.occupation9 <- summary(glht(years.burn_govt, mcp(occupation_group = "Tukey")))
years.age9 <- summary(glht(years.burn_govt, mcp(p_age = "Tukey")))
years.pid10 <- summary(glht(years.graffiti_business, mcp(PID_congruency = "Tukey")))
years.race10 <- summary(glht(years.graffiti_business, mcp(p_race = "Tukey")))
years.gender10 <- summary(glht(years.graffiti_business, mcp(p_gender = "Tukey")))
years.residence10 <- summary(glht(years.graffiti_business, mcp(residence_group = "Tukey")))
years.marital10 <- summary(glht(years.graffiti_business, mcp(marital = "Tukey")))
years.children10 <- summary(glht(years.graffiti_business, mcp(children = "Tukey")))
years.occupation10 <- summary(glht(years.graffiti_business, mcp(occupation_group = "Tukey")))
years.age10 <- summary(glht(years.graffiti_business, mcp(p_age = "Tukey")))
years.pid11 <- summary(glht(years.damage_business, mcp(PID_congruency = "Tukey")))
years.race11 <- summary(glht(years.damage_business, mcp(p_race = "Tukey")))
years.gender11 <- summary(glht(years.damage_business, mcp(p_gender = "Tukey")))
years.residence11 <- summary(glht(years.damage_business, mcp(residence_group = "Tukey")))
years.marital11 <- summary(glht(years.damage_business, mcp(marital = "Tukey")))
years.children11 <- summary(glht(years.damage_business, mcp(children = "Tukey")))
years.occupation11 <- summary(glht(years.damage_business, mcp(occupation_group = "Tukey")))
years.age11 <- summary(glht(years.damage_business, mcp(p_age = "Tukey")))
years.pid12 <- summary(glht(years.burn_business, mcp(PID_congruency = "Tukey")))
years.race12 <- summary(glht(years.burn_business, mcp(p_race = "Tukey")))
years.gender12 <- summary(glht(years.burn_business, mcp(p_gender = "Tukey")))
years.residence12 <- summary(glht(years.burn_business, mcp(residence_group = "Tukey")))
years.marital12 <- summary(glht(years.burn_business, mcp(marital = "Tukey")))
years.children12 <- summary(glht(years.burn_business, mcp(children = "Tukey")))
years.occupation12 <- summary(glht(years.burn_business, mcp(occupation_group = "Tukey")))
years.age12 <- summary(glht(years.burn_business, mcp(p_age = "Tukey")))

summary(likelihood.pid1$test$pvalues<.05) # 0/3
summary(likelihood.race1$test$pvalues<.05) # 1/10
summary(likelihood.gender1$test$pvalues<.05) # 0/1
summary(likelihood.residence1$test$pvalues<.05) # 0/3
summary(likelihood.marital1$test$pvalues<.05) # 0/10
summary(likelihood.children1$test$pvalues<.05) # 0/3
summary(likelihood.occupation1$test$pvalues<.05) # 0/1
summary(likelihood.age1$test$pvalues<.05) # 0/6
summary(likelihood.pid2$test$pvalues<.05) # 0/3
summary(likelihood.race2$test$pvalues<.05) # 0/10
summary(likelihood.gender2$test$pvalues<.05) # 0/1
summary(likelihood.residence2$test$pvalues<.05) # 0/3
summary(likelihood.marital2$test$pvalues<.05) # 0/10
summary(likelihood.children2$test$pvalues<.05) # 0/3
summary(likelihood.occupation2$test$pvalues<.05) # 0/1
summary(likelihood.age2$test$pvalues<.05) # 0/6
summary(likelihood.pid3$test$pvalues<.05) # 0/3
summary(likelihood.race3$test$pvalues<.05) # 0/10
summary(likelihood.gender3$test$pvalues<.05) # 1/1
summary(likelihood.residence3$test$pvalues<.05) # 0/3
summary(likelihood.marital3$test$pvalues<.05) # 0/10
summary(likelihood.children3$test$pvalues<.05) # 0/3
summary(likelihood.occupation3$test$pvalues<.05) # 0/1
summary(likelihood.age3$test$pvalues<.05) # 0/6
summary(likelihood.pid4$test$pvalues<.05) # 0/3
summary(likelihood.race4$test$pvalues<.05) # 1/10
summary(likelihood.gender4$test$pvalues<.05) # 0/1
summary(likelihood.residence4$test$pvalues<.05) # 0/3
summary(likelihood.marital4$test$pvalues<.05) # 0/10
summary(likelihood.children4$test$pvalues<.05) # 0/3
summary(likelihood.occupation4$test$pvalues<.05) # 0/1
summary(likelihood.age4$test$pvalues<.05) # 0/6
summary(likelihood.pid5$test$pvalues<.05) # 0/3
summary(likelihood.race5$test$pvalues<.05) # 1/10
summary(likelihood.gender5$test$pvalues<.05) # 0/1
summary(likelihood.residence5$test$pvalues<.05) # 0/3
summary(likelihood.marital5$test$pvalues<.05) # 0/10
summary(likelihood.children5$test$pvalues<.05) # 0/3
summary(likelihood.occupation5$test$pvalues<.05) # 0/1
summary(likelihood.age5$test$pvalues<.05) # 0/6
summary(likelihood.pid6$test$pvalues<.05) # 2/3
summary(likelihood.race6$test$pvalues<.05) # 0/10
summary(likelihood.gender6$test$pvalues<.05) # 0/1
summary(likelihood.residence6$test$pvalues<.05) # 0/3
summary(likelihood.marital6$test$pvalues<.05) # 0/10
summary(likelihood.children6$test$pvalues<.05) # 0/3
summary(likelihood.occupation6$test$pvalues<.05) # 0/1
summary(likelihood.age6$test$pvalues<.05) # 0/6
summary(likelihood.pid7$test$pvalues<.05) # 0/3
summary(likelihood.race7$test$pvalues<.05) # 0/10
summary(likelihood.gender7$test$pvalues<.05) # 0/1
summary(likelihood.residence7$test$pvalues<.05) # 0/3
summary(likelihood.marital7$test$pvalues<.05) # 0/10
summary(likelihood.children7$test$pvalues<.05) # 0/3
summary(likelihood.occupation7$test$pvalues<.05) # 0/1
summary(likelihood.age7$test$pvalues<.05) # 0/6
summary(likelihood.pid8$test$pvalues<.05) # 0/3
summary(likelihood.race8$test$pvalues<.05) # 0/10
summary(likelihood.gender8$test$pvalues<.05) # 0/1
summary(likelihood.residence8$test$pvalues<.05) # 0/3
summary(likelihood.marital8$test$pvalues<.05) # 0/10
summary(likelihood.children8$test$pvalues<.05) # 0/3
summary(likelihood.occupation8$test$pvalues<.05) # 0/1
summary(likelihood.age8$test$pvalues<.05) # 0/6
summary(likelihood.pid9$test$pvalues<.05) # 0/3
summary(likelihood.race9$test$pvalues<.05) # 0/10
summary(likelihood.gender9$test$pvalues<.05) # 0/1
summary(likelihood.residence9$test$pvalues<.05) # 0/3
summary(likelihood.marital9$test$pvalues<.05) # 0/10
summary(likelihood.children9$test$pvalues<.05) # 0/3
summary(likelihood.occupation9$test$pvalues<.05) # 0/1
summary(likelihood.age9$test$pvalues<.05) # 0/6
summary(likelihood.pid10$test$pvalues<.05) # 2/3
summary(likelihood.race10$test$pvalues<.05) # 0/10
summary(likelihood.gender10$test$pvalues<.05) # 0/1
summary(likelihood.residence10$test$pvalues<.05) # 0/3
summary(likelihood.marital10$test$pvalues<.05) # 0/10
summary(likelihood.children10$test$pvalues<.05) # 0/3
summary(likelihood.occupation10$test$pvalues<.05) # 0/1
summary(likelihood.age10$test$pvalues<.05) # 0/6
summary(likelihood.pid11$test$pvalues<.05) # 0/3
summary(likelihood.race11$test$pvalues<.05) # 0/10
summary(likelihood.gender11$test$pvalues<.05) # 0/1
summary(likelihood.residence11$test$pvalues<.05) # 1/3
summary(likelihood.marital11$test$pvalues<.05) # 0/10
summary(likelihood.children11$test$pvalues<.05) # 0/3
summary(likelihood.occupation11$test$pvalues<.05) # 0/1
summary(likelihood.age11$test$pvalues<.05) # 0/6
summary(likelihood.pid12$test$pvalues<.05) # 1/3
summary(likelihood.race12$test$pvalues<.05) # 0/10
summary(likelihood.gender12$test$pvalues<.05) # 0/1
summary(likelihood.residence12$test$pvalues<.05) # 0/3
summary(likelihood.marital12$test$pvalues<.05) # 0/10
summary(likelihood.children12$test$pvalues<.05) # 0/3
summary(likelihood.occupation12$test$pvalues<.05) # 0/1
summary(likelihood.age12$test$pvalues<.05) # 0/6
# Total: 10/444 (2.3%)

summary(years.pid1$test$pvalues<.05) # 0/3
summary(years.race1$test$pvalues<.05) # 0/3
summary(years.gender1$test$pvalues<.05) # 1/10
summary(years.residence1$test$pvalues<.05) # 0/1
summary(years.marital1$test$pvalues<.05) # 0/3
summary(years.children1$test$pvalues<.05) # 0/10
summary(years.occupation1$test$pvalues<.05) # 0/1
summary(years.age1$test$pvalues<.05) # 0/6
summary(years.pid2$test$pvalues<.05) # 0/3
summary(years.race2$test$pvalues<.05) # 1/10
summary(years.gender2$test$pvalues<.05) # 0/1
summary(years.residence2$test$pvalues<.05) # 0/3
summary(years.marital2$test$pvalues<.05) # 0/10
summary(years.children2$test$pvalues<.05) # 0/3
summary(years.occupation2$test$pvalues<.05) # 0/1
summary(years.age2$test$pvalues<.05) # 0/6
summary(years.pid3$test$pvalues<.05) # 1/3
summary(years.race3$test$pvalues<.05) # 0/10
summary(years.gender3$test$pvalues<.05) # 0/1
summary(years.residence3$test$pvalues<.05) # 0/3
summary(years.marital3$test$pvalues<.05) # 0/10
summary(years.children3$test$pvalues<.05) # 1/3
summary(years.occupation3$test$pvalues<.05) # 0/1
summary(years.age3$test$pvalues<.05) # 0/6
summary(years.pid4$test$pvalues<.05) # 0/3
summary(years.race4$test$pvalues<.05) # 0/10
summary(years.gender4$test$pvalues<.05) # 0/1
summary(years.residence4$test$pvalues<.05) # 0/3
summary(years.marital4$test$pvalues<.05) # 2/10
summary(years.children4$test$pvalues<.05) # 0/3
summary(years.occupation4$test$pvalues<.05) # 0/1
summary(years.age4$test$pvalues<.05) # 0/6
summary(years.pid5$test$pvalues<.05) # 0/3
summary(years.race5$test$pvalues<.05) # 0/10
summary(years.gender5$test$pvalues<.05) # 0/1
summary(years.residence5$test$pvalues<.05) # 0/3
summary(years.marital5$test$pvalues<.05) # 0/10
summary(years.children5$test$pvalues<.05) # 0/3
summary(years.occupation5$test$pvalues<.05) # 0/1
summary(years.age5$test$pvalues<.05) # 0/6
summary(years.pid6$test$pvalues<.05) # 0/3
summary(years.race6$test$pvalues<.05) # 0/10
summary(years.gender6$test$pvalues<.05) # 1/1
summary(years.residence6$test$pvalues<.05) # 0/3
summary(years.marital6$test$pvalues<.05) # 0/10
summary(years.children6$test$pvalues<.05) # 0/3
summary(years.occupation6$test$pvalues<.05) # 0/1
summary(years.age6$test$pvalues<.05) # 0/6
summary(years.pid7$test$pvalues<.05) # 0/3
summary(years.race7$test$pvalues<.05) # 0/10
summary(years.gender7$test$pvalues<.05) # 0/1
summary(years.residence7$test$pvalues<.05) # 1/3
summary(years.marital7$test$pvalues<.05) # 0/10
summary(years.children7$test$pvalues<.05) # 1/3
summary(years.occupation7$test$pvalues<.05) # 0/1
summary(years.age7$test$pvalues<.05) # 0/6
summary(years.pid8$test$pvalues<.05) # 0/3
summary(years.race8$test$pvalues<.05) # 0/10
summary(years.gender8$test$pvalues<.05) # 0/1
summary(years.residence8$test$pvalues<.05) # 0/3
summary(years.marital8$test$pvalues<.05) # 0/10
summary(years.children8$test$pvalues<.05) # 0/3
summary(years.occupation8$test$pvalues<.05) # 0/1
summary(years.age8$test$pvalues<.05) # 0/6
summary(years.pid9$test$pvalues<.05) # 0/3
summary(years.race9$test$pvalues<.05) # 1/10
summary(years.gender9$test$pvalues<.05) # 0/1
summary(years.residence9$test$pvalues<.05) # 0/3
summary(years.marital9$test$pvalues<.05) # 0/10
summary(years.children9$test$pvalues<.05) # 0/3
summary(years.occupation9$test$pvalues<.05) # 0/1
summary(years.age9$test$pvalues<.05) # 0/6
summary(years.pid10$test$pvalues<.05) # 1/3
summary(years.race10$test$pvalues<.05) # 0/10
summary(years.gender10$test$pvalues<.05) # 0/1
summary(years.residence10$test$pvalues<.05) # 0/3
summary(years.marital10$test$pvalues<.05) # 0/10
summary(years.children10$test$pvalues<.05) # 0/3
summary(years.occupation10$test$pvalues<.05) # 0/1
summary(years.age10$test$pvalues<.05) # 0/6
summary(years.pid11$test$pvalues<.05) # 0/3
summary(years.race11$test$pvalues<.05) # 0/10
summary(years.gender11$test$pvalues<.05) # 0/1
summary(years.residence11$test$pvalues<.05) # 0/3
summary(years.marital11$test$pvalues<.05) # 0/10
summary(years.children11$test$pvalues<.05) # 0/3
summary(years.occupation11$test$pvalues<.05) # 0/1
summary(years.age11$test$pvalues<.05) # 1/6
summary(years.pid12$test$pvalues<.05) # 1/3
summary(years.race12$test$pvalues<.05) # 0/10
summary(years.gender12$test$pvalues<.05) # 0/1
summary(years.residence12$test$pvalues<.05) # 0/3
summary(years.marital12$test$pvalues<.05) # 0/10
summary(years.children12$test$pvalues<.05) # 0/3
summary(years.occupation12$test$pvalues<.05) # 0/1
summary(years.age12$test$pvalues<.05) # 0/6
# Total: 13/444 (2.9%)

### Holding Constant Act Type
likelihood.protester <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==1)
likelihood.police <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==3)
likelihood.government <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==2)
likelihood.business <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==4)

likelihood.pidprotester <- summary(glht(likelihood.protester, mcp(PID_congruency = "Tukey")))
likelihood.raceprotester <- summary(glht(likelihood.protester, mcp(p_race = "Tukey")))
likelihood.genderprotester <- summary(glht(likelihood.protester, mcp(p_gender = "Tukey")))
likelihood.residenceprotester <- summary(glht(likelihood.protester, mcp(residence_group = "Tukey")))
likelihood.maritalprotester <- summary(glht(likelihood.protester, mcp(marital = "Tukey")))
likelihood.childrenprotester <- summary(glht(likelihood.protester, mcp(children = "Tukey")))
likelihood.occupationprotester <- summary(glht(likelihood.protester, mcp(occupation_group = "Tukey")))
likelihood.ageprotester <- summary(glht(likelihood.protester, mcp(p_age = "Tukey")))
likelihood.pidpolice <- summary(glht(likelihood.police, mcp(PID_congruency = "Tukey")))
likelihood.racepolice <- summary(glht(likelihood.police, mcp(p_race = "Tukey")))
likelihood.genderpolice <- summary(glht(likelihood.police, mcp(p_gender = "Tukey")))
likelihood.residencepolice <- summary(glht(likelihood.police, mcp(residence_group = "Tukey")))
likelihood.maritalpolice <- summary(glht(likelihood.police, mcp(marital = "Tukey")))
likelihood.childrenpolice <- summary(glht(likelihood.police, mcp(children = "Tukey")))
likelihood.occupationpolice <- summary(glht(likelihood.police, mcp(occupation_group = "Tukey")))
likelihood.agepolice <- summary(glht(likelihood.police, mcp(p_age = "Tukey")))
likelihood.pidgovernment <- summary(glht(likelihood.government, mcp(PID_congruency = "Tukey")))
likelihood.racegovernment <- summary(glht(likelihood.government, mcp(p_race = "Tukey")))
likelihood.gendergovernment <- summary(glht(likelihood.government, mcp(p_gender = "Tukey")))
likelihood.residencegovernment <- summary(glht(likelihood.government, mcp(residence_group = "Tukey")))
likelihood.maritalgovernment <- summary(glht(likelihood.government, mcp(marital = "Tukey")))
likelihood.childrengovernment <- summary(glht(likelihood.government, mcp(children = "Tukey")))
likelihood.occupationgovernment <- summary(glht(likelihood.government, mcp(occupation_group = "Tukey")))
likelihood.agegovernment <- summary(glht(likelihood.government, mcp(p_age = "Tukey")))
likelihood.pidbusiness <- summary(glht(likelihood.business, mcp(PID_congruency = "Tukey")))
likelihood.racebusiness <- summary(glht(likelihood.business, mcp(p_race = "Tukey")))
likelihood.genderbusiness <- summary(glht(likelihood.business, mcp(p_gender = "Tukey")))
likelihood.residencebusiness <- summary(glht(likelihood.business, mcp(residence_group = "Tukey")))
likelihood.maritalbusiness <- summary(glht(likelihood.business, mcp(marital = "Tukey")))
likelihood.childrenbusiness <- summary(glht(likelihood.business, mcp(children = "Tukey")))
likelihood.occupationbusiness <- summary(glht(likelihood.business, mcp(occupation_group = "Tukey")))
likelihood.agebusiness <- summary(glht(likelihood.business, mcp(p_age = "Tukey")))

summary(likelihood.pidprotester$test$pvalues<.05) # 0/3
summary(likelihood.raceprotester$test$pvalues<.05) # 0/10
summary(likelihood.genderprotester$test$pvalues<.05) # 0/1
summary(likelihood.residenceprotester$test$pvalues<.05) # 0/3
summary(likelihood.maritalprotester$test$pvalues<.05) # 0/10
summary(likelihood.childrenprotester$test$pvalues<.05) # 0/3
summary(likelihood.occupationprotester$test$pvalues<.05) # 0/1
summary(likelihood.ageprotester$test$pvalues<.05) # 0/6
summary(likelihood.pidpolice$test$pvalues<.05) # 2/3
summary(likelihood.racepolice$test$pvalues<.05) # 0/10
summary(likelihood.genderpolice$test$pvalues<.05) # 0/1
summary(likelihood.residencepolice$test$pvalues<.05) # 0/3
summary(likelihood.maritalpolice$test$pvalues<.05) # 0/10
summary(likelihood.childrenpolice$test$pvalues<.05) # 0/3
summary(likelihood.occupationpolice$test$pvalues<.05) # 0/1
summary(likelihood.agepolice$test$pvalues<.05) # 1/6
summary(likelihood.pidgovernment$test$pvalues<.05) # 0/3
summary(likelihood.racegovernment$test$pvalues<.05) # 0/10
summary(likelihood.gendergovernment$test$pvalues<.05) # 0/1
summary(likelihood.residencegovernment$test$pvalues<.05) # 0/3
summary(likelihood.maritalgovernment$test$pvalues<.05) # 0/10
summary(likelihood.childrengovernment$test$pvalues<.05) # 0/3
summary(likelihood.occupationgovernment$test$pvalues<.05) # 0/1
summary(likelihood.agegovernment$test$pvalues<.05) # 0/6
summary(likelihood.pidbusiness$test$pvalues<.05) # 0/3
summary(likelihood.racebusiness$test$pvalues<.05) # 1/10
summary(likelihood.genderbusiness$test$pvalues<.05) # 0/1
summary(likelihood.residencebusiness$test$pvalues<.05) # 0/3
summary(likelihood.maritalbusiness$test$pvalues<.05) # 0/10
summary(likelihood.childrenbusiness$test$pvalues<.05) # 0/3
summary(likelihood.occupationbusiness$test$pvalues<.05) # 0/1
summary(likelihood.agebusiness$test$pvalues<.05) # 0/6
# Total: 4/148 (2.7%)

years.protester <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==1)
years.police <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==3)
years.government <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==2)
years.business <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$target)==4)

years.pidprotester <- summary(glht(years.protester, mcp(PID_congruency = "Tukey")))
years.raceprotester <- summary(glht(years.protester, mcp(p_race = "Tukey")))
years.genderprotester <- summary(glht(years.protester, mcp(p_gender = "Tukey")))
years.residenceprotester <- summary(glht(years.protester, mcp(residence_group = "Tukey")))
years.maritalprotester <- summary(glht(years.protester, mcp(marital = "Tukey")))
years.childrenprotester <- summary(glht(years.protester, mcp(children = "Tukey")))
years.occupationprotester <- summary(glht(years.protester, mcp(occupation_group = "Tukey")))
years.ageprotester <- summary(glht(years.protester, mcp(p_age = "Tukey")))
years.pidpolice <- summary(glht(years.police, mcp(PID_congruency = "Tukey")))
years.racepolice <- summary(glht(years.police, mcp(p_race = "Tukey")))
years.genderpolice <- summary(glht(years.police, mcp(p_gender = "Tukey")))
years.residencepolice <- summary(glht(years.police, mcp(residence_group = "Tukey")))
years.maritalpolice <- summary(glht(years.police, mcp(marital = "Tukey")))
years.childrenpolice <- summary(glht(years.police, mcp(children = "Tukey")))
years.occupationpolice <- summary(glht(years.police, mcp(occupation_group = "Tukey")))
years.agepolice <- summary(glht(years.police, mcp(p_age = "Tukey")))
years.pidgovernment <- summary(glht(years.government, mcp(PID_congruency = "Tukey")))
years.racegovernment <- summary(glht(years.government, mcp(p_race = "Tukey")))
years.gendergovernment <- summary(glht(years.government, mcp(p_gender = "Tukey")))
years.residencegovernment <- summary(glht(years.government, mcp(residence_group = "Tukey")))
years.maritalgovernment <- summary(glht(years.government, mcp(marital = "Tukey")))
years.childrengovernment <- summary(glht(years.government, mcp(children = "Tukey")))
years.occupationgovernment <- summary(glht(years.government, mcp(occupation_group = "Tukey")))
years.agegovernment <- summary(glht(years.government, mcp(p_age = "Tukey")))
years.pidbusiness <- summary(glht(years.business, mcp(PID_congruency = "Tukey")))
years.racebusiness <- summary(glht(years.business, mcp(p_race = "Tukey")))
years.genderbusiness <- summary(glht(years.business, mcp(p_gender = "Tukey")))
years.residencebusiness <- summary(glht(years.business, mcp(residence_group = "Tukey")))
years.maritalbusiness <- summary(glht(years.business, mcp(marital = "Tukey")))
years.childrenbusiness <- summary(glht(years.business, mcp(children = "Tukey")))
years.occupationbusiness <- summary(glht(years.business, mcp(occupation_group = "Tukey")))
years.agebusiness <- summary(glht(years.business, mcp(p_age = "Tukey")))

summary(years.pidprotester$test$pvalues<.05) # 0/3
summary(years.raceprotester$test$pvalues<.05) # 2/10
summary(years.genderprotester$test$pvalues<.05) # 0/1
summary(years.residenceprotester$test$pvalues<.05) # 0/3
summary(years.maritalprotester$test$pvalues<.05) # 0/10
summary(years.childrenprotester$test$pvalues<.05) # 0/3
summary(years.occupationprotester$test$pvalues<.05) # 0/1
summary(years.ageprotester$test$pvalues<.05) # 0/6
summary(years.pidpolice$test$pvalues<.05) # 1/3
summary(years.racepolice$test$pvalues<.05) # 0/10
summary(years.genderpolice$test$pvalues<.05) # 0/1
summary(years.residencepolice$test$pvalues<.05) # 0/3
summary(years.maritalpolice$test$pvalues<.05) # 0/10
summary(years.childrenpolice$test$pvalues<.05) # 0/3
summary(years.occupationpolice$test$pvalues<.05) # 0/1
summary(years.agepolice$test$pvalues<.05) # 0/6
summary(years.pidgovernment$test$pvalues<.05) # 0/3
summary(years.racegovernment$test$pvalues<.05) # 0/10
summary(years.gendergovernment$test$pvalues<.05) # 0/1
summary(years.residencegovernment$test$pvalues<.05) # 0/3
summary(years.maritalgovernment$test$pvalues<.05) # 0/10
summary(years.childrengovernment$test$pvalues<.05) # 0/3
summary(years.occupationgovernment$test$pvalues<.05) # 0/1
summary(years.agegovernment$test$pvalues<.05) # 0/6
summary(years.pidbusiness$test$pvalues<.05) # 0/3
summary(years.racebusiness$test$pvalues<.05) # 0/10
summary(years.genderbusiness$test$pvalues<.05) # 0/1
summary(years.residencebusiness$test$pvalues<.05) # 0/3
summary(years.maritalbusiness$test$pvalues<.05) # 2/10
summary(years.childrenbusiness$test$pvalues<.05) # 0/3
summary(years.occupationbusiness$test$pvalues<.05) # 0/1
summary(years.agebusiness$test$pvalues<.05) # 1/6
# Total: 6/148 (4.1%)

### Holding Constant Act Severity
likelihood.mild <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$severity)==1)
likelihood.moderate <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$severity)==3)
likelihood.severe <- lmer(sentence_jail~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$severity)==2)
years.mild <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$severity)==1)
years.moderate <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$severity)==3)
years.severe <- lmer(sentence_years~PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$severity)==2)

likelihood.pidmild <- summary(glht(likelihood.mild, mcp(PID_congruency = "Tukey")))
likelihood.racemild <- summary(glht(likelihood.mild, mcp(p_race = "Tukey")))
likelihood.gendermild <- summary(glht(likelihood.mild, mcp(p_gender = "Tukey")))
likelihood.residencemild <- summary(glht(likelihood.mild, mcp(residence_group = "Tukey")))
likelihood.maritalmild <- summary(glht(likelihood.mild, mcp(marital = "Tukey")))
likelihood.childrenmild <- summary(glht(likelihood.mild, mcp(children = "Tukey")))
likelihood.occupationmild <- summary(glht(likelihood.mild, mcp(occupation_group = "Tukey")))
likelihood.agemild <- summary(glht(likelihood.mild, mcp(p_age = "Tukey")))
likelihood.pidmoderate <- summary(glht(likelihood.moderate, mcp(PID_congruency = "Tukey")))
likelihood.racemoderate <- summary(glht(likelihood.moderate, mcp(p_race = "Tukey")))
likelihood.gendermoderate <- summary(glht(likelihood.moderate, mcp(p_gender = "Tukey")))
likelihood.residencemoderate <- summary(glht(likelihood.moderate, mcp(residence_group = "Tukey")))
likelihood.maritalmoderate <- summary(glht(likelihood.moderate, mcp(marital = "Tukey")))
likelihood.childrenmoderate <- summary(glht(likelihood.moderate, mcp(children = "Tukey")))
likelihood.occupationmoderate <- summary(glht(likelihood.moderate, mcp(occupation_group = "Tukey")))
likelihood.agemoderate <- summary(glht(likelihood.moderate, mcp(p_age = "Tukey")))
likelihood.pidsevere <- summary(glht(likelihood.severe, mcp(PID_congruency = "Tukey")))
likelihood.racesevere <- summary(glht(likelihood.severe, mcp(p_race = "Tukey")))
likelihood.gendersevere <- summary(glht(likelihood.severe, mcp(p_gender = "Tukey")))
likelihood.residencesevere <- summary(glht(likelihood.severe, mcp(residence_group = "Tukey")))
likelihood.maritalsevere <- summary(glht(likelihood.severe, mcp(marital = "Tukey")))
likelihood.childrensevere <- summary(glht(likelihood.severe, mcp(children = "Tukey")))
likelihood.occupationsevere <- summary(glht(likelihood.severe, mcp(occupation_group = "Tukey")))
likelihood.agesevere <- summary(glht(likelihood.severe, mcp(p_age = "Tukey")))
years.pidmild <- summary(glht(years.mild, mcp(PID_congruency = "Tukey")))
years.racemild <- summary(glht(years.mild, mcp(p_race = "Tukey")))
years.gendermild <- summary(glht(years.mild, mcp(p_gender = "Tukey")))
years.residencemild <- summary(glht(years.mild, mcp(residence_group = "Tukey")))
years.maritalmild <- summary(glht(years.mild, mcp(marital = "Tukey")))
years.childrenmild <- summary(glht(years.mild, mcp(children = "Tukey")))
years.occupationmild <- summary(glht(years.mild, mcp(occupation_group = "Tukey")))
years.agemild <- summary(glht(years.mild, mcp(p_age = "Tukey")))
years.pidmoderate <- summary(glht(years.moderate, mcp(PID_congruency = "Tukey")))
years.racemoderate <- summary(glht(years.moderate, mcp(p_race = "Tukey")))
years.gendermoderate <- summary(glht(years.moderate, mcp(p_gender = "Tukey")))
years.residencemoderate <- summary(glht(years.moderate, mcp(residence_group = "Tukey")))
years.maritalmoderate <- summary(glht(years.moderate, mcp(marital = "Tukey")))
years.childrenmoderate <- summary(glht(years.moderate, mcp(children = "Tukey")))
years.occupationmoderate <- summary(glht(years.moderate, mcp(occupation_group = "Tukey")))
years.agemoderate <- summary(glht(years.moderate, mcp(p_age = "Tukey")))
years.pidsevere <- summary(glht(years.severe, mcp(PID_congruency = "Tukey")))
years.racesevere <- summary(glht(years.severe, mcp(p_race = "Tukey")))
years.gendersevere <- summary(glht(years.severe, mcp(p_gender = "Tukey")))
years.residencesevere <- summary(glht(years.severe, mcp(residence_group = "Tukey")))
years.maritalsevere <- summary(glht(years.severe, mcp(marital = "Tukey")))
years.childrensevere <- summary(glht(years.severe, mcp(children = "Tukey")))
years.occupationsevere <- summary(glht(years.severe, mcp(occupation_group = "Tukey")))
years.agesevere <- summary(glht(years.severe, mcp(p_age = "Tukey")))

summary(likelihood.pidmild$test$pvalues<.05) # 2/3
summary(likelihood.racemild$test$pvalues<.05) # 0/10
summary(likelihood.gendermild$test$pvalues<.05) # 0/1
summary(likelihood.residencemild$test$pvalues<.05) # 0/3
summary(likelihood.maritalmild$test$pvalues<.05) # 0/10
summary(likelihood.childrenmild$test$pvalues<.05) # 0/3
summary(likelihood.occupationmild$test$pvalues<.05) # 0/1
summary(likelihood.agemild$test$pvalues<.05) # 0/6
summary(likelihood.pidmoderate$test$pvalues<.05) # 1/3
summary(likelihood.racemoderate$test$pvalues<.05) # 0/10
summary(likelihood.gendermoderate$test$pvalues<.05) # 0/1
summary(likelihood.residencemoderate$test$pvalues<.05) # 0/3
summary(likelihood.maritalmoderate$test$pvalues<.05) # 0/10
summary(likelihood.childrenmoderate$test$pvalues<.05) # 0/3
summary(likelihood.occupationmoderate$test$pvalues<.05) # 0/1
summary(likelihood.agemoderate$test$pvalues<.05) # 0/6
summary(likelihood.pidsevere$test$pvalues<.05) # 0/3
summary(likelihood.racesevere$test$pvalues<.05) # 0/10
summary(likelihood.gendersevere$test$pvalues<.05) # 0/1
summary(likelihood.residencesevere$test$pvalues<.05) # 0/3
summary(likelihood.maritalsevere$test$pvalues<.05) # 0/10
summary(likelihood.childrensevere$test$pvalues<.05) # 0/3
summary(likelihood.occupationsevere$test$pvalues<.05) # 0/1
summary(likelihood.agesevere$test$pvalues<.05) # 0/6
# Total: 3/111 (2.7%)

summary(years.pidmild$test$pvalues<.05) # 0/3
summary(years.racemild$test$pvalues<.05) # 0/10
summary(years.gendermild$test$pvalues<.05) # 0/1
summary(years.residencemild$test$pvalues<.05) # 0/3
summary(years.maritalmild$test$pvalues<.05) # 0/10
summary(years.childrenmild$test$pvalues<.05) # 0/3
summary(years.occupationmild$test$pvalues<.05) # 0/1
summary(years.agemild$test$pvalues<.05) # 0/6
summary(years.pidmoderate$test$pvalues<.05) # 2/3
summary(years.racemoderate$test$pvalues<.05) # 2/10
summary(years.gendermoderate$test$pvalues<.05) # 0/1
summary(years.residencemoderate$test$pvalues<.05) # 0/3
summary(years.maritalmoderate$test$pvalues<.05) # 0/10
summary(years.childrenmoderate$test$pvalues<.05) # 1/3
summary(years.occupationmoderate$test$pvalues<.05) # 0/1
summary(years.agemoderate$test$pvalues<.05) # 2/6
summary(years.pidsevere$test$pvalues<.05) # 0/3
summary(years.racesevere$test$pvalues<.05) # 0/10
summary(years.gendersevere$test$pvalues<.05) # 0/1
summary(years.residencesevere$test$pvalues<.05) # 0/3
summary(years.maritalsevere$test$pvalues<.05) # 0/10
summary(years.childrensevere$test$pvalues<.05) # 0/3
summary(years.occupationsevere$test$pvalues<.05) # 0/1
summary(years.agesevere$test$pvalues<.05) # 0/6
# Total: 7/111 (6.3%)

### Hold constant party
likelihood.partyconstant1 <- lmer(sentence_jail~act+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$PID_congruency)==1)
likelihood.partyconstant2 <- lmer(sentence_jail~act+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$PID_congruency)==2)
likelihood.partyconstant3 <- lmer(sentence_jail~act+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$PID_congruency)==3)
years.partyconstant1 <- lmer(sentence_years~act+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$PID_congruency)==1)
years.partyconstant2 <- lmer(sentence_years~act+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$PID_congruency)==2)
years.partyconstant3 <- lmer(sentence_years~act+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$PID_congruency)==3)
pairwise.partyconstant1l <- summary(glht(likelihood.partyconstant1, mcp(act = "Tukey")))
pairwise.partyconstant2l <- summary(glht(likelihood.partyconstant2, mcp(act = "Tukey")))
pairwise.partyconstant3l <- summary(glht(likelihood.partyconstant3, mcp(act = "Tukey")))
pairwise.partyconstant1y <- summary(glht(years.partyconstant1, mcp(act = "Tukey")))
pairwise.partyconstant2y <- summary(glht(years.partyconstant2, mcp(act = "Tukey")))
pairwise.partyconstant3y <- summary(glht(years.partyconstant3, mcp(act = "Tukey")))
summary(pairwise.partyconstant1l$test$pvalues<.05) # 48/66
summary(pairwise.partyconstant2l$test$pvalues<.05) # 48/66
summary(pairwise.partyconstant3l$test$pvalues<.05) # 45/66

summary(pairwise.partyconstant1y$test$pvalues<.05) # 37/66
summary(pairwise.partyconstant2y$test$pvalues<.05) # 38/66
summary(pairwise.partyconstant3y$test$pvalues<.05) # 40/66
### Hold constant race
likelihood.raceconstant1 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==1)
likelihood.raceconstant2 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==2)
likelihood.raceconstant3 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==3)
likelihood.raceconstant4 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==4)
likelihood.raceconstant5 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==5)
years.raceconstant1 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==1)
years.raceconstant2 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==2)
years.raceconstant3 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==3)
years.raceconstant4 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==4)
years.raceconstant5 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_race)==5)
pairwise.raceconstant1l <- summary(glht(likelihood.raceconstant1, mcp(act = "Tukey")))
pairwise.raceconstant2l <- summary(glht(likelihood.raceconstant2, mcp(act = "Tukey")))
pairwise.raceconstant3l <- summary(glht(likelihood.raceconstant3, mcp(act = "Tukey")))
pairwise.raceconstant4l <- summary(glht(likelihood.raceconstant4, mcp(act = "Tukey")))
pairwise.raceconstant5l <- summary(glht(likelihood.raceconstant5, mcp(act = "Tukey")))
pairwise.raceconstant1y <- summary(glht(years.raceconstant1, mcp(act = "Tukey")))
pairwise.raceconstant2y <- summary(glht(years.raceconstant2, mcp(act = "Tukey")))
pairwise.raceconstant3y <- summary(glht(years.raceconstant3, mcp(act = "Tukey")))
pairwise.raceconstant4y <- summary(glht(years.raceconstant4, mcp(act = "Tukey")))
pairwise.raceconstant5y <- summary(glht(years.raceconstant5, mcp(act = "Tukey")))
summary(pairwise.raceconstant1l$test$pvalues<.05) # 53/66
summary(pairwise.raceconstant2l$test$pvalues<.05) # 36/66
summary(pairwise.raceconstant3l$test$pvalues<.05) # 38/66
summary(pairwise.raceconstant4l$test$pvalues<.05) # 39/66
summary(pairwise.raceconstant5l$test$pvalues<.05) # 34/66

summary(pairwise.raceconstant1y$test$pvalues<.05) # 42/66
summary(pairwise.raceconstant2y$test$pvalues<.05) # 32/66
summary(pairwise.raceconstant3y$test$pvalues<.05) # 30/66
summary(pairwise.raceconstant4y$test$pvalues<.05) # 35/66
summary(pairwise.raceconstant5y$test$pvalues<.05) # 29/66
### Hold constant gender
likelihood.genderconstant1 <- lmer(sentence_jail~act+PID_congruency+p_race+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_gender)==1)
likelihood.genderconstant2 <- lmer(sentence_jail~act+PID_congruency+p_race+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_gender)==2)
years.genderconstant1 <- lmer(sentence_years~act+p_race+PID_congruency+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_gender)==1)
years.genderconstant2 <- lmer(sentence_years~act+p_race+PID_congruency+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$p_gender)==2)
pairwise.genderconstant1l <- summary(glht(likelihood.genderconstant1, mcp(act = "Tukey")))
pairwise.genderconstant2l <- summary(glht(likelihood.genderconstant2, mcp(act = "Tukey")))
pairwise.genderconstant1y <- summary(glht(years.genderconstant1, mcp(act = "Tukey")))
pairwise.genderconstant2y <- summary(glht(years.genderconstant2, mcp(act = "Tukey")))
summary(pairwise.genderconstant1l$test$pvalues<.05) # 52/66
summary(pairwise.genderconstant2l$test$pvalues<.05) # 53/66

summary(pairwise.genderconstant1y$test$pvalues<.05) # 45/66
summary(pairwise.genderconstant2y$test$pvalues<.05) # 39/66
### Hold constant residence
likelihood.residenceconstant1 <- lmer(sentence_jail~act+p_race+p_gender+PID_congruency+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(as.factor(data$profile_residence_group))==1)
likelihood.residenceconstant2 <- lmer(sentence_jail~act+p_race+p_gender+PID_congruency+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(as.factor(data$profile_residence_group))==2)
likelihood.residenceconstant3 <- lmer(sentence_jail~act+p_race+p_gender+PID_congruency+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(as.factor(data$profile_residence_group))==3)
years.residenceconstant1 <- lmer(sentence_years~act+p_race+p_gender+PID_congruency+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(as.factor(data$profile_residence_group))==1)
years.residenceconstant2 <- lmer(sentence_years~act+p_race+p_gender+PID_congruency+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(as.factor(data$profile_residence_group))==2)
years.residenceconstant3 <- lmer(sentence_years~act+p_race+p_gender+PID_congruency+marital+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(as.factor(data$profile_residence_group))==3)
pairwise.residenceconstant1l <- summary(glht(likelihood.residenceconstant1, mcp(act = "Tukey")))
pairwise.residenceconstant2l <- summary(glht(likelihood.residenceconstant2, mcp(act = "Tukey")))
pairwise.residenceconstant3l <- summary(glht(likelihood.residenceconstant3, mcp(act = "Tukey")))
pairwise.residenceconstant1y <- summary(glht(years.residenceconstant1, mcp(act = "Tukey")))
pairwise.residenceconstant2y <- summary(glht(years.residenceconstant2, mcp(act = "Tukey")))
pairwise.residenceconstant3y <- summary(glht(years.residenceconstant3, mcp(act = "Tukey")))
summary(pairwise.residenceconstant1l$test$pvalues<.05) # 45/66
summary(pairwise.residenceconstant2l$test$pvalues<.05) # 49/66
summary(pairwise.residenceconstant3l$test$pvalues<.05) # 46/66

summary(pairwise.residenceconstant1y$test$pvalues<.05) # 40/66
summary(pairwise.residenceconstant2y$test$pvalues<.05) # 39/66
summary(pairwise.residenceconstant3y$test$pvalues<.05) # 36/66
### Hold constant marital status
likelihood.maritalconstant1 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==1)
likelihood.maritalconstant2 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==2)
likelihood.maritalconstant3 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==3)
likelihood.maritalconstant4 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==4)
likelihood.maritalconstant5 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==5)
years.maritalconstant1 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==1)
years.maritalconstant2 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==2)
years.maritalconstant3 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==3)
years.maritalconstant4 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==4)
years.maritalconstant5 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$marital)==5)
pairwise.maritalconstant1l <- summary(glht(likelihood.maritalconstant1, mcp(act = "Tukey")))
pairwise.maritalconstant2l <- summary(glht(likelihood.maritalconstant2, mcp(act = "Tukey")))
pairwise.maritalconstant3l <- summary(glht(likelihood.maritalconstant3, mcp(act = "Tukey")))
pairwise.maritalconstant4l <- summary(glht(likelihood.maritalconstant4, mcp(act = "Tukey")))
pairwise.maritalconstant5l <- summary(glht(likelihood.maritalconstant5, mcp(act = "Tukey")))
pairwise.maritalconstant1y <- summary(glht(years.maritalconstant1, mcp(act = "Tukey")))
pairwise.maritalconstant2y <- summary(glht(years.maritalconstant2, mcp(act = "Tukey")))
pairwise.maritalconstant3y <- summary(glht(years.maritalconstant3, mcp(act = "Tukey")))
pairwise.maritalconstant4y <- summary(glht(years.maritalconstant4, mcp(act = "Tukey")))
pairwise.maritalconstant5y <- summary(glht(years.maritalconstant5, mcp(act = "Tukey")))
summary(pairwise.maritalconstant1l$test$pvalues<.05) # 44/66
summary(pairwise.maritalconstant2l$test$pvalues<.05) # 41/66
summary(pairwise.maritalconstant3l$test$pvalues<.05) # 41/66
summary(pairwise.maritalconstant4l$test$pvalues<.05) # 42/66
summary(pairwise.maritalconstant5l$test$pvalues<.05) # 41/66

summary(pairwise.maritalconstant1y$test$pvalues<.05) # 36/66
summary(pairwise.maritalconstant2y$test$pvalues<.05) # 35/66
summary(pairwise.maritalconstant3y$test$pvalues<.05) # 32/66
summary(pairwise.maritalconstant4y$test$pvalues<.05) # 35/66
summary(pairwise.maritalconstant5y$test$pvalues<.05) # 34/66
### Hold constant # of children
likelihood.childrenconstant1 <- lmer(sentence_jail~act+p_race+p_gender+residence_group+marital+PID_congruency+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$children)==1)
likelihood.childrenconstant2 <- lmer(sentence_jail~act+p_race+p_gender+residence_group+marital+PID_congruency+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$children)==2)
likelihood.childrenconstant3 <- lmer(sentence_jail~act+p_race+p_gender+residence_group+marital+PID_congruency+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$children)==3)
years.childrenconstant1 <- lmer(sentence_years~act+p_race+p_gender+residence_group+marital+PID_congruency+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$children)==1)
years.childrenconstant2 <- lmer(sentence_years~act+p_race+p_gender+residence_group+marital+PID_congruency+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$children)==2)
years.childrenconstant3 <- lmer(sentence_years~act+p_race+p_gender+residence_group+marital+PID_congruency+occupation_group+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$children)==3)
pairwise.childrenconstant1l <- summary(glht(likelihood.childrenconstant1, mcp(act = "Tukey")))
pairwise.childrenconstant2l <- summary(glht(likelihood.childrenconstant2, mcp(act = "Tukey")))
pairwise.childrenconstant3l <- summary(glht(likelihood.childrenconstant3, mcp(act = "Tukey")))
pairwise.childrenconstant1y <- summary(glht(years.childrenconstant1, mcp(act = "Tukey")))
pairwise.childrenconstant2y <- summary(glht(years.childrenconstant2, mcp(act = "Tukey")))
pairwise.childrenconstant3y <- summary(glht(years.childrenconstant3, mcp(act = "Tukey")))
summary(pairwise.childrenconstant1l$test$pvalues<.05) # 54/66
summary(pairwise.childrenconstant2l$test$pvalues<.05) # 45/66
summary(pairwise.childrenconstant3l$test$pvalues<.05) # 44/66

summary(pairwise.childrenconstant1y$test$pvalues<.05) # 35/66
summary(pairwise.childrenconstant2y$test$pvalues<.05) # 39/66
summary(pairwise.childrenconstant3y$test$pvalues<.05) # 41/66
### Hold constant occupation
likelihood.classconstant1 <- lmer(sentence_jail~act+PID_congruency+p_race+residence_group+marital+children+p_gender+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$occupation_group)==1)
likelihood.classconstant2 <- lmer(sentence_jail~act+PID_congruency+p_race+residence_group+marital+children+p_gender+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$occupation_group)==2)
years.classconstant1 <- lmer(sentence_years~act+p_race+PID_congruency+residence_group+marital+children+p_gender+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$occupation_group)==1)
years.classconstant2 <- lmer(sentence_years~act+p_race+PID_congruency+residence_group+marital+children+p_gender+p_age + (1 | ResponseId),data=data,subset=as.numeric(data$occupation_group)==2)
pairwise.classconstant1l <- summary(glht(likelihood.classconstant1, mcp(act = "Tukey")))
pairwise.classconstant2l <- summary(glht(likelihood.classconstant2, mcp(act = "Tukey")))
pairwise.classconstant1y <- summary(glht(years.classconstant1, mcp(act = "Tukey")))
pairwise.classconstant2y <- summary(glht(years.classconstant2, mcp(act = "Tukey")))
summary(pairwise.classconstant1l$test$pvalues<.05) # 53/66
summary(pairwise.classconstant2l$test$pvalues<.05) # 55/66

summary(pairwise.classconstant1y$test$pvalues<.05) # 41/66
summary(pairwise.classconstant2y$test$pvalues<.05) # 39/66
### Hold constant age
likelihood.ageconstant1 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==1)
likelihood.ageconstant2 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==2)
likelihood.ageconstant3 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==3)
likelihood.ageconstant4 <- lmer(sentence_jail~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==4)
years.ageconstant1 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==1)
years.ageconstant2 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==2)
years.ageconstant3 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==3)
years.ageconstant4 <- lmer(sentence_years~act+PID_congruency+p_gender+residence_group+p_race+children+occupation_group+marital + (1 | ResponseId),data=data,subset=as.numeric(data$p_age)==4)
pairwise.ageconstant1l <- summary(glht(likelihood.ageconstant1, mcp(act = "Tukey")))
pairwise.ageconstant2l <- summary(glht(likelihood.ageconstant2, mcp(act = "Tukey")))
pairwise.ageconstant3l <- summary(glht(likelihood.ageconstant3, mcp(act = "Tukey")))
pairwise.ageconstant4l <- summary(glht(likelihood.ageconstant4, mcp(act = "Tukey")))
pairwise.ageconstant1y <- summary(glht(years.ageconstant1, mcp(act = "Tukey")))
pairwise.ageconstant2y <- summary(glht(years.ageconstant2, mcp(act = "Tukey")))
pairwise.ageconstant3y <- summary(glht(years.ageconstant3, mcp(act = "Tukey")))
pairwise.ageconstant4y <- summary(glht(years.ageconstant4, mcp(act = "Tukey")))
summary(pairwise.ageconstant1l$test$pvalues<.05) # 42/66
summary(pairwise.ageconstant2l$test$pvalues<.05) # 46/66
summary(pairwise.ageconstant3l$test$pvalues<.05) # 48/66
summary(pairwise.ageconstant4l$test$pvalues<.05) # 45/66

summary(pairwise.ageconstant1y$test$pvalues<.05) # 36/66
summary(pairwise.ageconstant2y$test$pvalues<.05) # 37/66
summary(pairwise.ageconstant3y$test$pvalues<.05) # 40/66
summary(pairwise.ageconstant4y$test$pvalues<.05) # 35/66

# Total, Likelihood: 1227/1782 (68.9%)
# Total, Years: 997/1782 (55.9%)

# Figure A2
## Motivation Regression
mlm_reg4 <- lmer(motivation_num~target+severity+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + (1 | ResponseId),data=data)
## Data
df.motivation <- data.frame(effect=factor(c("Target: Government","Target: Police","Target: Small Business","Severity: Medium","Severity: High","PID: Independent","PID: Incongruent","Race: Black","Race: Hispanic","Race: Asian","Race: Middle Eastern","Gender: Woman","Residence: Rural","Residence: Out of State","Marital: Cohabitating","Marital: Married","Marital: Divorced","Marital: Widowed","Children: One","Children: Two","Occupation: White Collar","Age: Mid-30s","Age: Mid-40s","Age: Mid-50s","Motivation"),levels=c("Motivation","Age: Mid-50s", "Age: Mid-40s", "Age: Mid-30s", "Occupation: White Collar", "Children: Two", "Children: One", "Marital: Widowed", "Marital: Divorced", "Marital: Married", "Marital: Cohabitating", "Residence: Out of State", "Residence: Rural", "Gender: Woman", "Race: Middle Eastern", "Race: Asian", "Race: Hispanic", "Race: Black", "PID: Incongruent", "PID: Independent", "Severity: High", "Severity: Medium", "Target: Small Business", "Target: Police", "Target: Government")),
                            motivation_prob=c(-.012146,-.021200,-.026696,-.025658,-.076035,-.016663,-.046152,.034349,.016222,.015340,.016582,.006873,-.002921,-.004764,-.001113,.007366,.001024,.010262,-.001244,-.002988,.003446,.006310,.005363,.006810,NA),
                            motivation_se=c(.005268,.005244,.005284,.004558,.004576,.004552,.004575,.005829,.005900,.005907,.005928,.003731,.004534,.004549,.005840,.005834,.005832,.005875,.004589,.004540,.0037720,.005277,.005294,.005280,NA),
                            likelihood_prob=c(0.0299229,0.0834442,0.0105397,0.2507487,0.3679302,.0029063,.0237610,-.0425860,-.0238851,-.0283720,-.0078734,-.0109237,-.0082372,.0070027,-.0147820,-.0007897,-.0161259,-.0147153,.0089006,.0145884,-.0093010,-.0090777,-.0032539,-.0061490,NA),
                            likelihood_se=c(.0084131,.0083752,.0084311,.0072807,.0073083,.0072664,.0073057,.0093031,.0094199,.0094240,.0094663,.0059563,.0072446,.0072656,.0093298,.0093169,.0093205,.0093897,0.0073297,.0072486,.0059423,.0084198,.0084526,.0084306,NA),
                            likelihoodmed_prob=c(.026433,.078058,.003343,.244612,.348705,-.001946,.011714,-.033508,-.019737,-.024266,-.003471,-.009389,-.008811,.005738,-.015004,.000923,-.015761,-.012009,.009058,.013825,-.008525,-.007604,-.001992,-.004733,-.252595),
                            likelihoodmed_se=c(.008291,.008256,.008315,.007180,.007266,.007164,.007224,.009178,.009284,.009288,.009330,.005869,.007138,.007159,.009192,.009180,.009183,.009253,.007222,.007142,.005855,.008296,.008329,.008307,.012764),
                            years_prob=c(.49950,.93126,.03912,.54893,3.29875,.07702,.32547,-.41854,-.18132,-.07736,-.11865,-.20239,.01799,.06881,.07513,.0694,.02914,.01674,.1099,-.07635,-.04954,-.04958,-.20573,-.01157,NA)/30,
                            years_se=c(.09445,.09402,.09470,.08173,.08204,.01859,.08202,.10447,.10576,.10585,.10627,.06688,.08131,.08156,.10472,.10459,.10460,.10537,.08227,.08138,.06670,.09457,.09491,.09466,NA)/30,
                            yearsmed_prob=c(.4754964,.8917784,-.0121386,.5024847,3.1574893,.0439953,.2385905,-.3536036,-.1510279,-.0480775,-.0870285,-.01902000,.0133024,.00597489,.0733028,.0827451,.0315581,.0363308,.1092674,-.0819854,-.0434015,-.0382060,-.01959792,.0002024,-1.857577),
                            yearsmed_se=c(.0939676,.0935772,.0942855,.0813820,.0823996,.0812025,.0818893,.1040517,.1052343,.1053163,.1057402,.0665365,.0808819,.0811303,.1041661,.1040443,.1040458,.1048268,.0818399,.0809524,.0663539,.0940761,.0944090,.0941615,.01499192))
df.motivation$motivation_lci <- df.motivation$motivation_prob-(1.96*df.motivation$motivation_se)
df.motivation$motivation_uci <- df.motivation$motivation_prob+(1.96*df.motivation$motivation_se)
df.motivation$likelihood_lci <- df.motivation$likelihood_prob-(1.96*df.motivation$likelihood_se)
df.motivation$likelihood_uci <- df.motivation$likelihood_prob+(1.96*df.motivation$likelihood_se)
df.motivation$likelihoodmed_lci <- df.motivation$likelihoodmed_prob-(1.96*df.motivation$likelihoodmed_se)
df.motivation$likelihoodmed_uci <- df.motivation$likelihoodmed_prob+(1.96*df.motivation$likelihoodmed_se)
df.motivation$years_lci <- df.motivation$years_prob-(1.96*df.motivation$years_se)
df.motivation$years_uci <- df.motivation$years_prob+(1.96*df.motivation$years_se)
df.motivation$yearsmed_lci <- df.motivation$yearsmed_prob-(1.96*df.motivation$yearsmed_se)
df.motivation$yearsmed_uci <- df.motivation$yearsmed_prob+(1.96*df.motivation$yearsmed_se)
df.motivation$motivation_sig <- ifelse(df.motivation$motivation_lci<0 & df.motivation$motivation_uci>0,"0","1")
df.motivation$likelihood_sig <- ifelse(df.motivation$likelihood_lci<0 & df.motivation$likelihood_uci>0,"0","1")
df.motivation$likelihoodmed_sig <- ifelse(df.motivation$likelihoodmed_lci<0 & df.motivation$likelihoodmed_uci>0,"0","1")
df.motivation$years_sig <- ifelse(df.motivation$years_lci<0 & df.motivation$years_uci>0,"0","1")
df.motivation$yearsmed_sig <- ifelse(df.motivation$yearsmed_lci<0 & df.motivation$yearsmed_uci>0,"0","1")

df.fig6 <- data.frame(effect=c(rep(df.motivation$effect,3)),
                      prob=c(df.motivation$motivation_prob,df.motivation$likelihood_prob,df.motivation$years_prob),
                      se=c(df.motivation$motivation_se,df.motivation$likelihood_se,df.motivation$years_se),
                      lci=c(df.motivation$motivation_lci,df.motivation$likelihood_lci,df.motivation$years_lci),
                      uci=c(df.motivation$motivation_uci,df.motivation$likelihood_uci,df.motivation$years_uci),
                      sig=c(df.motivation$motivation_sig,df.motivation$likelihood_sig,df.motivation$years_sig),
                      model=factor(c(rep("Motivation",25),rep("Likelihood",25),rep("Years",25)),levels=c("Years","Likelihood","Motivation")))
df.fig6 <- subset(df.fig6,effect!="Motivation")
### Plot
fig6 <- ggplot(df.fig6,aes(x=effect,y=prob,color=model,alpha=sig)) + geom_point(position=position_dodge(0.85)) + coord_flip() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(0.85)) + theme_classic() + geom_hline(yintercept=0,linetype="dashed") + ylab("Coefficient") + xlab("Predictor") + scale_alpha_manual(values=c(0.5,1),guide="none") + guides(color = guide_legend(reverse = TRUE,title="Dependent Variable"))
ggsave("Motivation Plot 1.png",fig6,device="png",width=8,height=6,units="in")


# Table A7/Figure A3
## Regressions (Models 1 and 3 of Table A7 come from Models 1 and 2 of Table A1)
mlm_reg5 <- lmer(sentence_jail~target+severity+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + motivation_num + (1 | ResponseId),data=data) # Model 2
mlm_reg6 <- lmer(sentence_years~target+severity+PID_congruency+p_race+p_gender+residence_group+marital+children+occupation_group+p_age + motivation_num + (1 | ResponseId),data=data) # Model 4
## Figure
df.fig7 <- data.frame(effect=c(rep(df.motivation$effect,4)),
                      prob=c(df.motivation$likelihood_prob,df.motivation$likelihoodmed_prob,df.motivation$years_prob,df.motivation$yearsmed_prob),
                      se=c(df.motivation$likelihood_se,df.motivation$likelihoodmed_se,df.motivation$years_se,df.motivation$yearsmed_se),
                      lci=c(df.motivation$likelihood_lci,df.motivation$likelihoodmed_lci,df.motivation$years_lci,df.motivation$yearsmed_lci),
                      uci=c(df.motivation$likelihood_uci,df.motivation$likelihoodmed_uci,df.motivation$years_uci,df.motivation$yearsmed_uci),
                      sig=c(df.motivation$likelihood_sig,df.motivation$likelihoodmed_sig,df.motivation$years_sig,df.motivation$yearsmed_sig),
                      model=factor(c(rep("Regular",25),rep("w/ Motivation",25),rep("Regular",25),rep("w/ Motivation",25)),levels=c("Regular","w/ Motivation")),
                      depvar=factor(c(rep("Likelihood",50),rep("Years",50)),levels=c("Likelihood","Years")))

fig7 <- ggplot(df.fig7,aes(x=effect,y=prob,color=model,alpha=sig)) + geom_point(position=position_dodge(0.9)) + coord_flip() + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(0.9)) + theme_classic() + xlab("Predictor") + ylab("Coefficient") + scale_color_manual(values=c("Black","Gray"),name="Model") + scale_alpha_manual(values=c(0.5,1),guide="none") + guides(color = guide_legend(reverse = TRUE)) + geom_hline(yintercept=0,linetype="dashed") + facet_grid(cols = vars(depvar),scales="free_x") + theme(legend.position="bottom")
ggsave("Model Differences Including Motivation.png",plot=fig7,device="png",width=8,height=6,units="in")