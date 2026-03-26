#-----------------------------------------------------------------------------#
# Paper: The new corruption crusaders: Security sector ties as an             #
#        anti-corruption voting heuristic                                     #
# Authors: Luiz Vilaça and Jacob Turner                                       #
# Last Update: January 2, 2024                                                #
#-----------------------------------------------------------------------------#

# This code replicates the analysis in the paper and duplicates Figures 6, 7, #
# 8, 9, and 10 from the main paper and the table and figure from Appendix E   #

#Packages

library(plm)
library(lme4)
library(cregg)
library(stargazer)
library(interplot)
library(dotwhisker)
library(tidyverse)
library(broom)
library(broom.mixed)

setwd("~/Projects/Luiz Joint Project/Dataverse")


########################### Load Data #########################################

facdat<-read.csv('cleaned_factorial_frame.csv',
                 encoding='UTF-8',
                 stringsAsFactors = TRUE)

facdat$prof<-relevel(factor(facdat$prof),ref='office')

facdat$slogan<-relevel(factor(facdat$slogan),ref='generic')

facdat$race<-relevel(factor(facdat$race),ref='white')

############################ Estimate All Models ##############################

#Within models
#vote choice
choose1<-plm(data=facdat,choice~prof+slogan+politician+econ+race+sex,
        index=c('id','order'),
        model='within',
        effect='twoways')

#security
secure1<-plm(data=facdat,sec~prof+slogan+politician+econ+race+sex,
            index=c('id','order'),
            model='within',
            effect='twoways')

#corruption
corrupt1<-plm(data=facdat,corrup~prof+slogan+politician+econ+race+sex,
            index=c('id','order'),
            model='within',
            effect='twoways')

#employment
employ1<-plm(data=facdat,emp~prof+slogan+politician+econ+race+sex,
            index=c('id','order'),
            model='within',
            effect='twoways')


#Multilevel Models, profile features only
#Vote choice
choose2<-lmer(data=facdat,choice~prof+slogan+politician+econ+race+sex+
               (1|id))

#security
secure2<-lmer(data=facdat,sec~prof+slogan+politician+econ+race+sex+
               (1|id))

#corruption
corrupt2<-lmer(data=facdat,corrup~prof+slogan+politician+econ+race+sex+
               (1|id))

#employment
employ2<-lmer(data=facdat,emp~prof+slogan+politician+econ+race+sex+
               (1|id))

#cjoint, profile features only
#Vote choice
choosecon1<-cj(data=facdat,choice~prof+slogan+politician+econ+race+sex,
              id=~id)

#security
securecon1<-cj(data=facdat[!is.na(facdat$sec),],sec~prof+slogan+politician+econ+race+sex,
              id=~id)

#corruption
corruptcon1<-cj(data=facdat,corrup~prof+slogan+politician+econ+race+sex,
               id=~id)

#employment
employcon1<-cj(data=facdat,emp~prof+slogan+politician+econ+race+sex,
              id=~id)

#Multilevel Models
#Vote Choice
choose6<-lmer(data=facdat,choice~prof*ideology_self+slogan+politician+econ+race+sex+
                age+(sex_respondent==1)+urban+
                (1|id))

#security
secure6<-lmer(data=facdat,sec~prof*ideology_self+slogan+politician+econ+race+sex+
                age+(sex_respondent==1)+urban+
                (1|id))

#corruption
corrupt6<-lmer(data=facdat,corrup~prof*ideology_self+slogan+politician+econ+race+sex+
                 age+(sex_respondent==1)+urban+
                 (1|id))

#employment
employ6<-lmer(data=facdat,emp~prof*ideology_self+slogan+politician+econ+race+sex+
                age+(sex_respondent==1)+urban+
                (1|id))



############# Figure 6 from main text, Plots from Appendix E ##################

#Corruption
feats<-c('term','estimate','std.error')
names(corruptcon1)[names(corruptcon1)=='level']<-'term'
df1<-tidy(corrupt1)[,feats]
df1$model<-'within'
df2<-tidy(corrupt2)[,feats]
df2$model<-'multilevel'
df3<-corruptcon1[,feats]
df3$model<-'clustered'

pframe<-rbind(df1,df2,df3)
pframe<-pframe[!(pframe$term%in%c('sd__(Intercept)','sd__Observation')),]
pframe$term<-gsub('prof|slogan|econ|race|sex','',pframe$term)
pframe$term<-gsub('politicianpolitician','politician',pframe$term)
pframe$term<-gsub('essor','Teacher',pframe$term)

p1<-dwplot(pframe,vline=geom_vline(xintercept=0),
           dot_args = list(size=1.5),
           vars_order=c('office','captain','doctor','pastor','police','Teacher',
                        'generic','corruption','jobs',
                        'novice','politician',
                        'left','right',
                        'white','pardo','black',
                        'female','male'))

p1<-relabel_predictors(p1,c(office="Neutral (Reference)",captain="Coronel",
                            doctor="Doctor",pastor="Pastor",police="Police",
                            Teacher="Teacher",
                            generic="Generic Slogan (Reference)",
                            corruption="Anti-corruption Slogan",
                            jobs="Employment Slogan",
                            novice="Newcomer (Reference)",politician="Politician",
                            left="Left-wing (Reference)",
                            right="Right-wing",
                            white="White (Reference)",black="Black",pardo="Pardo",
                            female="Female (Reference)",male="Male"))+
  scale_color_grey(start=.3,
                   end=.7,
                   name="Model",
                   labels=c("Within","Multilevel","OLS"))+
  theme_minimal()+
  ggtitle("Marginal Effects on Anti-Corruption Effectiveness")+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5,size=12),
        axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))

png("plots/appendix_e_corruption.png",width=500)
p1
dev.off()

#Only professions
p1<-dwplot(pframe,vline=geom_vline(xintercept=0),
           dot_args = list(size=1.5))

p1<-relabel_predictors(p1,c(office="Neutral (Reference)",captain="Coronel",
                            doctor="Doctor",
                            pastor="Pastor",police="Police",
                            Teacher="Teacher"))+
  scale_color_grey(start=.3,
                   end=.7,
                   name="Model",
                   labels=c("Within","Multilevel","OLS"))+
  theme_minimal()+
  ggtitle("Marginal Effects of Profession on \nAnti-Corruption Effectiveness")+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))+
  ylim(breaks=c('Doctor','Pastor','Police','Teacher','Coronel','Neutral (Reference)'))+
  xlim(-.6,.7)

png("plots/figure_6_corruption.png",width=500)
p1
dev.off()

#Vote choice
feats<-c('term','estimate','std.error')
names(choosecon1)[names(choosecon1)=='level']<-'term'
df1<-tidy(choose1)[,feats]
df1$model<-'within'
df2<-tidy(choose2)[,feats]
df2$model<-'multilevel'
df3<-choosecon1[,feats]
df3$model<-'clustered'

pframe<-rbind(df1,df2,df3)
pframe<-pframe[!(pframe$term%in%c('sd__(Intercept)','sd__Observation')),]
pframe$term<-gsub('prof|slogan|econ|race|sex','',pframe$term)
pframe$term<-gsub('politicianpolitician','politician',pframe$term)
pframe$term<-gsub('essor','Teacher',pframe$term)

p1<-dwplot(pframe,vline=geom_vline(xintercept=0),
           dot_args = list(size=1.5),
           vars_order=c('office','captain','doctor','pastor','police','Teacher',
                        'generic','corruption','jobs',
                        'novice','politician',
                        'left','right',
                        'white','pardo','black',
                        'female','male'))

p1<-relabel_predictors(p1,c(office="Neutral (Reference)",captain="Coronel",
                            doctor="Doctor",pastor="Pastor",police="Police",
                            Teacher="Teacher",
                            generic="Generic Slogan (Reference)",
                            corruption="Anti-corruption Slogan",
                            jobs="Employment Slogan",
                            novice="Newcomer (Reference)",politician="Politician",
                            left="Left-wing (Reference)",
                            right="Right-wing",
                            white="White (Reference)",black="Black",pardo="Pardo",
                            female="Female (Reference)",male="Male"))+
  scale_color_grey(start=.3,
                   end=.7,
                   name="Model",
                   labels=c("Within","Multilevel","OLS"))+
  theme_minimal()+
  ggtitle("Marginal Effects on Vote Choice")+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5,size=12),
        axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))

png("plots/appendix_e_choice.png",width=500)
p1
dev.off()

#Just Professions
p1<-dwplot(pframe,vline=geom_vline(xintercept=0),
           dot_args = list(size=1.5))

p1<-relabel_predictors(p1,c(office="Neutral (Reference)",captain="Coronel",
                            doctor="Doctor",
                            pastor="Pastor",police="Police",
                            Teacher="Teacher"))+
  scale_color_grey(start=.3,
                   end=.7,
                   name="Model",
                   labels=c("Within","Multilevel","OLS"))+
  theme_minimal()+
  ggtitle("Marginal Effects of Profession on \nVote Choice")+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))+
  ylim(breaks=c('Doctor','Pastor','Police','Teacher','Coronel','Neutral (Reference)'))+
  xlim(-.6,.7)

png("plots/figure_6_choice.png",width=500)
p1
dev.off()

#Security
#Just Professions
feats<-c('term','estimate','std.error')
names(securecon1)[names(securecon1)=='level']<-'term'
df1<-tidy(secure1)[,feats]
df1$model<-'within'
df2<-tidy(secure2)[,feats]
df2$model<-'multilevel'
df3<-securecon1[,feats]
df3$model<-'clustered'

pframe<-rbind(df1,df2,df3)
pframe<-pframe[!(pframe$term%in%c('sd__(Intercept)','sd__Observation')),]
pframe$term<-gsub('prof|slogan|econ|race|sex','',pframe$term)
pframe$term<-gsub('politicianpolitician','politician',pframe$term)
pframe$term<-gsub('essor','Teacher',pframe$term)

p1<-dwplot(pframe,vline=geom_vline(xintercept=0),
           dot_args = list(size=1.5))

p1<-relabel_predictors(p1,c(office="Neutral (Reference)",captain="Coronel",
                            doctor="Doctor",
                            pastor="Pastor",police="Police",
                            Teacher="Teacher"))+
  scale_color_grey(start=.3,
                   end=.7,
                   name="Model",
                   labels=c("Within","Multilevel","OLS"))+
  theme_minimal()+
  ggtitle("Marginal Effects of Profession on \nSecurity Effectiveness")+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))+
  ylim(breaks=c('Doctor','Pastor','Police','Teacher','Coronel','Neutral (Reference)'))+
  xlim(-.6,.7)

png("plots/figure_6_security.png",width=500)
p1
dev.off()

#Employment
#Just Professions
feats<-c('term','estimate','std.error')
names(employcon1)[names(employcon1)=='level']<-'term'
df1<-tidy(employ1)[,feats]
df1$model<-'within'
df2<-tidy(employ2)[,feats]
df2$model<-'multilevel'
df3<-employcon1[,feats]
df3$model<-'clustered'

pframe<-rbind(df1,df2,df3)
pframe<-pframe[!(pframe$term%in%c('sd__(Intercept)','sd__Observation')),]
pframe$term<-gsub('prof|slogan|econ|race|sex','',pframe$term)
pframe$term<-gsub('politicianpolitician','politician',pframe$term)
pframe$term<-gsub('essor','Teacher',pframe$term)

p1<-dwplot(pframe,vline=geom_vline(xintercept=0),
           dot_args = list(size=1.5))

p1<-relabel_predictors(p1,c(office="Neutral (Reference)",captain="Coronel",
                            doctor="Doctor",
                            pastor="Pastor",police="Police",
                            Teacher="Teacher"))+
  scale_color_grey(start=.3,
                   end=.7,
                   name="Model",
                   labels=c("Within","Multilevel","OLS"))+
  theme_minimal()+
  ggtitle("Marginal Effects of Profession on \nEmployment Effectiveness")+
  theme(legend.position="bottom",
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5,size=18),
        axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))+
  ylim(breaks=c('Doctor','Pastor','Police','Teacher','Coronel','Neutral (Reference)'))+
  xlim(-.6,.7)

png("plots/figure_6_employment.png",width=500)
p1
dev.off()

############################## Figures 7 and 8 ################################

polplot<-interplot(choose6,'profpolice','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Police on Vote Intention \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Police")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_7_police.png")
polplot
dev.off()

milplot<-interplot(choose6,'profcaptain','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Colonel on Vote Intention \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Colonel")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_7_military.png")
milplot
dev.off()

polplot<-interplot(corrupt6,'profpolice','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Police on Corruption Effectiveness \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Police")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_8_police.png")
polplot
dev.off()

milplot<-interplot(corrupt6,'profcaptain','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Colonel on Corruption Effectiveness \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Colonel")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_8_military.png")
milplot
dev.off()

polplot<-interplot(secure6,'profpolice','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Police on Security Effectiveness \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Police")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_9_police.png")
polplot
dev.off()

milplot<-interplot(secure6,'profcaptain','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Colonel on Security Effectiveness \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Colonel")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_9_military.png")
milplot
dev.off()

polplot<-interplot(employ6,'profpolice','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Police on Employment Effectiveness \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Police")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_10_police.png")
polplot
dev.off()

milplot<-interplot(employ6,'profcaptain','ideology_self',ercolor='black',
                   ralpha=0,point=TRUE)+
  geom_hline(yintercept=0)+theme_minimal()+
  ggtitle("Estimated Effect of Colonel on Employment Effectiveness \nConditional on Respondent Ideology")+
  xlab("Respondent Ideology")+
  ylab("Estimated Marginal Effect of Colonel")+
  theme(plot.title = element_text(hjust = 0.5,size=15),
        axis.text=element_text(size=12),
        axis.title =element_text(size=12),
        legend.text=element_text(size=12))+
  ylim(-1.1,1.1)

png("plots/figure_10_military.png")
milplot
dev.off()

####################### Table in Appendix E ###################################

stargazer(choose1,choose2,corrupt1,corrupt2,type='text',
          #out="factorial_models_1.html",
          style="apsr",
          dep.var.labels = c("Vote Choice","Corruption Effectiveness"),
          model.names=FALSE,
          column.labels=c("Within-Respondent","Random Intercept",
                          "Within-Respondent","Random Intercept"),
          covariate.labels=c("Captain","Doctor","Pastor","Police","Teacher",
                             "Corruption","Unemployment","Politician","Right-Wing",
                             "Black","Pardo","Male","Constant"),
          keep.stat=c("n","rsq","adj.rsq","aic","bic"))




















