
#Script for Models in : 
#A Probabilistic and Syntactic Account of Variable Clitic Agreement in Spanish Double Object Constructions

library(tidyverse)
library(party)
library(brms)
library(bayestestR)
library(ggeffects)
library(Hmisc)


dfpro= read.csv("dataFinal.csv", stringsAsFactors = TRUE)

write.csv(dfpro, "dfpro2.csv", row.names = FALSE)

#select variable for the tree model
dftree= select( dfpro, -ID, -Source, -Sentence, -Address, -AnimacyIOFactor, -NounIO,-LexicalDO,-Verb,
                -PersonV, -FrequencyVerb, -Verb_DO, -Verb_IO, -Verb_DO_IO, -FrequencyVerb, -DOIO, -ConcreteScoreDO, 
                -ConcreteScoreIO)

set.seed(0103)
tree1= ctree(Clitic~., data=dftree)

#Plot the tree
plot(tree1, tp_args=list(beside=TRUE))

tree1_pred= unlist(treeresponse(tree1))[c(FALSE, TRUE)]

#Calculation of C-index
round(somers2(tree1_pred, as.numeric(dftree$Clitic)-1),2)
C     Dxy       n Missing 
0.81    0.63 2414.00    0.00 

################################ Bayesian analysis  ########################################

#Setting up contrasts

## Model to calculate Bayes factors

dfproORTH= dfpro
dfproORTH$AnimacyIO=relevel(dfproORTH$AnimacyIO, ref="Low")
dfproORTH$Clitic= relevel(dfproORTH$Clitic, ref="Default")
dfproORTH$GenderDO= relevel(dfproORTH$GenderDO, ref="Masc")
dfproORTH$NumberDO= relevel(dfproORTH$NumberDO, ref="SG")
dfproORTH$WordOrder= relevel(dfproORTH$WordOrder, ref="DO_IO")
dfproORTH$Position= relevel(dfproORTH$Position, ref=)

contrasts(dfproORTH$NumberDO)  <- contr.orthonorm
contrasts(dfproORTH$WordOrder)  <- contr.orthonorm
contrasts(dfproORTH$AnimacyIO)  <- contr.orthonorm
contrasts(dfproORTH$DefiniteDO)  <- contr.orthonorm
contrasts(dfproORTH$NumberV)  <- contr.orthonorm
contrasts(dfproORTH$GenderDO)  <- contr.orthonorm
contrasts(dfproORTH$NumberDO)  <- contr.orthonorm
contrasts(dfproORTH$Position) <- contr.orthonorm()
dfproORTH$DistanceCL_IO2=scale(dfproORTH$DistanceCL_IO)

dfproORTH$Clitic= relevel(dfproORTH$Clitic, ref="Full")

## Model for reporting
dfpro2= dfpro

dfpro2$AnimacyIO=relevel(dfpro2$AnimacyIO, ref="High")
dfpro2$Clitic= relevel(dfpro2$Clitic, ref="Full")
dfpro2$GenderDO= relevel(dfpro2$GenderDO, ref="Fem")
dfpro2$NumberDO= relevel(dfpro2$NumberDO, ref="PL")
dfpro2$WordOrder= relevel(dfpro2$WordOrder, ref="IO_DO")
dfpro2$DefiniteDO= relevel(dfpro2$DefiniteDO, ref="Def")

(contrasts(dfpro2$NumberDO)= c(-0.5, +0.5))
(contrasts(dfpro2$GenderDO)= c(-0.5, +0.5))
(contrasts(dfpro2$AnimacyIO)= c(-0.5, +0.5))
(contrasts(dfpro2$WordOrder)= c(-0.5, +0.5))
(contrasts(dfpro2$NumberV)= c(-0.5, +0.5))
(contrasts(dfpro2$DefiniteDO)= c(-0.5, +0.5,0))

dfpro2$Clitic= relevel(dfpro2$Clitic, ref="Default")




#Model with orthonormal contrasts to compute Bayes factors

set.seed(157)
model1= brm(Clitic~AnimacyIO+DefiniteDO*Position+NumberDO*WordOrder+GenderDO*DefiniteDO+
                     DefiniteDO*DistanceCL_DO+
                     (NumberDO+AnimacyIO||Verb)+(NumberDO+AnimacyIO||LexicalDO)+ (NumberDO||NounIO)+
                     (NumberDO+AnimacyIO||Country),
                   data=dfproORTH,
                   family=bernoulli(link="logit"),iter=6000,
                   cores=4, seed=135, save_pars = save_pars(all=TRUE),
                   control = list(max_treedepth = 15, adapt_delta=0.99),
                   prior=set_prior("cauchy(0,2.5)", class="b")+
                     set_prior("cauchy(0,10)", class="Intercept"))


## Model for reporting
set.seed(157)
model2= brm(Clitic~AnimacyIO+DefiniteDO*Position+NumberDO*WordOrder+GenderDO*DefiniteDO+
                      DefiniteDO*DistanceCL_DO+
                      (NumberDO+AnimacyIO||Verb)+(NumberDO+AnimacyIO||LexicalDO)+ (NumberDO||NounIO)+
                      (NumberDO+AnimacyIO||Country),
                    data=dfpro2,
                    family=bernoulli(link="logit"),iter=6000,
                    cores=4, seed=135, save_pars = save_pars(all=TRUE),
                    control = list(max_treedepth = 15, adapt_delta=0.99),
                    prior=set_prior("cauchy(0,2.5)", class="b")+
                      set_prior("cauchy(0,10)", class="Intercept"))
#Bayes factors
set.seed(137)
model1_bf= bayesfactor_parameters(model1, null = c(-0.1813799,  0.1813799))


#Model Assessment

loobay= loo(model1, cores= 15)

probs= 1/ (1+ exp(-fitted(model1)))
round(somers2(probs[,1], as.numeric(dfproORTH$Clitic)-1),2)

C       Dxy       
0.95    0.89 

round(bayes_R2(model1),2)
      Estimate Est.Error      Q2.5 Q97.5
R2     0.38      0.03         0.32  0.45

preds= predict(model1, type="response")
preds= as.factor(ifelse(preds[,1]>0.5, "Default", "Full"))
caret::confusionMatrix(preds, dfproORTH$Clitic, mode="everything")
#results not shown

#Plots

color_scheme_set(scheme = "mix-red-teal")

x=as.matrix(model2)
m=mcmc_intervals(x,point_est="mean", prob=0.5, outer_size=4,inner_size =6, 
                 point_size= 9,
                 regex_pars=c( "b_DistanceCL_DO" ,"b_GenderDO",
                               "b_WordOrder","b_NumberDO","b_Position","b_DefiniteDO","b_AnimacyIO")) 


theme_set(theme_bw())

m+ ggplot2:: scale_y_discrete(labels=c(
  "DistanceCL_DO", "GenderDO: Masc", "WordOrder: DO-IO", "NumberDO: SG",
  "NumberDO: SG * WordOrder: DO-IO", "Position: Proclisis", "DefinitenessDO: Bare", 
  "DefinitenessDO: Indefinite", "DefinitenessDO: Bare * Position: Proclisis", "DefinitenessDO: Indefinite * Position: Proclisis",
  "DefinitenessDO: Bare * GenderDO: Masc", "DefinitenessDO: Indefinite * GenderDO: Masc", "DefinitenessDO: Bare * DistanceCL_DO",
  "DefinitenessDO: Indefinite * DistanceCL_DO", "AnimacyIO: Low"
))+
  ggtitle("Posterior Distribution Intervals")+
  theme(axis.text=element_text(size=18, face="bold"), 
        axis.title.x = element_text(size=18),
        plot.title = element_text(size=21,hjust=0.5, face="bold"))+
  labs(x="\n Log-Odds Ratio")+
  scale_color_manual(values=c("darkorange2", "turquoise3", "black"))


#Marginal Effects Plots

p1= as.data.frame(ggemmeans(model2, terms=c("DefiniteDO","Position")))

gg1=ggplot(p1, aes(x=,x, y= predicted, color=group)) +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),
                  fatten=4, size=1.8,
                  position = position_dodge(width=0.5))+
  ylim(0,1)+theme(legend.title= element_text(size=22),
                  legend.text= element_text( size=20), 
                  axis.title= element_text(size=20), 
                  axis.text=element_text(size=20),
                  plot.title = element_text(size=26,hjust=0.5, face="bold"))+
  labs(x="\n Definiteness of Direct Object \n ",y="Predicted Default Agreement")+
  scale_color_manual(values=c("darkorange2", "turquoise3", "black"), name="Position of Clitic", 
                     labels=c("\n Enclitic \n", "\n Proclitic \n"))+
  scale_x_discrete(labels=c("Definite", "Bare", "Indefinite"))+
  ggtitle("DefinitenessDO*Position")


p2= as.data.frame(ggemmeans(model2, terms=c("NumberDO","WordOrder")))


gg2= ggplot(p2, aes(x=,x, y= predicted, color=group)) +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),
                  fatten=4, size=1.8,
                  position = position_dodge(width=0.5))+
  ylim(0,1)+theme(legend.title= element_text(size=22),
                  legend.text= element_text( size=20), 
                  axis.title= element_text(size=20), 
                  axis.text=element_text(size=20),
                  plot.title = element_text(size=26,hjust=0.5, face="bold"))+
  labs(x="\n Number of Direct Object \n",y="")+
  scale_color_manual(values=c("darkorange2", "turquoise3", "black"), name="Word Order", labels=c("\n IO_DO \n", "\n DO_IO \n"))+
  scale_x_discrete(labels=c("Plural", "Singular"))+
  ggtitle("NumberDO*WordOrder")


p4= as.data.frame(ggemmeans(model2, terms=c("DistanceCL_DO [all]","DefiniteDO")))

gg4=ggplot(p4, aes(x=x, y= predicted, color=group)) +
  geom_line(size=2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              linetype=0,alpha = .1)+
  ylim(0,1)+theme(legend.title= element_text(size=22),
                  legend.text= element_text( size=20), 
                  axis.title= element_text(size=20), 
                  axis.text=element_text(size=20),
                  plot.title = element_text(size=26,hjust=0.5, face="bold"))+
  labs(x="\n Distance of Clitic to Direct Object in Syllables \n ", y="Predicted Default Agreement")+
  scale_color_manual(values=c("darkorange2", "turquoise3", "black"), name="Definiteness of DO", 
                     labels=c("\n Definite \n", "\n Bare \n", "\n Indefinite \n"))+
  ggtitle("DistanceCL-DO*DefinitenessDO")+
  scale_x_continuous(breaks = seq(0, 10, by = 2))


plot_grid(gg1,gg2, gg4, labels = "AUTO")


p5= as.data.frame(ggemmeans(model2, terms=c("WordOrder","NumberDO", "AnimacyIO")))

gg5=ggplot(p5, aes(x=,x, y= predicted, color=group)) +
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high),
                  fatten=4, size=1.8,
                  position = position_dodge(width=0.5))+
  ylim(0,1)+
  theme(plot.title=element_text(size=24,hjust=0.5, face="bold"),
        legend.text= element_text( size=20),legend.title = element_text(size=22),
        axis.title.x= element_blank(), axis.text=element_text(size=23),
        axis.title.y = element_text(size=24),
        panel.grid.major = element_blank(), 
        strip.text = element_text(colour = "white", size=28, face="bold"))+
  labs(x="Word Order",y="Predicted Probability of Default Agreement \n")+
  scale_color_manual(values=c("darkorange2", "turquoise3"), name="Number DO")+
  scale_x_discrete(labels=c("IO-DO", "DO-IO"))+
  ggtitle("Interaction AnimacyIO*WordOrder*NumberDO")+
  facet_wrap(~facet)



