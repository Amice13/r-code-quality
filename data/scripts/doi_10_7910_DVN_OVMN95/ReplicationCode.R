### Created by Nicholas W. Waterbury ###
### Replication file for: 
###"Justice Isn't Blind Attorney Attractiveness and Success in US Federal Court"
#Last Updated on 1/30/24

#########################################
######## Preliminaries ##################
#########################################
rm(list = ls())
set.seed(123)
library(MatchIt)
library(devtools)
library(CBPS)
library(DHARMa)
library(sandwich)
library(Matching)
library(stargazer)
library(cobalt)
library(ggplot2)

######### Read in datasets
AllCircuits<-read.csv("AllCircuitsData.csv")
NinthCircuit<-read.csv("NinthCircuitData.csv")
CaseLevel<-read.csv("CaseLevel.csv")
CorrData1<-read.csv("CorrData1.csv")
CorrData2<-read.csv("CorrData2.csv")

#########################################
######## Main Text Analysis #############
#########################################
######### Table 1
mean(AllCircuits$attractive_mean_AC_Opp)
sd(AllCircuits$attractive_mean_AC_Opp)

mean(AllCircuits$AIRating, na.rm = T)
sd(AllCircuits$AIRating, na.rm = T)

mean(NinthCircuit$attractive_mean_ac_9_Opp)
sd(NinthCircuit$attractive_mean_ac_9_Opp)

mean(NinthCircuit$OppUSDiffAttractivness9)
sd(NinthCircuit$OppUSDiffAttractivness9)

######### Figure 3
#Image Ratings
cbps.out<-CBPS(attractive_mean_AC_Opp~AllCircuits$NotFirstTime_Opp+
                 AllCircuits$NetAmicus+
                 AllCircuits$EliteLawSchool_Opp+
                 AllCircuits$OppResources+
                 AllCircuits$Enbanc+
                 AllCircuits$UnfavorableJudge+
                 AllCircuits$SharedGenderWithJudge+
                 AllCircuits$SharedRaceWithJudge+
                 AllCircuits$SharedGenderWithRater+
                 AllCircuits$SharedRaceWithRater, data = AllCircuits,
               method = "exact")
v <- data.frame(old = c("AllCircuits$NotFirstTime_Opp", "AllCircuits$NetAmicus", "AllCircuits$EliteLawSchool_Opp", "AllCircuits$OppResources", 
                        "AllCircuits$Enbanc", "AllCircuits$UnfavorableJudge", "AllCircuits$SharedGenderWithJudge", "AllCircuits$SharedRaceWithJudge",
                        "AllCircuits$SharedGenderWithRater","AllCircuits$SharedRaceWithRater"),
                new = c("Experienced Atty.", "Amicus Adv.", "Elite Law School Grad.", 
                        "Opp. Resources", "En banc", "Unfavorable Judge", "Shared Gender Judge", 
                        "Shared Race Judge", "Shared Gender Resp.","Shared Race Resp."))
love.plot(cbps.out, stats = "correlations",stars = "raw",colors = c("gray", "black"),
          shapes = c("circle", "circle"),sample.names = c("Original Data", "Matched Data"),
          var.names=v)+
  theme_bw()+
  labs(title = "Image Ratings\n", x="\nTreatment-Covariate Correlation")+
  theme( aspect.ratio=1, legend.position = "none",plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Computer Ratings
d1.ai<-as.data.frame(cbind(AllCircuits$OppVote,AllCircuits$binary_AI_1SD,
                           AllCircuits$NotFirstTime_Opp,
                           AllCircuits$NetAmicus, 
                           AllCircuits$EliteLawSchool_Opp,
                           AllCircuits$OppResources,
                           AllCircuits$Enbanc,
                           AllCircuits$UnfavorableJudge,
                           AllCircuits$SharedGenderWithJudge,
                           AllCircuits$SharedRaceWithJudge,
                           AllCircuits$AIRating))
colnames(d1.ai)<-c("OppVote", "binary_AI_1SD","NotFirstTime_Opp", "NetAmicus", "EliteLawSchool_Opp",
                   "OppResources", "Enbanc", "UnfavorableJudge", 
                   "SharedGenderWithJudge", "SharedRaceWithJudge", "AIRating")
matching.data.ai<-na.omit(d1.ai)
cbps.treat.ai<-matching.data.ai$AIRating
cbps.out.ai<-CBPS(cbps.treat.ai~matching.data.ai$NotFirstTime_Opp+
                    matching.data.ai$NetAmicus+
                    matching.data.ai$EliteLawSchool_Opp+
                    matching.data.ai$OppResources+
                    matching.data.ai$Enbanc+
                    matching.data.ai$UnfavorableJudge+
                    matching.data.ai$SharedGenderWithJudge+
                    matching.data.ai$SharedRaceWithJudge,
                  data=matching.data.ai,method = "exact")
v <- data.frame(old = c("matching.data.ai$NotFirstTime_Opp", "matching.data.ai$NetAmicus", "matching.data.ai$EliteLawSchool_Opp", "matching.data.ai$OppResources", 
                        "matching.data.ai$Enbanc", "matching.data.ai$UnfavorableJudge", "matching.data.ai$SharedGenderWithJudge", "matching.data.ai$SharedRaceWithJudge"
),
new = c("Experienced Atty.", "Amicus Adv.", "Elite Law School Grad.", 
        "Opp. Resources", "En banc", "Unfavorable Judge", "Shared Gender Judge", 
        "Shared Race Judge"))
love.plot(cbps.out.ai, stars = "raw",colors = c("gray", "black"),
          shapes = c("circle", "circle"), sample.names = c("Original Data", "Matched Data"),
          var.names=v)+
  theme_bw()+
  labs(title = "Computer Ratings\n", x="\nTreatment-Covariate Correlation")+
  theme( aspect.ratio=1,legend.position = "none" ,plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Video Ratings
cbps.out9<-CBPS(attractive_mean_ac_9_Opp~NinthCircuit$NotFirstTime_Opp+
                  NinthCircuit$NetAmicus+
                  NinthCircuit$EliteLawSchool_Opp+
                  NinthCircuit$OppResources+
                  NinthCircuit$Enbanc+
                  NinthCircuit$UnfavorableJudge+
                  NinthCircuit$SharedGenderWithJudge+
                  NinthCircuit$SharedRaceWithJudge+
                  NinthCircuit$SharedGenderWithRater+  
                  NinthCircuit$SharedRaceWithRater, 
                data = NinthCircuit,
                method = "exact")
v <- data.frame(old = c("NinthCircuit$NotFirstTime_Opp", "NinthCircuit$NetAmicus", "NinthCircuit$EliteLawSchool_Opp", "NinthCircuit$OppResources", 
                        "NinthCircuit$Enbanc", "NinthCircuit$UnfavorableJudge", "NinthCircuit$SharedGenderWithJudge", "NinthCircuit$SharedRaceWithJudge",
                        "NinthCircuit$SharedGenderWithRater", "NinthCircuit$SharedRaceWithRater"
),
new = c("Experienced Atty.", "Amicus Adv.", "Elite Law School Grad.", 
        "Opp. Resources", "En banc", "Unfavorable Judge", "Shared Gender Judge", 
        "Shared Race Judge", "Shared Gender Resp.", "Shared Race Resp."))
love.plot(cbps.out9, stars = "raw",colors = c("gray", "black"),
          shapes = c("circle", "circle"),sample.names = c("Original Data", "Matched Data"),
          var.names=v)+
  theme_bw()+
  labs(title = "Video Ratings\n", x="\nTreatment-Covariate Correlation")+
  theme( aspect.ratio=1, legend.position = "none" ,plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#US/Opp Differece Ratings
cbps.out.us<-CBPS(OppUSDiffAttractivness9~NinthCircuit$NotFirstTime_Opp+
                    NinthCircuit$NetAmicus+
                    NinthCircuit$EliteLawSchool_Opp+
                    NinthCircuit$OppResources+
                    NinthCircuit$Enbanc+
                    NinthCircuit$UnfavorableJudge+
                    NinthCircuit$SharedGenderWithJudge+
                    NinthCircuit$SharedRaceWithJudge+
                    NinthCircuit$SharedGenderWithRater+
                    NinthCircuit$SharedRaceWithRater  , 
                  data = NinthCircuit,
                  method = "exact")
v <- data.frame(old = c("NinthCircuit$NotFirstTime_Opp", "NinthCircuit$NetAmicus", "NinthCircuit$EliteLawSchool_Opp", "NinthCircuit$OppResources", 
                        "NinthCircuit$Enbanc", "NinthCircuit$UnfavorableJudge", "NinthCircuit$SharedGenderWithJudge", "NinthCircuit$SharedRaceWithJudge",
                        "NinthCircuit$SharedGenderWithRater",
                        "NinthCircuit$SharedRaceWithRater"  
),
new = c("Experienced Atty.", "Amicus Adv.", "Elite Law School Grad.", 
        "Opp. Resources", "En banc", "Unfavorable Judge", "Shared Gender Judge", 
        "Shared Race Judge", "Shared Gender Resp.", "Shared Race Resp."))
love.plot(cbps.out.us, stars = "raw",colors = c("gray", "black"),
          shapes = c("circle", "circle"),sample.names = c("Original Data", "Matched Data"),
          var.names=v)+
  theme_bw()+
  labs(title = "Opp-US Difference Ratings\n", x="\nTreatment-Covariate Correlation")+
  theme( aspect.ratio=1, legend.position = "none",plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Case Level
cbps.out.case<-CBPS(attractive_mean_AC_Opp~CaseLevel$NotFirstTime_Opp+
                      CaseLevel$NetAmicus+
                      CaseLevel$EliteLawSchool_Opp+
                      CaseLevel$OppResources+
                      CaseLevel$Enbanc+
                      CaseLevel$UnfavorablePanel+
                      CaseLevel$SharedGenderPanel+
                      CaseLevel$SharedRacePanel+
                      CaseLevel$SharedGenderWithRater+
                      CaseLevel$SharedRaceWithRater, data = CaseLevel,
                    method = "exact")
v <- data.frame(old = c("CaseLevel$NotFirstTime_Opp", "CaseLevel$NetAmicus", "CaseLevel$EliteLawSchool_Opp", "CaseLevel$OppResources", 
                        "CaseLevel$Enbanc", "CaseLevel$UnfavorablePanel", "CaseLevel$SharedGenderPanel", "CaseLevel$SharedRacePanel",
                        "CaseLevel$SharedGenderWithRater","CaseLevel$SharedRaceWithRater"),
                new = c("Experienced Atty.", "Amicus Adv.", "Elite Law School Grad.", 
                        "Opp. Resources", "En banc", "Unfavorable Panel", "Shared Gender Panel", 
                        "Shared Race Panel", "Shared Gender Resp.","Shared Race Resp."))
love.plot(cbps.out.case, stars = "raw",colors = c("gray", "black"),
          shapes = c("circle", "circle"),sample.names = c("Original Data", "Matched Data"),
          var.names=v)+
  theme_bw()+
  labs(title = "Case-Level Using Image Ratings\n", x="\nTreatment-Covariate Correlation")+
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5 ),legend.position="none",
         axis.title = element_text(family = "sans", size=10),
  )

######### Table 2
##Matching Performed in Figure 3 Required 
#Image Ratings
matched.cbps.logit<-glm(OppVote~attractive_mean_AC_Opp+
                          NotFirstTime_Opp+
                          NetAmicus+
                          EliteLawSchool_Opp+
                          OppResources+
                          Enbanc+
                          UnfavorableJudge+
                          SharedGenderWithJudge+
                          SharedRaceWithJudge,
                        data = AllCircuits,      
                        weights = cbps.out$weights,
                        family = quasibinomial(link = "logit"))
summary(matched.cbps.logit)
exp(matched.cbps.logit$coefficients)

#Computer Ratings
matched.cbps.logit.ai<-glm(OppVote~AIRating+
                             NotFirstTime_Opp+
                             NetAmicus+
                             EliteLawSchool_Opp+
                             OppResources+
                             Enbanc+
                             UnfavorableJudge+
                             SharedGenderWithJudge+
                             SharedRaceWithJudge,
                           data = matching.data.ai,   
                           weights = cbps.out.ai$weights,
                           family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.ai)
exp(matched.cbps.logit.ai$coefficients)

#Video Ratings
matched.cbps.logit.9<-glm(OppVote~attractive_mean_ac_9_Opp+
                            NotFirstTime_Opp+
                            NetAmicus+
                            EliteLawSchool_Opp+
                            OppResources+
                            Enbanc+
                            UnfavorableJudge+
                            SharedGenderWithJudge+
                            SharedRaceWithJudge,
                          data = NinthCircuit,   
                          weights = cbps.out9$weights,
                          family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.9)
exp(matched.cbps.logit.9$coefficients)

#US/Opp Difference Ratings
matched.cbps.logit.us<-glm(OppVote~OppUSDiffAttractivness9+
                             NotFirstTime_Opp+
                             NetAmicus+
                             EliteLawSchool_Opp+
                             OppResources+
                             Enbanc+
                             UnfavorableJudge+
                             SharedGenderWithJudge+
                             SharedRaceWithJudge,
                           data = NinthCircuit,   
                           weights = cbps.out.us$weights,
                           family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.us)
exp(matched.cbps.logit.us$coefficients)

######### Figure 4
##Models Fit in Table 2 Required 
#Image Ratings
pred.prob.vars<-c("NotFirstTime_Opp", "NetAmicus", "EliteLawSchool_Opp",
                     "OppResources", "Enbanc", "UnfavorableJudge", 
                     "SharedGenderWithJudge", "SharedRaceWithJudge")
pred.prob.data<-AllCircuits[pred.prob.vars]
pred.prob.data.final<-cbind.data.frame(rep(mean(pred.prob.data$NotFirstTime_Opp),100),
                                  rep(mean(pred.prob.data$NetAmicus),100),
                                  rep(mean(pred.prob.data$EliteLawSchool_Opp),100),
                                  rep(mean(pred.prob.data$OppResources),100),
                                  rep(mean(pred.prob.data$Enbanc),100),
                                  rep(mean(pred.prob.data$UnfavorableJudge),100),
                                  rep(mean(pred.prob.data$SharedGenderWithJudge),100),
                                  rep(mean(pred.prob.data$SharedRaceWithJudge),100))
colnames(pred.prob.data.final)<-pred.prob.vars

ilink<-family(matched.cbps.logit)$linkinv
pred.prob.data.final$attractive_mean_AC_Opp<-seq(0.1,10, by=.1)
pred.prob.data.final <- cbind(pred.prob.data.final,predict(matched.cbps.logit, newdata=pred.prob.data.final, type="link", se.fit=TRUE))
pred.prob.data.final <- within(pred.prob.data.final, {
  Fitted = ilink(pred.prob.data.final$fit) 
  Upper = ilink(pred.prob.data.final$fit + (2 * pred.prob.data.final$se.fit))
  Lower = ilink(pred.prob.data.final$fit - (2 * pred.prob.data.final$se.fit))
})

ggplot(pred.prob.data.final, aes(x=attractive_mean_AC_Opp, y = Fitted)) +
  geom_ribbon(data = pred.prob.data.final, aes(ymin = Lower, ymax = Upper, x = attractive_mean_AC_Opp),
              fill = "grey35", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(shape=16, size=1, fill="black") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(limits= c(1,10))+
  #geom_rug(sides='b')+
  labs(title= "Images\n", x="\nAttractiveness Rating (Images)", y="Probability of Receiving Vote\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Computer Ratings
pred.prob.data.ai.final<-cbind.data.frame(rep(mean(pred.prob.data$NotFirstTime_Opp),100),
                                  rep(mean(pred.prob.data$NetAmicus),100),
                                  rep(mean(pred.prob.data$EliteLawSchool_Opp),100),
                                  rep(mean(pred.prob.data$OppResources),100),
                                  rep(mean(pred.prob.data$Enbanc),100),
                                  rep(mean(pred.prob.data$UnfavorableJudge),100),
                                  rep(mean(pred.prob.data$SharedGenderWithJudge),100),
                                  rep(mean(pred.prob.data$SharedRaceWithJudge),100))
colnames(pred.prob.data.ai.final)<-c("NotFirstTime_Opp","NetAmicus","EliteLawSchool_Opp","OppResources",
                             "Enbanc","UnfavorableJudge","SharedGenderWithJudge","SharedRaceWithJudge")
pred.prob.data.ai.final$AIRating<-seq(0.1,10, by=.1)
ilink<-family(matched.cbps.logit.ai)$linkinv
pred.prob.data.ai.final <- cbind(pred.prob.data.ai.final,predict(matched.cbps.logit.ai, newdata=pred.prob.data.ai.final, type="link", se.fit=TRUE))
pred.prob.data.ai.final <- within(pred.prob.data.ai.final, {
  Fitted = ilink(pred.prob.data.ai.final$fit) 
  Upper = ilink(pred.prob.data.ai.final$fit + (2 * pred.prob.data.ai.final$se.fit))
  Lower = ilink(pred.prob.data.ai.final$fit - (2 * pred.prob.data.ai.final$se.fit))
})
pred.prob.data.ai.final$PP2<-predict(matched.cbps.logit.ai, newdata=pred.prob.data.ai.final, type="response")

#Plot
ggplot(pred.prob.data.ai.final, aes(x=AIRating, y = Fitted)) +
  geom_ribbon(data = pred.prob.data.ai.final, aes(ymin = Lower, ymax = Upper, x = AIRating),
              fill = "grey35", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(shape=16, size=1, fill="black") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(limits= c(1,10))+
  labs(title= "Computer\n", x="\nAttractiveness Rating (Comp.)", y="Probability of Receiving Vote\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Video Ratings
pred.prob.vars9<-c("NotFirstTime_Opp", "NetAmicus", "EliteLawSchool_Opp",
                     "OppResources", "Enbanc", "UnfavorableJudge", 
                     "SharedGenderWithJudge", "SharedRaceWithJudge")
pred.prob.data.9<-NinthCircuit[pred.prob.vars9]

pred.prob.data9.final<-cbind.data.frame(rep(mean(pred.prob.data.9$NotFirstTime_Opp),100),
                                  rep(mean(pred.prob.data.9$NetAmicus),100),
                                  rep(mean(pred.prob.data.9$EliteLawSchool_Opp),100),
                                  rep(mean(pred.prob.data.9$OppResources),100),
                                  rep(mean(pred.prob.data.9$Enbanc),100),
                                  rep(mean(pred.prob.data.9$UnfavorableJudge),100),
                                  rep(mean(pred.prob.data.9$SharedGenderWithJudge),100),
                                  rep(mean(pred.prob.data.9$SharedRaceWithJudge),100))
colnames(pred.prob.data9.final)<-c("NotFirstTime_Opp","NetAmicus","EliteLawSchool_Opp",
                             "OppResources","Enbanc","UnfavorableJudge","SharedGenderWithJudge","SharedRaceWithJudge")
pred.prob.data9.final$attractive_mean_ac_9_Opp<-seq(0.1,10, by=.1)
ilink<-family(matched.cbps.logit.9)$linkinv
pred.prob.data9.final <- cbind(pred.prob.data9.final,predict(matched.cbps.logit.9, newdata=pred.prob.data9.final, type="link", se.fit=TRUE))
pred.prob.data9.final <- within(pred.prob.data9.final, {
  Fitted = ilink(pred.prob.data9.final$fit) 
  Upper = ilink(pred.prob.data9.final$fit + (2 * pred.prob.data9.final$se.fit))
  Lower = ilink(pred.prob.data9.final$fit - (2 * pred.prob.data9.final$se.fit))
})

ggplot(pred.prob.data9.final, aes(x=attractive_mean_ac_9_Opp, y = Fitted)) +
  geom_ribbon(data = pred.prob.data9.final, aes(ymin = Lower, ymax = Upper, x = attractive_mean_ac_9_Opp),
              fill = "grey35", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(shape=16, size=1, fill="black") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(limits= c(1,10))+
  labs(title= "Ninth Circuit Videos\n", x="\nAttractiveness Rating (Videos)", y="Probability of Receiving Vote\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#US/Opp Difference Ratings
pred.prob.data.usdiff.final<-cbind.data.frame(rep(mean(pred.prob.data.9$NotFirstTime_Opp),78),
                                  rep(mean(pred.prob.data.9$NetAmicus),78),
                                  rep(mean(pred.prob.data.9$EliteLawSchool_Opp),78),
                                  rep(mean(pred.prob.data.9$OppResources),78),
                                  rep(mean(pred.prob.data.9$Enbanc),78),
                                  rep(mean(pred.prob.data.9$UnfavorableJudge),78),
                                  rep(mean(pred.prob.data.9$SharedGenderWithJudge),78),
                                  rep(mean(pred.prob.data.9$SharedRaceWithJudge),78))
colnames(pred.prob.data.usdiff.final)<-c("NotFirstTime_Opp","NetAmicus","EliteLawSchool_Opp",
                             "OppResources","Enbanc","UnfavorableJudge","SharedGenderWithJudge","SharedRaceWithJudge")
pred.prob.data.usdiff.final$OppUSDiffAttractivness9<-seq(-4.6,3.1, by=.1)

ilink<-family(matched.cbps.logit.us)$linkinv
pred.prob.data.usdiff.final <- cbind(pred.prob.data.usdiff.final,predict(matched.cbps.logit.us, newdata=pred.prob.data.usdiff.final, type="link", se.fit=TRUE))
pred.prob.data.usdiff.final <- within(pred.prob.data.usdiff.final, {
  Fitted = ilink(pred.prob.data.usdiff.final$fit) 
  Upper = ilink(pred.prob.data.usdiff.final$fit + (2 * pred.prob.data.usdiff.final$se.fit))
  Lower = ilink(pred.prob.data.usdiff.final$fit - (2 * pred.prob.data.usdiff.final$se.fit))
})

ggplot(pred.prob.data.usdiff.final, aes(x=OppUSDiffAttractivness9, y = Fitted)) +
  geom_ribbon(data = pred.prob.data.usdiff.final, aes(ymin = Lower, ymax = Upper, x = OppUSDiffAttractivness9),
              fill = "grey35", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(shape=16, size=1, fill="black") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(limits= c(-4.6,3.1))+
  labs(title= "Opp-US Difference\n", x="\nOpp-US Attractivness Difference", y="Probability of Receiving Vote\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

######### Table 3
##Matching Performed in Figure 3 Required 
matched.cbps.logit.case<-glm(OppWin~attractive_mean_AC_Opp+
                               NotFirstTime_Opp+
                               NetAmicus+
                               EliteLawSchool_Opp+
                               OppResources+
                               Enbanc+
                               UnfavorablePanel+
                               SharedGenderPanel+
                               SharedRacePanel,
                             data = CaseLevel,      
                             weights = cbps.out.case$weights,
                             family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.case)
exp(matched.cbps.logit.case$coefficients)

######### Figure 5
##Models Fit in Table 3 Required 
pred.prob.vars.case<-c("NotFirstTime_Opp", "NetAmicus", "EliteLawSchool_Opp",
                          "OppResources", "Enbanc", "UnfavorablePanel", 
                          "SharedGenderPanel", "SharedRacePanel")
pred.prob.data.case<-CaseLevel[pred.prob.vars.case]
ilink<-family(matched.cbps.logit.case)$linkinv
pred.prob.data.case.final<-cbind.data.frame(rep(mean(pred.prob.data.case$NotFirstTime_Opp),100),
                                  rep(mean(pred.prob.data.case$NetAmicus),100),
                                  rep(mean(pred.prob.data.case$EliteLawSchool_Opp),100),
                                  rep(mean(pred.prob.data.case$OppResources),100),
                                  rep(mean(pred.prob.data.case$Enbanc),100),
                                  rep(mean(pred.prob.data.case$UnfavorablePanel),100),
                                  rep(mean(pred.prob.data.case$SharedGenderPanel),100),
                                  rep(mean(pred.prob.data.case$SharedRacePanel),100))
colnames(pred.prob.data.case.final)<-pred.prob.vars.case
pred.prob.data.case.final$attractive_mean_AC_Opp<-seq(0.1,10, by=.1)
pred.prob.data.case.final <- cbind(pred.prob.data.case.final,predict(matched.cbps.logit.case, newdata=pred.prob.data.case.final, type="link", se.fit=TRUE))
pred.prob.data.case.final <- within(pred.prob.data.case.final, {
  Fitted = ilink(pred.prob.data.case.final$fit) 
  Upper = ilink(pred.prob.data.case.final$fit + (2 * pred.prob.data.case.final$se.fit))
  Lower = ilink(pred.prob.data.case.final$fit - (2 * pred.prob.data.case.final$se.fit))
})


#Plot
options(warn=-1)
ggplot(pred.prob.data.case.final, aes(x=attractive_mean_AC_Opp, y = Fitted)) +
  geom_ribbon(data = pred.prob.data.case.final, aes(ymin = Lower, ymax = Upper, x = attractive_mean_AC_Opp),
              fill = "grey35", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(shape=16, size=1, fill="black") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(limits= c(1,10))+
  labs(title= "Predicted Probability of Winning a Case (Post-CBPS)\n", x="\nAttractiveness Rating (Images)", y="Probability of Winning Case\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#########################################
######## Appendix Analysis ##############
#########################################

######### Table A.1
mean(AllCircuits$OppVote)
var(AllCircuits$OppVote)

ai.data <- AllCircuits[which(AllCircuits$AIRating!="NA"),]
mean(ai.data$OppVote)
var(ai.data$OppVote)

mean(NinthCircuit$OppVote)
var(NinthCircuit$OppVote)

usopp.data <- NinthCircuit[which(NinthCircuit$OppUSDiffAttractivness9 !="NA"),]
mean(usopp.data$OppVote)
var(usopp.data$OppVote)

mean(CaseLevel$OppWin)
var(CaseLevel$OppWin)

######### Figure A.1

#Image Ratings
full.plot<-ggplot(AllCircuits, aes(x=attractive_mean_AC_Opp))
full.plot + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(attractive_mean_AC_Opp)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")+
  theme_bw()+
  scale_x_continuous(limits= c(1, 10))+
  labs(title= "Distribution of Attractiveness Scores From Images", x="\nAttractiveness Score", y="Frequency\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Video Ratings
ninth.plot<-ggplot(NinthCircuit, aes(x=attractive_mean_ac_9_Opp))
ninth.plot + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(attractive_mean_ac_9_Opp)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")+
  theme_bw()+
  scale_x_continuous(limits= c(1, 10))+
  labs(title= "Distribution of Attractiveness Scores From Recorded Videos", x="\nAttractiveness Score", y="Frequency\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#Computer Ratings
ai.plot<-ggplot(AllCircuits, aes(x=AIRating))
ai.plot + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(AIRating, na.rm=T)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")+
  theme_bw()+
  scale_x_continuous(limits= c(1, 10))+
  labs(title= "Distribution of Attractiveness Scores From Computer Evaluation", x="\nAttractiveness Score", y="Frequency\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

#US Opp Difference Ratings
us.plot<-ggplot(NinthCircuit, aes(x=OppUSDiffAttractivness9))
us.plot + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(OppUSDiffAttractivness9, na.rm=T)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")+
  theme_bw()+
  scale_x_continuous(limits= c(-4.6, 3.1))+
  labs(title= "Distribution of Attractivness Advantage Between Opposing Attorneys", x="\nDifference in Attractiveness Score", y="Frequency\n") +
  theme( aspect.ratio=1, plot.title = element_text(family = "sans", face="bold", size=13, hjust=0.5),
         axis.title = element_text(family = "sans", size=10),
  )

######### Table A.2
cor(AllCircuits$AIRating, AllCircuits$attractive_mean_AC_Opp, use = "complete.obs")
cor(NinthCircuit$attractive_mean_ac_9_Opp, NinthCircuit$attractive_mean_AC_Opp, use = "complete.obs")
cor(NinthCircuit$OppUSDiffAttractivness9, NinthCircuit$attractive_mean_AC_Opp, use = "complete.obs")

cor(CorrData1$AIRating, CorrData1$attractive_mean_ac_9_opp, use = "complete.obs")
cor(CorrData2$AIRating, CorrData2$OppUSDiffAttractivness9, use = "complete.obs")

cor(NinthCircuit$attractive_mean_ac_9_Opp, NinthCircuit$OppUSDiffAttractivness9)

######### Table A.3
summary(AllCircuits$NotFirstTime_Opp)
var(AllCircuits$NotFirstTime_Opp)

summary(AllCircuits$NetAmicus)
var(AllCircuits$NetAmicus)

summary(AllCircuits$EliteLawSchool_Opp)
var(AllCircuits$EliteLawSchool_Opp)

summary(AllCircuits$OppResources)
var(AllCircuits$OppResources)

summary(AllCircuits$Enbanc)
var(AllCircuits$Enbanc)

summary(AllCircuits$UnfavorableJudge)
var(AllCircuits$UnfavorableJudge)

summary(AllCircuits$SharedGenderWithJudge)
var(AllCircuits$SharedGenderWithJudge)

summary(AllCircuits$SharedRaceWithJudge)
var(AllCircuits$SharedRaceWithJudge)

######### Table A.4
summary(NinthCircuit$NotFirstTime_Opp)
var(NinthCircuit$NotFirstTime_Opp)

summary(NinthCircuit$NetAmicus)
var(NinthCircuit$NetAmicus)

summary(NinthCircuit$EliteLawSchool_Opp)
var(NinthCircuit$EliteLawSchool_Opp)

summary(NinthCircuit$OppResources)
var(NinthCircuit$OppResources)

summary(NinthCircuit$Enbanc)
var(NinthCircuit$Enbanc)

summary(NinthCircuit$UnfavorableJudge)
var(NinthCircuit$UnfavorableJudge)

summary(NinthCircuit$SharedRaceWithJudge)
var(NinthCircuit$SharedRaceWithJudge)

summary(NinthCircuit$SharedGenderWithJudge)
var(NinthCircuit$SharedGenderWithJudge)

######### Table A.5
summary(AllCircuits$SharedGenderWithRater)
var(AllCircuits$SharedGenderWithRater)

summary(AllCircuits$SharedRaceWithRater)
var(AllCircuits$SharedRaceWithRater)

######### Table A.6
#Image Ratings
basic.logit<-glm(OppVote~attractive_mean_AC_Opp+
                   NotFirstTime_Opp+
                   NetAmicus+
                   EliteLawSchool_Opp+
                   OppResources+
                   Enbanc+
                   UnfavorableJudge+
                   SharedGenderWithJudge+
                   SharedRaceWithJudge,
                 data = AllCircuits,
                 family = binomial(link='logit'))
summary(basic.logit)

#Computer Ratings
basic.logit.ai<-glm(OppVote~AIRating+
                      NotFirstTime_Opp+
                      NetAmicus+
                      EliteLawSchool_Opp+
                      OppResources+
                      Enbanc+
                      UnfavorableJudge+
                      SharedGenderWithJudge+
                      SharedRaceWithJudge,
                    data = AllCircuits,
                    family = binomial(link='logit'))
summary(basic.logit.ai)

#Video Ratings
ninth.basic.logit<-glm(OppVote~attractive_mean_ac_9_Opp+
                         NotFirstTime_Opp+
                         NetAmicus+
                         EliteLawSchool_Opp+
                         OppResources+
                         Enbanc+
                         UnfavorableJudge+
                         SharedGenderWithJudge+
                         SharedRaceWithJudge,
                       data = NinthCircuit,
                       family = binomial(link='logit'))
summary(ninth.basic.logit)

#US Opp Difference Ratings
us.basic.logit<-glm(OppVote~OppUSDiffAttractivness9+
                      NotFirstTime_Opp+
                      NetAmicus+
                      EliteLawSchool_Opp+
                      OppResources+
                      Enbanc+
                      UnfavorableJudge+
                      SharedGenderWithJudge+
                      SharedRaceWithJudge,
                    data = NinthCircuit,
                    family = binomial(link='logit'))
summary(us.basic.logit)

######### Table A.7
balance(cbps.out)

######### Table A.8
balance(cbps.out.ai)

######### Table A.9
balance(cbps.out9)

######### Table A.10
balance(cbps.out.us)

######### Table A.11
balance(cbps.out.case)

######### Table A.12
##Analysis Performed in Table 2 Required 
#Image Ratings
summary(matched.cbps.logit)

#Computer Ratings
summary(matched.cbps.logit.ai)

#Video Ratings
summary(matched.cbps.logit.9)

#US/Opp Difference Ratings
summary(matched.cbps.logit.us)

######### Table A.13
#Image Ratings
View(pred.prob.data.final)

#Computer Ratings
View(pred.prob.data.ai.final)

#Video Ratings
View(pred.prob.data9.final)

#US/Opp Difference Ratings
View(pred.prob.data.usdiff.final)

######### Table A.14
basic.logit.case<-glm(OppWin~attractive_mean_AC_Opp+
                               NotFirstTime_Opp+
                               NetAmicus+
                               EliteLawSchool_Opp+
                               OppResources+
                               Enbanc+
                               UnfavorablePanel+
                               SharedGenderPanel+
                               SharedRacePanel,
                             data = CaseLevel,      
                             family = binomial(link = "logit"))
summary(basic.logit.case)
summary(matched.cbps.logit.case)

######### Table A.15
View(pred.prob.data.case.final)

######### Table A.16
mean(AllCircuits$binary_attractive_Opp_1SD)
sd(AllCircuits$binary_attractive_Opp_1SD)

mean(AllCircuits$binary_AI_1SD, na.rm=T)
sd(AllCircuits$binary_AI_1SD, na.rm=T)

mean(NinthCircuit$binary_attractive_9_Opp_1SD)
sd(NinthCircuit$binary_attractive_9_Opp_1SD)

mean(NinthCircuit$AttractiveAdvantage9)
sd(NinthCircuit$AttractiveAdvantage9)

######### Table A.17
#Image Ratings
cem.matching<-matchit(binary_attractive_Opp_1SD~
                        NotFirstTime_Opp+
                        NetAmicus+
                        EliteLawSchool_Opp+
                        OppResources+
                        Enbanc+
                        UnfavorableJudge+
                        SharedGenderWithRater+
                        SharedRaceWithRater+
                        SharedGenderWithJudge+
                        SharedRaceWithJudge,
                      data = AllCircuits,
                      method = "cem",
                      estimand = "ATT")

matched.data.cem<-match.data(cem.matching)

matched.cem.logit<-glm(OppVote~binary_attractive_Opp_1SD+
                         NotFirstTime_Opp+
                         NetAmicus+
                         EliteLawSchool_Opp+
                         OppResources+
                         UnfavorableJudge+
                         SharedGenderWithJudge+
                         SharedRaceWithJudge,
                       data = matched.data.cem,
                       weights = weights,
                       family = binomial(link = "logit"))
summary(matched.cem.logit)

#Computer Ratings
d1.ai<-as.data.frame(cbind(AllCircuits$OppVote,AllCircuits$binary_AI_1SD,
                           AllCircuits$NotFirstTime_Opp,
                           AllCircuits$NetAmicus, 
                           AllCircuits$EliteLawSchool_Opp,
                           AllCircuits$OppResources,
                           AllCircuits$Enbanc,
                           AllCircuits$UnfavorableJudge,
                           AllCircuits$SharedGenderWithJudge,
                           AllCircuits$SharedRaceWithJudge,
                           AllCircuits$AIRating))
colnames(d1.ai)<-c("OppVote", "binary_AI_1SD","NotFirstTime_Opp", "NetAmicus", "EliteLawSchool_Opp",
                   "OppResources", "Enbanc", "UnfavorableJudge", 
                   "SharedGenderWithJudge", "SharedRaceWithJudge", "AIRating")
matching.data.ai<-na.omit(d1.ai)
cem.matching.ai<-matchit(binary_AI_1SD~
                           NotFirstTime_Opp+
                           NetAmicus+
                           EliteLawSchool_Opp+
                           OppResources+
                           Enbanc+
                           UnfavorableJudge+
                           SharedGenderWithJudge+
                           SharedRaceWithJudge,
                         data = matching.data.ai,
                         method = "cem",
                         estimand = "ATT")

matched.data.cem.ai<-match.data(cem.matching.ai)

matched.cem.logit.ai<-glm(OppVote~binary_AI_1SD+
                            NotFirstTime_Opp+
                            NetAmicus+
                            EliteLawSchool_Opp+
                            OppResources+
                            UnfavorableJudge+
                            SharedGenderWithJudge+
                            SharedRaceWithJudge,
                          data = matched.data.cem.ai,
                          weights = weights,
                          family = binomial(link = "logit"))
summary(matched.cem.logit.ai)

#Video Ratings
cem.matching9<-matchit(NinthCircuit$binary_attractive_9_Opp_1SD~
                         NinthCircuit$NotFirstTime_Opp+
                         NinthCircuit$NetAmicus+
                         NinthCircuit$EliteLawSchool_Opp+
                         NinthCircuit$OppResources+
                         NinthCircuit$Enbanc+
                         NinthCircuit$UnfavorableJudge+
                         NinthCircuit$SharedGenderWithJudge+
                         NinthCircuit$SharedRaceWithJudge,
                       data = NinthCircuit,
                       method = "cem",
                       estimand = "ATT")


matched.data.cem9<-match.data(cem.matching9)

matched.cem.logit9<-glm(OppVote~binary_attractive_9_Opp_1SD+
                          NotFirstTime_Opp+
                          NetAmicus+
                          EliteLawSchool_Opp+
                          OppResources+
                          UnfavorableJudge+
                          SharedGenderWithJudge+
                          SharedRaceWithJudge,
                        data = matched.data.cem9,
                        weights = weights,
                        family = binomial(link = "logit"))
summary(matched.cem.logit9)

#US/Opp Difference Ratings
cem.matching.us<-matchit(NinthCircuit$AttractiveAdvantage9~
                           NotFirstTime_Opp+
                           NetAmicus+
                           EliteLawSchool_Opp+
                           OppResources+
                           Enbanc+
                           UnfavorableJudge+
                           SharedGenderWithJudge+
                           SharedRaceWithJudge,
                         data = NinthCircuit,
                         method = "cem",
                         estimand = "ATT")

matched.data.cem.us<-match.data(cem.matching.us)

matched.cem.logit.us<-glm(OppVote~AttractiveAdvantage9+
                            NotFirstTime_Opp+
                            NetAmicus+
                            EliteLawSchool_Opp+
                            OppResources+
                            UnfavorableJudge+
                            SharedGenderWithJudge+
                            SharedRaceWithJudge,
                          data = matched.data.cem.us,
                          weights = weights,
                          family = binomial(link = "logit"))
summary(matched.cem.logit.us)

######### Table A.18
case.logit.bin<-glm(OppWin~binary_attractive_Opp_1SD+
                      NotFirstTime_Opp+
                      NetAmicus+
                      EliteLawSchool_Opp+
                      OppResources+
                      Enbanc+
                      UnfavorablePanel+
                      SharedGenderPanel+
                      SharedRacePanel,
                    data = CaseLevel,      
                    family = binomial(link = "logit"))
summary(case.logit.bin)

cem.matching.case<-matchit(binary_attractive_Opp_1SD~
                             NotFirstTime_Opp+
                             NetAmicus+
                             EliteLawSchool_Opp+
                             OppResources+
                             UnfavorablePanel+
                             SharedGenderPanel,
                           data = CaseLevel,
                           method = "cem",
                           estimand = "ATT")

matched.data.cem.case<-match.data(cem.matching.case)

matched.cem.logit.case<-glm(OppWin~binary_attractive_Opp_1SD+
                              NotFirstTime_Opp+
                              NetAmicus+
                              EliteLawSchool_Opp+
                              OppResources+
                              Enbanc+
                              UnfavorablePanel+
                              SharedGenderPanel+
                              SharedRacePanel,
                            data = matched.data.cem.case,
                            weights = weights,
                            family = binomial(link = "logit"))
summary(matched.cem.logit.case)
######### Table A.19
summary(AllCircuits$WS)
var(AllCircuits$WS)

######### Table A.20
ws.basic.logit<-glm(OppVote~AllCircuits$WS+
                      NotFirstTime_Opp+
                      NetAmicus+
                      EliteLawSchool_Opp+
                      OppResources+
                      Enbanc+
                      UnfavorableJudge+
                      SharedGenderWithJudge+
                      SharedRaceWithJudge,
                    data = AllCircuits,
                    family = binomial(link = "logit"))
summary(ws.basic.logit)

cbps.treat<-as.matrix(AllCircuits$WS)
cbps.X<-as.matrix(cbind(AllCircuits$NotFirstTime_Opp,
                        AllCircuits$NetAmicus, 
                        AllCircuits$EliteLawSchool_Opp,
                        AllCircuits$OppResources,
                        AllCircuits$Enbanc,
                        AllCircuits$UnfavorableJudge,
                        AllCircuits$SharedGenderWithJudge,
                        AllCircuits$SharedRaceWithJudge,
                        AllCircuits$SharedGenderWithRater,
                        AllCircuits$SharedRaceWithRater))
cbps.Y<-AllCircuits$OppVote
cbps.out<-CBPS(cbps.treat~cbps.X, method = "exact")

matched.cbps.logit.ws<-glm(OppVote~WS+
                             NotFirstTime_Opp+
                             NetAmicus+
                             EliteLawSchool_Opp+
                             OppResources+
                             Enbanc+
                             UnfavorableJudge+
                             SharedGenderWithJudge+
                             SharedRaceWithJudge,
                           data = AllCircuits,      
                           weights = cbps.out$weights,
                           family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.ws)

######### Table A.21
add.basic.logit<-glm(OppVote~attractive_mean_AC_Opp+
                       NotFirstTime_Opp+
                       NetAmicus+
                       EliteLawSchool_Opp+
                       OppResources+
                       Enbanc+
                       UnfavorableJudgeIdeo+
                       OppAppt+
                       SharedGenderWithJudge+
                       SharedRaceWithJudge+
                       SCClerk_Opp+
                       CircuitClerk_Opp+
                       as.factor(CIRCUIT),
                     data = AllCircuits,
                     family = binomial(link = "logit"))
summary(add.basic.logit)

cbps.treat.add<-as.matrix(AllCircuits$attractive_mean_AC_Opp)
cbps.add<-as.matrix(cbind(AllCircuits$NotFirstTime_Opp,
                          AllCircuits$NetAmicus, 
                          AllCircuits$EliteLawSchool_Opp,
                          AllCircuits$OppResources,
                          AllCircuits$Enbanc,
                          AllCircuits$UnfavorableJudgeIdeo,
                          AllCircuits$SharedGenderWithJudge,
                          AllCircuits$SharedRaceWithJudge,
                          AllCircuits$SharedGenderWithRater,
                          AllCircuits$SharedRaceWithRater,
                          AllCircuits$CIRCUIT))

cbps.add.dv<-AllCircuits$OppVote
cbps.out.add<-CBPS(cbps.treat.add~cbps.add, method = "exact")

add.matched.cbps.logit<-glm(OppVote~attractive_mean_AC_Opp+
                              NotFirstTime_Opp+
                              NetAmicus+
                              EliteLawSchool_Opp+
                              OppResources+
                              Enbanc+
                              UnfavorableJudgeIdeo+
                              OppAppt+
                              SharedGenderWithJudge+
                              SharedRaceWithJudge+
                              SCClerk_Opp+
                              CircuitClerk_Opp+
                              as.factor(CIRCUIT),
                            data = AllCircuits,      
                            weights = cbps.out.add$weights,
                            family = quasibinomial(link = "logit"))
summary(add.matched.cbps.logit)

######### Table A.22
rg.logit.cont<-glm(OppVote~attractive_mean_AC_Opp+
                     Female_Opp+
                     NonWhite_Opp+
                     NotFirstTime_Opp+
                     NetAmicus+
                     EliteLawSchool_Opp+
                     OppResources+
                     Enbanc+
                     UnfavorableJudge+
                     SharedGenderWithJudge+
                     SharedRaceWithJudge,
                   data = AllCircuits,      
                   family = binomial(link = "logit"))
summary(rg.logit.cont)


cbps.out.rg<-CBPS(attractive_mean_AC_Opp~
                    AllCircuits$NonWhite_Opp+
                    AllCircuits$Female_Opp+
                    AllCircuits$NotFirstTime_Opp+
                    AllCircuits$NetAmicus+
                    AllCircuits$EliteLawSchool_Opp+
                    AllCircuits$OppResources+
                    AllCircuits$Enbanc+
                    AllCircuits$UnfavorableJudge+
                    AllCircuits$SharedGenderWithJudge+
                    AllCircuits$SharedRaceWithJudge+
                    AllCircuits$SharedGenderWithRater+
                    AllCircuits$SharedRaceWithRater, data = AllCircuits,
                  method = "exact")

matched.cbps.logit.rg<-glm(OppVote~attractive_mean_AC_Opp+
                             Female_Opp+
                             NonWhite_Opp+
                             NotFirstTime_Opp+
                             NetAmicus+
                             EliteLawSchool_Opp+
                             OppResources+
                             Enbanc+
                             UnfavorableJudge+
                             SharedGenderWithJudge+
                             SharedRaceWithJudge,
                           data = AllCircuits,      
                           weights = cbps.out.rg$weights,
                           family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.rg)
######### Table A.23
AllCircuits$gender.cont.treatment<-AllCircuits$attractive_mean_AC_Opp*AllCircuits$JudgeMale*AllCircuits$Female_Opp
gender.logit.cont<-glm(OppVote~attractive_mean_AC_Opp*JudgeMale*Female_Opp+
                         NotFirstTime_Opp+
                         NetAmicus+
                         EliteLawSchool_Opp+
                         OppResources+
                         Enbanc+
                         UnfavorableJudge+
                         SharedRaceWithJudge,
                       data = AllCircuits,      
                       family = binomial(link = "logit"))
summary(gender.logit.cont)

cbps.out.gender<-CBPS(gender.cont.treatment~AllCircuits$NotFirstTime_Opp+
                        AllCircuits$NetAmicus+
                        AllCircuits$EliteLawSchool_Opp+
                        AllCircuits$OppResources+
                        AllCircuits$Enbanc+
                        AllCircuits$UnfavorableJudge+
                        AllCircuits$SharedRaceWithJudge+
                        AllCircuits$SharedGenderWithRater+
                        AllCircuits$SharedRaceWithRater, data = AllCircuits,
                      method = "exact")
balance(cbps.out.gender)

matched.cbps.logit.gender<-glm(OppVote~attractive_mean_AC_Opp*JudgeMale*Female_Opp+
                                 NotFirstTime_Opp+
                                 NetAmicus+
                                 EliteLawSchool_Opp+
                                 OppResources+
                                 Enbanc+
                                 UnfavorableJudge+
                                 SharedRaceWithJudge,
                               data = AllCircuits,      
                               weights = cbps.out.gender$weights,
                               family = quasibinomial(link = "logit"))
summary(matched.cbps.logit.gender)
