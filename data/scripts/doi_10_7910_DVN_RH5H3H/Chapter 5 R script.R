### Chapter 5 The responsiveness of decision-makers to preferences of stakeholders in the EU legislative process
### 1. Load the dataset:
library(readxl)
data5_0 <- read_excel("Desktop/Dataset Archive/Chapter 5 Dataset/data5.0.xlsx")
View(data5_0)

# descriptive statistics Table A2
summary(data5_0)
sd(data5_0$po.position)
sd(data5_0$pcom)
sd(data5_0$pre.congruence)    
sd(data5_0$neg.interest)     
sd(data5_0$neg.nonstate)   
sd(data5_0$neg.stakeholder)
sd(data5_0$hetero.sta)
sd(data5_0$salience)
sd(data5_0$salience.dt)
sd(data5_0$eulegactor)
sd(data5_0$actcom)
sd(data5_0$actboepcou)
sd(data5_0$actcou)
sd(data5_0$deu.duration)
sd(data5_0$duration.m)
sd(data5_0$contestation)
sd(data5_0$leg.duration)
sd(data5_0$novelty)
sd(data5_0$policy.change)
sd(data5_0$complexity)
sd(data5_0$typeact)

# correlation matrix Table A3
cor(data5_0, method="pearson")


attach(data5_0)

#Table5.1 Multilevel OLS regression
#Model 1: DV:output score of decision-making; main IDV: interest groups: H1
model1<-lm(po.position~neg.interest+salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
summary(model1)
#Model 2: DV: position score of European Commission; main IDV: non-state groups: H2
model2<-lm(pcom~neg.nonstate+salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
summary(model2)
#Model 3:Interaction between Preference heterogeneity among stakeholders and salience H3
model3<-lm(po.position~hetero.sta*salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
summary(model3)

library(ggeffects)
library(sjPlot)
library(cowplot)

#Model 3: Figure 5.4
#pdf("./plots/pdf/hetero_sa_int.pdf",width=9,height=6)
#png("./plots/png/hetero_sa_int.png",width=1100, height=800, res=120)
par(mfrow=c(1,1))
library(car)
data5_0$salience_recoded<-recode(data5_0$salience.dt, '0="Low salience"; 1="High salience"')
m3<-lm(po.position~hetero.sta*salience_recoded+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
cbind(round(coef(m3),3),paste0("(",round(summary(m3)$coefficients[,2],3), ")"),round(summary(m3)$coefficients[,4],3))
plot_model(m3, type ="int",axis.title="Policy position of legislators",legend.title ="salience", title="")+labs(x="Preference heterogeneity among stakeholders")
#dev.off()


#Model 4: include all explanatory variables
model4<-lm(po.position~neg.interest+neg.nonstate+hetero.sta*salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
summary(model4)


#TableA4 Models of bivariate relationships between explanatory variables without controls
#Model A
modela<-lm(po.position~neg.interest+neg.nonstate+hetero.sta, data=data5_0) 
summary(modela)
#Model B
modelb<-lm(pcom~neg.interest+neg.nonstate+hetero.sta, data=data5_0)
summary(modelb)
#Model C
modelc<-lm(po.position~neg.interest+neg.nonstate+hetero.sta*salience, data=data5_0)
summary(modelc)



#TableA5 Additional Logistic regression models (DV is preference congruence)
#TableA5 Model 1 all EU decision-makers
modelA1<-glm(pre.congruence~neg.interest+salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, family = "binomial")
summary(modelA1)
cbind(round(coef(modelA1),3),paste0("(",round(summary(modelA1)$coefficients[,2],3), ")"),round(summary(modelA1)$coefficients[,4],3))

#TableA5 Model 2 European Commission
modelA2<-glm(pre.congruence~neg.nonstate+salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0, family = "binomial")
summary(modelA2)
cbind(round(coef(modelA2),3),paste0("(",round(summary(modelA2)$coefficients[,2],3), ")"),round(summary(modelA2)$coefficients[,4],3))

#TableA5 Model 3 EP and The Council
modelA3<-glm(pre.congruence~hetero.sta*salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0, family = "binomial")
summary(modelA3)
cbind(round(coef(modelA3),3),paste0("(",round(summary(modelA3)$coefficients[,2],3), ")"),round(summary(modelA3)$coefficients[,4],3))

#TableA5 Model 4 The Council only
modelA4<-glm(pre.congruence~neg.interest+neg.nonstate+hetero.sta*salience+eulegactor+actcom+actboepcou+actcou+contestation+duration.m+novelty+policy.change+complexity, data=data5_0, family = "binomial")
summary(modelA4)



#Table A6: Robustness checks of models of policy position of EU decision-makers
#Model 1 all EU decision-makers
model1<-lm(po.position~neg.interest+neg.nonstate+hetero.sta*salience+eulegactor+actcom+actboepcou+actcou+typeact+contestation+duration.m+novelty+policy.change+complexity)
summary(model1)

#Model 2 European Commission
model2<-lm(po.position~neg.interest+neg.nonstate+hetero.sta*salience+eulegactor+actcom+typeact+contestation+duration.m+novelty+policy.change+complexity)
summary(model2)

#Model 3 EP andThe Council
model3<-lm(po.position~neg.interest+neg.nonstate+hetero.sta*salience+eulegactor+actboepcou+typeact+contestation+duration.m+novelty+policy.change+complexity)
summary(model3)

#Model 4 The Council only
model4<-lm(po.position~neg.interest+neg.nonstate+hetero.sta*salience+eulegactor+actcou+typeact+contestation+duration.m+novelty+policy.change+complexity)
summary(model4)


#Model 5 Interaction between interest groups opposition and salience, H3
model5<-lm(po.position~neg.interest*salience+neg.nonstate+eulegactor+actcom+actboepcou+actcou+typeact+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
summary(model5)
cbind(round(coef(model5),3),paste0("(",round(summary(model5)$coefficients[,2],3), ")"),round(summary(model5)$coefficients[,4],3))
# Model 5 Figure A1
#pdf("./plots/pdf/hetero_sa_int.pdf",width=9,height=6)
#png("./plots/png/hetero_sa_int.png",width=1100, height=800, res=120)
data5_0$salience_recoded<-recode(data5_0$salience.dt, '0="Low salience"; 1="High salience"')
model5<-lm(po.position~neg.interest*salience_recoded+neg.nonstate+eulegactor+actcom+actboepcou+actcou+typeact+contestation+duration.m+novelty+policy.change+complexity, data=data5_0)
cbind(round(coef(model5),3),paste0("(",round(summary(model5)$coefficients[,2],3), ")"),round(summary(model5)$coefficients[,4],3))
par(mfrow=c(1,1))
plot_model(model5, type ="int",axis.title="Policy position of legislators",legend.title ="salience", title="")+labs(x="Interest groups opposition")
#dev.off()


#Model 6 Interaction between interest groups opposition and legislative duration, H3
model6<-lm(po.position~neg.interest*duration.m+eulegactor+actcom+actboepcou+actcou+typeact+salience+contestation+novelty+policy.change+complexity, data=data5_0)
summary(model6)
#Model 6 Figure A2
#pdf("./plots/pdf/hetero_sa_int.pdf",width=9,height=6)
#png("./plots/png/hetero_sa_int.png",width=1100, height=800, res=120)
data5_0$leg.duration_recoded<-recode(data5_0$leg.duration,'0="Short duration"; 1="Long duration"')
model6<-lm(po.position~neg.interest*leg.duration_recoded+eulegactor+actcom+actboepcou+actcou+typeact+salience+contestation+novelty+policy.change+complexity, data=data5_0)
par(mfrow=c(1,1))
plot_model(model6, type ="int",axis.title="Policy position of legislators",axis.lim = c(0,100),
           legend.title ="Legislative duration", title="")+labs(x="Interest groups opposition")
#dev.off()

#Model 7 Interaction between Preference heterogeneity among stakeholders and legislative duration, Figure A3
#pdf("./plots/pdf/hetero_sa_int.pdf",width=9,height=6)
#png("./plots/png/hetero_sa_int.png",width=1100, height=800, res=120)
model7<-lm(po.position~hetero.sta*leg.duration_recoded+eulegactor+actcom+actboepcou+actcou+typeact+salience+contestation+novelty+policy.change+complexity, data=data5_0)
plot_model(model7,type ="int",axis.title="Policy position of legislators",legend.title ="Legislative duration", title="")+labs(x="Preference heterogeneity among stakeholders")
#dev.off()
###THE END





