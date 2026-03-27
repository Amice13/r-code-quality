### Chapter 2 The influence of different stakeholders on the legislative duration in the initial stage of decision-making process
### 1. Load the dataset:
dataset2_1 <- read.table("./original data/chapter2.excel", header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
attach(Dataset)
library(multcomp)
library(MASS)
library(magrittr)

#Displaying the distribution of legislative duration 

ggplot(data=data2_1, mapping=aes(x=duration))+
  geom_histogram(fill="gray", color="black", binwidth=5)+
  labs(x="Legislative duration (in months)", y="Frequency")

# descriptive statistics 
summary(data2_1)
sd(data1$duration)
var(data1$duration)

# correlation matrix
cor(data2_1, method='pearson')


# Multilevel negative binomial regression (Table 2.1)
# Model1 main explanatory variables
model1 <- glm.nb(duration ~ hetero.sta+sta.sup.num+sta.opp.num+hetero.interest+hetero.nonstate, data = data2_1)
summary(model1)
# Model2 consultation related control variables
model2<-glm.nb(duration~hetero.sta+sta.sup.num+sta.opp.num+hetero.interest+hetero.nonstate+density+diversity, data=data2_1)
summary(model2)

# Model3 legislation related control variables
model3<-glm.nb(duration~hetero.sta+sta.sup.num+sta.opp.num+hetero.interest+hetero.nonstate+Dir+Reg+Dec+novelty+transparent+complexity, data=data2_1)
summary(model3)

# Model4 all control variables
model4<-glm.nb(duration~hetero.sta+sta.sup.num+sta.opp.num+hetero.interest+hetero.nonstate+density+diversity+Dir+Reg+Dec+novelty+transparent+complexity, data=data2_1)
summary(model4)


library(ggplot2)
library(MASS)
attach(data2_1)

#Figure 2.1 Predicted counts for Preference heterogeneity among stakeholders
#pdf("./plots/pdf/figure 2.1.pdf",width=9,height=6)
#png("./plots/png/figure 2.1.png",width=1100, height=800, res=120)
par(mfrow=c(1,1))
more_data<- data.frame(hetero.sta=rep(-0.5,1),sta.sup.num=rep(10,100,200), sta.opp.num=rep(10, 100, 200), hetero.interest=rep(-0.5,1),hetero.nonstate=rep(-0.5,1))
pred_1<-predict(model1,newdata=more_data)
pred_1
plot_1<-ggplot(data=data2_1, aes(x=hetero.sta, y=duration))+
  geom_point()+geom_smooth(method = "glm.nb")
plot_1<-plot_1+geom_point(data=more_data, aes(y=pred_1))+labs(x="Preference heterogeneity among stakeholders", y="Predicted counts of duration in months") 
plot_1

#Figure 2.2 Predicted counts for Stakeholders support
#pdf("./plots/pdf/figure 2.2.pdf",width=9,height=6)
#png("./plots/png/figure 2.2.png",width=1100, height=800, res=120)
plot_2<-ggplot(data=data2_1, aes(x=sta.sup.num, y=duration))+
  geom_point()+geom_smooth(method="glm.nb")
plot_2<-plot_2+geom_point(data=more_data, aes(y=pred_1))+labs(x="Stakeholders support", y="Predicted counts of duration in months")
plot_2
#Figure 2.3 Predicted counts for Stakeholders opposition
#pdf("./plots/pdf/figure 2.3.pdf",width=9,height=6)
#png("./plots/png/figure 2.3.png",width=1100, height=800, res=120)
plot_3<-ggplot(data=data2_1, aes(x=sta.opp.num, y=duration))+
  geom_point()+geom_smooth(method="glm.nb")
plot_3<-plot_3+geom_point(data=more_data,aes(y=pred_1))+labs(x="Stakeholders opposition", y="Predicted counts of duration in months")
plot_3

#Figure 2.4 Predicted counts for Preference heterogeneity among EU interest groups
#pdf("./plots/pdf/figure 2.4.pdf",width=9,height=6)
#png("./plots/png/figure 2.4.png",width=1100, height=800, res=120)
plot_4<-ggplot(data=data2_1,aes(x=hetero.interest, y=duration))+
  geom_point()+geom_smooth(method="glm.nb")
plot_4<-plot_4+geom_point(data=more_data,aes(y=pred_1))+labs(x="Preference heterogeneity among EU interest groups", y="Predicted counts of duration in months")
plot_4

#Figure 2.5 Predicted counts for Preference heterogeneity among non-state groups
#pdf("./plots/pdf/figure 2.5.pdf",width=9,height=6)
#png("./plots/png/figure 2.5.png",width=1100, height=800, res=120)
plot_5<-ggplot(data=data2_1,aes(x=hetero.nonstate,y=duration))+
  geom_point()+geom_smooth(method="glm.nb")
plot_5<-plot_5+geom_point(data=more_data,aes(y=pred_1))+labs(x="Preference heterogeneity among non-state groups", y="Predicted counts of duration in months")
plot_5



# Alternative model specifications (Table C2 in Appendix)
# ModelA main explanatory variables (different measurement for explanatory variables)
modela=glm(deu_duration~hetero.sta+sta.sup+sta.opp+hetero.interest+hetero.nonstate,quasipoisson, data2_1)
summary(modela)

# ModelB consultation related control variables (different measurement for explanatory variables)
modelb=glm(deu_duration~hetero.sta+sta.sup+sta.opp+hetero.interest+hetero.nonstate+density+diversity,quasipoisson, data2_1)
summary(modelb)

# ModelC legislation related control variables (different measurement for explanatory variables)
modelc=glm(deu_duration~hetero.sta+sta.sup+sta.opp+hetero.interest+hetero.nonstate+density+diversity+Dir+Reg+Dec+novelty+transparent+complexity,quasipoisson, data2_1)
summary(modelc)

# ModelD all control variables (different measurement for explanatory variables)
modeld=glm(deu_duration~hetero.sta+sta.sup+sta.opp+hetero.interest+hetero.nonstate+Dir+Reg+Dec+novelty+transparent+complexity,quasipoisson, data2_1)
summary(modeld)

# Negative binomial regression for actor types (Table C2)
# interest groups versus non-state groups
# Model E heterogenity of EU interest groups 
modele=glm(duration~hetero.sta+sta.sup.num+sta.opp.num+hetero.interest,quasipoisson, data2_1)
summary(modele)

# Model F heterogenity of non-state groups
modelf=glm(duration~hetero.sta+sta.sup.num+sta.opp.num+hetero.nonstate,quasipoisson, data2_1)
summary(modelf)

# Figure A2. Plots of the bivariate relationship between legislative duration and main explanatory factors  
par(mfrow=c(1,2))
par(mar=c(5,5,2,1))
# png(file="Figure A2.png", width=600,height=400, res=200)
reg<-lm(duration~hetero.interest, data2_1)
summary(reg)
summary(reg)$r.squared
cor(data2_1$duration, data2_1$hetero.interest)^2
summary(reg)$r.squared == cor(data2_1$duration, data2_1$hetero.interest)^2
plot(data2_1$duration~data2_1$hetero.interest, xlab="Preference heterogeneity among EU interest groups", ylab="Legislative duration (in months)")
abline(reg)

reg1<-lm(duration~hetero.nonstate, data2_1)
summary(reg1)
summary(reg1)$r.squared
cor(data2_1$duration,data2_1$hetero.nonstate)^2
summary(reg1)$r.squared == cor(data2_1$duration,data2_1$hetero.nonstate)^2
plot(data2_1$duration~data2_1$hetero.nonstate, xlab="Preference heterogeneity among non-state groups", ylab="Legislative duration (in months)")
abline(reg1)

reg2<-lm(duration~sta.sup,data2_1)
summary(reg2)
summary(reg2)$r.squared
cor(data2_1$duration,data2_1$sta.sup)^2
summary(reg2)$r.squared == cor(data2_1$duration,data2_1$sta.sup)^2
plot(data2_1$duration~data2_1$sta.sup, xlab="Stakeholders support", ylab="Legislative duration (in months)")
abline(reg2)


reg3<-lm(duration~sta.opp, data2_1)
summary(reg3)
summary(reg3)$r.squared
cor(data2_1$duration,data2_1$sta.opp)^2
summary(reg3)$r.squared == cor(data2_1$duration,data2_1$sta.opp)^2
plot(data2_1$duration~data2_1$sta.opp, xlab="Stakeholders opposition", ylab="Legislative duration (in months)")
abline(reg3)
###THE END

