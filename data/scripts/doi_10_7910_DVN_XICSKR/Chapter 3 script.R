### Chapter 3 The impact of interaction between stakeholders and member states on the decision-making duration of the European Union       
### 1. Load the dataset:
data3_0 <- read.table("./original data/chapter3.excel", header=TRUE, sep="\t", na.strings="NA", dec=",", strip.white=TRUE)
attach(data3_0)
library(multcomp)
library(MASS)
library(magrittr)
library(ggplot2)
library(sjmisc)
library(sjPlot)
data(efc)
theme_set(theme_sjplot())

#Displaying the distribution of legislative duration (Figure A1) 
ggplot(data=data3_0, mapping=aes(x=duration1))+
  geom_histogram(fill="gray", color="black", binwidth=5)+
  labs(x="Legislative duration (in months)", y="Frequency")

# descriptive statistics (Table 3.1)
summary(data3_0)
sd(data3_0$duration1)
var(data3_0$duration1)
sd(data3_0$intraconflict)
var(data3_0$intraconflict)
sd(data3_0$aitems)
var(data3_0$aitems)
sd(data3_0$bitems)
var(data3_0$bitems)
sd(data3_0$hetero.sta)
var(data3_0$hetero.sta)
sd(data3_0$sta.sup)
var(data3_0$sta.sup)
sd(data3_0$sta.opp) 
var(data3_0$sta.opp)
sd(data3_0$interconflict)
var(data3_0$interconflict)
sd(data3_0$epconflict)
var(data3_0$epconflict)
sd(data3_0$saliency)
var(data3_0$saliency)
sd(data3_0$con.duration)
var(data3_0$con.duration)

# correlation matrix (Table A2)
cor(data3_0, method="pearson")

#additional options to change
#type ="analysis and plotting produce that is performed
#axis.title = "label for y-axis"
#axis.lim = "range of values along y-axis"
#title ="title for your plot"
#legend.title = "title for your legend"

# multilevel negative binomial regression (Table 3.2)
# Model1 test H1:intra-institutional conflict* preference heterogeneity among stakeholders
model1=glm(duration1~intraconflict*hetero.sta+aitem+bitem+sta.sup+sta.opp+
             interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model1)

#Model 1:Figure 3.2
#pdf("./plots/pdf/figure3.2.pdf",width=9,height=6)
#png("./plots/png/figure 3.2.png",width=1100, height=800, res=120)
par(mfrow=c(1,1))
library(car)
data3_0$intraconflict_recoded<-recode(data3_0$intraconflict, '0="no intraconflict"; 1="intraconflict in the Council"')
m1=glm(duration1~hetero.sta*intraconflict_recoded+aitems+bitems+sta.sup+sta.opp+
         interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
plot_model(m1, type ="int",axis.title="Predicted counts of legislative duration ",legend.title ="", title="")+labs(x="Preference heterogeneity among stakeholders")
#dev.off()


# Model2 test H2:stakeholders’support*Preference homogeneity of member states
model2=glm(duration1~intraconflict+hetero.sta+aitems*sta.sup+bitems+sta.opp
           +interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model2)

# Model2 Figure 3.3
#pdf("./plots/pdf/figure3.3.pdf",width=9,height=6)
#png("./plots/png/figure3.3.png",width=1100, height=800, res=120)
par(mfrow=c(1,1))
data3_0$aitem_recoded<-recode(data3_0$aitem, '0="no homogeneity"; 1="homogeneity"')
m2=glm(duration1~intraconflict+hetero.sta+sta.sup*aitem_recoded+bitem+sta.opp
       +interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
plot_model(m2, type ="int",axis.title="Predicted counts of legislative duration ",legend.title ="", title="")+labs(x="Stakeholders support")
#dev.off()


# Model3 test H3: stakeholders’ opposition*Preference heterogeneity of member states 
model3=glm(duration1~intraconflict+hetero.sta+aitems+sta.sup+bitems*sta.opp+
             interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model3)

# Model3 Figure 3.4
#pdf("./plots/pdf/figure3.3.pdf",width=9,height=6)
#png("./plots/png/figure3.3.png",width=1100, height=800, res=120)
par(mfrow=c(1,1))
data3_0$bitem_recoded<-recode(data3_0$bitem, '0="no heterogeneity"; 1="heterogeneity"')
m3=glm(duration1~intraconflict+hetero.sta+aitem+sta.sup+sta.opp*bitem_recoded+
         interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
plot_model(m3, type ="int",axis.title="Predicted counts of legislative duration ",legend.title ="", title="")+labs(x="Stakeholders opposition")
#dev.off()


# Model4 test all hypotheses 
model4=glm(duration1~intraconflict*hetero.sta+aitems*sta.sup+bitems*sta.opp+
             interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model4)



# alternative model specifications (Table A3)
# model A
modela=glm(duration1~intraconflict+aitems+bitems+hetero.sta+sta.sup+sta.opp+interconflict+epconflict+saliency+con.duration,quasipoisson, data3_0)
summary(modela)
# model B
modelb=glm(duration1~intraconflict*hetero.sta+interconflict+epconflict+saliency+con.duration,quasipoisson, data3_0)
summary(modelb)
# model C
modelc=glm(duration1~aitems*sta.sup+interconflict+epconflict+saliency+con.duration,quasipoisson, data3_0)
summary(modelc)
# model D
modeld=glm(duration1~bitems*sta.opp+interconflict+epconflict+saliency+con.duration,quasipoisson, data3_0)
summary(modeld)

# alternative operationalization of the explanatory factors (Table A4)
# model 1
model1=glm(duration2~intraconflict*hetero.sta+aitems+bitems+sta.sup.num+sta.opp.num+interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model1)
# model 2
model2=glm(duration2~intraconflict+hetero.sta+aitems*sta.sup.num+bitems+sta.opp.num+interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model2)
# model 3
model3=glm(duration2~intraconflict+hetero.sta+aitems+sta.sup.num+bitems*sta.opp.num+interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model3)
# model 4
model4=glm(duration2~intraconflict*hetero.sta+aitems*sta.sup.num+bitems*sta.opp.num+interconflict+epconflict+saliency+con.duration, quasipoisson, data3_0)
summary(model4)



# OLS regression model (Table A5)
# Model 1: intra-institutional conflict* preference heterogeneity among stakeholders
model1<-lm(duration1~intraconflict*hetero.sta+aitems+bitems+sta.sup.num+sta.opp.num+interconflict+epconflict+saliency+con.duration, data=data3_0)
summary(model1)
# Model 2:Preference homogeneity of member states* Stakeholders support
model2<-lm(duration1~intraconflict+hetero.sta+aitems*sta.sup.num+sta.opp.num+bitems+interconflict+epconflict+saliency+con.duration,data=data3_0)
summary(model2)
# Model 3:Preference heterogeneity of member states * Stakeholders opposition
model3<-lm(duration1~intraconflict+hetero.sta+aitems+bitems*sta.opp.num+sta.sup.num+interconflict+epconflict+saliency+con.duration, data=data3_0)
summary(model3)
# Model 4:all interactions
model4<-lm(duration1~intraconflict*hetero.sta+aitems*sta.sup.num+bitems*sta.opp.num+interconflict+epconflict+saliency+con.duration, data=data3_0)
summary(model4)

###THE END







