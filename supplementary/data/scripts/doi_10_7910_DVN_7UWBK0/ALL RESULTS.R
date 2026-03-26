
### STUDY 1
# You need to read in the "All6Conditions.RData" file
# "study" is the data from the Turk survey
# "factorial" is the data from the first 4 conditions
# "theoretical conditions" is the data from the last two conditions

# The number of participants who have to be dropped
sum(as.numeric(study1[,3]))
# Reduced data
X2<-subset(study1[which(study1[,3]==0),])

# Descriptives for trials dropped
apply(X2[,5:8],2,FUN=mean)
apply(X2[,5:8],2,FUN=sum)
nrow(X2)
(nrow(X2)*80)-39-29-17-13

# white status bias measure
wsbstudy1<-as.numeric(X2[,4])
wsbstudy1<- wsbstudy1 * -1
# Histogram of white status bias (Figure 2)
library(ggplot2)
ggplot(as.data.frame(wsbstudy1), aes(wsbstudy1)) + geom_density(aes(), alpha=0.3) +
  scale_fill_manual( values=c( "grey1") ) +
  scale_color_manual (values=c("grey1")) + theme_bw() +
  geom_histogram(breaks=seq(-1.2, 1, by=.04)) +
  ylab ("Density") + xlab("") +
  theme(text = element_text(size=15))


white1<-ifelse(X2[,32]==1,1,0)
black1<-ifelse(X2[,32]==2,1,0)
hispanic1<-ifelse(X2[,32]==5,1,0)
asian1<-ifelse(X2[,32]==6,1,0)
other1<-rep(1,nrow(X2)) - (white1+black1+hispanic1+asian1)
age1<-X2[,26]
female1<-X2[,28]
class1<-X2[,29]
educ1<-X2[,30]
conlib1<-X2[,31]
age1<- age1 - mean(age1,na.rm=TRUE)
female1 <- female1 - mean(female1,na.rm=TRUE)
class1 <- class1 - mean(class1,na.rm=TRUE)
educ1 <- educ1 - mean(educ1,na.rm=TRUE)
conlib1 <- conlib1 - mean(conlib1,na.rm=TRUE)
dat<-data.frame(wsbstudy1,white1,black1,hispanic1,asian1,other1,age1,female1,class1,educ1,conlib1)
# Model in Table 3
summary(lm(wsbstudy1 ~ white1 + hispanic1 + asian1 + other1 + age1 + female1 + class1 + educ1 + conlib1,data=dat))
summary(lm(wsbstudy1 ~ black1 + hispanic1 + asian1 + other1 + age1 + female1 + class1 + educ1 + conlib1,data=dat))

# The explicit status belief items
explicits1<-X2[,9:25]
# Reverse code so all items favor whites
explicits1[,c(1,3,5,8,10,13,15,17)]<-10-explicits1[,c(1,3,5,8,10,13,15,17)]
require(psych)
# Loadings in Table 2, Model 1
fa(explicits1,nfactors=1,rotate="oblimin",fm="minres")
explicits1<-explicits1[,-c(2,12,13)]
# Loadings in Table 2, Model 2
fa(explicits1,nfactors=1,rotate="oblimin",fm="minres")

# Correlation, as reported in the text
explicits1<-rowSums(explicits1)
cor(explicits1,wsbstudy1)

### STUDY 2

# Get only the subject-level data
these5<-seq(from=1,to=nrow(factorial),by=20)
factorial2<-factorial[these5,]
# Race or cat/dog IAT
table(factorial2[,2])
# White
table(factorial2[,3])

whitestatusbias<-factorial2[,16]
raceiatcondition<-factorial2[,2]
white<-factorial2[,3]
ps<-factorial2[,18]

# Reverse code "whitestatusbias" so it is a measure of "white status bias"
whitestatusbias<- -1*whitestatusbias

# Get only the subject-level data for the theoretically relevant conditions
these6<-seq(from=1,to=nrow(theoreticalconditions),by=20)
theoreticalconditions2<-theoreticalconditions[these6,]
white2<-theoreticalconditions2[,6]
ps2<-theoreticalconditions2[,17]
whitestatusbias2<-theoreticalconditions2[,16]
# Reverse code "whitestatusbias2" so it is a measure of "white status bias"
whitestatusbias2<- -1*whitestatusbias2

# Explicit status beliefs. First, combine conditions
explicit<-factorial2[,19:29]
explicit2<-theoreticalconditions2[,18:28]
explicit<-rbind(explicit,explicit2)
# Recode explicit so white is relatively high status
explicit<-10-explicit
require(psych)
fa(explicit,nfactors=1,rotate="oblimin",fm="minres")
explicit<-explicit[,-1]
fa(explicit,nfactors=1,rotate="oblimin",fm="minres")
explicit<-explicit[,-2]
fa(explicit,nfactors=1,rotate="oblimin",fm="minres")
explicit<-explicit[,-2]
fa(explicit,nfactors=1,rotate="oblimin",fm="minres")
explicit<-explicit[,-6]
fa(explicit,nfactors=1,rotate="oblimin",fm="minres")
explicit<-explicit[,-5]
fa(explicit,nfactors=1,rotate="oblimin",fm="minres")
alpha(explicit)
# All loadings are .4 or higher and alpha is .66
explicit<-rowSums(explicit)
explicit2<-explicit[79:120]
explicit<-explicit[1:78]

# Third order status beliefs
thirdorder2<-theoreticalconditions2[,29:35]
#fa(thirdorder2,nfactors=1,rotate="oblimin",fm="minres")
#alpha(thirdorder2)
# All loadings are now above .4, and alpha is .94
thirdorder2<-rowSums(thirdorder2)

###Table 4
# Factorial Animal IAT
aggregate(subset(ps,raceiatcondition==0),by=list(subset(white,raceiatcondition==0)),FUN="mean")
aggregate(subset(whitestatusbias,raceiatcondition==0),by=list(subset(white,raceiatcondition==0)),FUN="mean")
aggregate(subset(whitestatusbias,raceiatcondition==0),by=list(subset(white,raceiatcondition==0)),FUN="sd")
aggregate(subset(explicit,raceiatcondition==0),by=list(subset(white,raceiatcondition==0)),FUN="mean")
aggregate(subset(explicit,raceiatcondition==0),by=list(subset(white,raceiatcondition==0)),FUN=sd)
#Factorial Race IAT
aggregate(subset(ps,raceiatcondition==1),by=list(subset(white,raceiatcondition==1)),FUN="mean")
aggregate(subset(whitestatusbias,raceiatcondition==1),by=list(subset(white,raceiatcondition==1)),FUN="mean")
aggregate(subset(whitestatusbias,raceiatcondition==1),by=list(subset(white,raceiatcondition==1)),FUN="sd")
aggregate(subset(explicit,raceiatcondition==1),by=list(subset(white,raceiatcondition==1)),FUN="mean")
aggregate(subset(explicit,raceiatcondition==1),by=list(subset(white,raceiatcondition==1)),FUN=sd)
#Theoretically relevant conditions
aggregate(ps2,by=list(white2),FUN=mean)
aggregate(whitestatusbias2,by=list(white2),FUN=mean)
aggregate(whitestatusbias2,by=list(white2),FUN=sd)
aggregate(thirdorder2,by=list(white2),FUN=mean)
aggregate(thirdorder2,by=list(white2),FUN=sd)
aggregate(explicit2,by=list(white2),FUN=mean)
aggregate(explicit2,by=list(white2),FUN=sd)
# Correlations
cor(cbind(ps,whitestatusbias,explicit))
cor(cbind(ps2,whitestatusbias2,thirdorder2,explicit2))


t.test(subset(explicit,white==0),mu=30)
t.test(subset(explicit,white==1),mu=30)
t.test(subset(thirdorder2,white2==0),mu=35)
t.test(subset(thirdorder2,white2==1),mu=35)


### Table 5
# Model 4
ps3<-c(ps,ps2)
white3<-c(white,white2)
raceiatcondition3<-c(raceiatcondition,rep(1,length(white2)))
summary(lm(ps3~white3*raceiatcondition3))
# Model 1
whitestatusbias3<-c(whitestatusbias,whitestatusbias2)
summary(lm(subset(whitestatusbias3,raceiatcondition3==1)~subset(white3,raceiatcondition3==1)))
#black3<-ifelse(white3==1,0,1)
#summary(lm(subset(whitestatusbias3,raceiatcondition3==1)~subset(black3,raceiatcondition3==1)))
# Model 2
summary(lm(whitestatusbias3~white3*raceiatcondition3))

### Illustrate the interaction in Model 2
#install.packages("emmeans")
m1<-lm(whitestatusbias3~white3*raceiatcondition3)
require(emmeans)
xw<-(white3+1)*3+raceiatcondition3
m4<-lm(whitestatusbias3 ~ as.factor(xw))
marmeans1<-as.data.frame(emmeans(m4, ~ xw ))
marmeans1[,1]<-c("Black","Black","White","White")
marmeans1
colnames(marmeans1)[1]<-"Participant Race"
marmeans1[,4]<-rep(c("Animal Evaluations","Racial Status"),2)
colnames(marmeans1)[4]<-"IAT"
marmeans1[,3]<-rep(c("Black","Gray"),2)
colnames(marmeans1)[3]<-"color"
require(ggplot2)
sp<-ggplot(marmeans1, aes(x=IAT, y=emmean, group=`Participant Race`,color=`Participant Race`)) + 
  theme_bw() +
  xlab("IAT") +
  ylab("Marginal Means") +
  scale_colour_manual(values=c("#000000", "#999999", "#000000","#999999"))+
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL))
sp 

# Model 5
summary(lm(subset(ps3,raceiatcondition3==1)~subset(white3,raceiatcondition3==1)))
# Model 6
summary(lm(subset(ps3,raceiatcondition3==1)~subset(white3,raceiatcondition3==1) + subset(whitestatusbias3,raceiatcondition3==1)))


### Inference for the first indirect effect
model4<-(lm(subset(ps3,raceiatcondition3==1)~subset(white3,raceiatcondition3==1) + subset(whitestatusbias3,raceiatcondition3==1)))
model1<-(lm(subset(whitestatusbias3,raceiatcondition3==1)~subset(white3,raceiatcondition3==1)))
indirecteffect1<-coef(model4)[3]*coef(model1)[2]
indirectdata1<-data.frame(subset(ps3,raceiatcondition3==1),subset(white3,raceiatcondition3==1),subset(whitestatusbias3,raceiatcondition3==1))
indirectdist1<-rep(0,1000)
for(i in 1:1000){
  these<-sample(1:82,78,replace=TRUE)
  dat<-indirectdata1[these,]
  model4<-lm(dat[,1]~dat[,2] + dat[,3],data=dat)
  model1<-lm(dat[,3]~dat[,2],data=dat)
  indirectdist1[i]<-coef(model4)[3]*coef(model1)[2]}
indirecteffect1
c(sort(indirectdist1)[25],sort(indirectdist1)[975])


# Inference for the conditional process model
m<-whitestatusbias3
w<-raceiatcondition3
x<-white3
y<-ps3

m1<-lm(m ~ x + w + x*w)
m2<- lm(y ~ x + m + w + m*w)
# Model 2 
summary(m1)
# Model 7 in Table 5
summary(m2)
# Conditional process model, i.e., moderated mediation
# The model is on p. 398, Fig 11.1, C
# The equations for it, is on p. 406 (Example 4)

# x on m (in the cat/dog condition):
coef(m1)[2]
# m on y
coef(m2)[3]

# Indirect effect for the cat/dog condition
coef(m1)[2]*coef(m2)[3]

# x on m (in the race condition):
(coef(m1)[2] + coef(m1)[4])
# m on y (in the race condition):
(coef(m1)[2]*coef(m2)[3]) + ((coef(m1)[2]*coef(m2)[5]) + (coef(m1)[4]*coef(m2)[3])) + (coef(m1)[4]*coef(m2)[5]) 

indirect.cats<-coef(m1)[2]*coef(m2)[3]
indirect.race<-(coef(m1)[2]*coef(m2)[3]) + ((coef(m1)[2]*coef(m2)[5]) + (coef(m1)[4]*coef(m2)[3])) + (coef(m1)[4]*coef(m2)[5]) 


indirect.dist<-matrix(0,nr=1000,nc=2)
set.seed(1982)
for(i in 1:1000){
  cases<-sample(1:120,115,replace=TRUE)
  m2<-m[cases]
  x2 <- x[cases]
  w2 <- w[cases]
  y2 <- y[cases]
  
  m1<-lm(m2 ~ x2 + w2 + x2*w2)
  m2<- lm(y2 ~ x2 + m2 + w2 + m2*w2)
  
  # Indirect effect for the cat/dog condition
  indirect.dist[i,1]<-coef(m1)[2]*coef(m2)[3]
  
  # Indirect effect for the race condition
  indirect.dist[i,2]<-(coef(m1)[2]*coef(m2)[3]) + ((coef(m1)[2]*coef(m2)[5]) + (coef(m1)[4]*coef(m2)[3])) + (coef(m1)[4]*coef(m2)[5]) 
}

hist(indirect.dist[,1])
hist(indirect.dist[,2])
indirect.dist.cat<-sort(indirect.dist[,1])
c(indirect.dist.cat[25],indirect.dist.cat[976])
indirect.dist.race<-sort(indirect.dist[,2])
c(indirect.dist.race[25],indirect.dist.race[976])

colnames(indirect.dist)<-c("Implicit Animal Evaluations","White Status Bias")
indirect.dist<-as.data.frame(indirect.dist)
indirect.dist2<-stack(indirect.dist)
colnames(indirect.dist2)<-c("values","IAT")
library(ggplot2)
ggplot(indirect.dist2, aes(x=values)) + geom_density(aes(group=`IAT`, colour=`IAT`, fill=`IAT`), alpha=0.1) +
  scale_fill_manual( values=c( "#999999","#000000" ) ) +
  scale_color_manual (values=c("#999999","#000000")) + theme_bw() +
  ylab ("Density") + xlab("") +
  theme(text = element_text(size=15))











# Inference for the explicit status beliefs scale (in a footnote)
explicit3<-c(explicit,explicit2)
model4<-(lm(ps3~white3 + explicit3))
model1<-(lm(explicit3~white3))
indirecteffect1<-coef(model4)[3]*coef(model1)[2]
indirectdata1<-data.frame(ps3,white3,explicit3)
indirectdist1<-rep(0,1000)
for(i in 1:1000){
  these<-sample(1:82,78,replace=TRUE)
  dat<-indirectdata1[these,]
  model4<-lm(dat[,1]~dat[,2] + dat[,3],data=dat)
  model1<-lm(dat[,3]~dat[,2],data=dat)
  indirectdist1[i]<-coef(model4)[3]*coef(model1)[2]}
indirecteffect1
c(sort(indirectdist1)[25],sort(indirectdist1)[975])






# And mediated moderation (third order)
# Fig 11.2 B on p. 404
m1<-lm(whitestatusbias2 ~ white2)
int<-whitestatusbias2*thirdorder2
m2<-lm(ps2 ~ white2 + whitestatusbias2 + thirdorder2 + int)
# Model 3 in Table 5
summary(m1)
# Model 8 in Table 5
summary(m2)
# "Conditional indirect effect of X on Y through M" (p 405)
coef(m1)[2]*coef(m2)[3] + coef(m1)[2]*coef(m2)[5]*mean(thirdorder2)
#ab1 + ab3W
coef(m1)[2]*coef(m2)[3] + coef(m1)[2]*coef(m2)[5]*(mean(thirdorder2)-sd(thirdorder2))
coef(m1)[2]*coef(m2)[3] + coef(m1)[2]*coef(m2)[5]*(mean(thirdorder2)+sd(thirdorder2))
# P 425 "index of moderated mediation"
coef(m1)[2]*coef(m2)[5]
imodmed<-rep(0,1000)
data5<-data.frame(ps2,white2,whitestatusbias2,thirdorder2,int)
for( i in 1:1000){
  cases<-sample(1:42,36)
  dat2<-data5[cases,]
  m1<-lm(whitestatusbias2 ~ white2,data=dat2)
  m2<-lm(ps2 ~ white2 + whitestatusbias2 + thirdorder2 + int,data=dat2)
  imodmed[i]<-coef(m1)[2]*coef(m2)[5]}
c(sort(imodmed)[25],sort(imodmed)[975])



marginsdata2<-data.frame(white2=.5,whitestatusbias2=rep(c(-.04-2*.37,-.04-1*.37,-.04,-.04+1*.37,-.04+2*.37),2),thirdorder2=rep(c(mean(thirdorder2)-sd(thirdorder2),mean(thirdorder2)+sd(thirdorder2)),each=5))
marginsdata2<-cbind(marginsdata2,marginsdata2[,2]*marginsdata2[,3])
colnames(marginsdata2)[4]<-"int"
margins3<-cbind(marginsdata2,predict(m2,newdata=marginsdata2,type="response",se.fit=TRUE))
ThirdOrder<-rep(c("-1 SD","+1 SD"),each=5)
margins3<-data.frame(margins3,ThirdOrder)
xaxis<-rep(c("-2 SD","-1 SD","Mean","+1 SD","+2 SD"),2)

require(ggplot2)
marplot1<-ggplot(margins3, aes(x=whitestatusbias2, y=fit,group=ThirdOrder)) + 
  # Use a black and white theme
  theme_bw() +
  # Label for the x and y-axes
  xlab("White Status Bias") +
  ylab("P(S)") +
  geom_point()+
  geom_pointrange(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit)) +
  scale_linetype_manual(name="Third Order Beliefs",values=c(2,1)) +
  scale_x_continuous(breaks=c(-.78,-.41,-.04,.33,.70), labels=c("-2 SD","-1 SD","Mean","+1 SD","+2 SD")) +
  geom_line(aes(linetype=ThirdOrder)) 
marplot1



