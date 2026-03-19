setwd("...")
library(epiDisplay)

alc <- c(25,24,24,27,26,26,24,25,25,27,26,25,25,25,26)
noalc <- c(20,20,19,21,23,22,19,20,23,21,22,19,19,22,22)
data <- data.frame(alc, noalc)
shapiro.test(data$alc)
shapiro.test(data$noalc)

var.test(alc,noalc, data = data)
t.test(data$alc,data$noalc)

#man whitney u test
whrmale<-c(0.99,0.98,0.98,0.91,0.55,0.96,0.75,0.89,0.86,0.89)
whrfemale<-c(0.59,0.75,0.76,0.83,0.86,0.86,0.89,0.89,0.96,0.98)
data <- data.frame(whrmale, whrfemale)
shapiro.test(data$whrmale)
shapiro.test(data$whrfemale)
wilcox.test(data$whrmale,data$whrfemale)

#Paired t-test
before<-c(30,28,33,28,30,29,30,31,32,29)
after<-c(24,23,24,25,24,23,24,23,23,25)
data <- data.frame(before, after)
shapiro.test(data$before-data$after)
t.test(data$before,data$after,paired = TRUE)

#Wilcoxon signed rank test 
before<-c(241,243,248,248,253,257,259,260,272,263)
after<-c(215,201,220,200,226,213,221,225,282,283)
data <- data.frame(before, after)
shapiro.test(data$before-data$after)
wilcox.test(data$before, data$after, paired=TRUE)

#One Way ANOVA
gr1<-c(146,132,186,168,129,143,112,124,150,124)
gr2<-c(124,126,118,127,111,127,114,86,93,109)
gr3<-c(119,131,122,122,126,108,104,124,125,111)
data <- data.frame(gr1, gr2,gr3)
#transform wide to long
anovadta<- reshape(data, 
                   varying = c("gr1", "gr2", "gr3"), 
                   v.names = "score",
                   timevar = "group", 
                   times = c("group I", "group II", "group III"), 
                   direction = "long")
#test normality
shapiro.test(anovadta$score[anovadta$group=="group I"])
shapiro.test(anovadta$score[anovadta$group=="group II"])
shapiro.test(anovadta$score[anovadta$group=="group III"])

#variance test
bartlett.test(score~group, data=anovadta)

#ANOVA for unequal variance
anvun<-oneway.test(anovadta$score~anovadta$group)
anvun

#for equal variance
library(userfriendlyscience)
posthocTGH(anovadta$score,anovadta$group)

#boxplot
boxplot(score ~ group,
        data = anovadta,
        ylab="waist circumference(cm)",
        xlab="groups")


#Kruskal–Wallis
high<-c(2,5,12,13,13,13,15,15,16,17)
medium<-c(16,14,16,14,12,13,0,13,13,12)
low<-c(18,13,9,13,13,17,12,9,11,18)
data <- data.frame(high, medium,low)
#transform wide to long
anovadta<- reshape(data, 
                   varying = c("high", "medium", "low"), 
                   v.names = "score",
                   timevar = "group", 
                   times = c("high", "medium", "low"), 
                   direction = "long")
#test normality
shapiro.test(anovadta$score[anovadta$group=="high"])
shapiro.test(anovadta$score[anovadta$group=="medium"])
shapiro.test(anovadta$score[anovadta$group=="low"])

kruskal.test(anovadta$score~anovadta$group)

#example 1
regufat <- c(50,40,51,53,54,42,43,59,53,57,59)
lowfat <- c(54,60,51,53,59,58,55,56,54,50,52)
data <- data.frame(regufat, lowfat)
shapiro.test(data$regufat)
shapiro.test(data$lowfat)

var.test(regufat,lowfat, data = data)
t.test(data$regufat,data$lowfat,var.equal = FALSE)

#example 2
gr1<-c(81,87,96,87,81,85,87,85,95,85)
gr2<-c(74,83,83,85,72,79,81,83,70,80)
gr3<-c(75,75,74,76,83,83,82,71,82,85)
data <- data.frame(gr1, gr2,gr3)
#transform wide to long
dtalong<- reshape(data, 
                  varying = c("gr1", "gr2", "gr3"), 
                  v.names = "score",
                  timevar = "group", 
                  times = c("normal", "underwt", "overwt"), 
                  direction = "long")
#test normality
shapiro.test(dtalong$score[dtalong$group=="normal"])
shapiro.test(dtalong$score[dtalong$group=="underwt"])
shapiro.test(dtalong$score[dtalong$group=="overwt"])

#variance test
bartlett.test(score~group, data=dtalong)

#ANOVA for equal variance
anvun<-aov(dtalong$score~dtalong$group)
summary(anvun)

#multiple comparison
TukeyHSD(anvun)

