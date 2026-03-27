setwd("....")
library(epiDisplay)
dt<-read.csv("data_linear.csv")
#exploratory
summ(dt$uric)
shapiro.test(dt$uric)
summ(dt$fpg)
shapiro.test(dt$fpg)
summ(dt$bmi)
shapiro.test(dt$bmi)
summ(dt$whr)
shapiro.test(dt$whr)
tab1(dt$gender)
dt$gender<-factor(dt$gender,levels = c("0","1"),labels = c("female","male"))
tab1(dt$gender)
shapiro.test(dt$uric[dt$gender=="female"])
shapiro.test(dt$uric[dt$gender=="male"])
tab1(dt$smk)
dt$smk<-factor(dt$smk,levels = c("0","1"),labels = c("no","yes"))
tab1(dt$smk)
shapiro.test(dt$uric[dt$smk=="no"])
shapiro.test(dt$uric[dt$smk=="yes"])

summ(dt$age)
dt$age_gr<-cut(dt$age, breaks=c(min(dt$age,na.rm = TRUE),45,55,max(dt$age,na.rm = TRUE)),
               labels = c("<46","46-55",">55yrs"),
               include.lowest = TRUE)
tab1(dt$age_gr)
shapiro.test(dt$uric[dt$age_gr=="<46"])
shapiro.test(dt$uric[dt$age_gr=="46-55"])
shapiro.test(dt$uric[dt$age_gr==">55yrs"])


#univariate
cor.test(dt$uric,dt$bmi)
cor.test(dt$uric,dt$fpg,method = c("spearman"))
cor.test(dt$uric,dt$whr,method = c("spearman"))
kruskal.test(uric ~ age_gr, data = dt)
t.test(dt$uric~dt$gender)
t.test(dt$uric~dt$smk)


#creating dummy varible
#http://slideplayer.com/slide/6421180/22/images/68/Regression+with+Dummy+Variables.jpg

#multivariate analysis
m1<-glm(uric~fpg+age_gr+bmi+whr+smk,data=dt)
summary(m1)
#del bmi
m2<-glm(uric~fpg+age_gr+    whr+smk,data=dt)
anova(m2,m1,test = "F")
summary(m2)
#del smk
m3<-glm(uric~fpg+age_gr+    whr     ,data=dt)
anova(m3,m2,test = "F")
summary(m3)
#del age gr
m4<-glm(uric~fpg+           whr     ,data=dt)
anova(m4,m3,test = "F")
summary(m4)
regress.display(m4)

#assumption
par(mfrow = c(2, 2))
plot(m4)

#tabl1 1
statStack(uric,by=c(age_gr,gender,smk),dataFrame  =dt)

#tabl2 regress
regress.display(m3)
