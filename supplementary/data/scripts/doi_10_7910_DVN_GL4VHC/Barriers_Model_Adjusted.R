### to clear data
rm()

### to open a file
datum=read.csv(file.choose())

### view column titles
head (datum)

### turn numbers into factors
datum$Cohort=as.factor(datum$Cohort)
datum$Age_code=as.factor(datum$Age)


### Barriers
### Anova model 2
### run an ANOVA with cohort include sex and age 
### male and 18-24 were the reference groups
###results1=lm(BarKQ1~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results2=lm(BarKQ2~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results3=lm(BarKQ11~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results4=lm(BarKQ3~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)

results5=lm(BarKFQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat+Qual,data=datum)
summary(results5)

###results6=lm(BarPQ4~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results7=lm(BarCQ5~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results8=lm(BarCQ6~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results9=lm(BarCQ7~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results10=lm(BarCQ8~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)

results11=lm(BarCQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat+Qual,data=datum)
summary(results11)

###results12=lm(BarFQ9~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results13=lm(BarMQ10~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results14=lm(BarSAQ12~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results15=lm(BarSAQ14~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results16=lm(BarSAQ15~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)

results17=lm(BarSAQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat+Qual,data=datum)
summary(results17)

###results18=lm(BarHQ13~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results19=lm(BarHQ16~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results20=lm(BarHQ17~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)
###results21=lm(BarHQ18~reformulate(Cohort_SB,ref="Yes")+Female+Age_New,data=datum)

results22=lm(BarHQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat+Qual,data=datum)
summary(results22)


###summary(results1)
###summary(results2)
###summary(results3)
summary(results5)
###summary(results6)
###summary(results7)
###summary(results8)
###summary(results9)
###summary(results10)
summary(results11)
###summary(results12)
###summary(results13)
###summary(results14)
###summary(results15)
###summary(results16)
summary(results17)
###summary(results18)
###summary(results19)
###summary(results20)
###summary(results21)
summary(results22)


# Installation
install.packages('ggplot2')
# Loading
library(ggplot2)

### to visualize
require(ggplot2)
ggplot(datum,aes(y=MDTotal,x=Sample,color=Cohort_SB))+geom_point()+geom_smooth(method="lm")

equation1=function(x){coef(results20)[2]*x+coef(results20)[1]}
equation2=function(x){coef(results20)[2]*x+coef(results20)[1]+coef(results20)[3]}

ggplot(datum, aes(x=Cohort_SB, y=MDTotal)) +
  geom_boxplot()

