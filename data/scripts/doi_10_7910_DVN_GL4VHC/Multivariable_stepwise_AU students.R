### to clear data
rm()

### to open a file
### use Turlock_Livermore_data for stepwise analysis
datum=read.csv(file.choose())

### view column titles
head (datum)

### need to number results to keep them seperate

install.packages("car")
library(car)

### How to change the reference category

help(relevel) ###use the relevel command to change reference categories

### turn numbers into factors
##datum$Race_code=as.factor(datum$Race_code)
##datum$Qual_code=as.factor(datum$Qual_code)
datum$Age=as.factor(datum$Age)
datum$Cohort=as.factor(datum$Cohort)
##datum$BMI_Cat=as.factor(datum$BMI_Cat)
##datum$DVEducation=as.factor(datum$DVEducation)
##datum$Sample=as.factor(datum$Sample)
##str(datum)
##str(datum, list.len = 999)

---------------------------------------------------------
### unadjusted model
### run an regression with MD Score and cohort
results1=lm(Med_Diet_Total~Cohort,data=datum)
summary(results1)
AIC(results1)
Anova(results1)
Anova(results1, type="3")

--------------------------------------------------------
### model with all variables
results10=lm(Med_Diet_Total~.,data=datum)
summary(results10)
AIC(results10)

### stepwise regression 
step(results10, direction = "backward")

### model with adjusted stepwise model variables (use the ones that are statistically significant in stepwise regression!!!)
results101=lm(Med_Diet_Total ~ Cohort + Sex + Age + Qual + BMI_Cat, data = datum)
summary(results101)
AIC(results101)
anova(results101) # use type I test for when order matters
Anova(results101) # use type II test when variables not significant
Anova(results101, type="III") # use type III when variables are significant




-----------------------------------------------------------------------
## do not need to the below ones. They were for old analysis!!!!rm
  
  
### mutivariable regression model with demographics 
### run an regression with MD Score and cohort and race
results2=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1"),data=datum)
summary(results2)

### mutivariable regression model with demographics
### run an regression with MD Score and cohort and race and sex
results3=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Sex,data=datum)
summary(results3)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education
### male, 18-24, white, below high school, and healthy BMI were the reference groups
results4=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Sex+High+GED+Tech+Assoc+Bach+Master,data=datum)
summary(results4)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI
### male, 18-24, white, high school, and healthy BMI were the reference groups
results5=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Sex+High+GED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Healthy"),data=datum)
summary(results5)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI
### male, 18-24, white, high school, and healthy BMI were the reference groups
results5b=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Sex+High+GED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Healthy")+Qual_code,data=datum)
summary(results5b)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI and Stages of Change
### male, 18-24, white, high school, and healthy BMI were the reference groups
results7=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Sex+High+GED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Healthy")+relevel(Stage,ref="UnawareUnengaged"),data=datum)
summary(results7)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI and Stages of Change and Barriers
### male, 18-24, white, high school, and healthy BMI were the reference groups
results8=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Female++HighGED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Normal")+relevel(Stage,ref="Unaware/Unengaged")+BarKQ_T+BarCQ_T+BarSAQ_T+BarHQ_T,data=datum)
summary(results8)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI and Stages of Change and Barriers and age
### male, 18-24, white, high school, and healthy BMI were the reference groups
results9=lm(Med_Diet_Total~Cohort+Age_New+relevel(Race_code,ref="1")+Female+High+GED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Healthy")+Qual_code+relevel(Stage,ref="UnawareUnengaged")+BarKQ_T+BarCQ_T+BarSAQ_T+BarHQ_T,data=datum)
summary(results9)
----------------------
### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI and Stages of Change and Barriers and age and Benefits
### male, 18-24, white, high school, and healthy BMI were the reference groups
results10=lm(Med_Diet_Total~Cohort+Age_New+relevel(Race_code,ref="1")+Female+High+GED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Healthy")+Qual_code+relevel(Stage,ref="UnawareUnengaged")+BarKQ_T+BarCQ_T+BarSAQ_T+BarHQ_T+BenHQ_T+BenWLQ_T+BenECQ_T+BenNCQ_T+BenFQ_T+BenPQ_T+BenSAQ_T+BenSAQ_T+BenMoodQ_T,data=datum)
summary(results10)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI
### male, 18-24, white, below high school, and healthy BMI were the reference groups
results21=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Age_New+Female+High+GED+Tech+Assoc+Bach+Master+relevel(BMI_Cat,ref="Healthy")+Qual_code,data=datum)
summary(results21)

### mutivariable regression model with demographics
### run regression with MD Score and cohort and race and sex and education and BMI
### male, 18-24, white, below high school, and healthy BMI were the reference groups
results22=lm(Med_Diet_Total~Cohort+relevel(Race_code,ref="1")+Age_New+Female+relevel(DVEducation,ref="1")+relevel(BMI_Cat,ref="Healthy")+Qual_code,data=datum)
summary(results22)

--------------------------------------------------------------------------
# Installation
install.packages('ggplot2')
# Loading
library(ggplot2)

### to visualize
require(ggplot2)
ggplot(datum,aes(y=Med_Diet_Total,x=Sample,color=Cohort))+geom_point()+geom_smooth(method="lm")

### to make regression equations for plotting
equation1=function(x){coef(results20)[2]*x+coef(results20)[1]}
equation2=function(x){coef(results20)[2]*x+coef(results20)[1]+coef(results20)[3]}

ggplot(datum, aes(x=Cohort, y=Med_Diet_Total)) +
  geom_boxplot()
