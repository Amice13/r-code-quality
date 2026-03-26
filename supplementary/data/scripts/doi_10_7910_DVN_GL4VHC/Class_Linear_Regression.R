### to clear data
rm()

### to open a file
datum=read.csv(file.choose())

### view column titles
head (datum)

### need to number results to keep them seperate

install.packages("car")
library(car)

### Factor
datum$Age=as.factor(datum$Age)
datum$Cohort=as.factor(datum$Cohort)
datum$Race=as.factor(datum$Race)
datum$Education=as.factor(datum$Education)


---------------------------------------------------------
### unadjusted model
### run an regression with MD Score and cohort
results1=lm(Med_Diet_Total~Class,data=datum)
summary(results1)

--------------------------------------------------------
### model with demographic variables
results101=lm(Med_Diet_Total ~ Class + Cohort+ Sex+ Age + Race + Education + Qual+ BMI_Cat, data = datum)
summary(results101)

results101=lm(Med_Diet_Total ~ Class + Cohort+ Sex+ Age + relevel (Race, ref="White") + relevel(Education, ref ="High school diploma or lower") + Qual + BMI_Cat, data = datum)
summary(results101)
                                                                                                   
anova(results1,results101)

Anova(results1,type="3")
Anova(results101, type="3")
