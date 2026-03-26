### This code is for Chi Square of categorical variables and to generate tables with percentage

### to clear data
rm()

library(MASS)

### to open a file
datum=read.csv(file.choose())
### this uses the full data csv file

### view column titles
head (datum)

### turn numbers into factors
datum$Race_code=as.factor(datum$Age)
datum$Cohort=as.factor(datum$Cohort)

### Demographic Variables Analysis
### sex between cohorts
tbl2=table(datum$Cohort, datum$Sex) 
tbl2
chisq.test(tbl2)
round(100*prop.table(tbl2,1),digits=1)


### test age between cohorts (warning!!!)
tbl3=table(datum$Cohort, datum$Age) 
tbl3
chisq.test(tbl3)
### run a fisher exact test; good for small sample sizes
fisher.test(tbl3,simulate.p.value=TRUE,B=1e7)
### get percentages
round(100*prop.table(tbl3,1),digits=1)

### test Race between cohorts (warning!!!)
tbl4=table(datum$Cohort, datum$Race) 
tbl4
chisq.test(tbl4)
### run a fisher exact test; good for small sample sizes
fisher.test(tbl4,simulate.p.value=TRUE,B=1e7)
### get percentages
round(100*prop.table(tbl4,1),digits=1)

### test Education between cohorts (warning!!!)
tbl5=table(datum$Cohort, datum$Edu) 
tbl5
chisq.test(tbl5)
### run a fisher exact test; good for small sample sizes
fisher.test(tbl5,simulate.p.value=TRUE,B=1e7)
### get percentages
round(100*prop.table(tbl5,1),digits=1)

### test Qualifications between cohorts
tbl6=table(datum$Cohort, datum$Qual) 
tbl6
chisq.test(tbl6)
### get percentages
round(100*prop.table(tbl6,1),digits=1)

### test BMI between cohorts
tbl7=table(datum$Cohort, datum$BMI_Cat)
tbl7
chisq.test(tbl7)
### get percentages
round(100*prop.table(tbl7,1),digits=1)

### Analysis of Med diet tertiles
tbl8=table(datum$Cohort, datum$MDTertile)
tbl8
chisq.test(tbl8)
### get percentages
round(100*prop.table(tbl8,1),digits=1)

### Analysis of Stages of Change
tbl9=table(datum$Cohort, datum$Stage_Change)
tbl9
chisq.test(tbl9)
### get percentages
round(100*prop.table(tbl9,1),digits=1)

### Analysis of Action Maintenance, Deciding, Deciding_No, Deciding_Yes, UnawareUnengaged
tbl9=table(datum$Cohort, datum$Stage_Change)
tbl9
chisq.test(tbl9)
### get percentages
round(100*prop.table(tbl9,1),digits=1)

### Analysis of MD Awareness
tb20=table(datum$Cohort, datum$Know_MD)
tb20
chisq.test(tb20)
### get percentages
round(100*prop.table(tb20,1),digits=1)


### Analysis of Class across Cohorts
tbl10=table(datum$Cohort, datum$Class)
tbl10
chisq.test(tbl10)
### get percentages
round(100*prop.table(tbl10,1),digits=1)



