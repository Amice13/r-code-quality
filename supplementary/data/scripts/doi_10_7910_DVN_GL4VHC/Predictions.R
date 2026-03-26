### to clear data
rm()

### to open a file
datum=read.csv(file.choose())

### view column titles and first six rows of data
head(datum)

### turn numbers into factors
datum$Age=as.factor(datum$Age)
datum$Cohort=as.factor(datum$Cohort)


### to get the odds ratio
exp()

### Med_Low (MedDiet Score)
### run logistic: with p vlues less than 0.25, keep if less than 0.1
results10=glm(MD_Low~b2020+c2022+Male+b25_34+c35_44+d45_54+e55_64+Asian_other+Black+Black_other+Chinese+Indian+White+High_blw+GED+Tech+Bach_H+Under+Over+Obese,data=datum,family=binomial)
summary(results10)


results11=glm(MD_Low~c2022+Male+b25_34+Asian_other+Black_other+Chinese+White+High_blw+Bach_H,data=datum,family=binomial)
summary(results11)

results12=glm(MD_Low~c2022+Male+b25_34+Black_other+High_blw,data=datum,family=binomial)
summary(results12)

exp()
exp(confint(results12))


### Med_High (MedDiet Score)
### run logistic: with p vlues less than 0.25, keep if less than 0.1
results13=glm(MD_High~b2020+c2022+Male+b25_34+c35_44+d45_54+e55_64+Asian_other+Black+Black_other+Chinese+Indian+White+High_blw+GED+Tech+Bach_H+Under+Over+Obese,data=datum,family=binomial)
summary(results13)

results14=glm(MD_High~c2022+Male+b25_34+Asian_other+Black_other+Chinese+White+High_blw,data=datum,family=binomial)
summary(results14)

results15=glm(MD_High~c2022+Male+b25_34+Black_other+High_blw,data=datum,family=binomial)
summary(results15)

exp()
exp(confint(results15))