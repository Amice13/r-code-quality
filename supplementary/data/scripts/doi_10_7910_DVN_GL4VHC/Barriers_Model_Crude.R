### to clear data
rm()

### to open a file
datum=read.csv(file.choose())

### view column titles
head (datum)

### turn numbers into factors
datum$Cohort=as.factor(datum$Cohort)



### Anova model 1
### run an ANOVA with cohort 
###results1=lm(BarKQ1~relevel(Cohort_SB,ref="Yes"),data=datum)
###results2=lm(BarKQ2~relevel(Cohort_SB,ref="Yes"),data=datum)
###results3=lm(BarKQ11~relevel(Cohort_SB,ref="Yes"),data=datum)
###results4=lm(BarKQ3~relevel(Cohort_SB,ref="Yes"),data=datum)
results1=lm(BarKFQ_T~Cohort,data=datum)
summary(results1)

results10=lm(BarKFQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat,data=datum)
###results6=lm(BarPQ4~relevel(Cohort_SB,ref="Yes"),data=datum)
###results7=lm(BarCQ5~relevel(Cohort_SB,ref="Yes"),data=datum)
###results8=lm(BarCQ6~relevel(Cohort_SB,ref="Yes"),data=datum)
###results9=lm(BarCQ7~relevel(Cohort_SB,ref="Yes"),data=datum)
###results10=lm(BarCQ8~relevel(Cohort_SB,ref="Yes"),data=datum)
results2=lm(BarCQ_T~Cohort,data=datum)
summary(results2)

results20=lm(BarCQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat,data=datum)
###results12=lm(BarFQ9~relevel(Cohort_SB,ref="Yes"),data=datum)
###results13=lm(BarMQ10~relevel(Cohort_SB,ref="Yes"),data=datum)
###results14=lm(BarSAQ12~relevel(Cohort_SB,ref="Yes"),data=datum)
###results15=lm(BarSAQ14~relevel(Cohort_SB,ref="Yes"),data=datum)
###results16=lm(BarSAQ15~relevel(Cohort_SB,ref="Yes"),data=datum)
results3=lm(BarSAQ_T~Cohort,data=datum)
summary(results3)

results30=lm(BarSAQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat,data=datum)
###results18=lm(BarHQ13~relevel(Cohort_SB,ref="Yes"),data=datum)
###results19=lm(BarHQ16~relevel(Cohort_SB,ref="Yes"),data=datum)
###results20=lm(BarHQ17~relevel(Cohort_SB,ref="Yes"),data=datum)
###results21=lm(BarHQ18~relevel(Cohort_SB,ref="Yes"),data=datum)
results4=lm(BarHQ_T~Cohort,data=datum)
summary(results4)

results40=lm(BarHQ_T~Cohort+Sex+Age+Race+Education+BMI_Cat,data=datum)

summary(results1)
summary(results10)
summary(results2)
summary(results20)
summary(results3)
summary(results30)
summary(results4)
summary(results40)
summary(results5)
summary(results50)
###summary(results6)
###summary(results7)
###summary(results8)
###summary(results9)
###summary(results10)
###summary(results11)
###summary(results12)
###summary(results13)
###summary(results14)
###summary(results15)
###summary(results16)
###summary(results17)
###summary(results18)
###summary(results19)
###summary(results20)
###summary(results21)
###summary(results22)


