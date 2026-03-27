### to clear data
rm()

### to open a file
datum=read.csv(file.choose())

### view column titles
head (datum)

### need to number results to keep them seperate

### How to change the reference category

help(relevel) ###use the relevel command to change reference categories

### turn numbers into factors
datum$Cohort=as.factor(datum$Cohort)


### basic Anova model
### run by cohort 
###results2=lm(BenHQ2~Cohort,data=datum)
###results3=lm(BenHQ4~Cohort,data=datum)
###results4=lm(BenHQ12~Cohort,data=datum)
###results5=lm(BenHQ5~Cohort,data=datum)
###results6=lm(BenHQ7~Cohort,data=datum)
###results7=lm(BenHQ8~Cohort,data=datum)
###results8=lm(BenHQ9~Cohort,data=datum)
###results9=lm(BenHQ23~Cohort,data=datum)
###results10=lm(BenHQ10~Cohort,data=datum)

results11=lm(BenHQ_T~Cohort,data=datum)

###results12=lm(BenWLQ3~Cohort,data=datum)
###results13=lm(BenWLQ11~Cohort,data=datum)

results14=lm(BenWLQ_T~Cohort,data=datum)

###results15=lm(BenECQ13~Cohort,data=datum)
###results16=lm(BenECQ14~Cohort,data=datum)

results17=lm(BenECQ_T~Cohort,data=datum)

###results18=lm(BenNCQ6~Cohort,data=datum)
###results19=lm(BenNCQ15~Cohort,data=datum)

results20=lm(BenNCQ_T~Cohort,data=datum)

###results21=lm(BenCQ16~Cohort,data=datum)
###results22=lm(BenFQ17~Cohort,data=datum)
###results23=lm(BenFQ18~Cohort,data=datum)

results24=lm(BenFQ_T~Cohort,data=datum)

###results25=lm(BenPQ19~Cohort,data=datum)
###results26=lm(BenPQ20~Cohort,data=datum)

results27=lm(BenPQ_T~Cohort,data=datum)

###results28=lm(BenSAQ21~Cohort,data=datum)
###results29=lm(BenSAQ22~Cohort,data=datum)

results30=lm(BenSAQ_T~Cohort,data=datum)

###results31=lm(BenMoodQ24~Cohort,data=datum)
###results32=lm(BenMoodQ25~Cohort,data=datum)
###results33=lm(BenMoodQ26~Cohort,data=datum)

results34=lm(BenMoodQ_T~Cohort,data=datum)

sink(Ben.model1_results)
###summary(results1)
###summary(results2)
###summary(results3)
###summary(results4)
###summary(results5)
###summary(results6)
###summary(results7)
###summary(results8)
###summary(results9)
###summary(results10)
summary(results11)
###summary(results12)
###summary(results13)
summary(results14)
###summary(results15)
###summary(results16)
summary(results17)
###summary(results18)
###summary(results19)
summary(results20)
###summary(results21)
###summary(results22)
###summary(results23)
summary(results24)
###summary(results25)
###summary(results26)
summary(results27)
###summary(results28)
###summary(results29)
summary(results30)
###summary(results31)
###summary(results32)
###summary(results33)
summary(results34)

sink()


