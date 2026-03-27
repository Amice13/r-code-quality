setwd("...")
library(epiDisplay)
library(ggfortify)

#import data
dtasurv<-read.csv("survival.csv")
summ(dtasurv)
dta <-na.omit(dtasurv)
summ(dta)

#univariate
summ(dta$rage)
dta$rage65 <-ifelse(dta$rage >65,1,0)
tab1(dta$rage65)
survdiff(Surv(fumont, outcome) ~ rage65, data = dta)

summ(dta$d_age)
dta$d_age60 <-ifelse(dta$d_age >50,1,0)
tab1(dta$d_age60)
survdiff(Surv(fumont, outcome) ~ d_age60, data = dta)

survdiff(Surv(fumont, outcome) ~ rsex, data = dta)
survdiff(Surv(fumont, outcome) ~ aboi_no, data = dta)
survdiff(Surv(fumont, outcome) ~ cmv_yn, data = dta)
survdiff(Surv(fumont, outcome) ~ bkvcmv_yn, data = dta)
survdiff(Surv(fumont, outcome) ~ hcv_r, data = dta)

#continuous data
hla_miscox <- coxph(Surv(fumont,outcome) ~hla_mis,data = dta)
summary(hla_miscox)
tdialymntcox <- coxph(Surv(fumont,outcome) ~tdialymnt,data = dta)
summary(tdialymntcox)

# graph
library(ggfortify)
km_AG_fit <- survfit(Surv(fumont, outcome) ~ aboi_no, data=dta)
autoplot(km_AG_fit)

#multivariate: cox proportional hazard model
mcox1 <- coxph(Surv(fumont, outcome) ~aboi_no+cmv_yn+hla_mis+tdialymnt, data = dta)
summary(mcox1)

#delete tdialymnt
mcox2 <- coxph(Surv(fumont, outcome) ~aboi_no+cmv_yn+hla_mis, data = dta)
lrtest(mcox2,mcox1)
summary(mcox2)

cox.display(mcox2)
