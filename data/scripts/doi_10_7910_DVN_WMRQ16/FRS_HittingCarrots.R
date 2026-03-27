### Replication code for Hitting Them With Carrots: Voter Intimidation and Vote Buying in Russia
### Authors: Timothy Frye, Ora John Reuter, and David Szakonyi
### Date of Upload: 2/6/2017

# install libraries
library(data.table)
library(ggplot2)
library(wesanderson)
library(texreg)
library(nlstools)
library(list)
library(stargazer)
library(Matching)
library(ebal)

#Data
rm(list=ls())

### Load Data
load(file="/Users/davidszakonyi/Dropbox/WorkPlaceMobilization/APSA 2014_PositivevsNegative/ReDo/Data//Dataverse/levadadata.Rda")


#########################################################
#############     SIMPLE CLEANING           ###################
#########################################################


### Employment Status
lev$employed3<-0
lev$employed3[lev$D4A=="Specialist" | lev$D4A=="Military man, security" | lev$D4A=="Clerical / Non-manual worker" | lev$D4A=="Manual worker"]<-1

### Male
lev$male<-0
lev$male[lev$D1=="Male"]<-1

### Age
lev$age<-lev$D2
lev$log_age<-log(lev$D2)

### Education
lev$ordeduc[lev$D3=="Primary or less"]<-1
lev$ordeduc[lev$D3=="Secondary school"]<-2
lev$ordeduc[lev$D3=="Vocational school on a basis of secondary school"]<-3
lev$ordeduc[lev$D3=="High school"]<-4
lev$ordeduc[lev$D3=="Vocational school on a basis of high school"]<-5
lev$ordeduc[lev$D3=="College"]<-6
lev$ordeduc[lev$D3=="Incomplete university / Bachelor"]<-7
lev$ordeduc[lev$D3=="University"]<-8

### Town Size
lev$townsize[lev$D14=="less than 20 ths"]<-1
lev$townsize[lev$D14=="20-99 ths"]<-2
lev$townsize[lev$D14=="100-249 ths."]<-3
lev$townsize[lev$D14=="250-499 ths"]<-4
lev$townsize[lev$D14=="500-999 ths."]<-5
lev$townsize[lev$D14=="More than 1 mln"]<-6
lev$townsize[lev$D14=="Moscow (more than 10 mln)"]<-7

### Income Measure
lev$income<-lev$D9
lev$income[lev$income==9 | lev$income==8]<-NA
### Income (log)
lev$logincome<-log(lev$income)

### Consumer Status
lev$consumer_status[lev$D8=='We haven"t enough money even for food']=1
lev$consumer_status[lev$D8=="We have enough money only for food, but not for clothes"]=2
lev$consumer_status[lev$D8=='We have enough money for food and clothes, but it"s a problem for us purchase TV']=3
lev$consumer_status[lev$D8=="We can purchase some expensive things, such as TV or fridge, but cannot purchase"]=4
lev$consumer_status[lev$D8=="We can purchase a car, but we cannot say we are in need of money"]=5
lev$consumer_status[lev$D8=="We can deny ourselves nothing"]=6

### Town Size
lev$socialstatus[lev$D10=="low"]<-1
lev$socialstatus[lev$D10=="low middle"]<-2
lev$socialstatus[lev$D10=="medium middle"]<-3
lev$socialstatus[lev$D10=="upper middle"]<-4
lev$socialstatus[lev$D10=="upper"]<-5

### Binary Social Status
lev$socialstatus_bin[lev$socialstatus<3]<-0
lev$socialstatus_bin[lev$socialstatus>2]<-1

### Unemployment Rate
lev$unemployedrate<-as.numeric(as.character(lev$unemployed))/as.numeric(as.character(lev$totalemployablepopulation))
lev$percemployed<-as.numeric(as.character(lev$totalemployedpopulation))/as.numeric(as.character(lev$totalemployablepopulation))

### Median Unemployment Rate
mediancityunemployment<-summary(lev$unemployedrate)[3]
mediancityemployed<-summary(lev$percemployed)[3]
lev$unempshare<-1-(lev$totalemployedpopulation/lev$totalemployablepopulation)

#########################################################
#############      MAIN PAPER TABLE 1             ###################
#########################################################

### DELETE MISSING
lev$q2_1[lev$q2_1==9]<-NA
lev$q2_2[lev$q2_2==9]<-NA
lev$q2_3[lev$q2_3==9]<-NA

### Consolidated Score
lev$fullnp<-lev$q2_1
lev$fullnp[is.na(lev$fullnp)==TRUE]<-lev$q2_2[is.na(lev$fullnp)==TRUE]
lev$fullnp[is.na(lev$fullnp)==TRUE]<-lev$q2_3[is.na(lev$fullnp)==TRUE]

### Overall Treatment Status
lev$treatstatnp[is.na(lev$q2_1)==FALSE]<-1
lev$treatstatnp[is.na(lev$q2_2)==FALSE]<-2
lev$treatstatnp[is.na(lev$q2_3)==FALSE]<-3

### Pair Treatment Status
lev$treatstat12np[is.na(lev$q2_1)==FALSE]<-0
lev$treatstat12np[is.na(lev$q2_2)==FALSE]<-1

lev$treatstat23np[is.na(lev$q2_2)==FALSE]<-0
lev$treatstat23np[is.na(lev$q2_3)==FALSE]<-1

lev$treatstat13np[is.na(lev$q2_1)==FALSE]<-0
lev$treatstat13np[is.na(lev$q2_3)==FALSE]<-1

### New Treatment Status with 0
lev$treatmentstatus<-lev$treatstatnp
lev$treatmentstatus[lev$treatstatnp==1]<-0
lev$treatmentstatus[lev$treatstatnp==2]<-1
lev$treatmentstatus[lev$treatstatnp==3]<-2


### Simple T-Tests for Table 1

### Control Vs. Negative
t.test(lev$fullnp~lev$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev))

### Negative Vs. Positive
t.test(lev$fullnp~lev$treatstat23np)
summary(lm(fullnp~factor(treatstat23np), data=lev))

### Control Vs. Positive
t.test(lev$fullnp~lev$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev))


#########################################################
#############      MAIN PAPER TABLE 2             ###################
#########################################################

### Subset to those with High and Low Income Data
lev_income_high<-subset(lev, is.na(income)==FALSE & income>20000)
lev_income_low<-subset(lev, is.na(income)==FALSE & income<20001)

### High Income
### Control Vs. Negative
t.test(lev_income_high$fullnp~lev_income_high$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_income_high))

### Control Vs. Positive
t.test(lev_income_high$fullnp~lev_income_high$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_income_high))

length(lev_income_high$fullnp[lev_income_high$treatstat12np==0 & is.na(lev_income_high$treatstat12np)==FALSE])
length(lev_income_high$fullnp[lev_income_high$treatstat12np==1 & is.na(lev_income_high$treatstat12np)==FALSE])
length(lev_income_high$fullnp[lev_income_high$treatstat23np==1 & is.na(lev_income_high$treatstat23np)==FALSE])


### Low Income
### Control Vs. Negative
t.test(lev_income_low$fullnp~lev_income_low$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_income_low))

### Control Vs. Positive
t.test(lev_income_low$fullnp~lev_income_low$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_income_low))

length(lev_income_low$fullnp[lev_income_low$treatstat12np==0 & is.na(lev_income_low$treatstat12np)==FALSE])
length(lev_income_low$fullnp[lev_income_low$treatstat12np==1 & is.na(lev_income_low$treatstat12np)==FALSE])
length(lev_income_low$fullnp[lev_income_low$treatstat23np==1 & is.na(lev_income_low$treatstat23np)==FALSE])




#########################################################
#############    MAIN PAPER TABLE 3            ###################
#########################################################

lev_ict<-subset(lev, is.na(treatmentstatus)==FALSE)

resultsnp_income <- ictreg(fullnp ~ logincome+ employed3 + male+log_age+ordeduc+townsize+monogorod, data = as.data.frame(lev_ict), treat = "treatmentstatus", J=4, method = "lm")

resultsnp_consumerstatus <- ictreg(fullnp ~ consumer_status+employed3 + male+log_age+ordeduc+townsize+monogorod, data = as.data.frame(lev_ict), treat = "treatmentstatus", J=4, method = "lm")

resultsnp_socialstatus <- ictreg(fullnp ~ socialstatus+employed3 + male+log_age+ordeduc+townsize+monogorod, data = as.data.frame(lev_ict), treat = "treatmentstatus", J=4, method = "lm")

### Negative 

coefnames_inc = rev(resultsnp_income$coef.names)
coefs_neg_inc<-rev(resultsnp_income$par.treat[[1]])
ses_neg_inc<-rev(resultsnp_income$se.treat[[1]])
tr_neg_inc_obs<-length(resultsnp_income$treat[resultsnp_income$treat==1])
pvalues_neg_inc <-dt(c(rev(resultsnp_income$par.treat[[1]])/rev(resultsnp_income$se.treat[[1]])),as.numeric(summary(resultsnp_income)[11]))
tr_neg_inc <- createTexreg( coef.names= coefnames_inc, coef = coefs_neg_inc, se = ses_neg_inc, model.name="Negative",gof.names="Observations",gof=tr_neg_inc_obs,pvalues=pvalues_neg_inc)

coefnames_con = rev(resultsnp_consumerstatus$coef.names)
coefs_neg_con<-rev(resultsnp_consumerstatus$par.treat[[1]])
ses_neg_con<-rev(resultsnp_consumerstatus$se.treat[[1]])
tr_neg_con_obs<-length(resultsnp_consumerstatus$treat[resultsnp_consumerstatus$treat==1])
pvalues_neg_con <-dt(c(rev(resultsnp_consumerstatus$par.treat[[1]])/rev(resultsnp_consumerstatus$se.treat[[1]])),as.numeric(summary(resultsnp_consumerstatus)[11]))
tr_neg_con <- createTexreg( coef.names= coefnames_con, coef = coefs_neg_con, se = ses_neg_con, model.name="Negative",gof.names="Observations",gof=tr_neg_con_obs,pvalues=pvalues_neg_con)

coefnames_soc = rev(resultsnp_socialstatus$coef.names)
coefs_neg_soc<-rev(resultsnp_socialstatus$par.treat[[1]])
ses_neg_soc<-rev(resultsnp_socialstatus$se.treat[[1]])
tr_neg_soc_obs<-length(resultsnp_socialstatus$treat[resultsnp_socialstatus$treat==1])
pvalues_neg_soc <-dt(c(rev(resultsnp_socialstatus$par.treat[[1]])/rev(resultsnp_socialstatus$se.treat[[1]])),as.numeric(summary(resultsnp_socialstatus)[11]))
tr_neg_soc <- createTexreg( coef.names= coefnames_soc, coef = coefs_neg_soc, se = ses_neg_soc, model.name="Negative",gof.names="Observations",gof=tr_neg_soc_obs,pvalues=pvalues_neg_soc)

negative_table<-texreg(list(tr_neg_inc,tr_neg_con,tr_neg_soc),reorder.coef=c(1,2,3,4,5,6,7,9,10,8),stars = c(0.01, 0.05, 0.1),booktabs=TRUE, table = FALSE,custom.note="",digits=3,custom.coef.names=c("Monogorod","Town Size","Education","Age (log)","Male","Employed","Income (log)","Intercept","Consumer Status","Social Status"),use.packages=FALSE)
negative_table<-gsub(".000 ","",negative_table,fixed=TRUE)

# negative_table<-gsub("\\toprule","\\toprule\\multicolumn{4}{c}{Panel A: Negative Inducements}\\\\\\hline",negative_table,fixed=TRUE)
# sink(file=paste(TablesPath,"ols_negative.tex",sep=''))
# cat(negative_table)
# sink()


### Positive 

coefnames_inc = rev(resultsnp_income$coef.names)
coefs_pos_inc<-rev(resultsnp_income$par.treat[[2]])
ses_pos_inc<-rev(resultsnp_income$se.treat[[2]])
tr_pos_inc_obs<-length(resultsnp_income$treat[resultsnp_income$treat==2])
pvalues_pos_inc <-dt(c(rev(resultsnp_income$par.treat[[2]])/rev(resultsnp_income$se.treat[[2]])),as.numeric(summary(resultsnp_income)[11]))
tr_pos_inc <- createTexreg( coef.names= coefnames_inc, coef = coefs_pos_inc, se = ses_pos_inc, model.name="Positive",gof.names="Observations",gof=tr_pos_inc_obs,pvalues=pvalues_pos_inc)

coefnames_con = rev(resultsnp_consumerstatus$coef.names)
coefs_pos_con<-rev(resultsnp_consumerstatus$par.treat[[2]])
ses_pos_con<-rev(resultsnp_consumerstatus$se.treat[[2]])
tr_pos_con_obs<-length(resultsnp_consumerstatus$treat[resultsnp_consumerstatus$treat==2])
pvalues_pos_con <-dt(c(rev(resultsnp_consumerstatus$par.treat[[2]])/rev(resultsnp_consumerstatus$se.treat[[2]])),as.numeric(summary(resultsnp_consumerstatus)[11]))
tr_pos_con <- createTexreg( coef.names= coefnames_con, coef = coefs_pos_con, se = ses_pos_con, model.name="Positive",gof.names="Observations",gof=tr_pos_con_obs,pvalues=pvalues_pos_con)

coefnames_soc = rev(resultsnp_socialstatus$coef.names)
coefs_pos_soc<-rev(resultsnp_socialstatus$par.treat[[2]])
ses_pos_soc<-rev(resultsnp_socialstatus$se.treat[[2]])
tr_pos_soc_obs<-length(resultsnp_socialstatus$treat[resultsnp_socialstatus$treat==2])
pvalues_pos_soc <-dt(c(rev(resultsnp_socialstatus$par.treat[[2]])/rev(resultsnp_socialstatus$se.treat[[2]])),as.numeric(summary(resultsnp_socialstatus)[11]))
tr_pos_soc <- createTexreg( coef.names= coefnames_soc, coef = coefs_pos_soc, se = ses_pos_soc, model.name="Positive",gof.names="Observations",gof=tr_pos_soc_obs,pvalues=pvalues_pos_soc)

positive_table<-texreg(list(tr_pos_inc,tr_pos_con,tr_pos_soc),reorder.coef=c(1,2,3,4,5,6,7,9,10,8),stars = c(0.01, 0.05, 0.1),booktabs=TRUE, table = FALSE,digits=3,custom.coef.names=c("Monogorod","Town Size","Education","Age (log)","Male","Employed","Income (log)","Intercept","Consumer Status","Social Status"),use.packages=FALSE,custom.model.names=c("","",""),custom.note="")
positive_table<-gsub(".000 ","",positive_table,fixed=TRUE)
positive_table<-gsub("\\toprule","\\multicolumn{4}{c}{Panel B: Positive Inducements}\\\\",positive_table,fixed=TRUE)
positive_table<-gsub("&  &  &  \\\\","",positive_table,fixed=TRUE)

# sink(file=paste(TablesPath,"ols_positive.tex",sep=''))
# cat(positive_table)
# sink()

#########################################################
#############     MAIN PAPER TABLE 4             ###################
#########################################################

### Subset to Employment
lev_unemployed<-subset(lev, employed3==0 & is.na(employed3)==FALSE)
lev_employed<-subset(lev, employed3==1 & is.na(employed3)==FALSE)

### Employed
### Control Vs. Negative
t.test(lev_employed$fullnp~lev_employed$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_employed))

### Control Vs. Positive
t.test(lev_employed$fullnp~lev_employed$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_employed))

length(lev_employed$fullnp[lev_employed$treatstat12np==0 & is.na(lev_employed$treatstat12np)==FALSE])
length(lev_employed$fullnp[lev_employed$treatstat12np==1 & is.na(lev_employed$treatstat12np)==FALSE])
length(lev_employed$fullnp[lev_employed$treatstat23np==1 & is.na(lev_employed$treatstat23np)==FALSE])


### Unemployed
### Control Vs. Negative
t.test(lev_unemployed$fullnp~lev_unemployed$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_unemployed))

### Control Vs. Positive
t.test(lev_unemployed$fullnp~lev_unemployed$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_unemployed))

length(lev_unemployed$fullnp[lev_unemployed$treatstat12np==0 & is.na(lev_unemployed$treatstat12np)==FALSE])
length(lev_unemployed$fullnp[lev_unemployed$treatstat12np==1 & is.na(lev_unemployed$treatstat12np)==FALSE])
length(lev_unemployed$fullnp[lev_unemployed$treatstat23np==1 & is.na(lev_unemployed$treatstat23np)==FALSE])




#########################################################
#############    MAIN PAPER TABLE 6             ###################
#########################################################
### Subset to Monogorod
lev_nonmonogorod<-subset(lev, monogorod==0 & is.na(monogorod)==FALSE)
lev_monogorod<-subset(lev, monogorod==1 & is.na(monogorod)==FALSE)

### Monogorod
### Control Vs. Negative
t.test(lev_monogorod$fullnp~lev_monogorod$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_monogorod))

### Control Vs. Positive
t.test(lev_monogorod$fullnp~lev_monogorod$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_monogorod))

length(lev_monogorod$fullnp[lev_monogorod$treatstat12np==0 & is.na(lev_monogorod$treatstat12np)==FALSE])
length(lev_monogorod$fullnp[lev_monogorod$treatstat12np==1 & is.na(lev_monogorod$treatstat12np)==FALSE])
length(lev_monogorod$fullnp[lev_monogorod$treatstat23np==1 & is.na(lev_monogorod$treatstat23np)==FALSE])


### Non-Monogorod
### Control Vs. Negative
t.test(lev_nonmonogorod$fullnp~lev_nonmonogorod$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_nonmonogorod))

### Control Vs. Positive
t.test(lev_nonmonogorod$fullnp~lev_nonmonogorod$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_nonmonogorod))

length(lev_nonmonogorod$fullnp[lev_nonmonogorod$treatstat12np==0 & is.na(lev_nonmonogorod$treatstat12np)==FALSE])
length(lev_nonmonogorod$fullnp[lev_nonmonogorod$treatstat12np==1 & is.na(lev_nonmonogorod$treatstat12np)==FALSE])
length(lev_nonmonogorod$fullnp[lev_nonmonogorod$treatstat23np==1 & is.na(lev_nonmonogorod$treatstat23np)==FALSE])




#########################################################
#############    MAIN PAPER TABLE 7             ###################
#########################################################
lev_monogorod_notemp<-subset(lev, monogorod==1 & is.na(monogorod)==FALSE & employed3==0 & is.na(employed3)==FALSE)
lev_monogorod_emp<-subset(lev, monogorod==1 & is.na(monogorod)==FALSE & employed3==1 & is.na(employed3)==FALSE)

### Monogorod Employment
### Control Vs. Negative
t.test(lev_monogorod_emp$fullnp~lev_monogorod_emp$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_monogorod_emp))

### Control Vs. Positive
t.test(lev_monogorod_emp$fullnp~lev_monogorod_emp$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_monogorod_emp))

length(lev_monogorod_emp$fullnp[lev_monogorod_emp$treatstat12np==0 & is.na(lev_monogorod_emp$treatstat12np)==FALSE])
length(lev_monogorod_emp$fullnp[lev_monogorod_emp$treatstat12np==1 & is.na(lev_monogorod_emp$treatstat12np)==FALSE])
length(lev_monogorod_emp$fullnp[lev_monogorod_emp$treatstat23np==1 & is.na(lev_monogorod_emp$treatstat23np)==FALSE])


### Monogorod Unemployment
### Control Vs. Negative
t.test(lev_monogorod_notemp$fullnp~lev_monogorod_notemp$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_monogorod_notemp))

### Control Vs. Positive
t.test(lev_monogorod_notemp$fullnp~lev_monogorod_notemp$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_monogorod_notemp))

length(lev_monogorod_notemp$fullnp[lev_monogorod_notemp$treatstat12np==0 & is.na(lev_monogorod_notemp$treatstat12np)==FALSE])
length(lev_monogorod_notemp$fullnp[lev_monogorod_notemp$treatstat12np==1 & is.na(lev_monogorod_notemp$treatstat12np)==FALSE])
length(lev_monogorod_notemp$fullnp[lev_monogorod_notemp$treatstat23np==1 & is.na(lev_monogorod_notemp$treatstat23np)==FALSE])



#########################################################
#############    MAIN PAPER TABLE 8             ###################
#########################################################
### List Experiment on Reporting

lev$q3A[lev$q3A==9]<-NA
lev$q3B[lev$q3B==9]<-NA

lev$full_tell<-lev$q3A
lev$full_tell[is.na(lev$full_tell)==TRUE]<-lev$q3B[is.na(lev$full_tell)==TRUE]
lev$full_tell[lev$full_tell==9]<-NA

lev$treatstat_tell[is.na(lev$q3A)==FALSE]=0
lev$treatstat_tell[is.na(lev$q3B)==FALSE]=1

### Control Vs. Treatment
t.test(lev$full_tell~lev$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev))

length(lev$fullnp[lev$treatstat_tell==0 & is.na(lev$treatstat_tell)==FALSE])
length(lev$fullnp[lev$treatstat_tell==1 & is.na(lev$treatstat_tell)==FALSE])


#########################################################
#############    MAIN PAPER TABLE 9             ###################
#########################################################

### Subset to Employment - Monitoring
lev_unemployed<-subset(lev, employed3==0 & is.na(employed3)==FALSE)
lev_employed<-subset(lev, employed3==1 & is.na(employed3)==FALSE)

### Employed
### Control Vs. Negative
t.test(lev_employed$full_tell~lev_employed$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_employed))

length(lev_employed$full_tell[lev_employed$treatstat_tell==0 & is.na(lev_employed$treatstat_tell)==FALSE])
length(lev_employed$full_tell[lev_employed$treatstat_tell==1 & is.na(lev_employed$treatstat_tell)==FALSE])


### Unemployed
### Control Vs. Negative
t.test(lev_unemployed$full_tell~lev_unemployed$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_unemployed))

length(lev_unemployed$full_tell[lev_unemployed$treatstat_tell==0 & is.na(lev_unemployed$treatstat_tell)==FALSE])
length(lev_unemployed$full_tell[lev_unemployed$treatstat_tell==1 & is.na(lev_unemployed$treatstat_tell)==FALSE])





#########################################################
#############    MAIN PAPER TABLE 10             ###################
#########################################################

### Subset to Employment - Monitoring
lev_nonmonogorod<-subset(lev, monogorod==0 & is.na(monogorod)==FALSE)
lev_monogorod<-subset(lev, monogorod==1 & is.na(monogorod)==FALSE)

### Employed
### Control Vs. Negative
t.test(lev_monogorod$full_tell~lev_monogorod$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_monogorod))

length(lev_monogorod$full_tell[lev_monogorod$treatstat_tell==0 & is.na(lev_monogorod$treatstat_tell)==FALSE])
length(lev_monogorod$full_tell[lev_monogorod$treatstat_tell==1 & is.na(lev_monogorod$treatstat_tell)==FALSE])


### Unemployed
### Control Vs. Negative
t.test(lev_nonmonogorod$full_tell~lev_nonmonogorod$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_nonmonogorod))

length(lev_nonmonogorod$full_tell[lev_nonmonogorod$treatstat_tell==0 & is.na(lev_nonmonogorod$treatstat_tell)==FALSE])
length(lev_nonmonogorod$full_tell[lev_nonmonogorod$treatstat_tell==1 & is.na(lev_nonmonogorod$treatstat_tell)==FALSE])


#########################################################
#############    MAIN PAPER TABLE 11             ###################
#########################################################

lev_monitoring<-subset(lev, is.na(full_tell)==FALSE)

#### Table 2

lm.results.tell <- ictreg(full_tell ~ employed3 + male+log_age+ordeduc+townsize+logincome+monogorod,  data = lev_monitoring ,  treat = "treatstat_tell", J=4, method = "lm")
summary(lm.results.tell)

#### Run this for the table

coefnames = rev(lm.results.tell $coef.names)
coefs1<-rev(lm.results.tell $par.treat)
ses1<-rev(lm.results.tell $se.treat)
pvalues1 <-dt(c(rev(lm.results.tell$par.treat)/rev(lm.results.tell$se.treat)),as.numeric(summary(lm.results.tell)[9]))

coefs2<-rev(lm.results.tell$par.control)
ses2<-rev(lm.results.tell$se.control)
pvalues2 <-dt(c(rev(lm.results.tell$par.control)/rev(lm.results.tell$se.control)),as.numeric(summary(lm.results.tell)[9]))
tr1obs<-length(lm.results.tell$treat[lm.results.tell$treat==1])
tr2obs<-length(lm.results.tell$treat[lm.results.tell$treat==0])

tr1 <- createTexreg( coef.names= coefnames, coef = coefs1, se = ses1, model.name="Treatment",gof.names="Observations",gof=tr1obs,pvalues=pvalues1)
tr2 <- createTexreg( coef.names= coefnames, coef = coefs2, se = ses2, model.name="Control",gof.names="Observations",gof=tr2obs,pvalues=pvalues2)


monitoring_table<-texreg(list(tr1,tr2),stars = c(0.01, 0.05, 0.1),booktabs=TRUE, table = FALSE,digits=3,custom.coef.names=c("Monogorod","Income (log)","Town Size","Education","Age (log)","Male","Employed","Intercept"),use.packages=FALSE,custom.model.names=c("Treatment","Control"),custom.note="")
monitoring_table<-gsub(".000 ","",monitoring_table,fixed=TRUE)
monitoring_table<-gsub("&  &  &  \\\\","",monitoring_table,fixed=TRUE)

# sink(file=paste(TablesPath,"ols_monitoring.tex",sep=''))
# cat(monitoring_table)
# sink()





#########################################################
#############     APPENDIX             ###################
#########################################################

#########################################################
#############     APPENDIX TABLE 1  - See State Code        ###################
#########################################################


#########################################################
#############     APPENDIX TABLE 2          ###################
#########################################################


t_n <- subset(lev_ict, treatmentstatus<2)
t_n1 <-as.matrix(t_n[,c("male","ordeduc","log_age","townsize","logincome","monogorod","socialstatus","consumer_status")])
t_n2 <-t_n[,c("treatmentstatus")]

bout.eb <- MatchBalance(t_n2~t_n1,ks=FALSE)
bal.eb  <- as.data.frame(baltest.collect(matchbal.out=bout.eb,var.names=colnames(t_n1),after=F))
bal.eb <- round(bal.eb,2)
bal.eb$m.diff<-bal.eb$mean.Tr-bal.eb$mean.Co
bal.tab.n<-bal.eb[,c("mean.Co","mean.Tr","m.diff","T pval")]

t_n <- subset(lev_ict, treatmentstatus!=1)
t_n$treatstatnp[t_n$treatmentstatus==2]<-1
t_n1 <-as.matrix(t_n[,c("male","ordeduc","log_age","townsize","logincome","monogorod","socialstatus","consumer_status")])
t_n2 <-t_n[,c("treatstatnp")]

bout.eb <- MatchBalance(t_n2~t_n1,ks=FALSE)
bal.eb  <- as.data.frame(baltest.collect(matchbal.out=bout.eb,var.names=colnames(t_n1),after=F))
bal.eb <- round(bal.eb,2)
bal.eb$m.diff<-bal.eb$mean.Tr-bal.eb$mean.Co
bal.tab.p<-bal.eb[,c("mean.Tr","m.diff","T pval")]

bal.tab<-cbind(bal.tab.n,bal.tab.p)
row.names(bal.tab) <- c("Male","Education","Age (log)","Town Size","Income (log)","Monogorod","Social Status","Consumer Status")
t<-stargazer(bal.tab, digits=2, summary=FALSE, title="Appendix Table 5: Balance Statistics: Negative/Positive List Experiment",colnames=TRUE,covariate.labels=c("Variable","Control Mean", "Negative Mean", "Diff.","p", "Positive Mean", "Diff.","p"),column.labels="Male",model.numbers=TRUE, notes=c("\\scriptsize This table presents balance statistics for the negative/positive list experiment between the treatment and control groups. Column 1 shows the mean for the control group, while Columns 2 and 5 show the mean for the negative and positive treatment, respectively. Differences between the control and treatment means are shown in Columns 3 and 6, with the p-value from a two-sided t\\-test given in Columns 4 and 7."), header=FALSE)

t<-gsub("\\multicolumn{8}{l}{", "\\multicolumn{8}{p{1.1\\linewidth}}{", t, fixed =TRUE)
t<-gsub("cccccccc", "lc|c|cc|c|cc", t, fixed =TRUE)
t<-gsub("\\caption", "\\caption*", t, fixed =TRUE)
t<-gsub("Variable", "&(1) & (2)& (3) & (4)& (5)& (6)& (7)\\\\Variable", t, fixed =TRUE)
t<-gsub("\\begin{tabular}", "\\scalebox{.8}{\\begin{tabular}", t, fixed =TRUE)
t<-gsub("\\end{tabular}", "\\end{tabular}}", t, fixed =TRUE)
# sink(file=paste(TablesPath,"balancetable_np.tex",sep=''))
# cat(t)
# sink()


#########################################################
#############     APPENDIX TABLE 3          ###################
#########################################################

mon_np1 <-as.matrix(as.data.frame(lev)[,c("male","ordeduc","log_age","townsize","logincome","monogorod")])
mon_np2 <-as.data.frame(lev)[,c("treatstat_tell")]

bout.eb <- MatchBalance(mon_np2 ~ mon_np1,ks=FALSE)
bal.eb  <- as.data.frame(baltest.collect(matchbal.out=bout.eb,var.names=colnames(mon_np1),after=F))
bal.eb <- round(bal.eb,2)
bal.eb$m.diff<-bal.eb$mean.Tr-bal.eb$mean.Co
bal.tab.n<-bal.eb[,c("mean.Co","mean.Tr","m.diff","T pval")]

row.names(bal.tab.n) <- c("Male","Education","Age (log)","Town Size","Income (log)","Monogorod")
t<-stargazer(bal.tab.n, digits=2, summary=FALSE, title="Appendix Table 6: Balance Statistics: Monitoring List Experiment",colnames=TRUE,covariate.labels=c("Variable","Control Mean", "Treatment Mean", "Diff.","p"),model.numbers=TRUE, notes=c("\\scriptsize This table presents balance statistics for the monitoring list experiment between the treatment and control groups. Column 1 shows the mean for the control group, while Column 2 shows the mean for the monitoring treatment, respectively. The difference between the control and treatment means is shown in Column 3, with the p-value from a two-sided t\\-test given in Column 4."), header=FALSE)

t<-gsub("\\multicolumn{5}{l}{", "\\multicolumn{5}{p{.7\\linewidth}}{", t, fixed =TRUE)
t<-gsub("ccccc", "lc|c|cc", t, fixed =TRUE)
t<-gsub("\\caption", "\\caption*", t, fixed =TRUE)

t<-gsub("Variable", "&(1) & (2)& (3) & (4)\\\\Variable", t, fixed =TRUE)
t<-gsub("\\begin{tabular}", "\\scalebox{.8}{\\begin{tabular}", t, fixed =TRUE)
t<-gsub("\\end{tabular}", "\\end{tabular}}", t, fixed =TRUE)
# sink(file=paste(TablesPath,"balancetable_monitoring.tex",sep=''))
# cat(t)
# sink()



#########################################################
#############     APPENDIX FIGURE 1            ###################
#########################################################

### Subset to those with High and Low Income Data
lev$treatstatnp12_bin<-lev$treatstat12np
lev_income_bottomquartile<-subset(lev, is.na(income)==FALSE & income<15001)
lev_income_lowmiddlequartile<-subset(lev, is.na(income)==FALSE & income>15000 & income<20001)
lev_income_highmiddlequartile<-subset(lev, is.na(income)==FALSE & income<30001 & income>20000)
lev_income_highquartile<-subset(lev, is.na(income)==FALSE & income>30000)

coef_neg_l<-lm(fullnp~factor(treatstat12np), data=lev_income_bottomquartile)
coef_neg_lm<-lm(fullnp~factor(treatstat12np), data=lev_income_lowmiddlequartile)
coef_neg_hm<-lm(fullnp~factor(treatstat12np), data=lev_income_highmiddlequartile)
coef_neg_h<-lm(fullnp~factor(treatstat12np), data=lev_income_highquartile)

coef_pos_l<-lm(fullnp~factor(treatstat13np), data=lev_income_bottomquartile)
coef_pos_lm<-lm(fullnp~factor(treatstat13np), data=lev_income_lowmiddlequartile)
coef_pos_hm<-lm(fullnp~factor(treatstat13np), data=lev_income_highmiddlequartile)
coef_pos_h<-lm(fullnp~factor(treatstat13np), data=lev_income_highquartile)

### Build Coefficients

coefficients<-data.frame(var=as.character(),fe = as.numeric(),se= as.numeric(), subset=as.character())
coefficients<-rbind(coefficients,data.frame(var="negative",fe = summary(coef_neg_l)$coefficients[,1][2],se= summary(coef_neg_l)$coefficients[,2][2], subset="l"))
coefficients<-rbind(coefficients,data.frame(var="negative",fe = summary(coef_neg_lm)$coefficients[,1][2],se= summary(coef_neg_lm)$coefficients[,2][2], subset="lm"))
coefficients<-rbind(coefficients,data.frame(var="negative",fe = summary(coef_neg_hm)$coefficients[,1][2],se= summary(coef_neg_hm)$coefficients[,2][2], subset="hm"))
coefficients<-rbind(coefficients,data.frame(var="negative",fe = summary(coef_neg_h)$coefficients[,1][2],se= summary(coef_neg_h)$coefficients[,2][2], subset="h"))

coefficients<-rbind(coefficients,data.frame(var="positive",fe = summary(coef_pos_l)$coefficients[,1][2],se= summary(coef_pos_l)$coefficients[,2][2], subset="l"))
coefficients<-rbind(coefficients,data.frame(var="positive",fe = summary(coef_pos_lm)$coefficients[,1][2],se= summary(coef_pos_lm)$coefficients[,2][2], subset="lm"))
coefficients<-rbind(coefficients,data.frame(var="positive",fe = summary(coef_pos_hm)$coefficients[,1][2],se= summary(coef_pos_hm)$coefficients[,2][2], subset="hm"))
coefficients<-rbind(coefficients,data.frame(var="positive",fe = summary(coef_pos_h)$coefficients[,1][2],se= summary(coef_pos_h)$coefficients[,2][2], subset="h"))




# Specify the width of your confidence intervals
interval <- -qnorm((1-0.95)/2)  # 95% multiplier
coefficients <- within(coefficients,
                       var <- ordered(var, levels = rev(sort(unique(var)))))
secbreaks = rev(unique(as.character(coefficients$var)))

# Plot

pal<-wes_palette("Darjeeling", 4, type = "continuous")
zp1<-ggplot(coefficients, aes(colour = subset))+ geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+ geom_linerange(aes(x = var, ymin = fe - se*interval, ymax = fe + se*interval),lwd = 1, position = position_dodge(width = 1/2))+ geom_pointrange(aes(x = var, y = fe, ymin = fe - se*interval,ymax = fe + se*interval), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21, fill = "WHITE")+ theme_bw()+scale_color_manual(values = pal,labels=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"))+ scale_x_discrete(breaks=secbreaks,labels=c('Difference: Positive vs. Control','Difference: Negative vs. Control'))+ guides(colour=guide_legend(title="Income Quartiles"))+ ylab("Point Estimate")+ xlab(" ")


#########################################################
#############     APPENDIX TABLE 3            ###################
#########################################################

###### Consumer Status - Negative directed at wealthier people
### Subset to those with High, Middle, Low Consumer Status
lev_con_low<-subset(lev, consumer_status<3 & is.na(consumer_status)==FALSE)
lev_con_middle<-subset(lev, consumer_status==3 & is.na(consumer_status)==FALSE)
lev_con_high<-subset(lev, consumer_status>3 & is.na(consumer_status)==FALSE)

### High Status
### Control Vs. Negative
t.test(lev_con_high$fullnp~lev_con_high$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_con_high))

### Control Vs. Positive
t.test(lev_con_high$fullnp~lev_con_high$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_con_high))

length(lev_con_high$fullnp[lev_con_high$treatstat12np==0 & is.na(lev_con_high$treatstat12np)==FALSE])
length(lev_con_high$fullnp[lev_con_high$treatstat12np==1 & is.na(lev_con_high$treatstat12np)==FALSE])
length(lev_con_high$fullnp[lev_con_high$treatstat23np==1 & is.na(lev_con_high$treatstat23np)==FALSE])


### Middle Status
### Control Vs. Negative
t.test(lev_con_middle$fullnp~lev_con_middle$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_con_middle))

### Control Vs. Positive
t.test(lev_con_middle$fullnp~lev_con_middle$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_con_middle))

length(lev_con_middle$fullnp[lev_con_middle$treatstat12np==0 & is.na(lev_con_middle$treatstat12np)==FALSE])
length(lev_con_middle$fullnp[lev_con_middle$treatstat12np==1 & is.na(lev_con_middle$treatstat12np)==FALSE])
length(lev_con_middle$fullnp[lev_con_middle$treatstat23np==1 & is.na(lev_con_middle$treatstat23np)==FALSE])



### Low Status
### Control Vs. Negative
t.test(lev_con_low$fullnp~lev_con_low$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_con_low))

### Control Vs. Positive
t.test(lev_con_low$fullnp~lev_con_low$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_con_low))

length(lev_con_low$fullnp[lev_con_low$treatstat12np==0 & is.na(lev_con_low$treatstat12np)==FALSE])
length(lev_con_low$fullnp[lev_con_low$treatstat12np==1 & is.na(lev_con_low$treatstat12np)==FALSE])
length(lev_con_low$fullnp[lev_con_low$treatstat23np==1 & is.na(lev_con_low$treatstat23np)==FALSE])



#########################################################
#############     APPENDIX TABLE 4            ###################
#########################################################

lev_social_low<-subset(lev, socialstatus<3 & is.na(socialstatus)==FALSE)
lev_social_high<-subset(lev, socialstatus>2 & is.na(socialstatus)==FALSE)



### High Status
### Control Vs. Negative
t.test(lev_social_low$fullnp~lev_social_low$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_social_low))

### Control Vs. Positive
t.test(lev_social_low$fullnp~lev_social_low$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_social_low))

length(lev_social_low$fullnp[lev_social_low$treatstat12np==0 & is.na(lev_social_low$treatstat12np)==FALSE])
length(lev_social_low$fullnp[lev_social_low$treatstat12np==1 & is.na(lev_social_low$treatstat12np)==FALSE])
length(lev_social_low$fullnp[lev_social_low$treatstat23np==1 & is.na(lev_social_low$treatstat23np)==FALSE])


### Low Status
### Control Vs. Negative
t.test(lev_social_high$fullnp~lev_social_high$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_social_high))

### Control Vs. Positive
t.test(lev_social_high$fullnp~lev_social_high$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_social_high))

length(lev_social_high$fullnp[lev_social_high$treatstat12np==0 & is.na(lev_social_high$treatstat12np)==FALSE])
length(lev_social_high$fullnp[lev_social_high$treatstat12np==1 & is.na(lev_social_high$treatstat12np)==FALSE])
length(lev_social_high$fullnp[lev_social_high$treatstat23np==1 & is.na(lev_social_high$treatstat23np)==FALSE])



#########################################################
#############     APPENDIX TABLE 5            ###################
#########################################################
### Subset to Pensioners
lev_pensioner<-subset(lev, is.na(D4A)==FALSE & D4A=="Retired")
lev_nonpensioner<-subset(lev, is.na(D4A)==FALSE & D4A!="Retired")

### Pensioners
### Control Vs. Negative
t.test(lev_pensioner$fullnp~lev_pensioner$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_pensioner))

### Control Vs. Positive
t.test(lev_pensioner$fullnp~lev_pensioner$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_pensioner))

length(lev_pensioner$fullnp[lev_pensioner$treatstat12np==0 & is.na(lev_pensioner$treatstat12np)==FALSE])
length(lev_pensioner$fullnp[lev_pensioner$treatstat12np==1 & is.na(lev_pensioner$treatstat12np)==FALSE])
length(lev_pensioner$fullnp[lev_pensioner$treatstat23np==1 & is.na(lev_pensioner$treatstat23np)==FALSE])


### Non-Pensioners
### Control Vs. Negative
t.test(lev_nonpensioner$fullnp~lev_nonpensioner$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_nonpensioner))

### Control Vs. Positive
t.test(lev_nonpensioner$fullnp~lev_nonpensioner$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_nonpensioner))

length(lev_nonpensioner$fullnp[lev_nonpensioner$treatstat12np==0 & is.na(lev_nonpensioner$treatstat12np)==FALSE])
length(lev_nonpensioner$fullnp[lev_nonpensioner$treatstat12np==1 & is.na(lev_nonpensioner$treatstat12np)==FALSE])
length(lev_nonpensioner$fullnp[lev_nonpensioner$treatstat23np==1 & is.na(lev_nonpensioner$treatstat23np)==FALSE])



#########################################################
#############     APPENDIX TABLE 6            ###################
#########################################################

medianavgsalary<-summary(lev$avgsalary)[3]

### Subset to Employment
lev_lowsalary<-subset(lev, avgsalary<medianavgsalary & is.na(avgsalary)==FALSE)
lev_highsalary<-subset(lev, avgsalary>=medianavgsalary & is.na(avgsalary)==FALSE)

### LOW SALARY
### Control Vs. Negative
t.test(lev_lowsalary$fullnp~lev_lowsalary$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_lowsalary,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_lowsalary$fullnp~lev_lowsalary$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_lowsalary,na.action="na.omit"))

length(lev_lowsalary$fullnp[lev_lowsalary$treatstat12np==0 & is.na(lev_lowsalary$treatstat12np)==FALSE])
length(lev_lowsalary$fullnp[lev_lowsalary$treatstat12np==1 & is.na(lev_lowsalary$treatstat12np)==FALSE])
length(lev_lowsalary$fullnp[lev_lowsalary$treatstat23np==1 & is.na(lev_lowsalary$treatstat23np)==FALSE])

### Unemployed in area with high unemployment
### Control Vs. Negative
t.test(lev_highsalary$fullnp~lev_highsalary$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_highsalary,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_highsalary$fullnp~lev_highsalary$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_highsalary,na.action="na.omit"))

length(lev_highsalary$fullnp[lev_highsalary$treatstat12np==0 & is.na(lev_highsalary$treatstat12np)==FALSE])
length(lev_highsalary$fullnp[lev_highsalary$treatstat12np==1 & is.na(lev_highsalary$treatstat12np)==FALSE])
length(lev_highsalary$fullnp[lev_highsalary$treatstat23np==1 & is.na(lev_highsalary$treatstat23np)==FALSE])

#########################################################
#############     APPENDIX TABLE 7            ###################
#########################################################

medianwage<-summary(lev$reg_avwage)[3]

### Subset to Employment
lev_highwages<-subset(lev, reg_avwage>=medianwage)
lev_lowwages<-subset(lev, reg_avwage<medianwage)

### Employed
### Control Vs. Negative
t.test(lev_highwages$fullnp~lev_highwages$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_highwages,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_highwages$fullnp~lev_highwages$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_highwages,na.action="na.omit"))

length(lev_highwages$fullnp[lev_highwages$treatstat12np==0 & is.na(lev_highwages$treatstat12np)==FALSE])
length(lev_highwages$fullnp[lev_highwages$treatstat12np==1 & is.na(lev_highwages$treatstat12np)==FALSE])
length(lev_highwages$fullnp[lev_highwages$treatstat23np==1 & is.na(lev_highwages$treatstat23np)==FALSE])

### Unemployed in area with high unemployment
### Control Vs. Negative
t.test(lev_lowwages$fullnp~lev_lowwages$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_lowwages,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_lowwages$fullnp~lev_lowwages$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_lowwages,na.action="na.omit"))

length(lev_lowwages$fullnp[lev_lowwages$treatstat12np==0 & is.na(lev_lowwages$treatstat12np)==FALSE])
length(lev_lowwages$fullnp[lev_lowwages$treatstat12np==1 & is.na(lev_lowwages$treatstat12np)==FALSE])
length(lev_lowwages$fullnp[lev_lowwages$treatstat23np==1 & is.na(lev_lowwages$treatstat23np)==FALSE])




#########################################################
#############     APPENDIX TABLE 8            ###################
#########################################################

coefnames_inc = rev(resultsnp_income$coef.names)
coefs_con_inc<-rev(resultsnp_income$par.control)
ses_con_inc<-rev(resultsnp_income$se.control)
tr_con_inc_obs<-length(resultsnp_income$treat[resultsnp_income$treat==0])
pvalues_con_inc <-dt(c(rev(resultsnp_income$par.control)/rev(resultsnp_income$se.control)),as.numeric(summary(resultsnp_income)[11]))
tr_con_inc <- createTexreg( coef.names= coefnames_inc, coef = coefs_con_inc, se = ses_con_inc, model.name="Positive",gof.names="Observations",gof=tr_con_inc_obs,pvalues=pvalues_con_inc)

coefnames_con = rev(resultsnp_consumerstatus$coef.names)
coefs_con_con<-rev(resultsnp_consumerstatus$par.control)
ses_con_con<-rev(resultsnp_consumerstatus$se.control)
tr_con_con_obs<-length(resultsnp_consumerstatus$treat[resultsnp_consumerstatus$treat==0])
pvalues_con_con <-dt(c(rev(resultsnp_consumerstatus$par.control)/rev(resultsnp_consumerstatus$se.control)),as.numeric(summary(resultsnp_consumerstatus)[11]))
tr_con_con <- createTexreg( coef.names= coefnames_con, coef = coefs_con_con, se = ses_con_con, model.name="Positive",gof.names="Observations",gof=tr_con_con_obs,pvalues=pvalues_con_con)

coefnames_soc = rev(resultsnp_socialstatus$coef.names)
coefs_con_soc<-rev(resultsnp_socialstatus$par.control)
ses_con_soc<-rev(resultsnp_socialstatus$se.control)
tr_con_soc_obs<-length(resultsnp_socialstatus$treat[resultsnp_socialstatus$treat==0])
pvalues_con_soc <-dt(c(rev(resultsnp_socialstatus$par.control)/rev(resultsnp_socialstatus$se.control)),as.numeric(summary(resultsnp_socialstatus)[11]))
tr_con_soc <- createTexreg( coef.names= coefnames_soc, coef = coefs_con_soc, se = ses_con_soc, model.name="Positive",gof.names="Observations",gof=tr_con_soc_obs,pvalues=pvalues_con_soc)

control_table<-texreg(list(tr_con_inc,tr_con_con,tr_con_soc),reorder.coef=c(1,2,3,4,5,6,7,9,10,8),stars = c(0.01, 0.05, 0.1),booktabs=TRUE, table = FALSE,digits=3,custom.coef.names=c("Single Company Town","Town Size","Education","Age (log)","Male","Employed","Income (log)","Intercept","Consumer Status","Social Status"),use.packages=FALSE,custom.note="")
control_table<-gsub(".000 ","",control_table,fixed=TRUE)
# 
# sink(file=paste(TablesPath,"ols_control.tex",sep=''))
# cat(control_table)
# sink()



#########################################################
#############     APPENDIX TABLE 9            ###################
#########################################################

### Subset to Employment
lev_highunemployed_emp<-subset(lev, unemployedrate>=mediancityunemployment & is.na(unemployedrate)==FALSE& employed3==1 & is.na(employed3)==FALSE)
lev_highunemployed_notemp<-subset(lev, unemployedrate>=mediancityunemployment & is.na(unemployedrate)==FALSE& employed3==0 & is.na(employed3)==FALSE)

### Employed
### Control Vs. Negative
t.test(lev_highunemployed_emp$fullnp~lev_highunemployed_emp$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_highunemployed_emp,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_highunemployed_emp$fullnp~lev_highunemployed_emp$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_highunemployed_emp,na.action="na.omit"))

length(lev_highunemployed_emp$fullnp[lev_highunemployed_emp$treatstat12np==0 & is.na(lev_highunemployed_emp$treatstat12np)==FALSE])
length(lev_highunemployed_emp$fullnp[lev_highunemployed_emp$treatstat12np==1 & is.na(lev_highunemployed_emp$treatstat12np)==FALSE])
length(lev_highunemployed_emp$fullnp[lev_highunemployed_emp$treatstat23np==1 & is.na(lev_highunemployed_emp$treatstat23np)==FALSE])

### Unemployed in area with high unemployment
### Control Vs. Negative
t.test(lev_highunemployed_notemp$fullnp~lev_highunemployed_notemp$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_highunemployed_notemp,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_highunemployed_notemp$fullnp~lev_highunemployed_notemp$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_highunemployed_notemp,na.action="na.omit"))

length(lev_highunemployed_notemp$fullnp[lev_highunemployed_notemp$treatstat12np==0 & is.na(lev_highunemployed_notemp$treatstat12np)==FALSE])
length(lev_highunemployed_notemp$fullnp[lev_highunemployed_notemp$treatstat12np==1 & is.na(lev_highunemployed_notemp$treatstat12np)==FALSE])
length(lev_highunemployed_notemp$fullnp[lev_highunemployed_notemp$treatstat23np==1 & is.na(lev_highunemployed_notemp$treatstat23np)==FALSE])



#########################################################
#############     APPENDIX TABLE 10            ###################
#########################################################

### Subset to Employment
lev_lowemployed_emp<-subset(lev, percemployed<=mediancityemployed & is.na(percemployed)==FALSE& employed3==1 & is.na(employed3)==FALSE)
lev_lowemployed_notemp<-subset(lev, percemployed<=mediancityemployed & is.na(percemployed)==FALSE& employed3==0 & is.na(employed3)==FALSE)

### Employed
### Control Vs. Negative
t.test(lev_lowemployed_emp$fullnp~lev_lowemployed_emp$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_lowemployed_emp,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_lowemployed_emp$fullnp~lev_lowemployed_emp$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_lowemployed_emp,na.action="na.omit"))

length(lev_lowemployed_emp$fullnp[lev_lowemployed_emp$treatstat12np==0 & is.na(lev_lowemployed_emp$treatstat12np)==FALSE])
length(lev_lowemployed_emp$fullnp[lev_lowemployed_emp$treatstat12np==1 & is.na(lev_lowemployed_emp$treatstat12np)==FALSE])
length(lev_lowemployed_emp$fullnp[lev_lowemployed_emp$treatstat23np==1 & is.na(lev_lowemployed_emp$treatstat23np)==FALSE])

### Unemployed in area with high unemployment
### Control Vs. Negative
t.test(lev_lowemployed_notemp$fullnp~lev_lowemployed_notemp$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_lowemployed_notemp,na.action="na.omit"))

### Control Vs. Positive
t.test(lev_lowemployed_notemp$fullnp~lev_lowemployed_notemp$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_lowemployed_notemp,na.action="na.omit"))

length(lev_lowemployed_notemp$fullnp[lev_lowemployed_notemp$treatstat12np==0 & is.na(lev_lowemployed_notemp$treatstat12np)==FALSE])
length(lev_lowemployed_notemp$fullnp[lev_lowemployed_notemp$treatstat12np==1 & is.na(lev_lowemployed_notemp$treatstat12np)==FALSE])
length(lev_lowemployed_notemp$fullnp[lev_lowemployed_notemp$treatstat23np==1 & is.na(lev_lowemployed_notemp$treatstat23np)==FALSE])



#########################################################
#############     APPENDIX TABLE 11            ###################
#########################################################


### Subset to Employment - Monitoring
lev_highunemployed<-subset(lev, unemployedrate>=mediancityunemployment & is.na(unemployedrate)==FALSE)
lev_lowunemployed<-subset(lev, unemployedrate<mediancityunemployment & is.na(unemployedrate)==FALSE)

### High Unemployment
### Control Vs. Negative
t.test(lev_highunemployed$full_tell~lev_highunemployed$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_highunemployed, na.action="na.omit"))

length(lev_highunemployed$full_tell[lev_highunemployed$treatstat_tell==0 & is.na(lev_highunemployed$treatstat_tell)==FALSE])
length(lev_highunemployed$full_tell[lev_highunemployed$treatstat_tell==1 & is.na(lev_highunemployed$treatstat_tell)==FALSE])

### Low Unemployment
### Control Vs. Negative
t.test(lev_lowunemployed$full_tell~lev_lowunemployed$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_lowunemployed, na.action="na.omit"))

length(lev_lowunemployed$full_tell[lev_lowunemployed$treatstat_tell==0 & is.na(lev_lowunemployed$treatstat_tell)==FALSE])
length(lev_lowunemployed$full_tell[lev_lowunemployed$treatstat_tell==1 & is.na(lev_lowunemployed$treatstat_tell)==FALSE])


#########################################################
#############     APPENDIX TABLE 12            ###################
#########################################################
### Subset to Employable Population- Monitoring

lev_lowemployed<-subset(lev, percemployed<=mediancityemployed & is.na(percemployed)==FALSE)
lev_highemployed<-subset(lev, percemployed>mediancityemployed & is.na(percemployed)==FALSE)

### High Unemployment
### Control Vs. Negative
t.test(lev_lowemployed$full_tell~lev_lowemployed$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_lowemployed, na.action="na.omit"))

length(lev_lowemployed$full_tell[lev_lowemployed$treatstat_tell==0 & is.na(lev_lowemployed$treatstat_tell)==FALSE])
length(lev_lowemployed$full_tell[lev_lowemployed$treatstat_tell==1 & is.na(lev_lowemployed$treatstat_tell)==FALSE])

### Low Unemployment
### Control Vs. Negative
t.test(lev_highemployed$full_tell~lev_highemployed$treatstat_tell,alternative="less")
summary(lm(full_tell~factor(treatstat_tell), data=lev_highemployed, na.action="na.omit"))

length(lev_highemployed$full_tell[lev_highemployed$treatstat_tell==0 & is.na(lev_lowunemployed$treatstat_tell)==FALSE])
length(lev_highemployed$full_tell[lev_highemployed$treatstat_tell==1 & is.na(lev_lowunemployed$treatstat_tell)==FALSE])


#########################################################
#############     APPENDIX TABLE 13           ###################
#########################################################
### State employment
lev$stateemp<-0
lev$stateemp[lev$q6=="Federal government and/or budget organizations" | lev$q6=="Regional and Local Govt or budget organizations"  | lev$q6=="State Enterprise (GUP, MUP, etc.)"]<-1

### Subset to Pensioners
lev_stateemployee<-subset(lev, employed3==1 & is.na(employed3)==FALSE& stateemp==1)
lev_nonstateemployee<-subset(lev, employed3==1 & is.na(employed3)==FALSE& stateemp==0)

### Pensioners
### Control Vs. Negative
t.test(lev_stateemployee$fullnp~lev_stateemployee$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_stateemployee))

### Control Vs. Positive
t.test(lev_stateemployee$fullnp~lev_stateemployee$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_stateemployee))

length(lev_stateemployee$fullnp[lev_stateemployee$treatstat12np==0 & is.na(lev_stateemployee$treatstat12np)==FALSE])
length(lev_stateemployee$fullnp[lev_stateemployee$treatstat12np==1 & is.na(lev_stateemployee$treatstat12np)==FALSE])
length(lev_stateemployee$fullnp[lev_stateemployee$treatstat23np==1 & is.na(lev_stateemployee$treatstat23np)==FALSE])


### Non-Pensioners
### Control Vs. Negative
t.test(lev_nonstateemployee$fullnp~lev_nonstateemployee$treatstat12np,alternative="less")
summary(lm(fullnp~factor(treatstat12np), data=lev_nonstateemployee))

### Control Vs. Positive
t.test(lev_nonstateemployee$fullnp~lev_nonstateemployee$treatstat13np,alternative="less")
summary(lm(fullnp~factor(treatstat13np), data=lev_nonstateemployee))

length(lev_nonstateemployee$fullnp[lev_nonstateemployee$treatstat12np==0 & is.na(lev_nonstateemployee$treatstat12np)==FALSE])
length(lev_nonstateemployee$fullnp[lev_nonstateemployee$treatstat12np==1 & is.na(lev_nonstateemployee$treatstat12np)==FALSE])
length(lev_nonstateemployee$fullnp[lev_nonstateemployee$treatstat23np==1 & is.na(lev_nonstateemployee$treatstat23np)==FALSE])







#########################################################
#############     APPENDIX TABLE 18           ###################
#########################################################

lev_sum <- as.data.table(lev)[,list(male,ordeduc,log_age,townsize,logincome,monogorod,socialstatus,consumer_status,stateemp,reg_levelofunempl.x,reg_avwage,avgsalary,unemployedrate,percemployed)]

t<-stargazer(lev_sum,summary=TRUE,title="Appendix Table 18: Descriptive Statistics",covariate.labels=c("Male","Education","Age (log)","Town Size","Income (log)","Monogorod","Social Status","Consumer Status","State Employee","Regional Level of Unemployment","Regional Average Wage (thous. rub)","City Average Wage (thous. rub)","City Level Unemployment Rate (Unemployment Benefits)","City Level Employment Rate (Labor Force Participation))"),font.size="footnotesize", digits=2,digits.extra=0,label="fullsummary",column.sep.width = "2pt",header=FALSE)

t<-gsub("\\caption", "\\caption*", t, fixed =TRUE)
t<-gsub("\\begin{tabular}", "\\scalebox{.95}{\\begin{tabular}", t, fixed =TRUE)
t<-gsub("\\end{tabular}", "\\end{tabular}}", t, fixed =TRUE)
sink(file=paste(TablesPath,"DescriptiveStats.tex",sep=''))
cat(t)





