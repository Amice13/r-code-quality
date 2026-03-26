#################################################
#ASSOCIATION BTWN HAIR RELAXER & UTERINE FIBROIDS
#################################################

rm(list=ls())

setwd("C:\\Users\\Mweu\\Documents\\UoN_files\\STUDENT_SUPERVISION\\MARY\\Data")

if(!'readr' %in% row.names(installed.packages())){install.packages('readr')}; require('readr')
if(!'graphPAF' %in% row.names(installed.packages())){install.packages('graphPAF')}; require('graphPAF')
if(!'dplyr' %in% row.names(installed.packages())){install.packages('dplyr')}; require('dplyr')

###################################################################################################################################
#CREATE SOME FUNCTIONS...
######################

#Function to return OR, CI and P-values
or.conf.p <- function(model){
  
  p.val <- c()
  
  pars <- length(summary(model)$coefficients[,1]) 
  
  for(i in 1:(pars-1)){
    
    p.val[i] <- ifelse(summary(model)$coefficients[(i+1),4] < 0.001,'<0.001',round(summary(model)$coefficients[(i+1),4],digits=3))
    
  }
  
  if(pars<=2){x <- suppressMessages(t(confint.default(model)[2:pars,]))}
  else {x <- suppressMessages(confint.default(model)[2:pars,])}
  
  df <- data.frame(round(exp(cbind(OR=coef(model)[2:pars],x)),digits=2),
                   p.value=p.val); colnames(df)[2:3] <- c('lcl.or','ucl.or')
  
  df
  
}

#Function to return P-values for each predictor
LRT.pval <- function(null.model,model,df,factors){
  
  x <- all.vars(formula(model)); x[2] <- factors[p]
  
  new.null <- update(null.model,data=na.omit(df[,x]))
  
  p.val <- ifelse(anova(new.null,model,test='LRT')[2,5] < 0.001,'<0.001',
                  round(anova(new.null,model,test='LRT')[2,5],digits=3))
  
  p.val

}

#################################################################################################################################
#LOAD DATASET
#############

uf.dat <- read.csv('uf_data.csv',header=T)

str(uf.dat)

#################################################################################################################################
#DATA CLEANING & RECODING
###########################

uf.dat$BMI <- uf.dat$Wt/((uf.dat$Ht/100)^2)

uf.dat$Mar_status <- factor(uf.dat$Mar_status,labels=c('Single','Married','Separated/Divorced',
                                                       'Widowed'))

uf.dat$Ethnic <- factor(uf.dat$Ethnic,labels=c('Kikuyu','Luhya','Kalenjin','Luo','Kamba',
                                                       'Others'))

uf.dat$Educ <- factor(uf.dat$Educ,labels=c('None','Primary','Secondary','Tertiary'))

uf.dat$Occup <- factor(uf.dat$Occup,labels=c('Unemployed','Employed'))

uf.dat$Income <- factor(uf.dat$Income,labels=c('Lower income','Middle income','Upper income'))

uf.dat$Exercise <- factor(uf.dat$Exercise,labels=c('Never','Infrequent','Regular'))

uf.dat$Fuel <- factor(uf.dat$Fuel,labels=c('Biomass/Kerosene','Natural/Biogas')) %>% relevel(ref='Natural/Biogas')

uf.dat$ALC <- factor(uf.dat$ALC,labels=c('None','Infrequent','Regular'))

uf.dat[,c('Co_morb','Fam_hist','Contra_use')] <- lapply(uf.dat[,c('Co_morb','Fam_hist','Contra_use')],
                                                                factor,levels=0:1,labels=c('No','Yes'))

###################################################################################################################################
#DESCRIPTIVE STATS: SEPARATE FOR CASES & CONTROLS
####################################################

#HR products analyses (Analysis done only among HR users)
#########################################################

#1. proportions of women using different HR products
hr.prod <- colnames(uf.dat[,25:37])

all.women.by.prod.freq <- all.women.by.prod.props <- list()
controls.by.prod.freq <- cases.by.prod.freq <- list()
controls.by.prod.props <- cases.by.prod.props <- list()

for(i in 1:length(hr.prod)){
  
  all.women.by.prod.freq[[i]] <- table(uf.dat[,hr.prod[i]][uf.dat[,'HR_XP']==1]); names(all.women.by.prod.freq)[i] <- hr.prod[i]
  all.women.by.prod.props[[i]] <- round(prop.table(table(uf.dat[,hr.prod[i]][uf.dat[,'HR_XP']==1])),digits=3); names(all.women.by.prod.props)[i] <- hr.prod[i]
  
}

for(k in 1:length(hr.prod)){
  
  controls.by.prod.freq[[k]] <- table(uf.dat[,hr.prod[k]][uf.dat$HR_XP==1 & uf.dat$UF==0]); names(controls.by.prod.freq)[k] <- hr.prod[k]
  cases.by.prod.freq[[k]] <- table(uf.dat[,hr.prod[k]][uf.dat$HR_XP==1 & uf.dat$UF==1]); names(cases.by.prod.freq)[k] <- hr.prod[k]
  
  controls.by.prod.props[[k]] <- round(prop.table(table(uf.dat[,hr.prod[k]][uf.dat$HR_XP==1 & uf.dat$UF==0])),digits=3); names(controls.by.prod.props)[k] <- hr.prod[k]
  cases.by.prod.props[[k]] <- round(prop.table(table(uf.dat[,hr.prod[k]][uf.dat$HR_XP==1 & uf.dat$UF==1])),digits=3); names(cases.by.prod.props)[k] <- hr.prod[k]
  
} 

all.women.by.prod.freq
all.women.by.prod.props

controls.by.prod.freq
controls.by.prod.props

cases.by.prod.freq
cases.by.prod.props

#2. Frequency of products used per woman  
uf.dat$tot.prod <- apply(uf.dat[,(25:37)],1,function(x)sum(x))

(all.prod.all.women.freq <- table(uf.dat$tot.prod[uf.dat$HR_XP==1])) #how many women by no. of products used
(all.prod.all.women.prop <- round(prop.table(table(uf.dat$tot.prod[uf.dat$HR_XP==1])),digits=3))

(all.prod.controls.freq <- table(uf.dat$tot.prod[uf.dat$HR_XP==1 & uf.dat$UF==0]))
(all.prod.controls.prop <- round(prop.table(table(uf.dat$tot.prod[uf.dat$HR_XP==1 & uf.dat$UF==0])),digits=3))

(all.prod.cases.freq <- table(uf.dat$tot.prod[uf.dat$HR_XP==1 & uf.dat$UF==1]))
(all.prod.cases.prop <- round(prop.table(table(uf.dat$tot.prod[uf.dat$HR_XP==1 & uf.dat$UF==1])),digits=3))

#3. Years product has been used
(all.women.yrs <- summary(parse_number(uf.dat$Yr_freq[uf.dat$HR_XP==1])))

(controls.yrs <- summary(parse_number(uf.dat$Yr_freq[uf.dat$HR_XP==1 & uf.dat$UF==0])))
(cases.yrs <- summary(parse_number(uf.dat$Yr_freq[uf.dat$HR_XP==1 & uf.dat$UF==1])))

#####################################################################
#Predictors
###########

#Continuous predictors
vars <- c('Age','BMI','Menarche','Parity'); cont.var.controls <- cont.var.cases <- list()

for(i in 1:length(vars)){
  
  cont.var.controls[[i]] <- summary(uf.dat[,vars[i]][uf.dat$UF==0]); names(cont.var.controls)[i] <- vars[i]
  cont.var.cases[[i]] <- summary(uf.dat[,vars[i]][uf.dat$UF==1]); names(cont.var.cases)[i] <- vars[i]
}

cont.var.controls
cont.var.cases

#Categorical predictors
n.vars <- colnames(uf.dat[,c(22,6:14,17,21)])

categ.var.controls.freq <- categ.var.controls.props <- list()
categ.var.cases.freq <- categ.var.cases.props <- list()

for(i in 1:length(n.vars)){
  
  categ.var.controls.freq[[i]] <- table(uf.dat[,n.vars[i]][uf.dat$UF==0]); names(categ.var.controls.freq)[i] <- n.vars[i]
  categ.var.controls.props[[i]] <- round(prop.table(table(uf.dat[,n.vars[i]][uf.dat$UF==0])),digits=3); names(categ.var.controls.props)[i] <- n.vars[i]
  
  categ.var.cases.freq[[i]] <- table(uf.dat[,n.vars[i]][uf.dat$UF==1]); names(categ.var.cases.freq)[i] <- n.vars[i]
  categ.var.cases.props[[i]] <- round(prop.table(table(uf.dat[,n.vars[i]][uf.dat$UF==1])),digits=3); names(categ.var.cases.props)[i] <- n.vars[i]
  
}

categ.var.controls.freq
categ.var.controls.props
categ.var.cases.freq
categ.var.cases.props

###################################################################################################################################################
#CRUDE ASSOCIATION BETWN HR & UF
################################

Crude.model <- glm(UF ~ HR_XP,family=binomial(link="logit"),data=uf.dat) 

(crude.or <- or.conf.p(model=Crude.model)) #Check crude OR, CI and P-value for univariable model

####################################################################################################################################################
#SCREENING PREDICTORS FOR ASSOCIATIONS WITH UF (P<0.05)
#######################################################

null.model <- update(Crude.model,.~.-HR_XP)

###########################################
#Recategorise some variables first
#BMI
uf.dat$BMI.cat[uf.dat$BMI<18.5] <- 'Underweight'
uf.dat$BMI.cat[uf.dat$BMI>=18.5 & uf.dat$BMI<25] <- 'Normal'
uf.dat$BMI.cat[uf.dat$BMI>=25 & uf.dat$BMI<30] <- 'Overweight'
uf.dat$BMI.cat[uf.dat$BMI>=30] <- 'Obese'

uf.dat$BMI.cat <- factor(uf.dat$BMI.cat,levels=c('Normal','Underweight','Overweight','Obese'))

#Mar_status
uf.dat$Mar_status_2 <- ifelse(uf.dat$Mar_status=='Married','Married','Single')

#Income
uf.dat$Income_2 <- ifelse(uf.dat$Income=='Lower income','Low income','High income')

#Alc
uf.dat$ALC_2 <- ifelse(uf.dat$ALC=='None','No','Yes')

#Educ
uf.dat$Educ_2[uf.dat$Educ=='None' | uf.dat$Educ=='Primary'] <- 'Primary'
uf.dat$Educ_2[uf.dat$Educ=='Secondary'] <- 'Secondary'
uf.dat$Educ_2[uf.dat$Educ=='Tertiary'] <- 'Tertiary'

uf.dat[,c('BMI.cat','Mar_status_2','Income_2','ALC_2','Educ_2')] <- lapply(uf.dat[,c('BMI.cat','Mar_status_2','Income_2','ALC_2','Educ_2')],factor)

#################################################

factors <- c('Age','BMI.cat','Menarche','Parity','Mar_status_2','Income_2','ALC_2','Educ_2',
             'Ethnic','Occup','Exercise','Co_morb','Fuel','Fam_hist','Contra_use')

odds.conf.UF <- UF.pvals <- list()

for(p in 1:length(factors)){
  
  var <- assign(factors[p],uf.dat[,factors[p]])
  
  var.model <- update(null.model,.~.+var) 
  
  odds.conf.UF[[p]] <- or.conf.p(model=var.model); names(odds.conf.UF)[p] <- factors[p]
  
  UF.pvals[[p]] <- LRT.pval(null.model=null.model,model=var.model,df=uf.dat,factors=factors); names(UF.pvals)[p] <- factors[p]
  
}

odds.conf.UF
UF.pvals

p.val.ind.UF <- parse_number(as.vector(unlist(UF.pvals))) < 0.05; (sig.vars.UF <- factors[p.val.ind.UF])

##################################################################################################################################################
#SCREENING PREDICTORS FOR ASSOCIATIONS WITH HR (P<0.05)
#######################################################

null.HR.model <- glm(HR_XP ~ 1,family=binomial(link="logit"),data=uf.dat) 

odds.conf.HR <- HR.pvals <- list()

for(p in 1:length(sig.vars.UF)){
  
  var <- assign(sig.vars.UF[p],uf.dat[,sig.vars.UF[p]])
  
  var.model <- update(null.HR.model,.~.+var) 
  
  odds.conf.HR[[p]] <- or.conf.p(model=var.model); names(odds.conf.HR)[p] <- sig.vars.UF[p]
  
  HR.pvals[[p]] <- LRT.pval(null.model=null.HR.model,model=var.model,df=uf.dat,factors=sig.vars.UF); names(HR.pvals)[p] <- sig.vars.UF[p]
  
}

odds.conf.HR
HR.pvals

p.val.ind.HR <- parse_number(as.vector(unlist(HR.pvals))) < 0.05; (sig.vars.HR <- sig.vars.UF[p.val.ind.HR])

##############################################################################################################################################
#MULTIVARIABLE ANALYSIS & PAF CALCULATION
##########################################

Multi.model <- update(Crude.model,.~.+ Age + Co_morb)

(adjusted.or <- or.conf.p(model=Multi.model))

#Is there confounding by Age & Co_morb: should be FALSE if no confounding
abs(as.vector((coef(Crude.model)[2] - coef(Multi.model)[2])/coef(Crude.model)[2])) > 0.20

#Calculate PAF (and its bootstrapped CI) from adjusted OR
set.seed(235690); (PAF <- PAF_calc_discrete(model=Multi.model,riskfactor='HR_XP',refval=0,data=uf.dat,
                                            calculation_method='B',ci=T,boot_rep=1000))

###################################################################################################################################################
#TEST SIGNIFICANCE OF MULTIVARIABLE COEFS USING LRT
###################################################

multi.pval <- function(model){
  
  p.val <- drop1(model,test='LRT')[-1,5]
  
  var <- all.vars(formula(model))[-1]; p.vals <- c()
  
  for(i in 1:length(var)){
    
    p.vals[i] <- ifelse(p.val[i] < 0.001,'<0.001',round(p.val[i],digits=3))

  }
  
  df <- data.frame(vars=var,p.val=p.vals)
  
  df
  
}

multi.pval(model=Multi.model)

########################################################################################################################################################  
#END