###########################################################################
#ASSOCIATION BETWN FERMENTED MILK & ESOPHAGEAL CARCINOMA - STATS ANALYSIS
###########################################################################

rm(list=ls())

setwd("C:\\Users\\Mweu\\Documents\\UoN_files\\STUDENT_SUPERVISION\\RANCY\\Data") #Change working directory

if(!'dplyr' %in% row.names(installed.packages())){install.packages('dplyr')}; require('dplyr')
if(!'gmodels' %in% row.names(installed.packages())){install.packages('gmodels')}; require('gmodels')
if(!'graphPAF' %in% row.names(installed.packages())){install.packages('graphPAF')}; require('graphPAF')

####################################################################################################################
#LOAD DATASET
#############

#sink(file='OC_fm_RESULTS.txt')

oc.fm <- read.csv('OC_data.csv',header=T)

str(oc.fm)

################################################################################################################
#LABELLING VARIABLES
#####################

summary(oc.fm$age[oc.fm$OC==1])
summary(oc.fm$age[oc.fm$OC==0])

oc.fm$age.cat[oc.fm$age<=40] <- '<=40'
oc.fm$age.cat[oc.fm$age>40 & oc.fm$age<=65] <- '41-65'
oc.fm$age.cat[oc.fm$age>65] <- '>65'

oc.fm$age.cat <- factor(oc.fm$age.cat,levels=c('<=40','41-65','>65'))

oc.fm$sex <- factor(oc.fm$sex,labels=c('male','female'))

oc.fm$mar_st <- factor(oc.fm$mar_st,labels=c('single','married','divorced/separated','widowed'))

oc.fm$educ <- factor(oc.fm$educ,labels=c('none','primary','secondary','tertiary'))

oc.fm$emply <- factor(oc.fm$emply,labels=c('unemployed','employed'))

oc.fm$alc <- factor(oc.fm$alc,labels=c('never','infrequent','regular'))

oc.fm$tob <- factor(oc.fm$tob,labels=c('no','yes'))

oc.fm$bev_temp <- factor(oc.fm$bev_temp,labels=c('warm','hot','very hot'))

oc.fm$fm <- factor(oc.fm$fm,labels=c('never','infrequent','regular'))

oc.fm$fuel <- ifelse(oc.fm$cook_fuel==0 | oc.fm$cook_fuel==1,0,1) %>%
  factor(labels=c('biomass/kerosene','natural/biogas'))

############################################################################################################
#DESCRIPTIVE STATISTICS
#######################

vars <- colnames(oc.fm)[c(3:10,12:13)]; descr.freq <- descr.props <- list()

for(i in 1:length(vars)){
  
  descr.freq[[i]] <- CrossTable(oc.fm[,vars[i]])$t; names(descr.freq)[i] <- vars[i]
  
  descr.props[[i]] <- CrossTable(oc.fm[,vars[i]])$prop.tbl; names(descr.props)[i] <- vars[i]
  
}

descr.freq

descr.props

#############################################################################################################
#CRUDE ASSOCIATION BETWN FM & OC (LOGISTIC REGRESSION)
#########################################################

fm.model <- glm(OC ~ fm,family=binomial(link="logit"),data=oc.fm) 

or.conf.p <- function(model){
  
  p.val <- c()
  
  pars <- length(summary(model)$coefficients[,1]) 
  
  for(i in 1:(pars-1)){
    
    p.val[i] <- ifelse(summary(model)$coefficients[(i+1),4] < 0.001,'<0.001',round(summary(model)$coefficients[(i+1),4],digits=3))
    
  }
  
  df <- data.frame(round(exp(cbind(OR=coef(model)[2:pars],confint.default(model)[2:pars,])),digits=2),
             p.value=p.val); colnames(df)[2:3] <- c('lcl.or','ucl.or')
  
  df
  
}

(crude.or <- or.conf.p(model=fm.model)) #Check crude OR, CI and P-value for univariable model

##########################################################################################################################################
#UNCONDITIONAL ASSOCIATIONS BETWN OUTCOME & PREDICTORS (FISHER'S EXACT TEST: P<0.05)
####################################################################################

fisher.O <- list(); vars.n <- vars[-8]

for(i in 1:length(vars.n)){
  
  fisher.O[[i]] <- round(fisher.test(table(oc.fm$OC,oc.fm[,vars.n[i]]))$p.value,digits=3)
  
  names(fisher.O)[i] <- vars.n[i]
}

fisher.O #educ, alc, tob, age.cat

##########################################################################################################################################
#UNCONDITIONAL ASSOCIATIONS BETWN EXPOSURE & PREDICTORS (FISHER'S EXACT TEST: P<0.05)
#####################################################################################

fisher.E <- list(); vars.exp <- names(fisher.O)[which(unlist(fisher.O) < 0.05)]

for(i in 1:length(vars.exp)){
  
  fisher.E[[i]] <- round(fisher.test(table(oc.fm$fm,oc.fm[,vars.exp[i]]))$p.value,digits=3)
  
  names(fisher.E)[i] <- vars.exp[i]
}

fisher.E #alc, age.cat

###########################################################################################################################################
#MULTIVARIABLE ANALYSIS & PAF CALCULATION
#########################################

multi.model <- glm(OC ~ fm + alc + age.cat,family=binomial(link="logit"),data=oc.fm)  

(adjusted.or <- or.conf.p(model=multi.model)) #check adjusted OR, CI and P.values for multivariable model

#Is there confounding by alcohol & age: should ALL be FALSE if no confounding
all(abs((coef(fm.model)[2:3] - coef(multi.model)[2:3])/coef(fm.model)[2:3]) > 0.20)

#Calculate PAF (and its bootstrapped CI) from crude OR
set.seed(235690); (PAF <- PAF_calc_discrete(model=fm.model,riskfactor='fm',refval='never',data=oc.fm,
                                            calculation_method='B',ci=T,boot_rep=1000))

#############################################################################################################################################
#TESTING SIGNIFICANCE OF COEFFICIENTS USING LIKELIHOOD RATIO TESTS
##################################################################

null.model <- update(fm.model,.~.-fm)

red.model.alc <- update(multi.model,.~.-alc)

red.model.age <- update(multi.model,.~.-age.cat)

red.model.fm <- update(multi.model,.~.-fm)

  anova(null.model,fm.model,test='LRT') #test for 'fm' in univariable model

  anova(red.model.alc,multi.model,test='LRT') #test for 'alcohol' in multivariable model

  anova(red.model.age,multi.model,test='LRT') #test for 'age' in multivariable model
  
  anova(red.model.fm,multi.model,test='LRT') #test for 'fm' in multivariable model

#sink(file=NULL)

#################################################################################################################################################
#END
  