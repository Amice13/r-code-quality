#########################################
#Import Data
#########################################
#Import data file
data<-data.frame(read.dta("teenpreg24feb14withindexpre1969.dta")) #Data file referenced in Sample.do

#########################################
#Table 1
#########################################

#Data adjustments made in Sample file

data$familyincome[data$familyincome==17999] <- 18499

data$lnfamincome<-log(data$familyincome)#Replace lnfamincome (showing up as NAs) with log(familyincom)

#Create matrix of variables needed for first table
data_1<-data[c("firstpregbirthdk","agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
"hasworkingmother", "momeduc", "hasmomeduc", "liveboth14", "educ", "educatl12", "agefirstmarriage", "infirstmarr", "marrafterfirst", 
"nevermarried", "numchildren", "working", "familyincome", "increlpoverty", 
"AGE", "survey", "survey1973", "survey1976", "survey1982", "survey1988",  "marrpreconceptdateUSE", "havemarrpreconceptdate", "wtuse")]

#Create separate matrices for each category of miscarriage/birth and married pre conception/not
birth_marriedpreconc<-data_1[(data_1$firstpregbirthdk==1 & data_1$marrpreconceptdateUSE==1),]
misc_marriedpreconc<-data_1[(data_1$firstpregbirthdk==0 & data_1$marrpreconceptdateUSE==1),]
birth_notmarpreconc<-data_1[(data_1$firstpregbirthdk==1 & data_1$marrpreconceptdateUSE==0 & data_1$havemarrpreconceptdate==1),]
misc_notmarpreconc<-data_1[(data_1$firstpregbirthdk==0 & data_1$marrpreconceptdateUSE==0 & data_1$havemarrpreconceptdate==1),]

#Calculate means of each column in each category using weights wtuse
a<-as.matrix(apply(birth_marriedpreconc,2,weighted.mean,w=birth_marriedpreconc$wtuse,na.rm=TRUE))
b<-as.matrix(apply(misc_marriedpreconc,2,weighted.mean,w=misc_marriedpreconc$wtuse,na.rm=TRUE))
c<-as.matrix(apply(birth_notmarpreconc,2,weighted.mean,w=birth_notmarpreconc$wtuse,na.rm=TRUE))
d<-as.matrix(apply(misc_notmarpreconc,2,weighted.mean,w=misc_notmarpreconc$wtuse,na.rm=TRUE))

#Combine mean vectors into single matrix
weight_means<-as.matrix(cbind(a,b,c,d))

#Get standard errors
library(Hmisc)
a<-sqrt(as.matrix(apply(birth_marriedpreconc,2,wtd.var,weight=birth_marriedpreconc$wtuse,na.rm=TRUE)))
b<-sqrt(as.matrix(apply(misc_marriedpreconc,2,wtd.var,weight=misc_marriedpreconc$wtuse,na.rm=TRUE)))
c<-sqrt(as.matrix(apply(birth_notmarpreconc,2,wtd.var,w=birth_notmarpreconc$wtuse,na.rm=TRUE)))
d<-sqrt(as.matrix(apply(misc_notmarpreconc,2,wtd.var,w=misc_notmarpreconc$wtuse,na.rm=TRUE)))

#Combine standard errors into single matrix
weight_se<-as.matrix(cbind(a,b,c,d))

#Recalculate liveboth14 to exclude 1995. NOTE: in paper they say that they just calculated Liveboth14 across 1982 and 1988, but in fact it should be across all except 1995, which is what was done in the actual analysis
weight_means[13,1]<-weighted.mean(birth_marriedpreconc$liveboth14[(birth_marriedpreconc$survey!=1995)],w=birth_marriedpreconc$wtuse[(birth_marriedpreconc$survey!=1995)],na.rm = TRUE)
weight_means[13,2]<-weighted.mean(misc_marriedpreconc$liveboth14[(misc_marriedpreconc$survey!=1995)],w=misc_marriedpreconc$wtuse[(misc_marriedpreconc$survey!=1995)],na.rm = TRUE)
weight_means[13,3]<-weighted.mean(birth_notmarpreconc$liveboth14[(birth_notmarpreconc$survey!=1995)], w=birth_notmarpreconc$wtuse[(birth_notmarpreconc$survey!=1995)],na.rm = TRUE)
weight_means[13,4]<-weighted.mean(misc_notmarpreconc$liveboth14[(misc_notmarpreconc$survey!=1995)],w=misc_notmarpreconc$wtuse[(misc_notmarpreconc$survey!=1995)],na.rm = TRUE)

weight_se[13,1]<-sqrt(wtd.var(birth_marriedpreconc$liveboth14[(birth_marriedpreconc$survey!=1995)],w=birth_marriedpreconc$wtuse[(birth_marriedpreconc$survey!=1995)]))
weight_se[13,2]<-sqrt(wtd.var(misc_marriedpreconc$liveboth14[(misc_marriedpreconc$survey!=1995)],w=misc_marriedpreconc$wtuse[(misc_marriedpreconc$survey!=1995)]))
weight_se[13,3]<-sqrt(wtd.var(birth_notmarpreconc$liveboth14[(birth_notmarpreconc$survey!=1995)], w=birth_notmarpreconc$wtuse[(birth_notmarpreconc$survey!=1995)]))
weight_se[13,4]<-sqrt(wtd.var(misc_notmarpreconc$liveboth14[(misc_notmarpreconc$survey!=1995)],w=misc_notmarpreconc$wtuse[(misc_notmarpreconc$survey!=1995)]))

#Recalculate for momeduc to exclude missing values (conditioning on hasmomeduc=1)
weight_means[10,1]<-weighted.mean(birth_marriedpreconc$momeduc[(birth_marriedpreconc$hasmomeduc==1)],w=birth_marriedpreconc$wtuse[(birth_marriedpreconc$hasmomeduc==1)],na.rm = TRUE)
weight_means[10,2]<-weighted.mean(misc_marriedpreconc$momeduc[(misc_marriedpreconc$hasmomeduc==1)],w=misc_marriedpreconc$wtuse[(misc_marriedpreconc$hasmomeduc==1)],na.rm = TRUE)
weight_means[10,3]<-weighted.mean(birth_notmarpreconc$momeduc[(birth_notmarpreconc$hasmomeduc==1)],w=birth_notmarpreconc$wtuse[(birth_notmarpreconc$hasmomeduc==1)],na.rm = TRUE)
weight_means[10,4]<-weighted.mean(misc_notmarpreconc$momeduc[(misc_notmarpreconc$hasmomeduc==1)],w=misc_notmarpreconc$wtuse[(misc_notmarpreconc$hasmomeduc==1)],na.rm = TRUE)

weight_se[10,1]<-sqrt(wtd.var(birth_marriedpreconc$momeduc[(birth_marriedpreconc$hasmomeduc==1)],w=birth_marriedpreconc$wtuse[(birth_marriedpreconc$hasmomeduc==1)]))
weight_se[10,2]<-sqrt(wtd.var(misc_marriedpreconc$momeduc[(misc_marriedpreconc$hasmomeduc==1)],w=misc_marriedpreconc$wtuse[(misc_marriedpreconc$hasmomeduc==1)]))
weight_se[10,3]<-sqrt(wtd.var(birth_notmarpreconc$momeduc[(birth_notmarpreconc$hasmomeduc==1)],w=birth_notmarpreconc$wtuse[(birth_notmarpreconc$hasmomeduc==1)]))
weight_se[10,4]<-sqrt(wtd.var(misc_notmarpreconc$momeduc[(misc_notmarpreconc$hasmomeduc==1)],w=misc_notmarpreconc$wtuse[(misc_notmarpreconc$hasmomeduc==1)]))

#Recalculate for working mother to exclude missing values (conditioning on hasworkingmother=1)
weight_means[8,1]<-weighted.mean(birth_marriedpreconc$workingmother[(birth_marriedpreconc$hasworkingmother==1)],w=birth_marriedpreconc$wtuse[(birth_marriedpreconc$hasworkingmother==1)],na.rm = TRUE)
weight_means[8,2]<-weighted.mean(misc_marriedpreconc$workingmother[(misc_marriedpreconc$hasworkingmother==1)],w=misc_marriedpreconc$wtuse[(misc_marriedpreconc$hasworkingmother==1)],na.rm = TRUE)
weight_means[8,3]<-weighted.mean(birth_notmarpreconc$workingmother[(birth_notmarpreconc$hasworkingmother==1)],w=birth_notmarpreconc$wtuse[(birth_notmarpreconc$hasworkingmother==1)],na.rm = TRUE)
weight_means[8,4]<-weighted.mean(misc_notmarpreconc$workingmother[(misc_notmarpreconc$hasworkingmother==1)],w=misc_notmarpreconc$wtuse[(misc_notmarpreconc$hasworkingmother==1)],na.rm = TRUE)

weight_se[8,1]<-sqrt(wtd.var(birth_marriedpreconc$workingmother[(birth_marriedpreconc$hasworkingmother==1)],w=birth_marriedpreconc$wtuse[(birth_marriedpreconc$hasworkingmother==1)]))
weight_se[8,2]<-sqrt(wtd.var(misc_marriedpreconc$workingmother[(misc_marriedpreconc$hasworkingmother==1)],w=misc_marriedpreconc$wtuse[(misc_marriedpreconc$hasworkingmother==1)]))
weight_se[8,3]<-sqrt(wtd.var(birth_notmarpreconc$workingmother[(birth_notmarpreconc$hasworkingmother==1)],w=birth_notmarpreconc$wtuse[(birth_notmarpreconc$hasworkingmother==1)]))
weight_se[8,4]<-sqrt(wtd.var(misc_notmarpreconc$workingmother[(misc_notmarpreconc$hasworkingmother==1)],w=misc_notmarpreconc$wtuse[(misc_notmarpreconc$hasworkingmother==1)]))

#Observations
obs<-c(nrow(birth_marriedpreconc),nrow(misc_marriedpreconc),nrow(birth_notmarpreconc),nrow(misc_notmarpreconc))

t1_means<-weight_means[c("agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                        "momeduc", "liveboth14", "educ", "educatl12", "agefirstmarriage", "infirstmarr", "marrafterfirst", 
                       "nevermarried", "numchildren", "working", "familyincome"),1:4]
colnames(t1_means)<-c("Birth","Miscarriage","Birth","Miscarriage")
t1_se<-weight_se[c("agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                   "momeduc", "liveboth14", "educ", "educatl12", "agefirstmarriage", "infirstmarr", "marrafterfirst", 
                   "nevermarried", "numchildren", "working", "familyincome"),1:4]
#########################################
#Table 2
#########################################

#Create matrix of variables needed for second table
data_2A<-data[c("firstpregbirthdk","agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                "momeduc",  "liveboth14", "year1concept", "marrpreconceptdateUSE", 
              "survey1973", "survey1976", "survey1982", "survey1988", "hasworkingmother","hasmomeduc","havemarrpreconceptdate","wtuse" )]

#Filter out abortions and non-pregnancies\
data_2<-data_2A[(data$abort==0 & (data$pregnum==1|data$survey==1973) & is.na(data$teenpregearlypd) & !is.na(data$firstpregbirthdk)),]

#Matrices to store coefficients and se
coef_2<-matrix(0,20,3)
robse_2<-matrix(0,20,3)

#OLS Regression
ols<-lm(firstpregbirthdk ~ agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + year1concept + marrpreconceptdateUSE + 
   survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc + havemarrpreconceptdate, data=data_2,weights=data_2$wtuse)
coef_2[,1]<-ols$coefficients
rownames(coef_2)<-c("Intercept",colnames(data_2[,2:20]))

#Robust SEs
library(sandwich)
library(lmtest)
robse_2[,1]<-diag(vcovHC(ols, type = "HC"))^0.5
rownames(robse_2)<-c("Intercept",colnames(data_2[,2:20]))

#Probit Regression - Note: the paper uses "marginal effects," but unclear how to replicate in R
probit<-as.formula(firstpregbirthdk ~ agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + year1concept + marrpreconceptdateUSE + 
                     survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc + havemarrpreconceptdate)

p <- glm(probit, data = data_2, family = binomial(link="probit"), weights=data_2$wtuse) 
coef_2[,2]<-p$coefficients
robse_2[,2]<-diag(vcovHC(p, type = "HC"))^0.5

#Logit Regression - Note: the paper uses "marginal effects," but unclear how to replicate in R
logit<-as.formula(firstpregbirthdk ~ agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + year1concept + marrpreconceptdateUSE + 
          survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc + havemarrpreconceptdate)

l <- glm(logit, data = data_2, family = binomial(link="logit"), weights=data_2$wtuse) 
coef_2[,3]<-l$coefficients
robse_2[,3]<-diag(vcovHC(l, type = "HC"))^0.5

t2_means<-coef_2[c("agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                         "momeduc", "liveboth14","marrpreconceptdateUSE","year1concept"),1:3]
colnames(t2_means)<-c("OLS","Probit","Logit")
t2_se<-robse_2[c("agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                 "momeduc", "liveboth14","marrpreconceptdateUSE","year1concept"),1:3]

#########################################
#Table 3
#########################################
c<-matrix(0,27105,1)

data_3A<-data[c("educ", "educatl12", "agefirstmarriage",  "infirstmarr", "marrafterfirst", "nevermarried", "numchildren", "working", "lnfamincome", "increlpoverty",
                "AGE","agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                "momeduc",  "liveboth14", "firstpregbirthdk", "birthmarrpreconceptdate", "marrpreconceptdateUSE", 
                "survey1973", "survey1976", "survey1982", "survey1988", "hasworkingmother","hasmomeduc","wtuse")]
data_3<-data.frame(data_3A[(data$abort==0 & (data$pregnum==1|data$survey==1973) & is.na(data$teenpregearlypd) & !is.na(data$firstpregbirthdk)),])

#Regress first ten variables separately on birth, birth x marrpreconcept, marrpreconceptdateUSE, and control variables
coef_3A<-sapply(1:10,function(x) coef(lm(data_3[,x]~ firstpregbirthdk + birthmarrpreconceptdate + marrpreconceptdateUSE + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                 survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_3, weights=data_3$wtuse)))

#Extract coefs for birth and birth x marrpreconcept, create row summing these two, and add headers
birthplusbirthmarrpreconcept<-coef_3A[2,]+coef_3A[3,]
coef_3<-rbind(coef_3A[2:3,], birthplusbirthmarrpreconcept)
colnames(coef_3)<-c("educ", "educatl12", "agefirstmarriage",  "infirstmarr", "marrafterfirst", "nevermarried", "numchildren", "working", "lnfamincome", "increlpoverty")

#Extract number of obs
n_obs_3<-sapply(1:10,function(x) nobs(lm(data_3[,x]~ firstpregbirthdk + birthmarrpreconceptdate + marrpreconceptdateUSE + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_3, weights=data_3$wtuse)))


#Extract r squared
r_sq_3<-sapply(1:10,function(x) summary(lm(data_3[,x]~ firstpregbirthdk + birthmarrpreconceptdate + marrpreconceptdateUSE + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +  
                                           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_3, weights=data_3$wtuse))$r.squared)

#Mean dependent variable
mean_3<-apply(data_3[,1:10],2,weighted.mean,w=data_3$wtuse,na.rm=TRUE)

merge_3<-rbind(coef_3,n_obs_3,r_sq_3,mean_3)

#Extract robust SE
robse_3A<-sapply(1:10,function(x) diag(vcovHC(lm(data_3[,x]~ firstpregbirthdk + birthmarrpreconceptdate + marrpreconceptdateUSE + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_3, weights=data_3$wtuse),type="HC"))^.5)
robse_3<-matrix(0,6,10)
robse_3[1:2,]<-robse_3A[2:3,]

#Note - in paper, significance stars are based on normal standard errors, despite the fact that robust standard errors are shown in the table. Need to override significance stars to use robust standard errors.

#########################################
#Table 4
#########################################

data_4A<-data[c("educ", "educatl12", "agefirstmarriage",  "infirstmarr", "marrafterfirst", "nevermarried", "numchildren", "working", "lnfamincome", "increlpoverty",
                "AGE","agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                "momeduc",  "liveboth14", "firstpregbirthdk", "birthmarrpreconceptdate", "marrpreconceptdateUSE","birthprededmarrpreconceptdate", "birthdkeducpredict2resc", "prededmarrpreconceptdate", "havemarrpreconceptdate",
                "survey1973", "survey1976", "survey1982", "survey1988", "hasworkingmother","hasmomeduc","wtuse","educpredict2")]
data_4<-data.frame(data_4A[(data$abort==0 & (data$pregnum==1|data$survey==1973) & is.na(data$teenpregearlypd) & !is.na(data$firstpregbirthdk)),])

#Note: birthdkeducpredict2resc is predicted education using control factors then subtracting 12

#Regress first ten variables separately. WHY INCLUDE prededmarrpreconceptdate, marrpreconceptdateUSE, and havemarrpreconceptdate? NOT INCLUDED IN PAPER! Also some coeffs are very slightly off
coef_4<-sapply(1:10,function(x) coef(lm(data_4[,x]~ firstpregbirthdk + birthdkeducpredict2resc + birthprededmarrpreconceptdate + birthmarrpreconceptdate + prededmarrpreconceptdate + marrpreconceptdateUSE +
                                         havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_4, weights=data_4$wtuse)))

#Only need rows 2-5 (for firstpregbirthdk + birthdkeducpredict2resc + birthprededmarrpreconceptdate + birthmarrpreconceptdate)
t4_betas<-coef_4[2:5,]

#Note - use same nobvs as 3

#Bootstrap standard errors-note that these will be slightly different because paper only used 500 draws
#Create matrix to store betas for each interaction
birth_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas
birtheduc_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas
birthedmar_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas
birthmarpre_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas

#Loop to bootstrap SEs - in paper, used n=500 but increased size to get more reliable SEs
for (i in 1:5000){
  index<-sample(1:nrow(data_4),nrow(data_4),replace=TRUE) #create index
  boot_data<-data_4[index,] #pull in data
  coefs<-sapply(1:10,function(x) coef(lm(boot_data[,x]~ firstpregbirthdk + birthdkeducpredict2resc + birthprededmarrpreconceptdate + birthmarrpreconceptdate + prededmarrpreconceptdate + marrpreconceptdateUSE +
                                            havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                            survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=boot_data)))
   birth_boot[i,]<-coefs[2,]
   birtheduc_boot[i,]<-coefs[3,]
   birthedmar_boot[i,]<-coefs[4,]
   birthmarpre_boot[i,]<-coefs[5,]
}

#Calculate SEs as std of bootstrapped betas
t4_se<-matrix(0,4,10)
t4_se[1,]<-as.matrix(apply(birth_boot,2,sd))
t4_se[2,]<-as.matrix(apply(birtheduc_boot,2,sd))
t4_se[3,]<-as.matrix(apply(birthedmar_boot,2,sd))
t4_se[4,]<-as.matrix(apply(birthmarpre_boot,2,sd))

#########################################
#Table 4.5 - Make separate table for effects of predicted education to clarify difference from above findings
#########################################
coef_preded<-matrix(0,6,10)
se_preded<-matrix(0,6,10)

#Slot in betas/SEs from first table, where coeffs on birth are for 12 years education, married
coef_preded[2,]<-t4_betas[1,]
se_preded[2,]<-t4_se[1,]

#10 years not married############################
educpredict_adj<-data_4$educpredict2-10 #Subtract 10 years to center instead of 12
birthdkeducpredict_adj = data_4$firstpregbirthdk*educpredict_adj #Create variables as in Sample do file
birthprededmarrpreconceptdate_adj = birthdkeducpredict_adj*data_4$marrpreconceptdateUSE
prededmarrpreconceptdate_adj = educpredict_adj*data_4$marrpreconceptdateUSE
data_preded<-cbind(data_4,educpredict_adj,birthdkeducpredict_adj,birthprededmarrpreconceptdate_adj,prededmarrpreconceptdate_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate + prededmarrpreconceptdate_adj + marrpreconceptdateUSE +
                                           havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +
                                           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse)))
coef_preded[1,]<-coefs[2,] #Store coefficient on educ in first row of coef_preded

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate + prededmarrpreconceptdate_adj + marrpreconceptdateUSE +
                                         havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse),type="HC"))^.5)
se_preded[1,]<-se[2,]

#14 years not married############################
educpredict_adj<-data_4$educpredict2-14 #Subtract 14 years instead of 12
birthdkeducpredict_adj = data_4$firstpregbirthdk*educpredict_adj 
birthprededmarrpreconceptdate_adj = birthdkeducpredict_adj*data_4$marrpreconceptdateUSE
prededmarrpreconceptdate_adj = educpredict_adj*data_4$marrpreconceptdateUSE
data_preded<-cbind(data_4,educpredict_adj,birthdkeducpredict_adj,birthprededmarrpreconceptdate_adj,prededmarrpreconceptdate_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate + prededmarrpreconceptdate_adj + marrpreconceptdateUSE +
                                         havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse)))
coef_preded[3,]<-coefs[2,]

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate + prededmarrpreconceptdate_adj + marrpreconceptdateUSE +
                                             havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse),type="HC"))^.5)
se_preded[3,]<-se[2,]

#10 years married############################
educpredict_adj<-data_4$educpredict2-10
birthdkeducpredict_adj <- data_4$firstpregbirthdk*educpredict_adj
birthprededmarrpreconceptdate_adj <- birthdkeducpredict_adj*(1-data_4$marrpreconceptdateUSE)
prededmarrpreconceptdate_adj <- educpredict_adj*(1-data_4$marrpreconceptdateUSE)
birthmarrpreconceptdate_adj <- data_4$firstpregbirthdk*(1-data_4$marrpreconceptdateUSE)#Need to add additional adjusted variables because not married
marrpreconceptdateUSE_adj <- (1-data_4$marrpreconceptdateUSE)
data_preded<-cbind(data_4,educpredict_adj,birthdkeducpredict_adj,birthprededmarrpreconceptdate_adj,prededmarrpreconceptdate_adj,marrpreconceptdateUSE_adj )

coefs<-sapply(1:10,function(x) coef(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate_adj + prededmarrpreconceptdate_adj + marrpreconceptdateUSE_adj +
                                         havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse)))
coef_preded[4,]<-coefs[2,]
#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate_adj + prededmarrpreconceptdate_adj + marrpreconceptdateUSE_adj +
                                             havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse),type="HC"))^.5)
se_preded[4,]<-se[2,]

#12 years married############################
educpredict_adj<-data_4$educpredict2-12
birthdkeducpredict_adj <- data_4$firstpregbirthdk*educpredict_adj
birthprededmarrpreconceptdate_adj <- birthdkeducpredict_adj*(1-data_4$marrpreconceptdateUSE)
prededmarrpreconceptdate_adj <- educpredict_adj*(1-data_4$marrpreconceptdateUSE)
data_preded<-cbind(data_4,educpredict_adj,birthdkeducpredict_adj,birthprededmarrpreconceptdate_adj,prededmarrpreconceptdate_adj,marrpreconceptdateUSE_adj )

coefs<-sapply(1:10,function(x) coef(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate_adj + prededmarrpreconceptdate_adj + marrpreconceptdateUSE_adj +
                                          havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +
                                          survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse)))
coef_preded[5,]<-coefs[2,]
#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate_adj + prededmarrpreconceptdate_adj + marrpreconceptdateUSE_adj +
                                             havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 +
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse),type="HC"))^.5)
se_preded[5,]<-se[2,]

#14 years married############################
educpredict_adj<-data_4$educpredict2-14
birthdkeducpredict_adj <- data_4$firstpregbirthdk*educpredict_adj
birthprededmarrpreconceptdate_adj <- birthdkeducpredict_adj*(1-data_4$marrpreconceptdateUSE)
prededmarrpreconceptdate_adj <- educpredict_adj*(1-data_4$marrpreconceptdateUSE)
data_preded<-cbind(data_4,educpredict_adj,birthdkeducpredict_adj,birthprededmarrpreconceptdate_adj,prededmarrpreconceptdate_adj,marrpreconceptdateUSE_adj )

coefs<-sapply(1:10,function(x) coef(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate_adj + prededmarrpreconceptdate_adj + marrpreconceptdateUSE_adj +
                                         havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse)))
coef_preded[6,]<-coefs[2,]
#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_preded[,x]~ firstpregbirthdk + birthdkeducpredict_adj + birthprededmarrpreconceptdate_adj + birthmarrpreconceptdate_adj + prededmarrpreconceptdate_adj + marrpreconceptdateUSE_adj +
                                             havemarrpreconceptdate + AGE + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_preded, weights=data_preded$wtuse),type="HC"))^.5)
se_preded[6,]<-se[2,]

#########################################
#Table 5
#########################################
data_5A<-data[c("educ", "educatl12", "agefirstmarriage",  "infirstmarr", "marrafterfirst", "nevermarried", "numchildren", "working", "lnfamincome", "increlpoverty",
                "AGE","agefirstconceptionUSE", "agelt15", "hispanic", "black", "white","protestant", "catholic", "workingmother", 
                "momeduc",  "liveboth14", "firstpregbirthdk", "marrpreconceptdateUSE",  "havemarrpreconceptdate", "birthdate1960",
                "birthdate1960marrpreconcept", "birthmarrpreconceptdate", "date1960marrpreconcept", "date1concept",
                "survey1973", "survey1976", "survey1982", "survey1988", "hasworkingmother","hasmomeduc","wtuse","educpredict2")]
data_5<-data.frame(data_5A[(data$abort==0 & (data$pregnum==1|data$survey==1973) & is.na(data$teenpregearlypd) & !is.na(data$firstpregbirthdk)),])

#Note: birthdkeducpredict2resc is predicted education using control factors then subtracting 12

#Regress first ten variables separately. WHY INCLUDE prededmarrpreconceptdate, marrpreconceptdateUSE, and havemarrpreconceptdate? NOT INCLUDED IN PAPER! Why exclude age?
coef_5<-sapply(1:10,function(x) coef(lm(data_5[,x]~ firstpregbirthdk + birthdate1960 + birthdate1960marrpreconcept + birthmarrpreconceptdate + date1960marrpreconcept + date1concept + marrpreconceptdateUSE +
                                          havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                          survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_5, weights=data_5$wtuse)))

#Only need rows 2-5 (for firstpregbirthdk + birthdate1960 + birthdate1960marrpreconcept + birthmarrpreconceptdate)
t5_betas<-coef_5[2:5,]

#Note - use same nobvs as 3

#Bootstrap standard errors-note that these will be slightly different because paper only used 500 draws
#Create matrix to store betas for each interaction
birth_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas
birth1960_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas
birth1960mar_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas
birthmarpre_boot<-matrix(0,nrow=500,ncol=10)#matrix to store betas

#Loop to bootstrap SEs
for (i in 1:500){
  index<-sample(1:nrow(data_4),nrow(data_4),replace=TRUE) #create index
  boot_data<-data_5[index,] #pull in data
  coefs<-sapply(1:10,function(x) coef(lm(boot_data[,x]~ firstpregbirthdk + birthdate1960 + birthdate1960marrpreconcept + birthmarrpreconceptdate + date1960marrpreconcept + date1concept + marrpreconceptdateUSE +
                                           havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=boot_data)))
  birth_boot[i,]<-coefs[2,]
  birth1960_boot[i,]<-coefs[3,]
  birth1960mar_boot[i,]<-coefs[4,]
  birthmarpre_boot[i,]<-coefs[5,]
}

#Calculate SEs as std of bootstrapped betas
t5_se<-matrix(0,4,10)
t5_se[1,]<-as.matrix(apply(birth_boot,2,sd))
t5_se[2,]<-as.matrix(apply(birth1960_boot,2,sd))
t5_se[3,]<-as.matrix(apply(birth1960mar_boot,2,sd))
t5_se[4,]<-as.matrix(apply(birthmarpre_boot,2,sd))

#########################################
#Table 5.5 - Make separate table for effects of predicted education to clarify difference from above
#########################################
coef_year<-matrix(0,6,10)
se_year<-matrix(0,6,10)

#Slot in betas/SEs from first table, where coeffs on birth are for 1960, married
coef_year[2,]<-t5_betas[1,]
se_year[2,]<-t5_se[1,]

#1952not married############################
birthdate1960_adj = data_5$firstpregbirthdk*(data_5$date1concept-1952)
birthdate1960marrpreconcept_adj = birthdate1960_adj*data_5$marrpreconceptdateUSE
date1960marrpreconcept_adj = (data_5$date1concept-1952)*data_5$marrpreconceptdateUSE
data_birth<-cbind(data_5,birthdate1960_adj,birthdate1960marrpreconcept_adj,date1960marrpreconcept_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE +
                                         havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse)))
coef_year[1,]<-coefs[2,] #Store coefficient on educ in first row of coef_year

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE +
                                             havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse),type="HC"))^.5)
se_year[1,]<-se[2,]

#1968not married############################
birthdate1960_adj = data_5$firstpregbirthdk*(data_5$date1concept-1968)
birthdate1960marrpreconcept_adj = birthdate1960_adj*data_5$marrpreconceptdateUSE
date1960marrpreconcept_adj = (data_5$date1concept-1968)*data_5$marrpreconceptdateUSE
data_birth<-cbind(data_5,birthdate1960_adj,birthdate1960marrpreconcept_adj,date1960marrpreconcept_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE +
                                         havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse)))
coef_year[3,]<-coefs[2,]

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE +
                                             havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse),type="HC"))^.5)
se_year[3,]<-se[2,]

#1952 married############################
birthdate1960_adj = data_5$firstpregbirthdk*(data_5$date1concept-1952)
birthdate1960marrpreconcept_adj = birthdate1960_adj*(1-data_5$marrpreconceptdateUSE)
date1960marrpreconcept_adj = (data_5$date1concept-1952)*(1-data_5$marrpreconceptdateUSE)
data_birth<-cbind(data_5,birthdate1960_adj,birthdate1960marrpreconcept_adj,date1960marrpreconcept_adj,prededmarrpreconceptdate_adj,marrpreconceptdateUSE_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate_adj + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE_adj +
                                         havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse)))
coef_year[4,]<-coefs[2,] #Store coefficient on educ in first row of coef_year

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate_adj + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE_adj +
                                             havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse),type="HC"))^.5)
se_year[4,]<-se[2,]

#1962 married############################
birthdate1960_adj = data_5$firstpregbirthdk*(data_5$date1concept-1960)
birthdate1960marrpreconcept_adj = birthdate1960_adj*(1-data_5$marrpreconceptdateUSE)
date1960marrpreconcept_adj = (data_5$date1concept-1960)*(1-data_5$marrpreconceptdateUSE)
data_birth<-cbind(data_5,birthdate1960_adj,birthdate1960marrpreconcept_adj,date1960marrpreconcept_adj,prededmarrpreconceptdate_adj,marrpreconceptdateUSE_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate_adj + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE_adj +
                                         havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse)))
coef_year[5,]<-coefs[2,] #Store coefficient on educ in first row of coef_year

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate_adj + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE_adj +
                                             havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse),type="HC"))^.5)
se_year[5,]<-se[2,]

#1968 married############################
birthdate1960_adj = data_5$firstpregbirthdk*(data_5$date1concept-1968)
birthdate1960marrpreconcept_adj = birthdate1960_adj*(1-data_5$marrpreconceptdateUSE)
date1960marrpreconcept_adj = (data_5$date1concept-1968)*(1-data_5$marrpreconceptdateUSE)
data_birth<-cbind(data_5,birthdate1960_adj,birthdate1960marrpreconcept_adj,date1960marrpreconcept_adj,prededmarrpreconceptdate_adj,marrpreconceptdateUSE_adj)

coefs<-sapply(1:10,function(x) coef(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate_adj + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE_adj +
                                         havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                         survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse)))
coef_year[6,]<-coefs[2,] #Store coefficient on educ in first row of coef_year

#SEs
se<-sapply(1:10,function(x) diag(vcovHC(lm(data_birth[,x]~ firstpregbirthdk + birthdate1960_adj + birthdate1960marrpreconcept_adj + birthmarrpreconceptdate_adj + date1960marrpreconcept_adj + date1concept + marrpreconceptdateUSE_adj +
                                             havemarrpreconceptdate + agefirstconceptionUSE + agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
                                             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_birth, weights=data_birth$wtuse),type="HC"))^.5)
se_year[6,]<-se[2,]

#########################################
#Table 6
#########################################
#Create matrix of variables needed for second table 
data_2<-data[c("educ", "shotgunnew", "date1concept","agefirstconceptionUSE", "agelt15", "hispanic", "black", 
               "white","protestant", "catholic", "workingmother", "momeduc",  "liveboth14", "year1concept",
               "marrpreconceptdateUSE", "survey1973", "survey1976", "survey1982", "survey1988", 
               "hasworkingmother","hasmomeduc","havemarrpreconceptdate","wtuse", "pregnum", "educatl12", 
               "agefirstmarriage", "infirstmarr", "marrafterfirst", "nevermarried", "numchildren", 
               "working", "lnfamincome", "increlpoverty")]


data_6<-data_2[(data$abort!=1 & (((!is.na(data$pregnum)) & data$pregnum==1)|data$survey==1973)) &  is.na(data$teenpregearlypd) & (!is.na(data$firstpregbirthdk) & data$firstpregbirthdk==1) & data$marrpreconceptdateUSE==0 & data$havemarrpreconceptdate==1,]
nrow(data_6)

LHS_var<-data[c("educ", "educatl12", "agefirstmarriage", "infirstmarr", "marrafterfirst", "nevermarried", 
                "numchildren", "working", "lnfamincome", "increlpoverty")]
LHS_var2<-LHS_var[(data$abort!=1 & (((!is.na(data$pregnum)) & data$pregnum==1)|data$survey==1973)) &  is.na(data$teenpregearlypd) & (!is.na(data$firstpregbirthdk) & data$firstpregbirthdk==1) & data$marrpreconceptdateUSE==0 & data$havemarrpreconceptdate==1,]
nrow(LHS_var2)


for (var in LHS_var2){
  ols2<-lm(var ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
             survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
  print(ols2)
  summary(ols2)
}  

#Matrices to store coefficients and se
coef_61<-matrix(0,19,4)
robse_61<-matrix(0,19,4)

coef_62<-matrix(0,19,4)
robse_62<-matrix(0,19,4)

coef_63<-matrix(0,19,4)
robse_63<-matrix(0,19,4)

coef_64<-matrix(0,19,4)
robse_64<-matrix(0,19,4)

coef_65<-matrix(0,19,4)
robse_65<-matrix(0,19,4)

coef_66<-matrix(0,19,4)
robse_66<-matrix(0,19,4)

coef_67<-matrix(0,19,4)
robse_67<-matrix(0,19,4)

coef_68<-matrix(0,19,4)
robse_68<-matrix(0,19,4)

coef_69<-matrix(0,19,4)
robse_69<-matrix(0,19,4)

#Education
ols1<-lm(educ ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_61[,1]<-ols1$coefficients
coef_61
summary(ols1)

robse_61[,1]<-diag(vcovHC(ols1, type = "HC"))^0.5
robse_61

#Education older than 12
ols2<-lm(educatl12 ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
summary(ols2)
coef_62[,1]<-ols2$coefficients
coef_62

robse_62[,1]<-diag(vcovHC(ols2, type = "HC"))^0.5
robse_62


coefs<-matrix(0, 19, 2)
coefs[,1]<-coef_61[,1]
coefs[,2]<-coef_62[,1]
coefs

robse<-matrix(0, 19, 2)
robse[,1]<-robse_61[,1]
robse[,2]<-robse_62[,1]
robse

#Age at first marriage
ols3<-lm(agefirstmarriage ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_63[,1]<-ols3$coefficients
coef_63

robse_63[,1]<-diag(vcovHC(ols3, type = "HC"))^0.5
robse_63

#In first marriage
ols4<-lm(infirstmarr ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_64[,1]<-ols4$coefficients
coef_64

robse_64[,1]<-diag(vcovHC(ols4, type = "HC"))^0.5
robse_64

#Married given first marriage over 
ols5<-lm(marrafterfirst ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_65[,1]<-ols5$coefficients
coef_65

robse_65[,1]<-diag(vcovHC(ols5, type = "HC"))^0.5
robse_65

#Number live births
ols6<-lm(numchildren ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_66[,1]<-ols6$coefficients
coef_66

robse_66[,1]<-diag(vcovHC(ols6, type = "HC"))^0.5
robse_66

#Working
ols7<-lm(working ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_67[,1]<-ols7$coefficients
coef_67

robse_67[,1]<-diag(vcovHC(ols7, type = "HC"))^0.5
robse_67

#ln(Family income)
ols8<-lm(lnfamincome ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_68[,1]<-ols8$coefficients
coef_68

robse_68[,1]<-diag(vcovHC(ols8, type = "HC"))^0.5
robse_68

#Percent poverty level
ols9<-lm(increlpoverty ~ shotgunnew + date1concept + agefirstconceptionUSE +agelt15 + hispanic + black + white + protestant + catholic + workingmother + momeduc + liveboth14 + 
           survey1973 + survey1976 + survey1982 + survey1988 + hasworkingmother + hasmomeduc, data=data_6,weights=data_6$wtuse)
coef_69[,1]<-ols9$coefficients
coef_69

robse_69[,1]<-diag(vcovHC(ols9, type = "HC"))^0.5
robse_69
 
coef_6<-t(cbind(coef_61[2,1], coef_62[2,1], coef_63[2,1], coef_64[2,1], coef_65[2,1], coef_66[2,1], coef_67[2,1], coef_68[2,1], coef_69[2,1]))
robse_6<-t(cbind(robse_61[2,1], robse_62[2,1], robse_63[2,1], robse_64[2,1], robse_65[2,1], robse_66[2,1], robse_67[2,1], robse_68[2,1], robse_69[2,1]))