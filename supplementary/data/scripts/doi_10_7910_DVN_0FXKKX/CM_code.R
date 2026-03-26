require(foreign)
setwd("E:/My Documents/Research/Paper Aid/R")
getwd()
list.files()

apsr_cm <- read.dta(file="apsr_cm.dta", convert.dates=TRUE, missing.type=FALSE, convert.factors=FALSE)
is.data.frame(apsr_cm)
summary(apsr_cm)

#RECODE NON-RESPONSE TO MISSING
apsr_cm$q1[apsr_cm$q1==-1] <-NA
apsr_cm$q2[apsr_cm$q2==-1] <-NA
apsr_cm$q3[apsr_cm$q3==-1] <-NA
apsr_cm$q4[apsr_cm$q4==-1] <-NA
apsr_cm$q5[apsr_cm$q5==-1] <-NA
apsr_cm$q7[apsr_cm$q7==-1] <-NA
apsr_cm$q8[apsr_cm$q8==-1] <-NA
apsr_cm$q9[apsr_cm$q9==-1] <-NA
apsr_cm$q10[apsr_cm$q10==-1] <-NA
apsr_cm$q11[apsr_cm$q11==-1] <-NA
apsr_cm$q12[apsr_cm$q12==-1] <-NA

#DV
		#Recodes
apsr_cm$q1dv <- ifelse(apsr_cm$q1==1, 0, ifelse(apsr_cm$q1==2, 10, ifelse(apsr_cm$q1==3,29.5, ifelse(apsr_cm$q1==4,40, ifelse(apsr_cm$q1==5,50, ifelse(apsr_cm$q1==6,69.5, ifelse(apsr_cm$q1==7,90, ifelse(apsr_cm$q1==NA,NA))))))))
apsr_cm$q2dv <- ifelse(apsr_cm$q2==1, 1, ifelse(apsr_cm$q2==2, 2, ifelse(apsr_cm$q2==3,3, ifelse(apsr_cm$q2==4,4, ifelse(apsr_cm$q2==5,5,ifelse(apsr_cm$q2==6,6,ifelse(apsr_cm$q2==7,7, ifelse(apsr_cm$q2==NA,NA))))))))
apsr_cm$q9dv <- ifelse(apsr_cm$q9==1, 5, ifelse(apsr_cm$q9==2, 4, ifelse(apsr_cm$q9==3,3, ifelse(apsr_cm$q9==4,2, ifelse(apsr_cm$q9==5,1,ifelse(apsr_cm$q9==NA,NA))))))
		#Get PCA scores
myvars <- c("q1dv", "q2dv", "q9dv")
pca_dv <- apsr_cm[myvars]
#pca_dv <- na.omit(pca_dv_temp) 
summary(pca_dv)
library(psych)
pca1 <- principal(pca_dv, nfactors=1, rotate="none", covar=FALSE, scores=TRUE)
pca1
apsr_cm$dv_impute <- pca1$scores
		#Now fill in the missing scores (just 32 of them) based on the questions that were answered. Can't do in Amelia, it will crash b/c of perfect collinearity
apsr_cm$q1dvz <- scale(apsr_cm$q1dv, center=TRUE, scale=TRUE)
apsr_cm$q2dvz <- scale(apsr_cm$q2dv, center=TRUE, scale=TRUE)
apsr_cm$q9dvz <- scale(apsr_cm$q9dv, center=TRUE, scale=TRUE)
scores <- lm(dv_impute ~ q1dvz + q2dvz + q9dvz, data=apsr_cm)
apsr_cm$q1dvz[is.na(apsr_cm$q1dvz)] <- 0
apsr_cm$q2dvz[is.na(apsr_cm$q2dvz)] <- 0
apsr_cm$q9dvz[is.na(apsr_cm$q9dvz)] <- 0
myvarsz <- c("q1dvz", "q2dvz", "q9dvz")
apsr_cm$dv <- predict(scores,apsr_cm[myvarsz],data=apsr_cm)
apsr_cm$dv[is.na(apsr_cm$q1) & is.na(apsr_cm$q2) & is.na(apsr_cm$q9)] <- NA
		#Cronbach's alpha
alpha(pca_dv)
	#mean of q1dv (reported in text)
library(psych)
library(Hmisc)
wtd.mean(apsr_cm$q1dv, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)

#IVs
	#Agency Index
		#Recodes
apsr_cm$q3iv <- ifelse(apsr_cm$q3==1, 5, ifelse(apsr_cm$q3==2, 4, ifelse(apsr_cm$q3==3,3, ifelse(apsr_cm$q3==4,2, ifelse(apsr_cm$q3==5,1,ifelse(apsr_cm$q3==NA,NA))))))
apsr_cm$q4iv <- ifelse(apsr_cm$q4==1, 5, ifelse(apsr_cm$q4==2, 4, ifelse(apsr_cm$q4==3,3, ifelse(apsr_cm$q4==4,2, ifelse(apsr_cm$q4==5,1,ifelse(apsr_cm$q4==NA,NA))))))
apsr_cm$q5iv <- ifelse(apsr_cm$q5==1, 5, ifelse(apsr_cm$q5==2, 4, ifelse(apsr_cm$q5==3,3, ifelse(apsr_cm$q5==4,2, ifelse(apsr_cm$q5==5,1,ifelse(apsr_cm$q5==NA,NA))))))
		#Get PCA scores 
myvars <- c("q3iv", "q4iv", "q5iv")
pca_agency <- apsr_cm[myvars]
summary(pca_agency)
pca2 <- principal(pca_agency, nfactors=1, rotate="none", covar=FALSE, scores=TRUE)
pca2
apsr_cm$agency_impute <- pca2$scores
		#Now fill in the missing scores (just 20 of them) based on the questions that were answered. 
apsr_cm$q3z <- scale(apsr_cm$q3iv, center=TRUE, scale=TRUE)
apsr_cm$q4z <- scale(apsr_cm$q4iv, center=TRUE, scale=TRUE)
apsr_cm$q5z <- scale(apsr_cm$q5iv, center=TRUE, scale=TRUE)
scores2 <- lm(agency_impute ~ q3z + q4z + q5z, data=apsr_cm)
apsr_cm$q3z[is.na(apsr_cm$q3z)] <- 0
apsr_cm$q4z[is.na(apsr_cm$q4z)] <- 0
apsr_cm$q5z[is.na(apsr_cm$q5z)] <- 0
myvarsz2 <- c("q3z", "q4z", "q5z")
apsr_cm$agency <- predict(scores2,apsr_cm[myvarsz2],data=apsr_cm)
apsr_cm$agency[is.na(apsr_cm$q3iv) & is.na(apsr_cm$q4iv) & is.na(apsr_cm$q5iv)] <- NA
		#Cronbach's alpha
alpha(pca_agency)
		#frequencies of items in agency index
q3table <- wtd.table(apsr_cm$q3iv, weights=apsr_cm$weight2, type='table', normwt=TRUE, na.rm=TRUE)
prop.table(q3table)
q4table <- wtd.table(apsr_cm$q4iv, weights=apsr_cm$weight2, type='table', normwt=TRUE, na.rm=TRUE)
prop.table(q4table)
q5table <- wtd.table(apsr_cm$q5iv, weights=apsr_cm$weight2, type='table', normwt=TRUE, na.rm=TRUE)
prop.table(q5table)

	#Resentment Index
		#Recodes
apsr_cm$q7iv <- ifelse(apsr_cm$q7==1, 5, ifelse(apsr_cm$q7==2, 4, ifelse(apsr_cm$q7==3,3, ifelse(apsr_cm$q7==4,2, ifelse(apsr_cm$q7==5,1,ifelse(apsr_cm$q7==NA,NA))))))
		#Calculate index
apsr_cm$resentmentindex <- (apsr_cm$q7iv + apsr_cm$q8)/2 
		#Now fill in the missing scores based on the question that was answered. 
apsr_cm$resentmentindex[is.na(apsr_cm$q7iv)] <- apsr_cm$q8[is.na(apsr_cm$q7iv)]
apsr_cm$resentmentindex[is.na(apsr_cm$q8)] <- apsr_cm$q7iv[is.na(apsr_cm$q8)]
		#Make missing if dv==NA. Needed for mediation analysis below
apsr_cm$resentmentindex[is.na(apsr_cm$dv)]<- NA
		#correlation b/w agency and resentment (reported in text)
ar <- c("resentmentindex", "agency")
corrar <- apsr_cm[ar]
cor(corrar, use="complete.obs", method="pearson")

	#Living Standards Index
		#Recodes
apsr_cm$q11 <- ifelse(apsr_cm$q11==1, 0, ifelse(apsr_cm$q11==2, 10, ifelse(apsr_cm$q11==3,20, ifelse(apsr_cm$q11==4,30, ifelse(apsr_cm$q11==5,40, ifelse(apsr_cm$q11==6,50, ifelse(apsr_cm$q11==7,60, ifelse(apsr_cm$q11==8,70, ifelse(apsr_cm$q11==9,80, ifelse(apsr_cm$q11==10,90,ifelse(apsr_cm$q11==11,100, ifelse(apsr_cm$q11==NA,NA))))))))))))
apsr_cm$q12 <- ifelse(apsr_cm$q12==1, 0, ifelse(apsr_cm$q12==2, 10, ifelse(apsr_cm$q12==3,20, ifelse(apsr_cm$q12==4,30, ifelse(apsr_cm$q12==5,40, ifelse(apsr_cm$q12==6,50, ifelse(apsr_cm$q12==7,60, ifelse(apsr_cm$q12==8,70, ifelse(apsr_cm$q12==9,80, ifelse(apsr_cm$q12==10,90,ifelse(apsr_cm$q12==11,100, ifelse(apsr_cm$q12==NA,NA))))))))))))
		#Calculate index
apsr_cm$livingstandardsindex <- (apsr_cm$q11 + apsr_cm$q12)/2 
		#Now fill in the missing scores based on the question that was answered. 
apsr_cm$livingstandardsindex[is.na(apsr_cm$q11)] <- apsr_cm$q12[is.na(apsr_cm$q11)]
apsr_cm$livingstandardsindex[is.na(apsr_cm$q12)] <- apsr_cm$q11[is.na(apsr_cm$q12)]
		#Make missing if dv==NA. Needed for mediation analysis below
apsr_cm$livingstandardsindex[is.na(apsr_cm$dv)]<- NA

	#One more recode
apsr_cm$xparty7flip <- ifelse(apsr_cm$xparty7==1, 7, ifelse(apsr_cm$xparty7==2, 6, ifelse(apsr_cm$xparty7==3,5, ifelse(apsr_cm$xparty7==4,4, ifelse(apsr_cm$xparty7==5,3,ifelse(apsr_cm$xparty7==6,2,ifelse(apsr_cm$xparty7==7,1, ifelse(apsr_cm$xparty7==NA,NA))))))))

	#Create dummies for experimental groups
apsr_cm$blacktreatment   <- ifelse(apsr_cm$xtess072==1, 1, ifelse(apsr_cm$xtess072==2, 1, ifelse(apsr_cm$xtess072==3, 1, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$whitetreatment   <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 1, ifelse(apsr_cm$xtess072==5, 1, ifelse(apsr_cm$xtess072==6, 1, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$controlgroup     <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,1, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$paternalproframe <- ifelse(apsr_cm$xtess072==1, 1, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 1, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$paternalnegframe <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 1, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 1, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  

apsr_cm$black_pat_pos  <- ifelse(apsr_cm$xtess072==1, 1, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$black_pat_neg  <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 1, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$black_unspec   <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 1, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$white_pat_pos  <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 1, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$white_pat_neg  <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 1, ifelse(apsr_cm$xtess072==6, 0, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  
apsr_cm$white_unspec   <- ifelse(apsr_cm$xtess072==1, 0, ifelse(apsr_cm$xtess072==2, 0, ifelse(apsr_cm$xtess072==3, 0, ifelse(apsr_cm$xtess072==4, 0, ifelse(apsr_cm$xtess072==5, 0, ifelse(apsr_cm$xtess072==6, 1, ifelse(apsr_cm$xtess072==7,0, ifelse(apsr_cm$xtess072==NA,NA))))))))  

#ANALYSIS
#treatment effects for Table 1 
fit1 <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=apsr_cm, weights=weight2)
summary(fit1)
fit2 <- lm(dv ~  black_pat_pos + black_pat_neg + black_unspec + white_pat_pos + white_pat_neg + controlgroup, data=apsr_cm, weights=weight2)
summary(fit2)
#treatment effects for Figure 2
fit3 <- lm(dv ~  blacktreatment + whitetreatment + controlgroup + paternalproframe + paternalnegframe-1, data=apsr_cm, weights=weight2)
summary(fit3)
#get some things reported in text
	#p value of blacktreatment v control
fit4 <- lm(dv ~  blacktreatment + whitetreatment + paternalproframe + paternalnegframe, data=apsr_cm, weights=weight2)
summary(fit4)
	
#mediation analyses 
library("mediation")
	#agency index
med.fit <- lm(agency ~ blacktreatment + paternalproframe + paternalnegframe + controlgroup + xideo + xparty7flip + ppeduc, data=apsr_cm, weights=weight2)
summary(med.fit)
out.fit <- lm(dv ~ blacktreatment + agency + paternalproframe + paternalnegframe + controlgroup + xideo + xparty7flip + ppeduc, data=apsr_cm, weights=weight2)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "blacktreatment", mediator = "agency")
summary(med.out)
sens.out <- medsens(med.out, rho.by = .1, effect.type = "indirect")
summary(sens.out)
plot(sens.out, sens.par = "rho", main = "Perception of Foreign Poor's Agency", ylim = c(-0.2, 0.2))
	#living standards 
med.fit <- lm(livingstandardsindex ~ blacktreatment + paternalproframe + paternalnegframe  + controlgroup + xideo + xparty7flip + ppeduc, data=apsr_cm, weights=weight2)
summary(med.fit)
out.fit <- lm(dv ~ blacktreatment + livingstandardsindex + paternalproframe + paternalnegframe + controlgroup + xideo + xparty7flip + ppeduc, data=apsr_cm, weights=weight2)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "blacktreatment", mediator = "livingstandardsindex")
summary(med.out)
	#resentment
med.fit <- lm(resentmentindex ~ blacktreatment + paternalproframe + paternalnegframe + controlgroup + xideo + xparty7flip + ppeduc, data=apsr_cm, weights=weight2)
summary(med.fit)
out.fit <- lm(dv ~ blacktreatment + resentmentindex + paternalproframe + paternalnegframe + controlgroup + xideo + xparty7flip + ppeduc, data=apsr_cm, weights=weight2)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "blacktreatment", mediator = "resentmentindex")
summary(med.out)

#Appendix
	#Randomization check
describeBy(apsr_cm$xideo, group=apsr_cm$xtess072, mat=TRUE)
describeBy(apsr_cm$xparty7flip, group=apsr_cm$xtess072, mat=TRUE)
describeBy(apsr_cm$ppeduc, group=apsr_cm$xtess072, mat=TRUE)
	#Heterogenous Effects. Can't do a describeBY because of weights
		#Ideology
apsr_cm$xideoEL <- ifelse(apsr_cm$xideo==1, 1, ifelse(apsr_cm$xideo==2, 0, ifelse(apsr_cm$xideo==3,0, ifelse(apsr_cm$xideo==4,0, ifelse(apsr_cm$xideo==5,0, ifelse(apsr_cm$xideo==6,0, ifelse(apsr_cm$xideo==7,0, ifelse(apsr_cm$xideo==NA,NA))))))))
xideoEL <- apsr_cm[ which(apsr_cm$xideoEL==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=xideoEL, weights=weight2)
summary(fit)

apsr_cm$xideoL <- ifelse(apsr_cm$xideo==1, 0, ifelse(apsr_cm$xideo==2, 1, ifelse(apsr_cm$xideo==3,1, ifelse(apsr_cm$xideo==4,0, ifelse(apsr_cm$xideo==5,0, ifelse(apsr_cm$xideo==6,0, ifelse(apsr_cm$xideo==7,0, ifelse(apsr_cm$xideo==NA,NA))))))))
xideoL <- apsr_cm[ which(apsr_cm$xideoL==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=xideoL, weights=weight2)
summary(fit)


apsr_cm$xideoM <- ifelse(apsr_cm$xideo==1, 0, ifelse(apsr_cm$xideo==2, 0, ifelse(apsr_cm$xideo==3,0, ifelse(apsr_cm$xideo==4,1, ifelse(apsr_cm$xideo==5,0, ifelse(apsr_cm$xideo==6,0, ifelse(apsr_cm$xideo==7,0, ifelse(apsr_cm$xideo==NA,NA))))))))
xideoM <- apsr_cm[ which(apsr_cm$xideoM==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=xideoM, weights=weight2)
summary(fit)


apsr_cm$xideoC <- ifelse(apsr_cm$xideo==1, 0, ifelse(apsr_cm$xideo==2, 0, ifelse(apsr_cm$xideo==3,0, ifelse(apsr_cm$xideo==4,0, ifelse(apsr_cm$xideo==5,1, ifelse(apsr_cm$xideo==6,1, ifelse(apsr_cm$xideo==7,0, ifelse(apsr_cm$xideo==NA,NA))))))))
xideoC <- apsr_cm[ which(apsr_cm$xideoC==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=xideoC, weights=weight2)
summary(fit)


apsr_cm$xideoEC <- ifelse(apsr_cm$xideo==1, 0, ifelse(apsr_cm$xideo==2, 0, ifelse(apsr_cm$xideo==3,0, ifelse(apsr_cm$xideo==4,0, ifelse(apsr_cm$xideo==5,0, ifelse(apsr_cm$xideo==6,0, ifelse(apsr_cm$xideo==7,1, ifelse(apsr_cm$xideo==NA,NA))))))))
xideoEC <- apsr_cm[ which(apsr_cm$xideoEC==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=xideoEC, weights=weight2)
summary(fit)

		#Education
apsr_cm$ppeduc1 <- ifelse(apsr_cm$ppeduc==1, 1, ifelse(apsr_cm$ppeduc==2, 1, ifelse(apsr_cm$ppeduc==3,1, ifelse(apsr_cm$ppeduc==4,1, ifelse(apsr_cm$ppeduc==5,1, ifelse(apsr_cm$ppeduc==6,1, ifelse(apsr_cm$ppeduc==7,1, 
ifelse(apsr_cm$ppeduc==8, 1, ifelse(apsr_cm$ppeduc==9, 0, ifelse(apsr_cm$ppeduc==10,0, ifelse(apsr_cm$ppeduc==11,0, ifelse(apsr_cm$ppeduc==12,0, ifelse(apsr_cm$ppeduc==13,0, ifelse(apsr_cm$ppeduc==14,0, ifelse(apsr_cm$ppeduc==NA,NA)))))))))))))))
ppeduc1 <- apsr_cm[ which(apsr_cm$ppeduc1==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=ppeduc1, weights=weight2)
summary(fit)

apsr_cm$ppeduc2 <- ifelse(apsr_cm$ppeduc==1, 0, ifelse(apsr_cm$ppeduc==2, 0, ifelse(apsr_cm$ppeduc==3,0, ifelse(apsr_cm$ppeduc==4,0, ifelse(apsr_cm$ppeduc==5,0, ifelse(apsr_cm$ppeduc==6,0, ifelse(apsr_cm$ppeduc==7,0, 
ifelse(apsr_cm$ppeduc==8, 0, ifelse(apsr_cm$ppeduc==9, 1, ifelse(apsr_cm$ppeduc==10,0, ifelse(apsr_cm$ppeduc==11,0, ifelse(apsr_cm$ppeduc==12,0, ifelse(apsr_cm$ppeduc==13,0, ifelse(apsr_cm$ppeduc==14,0, ifelse(apsr_cm$ppeduc==NA,NA)))))))))))))))
ppeduc2 <- apsr_cm[ which(apsr_cm$ppeduc2==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=ppeduc2, weights=weight2)
summary(fit)

apsr_cm$ppeduc3 <- ifelse(apsr_cm$ppeduc==1, 0, ifelse(apsr_cm$ppeduc==2, 0, ifelse(apsr_cm$ppeduc==3,0, ifelse(apsr_cm$ppeduc==4,0, ifelse(apsr_cm$ppeduc==5,0, ifelse(apsr_cm$ppeduc==6,0, ifelse(apsr_cm$ppeduc==7,0, 
ifelse(apsr_cm$ppeduc==8, 0, ifelse(apsr_cm$ppeduc==9, 0, ifelse(apsr_cm$ppeduc==10,1, ifelse(apsr_cm$ppeduc==11,1, ifelse(apsr_cm$ppeduc==12,0, ifelse(apsr_cm$ppeduc==13,0, ifelse(apsr_cm$ppeduc==14,0, ifelse(apsr_cm$ppeduc==NA,NA)))))))))))))))
ppeduc3 <- apsr_cm[ which(apsr_cm$ppeduc3==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=ppeduc3, weights=weight2)
summary(fit)

apsr_cm$ppeduc4 <- ifelse(apsr_cm$ppeduc==1, 0, ifelse(apsr_cm$ppeduc==2, 0, ifelse(apsr_cm$ppeduc==3,0, ifelse(apsr_cm$ppeduc==4,0, ifelse(apsr_cm$ppeduc==5,0, ifelse(apsr_cm$ppeduc==6,0, ifelse(apsr_cm$ppeduc==7,0, 
ifelse(apsr_cm$ppeduc==8, 0, ifelse(apsr_cm$ppeduc==9, 0, ifelse(apsr_cm$ppeduc==10,0, ifelse(apsr_cm$ppeduc==11,0, ifelse(apsr_cm$ppeduc==12,1, ifelse(apsr_cm$ppeduc==13,0, ifelse(apsr_cm$ppeduc==14,0, ifelse(apsr_cm$ppeduc==NA,NA)))))))))))))))
ppeduc4 <- apsr_cm[ which(apsr_cm$ppeduc4==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=ppeduc4, weights=weight2)
summary(fit)

apsr_cm$ppeduc5 <- ifelse(apsr_cm$ppeduc==1, 0, ifelse(apsr_cm$ppeduc==2, 0, ifelse(apsr_cm$ppeduc==3,0, ifelse(apsr_cm$ppeduc==4,0, ifelse(apsr_cm$ppeduc==5,0, ifelse(apsr_cm$ppeduc==6,0, ifelse(apsr_cm$ppeduc==7,0, 
ifelse(apsr_cm$ppeduc==8, 0, ifelse(apsr_cm$ppeduc==9, 0, ifelse(apsr_cm$ppeduc==10,0, ifelse(apsr_cm$ppeduc==11,0, ifelse(apsr_cm$ppeduc==12,0, ifelse(apsr_cm$ppeduc==13,1, ifelse(apsr_cm$ppeduc==14,1, ifelse(apsr_cm$ppeduc==NA,NA)))))))))))))))
ppeduc5 <- apsr_cm[ which(apsr_cm$ppeduc5==1),]
fit <- lm(dv ~ blacktreatment + controlgroup + paternalproframe + paternalnegframe, data=ppeduc5, weights=weight2)
summary(fit)

	#Descriptive Stats
			#mins and maxes
summary(apsr_cm)
			#means and sd's, using weights
wtd.mean(apsr_cm$dv, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$dv, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q1dv, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q1dv, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q2, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q2, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q9, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q9, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$agency, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$agency, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q3, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q3, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q4, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q4, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q5, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q5, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$livingstandardsindex, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$livingstandardsindex, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q11, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q11, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q12, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q12, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$resentmentindex, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$resentmentindex, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q7, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q7, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$q8, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$q8, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$xideo, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$xideo, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$xparty7flip, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$xparty7flip, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_cm$ppeduc, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_cm$ppeduc, weights=apsr_cm$weight2, normwt=TRUE, na.rm=TRUE)
