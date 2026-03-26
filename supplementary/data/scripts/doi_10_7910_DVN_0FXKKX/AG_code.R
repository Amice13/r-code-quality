require(foreign)
setwd("E:/My Documents/Research/Paper Aid/R")
getwd()
list.files()

apsr_ag <- read.dta(file="apsr_ag.dta", convert.dates=TRUE, missing.type=FALSE, convert.factors=FALSE)
is.data.frame(apsr_ag)
summary(apsr_ag)

#DV
		#Recodes
apsr_ag$q1dv <- ifelse(apsr_ag$CUB3AB1==1, 0, ifelse(apsr_ag$CUB3AB1==2, 10, ifelse(apsr_ag$CUB3AB1==3,29.5, ifelse(apsr_ag$CUB3AB1==4,40, ifelse(apsr_ag$CUB3AB1==5,50, ifelse(apsr_ag$CUB3AB1==6,69.5, ifelse(apsr_ag$CUB3AB1==7,90, ifelse(apsr_ag$CUB3AB1==NA,NA))))))))
apsr_ag$q2dv <- ifelse(apsr_ag$CUB3AB2==1, 1, ifelse(apsr_ag$CUB3AB2==2, 2, ifelse(apsr_ag$CUB3AB2==3,3, ifelse(apsr_ag$CUB3AB2==4,4, ifelse(apsr_ag$CUB3AB2==5,5,ifelse(apsr_ag$CUB3AB2==6,6,ifelse(apsr_ag$CUB3AB2==7,7, ifelse(apsr_ag$CUB3AB2==NA,NA))))))))
apsr_ag$q6dv <- ifelse(apsr_ag$CUB3AB6==1, 5, ifelse(apsr_ag$CUB3AB6==2, 4, ifelse(apsr_ag$CUB3AB6==3,3, ifelse(apsr_ag$CUB3AB6==4,2, ifelse(apsr_ag$CUB3AB6==5,1,ifelse(apsr_ag$CUB3AB6==NA,NA))))))
		#Get PCA scores
myvars <- c("q1dv", "q2dv", "q6dv")
pca_dv <- apsr_ag[myvars]
summary(pca_dv)
library(psych)
pca1 <- principal(pca_dv, nfactors=1, rotate="none", covar=FALSE, scores=TRUE)
pca1
apsr_ag$dv_impute <- pca1$scores
		#Now fill in the missing scores (just 19 of them) based on the questions that were answered. Can't do in Amelia, it will crash b/c of perfect collinearity
apsr_ag$q1dvz <- scale(apsr_ag$q1dv, center=TRUE, scale=TRUE)
apsr_ag$q2dvz <- scale(apsr_ag$q2dv, center=TRUE, scale=TRUE)
apsr_ag$q6dvz <- scale(apsr_ag$q6dv, center=TRUE, scale=TRUE)
scores <- lm(dv_impute ~ q1dvz + q2dvz + q6dvz, data=apsr_ag)
apsr_ag$q1dvz[is.na(apsr_ag$q1dvz)] <- 0
apsr_ag$q2dvz[is.na(apsr_ag$q2dvz)] <- 0
apsr_ag$q6dvz[is.na(apsr_ag$q6dvz)] <- 0
myvarsz <- c("q1dvz", "q2dvz", "q6dvz")
apsr_ag$dv <- predict(scores,apsr_ag[myvarsz],data=apsr_ag)
apsr_ag$dv[is.na(apsr_ag$q1dv) & is.na(apsr_ag$q2dv) & is.na(apsr_ag$q6dv)] <- NA
		#Cronbach's alpha
alpha(pca_dv)

#IVs
	#Agency Index
		#Recodes
apsr_ag$q3 <- ifelse(apsr_ag$CUB3AB3==1, 1, ifelse(apsr_ag$CUB3AB3==2, 2, ifelse(apsr_ag$CUB3AB3==3,3, ifelse(apsr_ag$CUB3AB3==4,4, ifelse(apsr_ag$CUB3AB3==5,5,ifelse(apsr_ag$CUB3AB3==NA,NA))))))
apsr_ag$q4 <- ifelse(apsr_ag$CUB3AB4==1, 5, ifelse(apsr_ag$CUB3AB4==2, 4, ifelse(apsr_ag$CUB3AB4==3,3, ifelse(apsr_ag$CUB3AB4==4,2, ifelse(apsr_ag$CUB3AB4==5,1,ifelse(apsr_ag$CUB3AB4==NA,NA))))))
apsr_ag$q8 <- ifelse(apsr_ag$CUB3AB8==1, 5, ifelse(apsr_ag$CUB3AB8==2, 4, ifelse(apsr_ag$CUB3AB8==3,3, ifelse(apsr_ag$CUB3AB8==4,2, ifelse(apsr_ag$CUB3AB8==5,1,ifelse(apsr_ag$CUB3AB8==NA,NA))))))
		#Get PCA scores 
myvars <- c("q3", "q4", "q8")
pca_agency <- apsr_ag[myvars]
summary(pca_agency)
pca2 <- principal(pca_agency, nfactors=1, rotate="none", covar=FALSE, scores=TRUE)
pca2
apsr_ag$agency_impute <- pca2$scores
		#Now fill in the missing scores (just 20 of them) based on the questions that were answered. 
apsr_ag$q3z <- scale(apsr_ag$q3, center=TRUE, scale=TRUE)
apsr_ag$q4z <- scale(apsr_ag$q4, center=TRUE, scale=TRUE)
apsr_ag$q8z <- scale(apsr_ag$q8, center=TRUE, scale=TRUE)
scores2 <- lm(agency_impute ~ q3z + q4z + q8z, data=apsr_ag)
apsr_ag$q3z[is.na(apsr_ag$q3z)] <- 0
apsr_ag$q4z[is.na(apsr_ag$q4z)] <- 0
apsr_ag$q8z[is.na(apsr_ag$q8z)] <- 0
myvarsz2 <- c("q3z", "q4z", "q8z")
apsr_ag$agency <- predict(scores2,apsr_ag[myvarsz2],data=apsr_ag)
apsr_ag$agency[is.na(apsr_ag$q3) & is.na(apsr_ag$q4) & is.na(apsr_ag$q8)] <- NA
		#Cronbach's alpha
alpha(pca_agency)
		#frequencies of items in agency index
library(psych)
library(Hmisc)
q3table <- wtd.table(apsr_ag$q3, weights=apsr_ag$V102, type='table', normwt=TRUE, na.rm=TRUE)
prop.table(q3table)
q4table <- wtd.table(apsr_ag$q4, weights=apsr_ag$V102, type='table', normwt=TRUE, na.rm=TRUE)
prop.table(q4table)
q8table <- wtd.table(apsr_ag$q8, weights=apsr_ag$V102, type='table', normwt=TRUE, na.rm=TRUE)
prop.table(q8table)


	#Resentment Index
		#Recodes
apsr_ag$q5 <- ifelse(apsr_ag$CUB3AB5==1, 5, ifelse(apsr_ag$CUB3AB5==2, 4, ifelse(apsr_ag$CUB3AB5==3,3, ifelse(apsr_ag$CUB3AB5==4,2, ifelse(apsr_ag$CUB3AB5==5,1,ifelse(apsr_ag$CUB3AB5==NA,NA))))))
apsr_ag$q9 <- ifelse(apsr_ag$CUB3AB9==1, 1, ifelse(apsr_ag$CUB3AB9==2, 2, ifelse(apsr_ag$CUB3AB9==3,3, ifelse(apsr_ag$CUB3AB9==4,4, ifelse(apsr_ag$CUB3AB9==5,5,ifelse(apsr_ag$CUB3AB9==NA,NA))))))
apsr_ag$q10 <- ifelse(apsr_ag$CUB3AB10==1, 5, ifelse(apsr_ag$CUB3AB10==2, 4, ifelse(apsr_ag$CUB3AB10==3,3, ifelse(apsr_ag$CUB3AB10==4,2, ifelse(apsr_ag$CUB3AB10==5,1,ifelse(apsr_ag$CUB3AB10==NA,NA))))))
		#Get PCA scores 
myvars3 <- c("q5", "q9", "q10")
pca_resent <- apsr_ag[myvars3]
summary(pca_resent)
pca3 <- principal(pca_resent, nfactors=1, rotate="none", covar=FALSE, scores=TRUE)
pca3
apsr_ag$resent_impute <- pca3$scores
		#Now fill in the missing scores (just 16 of them) based on the questions that were answered. 
apsr_ag$q5z <- scale(apsr_ag$q5, center=TRUE, scale=TRUE)
apsr_ag$q9z <- scale(apsr_ag$q9, center=TRUE, scale=TRUE)
apsr_ag$q10z <- scale(apsr_ag$q10, center=TRUE, scale=TRUE)
scores3 <- lm(resent_impute ~ q5z + q9z + q10z, data=apsr_ag)
apsr_ag$q5z[is.na(apsr_ag$q5z)] <- 0
apsr_ag$q9z[is.na(apsr_ag$q9z)] <- 0
apsr_ag$q10z[is.na(apsr_ag$q10z)] <- 0
myvarsz3 <- c("q5z", "q9z", "q10z")
apsr_ag$resentmentindex <- predict(scores3,apsr_ag[myvarsz3],data=apsr_ag)
apsr_ag$resentmentindex[is.na(apsr_ag$q5) & is.na(apsr_ag$q9) & is.na(apsr_ag$q10)] <- NA
		#Make missing if dv==NA. Needed for mediation analysis below
apsr_ag$resentmentindex[is.na(apsr_ag$dv)]<- NA

	#Living Standards Index
		#Recodes
apsr_ag$q11 <- ifelse(apsr_ag$CUB3AB11==1, 0, ifelse(apsr_ag$CUB3AB11==2, 10, ifelse(apsr_ag$CUB3AB11==3,20, ifelse(apsr_ag$CUB3AB11==4,30, ifelse(apsr_ag$CUB3AB11==5,40, ifelse(apsr_ag$CUB3AB11==6,50, ifelse(apsr_ag$CUB3AB11==7,60, ifelse(apsr_ag$CUB3AB11==8,70, ifelse(apsr_ag$CUB3AB11==9,80, ifelse(apsr_ag$CUB3AB11==10,90,ifelse(apsr_ag$CUB3AB11==11,100, ifelse(apsr_ag$CUB3AB11==NA,NA))))))))))))
apsr_ag$q12 <- ifelse(apsr_ag$CUB3AB12==1, 0, ifelse(apsr_ag$CUB3AB12==2, 10, ifelse(apsr_ag$CUB3AB12==3,20, ifelse(apsr_ag$CUB3AB12==4,30, ifelse(apsr_ag$CUB3AB12==5,40, ifelse(apsr_ag$CUB3AB12==6,50, ifelse(apsr_ag$CUB3AB12==7,60, ifelse(apsr_ag$CUB3AB12==8,70, ifelse(apsr_ag$CUB3AB12==9,80, ifelse(apsr_ag$CUB3AB12==10,90,ifelse(apsr_ag$CUB3AB12==11,100, ifelse(apsr_ag$CUB3AB12==NA,NA))))))))))))
		#Calculate index
apsr_ag$livingstandardsindex <- (apsr_ag$q11 + apsr_ag$q12)/2 
		#Now fill in the missing scores based on the question that was answered. 
apsr_ag$livingstandardsindex[is.na(apsr_ag$q11)] <- apsr_ag$q12[is.na(apsr_ag$q11)]
apsr_ag$livingstandardsindex[is.na(apsr_ag$q12)] <- apsr_ag$q11[is.na(apsr_ag$q12)]
		#Make missing if dv==NA. Needed for mediation analysis below
apsr_ag$livingstandardsindex[is.na(apsr_ag$dv)]<- NA

	#Two more recodes	
apsr_ag$pid <- ifelse(apsr_ag$pid7==1, 1, ifelse(apsr_ag$pid7==2, 2, ifelse(apsr_ag$pid7==3,3, ifelse(apsr_ag$pid7==4,4, ifelse(apsr_ag$pid7==5,5,ifelse(apsr_ag$pid7==6,6,ifelse(apsr_ag$pid7==7,7, ifelse(apsr_ag$pid7==8,NA, ifelse(apsr_ag$pid7==NA,NA)))))))))
apsr_ag$ideo <- ifelse(apsr_ag$CC334A==1, 1, ifelse(apsr_ag$CC334A==2, 2, ifelse(apsr_ag$CC334A==3,3, ifelse(apsr_ag$CC334A==4,4, ifelse(apsr_ag$CC334A==5,5,ifelse(apsr_ag$CC334A==6,6,ifelse(apsr_ag$CC334A==7,7, ifelse(apsr_ag$CC334A==8,NA, ifelse(apsr_ag$CC334A==NA,NA)))))))))

	#Create dummies for experimental groups
apsr_ag$blacktreatment   <- ifelse(apsr_ag$treat==1, 1, ifelse(apsr_ag$treat==2, 1, ifelse(apsr_ag$treat==3, 0, ifelse(apsr_ag$treat==4, 0, ifelse(apsr_ag$treat==NA,NA)))))  
apsr_ag$whitetreatment   <- ifelse(apsr_ag$treat==1, 0, ifelse(apsr_ag$treat==2, 0, ifelse(apsr_ag$treat==3, 1, ifelse(apsr_ag$treat==4, 1, ifelse(apsr_ag$treat==NA,NA)))))  
apsr_ag$cashtreatment    <- ifelse(apsr_ag$treat==1, 1, ifelse(apsr_ag$treat==2, 0, ifelse(apsr_ag$treat==3, 1, ifelse(apsr_ag$treat==4, 0, ifelse(apsr_ag$treat==NA,NA)))))  
apsr_ag$inkindtreatment  <- ifelse(apsr_ag$treat==1, 0, ifelse(apsr_ag$treat==2, 1, ifelse(apsr_ag$treat==3, 0, ifelse(apsr_ag$treat==4, 1, ifelse(apsr_ag$treat==NA,NA)))))  

apsr_ag$blackxcash   <- ifelse(apsr_ag$treat==1, 1, ifelse(apsr_ag$treat==2, 0, ifelse(apsr_ag$treat==3, 0, ifelse(apsr_ag$treat==4, 0, ifelse(apsr_ag$treat==NA,NA)))))  
apsr_ag$blackxinkind <- ifelse(apsr_ag$treat==1, 0, ifelse(apsr_ag$treat==2, 1, ifelse(apsr_ag$treat==3, 0, ifelse(apsr_ag$treat==4, 0, ifelse(apsr_ag$treat==NA,NA)))))  
apsr_ag$whitexcash   <- ifelse(apsr_ag$treat==1, 0, ifelse(apsr_ag$treat==2, 0, ifelse(apsr_ag$treat==3, 1, ifelse(apsr_ag$treat==4, 0, ifelse(apsr_ag$treat==NA,NA)))))  
apsr_ag$whitexinkind <- ifelse(apsr_ag$treat==1, 0, ifelse(apsr_ag$treat==2, 0, ifelse(apsr_ag$treat==3, 0, ifelse(apsr_ag$treat==4, 1, ifelse(apsr_ag$treat==NA,NA)))))  

	#create variable to drop African-Americans 
apsr_ag$dropaa <- ifelse(apsr_ag$race==1, 1, ifelse(apsr_ag$race==2, 0, ifelse(apsr_ag$race==3,1, ifelse(apsr_ag$race==4,1, ifelse(apsr_ag$race==5,1, ifelse(apsr_ag$race==6,1, ifelse(apsr_ag$race==7,1, ifelse(apsr_ag$race==8,1, ifelse(apsr_ag$race==NA,NA)))))))))
dropaa <- apsr_ag[ which(apsr_ag$dropaa==1),]

#ANALYSIS
	#treatment effects for Table 1 and Figure 2
fit1 <- lm(dv ~ blacktreatment + cashtreatment, data=dropaa, weights=V102)
summary(fit1)
fit2 <- lm(dv ~ blackxinkind + blackxcash + whitexcash, data=dropaa, weights=V102)
summary(fit2)
fit3 <- lm(dv ~ blackxinkind + whitexinkind + blackxcash + whitexcash-1, data=dropaa, weights=V102)
summary(fit3)

	#mediation analyses with casewise deletion (not reported)
library("mediation")
		#agency index
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=dropaa, weights=V102)
summary(med.fit)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=dropaa, weights=V102)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "agency")
summary(med.out)
sens.out <- medsens(med.out, rho.by = .1, effect.type = "indirect")
summary(sens.out)
plot(sens.out, sens.par = "rho", main = "Perceptions of Agency", ylim = c(-0.2, 0.2))
		#living standards 
med.fit <- lm(livingstandardsindex ~ blackxinkind + blackxcash + ideo + pid + educ, data=dropaa, weights=V102)
summary(med.fit)
out.fit <- lm(dv ~ blackxinkind +  livingstandardsindex + blackxcash + ideo + pid + educ, data=dropaa, weights=V102)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "livingstandardsindex")
summary(med.out)
		#resentment
med.fit <- lm(resentmentindex ~ blackxinkind + blackxcash + ideo + pid + educ, data=dropaa, weights=V102)
summary(med.fit)
out.fit <- lm(dv ~ blackxinkind +  resentmentindex + blackxcash + ideo + pid + educ, data=dropaa, weights=V102)
summary(out.fit)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "resentmentindex")
summary(med.out)

	#multiple imputation for mediation analyses
set.seed(131313)
require(Amelia)
ameliavarstemp <- cbind(dropaa$blackxinkind, dropaa$blackxcash, dropaa$whitexinkind, dropaa$agency, dropaa$dv, dropaa$resentmentindex, dropaa$livingstandardsindex, dropaa$ideo, dropaa$pid, dropaa$educ, dropaa$V102)
ameliavars <- as.data.frame(ameliavarstemp) 
names(ameliavars) <- c("blackxinkind","blackxcash","whitexcash","agency","dv","resentmentindex","livingstandardsindex", "ideo","pid","educ","V102") 
a.out <- amelia(ameliavars, m = 5)
save(a.out, file="imputations.RData")
write.amelia(obj=a.out,file.stem="dropaaimpute", format="dta")

		#Agency Index
			#mediation equation (for table 3)
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(agency ~ blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#outcome equation
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(dv ~ agency+ blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#get mediation statistics and sensitivity analysis (i just did all 5 and used Rubin's rules. "mediations" can't handle weights)
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[1]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=a.out$imputations[[1]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "agency")
summary(med.out)
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[2]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=a.out$imputations[[2]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "agency")
summary(med.out)
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "agency")
summary(med.out)
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[4]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=a.out$imputations[[4]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "agency")
summary(med.out)
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[5]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=a.out$imputations[[5]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "agency")
summary(med.out)
			#You can't do a sensitivity analysis over all the imputed data, so I picked the dataset that was closest to final results
med.fit <- lm(agency ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + agency + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
sens.out <- medsens(med.out, rho.by = .1, effect.type = "indirect")
summary(sens.out)
plot(sens.out, sens.par = "rho", main = "Perception of Foreign Poor's Agency", ylim = c(-0.2, 0.2))

		#Resentment Index
			#mediation equation
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(resentmentindex ~ blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#outcome equation
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(dv ~ resentmentindex + blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#get mediation statistics (i just did all 5 and used Rubin's rules. "mediations" can't handle weights)
med.fit <- lm(resentmentindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[1]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + resentmentindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[1]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "resentmentindex")
summary(med.out)
med.fit <- lm(resentmentindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[2]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + resentmentindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[2]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "resentmentindex")
summary(med.out)
med.fit <- lm(resentmentindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + resentmentindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "resentmentindex")
summary(med.out)
med.fit <- lm(resentmentindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[4]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + resentmentindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[4]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "resentmentindex")
summary(med.out)
med.fit <- lm(resentmentindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[5]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + resentmentindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[5]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "resentmentindex")
summary(med.out)

		#Living Standards Index
			#mediation equation
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(livingstandardsindex ~ blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#outcome equation
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(dv ~ livingstandardsindex + blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#get mediation statistics (i just did all 5 and used Rubin's rules. "mediations" can't handle weights)
med.fit <- lm(livingstandardsindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[1]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + livingstandardsindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[1]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "livingstandardsindex")
summary(med.out)
med.fit <- lm(livingstandardsindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[2]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + livingstandardsindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[2]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "livingstandardsindex")
summary(med.out)
med.fit <- lm(livingstandardsindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + livingstandardsindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[3]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "livingstandardsindex")
summary(med.out)
med.fit <- lm(livingstandardsindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[4]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + livingstandardsindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[4]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "livingstandardsindex")
summary(med.out)
med.fit <- lm(livingstandardsindex  ~ blackxinkind + blackxcash + ideo + pid + educ, data=a.out$imputations[[5]], weights=V102)
out.fit <- lm(dv ~ blackxinkind + livingstandardsindex  + blackxcash + ideo + pid + educ, data=a.out$imputations[[5]], weights=V102)
med.out <- mediate(med.fit, out.fit, treat = "blackxinkind", mediator = "livingstandardsindex")
summary(med.out)

	#Analysis for African-Americans only. For footnote
		#create variable to identify only African-Americans 
apsr_ag$onlyaa <- ifelse(apsr_ag$race==1, 0, ifelse(apsr_ag$race==2, 1, ifelse(apsr_ag$race==3,0, ifelse(apsr_ag$race==4,0, ifelse(apsr_ag$race==5,0, ifelse(apsr_ag$race==6,0, ifelse(apsr_ag$race==7,0, ifelse(apsr_ag$race==8,0, ifelse(apsr_ag$race==NA,NA)))))))))
onlyaa <- apsr_ag[ which(apsr_ag$onlyaa==1),]
		#treatment effects among African-Americans only 
fit1 <- lm(dv ~ blacktreatment + cashtreatment, data=onlyaa, weights=V102)
summary(fit1)
fit2 <- lm(dv ~ blackxinkind + blackxcash + whitexcash, data=onlyaa, weights=V102)
summary(fit2)
		#multiple imputation for mediation analyses
set.seed(441133)
require(Amelia)
ameliavarstemp <- cbind(onlyaa$blackxinkind, onlyaa$blackxcash, onlyaa$whitexinkind, onlyaa$agency, onlyaa$dv, onlyaa$resentmentindex, onlyaa$livingstandardsindex, onlyaa$ideo, onlyaa$pid, onlyaa$educ, onlyaa$V102)
ameliavars <- as.data.frame(ameliavarstemp) 
names(ameliavars) <- c("blackxinkind","blackxcash","whitexcash","agency","dv","resentmentindex","livingstandardsindex", "ideo","pid","educ","V102") 
a.out <- amelia(ameliavars, m = 5)
save(a.out, file="imputations.RData")
write.amelia(obj=a.out,file.stem="dropaaimpute", format="dta")
			#mediator equation 
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(agency ~ blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results
			#outcome equation
b.out<-NULL
se.out<-NULL
for(i in 1:a.out$m) {
ols.out <- lm(dv ~ agency+ blackxinkind + blackxcash + ideo + pid +educ , weights=V102, data = a.out$imputations[[i]])
b.out <- rbind(b.out, ols.out$coef)
se.out <- rbind(se.out, coef(summary(ols.out))[,2])
}
combined.results <- mi.meld(q = b.out, se = se.out)
combined.results

#Appendix
	#Randomization check
describeBy(apsr_ag$ideo, group=apsr_ag$treat, mat=TRUE)
describeBy(apsr_ag$pid, group=apsr_ag$treat, mat=TRUE)
describeBy(apsr_ag$educ, group=apsr_ag$treat, mat=TRUE)
	#Heterogenous Effects. Can't do a describeBY because of weights
		#Ideology
dropaa$ideoEL <- ifelse(dropaa$ideo==1, 1, ifelse(dropaa$ideo==2, 0, ifelse(dropaa$ideo==3,0, ifelse(dropaa$ideo==4,0, ifelse(dropaa$ideo==5,0, ifelse(dropaa$ideo==6,0, ifelse(dropaa$ideo==7,0, ifelse(dropaa$ideo==NA,NA))))))))
ideoEL <- dropaa[ which(dropaa$ideoEL==1),]
fit <- lm(dv ~ blackxinkind + blackxcash, data=ideoEL, weights=V102)
summary(fit)

dropaa$ideoL <- ifelse(dropaa$ideo==1, 0, ifelse(dropaa$ideo==2, 1, ifelse(dropaa$ideo==3,1, ifelse(dropaa$ideo==4,0, ifelse(dropaa$ideo==5,0, ifelse(dropaa$ideo==6,0, ifelse(dropaa$ideo==7,0, ifelse(dropaa$ideo==NA,NA))))))))
ideoL <- dropaa[ which(dropaa$ideoL==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=ideoL, weights=V102)
summary(fit)


dropaa$ideoM <- ifelse(dropaa$ideo==1, 0, ifelse(dropaa$ideo==2, 0, ifelse(dropaa$ideo==3,0, ifelse(dropaa$ideo==4,1, ifelse(dropaa$ideo==5,0, ifelse(dropaa$ideo==6,0, ifelse(dropaa$ideo==7,0, ifelse(dropaa$ideo==NA,NA))))))))
ideoM <- dropaa[ which(dropaa$ideoM==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=ideoM, weights=V102)
summary(fit)


dropaa$ideoC <- ifelse(dropaa$ideo==1, 0, ifelse(dropaa$ideo==2, 0, ifelse(dropaa$ideo==3,0, ifelse(dropaa$ideo==4,0, ifelse(dropaa$ideo==5,1, ifelse(dropaa$ideo==6,1, ifelse(dropaa$ideo==7,0, ifelse(dropaa$ideo==NA,NA))))))))
ideoC <- dropaa[ which(dropaa$ideoC==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=ideoC, weights=V102)
summary(fit)


dropaa$ideoEC <- ifelse(dropaa$ideo==1, 0, ifelse(dropaa$ideo==2, 0, ifelse(dropaa$ideo==3,0, ifelse(dropaa$ideo==4,0, ifelse(dropaa$ideo==5,0, ifelse(dropaa$ideo==6,0, ifelse(dropaa$ideo==7,1, ifelse(dropaa$ideo==NA,NA))))))))
ideoEC <- dropaa[ which(dropaa$ideoEC==1),]
fit <- lm(dv ~ blackxinkind + blackxcash, data=ideoEC, weights=V102)
summary(fit)

		#Education
dropaa$educ1 <- ifelse(dropaa$educ==1, 1, ifelse(dropaa$educ==2, 0, ifelse(dropaa$educ==3,0, ifelse(dropaa$educ==4,0, ifelse(dropaa$educ==5,0, ifelse(dropaa$educ==6,0, ifelse(dropaa$educ==NA,NA)))))))
educ1 <- dropaa[ which(dropaa$educ1==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=educ1, weights=V102)
summary(fit)

dropaa$educ2 <- ifelse(dropaa$educ==1, 0, ifelse(dropaa$educ==2, 1, ifelse(dropaa$educ==3,0, ifelse(dropaa$educ==4,0, ifelse(dropaa$educ==5,0, ifelse(dropaa$educ==6,0, ifelse(dropaa$educ==NA,NA)))))))
educ2 <- dropaa[ which(dropaa$educ2==1),]
fit <- lm(dv ~ blackxinkind + blackxcash, data=educ2, weights=V102)
summary(fit)

dropaa$educ3 <- ifelse(dropaa$educ==1, 0, ifelse(dropaa$educ==2, 0, ifelse(dropaa$educ==3,1, ifelse(dropaa$educ==4,1, ifelse(dropaa$educ==5,0, ifelse(dropaa$educ==6,0, ifelse(dropaa$educ==NA,NA)))))))
educ3 <- dropaa[ which(dropaa$educ3==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=educ3, weights=V102)
summary(fit)

dropaa$educ4 <- ifelse(dropaa$educ==1, 0, ifelse(dropaa$educ==2, 0, ifelse(dropaa$educ==3,0, ifelse(dropaa$educ==4,0, ifelse(dropaa$educ==5,1, ifelse(dropaa$educ==6,0, ifelse(dropaa$educ==NA,NA)))))))
educ4 <- dropaa[ which(dropaa$educ4==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=educ4, weights=V102)
summary(fit)

dropaa$educ5 <- ifelse(dropaa$educ==1, 0, ifelse(dropaa$educ==2, 0, ifelse(dropaa$educ==3,0, ifelse(dropaa$educ==4,0, ifelse(dropaa$educ==5,0, ifelse(dropaa$educ==6,1, ifelse(dropaa$educ==NA,NA)))))))
educ5 <- dropaa[ which(dropaa$educ5==1),]
fit <- lm(dv ~ blackxinkind + blackxcash , data=educ5, weights=V102)
summary(fit)

	#Descriptive Stats
			#mins and maxes
summary(apsr_ag)
			#means and sd's, using weights
wtd.mean(apsr_ag$dv, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$dv, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$q1dv, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$q1dv, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$q2dv, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$q2dv, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB6, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB6, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$agency, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$agency, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB4, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB4, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB3, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB3, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB8, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB8, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$livingstandardsindex, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$livingstandardsindex, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$ideo, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$ideo, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$pid, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$pid, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$educ, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$educ, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$resentmentindex, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$resentmentindex, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB5, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB5, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB9, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB9, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$CUB3AB10, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$CUB3AB10, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$q11, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$q11, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.mean(apsr_ag$q12, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
wtd.var(apsr_ag$q12, weights=apsr_ag$V102, normwt=TRUE, na.rm=TRUE)
