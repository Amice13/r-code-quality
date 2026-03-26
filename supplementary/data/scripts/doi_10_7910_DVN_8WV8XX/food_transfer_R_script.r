#fitting logistic model to the behaviour 'food transfer'
#written by T Revathe

rm(list=ls())

#required libraries
library(lme4)
library(car)
library(MuMIn)
library(car)
library(xlsx)

#reading and wrangling data
#--------------------------

xdata=read.table(file="food_transfer.txt", header=T,
                 stringsAsFactors=T, sep="\t")
str(xdata)
names(xdata)

#no. of follows per unique mother-offspring pair 
mother_offspring_follow=aggregate(x=xdata$followNr, by=list(motherID=xdata$motherID, offspringID=xdata$offspringID), FUN=length) 
mother_offspring_follow

#add a helper column
mother_offspring_follow$mother_offspring=paste(mother_offspring_follow$motherID,mother_offspring_follow$offspringID, sep="_")

#add the no. of follow per unique mother-offspring pair to the dataframe
xdata$no_of_follows = mother_offspring_follow$x[match(xdata$mother_offspring, mother_offspring_follow$mother_offspring)]

#apply follow filter - remove levels of offspringID with less than 5 follows
xdata=subset(xdata, no_of_follows>4)
xdata=droplevels(xdata)

#to check if the no. of unique mothers is equal to the no. of unique offspring
#if no. of unique mothers is < no. of unique offspring, there are mothers with more than one offspring in the dataset
length(unique(xdata$motherID))==length(unique(xdata$offspringID)) 

#to get the no. and range of unique offspring for each mother
as.data.frame(table(mother_offspring_follow$motherID))
range(table(mother_offspring_follow$motherID)) 

#since there are mothers with more than one offspring in the dataset, 
#offspring ID will have to be nested within motherID.

#to check if there are missing values in the data
which(is.na(xdata), arr.ind=TRUE)

#sample size table

mother_offspring_follow=aggregate(x=xdata$followNr, by=list(motherID=xdata$motherID, offspringID=xdata$offspringID), FUN=length) 
mother_offspring_follow

mother_offspring_duration=aggregate(x=xdata$duration, by=list(motherID=xdata$motherID, offspringID=xdata$offspringID), FUN=sum)
mother_offspring_duration

mother_offspring_ss=data.frame(MotherID = c(mother_offspring_follow$motherID), offspringID = c(mother_offspring_follow$offspringID),
							No_of_follows=c(mother_offspring_follow$x), Duration_of_follows=c(mother_offspring_duration$x))

#setting up the data for the model
#--------------------

#the logistic model gets the response handed over as a matrix with
#two columns with the number of "correct" responses in the first and
#the number of "incorrect" responses in the second column
#creating a response matrix for the logistic model

xdata$resp.mat=cbind(transfer=xdata$No_of_transfer_Result, notransfer=xdata$Total_no_of_begging_events - xdata$No_of_transfer_Result)
head(xdata$resp.mat)

#implicit nesting of scans within follows
xdata$followID=as.factor(paste(xdata$offspringID, xdata$followNr, sep="."))
max(table(xdata$followID))

#z transform offspring age, association size, and FAI for ease of interpretation of model output and model convergence
xdata$z.age=as.vector(scale(xdata$offspringAge))
xdata$z.assoc_size=as.vector(scale(xdata$assoc_size))
xdata$z.FAI=as.vector(scale(xdata$FAI))

#original mean and sd of the z-transformed predictors

mean_sd=data.frame(Predictor=c("FAI","Association size", "offspring age"),
				   Mean=c(mean(xdata$FAI),mean(xdata$assoc_size),mean(xdata$offspringAge)),
				   SD=c(sd(xdata$FAI),sd(xdata$assoc_size),sd(xdata$offspringAge)))

#to check which random slopes to include

source("diagnostic_fcns.r")
xx.fe.re=fe.re.tab(fe.model="resp.mat~ z.age + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A",
                             re="(1|offspringID) + (1|motherID) + (1|followID)", data=xdata)
xx.fe.re$summary

#males_P_A, z.age, z.FAI, z.assoc_size within offspringID
#males_P_A, z.age, z.FAI, z.assoc_size within motherID

#manually dummy coding factors entering the random slopes part is essential
#the dummy coding was conducted by the function fe.re.tab:
str(xx.fe.re$data)
t.data=xx.fe.re$data

#centering the dummy coded variables to get results ± unconditional of the reference level of each variable:
t.data$males_P_A.P.c=t.data$males_P_A.P-mean(t.data$males_P_A.P)

#setting up the model

full.wac=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A +
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c | offspringID) + 
								(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c | motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

#singular fit; correlation parameters within mother ID and offspring ID are close to 1

#refitting without the correlation parameters
full.wnc=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A +
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c || offspringID) + 
								(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c || motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

summary(full.wnc)$coefficients

full.wnc.1=glmer (resp.mat ~ z.age + I(z.age^2) + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A +
							(1 + I(z.age^2) + z.age + z.FAI + z.assoc_size + males_P_A.P.c || offspringID) + 
								(1 + I(z.age^2) + z.age + z.FAI + z.assoc_size + males_P_A.P.c || motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

full.wnc.2=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A +
							(1 + I(z.age^2) + z.age + z.FAI + z.assoc_size + males_P_A.P.c || offspringID) + 
								(1 + I(z.age^2) + z.age + z.FAI + z.assoc_size + males_P_A.P.c || motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

#comparing log likelihoods of the models
logLik(full.wac)
logLik(full.wnc)
logLik(full.wnc.1)

#including all the correlations among random intercepts and slopes didn’t add much to the model’s explanatory value

#assumptions & issues: overdispersion
overdisp.test(full.wnc)

#assumptions & issues: distribution of BLUPs
ranef.diagn.plot(full.wnc)

#assumptions & issues: collinearity
collinearity=vif(full.wnc)
			
#relative model complexity or sample size
length(residuals(full.wnc))/
(length(fixef(full.wnc))+
nrow(as.data.frame(summary(full.wnc)$varcor)))

#inference: full-null model

null=glmer (resp.mat ~ (1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c || offspringID) + 
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c || motherID) +
								(1 | followID), data = t.data, family = binomial,
									control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

full_null=as.data.frame(anova(null, full.wnc, test="Chisq"))
full_null_quad=as.data.frame(anova(full.wnc.1, full.wnc.2, test="Chisq"))

save.image(file="mother_food_transfer.RData")