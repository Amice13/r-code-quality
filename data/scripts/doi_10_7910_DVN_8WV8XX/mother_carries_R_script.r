#fitting logistic model to the behaviour 'carry'
#written by T Revathe

rm(list=ls())

#required libraries
library(lme4)
library(car)
library(MuMIn)
library(car)
library(xlsx)

xdata=read.csv(file="mother_carries.txt", header=T,
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

xdata$resp.mat=cbind(carry=xdata$no_of_scans_mother_carries, notcarry=xdata$no_of_scans_mother_moves - xdata$no_of_scans_mother_carries)
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

glm.full=glm(resp.mat ~ 1+ z.age, data = t.data, family = binomial)
glm.null=glm(resp.mat ~ 1, data = t.data, family = binomial)

ll.full=as.vector(logLik(glm.full)) #-3413.143
ll.null=as.vector(logLik(glm.null)) # -17888.93

full_null.glm=as.data.frame(anova(glm.null, glm.full, test="Chisq"))

#R2
n=length(residuals(glm.full))
(1-exp((ll.null-ll.full)*2/n))/(1-exp(ll.null*2/n)) #1

#plotting ci and then the data

pred.data=data.frame(z.age= seq(from=min(xdata$z.age),
									to=max(xdata$z.age), by=0.1))

ci.plot=predict.glm(object=glm.full, newdata=pred.data, type="link", se.fit=T)

ci.plot=data.frame(
lwr=ci.plot$fit-ci.plot$se.fit*abs(qt(p=0.025, df=glm.full$df.residual)),
upr=ci.plot$fit+ci.plot$se.fit*abs(qt(p=0.025, df=glm.full$df.residual))
) 

ci.plot=exp(ci.plot)/(1+exp(ci.plot))

# Merging data frames based on names to get colors for each individual
merged_data <- merge(xdata, colour_code, by.x = "offspringID", by.y = "offspringID")

par(mar=c(3, 3, 0.2, 0.2)) #setting margin widths for figure 
par(mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1) #setting where axis labels are displayed

plot(x=merged_data$z.age, y=merged_data$proportion_of_scans_mother_carries_while_moving, 
     pch=19, cex=1.2,
	 col=adjustcolor(merged_data$colour,alpha=0.4),
     xlab="offspring age (in years)",
     ylab="Probability of mother carrying offspring",
     ylim=c(0,1),xaxt="n")

xlab=seq(from=0, to=8, by=1)

#map to z-space:
xat=(xlab-mean(merged_data$offspringAge))/sd(merged_data$offspringAge)

#add axis:
axis(side=1, at=xat, labels=xlab)

#adding ci
polygon(x=c(pred.data$z.age, rev(pred.data$z.age)),
y=c(ci.plot$lwr, rev(ci.plot$upr)),
border=NA, col=adjustcolor("black", alpha.f=0.3))

#lines(x=pred.data$z.age, y=ci.plot[, "lwr"],
#lty=3)
#lines(x=pred.data$z.age, y=ci.plot[, "upr"],
#lty=3)

#adding the fitted model
xvals=seq(from=min(merged_data$z.age), to=max(merged_data$z.age),
          length.out=100)
coefs=coef(glm.full)
yvals=coefs["(Intercept)"]+coefs["z.age"]*xvals

#transform to response space:
yvals=exp(yvals)/(1+exp(yvals))
lines(x=xvals, y=yvals, col="black", lwd=2, lty=1)

###################################################################################################################

full.wac=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A +
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c | offspringID) + 
								(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c | motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="nlminbwrap",
											optCtrl=list(maxfun=1e6)))

summary(full.wac)$varcor #singular fit

#refitting without the correlation parameters
full.wnc=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex + motherParity + males_P_A +
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c || offspringID) + 
								(1 + z.age + z.FAI + z.assoc_size + males_P_A.P.c || motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="nlminbwrap",
											optCtrl=list(maxfun=1e6)))

summary(full.wnc)$varcor #singular fit due to some random effects being 0 (at the boundary)

#comparing log likelihoods of the models
logLik(full.wac)
logLik(full.wnc)

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

#testing significance of individual predictors
sig=as.data.frame(drop1(full.wnc, test="Chisq"))
sig

#inference: model coefficients
summary(full.wnc)$coefficients

r.squaredGLMM(object=full.wnc)

source("boot_glmm.r")
boot.bin=boot.glmm.pred(model.res=full.wnc,nboots=100, para=T, resol=1000, level=0.95,n.cores=1)
ci=boot.bin$ci.estimates

###################################################################################################

#plotting the effect of age
#-------------------------

t.data$offspringSex.d=as.numeric(t.data$offspringSex==levels(t.data$offspringSex)[2])
t.data$offspringSex.d.c=t.data$offspringSex.d-mean(t.data$offspringSex.d)
t.data$males_P_A.d=as.numeric(t.data$males_P_A==levels(t.data$males_P_A)[2])
t.data$males_P_A.d.c=t.data$males_P_A.d-mean(t.data$males_P_A.d)
t.data$motherParity.d.c=as.numeric(t.data$motherParity==levels(t.data$motherParity)[2])
t.data$motherParity.d.c=t.data$motherParity.d.c-mean(t.data$motherParity.d.c)

#read the names and colours

colour_code=read.table(file="colour_code_mother_carries.txt", header=T,
                 stringsAsFactors=T, sep="\t")

# Merging data frames based on names to get colors for each individual
merged_data <- merge(xdata, colour_code, by.x = "offspringID", by.y = "offspringID")

plot.full.wnc.1=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex.d.c + motherParity.d.c + males_P_A.d.c +
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.d.c || offspringID) + 
								(1 + z.age + z.FAI + z.assoc_size + males_P_A.d.c || motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

#plotting ci and then the data

boot.bin.1=boot.glmm.pred(model.res=plot.full.wnc.1, use="z.age",nboots=100, para=T, resol=1000, level=0.95,n.cores=1)
ci.1=boot.bin.1$ci.predicted

par(mar=c(3, 3, 0.2, 0.2)) #setting margin widths for figure 
par(mgp=c(1.7, 0.3, 0), tcl=-0.15, las=1) #setting where axis labels are displayed

plot(x=merged_data$z.age, y=merged_data$proportion_of_scans_mother_carries_while_moving, 
     pch=19, cex=1.2,
	 col=adjustcolor(merged_data$colour,alpha=0.4),
     xlab="Offspring age (in years)",
     ylab="Probability of carrying",
     ylim=c(0,1),xaxt="n")

xlab=seq(from=1, to=8, by=1)

#map to z-space:
xat=(xlab-mean(merged_data$offspringAge))/sd(merged_data$offspringAge)

#add axis:
axis(side=1, at=xat, labels=xlab)

#adding ci
polygon(x=c(ci.1$z.age, rev(ci.1$z.age)),
y=c(ci.1$lower.cl, rev(ci.1$upper.cl)),
border=NA, col=adjustcolor("black", alpha.f=0.3))

#adding the fitted model
xvals=seq(from=min(merged_data$z.age), to=max(merged_data$z.age),
          length.out=100)
coefs=fixef(plot.full.wnc.1)
yvals=coefs["(Intercept)"]+coefs["z.age"]*xvals

#transform to response space:
yvals=exp(yvals)/(1+exp(yvals))
lines(x=xvals, y=yvals, col="black", lwd=2, lty=1)

###################################################################################################

##plotting the effect of male P/A

plot.full.wnc.2=glmer (resp.mat ~ z.age + z.FAI + z.assoc_size + offspringSex.d.c + motherParity.d.c + males_P_A +
							(1 + z.age + z.FAI + z.assoc_size + males_P_A.d.c || offspringID) + 
								(1 + z.age + z.FAI + z.assoc_size + males_P_A.d.c || motherID) +
									(1 | followID), data = t.data, family = binomial,
										control=glmerControl(optimizer="bobyqa",
											optCtrl=list(maxfun=1e6)))

#plotting ci and then the data

boot.bin.2=boot.glmm.pred(model.res=plot.full.wnc.2, use="males_P_A",nboots=100, para=T, resol=1000, level=0.95,n.cores=1)
ci.2=boot.bin.2$ci.predicted

xfit=c(fixef(plot.full.wnc.2)["(Intercept)"], fixef(plot.full.wnc.2)["(Intercept)"]+fixef(plot.full.wnc.2)["males_P_AP"])
xfit=exp(xfit)/(1+exp(xfit))
jit.range=0.25

# Reorder the factor levels to plot primiparous first and then multiparous
xdata$males_P_A <- factor(xdata$males_P_A, levels = c("A", "P"))

par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), las=1, tcl=-0.2)
plot(x=as.numeric(xdata$males_P_A)+runif(n=nrow(xdata), min=-jit.range, max=jit.range), 
	y=xdata$resp.mat[, 1]/apply(X=xdata$resp.mat, MARGIN=1, FUN=sum),
	xlim=c(0.5, 2.5), xaxt="n", xaxs="i", xlab="", ylab="Probability of carrying",
	pch=19, col=adjustcolor(merged_data$colour,alpha=0.4), cex=1.25)
mtext(side=1, at=1:2, line=0.2, text=c("Male absent","Male present"), cex=1)
segments(x0=(1:2)-jit.range, x1=(1:2)+jit.range, y0=xfit, y1=xfit, lwd=1.5)

where=c(1,2)
arrows(x0=(1:2), x1=(1:2), y0=ci.2[where, "lower.cl"], y1=ci.2[where, "upper.cl"], code=3, angle=90, len=0.05,lwd=1.5)

###################################################################################################

save.image(file="mother_carries.RData")