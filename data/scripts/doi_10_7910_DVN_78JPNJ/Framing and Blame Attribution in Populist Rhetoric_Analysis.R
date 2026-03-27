##Prompting Populism, Busby, Gubler, and Hawkins
#Journal of Politics
#Last updated: 12 February 2018
#Created by Josh Gubler and Ethan Busby

##To load the packages necessary for analysis:
library(readstata13)
library(ggplot2)
library(stargazer)
library(car)
library(lmtest)
library(xtable)
library(MASS)
library(sandwich)
library(RItools)
library(multiwayvcov)
library(irr)
library(psych)

####User-written functions used in the following code####
###############################################
###############################################
predict.logit.f <- function(mod,predict.df){
  
  ##Written by Joshua Gubler ~  http://joshuagubler.com
  ##Last updated on 7 October 2016
  ##This organizes the data in a format so it can be automatically graphed by my predict.plot and/or marginal effects functions (similar to the "margins" command in Stata)
  ##Note, that for this function to work well, you must input factor variables with more than two levels individually (as individual dummies).
  ##Note: when estimating a polynomial, you must create the quadratic/cubic as a separate variable first!!  This is also the best procedure when estimating logged effects.  However, when estimating interaction effects, there is no need to create a separate interaction term.
  
  ##mod = the object for the model you estimated
  ##predict.df = the dataframe that indicates the values you want to use to predict change in Y.  Same as in the default R "predict" function.
  ##This will produce predicted probabilities
  
  tt <- terms(mod)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=(predict.df))
  fit1 <- as.vector(m.mat %*% mod$coef)
  fit <- 1/(1+exp(-(fit1)))
  
  require(sandwich)
  se.fit <- predict(mod,predict.df,se.fit=TRUE,type="response")$se.fit
  ci.lower95 <- fit - 1.96*se.fit
  ci.upper95 <- fit + 1.96*se.fit
  
  pred.df <- data.frame(m.mat,predicted.value=fit,se=se.fit,ci.lower95=ci.lower95,ci.upper95=ci.upper95)[,-1]
  nm <-deparse(substitute(mod))
  mdlname <- paste(nm,"pred.df",sep=".")
  assign(mdlname,pred.df,envir = .GlobalEnv)
}


###############################################
###############################################
predict.plot.f <- function(predicted.df, X, Z, Zname, xlab, ylab)
{
  ##Written by Joshua Gubler ~  http://joshuagubler.com
  ##Last updated: 25 April 2017 (removed the "print" command, and added notes on how one could easily add lines between the points on the plot, by group)
  ##predicted.df = the dataframe created by Gubler's predict functions -- always ending in .pred.df
  ##X = the variable you want to the predicted effect for (all others are most often held at their means).  Note: you need to enter it with the full path (e.g. .pred.df$X)
  ##Z = if you want predictions based on the particular levels of Z you specified in your pred.df dataframe, type your Z variable here (entering the full path as with X).  If not, leave blank.
  ##Zname = the name to put in the legend for Z (if using Z).  If not using Z, leave blank.
  ##xlab = the label for the X axis; put in quotation marks
  ##ylab = the label for the Y axis; put in quotation marks
  
  require(ggplot2)
  if(missing(Z)){
    predplot <- qplot(as.factor(X),predicted.df$predicted.value, xlab=xlab,ylab=ylab) + geom_point(stat="identity") + theme_bw()
  } else{
    predplot <- qplot(X,predicted.df$predicted.value, xlab=xlab,ylab=ylab, colour = as.factor(Z)) + geom_point(stat="identity") + theme_bw() + guides(color=guide_legend(title=Zname))
  }
  predplot + geom_linerange(aes(ymin=predicted.df$ci.lower95,ymax=predicted.df$ci.upper95))
  
  ##Example code: (using Z):
  ##First, estimate a model and run one of my predict functions.  Here's an example using predict.probit.f for some mortgage data found here: https://www.dropbox.com/s/2cdinsxfd59yh83/hmda_sw.dta?dl=0.  "deny" is coded 0/1 (1 if the application is denied); pi_ratio is a measure of an applicant's "payment to income ratio", and "black" is coded 0/1:
  #mort.probit <- glm(deny ~ pi_rat + black + pi_rat*black)
  #pred.df <- data.frame("pi_rat" = rep(seq(.2,.5,.01),2), "black" = c(rep(0,62),rep(1,62)))
  #predict.probit.f(mort.probit,pred.df)
  #predict.plot.f(mort.probit.pred.df,mort.probit.pred.df$pi_rat, mort.probit.pred.df$black, "Race","PI_ratio","Prob of Deny")
  
  ##If you don't have a moderating variable (Z), then simply leave the spots for Z and Zname in the function blank -- e.g.: predict.plot.f(mort.probit.pred.df,mort.probit.pred.df$pi_rat,,,"PI_ratio","Prob of Deny")
  
  ##Due to my personal preference, this function does not automatically plot lines between points.  To add lines, simply make this plot an object (e.g. plot <- predict.plot.f(...)), and then add a line layer: 
  #plot + geom_line(aes(group=1))
  #To add lines by group: plot + geom_line(aes(group=as.factor(variablename)))
}

###############################################
###############################################
predict.lm.f <- function(mod,predict.df,rob,cluster,df_correction){
  
  ##Written by Joshua Gubler ~  http://joshuagubler.com
  ##Last updated on 3 November 2016
  ##This organizes the data in a format so it can be automatically graphed by my predict.plot and/or marginal effects functions (similar to the "margins" command in Stata)
  ##Note, that for this function to work well, you must input factor variables with more than two levels individually (as individual dummies).
  
  ##mod = the object for the model you estimated
  ##predict.df = the dataframe that indicates the values you want to use to predict change in Y.  Same as in the default R "predict" function.
  ##rob = include this if you want robust standard errors; exclude if you do not.
  ##cluster = include the name of the cluster variable (with full .df$varname path) for these (and don't include rob earlier)
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "T".  Otherwise, type "F" and these levels will be counted against your degrees of freedom (like the "areg" option in Stata)
  
  #Example code:
  #Huber-White Robust standard errors: predict.lm.f(test.lm,pred.df,rob)
  #For clustered errors with no dfc correction: predict.lm.f(test.lm,pred.df,df$clustervar)
  #For clustered errors with dfc correction: predict.lm.f(test.lm,pred.df,df$clustervar,T)
  
  require(sandwich)
  require(multiwayvcov)
  tt <- terms(mod)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=(predict.df))
  fit <- as.vector(m.mat %*% mod$coef)
  
  if(missing(cluster)){
    if(missing(rob)){
      varcov <- vcov(mod)
      se.fit <- sqrt(diag(m.mat%*%varcov%*%t(m.mat)))
      ci.lower95 <- fit - qt(.975,mod$df.residual)*se.fit
      ci.upper95 <- fit + qt(.975,mod$df.residual)*se.fit
    }
    else{
      ##To generate the robust covariance matrix
      varcov <- vcovHC(mod,"HC1")
      se.fit <- sqrt(diag(m.mat%*%varcov%*%t(m.mat)))
      ci.lower95 <- fit - qt(.975,mod$df.residual)*se.fit
      ci.upper95 <- fit + qt(.975,mod$df.residual)*se.fit
    }
  }
  else {
    varcov <- cluster.vcov(mod, cluster, df_correction = df_correction)
    se.fit <- sqrt(diag(m.mat%*%varcov%*%t(m.mat)))
    ci.lower95 <- fit - qt(.975,mod$df.residual)*se.fit
    ci.upper95 <- fit + qt(.975,mod$df.residual)*se.fit
  }
  
  pred.df <- data.frame(m.mat,predicted.value=fit,se=se.fit,ci.lower95=ci.lower95,ci.upper95=ci.upper95)[,-1]
  nm <-deparse(substitute(mod))
  mdlname <- paste(nm,"pred.df",sep=".")
  assign(mdlname,pred.df,envir = .GlobalEnv)
}


###############################################
###############################################
robustse.f <- function(model, cluster, df_correction) {
  ## Huber-White heteroskedasticity-robust standard error calculation and table generation code for lm and glm models in R.
  ##Written by Joshua Gubler ~  http://joshuagubler.com.  Note that the second half of this function is just a wrapper for the excellent "multiwaycov" package here: https://cran.r-project.org/web/packages/multiwayvcov/multiwayvcov.pdf .  Love the authors of that package...
  ##Last updated: 3 July 2017  
  
  #model = the model you estimated, now calculated with robust standard errors
  #cluster = the name of the variable on which you will cluster. Put a tilda in front of it (e.g. ~ state).  If you don't put in a cluster, you will simply get huber-white robust errors.
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "F".  Otherwise, type "T" and these levels will be counted against your degrees of freedom
  
  require(sandwich)
  require(lmtest)
  require(multiwayvcov)
  if(missing(cluster)) {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    model$se <- coeftest(model, vcov=vcovHC(model,"HC1"))[,2]
    model$vcovHC <- vcovHC(model,"HC1")
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcov=vcovHC(model,"HC1"))
  } else {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    vcovCL <- cluster.vcov(model, cluster, df_correction = df_correction)
    model$vcovCL <- vcovCL
    modelname <- paste(name,"clustrob",sep=".")
    model$se <- coeftest(model, vcovCL)[,2]
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcovCL)
  }
}

###############################################
###############################################
TwowayME.f <- function(M,X,Z,xlab,ylab,level,rob,cluster,df_correction,hist)
{
  ## A function to generate 2-way marginal effects plots in R.
  ## Written by Joshua Gubler ~  http://joshuagubler.com
  ## Last modified: 3 July 2017
  ## Variables must be in the following order: y = x z (control variables
  ## here) xz. The model can include as many control variables as you need.
  ## M = an object of type "lm," "glm," or other estimation -- i.e. the object
  ## that contains the regression estimation you seek to plot.
  ## X = the variable whose effect on Y you seek to plot
  ## Z = the moderating variable (will be positioned on the X-axis of the
  ## plot)
  ## xlab = label for x-axis (in quotes)
  ## ylab = label for y-axis (in quotes)
  ## level = to set the confidence level. Two options (don't put these in
  ## quotes): 95, 90.  If you do not specify either option, the confidence
  ## intervals will not be correct.
  ## rob: if you desire huber-white robust standard errors
  ##cluster: the name of the variable you desire to cluster on, with a tilda in front (e.g.: ~ state).  Don't include rob when including this.
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "F".  Otherwise, type "T" and these levels will be counted against your degrees of freedom
  ## hist: if you indicate histogram, you will get one showing the distribution of the varible on the x-axis.  If you'll leave it blank, you'll get a rug plot for the same.
  
  #Example code:
  #Without robust standard errors; including histogram: TwowayME.f(test.lm,df$effect,df$moderator,"Levels of Moderator","ME of effect on Y",95,,,,hist)
  #With robust standard errors; with rugplot: TwowayME.f(test.lm,df$effect,df$moderator,"Levels of Moderator","ME of effect on Y",95,rob,,,)
  #With cluster-robust standard errors; no dfc correction; with histogram: TwowayME.f(test.lm,df$effect,df$moderator,"Levels of Moderator","ME of effect on Y",95,,~clustervar,T,hist)
  
  require(multiwayvcov)
  S <- summary(M)
  N <- c(1:20)
  
  ## 20 equally-spaced values for the moderating variable
  zmin <- rep(min(Z, na.rm = TRUE), 20)
  zmax <- rep(max(Z, na.rm = TRUE), 20)
  Znew <- (((N - 1) / (20 - 1)) * (zmax - zmin)) + zmin
  
  ## Grab elements of coefficient and vcov matrix
  H <- head(S$coefficients, 3)
  T <- tail(S$coefficients, 1)
  b <- rbind(H, T)
  if(missing(cluster)){
    if(missing(rob)){Vcov <- vcov(M)}
    else{
      Vcov <- vcovHC(M,"HC1")
    }
  } else{
    Vcov <- cluster.vcov(M, cluster, df_correction = df_correction)
  }
  Vcov <- as.data.frame(Vcov)
  Vcov1 <- Vcov[, c(1:3)]
  Vcov2 <- Vcov[, -c(0:0 - length(Vcov))]
  Vcov <- cbind(Vcov1, Vcov2)
  Vh <- head(Vcov, 3)
  Vt <- tail(Vcov, 1)
  V <- rbind(Vh, Vt)
  b1 <- b[2, 1]
  b3 <- b[4, 1]
  varb1 <- V[2, 2]
  varb3 <- V[4, 4]
  covb1b3 <- V[4, 2]
  
  ## Calculate ME values
  conb <- b1 + b3 * Znew
  
  ## Calculate standard errors
  conse <- sqrt(varb1 + varb3 * (Znew^2) + 2 * covb1b3 * Znew)
  
  ## Upper and lower CIs
  ci <- NA
  ci[level == 95] <- qt(.975,M$df.residual)
  ci[level == 90] <- qt(.95,M$df.residual)
  a = ci * conse
  upper = conb + a
  lower = conb - a
  
  ##Graph the results
  if(missing(hist)){
    plot(c(Znew,Znew,Znew), c(a,upper,lower), type="n",xlab=xlab,ylab=ylab)+rug(jitter(Z))
    lines(Znew,conb,col="black")
    lines(Znew,upper,col="black",lty=2)
    lines(Znew,lower,col="black",lty=2)
    abline(h=0)
  }else{
    hist(Z, axes=F, xlab="", ylab="",main="", col="light gray")
    par(new=TRUE)
    plot(c(Znew,Znew,Znew), c(a,upper,lower), type="n",xlab=xlab,ylab=ylab)
    lines(Znew,conb,col="black")
    lines(Znew,upper,col="black",lty=2)
    lines(Znew,lower,col="black",lty=2)
    abline(h=0)  
  }   
}




###############################################
###############################################
TwowayDME.f <- function(M,X,Z,xlab,ylab,level,rob,cluster,df_correction,Low,High)
{ 
  ## A function to generate 2-way marginal effects plots in R with a dichotomous modifying variable 
  ## Written by Joshua Gubler ~  http://joshuagubler.com 
  ## Last modified: 3 July 2017 
  ## Variables must be in the following order: y = x z (control variables here) xz .  The model can include as many control variables as you need.
  ## M = an object of type "lm," "glm," or other estimation -- i.e. the object that contains the regression estimation you seek to plot
  ## X = the variable whose effect on Y you seek to plot
  ## Z = the moderating variable (will be positioned on the X-axis of the plot)
  ## xlab = label for x-axis (in quotes)
  ## ylab = label for y-axis (in quotes)
  ## level = to set the confidence level.  Two options (don't put these in quotes): 95, 90.  If you do not specify either option, the confidence intervals will not be correct.
  ## rob: if you desire huber-white robust standard errors
  ##cluster: the name of the variable you desire to cluster on, with a tilda in front (e.g.: ~ state).  Don't include rob when including this.
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "F".  Otherwise, type "T" and these levels will be counted against your degrees of freedom
  ## Low/High: Here you add (in quotes) the labels for your dichotomous modifying variable
  
  #Example code:
  #Without robust standard errors: TwowayME.f(test.lm,df$effect,df$moderator,"Levels of Moderator","ME of effect on Y",95,,,,"Low","High")
  #With robust standard errors: TwowayME.f(test.lm,df$effect,df$moderator,"Levels of Moderator","ME of effect on Y",95,rob,,,"Low","High")
  #With cluster-robust standard errors; no dfc correction: TwowayME.f(test.lm,df$effect,df$moderator,"Levels of Moderator","ME of effect on Y",95,,~clustervar,T,"Low","High")
  
  require(multiwayvcov)
  S <- summary(M)
  
  ##To create the high/low dichotomous variable
  Z0 <- quantile(Z,   0, na.rm=TRUE) 
  Z1 <- quantile(Z,   1, na.rm=TRUE)
  
  ## Grab elements of coefficient and vcov matrix
  require(sandwich)
  H <- head(S$coefficients,3)
  T <- tail(S$coefficients,1)
  b <- rbind(H,T)
  if(missing(cluster)){
    if(missing(rob)){Vcov <- vcov(M)}
    else{
      Vcov <- vcovHC(M,"HC1")
    }
  } else{
    Vcov <- cluster.vcov(M, cluster, df_correction = df_correction)
  }
  Vcov <- as.data.frame(Vcov)
  Vcov1 <- Vcov[,c(1:3)]
  Vcov2 <- Vcov[,-c(0:0-length(Vcov))]
  Vcov <- cbind(Vcov1,Vcov2)
  Vh <- head(Vcov,3)
  Vt <- tail(Vcov,1)
  V <- rbind(Vh,Vt)
  b1 <- b[2,1]
  b3 <- b[4,1]
  varb1 <- V[2,2]
  varb3 <- V[4,4]
  covb1b3 <- V[4,2]
  
  ## Calculate ME values
  conb0 <- b1+b3*Z0
  conb1 <- b1+b3*Z1
  
  ## Calculate standard errors    
  conse0 <- sqrt(varb1 + varb3*(Z0^2) + 2*covb1b3*Z0)
  conse1 <- sqrt(varb1 + varb3*(Z1^2) + 2*covb1b3*Z1)
  
  ## Upper and lower CIs
  ci <- NA
  ci[level == 95] <- qt(.975,M$df.residual)
  ci[level == 90] <- qt(.95,M$df.residual)
  a0 = ci*conse0
  a1 = ci*conse1
  upper0 = conb0 + a0
  lower0 = conb0 - a0
  upper1 = conb1 + a1
  lower1 = conb1 - a1
  
  ##Graph the results
  require(ggplot2)
  margplot <- qplot(
    c(Low,High),
    c(conb0,conb1), 
    xlab=xlab,
    ylab=ylab
  ) + 
    geom_point() +
    theme_bw()
  
  margplot + geom_linerange(aes(ymin=c(lower0,lower1),ymax=c(upper0,upper1))) + geom_abline(intercept=0,slope=0)
}


####To load the data:####
#Update this to match the file directories on your local computer
pop6.df <- read.dta13("Framing and Blame Attribution in Populist Rhetoric_Study1.dta", convert.factors=F)
pop7.df <- read.dta13("Framing and Blame Attribution in Populist Rhetoric_Study2.dta",convert.factors=F)

####A few small coding points before the analyses####
#Study 1
#In the analyses that follow, we use a compliance variable to exclude those who did not give responses to the open-ended items.
#This is based on if people have responses on the open-ended coding
pop6.df$comply=0
pop6.df$comply[!is.na(pop6.df$goodpeople)|!is.na(pop6.df$badactor)|!is.na(pop6.df$badelite)]=1

#This is a more restrictive comply variable, based on the missingness of open-ended AND timing
pop6.df$comply2=pop6.df$comply
pop6.df$comply2[pop6.df$Q46_3<9|pop6.df$Q50_3<9|pop6.df$Q54_3<9|pop6.df$Q58_3<9]=0

#Study 2
#We need to make a 3 point version of the PID variable
pop7.df$PID <- NA
pop7.df$PID[pop7.df$PID3==1] <-1
pop7.df$PID[pop7.df$PID3==2] <-2
pop7.df$PID[pop7.df$PID3==3] <-3

#Reorder ideology variable in a sequential way from liberal to conservative
pop7.df$IDEO3_r[pop7.df$IDEO3==1]=1
pop7.df$IDEO3_r[pop7.df$IDEO3==3]=2
pop7.df$IDEO3_r[pop7.df$IDEO3==2]=3

#We need to make a difference variable for the feeling thermometer analyses
pop7.df$thermdiff <- pop7.df$Q96_1 - pop7.df$Q102_1
pop7.df$bthermdiff <- pop7.df$Q103_1 - pop7.df$Q102_1
pop7.df$bthermdiff2 <- pop7.df$Q103_1 - pop7.df$Q96_1



###########################################
####Analyses from main paper start here####
###########################################

####Study 1 analyses####
###Demographics (not in a table)
table(pop6.df$IDEO3) #1 is liberal, 2 is moderate, 3 is conservative, 4 is don't know
(422/(422+132+198+6))
table(pop6.df$SEX) #2 is female, 1 is male
(388/(388+413))
median(pop6.df$AGE, na.rm=T)
median(pop6.df$EDUC, na.rm=T) # 1 is Less than HS, 2 is HS/GED, 3 is Some college, 4 is 2 year degree,
                              # 5 is a 4 year degree, 6 is a Masters, 7 is a doctoral degree, 8 is a professional degree

#Correlation between populism and authoriatianism (footnote)
attach(pop7.df)
cor.test(IDEO3_r, POP_index5, alternative="two.sided")
cor.test(AUTH_score,POP_index5,use="complete.obs")
#We find similar things if we use spearman's of kendall's methods of correlations.
detach(pop7.df)

#Same values for study 1: 
attach(pop6.df)
cor(IDEO3,POP_index5,use="complete.obs")
detach(pop6.df)

#Amounts of populism (not in a table)
mean(pop6.df$POP_index5, na.rm=T) #5.3
median(pop6.df$POP_index5, na.rm=T) #5.2
sd(pop6.df$POP_index5, na.rm=T)#0.9

####Table 1####
#To make the table, we need a version where 0.5 and 1 are grouped together. Here are those commands:
pop6.df$badactor2[pop6.df$badactor>=0.5]=1
pop6.df$badactor2[pop6.df$badactor<0.5]=0
pop6.df$badelite2[pop6.df$badelite>=0.5]=1
pop6.df$badelite2[pop6.df$badelite<0.5]=0
pop6.df$goodpeople2[pop6.df$goodpeople>=0.5]=1
pop6.df$goodpeople2[pop6.df$goodpeople<0.5]=0

#Now the actual values in the table:
attach(pop6.df)
#Responsible actor
table(badactor2[comply==1])
(581)/(193+581) #75.1%
#Conspiring elite
table(badelite2[comply==1])
(483)/(291+483) #62.4%
#Good people
table(goodpeople2[comply==1])
(393)/(383+393) #50.6%
#Populism
table(populism[comply==1])
(276)/(276+501) #35.5%
detach(pop6.df)

#Cohen's kappa, located in a footnote
#Conspiring elite
agree(subset(pop6.df, select=c(badelite_n, badelite_r)), tolerance=0)
kappa2(subset(pop6.df, select=c(badelite_n, badelite_r)),weight="unweighted") 
#Good people
agree(subset(pop6.df, select=c(goodpeople_n, goodpeople_r)), tolerance=0)
kappa2(subset(pop6.df, select=c(goodpeople_n, goodpeople_r)),weight="unweighted") 

##Footnote mentions imbalance checks. This code performs them for study 1 and study 2
#Study 1
xbal6.df <- pop6.df[complete.cases(pop6.df$TREATMENT>=0),]
balance <- xBalance(TREATMENT ~ AGE + r_INCOME + EDUC + WHITE + MALE + PID3 + IDEO3, data = xbal6.df)
plot(balance)

#Study 2
xbal7.df <- pop7.df[complete.cases(pop7.df$TREAT_2>=0),]
balance <- xBalance(TREAT_2 ~ AGE + INCOME + EDUC + WHITE + MALE + PID + IDEO3, data = xbal7.df)
plot(balance)

####Figure 1####
#These are for the models behind the figure
attach(pop6.df)
s6open1.glm <- glm(populism ~ TREATMENT, subset=comply==1,family=binomial("logit"))
s6open.glm <- glm(populism ~ TREATMENT + POP2_r + TREATMENT*POP2_r, subset=comply==1,family=binomial("logit"))
summary(s6open1.glm)
summary(s6open.glm)

#The plotting of Figure 1
pred6.df <- data.frame("TREATMENT" = c(0,1,0,1), "POP2_r" = c(0,0,1,1))
predict.logit.f(s6open.glm,pred6.df)

s6open.glm.pred.df$POP2_r <- factor(s6open.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(s6open.glm.pred.df, 
       aes(c("Situational","Dispositional","Situational","Dispositional"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=s6open.glm.pred.df$ci.lower95,ymax=s6open.glm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 1: Probability of expressing populist views post-treatment (Study 1)", y = "Probability of a Populist Open Response") + 
  theme(strip.background = element_rect(color = 'white', fill = 'white'),axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure1.pdf", width=7, height=5,family="Times")

#p-values reported in text:
#Low populists
t.test(populism[TREATMENT==0 & comply==1&POP2_r==0],populism[TREATMENT==1 & comply==1&POP2_r==0]) 
#High populists
t.test(populism[TREATMENT==0 & comply==1&POP2_r==1],populism[TREATMENT==1 & comply==1&POP2_r==1]) 
#Comparing dispositional low group to high populist groups
t.test(populism[TREATMENT==1 & comply==1&POP2_r==0],populism[TREATMENT==1 & comply==1&POP2_r==1]) 
t.test(populism[TREATMENT==1 & comply==1&POP2_r==0],populism[TREATMENT==0 & comply==1&POP2_r==1]) 
detach(pop6.df)

####Study 2 analyses####
###Demographics (not in a table)
table(pop7.df$IDEO3_r) #1 is liberal, 2 is moderate, 3 is conservative, 4 is don't know
(226/(226+286+328))
(328/(226+286+328))
table(pop7.df$MALE) #0 is female, 1 is male
435/(435+405)
median(pop7.df$AGE, na.rm=T) # 1 is 18-24, 2 is 25-34, 3 is 35-44, 4 is 45-54, 5 is 55-64, 6 is 65+
median(pop7.df$EDUC, na.rm=T) # 1 is Less than HS, 2 is HS/GED, 3 is Some college, 4 is 2 year degree,
# 5 is a 4 year degree, 6 is a Masters, 7 is a doctoral degree, 8 is a professional degree

mean(pop7.df$POP_index5, na.rm=T) #5.4
median(pop7.df$POP_index5, na.rm=T) #5.4
sd(pop7.df$POP_index5, na.rm=T)#1.1

####Table 2####
#First we need a binary version of the coding variables for the table:
pop7.df$badactor2[pop7.df$badactor>=0.5]=1
pop7.df$badactor2[pop7.df$badactor<0.5]=0
pop7.df$badelite2[pop7.df$badelite>=0.5]=1
pop7.df$badelite2[pop7.df$badelite<0.5]=0
pop7.df$goodpeople2[pop7.df$goodpeople>=0.5]=1
pop7.df$goodpeople2[pop7.df$goodpeople<0.5]=0

###These commands produce table 2
attach(pop7.df)
####Whole sample#####
table(badactor2[comply==1])
(384)/(384+154) 
table(badelite2[comply==1])
(265)/(265+272) 
table(goodpeople2[comply==1])
(164)/(164+373) 
table(populism[comply==1&!is.na(TREAT_2)])
(111)/(111+427) 
detach(pop7.df)

#Cohen's kappa, located in a footnote
#First we need a numeric version of the coding variables; the NA coding is messing this up
pop7.df$goodpeople_n2[pop7.df$goodpeople_n=="1"]=1
pop7.df$goodpeople_n2[pop7.df$goodpeople_n=="0"]=0
pop7.df$goodpeople_r2[pop7.df$goodpeople_r=="1"]=1
pop7.df$goodpeople_r2[pop7.df$goodpeople_r=="0"]=0
pop7.df$badelite_n2[pop7.df$badelite_n=="1"]=1
pop7.df$badelite_n2[pop7.df$badelite_n=="0"]=0
pop7.df$badelite_r2[pop7.df$badelite_r=="1"]=1
pop7.df$badelite_r2[pop7.df$badelite_r=="0"]=0
#Conspiring elite
agree(subset(pop7.df, select=c(badelite_n2, badelite_r2)), tolerance=0)
kappa2(subset(pop7.df, select=c(badelite_n2, badelite_r2)),weight="unweighted") 
#Good people
agree(subset(pop7.df, select=c(goodpeople_n2, goodpeople_r2)), tolerance=0)
kappa2(subset(pop7.df[], select=c(goodpeople_n2, goodpeople_r2)),weight="unweighted") 

####Figure 2####
##These are the models behind the figure
attach(pop7.df)
s7open1.glm <- glm(populism ~ TREAT_2, subset=comply==1,family=binomial("logit"))
s7open.glm <- glm(populism ~ TREAT_2 + POP2_r + TREAT_2*POP2_r, subset=comply==1,family=binomial("logit"))
summary(s7open1.glm)
summary(s7open.glm)

##And now to plot our results:
pred7.df <- data.frame("TREAT_2" = c(0,1,0,1), "POP2_r" = c(0,0,1,1))
predict.logit.f(s7open.glm,pred7.df)

s7open.glm.pred.df$POP2_r <- factor(s7open.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(s7open.glm.pred.df, 
       aes(c("Situational","Dispositional","Situational","Dispositional"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=s7open.glm.pred.df$ci.lower95,ymax=s7open.glm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 2: Probability of expressing populist views post-treatment (Study 2)", y = "Probability of a Populist Response") +
  theme(strip.background = element_rect(color = 'white', fill = 'white'))+
  theme(axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure2.pdf", width=7, height=5,family="Times")  

detach(pop7.df)
#p-values reported in text
attach(pop7.df)
t.test(populism[TREAT_2==0 & comply==1&POP2_r==0],populism[TREAT_2==1 & comply==1&POP2_r==0]) 
t.test(populism[TREAT_2==0 & comply==1&POP2_r==1],populism[TREAT_2==1 & comply==1&POP2_r==1]) 
detach(pop7.df)

####Figures 3.1 and 3.2####
##Clinton:
#Models behind the figure
attach(pop7.df)
summary(ClintonAll.glm <- glm(Clinton ~ as.factor(TREATMENT) + POP2_r + as.factor(TREATMENT)*POP2_r,subset=comply==1,family=binomial("logit")))

pred7All.df <- data.frame("TREATMENT" = c(0,1,2,0,1,2), "POP2_r" = c(0,0,0,1,1,1))
predict.logit.f(ClintonAll.glm,pred7All.df)

summary(TrumpAll.glm <- glm(Trump ~ as.factor(TREATMENT) + POP2_r + as.factor(TREATMENT)*POP2_r,subset=comply==1,family=binomial("logit")))

pred7All.df <- data.frame("TREATMENT" = c(0,1,2,0,1,2), "POP2_r" = c(0,0,0,1,1,1))
predict.logit.f(TrumpAll.glm,pred7All.df)

TrumpAll.glm.pred.df$POP2_r <- factor(TrumpAll.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

#Plotting the Clinton Panes
ClintonAll.glm.pred.df$POP2_r <- factor(ClintonAll.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(ClintonAll.glm.pred.df, 
       aes(c("Pure Control","Dispositional","Situational","Pure Control","Dispositional","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=ClintonAll.glm.pred.df$ci.lower95,ymax=ClintonAll.glm.pred.df$ci.upper95),linetype=c(2,1,1,2,1,1)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 3.1: Probability of voting for Clinton, by treatment", y = "Predicted Probability of a Vote for Clinton") + 
  scale_x_discrete(limits = c("Dispositional","Situational","Pure Control")) +
  theme(strip.background = element_rect(color = 'white', fill = 'white'))+
  theme(axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure3.1.pdf", width=7, height=5,family="Times")


##Plotting the Trump panes
ggplot(TrumpAll.glm.pred.df, 
       aes(c("Pure Control","Dispositional","Situational","Pure Control","Dispositional","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=TrumpAll.glm.pred.df$ci.lower95,ymax=TrumpAll.glm.pred.df$ci.upper95),linetype=c(2,1,1,2,1,1)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 3.2: Probability of voting for Trump, by treatment", y = "Predicted Probability of a Vote for Trump") +
  scale_x_discrete(limits = c("Dispositional","Situational","Pure Control")) +
  theme(strip.background = element_rect(color = 'white', fill = 'white'))+
  theme(axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure3.2.pdf", width=7, height=5,family="Times")

##Clinton p-values
t.test(Clinton[TREAT_2==0 & POP2_r==0 & comply==1],Clinton[TREAT_2==1 & POP2_r==0 & comply==1]) # p=.11
t.test(Clinton[TREAT_2==0 & POP2_r==1 & comply==1],Clinton[TREAT_2==1 & POP2_r==1 & comply==1]) # p=.29
t.test(Clinton[TREATMENT==0 & POP2_r==0 & comply==1],Clinton[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.03
t.test(Clinton[TREATMENT==0 & POP2_r==1 & comply==1],Clinton[TREATMENT==1 & POP2_r==1 & comply==1]) # p=.64
t.test(Clinton[TREATMENT==0 & POP2_r==0 & comply==1],Clinton[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.56
t.test(Clinton[TREATMENT==0 & POP2_r==1 & comply==1],Clinton[TREATMENT==2 & POP2_r==1 & comply==1]) # p=.12

##Trump p-values
t.test(Trump[TREAT_2==0 & POP2_r==0 & comply==1],Trump[TREAT_2==1 & POP2_r==0 & comply==1]) # p=.02
t.test(Trump[TREAT_2==0 & POP2_r==1 & comply==1],Trump[TREAT_2==1 & POP2_r==1 & comply==1]) # p=.74
t.test(Trump[TREATMENT==0 & POP2_r==0 & comply==1],Trump[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.01
t.test(Trump[TREATMENT==0 & POP2_r==1 & comply==1],Trump[TREATMENT==1 & POP2_r==1 & comply==1]) # p=.50
t.test(Trump[TREATMENT==0 & POP2_r==0 & comply==1],Trump[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.81
t.test(Trump[TREATMENT==0 & POP2_r==1 & comply==1],Trump[TREATMENT==2 & POP2_r==1 & comply==1]) # p=.31
detach(pop7.df)


####Figure 4####
#Models behind the figures
attach(pop7.df)
DiffAll.lm <- lm(thermdiff ~ as.factor(TREATMENT) + POP2_r + as.factor(TREATMENT)*POP2_r,subset=comply==1)
robustse.f(DiffAll.lm)

pred7AT.df <- data.frame("TREATMENT" = c(0,1,2,0,1,2), "POP2_r" = c(0,0,0,1,1,1))
predict.lm.f(DiffAll.lm,pred7AT.df,rob)

DiffAll.lm.pred.df$POP2_r <- factor(DiffAll.lm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(DiffAll.lm.pred.df, 
       aes(c("Pure Control","Dispositional","Situational","Pure Control","Dispositional","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=DiffAll.lm.pred.df$ci.lower95,ymax=DiffAll.lm.pred.df$ci.upper95),linetype=c(2,1,1,2,1,1)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 4: Trump-Clinton feeling thermometer difference, post-treatment", y = "Predicted Trump-Clinton Difference") +
  scale_x_discrete(limits = c("Dispositional","Situational","Pure Control")) +
  theme(strip.background = element_rect(color = 'white', fill = 'white'))+
  theme(axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure4.pdf", width=7, height=5,family="Times")

t.test(thermdiff[TREAT_2==0 & POP2_r==0 & comply==1],thermdiff[TREAT_2==1 & POP2_r==0 & comply==1]) # p=.01
t.test(thermdiff[TREATMENT==0 & POP2_r==0 & comply==1],thermdiff[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.01
t.test(thermdiff[TREAT_2==0 & POP2_r==1 & comply==1],thermdiff[TREAT_2==1 & POP2_r==1 & comply==1]) # p=.54
t.test(thermdiff[TREATMENT==0 & POP2_r==1 & comply==1],thermdiff[TREATMENT==1 & POP2_r==1 & comply==1]) # p=.86
t.test(thermdiff[TREATMENT==0 & POP2_r==0 & comply==1],thermdiff[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.93
t.test(thermdiff[TREATMENT==0 & POP2_r==1 & comply==1],thermdiff[TREATMENT==2 & POP2_r==1 & comply==1]) # p=.41

detach(pop7.df)

####Figures 5.1 and 5.2 ####
##Models behind the figure
attach(pop7.df)
DiffSC.lm <- lm(bthermdiff ~ as.factor(TREATMENT) + POP2_r + as.factor(TREATMENT)*POP2_r,subset=comply==1)
robustse.f(DiffSC.lm)

pred7AT.df <- data.frame("TREATMENT" = c(0,1,2,0,1,2), "POP2_r" = c(0,0,0,1,1,1))
predict.lm.f(DiffSC.lm,pred7AT.df,rob)

DiffSC.lm.pred.df$POP2_r <- factor(DiffSC.lm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

DiffST.lm <- lm(bthermdiff2 ~ as.factor(TREATMENT) + POP2_r + as.factor(TREATMENT)*POP2_r,subset=comply==1)
robustse.f(DiffST.lm)

pred7AT.df <- data.frame("TREATMENT" = c(0,1,2,0,1,2), "POP2_r" = c(0,0,0,1,1,1))
predict.lm.f(DiffST.lm,pred7AT.df,rob)

DiffST.lm.pred.df$POP2_r <- factor(DiffST.lm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

#Plotting the Sanders-Clinton pane
ggplot(DiffSC.lm.pred.df, 
       aes(c("Pure Control","Dispositional","Situational","Pure Control","Dispositional","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=DiffSC.lm.pred.df$ci.lower95,ymax=DiffSC.lm.pred.df$ci.upper95),linetype=c(2,1,1,2,1,1)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 5.1: Sanders-Clinton feeling therm. difference, post-treatment", y = "Predicted Sanders-Clinton Difference") +
  scale_x_discrete(limits = c("Dispositional","Situational","Pure Control")) +
  theme(strip.background = element_rect(color = 'white', fill = 'white'))+
  theme(axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure5.1.pdf", width=7, height=5,family="Times")

#Plotting the Sanders-Trump pane
ggplot(DiffST.lm.pred.df, 
       aes(c("Pure Control","Dispositional","Situational","Pure Control","Dispositional","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=DiffST.lm.pred.df$ci.lower95,ymax=DiffST.lm.pred.df$ci.upper95),linetype=c(2,1,1,2,1,1)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "Figure 5.2: Sanders-Trump feeling therm. difference, post-treatment", y = "Predicted Sanders-Trump Difference") +
  scale_x_discrete(limits = c("Dispositional","Situational","Pure Control")) +
  theme(strip.background = element_rect(color = 'white', fill = 'white'))+
  theme(axis.title.x = element_text(colour="black",size=15))

ggsave("/PutPathHere/Figure5.2.pdf", width=7, height=5,family="Times")

#Sanders - Clinton p-values
t.test(bthermdiff[TREAT_2==0 & POP2_r==0 & comply==1],bthermdiff[TREAT_2==1 & POP2_r==0 & comply==1]) # p=.05
t.test(bthermdiff[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.03
t.test(bthermdiff[TREAT_2==0 & POP2_r==1 & comply==1],bthermdiff[TREAT_2==1 & POP2_r==1 & comply==1]) # p=.81
t.test(bthermdiff[TREATMENT==0 & POP2_r==1 & comply==1],bthermdiff[TREATMENT==1 & POP2_r==1 & comply==1]) # p=.60
t.test(bthermdiff[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.70
t.test(bthermdiff[TREATMENT==0 & POP2_r==1 & comply==1],bthermdiff[TREATMENT==2 & POP2_r==1 & comply==1]) # p=.42

##Sanders-Trump p-values
t.test(bthermdiff2[TREAT_2==0 & POP2_r==0 & comply==1],bthermdiff2[TREAT_2==1 & POP2_r==0 & comply==1]) # p=.14
t.test(bthermdiff2[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff2[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.19
t.test(bthermdiff2[TREAT_2==0 & POP2_r==1 & comply==1],bthermdiff2[TREAT_2==1 & POP2_r==1 & comply==1]) # p=.56
t.test(bthermdiff2[TREATMENT==0 & POP2_r==1 & comply==1],bthermdiff2[TREATMENT==1 & POP2_r==1 & comply==1]) # p=.85
t.test(bthermdiff2[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff2[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.84
t.test(bthermdiff2[TREATMENT==0 & POP2_r==1 & comply==1],bthermdiff2[TREATMENT==2 & POP2_r==1 & comply==1]) # p=.68
detach(pop7.df)


###########################################
####Analyses from Appendices start here####
###########################################

####Appendix A: Populist Attitudes Index####

##Table A.1, populism items in study 1
attach(pop6.df)
mean(POP_1, na.rm=T)
sd(POP_1, na.rm=T)
mean(POP_2, na.rm=T)
sd(POP_2, na.rm=T)
mean(POP_3, na.rm=T)
sd(POP_3, na.rm=T)
mean(POP_11, na.rm=T)
sd(POP_11, na.rm=T)
mean(POP_12, na.rm=T)
sd(POP_12, na.rm=T)
mean(POP_index5, na.rm=T)
sd(POP_index5, na.rm=T)
detach(pop6.df)
#Factor loadings
(principal(na.omit(subset(pop6.df, select=c(POP_1, POP_2, POP_3, POP_11, POP_12))), nfactors=1, rotate="varimax"))
alpha(subset(pop6.df, select=c(POP_1, POP_2, POP_3, POP_11, POP_12)))

##Table A.2, populism items in study 2
attach(pop7.df)
mean(POP_1, na.rm=T)
sd(POP_1, na.rm=T)
mean(POP_2, na.rm=T)
sd(POP_2, na.rm=T)
mean(POP_3, na.rm=T)
sd(POP_3, na.rm=T)
mean(POP_11, na.rm=T)
sd(POP_11, na.rm=T)
mean(POP_12, na.rm=T)
sd(POP_12, na.rm=T)
mean(POP_index5, na.rm=T)
sd(POP_index5, na.rm=T)
detach(pop7.df)
#Factor loadings
(principal(na.omit(subset(pop7.df, select=c(POP_1, POP_2, POP_3, POP_11, POP_12))), nfactors=1, rotate="varimax"))
factanal(na.omit(subset(pop7.df, select=c(POP_1, POP_2, POP_3, POP_11, POP_12))), factors=1)
alpha(subset(pop7.df, select=c(POP_1, POP_2, POP_3, POP_11, POP_12)))

#Populism by ideological categories (study 2)
mean(pop7.df$POP_index5[pop7.df$IDEO3_r==1], na.rm=T) #Liberals
mean(pop7.df$POP_index5[pop7.df$IDEO3_r==2], na.rm=T) #Moderates
mean(pop7.df$POP_index5[pop7.df$IDEO3_r==3], na.rm=T) #Conservatives
#Correlation
cor.test(pop7.df$POP_index5, pop7.df$IDEO3_r, use="complete.obs")

#Correlation with authoritarianism
cor.test(pop7.df$POP_index5, pop7.df$AUTH_score, use="complete.obs")

##Figure A.1: Distribution of populist attitudes, study 1
hist(pop6.df$POP_index5, xlab= "Populism index", main=NULL) 
##Figure A.2: Distribution of populist attitudes, study 2
hist(pop7.df$POP_index5, xlab= "Populism index", main=NULL)

####Appendix C: Replication of key results####
##Table C.1 Models, study 1
attach(pop6.df)
s6open1.glm <- glm(populism ~ TREATMENT, subset=comply==1,family=binomial("logit"))
s6open.glm <- glm(populism ~ TREATMENT + POP2_r + TREATMENT*POP2_r, subset=comply==1,family=binomial("logit"))
summary(s6open1.glm)
summary(s6open.glm)
detach(pop6.df)

stargazer(s6open1.glm,s6open.glm,
          covariate.labels = c("Intercept","Treatment (Dispositional = 1)","Pre-Treatment Populism Levels","Treatment*Pre-treatment Pop."),
          type="html",
          style = "apsr",
          label="tab:study1",
          title="The effects of the treatment and pre-treatment populism on the likelihood of a populist open response (Study 1)",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          out="Local file path here.htm"
)

##Table C.2 Models, study 2
attach(pop7.df)
s7open1.glm <- glm(populism ~ TREAT_2, subset=comply==1,family=binomial("logit"))
s7open.glm <- glm(populism ~ TREAT_2 + POP2_r + TREAT_2*POP2_r, subset=comply==1,family=binomial("logit"))
summary(s7open1.glm)
summary(s7open.glm)

stargazer(s7open1.glm,s7open.glm,
          covariate.labels = c("Intercept","Treatment (Dispositional = 1)","Pre-Treatment Populism Levels","Treatment*Pre-treatment Pop."),
          type="html",
          style = "apsr",
          label="tab:study2",
          title="The effects of the treatment and pre-treatment populism on the likelihood of a populist open response (Study 2)",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          out="Local file path here.htm"
)
detach(pop7.df)

##Table C.3, other DVs, study 2
#Models
attach(pop7.df)
Clinton.glm <- glm(Clinton ~ TREAT_2 + POP2_r + TREAT_2*POP2_r,subset=comply==1,family=binomial("logit"))
summary(Clinton.glm)
Trump.glm <- glm(Trump ~ TREAT_2 + POP2_r + TREAT_2*POP2_r,subset=comply==1,family=binomial("logit"))
summary(Trump.glm)
Diff.lm <- lm(thermdiff ~ TREAT_2 + POP2_r + TREAT_2*POP2_r,subset=comply==1)
robustse.f(Diff.lm)

stargazer(Clinton.glm,Trump.glm,Diff.lm.rob,
          covariate.labels = c("Intercept","Treatment (Dispositional = 1)","Pre-Treatment Populism Levels","Treatment*Pre-treatment Pop."),
          type="html",
          style = "apsr",
          label="tab:study2b",
          title="The interactive effects of the treatment and pre-treatment populism on presidential candidate preferences (Study 2)",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=2, 
          star.cutoffs=c(.05,.01,.001),
          column.labels = c("Clinton Vote", "Trump Vote", "Therm. Diff") ,
          se=list(NULL,NULL,Diff.lm.rob$se),
          notes=c("Models 1-2 are estimated using logit.  Model 4 is estimated using OLS with robust standard errors"),
          out="FILL IN FILEPATH.htm"
)
detach(pop7.df)

##Table C.4, Rhetorical elements, study 1
attach(pop6.df)
##Full Sample
#Responsible actor
table(badactor2[comply==1])
(581)/(193+581) #75.1%
#Conspiring elite
table(badelite2[comply==1])
(483)/(291+483) #62.4%
#Good people
table(goodpeople2[comply==1])
(393)/(383+393) #50.6%
#Populism
table(populism[comply==1])
(276)/(276+501) #35.5%
#T-test on populism by treatment group
t.test(populism[TREATMENT==0 & comply==1],populism[TREATMENT==1 & comply==1]) 

##Low populists
table(badactor2[comply==1&POP2_r==0])
(268)/(114+268) #70.2%
table(badelite2[comply==1&POP2_r==0])
(210)/(172+210) #55.0%
table(goodpeople2[comply==1&POP2_r==0])
(175)/(208+175) #45.7%
table(populism[comply==1&POP2_r==0])
(114)/(114+270) #29.7%
#T-test on populism by treatment group
t.test(populism[TREATMENT==0 & comply==1&POP2_r==0],populism[TREATMENT==1 & comply==1&POP2_r==0]) 

##High populists
table(badactor2[comply==1&POP2_r==1])
(313)/(79+313) #79.8%
table(badelite2[comply==1&POP2_r==1])
(273)/(273+119) #69.6%
table(goodpeople2[comply==1&POP2_r==1])
(218)/(218+175) #55.5%
table(populism[comply==1&POP2_r==1])
(162)/(162+231) #41.2%
#T-test on populism by treatment group
t.test(populism[TREATMENT==0 & comply==1&POP2_r==1],populism[TREATMENT==1 & comply==1&POP2_r==1]) 

detach(pop6.df)

##Table C.5, Rhetorical elements, study 2
attach(pop7.df)
##Full sample
table(badactor2[comply==1])
(384)/(384+154) 
table(badelite2[comply==1])
(265)/(265+272) 
table(goodpeople2[comply==1])
(164)/(164+373) 
table(populism[comply==1&!is.na(TREAT_2)])
(111)/(111+427) 
#T-test on populism by treatment group
t.test(populism[TREAT_2==0 & comply==1],populism[TREAT_2==1 & comply==1]) 

##Low populists
table(badactor2[comply==1&POP2_r==0])
(156)/(80+156)
table(badelite2[comply==1&POP2_r==0])
(92)/(92+143) 
table(goodpeople2[comply==1&POP2_r==0])
(58)/(58+177) 
table(populism[comply==1&POP2_r==0&!is.na(TREAT_2)])
(35)/(201+35)
#T-test on populism by treatment group
t.test(populism[TREAT_2==0 & comply==1&POP2_r==0],populism[TREAT_2==1 & comply==1&POP2_r==0]) 

##High populists
table(badactor2[comply==1&POP2_r==1])
(228)/(228+74) 
table(badelite2[comply==1&POP2_r==1])
(173)/(173+129) 
table(goodpeople2[comply==1&POP2_r==1])
(106)/(106+196) 
table(populism[comply==1&POP2_r==1&!is.na(TREAT_2)])
(76)/(76+226) 
#T-test on populism by treatment group
t.test(populism[TREAT_2==0 & comply==1&POP2_r==1],populism[TREAT_2==1 & comply==1&POP2_r==1]) 
detach(pop7.df)

##Table C.6, results with controls
##Study 6 open response:
attach(pop6.df)
s6open2.glm <- glm(populism ~ TREATMENT + POP2_r + r_INCOME + EDUC + WHITE + MALE + as.factor(IDEO3) + TREATMENT*POP2_r, subset=comply==1 & IDEO3<=3,family=binomial("logit"))
summary(s6open2.glm)
detach(pop6.df)

##Study 7 open response:
attach(pop7.df)
s7open2.glm <- glm(populism ~ TREAT_2 + POP2_r + INCOME + EDUC + WHITE + MALE + as.factor(IDEO3) + TREAT_2*POP2_r, subset=comply==1,family=binomial("logit"))
summary(s7open2.glm)
##Study 7 Clinton, Trump, Therm:
Clinton3.glm <- glm(Clinton ~ TREAT_2 + POP2_r + INCOME + EDUC + WHITE + MALE + as.factor(IDEO3) + TREAT_2*POP2_r,subset=comply==1,family=binomial("logit"))
summary(Clinton3.glm)
Trump3.glm <- glm(Trump ~ TREAT_2 + POP2_r + INCOME + EDUC + WHITE + MALE + as.factor(IDEO3) + TREAT_2*POP2_r,subset=comply==1,family=binomial("logit"))
summary(Trump3.glm)
Diff3.lm <- lm(thermdiff ~ TREAT_2 + POP2_r + INCOME + EDUC + WHITE + MALE + as.factor(IDEO3) + TREAT_2*POP2_r,subset=comply==1)
robustse.f(Diff3.lm)
detach(pop7.df)

stargazer(s6open2.glm,s7open2.glm,Clinton3.glm,Trump3.glm,Diff3.lm.rob,
          #covariate.labels = c("Intercept","Treatment (Dispositional = 1)","Pre-Treatment Populism Levels","Treatment*Pre-treatment Pop."),
          type="html",
          style = "apsr",
          label="tab:controls",
          title="Results from Studies 1 and 2 with controls",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          column.labels = c("Study1 Open Response","Study 2 Open Response", "Clinton Vote", "Trump Vote", "Therm. Diff") ,
          se=list(NULL,NULL,NULL,NULL,Diff3.lm.rob$se),
          notes=c("Models 1-4 are estimated using logit.  Model 5 is estimated using OLS with robust standard errors"),
          out="FILEPATH HERE.htm"
)

attach(pop7.df)
##Figure C.1
#Study 1 pane
TwowayDME.f(s6open2.glm,TREATMENT,POP2_r,"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a populist open response",95,,,,"Low Pre-treat Populism","High Pre-treat Populism")
#Study 2 pane
TwowayDME.f(s7open2.glm,TREAT_2,POP2_r,"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a populist open response",95,,,,"Low Pre-treat Populism","High Pre-treat Populism")

##Figure C.2
#Clinton vote
TwowayDME.f(Clinton3.glm,TREAT_2,POP2_r,"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a vote for Clinton",95,,,,"Low Pre-treat Populism","High Pre-treat Populism")
#Trump vote
TwowayDME.f(Trump3.glm,TREAT_2,POP2_r,"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a vote for Trump",95,,,,"Low Pre-treat Populism","High Pre-treat Populism")
#FT diff
TwowayDME.f(Diff3.lm.rob,TREAT_2,POP2_r,"Pre-treatment Populism levels","M.E. of the Dispositional treatment on Feeling Therm. Diff",95,,,,"Low Pre-treat Populism","High Pre-treat Populism")
detach(pop7.df)

##Table C.7, Quasi-continuous measure of populism
##Study 6 Open Response:
attach(pop6.df)
s6open.cont.glm <- glm(populism ~ TREATMENT + POP_index5 + TREATMENT*POP_index5, subset=comply==1 & POP_index5>3,family=binomial("logit"))
detach(pop6.df)

##Study 7 Open Response:
attach(pop7.df)
s7open.cont.glm <- glm(populism ~ TREAT_2 + POP_index5 + TREAT_2*POP_index5, subset=comply==1 & POP_index5>3,family=binomial("logit"))
detach(pop7.df)

##Study 7 Trump, Clinton, Therm:
attach(pop7.df)
Clinton2.glm <- glm(Clinton ~ TREAT_2 + POP_index5 + TREAT_2*POP_index5,subset=comply==1 & POP_index5>3,family=binomial("logit"))
Trump2.glm <- glm(Trump ~ TREAT_2 + POP_index5 + TREAT_2*POP_index5,subset=comply==1 & POP_index5>3,family=binomial("logit"))
Diff2.lm <- lm(thermdiff ~ TREAT_2 + POP_index5 + TREAT_2*POP_index5,subset=comply==1 & POP_index5>3)
robustse.f(Diff2.lm)
detach(pop7.df)

stargazer(s6open.cont.glm,s7open.cont.glm,Clinton2.glm,Trump2.glm,Diff2.lm.rob,
          #covariate.labels = c("Intercept","Treatment (Dispositional = 1)","Pre-Treatment Populism Levels","Treatment*Pre-treatment Pop."),
          type="html",
          style = "apsr",
          label="tab:controls",
          title="Results from Studies 1 and 2: quasi-continuous pre-treatment populism measure",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          column.labels = c("Study1 Open Response","Study 2 Open Response", "Clinton Vote", "Trump Vote", "Therm. Diff") ,
          se=list(NULL,NULL,NULL,NULL,Diff2.lm.rob$se),
          notes=c("Models 1-4 are estimated using logit.  Model 5 is estimated using OLS with robust standard errors"),
          out="FILEPATH HERE.htm"
)

##Figure C.3
#Study 1 pane
TwowayME.f(s6open.cont.glm,pop6.df$TREATMENT[pop6.df$POP_index5>3],pop6.df$POP_index5[pop6.df$POP_index5>3],"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a Populist Open Response",95,,,,hist)
#Study 2 pane
TwowayME.f(s7open.cont.glm,pop7.df$TREAT_2[pop7.df$POP_index5>3],pop7.df$POP_index5[pop7.df$POP_index5>3],"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a Populist Open Response",95,,,,hist)

##Figure C.4
attach(pop7.df)
#Clinton vote
TwowayME.f(Clinton2.glm,TREAT_2[POP_index5>3],POP_index5[POP_index5>3],"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a vote for Clinton",95,,,,hist)
#Trump vote
TwowayME.f(Trump2.glm,TREAT_2[POP_index5>3],POP_index5[POP_index5>3],"Pre-treatment Populism levels","M.E. of the Dispositional treatment on a vote for Trump",95,,,,hist)
#FT Diff
TwowayME.f(Diff2.lm,TREAT_2[POP_index5>3],POP_index5[POP_index5>3],"Pre-treatment Populism levels","M.E. of the Dispositional treatment on the Feeling Therm Diff.",95,rob,,,hist)
detach(pop7.df)

##Figure C.5, Clinton vote, comparisons to pure control
attach(pop7.df)
#Comparing the pure control to the dispositional treatment
Clinton.glm <- glm(Clinton ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=2,family=binomial("logit"))
summary(Clinton.glm)

#And now to plot our results:
pred7.df <- data.frame("TREATMENT" = c(1,0,1,0), "POP2_r" = c(0,0,1,1))
predict.logit.f(Clinton.glm,pred7.df)

Clinton.glm.pred.df$POP2_r <- factor(Clinton.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Clinton.glm.pred.df, 
       aes(c("Dispositional", "Pure control","Dispositional", "Pure control"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=Clinton.glm.pred.df$ci.lower95,ymax=Clinton.glm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Probability of a Vote for Clinton") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Now let's do it comparing the pure control to the situational treatment
Clinton.glm <- glm(Clinton ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=1,family=binomial("logit"))
summary(Clinton.glm)

#And now to plot our results:
pred7.df <- data.frame("TREATMENT" = c(2,0,2,0), "POP2_r" = c(0,0,1,1))
predict.logit.f(Clinton.glm,pred7.df)

Clinton.glm.pred.df$POP2_r <- factor(Clinton.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Clinton.glm.pred.df, 
       aes(c("Situational", "Pure control","Situational", "Pure control"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=Clinton.glm.pred.df$ci.lower95,ymax=Clinton.glm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Probability of a Vote for Clinton") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

##This is in the right direction and is significant at the p=0.03 level:
t.test(Clinton[TREATMENT==0 & POP2_r==0 & comply==1],Clinton[TREATMENT==1 & POP2_r==0 & comply==1])
##This shows that the two groups are comparable to one another:
t.test(Clinton[TREATMENT==0 & POP2_r==0 & comply==1],Clinton[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.56


##Figure C.6, Trump vote pure control comparisons
Trump.glm <- glm(Trump ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=2,family=binomial("logit"))
summary(Trump.glm)

#And now to plot our results:
pred7.df <- data.frame("TREATMENT" = c(0,1,0,1), "POP2_r" = c(0,0,1,1))
predict.logit.f(Trump.glm,pred7.df)

Trump.glm.pred.df$POP2_r <- factor(Trump.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Trump.glm.pred.df, 
       aes(c("Pure control","Dispositional","Pure control", "Dispositional"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=Trump.glm.pred.df$ci.lower95,ymax=Trump.glm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Probability of a Vote for Trump") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

Trump.glm <- glm(Trump ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=1,family=binomial("logit"))
summary(Trump.glm)

#And now to plot our results:
pred7.df <- data.frame("TREATMENT" = c(0,2,0,2), "POP2_r" = c(0,0,1,1))
predict.logit.f(Trump.glm,pred7.df)

Trump.glm.pred.df$POP2_r <- factor(Trump.glm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Trump.glm.pred.df, 
       aes(c("Pure control","Situational","Pure control", "Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=Trump.glm.pred.df$ci.lower95,ymax=Trump.glm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Probability of a Vote for Trump") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

##This is in the right direction, and highly statistically significant (p=0.01):
t.test(Trump[TREATMENT==0 & POP2_r==0 & comply==1],Trump[TREATMENT==1 & POP2_r==0 & comply==1]) 
##This shows that the groups are the same:
t.test(Trump[TREATMENT==0 & POP2_r==0 & comply==1],Trump[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.81


##Figure C.7, Feeling thermometer (T-C) differences pure control comparisons
#clinton v. Trump
#Pure control vs. dispositional
Diff.lm <- lm(thermdiff ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=2)
robustse.f(Diff.lm)

pred7.df <- data.frame("TREATMENT" = c(0,1,0,1), "POP2_r" = c(0,0,1,1))
predict.lm.f(Diff.lm,pred7.df,rob)

Diff.lm.pred.df$POP2_r <- factor(Diff.lm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Diff.lm.pred.df, 
       aes(c("Pure control","Dispositional","Pure control","Dispositional"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=Diff.lm.pred.df$ci.lower95,ymax=Diff.lm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Trump-Clinton Difference") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Pure control vs. situational
Diff.lm <- lm(thermdiff ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=1)
robustse.f(Diff.lm)

pred7.df <- data.frame("TREATMENT" = c(0,2,0,2), "POP2_r" = c(0,0,1,1))
predict.lm.f(Diff.lm,pred7.df,rob)

Diff.lm.pred.df$POP2_r <- factor(Diff.lm.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Diff.lm.pred.df, 
       aes(c("Pure control","Situational","Pure control","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() + 
  geom_linerange(aes(ymin=Diff.lm.pred.df$ci.lower95,ymax=Diff.lm.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Trump-Clinton Difference") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Highly significant
t.test(thermdiff[TREATMENT==0 & POP2_r==0 & comply==1],thermdiff[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.01
#Not significant
t.test(thermdiff[TREATMENT==0 & POP2_r==0 & comply==1],thermdiff[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.93

##Figure C.8, FT (Sanders-Clinton) comparisons to pure control
#Pure control vs. Dispositional
Diff.lm2 <- lm(bthermdiff ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=2)
robustse.f(Diff.lm2)

pred7.df2 <- data.frame("TREATMENT" = c(0,1,0,1), "POP2_r" = c(0,0,1,1))
predict.lm.f(Diff.lm2,pred7.df2,rob)

Diff.lm2.pred.df$POP2_r <- factor(Diff.lm2.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Diff.lm2.pred.df, 
       aes(c("Pure control","Dispositional","Pure control","Dispositional"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() +
  geom_linerange(aes(ymin=Diff.lm2.pred.df$ci.lower95,ymax=Diff.lm2.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Sanders-Clinton Difference") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Pure control vs. Situational
Diff.lm2 <- lm(bthermdiff ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=1)
robustse.f(Diff.lm2)

pred7.df2 <- data.frame("TREATMENT" = c(0,2,0,2), "POP2_r" = c(0,0,1,1))
predict.lm.f(Diff.lm2,pred7.df2,rob)

Diff.lm2.pred.df$POP2_r <- factor(Diff.lm2.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Diff.lm2.pred.df, 
       aes(c("Pure control","Situational","Pure control","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() +
  geom_linerange(aes(ymin=Diff.lm2.pred.df$ci.lower95,ymax=Diff.lm2.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Sanders-Clinton Difference") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Significant as we would hope
t.test(bthermdiff[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.03
#Insignificant as we would hope
t.test(bthermdiff[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.70


##Figure C.9, FT (Sanders-Trump) comparisons to pure control
#Pure control vs. Dispositional
Diff.lm2 <- lm(bthermdiff2 ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=2)
robustse.f(Diff.lm2)

pred7.df2 <- data.frame("TREATMENT" = c(0,1,0,1), "POP2_r" = c(0,0,1,1))
predict.lm.f(Diff.lm2,pred7.df2,rob)

Diff.lm2.pred.df$POP2_r <- factor(Diff.lm2.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Diff.lm2.pred.df, 
       aes(c("Pure control","Dispositional","Pure control","Dispositional"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() +
  geom_linerange(aes(ymin=Diff.lm2.pred.df$ci.lower95,ymax=Diff.lm2.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Sanders-Trump Difference") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Pure control vs. Situational
Diff.lm2 <- lm(bthermdiff2 ~ TREATMENT + POP2_r + TREATMENT*POP2_r,subset=comply==1&TREATMENT!=1)
robustse.f(Diff.lm2)

pred7.df2 <- data.frame("TREATMENT" = c(0,2,0,2), "POP2_r" = c(0,0,1,1))
predict.lm.f(Diff.lm2,pred7.df2,rob)

Diff.lm2.pred.df$POP2_r <- factor(Diff.lm2.pred.df$POP2_r, levels = c(0,1), labels = c("Low Pre-treatment Populism","High Pre-treatment Populism"))

ggplot(Diff.lm2.pred.df, 
       aes(c("Pure control","Situational","Pure control","Situational"),predicted.value)) +
  geom_point(position="identity") + 
  theme_bw() +
  geom_linerange(aes(ymin=Diff.lm2.pred.df$ci.lower95,ymax=Diff.lm2.pred.df$ci.upper95)) + 
  facet_wrap(~POP2_r, strip.position="bottom") +
  theme(strip.placement="outside") +
  labs(x = "", y = "Predicted Sanders-Trump Difference") +  
  theme(strip.background = element_rect(color = 'white', fill = 'white')
  )

#Insignificant as we would hope
t.test(bthermdiff2[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff2[TREATMENT==1 & POP2_r==0 & comply==1]) # p=.19
#Insignificant as we would hope
t.test(bthermdiff2[TREATMENT==0 & POP2_r==0 & comply==1],bthermdiff2[TREATMENT==2 & POP2_r==0 & comply==1]) # p=.84
detach(pop7.df)

####Appendix E: Sample characteristics####
##Table E.1
#Liberal
(sum(pop6.df$IDEO3==1, na.rm=T))/(sum(pop6.df$IDEO3>0, na.rm=T))
(sum(pop7.df$IDEO3_r==1, na.rm=T))/(sum(pop7.df$IDEO3_r>0, na.rm=T))
#Moderate
(sum(pop6.df$IDEO3==2|pop6.df$IDEO3==4, na.rm=T))/(sum(pop6.df$IDEO3>0, na.rm=T))
(sum(pop7.df$IDEO3_r==2, na.rm=T))/(sum(pop7.df$IDEO3_r>0, na.rm=T))
#Conservative
(sum(pop6.df$IDEO3==3, na.rm=T))/(sum(pop6.df$IDEO3>0, na.rm=T))
(sum(pop7.df$IDEO3_r==3, na.rm=T))/(sum(pop7.df$IDEO3_r>0, na.rm=T))
#Democrat
(sum(pop6.df$PID3==1, na.rm=T))/(sum(pop6.df$PID3>0, na.rm=T))
(sum(pop7.df$PID3==1, na.rm=T))/(sum(pop7.df$PID3>0, na.rm=T))
#Independent
(sum(pop6.df$PID3==2|pop6.df$PID3==4, na.rm=T))/(sum(pop6.df$PID3>0, na.rm=T))
(sum(pop7.df$PID3==2|pop7.df$PID3==4, na.rm=T))/(sum(pop7.df$PID3>0, na.rm=T))
#Republican
(sum(pop6.df$PID3==3, na.rm=T))/(sum(pop6.df$PID3>0, na.rm=T))
(sum(pop7.df$PID3==3, na.rm=T))/(sum(pop7.df$PID3>0, na.rm=T))
#Female
(sum(pop6.df$SEX==2, na.rm=T))/(sum(pop6.df$SEX>0, na.rm=T))
(sum(pop7.df$MALE==0, na.rm=T))/(sum(pop7.df$MALE>=0, na.rm=T))
#White
(sum(pop6.df$WHITE==1, na.rm=T))/(sum(pop6.df$WHITE>=0, na.rm=T))
(sum(pop7.df$WHITE==1, na.rm=T))/(sum(pop7.df$WHITE>=0, na.rm=T))
#Age
median(pop6.df$AGE, na.rm=T)
median(pop7.df$AGE, na.rm=T) #45-54 category
#Income
median(pop6.df$r_INCOME, na.rm=T) # Between $32K and $57K
median(pop7.df$INCOME, na.rm=T) #Between $35K and $50K
#Education
median(pop6.df$EDUC, na.rm=T) # Bachelor's Degree
median(pop7.df$EDUC, na.rm=T) # Some college

####Appendix F: Data quality####
#We have these variables for study 7, but th
#Variables to use
#Time (problem and explanation)
mean(pop6.df$PROB_respons_t, na.rm=T)
sd(pop6.df$PROB_respons_t, na.rm=T)
sum(pop6.df$PROB_respons_t>0, na.rm=T)
mean(pop7.df$PROB_respons_t, na.rm=T)
sd(pop7.df$PROB_respons_t, na.rm=T)
sum(pop7.df$PROB_respons_t>0, na.rm=T)

t.test(pop6.df$PROB_respons_t, pop7.df$PROB_respons_t, alternative="greater")
t.test(pop6.df$PROB_respons_t, pop7.df$PROB_respons_t)

mean(pop6.df$PROB_why_t, na.rm=T)
sd(pop6.df$PROB_why_t, na.rm=T)
sum(pop6.df$PROB_why_t>0, na.rm=T)
mean(pop7.df$PROB_why_t, na.rm=T)
sd(pop7.df$PROB_why_t, na.rm=T)
sum(pop7.df$PROB_why_t>0, na.rm=T)

t.test(pop6.df$PROB_why_t, pop7.df$PROB_why_t, alternative="greater")
t.test(pop6.df$PROB_why_t, pop7.df$PROB_why_t)


#Words (problem and explanation)
mean(pop6.df$PROB_respons_words, na.rm=T)
sd(pop6.df$PROB_respons_words, na.rm=T)
sum(pop6.df$PROB_respons_words>0, na.rm=T)
mean(pop7.df$PROB_respons_words, na.rm=T)
sd(pop7.df$PROB_respons_words, na.rm=T)
sum(pop7.df$PROB_respons_words>0, na.rm=T)

t.test(pop6.df$PROB_respons_words, pop7.df$PROB_respons_words, alternative="greater")
t.test(pop6.df$PROB_respons_words, pop7.df$PROB_respons_words)

mean(pop6.df$PROB_why_words, na.rm=T)
sd(pop6.df$PROB_why_words, na.rm=T)
sum(pop6.df$PROB_why_words>0, na.rm=T)
mean(pop7.df$PROB_why_words, na.rm=T)
sd(pop7.df$PROB_why_words, na.rm=T)
sum(pop7.df$PROB_why_words>0, na.rm=T)

t.test(pop6.df$PROB_why_words, pop7.df$PROB_why_words, alternative="greater")
t.test(pop6.df$PROB_why_words, pop7.df$PROB_why_words)


####Appendix G: Choice of threat####
##To correctly code IDEO3 (making the 4th category NA):
pop6.df$IDEO3[pop6.df$IDEO3==4] <- NA
attach(pop6.df)

##Table G.1
s6type.glm <- glm(POP2_r ~ r_INCOME + EDUC + WHITE + MALE + as.factor(IDEO3) + as.factor(PROB), subset=comply==1,family=binomial("logit"))
summary(s6type.glm)

stargazer(s6type.glm,
          covariate.labels = c("Intercept","Income","Education","White","Male","Political Ideology = 2","Political Ideology = 3","Prob: Gov't","Prob: Environment","Prob: Inequality","Prob: Racism","Prob: Economy","Prob: Terrorism","Prob: HealthCare","Prob: Education"),
          column.labels = c("Pre-treatment Populism"),
          type="html",
          style = "apsr",
          label="tab:G1",
          title="Relationship between Problem Choice and Pre-treatment Populism Levels",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          out="FILL IN FILEPATH HERE.htm"
)

##Table G.2
s6ttype.glm <- glm(populism ~ TREATMENT + as.factor(PROB) + TREATMENT*as.factor(PROB), subset=comply==1,family=binomial("logit"))
summary(s6ttype.glm)

##To produce Table G2 in the online appendix:
stargazer(s6ttype.glm,
          covariate.labels = c("Intercept","Treatment","Prob: Gov't","Prob:  Environment","Prob: Inequality","Prob: Racism","Prob: Economy","Prob: Terrorism","Prob: HealthCare","Prob: Education","Treatment*Gov't","Treatment*Environment","Treatment*Inequality","Treatment*Racism","Treatment*Economy","Treatment*Terrorism","Treatment*HealthCare","Treatment*Education"),
          column.labels = c("Post-treatment Populist Expression"),
          type="html",
          style = "apsr",
          label="tab:G2",
          title="Table G2: Interactive effect of problem choice and treatment on post-treatment populist expression",
          intercept.bottom = FALSE, 
          intercept.top=TRUE, 
          digits=3, 
          star.cutoffs=c(.05,.01,.001),
          out="FILL IN FILEPATH HERE.htm"
)
detach(pop6.df)

####Appendix H: Expanded coding tables####
##Table H.1, coding table for study 1
attach(pop6.df)
##Whole sample
table(badactor2[comply==1])
(581)/(193+581) #75.1%
#Conspiring elite
table(badelite2[comply==1])
(483)/(291+483) #62.4%
#Good people
table(goodpeople2[comply==1])
(393)/(383+393) #50.6%
#Populism
table(populism[comply==1])
(276)/(276+501) #35.5%
#Each treatment group
t.test(badactor2[TREATMENT==0 & comply==1],badactor2[TREATMENT==1 & comply==1]) #p=0.000
t.test(badelite2[TREATMENT==0 & comply==1],badelite2[TREATMENT==1 & comply==1]) #p=0.000
t.test(goodpeople2[TREATMENT==0 & comply==1],goodpeople2[TREATMENT==1 & comply==1]) #p=0.085
t.test(populism[TREATMENT==0 & comply==1],populism[TREATMENT==1 & comply==1]) 

##Low populists
table(badactor2[comply==1&POP2_r==0])
(268)/(114+268) #70.2%
table(badelite2[comply==1&POP2_r==0])
(210)/(172+210) #55.0%
table(goodpeople2[comply==1&POP2_r==0])
(175)/(208+175) #45.7%
table(populism[comply==1&POP2_r==0])
(114)/(114+270) #29.7%
#Each treatment group
t.test(badactor2[TREATMENT==0 & comply==1&POP2_r==0],badactor2[TREATMENT==1 & comply==1&POP2_r==0]) 
t.test(badelite2[TREATMENT==0 & comply==1&POP2_r==0],badelite2[TREATMENT==1 & comply==1&POP2_r==0]) 
t.test(goodpeople2[TREATMENT==0 & comply==1&POP2_r==0],goodpeople2[TREATMENT==1 & comply==1&POP2_r==0]) 
t.test(populism[TREATMENT==0 & comply==1&POP2_r==0],populism[TREATMENT==1 & comply==1&POP2_r==0]) 

##High populists
table(badactor2[comply==1&POP2_r==1])
(313)/(79+313) #79.8%
table(badelite2[comply==1&POP2_r==1])
(273)/(273+119) #69.6%
table(goodpeople2[comply==1&POP2_r==1])
(218)/(218+175) #55.5%
table(populism[comply==1&POP2_r==1])
(162)/(162+231) #41.2%
#Each treatment group
t.test(badactor2[TREATMENT==0 & comply==1&POP2_r==1],badactor2[TREATMENT==1 & comply==1&POP2_r==1]) 
t.test(badelite2[TREATMENT==0 & comply==1&POP2_r==1],badelite2[TREATMENT==1 & comply==1&POP2_r==1]) 
t.test(goodpeople2[TREATMENT==0 & comply==1&POP2_r==1],goodpeople2[TREATMENT==1 & comply==1&POP2_r==1]) 
t.test(populism[TREATMENT==0 & comply==1&POP2_r==1],populism[TREATMENT==1 & comply==1&POP2_r==1]) 
detach(pop6.df)


##Table H.2, coding table for study 2
attach(pop7.df)
##Whole sample
table(badactor2[comply==1])
(384)/(384+154) 
table(badelite2[comply==1])
(265)/(265+272) 
table(goodpeople2[comply==1])
(164)/(164+373) 
table(populism[comply==1&!is.na(TREAT_2)])
(111)/(111+427) 
#Each treatment group
t.test(badactor2[TREAT_2==0 & comply==1],badactor2[TREAT_2==1 & comply==1])
t.test(badelite2[TREAT_2==0 & comply==1],badelite2[TREAT_2==1 & comply==1]) 
t.test(goodpeople2[TREAT_2==0 & comply==1],goodpeople2[TREAT_2==1 & comply==1]) 
t.test(populism[TREAT_2==0 & comply==1],populism[TREAT_2==1 & comply==1]) 

##Low populists
table(badactor2[comply==1&POP2_r==0])
(156)/(80+156)
table(badelite2[comply==1&POP2_r==0])
(92)/(92+143) 
table(goodpeople2[comply==1&POP2_r==0])
(58)/(58+177) 
table(populism[comply==1&POP2_r==0&!is.na(TREAT_2)])
(35)/(201+35)
#Each treatment group
t.test(badactor2[TREAT_2==0 & comply==1&POP2_r==0],badactor2[TREAT_2==1 & comply==1&POP2_r==0])
t.test(badelite2[TREAT_2==0 & comply==1&POP2_r==0],badelite2[TREAT_2==1 & comply==1&POP2_r==0]) 
t.test(goodpeople2[TREAT_2==0 & comply==1&POP2_r==0],goodpeople2[TREAT_2==1 & comply==1&POP2_r==0]) 
t.test(populism[TREAT_2==0 & comply==1&POP2_r==0],populism[TREAT_2==1 & comply==1&POP2_r==0]) 

##High populists
table(badactor2[comply==1&POP2_r==1])
(228)/(228+74) 
table(badelite2[comply==1&POP2_r==1])
(173)/(173+129) 
table(goodpeople2[comply==1&POP2_r==1])
(106)/(106+196) 
table(populism[comply==1&POP2_r==1&!is.na(TREAT_2)])
(76)/(76+226) 
#Each treatment group
t.test(badactor2[TREAT_2==0 & comply==1&POP2_r==1],badactor2[TREAT_2==1 & comply==1&POP2_r==1]) 
t.test(badelite2[TREAT_2==0 & comply==1&POP2_r==1],badelite2[TREAT_2==1 & comply==1&POP2_r==1]) 
t.test(goodpeople2[TREAT_2==0 & comply==1&POP2_r==1],goodpeople2[TREAT_2==1 & comply==1&POP2_r==1]) 
t.test(populism[TREAT_2==0 & comply==1&POP2_r==1],populism[TREAT_2==1 & comply==1&POP2_r==1]) 
detach(pop7.df)