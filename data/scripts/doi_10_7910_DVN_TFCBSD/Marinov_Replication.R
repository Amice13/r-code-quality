rm(list=ls())

# Packages
library(PFCRE)
library(survival)
library(readstata13)
library(ggplot2)
library(xtable)
library(margins)
library(data.table)

# Set working directory
# setwd()

# Import data
data <- read.dta13("marinov_ldpaper.dta")

# Variable names
vs <- c("fail", "sanctionsl1", "forcel1" , "growthpc" ,"lngdppc" , "democl1" , "democlnt" , "mixedl1",
        "mixedlnt" , "age" , "ot3")
# List-wise deletion of incomplete cases
data2 = data[complete.cases(data[,vs]),]
data2$id <- data2$ccode
data2$s1 <- data2$`_spline1`
data2$s2 <- data2$`_spline2`
data2$s3 <- data2$`_spline3`

# Estimate the Conditional Maximum Likelihood 
cmle <- clogit(fail~ sanctionsl1 + forcel1 + growthpc + lngdppc + democl1 + democlnt + mixedl1+
                 mixedlnt + age + ot3 +  + s1 + s2 +s3 + strata(ccode),data  = data2)

# Formula for PF-CRE estimator
pf.form <- as.formula(fail~ sanctionsl1 + forcel1 + growthpc + lngdppc + democl1 + democlnt + mixedl1+
                        mixedlnt + age + ot3 + s1 + s2 +s3)

# Estimate a standard logit model (unreported)
logt <- glm(pf.form, data = data2, family=binomial(link="logit"))

# Estimate PF-CRE doing cross-validation for the selection of the lambda penalization parameter
start.time <- Sys.time()
CV <- pfcrems(pf.form, data = data2, family=binomial(link="logit"), id = "id", max.steps = 2000, lambda.cand = exp(seq(1,4,by=0.25)))
end.time <- Sys.time()
pfcre.time <- end.time - start.time

# Plot to check cross-validation results
plot(CV$lambda.cand,CV$cv,pch=20)

# Formula for estimating the fixed-effects model usign alpaca's fast algorithm
alpaca <- as.formula(fail~ sanctionsl1 + forcel1 + growthpc + lngdppc + democl1 + democlnt + mixedl1+
                       mixedlnt + age + ot3 + s1 + s2 +s3 + as.factor(ccode))
# Estimation of FE model.
fe <- glm(alpaca,  data = data2, family = binomial(link = "logit"))

# Use dataprep from PFCRE package to obtain time-means of covariates to estimate CRE model. 
dp <- dataprep(pf.form, data = data2, id = "id", degree = 2)

data3 <- cbind(dp$y, dp$xx)
colnames(data3)[1] <- "fail"
data3$id <- data2$id

# Formula for CRE model
cre.form <- as.formula(fail~ sanctionsl1 + forcel1 + growthpc + lngdppc + democl1 + democlnt + mixedl1+
                         mixedlnt + age + ot3 + s1 + s2 +s3 + pol1 + pol2 + pol3 + pol4 + pol5 + pol6 +
                         pol7 + pol8 + pol9 + pol10 + pol11 + (1|id) )
# Estimation of CRE model
start.time <- Sys.time()
CRE <- lme4::glmer(cre.form, data = data3, family = binomial(link = "logit"))
end.time <- Sys.time()
cre.time<- end.time - start.time



## GENERATE COEFFICIENT PLOTS
CLc=summary(cmle)[7]$coefficients[,1]
CLs=summary(cmle)[7]$coefficients[,3]
CLl=CLc+qnorm(0.025)*CLs
CLu=CLc+qnorm(0.975)*CLs

PFc=CV$Model$beta[-1]
PFs=CV$Model$sdbeta[-1]
PFl=PFc+qnorm(0.025)*PFs
PFu=PFc+qnorm(0.975)*PFs

FEc=fe$coefficients[2:14]
FEs=diag(as.matrix(vcov(fe)[2:14,2:14]))^0.5
FEl=FEc+qnorm(0.025)*FEs
FEu=FEc+qnorm(0.975)*FEs

CRc=CRE@beta[2:14]
CRs=(diag(as.matrix(vcov(CRE)))[2:14])^0.5
CRl=CRc+qnorm(0.025)*CRs
CRu=CRc+qnorm(0.975)*CRs



out1=data.frame(CLc,CLl,CLu,rep("CMLE",length(CLc)))
out1 <- cbind(out1, rownames(out1))
colnames(out1) <- c("coef","lower","upper","method","variable")
out2=data.frame(PFc,PFl,PFu,rep("PF-CRE",length(PFc)))
out2 <- cbind(out2, rownames(out2))
colnames(out2) <- c("coef","lower","upper","method","variable")
out3=data.frame(FEc,FEl,FEu,rep("FE",length(FEc)))
out3 <- cbind(out3, rownames(out3))
colnames(out3) <- c("coef","lower","upper","method","variable")
out4=data.frame(CRc,CRl,CRu,rep("CRE",length(CRc)))
out4 <- cbind(out4, rownames(out4))
colnames(out4) <- c("coef","lower","upper","method","variable")


out=rbind(out1,out2,out3,out4)

out=data.frame(out)
out$var=paste(out$method,out$variable,sep="")
out=out[order(out$variable),]
out$variable <- as.factor(out$variable)

l1 <- 5
l2 <- l1+1

Out_Short <- out[out$variable %in% names(cmle$coefficients)[1:l1],]

scalef=function(x) sprintf("%.1f",x)

pdf("Marinov_Replication_2020.pdf",width=5,height=3.5)
p <- ggplot(Out_Short,aes(method,coef))+geom_point(aes(y=coef),group="method")+theme_bw()+geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+geom_hline(yintercept=0,color="red",linetype=2)
p <- p+scale_y_continuous(breaks=seq(-3.5,1,by=0.5),limits=c(-3.5,1),labels=scalef)+theme(axis.title=element_blank(),text=element_text(size=15),title=element_text(size=13))
p <- p + facet_grid(.~variable) + theme(axis.text.x = element_text(angle=90))
p
dev.off()

# SPECIFICATION TEST
ST=t((cmle$coefficients[1:l1]-CV$Model$beta[2:l2]))%*%solve(vcov(cmle)[1:l1,1:l1]-CV$Model$vcov[2:l2,2:l2])%*%(cmle$coefficients[1:l1]-CV$Model$beta[2:l2])
STpval=pchisq(ST,l1,lower.tail=F)
ST
STpval


ST_cre=t((cmle$coefficients[1:l1]-CRE@beta[2:l2]))%*%solve(vcov(cmle)[1:l1,1:l1]-vcov(CRE)[2:l2,2:l2])%*%(cmle$coefficients[1:l1]-CRE@beta[2:l2])
STpval_cre=pchisq(as.numeric(ST_cre),l1,lower.tail=F)
ST_cre
STpval_cre


# Tables:
xtable(cbind(out2[,1:3],out1[,1:3],out3[,1:3], out4[,1:3]),align = "|l|rrr|rrr|rrr|rrr|")
# Observations
fe$nobs[c(1,4)]
length(unique(data2$id))
# Countries
fe$lvls.k

# Total Gamma Terms
dp <- dataprep(pf.form, data = data2, id = "id", degree = 2)
colnames(dp$xx)[length(colnames(dp$xx))]
# Selected
length(CV$Model$gamma)


# Partial Effects

e.form <- paste("fail~",paste(c(names(CV$Model$beta[-1]),names(CV$Model$gamma)),collapse="+"),"+(1|id)")

data3 <- cbind(data2,dp$xx)

pfpe <- lme4::glmer(e.form, family=binomial(link="logit"),data = data3)

pfcre_pe <- summary(margins(pfpe, variables = "sanctionsl1"))
cre_pe <- summary(margins(CRE, variables = "sanctionsl1"))
fe_pe <- summary(margins(fe, variables = "sanctionsl1"))


peout <- cbind(c(fe_pe[1,2],pfcre_pe[1,2],cre_pe[1,2]),c(fe_pe[1,2] -1.96*fe_pe[1,3] , pfcre_pe[1,2] - 1.96*pfcre_pe[1,3],cre_pe[1,2] - 1.96*cre_pe[1,3]),
               c(fe_pe[1,2] + 1.96*fe_pe[1,3] , pfcre_pe[1,2] + 1.96*pfcre_pe[1,3], cre_pe[1,2] + 1.96*cre_pe[1,3]) )
peout <- data.frame(peout)
colnames(peout) <- c("coef","lower","upper")
peout$method <- c("FE","PF-CRE","CRE")
peout$variable <- rep("Sanctions",3)

peout$method <- factor(peout$method, levels = c("PF-CRE","CRE","FE"))

pdf("Marinov_APE_2020.pdf",width=4,height=2)
p2 <- ggplot(peout,aes(method,coef))+geom_point(aes(y=coef),group="method")+theme_bw()+geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+geom_hline(yintercept=0,color="red",linetype=2)
p2 <- p2  + theme(axis.title = element_blank())
p2 <- p2 + facet_grid(variable ~ .)
p2
dev.off()


library(xtable)
xtable(cbind(out[out$method == "PF-CRE",1:3],out[out$method == "CMLE",1:3],out[out$method == "CRE",1:3],out[out$method == "FE",1:3]))


save.image("Marinov_Replication_2020.RData")



