rm(list=ls())

library(PFCRE)
library(readstata13)
library(survival)
library(mice)
library(gridExtra)
library(ggplot2)
library(bife)
library(lfe)
library(margins)
library(dplyr)

# Set working directory
# setwd()

# Import recoded BES data
# data=read.dta13("BESTactical2015.dta",convert.factors=FALSE)

# Impute missing values
# miced=mice(data,m=1,seed=1234)
# data=complete(miced,1,include=F)
# save(data,file="ImputedData2015.Rdata")
load("ImputedData2015.Rdata")


# Main variables to use and time invariant ones only for logit comparison (not reported)
mainvars=c("contFirst","contSecond","likeFirst","likeSecond")
timeinv <- c("Unemploy1","Retired1","CollegeMore","female","age","ownHome","ownMort")

# Keep only the relevant data:
data=data[,c("id","wave","tactical",mainvars,timeinv)]

# PF-CRE formula
mainform=as.formula(paste("tactical~",paste(mainvars,collapse="+")))
invform = NULL # No time invariant covariates. 

# Keep only complete cases (inputation does not fill in all values)
data <- data[complete.cases(data),] 

# Create the correlated random effects terms for CRE. 
a <- data %>% group_by(id) %>% transmute(CF = sum(contFirst), CS = sum(contSecond), LF = sum(likeFirst), LS = sum(likeSecond))
dataCRE <- c(data[1:7],a[2:5])
dataCRE <- as.data.frame(dataCRE)
colnames(dataCRE) <- c("wave","id","tactical",mainvars,"pol1","pol2","pol3","pol4")
ii <- "(1|id)"
tt <- "(1|wave)"
it <- c(ii,tt)

# Candidates for cross-validation of penalization parameter and estimation of PF-CRE via cross-validation
cands <- exp(seq(1,3,by=0.25))
start_time <- Sys.time()
CV <- pfcrems(formula = mainform, addcovars = invform, data = data,
              family=binomial(link="logit"), id = "id", degree = 2,
              lambda.cand = cands, max.steps = 500)
pfcre.time <- Sys.time()-start_time
plot(log(CV$lambda.cand), CV$cv,pch=20) # Cross validation plot

# CMLE, FE, CRE, and standard logit estimation
CL=clogit(tactical~contFirst+contSecond+likeFirst+likeSecond+strata(id),data=data)
firstdeg <- as.formula(paste("tactical~",paste(mainvars,collapse="+"),"+pol1+pol2+pol3+pol4+",paste(it,collapse="+")))
mainwcontrols <- as.formula(paste("tactical~",paste(mainvars,collapse="+"),"+",paste(timeinv,collapse="+")))
log <- glm(mainwcontrols, data = data, family=binomial(link="logit"))
start_time <- Sys.time()
cre <- lme4::glmer(firstdeg, data = dataCRE, family=binomial(link = "logit"))
cre.time <- Sys.time() - start_time
alpaca <- as.formula(tactical ~ contFirst + contSecond + likeFirst + likeSecond | id)
fe <- bife(alpaca,  data = data, model = "logit")

# SPECIFICATION TEST FOR PFCRE
chisq=t((CL$coefficients-CV$Model$beta[2:5]))%*%solve(vcov(CL)-CV$Model$vcov[2:5,2:5])%*%(CL$coefficients-CV$Model$beta[2:5])
pchisq=pchisq(chisq,4,lower.tail=F)
chisq
pchisq
# SPECIFICATION TEST FOR CRE
chisq_cre=as.numeric(t((CL$coefficients-cre@beta[2:5]))%*%solve(vcov(CL)-vcov(cre)[2:5,2:5])%*%(CL$coefficients-cre@beta[2:5]))
pchisq_cre=pchisq(chisq_cre,4,lower.tail=F)
chisq_cre
pchisq_cre

######## POST ESTIMATION ANALYSIS AND PLOTS AND TABLES
l1=length(CL$coefficients)
l2=l1+1
## GENERATE COEFFICIENT PLOTS
CLc=summary(CL)[7]$coefficients[,1]
CLs=summary(CL)[7]$coefficients[,3]
CLl=CLc+qnorm(0.025)*CLs
CLu=CLc+qnorm(0.975)*CLs

PFc=CV$Model$beta[2:l2]
PFs=CV$Model$sdbeta[2:l2]
PFl=PFc+qnorm(0.025)*PFs
PFu=PFc+qnorm(0.975)*PFs

LOc=log$coefficients[2:l2]
LOs=diag(as.matrix(vcov(log)[2:l2,2:l2]))^0.5
LOl=LOc+qnorm(0.025)*LOs
LOu=LOc+qnorm(0.975)*LOs

CRc=cre@beta[2:l2]
CRs=diag(as.matrix(vcov(cre)[2:l2,2:l2]))^0.5
CRl=CRc+qnorm(0.025)*CRs
CRu=CRc+qnorm(0.975)*CRs

FEc=fe$coefficients
FEs=diag(as.matrix(vcov(fe)))^0.5
FEl=FEc+qnorm(0.025)*FEs
FEu=FEc+qnorm(0.975)*FEs

out1=data.frame(CLc,CLl,CLu,rep("CL",l1))
out1 <- cbind(out1, rownames(out1))
colnames(out1) <- c("coef","lower","upper","method","variable")
out2=data.frame(PFc,PFl,PFu,rep("PFCRE",l1))
out2 <- cbind(out2, rownames(out2))
colnames(out2) <- c("coef","lower","upper","method","variable")
out3=data.frame(FEc,FEl,FEu,rep("FE",l1))
out3 <- cbind(out3, rownames(out3))
colnames(out3) <- c("coef","lower","upper","method","variable")
out4=data.frame(CRc,CRl,CRu,rep("CRE",l1))
out4 <- cbind(out4, rownames(out4))
colnames(out4) <- c("coef","lower","upper","method","variable")


out=rbind(out1,out2,out3,out4)

out=data.frame(out)
out$var=paste(out$method,out$variable,sep="")
out=out[order(out$variable),]


# Average Partial Effects
data.prep=dataprep(formula = mainform,addcovars = invform, data = data,id = "id",degree=2)
data2 <- data.frame(data,data.prep$xx)
eq <-  as.formula(paste(CV$Model$formula,"+",paste(names(CV$Model$gamma),collapse="+"),"+(1|id)"))
pfcre_lme <- lme4::glmer(eq,data=data2,family=binomial(link="logit"),control = lme4::glmerControl(calc.derivs = FALSE, optimizer = "nloptwrap"))
ape_pfcre <- summary(margins(pfcre_lme, variables = c("contFirst","contSecond","likeFirst","likeSecond")))[,c(1,2,6,7)]
colnames(ape_pfcre) <- c("variable","APE","lower","upper")
ape_pfcre$method <- "PFCRE"
ape_log <- summary(margins(log, variables = c("contFirst","contSecond","likeFirst","likeSecond")))[,c(1,2,6,7)]
colnames(ape_log) <- c("variable","APE","lower","upper")
ape_log$method <- "Logit"
ape_cre <- summary(margins(cre, variables = c("contFirst","contSecond","likeFirst","likeSecond")))[,c(1,2,6,7)]
colnames(ape_cre) <- c("variable","APE","lower","upper")
ape_cre$method <- "CRE"
ape_fe_c <- get_APEs(fe)$delta
ape_fe_s <- diag(get_APEs(fe)$vcov)^0.5
ape_fe <- data.frame(names(ape_fe_c),ape_fe_c, ape_fe_c - 1.96*ape_fe_s, ape_fe_c + 1.96*ape_fe_s)
colnames(ape_fe) <- c("variable","APE","lower","upper")
ape_fe$method <- "FE"

apeout <- rbind(ape_pfcre,ape_fe,ape_cre)
apeout$var=paste(apeout$method,apeout$variable,sep="")
apeout=apeout[order(apeout$variable),]
out$variable <- factor(out$variable, levels = c("contFirst","contSecond","likeFirst","likeSecond"), 
                       labels = c("Cont 1st","Cont 2nd","Like 1st","Like 2nd"))

pdf("BESTacticalVoting2015beta.pdf",width=5,height=4)
scalef=function(x) sprintf("%.1f",x)
p <- ggplot(out,aes(method,coef))+geom_point(aes(y=coef),group="method")+theme_bw()+geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+geom_hline(yintercept=0,color="red",linetype=2)
p <- p+scale_y_continuous(breaks=seq(-1.6,1.6,by=0.4),limits=c(-1.6,1.6),labels=scalef)+coord_flip()+theme(axis.title=element_blank(),text=element_text(size=15),title=element_text(size=13))
p <- p + facet_grid(variable~.) + scale_x_discrete(labels=c("CMLE","PF-CRE","FE","CRE"))
p
dev.off()

apeout$variable <- factor(apeout$variable, levels = c("contFirst","contSecond","likeFirst","likeSecond"), 
                          labels = c("Cont 1st","Cont 2nd","Like 1st","Like 2nd"))

pdf("BESTacticalVoting2015APE.pdf",width=5,height=4)
scalef=function(x) sprintf("%.2f",x)
p2 <- ggplot(apeout,aes(method,APE))+geom_point(aes(y=APE),group="method")+theme_bw()+geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+geom_hline(yintercept=0,color="red",linetype=2)
p2 <- p2 +coord_flip()+theme(axis.title=element_blank(),text=element_text(size=15),title=element_text(size=13))
p2 <- p2 + scale_y_continuous(breaks=seq(-0.25,0.25,by=0.1),limits=c(-0.25,0.25),labels=scalef)
p2 <- p2 + facet_grid(variable~.) + scale_x_discrete(labels=c("CRE","FE","PF-CRE"))
p2
dev.off()


library(xtable)
xtable(cbind(out[out$method == "PFCRE",1:3],out[out$method == "CL",1:3],out[out$method == "CRE",1:3],out[out$method == "FE",1:3]))

save.image("TacticalVotingResultsImage.RData")

