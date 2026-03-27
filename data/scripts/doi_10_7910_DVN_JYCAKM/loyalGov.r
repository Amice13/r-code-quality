######################
# BJPS revision
# Candada Cohesion   
######################
rm(list=ls())
allout <- read.csv("loyaltyLib.cvs") # Liberal
alloutc <- read.csv("loyaltyCon.cvs") # Conservative
# Government votes
allout$parl <- allout$parliament
allout$parl2 <- allout$parliament^2
allout$coh <- allout$cohort
allout$coh2 <- allout$cohort^2
alloutc$parl <- alloutc$parliament
alloutc$parl2 <- alloutc$parliament^2
alloutc$coh <- alloutc$cohort
alloutc$coh2 <- alloutc$cohort^2

splittest <- c(5:35)
dev <-NULL
devC <- NULL
model <- "loyalty.y ~  participation.norm.y + eff.candidates + Total.vote.norm + Cabinet + govparty + govtype + joiner + last + West + Quebec + Maritime + parl+ parl2 + coh + coh2"
model1 <- "loyalty.y ~  participation.norm.y + eff.candidates + Total.vote.norm + Cabinet + govparty + joiner + last + West + Quebec + Maritime + parl+ parl2 + coh + coh2"
fix_model <- "loyalty.y ~  participation.norm.y + eff.candidates + Total.vote.norm + Cabinet + joiner + last + West + Quebec + Maritime + coh + coh2 + as.factor(parliament)"
fix_cohort_model <- "loyalty.y ~  participation.norm.y + eff.candidates + Total.vote.norm + Cabinet + joiner + last + West + Quebec + Maritime +as.factor(cohort) + as.factor(parliament)"

modlist <- c(model,fix_model,fix_cohort_model)

Split <- 16
library(parallel)
modslib <- mclapply(modlist,glm,data=allout,family=quasibinomial(logit),mc.cores=3)
modscon <- mclapply(modlist,glm,data=alloutc,family=quasibinomial(logit),mc.cores=3)

modslib[[4]] <- glm(model1,data=allout[allout$parliament<Split,],family=quasibinomial)
modslib[[5]]<- glm(model,data=allout[allout$parliament>=Split,],family=quasibinomial)

modscon[[4]] <- glm(model1,data=alloutc[alloutc$parliament<Split,],family=quasibinomial)
modscon[[5]]<- glm(model,data=alloutc[alloutc$parliament>=Split,],family=quasibinomial)

(n.ons <- lapply(modslib,function(x) length(x$y)))
(n.ons <- lapply(modscon,function(x) length(x$y)))
# robust standard errors
library(AER)
libres <- mclapply(modslib,coeftest,df=Inf,vcov=vcovHAC,mc.cores=3)
conres <- mclapply(modscon,coeftest,df=Inf,vcov=vcovHAC,mc.cores=3)


libres
conres
########## make tables
coefcol <- function(indata){
  coefs <- round(indata[,1],3)
  st.errors <- round(indata[,2],3)
  stars <- ifelse(abs(indata[,1])>1.96*indata[,2],"*","")
  res <- paste(coefs,stars,sep="")
  st.errs <- paste("(",st.errors,")",sep="")
  st.errs <- ifelse(st.errs=="(0)","(0.00)",st.errs)
  res <- ifelse(res=="0","0.00",res)
  names(res) <- rownames(indata)
  names(st.errs) <- rownames(indata)
  out <- cbind(res,st.errs)
  return(out)
}
test <- lapply(libres,coefcol)
testcon <- lapply(conres,coefcol)
outtest <- merge(test[[1]],test[[3]],by="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- outtest[-c(17:95),] # drops fixed effects
outtest

outtestcon <- merge(testcon[[1]],testcon[[3]],by="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- outtestcon[-c(17:95),] # drops fixed effects
outtestcon
out <- merge(outtest,outtestcon,by="Row.names",sort=F)


longres <- function(wideres){
  odd <- seq(from=1,to=2*nrow(wideres),by=2)
  even <- seq(from=2,to=2*nrow(wideres),by=2)
  mods <- seq(from=2,to=ncol(wideres),by=2)
  resmat <- matrix(NA,nrow=2*nrow(wideres),ncol=(ncol(wideres)+1)/2)
  rescols <- seq(from=2,to=ncol(resmat))
  for (i in mods){
  tmp <- rep(NA,2*nrow(wideres))
  tmp[odd] <- as.character(wideres[,i])
  tmp[even] <- as.character(wideres[,i+1])
  j <- rescols[i/2]
  resmat[,j] <- tmp
  }
  resmat[odd,1] <- wideres[,1] 
  return(resmat)
}
loyaltyres <- longres(out)
colnames(loyaltyres) <- c("variables","base (lib)","fe (lib)","before (lib)","after (lib)","base (con)","fe (con)","before (con)","after (con)")
library(xtable)
print(xtable(loyaltyres,caption="Fraction Logit: Voting Loyalty (Government sponsored votes)",label="tab:loyaltyHix"),include.rownames=F,size="tiny",file="Table-B1.tex",tabular.environment="longtable",floating=FALSE)
###################
