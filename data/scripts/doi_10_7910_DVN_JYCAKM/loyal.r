################
# Analysis of MP level loyalty
# Liberal party
allout <- read.csv("loyaltyLib.cvs") # Liberal
alloutc <- read.csv("loyaltyCon.cvs") # Conservative
allout$parl <- allout$parliament
allout$parl2 <- allout$parliament^2
allout$coh <- allout$cohort
allout$coh2 <- allout$cohort^2
alloutc$parl <- alloutc$parliament
alloutc$parl2 <- alloutc$parliament^2
alloutc$coh <- alloutc$cohort
alloutc$coh2 <- alloutc$cohort^2
#########
# french and english speakers
#liberals
aggregate(allout$French,by=list(allout$parliament),mean)
aggregate(allout$French[allout$Quebec==0],by=list(allout$parliament[allout$Quebec==0]),mean)
mean(allout$French)
mean(allout$French[allout$Quebec==0])
# conservatives
aggregate(alloutc$French,by=list(alloutc$parliament),mean)
aggregate(alloutc$French[alloutc$Quebec==0],by=list(alloutc$parliament[alloutc$Quebec==0]),mean)
mean(alloutc$French)
mean(alloutc$French[alloutc$Quebec==0])
##################
splittest <- c(5:35)
dev <-NULL
devC <- NULL
empty <- "loyalty.x ~  parliament + I(parliament^2) +split+ parliament:split + I(parliament^2):split"
emptygov <- "loyalty.y ~  parliament + I(parliament^2) +split+ parliament:split + I(parliament^2):split"
nomod <- "loyalty.x ~  1"
model <- "loyalty.x ~  participation.norm.x + eff.candidates + Total.vote.norm + Cabinet + govparty + govtype + joiner + last + West + Quebec + Maritime + parl+ parl2 + coh + coh2"
model1 <- "loyalty.x ~  participation.norm.x + eff.candidates + Total.vote.norm + Cabinet + govparty + joiner + last + West + Quebec + Maritime + parl+ parl2 + coh + coh2"
fix_model <- "loyalty.x ~  participation.norm.x + eff.candidates + Total.vote.norm + Cabinet + joiner + last + West + Quebec + Maritime + coh + coh2 + as.factor(parliament)"
fix_cohort_model <- "loyalty.x ~  participation.norm.x + eff.candidates + Total.vote.norm + Cabinet + joiner + last + West + Quebec + Maritime +as.factor(cohort) + as.factor(parliament)"

modlist <- c(empty,model,fix_model,fix_cohort_model)
all <- rbind(allout,alloutc)
for (i in 1:length(splittest)){
  all$split <- as.numeric(ifelse(all$parliament<splittest[i],0,1))
  tmp <- glm(empty,data=all,family=quasibinomial(logit))
  dev[i] <-  deviance(tmp)
  cat(splittest[i]," ")
}
(Split <- splittest[which.min(dev)])

allout$split <- as.numeric(ifelse(allout$parliament<Split,0,1))
alloutc$split <- as.numeric(ifelse(alloutc$parliament<Split,0,1))

library(parallel)
modslib <- mclapply(modlist,glm,data=allout,family=quasibinomial(logit),mc.cores=3)
modscon <- mclapply(modlist,glm,data=alloutc,family=quasibinomial(logit),mc.cores=3)

modslib[[5]] <- glm(model1,data=allout[allout$parliament<Split,],family=quasibinomial(logit))
modslib[[6]]<- glm(model,data=allout[allout$parliament>=Split,],family=quasibinomial(logit))

modscon[[5]] <- glm(model1,data=alloutc[alloutc$parliament<Split,],family=quasibinomial(logit))
modscon[[6]]<- glm(model,data=alloutc[alloutc$parliament>=Split,],family=quasibinomial(logit))


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
outtest <- merge(test[[2]],test[[4]],by="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[6]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- outtest[-c(17:95),] # drops fixed effects
outtest

outtestcon <- merge(testcon[[2]],testcon[[4]],by="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[6]],by.x="Row.names",by.y="row.names",all=T,sort=F)
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
print(xtable(loyaltyres,caption="Fraction Logit: Voting Loyalty",label="tab:loyalty"),include.rownames=F,size="tiny",file="Table-1.tex",tabular.environment="longtable",floating=FALSE)
###################
# loyalty with backgorund variables
# results with language and background

model2 <- "loyalty.x ~ participation.norm.x + eff.candidates +Total.vote.norm+Cabinet +govparty+govtype+ joiner + last +West + Quebec + Maritime + French + occupation.business + occupation.educ + occupation.health + occupation.news + occupation.farmer + occupation.business:West + occupation.educ:West + occupation.health:West + occupation.news:West + occupation.farmer:West +occupation.business:Quebec + occupation.educ:Quebec + occupation.health:Quebec + occupation.news:Quebec + occupation.farmer:Quebec+occupation.business:Maritime+ occupation.educ:Maritime + occupation.health:Maritime + occupation.news:Maritime + occupation.farmer:Maritime + parl+ parl2 + coh + coh2"
model2early<- "loyalty.x ~ participation.norm.x + eff.candidates +Total.vote.norm+Cabinet +govparty+ joiner + last +West + Quebec + Maritime + French + occupation.business + occupation.educ + occupation.health + occupation.news + occupation.farmer + occupation.business:West + occupation.educ:West + occupation.health:West + occupation.news:West + occupation.farmer:West +occupation.business:Quebec + occupation.educ:Quebec + occupation.health:Quebec + occupation.news:Quebec + occupation.farmer:Quebec+occupation.business:Maritime+ occupation.educ:Maritime + occupation.health:Maritime + occupation.news:Maritime + occupation.farmer:Maritime + parl+ parl2 + coh + coh2"
fix_model2 <- "loyalty.x ~ participation.norm.x + eff.candidates + Total.vote.norm+ joiner + last +West + Quebec + Maritime+ French + occupation.business + occupation.educ + occupation.health + occupation.news + occupation.farmer + occupation.business:West + occupation.educ:West + occupation.health:West + occupation.news:West + occupation.farmer:West +occupation.business:Quebec + occupation.educ:Quebec + occupation.health:Quebec + occupation.news:Quebec + occupation.farmer:Quebec+occupation.business:Maritime+ occupation.educ:Maritime + occupation.health:Maritime + occupation.news:Maritime + occupation.farmer:Maritime + as.factor(parliament)"
fix_cohort_model2 <- "loyalty.x ~ participation.norm.x + eff.candidates +Total.vote.norm + joiner + last +West + Quebec + Maritime+French + occupation.business + occupation.educ + occupation.health + occupation.news + occupation.farmer + occupation.business:West + occupation.educ:West + occupation.health:West + occupation.news:West + occupation.farmer:West +occupation.business:Quebec + occupation.educ:Quebec + occupation.health:Quebec + occupation.news:Quebec + occupation.farmer:Quebec+occupation.business:Maritime+ occupation.educ:Maritime + occupation.health:Maritime + occupation.news:Maritime + occupation.farmer:Maritime +as.factor(cohort) + as.factor(parliament)"

modlist2 <- list(model2,fix_model2,fix_cohort_model2)
modslib2 <- mclapply(modlist2,glm,data=allout,family=quasibinomial(logit),mc.cores=3)
alloutc <- alloutc[alloutc$parliament<40,]
modscon2 <- mclapply(modlist2,glm,data=alloutc,family=quasibinomial(logit),mc.cores=3)
Split <- 16
modslib2[[4]] <- glm(model2early,data=allout[allout$parliament<Split,],family=quasibinomial)
modslib2[[5]] <- glm(model2,data=allout[allout$parliament>=Split,],family=quasibinomial)
modscon2[[4]] <- glm(model2early,data=alloutc[alloutc$parliament<Split,],family=quasibinomial)
modscon2[[5]] <- glm(model2,data=alloutc[alloutc$parliament>=Split,],family=quasibinomial)

# robust standard errors
libres2 <- mclapply(modslib2,coeftest,df=Inf,vcov=vcovHAC,mc.cores=3)
conres2 <- mclapply(modscon2,coeftest,df=Inf,vcov=vcovHAC,mc.cores=3)

# make results table
test <- lapply(libres2,coefcol)
testcon <- lapply(conres2,coefcol)
outtest <- merge(test[[1]],test[[3]],by="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- outtest[1:37,] # drops fixed effects
outtest

(n.onsBack <- lapply(modslib2,function(x) length(x$y)))
(n.onsBakkcon <- lapply(modscon2,function(x) length(x$y)))

outtestcon <- merge(testcon[[1]],testcon[[3]],by="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- outtestcon[1:37,] # drops fixed effects
outtestcon
out <- merge(outtest,outtestcon,by="Row.names",sort=F)

loyaltyback <- longres(out)
colnames(loyaltyback) <- c("variables","base (lib)","fe (lib)","before (lib)","after (lib)","base (con)","fe (con)","before (con)","after (con)")
#print(xtable(loyaltyback,caption="Fraction Logit: Voting Loyalty, MPs background",label="tab:loyaltyback"),include.rownames=F,size="tiny",file="loyaltyback.tex",tabular.environment="longtable",floating=FALSE)

modlist2 <- list(model2,fix_model2,fix_cohort_model2)
mods2 <- mclapply(modlist2,glm,data=all,family=quasibinomial(logit),mc.cores=3)
modslib2 <- mclapply(modlist2,glm,data=allout,family=quasibinomial(logit),mc.cores=3)
alloutc <- alloutc[alloutc$parliament<40,]
modscon2 <- mclapply(modlist2,glm,data=alloutc,family=quasibinomial(logit),mc.cores=3)
Split <- 16
modslib2[[4]] <- glm(model2early,data=allout[allout$parliament<Split,],family=quasibinomial)
modslib2[[5]] <- glm(model2,data=allout[allout$parliament>=Split,],family=quasibinomial)
modscon2[[4]] <- glm(model2early,data=alloutc[alloutc$parliament<Split,],family=quasibinomial)
modscon2[[5]] <- glm(model2,data=alloutc[alloutc$parliament>=Split,],family=quasibinomial)

# robust standard errors
libres2 <- mclapply(modslib2,coeftest,df=Inf,vcov=vcovHAC,mc.cores=3)
conres2 <- mclapply(modscon2,coeftest,df=Inf,vcov=vcovHAC,mc.cores=3)

# make results table
test <- lapply(libres2,coefcol)
testcon <- lapply(conres2,coefcol)
outtest <- merge(test[[1]],test[[3]],by="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- outtest[1:37,] # drops fixed effects
outtest

(n.onsBack <- lapply(modslib2,function(x) length(x$y)))
(n.onsBakkcon <- lapply(modscon2,function(x) length(x$y)))

outtestcon <- merge(testcon[[1]],testcon[[3]],by="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[5]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- outtestcon[1:37,] # drops fixed effects
outtestcon
out <- merge(outtest,outtestcon,by="Row.names",sort=F)

loyaltyback <- longres(out)
colnames(loyaltyback) <- c("variables","base (lib)","fe (lib)","before (lib)","after (lib)","base (con)","fe (con)","before (con)","after (con)")
print(xtable(loyaltyback,caption="Fraction Logit: Voting Loyalty, MPs background",label="tab:loyaltyback"),include.rownames=F,size="tiny",file="Table-3 (Full Appendix-D).tex",tabular.environment="longtable",floating=FALSE)

