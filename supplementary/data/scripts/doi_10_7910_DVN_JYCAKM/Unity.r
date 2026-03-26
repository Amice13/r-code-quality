###############
# voting unity
library(AER)
libs <- read.csv("libs.csv")
cons <- read.csv("cons.csv")
cons$org.type <- recode(cons$Original.Type,"c('adjournement','adjournment','ajournment')='other';
                        c('concurence in committee report','concurrence in committe','concurrence in committee',
                        'Concurrence in committee','concurrence in committee and second reading',
                        'concurrence in committee and Second reading','special committee report')='committee';
                        c('Speech of the throne','throne speech','Throne speech','Throne Speech','Trhone speech')='throne';c('first reading','First reading')='other';
                        c('second reading','Second reading','seond reading')='second';c('third reading','Third reading')='third';
                        c('speaker leave','speaker leaves','Speaker leaves')='supply motion';c('resolution','Resolution','Motion','motion')='other';else='other'")

libs$org.type <- recode(libs$Original.Type,"c('adjournement','adjournment','ajournment')='other';
                        c('concurence in committee report','concurrence in committe','concurrence in committee',
                        'Concurrence in committee','concurrence in committee and second reading',
                        'concurrence in committee and Second reading','special committee report')='committee';
                        c('Speech of the throne','throne speech','Throne speech','Throne Speech','Trhone speech')='throne';c('first reading','First reading')='other';
                        c('second reading','Second reading','seond reading')='second';c('third reading','Third reading')='third';
                        c('speaker leave','speaker leaves','Speaker leaves')='supply motion';c('resolution','Resolution','Motion','motion')='other';else='other'")
libs$committee <- ifelse(libs$org.type=="committee",1,0)
libs$second <- ifelse(libs$org.type=="second",1,0)
libs$supply <- ifelse(libs$org.type=="supply motion",1,0)
libs$third <- ifelse(libs$org.type=="third",1,0)
libs$throne<- ifelse(libs$org.type=="throne",1,0)

cons$committee <- ifelse(cons$org.type=="committee",1,0)
cons$second <- ifelse(cons$org.type=="second",1,0)
cons$supply <- ifelse(cons$org.type=="supply motion",1,0)
cons$third <- ifelse(cons$org.type=="third",1,0)
cons$throne<- ifelse(cons$org.type=="throne",1,0)
both <- rbind(libs,cons)
##
standard <- "Rice ~ ownparty + govtype + govparty + per.seat + Origin + supply +supply: Origin + throne+ throne: Origin + third+third:Origin + second + second:Origin + committee + committee:Origin + Parlement + I(Parlement^2)"
standard1 <- "Rice ~ ownparty + govparty + per.seat + Origin + supply +supply: Origin + throne+ throne: Origin + third+third:Origin + second + second:Origin + committee + committee:Origin + Parlement + I(Parlement^2)"
splitmodel <- "Rice ~ ownparty + govtype + govparty + per.seat + Origin + supply +supply: Origin + throne+ throne: Origin + third+third:Origin + second + second:Origin + committee + committee:Origin + Parlement + I(Parlement^2) + split + ownparty:split + govtype:split + govparty:split + per.seat:split + Origin:split + supply:split +supply: Origin:split + throne:split+ throne: Origin:split + third:split+third:Origin:split + second:split + second:Origin:split + committee:split + committee:Origin:split + Parlement:split + I(Parlement^2):split"
fixed <- "Rice ~ ownparty + per.seat + Origin + supply +supply: Origin + throne+ throne: Origin + third+third:Origin + second + second:Origin + committee + committee:Origin + as.factor(Parlement)"
emptyVotes <- "Rice ~ Parlement + I(Parlement^2) +split+ Parlement:split + I(Parlement^2):split"

modlist <- c(standard,fixed)

nonpriv <- aggregate(both$Rice[both$private==0],by=list(both$org.type[both$private==0]),mean,na.rm=TRUE)
priv <- aggregate(both$Rice[both$private==1],by=list(both$org.type[both$private==1]),mean,na.rm=TRUE)
nonpriv;priv

splittest <- c(5:35)
devVotes <-NULL
for (i in 1:length(splittest)){
  both$split <- as.numeric(ifelse(both$Parlement<splittest[i],0,1))
  tmp <- glm(emptyVotes,data=both,family=quasibinomial(logit))
  devVotes[i] <-  deviance(tmp)
  cat(splittest[i]," ")
}
(Split <- splittest[which.min(devVotes)])

both$split <- as.numeric(ifelse(both$Parlement<Split,0,1))
libs$split <- as.numeric(ifelse(libs$Parlement<Split,0,1))
cons$split <- as.numeric(ifelse(cons$Parlement<Split,0,1))
library(parallel)
modslibVotes <- mclapply(modlist,glm,data=libs,family=quasibinomial(logit),mc.cores=length(modlist))
modsconVotes <- mclapply(modlist,glm,data=cons,family=quasibinomial(logit),mc.cores=length(modlist))
modslibVotes[[3]] <- glm(standard1,data=libs[libs$Parlement<Split,],family=quasibinomial(logit))
modslibVotes[[4]] <- glm(standard,data=libs[libs$Parlement>=Split,],family=quasibinomial(logit))

modsconVotes[[3]] <- glm(standard1,data=cons[cons$Parlement<Split,],family=quasibinomial(logit))
modsconVotes[[4]] <- glm(standard,data=cons[cons$Parlement>=Split,],family=quasibinomial(logit))

# robust standard errors
libVotes <- mclapply(modslibVotes,coeftest,df=Inf,vcov=vcovHAC,mc.cores=length(modlist))
conVotes <- mclapply(modsconVotes,coeftest,df=Inf,vcov=vcovHAC,mc.cores=length(modlist))

libVotes
conVotes
# make results table
######### make tables
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
test <- lapply(libVotes,coefcol)
testcon <- lapply(conVotes,coefcol)
outtest <- merge(test[[1]],test[[2]],by="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[3]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- merge(outtest,test[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtest <- outtest[1:18,] # drops fixed effects
outtest

outtestcon <- merge(testcon[[1]],testcon[[2]],by="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[3]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- merge(outtestcon,testcon[[4]],by.x="Row.names",by.y="row.names",all=T,sort=F)
outtestcon <- outtestcon[1:18,] # drops fixed effects
outtestcon
out <- merge(outtest,outtestcon,by="Row.names",sort=F)

unityres <- longres(out)
colnames(unityres) <- c("variables","base (lib)","fe (lib)","before (lib)","after (lib)","base (con)","fe (con)","before (con)","after (con)")
print(xtable(unityres,caption="Fraction Logit: Party Unity", label="tab:unityres"), size="tiny",include.rownames=F, file="Table-2.tex", tabular.environment="longtable", floating=FALSE, sanitize.text.function=identity)
