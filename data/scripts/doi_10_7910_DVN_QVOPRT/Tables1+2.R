rm(list=ls())
load("classd-voting-data-nocontrols.Rdata")
load("churchat-voting-data-nocontrols.Rdata")
#library(MASS)
source("glmmPQL.R")

classd.voting.data.nocontrols <- transform(classd.voting.data.nocontrols,
  nation = factor(nation,levels=sort(levels(nation))),
  #year = (year - max(year))/(max(year)-min(year)),
  year = (year - mean(max(year),min(year)))/10,
  ebclassd = interaction(eb,classd),
  Year = year)

churchat.voting.data.nocontrols <- transform(churchat.voting.data.nocontrols,
  nation = factor(nation,levels=sort(levels(nation))),
  #year = (year - max(year))/(max(year)-min(year)),
  year = (year - mean(max(year),min(year)))/10,
  ebchurchat = interaction(eb,churchat),
  Year = year)
  
oexists <- exists
exists <- function(x,...) if(x=="nlminb") FALSE else oexists(x,...)
  
models.classd.time <- by(classd.voting.data.nocontrols,
    classd.voting.data.nocontrols$nation,
    function(x) {
            cat(unique(as.character(x$nation)),"\n")
            res <- glmmPQL(cbind(succ,fail)~classd*year,
                            random=~1|eb/classd,
                            data=x,
                            family=binomial(),
                            )
            res$response <- subset(x,select=c(succ,fail))
            res
      }
    )
        
    
    
models.churchat.time <- by(churchat.voting.data.nocontrols,
    churchat.voting.data.nocontrols$nation,
    function(x) {
            cat(unique(as.character(x$nation)),"\n")
            res <- glmmPQL(cbind(succ,fail)~churchat*year,
                            random=~1|eb/churchat,
                            data=x,
                            family=binomial(),
                            )
            res$response <- subset(x,select=c(succ,fail))
            res
      }
    )
  
  
coeftabs.classd.time <- sapply(models.classd.time,function(x){
    summary(x)$tTable
  })

dim(coeftabs.classd.time)  <- c(dim(coeftabs.classd.time)[1]/5,5,dim(coeftabs.classd.time)[2])
dimnames(coeftabs.classd.time) <- c(
    dimnames(summary(models.classd.time[[1]])$tTable),
    list(names(models.classd.time))
  )

coeftabs.churchat.time <- sapply(models.churchat.time,function(x){
    summary(x)$tTable
})

dim(coeftabs.churchat.time)  <- c(dim(coeftabs.churchat.time)[1]/5,5,dim(coeftabs.churchat.time)[2])
dimnames(coeftabs.churchat.time) <- c(
    dimnames(summary(models.churchat.time[[1]])$tTable),
    list(names(models.churchat.time))
  )
 
fixDF.classd.time <- sapply(models.classd.time,function(x)x$fixDF$terms)
fixDF.churchat.time <- sapply(models.churchat.time,function(x)x$fixDF$terms)

N.classd.time <- sapply(models.classd.time,function(x)sum(x$response))
N.churchat.time <- sapply(models.churchat.time,function(x)sum(x$response))

varParms <- function(object){
      vp <-  lapply(object$modelStruct$reStruct,function(x){
        ans <- summary(x)
        sdev <- attr(ans,"stdDev")
        return((object$sigma*sdev)^2)
      })
      unlist(c(vp,list(Residual=object$sigma)))
    }

varParms.classd.time <- sapply(models.classd.time,varParms)    
varParms.churchat.time <- sapply(models.churchat.time,varParms)    

source("applyTemplate.R")    
coef.template = c(
                "($1:2)($5:*)",
                "(($2:2))"
                )    
fcoeftabs.classd.time <- apply(coeftabs.classd.time,c(1,3),function(x)apply.template(x,coef.template))
dim(fcoeftabs.classd.time) <- c(dim(fcoeftabs.classd.time)[1]*dim(fcoeftabs.classd.time)[2],dim(fcoeftabs.classd.time)[3])

colnames(fcoeftabs.classd.time) <- names(models.classd.time)
rown <- rep("",nrow(fcoeftabs.classd.time))
rown[seq(length(rown)/2)*2-1] <- dimnames(coeftabs.classd.time)[[1]]
rownames(fcoeftabs.classd.time) <- rown

fcoeftabs.churchat.time <- apply(coeftabs.churchat.time,c(1,3),function(x)apply.template(x,coef.template))
dim(fcoeftabs.churchat.time) <- c(dim(fcoeftabs.churchat.time)[1]*dim(fcoeftabs.churchat.time)[2],dim(fcoeftabs.churchat.time)[3])

colnames(fcoeftabs.churchat.time) <- names(models.churchat.time)
rown <- rep("",nrow(fcoeftabs.churchat.time))
rown[seq(length(rown)/2)*2-1] <- dimnames(coeftabs.churchat.time)[[1]]
rownames(fcoeftabs.churchat.time) <- rown

varp.template <- "($1:2)"

print(fcoeftabs.classd.time <- rbind(
    fcoeftabs.classd.time,
    apply(fixDF.classd.time[-1,],1:2,as.character),
    as.character(N.classd.time),
    apply(varParms.classd.time,1:2,function(x)apply.template(x,varp.template))
    ))
