rm(list=ls())
load("classd-voting-data-nocontrols.Rdata")
load("churchat-voting-data-nocontrols.Rdata")
source("glmmPQL.R")

classd.voting.data.nocontrols <- transform(classd.voting.data.nocontrols,
  nation = factor(nation,levels=sort(levels(nation))),
  #year = (year - max(year))/(max(year)-min(year)),
  year = (year - mean(c(max(year),min(year))))/10,
  ebclassd = interaction(eb,classd),
  Year = year)

churchat.voting.data.nocontrols <- transform(churchat.voting.data.nocontrols,
  nation = factor(nation,levels=sort(levels(nation))),
  #year = (year - max(year))/(max(year)-min(year)),
  year = (year - mean(c(max(year),min(year))))/10,
  ebchurchat = interaction(eb,churchat),
  Year = year)
  
oexists <- exists
exists <- function(x,...) if(x=="nlminb") FALSE else oexists(x,...)
  
models.classd.time <- by(classd.voting.data.nocontrols,
    classd.voting.data.nocontrols$nation,
    function(x) {
            cat(unique(as.character(x$nation)),"\n")
            res <- glmmPQL(cbind(succ,fail)~classd*year,
                            #random=list(eb=pdDiag(~0+classd)),
                            random=~1|eb,
                            data=x,
                            family=binomial(),
                            #control=lmeControl(maxIter=500,msMaxIter=500)
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
                            #random=list(eb=pdDiag(~0+churchat)),
                            random=~1|eb,
                            data=x,
                            family=binomial(),
                            #control=lmeControl(maxIter=500,msMaxIter=500)
                            )
            res$response <- subset(x,select=c(succ,fail))
            res
      }
    )
  

zTable <- function(x){
  tTable <- summary(x)$tTable
  z <- tTable[,1]/tTable[,2]
  p <- pnorm(abs(z),lower.tail=FALSE)
  cbind(tTable[,1],tTable[,2],z,p)
}

coeftabs.classd.time <- sapply(models.classd.time,zTable)

dim(coeftabs.classd.time)  <- c(dim(coeftabs.classd.time)[1]/4,4,dim(coeftabs.classd.time)[2])
dimnames(coeftabs.classd.time) <- c(
    dimnames(zTable(models.classd.time[[1]])),
    list(names(models.classd.time))
  )

coeftabs.churchat.time <- sapply(models.churchat.time,zTable)

dim(coeftabs.churchat.time)  <- c(dim(coeftabs.churchat.time)[1]/4,4,dim(coeftabs.churchat.time)[2])
dimnames(coeftabs.churchat.time) <- c(
    dimnames(zTable(models.churchat.time[[1]])),
    list(names(models.churchat.time))
  )
 
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

replace.sub <- function(x,patterns,replacements){
  stopifnot(length(patterns)==length(replacements))
  for(i in seq(along=patterns))
    x <- gsub(patterns[i],replacements[i],x,fixed=TRUE)
  x
}

source("applyTemplate.R")    
coef.template = c(
                "($1:2)($4:*)",
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

fcoeftabs.classd.time <- noquote(rbind(
    fcoeftabs.classd.time,
    #apply(fixDF.classd.time[-1,],1:2,as.character),
    apply(varParms.classd.time,1:2,function(x)apply.template(x,varp.template)),
    as.character(N.classd.time)
    ))
rownames(fcoeftabs.classd.time) <- replace.sub(rownames(fcoeftabs.classd.time),
    c("eb.(Intercept)","(Intercept)","classd","Service class","Self employed",":year",  "year"),
    c("Survey no.",    "Constant",   "",      "Salariat",     "Self-employed"," * Time","Time")
    )


fcoeftabs.churchat.time <- noquote(rbind(
    fcoeftabs.churchat.time,
#apply(fixDF.classd.time[-1,],1:2,as.character),
    apply(varParms.churchat.time,1:2,function(x)apply.template(x,varp.template)),
    as.character(N.churchat.time)
    ))
rownames(fcoeftabs.churchat.time) <- replace.sub(rownames(fcoeftabs.churchat.time),
    c("eb.(Intercept)","(Intercept)","churchat",":year",  "year"),
    c("Survey no.",    "Constant",   "",        " * Time","Time"  )
    )

print(fcoeftabs.classd.time)
print(fcoeftabs.churchat.time)

MeanDiffs <- function(object,data1,data2,alpha=.05){
  X1 <- if(is.data.frame(data1)) model.matrix.default(object,data=data1)
    else as.matrix(data1)
  X2 <- if(is.data.frame(data2)) model.matrix.default(object,data=data2)
    else as.matrix(data2)
  muFunc <- object$family$linkinv
  VarFunc <- object$family$mu.eta
  eta1 <- c(X1 %*% fixef(object))
  eta2 <- c(X2 %*% fixef(object))
  mu1 <- muFunc(eta1)
  mu2 <- muFunc(eta2)
  meandiff <- mu1-mu2
  diff.mu.eta.X <- VarFunc(eta1)*X1 - VarFunc(eta2)*X2
  se.meandiff <- sqrt(rowSums((diff.mu.eta.X%*%vcov(object))*diff.mu.eta.X))
  limits <- c(alpha/2,1-alpha/2)
  limits <- qnorm(limits)
  lower <- meandiff + se.meandiff * limits[1]
  upper <- meandiff + se.meandiff * limits[2]
  list(meandiff=meandiff,se.meandiff=se.meandiff,lower=lower,upper=upper)
}

salariat.percdiffs <- lapply(models.classd.time,function(x){
  manual.workers <- subset(x$model.frame,classd=="Working class")
  salariat <- subset(x$model.frame,classd=="Service class")
  perc.manual.workers <- with(manual.workers,
      100*succ/(succ+fail))
  perc.salariat <- with(salariat,
      100*succ/(succ+fail))
  
  salariat.meandiffs <- MeanDiffs(x,manual.workers,salariat,alpha=0.05)

  data.frame(
    percdiff.raw=perc.manual.workers-perc.salariat,
    percdiff.model=100*salariat.meandiffs$meandiff,
    percdiff.model.se=100*salariat.meandiffs$se.meandiff,
    percdiff.model.lower=100*salariat.meandiffs$lower,
    percdiff.model.upper=100*salariat.meandiffs$upper,
    Year=salariat$year*10 + with(classd.voting.data.nocontrols,mean(c(min(Year),max(Year))))
    )
})

selfempl.percdiffs <- lapply(models.classd.time,function(x){
  manual.workers <- subset(x$model.frame,classd=="Working class")
  selfempl <- subset(x$model.frame,classd=="Self employed")
  perc.manual.workers <- with(manual.workers,
      100*succ/(succ+fail))
  perc.selfempl <- with(selfempl,
      100*succ/(succ+fail))
  
  selfempl.meandiffs <- MeanDiffs(x,manual.workers,selfempl,alpha=0.05)

  data.frame(
    percdiff.raw=perc.manual.workers-perc.selfempl,
    percdiff.model=100*selfempl.meandiffs$meandiff,
    percdiff.model.se=100*selfempl.meandiffs$se.meandiff,
    percdiff.model.lower=100*selfempl.meandiffs$lower,
    percdiff.model.upper=100*selfempl.meandiffs$upper,
    Year=selfempl$year*10 + with(classd.voting.data.nocontrols,mean(c(min(Year),max(Year))))
    )
})

churchat.percdiffs <- lapply(models.churchat.time,function(x){
  weekly <- subset(x$model.frame,churchat=="Weekly")
  never <- subset(x$model.frame,churchat=="Never")
  perc.weekly <- with(weekly,
      100*succ/(succ+fail))
  perc.never <- with(never,
      100*succ/(succ+fail))
  
  never.meandiffs <- MeanDiffs(x,weekly,never,alpha=0.05)

  data.frame(
    percdiff.raw=perc.weekly-perc.never,
    percdiff.model=100*never.meandiffs$meandiff,
    percdiff.model.se=100*never.meandiffs$se.meandiff,
    percdiff.model.lower=100*never.meandiffs$lower,
    percdiff.model.upper=100*never.meandiffs$upper,
    Year=never$year*10 + with(churchat.voting.data.nocontrols,mean(c(min(Year),max(Year))))
    )
})


library(gdata)
salariat.percdiffs <- do.call(combine,salariat.percdiffs)
selfempl.percdiffs <- do.call(combine,selfempl.percdiffs)
churchat.percdiffs <- do.call(combine,churchat.percdiffs)

library(lattice)
trellis.par.set(theme=col.whitebg())

percdiff.plot <- function(data,...){
  xyplot(
    percdiff.raw+percdiff.model+percdiff.model.lower+percdiff.model.upper~Year|source,
    data=data,
    allow.multiple=TRUE,
    as.table=TRUE,
    aspect=1,
   layout=c(7,1),
    type="b",
    lwd=1.5,
    lty=c(0,1,2,2),
    par.settings=list(
      superpose.symbol=list(
        col=c("gray60","black","black","black"),
  #pch=19
        ),
      superpose.line=list(col=c("gray50","black","black","black"))
    ),
    cex=c(.3,0,0,0),
    ylab="Percentage difference",
    panel=function(x,y,...){
                  panel.grid(h=-1,v=0)
                  panel.superpose(x,y,...)
  },
  ...)
}

print(xyplot.salariat <- percdiff.plot(salariat.percdiffs,ylim=c(-5,85)))

trellis.device(postscript,file="TrendLattice-salariat.eps",
    paper="special",
    horizontal=FALSE,
    width=12,
    height=3
    )
    print(xyplot.salariat)
dev.off()
trellis.device(pdf,file="TrendLattice-salariat.pdf",
    paper="special",
    width=12,
    height=3,
    color=FALSE
    )
    print(xyplot.salariat)
dev.off()
  
print(xyplot.selfempl <- percdiff.plot(selfempl.percdiffs,ylim=c(-5,85)))

trellis.device(postscript,file="TrendLattice-selfempl.eps",
    paper="special",
    horizontal=FALSE,
    width=12,
    height=3
    )
    print(xyplot.selfempl)
dev.off()
trellis.device(pdf,file="TrendLattice-selfempl.pdf",
    paper="special",
    width=12,
    height=3,
    color=FALSE
    )
    print(xyplot.selfempl)
dev.off()

print(xyplot.churchat <- percdiff.plot(churchat.percdiffs,ylim=c(-5,85)))

trellis.device(postscript,file="TrendLattice-churchat.eps",
    paper="special",
    horizontal=FALSE,
    width=12,
    height=3
    )
    print(xyplot.churchat)
dev.off()
trellis.device(pdf,file="TrendLattice-churchat.pdf",
    paper="special",
    width=12,
    height=3,
    color=FALSE
    )
    print(xyplot.churchat)
dev.off()

