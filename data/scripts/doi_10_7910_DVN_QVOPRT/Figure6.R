rm(list=ls())
# library(MASS)
source("glmmPQL.R")
load("classd-voting-data-postmat.Rdata")
classd.voting.data.postmat <- subset(classd.voting.data.postmat,
  nation %in% c(
    "Denmark",
    "France",
    "Great Britain",
    "Netherlands",
    "West Germany"))

classd.voting.data.postmat <- transform(classd.voting.data.postmat,
      year = (year-sum(range(year))/2)/diff(range(year)),
      Year = year,
      nation = factor(nation,levels=sort(levels(nation[,drop=TRUE]))))

models.classd.time.valpri <- by(classd.voting.data.postmat,
    list(
      Country=classd.voting.data.postmat$nation,
      "Value priorities"=classd.voting.data.postmat$matpmat,
    ),
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
})

models.valpri.time.classd <- by(classd.voting.data.postmat,
    list(
      Country=classd.voting.data.postmat$nation,
      "Class"=classd.voting.data.postmat$classd,
    ),
    function(x) {
            cat(unique(as.character(x$nation)),"\n")
            res <- glmmPQL(cbind(succ,fail)~matpmat*year,
#random=list(eb=pdDiag(~0+classd)),
                            random=~1|eb,
                            data=x,
                            family=binomial(),
#control=lmeControl(maxIter=500,msMaxIter=500)
                            )
            res$response <- subset(x,select=c(succ,fail))
            res
})


zTable <- function(x){
  tTable <- summary(x)$tTable
  z <- tTable[,1]/tTable[,2]
  p <- pnorm(abs(z),lower.tail=FALSE)
  cbind(est=tTable[,1],se=tTable[,2],z,p)
}

coeftabs.classd.time.valpri <- sapply(models.classd.time.valpri,zTable)
dim(coeftabs.classd.time.valpri) <- c(dim(coeftabs.classd.time.valpri)[1]/4,
                                      4,
                                      dim(models.classd.time.valpri))
dimnames(coeftabs.classd.time.valpri) <- c(
    dimnames(zTable(models.classd.time.valpri[[1]])),
    dimnames(models.classd.time.valpri)
  )

coeftabs.valpri.time.classd <- sapply(models.valpri.time.classd,zTable)
dim(coeftabs.valpri.time.classd) <- c(dim(coeftabs.valpri.time.classd)[1]/4,
                                      4,
                                      dim(models.valpri.time.classd))
dimnames(coeftabs.valpri.time.classd) <- c(
    dimnames(zTable(models.valpri.time.classd[[1]])),
    dimnames(models.valpri.time.classd)
  )

N.classd.time.valpri <- sapply(models.classd.time.valpri,function(x)sum(x$response))
N.valpri.time.classd <- sapply(models.valpri.time.classd,function(x)sum(x$response))

dim(N.classd.time.valpri) <- c(1,dim(models.classd.time.valpri))
dim(N.valpri.time.classd) <- c(1,dim(models.valpri.time.classd))


varParms <- function(object){
      vp <-  lapply(object$modelStruct$reStruct,function(x){
        ans <- summary(x)
        sdev <- attr(ans,"stdDev")
        return((object$sigma*sdev)^2)
})
      unlist(c(vp,list(Residual=object$sigma)))
}

varParms.classd.time.valpri <- sapply(models.classd.time.valpri,varParms)
varParms.valpri.time.classd <- sapply(models.valpri.time.classd,varParms)

rown <- rownames(varParms.classd.time.valpri)
dim(varParms.classd.time.valpri) <- c(dim(varParms.classd.time.valpri)[1],dim(models.classd.time.valpri))
dimnames(varParms.classd.time.valpri) <-c(list(rown),dimnames(models.classd.time.valpri))

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
fcoeftabs.classd.time.valpri <- apply(coeftabs.classd.time.valpri,c(1,3,4),function(x)apply.template(x,coef.template))
dim(fcoeftabs.classd.time.valpri) <- c(dim(fcoeftabs.classd.time.valpri)[1]*dim(fcoeftabs.classd.time.valpri)[2],dim(fcoeftabs.classd.time.valpri)[3:4])


rown <- rep("",nrow(fcoeftabs.classd.time.valpri))
rown[seq(length(rown)/2)*2-1] <- dimnames(coeftabs.classd.time.valpri)[[1]]
dimnames(fcoeftabs.classd.time.valpri) <- c(list(rown),dimnames(coeftabs.classd.time.valpri)[3:4])

varp.template <- "($1:2)"
fvarParms.classd.time.valpri <- apply(varParms.classd.time.valpri,1:3,function(x)apply.template(x,varp.template))
dimnames(fvarParms.classd.time.valpri)[[1]] <- dimnames(varParms.classd.time.valpri)[[1]]

fN.classd.time.valpri <- apply(N.classd.time.valpri,2:3,as.character)
dim(fN.classd.time.valpri) <- c(1,dim(fN.classd.time.valpri))
dimnames(fN.classd.time.valpri) <- c(list("Number of cases"),dimnames(models.classd.time.valpri))

library(abind)
fcoeftabs.classd.time.valpri <- abind(fcoeftabs.classd.time.valpri,fvarParms.classd.time.valpri,fN.classd.time.valpri,along=1)
dimnames(fcoeftabs.classd.time.valpri) <- unname(dimnames(fcoeftabs.classd.time.valpri))
dimnames(fcoeftabs.classd.time.valpri)[[1]] <- replace.sub(dimnames(fcoeftabs.classd.time.valpri)[[1]],
    c("eb.(Intercept)","(Intercept)","classd","Service class","Self employed",":year",  "year"),
    c("Survey no.",    "Constant",   "",      "Salariat",     "Self-employed"," * Time","Time")
    )
dimnames(fcoeftabs.classd.time.valpri)[[3]] <- replace.sub(dimnames(fcoeftabs.classd.time.valpri)[[3]],
    c("Materialist","Postmaterialist"),
    c("Mat.","Postmat.")
    )


# write.ftable(ftable(as.table(fcoeftabs.classd.time.valpri[,,c(1,3)]),col.vars=c(2,3)),
#   quote=FALSE,
#   file="windows2000/table-classd-valpri.txt"
#   )
# system("recode ..ms-ansi windows2000/table-classd-valpri.txt")


manual.workers <- unique(subset(classd.voting.data.postmat,
    classd=="Working class",
    select=c(classd,year,Year)))
salariat <- unique(subset(classd.voting.data.postmat,
    classd=="Service class",
    select=c(classd,year,Year)))
selfempl <- unique(subset(classd.voting.data.postmat,
    classd=="Self employed",
    select=c(classd,year,Year)))

manual.workers <- transform(manual.workers,
    succ=0*year,fail=0*year)
salariat <- transform(salariat,
    succ=0*year,fail=0*year)
selfempl <- transform(selfempl,
    succ=0*year,fail=0*year)

library(gdata)

replace.sub <- function(x,patterns,replacements){
  stopifnot(length(patterns)==length(replacements))
  for(i in seq(along=patterns))
    x <- gsub(patterns[i],replacements[i],x,fixed=TRUE)
  x
}

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
#print(vcov(object))
  limits <- c(alpha/2,1-alpha/2)
  limits <- qnorm(limits)
  lower <- meandiff + se.meandiff * limits[1]
  upper <- meandiff + se.meandiff * limits[2]
  data.frame(meandiff=meandiff,se.meandiff=se.meandiff,lower=lower,upper=upper)
}

genPercdiffs <- function(x){
  Salariat <- MeanDiffs(x,manual.workers,salariat,alpha=0.05)
  `Self-employed` <- MeanDiffs(x,manual.workers,selfempl,alpha=0.05)
  res <- combine(Salariat,`Self-employed`)
  names(res) <- replace.sub(names(res),
    c("se.meandiff","meandiff","source"),
    c("se",         "percdiff","classd")
    )
  transform(res,
    percdiff = 100*percdiff,
    se = 100*se,
    lower = 100*lower,
    upper = 100*upper,
    Year = manual.workers$Year
    )
}



percdiffs.classd.valpri <- lapply(models.classd.time.valpri,genPercdiffs)
dim(percdiffs.classd.valpri) <- dim(models.classd.time.valpri)
dimnames(percdiffs.classd.valpri) <- dimnames(models.classd.time.valpri)

rbindValpri <- function(x){
  Materialist <- x$Materialist
  Postmaterialist <- x$Postmaterialist
  names(Materialist) <- replace.sub(names(Materialist),
    c("percdiff","se","lower","upper"),
    c("mat","mat.se","mat.lower","mat.upper")
    )
  names(Postmaterialist) <- replace.sub(names(Postmaterialist),
    c("percdiff","se","lower","upper"),
    c("pmat","pmat.se","pmat.lower","pmat.upper")
    )
  merge(Materialist,Postmaterialist,by=c("classd","Year"))
}

pcv <- apply(percdiffs.classd.valpri,1,rbindValpri)
pcv <- do.call("combine",pcv)
names(pcv) <- gsub("source","country",names(pcv),fixed=TRUE)

# combineList <- function(x,...) do.call("combine",c(x,...))
# pcv <- apply(percdiffs.classd.valpri,2,combineList)
# pcv <- lapply(pcv,function(x){
#     names(x) <- gsub("source","Country",names(x))
#     x
#   })
# pcv <- combineList(pcv)
# names(pcv) <- gsub("source","matpmat",names(pcv))
# 
library(lattice)
trellis.par.set(theme=col.whitebg())

# (xyplot.classdDiffs.postmat <- xyplot( pmat.lower+pmat.upper+mat.lower+mat.upper+pmat+mat~Year|country*classd,
#   data=pcv,
#   type="l",
#   lty=c(1,1,2,2,1,2),
#   lwd=c(1,1,1,1,2,2),
#   col=c(rep("gray50",4),rep("black",2)),
#   ylab="Percentage difference",
#   aspect=1,
#   panel=function(x,y,...){
#                   panel.grid(h=-1,v=0,lty=3)
#                   panel.superpose(x,y,...)},
#   key=list(
#           text=list(c("Postmaterialists","Materialists")),
#           lines=list(lwd=2,lty=c(1,2),col="black"),
#           space="bottom"),
#   par.strip.text=list(cex=.8)
# ))
# 
# trellis.device(postscript,file="TrendLattice-classdDiffs-valpri.eps",
#     paper="special",
#     horizontal=FALSE,
#     width=6,
#     height=4.2,
#     )
#     print(xyplot.classdDiffs.postmat)
# dev.off()
# 
# system("convert -antialias -density 600 TrendLattice-classdDiffs-valpri.eps TrendLattice-classdDiffs-valpri.png")

Mu <- function(object,data,alpha=.05){
  X <- if(is.data.frame(data)) model.matrix.default(object,data=data)
    else as.matrix(data)
  muFunc <- object$family$linkinv
  VarFunc <- object$family$mu.eta
  eta <- c(X %*% fixef(object))
  se.eta <- sqrt(rowSums((X%*%vcov(object))*X))
#print(vcov(object))
  limits <- c(alpha/2,1-alpha/2)
  limits <- qnorm(limits)
  lower <- eta + se.eta * limits[1]
  upper <- eta + se.eta * limits[2]
  mu <- muFunc(eta)
  lower <- muFunc(lower)
  upper <- muFunc(upper)
  data.frame(mu=mu,lower=lower,upper=upper)
}


genPerc <- function(x){
  Salariat <- Mu(x,salariat,alpha=0.05)
  `Self-employed` <- Mu(x,selfempl,alpha=0.05)
  `Manual workers` <- Mu(x,manual.workers,alpha=0.05)
  res <- combine(`Self-employed`,Salariat,`Manual workers`)
  names(res) <- replace.sub(names(res),
    c("mu","source"),
    c("percentage","classd")
    )
  transform(res,
    percentage = 100*percentage,
    lower = 100*lower,
    upper = 100*upper,
    Year = manual.workers$Year
    )
}


percentages.classd.valpri <- lapply(models.classd.time.valpri,genPerc)
dim(percentages.classd.valpri) <- dim(models.classd.time.valpri)
dimnames(percentages.classd.valpri) <- dimnames(models.classd.time.valpri)

rbindValpriPerc <- function(x){
  Materialist <- x$Materialist
  Postmaterialist <- x$Postmaterialist
  names(Materialist) <- replace.sub(names(Materialist),
    c("percentage","lower","upper"),
    c("mat","mat.lower","mat.upper")
    )
  names(Postmaterialist) <- replace.sub(names(Postmaterialist),
    c("percentage","lower","upper"),
    c("pmat","pmat.lower","pmat.upper")
    )
  merge(Materialist,Postmaterialist,by=c("classd","Year"))
}

pct <- apply(percentages.classd.valpri,1,rbindValpriPerc)
pct <- do.call("combine",pct)
names(pct) <- gsub("source","country",names(pct),fixed=TRUE)


(xyplot.classd.postmat <- xyplot( pmat.lower+pmat.upper+mat.lower+mat.upper+pmat+mat~Year|country*classd,
  data=pct,
  type="l",
  lty=c(1,1,2,2,1,2),
  lwd=c(1,1,1,1,2,2),
  col=c(rep("gray50",4),rep("black",2)),
  ylab="Percentage support for labor parties",
  ylim=c(0,100),
  aspect=1,
  panel=function(x,y,...){
                  panel.grid(h=-1,v=0,lty=3)
                  panel.superpose(x,y,...)},
  key=list(
          text=list(c("Postmaterialists","Materialists")),
          lines=list(lwd=2,lty=c(1,2),col="black"),
          space="bottom"),
  par.strip.text=list(cex=.8)
))

trellis.device(postscript,file="TrendLattice-classd-valpri.eps",
    paper="special",
    horizontal=FALSE,
    width=7,
    height=6,
    )
    print(xyplot.classd.postmat)
dev.off()

trellis.device(pdf,file="TrendLattice-classd-valpri.pdf",
    paper="special",
    color=FALSE,
    width=7,
    height=6,
    )
    print(xyplot.classd.postmat)
dev.off()


system("convert -antialias -density 600 TrendLattice-classd-valpri.eps TrendLattice-classd-valpri.png")
system("convert -antialias -density 96 TrendLattice-classd-valpri.eps TrendLattice-classd-valpri-small.png")

# 
# 
# genPercV <- function(x){
#   Materialists <- Mu(x,Materialists,alpha=0.05)
#   Postmaterialists <- Mu(x,Postmaterialists,alpha=0.05)
#   res <- combine(Materialists,Postmaterialists)
#   names(res) <- replace.sub(names(res),
#     c("se.mu","mu","source"),
#     c("se",   "percentage","matpmat")
#     )
#   transform(res,
#     percentage = 100*percentage,
#     se = 100*se,
#     lower = 100*lower,
#     upper = 100*upper
#     )
# }
# 
# 
# rbindValpriClassdPerc <- function(x){
#   Materialist <- x$Materialist
#   Postmaterialist <- x$Postmaterialist
#   res <- combine(Materialist,Postmaterialist)
#   salariat <- subset(res,classd=="Salariat",select=c(percentage,se,lower,upper,Year,source))
#   selfempl <- subset(res,classd=="Self-employed",select=c(percentage,se,lower,upper,Year,source))
#   manual.workers <- subset(res,classd=="Manual workers",select=c(percentage,se,lower,upper,Year,source))
#   names(salariat) <- replace.sub(names(salariat),
#     c("se",         "percentage","lower",         "upper",         "source"),
#     c("se.salariat","salariat",  "lower.salariat","upper.salariat","matpmat")
#     )
#   
#   names(selfempl) <- replace.sub(names(selfempl),
#     c("se",        "percentage","lower",         "upper",         "source"),
#     c("se.selfempl","selfempl","lower.selfempl","upper.selfempl","matpmat")
#     )
#   
#   names(manual.workers) <- replace.sub(names(manual.workers),
#     c("se",         "percentage","lower",         "upper",         "source"),
#     c("se.manwork", "manwork","lower.manwork","upper.manwork","matpmat")
#     )
# 
#   res <- merge(salariat,selfempl,by=c("matpmat","Year"))
#   merge(res,manual.workers,by=c("matpmat","Year"))
#   
# }
# 
# 
# pctv <- apply(percentages.classd.valpri,1,rbindValpriClassdPerc)
# 
# pctv <- do.call("combine",pctv)
# names(pctv) <- gsub("source","country",names(pctv),fixed=TRUE)
# 
# (xyplot.classd.postmat.2 <- xyplot( manwork+salariat+selfempl +
#     lower.manwork+lower.salariat+lower.selfempl +
#     upper.manwork+upper.salariat+upper.selfempl~Year|country*matpmat,
#   data=pctv,
#   type="l",
#   lty=rep(1:3,3),
#   lwd=c(rep(2,3),rep(1,6)),
#   col=c(rep("black",3),rep("gray50",6),),
#   ylab="Percentage",
#   aspect=1,
#   panel=function(x,y,...){
#                   panel.grid(h=-1,v=0)
#                   panel.superpose(x,y,...)},
#   key=list(
#           text=list(c("Manual Workers","Salariat","Self-employed")),
#           lines=list(lwd=2,lty=c(1,2,3),col="black"),
#           space="bottom"),
#   par.strip.text=list(cex=.8)
# ))
