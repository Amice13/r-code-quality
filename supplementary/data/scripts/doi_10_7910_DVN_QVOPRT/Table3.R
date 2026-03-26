source("glmmPQL.R")
source("Cox.R")
exec.cox <- function(
        predictor.formula,
        data
        ){
        target.model <- glmmPQL(cbind(succ,fail)~classd*year,
                            random=~1|eb,
                            data=data,
                            family=binomial()
                            )
        predictor.formula <- update(cbind(succ,fail)~.,predictor.formula)
        predictor.model <- glmmPQL(predictor.formula,
                            random=~1|eb,
                            data=data,
                            family=binomial()
                            )
        #print(summary(predictor.model)$tTable)                    
        predictor.model$call$fixed <- eval(predictor.formula)
        prediction <- predictor.model$fitted.values
        succ.pred <- prediction*data$freq
        fail.pred <- (1-prediction)*data$freq
        predicted.model <- glmmPQL(cbind(succ.pred,fail.pred)~classd*year,
                            random=~1|eb,
                            data=data,
                            family=binomial()
                            )
        structure(cox.glmmPQL(
            target=target.model,
            predictor=predictor.model,
            predicted=predicted.model,
            data=data,
            test.terms=~year+classd:year,
            use=c(3:4)
            ),year.range=range(data$Year))
}

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

cox.postmat <- by(classd.voting.data.postmat,
                  classd.voting.data.postmat$nation,
                  function(x) {
                    cat(unique(as.character(x$nation)),"\n")
                    exec.cox(predictor.formula=~matpmat/classd+year,
                      data=x)
                  })

cox.postmat.extended <- by(classd.voting.data.postmat,
                  classd.voting.data.postmat$nation,
                  function(x) {
                    cat(unique(as.character(x$nation)),"\n")
                    exec.cox(predictor.formula=~matpmat/classd+matpmat*year,
                      data=x)
                  })
                  

load("classd-voting-data-newstv.Rdata")
classd.voting.data.newstv <- subset(classd.voting.data.newstv,
  nation %in% c(
    "Denmark",
    "France",
    "Great Britain",
    "Netherlands",
    "West Germany"))

classd.voting.data.newstv <- transform(classd.voting.data.newstv,
      year = (year-sum(range(year))/2)/diff(range(year)),
      Year = year,
      nation = factor(nation,levels=sort(levels(nation[,drop=TRUE]))))

cox.newstv <- by(classd.voting.data.newstv,
                  classd.voting.data.newstv$nation,
                  function(x) {
                    cat(unique(as.character(x$nation)),"\n")
                    exec.cox(predictor.formula=~newstv/classd+year,
                      data=x)
                  })


load("classd-voting-data-educrec.Rdata")
classd.voting.data.educrec <- subset(classd.voting.data.educrec,
  nation %in% c(
    "Denmark",
    "France",
    "Great Britain",
    "Netherlands",
    "West Germany"))

classd.voting.data.educrec <- transform(classd.voting.data.educrec,
      year = (year-sum(range(year))/2)/diff(range(year)),
      Year = year,
      nation = factor(nation,levels=sort(levels(nation[,drop=TRUE]))))

cox.educrec <- by(classd.voting.data.educrec,
                  classd.voting.data.educrec$nation,
                  function(x) {
                    cat(unique(as.character(x$nation)),"\n")
                    exec.cox(predictor.formula=~educrec/classd+year,
                      data=x)
})


load("classd-voting-data-poldisc.Rdata")
classd.voting.data.poldisc <- subset(classd.voting.data.poldisc,
  nation %in% c(
    "Denmark",
    "France",
    "Great Britain",
    "Netherlands",
    "West Germany"))

classd.voting.data.poldisc <- transform(classd.voting.data.poldisc,
      year = (year-sum(range(year))/2)/diff(range(year)),
      Year = year,
      nation = factor(nation,levels=sort(levels(nation[,drop=TRUE]))))

cox.poldisc <- by(classd.voting.data.poldisc,
                  classd.voting.data.poldisc$nation,
                  function(x) {
                    cat(unique(as.character(x$nation)),"\n")
                    exec.cox(predictor.formula=~poldisc/classd+year,
                      data=x)
})
    

nations <- names(cox.postmat)

source("applyTemplate.R")

collect.cox.diffs <- function(nation){
  tests <- list(
    "Watching TV news" = cox.newstv[[nation]],
    Education = cox.educrec[[nation]],
    "Discussing Politics" = cox.poldisc[[nation]],
    "Value priorities" = cox.postmat[[nation]],
    "Value priorities (extended model)" = cox.postmat.extended[[nation]]
    )
  get.baseline.parms <- function(test){
      parms <- attr(test,"Parameters")
      parms["year",]
  }
  get.salariat.parms <- function(test){
      parms <- attr(test,"Parameters")
      parms["classdService class:year",]
    }
  get.selfempl.parms <- function(test){
      parms <- attr(test,"Parameters")
      parms["classdSelf employed:year",]
    }
  salariat.parms <- sapply(tests,get.salariat.parms)
  selfempl.parms <- sapply(tests,get.selfempl.parms)
  res <- array(
      c(salariat.parms,selfempl.parms),
      dim=c(dim(salariat.parms),2),
      dimnames = c(dimnames(salariat.parms),
        list(c("Salariat","Self-employed"))
        )
    )
  res
}

collect.cox.tests <- function(nation){
  tests <- list(
    "Watching TV news" = cox.newstv[[nation]],
    Education = cox.educrec[[nation]],
    "Discussing Politics" = cox.poldisc[[nation]],
    "Value priorities" = cox.postmat[[nation]],
    "Value priorities (extended model)" = cox.postmat.extended[[nation]]
    )
  do.call("cbind",tests)  
}

library(abind)
all.cox.diffs <- sapply(nations,collect.cox.diffs,simplify=FALSE)
all.cox.diffs <- do.call("abind",c(all.cox.diffs,along=4))



cox.template <- c("($4:2)","($1:2)","($5:2)($7:*)")
all.cox.diffs <- apply(all.cox.diffs,2:4,apply.template,cox.template)
dimnames(all.cox.diffs)[[1]] <- c("Prediction","Actual change","Difference")
dimnames(all.cox.diffs)[[4]] <- c("Den","Fra","GBr","Nth","WGe")

source("writeout-ftable.R")
writeout.ftable(ftable(all.cox.diffs,row.vars=c(2,3,1),col.vars=c(4)),quote=FALSE,file="")
writeout.ftable(ftable(all.cox.diffs,row.vars=c(2,3,1),col.vars=c(4)),quote=FALSE,
  file="windows2000/all-cox-differences.txt")

all.cox.tests <- sapply(nations,collect.cox.tests,simplify=FALSE)
all.cox.tests <- do.call("abind",c(all.cox.tests,along=3))
test.template <- c("($1:1)","($2:0)","($3:3)","($4:0)")
all.cox.tests <- apply(all.cox.tests,2:3,apply.template,test.template)

dimnames(all.cox.tests)[[1]] <- c("W","Df","p","Number of cases")
dimnames(all.cox.tests)[[3]] <- c("Den","Fra","GBr","Nth","WGe")
writeout.ftable(ftable(all.cox.tests[4,,,],col.vars=3,quote=FALSE,file=""))
writeout.ftable(ftable(all.cox.tests,col.vars=c(2,3)),quote=FALSE,file="windows2000/all-cox-tests.txt")
