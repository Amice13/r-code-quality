ZWZ.lme <- function(object,data=object$data){
  nlevs <- object$dims$Q
  if(nlevs > 1) stop("more then one level not yet implemented")
  rs <- object$modelStruct$reStruct
  Z <- model.matrix(rs,data=data)
  iD <- lapply(as.matrix(rs),solve)
  rfact <- object$groups[[1]]
  if(length(object$modelStruct$varStruct))
    #w <- varWeights(object$modelStruct$varStruct)
    w <- sqrt(object$weights)
  else
    w <- 1
  cn <- colnames(Z)
  nc <- ncol(Z)
  Z <- Z*w
  ZWZ <- lapply(levels(rfact), function(rfl){
    Z <- Z[rfact==rfl,]
    ZWZ <- crossprod(Z,Z) + iD[[1]]
    colnames(ZWZ) <- rownames(ZWZ) <- cn
    ZWZ
    })
  if(nc == 1)
    ZWZ <- unlist(ZWZ)
  names(ZWZ) <- levels(rfact)
  ZWZ
  }

ZWZ <- function(object,...)
  UseMethod("ZWZ")
  

XWZ.lme <- function(object,
      formula=eval(object$call$fixed[-2]),
      data=object$data,
      xdata=data,
      subset=TRUE){
  nlevs <- object$dims$Q
  if(nlevs > 1) stop("more then one level not yet implemented")
  rs <- object$modelStruct$reStruct
  Z <- model.matrix(rs,data=data)
  X <- model.matrix(formula,data=xdata)
  rfact <- object$groups[[1]]
  if(length(object$modelStruct$varStruct))
    #w <- varWeights(object$modelStruct$varStruct)
    w <- sqrt(object$weights)
  else
    w <- 1
  cn <- colnames(Z)
  nc <- ncol(Z)
  Z <- Z*w
  X <- X*w
  XWZ <- lapply(levels(rfact), function(rfl){
    Z <- Z[rfact==rfl & subset,,drop=FALSE]
    X <- X[rfact==rfl & subset,,drop=FALSE]
    XWZ <- crossprod(X,Z)
    colnames(XWZ) <- cn
    XWZ
    })
  names(XWZ) <- levels(rfact)
  XWZ
  }



XWZ <- function(object,...)
  UseMethod("XWZ")


XWY.lme <- function(object,
    xformula=eval(object$call$fixed[-2]),
    yformula=xformula,
    xdata=object$data,
    ydata=xdata,
    subset=TRUE){
  X <- model.matrix(xformula,data=xdata)
  Y <- model.matrix(yformula,data=ydata)
  if(length(object$modelStruct$varStruct))
    #w <- varWeights(object$modelStruct$varStruct)
    w <- sqrt(object$weights)
  else
    w <- 1
  X <- X*w
  Y <- Y*w
  crossprod(X[subset,,drop=FALSE],Y[subset,,drop=FALSE])
  }

sumUpMatrices <- function(matlist){
  ans <- matrix(0,nrow(matlist[[1]]),ncol(matlist[[1]]))
  for(i in seq(along=matlist)){
    ans <- ans + matlist[[i]]
    }
  ans
  }
  
XWY <- function(object,...)
  UseMethod("XWY")

mrank <- function(x){
  x.qr <- qr(x)
  x.qr$rank
  }
  
cox.glmmPQL <- function(
        target.model,
        predictor.model,
        predicted.model,
        data,
        use=TRUE,
                test.terms=NULL,
        subset=TRUE
          ){
        subset <- eval(substitute(subset),data,parent.frame())
        if(!is.logical(subset)) stop("'subset' should evaluate to logical")
        subset <- subset & !is.na(subset)
        iK.h <- vcov(predicted.model)
        C.h <- solve(vcov(predicted.model))
        iC.g <- vcov(predictor.model)
        iZWZ.p <- lapply(ZWZ(predictor.model,data=data),solve)
        XWZ.p <- XWZ(predictor.model,data=data)
        YWZ.p <- XWZ(predictor.model,
                  eval(predicted.model$call$fixed)[-2],
                  data=data,
                  subset=subset)
        XWY.p <- XWY(predictor.model,
              eval(predictor.model$call$fixed)[-2],
              eval(predicted.model$call$fixed)[-2],
              xdata=data,subset=subset)
        YWY.p <- XWY(predictor.model,
              eval(predicted.model$call$fixed)[-2],
              xdata=data,subset=subset)
  
        iZWZ.ZWY.p <- lapply(seq(along=iZWZ.p),function(i)
          iZWZ.p[[i]] %*% t(YWZ.p[[i]]))
  
        XWZ.iZWZ.ZWY.p <- sumUpMatrices(lapply(seq(along=iZWZ.ZWY.p),function(i)
          XWZ.p[[i]] %*% iZWZ.ZWY.p[[i]])
          )
  
        YWZ.iZWZ.ZWY.p <- sumUpMatrices(lapply(seq(along=iZWZ.ZWY.p),function(i)
          YWZ.p[[i]] %*% iZWZ.ZWY.p[[i]])
          )
  
        C.h <-  YWY.p - YWZ.iZWZ.ZWY.p
        C.gh <- XWY.p - XWZ.iZWZ.ZWY.p
  
        #vcov.diff <- iK.h%*%(C.h-
        #  crossprod(C.gh,iC.g)%*%C.gh)%*%iK.h
        vcov0.target <- iK.h%*%C.h%*%iK.h
        vcov0.pred <- iK.h%*%(crossprod(C.gh,iC.g)%*%C.gh)%*%iK.h
                vcov.diff <- vcov0.target - vcov0.pred
        vcov.diff.eigen <- eigen(vcov.diff,symmetric=TRUE)
        vcov.diff.orig <- vcov.diff
        vd.evect <- vcov.diff.eigen$vectors
        vd.eval <- vcov.diff.eigen$values
        vd.eval[vd.eval<0] <- 0
        vcov.diff <- vd.evect %*% diag(vd.eval) %*% t(vd.evect)
        dimnames(vcov.diff) <- dimnames(vcov.diff.orig)
  
        DD <- rbind(diag(nrow(vcov0.target)),-diag(nrow(vcov0.target)))
  
        vcov.full <- rbind(
              cbind(vcov0.target,vcov0.pred),
              cbind(vcov0.pred,vcov0.pred)
              )
        #vcov.diff <- crossprod(DD,vcov.full)%*%DD
        #vcov.diff <- vcov(target.model)
        if(!missing(test.terms)){
            canonicalOrder <- function(term) {
                tt <- strsplit(term, ":")
                tt <- lapply(tt, sort)
                sapply(tt, paste, collapse = ":")
            }
            if (inherits(test.terms, "formula"))
                test.terms <- attr(terms(test.terms), "term.labels")
            tt <- attr(terms(target.model), "term.labels")
            aa <- attr(model.matrix.glmmPQL(target.model), "assign")
            useTerms <- which(aa %in% match(canonicalOrder(test.terms),
                canonicalOrder(tt)))
            if (any(is.na(useTerms)) && length(useTerms)==0)
                stop("Terms didn't match:", canonicalOrder(test.terms),
                    canonicalOrder(tt))
          use <- useTerms[use]
                    denDF <- predicted.model$fixDF$terms[test.terms]
                    }
        coef.target <- fixef(target.model)[use]
        coef.predicted <- fixef(predicted.model)[use]
        diff.coef <- (coef.target - coef.predicted)
        se.diff <- sqrt(diag(vcov.diff)[use])
        z.diff <- diff.coef/se.diff
        p.diff <- pnorm(-abs(z.diff))
  
  
        W <- diff.coef %*% ginv(vcov.diff[use,use]) %*% diff.coef
        df.test <- mrank(vcov.diff[use,use])
        p.test <- pchisq(W,df.test,lower.tail=FALSE)
        se.target <- sqrt(diag(vcov(target.model))[use])
        z.target <- coef.target/se.target
        p.target <- pnorm(-abs(z.target))
  
        ans <- cbind(coef.target,
            se.target,
            p.target,
            coef.predicted,
            diff.coef,
            se.diff,
            p.diff)
        colnames(ans) <- c(  "Target",
                  "S.E.Target",
                  "P.Target",
                  "Prediction",
                  "Difference",
                  "S.E.Diff",
                  "P.Diff")
  
        Test <- c(W=W,df=df.test,p=p.test,
              N=sum(target.model$prior.weights)
            )
        structure(Test,
          Parameters=ans#,
          #vcov.diff=vcov.diff,
          #vcov.full=vcov.full
          )
}

