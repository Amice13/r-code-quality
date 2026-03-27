##########################################################################
#########                                                        #########
######### Giacomo Chiozza                                        #########
######### R functions for Mallar's simultaneous equations model  #########
######### with clustered standard errors                         #########
######### (multiple imputation functions, M=5)                   #########
#########                                                        #########
##########################################################################



probit.nr <- function(X,y,method='BFGS',predictors=colnames(X),intercept=TRUE){
    if (nrow(na.omit(X)) != length(y)) {
        y <- na.omit(cbind(X,y))[,ncol(cbind(X,y))]
        }
    if (intercept) {
          X <- na.omit(cbind(X,1))
          colnames(X)[ncol(X)] <- 'Intercept'
        }
    N <- nrow(na.omit(X))
    negLogL <- function(b,X,y) {
        p <- as.vector(pnorm(X%*%b))
        - sum(y*log(p) + (1 - y)*log(1 - p))
        }
    grad <- function(b,X,y){
        p <- as.vector(pnorm(X%*%b))
##        - apply((y/p - (1-y)/(1-p))*as.vector(dnorm(X[,1:ncol(X)]%*%b))*X[,1:ncol(X)],2,sum)
        - apply((y/p - (1-y)/(1-p))*as.vector(dnorm(X%*%b))*X,2,sum)
            }
    proc <- optim(rep(0, ncol(X)), negLogL, gr=grad,
        hessian=TRUE, method=method, X=X, y=y)
    pr.1 <- X%*%proc$par
    g1 <- NULL
    g1[y==1] <- dnorm(pr.1[y==1])/pnorm(pr.1[y==1])
    g1[y==0] <- -dnorm(pr.1[y==0])/(1-pnorm(pr.1[y==0]))
    mz.r2 <- (N-1)*var(pr.1+g1)/(N + (N-1)*var(pr.1+g1))
    pp <- sum(y)/length(y)
##    lognull <- sum(y*log(pp) + (1 - y)*log(1 - pp))
    lognull <- N*(pp*log(pp) + (1 - pp)*log(1 - pp))
    r2 <- 1 - (-proc$value)/lognull
    e.r2 <- 1 - ((-proc$value)/lognull)^(-2*lognull/N)
    result <- list(coefficients=proc$par, var=solve(proc$hessian), 
        deviance=2*proc$value, converged=proc$convergence == 0, predictors=predictors,
        N=N,mz.r2=mz.r2,lognull=lognull,r2=r2,e.r2=e.r2)
    class(result) <- 'glm.nr'
    result
    }


summary.probit.nr <- function(object) {
    b <- object$coefficients
    se <- sqrt(diag(object$var))
    z <- b/se
    table <- round(cbind(b, se, z, 2*(1-pnorm(abs(z)))),6)
    colnames(table) <- c('b', 'se', 'z', 'p')
    rownames(table) <- object$predictors
    print(table)
#    cat('\nDeviance =', object$deviance,'\n')
#    if (!object$converged) cat('\n Note: *** lreg did not converge ***\n')
    }






probit.clust <- function(X,y,group,predictors=colnames(X),...){
##    mmodel <- glm(y~X,family=binomial(probit))
    model <- probit.nr(X,y)
    if (nrow(na.omit(X)) != length(y)) {
        y <- na.omit(cbind(X,y))[,ncol(cbind(X,y))]
        group <- na.omit(cbind(X,group))[,ncol(cbind(X,group))]
        }
    X <- na.omit(cbind(X,1))
    colnames(X)[ncol(X)] <- 'Intercept'
    sc <- (y/as.vector(pnorm(X%*%model$coef)) - (1-y)/(1-as.vector(pnorm(X%*%model$coef))))*as.vector(dnorm(X%*%model$coef))*X
##    sc <- na.omit(estfun(mmodel))  ## requires library(sandwich)
    uu <- matrix(NA,dim(tapply(group,group,sum))[1],ncol(X))
        for (i in 1:ncol(X)) {
            uu[,i] <- tapply(sc[,i],INDEX=group,sum)
            }
    dg <- dim(uu)[1]
    W <- matrix(NA,ncol(X),ncol(X))
    W[1,1] <- crossprod(uu[,1])
    W[2:ncol(X),1] <- crossprod(uu[,1],uu[,2:ncol(X)])
    W[1,2:ncol(X)] <- crossprod(uu[,2:ncol(X)],uu[,1])
    W[2:ncol(X),2:ncol(X)] <- crossprod(uu[,2:ncol(X)])
    W <- dg/(dg-1)*W
##    V1 <- vcov(model)%*%W%*%vcov(model)
    V1 <- model$var%*%W%*%model$var
    std.err <- sqrt(diag(V1))
    p <- 2*pnorm(abs(model$coef/std.err),lower.tail=FALSE)
    list(coefficients=as.vector(model$coef), var=V1, std.err=std.err,p=p, predictors=predictors,
         deviance=model$deviance, N=model$N, r2=model$r2, mz.r2=model$mz.r2,e.r2=model$e.r2)
    }





probit.seq <- function(X,X1,y1,X2,y2,group,...){
##    X <- merge(X1,X2)
    
    W <- na.omit(cbind(X1,X2,y1,y2,group))
##    W <- na.omit(cbind(X,y1,y2,group))
    X1 <- W[,1:ncol(X1)]
    X2 <- W[,(ncol(X1)+1):(ncol(W)-3)]
    y1 <- W[,ncol(W)-2]
    y2 <- W[,ncol(W)-1]
    group <- W[,ncol(W)]
    X <- na.omit(X)
    
    ## reduced-form 1
    mr.1 <- probit.clust(X,y1,group)
    pr.1 <- cbind(X,1)%*%mr.1$coef
    v21 <-  mr.1$var
    g1 <- NULL
    g1[y1==1] <- dnorm(pr.1[y1==1])/pnorm(pr.1[y1==1])
    g1[y1==0] <- -dnorm(pr.1[y1==0])/(1-pnorm(pr.1[y1==0]))
    
    ## reduced-form 2
    mr.2 <- probit.clust(X,y2,group)
    pr.2 <- cbind(X,1)%*%mr.2$coef
    v22 <-  mr.2$var
    g2 <- NULL
    g2[y2==1] <- dnorm(pr.2[y2==1])/pnorm(pr.2[y2==1])
    g2[y2==0] <- -dnorm(pr.2[y2==0])/(1-pnorm(pr.2[y2==0]))    
    
    ## structural 1
    X <- cbind(X,1)
    Z1 <- cbind(pr.2,X1)
    colnames(Z1)[1] <- 'y2hat'
    ms.1 <- probit.clust(Z1,y1,group)
    ps.1 <- cbind(Z1,1)%*%ms.1$coef
    r1 <- NULL
    r1[y1==1] <- dnorm(ps.1[y1==1])/pnorm(ps.1[y1==1])
    r1[y1==0] <- -dnorm(ps.1[y1==0])/(1-pnorm(ps.1[y1==0]))
    u1 <- r1*ms.1$coef[1]*r1
    u2 <- r1*g2
    Z1 <- cbind(Z1,1)
    C1 <- matrix(NA,nrow=ncol(Z1),ncol=ncol(X))
    C1[nrow(C1),1] <- crossprod(u1*Z1[,ncol(Z1)],X[,1])
    C1[nrow(C1),2:ncol(C1)] <- crossprod(u1*Z1[,ncol(Z1)],X[,2:ncol(X)])
    C1[1:nrow(C1)-1,ncol(C1)] <- crossprod(u1*Z1[,1:ncol(Z1)-1],X[,ncol(X)])
    C1[1:nrow(C1)-1,1:ncol(C1)-1] <- crossprod(u1*Z1[,1:ncol(Z1)-1],X[,1:ncol(X)-1])
    R1 <- matrix(NA,nrow=ncol(Z1),ncol=ncol(X))
    R1[nrow(R1),1] <- crossprod(u2*Z1[,ncol(Z1)],X[,1])
    R1[nrow(R1),2:ncol(R1)] <- crossprod(u2*Z1[,ncol(Z1)],X[,2:ncol(X)])
    R1[1:nrow(R1)-1,ncol(R1)] <- crossprod(u2*Z1[,1:ncol(Z1)-1],X[,ncol(X)])
    R1[1:nrow(R1)-1,1:ncol(R1)-1] <- crossprod(u2*Z1[,1:ncol(Z1)-1],X[,1:ncol(X)-1])
    VA1 <- C1%*%v22%*%t(C1) - R1%*%v22%*%t(C1) - C1%*%v22%*%t(R1)
    VC1 <- ms.1$var + ms.1$var%*%VA1%*%ms.1$var
    
    ## structural 2
    Z2 <- cbind(pr.1,X2)
    colnames(Z2)[1] <- 'y1hat'
    ms.2 <- probit.clust(Z2,y2,group)
    ps.2 <- na.omit(cbind(Z2,1))%*%ms.2$coef
    r2 <- NULL
    r2[y2==1] <- dnorm(ps.2[y2==1])/pnorm(ps.2[y2==1])
    r2[y2==0] <- -dnorm(ps.2[y2==0])/(1-pnorm(ps.2[y2==0]))
    u1 <- r2*ms.2$coef[1]*r2
    u2 <- r2*g1
    Z2 <- na.omit(cbind(Z2,1))
    C1 <- matrix(NA,nrow=ncol(Z2),ncol=ncol(X))
    C1[nrow(C1),1] <- crossprod(u1*Z2[,ncol(Z2)],X[,1])
    C1[nrow(C1),2:ncol(C1)] <- crossprod(u1*Z2[,ncol(Z2)],X[,2:ncol(X)])
    C1[1:nrow(C1)-1,ncol(C1)] <- crossprod(u1*Z2[,1:ncol(Z2)-1],X[,ncol(X)])
    C1[1:nrow(C1)-1,1:ncol(C1)-1] <- crossprod(u1*Z2[,1:ncol(Z2)-1],X[,1:ncol(X)-1])
    R1 <- matrix(NA,nrow=ncol(Z2),ncol=ncol(X))
    R1[nrow(R1),1] <- crossprod(u2*Z2[,ncol(Z2)],X[,1])
    R1[nrow(R1),2:ncol(R1)] <- crossprod(u2*Z2[,ncol(Z2)],X[,2:ncol(X)])
    R1[1:nrow(R1)-1,ncol(R1)] <- crossprod(u2*Z2[,1:ncol(Z2)-1],X[,ncol(X)])
    R1[1:nrow(R1)-1,1:ncol(R1)-1] <- crossprod(u2*Z2[,1:ncol(Z2)-1],X[,1:ncol(X)-1])
    VA2 <- C1%*%v21%*%t(C1) - R1%*%v21%*%t(C1) - C1%*%v21%*%t(R1)
    VC2   = ms.2$var + ms.2$var%*%VA2%*%ms.2$var
    
    result <- list(coef.mr1=mr.1$coef,coef.mr2=mr.2$coef,var.mr1=v21,var.mr2=v22,
                   coef.ms1=ms.1$coef,coef.ms2=ms.2$coef,var.ms1=VC1,var.ms2=VC2,
                   pred.mr1=mr.1$predictors,pred.mr2=mr.2$predictors,
                   pred.ms1=ms.1$predictors,pred.ms2=ms.2$predictors,
                   N=mr.1$N,
                   dev.mr1=mr.1$deviance,dev.mr2=mr.2$deviance,
                   dev.ms1=ms.1$deviance,dev.ms2=ms.2$deviance,
                   r2.mr1=mr.1$r2,r2.mr2=mr.2$r2,
                   r2.ms1=ms.1$r2,r2.ms2=ms.2$r2,
                   mz.r2.mr1=mr.1$mz.r2,mz.r2.mr2=mr.2$mz.r2,
                   mz.r2.ms1=ms.1$mz.r2,mz.r2.ms2=ms.2$mz.r2,
                   e.r2.mr1=mr.1$e.r2,e.r2.mr2=mr.2$e.r2,
                   e.r2.ms1=ms.1$e.r2,e.r2.ms2=ms.2$e.r2)
    }



summary.probit.seq <- function(object) {
    b.mr1 <- object$coef.mr1
    se.mr1 <- sqrt(diag(object$var.mr1))
    z.mr1 <- b.mr1/se.mr1
    table <- round(cbind(b.mr1, se.mr1, z.mr1, 2*(1-pnorm(abs(z.mr1)))),3)
    colnames(table) <- c('b', 'se', 'z', 'p')
    rownames(table) <- object$pred.mr1
    cat('\nReduced-Form Equation 1\n')
    print(table)

    b.mr2 <- object$coef.mr2
    se.mr2 <- sqrt(diag(object$var.mr2))
    z.mr2 <- b.mr2/se.mr2
    table <- round(cbind(b.mr2, se.mr2, z.mr2, 2*(1-pnorm(abs(z.mr2)))),3)
    colnames(table) <- c('b', 'se', 'z', 'p')
    rownames(table) <- object$pred.mr2
    cat('\nReduced-Form Equation 2\n')
    print(table)
    
    b.ms1 <- object$coef.ms1
    se.ms1 <- sqrt(diag(object$var.ms1))
    z.ms1 <- b.ms1/se.ms1
    table <- round(cbind(b.ms1, se.ms1, z.ms1, 2*(1-pnorm(abs(z.ms1)))),3)
    colnames(table) <- c('b', 'se', 'z', 'p')
    rownames(table) <- object$pred.ms1
    cat('\nStructural Equation 1\n')
    print(table)
    
    b.ms2 <- object$coef.ms2
    se.ms2 <- sqrt(diag(object$var.ms2))
    z.ms2 <- b.ms2/se.ms2
    table <- round(cbind(b.ms2, se.ms2, z.ms2, 2*(1-pnorm(abs(z.ms2)))),3)
    colnames(table) <- c('b', 'se', 'z', 'p')
    rownames(table) <- object$pred.ms2
    cat('\nStructural Equation 2\n')
    print(table)
        
#    cat('\nDeviance =', object$deviance,'\n')
#    if (!object$converged) cat('\n Note: *** lreg did not converge ***\n')
    }




mi.results <- function(obj1,obj2,obj3,obj4,obj5,confidence=0.95){
        c.mr1 <- cbind(obj1$coef.mr1,obj2$coef.mr1,obj3$coef.mr1,obj4$coef.mr1,obj5$coef.mr1)
        c.mr2 <- cbind(obj1$coef.mr2,obj2$coef.mr2,obj3$coef.mr2,obj4$coef.mr2,obj5$coef.mr2)
        c.ms1 <- cbind(obj1$coef.ms1,obj2$coef.ms1,obj3$coef.ms1,obj4$coef.ms1,obj5$coef.ms1)
        c.ms2 <- cbind(obj1$coef.ms2,obj2$coef.ms2,obj3$coef.ms2,obj4$coef.ms2,obj5$coef.ms2)
        
        v.mr1 <- cbind(diag(obj1$var.mr1),diag(obj2$var.mr1),diag(obj3$var.mr1),diag(obj4$var.mr1),diag(obj5$var.mr1))
        v.mr2 <- cbind(diag(obj1$var.mr2),diag(obj2$var.mr2),diag(obj3$var.mr2),diag(obj4$var.mr2),diag(obj5$var.mr2))
        v.ms1 <- cbind(diag(obj1$var.ms1),diag(obj2$var.ms1),diag(obj3$var.ms1),diag(obj4$var.ms1),diag(obj5$var.ms1))
        v.ms2 <- cbind(diag(obj1$var.ms2),diag(obj2$var.ms2),diag(obj3$var.ms2),diag(obj4$var.ms2),diag(obj5$var.ms2))

        coef.mr1 <- apply(c.mr1,1,mean)
        coef.mr2 <- apply(c.mr2,1,mean)
        coef.ms1 <- apply(c.ms1,1,mean)
        coef.ms2 <- apply(c.ms2,1,mean)
        
        vbar.mr1 <- apply(v.mr1,1,mean)
        vbar.mr2 <- apply(v.mr2,1,mean)
        vbar.ms1 <- apply(v.ms1,1,mean)
        vbar.ms2 <- apply(v.ms2,1,mean)
        
        bm.mr1 <- apply(c.mr1,1,var)
        bm.mr2 <- apply(c.mr2,1,var)
        bm.ms1 <- apply(c.ms1,1,var)
        bm.ms2 <- apply(c.ms2,1,var)
        
        m <- ncol(c.mr1)
        
        tm.mr1 <- vbar.mr1 + ((1+1/m))*bm.mr1
        tm.mr2 <- vbar.mr2 + ((1+1/m))*bm.mr2
        tm.ms1 <- vbar.ms1 + ((1+1/m))*bm.ms1
        tm.ms2 <- vbar.ms2 + ((1+1/m))*bm.ms2
        
        rem.mr1 <- (1 + (1/m)) * bm.mr1/vbar.mr1
        rem.mr2 <- (1 + (1/m)) * bm.mr2/vbar.mr2
        rem.ms1 <- (1 + (1/m)) * bm.ms1/vbar.ms1
        rem.ms2 <- (1 + (1/m)) * bm.ms2/vbar.ms2
        
        nu.mr1 <- (m - 1) * (1 + (1/rem.mr1))^2        
        nu.mr2 <- (m - 1) * (1 + (1/rem.mr2))^2
        nu.ms1 <- (m - 1) * (1 + (1/rem.ms1))^2
        nu.ms2 <- (m - 1) * (1 + (1/rem.ms2))^2
        
        alpha <- 1 - (1 - confidence)/2
        
        low.mr1 <- coef.mr1 - qt(alpha, nu.mr1) * sqrt(tm.mr1)
        low.mr2 <- coef.mr2 - qt(alpha, nu.mr2) * sqrt(tm.mr2)
        low.ms1 <- coef.ms1 - qt(alpha, nu.ms1) * sqrt(tm.ms1)
        low.ms2 <- coef.ms2 - qt(alpha, nu.ms2) * sqrt(tm.ms2)
        
        up.mr1 <- coef.mr1 + qt(alpha, nu.mr1) * sqrt(tm.mr1)
        up.mr2 <- coef.mr2 + qt(alpha, nu.mr2) * sqrt(tm.mr2)
        up.ms1 <- coef.ms1 + qt(alpha, nu.ms1) * sqrt(tm.ms1)
        up.ms2 <- coef.ms2 + qt(alpha, nu.ms2) * sqrt(tm.ms2)
        
        p.val.mr1 <- 2 * (1 - pt(abs(coef.mr1/sqrt(tm.mr1)), nu.mr1))
        p.val.mr2 <- 2 * (1 - pt(abs(coef.mr2/sqrt(tm.mr2)), nu.mr2))
        p.val.ms1 <- 2 * (1 - pt(abs(coef.ms1/sqrt(tm.ms1)), nu.ms1))
        p.val.ms2 <- 2 * (1 - pt(abs(coef.ms2/sqrt(tm.ms2)), nu.ms2))
        
        fminf.mr1 <- (rem.mr1 + 2/(nu.mr1 + 3))/(rem.mr1 + 1)
        fminf.mr2 <- (rem.mr2 + 2/(nu.mr2 + 3))/(rem.mr2 + 1)
        fminf.ms1 <- (rem.ms1 + 2/(nu.ms1 + 3))/(rem.ms1 + 1)
        fminf.ms2 <- (rem.ms2 + 2/(nu.ms2 + 3))/(rem.ms2 + 1)
        
        result <- list(coef.mr1=coef.mr1,coef.mr2=coef.mr2,
                       coef.ms1=coef.ms1,coef.ms2=coef.ms2,
                       std.err.mr1=sqrt(tm.mr1),std.err.mr2=sqrt(tm.mr2),
                       std.err.ms1=sqrt(tm.ms1),std.err.ms2=sqrt(tm.ms2),
                       df.mr1=nu.mr1,df.mr2=nu.mr2,
                       df.ms1=nu.ms1,df.ms2=nu.ms2,
                       pval.mr1=p.val.mr1,pval.mr2=p.val.mr2,
                       pval.ms1=p.val.ms1,pval.ms2=p.val.ms2,
                       r.mr1=rem.mr1,r.mr2=rem.mr2,
                       r.ms1=rem.ms1,r.ms2=rem.ms2,
                       fminf.mr1=fminf.mr1,fminf.mr2=fminf.mr2,
                       fminf.ms1=fminf.ms1,fminf.ms2=fminf.ms2,
                       pred.mr1=obj1$pred.mr1,pred.mr2=obj1$pred.mr2,
                       pred.ms1=obj1$pred.ms1,pred.ms2=obj1$pred.ms2)
        result
        }
        
        
        
summary.mi.results <- function(object){
        table <- round(cbind(object$coef.mr1,object$std.err.mr1,object$pval.mr1),3)
        colnames(table) <- c('b', 'se', 'p')
        rownames(table) <- object$pred.mr1
        cat('\nReduced-Form Equation 1\n')
        print(table)
        
        
        table <- round(cbind(object$coef.mr2,object$std.err.mr2,object$pval.mr2),3)
        colnames(table) <- c('b', 'se', 'p')
        rownames(table) <- object$pred.mr2
        cat('\nReduced-Form Equation 2\n')
        print(table)
        
        table <- round(cbind(object$coef.ms1,object$std.err.ms1,object$pval.ms1),3)
        colnames(table) <- c('b', 'se', 'p')
        rownames(table) <- object$pred.ms1
        cat('\nStructural Equation 1\n')
        print(table)
        
        table <- round(cbind(object$coef.ms2,object$std.err.ms2,object$pval.ms2),3)
        colnames(table) <- c('b', 'se', 'p')
        rownames(table) <- object$pred.ms2
        cat('\nStructural Equation 2\n')
        print(table)
        }
