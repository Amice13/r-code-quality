

##########################################################################
#########                                                        #########
######### Eelco van der Maat                                     #########
######### two stage probit (analytic)  2013                      #########
######### with clustered standard errors                         #########
#########                                                        #########
#########  Run "functions"                                       #########
#########                                                        #########
##########################################################################


# two stage probit: analytically derived standard errors
# adopted from Chiozza and modified to be non-recursive
# ie. the first stage informs the second stage but not vice versa

# takes 6 arguments:
# X1 is the indep. vars in the second stage "structural" model: eg X1 <- cbind(var1 var2)
# X2 is the indep. vars in the first stage "reduced" model
# X is all vars in X1 & X2
# y1 is the dep var in the second stage "structural" model
# y2 is the dep var n the first stage "reduced" model
# group is the clustering variable

### Note that this still does not handle missing data well: 
### make sure all columns are of equal length


probit.ts <- function(X,X1,y1,X2,y2,group,...){
  
# EvdM: merge variables from two equations
   
W <- na.omit(cbind(X1,X2,y1,y2,group))
X1 <- W[,1:ncol(X1)]
X2 <- W[,(ncol(X1)+1):(ncol(W)-3)]
y1 <- W[,ncol(W)-2]
y2 <- W[,ncol(W)-1]
group <- W[,ncol(W)]
X <- na.omit(X)


# EvdM: first reduced regression equation 
mr.2 <- probit.clust(X,y2,group) 
pr.2 <- cbind(X,1)%*%mr.2$coef # add pnorm() to get predicted value ?
v22 <-  mr.2$var    
g2 <- NULL
g2[y2==1] <- dnorm(pr.2[y2==1])/pnorm(pr.2[y2==1])
g2[y2==0] <- -dnorm(pr.2[y2==0])/(1-pnorm(pr.2[y2==0]))  
summary.probit.nr(mr.2)

## structural 1: with clustered probit
X <- cbind(X,1)
Z1 <- cbind(pr.2,X1) # modify pr.2 to pr.3
colnames(Z1)[1] <- 'y2hat'
ms.1 <- probit.clust(Z1,y1,group)
ps.1 <- cbind(Z1,1)%*%ms.1$coef
summary.probit.nr(ms.1)
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



result <- list(coef.mr2=mr.2$coef,
               var.mr2=v22,
               coef.ms1=ms.1$coef,
               var.ms1=VC1,
               pred.mr2=mr.2$predictors,
               pred.ms1=ms.1$predictors,
               N=mr.2$N,
               dev.mr2=mr.2$deviance,
               dev.ms1=ms.1$deviance,
               r2.mr2=mr.2$r2,
               r2.ms1=ms.1$r2,
               mz.r2.mr2=mr.2$mz.r2,
               mz.r2.ms1=ms.1$mz.r2,
               e.r2.mr2=mr.2$e.r2,
               e.r2.ms1=ms.1$e.r2)

}


summary.probit.ts <- function(object) {

  
  b.mr2 <- object$coef.mr2
  se.mr2 <- sqrt(diag(object$var.mr2))
  z.mr2 <- b.mr2/se.mr2
  table <- round(cbind(b.mr2, se.mr2, z.mr2, 2*(1-pnorm(abs(z.mr2)))),3)
  colnames(table) <- c('b', 'se', 'z', 'p')
  rownames(table) <- object$pred.mr2
  cat('\nReduced-Form Equation 2\n')
  print(table)
  
  MZ_R2 <- round(cbind(object$mz.r2.mr2),3)
  rownames(MZ_R2) <- 'McKelvey & Zavoina R^2'
  print(MZ_R2)
  
  b.ms1 <- object$coef.ms1
  se.ms1 <- sqrt(diag(object$var.ms1))
  z.ms1 <- b.ms1/se.ms1
  table <- round(cbind(b.ms1, se.ms1, z.ms1, 2*(1-pnorm(abs(z.ms1)))),3)
  colnames(table) <- c('b', 'se', 'z', 'p')
  rownames(table) <- object$pred.ms1
  cat('\nStructural Equation 1\n')
  print(table)

  MZ_R2 <- round(cbind(object$mz.r2.ms1),3)
  rownames(MZ_R2) <- 'McKelvey & Zavoina R^2'
  print(MZ_R2)

  #    cat('\nDeviance =', object$deviance,'\n')
  #    if (!object$converged) cat('\n Note: *** lreg did not converge ***\n')
}

