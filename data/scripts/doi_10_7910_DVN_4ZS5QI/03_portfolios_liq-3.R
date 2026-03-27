

#####################################
###
###  RISKY ASSET v. CONTRACT
###

### Certainty equivalent

DiffEUxy <- function(CE) {
  w1 <- matrix(1, Nsimu, 31)
  w2 <- matrix(1, Nsimu, 31)
  w1[,1] <- 1
  w2[,1] <- 1
  for (t in 2:31) {
    w1[,t] <- w1[,t-1] * (1 + (1-phi)*x[,T0-1+t] + CE)
    w2[,t] <- w2[,t-1] * (1 + y[,T0-1+t])
  }
  # EU of w1+CE
  EU1 <- mean( (w1[,2:31])^(1-gamma)/(1-gamma) * pdfH * (1+rf)^(-(1:30)) )
  # EU of w2
  EU2 <- mean( (w2[,2:31])^(1-gamma)/(1-gamma) * pdfH * (1+rf)^(-(1:30)) )
  # Difference in EU
  return(EU2-EU1)
}
CEgainxyperyear <- nleqslv ( 0 , DiffEUxy ) $x



#####################################
###
###  OPTIMAL PORTFOLIOS
###

###  RISKY ASSET + RISK-FREE ASSET

#Expected return as function risky asset share
w <- matrix(1, Nsimu, 31)
fcn1 <- function(share) {
  w[,1] <- 1
  for (t in 2:31) {
    w[,t] <- w[,t-1] * (1 + (1-share)*rf + share*(1-phi)*x[,T0-1+t])
  } 
  return( mean( (w[,2:31])^(1-gamma)/(1-gamma) * pdfH ) )
}

#Solve for risky share that maximizes expected utility
share1 <- optimize ( fcn1, interval=c(0,10), maximum=TRUE) $maximum


###  RISKY ASSET + RISKY-ASSET + CONTRACT

#Expected return as function of risky asset share
w <- matrix(1, Nsimu, 31)
fcn2 <- function(share) {
  share_y <- share[1]
  share_x <- share[2]
  w[,1] <- 1
  for (t in 2:31) {
    w[,t] <- w[,t-1] * (1 + (1-share_y-share_x)*rf + share_y*y[,T0-1+t] + share_x*(1-phi)*x[,T0-1+t])
  }
  return( -mean((w[,2:31])^(1-gamma)/(1-gamma)*pdfH*(1+rf)^(-(1:30))) + LEV*(share_y>1)*(share_y-1)^2 )
}

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optim

LEV <- 100
share2 <- optim( c(.2,.2), fcn2 ) $par
share2y <- share2[1]
share2x <- share2[2]



### Certainty equivalent

DiffEU <- function(CE) {
  w1 <- matrix(1, Nsimu, 31)
  w2 <- matrix(1, Nsimu, 31)
  w1[,1] <- 1
  w2[,1] <- 1
  for (t in 2:(31)) {
    w1[,t] <- w1[,t-1] * (1 + (1-share1)*rf + share1*(1-phi)*x[,T0-1+t] + CE)
    w2[,t] <- w2[,t-1] * (1 + (1-share2y-share2x)*rf + share2y*y[,T0-1+t] + share2x*(1-phi)*x[,T0-1+t])
  }
  # EU of w1+CE
  EU1 <- mean( (w1[,2:31])^(1-gamma)/(1-gamma) * pdfH * (1+rf)^(-(1:30)) )
  # EU of w1+CE
  EU2 <- mean( (w2[,2:31])^(1-gamma)/(1-gamma) * pdfH * (1+rf)^(-(1:30)) )
  # Difference in EU
  return(EU2-EU1)
}
CEgainperyear <- nleqslv ( 0 , DiffEU ) $x

