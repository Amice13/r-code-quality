
#This code calculates the return statistics of:
#1. Insurer asset vs. contract
#2. Optimal portfolios with vs. without contract


#####################################
###
###  INSURER ASSET vs. CONTRACT
###

### Returns

#Matrix of insurer asset return
wxm <- matrix(1, Nsimu, H+1)
for (t in 2:(H+1)) {
  wxm[,t] <- wxm[,t-1] * (1 + (1-phi)*x[,T0-1+t])
}

#Matrix of contract return
wym <- matrix(1, Nsimu, H+1)
for (t in 2:(H+1)) {
  wym[,t] <- wym[,t-1] * (1 + y[,T0-1+t])
}


### Certainty equivalent gain of contract vs. insurer asset

# H-year CE
fcn <- function(CE) {
  return( mean((wym[,H+1])^(1-gamma)/(1-gamma)) - mean((wxm[,H+1]+CE)^(1-gamma)/(1-gamma)) )
}
CEgainxy <- nleqslv ( 0 , fcn ) $x

# Annual CE
DiffEUxy <- function(CE) {
  w1 <- matrix(1, Nsimu, H+1)
  w2 <- matrix(1, Nsimu, H+1)
  w1[,1] <- 1
  w2[,1] <- 1
  for (t in 2:(H+1)) {
    w1[,t] <- w1[,t-1] * (1 + (1-phi)*x[,T0-1+t] + CE)
    w2[,t] <- w2[,t-1] * (1 + y[,T0-1+t])
  }
  return( mean((w2[,H+1])^(1-gamma)/(1-gamma)) - mean((w1[,H+1])^(1-gamma)/(1-gamma)) )
}
CEgainxyperyear <- nleqslv ( 0 , DiffEUxy ) $x


### Save return statistics

#Insurer asset
w <- wxm
stat_asset <- paste(
  "Insurer assets      ", "1", "", " ", "",
  round( mean(w[,H+1])-(1+rf)^H, 3)*100,
  round( sd(w[,H+1]), 3)*100,
  round( (mean(w[,H+1])-(1+rf)^H) / sd(w[,H+1]), 2),
  sep = " & ")

#Contract
w <- wym
stat_contract <- paste(
  "Contract            ", " ", "", "1", "",
  round( mean(w[,H+1])-(1+rf)^H, 3)*100,
  round( sd(w[,H+1]), 3)*100,
  round( (mean(w[,H+1])-(1+rf)^H) / sd(w[,H+1]), 2),
  round(CEgainxy, 3)*100,
  sep = " & ")



#####################################
###
###  OPTIMAL PORTFOLIOS
###

###  OPTIMAL PORTFOLIO WITH INSURER ASSET + RISK-FREE ASSET

#Expected return as function risky asset share
w <- matrix(1, Nsimu, H+1)
fcn1 <- function(share) {
  w[,1] <- 1
  for (t in 2:(H+1)) {
    w[,t] <- w[,t-1] * (1 + (1-share)*rf + share*(1-phi)*x[,T0-1+t])
  }
  return( mean( (w[,H+1])^(1-gamma)/(1-gamma) ) )
}

#Solve for risky share that maximizes expected utility
share1 <- optimize ( fcn1, interval=c(0,10), maximum=TRUE) $maximum


###  OPTIMAL PORTFOLIO WITH INSURER ASSET + RISK-FREE ASSET + CONTRACT

#Expected return as function of risky asset share
w <- matrix(1, Nsimu, H+1)
fcn2 <- function(share) {
  share_y <- share[1]
  share_x <- share[2]
  w[,1] <- 1
  for (t in 2:(H+1)) {
    w[,t] <- w[,t-1] * (1 + (1-share_y-share_x)*rf + share_y*y[,T0-1+t] + share_x*(1-phi)*x[,T0-1+t])
  }
  return( -mean((w[,H+1])^(1-gamma)/(1-gamma)) + LEV*(share_y>1)*(share_y-1)^2 )
}

#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/optim

#We impose the constraint that the contract share is <=1 by adding a large penalty for a share >1
LEV <- 100
share2 <- optim( c(.2,.2), fcn2 ) $par
share2y <- share2[1]
share2x <- share2[2]


### Returns

w1m <- matrix(1, Nsimu, H+1)
w2m <- matrix(1, Nsimu, H+1)
for (t in 2:(H+1)) {
  w1m[,t] <- w1m[,t-1] * (1 + (1-share1)*rf + share1*(1-phi)*x[,T0-1+t])
  w2m[,t] <- w2m[,t-1] * (1 + (1-share2y-share2x)*rf + share2y*y[,T0-1+t] + share2x*(1-phi)*x[,T0-1+t])
}


### Certainty equivalent

# H-year CE
fcn <- function(CE) {
  return( mean((w2m[,H+1])^(1-gamma)/(1-gamma)) - mean((w1m[,H+1]+CE)^(1-gamma)/(1-gamma)) )
}
CEgain <- nleqslv ( 0 , fcn ) $x

# Annual CE
DiffEU <- function(CE) {
  w1 <- matrix(1, Nsimu, H+1)
  w2 <- matrix(1, Nsimu, H+1)
  w1[,1] <- 1
  w2[,1] <- 1
  for (t in 2:(H+1)) {
    w1[,t] <- w1[,t-1] * (1 + (1-share1)*rf + share1*(1-phi)*x[,T0-1+t] + CE)
    w2[,t] <- w2[,t-1] * (1 + (1-share2y-share2x)*rf + share2y*y[,T0-1+t] + share2x*(1-phi)*x[,T0-1+t])
  }
  return( mean((w2[,H+1])^(1-gamma)/(1-gamma)) - mean((w1[,H+1])^(1-gamma)/(1-gamma)) )
}
CEgainperyear <- nleqslv ( 0 , DiffEU ) $x


### Save return statistics

w <- w1m
stat_optimalwithout <- paste(
  "Optimal w/o contract",
  round(share1, 2),
  1-round(share1, 2),
  " ",
  "",
  round( mean(w[,H+1])-(1+rf)^H, 3)*100,
  round( sd(w[,H+1]), 3)*100,
  round( (mean(w[,H+1])-(1+rf)^H) / sd(w[,H+1]), 2),
  sep = " & ")

w <- w2m
stat_optimalwith <- paste(
  "Optimal w/ contract ",
  round(share2x, 2),
  1-round(share2x+share2y, 2),
  round(share2y, 2),
  "",
  round( mean(w[,H+1])-(1+rf)^H, 3)*100,
  round( sd(w[,H+1]), 3)*100,
  round( (mean(w[,H+1])-(1+rf)^H) / sd(w[,H+1]), 2),
  round(CEgain, 3)*100,
  sep = " & ")
