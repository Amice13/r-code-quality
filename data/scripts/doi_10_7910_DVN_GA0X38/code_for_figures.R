
rm(list=ls())


####################
# EMPIRICAL GRAPHS
####################

# Set to appropriate directory
setwd("~/Dropbox/writings/timing/replication")

# Reading in files produced stata cleaning code code
# Files named by country codes 
bf <- read.csv("439year.csv")
ghana<- read.csv("452year.csv")
kenya <- read.csv("501year.csv")
nigeria <- read.csv("475year.csv")

avgcont <- read.csv("dist2elec30contest.csv")
avgnc <- read.csv("dist2elec30NOcontest.csv")


avgcont180 <- read.csv("dist2elec180contest.csv")
avgnc180 <- read.csv("dist2elec180NOcontest.csv")
avgcont365 <- read.csv("dist2elec365contest.csv")
avgnc365 <- read.csv("dist2elec365NOcontest.csv")

avgpro <- read.csv("dist2elec45progovt.csv")
avganti <- read.csv("dist2elec30antigovt2.csv")


# Figure 1 for paper
par(mfrow=c(2,2), mar=c(3,3,1.2,2), mgp=c(2, .7, 0))
plot(kenya$year, kenya$numb_allevents, type="l", xlab="Year", 
     ylab="Violent Events", axes=FALSE, ylim=c(0, max(kenya$numb_allevents)))
box()
axis(1)
axis(2, at=c(0, max(kenya$numb_allevents)))
mtext("Kenya", side=3)
abline(v=kenya$year[kenya$numbelec_all_contest > 0])

plot(ghana$year, ghana$numb_allevents, type="l", xlab="Year", 
     ylab="Violent Events", axes=FALSE, ylim=c(0, max(ghana$numb_allevents)))
abline(v=ghana$year[ghana$numbelec_all_contest > 0])
box()
axis(1)
axis(2, at=c(0, max(ghana$numb_allevents)))
mtext("Ghana", side=3)

plot(bf$year, bf$numb_allevents, type="l", xlab="Year", 
     ylab="Violent Events", axes=FALSE, ylim=c(0, max(bf$numb_allevents)))
abline(v=bf$year[bf$numbelec_all_contest > 0])
box()
axis(1)
axis(2, at=c(0, max(bf$numb_allevents)))
mtext("Burkina Faso", side=3)

plot(nigeria$year, nigeria$numb_allevents, type="l", xlab="Year", 
     ylab="Violent Events", axes=FALSE, ylim=c(0, max(nigeria$numb_allevents)))
abline(v=nigeria$year[nigeria$numbelec_all_contest > 0])
box()
axis(1)
axis(2, at=c(0, max(nigeria$numb_allevents)))
mtext("Nigeria", side=3)
# End Figure 1

# Figure 2 for paper
par(mfrow=c(1,2), mar=c(3,3,1.2,2), mgp=c(2, .7, 0))
plot(avgcont$dist2elec, avgcont$numb_allevents, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .6), cex.axis=.8, 
     axes=FALSE)
mtext("Incumbency at Stake", side=3)
box()
axis(1, at=c(-30, 0, 30))
axis(2, at=c(0, .3, .6))
plot(avgnc$dist2elec, avgnc$numb_allevents, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .6), cex.axis=.8,
     axes=FALSE)
mtext("Incumbency not at Stake", side=3)
box()
axis(1, at=c(-30, 0, 30))
axis(2, at=c(0, .3, .6))
# End Figure 2

# Figure 7 for paper
par(mfrow=c(1,2), mar=c(3,3,1.2,2), mgp=c(2, .7, 0))
plot(avgpro$dist2elec[abs(avgpro$dist2elec) < 31], 
     avgpro$numb_progovt[abs(avgpro$dist2elec) < 31], type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .1), cex.axis=.8, 
     axes=FALSE)
mtext("Pro-Government", side=3)
box()
axis(1, at=c(-30, 0, 30))
axis(2, at=c(0, .05, .1))

plot(avganti$dist2elec, avganti$numb_antigovt2, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .4), cex.axis=.8,
     axes=FALSE)
mtext("Anti-Government", side=3)
box()
axis(1, at=c(-30, 0, 30))
axis(2, at=c(0, .2, .4))
# End Figure 7

# Figure 8 for appendix
par(mfrow=c(2,2), mar=c(3,3,1.2,2), mgp=c(2, .7, 0))
plot(avgcont180$dist2elec, avgcont180$numb_allevents, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .6), cex.axis=.8,
     axes=FALSE)
mtext("Incumbency at Stake", side=3)
box()
axis(1, at=c(-180, 0, 180))
axis(2, at=c(0, .3, .6))
plot(avgnc180$dist2elec, avgnc180$numb_allevents, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .6), cex.axis=.8,
     axes=FALSE)
mtext("Incumbency not at Stake", side=3)
box()
axis(1, at=c(-180, 0, 180))
axis(2, at=c(0, .3, .6))
plot(avgcont365$dist2elec, avgcont365$numb_allevents, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .6), cex.axis=.8,
     axes=FALSE)
mtext("Incumbency at Stake", side=3)
box()
axis(1, at=c(-365, 0, 365))
axis(2, at=c(0, .3, .6))
plot(avgnc365$dist2elec, avgnc365$numb_allevents, type="l",
     xlab="Days to Election", ylab="Avg Violent Events", ylim=c(0, .6), cex.axis=.8,
     axes=FALSE)
mtext("Incumbency not at Stake", side=3)
box()
axis(1, at=c(-365, 0, 365))
axis(2, at=c(0, .3, .6))
# End Figure 8





#############################################
# THEORETICAL GRAPHS (Other than figure 5 - see "simulated_comparative_statics_and_fig5.R")
############################################

# function to make a k vector with election every per periods
makek <- function(len=100, per=10, kn=.3, ke=.9){
  k <- rep(kn, len)
  n.elec <- floor(len/per)
  k[seq(per, per*n.elec, per)] <- ke
  return(k)
}

# Alternate version where violence "ramps up" in period before
makekramp <- function(len=100, per=10, kn=.3, ke=.9, kpre=.6){
  k <- rep(kn, len)
  n.elec <- floor(len/per)
  k[seq(per, per*n.elec, per)] <- ke
  k[seq(per, per*n.elec, per)-1] <- kpre
  return(k)
}

# Probability of taking over, standard parameterization sets vo=1
pwin <- function(vt, kt, vo=1) kt*(vt/(vo + vt))

# Function to calculate violence level given contituation values
# Using quadratic cost function
optviolgen <- function(uinext=3.4, uonext=1, delta=.9, kt=.5, vo=1){
  if (uinext - uonext < 0) return(0)
  a <- delta*kt*(uinext - uonext)
  t1 <- (2^(2/3) *vo^2)/(27 * a * vo + 4 * vo^3 + 3 * sqrt(3) * sqrt(27 * a^2 * vo^2 + 8 * a * vo^4))^(1/3)
  t2 <- (27 * a * vo + 4 * vo^3 + 3 * sqrt(3) * sqrt(27 *a^2 *vo^2 + 8 *a *vo^4))^(1/3)/2^(2/3)
  return(1/3 * (-2*vo + t1 + t2))
}

optviolgen()

# Function to compute equilibrium violence levels
# and net present values
# UI and UO are continuation values for end of game (\pi^*_J(T+1) in final notation)

backsolve <- function(UI=NA, UO=0, delta=.9, k = rep(.3, 10), psi=1, vo=1){
  # If UI not inputted, this computes stationary value
  if (is.na(UI)){
    diff <- function(D) {
      D - ((psi + optviolgen(uinext=D, uonext=0, delta=delta, kt=k[1], vo=vo)^2))/
            (1 - delta*(1 - 2*pwin(vt=optviolgen(uinext=D, uonext=0, delta=delta, kt=k[1], vo=vo), kt=k[1], vo=vo)))
    }
      UI <- uniroot(diff, interval=c(0, 1e+07))$root
    }
  periods <- length(k)
  v.vec <- rep(NA, periods)
  ui.vec <- rep(NA, periods)
  uo.vec <- rep(NA, periods)
  #First doing last period
  v.vec[periods] <- optviolgen(uinext=UI, uonext=UO, delta=delta, kt=k[periods], vo=vo)
  uo.vec[periods] <- -v.vec[periods]^2 + delta*(pwin(v.vec[periods], kt=k[periods], vo=vo)*UI  + 
    (1 - pwin(v.vec[periods], kt=k[periods], vo=vo)) * UO) 
  ui.vec[periods] <- psi + delta*(pwin(v.vec[periods], kt=k[periods], vo=vo)*UO  + 
    (1 - pwin(v.vec[periods], kt=k[periods], vo=vo)) * UI)
  # Now looping back from T-1 to 1
  for (i in (periods-1):1){
    v.vec[i] <- optviolgen(uinext=ui.vec[i+1], uonext=uo.vec[i+1], delta=delta, kt=k[i], vo=vo)
    uo.vec[i] <- - v.vec[i]^2 + delta*(pwin(v.vec[i], kt=k[i], vo=vo)*ui.vec[i+1]  + 
      (1 - pwin(v.vec[i], kt=k[i], vo=vo)) * uo.vec[i+1] )
    ui.vec[i] <- psi + delta*(pwin(v.vec[i], kt=k[i], vo=vo)*uo.vec[i+1]  + 
      (1 - pwin(v.vec[i], kt=k[i], vo=vo)) * ui.vec[i+1])
  }
  return(list(v.vec=v.vec, ui.vec=ui.vec, uo.vec=uo.vec, p.vec = pwin(v.vec, k, vo=vo)))
}

backsolve()


#  Figure 3 for paper
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(2, .7, 0))
k2e <- makek(len=20, per=6, ke=1)
plot(backsolve(k=rep(.3, 20), delta=.95)$v.vec, type="l", ylim=c(0, 1),
     xlab = "Time Period", ylab = "Violence Level", col=rgb(.5, .5, .5))
lines(backsolve(k=k2e, delta=.95)$v.vec, type="l")
abline(v=c(6, 12, 18))
abline(h=mean(backsolve(k=k2e, delta=.95)$v.vec))
text(3, .46, "No Elections", col=rgb(.5, .5, .5))
text(3, .23, "Elections")
# End Figure 3


# Figure 4 for paper
par(mfrow=c(1,2), mar=c(3,3,1,2), mgp=c(2, .7, 0))
plot(backsolve(k=rep(.3, 20))$v.vec, type="l", ylim=c(0, 1),
     xlab = "Time Period", ylab = "Violence Level", col="white")
avgviol <- rep(NA, 20)
for (ke in seq(.4, 1, length.out=4)){
  k2e <- makek(len=20, per=6, ke=ke)
  lines(backsolve(k=k2e)$v.vec, type="l", col=rgb(0, 0, 0, alpha=ke))
  avgviol[ke == seq(.3, 1, length.out=20)] <- mean(backsolve(k=k2e)$v.vec)
}

avgviol <- rep(NA, 20)
dviol <- rep(NA, 20)
for (ke in seq(.3, 1, length.out=20)){
  k2e <- makek(len=20, per=6, ke=ke)
  avgviol[ke == seq(.3, 1, length.out=20)] <- mean(backsolve(k=k2e)$v.vec)
  dviol[ke == seq(.3, 1, length.out=20)] <- mean(backsolve(k=k2e)$v.vec[c(6,12,18)]) -
    mean(backsolve(k=k2e)$v.vec[-c(6,12,18)]) 
}
abline(v=c(6, 12, 18))
plot(avgviol, type="l", ylim=c(.3, .4),
     xlab="Election Period Effectiveness",
     ylab = "Average Violence Level",
     axes=FALSE, lwd=2)
axis(1, at=c(1, 9, 17), label=c(expression(k[n]), expression(2*k[n]), expression(3*k[n])))
axis(2, at=c(.3, .4))
box()
abline(h=avgviol[1], col=rgb(.7, .7, .7))
# text(7.5, .343, "No Elections", col=rgb(.7, .7, .7))
text(14, .33, "Avg Violence \n with Elections")
text(9.6, .39, "Increase in Violence \n During Elections")
lines(.3 + dviol/4, lwd=2, lty=3)
mtext("Increase in Violence", side=4, line=2)
axis(4, at=c(.3, .4), labels=c(.1, .4))
# End Figure 4



# Extension with nonviolent actions;

# Optimal violence level (some ugly mathematica code to produce available pon request)
optviol2 <- function(uinext=3.4, uonext=1, delta=.9, kt=.5, beta=1){ 
  du <- uinext - uonext
  return(-2/3/(beta +
    1) + (1/108*(27*beta*delta*du*kt + 27*delta*du*kt + 4)/(beta^3 +
    3*beta^2 + 3*beta + 1) + 1/36*sqrt((27*(beta + 1)*delta*du*kt +
    8)*delta*du*kt/(beta + 1))*sqrt(3)/(beta + 1)^2)^(1/3) +
    1/9/((1/108*(27*beta*delta*du*kt + 27*delta*du*kt + 4)/(beta^3 +
    3*beta^2 + 3*beta + 1) + 1/36*sqrt((27*(beta + 1)*delta*du*kt +
    8)*delta*du*kt/(beta + 1))*sqrt(3)/(beta + 1)^2)^(1/3)*(beta^2 + 2*beta
                                                            + 1)))
}

pwin2 <- function(vt, kt, beta) kt*(vt*(1 + beta)/(1 + vt))


# 
backsolve2 <- function(UI=5.5, UO=1, delta=.9, beta=1, 
                       len=100, per=10, kn=.3, ke=.9){
#   browser()
  k <- rep(kn, len)
  n.elec <- floor(len/per)
  elecs <- seq(per, per*n.elec, per)
  k[elecs] <- ke
  beta <- rep(beta, len)
  beta[-elecs] <- 0
  v.vec <- rep(NA, len)
  ui.vec <- rep(NA, len)
  uo.vec <- rep(NA, len)
  v.vec[len] <- optviol2(uinext=UI, uonext=UO, 
                             delta=delta, kt=k[len], beta=beta[len])
  uo.vec[len] <- -v.vec[len]^2 - beta[len]*(v.vec[len])^2 + 
    delta*(pwin2(v.vec[len], kt=k[len], beta=beta[len])*UI  + 
    (1 - pwin2(v.vec[len], kt=k[len], beta=beta[len])) * UO) 
  ui.vec[len] <- 1 + delta*(pwin2(v.vec[len], kt=k[len], 
                                      beta=beta[len])*UO  + 
    (1 - pwin2(v.vec[len], kt=k[len], beta=beta[len])) * UI)
  for (i in (len-1):1){
    v.vec[i] <- ifelse(uo.vec[i+1] > ui.vec[i+1], 0, optviol2(uinext=ui.vec[i+1], uonext=uo.vec[i+1], 
                         delta=delta, kt=k[i], beta=beta[i]))
    uo.vec[i] <- -v.vec[i]^2 - beta[len]*(v.vec[i])^2 + 
      delta*(pwin2(v.vec[i], kt=k[i], beta=beta[i])*ui.vec[i+1]  + 
      (1 - pwin2(v.vec[i], kt=k[i], beta=beta[i])) * uo.vec[i+1] )
    ui.vec[i] <- 1 + delta*(pwin(v.vec[i], kt=k[i])*uo.vec[i+1]  + 
      (1 - pwin2(v.vec[i], kt=k[i], beta=beta[i])) * ui.vec[i+1])
  }
  return(list(v.vec=v.vec, ui.vec=ui.vec, uo.vec=uo.vec))
}


#Figure 6 for paper
par(mfrow=c(1,2), mar=c(3,3,1,2), mgp=c(2, .7, 0))

plot(backsolve2(beta=.01, ke=.3, len=25, delta=.7, UI = 3.8)$v.vec, type="l", ylim=c(0,.5),
            xlab = "Time Period", ylab = "Violence Level", col=rgb(.7, .7, .7))
lines(backsolve2(beta=0, ke=.9, len=25, delta=.7, UI = 3.8)$v.vec, lty=3)
# lines(backsolve2(beta=1, ke=.9, len=25, delta=.7, UI = 3.8)$v.vec, col=rgb(.3, .3, .3))
lines(backsolve2(beta=5, ke=.9, len=25, delta=.7, UI = 3.8)$v.vec)
text(5.5, .4, "Expensive \n Nonviol \n Tech", cex=.8)
text(4.5, .095, "Cheap \n Nonviol \n Tech", cex=.8)

avg.viol <- rep(NA, 20)

for (i in seq(0, 5, length.out=20)){
  avg.viol[i == seq(0, 5, length.out=20)] <- mean(backsolve2(beta=i, ke=.8, len=25, delta=.7)$v.vec)
  
}

plot(seq(0, 5, length.out=20), avg.viol, type="l",
     xlab = "Relative Cost of Violence", ylab = "Average Level of Violence", 
     ylim=c(0.15, 0.25), axes=FALSE, lwd=2)
axis(1)
axis(2, at=c(.15, 2, .25))
box()
abline(h=mean(backsolve2(beta=0, ke=.3, len=25, delta=.7)$v.vec), col=rgb(.5, .5, .5),
       lwd=2)
text(2.5, .217, "No Elections", col=rgb(.5, .5, .5))
text(2.5, .2, "Elections")
# End Figure 6


