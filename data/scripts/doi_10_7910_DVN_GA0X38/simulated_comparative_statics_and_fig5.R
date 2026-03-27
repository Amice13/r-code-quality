
rm(list=ls())

set.seed(43210)
# function to make a k vector for 100 periods, with election is every per periods
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

# vo > o
pwin <- function(vt, kt, v0=1) kt*(vt/(v0 + vt))

# Alpha graeter than 1
cost.gen <- function(vt, alpha) vt^alpha



# Function for optimal violence level
optviolgen <- function(uinext=3.4, uonext=1, delta=.9, kt=.5, v0=1,
                       alpha=2, maxviol=1e5){
  if (uinext - uonext < 0) return(0)
  foc <- function(vt) alpha*vt^{alpha - 1} - 
    delta*(uinext - uonext)*kt*v0/(vt + v0)^2
  optviol <- uniroot(foc, c(0, maxviol))$root
  return(optviol)
}

# plotobj <- function(uinext=3.4, uonext=1, delta=.9, kt=.5, v0=1, alpha=2){
#   curve(delta*(pwin(vt=x, kt=kt, v0=v0)*uinext + (1 - pwin(vt=x, kt=kt, v0=v0))*uonext)
#         - x^alpha)
#   abline(v=optviolgen(uinext=uinext, uonext=uonext, delta=delta, kt=kt, v0=v0, alpha=alpha))
# }
# plotobj(uonext=1.1)
# 
# optviolgen()
# optviolgen(alpha=3, uinext=1.01)
# 
# optviolgen(alpha=2)

# Getting the value of UI such that violence is stationary w/o elections
stationaryUI <- function(UO=0, delta=.9, k1=.3, v0=1, alpha=2, psi=1){
  diff <- function(D) {
    D - ((psi + optviolgen(uinext=D, uonext=UO, 
                           delta=delta, kt=1, 
                           v0=v0, alpha=alpha)^2))/
      (1 - delta*(1 - 2*pwin(vt=optviolgen(uinext=D, uonext=UO, 
                                           delta=delta, kt=1,
                                           alpha=alpha, v0=v0), kt=k1, v0=v0)))
  }
  UI <- uniroot(diff, interval=c(0, 1e+15))$root
  return(UI)
}

# stationaryUI(alpha=1.001)

# Function that solves model
backsolve <- function(UI=NA, UO=0, delta=.9, k = rep(.3, 10), 
                      psi=1, v0=1, alpha=2){
  if (is.na(UI)){
    diff <- function(D) {
      D - ((psi + optviolgen(uinext=D, uonext=0, 
                             delta=delta, kt=k[1], 
                             v0=v0, alpha=alpha)^2))/
            (1 - delta*(1 - 2*pwin(vt=optviolgen(uinext=D, uonext=0, 
                                                 delta=delta, kt=k[1],
                                                 alpha=alpha, v0=v0), kt=k[1], v0=v0)))
    }
      UI <- uniroot(diff, interval=c(0, 1e+08))$root
    }
  periods <- length(k)
  v.vec <- rep(NA, periods)
  ui.vec <- rep(NA, periods)
  uo.vec <- rep(NA, periods)
  v.vec[periods] <- optviolgen(uinext=UI, uonext=UO, delta=delta, 
                               kt=k[periods], v0=v0, alpha=alpha)
  uo.vec[periods] <- -v.vec[periods]^2 + 
    delta*(pwin(v.vec[periods], kt=k[periods], v0=v0)*UI  + 
    (1 - pwin(v.vec[periods], kt=k[periods], v0=v0)) * UO) 
  ui.vec[periods] <- psi + delta*(pwin(v.vec[periods], 
                                       kt=k[periods], v0=v0)*UO  + 
    (1 - pwin(v.vec[periods], kt=k[periods], v0=v0)) * UI)
  for (i in (periods-1):1){
    v.vec[i] <- optviolgen(uinext=ui.vec[i+1], uonext=uo.vec[i+1], 
                           delta=delta, kt=k[i], v0=v0,
                           alpha=alpha)
    uo.vec[i] <- - v.vec[i]^alpha + 
      delta*(pwin(v.vec[i], kt=k[i], v0=v0)*ui.vec[i+1]  + 
      (1 - pwin(v.vec[i], kt=k[i], v0=v0)) * uo.vec[i+1] )
    ui.vec[i] <- psi + delta*(pwin(v.vec[i], kt=k[i], v0=v0)*uo.vec[i+1]  + 
      (1 - pwin(v.vec[i], kt=k[i], v0=v0)) * ui.vec[i+1])
  }
  return(list(v.vec=v.vec, ui.vec=ui.vec, uo.vec=uo.vec, 
              p.vec = pwin(v.vec, k, v0=v0)))
}


backsolve(k=rep(.3, 100))

# Difference between violence levels with and without elections
# Returns percent change if pchange=TRUE
comparenoelex <- Vectorize(function(len=100, kn=.3, ke=.9, UI=NA, UO=0, 
                          delta=.9, psi=1, v0=1, per=4,
                          alpha=2, pchange=FALSE){
  nseq <- backsolve(k=rep(kn, len), UI=UI, UO=UO,
                    delta=delta, psi=psi, v0=v0,
                    alpha=alpha)$v.vec
  eseq <- backsolve(k=makek(len, per=per, kn=kn, ke=ke), UI=UI, UO=UO,
                    delta=delta, psi=psi, v0=v0,
                    alpha=alpha)$v.vec
  diff <- mean(eseq) - mean(nseq)
  if (pchange) diff <-(mean(eseq) - mean(nseq))/mean(nseq)
  return(diff)
})

# Code for Figure 5
par(mfrow=c(1,3), mar=c(3,3,1,1), mgp=c(2, .7, 0))
curve(comparenoelex(delta=x), .01, 1,
      xlab=expression(paste("Discount Rate, (", delta, ")")),
      ylab="Change in Violence")
abline(h=0, lty=3)
abline(v=uniroot(function(x) comparenoelex(delta=x), c(.5, 1))$root, lty=3)

curve(comparenoelex(psi=x), .1, 10,
      xlab=expression(paste("Value of Office (", psi, ")")),
      ylab="Change in Violence")
abline(h=0, lty=3)
abline(v=uniroot(function(x) comparenoelex(psi=x), c(.001, 1))$root, lty=3)

plot((1:13), comparenoelex(per=(1:13)),
     xlab=expression(paste("Frequency of Elections")),
     ylab="Change in Violence", type="l")
abline(h=0, lty=3)
abline(v=3, lty=3)
# End figure 5

###################################################
#################Simulated Comparative Statics
#################

# Quartile functions 
# Need to make sure to drop NAs when looking at p change and can get zeroes
q25 <- function(x) quantile(x, .25, na.rm=TRUE)
q5 <- function(x) quantile(x, .5, na.rm=TRUE)
q75 <- function(x) quantile(x, .75, na.rm=TRUE)


### For each of the parameters of the model, writing a function to simulate
## the effect of chaning that parameter for random draws of the others
# Drawing simulations for the effect of changing ke
randcompare.ke <- function(ke.low=0, ke.high=1, slen=20, n.sims=10,
                           pchange=TRUE){
  ke.range <- seq(ke.low, ke.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .5)
    psi <- .5 + rexp(1, 2)
    delta <- rbeta(1, 9, 1)
    v0 <- rexp(1)
    per <- ceiling(runif(1, 1, 10))
    alpha <- 1.1 + rexp(1, 1.1)
    out.mat[,i] <- try(comparenoelex(len=100, kn, ke=kn + ke.range*(1 - kn), 
                                     delta=delta,
                                     psi=psi, v0=v0, per=per,
                                     alpha=alpha, pchange=pchange))
#     if (sum(is.na(out.mat[,i])) > 0) print(c(kn, psi, delta, v0, per, alpha))
   }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(ke.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Increased Effectiveness in Elections")))
  for (i in 2:n.sims) lines(ke.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(ke.range, apply(out.mat, 1, q5), lwd=3)
  lines(ke.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(ke.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}

# backsolve(k=rep(.283, 100),  
#               delta=.7,
#               psi=.1, v0=2, 
#               alpha=1.2)

par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(ke=x), .3, 1,
      xlab="Effectivness During Elections",
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.ke(n.sims=100, pchange=FALSE)


randcompare.kn <- function(kn.low=.1, kn.high=1, slen=20, n.sims=10,
                           pchange=TRUE){
  kn.range <- seq(kn.low, kn.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    ke <- runif(1, .5, 1)
    psi <- .5 + rexp(1, 2)
    out.mat[,i] <- try(comparenoelex(len=100, kn=ke*kn.range, ke=ke, 
                                     delta=rbeta(1, 9, 1),
                                     psi=psi, v0=rexp(1), per=ceiling(runif(1, 1, 10)),
                                     alpha=1.1 + rexp(1, 1.1), pchange=pchange))
  }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(kn.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Non-Electoral Effectiveness")))
  for (i in 2:n.sims) lines(kn.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(kn.range, apply(out.mat, 1, q5), lwd=3)
  lines(kn.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(kn.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
randcompare.kn()

par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(kn=x), .1, .9,
      xlab="Non-Electoral Effectiveness",
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.kn(n.sims=100, pchange=FALSE)


randcompare.alpha <- function(alpha.low=1.2, alpha.high=5, slen=20, n.sims=10,
                           pchange=TRUE){
  alpha.range <- seq(alpha.low, alpha.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .5)
    ke <- runif(1, .6, 1)
    psi <- .5 + rexp(1, 2)
    out.mat[,i] <- try(comparenoelex(len=100, kn=kn, ke=ke, 
                                     delta=rbeta(1, 9, 1),
                                     psi=psi, v0=rexp(1), per=ceiling(runif(1, 1, 10)),
                                     alpha=alpha.range, pchange=pchange))
  }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(alpha.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Concavity of Cost (", alpha, ")")))
  for (i in 2:n.sims) lines(alpha.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(alpha.range, apply(out.mat, 1, q5), lwd=3)
  lines(alpha.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(alpha.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
# randcompare.alpha(pchange=TRUE)

par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(alpha=x), 1.1, 5,
      xlab=expression(paste("Concavity of Cost (", alpha, ")")),
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.alpha(n.sims=100, pchange=FALSE)



randcompare.delta <- function(delta.low=.1, delta.high=1, slen=20, n.sims=10,
                              pchange=TRUE, yq=c(.1, .9)){
  delta.range <- seq(delta.low, delta.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .6)
    ke <- runif(1, .6, 1)
    psi <- .5 + rexp(1, 2)
    out.mat[,i] <- try(comparenoelex(len=100, kn=kn, ke=ke, 
                                     delta=delta.range,
                                     psi=psi, v0=rexp(1), per=ceiling(runif(1, 1, 10)),
                                     alpha=1.1 + rexp(1, 1.1), pchange=pchange))
  }
  ymin <- quantile(out.mat, yq[1], na.rm=TRUE)
  ymax <- quantile(out.mat, yq[2], na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(delta.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Discount Rate (", delta, ")")))
  for (i in 2:n.sims) lines(delta.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(delta.range, apply(out.mat, 1, q5), lwd=3)
  lines(delta.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(delta.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
# randcompare.delta(pchange=TRUE)
# randcompare.delta(pchange=FALSE)


par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(delta=x), .01, 1,
      xlab=expression(paste("Discount Rate, (", delta, ")")),
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.delta(n.sims=100,  pchange=FALSE, yq=c(.05, .95))



randcompare.psi <- function(psi.low=.1, psi.high=10, slen=20, n.sims=10,
                               pchange=TRUE){
  psi.range <- seq(psi.low, psi.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .6)
    ke <- runif(1, .6, 1)
    out.mat[,i] <- try(comparenoelex(len=100, kn=kn, ke=ke, 
                                     delta=rbeta(1, 9, 1),
                                     psi=psi.range, v0=rexp(1), per=ceiling(runif(1, 1, 10)),
                                     alpha=1.1 + rexp(1,1.1), pchange=pchange))
  }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(psi.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Value of Office (", psi, ")")))
  for (i in 2:n.sims) lines(psi.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(psi.range, apply(out.mat, 1, q5), lwd=3)
  lines(psi.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(psi.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
randcompare.psi()



par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(psi=x), .1, 10,
      xlab=expression(paste("Value of Office (", psi, ")")),
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.psi(n.sims=100, pchange=FALSE)







randcompare.v0 <- function(v0.low=.01, v0.high=1, slen=20, n.sims=10,
                            pchange=TRUE){
  v0.range <- seq(v0.low, v0.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .6)
    ke <- runif(1, .6, 1)
    out.mat[,i] <- try(comparenoelex(len=100, kn=kn, ke=ke, 
                                     delta=rbeta(1, 9, 1),
                                     psi=.5 + rexp(1, 2), v0=v0.range, per=ceiling(runif(1, 1, 10)),
                                     alpha=1.1 + rexp(1,1.1), pchange=pchange))
  }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(v0.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Shape of p (", v[0], ")")))
  for (i in 2:n.sims) lines(v0.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(v0.range, apply(out.mat, 1, q5), lwd=3)
  lines(v0.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(v0.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
randcompare.v0(pchange=FALSE)



par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(v0=x), .1, 10,
      xlab=expression(paste("Shape of p (", v[0], ")")),
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.v0(n.sims=100, pchange=FALSE)



randcompare.per <- function(per.low=1, per.high=10, n.sims=10,
                           pchange=TRUE){
  per.range <- seq(per.low, per.high)
  slen <- length(per.range)
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .6)
    ke <- runif(1, .6, 1)
    out.mat[,i] <- try(comparenoelex(len=100, kn=kn, ke=ke, 
                                     delta=rbeta(1, 9, 1),
                                     psi=.5 + rexp(1, 2), v0=rexp(1), per=per.range,
                                     alpha=1.1 + rexp(1,1.1), pchange=pchange))
    # if (norm) out.mat[,i] <- out.mat[,i]/psi
  }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(per.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Frequency of Elections")))
  for (i in 2:n.sims) lines(per.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(per.range, apply(out.mat, 1, q5), lwd=3)
  lines(per.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(per.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
randcompare.per()



par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
plot((1:10), comparenoelex(per=(1:10)),
      xlab=expression(paste("Frequency of Elections")),
      ylab="Change in Violence", type="l")
abline(h=0, lty=3)
randcompare.per(n.sims=100, pchange=FALSE)




randcompare.UI <- function(UI.low=0, UI.high=50, slen=20, n.sims=10,
                           pchange=TRUE){
  UI.range <- seq(UI.low, UI.high, length.out=slen)  
  out.mat <- matrix(NA, nrow=slen, ncol=n.sims)
  for (i in 1:n.sims){
    kn <- runif(1, .1, .6)
    ke <- runif(1, .6, 1)
    out.mat[,i] <- try(comparenoelex(len=100, kn=kn, ke=ke, UI=UI.range,
                                     delta=rbeta(1, 9, 1),
                                     psi=rexp(1), v0=rexp(1), per=ceiling(runif(1, 1, 10)),
                                     alpha=1.1 + rexp(1,1.1), pchange=pchange))
    # if (norm) out.mat[,i] <- out.mat[,i]/psi
  }
  ymin <- quantile(out.mat, .05, na.rm=TRUE)
  ymax <- quantile(out.mat, .95, na.rm=TRUE)
  ylab <- ifelse(pchange, "% Change in Violence", "Change in Violence")
  plot(UI.range, out.mat[,1], ylim=c(ymin, ymax), 
       type="l", col=rgb(0,0,0, alpha=.1),
       ylab=ylab,
       xlab=expression(paste("Continuation Value of Incumbency")))
  for (i in 2:n.sims) lines(UI.range, out.mat[,i], col=rgb(0,0,0, alpha=.1))
  lines(UI.range, apply(out.mat, 1, q5), lwd=3)
  lines(UI.range, apply(out.mat, 1, q25), lty=2, lwd=3)
  lines(UI.range, apply(out.mat, 1, q75), lty=2, lwd=3)
  abline(h=0, lty=3)
}
randcompare.UI()



par(mfrow=c(1,2), mgp=c(2, .7, 0), mar=c(3,3,1,1))
curve(comparenoelex(UI=x), 0, 10,
      xlab=expression(paste("Continuation Value of Incumbency")),
      ylab="Change in Violence")
abline(h=0, lty=3)
randcompare.UI(n.sims=100, pchange=FALSE)
# 
# 
# # Exploring psi result
# 
# curve(comparenoelex(psi=x, ke=.9), .1, 5)
# curve(comparenoelex(psi=x, ke=.8), add=TRUE)
# curve(comparenoelex(psi=x, ke=.7), add=TRUE)
# curve(comparenoelex(psi=x, ke=.6), add=TRUE)
# abline(h=0, lty=3)
# 
# curve(comparenoelex(psi=x, kn=.2), .1, 5)
# curve(comparenoelex(psi=x, kn=.3), add=TRUE)
# curve(comparenoelex(psi=x, kn=.4), add=TRUE)
# curve(comparenoelex(psi=x, kn=.5), add=TRUE)
# curve(comparenoelex(psi=x, kn=.6), add=TRUE)
# abline(h=0, lty=3)





