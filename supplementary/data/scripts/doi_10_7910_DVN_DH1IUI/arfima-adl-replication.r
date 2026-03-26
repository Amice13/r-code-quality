# Replication Data for 
# "Fractionally integrated data and the autodistributed lag model: results from a simulation Study"
# by Justin Esarey (justin@justinesarey.com)
# Date: 10/21/2015


# immediate impact (no cumulative effect)
# true effect of x = 0
rm( list=ls() )
library(arfima)
library(fracdiff)
library(AER)
library(mvtnorm)

set.seed(123456)

reps=1000

N <- 100
d.y <- c(0, 0.1, 0.2, 0.3, 0.4, 0.45)
d.x <- 0.3
phi.y <- 0.5
phi.x <- 0.5
x.coef <- 0

srm <- matrix(data=NA, nrow=reps, ncol=length(d.y))
srm.p <- matrix(data=NA, nrow=reps, ncol=length(d.y))
lrm <- matrix(data=NA, nrow=reps, ncol=length(d.y))
lrm.p <- matrix(data=NA, nrow=reps, ncol=length(d.y))

for(j in 1:length(d.y)){
  
  cat("\n", "\n", "d.y = ", d.y[j], "\n")
  pb <- txtProgressBar(min=0, max=reps, initial=0, style=3)
  for(i in 1:reps){
    
    setTxtProgressBar(pb, value=i)
    
    # independent variable and its lag and difference
    x <- arfima.sim( n = N, model=list(phi = phi.x, dfrac = d.x, dint = 0) )
    x.l <- c( NA, x[1:(N-1)])
    dif.x <- x - x.l
    
    # dependent variable and its lag and difference
    y <- arfima.sim(n = N, model=list(phi = phi.y, dfrac = d.y[j], dint = 0)) + x * x.coef
    y.l <- c( NA, y[1:(N-1)] )
    dif.y <- y - y.l
    
    # standard ADL-type model
    adl <- lm(y ~ y.l + dif.x + x.l)
    bewley <- ivreg(y ~ dif.y + x + dif.x | y.l + x + dif.x)
    
    
    # value of SR impact
    srm[i,j] <- summary(adl)$coefficients[3,1]
    # check significance of SR impact
    srm.p[i,j] <- summary(adl)$coefficients[3,4]
    # value of LRM
    lrm[i,j] <- summary(bewley)$coefficients[3,1]
    # check significance of LRM
    lrm.p[i,j] <- summary(bewley)$coefficients[3,4]
    
  }
  close(pb)
}



# short run impacts
par(mfrow=c(1,1), mar=c(5,4,4,2))
boxplot(srm[,1], srm[,2], srm[,3], srm[,4], srm[,5], srm[,6], main="immediate impacts of x on y", ylim=c(-0.5, 0.5), ylab="estimate of immediate dy/dx", names=d.y, xlab="d value for y")
text(x=3.5, y=-0.45, labels="percentage of statistically significant results")
for(j in 1:length(d.y)){
  text(x=j, y=-0.5, labels=paste(100*(sum(srm.p[,j] < 0.05)/reps ), "%" ))
}
# This is Figure 1, Panel A
dev.copy2pdf(file="short-run-null.pdf")


# long run impacts
par(mfrow=c(1,1), mar=c(5,4,4,2))
boxplot(lrm[,1], lrm[,2], lrm[,3], lrm[,4], lrm[,5], lrm[,6], main="long-run impacts of x on y", ylim=c(-3,3), ylab="estimate of long-run multiplier for dy/dx", names=d.y, xlab="d value for y")
text(x=3.5, y=-2.75, labels="percentage of statistically significant results")
for(j in 1:length(d.y)){
  text(x=j, y=-3, labels=paste(100*(sum(lrm.p[,j] < 0.05)/reps ), "%" ))
}
# This is Figure 1, Panel B
dev.copy2pdf(file="long-run-null.pdf")





# immediate impact (no cumulative effect)
# true effect of x = 0.5
rm( list=ls() )
library(arfima)
library(fracdiff)
library(AER)
library(mvtnorm)

set.seed(123456)

reps=1000

N <- 100
d.y <- c(0, 0.1, 0.2, 0.3, 0.4, 0.45)
d.x <- 0.3
phi.y <- 0.5
phi.x <- 0.5
x.coef <- 0.5

srm <- matrix(data=NA, nrow=reps, ncol=length(d.y))
srm.p <- matrix(data=NA, nrow=reps, ncol=length(d.y))
lrm <- matrix(data=NA, nrow=reps, ncol=length(d.y))
lrm.p <- matrix(data=NA, nrow=reps, ncol=length(d.y))
sr.lr.sig <- matrix(data=NA, nrow=reps, ncol=length(d.y))

for(j in 1:length(d.y)){
  
  cat("\n", "\n", "d.y = ", d.y[j], "\n")
  pb <- txtProgressBar(min=0, max=reps, initial=0, style=3)
  for(i in 1:reps){
    
    setTxtProgressBar(pb, value=i)
    
    # independent variable and its lag and difference
    x <- arfima.sim( n = N, model=list(phi = phi.x, dfrac = d.x, dint = 0) )
    x.l <- c( NA, x[1:(N-1)])
    dif.x <- x - x.l
    
    # dependent variable and its lag and difference
    y <- arfima.sim(n = N, model=list(phi = phi.y, dfrac = d.y[j], dint = 0)) + x * x.coef
    y.l <- c( NA, y[1:(N-1)] )
    dif.y <- y - y.l
  
    # standard ADL-type model
    adl <- lm(y ~ y.l + dif.x + x.l)
    bewley <- ivreg(y ~ dif.y + x + dif.x | y.l + x + dif.x)
    
    
    # value of SR impact
    srm[i,j] <- summary(adl)$coefficients[3,1]
    # check significance of SR impact
    srm.p[i,j] <- summary(adl)$coefficients[3,4]
    # value of LRM
    lrm[i,j] <- summary(bewley)$coefficients[3,1]
    # check significance of LRM
    lrm.p[i,j] <- summary(bewley)$coefficients[3,4]
    
    # are SR and LRM different?
    all.draw <- rmvnorm(1000, mean=summary(adl)$coefficients[,1], sigma = vcov(adl))
    lr.draw <- all.draw[,4] / (1 - all.draw[,2])
    sr.draw <- all.draw[,3]
    sr.lr.dif <- sr.draw - lr.draw
    sr.lr.sig[i,j] <- sign(quantile(sr.lr.dif, probs=c(0.025))) == sign(quantile(sr.lr.dif, probs=c(0.975)))
    
  }
  close(pb)
}


# short run impacts
par(mfrow=c(1,1), mar=c(5,4,4,2))
boxplot(srm[,1], srm[,2], srm[,3], srm[,4], srm[,5], srm[,6], main="immediate impacts of x on y", ylim=c(0, 1), ylab="estimates of immediate dy/dx", names=d.y, xlab="d value for y")
text(x=3.5, y=0.05, labels="percentage of statistically significant results")
for(j in 1:length(d.y)){
  text(x=j, y=-0, labels=paste(100*(sum(srm.p[,j] < 0.05)/reps ), "%" ))
}
# This is Figure 2, Panel A
dev.copy2pdf(file="short-run-immediate.pdf")


# long run impacts
par(mfrow=c(1,1), mar=c(5,4,4,2))
boxplot(lrm[,1], lrm[,2], lrm[,3], lrm[,4], lrm[,5], lrm[,6], main="long-run impacts of x on y", ylim=c(-4,4), ylab="estimates of long-run multiplier for dy/dx", names=d.y, xlab="d value for y")
text(x=3.5, y=-2.65, labels="percentage of statistically significant results")
text(x=3.5, y=-3.65, labels="percentage stat. distinguishable from immediate impact")
for(j in 1:length(d.y)){
  text(x=j, y=-3, labels=paste(100*(sum(lrm.p[,j] < 0.05)/reps ), "%" ))
  text(x=j, y=-4, labels=paste(100*(sum(sr.lr.sig[,j])/reps ), "%" ))
}
# This is Figure 2, Panel B
dev.copy2pdf(file="long-run-immediate.pdf")
















# with long run change, N=100
rm( list=ls() )
library(arfima)
library(fracdiff)
library(AER)
library(mvtnorm)

set.seed(123456)

reps=1000

N <- 100
d.y <- c(0, 0.1, 0.2, 0.3, 0.4, 0.45)
d.x <- 0.3
phi.y <- 0.5
phi.x <- 0.5
x.coef <- 0.5

srm <- matrix(data=NA, nrow=reps, ncol=length(d.y))
srm.p <- matrix(data=NA, nrow=reps, ncol=length(d.y))
lrm <- matrix(data=NA, nrow=reps, ncol=length(d.y))
lrm.p <- matrix(data=NA, nrow=reps, ncol=length(d.y))
sr.lr.sig <- matrix(data=NA, nrow=reps, ncol=length(d.y))

y.pred.err <- array(data=NA, dim=c(length(d.y), reps, 20))


for(k in 1:length(d.y)){
  
  cat("\n", "\n", "d.y = ", d.y[k], "\n")
  pb <- txtProgressBar(min=0, max=reps, initial=0, style=3)
  for(i in 1:reps){
    
    setTxtProgressBar(pb, value=i)
    
    # independent variable and its lag
    x <- arfima.sim( n = N, model=list(phi = phi.x, dfrac = d.x, dint = 0) )
    x.l <- c( NA, x[1:(N-1)])
    dif.x <- x - x.l
    
    # noise process
    u <- rnorm(N, mean=0, sd = 1)
    
    # innovation (signal + noise)
    e <- x.coef*x + u
    
    # dependent variable and its lag
    y <- arfima.sim(n = N, model=list(phi = phi.y, dfrac = d.y[k], dint = 0), innov = e)
    y.l <- c( NA, y[1:(N-1)] )
    dif.y <- y - y.l
    
    # standard ADL-type model
    adl <- lm(y ~ y.l + dif.x + x.l)
    bewley <- ivreg(y ~ dif.y + x + dif.x | y.l + x + dif.x)
    
    # hypothetical change in x
    y.eq <- summary(adl)$coefficients[1,1] / (1-summary(adl)$coefficients[2,1])
    y.l.pred <- y.eq
    x.pred <- c(rep(0, 4), rep(1, 16))
    x.l.pred <- c(rep(0,5), rep(1,15))
    dif.x.pred <- x.pred - x.l.pred
    
    # adl predictions of the future
    y.path <- c()
    for(j in 1:length(x.pred)){
      pred.dat <- data.frame(dif.x = dif.x.pred[j], y.l = y.l.pred[j], x.l = x.l.pred[j])
      y.path[j] <- predict(adl, newdata=pred.dat)
      y.l.pred[j+1] <- y.path[j]
    }
    y.path <- y.path - y.eq
    
    # noise process
    u <- 0
    
    # innovation (signal+ noise)
    e <- x.coef*x.pred + u
    
    # future true dependent variable and its lag
    y.true <- arfima.sim(n = 20, model=list(phi = phi.y, dfrac = d.y[k], dint = 0), innov = e)
    # determine prediction error over time
    y.pred.err[k,i, ] <- y.path - y.true
    
    
    # value of SR impact
    srm[i,k] <- summary(adl)$coefficients[3,1]
    # check significance of SR impact
    srm.p[i,k] <- summary(adl)$coefficients[3,4]
    # value of LRM
    lrm[i,k] <- summary(bewley)$coefficients[3,1]
    # check significance of LRM
    lrm.p[i,k] <- summary(bewley)$coefficients[3,4]
    
    # are SR and LRM different?
    all.draw <- rmvnorm(1000, mean=summary(adl)$coefficients[,1], sigma = vcov(adl))
    lr.draw <- all.draw[,4] / (1 - all.draw[,2])
    sr.draw <- all.draw[,3]
    sr.lr.dif <- sr.draw - lr.draw    
    sr.lr.sig[i,k] <- sign(quantile(sr.lr.dif, probs=c(0.025))) == sign(quantile(sr.lr.dif, probs=c(0.975)))
    
    
  }
  close(pb)
  
}




# short run impacts
par(mfrow=c(1,1), mar=c(5,4,4,2), las=1)
boxplot(srm[,1], srm[,2], srm[,3], srm[,4], srm[,5], srm[,6], main="immediate impacts of x on y", ylim=c(0, 1), ylab="estimates of immediate dy/dx", names=d.y, xlab="d value for y")
text(x=3.5, y=0.05, labels="percentage of statistically significant results")
for(j in 1:length(d.y)){
  text(x=j, y=0, labels=paste(100*(sum(srm.p[,j] < 0.05)/reps ), "%" ))
}
# This is Figure 3, Panel A
dev.copy2pdf(file="short-run-cumulative.pdf")


# long run impacts: statistical significance
100*(colSums(lrm.p < 0.05)/reps )   # 99.9 100.0  99.9  99.9  98.1  93.7
100*(colSums(sr.lr.sig)/reps )      # 92.6 98.5 99.4 99.8 99.1 95.7





par(mfrow=c(2,2), mar=c(4,5,4,2), las=2)
boxplot(y.pred.err[1,,6], y.pred.err[2,,6], y.pred.err[3,,6], y.pred.err[4,,6], y.pred.err[5,,6], y.pred.err[6,,6], main = "DV trajectory error, t* + 1", names=d.y, xlab="d parameter", ylab=expression(paste("prediction error for  ",  y[t^"*" + 1] - y^"*" )) )
boxplot(y.pred.err[1,,10], y.pred.err[2,,10], y.pred.err[3,,10], y.pred.err[4,,10], y.pred.err[5,,10], y.pred.err[6,,10], main = "DV trajectory error, t* + 5", names=d.y, xlab="d parameter", ylab=expression(paste("prediction error for  ",  y[t^"*" + 5] - y^"*" )) )
boxplot(y.pred.err[1,,15], y.pred.err[2,,15], y.pred.err[3,,15], y.pred.err[4,,15], y.pred.err[5,,15], y.pred.err[6,,15], main = "DV trajectory error, t* + 10", names=d.y, xlab="d parameter", ylab=expression(paste("prediction error for  ",  y[t^"*" + 10] - y^"*" )) )
boxplot(y.pred.err[1,,20], y.pred.err[2,,20], y.pred.err[3,,20], y.pred.err[4,,20], y.pred.err[5,,20], y.pred.err[6,,20], main = "DV trajectory error, t* + 15", names=d.y, xlab="d parameter", ylab=expression(paste("prediction error for  ",  y[t^"*" + 15] - y^"*" )) )

# This is Figure 3, Panel B
dev.copy2pdf(file="long-run-cumulative.pdf")





