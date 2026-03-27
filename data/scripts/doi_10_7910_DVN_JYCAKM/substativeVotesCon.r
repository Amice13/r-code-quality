# present substantive effects: Conservative votes
load("estimates.RData")
source("effectSim.R");source("effectSimPlot.R")
library(AER);library(arm)
summary(modsconVotes[[1]])
x <- cbind(1,0,0,0,0,0,1,0,0,0,0,1:40,1:40,0,0,0,0,0)
prob.base <- effectSim(model=modsconVotes[[1]],covvar=vcovHAC(modsconVotes[[1]]),
                       x=x,sims=1000)
x1 <- cbind(1,0,0,0,0,1,1,0,0,0,0,1:40,1:40,1,0,0,0,0)
x2 <- cbind(1,0,0,0,0,0,0,0,1,0,0,1:40,1:40,0,0,0,0,0)
x3 <- cbind(1,0,0,0,0,1,0,0,1,0,0,1:40,1:40,0,0,1,0,0)
prob.priv <- effectSim(model=modsconVotes[[1]],covvar=vcovHAC(modsconVotes[[1]]),
                       x=x1,sims=1000)
input.prob <- function(p,low,high){
  predicted.prob <- t(apply(p, 1, quantile, probs = c(0.05, 
                                                      0.5, 0.95)))
  cord.y <- c(predicted.prob[, 1], rev(predicted.prob[, 
                                                      3]))
  cord.x <- c(seq(low, high, length = nrow(p)), seq(high, 
                                                    low, length = nrow(p)))
  return(cbind(cord.x,cord.y))
}
privs <- input.prob(prob.priv,low=1,high=40) 

prob.com <- effectSim(model=modsconVotes[[1]],covvar=vcovHAC(modsconVotes[[1]]),
                      x=x2,sims=1000)
prob.priv.com <- effectSim(model=modsconVotes[[1]],covvar=vcovHAC(modsconVotes[[1]]),
                           x=x3,sims=1000)
privs.com <- input.prob(prob.priv.com,low=1,high=40) 

lab <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1918,1922,1925,1926,1930,1935,1940,1945,1949,1953,1957,1958,1962,1963,1965,1968,1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
tlab <- 1:40

pdf("Figure-C1a.pdf",,width=7,height=2.75)
par(mar=c(4.1,4.1,3.1,2.1))
effectSimPlot(prob.base,low=1,high=40,i=12,x=x,ylim=c(0,1),rug=F,color="grey70",
              covariate=cons$Parlement,xlab="Party Unity (3rd Reading)", sub="Cabinet vs. Private Member Motion (Conservative)",
              ylab="Party Unity",bty="n",col="grey75",xaxt="n")
axis(1,at=seq(1,40,1),labels=lab)
polygon(privs[,1],privs[,2],col="grey40",border=NA)
lines(x[, 12], t(apply(prob.priv, 1, quantile, probs = 0.5)), lty = 1)
legend("right",legend=c("Cabinet","Private"),#,"Proportion of Private Motion"),
       col=c("grey70","grey40","black"),lwd=4,bty="n",lty=c(1,1,3),cex=.75)
dev.off()
pdf("Figure-C1b.pdf",,width=7,height=2.75)
par(mar=c(4.1,4.1,3.1,2.1))
effectSimPlot(prob.com,low=1,high=40,i=12,x=x,ylim=c(0,1),rug=F,color="grey70",
              covariate=cons$Parlement,xlab="Party Unity (Supply Motion)", sub="Cabinet vs. Private Member Motion (Conservative)",
              ylab="Party Unity",bty="n",xaxt="n")
polygon(privs.com[,1],privs.com[,2],col="grey40",border=NA)
lines(x[, 12], t(apply(prob.priv.com, 1, quantile, probs = 0.5)), lty = 1)
axis(1,at=seq(1,40,1),labels=lab)
legend("right",legend=c("Cabinet","Private"),#,"Proportion of Supply Motion"),
       col=c("grey70","grey40","black"),lwd=4,bty="n",lty=c(1,1,3),cex=.75)
dev.off()
