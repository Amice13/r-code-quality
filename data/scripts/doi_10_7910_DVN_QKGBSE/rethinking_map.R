plotParams <- function(post, row, col, credMass) {
    attach(mtcars)
		par(mfrow=c(row,col))
		
		mcmcMat = as.matrix(post)
		
		cols = colnames(mcmcMat)
		for ( i in 1:length(cols) ) {
				plotPost(mcmcMat[,cols[i]],xlab=cols[i], credMass=credMass)
		}
}

setwd("/Statistics/dataset03/stan")
dat <- read.csv("20180224_Legends_345.csv", header=TRUE)

library(rethinking)

#============= models of B ==========
mB.1 <- map2stan(
alist(
B ~ dbinom( 1 , lp ) ,
logit(lp) <- a + bVB*VB + bAVB*AVB,
a ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bAVB~ dnorm(0,10)
) , 
chains = 4,
data=dat )

mB.2 <- map2stan(
alist(
B ~ dbinom( 1 , lp ) ,
logit(lp) <- a + (bVB + bVC*VC +bVT*VT)*VB + bAVB*AVB,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVB~ dnorm(0,10)
) ,
chains = 4,
data=dat )

mB.3 <- map2stan(
alist(
B ~ dbinom( 1 , lp ) ,
logit(lp) <- a + (bVB*VB + bVC*VC +bVT*VT) + bAVB*AVB,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVB~ dnorm(0,10)
) ,
chains = 4,
data=dat )

stan_diag(attr(mB.1,"stanfit"))
stan_diag(attr(mB.2,"stanfit"))
stan_diag(attr(mB.3,"stanfit"))

#stan_plot(attr(mB.1,"stanfit"))

precis(mB.1)
precis(mB.2)
precis(mB.3)

plot(coeftab(mB.1,mB.2,mB.3))
//pairs(mB.1)

pars <- attr(mB.1,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mB.1,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mB.1,"stanfit")),select=pars), 3, 2, 0.95)

pars <- attr(mB.2,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mB.2,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mB.2,"stanfit")),select=pars), 3, 2, 0.95)

pars <- attr(mB.3,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mB.3,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mB.3,"stanfit")),select=pars), 3, 2, 0.95)

#mcmcpairs(mB.1)

#============= models of C ==========
mC.1 <- map2stan(
alist(
C ~ dbinom( 1 , lp ) ,
logit(lp) <- a + bVC*VC + bAVC*AVC,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bAVC~ dnorm(0,10)
) ,
chains = 4,
data=dat)

mC.2 <- map2stan(
alist(
C ~ dbinom( 1 , lp ) ,
logit(lp) <- a + (bVC + bVB*VB +bVT*VT)*VC + bAVC*AVC,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVC~ dnorm(0,10)
) ,
chains = 4,
data=dat )

mC.3 <- map2stan(
alist(
C ~ dbinom( 1 , lp ) ,
logit(lp) <- a + (bVC*VC + bVB*VB +bVT*VT) + bAVC*AVC,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVC~ dnorm(0,10)
) ,
chains = 4,
data=dat )

plot(coeftab(mC.1,mC.2,mC.3))

pars <- attr(mC.1,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mC.1,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mC.1,"stanfit")),select=pars), 3, 2, 0.95)

pars <- attr(mC.2,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mC.2,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mC.2,"stanfit")),select=pars), 3, 2, 0.95)

pars <- attr(mC.3,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mC.3,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mC.3,"stanfit")),select=pars), 3, 2, 0.95)

#============= models of T ==========
mT.1 <- map2stan(
alist(
T ~ dbinom( 1 , lp ) ,
logit(lp) <- a + bVT*VT + bAVT*AVT,
a ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVT~ dnorm(0,10)
) ,
chains = 4,
data=dat )

mT.2 <- map2stan(
alist(
T ~ dbinom( 1 , lp ) ,
logit(lp) <- a + (bVT + bVB*VB +bVC*VC)*VT + bAVT*AVT,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVT~ dnorm(0,10)
) ,
chains = 4,
data=dat )

mT.3 <- map2stan(
alist(
T ~ dbinom( 1 , lp ) ,
logit(lp) <- a + (bVT*VT + bVB*VB +bVC*VC) + bAVT*AVT,
a ~ dnorm(0,10),
bVC ~ dnorm(0,10),
bVB ~ dnorm(0,10),
bVT ~ dnorm(0,10),
bAVT~ dnorm(0,10)
) ,
chains = 4,
data=dat )

plot(coeftab(mT.1,mT.2,mT.3))

pars <- attr(mT.1,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mT.1,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mT.1,"stanfit")),select=pars), 3, 2, 0.95)

pars <- attr(mT.2,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mT.2,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mT.2,"stanfit")),select=pars), 3, 2, 0.95)

pars <- attr(mT.3,"pars")
pars <-pars[pars!="dev"]
mcmcpairs(subset(as.data.frame(attr(mT.3,"stanfit")),select=pars), col=col.alpha("maroon",0.8), cex=1.5 )
plotParams(subset(as.data.frame(attr(mT.3,"stanfit")),select=pars), 3, 2, 0.95)
