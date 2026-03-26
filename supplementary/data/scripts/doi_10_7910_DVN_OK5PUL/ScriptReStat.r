### Script for Info-Rigid Project, revised for ReStat
#	Tucker McElroy

# load package Matrix, expm
library(expm)

# load all functions in "FunctionsReStat.r"
source("infoDyn.ssf.r")
source("infoDyn.ssf2.r")
source("varp.param.r")
source("vol.pg.process.r")
source("vol.cs.process.r")
source("matrix.exp.r")
source("sigex.blocktoep.r")
source("VARMAauto.r")


################# simulate private and public data

# data.private  is T x M1
# data.public   is T x M2
# obs.private     is M1 x N
# obs.public      is M2 x N

## Data for project:  a trivariate stable VAR(2) with public and private noise
 
N <- 3
T <- 100
burnin <- 500
disp.private.low <- .1		# low (valuable private info)
disp.private.hi <- 20		# high (less valuable private info)
disp.public <- .01		# good amount of noise for MSE
disp.coef <- .001

##################################################
### one choice based on fitted VAR(2) to IP, Inflation, and FFR
param <- array(0,c(3,3,3))
var.order <- 2
param[,,1] <- cbind(c(0.774504,0.055621,0.106624),
	c(-0.147158,0.313947,0.058166),c(-0.429192,0.111648,1.330926))
param[,,2] <- cbind(c(-0.051712,0.017029,-0.005438),
	c(0.097873,0.181283,0.038404),c(0.470758,-0.015093,-0.387158))
param[,,3] <- diag(3)
   
 

# simulate signal process pi

pi.init <- matrix(rnorm(N*var.order),N,var.order)
pi <- pi.init
for(t in (var.order+1):(burnin+T))
{
	pi.new  <- param[,,1] %*% pi[,t-1] + param[,,2] %*% pi[,t-2] + 
		t(chol(param[,,3])) %*% matrix(rnorm(N),nrow=N)
	pi <- cbind(pi,pi.new)
}
pi <- pi[,(burnin+1):(burnin+T)]
pi <- t(pi)
plot(ts(pi,start=1,names=c("Industrial Production","Inflation","Funds Rate")),main="Signal",lwd=2)

###########################
# define shocks

#shock <- 50+1*1i	# LS shock halfway through
shock <- 50+0*1i 	# AO shock halfway through
#shock <- 0+0*1i	# no shock

########################################
# simulate public noise eta for all inattentive and attentive agents
#	het.covs is larger at time t=Re(shock), and eta at the shock time(s)
#		is generated from the tails

# large shock
z.right <- 2
het.covs <- vol.cs.process(disp.public,N,100,shock,19)
het.covs.large <- het.covs

# moderate shock
#z.right <- 1.5
#het.covs <- vol.cs.process(disp.public,N,100,shock,3)
#het.covs.mod <- het.covs

eta <- NULL
for(t in 1:T)
{
	if( ((t==Re(shock)) && (Im(shock)==0)) || ((t>=Re(shock)) && (Im(shock)==1)) )
	{ 
		z.sim <- matrix(qnorm(1 - runif(N)*(1-pnorm(z.right))),nrow=N)
	} else { z.sim <- matrix(rnorm(N),nrow=N) }
	eta.new <- t(chol(het.covs[,t,])) %*% z.sim
	eta <- cbind(eta,eta.new)
}
eta <- t(eta)
plot(ts(eta,start=1,names=c("Industrial Production","Inflation","Funds Rate")),main="Public Noise",lwd=2)

eta.large <- eta
#eta.mod <- eta

###############################################################
## compute all measures for batch of inattentive agents

# user sets quantities of agents
# batch 1: inattentive
num.inattentive <- 10
num.attentive <- 0

num.agents <- num.inattentive+num.attentive
if(num.inattentive > 0) {
	betas.inattentive <- 9 + runif(num.inattentive)
} else { betas.inattentive <- NULL }
if(num.attentive > 0) {
	betas.attentive <- 45 + 10*runif(num.attentive)	
} else { betas.attentive <- NULL }
betas <- c(betas.inattentive,betas.attentive)
alpha <- 1

betas.largeinatt <- betas.inattentive

obs.public <- diag(N)
data.public <- pi %*% t(obs.public) + eta
disp.private <- disp.private.low

G.mat <- t(c(1,rep(0,var.order-1)) %x% diag(N))
hom.covs <- array(0,c(N,N,num.agents))
priv.units <- array(0,c(T,num.agents))
rigid.tr <- rep(0,T)
rigid.det <- rep(0,T)
mean.fores <- matrix(0,nrow=N,ncol=T)
agent.mse <- array(0,c(N,N,T,num.agents))
agent.disp <- array(0,c(N,N,T,num.agents))
consensus.mse <- array(0,c(N,N,T))

# first, determine all purchases of private info,
#	and record private info variances and purchase orders;
#	determine overall IR, consensus forecasts, and agent MSEs
for(i in 1:num.agents)
{
	# generate random private information for agent i
	hom.cov <- varp.param(0,N,disp.coef,disp.private)[,,1]
	hom.covs[,,i] <- hom.cov
	# compute SSF quantities, and units of private info purchased
	out.ssf <- infoDyn.ssf(data.public,pi,obs.public,param,hom.cov,het.covs,betas[i],alpha)
	# print units of private info at each time purchased
	priv.units[,i] <- out.ssf[[4]]
	mean.fores <- mean.fores + G.mat %*% out.ssf[[1]][,-1]/num.agents
	rigids.tr <- NULL
	rigids.det <- NULL
	for(t in 1:T)
	{
		rigids.tr <- c(rigids.tr,1-sum(diag(out.ssf[[3]][,,t]))/N)
		rigids.det <- c(rigids.det,det(diag(N)-out.ssf[[3]][,,t]))
	}
	rigid.tr <- rigid.tr + rigids.tr/num.agents
	rigid.det <- rigid.det + rigids.det/num.agents
	agent.mse[,,,i] <- array(do.call(cbind,lapply(seq(1,T), function(t) G.mat %*% out.ssf[[2]][,,t+1] %*% t(G.mat))),c(N,N,T))

	print(i)
}

hom.covs.largeinatt <- hom.covs

# second, use private info variances and purchase orders
#	to compute consensus MSE and agent dispersions
for(j in 1:num.agents)
{
	out.ssf <- infoDyn.ssf(data.public,pi,obs.public,param,hom.covs[,,j],het.covs,betas[j],alpha)
	for(k in 1:num.agents)
	{
		if(k == j) { out.cross <- out.ssf[[2]] } else {
				out.cross <- infoDyn.ssf2(data.public,pi,obs.public,param,hom.covs[,,j],hom.covs[,,k],het.covs,priv.units[,j],priv.units[,k]) 
		}			
		consensus.mse <- consensus.mse + array(do.call(cbind,lapply(seq(1,T), function(t) G.mat %*% out.cross[,,t+1] %*% t(G.mat))),c(N,N,T))/num.agents^2
		for(i in 1:num.agents)
		{
			disp.weights <- rep(-1/num.agents,num.agents)
			disp.weights[i] <- 1-1/num.agents
			agent.disp[,,,i] <- agent.disp[,,,i] + disp.weights[j]*disp.weights[k]*array(do.call(cbind,lapply(seq(1,T), function(t) G.mat %*% out.cross[,,t+1] %*% t(G.mat))),c(N,N,T))
		}

		print(c(j,k))
	}
}

agents.mse <- apply(agent.mse,c(1,2,3),mean)
agents.disp <- apply(agent.disp,c(1,2,3),mean)

rigid.tr.largeinatt <- rigid.tr

agents.mse.largeinatt <- agents.mse

agents.disp.largeinatt <- agents.disp


########################################
## plotting for inattentive agents


# information rigidity

low <- .02
upp <- .08
plot(ts(rigid.tr.largeinatt[-seq(1,5)],start=6),ylab="",main="Inattentive",ylim=c(low,upp),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

## estimated consensus forecasts

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(pi[,1],start=1),ylab="Industrial Production",main="",col=1,lwd=2)
lines(ts(mean.fores[1,],start=1),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(pi[,2],start=1),ylab="Inflation",main="",col=1,lwd=2)
lines(ts(mean.fores[2,],start=1),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(pi[,3],start=1),ylab="Funds Rate",main="",col=1,lwd=2)
lines(ts(mean.fores[3,],start=1),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Signal with Consensus Forecast",side=3,line=-1,outer=TRUE)

dev.off()
 

## mse of consensus forecasts

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(consensus.mse[1,1,-seq(1,5)],start=6),ylim=c(min(consensus.mse[1,1,1:T]),
	max(consensus.mse[1,1,(var.order+1):T])),ylab="Industrial Production",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(consensus.mse[2,2,-seq(1,5)],start=6),ylim=c(min(consensus.mse[2,2,1:T]),
	max(consensus.mse[2,2,(var.order+1):T])),ylab="Inflation",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(consensus.mse[3,3,-seq(1,5)],start=6),ylim=c(min(consensus.mse[3,3,1:T]),
	max(consensus.mse[3,3,(var.order+1):T])),ylab="Funds Rate",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Consensus MSE",side=3,line=-1,outer=TRUE)

dev.off()


## agent mse (choose an index from 1:num.agents)

agent.index <- num.inattentive				# an example inattentive agent

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=.8)

plot(ts(agent.mse[1,1,-seq(1,5),agent.index],start=6),ylim=c(min(agent.mse[1,1,1:T,agent.index]),
	max(agent.mse[1,1,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.mse[2,2,-seq(1,5),agent.index],start=6),ylim=c(min(agent.mse[2,2,1:T,agent.index]),
	max(agent.mse[2,2,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.mse[3,3,-seq(1,5),agent.index],start=6),ylim=c(min(agent.mse[3,3,1:T,agent.index]),
	max(agent.mse[3,3,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent MSE",side=3,line=-1,outer=TRUE)

dev.off()


## agent disagreement (choose an index from 1:num.agents)

agent.index <- num.inattentive				# an example inattentive agent

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=.8)

plot(ts(agent.disp[1,1,-seq(1,5),agent.index],start=6),ylim=c(min(agent.disp[1,1,1:T,agent.index]),
	max(agent.disp[1,1,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.disp[2,2,-seq(1,5),agent.index],start=6),ylim=c(min(agent.disp[2,2,1:T,agent.index]),
	max(agent.disp[2,2,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.disp[3,3,-seq(1,5),agent.index],start=6),ylim=c(min(agent.disp[3,3,1:T,agent.index]),
	max(agent.disp[3,3,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent Disagreement",side=3,line=-1,outer=TRUE)

dev.off()



## aggregate agent mse 

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(agents.mse[1,1,-seq(1,5)],start=6),ylim=c(min(agents.mse[1,1,1:T]),
	max(agents.mse[1,1,(var.order+1):T])),ylab="Industrial Production",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.mse[2,2,-seq(1,5)],start=6),ylim=c(min(agents.mse[2,2,1:T]),
	max(agents.mse[2,2,(var.order+1):T])),ylab="Inflation",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.mse[3,3,-seq(1,5)],start=6),ylim=c(min(agents.mse[3,3,1:T]),
	max(agents.mse[3,3,(var.order+1):T])),ylab="Funds Rate",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent MSE",side=3,line=-1,outer=TRUE)

dev.off()



## aggregate agent disagreement 
 
par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(agents.disp[1,1,-seq(1,5)],start=6),ylim=c(min(agents.disp[1,1,-seq(1,5)]),
	max(agents.disp[1,1,(var.order+1):T])),ylab="Industrial Production",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.disp[2,2,-seq(1,5)],start=6),ylim=c(min(agents.disp[2,2,-seq(1,5)]),
	max(agents.disp[2,2,(var.order+1):T])),ylab="Inflation",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.disp[3,3,-seq(1,5)],start=6),ylim=c(min(agents.disp[3,3,-seq(1,5)]),
	max(agents.disp[3,3,(var.order+1):T])),ylab="Funds Rate",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent Disagreement",side=3,line=-1,outer=TRUE)

dev.off()


 

###############################################################
## compute all measures for batch of inattentive/attentive agents

# user sets quantities of agents
# batch 2: attentive
num.inattentive <- 0
num.attentive <- 10

num.agents <- num.inattentive+num.attentive
if(num.inattentive > 0) {
	betas.inattentive <- 9 + runif(num.inattentive)
} else { betas.inattentive <- NULL }
if(num.attentive > 0) {
	betas.attentive <- 45 + 10*runif(num.attentive)	
} else { betas.attentive <- NULL }
betas <- c(betas.inattentive,betas.attentive)
alpha <- 1

betas.largeatt <- betas.attentive

obs.public <- diag(N)
data.public <- pi %*% t(obs.public) + eta
disp.private <- disp.private.low

G.mat <- t(c(1,rep(0,var.order-1)) %x% diag(N))
hom.covs <- array(0,c(N,N,num.agents))
priv.units <- array(0,c(T,num.agents))
rigid.tr <- rep(0,T)
rigid.det <- rep(0,T)
mean.fores <- matrix(0,nrow=N,ncol=T)
agent.mse <- array(0,c(N,N,T,num.agents))
agent.disp <- array(0,c(N,N,T,num.agents))
consensus.mse <- array(0,c(N,N,T))

# first, determine all purchases of private info,
#	and record private info variances and purchase orders;
#	determine overall IR, consensus forecasts, and agent MSEs
for(i in 1:num.agents)
{
	# generate random private information for agent i
	hom.cov <- varp.param(0,N,disp.coef,disp.private)[,,1]
	hom.covs[,,i] <- hom.cov
	# compute SSF quantities, and units of private info purchased
	out.ssf <- infoDyn.ssf(data.public,pi,obs.public,param,hom.cov,het.covs,betas[i],alpha)
	# print units of private info at each time purchased
	priv.units[,i] <- out.ssf[[4]]
	mean.fores <- mean.fores + G.mat %*% out.ssf[[1]][,-1]/num.agents
	rigids.tr <- NULL
	rigids.det <- NULL
	for(t in 1:T)
	{
		rigids.tr <- c(rigids.tr,1-sum(diag(out.ssf[[3]][,,t]))/N)
		rigids.det <- c(rigids.det,det(diag(N)-out.ssf[[3]][,,t]))
	}
	rigid.tr <- rigid.tr + rigids.tr/num.agents
	rigid.det <- rigid.det + rigids.det/num.agents
	agent.mse[,,,i] <- array(do.call(cbind,lapply(seq(1,T), function(t) G.mat %*% out.ssf[[2]][,,t+1] %*% t(G.mat))),c(N,N,T))

	print(i)
}

hom.covs.largeatt <- hom.covs

# second, use private info variances and purchase orders
#	to compute consensus MSE and agent dispersions
for(j in 1:num.agents)
{
	out.ssf <- infoDyn.ssf(data.public,pi,obs.public,param,hom.covs[,,j],het.covs,betas[j],alpha)
	for(k in 1:num.agents)
	{
		if(k == j) { out.cross <- out.ssf[[2]] } else {
				out.cross <- infoDyn.ssf2(data.public,pi,obs.public,param,hom.covs[,,j],hom.covs[,,k],het.covs,priv.units[,j],priv.units[,k]) 
		}			
		consensus.mse <- consensus.mse + array(do.call(cbind,lapply(seq(1,T), function(t) G.mat %*% out.cross[,,t+1] %*% t(G.mat))),c(N,N,T))/num.agents^2
		for(i in 1:num.agents)
		{
			disp.weights <- rep(-1/num.agents,num.agents)
			disp.weights[i] <- 1-1/num.agents
			agent.disp[,,,i] <- agent.disp[,,,i] + disp.weights[j]*disp.weights[k]*array(do.call(cbind,lapply(seq(1,T), function(t) G.mat %*% out.cross[,,t+1] %*% t(G.mat))),c(N,N,T))
		}

		print(c(j,k))
	}
}

agents.mse <- apply(agent.mse,c(1,2,3),mean)
agents.disp <- apply(agent.disp,c(1,2,3),mean)

rigid.tr.largeatt <- rigid.tr

agents.mse.largeatt <- agents.mse

agents.disp.largeatt <- agents.disp



########################################
## plotting for attentive agents


# information rigidity

low <- .02
upp <- .08
plot(ts(rigid.tr.largeatt[-seq(1,5)],start=6),ylab="",main="Attentive",ylim=c(low,upp),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)


## estimated consensus forecasts

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(pi[,1],start=1),ylab="Industrial Production",main="",col=1,lwd=2)
lines(ts(mean.fores[1,],start=1),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(pi[,2],start=1),ylab="Inflation",main="",col=1,lwd=2)
lines(ts(mean.fores[2,],start=1),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(pi[,3],start=1),ylab="Funds Rate",main="",col=1,lwd=2)
lines(ts(mean.fores[3,],start=1),col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Signal with Consensus Forecast",side=3,line=-1,outer=TRUE)

dev.off()
 

## mse of consensus forecasts

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(consensus.mse[1,1,-seq(1,5)],start=6),ylim=c(min(consensus.mse[1,1,1:T]),
	max(consensus.mse[1,1,(var.order+1):T])),ylab="Industrial Production",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(consensus.mse[2,2,-seq(1,5)],start=6),ylim=c(min(consensus.mse[2,2,1:T]),
	max(consensus.mse[2,2,(var.order+1):T])),ylab="Inflation",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(consensus.mse[3,3,-seq(1,5)],start=6),ylim=c(min(consensus.mse[3,3,1:T]),
	max(consensus.mse[3,3,(var.order+1):T])),ylab="Funds Rate",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Consensus MSE",side=3,line=-1,outer=TRUE)

dev.off()


## agent mse (choose an index from 1:num.agents)

agent.index <- num.inattentive + num.attentive		# an example attentive agent

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=.8)

plot(ts(agent.mse[1,1,-seq(1,5),agent.index],start=6),ylim=c(min(agent.mse[1,1,1:T,agent.index]),
	max(agent.mse[1,1,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.mse[2,2,-seq(1,5),agent.index],start=6),ylim=c(min(agent.mse[2,2,1:T,agent.index]),
	max(agent.mse[2,2,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.mse[3,3,-seq(1,5),agent.index],start=6),ylim=c(min(agent.mse[3,3,1:T,agent.index]),
	max(agent.mse[3,3,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent MSE",side=3,line=-1,outer=TRUE)

dev.off()


## agent disagreement (choose an index from 1:num.agents)

agent.index <- num.inattentive + num.attentive		# an example attentive agent

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=.8)

plot(ts(agent.disp[1,1,-seq(1,5),agent.index],start=6),ylim=c(min(agent.disp[1,1,1:T,agent.index]),
	max(agent.disp[1,1,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.disp[2,2,-seq(1,5),agent.index],start=6),ylim=c(min(agent.disp[2,2,1:T,agent.index]),
	max(agent.disp[2,2,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agent.disp[3,3,-seq(1,5),agent.index],start=6),ylim=c(min(agent.disp[3,3,1:T,agent.index]),
	max(agent.disp[3,3,(var.order+1):T,agent.index])),ylab="",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent Disagreement",side=3,line=-1,outer=TRUE)

dev.off()



## aggregate agent mse 

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(agents.mse[1,1,-seq(1,5)],start=6),ylim=c(min(agents.mse[1,1,1:T]),
	max(agents.mse[1,1,(var.order+1):T])),ylab="Industrial Production",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.mse[2,2,-seq(1,5)],start=6),ylim=c(min(agents.mse[2,2,1:T]),
	max(agents.mse[2,2,(var.order+1):T])),ylab="Inflation",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.mse[3,3,-seq(1,5)],start=6),ylim=c(min(agents.mse[3,3,1:T]),
	max(agents.mse[3,3,(var.order+1):T])),ylab="Funds Rate",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent MSE",side=3,line=-1,outer=TRUE)

dev.off()



## aggregate agent disagreement 
 
par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(agents.disp[1,1,-seq(1,5)],start=6),ylim=c(min(agents.disp[1,1,-seq(1,5)]),
	max(agents.disp[1,1,(var.order+1):T])),ylab="Industrial Production",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.disp[2,2,-seq(1,5)],start=6),ylim=c(min(agents.disp[2,2,-seq(1,5)]),
	max(agents.disp[2,2,(var.order+1):T])),ylab="Inflation",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

plot(ts(agents.disp[3,3,-seq(1,5)],start=6),ylim=c(min(agents.disp[3,3,-seq(1,5)]),
	max(agents.disp[3,3,(var.order+1):T])),ylab="Funds Rate",main="",col=3,lwd=2)
abline(v=Re(shock),col=2,lty=2,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent Disagreement",side=3,line=-1,outer=TRUE)

dev.off()





##########################################################
### Some combined graphs for inattentive with attentive


## aggregate agent mse: large shock, inattentive vs attentive

par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(agents.mse.largeinatt[1,1,-seq(1,5)],start=6),ylim=c(1.01,1.09),ylab="Industrial Production",main="",col=3,lwd=2,lty=2)
lines(ts(agents.mse.largeatt[1,1,-seq(1,5)],start=6),col=4,lwd=2)
abline(v=Re(shock),col=2,lty=3,lwd=2)

plot(ts(agents.mse.largeinatt[2,2,-seq(1,5)],start=6),ylim=c(1.00,1.015),ylab="Inflation",main="",col=3,lwd=2,lty=2)
lines(ts(agents.mse.largeatt[2,2,-seq(1,5)],start=6),col=4,lwd=2)
abline(v=Re(shock),col=2,lty=3,lwd=2)

plot(ts(agents.mse.largeinatt[3,3,-seq(1,5)],start=6),ylim=c(1.03,1.2),ylab="Funds Rate",main="",col=3,lwd=2,lty=2)
lines(ts(agents.mse.largeatt[3,3,-seq(1,5)],start=6),col=4,lwd=2)
abline(v=Re(shock),col=2,lty=3,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent MSE",side=3,line=-1,outer=TRUE)

dev.off()

## aggregate agent disagreement: large shock, inattentive vs attentive 
 
par(oma=c(2,0,0,0)+.5,mar=c(2,4,2,2)+0.1,mfrow=c(3,1),cex.lab=1)

plot(ts(agents.disp.largeinatt[1,1,-seq(1,5)],start=6),ylim=c(0,.07),ylab="Industrial Production",main="",col=3,lwd=2,lty=2)
lines(ts(agents.disp.largeatt[1,1,-seq(1,5)],start=6),col=4,lwd=2)
abline(v=Re(shock),col=2,lty=3,lwd=2)

plot(ts(agents.disp.largeinatt[2,2,-seq(1,5)],start=6),ylim=c(0,.015),ylab="Inflation",main="",col=3,lwd=2,lty=2)
lines(ts(agents.disp.largeatt[2,2,-seq(1,5)],start=6),col=4,lwd=2)
abline(v=Re(shock),col=2,lty=3,lwd=2)

plot(ts(agents.disp.largeinatt[3,3,-seq(1,5)],start=6),ylim=c(0,.15),ylab="Funds Rate",main="",col=3,lwd=2,lty=2)
lines(ts(agents.disp.largeatt[3,3,-seq(1,5)],start=6),col=4,lwd=2)
abline(v=Re(shock),col=2,lty=3,lwd=2)

mtext(text="Time",side=1,line=1,outer=TRUE)
mtext(text="Agent Disagreement",side=3,line=-1,outer=TRUE)

dev.off()

