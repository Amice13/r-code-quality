####
# Analysis of N=2000, low prevalence, J=4 simulations
# as appearing in the supplementary materials for
# ``List Experiment Design, Non-Strategic Respondent Error, and Item Count Technique Estimators''
# John S. Ahlquist
####
set.seed(93076)
library(boot)
h.param<-c(0,1)
m.param<-c(0,-2)
l.param<-c(0,-4)
h.pop<-as.vector(inv.logit(h.param%*%c(1,.5)))
m.pop<-as.vector(inv.logit(m.param%*%c(1,.5)))
l.pop<-as.vector(inv.logit(l.param%*%c(1,.5)))
alpha <- 0.1
critical <- abs(qnorm(alpha/2))

### Analysis of "Designed" simulations
load("PAsimulations_DLn2000J4b_4.RData")
N<-sim.object.designed[[1]]$ssize
sims<-length(sim.object.designed[[1]]$count.in.max)

#Expected \J(1,J+1) cardinality
e.top<-cbind(
	c(N*1/2*h.pop*1/2*1/2*1/2,
		N*1/2*m.pop*1/2*1/2*1/2,
		N*1/2*l.pop*1/2*1/2*1/2),
	c(N*1/2*h.pop*1/6*1/2*2/3*2/3, 
		N*1/2*m.pop*1/6*1/2*2/3*2/3,
		N*1/2*l.pop*1/6*1/2*2/3*2/3),
	c(N*1/2*h.pop*(.5*.5 - .6*(.5*.5))*.15, 
		N*1/2*m.pop*(.5*.5 - .6*(.5*.5))*.15,
		N*1/2*l.pop*(.5*.5 - .6*(.5*.5))*.15),
	c(N*1/2*h.pop*(.5*.5 - .6*(.5*.5))*.15*.85, 
		N*1/2*m.pop*(.5*.5 - .6*(.5*.5))*.15*.85,
		N*1/2*l.pop*(.5*.5 - .6*(.5*.5))*.15*.85)

)
xtable(round(e.top,1))


in.top.cat<-c(mean(sim.object.designed[[1]]$count.in.max), 
			median(sim.object.designed[[1]]$count.in.max),
			sum(sim.object.designed[[1]]$count.in.max==0)
			)
in.top.cat.err<-c(mean(sim.object.designed[[1]]$count.in.max.err), 
			median(sim.object.designed[[1]]$count.in.max.err),
			sum(sim.object.designed[[1]]$count.in.max.err==0)
			)
#convergence
crash.pct<-c(100*(sum(is.na(sim.object.designed[[1]]$est.pop.ml))/sims), 
			100*(sum(is.na(sim.object.designed[[1]]$est.pop.ml.err))/sims)
			)
data.frame(Error=c("none", "3%"), 
	Avgj1=c(in.top.cat[1],in.top.cat.err[1]),
	pct0=c(100*in.top.cat[3]/sims,100*in.top.cat.err[3]/sims),
	crash=c(crash.pct[1],crash.pct[2])
	)
#Stability/coverage
xtable(cbind(summary(sim.object.designed[[1]]$est.treat.ml[,2])[2:5],summary(sim.object.designed[[1]]$est.treat.ml.err[,2])[2:5]))

#pdf("N2000b_4popprev.pdf")
plot(density(sim.object.designed[[1]]$est.pop.ml, na.rm=T, adjust=3), lty=2, lwd=2,
	xlab = expression(hat(pi)[Z^"*"]^L), xlim=c(0,.35), bty="n",
	main="Distribution of point estimates for sensitive item prevalence")
lines(density(sim.object.designed[[1]]$est.pop.ols, na.rm=T, adjust=3),
	lwd=2, lty=2, col="SlateBlue")
lines(density(sim.object.designed[[1]]$est.pop.ml.err, na.rm=T, adjust=3), lwd=2)
lines(density(sim.object.designed[[1]]$est.pop.ols.err, na.rm=T, adjust=3),
	lwd=2, col="SlateBlue")
abline(v=.12, col="red", lty=3)

legend("topright",
	legend = c("ICT", #"J=3 ICT w/ error", 
		"DiM",
		"ICT w/error", 
		"DiM w/error"),
	#pch = c(2,2,1,1),
	col= c(1,"SlateBlue",1,"SlateBlue"),
	lty= c(2,2,1,1), bty="n"
	)
#dev.off()

#END