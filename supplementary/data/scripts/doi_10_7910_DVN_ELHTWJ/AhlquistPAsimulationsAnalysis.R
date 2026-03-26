## ##########################################
## This code anlyzes the Monte Carlo simulations and generates graphics as found in 
## "List Experiment Design, Non-Strategic Respondent Error, and Item Count Technique Estimators"
##
## ##########################################
rm(list=ls()); gc()
setwd("/PAReplicationFiles/MCoutput/")
set.seed(93076)
library(boot); library(xtable); library(extrafont); library(list)
library(foreign)
loadfonts()
h.param<-c(0,1)
m.param<-c(0,-2)
l.param<-c(0,-4)
h.pop<-as.vector(inv.logit(h.param%*%c(1,.5)))
m.pop<-as.vector(inv.logit(m.param%*%c(1,.5)))
l.pop<-as.vector(inv.logit(l.param%*%c(1,.5)))
alpha <- 0.1
critical <- abs(qnorm(alpha/2))


### Analysis of "Designed" simulations
load("PAsimulations_designed1000.Rdata")
sims<-length(sim.object.designed[[1]]$count.in.max)
N<-sim.object.designed[[1]]$ssize

#Table 1
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

in.top.cat<-in.top.cat.err<-NULL
for(i in 1:6){
	in.top.cat<-rbind(in.top.cat,
		c(mean(sim.object.designed[[i]]$count.in.max), 
			median(sim.object.designed[[i]]$count.in.max),
			sum(sim.object.designed[[i]]$count.in.max==0)
			)
		)
	in.top.cat.err<-rbind(in.top.cat.err,
		c(mean(sim.object.designed[[i]]$count.in.max.err), 
			median(sim.object.designed[[i]]$count.in.max.err),
			sum(sim.object.designed[[i]]$count.in.max.err==0)
			)
		)
}


#convergence
crash.pct<-NULL
for(i in 1:6){
	crash.pct<-rbind(crash.pct,
		c(100*(sum(is.na(sim.object.designed[[i]]$est.pop.ml))/sims), 
			100*(sum(is.na(sim.object.designed[[i]]$est.pop.ml.err))/sims)
		))
}

###### Fig  1
#pdf("ConvergenceDesigned.pdf")
#par(mfrow=c(2,1))
plot(in.top.cat[,1],crash.pct[,1], bty="n", 
	xlim=c(0,21), ylim=c(-1,6),# yaxt = "n",
	xlab="Mean number of J+1 observations",
	ylab="% exiting with error", type="n", family="Verdana",
	main="ICT-MLE stability for designed list experiments", las=1)
#axis(side=2, at = 0:6, family="Verdanda")
text(x=in.top.cat[,1],y=crash.pct[,1], family="Verdana",
	labels = c("3H","3M","3L","4H","4M","4L"))
text(x=jitter(in.top.cat.err[,1], amount=.5),y=jitter(crash.pct[,2], amount=.5 ), 
	labels = c("3H","3M","3L","4H","4M","4L"),
	font=2,family="Verdana", pos=4)
#dev.off()
#embed_fonts("ConvergenceDesigned.pdf", outfile="ConvergenceDesigned.pdf")


#### b_1 bias, RMSE, coverage
	param.bias<-NULL
	for(i in 1:6){
		if(i %in% c(1,4)){
		param.bias<-rbind(param.bias,
			c(apply(sim.object.designed[[i]]$est.treat.ml, 2, mean, na.rm=T)-h.param, 
			  apply(sim.object.designed[[i]]$est.treat.ml.err, 2, mean, na.rm=T)-h.param
				)
			)
		}
		if(i %in% c(2,5)){
			param.bias<-rbind(param.bias,
			c(apply(sim.object.designed[[i]]$est.treat.ml, 2, mean, na.rm=T)-m.param, 
			  apply(sim.object.designed[[i]]$est.treat.ml.err, 2, mean, na.rm=T)-m.param
				)
			)
		}
		if(i %in% c(3,6)){
			param.bias<-rbind(param.bias,
			c(apply(sim.object.designed[[i]]$est.treat.ml, 2, mean, na.rm=T)-l.param, 
			  apply(sim.object.designed[[i]]$est.treat.ml.err, 2, mean, na.rm=T)-l.param
				)
			)
		}
	}

	param.var<-NULL
	for(i in 1:6){
		if(i %in% c(1,4)){
		param.var<-rbind(param.var,
			c(apply(sim.object.designed[[i]]$est.treat.ml, 2, var, na.rm=T), 
			  apply(sim.object.designed[[i]]$est.treat.ml.err, 2, var, na.rm=T)
				)
			)
		}
		if(i %in% c(2,5)){
			param.var<-rbind(param.var,
			c(apply(sim.object.designed[[i]]$est.treat.ml, 2, var, na.rm=T), 
			  apply(sim.object.designed[[i]]$est.treat.ml.err, 2, var, na.rm=T)
				)
			)
		}
		if(i %in% c(3,6)){
			param.var<-rbind(param.var,
			c(apply(sim.object.designed[[i]]$est.treat.ml, 2, var, na.rm=T), 
			  apply(sim.object.designed[[i]]$est.treat.ml.err, 2, var, na.rm=T)
				)
			)
		}
	}
	param.rmse<-sqrt(param.bias^2+param.var)
	param.bias<-cbind(param.bias,c(3,3,3,4,4,4), c(h.pop, m.pop,l.pop,h.pop, m.pop,l.pop))
	colnames(param.bias)<-c("b0","b1","b0.err","b1.err", "J","pop.prev")
	param.bias<-as.data.frame(param.bias)

	param.rmse<-cbind(param.rmse,c(3,3,3,4,4,4), c(h.pop, m.pop,l.pop,h.pop, m.pop,l.pop))
	colnames(param.rmse)<-c("b0","b1","b0.err","b1.err", "J","pop.prev")
	param.rmse<-as.data.frame(param.rmse)

	param.est.tab<-NULL
	for(i in 1:6){
		param.est.tab<-rbind(param.est.tab,
		summary(sim.object.designed[[i]]$est.treat.ml[,2]))
	}
	rownames(param.est.tab)<-c("$J=3$, high", "$J=3$, mid", "$J=3$, low",
		"$J=4$, high", "$J=4$, mid", "$J=4$, low")

###### Table 2
param.est.tab<-param.est.tab[,2:5]
xtable(param.est.tab, digits=1)		
######

	param.est.tab.err<-NULL
	for(i in 1:6){
		param.est.tab.err<-rbind(param.est.tab.err,
		summary(sim.object.designed[[i]]$est.treat.ml.err[,2]))
	}
	rownames(param.est.tab.err)<-c("$J=3$, high", "$J=3$, mid", "$J=3$, low",
		"$J=4$, high", "$J=4$, mid", "$J=4$, low")
	param.est.tab.err<-param.est.tab.err[,2:5]
	xtable(param.est.tab.err, digits=1)		

	## b_1 coverage
	param.coverage<-param.coverage.err<-NULL
	for(i in 1:6){
		param.coverage<-rbind(param.coverage,
			apply(sim.object.designed[[i]]$cov.treat.ml, 2, mean, na.rm=T)
			)
		param.coverage.err<-rbind(param.coverage.err,
			apply(sim.object.designed[[i]]$cov.treat.ml.err, 2,mean,na.rm=T)
			)	
	}
	param.coverage<-cbind(param.coverage,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop))
	param.coverage.err<-cbind(param.coverage.err,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop))
	param.coverage<-cbind(param.coverage,c(3,3,3,4,4,4))
	param.coverage.err<-cbind(param.coverage.err,c(3,3,3,4,4,4))

	colnames(param.coverage)<-colnames(param.coverage.err)<-c("b0", "b1", "pop.prev", "J")
	param.coverage.err<-data.frame(param.coverage.err)
	param.coverage<-data.frame(param.coverage)


###### Figure 3 components
#pdf("ParamBiasDesignedError.pdf")
	par(family="Verdana")
	#3a
	plot(x=param.bias$pop.prev[param.bias$J==3], 
		y = param.bias$b1.err[param.bias$J==3],
		type="b", pch = 17, lwd=1.5, 
		xlab = "Prevalence of sensititive item",
		ylab = "Bias",
		main = expression(paste("Bias in ", hat("b")[1])),
		bty="n", xlim=c(.1,.7), ylim=c(-1,3))
	abline(h=0, lty=2, col=grey(0.6))
	lines(x=param.bias$pop.prev[param.bias$J==4], 
		y = param.bias$b1.err[param.bias$J==4],
		type="b", pch = 16, lwd=1.5)
	legend(x="bottomright", lwd=1.5, pch=c(17,16), 
		legend = c("J=3 w/ error", "J=4 w/ error"),
		bty="n")
#dev.off()
#embed_fonts("ParamBiasDesignedError.pdf", outfile="ParamBiasDesignedError.pdf")

#pdf("ParamCoverageDesignedError.pdf")
	par(family="Verdana")
	#3b
	plot(x=param.coverage.err$pop.prev[param.coverage$J==3], 
		y = param.coverage.err[param.coverage$J==3,"b1"], 
		xlab = "Prevalence of sensititive item",
		ylab = "Proportion",
		main = expression(paste("Nominal CI coverage rate for ", hat("b")[1])),
		type="b", lwd=1.5,
		bty="n", xlim=c(.1,.7), ylim=c(0,1), pch=17)
	abline(h=0.9, lty=2)
	#lines(x=param.coverage$pop.prev[param.coverage$J==4], 
	#	y = param.coverage$b1[param.coverage$J==4],
	#	type="b", pch = 1, lwd=1.5)
	lines(x=param.coverage.err$pop.prev[param.coverage.err$J==4], 
		y = param.coverage.err$b1[param.coverage.err$J==4],
		type="b", pch = 16, lwd=1.5)
	legend(x="bottomright", lwd=1.5, pch=c(17,16), 
		legend = c("J=3 w/ error", "J=4 w/ error"),
		bty="n")
#dev.off()
#embed_fonts("ParamCoverageDesignedError.pdf", outfile="ParamCoverageDesignedError.pdf")

######


#### population prevalence est, variance, coverage

###### Figure 7 #######
mat<-matrix(1:6, 3)
#pdf("PopEstDesigned.pdf")
par(family="Verdana",layout(mat))
for(i in 1:6){
	if(i %in% c(1,4)){
		plot(density(sim.object.designed[[i]]$est.pop.ols, adjust=3,na.rm=T),
			col=grey(0.5), bty="n",
			lty=2, lwd=2, ylim=c(0,12), xlim=c(0,1),
			xlab=expression(hat(pi)[Z^"*"]),
			main = paste("High prevalence, ", "J=",
				sim.object.designed[[i]]$J, sep="")
			)
		lines(density(sim.object.designed[[i]]$est.pop.ols.err,adjust=3,na.rm=T), 
			col=grey(0.5), lwd=2)
		lines(density(sim.object.designed[[i]]$est.pop.ml,adjust=3,na.rm=T),
			lwd=2, lty=2)
		lines(density(sim.object.designed[[i]]$est.pop.ml.err,adjust=3,na.rm=T),
			lwd=2)
		abline(v=h.pop, lwd=1.2, lty=3)
	}
	if(i %in% c(2,5)){
		plot(density(sim.object.designed[[i]]$est.pop.ols,adjust=3,na.rm=T),
			col=grey(0.5), bty="n",
			lty=2, lwd=2, ylim=c(0,12), xlim=c(0,1),
			xlab=expression(hat(pi)[Z^"*"]),
			main = paste("Medium prevalence, ", "J=",
				sim.object.designed[[i]]$J, sep="")
			)
		lines(density(sim.object.designed[[i]]$est.pop.ols.err,adjust=3,na.rm=T),
		 col=grey(0.5), lwd=2)
		lines(density(sim.object.designed[[i]]$est.pop.ml, adjust=3,na.rm=T),
			lwd=2, lty=2)
		lines(density(sim.object.designed[[i]]$est.pop.ml.err,adjust=3,na.rm=T),
			lwd=2)
		abline(v=m.pop, lwd=1.2, lty=3)
	}
	if(i %in% c(3,6)){
		plot(density(sim.object.designed[[i]]$est.pop.ols, adjust=3,na.rm=T),
			col=grey(0.5), bty="n",
			lty=2, lwd=2,ylim=c(0,12), xlim=c(0,1),
			xlab=expression(hat(pi)[Z^"*"]),
			main = paste("Low prevalence, ", "J=",
				sim.object.designed[[i]]$J, sep="")
			)
		lines(density(sim.object.designed[[i]]$est.pop.ols.err,adjust=3,na.rm=T), 
			col=grey(0.5), lwd=2)
		lines(density(sim.object.designed[[i]]$est.pop.ml,adjust=3,na.rm=T),
			lwd=2, lty=2)
		lines(density(sim.object.designed[[i]]$est.pop.ml.err,adjust=3,na.rm=T),
			lwd=2)
		abline(v=l.pop, lwd=1.2, lty=3)
	}
}
legend(x="bottomright",
	legend = c("ICT", #"J=3 ICT w/ error", 
		"DiM", #"J=3 DiM w/error",
		"ICT w/error", #"J=4 ICT w/error", 
		"DiM w/error"), #"J=4 DiM w/error"),,
	col= c(1,grey(0.5),1,grey(0.5)),
	lty= c(2,2,1,1), bty="n", lwd=1.5)
#dev.off()
#embed_fonts("PopEstDesigned.pdf", outfile="PopEstDesigned.pdf")

######

##### pop coverage
for(i in 1:6){
	sim.object.designed[[i]]$lb.pop.ols<- sim.object.designed[[i]]$est.pop.ols - sim.object.designed[[i]]$se.pop.ols*critical
	sim.object.designed[[i]]$ub.pop.ols<- sim.object.designed[[i]]$est.pop.ols + sim.object.designed[[i]]$se.pop.ols*critical
	sim.object.designed[[i]]$lb.pop.ols.err<- sim.object.designed[[i]]$est.pop.ols.err - sim.object.designed[[i]]$se.pop.ols.err*critical
	sim.object.designed[[i]]$ub.pop.ols.err<- sim.object.designed[[i]]$est.pop.ols.err + sim.object.designed[[i]]$se.pop.ols.err*critical
	sim.object.designed[[i]]$lb.pop.ml<- sim.object.designed[[i]]$est.pop.ml - sim.object.designed[[i]]$se.pop.ml*critical
	sim.object.designed[[i]]$ub.pop.ml<- sim.object.designed[[i]]$est.pop.ml + sim.object.designed[[i]]$se.pop.ml*critical
	sim.object.designed[[i]]$lb.pop.ml.err<- sim.object.designed[[i]]$est.pop.ml.err - sim.object.designed[[i]]$se.pop.ml.err*critical
	sim.object.designed[[i]]$ub.pop.ml.err<- sim.object.designed[[i]]$est.pop.ml.err + sim.object.designed[[i]]$se.pop.ml.err*critical
}

for(i in 1:6){
	if(i %in% c(1,4)){
		sim.object.designed[[i]]$cov.pop.ols<- ((sim.object.designed[[i]]$lb.pop.ols < h.pop) & (h.pop < sim.object.designed[[i]]$ub.pop.ols))
		sim.object.designed[[i]]$cov.pop.ols.err<- ((sim.object.designed[[i]]$lb.pop.ols.err < h.pop) & (h.pop < sim.object.designed[[i]]$ub.pop.ols.err))
		sim.object.designed[[i]]$cov.pop.ml<- ((sim.object.designed[[i]]$lb.pop.ml < h.pop) & (h.pop < sim.object.designed[[i]]$ub.pop.ml))
		sim.object.designed[[i]]$cov.pop.ml.err<- ((sim.object.designed[[i]]$lb.pop.ml.err < h.pop) & (h.pop < sim.object.designed[[i]]$ub.pop.ml.err))
	}
	if(i %in% c(2,5)){
		sim.object.designed[[i]]$cov.pop.ols<- ((sim.object.designed[[i]]$lb.pop.ols < m.pop) & (m.pop < sim.object.designed[[i]]$ub.pop.ols))
		sim.object.designed[[i]]$cov.pop.ols.err<- ((sim.object.designed[[i]]$lb.pop.ols.err < m.pop) & (m.pop < sim.object.designed[[i]]$ub.pop.ols.err))
		sim.object.designed[[i]]$cov.pop.ml<- ((sim.object.designed[[i]]$lb.pop.ml < m.pop) & (m.pop < sim.object.designed[[i]]$ub.pop.ml))
		sim.object.designed[[i]]$cov.pop.ml.err<- ((sim.object.designed[[i]]$lb.pop.ml.err < m.pop) & (m.pop < sim.object.designed[[i]]$ub.pop.ml.err))
	}
	if(i %in% c(3,6)){
		sim.object.designed[[i]]$cov.pop.ols<- ((sim.object.designed[[i]]$lb.pop.ols < l.pop) & (l.pop < sim.object.designed[[i]]$ub.pop.ols))
		sim.object.designed[[i]]$cov.pop.ols.err<- ((sim.object.designed[[i]]$lb.pop.ols.err < l.pop) & (l.pop < sim.object.designed[[i]]$ub.pop.ols.err))
		sim.object.designed[[i]]$cov.pop.ml<- ((sim.object.designed[[i]]$lb.pop.ml < l.pop) & (l.pop < sim.object.designed[[i]]$ub.pop.ml))
		sim.object.designed[[i]]$cov.pop.ml.err<- ((sim.object.designed[[i]]$lb.pop.ml.err < l.pop) & (l.pop < sim.object.designed[[i]]$ub.pop.ml.err))
	}
}

pop.coverage<-NULL
for(i in 1:6){
	pop.coverage<-rbind(pop.coverage,
		c(mean(sim.object.designed[[i]]$cov.pop.ols, na.rm=T),
			mean(sim.object.designed[[i]]$cov.pop.ols.err, na.rm=T),
			mean(sim.object.designed[[i]]$cov.pop.ml, na.rm=T),
			mean(sim.object.designed[[i]]$cov.pop.ml.err, na.rm=T)
			)
		)	
}
pop.coverage<-cbind(pop.coverage,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop),c(3,3,3,4,4,4))
colnames(pop.coverage)<-c("ols", "ols.err","ml","ml.err", "pop.prev","J")
pop.coverage<-as.data.frame(pop.coverage)

###### Figure 8
#pdf("PopCoverageDesigned.pdf")
#par(mfrow=c(1,2))
	par(family="Verdana", mfrow=c(1,2))
	plot(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ols[pop.coverage$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "Proportion",
		main = expression(paste("Nominal CI coverage rate for ", hat(pi)[Z^"*"], ", no error", sep="")), 
		type="b", lwd=1.5, col=grey(0.5), lty=2,
		bty="n", xlim=c(.1,.7), ylim=c(0,1), pch=2)
	abline(h=0.9, lty=3)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ml[pop.coverage$J==3],
		type="b", pch = 2, lwd=1.5)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ols[pop.coverage$J==4],
		type="b", pch = 1, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ml[pop.coverage$J==4],
		type="b", pch = 1, lwd=1.5)

legend(x=.45, y=.7,
	legend = c("J=3 ICT", #"J=3 ICT w/ error", 
		"J=3 DiM", #"J=3 DiM w/error",
		"J=4 ICT", #"J=4 ICT w/error", 
		"J=4 DiM"), #"J=4 DiM w/error"),
	pch = c(2,2,1,1),
	col= c(1,grey(0.5),1,grey(0.5)),
	lty= c(1,2,1,2), bty="n"
	)

	
	plot(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ols.err[pop.coverage$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "Proportion",
		main = expression(paste("Nominal CI coverage rate for ", hat(pi)[Z^"*"], ", 3% error", sep="")), 
		type="b", lwd=1.5, col=grey(0.5), lty=2,
		bty="n", xlim=c(.1,.7), ylim=c(0,1), pch=17)
	abline(h=0.9, lty=3)

	lines(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ml.err[pop.coverage$J==3],
		type="b", pch = 17, lwd=1.5)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ols.err[pop.coverage$J==4],
		type="b", pch = 16, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ml.err[pop.coverage$J==4],
		type="b", pch = 16, lwd=1.5)

legend(x="bottomleft",
	legend = c("J=3 ICT w/ error", 
		"J=3 DiM w/error",
		"J=4 ICT w/error", 
		"J=4 DiM w/error"),
	pch = c(17,17,16,16),
	col= c(1,grey(0.5),1,grey(0.5)),
	lty= c(1,2,1,2), bty="n"
	)
#dev.off()
#embed_fonts("PopCoverageDesigned.pdf", outfile="PopCoverageDesigned.pdf")
######

### Analyzing simuations generated using the Blair-Imai style lists
rm(sim.object.designed)
load("PAsimulations_BI.Rdata")
N<-sim.object.BI[[1]]$ssize
sims<-length(sim.object.BI[[1]]$count.in.max)
#### count in max category
in.top.cat<-in.top.cat.err<-NULL
for(i in 1:6){
	in.top.cat<-rbind(in.top.cat,
		c(mean(sim.object.BI[[i]]$count.in.max), 
			median(sim.object.BI[[i]]$count.in.max),
			sum(sim.object.BI[[i]]$count.in.max==0)
			)
		)
	in.top.cat.err<-rbind(in.top.cat.err,
		c(mean(sim.object.BI[[i]]$count.in.max.err), 
			median(sim.object.BI[[i]]$count.in.max.err),
			sum(sim.object.BI[[i]]$count.in.max.err==0)
			)
		)
}

in.top.cat<-cbind(in.top.cat,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop))
in.top.cat.err<-cbind(in.top.cat.err,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop))
in.top.cat<-cbind(in.top.cat,c(3,3,3,4,4,4))
in.top.cat.err<-cbind(in.top.cat.err,c(3,3,3,4,4,4))
in.top.cat.err<-cbind(in.top.cat.err,(in.top.cat.err[,2]-in.top.cat[,2])/in.top.cat.err[,2])

colnames(in.top.cat)<-c("mean","median","num0","pop.prev","J")
colnames(in.top.cat.err)<-c("mean","median","num0","pop.prev","J","pct.err")
in.top.cat<-data.frame(in.top.cat)
in.top.cat.err<-data.frame(in.top.cat.err)


#### convergence
convergence.pct<-NULL
for(i in 1:6){
	convergence.pct<-rbind(convergence.pct,
		c(100*(1-sum(is.na(sim.object.BI[[i]]$est.pop.ml))/sims), 
			100*(1-sum(is.na(sim.object.BI[[i]]$est.pop.ml.err))/sims)
		))
}
convergence.pct  #no real problems

#### b_1 param bias/RMSE/Coverage
param.bias<-NULL
for(i in 1:6){
	if(i %in% c(1,4)){
	param.bias<-rbind(param.bias,
		c(apply(sim.object.BI[[i]]$est.treat.ml, 2, mean, na.rm=T)-h.param, 
		  apply(sim.object.BI[[i]]$est.treat.ml.err, 2, mean, na.rm=T)-h.param
			)
		)
	}
	if(i %in% c(2,5)){
		param.bias<-rbind(param.bias,
		c(apply(sim.object.BI[[i]]$est.treat.ml, 2, mean, na.rm=T)-m.param, 
		  apply(sim.object.BI[[i]]$est.treat.ml.err, 2, mean, na.rm=T)-m.param
			)
		)
	}
	if(i %in% c(3,6)){
		param.bias<-rbind(param.bias,
		c(apply(sim.object.BI[[i]]$est.treat.ml, 2, mean, na.rm=T)-l.param, 
		  apply(sim.object.BI[[i]]$est.treat.ml.err, 2, mean, na.rm=T)-l.param
			)
		)
	}
}

# param rmse
param.var<-NULL
for(i in 1:6){
	if(i %in% c(1,4)){
	param.var<-rbind(param.var,
		c(apply(sim.object.BI[[i]]$est.treat.ml, 2, var, na.rm=T), 
		  apply(sim.object.BI[[i]]$est.treat.ml.err, 2, var, na.rm=T)
			)
		)
	}
	if(i %in% c(2,5)){
		param.var<-rbind(param.var,
		c(apply(sim.object.BI[[i]]$est.treat.ml, 2, var, na.rm=T), 
		  apply(sim.object.BI[[i]]$est.treat.ml.err, 2, var, na.rm=T)
			)
		)
	}
	if(i %in% c(3,6)){
		param.var<-rbind(param.var,
		c(apply(sim.object.BI[[i]]$est.treat.ml, 2, var, na.rm=T), 
		  apply(sim.object.BI[[i]]$est.treat.ml.err, 2, var, na.rm=T)
			)
		)
	}
}
param.rmse<-sqrt(param.bias^2+param.var)
param.bias<-cbind(param.bias,c(3,3,3,4,4,4), c(h.pop, m.pop,l.pop,h.pop, m.pop,l.pop))
colnames(param.bias)<-c("b0","b1","b0.err","b1.err", "J","pop.prev")
param.bias<-as.data.frame(param.bias)

param.rmse<-cbind(param.rmse,c(3,3,3,4,4,4), c(h.pop, m.pop,l.pop,h.pop, m.pop,l.pop))
colnames(param.rmse)<-c("b0","b1","b0.err","b1.err", "J","pop.prev")
param.rmse<-as.data.frame(param.rmse)

param.coverage<-param.coverage.err<-NULL
for(i in 1:6){
	param.coverage<-rbind(param.coverage,
		apply(sim.object.BI[[i]]$cov.treat.ml, 2, mean, na.rm=T)
		)
	param.coverage.err<-rbind(param.coverage.err,
		apply(sim.object.BI[[i]]$cov.treat.ml.err, 2,mean,na.rm=T)
		)	
}
param.coverage<-cbind(param.coverage,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop))
param.coverage.err<-cbind(param.coverage.err,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop))
param.coverage<-cbind(param.coverage,c(3,3,3,4,4,4))
param.coverage.err<-cbind(param.coverage.err,c(3,3,3,4,4,4))

colnames(param.coverage)<-colnames(param.coverage.err)<-c("b0", "b1", "pop.prev", "J")
param.coverage.err<-data.frame(param.coverage.err)
param.coverage<-data.frame(param.coverage)

#Figure 2
#pdf("b1biasBI.pdf")
par(family="Verdana")
	#2a
	plot(x=param.bias$pop.prev[param.bias$J==3], 
		y = param.bias[param.bias$J==3,"b1"], 
		xlab = "Prevalence of sensititive item",
		ylab = "Bias",
		main = expression(paste("Bias in ", hat("b")[1])),
		type="b", lwd=1.5,
		bty="n", xlim=c(.1,.7), ylim=c(-2,3), pch=2)
	abline(h=0, lty=2, col=grey(0.6))
	lines(x=param.bias$pop.prev[param.bias$J==3], 
		y = param.bias$b1.err[param.bias$J==3],
		type="b", pch = 17, lwd=1.5)
	lines(x=param.bias$pop.prev[param.bias$J==4][1:2], 
		y = param.bias$b1[param.bias$J==4][1:2],
		type="b", pch = 1, lwd=1.5)
	lines(x=param.bias$pop.prev[param.bias$J==4], 
		y = param.bias$b1.err[param.bias$J==4],
		type="b", pch = 16, lwd=1.5)
	legend(x="bottomright", lwd=1.5, pch=c(2,17,1,16), 
	legend = c("J=3", "J=3 w/ error", "J=4", "J=4 w/ error"),
	bty="n")
#dev.off()
#embed_fonts("b1biasBI.pdf", outfile="b1biasBI.pdf")

#pdf("b1rmseBI.pdf")
par(family="Verdana")
#2b
plot(x=param.rmse$pop.prev[param.rmse$J==3], 
		y = param.rmse[param.rmse$J==3,"b1"], 
		xlab = "Prevalence of sensititive item",
		ylab = "RMSE",
		main = expression(paste("RMSE in ", hat("b")[1])),
		type="b", lwd=1.5,
		bty="n", xlim=c(.1,.7), ylim=c(0,4), pch=2)
	lines(x=param.rmse$pop.prev[param.rmse$J==3], 
		y = param.rmse$b1.err[param.rmse$J==3],
		type="b", pch = 17, lwd=1.5)
	lines(x=param.rmse$pop.prev[param.rmse$J==4][1:2], 
		y = param.rmse$b1[param.rmse$J==4][1:2],
		type="b", pch = 1, lwd=1.5)
	lines(x=param.rmse$pop.prev[param.rmse$J==4], 
		y = param.rmse$b1.err[param.rmse$J==4],
		type="b", pch = 16, lwd=1.5)
	legend(x="bottomright", lwd=1.5, pch=c(2,17,1,16), 
	legend = c("J=3", "J=3 w/ error", "J=4", "J=4 w/ error"),
	bty="n")
#dev.off()
#embed_fonts("b1rmseBI.pdf", outfile="b1rmseBI.pdf")

#pdf("b1coverageBI.pdf")
par(family="Verdana")
#2c
plot(x=param.coverage$pop.prev[param.coverage$J==3], 
		y = param.coverage[param.coverage$J==3,"b1"], 
		xlab = "Prevalence of sensititive item",
		ylab = "Proportion",
		main = expression(paste("Nominal CI coverage rate for ", hat("b")[1])),
		type="b", lwd=1.5,
		bty="n", xlim=c(.1,.7), ylim=c(0,1), pch=2)
	abline(h=0.9, lty=2)
	lines(x=param.coverage.err$pop.prev[param.coverage$J==3], 
		y = param.coverage.err$b1[param.coverage$J==3],
		type="b", pch = 17, lwd=1.5)
	lines(x=param.coverage$pop.prev[param.coverage$J==4], 
		y = param.coverage$b1[param.coverage$J==4],
		type="b", pch = 1, lwd=1.5)
	lines(x=param.coverage.err$pop.prev[param.coverage.err$J==4], 
		y = param.coverage.err$b1[param.coverage.err$J==4],
		type="b", pch = 16, lwd=1.5)
legend(x="bottomright", lwd=1.5, pch=c(2,17,1,16), 
	legend = c("J=3", "J=3 w/ error", "J=4", "J=4 w/ error"),
	bty="n")
#embed_fonts("b1coverageBI.pdf", outfile="b1coverageBI.pdf")


# pop bias
pop.bias<-NULL
for(i in 1:6){
	if(i %in% c(1,4)){
	pop.bias<-rbind(pop.bias,
		c(mean(sim.object.BI[[i]]$est.pop.ols, na.rm=T)-h.pop ,
			mean(sim.object.BI[[i]]$est.pop.ols.err, na.rm=T)-h.pop,
			mean(sim.object.BI[[i]]$est.pop.ml, na.rm=T)-h.pop, 
			mean(sim.object.BI[[i]]$est.pop.ml.err, na.rm=T)-h.pop
			)
		)
	}
	if(i %in% c(2,5)){
		pop.bias<-rbind(pop.bias,
		c(mean(sim.object.BI[[i]]$est.pop.ols, na.rm=T)-m.pop,
			mean(sim.object.BI[[i]]$est.pop.ols.err, na.rm=T)-m.pop ,
			mean(sim.object.BI[[i]]$est.pop.ml, na.rm=T)-m.pop, 
			mean(sim.object.BI[[i]]$est.pop.ml.err, na.rm=T)-m.pop
			)
		)
	}
	if(i %in% c(3,6)){
		pop.bias<-rbind(pop.bias,
		c(mean(sim.object.BI[[i]]$est.pop.ols, na.rm=T)-l.pop,
			mean(sim.object.BI[[i]]$est.pop.ols.err, na.rm=T)-l.pop,
			mean(sim.object.BI[[i]]$est.pop.ml, na.rm=T)-l.pop , 
			mean(sim.object.BI[[i]]$est.pop.ml.err, na.rm=T)-l.pop 
			)
		)
	}
}



# pop rmse
pop.var<-NULL
for(i in 1:6){
	pop.var<-rbind(pop.var,
		c(var(sim.object.BI[[i]]$est.pop.ols, na.rm=T),
			var(sim.object.BI[[i]]$est.pop.ols.err, na.rm=T),
			var(sim.object.BI[[i]]$est.pop.ml, na.rm=T), 
			var(sim.object.BI[[i]]$est.pop.ml.err, na.rm=T)
			)
		)
}
pop.sd<-sqrt(pop.var)	
colnames(pop.var)<-colnames(pop.sd)<-c("ols", "ols.err","ml","ml.err")
rownames(pop.var)<-c("J=3, hi", "J=3, med", "J=3, lo", "J=4, hi", "J=4, med", "J=4, lo")

pop.rmse <- sqrt(pop.bias^2 + pop.var)
pop.rmse<- cbind(pop.rmse, c(3,3,3,4,4,4), c(h.pop, m.pop,l.pop,h.pop, m.pop,l.pop))
colnames(pop.rmse)[c(5,6)]<-c("J", "pop.prev")

pop.bias<-cbind(pop.bias, c(3,3,3,4,4,4), c(h.pop, m.pop,l.pop,h.pop, m.pop,l.pop))
colnames(pop.bias)<-c("ols", "ols.err","ml","ml.err", "J", "pop.prev")
rownames(pop.bias)<-c("J=3, hi", "J=3, med", "J=3, lo", "J=4, hi", "J=4, med", "J=4, lo")
pop.bias<-as.data.frame(pop.bias)
pop.rmse<-as.data.frame(pop.rmse)

##pop est coverage
for(i in 1:6){
	sim.object.BI[[i]]$lb.pop.ols<- sim.object.BI[[i]]$est.pop.ols - sim.object.BI[[i]]$se.pop.ols*critical
	sim.object.BI[[i]]$ub.pop.ols<- sim.object.BI[[i]]$est.pop.ols + sim.object.BI[[i]]$se.pop.ols*critical
	sim.object.BI[[i]]$lb.pop.ols.err<- sim.object.BI[[i]]$est.pop.ols.err - sim.object.BI[[i]]$se.pop.ols.err*critical
	sim.object.BI[[i]]$ub.pop.ols.err<- sim.object.BI[[i]]$est.pop.ols.err + sim.object.BI[[i]]$se.pop.ols.err*critical
	sim.object.BI[[i]]$lb.pop.ml<- sim.object.BI[[i]]$est.pop.ml - sim.object.BI[[i]]$se.pop.ml*critical
	sim.object.BI[[i]]$ub.pop.ml<- sim.object.BI[[i]]$est.pop.ml + sim.object.BI[[i]]$se.pop.ml*critical
	sim.object.BI[[i]]$lb.pop.ml.err<- sim.object.BI[[i]]$est.pop.ml.err - sim.object.BI[[i]]$se.pop.ml.err*critical
	sim.object.BI[[i]]$ub.pop.ml.err<- sim.object.BI[[i]]$est.pop.ml.err + sim.object.BI[[i]]$se.pop.ml.err*critical
}

for(i in 1:6){
	if(i %in% c(1,4)){
		sim.object.BI[[i]]$cov.pop.ols<- ((sim.object.BI[[i]]$lb.pop.ols < h.pop) & (h.pop < sim.object.BI[[i]]$ub.pop.ols))
		sim.object.BI[[i]]$cov.pop.ols.err<- ((sim.object.BI[[i]]$lb.pop.ols.err < h.pop) & (h.pop < sim.object.BI[[i]]$ub.pop.ols.err))
		sim.object.BI[[i]]$cov.pop.ml<- ((sim.object.BI[[i]]$lb.pop.ml < h.pop) & (h.pop < sim.object.BI[[i]]$ub.pop.ml))
		sim.object.BI[[i]]$cov.pop.ml.err<- ((sim.object.BI[[i]]$lb.pop.ml.err < h.pop) & (h.pop < sim.object.BI[[i]]$ub.pop.ml.err))
	}
	if(i %in% c(2,5)){
		sim.object.BI[[i]]$cov.pop.ols<- ((sim.object.BI[[i]]$lb.pop.ols < m.pop) & (m.pop < sim.object.BI[[i]]$ub.pop.ols))
		sim.object.BI[[i]]$cov.pop.ols.err<- ((sim.object.BI[[i]]$lb.pop.ols.err < m.pop) & (m.pop < sim.object.BI[[i]]$ub.pop.ols.err))
		sim.object.BI[[i]]$cov.pop.ml<- ((sim.object.BI[[i]]$lb.pop.ml < m.pop) & (m.pop < sim.object.BI[[i]]$ub.pop.ml))
		sim.object.BI[[i]]$cov.pop.ml.err<- ((sim.object.BI[[i]]$lb.pop.ml.err < m.pop) & (m.pop < sim.object.BI[[i]]$ub.pop.ml.err))
	}
	if(i %in% c(3,6)){
		sim.object.BI[[i]]$cov.pop.ols<- ((sim.object.BI[[i]]$lb.pop.ols < l.pop) & (l.pop < sim.object.BI[[i]]$ub.pop.ols))
		sim.object.BI[[i]]$cov.pop.ols.err<- ((sim.object.BI[[i]]$lb.pop.ols.err < l.pop) & (l.pop < sim.object.BI[[i]]$ub.pop.ols.err))
		sim.object.BI[[i]]$cov.pop.ml<- ((sim.object.BI[[i]]$lb.pop.ml < l.pop) & (l.pop < sim.object.BI[[i]]$ub.pop.ml))
		sim.object.BI[[i]]$cov.pop.ml.err<- ((sim.object.BI[[i]]$lb.pop.ml.err < l.pop) & (l.pop < sim.object.BI[[i]]$ub.pop.ml.err))
	}
}

pop.coverage<-NULL
for(i in 1:6){
	pop.coverage<-rbind(pop.coverage,
		c(mean(sim.object.BI[[i]]$cov.pop.ols, na.rm=T),
			mean(sim.object.BI[[i]]$cov.pop.ols.err, na.rm=T),
			mean(sim.object.BI[[i]]$cov.pop.ml, na.rm=T),
			mean(sim.object.BI[[i]]$cov.pop.ml.err, na.rm=T)
			)
		)	
}

pop.coverage<-cbind(pop.coverage,c(h.pop,m.pop,l.pop,h.pop,m.pop,l.pop),c(3,3,3,4,4,4))
colnames(pop.coverage)<-c("ols", "ols.err","ml","ml.err", "pop.prev","J")
pop.coverage<-as.data.frame(pop.coverage)
#open symbols = no error
#closed symbols = w/ error
#triangles = J=3 (pch 2 open; pch 17 closed)
#circles = J=4  (1 open, pch 16 closed)
#black, solid = ICT
#grey, lty=2 = DiM

## Figure 4
#pdf("popbiasBI_noerror.pdf")	
	par(family="Verdana")
	#4L
	plot(x=pop.bias$pop.prev[pop.bias$J==3], 
		y = pop.bias$ml[pop.bias$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "Bias",
		main = expression(paste("Bias in ", hat(pi)[Z^"*"],", no error", sep="")),
		type="b", lwd=1.5, bty="n", xlim=c(.1,.7), 
		ylim=c(-.15,.15), pch=2)
	abline(h=0, lty=3, col=grey(0.6), lwd=.75)
	lines(x=pop.bias$pop.prev[pop.bias$J==3], 
		y = pop.bias$ols[pop.bias$J==3],
		type="b", pch = 2, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.bias$pop.prev[pop.bias$J==4], 
		y = pop.bias$ml[pop.bias$J==4],
		type="b", pch = 1, lwd=1.5)
	lines(x=pop.bias$pop.prev[pop.bias$J==4], 
		y = pop.bias$ols[pop.bias$J==4],
		type="b", pch = 1, lwd=1.5, col=grey(0.5), lty=2)
	legend(x="bottomright",
		legend = c("J=3 ICT", 
		"J=3 DiM", 
		"J=4 ICT", 
		"J=4 DiM"), 
		pch = c(2,2,1,1),
		col= c(1,grey(0.5),1,grey(0.5)),
		lty= c(1,2,1,2), bty="n")
#dev.off()
#embed_fonts("popbiasBI_noerror.pdf", outfile="popbiasBI_noerror.pdf")


#pdf("popbiasBI.pdf")	
	par(family="Verdana")
	#4R
plot(x=pop.bias$pop.prev[pop.bias$J==3], 
		y = pop.bias$ml.err[pop.bias$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "Bias",
		main = expression(paste("Bias in ", hat(pi)[Z^"*"],", 3% error", sep="")),
		type="b", lwd=1.5, bty="n", xlim=c(.1,.7), 
		ylim=c(-.15,.15), pch=17)
	abline(h=0, lty=3, col=grey(0.6), lwd=.75)
	lines(x=pop.bias$pop.prev[pop.bias$J==3], 
		y = pop.bias$ols.err[pop.bias$J==3],
		type="b", pch = 17, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.bias$pop.prev[pop.bias$J==4], 
		y = pop.bias$ml.err[pop.bias$J==4],
		type="b", pch = 16, lwd=1.5)
	lines(x=pop.bias$pop.prev[pop.bias$J==4], 
		y = pop.bias$ols.err[pop.bias$J==4],
		type="b", pch = 16, lwd=1.5, col=grey(0.5), lty=2)
	legend(x="bottomright",
	legend = c("J=3 ICT w/ error",  
		"J=3 DiM w/error",
		"J=4 ICT w/error", 
		"J=4 DiM w/error"),
	pch = c(17,17,16,16),
	col= c(1,grey(0.5),1,grey(0.5)),
	lty= c(1,2,1,2), bty="n")
#dev.off()
#embed_fonts("popbiasBI.pdf", outfile="popbiasBI.pdf")

## Figure 5
#pdf("poprmseBI_noerror.pdf")	
	par(family="Verdana")
	#5L
plot(x=pop.rmse$pop.prev[pop.rmse$J==3], 
		y = pop.rmse$ml[pop.rmse$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "RMSE",
		main = expression(paste(hat(pi)[Z^"*"]," RMSE, no error", sep="")),
		type="b", lwd=1.5, bty="n", xlim=c(.1,.7), 
		ylim=c(0,.15), pch=2)
	lines(x=pop.rmse$pop.prev[pop.rmse$J==3], 
		y = pop.rmse$ols[pop.rmse$J==3],
		type="b", pch = 2, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.rmse$pop.prev[pop.rmse$J==4], 
		y = pop.rmse$ml[pop.rmse$J==4],
		type="b", pch = 1, lwd=1.5)
	lines(x=pop.rmse$pop.prev[pop.rmse$J==4], 
		y = pop.rmse$ols[pop.rmse$J==4],
		type="b", pch = 1, lwd=1.5, col=grey(0.5), lty=2)
	legend(x="bottomright",
		legend = c("J=3 ICT", 
		"J=3 DiM", 
		"J=4 ICT", 
		"J=4 DiM"), 
		pch = c(2,2,1,1),
		col= c(1,grey(0.5),1,grey(0.5)),
		lty= c(1,2,1,2), bty="n")
#dev.off()
#embed_fonts("poprmseBI_noerror.pdf", outfile="poprmseBI_noerror.pdf")

#pdf("poprmseBI.pdf")	
	par(family="Verdana")
	#5R
plot(x=pop.rmse$pop.prev[pop.rmse$J==3], 
		y = pop.rmse$ml.err[pop.rmse$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "RMSE",
		main = expression(paste(hat(pi)[Z^"*"]," RMSE, 3% error", sep="")),
		type="b", lwd=1.5, bty="n", xlim=c(.1,.7), 
		ylim=c(0,.15), pch=17)
	lines(x=pop.rmse$pop.prev[pop.rmse$J==3], 
		y = pop.rmse$ols.err[pop.rmse$J==3],
		type="b", pch = 17, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.rmse$pop.prev[pop.rmse$J==4], 
		y = pop.rmse$ml.err[pop.rmse$J==4],
		type="b", pch = 16, lwd=1.5)
	lines(x=pop.rmse$pop.prev[pop.rmse$J==4], 
		y = pop.rmse$ols.err[pop.rmse$J==4],
		type="b", pch = 16, lwd=1.5, col=grey(0.5), lty=2)
	legend(x="bottomright",
	legend = c("J=3 ICT w/ error",  
		"J=3 DiM w/error",
		"J=4 ICT w/error", 
		"J=4 DiM w/error"),
	pch = c(17,17,16,16),
	col= c(1,grey(0.5),1,grey(0.5)),
	lty= c(1,2,1,2), bty="n")
#dev.off()
#embed_fonts("poprmseBI.pdf", outfile="poprmseBI.pdf")

## Figure 6
#pdf("popcoverageBI_noerror.pdf")	
	par(family="Verdana")
	#6L
plot(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ols[pop.coverage$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "Proportion",
		main = expression(paste("Nominal CI coverage rate for ", hat(pi)[Z^"*"], ", no error", sep="")), 
		type="b", lwd=1.5, col=grey(0.5), lty=2,
		bty="n", xlim=c(.1,.7), ylim=c(0,1), pch=2)
	abline(h=0.9, lty=3, col=grey(0.6))
	lines(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ml[pop.coverage$J==3],
		type="b", pch = 2, lwd=1.5)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ols[pop.coverage$J==4],
		type="b", pch = 1, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ml[pop.coverage$J==4],
		type="b", pch = 1, lwd=1.5)
	legend(x="bottomright",
		legend = c("J=3 ICT", 
		"J=3 DiM", 
		"J=4 ICT", 
		"J=4 DiM"), 
		pch = c(2,2,1,1),
		col= c(1,grey(0.5),1,grey(0.5)),
		lty= c(1,2,1,2), bty="n")
#dev.off()
#embed_fonts("popcoverageBI_noerror.pdf", outfile="popcoverageBI_noerror.pdf")

#pdf("popcoverageBI.pdf")	
	par(family="Verdana")
	#6R
plot(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ols.err[pop.coverage$J==3], 
		xlab = "Prevalence of sensititive item",
		ylab = "Proportion",
		main = expression(paste("Nominal CI coverage rate for ", hat(pi)[Z^"*"], ", 3% error", sep="")), 
		type="b", lwd=1.5, col=grey(0.5), lty=2,
		bty="n", xlim=c(.1,.7), ylim=c(0,1), pch=17)
	abline(h=0.9, lty=3, col=grey(0.6))
	lines(x=pop.coverage$pop.prev[pop.coverage$J==3], 
		y = pop.coverage$ml.err[pop.coverage$J==3],
		type="b", pch = 17, lwd=1.5)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ols.err[pop.coverage$J==4],
		type="b", pch = 16, lwd=1.5, col=grey(0.5), lty=2)
	lines(x=pop.coverage$pop.prev[pop.coverage$J==4], 
		y = pop.coverage$ml.err[pop.coverage$J==4],
		type="b", pch = 16, lwd=1.5)

legend(x="bottomright",
	legend = c("J=3 ICT w/ error",  
		"J=3 DiM w/error",
		"J=4 ICT w/error", 
		"J=4 DiM w/error"),
	pch = c(17,17,16,16),
	col= c(1,grey(0.5),1,grey(0.5)),
	lty= c(1,2,1,2), bty="n")
#dev.off()
#embed_fonts("popcoverageBI.pdf", outfile="popcoverageBI.pdf")

##### AMJ example
w2.data <- read.spss(file="../AMJ_wave_2.sav",
                  to.data.frame=TRUE)

###merging together responses across randomized question ordering.
QA<-w2.data$Qa
QA[is.na(w2.data$Qa)]<-w2.data$Qa2[is.na(w2.data$Qa)]
QA<-as.numeric(QA)-1 #Qa was a factor...
QB<-w2.data$Qb
QB[is.na(w2.data$Qb)]<-w2.data$Qb2[is.na(w2.data$Qb)]
QB<-as.numeric(QB)-1
QC<-w2.data$Qc
QC[is.na(w2.data$Qc)]<-w2.data$Qc2[is.na(w2.data$Qc)]
QC<-as.numeric(QC)-1
QD<-w2.data$Qd
QD[is.na(w2.data$Qd)]<-w2.data$Qd2[is.na(w2.data$Qd)]
QD<-as.numeric(QD)-1
vf.treat<-w2.data$group=="Group 1 - 5A, 4B, 4C, 5D"
vb.treat<- 1-vf.treat

w2.data<-cbind(w2.data,vf.treat,vb.treat, QA, QB, QC, QD)

w2.data$white<-0
w2.data$white[w2.data$race=="White"]<-1
w2.data$female<-0
w2.data$female[w2.data$gender=="Female"]<-1
w2.data$age<-w2.data$birthyr-min(w2.data$birthyr)
w2.data$age2<-w2.data$age^2

#Diff-in-means
vf.fit<-lm(QA~vf.treat)
fraud2<-coef(vf.fit)[2]
vf.se<-sqrt(diag(vcovHC(vf.fit)))
n<-length(resid(vf.fit))
CI.vf<-c(fraud2 - qt(0.975,df=(n-2))*vf.se[2],
         fraud2 + qt(0.975,df=(n-2))*vf.se[2])

alien.fit<-lm(QD~vf.treat, data=w2.data)
alien.se<-sqrt(diag(vcovHC(alien.fit)))
alien<-coef(alien.fit)[2]
n<-length(resid(alien.fit))
CI.alien<-c(alien - qt(0.975,df=(n-2))*alien.se[2],
         alien + qt(0.975,df=(n-2))*alien.se[2])

sms.fit<-lm(QC~vb.treat)
sms.se<-sqrt(diag(vcovHC(sms.fit)))
sms<-coef(sms.fit)[2]
n<-length(resid(sms.fit))
CI.sms<-c(sms - qt(0.975,df=(n-2))*sms.se[2],
         sms + qt(0.975,df=(n-2))*sms.se[2])

ICTw2.fraud<-ictreg(QA~ age + white + female, treat = "vf.treat", J=4, data= w2.data)
p.w2.fraud<-predict(ICTw2.fraud, se.fit=T, avg=T)
ICTw2.alien<-ictreg(QD~age + white + female, treat = "vf.treat", J=4, data= w2.data)
p.w2.alien<-predict(ICTw2.alien, se.fit=T, avg=T)
ICTw2.sms<-ictreg(QC~age + white + female, treat = "vb.treat", J=4, data= w2.data)
p.w2.sms<-predict(ICTw2.sms, se.fit=T, avg=T)

plot.data<-data.frame(
                     ques=c("impersonation", "impersonation",
                      "alien abduction", "alien abduction", "SMS", "SMS"),
                     ques2=c("impersonation \n(DiM)", "impersonation \n(ICT)",
                      "abduction \n (DiM)", "abduction \n (ICT)",
                      "SMS \n(DiM)", "SMS \n(ICT)"),
                     meth=c("DIM", "ICT", "DIM", "ICT", "DIM", "ICT"),
                     y=c(fraud2, p.w2.fraud$fit , alien, p.w2.alien$fit, sms, p.w2.sms$fit),
                     lower=c(CI.vf[1], p.w2.fraud$fit-2*p.w2.fraud$se.fit, 
                     	CI.alien[1], p.w2.alien$fit-2*p.w2.alien$se.fit,
                     	CI.sms[1], p.w2.sms$fit-2*p.w2.sms$se.fit),
                     upper=c(CI.vf[2], p.w2.fraud$fit+2*p.w2.fraud$se.fit,
                      CI.alien[2],p.w2.alien$fit+2*p.w2.alien$se.fit,
                      CI.sms[2], p.w2.sms$fit+2*p.w2.sms$se.fit)
                     )
mn<-min(plot.data$lower)
mx<-max(plot.data$upper)
steps <- floor(mn):ceiling(mx)
num<-length(plot.data$ques2)

#Figure 9
#pdf("wave2compared.pdf", family="Verdana")
par(mar=c(5.1, 8.1, 4.1, 2.1))
plot(0, 0, xlim = c(mn, mx), ylim = c(1, num), 
        xlab = "estimated prevalence (proportion)",
        main = "Results from 3 List Experiments", ylab = "", 
        axes = FALSE, type = "n", family="Verdana")

axis(side = 2, at = 1:length(plot.data$ques2),
	labels = plot.data$ques2, 
	las = 2, tck = 0, lty = 0, , family="Verdana")
axis(side = 1, las = 0, lty = 0, lwd=2, line=.5, family="Verdana")
    zeros <- rep(0, length(steps))
    ends <- rep((num + 1), length(steps))
segments(steps, zeros, steps, ends, col = "gray", lwd = 1)
segments(0, 0, 0, num + 1, lwd = 4, col = "grey75")
segments(plot.data$lower[c(1,3,5)],c(1,3,5),plot.data$upper[c(1,3,5)],c(1,3,5), lwd=2, lty=2)
segments(plot.data$lower[c(2,4,6)],c(2,4,6),plot.data$upper[c(2,4,6)],c(2,4,6), lwd=2)
points(plot.data$y,1:num,pch=16,cex=1.5)
#dev.off()
#embed_fonts("wave2compared.pdf", outfile="wave2compared.pdf")

#END