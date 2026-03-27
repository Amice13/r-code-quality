library(xtable)

###Figures 1 and 2

###Monte Carlo DGP:

set.seed(1381)

alpha2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) ##Going to test models with these parameters
bigr <- bigr2 <- matrix(0, nrow=4, ncol=length(alpha2)) ##Storing model outputs

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

###Set other model parameters
phi <- 0.75
rho <- 0.95
alpha <- alpha2[q]
beta <- 0.5

###Generate Monte Carlo TS Data
u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

###Prepare time series and lagged variable data
ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

###Test different model specifications:
reg <- summary(lm(ynl~0+ylag1+ylag2+xn+xlag))

res[d] <- reg$coef[3,1]

reg <- summary(lm(ynl~0+ylag1+ylag2+xn))

res2[d] <- reg$coef[3,1]

reg <- summary(lm(ynl~0+ylag1+xn))

res3[d] <- reg$coef[2,1]

reg <- summary(lm(ynl~0+xn))

res4[d] <- reg$coef[1,1]


}

bigr[1,q] <- (mean(res)-beta)/beta
bigr[2,q] <- (mean(res2)-beta)/beta
bigr[3,q] <- (mean(res3)-beta)/beta
bigr[4,q] <- (mean(res4)-beta)/beta


bigr2[1,q] <- sqrt(mean( (res-beta)^2 ))
bigr2[2,q] <- sqrt(mean( (res2-beta)^2 ))
bigr2[3,q] <- sqrt(mean( (res3-beta)^2 ))
bigr2[4,q] <- sqrt(mean( (res4-beta)^2 ))
}

###Make Figure 1:
##Graph Results:
txt <- c("EQ4", "LGDV2", "LGDV", "REG")
col <- c("black", "black", "blue", "black")
pts <- c(1, 20, 17, 15)
for(i in 1:nrow(bigr)){
	id <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
	if(i==1) plot(id, 100*bigr[i,], type="l", lty=i, ylim=c(-75, 150), xlab=substitute(alpha), ylab=substitute(paste("Percent Bias in Estimated ", beta)), main="", col=col[i], las=1)
	if(i>1) lines(id, 100*bigr[i,], lty=i, col=col[i])
	points(id, 100*bigr[i,], pch=pts[i], col=col[i])
	
	if(i==1) text(id, 100*bigr[i,], round(100*bigr[i,], 2), pos=1, cex=0.5, col=col[i], font=3)
	if(i==3) text(id, 100*bigr[i,], round(100*bigr[i,], 2), pos=1, cex=0.5, col=col[i])
	if(i==2 | i==4) text(id, 100*bigr[i,], round(100*bigr[i,], 2), pos=3, cex=0.5, col=col[i])
	
	if(i != 3) text(0.25, (100*bigr[i,3]+100*bigr[i,4])/2, txt[i], col=col[i])
	if(i==3) text(0.35, (100*bigr[i,4]+100*bigr[i,5])/2, txt[i], col=col[i])
	
	if(i==1) legend("topright", legend=txt, pch=pts, lty=1:nrow(bigr), col=col, text.col=c("black", "black", "blue", "black"))
}


##Make Figure 2:
txt <- c("EQ4", "LGDV2", "LGDV", "REG")
pts <- c(1, 20, 17, 15)
col <- c("black", "black", "blue", "black")
for(i in 1:nrow(bigr2)){
	id <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
	if(i==1) plot(id, bigr2[i,], type="l", lty=i, ylim=c(0, 0.65), xlab=substitute(alpha), ylab=substitute(paste("Root Mean Square Error in Estimated ", beta)), main="", col=col[i], las=1)
	if(i>1) lines(id, bigr2[i,], lty=i, col=col[i])
	points(id, bigr2[i,], pch=pts[i], col=col[i])
	
	if(i==1) text(id, bigr2[i,], round(bigr2[i,], 2), pos=1, cex=0.5, col=col[i], font=3)
	
	if(i==2 | i==4) text(id, bigr2[i,], round(bigr2[i,], 2), pos=1, cex=0.5, col=col[i])
	if(i==3) text(id, bigr2[i,], round(bigr2[i,], 2), pos=3, cex=0.5, col=col[i])
	
	if(i == 3) text(0.35, (bigr2[i,4]+bigr2[i,5])/2, txt[i], lwd=2, col=col[i])
	if(i != 3) text(0.25, (bigr2[i,3]+bigr2[i,4])/2, txt[i], lwd=2, col=col[i])
	
	if(i==1) legend("topright", legend=txt, pch=pts, lty=1:nrow(bigr2), col=col, text.col=c("black", "black", "blue", "black"))
}




##Figures 3 and 4:

###Monte Carlo DGP:

set.seed(143)

phi2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
bigr <- bigr2 <- matrix(0, nrow=4, ncol=length(phi2))

for(q in 1:length(phi2)){
res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

phi <- phi2[q]
rho <- 0.95
alpha <- 0.75
beta <- 0.5

u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

reg <- summary(lm(ynl~0+ylag1+ylag2+xn+xlag))

res[d] <- reg$coef[3,1]

reg <- summary(lm(ynl~0+ylag1+ylag2+xn))

res2[d] <- reg$coef[3,1]

reg <- summary(lm(ynl~0+ylag1+xn))

res3[d] <- reg$coef[2,1]

reg <- summary(lm(ynl~0+xn))

res4[d] <- reg$coef[1,1]


}

bigr[1,q] <- (mean(res)-beta)/beta
bigr[2,q] <- (mean(res2)-beta)/beta
bigr[3,q] <- (mean(res3)-beta)/beta
bigr[4,q] <- (mean(res4)-beta)/beta

bigr2[1,q] <- sqrt(mean( (res-beta)^2 ))
bigr2[2,q] <- sqrt(mean( (res2-beta)^2 ))
bigr2[3,q] <- sqrt(mean( (res3-beta)^2 ))
bigr2[4,q] <- sqrt(mean( (res4-beta)^2 ))
}


##Graph Results:

##Make Figure 3:
xt <- c("EQ4", "LGDV2", "LGDV", "REG")
pts <- c(1, 20, 17, 15)
col <- c("black", "black", "blue", "black")
#col <- "gray"
for(i in 1:nrow(bigr)){
	id <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
	
	vals <- bigr[i,]
	if(i==4) vals <- vals-2
	
	if(i==1) plot(id, 100*vals, type="l", lty=i, ylim=c(-35, 25), xlim=c(-0.01, 0.514), xlab=substitute(phi), ylab=substitute(paste("Percent Bias in Estimated ", beta)), main="", col=col[i], las=1)
	if(i>1) lines(id, 100*vals, lty=i, col=col[i])
	points(id, 100*vals, pch=pts[i], col=col[i])
	
	if(i==1) text(id, 100*vals, round(100*vals, 2), pos=c(1,3,1,3,1,3), cex=0.5, col=col[i], font=3)
	if(i==2) text(id, 100*vals, round(100*vals, 2), pos=2, cex=0.5, col=col[i])
	if(i==3) text(id, 100*vals, round(100*vals, 2), pos=4, cex=0.5, col=col[i])
	if(i==4) text(id, 100*vals, round(100*vals, 2)+200, pos=1, cex=0.5, col=col[i])
	text(0.25, (100*vals[3]+100*vals[4])/2, txt[i], col=col[i])
	
	if(i==1) legend("bottomleft", legend=txt, pch=pts, lty=1:nrow(bigr), text.col=c("black", "black", "blue", "black"), col=col)
}



##Make Figure 4:
xt <- c("EQ4", "LGDV2", "LGDV", "REG")
pts <- c(1, 20, 17, 15)
col <- c("black", "black", "blue", "black")
#col <- "gray"
for(i in 1:nrow(bigr)){
	id <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
	
	vals <- bigr2[i,]
	if(i==4) vals <- vals-0.9
	
	if(i==1) plot(id, vals, type="l", lty=i, ylim=c(0, 0.25), xlim=c(-0.01, 0.514), xlab=substitute(phi), ylab=substitute(paste("Root Mean Square Error in Estimated ", beta)), main="", col=col[i], las=1)
	if(i>1) lines(id, vals, lty=i, col=col[i])
	points(id, vals, pch=pts[i], col=col[i])
	
	if(i==1) text(id, vals, round(vals, 2), pos=1, cex=0.5, col=col[i], font=3)
	if(i==2) text(id, vals, round(vals, 2), pos=3, cex=0.5, col=col[i])
	if(i==3) text(id, vals, round(vals, 2), pos=c(1,1,1,1,3,1), cex=0.5, col=col[i])
	if(i==4) text(id, vals, round(vals, 2)+0.9, pos=1, cex=0.5, col=col[i])
	text(0.25, (vals[3]+vals[4])/2, txt[i], col=col[i])
	
	if(i==1) legend("bottomleft", legend=txt, pch=pts, lty=1:nrow(bigr), text.col=c("black", "black", "blue", "black"), col=col)
}








#####Autocorrelation

###Ljung-Box Test
box.test <- function(resid, lags){
	n <- length(resid)
	rho <- as.vector(acf(reg$residuals, lag.max=lags, plot=F)$acf)[-1]
	rho <- rho^2/(n - seq(1, lags, 1))
	sum(rho)*n*(n+2)
}


##Test Autocorrelation across more values of phi and alpha:

set.seed(1346)

B <- A <- c(seq(0, 0.7, .1), 0.75)
Bias1 <- Bias2 <- Bias3 <- Bias4 <- matrix(0, nrow=length(A), ncol=length(A))


for(q in 1:length(A)){

for(r in 1:length(B)){
	rres <- rres2 <- res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

phi <- A[q]
rho <- 0.95
alpha <- B[r]
beta <- 0.5

u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

reg <- summary(lm(ynl~0+ylag1+ylag2+xn+xlag))

res[d] <- box.test(reg$residuals, lag=12)
rres[d] <- reg$coef[3,1]

reg <- summary(lm(ynl~0+ylag1+xn+xlag))

res2[d] <- box.test(reg$residuals, lag=12)
rres2[d] <- reg$coef[2,1]



}


Bias1[q,r] <- mean( res > qchisq(.95, 10) )
Bias2[q,r] <- mean( res2 > qchisq(.95, 11) )

Bias3[q,r] <- paste("[", paste( round(quantile(1-pchisq(res, 10), c(.25, .75)), 3), sep="", collapse=", " ), "]", sep="") ##25th and 75th percentiles of p-values for autocorrelation test

Bias4[q,r] <- paste("[", paste( round(quantile(1-pchisq(res2, 11), c(.25, .75)), 3), sep="", collapse=", " ), "]", sep="") ##25th and 75th percentiles of p-values for autocorrelation test

}
}

##Print comment "Table 1"
cat("\\clearpage Table 1\n")

###Generate Table 1 (LaTeX Output)
print(xtable(round(100*Bias1, 2)))

##View Table 1 (R Output)
#round(100*Bias1, 2)

##Print comment "Table 2"
cat("\\clearpage Table 2\n")

###Generate Table 2 (LaTeX Output)
print(xtable(round(100*Bias2, 2)))

##View Table 2 (R Output)
#round(100*Bias2, 2)


###Generate Figure 5:
contour(c(seq(0, 0.7, 0.1), 0.75), c(seq(0, 0.7, 0.1), 0.75), Bias2, xlab = expression(paste("Value of ", phi)), ylab=expression(paste("Value of ", alpha)), nlevels = 25, las=1)











###Figure 6:
beta <- 0.5
rho <- 0.95

B <- A <- seq(0, 0.9, .01)
Bias <- matrix(0, nrow=length(A), ncol=length(A))

for(i in 1:length(A)){
	for(j in 1:length(B)){
		alpha <- A[i]
		phi <- B[j]
		beta1 <- beta
		beta2 <- -beta*phi
		alpha1 <- alpha+phi
		alpha2 <- -alpha*phi
		beta3 <- beta1*rho + beta2
		
		a <- 1/(1-rho^2)
		b <- rho*a
		c <- (beta1*a + beta2*b)/(1-alpha1*rho - alpha2*rho^2)
		
		z <- alpha2*beta3*rho*c + beta3*alpha1*c + beta3*alpha2*rho*c + beta3^2*a + (beta1^2 + 1)
		f <- ( alpha1*beta3*c*(1+alpha2) + z*(1-alpha2) )/(1 - alpha2 - alpha2^2 + alpha2^3 - alpha1^2 - alpha2*alpha1^2)
		d <- (alpha1*f + beta3*c)/(1-alpha2)
		
		Bias[i,j] <- ( rho*c*(alpha+phi)/a + rho^2*c*(-alpha*phi)/a + rho*(-beta*phi) )/beta1  ##REG
		
	}
}

contour(A, B, 100*Bias, xlab = expression(paste("Value of ", alpha)), ylab=expression(paste("Value of ", phi)), nlevels = 25, las=1)



###Figure 7:
##Bias REG, varying rho:
beta <- 0.5
phi <- 0.75

B <- A <- seq(0, 0.9, .01)
Bias <- matrix(0, nrow=length(A), ncol=length(A))

for(i in 1:length(A)){
	for(j in 1:length(B)){
		alpha <- A[i]
		rho <- B[j]
		beta1 <- beta
		beta2 <- -beta*phi
		alpha1 <- alpha+phi
		alpha2 <- -alpha*phi
		beta3 <- beta1*rho + beta2
		
		a <- 1/(1-rho^2)
		b <- rho*a
		c <- (beta1*a + beta2*b)/(1-alpha1*rho - alpha2*rho^2)
		
		z <- alpha2*beta3*rho*c + beta3*alpha1*c + beta3*alpha2*rho*c + beta3^2*a + (beta1^2 + 1)
		f <- ( alpha1*beta3*c*(1+alpha2) + z*(1-alpha2) )/(1 - alpha2 - alpha2^2 + alpha2^3 - alpha1^2 - alpha2*alpha1^2)
		d <- (alpha1*f + beta3*c)/(1-alpha2)
		
		Bias[i,j] <- ( rho*c*(alpha+phi)/a + rho^2*c*(-alpha*phi)/a + rho*(-beta*phi) )/beta1
	}
}

contour(A, B, 100*Bias, xlab = expression(paste("Value of ", alpha)), ylab=expression(paste("Value of ", rho)), nlevels = 35, las=1)


###Figure 8:
beta <- 0.5
rho <- 0.95

B <- A <- seq(0, 0.9, .01)
Bias <- matrix(0, nrow=length(A), ncol=length(A))

for(i in 1:length(A)){
	for(j in 1:length(B)){
		alpha <- A[i]
		phi <- B[j]
		beta1 <- beta
		beta2 <- -beta*phi
		alpha1 <- alpha+phi
		alpha2 <- -alpha*phi
		beta3 <- beta1*rho + beta2
		
		a <- 1/(1-rho^2)
		b <- rho*a
		c <- (beta1*a + beta2*b)/(1-alpha1*rho - alpha2*rho^2)
		
		z <- alpha2*beta3*rho*c + beta3*alpha1*c + beta3*alpha2*rho*c + beta3^2*a + (beta1^2 + 1)
		f <- ( alpha1*beta3*c*(1+alpha2) + z*(1-alpha2) )/(1 - alpha2 - alpha2^2 + alpha2^3 - alpha1^2 - alpha2*alpha1^2)
		d <- (alpha1*f + beta3*c)/(1-alpha2)
		
		Bias[i,j] <- ( -beta*phi*( (2*rho^2*c^2*d - rho*c^2*f - rho^3*c^2*f + rho*a*f^2 - rho*a*d^2)/(a*f^2 + 2*rho^3*c^2*d - rho^4*c^2*f - rho^2*c^2*f - a*d^2) )  )/beta1 ##LGDV2
		
	}
}

contour(A, B, 100*Bias, xlab = expression(paste("Value of ", alpha)), ylab=expression(paste("Value of ", phi)), nlevels = 25, las=1)


###Figure 9:

##Bias of LGDV2, varying rho:
beta <- 0.5
alpha <- 0.75

B <- A <- seq(0, 0.9, .01)
Bias <- matrix(0, nrow=length(A), ncol=length(A))

for(i in 1:length(A)){
	for(j in 1:length(B)){
		phi <- A[i]
		rho <- B[j]
		beta1 <- beta
		beta2 <- -beta*phi
		alpha1 <- alpha+phi
		alpha2 <- -alpha*phi
		beta3 <- beta1*rho + beta2
		
		a <- 1/(1-rho^2)
		b <- rho*a
		c <- (beta1*a + beta2*b)/(1-alpha1*rho - alpha2*rho^2)
		
		z <- alpha2*beta3*rho*c + beta3*alpha1*c + beta3*alpha2*rho*c + beta3^2*a + (beta1^2 + 1)
		f <- ( alpha1*beta3*c*(1+alpha2) + z*(1-alpha2) )/(1 - alpha2 - alpha2^2 + alpha2^3 - alpha1^2 - alpha2*alpha1^2)
		d <- (alpha1*f + beta3*c)/(1-alpha2)
		
		Bias[i,j] <- ( -beta*phi*( (2*rho^2*c^2*d - rho*c^2*f - rho^3*c^2*f + rho*a*f^2 - rho*a*d^2)/(a*f^2 + 2*rho^3*c^2*d - rho^4*c^2*f - rho^2*c^2*f - a*d^2) )  )/beta1 
		
	}
}

contour(A, B, 100*Bias, xlab = expression(paste("Value of ", phi)), ylab=expression(paste("Value of ", rho)), nlevels = 25, las=1)




###Figure 10:
beta <- 0.5
rho <- 0.95

B <- A <- seq(0, 0.9, .01)
Bias <- matrix(0, nrow=length(A), ncol=length(A))

for(i in 1:length(A)){
	for(j in 1:length(B)){
		alpha <- A[i]
		phi <- B[j]
		beta1 <- beta
		beta2 <- -beta*phi
		alpha1 <- alpha+phi
		alpha2 <- -alpha*phi
		beta3 <- beta1*rho + beta2
		
		a <- 1/(1-rho^2)
		b <- rho*a
		c <- (beta1*a + beta2*b)/(1-alpha1*rho - alpha2*rho^2)
		
		z <- alpha2*beta3*rho*c + beta3*alpha1*c + beta3*alpha2*rho*c + beta3^2*a + (beta1^2 + 1)
		f <- ( alpha1*beta3*c*(1+alpha2) + z*(1-alpha2) )/(1 - alpha2 - alpha2^2 + alpha2^3 - alpha1^2 - alpha2*alpha1^2)
		d <- (alpha1*f + beta3*c)/(1-alpha2)
		
		Bias[i,j] <- ( -beta*phi*( (2*rho^2*c^2*d - rho*c^2*f - rho^3*c^2*f + rho*a*f^2 - rho*a*d^2)/(a*f^2 + 2*rho^3*c^2*d - rho^4*c^2*f - rho^2*c^2*f - a*d^2) )  )/beta1 ##LGDV2
		
		Bias[i,j] <- ( rho*c*(alpha+phi)/a + rho^2*c*(-alpha*phi)/a + rho*(-beta*phi) )/beta1  ##REG
		
		Bias[i,j] <- ( -beta*phi * (f*b - rho*c^2)/(a*f-rho^2*c^2) + (f*rho^2*c - rho*c*d)/(a*f-rho^2*c^2) * (-alpha*phi) )/beta1 ##LGDV
		
	}
}

contour(A, B, 100*Bias, xlab = expression(paste("Value of ", alpha)), ylab=expression(paste("Value of ", phi)), nlevels = 25, las=1)




###Figure 11:
##Bias of LGDV, varying rho:

alphavalues <- c(0, 0.2, 0.4, 0.6)
par(mfrow=c(2,2))
for(i in 1:length(alphavalues)){
beta <- 0.5
alpha <- alphavalues[i]

B <- A <- seq(0, 0.9, .01)
Bias <- matrix(0, nrow=length(A), ncol=length(A))

for(i in 1:length(A)){
	for(j in 1:length(B)){
		phi <- A[i]
		rho <- B[j]
		beta1 <- beta
		beta2 <- -beta*phi
		alpha1 <- alpha+phi
		alpha2 <- -alpha*phi
		beta3 <- beta1*rho + beta2
		
		a <- 1/(1-rho^2)
		b <- rho*a
		c <- (beta1*a + beta2*b)/(1-alpha1*rho - alpha2*rho^2)
		
		z <- alpha2*beta3*rho*c + beta3*alpha1*c + beta3*alpha2*rho*c + beta3^2*a + (beta1^2 + 1)
		f <- ( alpha1*beta3*c*(1+alpha2) + z*(1-alpha2) )/(1 - alpha2 - alpha2^2 + alpha2^3 - alpha1^2 - alpha2*alpha1^2)
		d <- (alpha1*f + beta3*c)/(1-alpha2)
		#Bias[i,j] <- (-rho*c^2 + a*d)/(a*f - c^2)*alpha2
		#Bias[i,j] <- (beta1 - (1/a)*(rho*a*beta2 + rho*c*alpha1 + rho^2*c*alpha2 ))/beta1
		#Bias[i,j] <- ( -beta*phi*( (2*rho^2*c^2*d - rho*c^2*f - rho^3*c^2*f + rho*a*f^2 - rho*a*d^2)/(a*f^2 + 2*rho^3*c^2*d - rho^4*c^2*f - rho^2*c^2*f - a*d^2) )  )/beta1 
		
		#Bias[i,j] <- ( rho*c*(alpha+phi)/a + rho^2*c*(-alpha*phi)/a + rho*(-beta*phi) )/beta1
		
		Bias[i,j] <- ( -beta*phi * (f*b - rho*c^2)/(a*f-rho^2*c^2) + (f*rho^2*c - rho*c*d)/(a*f-rho^2*c^2) * (-alpha*phi) )/beta1 ##LGDV
	}
}

plottitle <- parse(text=paste("paste(alpha, \" = ", alpha, "\")"))
contour(A, B, 100*Bias, xlab = expression(paste("Value of ", phi)), ylab=expression(paste("Value of ", rho)), nlevels = 25, las=1, main=plottitle  )

}





###Tables 4, 5, 6 and Appendix Tables 11, 12, 13


rhoval <- c(0.25, 0.55, 0.95)
for(w in 1:3){
if(w==1) set.seed(156) #rho=0.25, varying alpha (bigr: Table 11 in Appendix; bigr2: Table 4)
if(w==2) set.seed(178) #rho=0.55, varying alpha (bigr: Table 12 in Appendix; bigr2: Table 5)
if(w==3) set.seed(143) #rho=0.95, varying alpha (bigr: Table 13 in Appendix; bigr2: Table 6)
rho <- rhoval[w]

phi2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
alpha2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
bigr <- bigr2 <- bigr3 <- matrix(0, nrow=5, ncol=length(alpha2))

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- res5 <- numeric(30000)

for(d in 1:length(res)){

u <- numeric(102)

alpha <- alpha2[q]

phi <- 0.75
beta <- 0.5

u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

rhocalc <- lm(xn~0+xlag)$coef[1]

reg <- summary(lm(ynl~0+ylag1+ylag2+xn+xlag))

res[d] <- (reg$coef[3,1]+reg$coef[4,1])/( (1-rhocalc)*(1-reg$coef[1,1]-reg$coef[2,1]) )

reg <- summary(lm(ynl~0+ylag1+xn+xlag))

res2[d] <- (reg$coef[2,1]+reg$coef[3,1])/( (1-rhocalc)*(1-reg$coef[1,1]) )

reg <- summary(lm(ynl~0+ylag1+ylag2+xn))

res3[d] <- (reg$coef[3,1])/( (1-rhocalc)*(1-reg$coef[1,1]-reg$coef[2,1]) )

reg <- summary(lm(ynl~0+ylag1+xn))

res4[d] <- reg$coef[2,1]/( (1-rhocalc)*(1-reg$coef[1,1]) )

reg <- summary(lm(ynl~0+xn))

res5[d] <- reg$coef[1,1]/(1-rhocalc)
}

newbeta <- beta/( (1-rho)*(1-alpha) )

bigr[1,q] <- (mean(res)-newbeta)/newbeta
bigr[2,q] <- (mean(res2)-newbeta)/newbeta
bigr[3,q] <- (mean(res3)-newbeta)/newbeta
bigr[4,q] <- (mean(res4)-newbeta)/newbeta
bigr[5,q] <- (mean(res5)-newbeta)/newbeta

bigr2[1,q] <- (median(res)-newbeta)/newbeta
bigr2[2,q] <- (median(res2)-newbeta)/newbeta
bigr2[3,q] <- (median(res3)-newbeta)/newbeta
bigr2[4,q] <- (median(res4)-newbeta)/newbeta
bigr2[5,q] <- (median(res5)-newbeta)/newbeta

bigr3[1,q] <- sqrt(mean( (res-newbeta)^2  ))
bigr3[2,q] <- sqrt(mean( (res2-newbeta)^2  ))
bigr3[3,q] <- sqrt(mean( (res3-newbeta)^2  ))
bigr3[4,q] <- sqrt(mean( (res4-newbeta)^2  ))
bigr3[5,q] <- sqrt(mean( (res5-newbeta)^2  ))

}

##Print comment "Table 4"
if(w==1) cat("\\clearpage Table 4\n")
##Print comment "Table 5"
if(w==2) cat("\\clearpage Table 5\n")
##Print comment "Table 6"
if(w==3) cat("\\clearpage Table 6\n")

print(xtable(
round(100*bigr2, 2) ##Tables 4, 5, or 6
))

##Print comment "Table 11"
if(w==1) cat("\\clearpage Table 11\n")
##Print comment "Table 12"
if(w==2) cat("\\clearpage Table 12\n")
##Print comment "Table 13"
if(w==3) cat("\\clearpage Table 13\n")

print(xtable(
round(100*bigr, 2) ##Tables 11, 12, or 13
))

}






###Tables 7, 8, 9 and Appendix Tables 14, 15, 16


rhoval <- c(0.25, 0.55, 0.95)
for(w in 1:3){

if(w==1) set.seed(434) #rho=0.25, varying phi (bigr: Table 14 in Appendix; bigr2: Table 7)
if(w==2) set.seed(242) #rho=0.55, varying phi (bigr: Table 15 in Appendix; bigr2: Table 8)
if(w==3) set.seed(848) #rho=0.95, varying phi (bigr: Table 16 in Appendix; bigr2: Table 9)
rho <- rhoval[w]

phi2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
alpha2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
bigr <- bigr2 <- bigr3 <- matrix(0, nrow=5, ncol=length(alpha2))

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- res5 <- numeric(30000)

for(d in 1:length(res)){

u <- numeric(102)

alpha <- 0.75

phi <- phi2[q]
beta <- 0.5

u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

rhocalc <- lm(xn~0+xlag)$coef[1]

reg <- summary(lm(ynl~0+ylag1+ylag2+xn+xlag))

res[d] <- (reg$coef[3,1]+reg$coef[4,1])/( (1-rhocalc)*(1-reg$coef[1,1]-reg$coef[2,1]) )

reg <- summary(lm(ynl~0+ylag1+xn+xlag))

res2[d] <- (reg$coef[2,1]+reg$coef[3,1])/( (1-rhocalc)*(1-reg$coef[1,1]) )

reg <- summary(lm(ynl~0+ylag1+ylag2+xn))

res3[d] <- (reg$coef[3,1])/( (1-rhocalc)*(1-reg$coef[1,1]-reg$coef[2,1]) )

reg <- summary(lm(ynl~0+ylag1+xn))

res4[d] <- reg$coef[2,1]/( (1-rhocalc)*(1-reg$coef[1,1]) )

reg <- summary(lm(ynl~0+xn))

res5[d] <- reg$coef[1,1]/(1-rhocalc)
}

newbeta <- beta/( (1-rho)*(1-alpha) )

bigr[1,q] <- (mean(res)-newbeta)/newbeta
bigr[2,q] <- (mean(res2)-newbeta)/newbeta
bigr[3,q] <- (mean(res3)-newbeta)/newbeta
bigr[4,q] <- (mean(res4)-newbeta)/newbeta
bigr[5,q] <- (mean(res5)-newbeta)/newbeta

bigr2[1,q] <- (median(res)-newbeta)/newbeta
bigr2[2,q] <- (median(res2)-newbeta)/newbeta
bigr2[3,q] <- (median(res3)-newbeta)/newbeta
bigr2[4,q] <- (median(res4)-newbeta)/newbeta
bigr2[5,q] <- (median(res5)-newbeta)/newbeta

bigr3[1,q] <- sqrt(mean( (res-newbeta)^2  ))
bigr3[2,q] <- sqrt(mean( (res2-newbeta)^2  ))
bigr3[3,q] <- sqrt(mean( (res3-newbeta)^2  ))
bigr3[4,q] <- sqrt(mean( (res4-newbeta)^2  ))
bigr3[5,q] <- sqrt(mean( (res5-newbeta)^2  ))

}

##Print comment "Table 7"
if(w==1) cat("\\clearpage Table 7\n")
##Print comment "Table 8"
if(w==2) cat("\\clearpage Table 8\n")
##Print comment "Table 9"
if(w==3) cat("\\clearpage Table 9\n")

print(xtable(
round(100*bigr2, 2) ##Tables 7, 8, or 9
))

##Print comment "Table 14"
if(w==1) cat("\\clearpage Table 14\n")
##Print comment "Table 15"
if(w==2) cat("\\clearpage Table 15\n")
##Print comment "Table 16"
if(w==3) cat("\\clearpage Table 16\n")

print(xtable(
round(100*bigr, 2) ##Tables 14, 15, or 16
))

}



#################
####Replicate Table 3
#################

###First two rows of table 3:
set.seed(1381)

alpha2 <- c(0, 0.1, 0.2, 0.5) ##Going to test models with these parameters
bigr <- bigr2 <- matrix(0, nrow=1, ncol=length(alpha2)) ##Storing model outputs

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

###Set other model parameters
phi <- 0.75
rho <- 0.95
alpha <- alpha2[q]
beta <- 0.5

###Generate Monte Carlo TS Data
u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

###Prepare time series and lagged variable data
ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

###Test different model specifications:
reg <- summary(lm(ynl~0+ylag1+xn+xlag))

res[d] <- reg$coef[2]


}

bigr[1,q] <- (mean(res)-beta)/beta


bigr2[1,q] <- sqrt(mean( (res-beta)^2 ))
}

##Print comment "Table 3, rows 1 and 2"
cat("\\clearpage Table 3, rows 1 and 2\n")

print(xtable(
rbind(
round(100*bigr, 2), ###Table 3, row 1
round(bigr2, 2) ###Table 3, row 2
)))


##Last two rows of Table 3

set.seed(143)

phi2 <- c(0, 0.1, 0.2, 0.5) ##Going to test models with these parameters
bigr <- bigr2 <- matrix(0, nrow=1, ncol=length(phi2)) ##Storing model outputs

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

###Set other model parameters
alpha <- 0.75
rho <- 0.95
phi <- phi2[q]
beta <- 0.5

###Generate Monte Carlo TS Data
u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

###Prepare time series and lagged variable data
ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

###Test different model specifications:
reg <- summary(lm(ynl~0+ylag1+xn+xlag))

res[d] <- reg$coef[2]


}

bigr[1,q] <- (mean(res)-beta)/beta


bigr2[1,q] <- sqrt(mean( (res-beta)^2 ))
}

##Print comment "Table 3, rows 3 and 4"
cat("\\clearpage Table 3, rows 3 and 4\n")

print(xtable(
rbind(
round(100*bigr, 2), ###Table 3, row 3
round(bigr2, 2) ###Table 3, row 4
)))







#################
####Replicate Table 10 in Appendix
#################

###First two rows of table 10:
set.seed(1381)

alpha2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) ##Going to test models with these parameters
bigr <- bigr2 <- matrix(0, nrow=1, ncol=length(alpha2)) ##Storing model outputs

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

###Set other model parameters
phi <- 0.75
rho <- 0.95
alpha <- alpha2[q]
beta <- 0.5

###Generate Monte Carlo TS Data
u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

###Prepare time series and lagged variable data
ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

###Test different model specifications:

func <- function(x){
	alpha <- x[1]
	phi <- x[2]
	beta <- x[3]
	return( sum( (ynl-( (alpha+phi)*ylag1 + (-alpha*phi)*ylag2 + beta*xn + (-beta*phi)*xlag ))^2 ) )
}
reg <- nlm(f=func, p=c(alpha, phi, beta)) ###Initialize estimate with actual values for variables

res[d] <- reg$estimate[3]


}

bigr[1,q] <- (mean(res)-beta)/beta


bigr2[1,q] <- sqrt(mean( (res-beta)^2 ))
}

##Print comment "Table 10, rows 1 and 2"
cat("\\clearpage Table 10, rows 1 and 2\n")

print(xtable(
rbind(
round(100*bigr, 2), ###Table 10, row 1
round(bigr2, 2) ###Table 10, row 2
)))


##Last two rows of Table 10

set.seed(143)

phi2 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5) ##Going to test models with these parameters
bigr <- bigr2 <- matrix(0, nrow=1, ncol=length(phi2)) ##Storing model outputs

for(q in 1:length(alpha2)){
res <- res2 <- res3 <- res4 <- numeric(1000)

for(d in 1:length(res)){

u <- numeric(102)

###Set other model parameters
alpha <- 0.75
rho <- 0.95
phi <- phi2[q]
beta <- 0.5

###Generate Monte Carlo TS Data
u[1] <- rnorm(1, 0, 1)
for(i in 2:length(u)){
	u[i] <- phi*u[i-1] + rnorm(1, 0, 1)
}

x <- numeric(length(u))
x[1] <- rnorm(1, 0, 1)
for(i in 2:length(x)){
	x[i] <- rho*x[i-1] + rnorm(1, 0, 1)
}

y <- numeric(length(u))
yinit <- rnorm(1, 0, 1)

for(i in 1:length(y)){
	if(i==1) y[i] <- alpha*yinit + beta*x[i] + u[i]
	if(i > 1) y[i] <- alpha*y[i-1] + beta*x[i] + u[i]
}

###Prepare time series and lagged variable data
ynl <- y[-(1:2)]
ylag1 <- y[-c(1, length(y))]
ylag2 <- y[-(length(y):(length(y)-1)) ]

xn <- x[-1]
xlag <- x[-length(x)]

xn <- xn[-1]
xlag <- xlag[-1]

###Test different model specifications:

func <- function(x){
	alpha <- x[1]
	phi <- x[2]
	beta <- x[3]
	return( sum( (ynl-( (alpha+phi)*ylag1 + (-alpha*phi)*ylag2 + beta*xn + (-beta*phi)*xlag ))^2 ) )
}
reg <- nlm(f=func, p=c(alpha, phi, beta)) ###Initialize estimate with actual values for variables

res[d] <- reg$estimate[3]


}

bigr[1,q] <- (mean(res)-beta)/beta


bigr2[1,q] <- sqrt(mean( (res-beta)^2 ))
}

##Print comment "Table 10, rows 3 and 4"
cat("\\clearpage Table 10, rows 3 and 4\n")

print(xtable(
rbind(
round(100*bigr, 2), ###Table 10, row 3
round(bigr2, 2) ###Table 10, row 4
)))