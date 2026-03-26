betas <- coef(model.fe)
	varCov <- vcov(model.fe)
		reps <- 1000
			yhat <- matrix(NA,reps,dim(values)[1]) 

for(k in 1:reps){
yhat[k,] <- values %*% mvrnorm(1,betas,varCov)
}

data<- melt(yhat)

predvalues <- apply(yhat, 2, quantile, probs = c(0.025,0.5,0.975,0.05,0.95,0.1,0.9)) 
	data.2<- melt(predvalues)



