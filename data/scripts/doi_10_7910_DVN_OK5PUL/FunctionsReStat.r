## All functions for "Expectation Formation Following Large Unexpected Shocks"

infoDyn.ssf <- function(data.public,pi,obs.public,param,hom.cov,het.covs,beta,alpha)
{

	################################################################################
	#
	#	infoDyn.ssf by Tucker McElroy
	#
	#		Computes dynamic Kalman filter for a VAR(p) signal observed via obs.mat,
	#			split according to private and public information, with
	#			heteroscedastic noise given by het.covs, and a dynamic
	#			amount of private information governed by hom.cov
	#		data.public is T x M.pub dimensional
	#		pi is N dimensional VAR(p) latent signal process, where p = var.order
	#		obs.public governs how signal is observed as public data,
	#			and is M.pub x N dimensional
	#		param is p+1 x N x N dimensional array, with param[,,i] = ith
	#			VAR coefficient matrix, and param[,,p+1] = innovation variance matrix
	#		hom.cov is the M.priv x M.priv covariance matrix for one unit of private noise
	#		het.covs are the heteroscedastic covariances, an array M.pub x T x M.pub
	#		beta governs the cost of forecast MSE to the agent, with beta=0 indicating
	#			indifference and beta=Inf indicating supreme importance
	#		alpha govers the cost of purchasing l units of private information: alpha*l
	#
	###################################################################################

	max.info <- 20

	M.pub <- dim(data.public)[2]
	T <- dim(data.public)[1]
	N <- dim(param)[2]
	var.order <- dim(param)[3]-1

	# put model in SSF
	phis.var <- matrix(param[,,1:var.order],nrow=N)
	sig.var <- param[,,var.order+1]
	if(var.order==1) { trans.mat <- phis.var } else {
		trans.mat <- rbind(phis.var,diag(N*var.order)[1:(N*(var.order-1)),]) }
	G.mat <- t(c(1,rep(0,var.order-1)) %x% diag(N))
 
	# initialize Kalman filter   
	state.init <- rep(0,var.order*N)
	state.init <- matrix(t(state.init),ncol=1)
	# generate acf at negative lags
	var.acf <- aperm(VARMAauto(param[,,1:var.order],NULL,sig.var,var.order-1),c(2,3,1))
	var2.acf <- var.acf
	var2.acf[,1,] <- var.acf[,1,]/2
	var.init <- matrix(sigex.blocktoep(aperm(var2.acf,c(1,3,2))),N*var.order,N*var.order)
	var.init <- var.init + t(var.init)
	
	# given initial state estimate and precision, run state space filter
	#	state.vec is state vector given the past data
	#	state.mse is mse of state.vec
	state.len <- length(state.init)
	state.vecs <- state.init
	state.mses <- array(0,c(state.len,state.len,T+1))
	state.mses[,,1] <- var.init
	info <- array(0,c(N,N,T))
	best.ls <- NULL

	for(t in 1:T)
	{

		# run next step of KF based on variable amount of private info
		best.cost <- Inf
		best.l <- 0

		for(l in 0:max.info)
		{
	
			obs.private <- NULL
			data.private <- NULL
			delta.cov <- het.covs[,t,]
			M.priv <- l*N
	
			if(l > 0) {
			obs.private <- rep(1,l) %x% diag(N)
			nu.new <- t(chol(diag(l) %x% hom.cov)) %*% matrix(rnorm(N*l),ncol=1)
			data.private <- obs.private %*% t(pi[t,,drop=FALSE]) + nu.new
	 		delta.cov <- matrix(bdiag(diag(l) %x% hom.cov,het.covs[,t,]),M.priv+M.pub,M.priv+M.pub) 
			}		
		
			obs.mat <- rbind(obs.private,obs.public)
			H.mat <- obs.mat %*% G.mat
	 		x.new <- rbind(data.private,t(data.public[t,,drop=FALSE]))
 			resid.vec <- x.new - H.mat %*% state.vecs[,t]		
			resid.var <- H.mat %*% state.mses[,,t] %*% t(H.mat) +	delta.cov
	 		kalman <- trans.mat %*% state.mses[,,t] %*% t(H.mat) %*% solve(resid.var)
	 		state.vec <- trans.mat %*% state.vecs[,t] + kalman %*% resid.vec
			state.mse <- (trans.mat - kalman %*% H.mat) %*% state.mses[,,t] %*% t(trans.mat)
			state.mse <- state.mse + t(G.mat) %*% sig.var %*% G.mat
			kalman.obs <- kalman %*% obs.mat

			# choose the best amount of private info
			cost <- beta*sum(diag(G.mat %*% state.mse %*% t(G.mat))) + alpha*l
			if(cost < best.cost) 
			{ 
				best.cost <- cost
				best.l <- l 
				state.vec.best <- state.vec
				state.mse.best <- state.mse
				kalman.obs.best <- kalman.obs
			}
		}

		best.ls <- c(best.ls,best.l)
		state.vecs <- cbind(state.vecs,state.vec.best)
		state.mses[,,t+1] <- state.mse.best
		info[,,t] <- G.mat %*% solve(trans.mat) %*% kalman.obs.best

	}
  
	return(list(state.vecs,state.mses,info,best.ls))

}

infoDyn.ssf2 <- function(data.public,pi,obs.public,param,hom.cov1,hom.cov2,het.covs,units.priv1,units.priv2)
{

	################################################################################
	#
	#	infoDyn.ssf2 by Tucker McElroy
	#
	#		Computes dynamic cross-Kalman filter for a VAR(p) signal observed via obs.mat,
	#			split according to private and public information, with
	#			heteroscedastic noise given by het.covs, and a dynamic
	#			amount of private information governed by hom.cov1 and hom.cov2
	#			for agents 1 and 2 respectively.
	#		data.public is T x M.pub dimensional
	#		pi is N dimensional VAR(p) latent signal process, where p = var.order
	#		obs.public governs how signal is observed as public data,
	#			and is M.pub x N dimensional
	#		param is p+1 x N x N dimensional array, with param[,,i] = ith
	#			VAR coefficient matrix, and param[,,p+1] = innovation variance matrix
	#		hom.cov1 is the M.priv x M.priv covariance matrix for one unit of private noise
	#			for agent 1
	#		hom.cov2 is the M.priv x M.priv covariance matrix for one unit of private noise
	#			for agent 2
	#		het.covs are the heteroscedastic covariances, an array M.pub x T x M.pub
	#		units.priv1 is a T-vector of integer units of private info purchased by agent 1
	#		units.priv2 is a T-vector of integer units of private info purchased by agent 2
	#
	#		Returns: array of cross-covariances of errors for estimated state vector
	###################################################################################

	M.pub <- dim(data.public)[2]
	T <- dim(data.public)[1]
	N <- dim(param)[2]
	var.order <- dim(param)[3]-1

	# put model in SSF
	phis.var <- matrix(param[,,1:var.order],nrow=N)
	sig.var <- param[,,var.order+1]
	if(var.order==1) { trans.mat <- phis.var } else {
		trans.mat <- rbind(phis.var,diag(N*var.order)[1:(N*(var.order-1)),]) }
	G.mat <- t(c(1,rep(0,var.order-1)) %x% diag(N))
 
	# initialize Kalman filter   
	state.init <- rep(0,var.order*N)
	state.init <- matrix(t(state.init),ncol=1)
	# generate acf at negative lags
	var.acf <- aperm(VARMAauto(param[,,1:var.order],NULL,sig.var,var.order-1),c(2,3,1))
	var2.acf <- var.acf
	var2.acf[,1,] <- var.acf[,1,]/2
	var.init <- matrix(sigex.blocktoep(aperm(var2.acf,c(1,3,2))),N*var.order,N*var.order)
	var.init <- var.init + t(var.init)
	
	# given initial state estimate and precision, run state space filter
	#	state.vec is state vector given the past data
	#	state.mse is mse of state.vec
	state.len <- length(state.init)
	state.mses1 <- array(0,c(state.len,state.len,T+1))
	state.mses1[,,1] <- var.init
	state.mses2 <- array(0,c(state.len,state.len,T+1))
	state.mses2[,,1] <- var.init
	state.mses12 <- array(0,c(state.len,state.len,T+1))
	state.mses12[,,1] <- var.init	
	info1 <- array(0,c(N,N,T))
	info2 <- array(0,c(N,N,T))
	 
	for(t in 1:T)
	{

		obs.private1 <- NULL
		data.private1 <- NULL
		delta.cov1 <- het.covs[,t,]
		M.priv1 <- units.priv1[t]*N
	
		if(units.priv1[t] > 0) {
		obs.private1 <- rep(1,units.priv1[t]) %x% diag(N)
 		delta.cov1 <- matrix(bdiag(diag(units.priv1[t]) %x% hom.cov1,het.covs[,t,]),M.priv1+M.pub,M.priv1+M.pub) 
		}	

		obs.private2 <- NULL
		data.private2 <- NULL
		delta.cov2 <- het.covs[,t,]
		M.priv2 <- units.priv2[t]*N

		if(units.priv2[t] > 0) {
		obs.private2 <- rep(1,units.priv2[t]) %x% diag(N)
 		delta.cov2 <- matrix(bdiag(diag(units.priv2[t]) %x% hom.cov2,het.covs[,t,]),M.priv2+M.pub,M.priv2+M.pub) 
		}	
		
		obs.mat1 <- rbind(obs.private1,obs.public)
		H.mat1 <- obs.mat1 %*% G.mat
		obs.mat2 <- rbind(obs.private2,obs.public)
		H.mat2 <- obs.mat2 %*% G.mat

		resid.var1 <- H.mat1 %*% state.mses1[,,t] %*% t(H.mat1) + delta.cov1
		resid.var2 <- H.mat2 %*% state.mses2[,,t] %*% t(H.mat2) + delta.cov2
 		kalman1 <- trans.mat %*% state.mses1[,,t] %*% t(H.mat1) %*% solve(resid.var1)
		kalman2 <- trans.mat %*% state.mses2[,,t] %*% t(H.mat2) %*% solve(resid.var2)
		state.mse1 <- (trans.mat - kalman1 %*% H.mat1) %*% state.mses1[,,t] %*% t(trans.mat) +
			t(G.mat) %*% sig.var %*% G.mat
		state.mse2 <- (trans.mat - kalman2 %*% H.mat2) %*% state.mses2[,,t] %*% t(trans.mat) +
			t(G.mat) %*% sig.var %*% G.mat
		state.mse12 <- (trans.mat - kalman1 %*% H.mat1) %*% state.mses12[,,t] %*% t(trans.mat - kalman2 %*% H.mat2) + 
			t(G.mat) %*% sig.var %*% G.mat + kalman1 %*% as.matrix(bdiag(matrix(0,M.priv1,M.priv2),het.covs[,t,])) %*% t(kalman2)	 
		state.mses1[,,t+1] <- state.mse1
		state.mses2[,,t+1] <- state.mse2
		state.mses12[,,t+1] <- state.mse12
		info1[,,t] <- G.mat %*% solve(trans.mat) %*% kalman1 %*% obs.mat1
		info2[,,t] <- G.mat %*% solve(trans.mat) %*% kalman2 %*% obs.mat2
	}
  
	return(state.mses12)

}

varp.param <- function(var.order,N,disp.coef,disp.sig)
{

	####################################################################
	#
	#	varp.param.r by Tucker McElroy, modifying code from Peter Linton
	#
	#	generates a stable dimension N VAR(p) process, with p = var.order
	#	disp.coef controls size of VAR coefficients, disp.sig the spread
	#		of the innovation variance matrix
	#	Algorithm is that of Roy, McElroy, Linton.
	#	Checks stability, and returns N x N x (var.order+1) array
	#	
	##################################

sqrtm <- function(A){
  # Function returns the square root of the matrix
  # Input:
  #   A: m x m non-negative definite matrix
  # Output:
  #   m x m matrix
  return(eigen(A)$vectors %*% diag(sqrt(eigen(A)$values)) %*% t(eigen(A)$vectors))
}

if(var.order > 0) {
	phi <- matrix(rnorm((var.order+1)*N*N)*disp.coef,nrow=N*N,ncol=var.order+1)
	v <- array(0, dim = c(N, N, var.order))
	Q <- v

	for(j in 1:var.order){
	    l <- diag(N)
	    l[lower.tri(l)] <- phi[1:choose(N, 2), j]
	    d <- diag(exp(phi[(choose(N, 2) + 1):choose(N + 1, 2), j]))
   	    v[,,j] <- l %*% d %*% t(l)
	    s <- diag(0, N)
	    s[lower.tri(s)] <- phi[(choose(N + 1, 2) + 1):(N^2), j]
    	    s <- s - t(s)
	    delta <- (-1)^rbinom(1,prob=.5,1)	
    	    Q[,,j] <- diag(c(delta, rep(1, (N - 1)))) %*% 
			(diag(N) - s) %*% solve(diag(N) + s)
  	}

	l <- diag(N)
	l[lower.tri(l)] <- phi[1:choose(N, 2),(var.order + 1)]
	d <- diag(exp(phi[(choose(N,2)+1):choose(N + 1, 2),(var.order + 1)]))
	sigma <- l %*% d %*% t(l) * disp.sig

  	param <- array(0, dim = c(N,N,var.order+1))
	u <- array(0, dim = c(N,N,(var.order + 1)))
  	u[,,1] <- diag(N) + apply(v, c(1, 2), sum)
  	dd <- u[,,1]
	u[,,2] <- sqrtm(v[,,1]) %*% Q[,,1] %*% sqrtm(dd)
	tkappa <- t(u[,,2])
	txi <- u[,,2]
  	bigu <- u[,,1]
	if (var.order > 1) {
	    for(j in 3:(var.order + 1)){
          	dd <- u[,,1] - tkappa %*% solve(bigu) %*% t(tkappa)
      	u[,,j] <- (txi %*% solve(bigu) %*% t(tkappa) + 
                  sqrtm(v[,,(j - 1)]) %*% Q[,,(j - 1)] %*% sqrtm(dd) )
      	bigu <- rbind(cbind(bigu,t(tkappa)), cbind(tkappa,u[,,1]))
  	      tkappa <- cbind(t(u[,,j]), tkappa)
   		txi <- cbind(txi, u[,,j])
    	    }
  	}
  	phi <- txi %*% solve(bigu)
	for(j in 1:var.order) { param[,,j] <- phi[,(N * (j - 1) + 1):(N * j)] }
	param[,,var.order+1] <- sigma
   
	# stability check
	phis.var <- matrix(param[,,1:var.order],nrow=N)
	sig.var <- param[,,var.order+1]
	if(var.order==1) { trans.mat <- phis.var } else {
		trans.mat <- rbind(phis.var,diag(N*var.order)[1:(N*(var.order-1)),]) }
	if(max(Mod(eigen(trans.mat)$value)) < 1) { print("Stable!") }
} else	# white noise case 
{	
	phi <- matrix(rnorm(N*N)*disp.coef,nrow=N*N,ncol=1)
	l <- diag(N)
	l[lower.tri(l)] <- phi[1:choose(N, 2),1]
	d <- diag(exp(phi[(choose(N,2)+1):choose(N + 1, 2),1]))
	sigma <- l %*% d %*% t(l) * disp.sig
	param <- array(0, dim = c(N,N,1))
	param[,,1] <- sigma
}

  return(param)

}

vol.pg.process <- function(disp.private,disp.public,M1,M2,maxlag)
{

	###########################################################
	#	vol.pg.process by Tucker McElroy
	#
	#	generates a simulation off Phillipov-Glickman
	#		stochastic volatility process
	#	consists of two volatility matrix processes of
	#		dimensions M1 and M2 for private and public,
	#		governed by respective dispersion parameters.
	#		maxlag is the sample size of the simulation.
	############################################################

	burnin <- 500
	T <- burnin + maxlag
	scale.private <- diag(M1)
	scale.public <- diag(M2)
	het.array <- array(0,c(M1+M2,T,M1+M2))
	omega <- M1+M2
	rho.private <- rnorm(1)*disp.private
	rho.private <- exp(rho.private)/(1+exp(rho.private))
	rho.public <- rnorm(1)*disp.public
	rho.public <- exp(rho.public)/(1+exp(rho.public))
	rho.private <- .8
	rho.public <- .8
	Lmat.private <- diag(M1)
	Lmat.private[lower.tri(Lmat.private)] <- rnorm(M1*(M1-1)/2)*disp.private
	Lmat.public <- diag(M2)
	Lmat.public[lower.tri(Lmat.public)] <- rnorm(M2*(M2-1)/2)*disp.public
	for(t in 1:T)
	{
		het.private <- rWishart(1,omega,scale.private)[,,1]
		Q.private <- eigen(het.private)$vectors
		D.private <- diag(eigen(het.private)$values)
		scale.private <- omega^(-1)*Lmat.private %*% Q.private %*% D.private^rho.private %*% t(Q.private) %*% t(Lmat.private)
		het.public <- rWishart(1,omega,scale.public)[,,1]
		Q.public <- eigen(het.public)$vectors
		D.public <- diag(eigen(het.public)$values)
		scale.public <- omega^(-1)*Lmat.public %*% Q.public %*% D.public^rho.public %*% t(Q.public) %*% t(Lmat.public)
		het.array[,t,] <- rbind(cbind(solve(het.private),matrix(0,M1,M2)),
			cbind(matrix(0,M2,M1),solve(het.public)))
	}
	het.acf <- het.array[,(burnin+1):T,]

	return(het.acf)
}

vol.cs.process <- function(disp.pub,M.pub,maxlag,shock,size)
{
	
	###########################################################
	#	vol.cs.process by Tucker McElroy
	#
	#	generates a simulation  based off Cogley-Sargent
	#		stochastic volatility process
	#	consists of a volatility matrix processes of
	#		dimension  M.pub for   public noise,
	#		governed by   dispersion parameter.
	#		maxlag is the sample size of the simulation.
	#		shock is an index between 1 and maxlag,
	#			where an increase in variability occurs;
	#			a non-positive value indicates no shock occurs
	#			real(shock) is index, imaginary(shock) = 1 for IO(LS), 0 for AO
	#		size is a parameter governing shock size
	############################################################

	burnin <- 500
	T <- burnin + maxlag
	shock.LS <- Im(shock)
	shock <- Re(shock)
 
	het.array <- array(0,c(M.pub,T,M.pub))
 	b.public <- array(0,c(M.pub,T,M.pub))
	omega.public <- array(0,c(M.pub,T,M.pub))
	b.public[,1,] <- diag(M.pub)
	omega.public[,1,] <- diag(M.pub)
	innov.shock <- rep(20*disp.pub,M.pub)
	for(t in 2:T)
	{	
		c.new <- 0*diag(M.pub)
		AOflag <- 0
		c.new[lower.tri(c.new)] <- rnorm(M.pub*(M.pub-1)/2,sd=disp.pub)
		b.public[,t,] <- b.public[,t-1,] %*% matrix.exp(c.new,M.pub-1)
		innov <- rnorm(M.pub,sd=disp.pub)
		if((t == (burnin + shock)) && (shock > 0)) 
		{ 
			if(shock.LS) { innov <- innov.shock } else { AOflag <- 1 }
			
		}
		d.new <- diag(exp(innov))
		omega.public[,t,] <- omega.public[,t-1,] %*% d.new
		het.public <- b.public[,t,] %*% omega.public[,t,] %*% t(b.public[,t,])
		het.array[,t,] <- het.public * (1 + AOflag * size) 
	}
	het.acf <- het.array[,(burnin+1):T,]

	return(het.acf)
}

matrix.exp <- function(A.matrix,terms)
{
	# computes matrix exponential

	m <- dim(A.matrix)[1]
	P.matrix <- diag(m)
	E.matrix <- diag(m)
	for(i in 1:terms)
	{
		P.matrix <- A.matrix %*% P.matrix
		E.matrix <- E.matrix + P.matrix/factorial(i)
	}
	return(E.matrix)
} 
 
sigex.blocktoep <- function(x.array)
{
	
	#################################
	#	sigex.blocktoep
	#
	#	takes x.array of dimension N,N,H
	#		and generates block Toeplitz lower triangular 
	#		array of dimension N,H,N,H
	#
	##############################

N <- dim(x.array)[1]
H <- dim(x.array)[3]

x.toep <- array(0,c(N,H,N,H))
for(j in 1:H)
{
	for(k in 1:H)
	{
		if(j >= k) { x.toep[,j,,k] <- x.array[,,j-k+1] }
	}
}

return(x.toep)
}

VARMAauto <- function(phi,theta,sigma,maxlag)
{
	#### Function computes autocovariances of VARMA (p,q) from lag zero
	#	to maxlag, with inputs phi and theta.
	#	(1 - phi[1]z ... - phi[p]z^p) X_t = (1 + theta[1]z ...+ theta[q]z^q) WN
	#  output: autocovariance string of length maxlag
	#  for absent AR or MA portions, pass in NULL
	#  phi and theta should be arrays of m x m matrices
	#	sigma should be an m x m matrix
	#  e.g. phi <- array(cbind(phi1,phi2,...,phip),c(m,m,p))

polymulMat <- function(amat,bmat)
{
	p <- dim(amat)[3]
	q <- dim(bmat)[3]
	amatd <- amat[,,p:1]
	if(q > 1) amatd <- array(c(matrix(0,m,m*(q-1)),amatd),c(m,m,p+q-1))
	bigmat <- NULL
	for(i in 1:(p+q-1)) 
	{
		nextmat <- matrix(amatd[,,1:(p+q-1)],m,m*(p+q-1))
		bigmat <- rbind(nextmat,bigmat)
		amatd <- amatd[,,-1]
		amatd <- array(c(amatd,matrix(0,m,m)),c(m,m,p+q-1))
	}
	bigmat <- bigmat[,1:(m*q)]
	out <- bigmat %*% t(matrix(bmat[,,q:1],m,m*q))
	out <- array(out,c(m,p+q-1,m))
	temp <- NULL
	for(i in 1:(p+q-1))
	{
		temp <- cbind(temp,out[,i,])
	}
	out <- array(temp,c(m,m,p+q-1))

	return(out)
}

Kcommut <- function(vect,m,n)
{
	return(matrix(t(matrix(vect,nrow=m,ncol=n)),ncol=1))
}

m <- dim(sigma)[2]
p <- 0
q <- 0
if (length(phi) > 0) p <- dim(phi)[3]
if (length(theta) > 0) q <- dim(theta)[3]
Kmat <- apply(diag(m^2),1,Kcommut,m,m)

if (q == 0) { gamMA <- array(sigma,c(m,m,1)) } else 
{
	temp <- polymulMat(array(cbind(diag(m),matrix(theta,m,m*q)),c(m,m,q+1)),
		array(sigma,c(m,m,1)))
	gamMA <- polymulMat(temp,array(cbind(diag(m),matrix(theta,m,m*q)),c(m,m,q+1)))
}
gamMA <- gamMA[,,(q+1):(2*q+1)]
gamMAvec <- matrix(gamMA,m^2*(q+1),1)

if (p > 0) 
{
	Amat <- matrix(0,nrow=m^2*(p+1),ncol=m^2*(2*p+1))
	Amat <- array(Amat,c(m^2,p+1,m^2,2*p+1))
	Arow <- diag(m^2)
	for(i in 1:p)
	{
		Arow <- cbind(-1*diag(m) %x% phi[,,i],Arow)
	}
	for(i in 1:(p+1))
	{
		Amat[,i,,i:(i+p)] <- Arow
	}
	newA <- array(matrix(Amat[,1:(p+1),,1:p],m^2*(p+1),m^2*(p)),c(m^2,p+1,m^2,p))
	for(i in 1:(p+1))
	{
		for(j in 1:p)
		{
 			newA[,i,,j] <- newA[,i,,j] %*% Kmat
		}
	}
	Amat <- cbind(matrix(Amat[,,,p+1],m^2*(p+1),m^2),
			matrix(Amat[,,,(p+2):(2*p+1)],m^2*(p+1),m^2*(p)) + 
			matrix(newA[,,,p:1],m^2*(p+1),m^2*(p)))

	Bmat <- matrix(0,nrow=m^2*(q+1),ncol=m^2*(p+q+1))
	Bmat <- array(Bmat,c(m^2,q+1,m^2,p+q+1))
	Brow <- diag(m^2)
	for(i in 1:p)
	{
		Brow <- cbind(Brow,-1*phi[,,i] %x% diag(m))
	}
	for(i in 1:(q+1))
	{
		Bmat[,i,,i:(i+p)] <- Brow
	}
	Bmat <- Bmat[,,,1:(q+1)]
	Bmat <- matrix(Bmat,m^2*(q+1),m^2*(q+1))
	Binv <- solve(Bmat)

	gamMix <- Binv %*% gamMAvec
	if (p <= q) gamMixTemp <- gamMix[1:((p+1)*m^2)] else 
		gamMixTemp <- c(gamMix,rep(0,(p-q)*m^2))
	gamARMA <- solve(Amat) %*% gamMixTemp 
	gamMix <- array(matrix(gamMix,m,m*(q+1)),c(m,m,q+1))
	gamARMA <- array(matrix(gamARMA,m,m*(p+1)),c(m,m,p+1))
} else 
{
	gamARMA <- array(gamMA[,,1],c(m,m,1))
	if (q == 0) { gamMix <- array(sigma,c(m,m,1)) } else 	
		gamMix <- gamMA[,,1:(q+1)]
}

if (maxlag <= p) 
{
	gamARMA <- gamARMA[,,1:(maxlag+1)] 
} else
{
	if (maxlag > q) gamMix <- array(cbind(matrix(gamMix,m,m*(q+1)),
		matrix(0,m,m*(maxlag-q))),c(m,m,(maxlag+1)))
	for(k in 1:(maxlag-p))
	{
		len <- dim(gamARMA)[3]
		acf <- gamMix[,,p+1+k]
		if (p > 0) 
		{
			temp <- NULL
			for(i in 1:p)
			{
				temp <- rbind(temp,gamARMA[,,len-i+1])
			}
			acf <- acf + matrix(phi,m,m*p) %*% temp
		} 
		gamARMA <- array(cbind(matrix(gamARMA,m,m*len),acf),c(m,m,len+1))
	}
}

return(gamARMA)
}

