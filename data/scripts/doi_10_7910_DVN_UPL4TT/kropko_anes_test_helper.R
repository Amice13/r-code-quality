# Code adapted from Kropko et al. (2007) Multiple Imputation for Continuous and Categorical Data: Comparing Joint Multivariate Normal and Conditional Approaches
# Source: https://doi.org/10.7910/DVN/24672

.MPinverse <- function(eta, tol = sqrt(.Machine$double.eps)) {
	cov_eta <- cov(eta)
	ev <- eigen(cov_eta, TRUE)
	ev$values <- ifelse(ev$values > tol, 1/ev$values, 0)
	Sigma_inv <- crossprod(sqrt(ev$values)*(t(ev$vectors)))
	return(Sigma_inv)
}

.amelia.pmm <- function(dgp, imp, V, V2, k, missing){ # v = column number(s) of the variable being imputed in adjusted amelia data
	res <- list()
	data.cmu <- imp$imputations[[k]][,-V]
 	mu <- imp$mu[,k]
 	sigma <- imp$covMatrices[,,k]	
 	B <- sigma[V,V]
 	C <- sigma[V,-V,drop=FALSE]
 	D <- sigma[-V,-V]
 	CDinv <- C%*%solve(D)
	cMu <- apply(data.cmu, 1, FUN=function(x){c(mu[V] + CDinv %*% (x - mu[-V]))})
	cmeans <- t(cMu) 
	miss <- missing
	if(length(V)>1){ 
		mark <- apply(cmeans[miss,], 1, FUN=function(x){
			Sigma_inv <- .MPinverse(x-cmeans[!miss,])
			mdiff <- (x-cmeans[!miss,])
			MD <- mahalanobis(mdiff, rep(0,ncol(mdiff)), Sigma_inv, inverted=TRUE)
			return(which.min(MD))
		})
	} else mark <- sapply(cmeans[miss], FUN=function(x){which.min(abs(x-cmeans[!miss]))}) 
	draws <- dgp$truedata[mark, V2]
	res$draws <- draws
	res$mark <- mark
	return(res)
}

.dgp <- function(miss.prop = .1, imp_meth="ppd", use.miss=FALSE){

	#Load and clean complete data
	anes <- read.dta("data/base2008_3.dta")
	anes <- anes[,c(6:8,11:13,25,42,44:46)]
	levels(anes$imp_enviro) <- c("Not important", "Not important", "Important", "Important")
	levels(anes$religion) <- c("Protestant", "Catholic/Orthodox", "Atheist/Other", "Atheist/Other")
	anes <- anes[!as.logical(rowSums(is.na(anes[,c(2,3,9,11)]))),] 
	
	#Apply missingness
	anes[["education"]] <- factor(anes[["education"]], ordered=TRUE)
	anes[["jobs_r"]] <- factor(anes[["jobs_r"]], ordered=TRUE)
	anes[["income"]] <- factor(anes[["income"]], ordered=TRUE)
	anes <- na.omit(anes) 

	miss.data <- cbind(1, scale(model.matrix(runif(nrow(anes)) ~ anes[,2] + anes[,3] + anes[,9] + anes[,11]))[,-1])
	coef <- matrix(rnorm(ncol(anes[,-c(2,3,9,11)])*ncol(miss.data)),ncol(miss.data),ncol(anes[,-c(2,3,9,11)]))
	miss.eta <- miss.data%*%coef
	miss.error <- rdata.frame(nrow(anes), restrictions="none", n_full=ncol(anes[,-c(2,3,9,11)]), n_partial=0)$true
	miss.eta <- miss.eta + .3*miss.error 
	miss.pr <- apply(miss.eta, 2, plogis) - matrix(runif(nrow(miss.eta)*ncol(miss.eta)), nrow(miss.eta), ncol(miss.eta))
	miss.indic <- apply(miss.pr, 2, FUN=function(x){x >= quantile(x, (1 - miss.prop))})
	miss.indic <- cbind(miss.indic[,1], rep(FALSE, nrow(miss.indic)), rep(FALSE, nrow(miss.indic)), 
		miss.indic[,2:6], rep(FALSE, nrow(miss.indic)), miss.indic[,7], rep(FALSE, nrow(miss.indic)))
	anes.miss <- anes
	is.na(anes.miss) <- miss.indic
	missind <- list()
	missind$religion <- miss.indic[,1] 		#unordered
	missind$married <- miss.indic[,2] 		#unordered
	missind$education <- miss.indic[,3] 	#ordered
	missind$income <- miss.indic[,4] 		#ordered
	missind$time <- miss.indic[,5] 			#continuous
	missind$vote <- miss.indic[,6] 			#unordered
	missind$jobs_r <- miss.indic[,7] 		#ordered
	missind$imp_enviro <- miss.indic[,8] 	#binary
	missind$female <- miss.indic[,9] 		#binary
	missind$white <- miss.indic[,10] 		#binary
	missind$age <- miss.indic[,11] 			#continuous
	if(use.miss){
		miss.data <- sapply(missind, FUN=function(x){x}, simplify=TRUE)
		colnames(miss.data) <- paste("miss.", colnames(miss.data), sep="")
		anes.miss <- cbind(anes.miss, miss.data)
		anes <- cbind(anes, miss.data)
		}
	
	dgp <- list()
	dgp$truedata <- anes
	dgp$data <- anes.miss
	dgp$imp_meth <- imp_meth
	dgp$use.miss <- use.miss
	dgp$co <- dgp$bi <- dgp$or <- dgp$un <- list()

	f1 <- " ~ age + female + as.numeric(education) + married + white + as.numeric(income) + religion"
	f2 <- " + miss.white + miss.income + miss.religion + miss.time + miss.vote + miss.jobs_r + miss.imp_enviro"
	dv.list <- c("time", "imp_enviro", "jobs_r", "vote")

	for(t in c("co","bi","or","un")){
		if(t=="co"){
			dv <- "time"
			dv2 <- dv.list[-which(is.element(dv.list, dv))]
			dv2 <- paste("+",paste(dv2, collapse="+"), collapse="")
			if(use.miss) f <- as.formula(paste(dv, f1, f2, collapse="")) else f <- as.formula(paste(dv, f1, collapse=""))
			if(use.miss) fm <- as.formula(paste(dv, f1, dv2, f2, collapse="")) else fm <- as.formula(paste(dv, f1, dv2, collapse=""))
			truevar <- anes$time
			fitmodel <- bayesglm(fm, data=anes)
			true <- bayesglm(f, data=anes)
			miss <- missind$time
			col <- NA
			fitted <- model.matrix(true)%*%coef(true)
			ev <- eigen(vcov(fitmodel), symmetric = TRUE)
			params <- (coef(fitmodel) + (ev$vectors %*% (sqrt(ev$values) * rnorm(length(coef(fitmodel)))))[,1])
			draws <- model.matrix(fitmodel)%*%params
			truematch <- sqrt(mean((draws[miss] - anes$time[miss])^2))
			if(imp_meth=="pmm"){
				mark <- sapply((model.matrix(fitmodel)%*%params)[miss], FUN=function(m){which.min(abs(m-anes$time[!miss]))})
				truematch <- sqrt(mean((anes$time[miss] - anes$time[!miss][mark])^2))
			}
		}
		if (t=="bi"){
			dv <- "imp_enviro"
			dv2 <- dv.list[-which(is.element(dv.list, dv))]
			dv2 <- paste("+",paste(dv2, collapse="+"), collapse="")
			if(use.miss) f <- as.formula(paste(dv, f1, f2, collapse="")) else f <- as.formula(paste(dv, f1, collapse=""))
			if(use.miss) fm <- as.formula(paste(dv, f1, dv2, f2, collapse="")) else fm <- as.formula(paste(dv, f1, dv2, collapse=""))
			truevar <- anes$imp_enviro
			fitmodel <- bayesglm(fm, family = binomial(link="logit"), data=anes)
			true <- bayesglm(f, family = binomial(link="logit"), data=anes)
			miss <- missind$imp_enviro
			col <- cbind(1:sum(miss), as.integer(anes$imp_enviro[miss]))
			fitted <- model.matrix(true)%*%coef(true)
			ev <- eigen(vcov(fitmodel), symmetric = TRUE)
			params <- (coef(fitmodel) + (ev$vectors %*% (sqrt(ev$values) * rnorm(length(coef(fitmodel)))))[,1])
			eta <- model.matrix(fitmodel)%*%params
			Pr <- plogis(eta)
			draws <- Pr > runif(length(Pr))
			Pr <- cbind(1-Pr, Pr)[miss,]
			truematch <- mean(factor(draws, labels=c("Not important", "Important"))[miss] == anes$imp_enviro[miss])
			if(imp_meth=="pmm"){
				mark <- sapply(eta[miss], FUN=function(m){which.min(abs(m-eta[!miss]))})
				truematch <- mean(anes$imp_enviro[miss] == anes$imp_enviro[!miss][mark])
				Pr <- cbind((1-plogis(eta)), plogis(eta))[!miss,][mark,]	
			}
		}
		if(t=="or"){
			dv <- "jobs_r"
			dv2 <- dv.list[-which(is.element(dv.list, dv))]
			dv2 <- paste("+",paste(dv2, collapse="+"), collapse="")
			if(use.miss) f <- as.formula(paste(dv, f1, f2, collapse="")) else f <- as.formula(paste(dv, f1, collapse=""))
			if(use.miss) fm <- as.formula(paste(dv, f1, dv2, f2, collapse="")) else fm <- as.formula(paste(dv, f1, dv2, collapse=""))			
			truevar <- anes$jobs_r
			fitmodel <- bayespolr(fm, data=anes)
			true <- bayespolr(f, data=anes)
			miss <- missind$jobs_r
			col <- cbind(1:sum(miss), sapply(anes$jobs_r[miss], FUN=function(x){which(x==levels(anes$jobs_r[miss]))}))
			fitted <- sapply(true$zeta, FUN=function(x){x - model.matrix(true)[,-1]%*%coef(true)}, simplify=TRUE)
			ev <- eigen(vcov(fitmodel), symmetric = TRUE)
			params <- (c(coef(fitmodel), fitmodel$zeta) + (ev$vectors %*% (sqrt(ev$values) * rnorm(length(c(coef(fitmodel), fitmodel$zeta)))))[,1])
			coef <- params[1:(length(params)-6)]; zeta <- params[(length(params)-5):length(params)]
			eta <- sapply(zeta, FUN=function(x){x - model.matrix(fitmodel)[,-1]%*%coef}, simplify=TRUE)
			eta <- cbind(0, plogis(eta), 1)
			Pr <- t(diff(t(eta)))
			draws <- apply(Pr, 1, FUN = function(p) which(rmultinom(1, 1, p) == 1))
			draws <-  factor(draws, ordered=TRUE)
			truematch <- mean(factor(levels(anes$jobs)[draws], ordered=TRUE, levels=levels(anes$jobs_r))[miss] == anes$jobs_r[miss])
			Pr <- Pr[miss,]
			if(imp_meth=="pmm"){
				mark <- sapply((model.matrix(fitmodel)[,-1]%*%coef)[miss], FUN=function(m){which.min(abs(m-(model.matrix(fitmodel)[,-1]%*%coef)[!miss]))})
				truematch <- mean(anes$jobs_r[miss] == anes$jobs_r[!miss][mark])
				Pr <- cbind((1-t(diff(t(eta)))), t(diff(t(eta))))[!miss,][mark,]			
			}
		}
		if(t=="un"){
			dv <- "vote"
			dv2 <- dv.list[-which(is.element(dv.list, dv))]
			dv2 <- paste("+",paste(dv2, collapse="+"), collapse="")
			if(use.miss) f <- as.formula(paste(dv, f1, f2, collapse="")) else f <- as.formula(paste(dv, f1, collapse=""))
			if(use.miss) fm <- as.formula(paste(dv, f1, dv2, f2, collapse="")) else fm <- as.formula(paste(dv, f1, dv2, collapse=""))
			truevar <- anes$vote
			text <- capture.output(fitmodel <- multinom(fm, data=anes))
			text <- capture.output(true <- multinom(f, data=anes))
			miss <- missind$vote
			col <- cbind(1:sum(miss), sapply(anes$vote[miss], FUN=function(x){which(x==levels(anes$vote[miss]))}))
			fitted <- model.matrix(true) %*% t(coef(true))
			params <- matrix(mvrnorm(1, c(t(coef(fitmodel))), vcov(fitmodel), tol=1e-10), ncol = nrow(as.matrix(coef(fitmodel))), byrow = FALSE)
			eta <- model.matrix(fitmodel) %*% params
			exp_eta <- matrix(pmin(.Machine$double.xmax / ncol(eta), cbind(1, exp(eta))), ncol = ncol(eta) + 1)
			denom <- rowSums(exp_eta)
			Pr <- exp_eta / denom
			draws <- apply(Pr, 1, FUN = function(p) which(rmultinom(1, 1, p) == 1))
			truematch <- mean(factor(draws, labels=c("Obama", "McCain", "No vote/Other"))[miss]==anes$vote[miss])
			Pr <- Pr[miss,]
			if(imp_meth=="pmm"){
				Sigma_inv <- .MPinverse(eta)      		
      			dist <- apply(eta[miss,], 1, FUN=function(x){
					mahalanobis(eta[!miss,], x, Sigma_inv, inverted=TRUE)
				})
				draws <- apply(dist, 2, which.min)
	      		truematch <- mean(anes$vote[miss]==anes$vote[!miss][draws])
	      		Pr <- (exp_eta / denom)[!miss,][draws,]
			}		
		}
		if(t!="co") Pr <- Pr[col] else Pr <- NA
		dgp[[t]]$true <- truevar
		dgp[[t]]$miss <- miss
		dgp[[t]]$col <- col
		dgp[[t]]$model <- true
		dgp[[t]]$fitmodel <- fitmodel
		dgp[[t]]$eta <- fitted 
		dgp[[t]]$pr <- Pr
		dgp[[t]]$truematch <- truematch
		dgp[[t]]$f <- f
		dgp[[t]]$fm <- fm
		dgp[[t]]$varname <- dv
		dgp[[t]]$c <- which(is.element(colnames(anes), dv))
		dgp[[t]]$obs <- rowSums(is.na(dgp$data[,c(dgp[[t]]$c,1,2,3,4,9,10,11)]))==0
	}
	return(dgp)	
}

.midas_export <- function(dgp) {
  imp_meth <- dgp$imp_meth
  na.mat <- is.na(dgp$data)#[,c(1,1,2,2,3,4,5,6,6,7,8,9,10,11)]
  data <- dgp$truedata
  data$white <- ifelse(data$white == "White",1,0)
  data$female <- ifelse(data$female == "Female",1,0)
  data$imp_enviro <- ifelse(data$imp_enviro == "Important",1,0)
  # data$temp <- runif(nrow(data))
  data$education <- as.numeric(data$education)
  data$income <- as.numeric(data$income)
  data$jobs_r <- as.numeric(data$jobs_r) 
  # data <- model.matrix(lm(paste("temp~",paste(names(dgp$data), collapse="+"),collapse=""), data=data, na.action=na.exclude))[,-1]				
  is.na(data) <- na.mat
  
  return(data)
}

.amelia <- function(dgp, m, command="amelia", iter = NULL){
	imp_meth <- dgp$imp_meth
	na.mat <- is.na(dgp$data)[,c(1,1,2,2,3,4,5,6,6,7,8,9,10,11)]
	data <- dgp$truedata
	data$temp <- runif(nrow(data))
	data$education <- as.numeric(data$education)
	data$income <- as.numeric(data$income)
	data$jobs_r <- as.numeric(data$jobs_r) 
	data <- model.matrix(lm(paste("temp~",paste(names(dgp$data), collapse="+"),collapse=""), data=data, na.action=na.exclude))[,-1]				
	is.na(data) <- na.mat

	time1 <- proc.time() 
	if(command=="amelia") imp <- amelia(data, m=m, p2s = 0) else {
		imp <- list()
		imp$imputations <- vector("list", m)
		imp$mu <- matrix(, nrow=ncol(data), ncol=m)
		imp$covMatrices <- array(, dim = c(ncol(data), ncol(data), m))
		if(command=="mlest"){
			mvn <- mlest(data)
			for(k in 1:m){
				imp$imputations[[k]] <- data
				draws <- mvrnorm(n=nrow(data), mu=mvn$muhat, Sigma=mvn$sigmahat)
				imp$imputations[[k]][is.na(data)] <- draws[is.na(data)]
				imp$mu[,k] <- colMeans(imp$imputations[[k]])
				imp$covMatrices[,,k] <- cov(imp$imputations[[k]])
			}
		}
		
		if(command=="norm"){
			for(k in 1:m){
				prelim <- prelim.norm(as.matrix(data))
				text <- capture.output(thetahat <- em.norm(prelim))
				rngseed(round(runif(1, min=0, max=10000000)))
				draws <- imp.norm(prelim, thetahat, data)
				imp$imputations[[k]] <- data.frame(draws)
				imp$mu[,k] <- colMeans(draws)
				imp$covMatrices[,,k] <- cov(draws)
			}
		}
		
		if(command=="midas"){
		  for(j in 1:m){
		    
		    mid_imp <- read.csv(paste0("kropko/anes_midas/ANES_imp_mid_",it,"_",j,".csv"))
		    
		    names(mid_imp)[names(mid_imp) == "religion_Catholic.Orthodox"] <- "religionCatholic/Orthodox"
		    names(mid_imp)[names(mid_imp) == "religion_Atheist.Other"] <- "religionAtheist/Other"
		    names(mid_imp)[names(mid_imp) == "married_No.longer.married"] <- "marriedNo longer married"
		    names(mid_imp)[names(mid_imp) == "married_Married"] <- "marriedMarried"
		    names(mid_imp)[names(mid_imp) == "vote_McCain"] <- "voteMcCain"
		    names(mid_imp)[names(mid_imp) == "vote_No.vote.Other"] <- "voteNo vote/Other"
		    names(mid_imp)[names(mid_imp) == "imp_enviro"] <- "imp_enviroImportant"
		    names(mid_imp)[names(mid_imp) == "female"] <- "femaleFemale"
		    names(mid_imp)[names(mid_imp) == "white"] <- "whiteWhite"
		    
		    mid_imp <- mid_imp[,colnames(data)] #reorder columns
		    imp$imputations[[j]] <- mid_imp
		    imp$mu[,j] <- colMeans(mid_imp)
		    imp$covMatrices[,,j] <- cov(mid_imp)
		  }
		}
		
	}
	
	
	time2 <- proc.time()
	time <- as.numeric((time2-time1)[3])			
	class(imp$imputations) <- "list"
    
    amelia.data <- imp
    res <- list(); res$co <- res$bi <- res$or <- res$un <- list()
    res$co$mvmatch <- res$co$mvmatch.bias <- as.numeric()
    res$bi$mvmatch <- res$bi$pr.rmse <- res$bi$pr.bias <- as.numeric()
    res$or$mvmatch <- res$or$pr.rmse <- res$or$pr.bias <- as.numeric()
    res$un$mvmatch <- res$un$pr.rmse <- res$un$pr.bias <- as.numeric()
    res$time <- time
    
    amelia.imp <- list()
    
    for(k in seq_along(amelia.data$imputations)){
    	
    	amelia.imp[[k]] <- dgp$data
    	
    	#Religion (1,2) -> 1, no prob necessary 
    	g <- apply(amelia.data$imputations[[k]][,1:2], 2, FUN=function(k){k*(k>0)*(k<1)+(k>1)})
		rs <- rowSums(g)
		rs <- (rs<=1) + rs*(rs>1)
		g <- g/rs
		gzero <- 1-rowSums(g)
		g <- cbind(gzero, g)
		if(any(g<0)) {
			g[g<0] <- 0
			g <- g/rowSums(g)
		}
		draws <- apply(g, 1, FUN=function(y){which(rmultinom(1,1,y)==1)})[is.na(amelia.imp[[k]]$religion)]
		if(imp_meth=="pmm") draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=c(1,2), V2=1, k=k, missing=is.na(amelia.imp[[k]]$religion))$draws
		amelia.imp[[k]]$religion[is.na(amelia.imp[[k]]$religion)] <- levels(dgp$data$religion)[draws]
		
		#Income 6 -> 4, no prob, ordinal
		p <- data.frame(amelia.data$imputations[[k]])$income
		ncat <- 3
		p <- (p - min(p, na.rm=T))/(max(p, na.rm=T)-min(p, na.rm=T))
		p <- p*(p>=0)*(p<=1) + (p>1)
		d <- sapply(p, FUN=function(y){rbinom(1, (ncat-1), y)+1})
		draws <- d[is.na(amelia.imp[[k]]$income)]
		if(imp_meth=="pmm") draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=6, V2=4, k=k, missing=is.na(amelia.imp[[k]]$income))$draws
		amelia.imp[[k]]$income[is.na(amelia.imp[[k]]$income)] <- levels(dgp$data$income)[draws]

		#Vote (8,9) -> 6
		g <- apply(amelia.data$imputations[[k]][,8:9], 2, FUN=function(k){k*(k>0)*(k<1)+(k>1)})
		rs <- rowSums(g)
		rs <- (rs<=1) + rs*(rs>1)
		g <- g/rs
		gzero <- 1-rowSums(g)
		g <- cbind(gzero, g)
		if(any(g<0)) {
			g[g<0] <- 0
			g <- g/rowSums(g)
		}
		draws <- apply(g, 1, FUN=function(y){which(rmultinom(1,1,y)==1)})[is.na(amelia.imp[[k]]$vote)]
		if(imp_meth=="pmm"){
			draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=c(8,9), V2=6, k=k, missing=dgp$un$miss)$draws
			g <- g[.amelia.pmm(dgp=dgp, imp=amelia.data, V=c(8,9), V2=6, k=k, missing=dgp$un$miss)$mark,]
			g <- g[dgp$un$col]		
		} else g <- g[dgp$un$miss,][dgp$un$col]
		amelia.imp[[k]]$vote[is.na(amelia.imp[[k]]$vote)] <- levels(dgp$data$vote)[draws]
		res$un$mvmatch[k] <- mean(levels(dgp$un$true)[draws] == dgp$un$true[dgp$un$miss])
		res$un$pr.rmse[k] <- sqrt(mean((g - dgp$un$pr)^2))
		res$un$pr.bias[k] <- abs(mean(g)-mean(dgp$un$pr))
		
		#Time 7
		d <- amelia.data$imputations[[k]][,7]
		if(imp_meth=="pmm"){
			draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=7, V2=5, k=k, missing=dgp$co$miss)$draws
			d[dgp$co$miss] <- draws
		}
		amelia.imp[[k]]$time[is.na(amelia.imp[[k]]$time)] <- d[is.na(amelia.imp[[k]]$time)]
		res$co$mvmatch[k] <- sqrt(mean((d[dgp$co$miss] - dgp$co$true[dgp$co$miss])^2))
		res$co$mvmatch.bias[k] <- abs(mean(d[dgp$co$miss]) - mean(dgp$co$true[dgp$co$miss]))
		
		#Jobs_r 10 -> 7, prob, ordinal
		p <- data.frame(amelia.data$imputations[[k]])$jobs_r
		ncat <- 7
		p <- (p - min(p, na.rm=T))/(max(p, na.rm=T)-min(p, na.rm=T))
		p <- p*(p>=0)*(p<=1) + (p>1)
		d <- sapply(p, FUN=function(y){rbinom(1, (ncat-1), y)+1})
		pr <- t(sapply(p, FUN=function(y){dbinom(0:(ncat-1), (ncat-1), y)}, simplify=TRUE))
		draws <- d[is.na(amelia.imp[[k]]$jobs_r)]
		if(imp_meth=="pmm"){
			draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=10, V2=7, k=k, missing=dgp$or$miss)$draws
			pr <- pr[.amelia.pmm(dgp=dgp, imp=amelia.data, V=10, V2=7, k=k, missing=dgp$or$miss)$mark,]
			pr <- pr[dgp$or$col]		
		} else pr <- pr[dgp$or$miss,][dgp$or$col]
		amelia.imp[[k]]$jobs_r[is.na(amelia.imp[[k]]$jobs_r)] <- levels(dgp$data$jobs_r)[draws] 
		res$or$mvmatch[k] <- mean(levels(dgp$or$true)[draws] == dgp$or$true[dgp$or$miss])
		res$or$pr.rmse[k] <- sqrt(mean((pr - dgp$or$pr)^2))
		res$or$pr.bias[k] <- abs(mean(pr)-mean(dgp$or$pr))
		
		#imp_enviro 11 -> 8, prob, binary
		p <- data.frame(amelia.data$imputations[[k]])$imp_enviro
		ncat <- 2
		p <- p*(p>=0)*(p<=1) + (p>1)
		d <- sapply(p, FUN=function(y){rbinom(1, (ncat-1), y)+1})
		pr <- t(sapply(p, FUN=function(y){dbinom(0:(ncat-1), (ncat-1), y)}, simplify=TRUE))
		draws <- d[is.na(amelia.imp[[k]]$imp_enviro)]
		if(imp_meth=="pmm"){
			draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=11, V2=8, k=k, missing=dgp$bi$miss)$draws
			pr <- pr[.amelia.pmm(dgp=dgp, imp=amelia.data, V=11, V2=8, k=k, missing=dgp$bi$miss)$mark,]
			pr <- pr[dgp$bi$col]		
		} else pr <- pr[dgp$bi$miss,][dgp$bi$col]
		amelia.imp[[k]]$imp_enviro[is.na(amelia.imp[[k]]$imp_enviro)] <- levels(dgp$data$imp_enviro)[draws]
		res$bi$mvmatch[k] <- mean(levels(dgp$bi$true)[draws] == dgp$bi$true[dgp$bi$miss])
		res$bi$pr.rmse[k] <- sqrt(mean((pr - dgp$bi$pr)^2))
		res$bi$pr.bias[k] <- abs(mean(pr)-mean(dgp$bi$pr))	
		
		#white 13 -> 10, no prb, binary
		p <- data.frame(amelia.data$imputations[[k]])$white
		ncat <- 2
		p <- p*(p>=0)*(p<=1) + (p>1)
		d <- sapply(p, FUN=function(y){rbinom(1, (ncat-1), y)+1})
		pr <- t(sapply(p, FUN=function(y){dbinom(0:(ncat-1), (ncat-1), y)}, simplify=TRUE))
		draws <- d[is.na(amelia.imp[[k]]$white)]
		if(imp_meth=="pmm") draws <- .amelia.pmm(dgp=dgp, imp=amelia.data, V=13, V2=10, k=k, missing=is.na(amelia.imp[[k]]$white))$draws
		amelia.imp[[k]]$white[is.na(amelia.imp[[k]]$white)] <- levels(dgp$data$white)[draws]

    }
    
	for(t in c("co","bi","or","un")){
		if(t=="co"){
				model <- pool(dgp[[t]]$f, data=amelia.imp, m=5, FUN=bayesglm)
				coef <- sqrt(mean((model@coefficients - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(model@coefficients) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=model@coefficients, cov=vcov(dgp[[t]]$model))				
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- fitted(x)
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))					
				})						
		}
		if(t=="bi"){
				model <- pool(dgp[[t]]$f, data=amelia.imp, m=5, FUN=bayesglm, family=binomial(link="logit"))
				coef <- sqrt(mean((model@coefficients - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(model@coefficients) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=model@coefficients, cov=vcov(dgp[[t]]$model))
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- model.matrix(x)%*%coef(model)
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))	
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))					
				})									
		}
		if(t=="or"){
				model <- pool(dgp[[t]]$f, data=amelia.imp, m=5, FUN=bayespolr)
				r1 <- 1:(length(model@coefficients)-6); r2 <- (length(model@coefficients)-5):length(model@coefficients)
				coef <- sqrt(mean((model@coefficients[r1] - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(model@coefficients[r1]) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=model@coefficients[r1], cov=vcov(dgp[[t]]$model)[r1,r1])
				cf <- model@coefficients[r1]; zeta <- model@coefficients[r2]
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- sapply(zeta, simplify=TRUE, FUN=function(y){y - model.matrix(x)[,-1]%*%cf})
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))	
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))					
				})				
		}
		if(t=="un"){
				if(dgp$use.miss) r <- 17 else r <- 10
				text <- capture.output(model <- pool(dgp[[t]]$f, data=amelia.imp, m=5, FUN=multinom))
				coef <- sqrt((mean((t(matrix(model@coefficients, r, 2)) - coef(dgp[[t]]$model))^2))) ## 10???
				coef.bias <- abs(mean(t(matrix(model@coefficients, r, 2))) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=c(t(coef(dgp[[t]]$model))), center=model@coefficients, cov=vcov(dgp[[t]]$model))
				cf <- t(matrix(model@coefficients,r,2))
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- model.matrix(x)%*%(matrix(model@coefficients, r, 2))
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))		
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))				
				})				
		}
		res[[t]]$coef <- coef
		res[[t]]$coef.bias <- coef.bias
		res[[t]]$coef.mns <- coef.mns
		res[[t]]$fit.all <- rowMeans(fit)[1]
		res[[t]]$fit.all.bias <- rowMeans(fit)[2]
		res[[t]]$fit.av <- rowMeans(fit)[3]
		res[[t]]$fit.av.bias <- rowMeans(fit)[4]
	}
	return(res)
}

.mi <- function(dgp, m, n.iter=30, n.chains=5, est="MNL"){
	mdf <- missing_data.frame(dgp$data)
	mdf@variables[["vote"]]@estimator <- est
	mdf@variables[["religion"]]@estimator <- est
	for(im in seq_along(mdf@variables)) mdf@variables[[im]]@imputation_method <- dgp$imp_meth
	time1 <- proc.time(); imp <- mi(mdf, n.chains=n.chains, n.iter=n.iter, verbose = FALSE, max.minutes=1000); time2 <- proc.time()
	time <- as.numeric((time2-time1)[3])
	imp.complete <- complete(imp, 5)
	res <- list(); res$co <- res$bi <- res$or <- res$un <- list()
	res$time <- time
	
	for(t in c("co","bi","or","un")){
				
		if(t=="co") {
				mvmatch <- mean(sapply(imp.complete, FUN = function(d){sqrt(mean(((d[[dgp[[t]]$c]] - dgp[[t]]$true)^2)[dgp[[t]]$miss]))}))
				mvmatch.bias <- mean(sapply(imp.complete, FUN = function(d){abs(mean(d[[dgp[[t]]$c]][dgp[[t]]$miss]) - mean(dgp[[t]]$true[dgp[[t]]$miss]))}))
		}				
		if(t!="co"){
				mvmatch <- mean(sapply(imp.complete, FUN = function(d){mean((d[[dgp[[t]]$c]]==dgp[[t]]$true)[dgp[[t]]$miss])})) 
				mvmatch.bias <- NA
		}

		if(t=="co") Pr <- Pr.rmse <- Pr.bias <- NA else {
				Pr <- lapply(imp@data, FUN=function(x){
						pr <- x@variables[[dgp[[t]]$c]]@fitted
						if(t=="bi") pr <- cbind(1-pr, pr)
						pr <- pr[dgp[[t]]$miss,]
						pr <- pr[dgp[[t]]$col]
						return(pr)		
				})
				Pr.rmse <- mean(sapply(Pr, FUN=function(x){sqrt(mean((x-dgp[[t]]$pr)^2))}, simplify=TRUE))
				Pr.bias <- abs(mean(sapply(Pr, FUN=function(x){abs(mean(x)-mean(dgp[[t]]$pr))})))
		}
		if(t=="co"){
				model <- pool(dgp[[t]]$f, data=imp, m=5, FUN=bayesglm)
				coef <- sqrt(mean((model@coefficients - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(model@coefficients) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=model@coefficients, cov=vcov(dgp[[t]]$model))
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- fitted(x)
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))					
				})		
		}
		if(t=="bi"){
				model <- pool(dgp[[t]]$f, data=imp, m=5, FUN=bayesglm, family=binomial(link="logit"))
				coef <- sqrt(mean((model@coefficients - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(model@coefficients) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=model@coefficients, cov=vcov(dgp[[t]]$model))
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- model.matrix(x)%*%coef(model)
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))	
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))					
				})		
		}
		if(t=="or"){
				model <- pool(dgp[[t]]$f, data=imp, m=5, FUN=bayespolr)
				r1 <- 1:(length(model@coefficients)-6); r2 <- (length(model@coefficients)-5):length(model@coefficients)
				coef <- sqrt(mean((model@coefficients[r1] - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(model@coefficients[r1]) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=model@coefficients[r1], cov=vcov(dgp[[t]]$model)[r1,r1])
				cf <- model@coefficients[r1]; zeta <- model@coefficients[r2]
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- sapply(zeta, simplify=TRUE, FUN=function(y){y - model.matrix(x)[,-1]%*%cf})
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))	
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))					
				})			
		}
		if(t=="un"){
				if(dgp$use.miss) r <- 17 else r <- 10
				text <- capture.output(model <- pool(dgp[[t]]$f, data=imp, m=5, FUN=multinom))
				coef <- sqrt((mean((t(matrix(model@coefficients, r, 2)) - coef(dgp[[t]]$model))^2))) ## 10???
				coef.bias <- abs(mean(t(matrix(model@coefficients, r, 2))) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=c(t(coef(dgp[[t]]$model))), center=model@coefficients, cov=vcov(dgp[[t]]$model))
				cf <- t(matrix(model@coefficients,r,2))
				fit <- sapply(model@models, simplify=TRUE, FUN=function(x){
					eta <- model.matrix(x)%*%(matrix(model@coefficients, r, 2))
					fit.all <- sqrt(mean((eta - dgp[[t]]$eta)^2))
					fit.all.bias <- abs(mean(eta) - mean(dgp[[t]]$eta))
					fit.av <- sqrt(mean((eta[dgp[[t]]$obs] - dgp[[t]]$eta[dgp[[t]]$obs])^2))
					fit.av.bias <- abs(mean(eta[dgp[[t]]$obs]) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))		
					return(c(fit.all, fit.all.bias, fit.av, fit.av.bias))				
				})			
		}

		res[[t]]$mvmatch <- mvmatch
		res[[t]]$mvmatch.bias <- mvmatch.bias
		res[[t]]$pr.rmse <- Pr.rmse
		res[[t]]$pr.bias <- Pr.bias
		res[[t]]$coef <- coef
		res[[t]]$coef.bias <- coef.bias
		res[[t]]$coef.mns <- coef.mns
		res[[t]]$fit.all <- rowMeans(fit)[1]
		res[[t]]$fit.all.bias <- rowMeans(fit)[2]
		res[[t]]$fit.av <- rowMeans(fit)[3]
		res[[t]]$fit.av.bias <- rowMeans(fit)[4]
	}
	return(res)
}

.completecase <- function(dgp){
	res <- list()
	for(t in c("co","bi","or","un")){
		if(t=="co"){
				model <- bayesglm(dgp[[t]]$f, data=dgp$data)
				coef <- sqrt(mean((coef(model) - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(coef(model)) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=coef(model), cov=vcov(dgp[[t]]$model))
				eta <- model.matrix(model)%*%coef(model)		
				fit.av <- sqrt(mean((eta - dgp[[t]]$eta[dgp[[t]]$obs])^2))
				fit.av.bias <- abs(mean(eta) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))
		}
		if(t=="bi"){
				model <- bayesglm(dgp[[t]]$f, data=dgp$data, family=binomial(link="logit"))
				coef <- sqrt(mean((coef(model) - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(coef(model)) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=coef(model), cov=vcov(dgp[[t]]$model))
				eta <- model.matrix(model)%*%coef(model)		
				fit.av <- sqrt(mean((eta - dgp[[t]]$eta[dgp[[t]]$obs])^2))
				fit.av.bias <- abs(mean(eta) - mean(dgp[[t]]$eta[dgp[[t]]$obs]))
		}
		if(t=="or"){
				model <- bayespolr(dgp[[t]]$f, data=dgp$data)
				r1 <- 1:length(coef(model))
				coef <- sqrt(mean((coef(model) - coef(dgp[[t]]$model))^2))
				coef.bias <- abs(mean(coef(model)) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=coef(dgp[[t]]$model), center=coef(model), cov=vcov(dgp[[t]]$model)[r1,r1])
				cf <- coef(model); zeta <- model$zeta
				eta <- sapply(zeta, FUN=function(x){x - model.matrix(model)[,-1]%*%cf}, simplify=TRUE)
				fit.av <- sqrt(mean((eta - dgp[[t]]$eta[dgp[[t]]$obs,])^2))
				fit.av.bias <- abs(mean(eta) - mean(dgp[[t]]$eta[dgp[[t]]$obs,]))
		}
		if(t=="un"){
				if(dgp$use.miss) r <- 17 else r <- 10
				text <- capture.output(model <- multinom(dgp[[t]]$f, data=dgp$data))
				coef <- sqrt((mean((coef(model) - coef(dgp[[t]]$model))^2))) 
				coef.bias <- abs(mean(coef(model)) - mean(coef(dgp[[t]]$model)))
				coef.mns <- mahalanobis(x=c(t(coef(dgp[[t]]$model))), center=c(t(coef(model))), cov=vcov(dgp[[t]]$model))
				cf <- t(coef(model))
				eta <- model.matrix(model)%*%cf
				fit.av <- sqrt(mean((eta - dgp[[t]]$eta[dgp[[t]]$obs,])^2))
				fit.av.bias <- abs(mean(eta) - mean(dgp[[t]]$eta[dgp[[t]]$obs,]))
		}
		res[[t]]$coef <- coef
		res[[t]]$coef.bias <- coef.bias
		res[[t]]$coef.mns <- coef.mns
		res[[t]]$fit.av <- fit.av
		res[[t]]$fit.av.bias <- fit.av.bias
	}
	return(res)
}