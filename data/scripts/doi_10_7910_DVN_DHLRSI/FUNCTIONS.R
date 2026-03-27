library(JGL)
library(pheatmap)
library(network)
library(ggnet)
# ---------------------------------------------
# continuize functions -- zip : test_equSA.R
gasdev <- function(r=1){
  while(r>=1){
    v1 <- runif(1)*2-1
    v2 <- runif(1)*2-1
    r=v1*v1 + v2*v2
  }
  fac <- sqrt(-2*log(r)/r)
  gset <- v1*fac
  out = v2*fac
  return(out)
}


library(zoo)

# --------------------------------
huge_stars_joint <- function(inputlist, lambda = NULL, nlambda = 10, lambda.min.ratio = NULL,
                             stars.subsample.ratio.vec = NULL, rep.num = 20, stars.thresh = 0.1,
                             lam2init = NULL, lam1init = NULL, verbose = T, ...){
  commong <- Reduce(intersect, lapply(inputlist, colnames))
  inputlist <- lapply(inputlist, function(x){
    x[,commong]
  })
  
  d <- ncol(inputlist[[1]]) #number of genes
  if (is.null(lambda)){
    templam <- lapply(inputlist, function(x){
      x = scale(x)
      S = cor(x)
      if (is.null(lambda)) {
        if (is.null(nlambda))
          nlambda = 10
        if (is.null(lambda.min.ratio))
          lambda.min.ratio = 0.1
        lambda.max = max(max(S - diag(d)), -min(S - diag(d)))
        lambda.min = lambda.min.ratio * lambda.max
        lambda = exp(seq(log(lambda.max), log(lambda.min), length = nlambda))
      }
      return(lambda)
    })
    ltemp <- round(unlist(templam),2)
    lambda <- unique(ltemp)[order(unique(ltemp))]
    nlambda <- length(lambda)
  } else {
    nlambda <- length(lambda)
  }
  
  # --------------
  
  est.merge = list()
  for(i in 1:nlambda){
    est.merge[[i]] <- list()
    for (k in 1:length(inputlist)){
      est.merge[[i]][[k]] =  Matrix::Matrix(0,d,d) #matrix(0,d,d)#
    }
  }
  
  
  if(is.null(stars.subsample.ratio.vec)){
    stars.subsample.ratio.vec <- vector(length = length(inputlist))
    for (k in 1:length(inputlist)){
      nk <- nrow(inputlist[[k]])
      if(nk>144) stars.subsample.ratio = 10*sqrt(nk)/nk
      if(nk<=144) stars.subsample.ratio = 0.8
      stars.subsample.ratio.vec[k] <- stars.subsample.ratio
    }
  }
  
  for(i in 1:rep.num){
    if(verbose){
      mes <- paste(c("Conducting Subsampling....in progress:", floor(100*i/rep.num), "%"), collapse="")
      cat(mes, "\r")
      flush.console()
    }
    
    templist <- inputlist
    for (k in 1:length(inputlist)){
      ind.samplek = sample(c(1:nk), floor(nk*stars.subsample.ratio.vec[k]), replace=FALSE)
      templist[[k]] <- as.matrix(templist[[k]][ind.samplek,])
    }
    
    for(j in 1:nlambda){
      cat(j,"... lambda1 =", lambda[j], "\n ")
      if(is.null(lam1init)){
        tmp = try(JGL(templist, lambda1 = lambda[j], lambda2 = lam2init, return.whole.theta = T), silent = T)
      } else if(is.null(lam2init)){
        tmp = try(JGL(templist, lambda1 = lam1init, lambda2 = lambda[j], return.whole.theta = T), silent = T)
      }
      
      # if (!is.null(tmp)){ # will trigger error when tmp=NULL
      for (k in 1:length(inputlist)){
        if (!is.null(tmp$theta[[k]])){ #just in case sometimes, there's error of JGL...
          temp01 <- tmp$theta[[k]]
          est.merge[[j]][[k]] = est.merge[[j]][[k]] + abs(sign(temp01))
        }
      }
    }
    try(rm(ind.samplek,tmp, templist, temp01), silent = T)
    gc()
  }
  
  if(verbose){
    mes = "Conducting Subsampling....done.                 "
    cat(mes, "\r")
    cat("\n")
    flush.console()
  }
  
  est.variability = rep(0,nlambda)
  for(i in 1:nlambda){
    for (k in 1:length(inputlist)){
      est.merge[[i]][[k]] = est.merge[[i]][[k]]/rep.num
      est.variability[i] = est.variability[i] + 4*sum(est.merge[[i]][[k]]*(1-est.merge[[i]][[k]]))/(d*(d-1))
    }
    
  }
  # -----------
  est.opt.index = max(which.max(est.variability >= stars.thresh)[1]-1,1) # why >=? shouldn't it be <=?
  opt.lambda = lambda[est.opt.index]
  est.opt.index2 = max(which.max(est.variability <= stars.thresh)[1]-1,1) # why >=? shouldn't it be <=?
  opt.lambda2 = lambda[est.opt.index2]
  return(list(opt.lambda = opt.lambda,
              lambda = lambda,
              opt.lambda.correction = opt.lambda2))
}

#------------------------------

stars.select <- function(inputlist, nlamb = 15, rep.num = 15, lambvec = NULL){
  tt0 = proc.time()
  lam2init = 0.05
  stars.l1 <- huge_stars_joint(inputlist, lambda = lambvec, nlambda = nlamb, lambda.min.ratio = NULL, stars.subsample.ratio.vec = NULL,
                               rep.num = rep.num, stars.thresh = 0.1, lam2init = lam2init, lam1init = NULL, verbose = T)
  stars.l2 <- huge_stars_joint(inputlist, lambda = lambvec, nlambda = nlamb, lambda.min.ratio = NULL, stars.subsample.ratio.vec = NULL,
                               rep.num = rep.num, stars.thresh = 0.1, lam2init = NULL, lam1init = stars.l1$opt.lambda.correction, verbose = T)
  
  while (abs(stars.l2$opt.lambda.correction - lam2init) > 0.01){
    lam2init = stars.l2$opt.lambda.correction
    stars.l1 <- huge_stars_joint(inputlist, lambda = lambvec, nlambda = nlamb, lambda.min.ratio = NULL, stars.subsample.ratio.vec = NULL,
                                 rep.num = rep.num, stars.thresh = 0.1, lam2init = lam2init, lam1init = NULL, verbose = T)
    stars.l2 <- huge_stars_joint(inputlist, lambda = lambvec, nlambda = nlamb, lambda.min.ratio = NULL, stars.subsample.ratio.vec = NULL,
                                 rep.num = rep.num, stars.thresh = 0.1, lam2init = NULL, lam1init = stars.l1$opt.lambda.correction, verbose = T)
  }
  
  res = try(JGL(inputlist, lambda1 = stars.l1$opt.lambda.correction, lambda2 = stars.l2$opt.lambda.correction, return.whole.theta = T), silent = T)
  partcorr = lapply(res$theta, prec2partialcorr)
  tt1 = proc.time()
  return(list(stars.l1 = stars.l1,
              stars.l2 = stars.l2,
              partcorr = partcorr,
              runtime = tt1 - tt0))
}


# =========================================
# COUNT SIMULATION METHOD REFERING TO CFGL
generate2Sigma <- function(nblk=10, ni=25, u=c(-60:60)/100 , ud = c(-100:-60, 60:100)/100 ){
  blklist <- list()
  sigma <- matrix(0, nrow = 0, ncol = ni*nblk)
  # sigma <- matrix(0, nrow = ni*nblk, ncol = ni*nblk)
  for (b in 1:nblk){
    blklist[[b]] <- generateBlki(ni=25, ud=ud)
    zeroleft <- matrix(0, nrow = ni, ncol = (b-1)*ni)
    zeroright <- matrix(0, nrow = ni, ncol = (nblk-b)*ni)
    temp <- blklist[[b]]$sigmam
    sigma <- rbind(sigma, cbind(zeroleft, temp, zeroright))
  }
  
  mm = rep(paste("M",1:nblk, sep = ""), each = ni)
  gg = paste("G",1:ni, sep = "")
  gnames = paste(mm, gg, sep = "")
  rownames(sigma) <- gnames
  colnames(sigma) <- gnames
  
  # generate simular structure but varied weight
  sigma2 <- matrix(0, nrow = 0, ncol = ni*nblk)
  for (b in 1:nblk){
    
    for (i in 1:20){
      # cat(i,".. \n")
      mati1 <- blklist[[b]]$Bm
      ustar <- matrix(sample(u, ni*ni, replace = T), nrow = ni)
      ustar2 <- (ustar + t(ustar))/2
      scrmat <- abs(sign(mati1))
      ustar <- ustar2*scrmat
      diag(ustar) <-0
      mati1 <- mati1 + ustar
      if (is.positive.definite(mati1)){
        break
      }
    }
    binv = solve(mati1)
    bii <- diag(binv)
    sigmam <- binv
    for (i in 1:ni){
      for (j in 1:ni){
        sigmam[i,j] <- binv[i,j]/sqrt(bii[i]*bii[j])
      }
    }
    
    zeroleft <- matrix(0, nrow = ni, ncol = (b-1)*ni)
    zeroright <- matrix(0, nrow = ni, ncol = (nblk-b)*ni)
    temp <- sigmam
    sigma2 <- rbind(sigma2, cbind(zeroleft, temp, zeroright))
  }
  rownames(sigma2) <- gnames
  colnames(sigma2) <- gnames
  
  sigma.list <- list(sigma1 = sigma, sigma2= sigma2)
  return(sigma.list)
}



generateBlki <- function(ni, ud){
  mati <- matrix(0, ni, ni)
  for (i in 1:ni){
    for (j in i:ni){
      mati[i,j] <- ifelse(i!=j & runif(1) >0.6, sample(ud, 1),  #half chance: coexpression of i and j. prob in [-1,-0.6] U [0.6,1]
                          ifelse(i==j,1,0))
    }
  }
  mati0 = mati + t(mati) - diag(1, nrow = ni, ncol = ni)
  # eg = eigen(mati0)
  # str(eg)
  # delta = min(eg$values[eg$values>0])
  mati1 <- mati0 + diag(1, nrow = ni, ncol = ni)
  for (i in 1:20){
    # cat(i,".. \n")
    if (is.positive.definite(mati1)){
      mati1 <- mati1
      break
    } else {
      mati1 <- mati1 + diag(1, nrow = ni, ncol = ni)
    }
  }
  
  binv = solve(mati1)
  bii <- diag(binv)
  sigmam <- binv
  for (i in 1:ni){
    for (j in 1:ni){
      sigmam[i,j] <- binv[i,j]/sqrt(bii[i]*bii[j])
    }
  }
  res <- list(sigmam = sigmam,
              Bm = mati1)
  return(res)
}

generateDiffSigma <- function(nivec.list=list(nivec= c(10,15, rep(25,9)),nivec2 = rep(25,10)) , u=c(-60:60)/100 , ud = c(-100:-60, 60:100)/100 ){
  sigma.list <- list()
  for(ss in 1:length(nivec.list)){
    blklist <- list()
    sigma <- matrix(0, nrow = 0, ncol = sum(nivec.list[[ss]]))
    # sigma <- matrix(0, nrow = ni*nblk, ncol = ni*nblk)
    nblk <- length(nivec.list[[ss]])
    for (b in 1:nblk){
      ni <- nivec.list[[ss]][b]
      blklist[[b]] <- generateBlki(ni=ni, ud=ud)
      zeroleft <- matrix(0, nrow = ni, ncol = sum(nivec.list[[ss]][0:(b-1)]))
      zeroright <- matrix(0, nrow = ni, ncol = ifelse(b<nblk,sum(nivec.list[[ss]][(b+1):nblk]),0))
      temp <- blklist[[b]]$sigmam
      sigma <- rbind(sigma, cbind(zeroleft, temp, zeroright))
    }
    gnames = paste("gene",1:sum(nivec.list[[ss]]), sep = "")
    # gnames = NULL
    # for (i in 1:nblk){
    #   for (j in 1:nivec.list[ss][i]){
    #     gnames <- c(gnames, paste("M",i,"G",j, sep = ""))
    #   }
    # }
    rownames(sigma) <- gnames
    colnames(sigma) <- gnames
    sigma.list[[ss]] <- sigma
  }
  
  return(sigma.list)
}

CountMap2 <- function(sigma, ngene, n, a10 = 3, b10=2, a20 = 1, b20 = 10, a3=2 , b3=1){
  precision1 <- solve(sigma)
  mu <- rep(0, ngene)
  adj <- abs(sign(precision1))
  diag(adj) <- 0
  x <- mvtnorm::rmvnorm(n, mu, sigma)
  y <- x
  theta.true <- matrix(nrow = n, ncol = ngene)
  z.true <- matrix(nrow = n, ncol = ngene)
  
  # ---------------------
  for (j in 1:ngene) {
    dat <- x[, j]
    mu_v <- mean(dat)
    sd_v <- sd(dat)
    p_v <- pnorm(dat, mu_v, sd_v)
    
    # ---------------------
    # simulate sc from zip -- 1 gene
    # 3. alpha, beta, thetaij
    alphai <- rgamma(1, shape = a10, rate = b10)
    betai <- rgamma(1, shape = a20, rate = b20)
    if (any(alphai / betai >=2000)){
      alphai <- rgamma(1, shape = a10, rate = b10)
      betai <- rgamma(1, shape = a20, rate = b20)
    }
    thetaij <- rgamma(n, shape = alphai, rate = betai)
    
    scdat <- sapply(thetaij, rpois, n=1)
    
    # 1. draw from pij: THE PROB OF NOT DROPOUT
    pij <- rbeta(n, shape1 = a3 , shape2 = b3)
    # pij <- rbeta(n, shape1 = a3 +1, shape2 = b3)
    
    # 2. random sample zij
    # zij <- rbinom(n,1, prob = 1-(pij*exp(-thetaij))/(pij*exp(-thetaij) + (1-pij)))
    zij <- rbinom(n,1, prob = pij)
    ytemp <- quantile(scdat, p_v)
    thetatemp <- quantile(thetaij, p_v)
    y[,j] <- ytemp*zij
    z.true[,j] <- zij
    theta.true[,j] <- thetatemp
  }
  colnames(y) <- paste("gene",1:ngene, sep = "")
  rownames(y) <- paste("sample", 1:n, sep = "")
  rownames(adj) <- paste("gene",1:ngene, sep = "")
  colnames(adj) <- paste("gene",1:ngene, sep = "")
  colnames(theta.true) <- paste("gene",1:ngene, sep = "")
  rownames(theta.true) <- paste("sample", 1:n, sep = "")
  
  result <- list()
  result$data = y
  result$Adj = adj
  result$theta = theta.true #HERE, THETA MEANS THE TRUE MEAN EXPRESSION LEVEL, NOT THE PRECISION MATRIX
  result$zij = z.true
  result$sigma <- sigma
  result$precision <- precision1
  return(result)
}

screenByPosteriorZ <- function(x,imp.rate=0.6){
  x[is.na(x)] <- 1
  y=x[x<1]
  if (length(y)>2){
    scrvec <- x > quantile(y,imp.rate)
  } else {
    scrvec <- rep(T, length(x))
  }
  return(scrvec)
}


# Then generate expression data for condition 1 using N(0, sigma1)
# sigma.list <- sigma.list.diffSdiffW
getCountList <- function(sigma.list, nvec = c(500, 500), ngene = NULL, a3 = 1.07, b3 = 2.28){
  if (is.null(ngene)){
    ngene = ncol(sigma.list[[1]])
  }
  count.list <- list()
  for (c in 1:length(sigma.list)){
    counti <- CountMap2(sigma = sigma.list[[c]], ngene = ngene, n=nvec[c], a3 = a3, b3 = b3)
    count.list[[c]] <- counti
  }
  return(count.list)
}


generateBlki <- function(ni, ud= c(-100:-60, 60:100)/100){
  mati <- matrix(0, ni, ni)
  for (i in 1:ni){
    for (j in i:ni){
      mati[i,j] <- ifelse(i!=j & runif(1) >0.6, sample(ud, 1),  #half chance: coexpression of i and j. prob in [-1,-0.6] U [0.6,1]
                          ifelse(i==j,1,0))
    }
  }
  mati0 = mati + t(mati) - diag(1, nrow = ni, ncol = ni)
  # eg = eigen(mati0)
  # str(eg)
  # delta = min(eg$values[eg$values>0])
  mati1 <- mati0 + diag(1, nrow = ni, ncol = ni)
  for (i in 1:20){
    # cat(i,".. \n")
    if (matrixcalc::is.positive.definite(mati1)){
      mati1 <- mati1
      break
    } else {
      mati1 <- mati1 + diag(1, nrow = ni, ncol = ni)
    }
  }
  
  binv = solve(mati1)
  bii <- diag(binv)
  sigmam <- binv
  for (i in 1:ni){
    for (j in 1:ni){
      sigmam[i,j] <- binv[i,j]/sqrt(bii[i]*bii[j])
    }
  }
  res <- list(sigmam = sigmam,
              Bm = mati1)
  return(res)
}
# blk1 <- generateBlki(ni=25)
check_ident_list <- function(xlist){
  if (length(unique(unlist(lapply(xlist, length)))) == 1){ #length of sublists are the same
    nsame = 0 # identical pairs of sublist
    for (cc in 1:length(xlist)){
      for (dd in 1:length(xlist)){
        if (cc < dd){
          nsame = nsame + all(xlist[[cc]] == xlist[[dd]])
        }
      }
    }
    if (nsame == choose(length(xlist), 2)){
      y = T
    } else {
      message("The elements in sublists are not all identical... \n")
      y = F
    }
  } else {
    message("The length of list elements are different... \n")
    y = F
  }
  return(y)
}

generateSigmaList <- function(nivec.list, u=c(-60:60)/100 , ud = c(-100:-60, 60:100)/100,
                              structure = "Identical S, Identical W", diffblk = NULL){
  # structure S, Weight W: "Identical S, Identical W", "Identical S, Diff W", "Diff S, Identical W", "Diff S, Diff W"
  # if using the structure "Diff S, Identical W", setting some structure different while keep the rest of structure and weights the same, specify diffblk = list(diffblk1=1, diffblk2=c(1,2),...)
  
  sigma.list <- list()
  # check if identical structure
  checkI <- check_ident_list(nivec.list)
  if (structure =="Identical S, Identical W"){
    if (checkI){ #------------------------------------------- if structures are identical
      blklist <- list()
      sigma <- matrix(0, nrow = 0, ncol = sum(nivec.list[[1]]))
      nblk <- length(nivec.list[[1]])
      for (b in 1:nblk){
        ni <- nivec.list[[1]][b]
        blklist[[b]] <- generateBlki(ni=ni, ud=ud)
        zeroleft <- matrix(0, nrow = ni, ncol = sum(nivec.list[[1]][0:(b-1)]))
        zeroright <- matrix(0, nrow = ni, ncol = ifelse(b<nblk,sum(nivec.list[[1]][(b+1):nblk]),0))
        temp <- blklist[[b]]$sigmam
        sigma <- rbind(sigma, cbind(zeroleft, temp, zeroright))
      }
      gnames = paste("gene",1:sum(nivec.list[[1]]), sep = "")
      rownames(sigma) <- gnames
      colnames(sigma) <- gnames
      for(ss in 1:length(nivec.list)){
        sigma.list[[ss]] <- sigma
      }
    } else {
      message("nivec.list and the selected network structure do NOT match ...\n")
      break()
    }
    
  } else if (structure =="Identical S, Diff W"){
    if (checkI){ #------------------------------------- if structures are identical
      for(ss in 1:length(nivec.list)){
        blklist <- list()
        sigma <- matrix(0, nrow = 0, ncol = sum(nivec.list[[ss]]))
        # sigma <- matrix(0, nrow = ni*nblk, ncol = ni*nblk)
        nblk <- length(nivec.list[[ss]])
        for (b in 1:nblk){
          ni <- nivec.list[[ss]][b]
          blklist[[b]] <- generateBlki(ni=ni, ud=ud)
          zeroleft <- matrix(0, nrow = ni, ncol = sum(nivec.list[[ss]][0:(b-1)]))
          zeroright <- matrix(0, nrow = ni, ncol = ifelse(b<nblk,sum(nivec.list[[ss]][(b+1):nblk]),0))
          temp <- blklist[[b]]$sigmam
          sigma <- rbind(sigma, cbind(zeroleft, temp, zeroright))
        }
        gnames = paste("gene",1:sum(nivec.list[[ss]]), sep = "")
        rownames(sigma) <- gnames
        colnames(sigma) <- gnames
        sigma.list[[ss]] <- sigma
      }
    } else {
      message("nivec.list and the selected network structure do NOT match ...\n")
      break()
    }
    
  } else if (structure =="Diff S, Identical W"){
    # for the part that structures are the same, weights are the same
    # assume the first ndiff rows have different structure.
    blklist <- list()
    sigma <- matrix(0, nrow = 0, ncol = sum(nivec.list[[1]]))
    nblk <- length(nivec.list[[1]])
    for (b in 1:nblk){
      ni <- nivec.list[[1]][b]
      blklist[[b]] <- generateBlki(ni=ni, ud=ud)
      zeroleft <- matrix(0, nrow = ni, ncol = sum(nivec.list[[1]][0:(b-1)]))
      zeroright <- matrix(0, nrow = ni, ncol = ifelse(b<nblk,sum(nivec.list[[1]][(b+1):nblk]),0))
      temp <- blklist[[b]]$sigmam
      sigma <- rbind(sigma, cbind(zeroleft, temp, zeroright))
    }
    gnames = paste("gene",1:sum(nivec.list[[1]]), sep = "")
    rownames(sigma) <- gnames
    colnames(sigma) <- gnames
    sigma.list[[1]] <- sigma
    
    for(ss in 2:length(nivec.list)){ # from the second matrix, only change the diffblk[[ss]] block part
      blklist <- list()
      sigma2 <- sigma
      nblk <- length(nivec.list[[ss]])
      temps <- matrix(0, nrow = 0, ncol = sum(nivec.list[[ss]]))
      for (b in diffblk[[ss]]){
        ni <- nivec.list[[ss]][b]
        blklist[[b]] <- generateBlki(ni=ni, ud=ud)
        zeroleft <- matrix(0, nrow = ni, ncol = sum(nivec.list[[ss]][0:(b-1)]))
        zeroright <- matrix(0, nrow = ni, ncol = ifelse(b<nblk,sum(nivec.list[[ss]][(b+1):nblk]),0))
        temp <- blklist[[b]]$sigmam
        temps <- cbind(zeroleft, temp, zeroright)
        diffid <- (sum(nivec.list[[ss]][0:(b-1)])+1) : sum(nivec.list[[ss]][0:b])
        sigma2[diffid,] <- temps
      }
      sigma.list[[ss]] <- sigma2
    }
    
    
  } else if (structure == "Diff S, Diff W"){
    for(ss in 1:length(nivec.list)){
      blklist <- list()
      sigma <- matrix(0, nrow = 0, ncol = sum(nivec.list[[ss]]))
      # sigma <- matrix(0, nrow = ni*nblk, ncol = ni*nblk)
      nblk <- length(nivec.list[[ss]])
      for (b in 1:nblk){
        ni <- nivec.list[[ss]][b]
        blklist[[b]] <- generateBlki(ni=ni, ud=ud)
        zeroleft <- matrix(0, nrow = ni, ncol = sum(nivec.list[[ss]][0:(b-1)]))
        zeroright <- matrix(0, nrow = ni, ncol = ifelse(b<nblk,sum(nivec.list[[ss]][(b+1):nblk]),0))
        temp <- blklist[[b]]$sigmam
        sigma <- rbind(sigma, cbind(zeroleft, temp, zeroright))
      }
      gnames = paste("gene",1:sum(nivec.list[[ss]]), sep = "")
      rownames(sigma) <- gnames
      colnames(sigma) <- gnames
      sigma.list[[ss]] <- sigma
    }
  } else {
    message("Please choose the joint network structure from: 'Identical S, Identical W', 'Identical S, Diff W', 'Diff S, Identical W', 'Diff S, Diff W'")
  }
  return(sigma.list)
}


trunc_precision <- function(z, threshold=0.01){
  z[abs(z) <threshold] <-0
  return(z)
}

fisher_transform <- function(x){
  1/2*log((1+x)/(1-x))
}

trunc_pval <- function(x, p0=0.001){
  y = x
  y[x <  p0] <- 1
  y[x >= p0] <- 0
  return(y)
}

prec2partialcorr <- function(mat){
  # mat should be a precision matrix
  denom <- diag(mat)
  n <- ncol(mat)
  out <- mat
  for (i in 1:n){
    for (j in 1:n){
      out[i,j] <- - mat[i,j]/sqrt(denom[i]*denom[j])
    }
  }
  diag(out) <- 0
  return(out)
}

screen_partcorr <- function(m, nsample = 200, alpha=0.05){
  m[] <- vapply(m, fisher_transform, numeric(1))
  screenmat <- m
  n <- ncol(m)
  for (i in 1:n){
    for (j in 1:n){
      zk <- sum(abs(sign(m[i,-j])))
      screenmat[i,j] <- abs(m[i,j])*sqrt(nsample - zk - 3) > qnorm(1-alpha/2)
    }
  }
  return(screenmat)
}


tuning_select <- function(mat.list.t.gau, lam1 = 0.1, lam2 = 0.1){
  res.S <- lapply(mat.list.t.gau, cov)
  res.jgl <- JGL(mat.list.t.gau, lambda1 = lam1, lambda2 = lam2, return.whole.theta = T)
  aic <- 0; bic <- 0; ebic <- 0; ebic1 <-0; ebic2 <-0;
  p <- ncol(res.jgl$theta[[1]])
  for (k in 1:length(mat.list.t.gau)){
    nk <- nrow(mat.list.t.gau[[k]])
    traceST <- sum(diag(res.S[[k]] %*% res.jgl$theta[[k]]))
    Ek <- sum(! res.jgl$theta[[k]] == 0)
    detT <- det(res.jgl$theta[[k]])
    aick <- (traceST*nk - log(detT)*nk + 2*Ek)/1e4
    aic <- aic + aick
    bick <- (traceST*nk - log(detT)*nk + log(nk)*Ek)/1e4
    bic <- bic + bick
    ebick <- (traceST*nk - log(detT)*nk + log(nk)*Ek + 2*0.5*Ek*log(p))/1e4
    ebic <- ebic + ebick
    ebick1 <- (traceST*nk - log(detT)*nk + log(nk)*Ek + 2*0.25*Ek*log(p))/1e4
    ebic1 <- ebic1 + ebick1
    ebick2 <- (traceST*nk - log(detT)*nk + log(nk)*Ek + 2*1*Ek*log(p))/1e4
    ebic2 <- ebic2 + ebick2
  }
  res <- c(lam1, lam2, aic, bic, ebic, ebic1, ebic2)
  return(list(res,
              res.jgl))
}
# res.raw <- tuning_select(mat.list.t.gau = list(countlist.1[[1]]$data, countlist.1[[2]]$data), lam1 = lam1select/1000, lam2 = lam2select/1000)


library(precrec)
myROCPRC <- function(parcorr, trueadj, methodlist, curve = "ROC", tuningparam="l1=,l2="){
  trueadj = abs(sign(trueadj))
  parcorr = abs(parcorr)
  if (all(colnames(trueadj) != colnames(parcorr))){
    parcorr <- parcorr[rownames(trueadj), colnames(trueadj)]
  }
  testc <- evalmod(scores = c(parcorr), labels = c(trueadj))
  autoplot(testc)
}


getROCxy <- function(trueadj= parcorr.true.trunc[[1]], parcorr, method="", PRC=F ){
  trueadj = abs(sign(trueadj))
  parcorr = abs(parcorr)
  if (all(colnames(trueadj) != colnames(parcorr))){
    parcorr <- parcorr[rownames(trueadj), colnames(trueadj)]
  }
  testc <- evalmod(scores = c(parcorr), labels = c(trueadj))
  if (PRC){
    res = cbind.data.frame(testc$prcs[[1]]$x, testc$prcs[[1]]$y)
    colnames(res) <- c("Recall","Precision")
  } else{
    res = cbind.data.frame(testc$rocs[[1]]$x, testc$rocs[[1]]$y)
    colnames(res) <- c("FPR","TPR")
  }
  res$method <- method
  return(res)
}

eval_partcorrList <- function(plist, partcorr.true.trunc, methods, methods.order, psi_scores = NULL, psi_methods = NULL){
  trueadj.list <- lapply(partcorr.true.trunc, function(x){
    y = abs(sign(trunc_precision(x))) 
    return(y)
  }) 
  
  if (!is.null(psi_scores)){
    psilists <- lapply(psi_scores, function(xx){
      x2 = list()
      for (xxx in 1:dim(xx[[1]])[1]){
        temp <- matrix(data = 0, nrow = dim(xx[[1]])[2], ncol = dim(xx[[1]])[2])
        for (xi in 1:dim(xx[[3]])[1]){
          k1 = xx[[3]][xi,1]
          k2 = xx[[3]][xi,2]
          temp[k1, k2] <- xx[[3]][xi,xxx+2]
          temp[k2, k1] <- xx[[3]][xi,xxx+2]
        }
        x2[[xxx]] <- temp
      }
      return(x2)
    })
    plist2 <- do.call(c, list(plist, psilists))
    aucvec <- unlist(lapply(plist2, function(z){
      auroc(score = c(abs(unlist(z))), bool = c(unlist(trueadj.list)))
    }))
    dts3 <- data.frame(AUC = aucvec, methods = factor(c(methods, psi_methods), levels = c(methods.order, psi_methods)))
    
    PRROC_pr <- function(z, trueadj.list){ 
      s1 = c(abs(unlist(z)))[c(unlist(trueadj.list))==1]
      s0 = c(abs(unlist(z)))[c(unlist(trueadj.list))==0]
      pr <- pr.curve(scores.class0 = s1,
                     scores.class1 = s0)
      pr$auc.integral
    }
    auprc <- unlist(lapply(plist2, PRROC_pr, trueadj.list = trueadj.list))
    dts4 <- data.frame(AUPRC = auprc, methods = factor(c(methods, psi_methods), levels = c(methods.order, psi_methods)))
  } else {
    aucvec <- unlist(lapply(plist, function(z){
      genes <- colnames(z[[1]])
      ttt <- lapply(trueadj.list, function(t0){
        return(t0[genes, genes])
      })
      auroc(score = c(abs(unlist(z))), bool = c(unlist(ttt)))
    }))
    dts3 <- data.frame(AUC = aucvec, methods = factor(methods, levels = methods.order ))
    
    PRROC_pr <- function(z, trueadj.list){ 
      genes <- colnames(z[[1]])
      ttt <- lapply(trueadj.list, function(t0){
        return(t0[genes, genes])
      })
      s1 = c(abs(unlist(z)))[c(unlist(ttt))==1]
      s0 = c(abs(unlist(z)))[c(unlist(ttt))==0]
      pr <- pr.curve(scores.class0 = s1,
                     scores.class1 = s0)
      pr$auc.integral
    }
    auprc <- unlist(lapply(plist, PRROC_pr, trueadj.list = trueadj.list))
    dts4 <- data.frame(AUPRC = auprc, methods = factor(methods, levels = methods.order))
  }
  
  
  
  ssev <- unlist(lapply(plist, getSSEacross, trueadj = partcorr.true.trunc))
  dts1 <- data.frame(SSE = ssev, methods = factor(methods, levels = methods.order))
  
  PCORv <- unlist(lapply(plist, getPCORacross, trueadj = partcorr.true.trunc))
  dts2 <- data.frame(Pearson = PCORv, methods = factor(methods, levels = methods.order))
  return(list(auc = dts3,
              prc = dts4,
              sse = dts1,
              pearson = dts2))
}

getSSEacross <- function(trueadj = parcorr.true.trunc, parcorr){
  if (is.list(trueadj)){
    genes <- colnames(parcorr[[1]])
    ttt <- lapply(trueadj, function(t0){
      return(t0[genes, genes])
    })
    sse = sum((unlist(ttt) - unlist(parcorr))^2)
  } else {
    genes <- colnames(parcorr)
    ttt <- trueadj[genes, genes]
    sse = sum((c(ttt) - c(parcorr))^2)
  }
  return(sse)
}

getPCORacross <- function(trueadj = parcorr.true, parcorr){
  if (is.list(trueadj)){
    genes <- colnames(parcorr[[1]])
    ttt <- lapply(trueadj, function(t0){
      return(t0[genes, genes])
    })
    p = cor(unlist(ttt) , unlist(parcorr))
  } else {
    genes <- colnames(parcorr)
    ttt <- trueadj[genes, genes]
    p = cor(c(ttt) , c(parcorr))
  }
  return(p)
}



# ==============================
instPkgPlusDeps <- function(pkg, install = FALSE,
                            which = c("Depends", "Imports", "LinkingTo"),
                            inc.pkg = TRUE) {
  stopifnot(require("tools")) ## load tools
  ap <- available.packages() ## takes a minute on first use
  ## get dependencies for pkg recursively through all dependencies
  deps <- package_dependencies(pkg, db = ap, which = which, recursive = TRUE)
  ## the next line can generate warnings; I think these are harmless
  ## returns the Priority field. `NA` indicates not Base or Recommended
  pri <- sapply(deps[[1]], packageDescription, fields = "Priority")
  ## filter out Base & Recommended pkgs - we want the `NA` entries
  deps <- deps[[1]][is.na(pri)]
  ## install pkg too?
  if (inc.pkg) {
    deps = c(pkg, deps)
  }
  ## are we installing?
  if (install) {
    install.packages(deps)
  }
  deps ## return dependencies
}


# -----------------------
# soft threshold
softThreshold <- function(x, thld = 0.5){
  z= sign(x)* max(0, abs(x) - thld)
  return(z)
}

# preprocess = T
# eps = 1e-12
# normfac = 1
# insweep = 20
# tol = 1e-4
# decfac = 0.7
# min_count=1
# min_cells=1
# verbose = F

mcImpute.R <- function(data, preprocess = T,
                       eps = 1e-12, normfac = 1, insweep = 20,
                       tol = 1e-4, decfac = 0.7, min_count=1,
                       min_cells=1, verbose = F){
  # data: samples by genes
  # don't need to change the other constants
  # M = data >0
  if (any(is.na(data))){
    message("NA values exist in the data matrix.")
    break
  }
  if (preprocess){
    # Removing BAD genes, median normalization and log-transformation
    gene.filter = colSums(data >= min_count) > min_cells
    data.filterg = data[,gene.filter]
    libsize = rowSums(data.filterg)
    data.filterg.norm = t(apply(data.filterg, 1, function(x){
      x/sum(x)*median(libsize)
    }))
    y = log2(data.filterg.norm + 1)
  } else {
    y = data
  }
  alpha = 1.1*normfac
  xinit = matrix(0, nrow = nrow(y), ncol = ncol(y))
  x = xinit
  M = y > 0
  lambda.init = decfac*max(abs(y))
  lambda = lambda.init
  
  f_current = norm(y - M*x,"f") + lambda*norm(x)
  # norm(y - M*x,"f")
  # lambda*norm(x)
  
  while (lambda > lambda.init*tol){
    if (verbose){
      cat("Lambda = ", lambda, "\n")
    }
    for (ins in 1:insweep){
      f_previous = f_current
      B = x + (1/alpha)*M*(y - M*x)
      USV = svd(B)
      # USV$d
      # USV$u
      # round(t(USV$u) %*% USV$u, 1)
      # USV$v
      # round(t(USV$v) %*% USV$v, 1)
      # round(B,1)
      # round(USV$u %*% diag(USV$d) %*% t(USV$v),1)
      
      s = sapply(USV$d, softThreshold, thld = lambda/(2*alpha))
      S = diag(s)
      X = USV$u %*% S %*% t(USV$v)
      X[X<0] = 0
      x =X
      f_current = norm(y - M*x,"f") + lambda*norm(x)
      if (abs(f_current-f_previous)/ abs(f_current+f_previous) < tol){
        break
      }
    }
    if (norm(y - M*x,"f") < eps){
      break
    }
    lambda = decfac*lambda
  }
  
  if (preprocess){
    resX = round(2^x - 1)
    colnames(resX) = colnames(data)[gene.filter]
  } else {
    resX = round(x)
    colnames(resX) = colnames(data)
  }
  rownames(resX) = rownames(data)
  return(resX)
}

# https://blog.mbq.me/augh-roc/
auroc <- function(score, bool) {
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

PRROC_pr <- function(z, trueadj.list){
  s1 = c(abs(unlist(z)))[c(unlist(trueadj.list))==1]
  s0 = c(abs(unlist(z)))[c(unlist(trueadj.list))==0]
  pr <- pr.curve(scores.class0 = s1,
                 scores.class1 = s0)
  pr$auc.integral
}



# =======================
# a0002, plot networks
library(network)
library(ggnet)
library(cowplot)
library(ggnetwork)
plot_onenet <- function(parcorr, mode = "circle",gname="", lsize=2.5,
                        nodecolor="grey", family.vec = 0, circlenet = F){
  # mode = "spring" /
  net1 <- network(parcorr, ignore.eval = F, names.eval="weights")
  set.edge.attribute(net1, "size", abs(net1 %e% "weights")*5)
  set.edge.attribute(net1, "color", ifelse(net1 %e% "weights"> 0, "#CC6677","#44AA99"))
  net1 %v% "family" <- family.vec
  net1 %v% "importance"  <- abs(rowSums(abs(parcorr)))*2
  net1 %v% "nconnect"  <- abs(rowSums(abs(parcorr)>0))/10
  if (circlenet){
    ti <- ggnet2(net1, label = T, mode = mode, edge.size = "size",
                 edge.color = "color", node.size = "nconnect", label.size = lsize,
                 color = nodecolor, legend.position = "none") + ggtitle(gname) #+ theme(legend.position = "none")
  } else {
    ti = ggplot(net1, aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_edges(color = "grey70") +
      geom_nodes(aes(color = family, size = importance)) +
      geom_nodetext(aes(label = vertex.names),
                    fontface = "bold", size = 2.5) +
      theme_blank( )+ theme(legend.position = "none") + ggtitle(gname)
  }
  return(ti)
}


# --------------------
AIC_select <- function(mat.list.t.gau, lam1 = 0.1, lam2 = 0.1, returnJGL = F){
  res.S <- lapply(mat.list.t.gau, cov)
  res.jgl <- JGL(mat.list.t.gau, lambda1 = lam1, lambda2 = lam2, return.whole.theta = T)
  aic <- 0; bic <- 0; ebic <- 0
  p <- ncol(res.jgl$theta[[1]])
  for (k in 1:length(mat.list.t.gau)){
    nk <- nrow(mat.list.t.gau[[k]])
    traceST <- sum(diag(res.S[[k]] %*% res.jgl$theta[[k]]))
    Ek <- sum(! res.jgl$theta[[k]] == 0)
    detT <- det(res.jgl$theta[[k]])
    aick <- (traceST*nk - log(detT)*nk + 2*Ek)/1e4
    aic <- aic + aick
    # bick <- (traceST*nk - log(detT)*nk + log(nk)*Ek)/1e4
    # bic <- bic + bick
    # ebick <- (traceST*nk - log(detT)*nk + log(nk)*Ek + 4/2*Ek*log(p))/1e4
    # ebic <- ebic + ebick
  }
  res <- c(lam1, lam2, aic)
  if (returnJGL){
    return(list(res,
                res.jgl))
  }else{
    return(res)
  }
}

getJGLTuningParamResult <- function(GauList, l1vec = NULL, l2vec = NULL){
  # Gaulist: samples by genes
  if (is.null(l1vec)){
    l1vec = 1:5/20
  }
  if (is.null(l2vec)){
    l2vec = 1:5/50
  }
  aic.vec = NULL
  jgl.res = NULL
  for (l1 in l1vec){
    for (l2 in l2vec){
      cat("Tuning parameter l1=", l1,", l2=", l2, "\n")
      t1 <- proc.time()
      tps <- AIC_select(mat.list.t.gau = GauList, lam1 = l1, lam2 = l2, returnJGL = T)
      t2 <- proc.time()
      message(t2 -t1, "\n")
      
      if (is.null(aic.vec)){
        jgl.res <- tps[[2]]
      } else if (tps[[1]][3] < min(aic.vec)){
        jgl.res <- tps[[2]]
      }
      aic.vec <- c(aic.vec, tps[[1]][3])
    }
  }
  return(list(aic.vec = aic.vec,
              jgl.res = jgl.res))
}

JGNsc <- function(observed.list, warm = 100, iter = 200,
                  mask.rate = 0.15, nrep = 50, min.cell = 3, runNetwork = F, l1.vec = NULL,
                  l2.vec = NULL){
  # observed.list: the list containing the matrices of K conditions. dim: genes by samples
  # mask.rate: iterative imputation procedure
  # nrep: number of iterations in the imputation procedure
  zip.list <- lapply(observed.list, JGNsc.zip, warm = warm, iter = iter, min.cell= min.cell)
  # theta.star <- list(zip.list[[1]]$y.cont,
  #                    zip.list[[2]]$y.cont)
  g.common <- Reduce(intersect, lapply(zip.list, function(x){rownames(x$y.cont)}))
  theta.star <- lapply(zip.list, function(x){
    y = x$y.cont[g.common,]
    return(y)
  })
  observed.list2 <- lapply(observed.list, function(x){
    y = x[g.common,]
  })
  
  for (k in 1:length(zip.list)){
    nr = nrow(observed.list2[[k]])
    nc = ncol(observed.list2[[k]])
    cat("cond",k,"\n")
    # current.imp <- theta.star[[k]]
    imp.sum <-0
    mask.sum <-0
    x = t(theta.star[[k]])
    for(kk in 1:nrep){
      rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
      mask <- t((observed.list2[[k]]!=0) | rmat)
      x.mask = x*mask
      x.mask.mcimpute = mcImpute.R(x.mask, preprocess = T) #samples by genes input
      current.imp <- t(x.mask.mcimpute)
      x = t(current.imp)
      imp.sum <- imp.sum + current.imp
    }
    imp.mean <- imp.sum / nrep
    rownames(imp.mean) <- rownames(observed.list2[[k]])
    colnames(imp.mean) <- colnames(observed.list2[[k]])
    if (is.null(rownames(observed.list2[[k]]))){
      rownames(imp.mean) <- paste("gene",1:ncol(x), sep = "")
    }
    if (is.null(colnames(observed.list2[[k]]))){
      colnames(imp.mean) <- paste("sample",1:nrow(x), sep = "")
    }
    theta.star[[k]] <- imp.mean
  }
  # theta.star[[1]][1:4,1:5]
  theta.star.t <- lapply(theta.star, t)
  theta.star.npn <- lapply(theta.star.t, huge.npn)
  # theta.star.npn[[1]][1:15,1:9]
  
  # joint lasso:
  if(runNetwork){
    if (is.null(l1.vec)){
      l1.vec <- seq(1,30,by=2)/100
    }
    if (is.null(l2.vec)){
      l2.vec <- seq(1,30,by=2)/100
    }
    jgnsc.aic <- NULL
    for (lam1 in l1.vec){
      for(lam2 in l2.vec){
        cat("tuning parameter:", lam1,", ", lam2,"\n")
        tps <- AIC_select(mat.list.t.gau = theta.star.npn, lam1 = lam1, lam2 = lam2)
        jgnsc.aic <- rbind(jgnsc.aic, tps)
      }
    }
    JGL.res <- JGL(theta.star.npn, lambda1 = jgnsc.aic[which.min(jgnsc.aic[,3]),1],
                   lambda2 = jgnsc.aic[which.min(jgnsc.aic[,3]),2], return.whole.theta = T)
    partcorr <- lapply(JGL.res$theta, prec2partialcorr)
  } else {
    JGL.res <- NULL
    jgnsc.aic <- NULL
    partcorr <- NULL
  }
  return(list(theta.star.npn = theta.star.npn,
              JGL = JGL.res,
              aic.table = jgnsc.aic,
              partcorr = partcorr))
}


getObservedList <- function(mtx, group = NULL, min.cells = 10, geneSet = NULL){
  # mtx: raw count matrix, genes by samples
  # group: vector of group identities
  # min.cells: keep genes with at least __ cells expressed. Default is 10.
  # geneSet: a vector of genes of interest
  if (is.null(geneSet)){
    message("Screening all available genes /n Please make sure group variable matches the samples ID")
    geneSet = rownames(mtx)
  }
  if (is.null(group)){
    group = rep("OneGroup", ncol(mtx))
  }
  mtx.filter = mtx[rowSums(mtx>0) > min.cells,]
  genes.use = intersect(rownames(mtx.filter), geneSet)
  mtx.filter = mtx.filter[genes.use,]
  gid = unique(group)
  obsList = list()
  for (i in 1:length(gid)){
    obsList[[i]] = mtx.filter[,group == gid[i]]
    names(obsList)[i] <- gid[i]
  }
  message("The generated List dimensions:", lapply(obsList, dim))
  return(obsList)
}


getCondRelated <- function(gs, pcor0 = 0.0001, partcorr.list, conditions = NULL){
  Grelated <- list()
  for (k in 1:length(partcorr.list)){
    Grelated[[k]] <- names(which(abs(partcorr.list[[k]][gs,])>pcor0))
  }
  gset.temp <- unique(unlist(Grelated))
  xtemp <- NULL
  for (k in 1:length(partcorr.list)){
    xtemp <- cbind(xtemp, partcorr.list[[k]][gset.temp,gs])
  }
  colnames(xtemp) <- gsub(" ", "_", conditions)
  return(xtemp)
}

library(stringr)
library(plyr)
getPathFreq <- function(meta, VARPATH ="SigmaMiniMap.Term"){
  nn <- nrow(meta)
  paths <- str_trim(unlist(strsplit(meta[,VARPATH], ";")), side = "both")
  paths <- paths[paths !=""]
  paths.count <- count(paths)
  paths.count$pct <- paths.count$freq / nn
  return(paths.count)
}

getGENEbox <- function(countmat, group, gene){
  data = data.frame(Gene = countmat[gene,],
                    group = group)
  datamelt <- melt(data)
  ggplot(datamelt, aes(x=group, y=value)) + geom_jitter(alpha = 0.3, color = "lightblue")+ geom_boxplot(alpha = 0.5) +
    ggtitle(gene) + xlab("") + ylab("")
}

getGENEhist <- function(countmat, group, gene, xlabel.pos = 500){
  # countmat: raw count matrix
  data <- data.frame(group = group,
                     GENEbin = countmat[gene,] > 0,
                     GENEraw = countmat[gene,])
  groupn = table(data$group)
  pct = round(table(data$group, data$GENEbin)[,2] / groupn,2)
  gid = names(groupn)
  for (ii in 1:length(gid)){
    data$pct[data$group == gid[ii]] <- pct[ii]
  }
  p1 = ggplot(data, aes(x = GENEraw)) + geom_histogram(binwidth = 30) +
    facet_wrap(. ~ group , scales = "free_y") + geom_text(aes(x=xlabel.pos, y=min(groupn)/4, label=pct)) +
    xlab("Raw") + ylab("Frequency") + ggtitle(gene)
  print(p1)
}

Map2Pathways <- function(partcorr.list, conditions = NULL, GeneInterest = NULL,
                         pathwayRef, pathwayRef_geneVariable, pathwayRef_pathVariable,
                         threshold =0){
  # partcorr.list: a list of estimated partial correlations matrices.
  # conditions: char vector showing the name of each condition for partcorr.list
  # GeneInterest: a character gene name, e.g. "MYC"
  # pathwayRef: a data frame / matrix, with genes and their potential pathways
  # pathwayRef_geneVariable: the column name of the gene variable
  # pathwayRef_pathVariable: the column name of the pathway variable
  # threshold: the cutoff value for partial correlation, for visualization and GSEA purpose.
  if (!is.data.frame(pathwayRef)){
    pathwayRef <- as.data.frame(pathwayRef)
  }
  relatedGenes <- getCondRelated(gs=GeneInterest, partcorr.list = partcorr.list, pcor0 = threshold, conditions = conditions)
  x.connect <- pathwayRef[toupper(pathwayRef[,pathwayRef_geneVariable]) %in% rownames(relatedGenes),]
  
  # constant
  x.connect <- cbind.data.frame(x.connect, relatedGenes[toupper(x.connect[,pathwayRef_geneVariable]),])
  x.connect[,pathwayRef_pathVariable][x.connect[,pathwayRef_pathVariable] ==""] <- "Undefined;"
  x.allgenes <- pathwayRef[toupper(pathwayRef[,pathwayRef_geneVariable]) %in% toupper(rownames(partcorr.list[[1]])),]
  x.allgenes[,pathwayRef_pathVariable][x.allgenes[,pathwayRef_pathVariable] ==""] <- "Undefined;"
  
  cond.connect <- list()
  cond.freqs <- list()
  cond.vars <- gsub(" ", "_", conditions)
  for (m in 1:length(conditions)){
    cond.connect[[m]] <- x.connect[abs(x.connect[,cond.vars[m]]) >0,]
    cond.freqs[[m]] <- getPathFreq(cond.connect[[m]], VARPATH =pathwayRef_pathVariable)
  }
  all.freqs <- getPathFreq(x.allgenes, VARPATH =pathwayRef_pathVariable)
  
  # merge these proportions
  meta.merge <- all.freqs
  for (i in 1:length(cond.freqs)){
    meta.merge <- merge(x = meta.merge, cond.freqs[[i]], by = "x", all = T)
    colnames(meta.merge)[-1] <- paste("V",1:(ncol(meta.merge)-1), sep = "")
  }
  # TODO: make sure conditions and the data col names match!!!!
  colnames(meta.merge)[2:ncol(meta.merge)] <- c("AllGenes_freq","AllGenes_pct",
                                                paste(rep(cond.vars, each =2), c("freq","pct"), sep = "_"))
  meta.merge[is.na(meta.merge)] <- 0
  nconnect = c(nrow(x.allgenes), unlist(lapply(cond.connect, nrow)))
  names(nconnect) <- c("All", cond.vars)
  
  getFisherPval <- function(subg = 1){
    ft.table <- NULL
    for(ii in 1:nrow(all.freqs)){
      subpath <- all.freqs$x[ii]
      x1 = c(all.freqs[all.freqs$x == subpath,2], nrow(x.allgenes)-all.freqs[all.freqs$x == subpath,2])
      x2 = c(cond.freqs[[subg]][cond.freqs[[subg]]$x == subpath,2], nrow(cond.connect[[subg]]) - cond.freqs[[subg]][cond.freqs[[subg]]$x == subpath,2])
      if (length(x2)>0){
        contigency = rbind(x2, x1-x2)
        ft = fisher.test(contigency)
        ft.table <- rbind(ft.table,  c(round(ft$p.value,4), all.freqs$x[ii]))
      } else {
        x2 <- c(0,nrow(cond.connect[[2]]))
        contigency = rbind(x2, x1-x2)
        ft = fisher.test(contigency)
        ft.table <- rbind(ft.table,  c(round(ft$p.value,4), all.freqs$x[ii]))
      }
    }
    colnames(ft.table) <- c("Fisher_pval","x")
    ft.table <- as.data.frame(ft.table)
    ft.table$Fisher_pval <- as.numeric(ft.table$Fisher_pval)
    return(ft.table)
  }
  
  test.fisher <- lapply(1:length(conditions), getFisherPval)
  # all(test.fisher[[1]]$x ==test.fisher[[2]]$x)
  temp = do.call(cbind, test.fisher)[,- (2*(1:length(conditions)))]
  colnames(temp) <- paste("Fisher_pval",cond.vars, sep = "_")
  
  GSEA.table = cbind(meta.merge, temp)
  return(list(x.connect = x.connect,
              cond.connect = cond.connect,
              GSEA.table = GSEA.table,
              nconnect = nconnect))
}




JGNsc.zip2 <- function(y, stepsize = 0.5, warm = 50, a2 = 0.001, a3 = 0.001, b2 = 1e8, b3 = 1e8,
                       a1 = 2, b1 = 1, iter = 50, c0 = 0, min.cell = 2,
                       subjid = NULL, dropThreshold = 0.75){
  # change this to Rcpp
  # c0: a constant number close to zero, for the dropout case, theta value
  # y is genes by samples
  # remove genes with zero expression (low expression level: at least 20 cells expressed this gene) across samples
  # a1 and b1 are the parameter for the non-dropout event!!!!
  # nmi: number of multiple imputation
  subjid <- as.character(subjid)
  keep.gene <- rowSums(y>0)> min.cell
  sum(keep.gene)
  y <- y[keep.gene,]
  ng <- nrow(y)
  nsample <- ncol(y)
  nsubj <- length(unique(subjid))
  z <- matrix(1, ncol = ncol(y), nrow = nrow(y))
  yz = y*z
  # by subject: subj by genes
  yzsum = apply(yz, 1, function(x){
    aggregate(x, list(subjid), sum)[,2]
  })
  zsum = apply(z,1, function(x){
    aggregate(x, list(subjid), sum)[,2]
  })
  namesubj <- aggregate(z[1,], list(subjid), sum)[,1]
  
  # set up initial values
  logalpha <- rep(0, ng)
  alpha <- exp(logalpha)
  beta <- seq(1, ng)
  theta <- (alpha + yzsum)/(beta + zsum)
  rownames(theta) <- namesubj
  z.total <- matrix(0, ncol = nsample, nrow = ng)
  pz.total <- matrix(0, ncol = ng, nrow = nsubj)
  
  ave <- matrix(0, ncol = ng, nrow = nsubj)
  theta.total <- matrix(0, ncol = ng, nrow = nsubj)
  npoi.total <- matrix(0, ncol = ng, nrow = nsubj)
  pij <- matrix(0.5,nrow = nsubj, ncol = ng)
  
  # write this in parallel mode ???
  # estimation of posterior parameters
  # for a gene, if it expressed in a subject in less than 3 cells, skip impute
  for (ii in 1:(iter+warm)){ #number of iterations, warm: warm up (burn), dump the first warm number of initial results
    cat(ii,"..\n")
    for (gg in 1:ng){ # for each gene
      # ii=1
      # gg=1
      ##### update z #####
      P0 <- ifelse(y[gg,]==0, 1,0)*(1-pij[gg])
      P1 <- exp(-theta[,gg][subjid])*pij[gg]
      z[gg,] <- sapply(P1/(P1+P0), function(x){
        if (is.na(x)){
          x = 1
        }
        rbinom(1,1, x)
      })
      z[gg,y[gg,]>0] <- 1 #make sure: y>0 -> z=1
      
      ##### update alpha #####comparing log(f), use the complete function, not propto...
      newlogalpha <- logalpha[gg] + gasdev(1)*stepsize
      alpha[gg] <- exp(logalpha[[gg]])
      newalpha <- exp(newlogalpha)
      
      newsum <- 0
      newsum <- (a2-1)*newlogalpha - nsubj*lgamma(newalpha) +
        newalpha*(- b2+nsubj*log(beta[gg]) + sum(log(theta[,gg])))
      
      newsum <- newsum / b2
      sum <- 0
      sum <- (a2-1)*logalpha[gg] - nsubj*lgamma(alpha[gg]) +
        alpha[gg]*(- b2+nsubj*log(beta[gg]) + sum(log(theta[,gg])))
      sum <- sum / b2
      
      r <- newsum - sum
      if (is.na(r)){
        accept = 0} else if(r>0){
          accept = 1
        } else {
          un = runif(1)
          if (un < exp(r) & newalpha < alpha[gg]){
            accept = 1
          } else{
            accept = 0
          }}
      
      if (accept ==1){
        logalpha[gg] = newlogalpha
        alpha[gg] = newalpha}
      # accept_loc[gg] = accept_loc[gg] +1}
      # total_loc[gg] = total_loc[gg] + 1
      
      ##### update beta #####
      sum <- 0
      sum <- sum(theta[,gg])
      beta[gg] <- rgamma(1, shape = alpha[gg]*nsubj+a3, rate = sum + b3)
      
      ##### update theta*z #####
      temp.yz = y[gg,]*z[gg,]
      # by subject: subj by genes
      temp.yzsum = aggregate(temp.yz, list(subjid), sum)[,2]
      temp.zsum = aggregate(z[gg,], list(subjid), sum)[,2]
      temp.y0sum = aggregate(y[gg,]>0, list(subjid), sum)[,2]
      
      for (ss in 1:nsubj){
        if (temp.y0sum[ss] > 1){ #if only <2 cells in a subject expressed this gene, skip
          theta[ss,gg] <- rgamma(1, shape = alpha[gg]+temp.yzsum[ss], rate = beta[gg]+temp.zsum[ss])  
        } else {
          theta[ss,gg] <- 0
        }
      }
      # theta[,gg][is.infinite(theta[gg,])] <- 0 # if infinity, provides no info...
      
      ##### update p #####
      temp.1zy0 = aggregate((y[gg,]==0)*(1-z[gg,]), list(subjid), sum)[,2]
      for (ss in 1:nsubj){
        pij[ss,gg] <- rbeta(1, shape1 = a1 + temp.zsum[ss], shape2 = b1+temp.1zy0[ss])
        # pz[gg,ss] <- rbeta(1, shape1 = a1 + z[gg,ss], shape2 = b1+1-z[gg,ss])
      }#end for in update p
      
    } #end for for each gene gg
    b2 <- b2 + 1/iter
    b3 <- b2
    if (ii > warm){
      theta.total = theta.total + theta
      npoi <- theta >0
      npoi.total <- npoi.total + npoi
      pz.total <- pz.total + pij
      z.total <- z.total + z
    }
  }
  
  # estimate the final theta values
  rownames(pz.total) <- namesubj
  ave <- theta.total/npoi.total
  pz.final <- pz.total/iter
  z.final <- z.total/iter
  ave[is.na(ave)] <- 0
  ave[is.infinite(ave)] <- 0
  
  # --------
  #imputed theta: y>0 parts are the same, y=0 parts are imputed using Gaussian distribution 
  z <- z.final >= dropThreshold
  ave.impute <- y
  for (gg in 1:ng){
    ave.impute[gg,] = y[gg,] + ave[,gg][subjid]*(1-z[gg,])*(y[gg,]==0) 
  } 
  ave.impute[is.na(ave.impute)] <- 0
  ave.impute[is.infinite(ave.impute)] <- 0
  
  res <- list(thetaij = ave,
              pij = pz.final,
              zijc = z ,
              y.impute = ave.impute, 
              ids = as.character(subjid),
              keep.gene = keep.gene )
  return(res)
}


JGNsc_iterimp <- function(observed.list, imputedList, 
                          mask.rate = 0.15, nrep = 50){
  # observed.list: the list containing the matrices of K conditions. dim: genes by samples
  # mask.rate: iterative imputation procedure
  # nrep: number of iterations in the imputation procedure 
  
  g.common <- Reduce(intersect, lapply(imputedList, function(x){rownames(x$y.impute)}))
  theta.star <- lapply(imputedList, function(x){
    y = x$y.impute[g.common,]
    return(y)
  })
  observed.list2 <- lapply(observed.list, function(x){
    y = x[g.common,]
  })
  
  for (k in 1:length(imputedList)){
    nr = nrow(observed.list2[[k]])
    nc = ncol(observed.list2[[k]])
    cat("cond",k,"\n")
    # current.imp <- theta.star[[k]]
    imp.sum <-0
    mask.sum <-0
    x = t(theta.star[[k]])
    for(kk in 1:nrep){
      rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
      mask <- t((observed.list2[[k]]!=0) | rmat)
      x.mask = x*mask
      x.mask.mcimpute = mcImpute.R(x.mask, preprocess = T) #samples by genes input
      current.imp <- t(x.mask.mcimpute)
      x = t(current.imp)
      imp.sum <- imp.sum + current.imp
    }
    imp.mean <- imp.sum / nrep
    rownames(imp.mean) <- rownames(observed.list2[[k]])
    colnames(imp.mean) <- colnames(observed.list2[[k]])
    if (is.null(rownames(observed.list2[[k]]))){
      rownames(imp.mean) <- paste("gene",1:ncol(x), sep = "")
    }
    if (is.null(colnames(observed.list2[[k]]))){
      colnames(imp.mean) <- paste("sample",1:nrow(x), sep = "")
    }
    theta.star[[k]] <- imp.mean
  }
  theta.star.t <- lapply(theta.star, t) 
  
  return( theta.star.t)
}


JGNsc_GaussianTrans <- function(xx, subjidName = NULL, CountMatName = NULL, transposeCount = F){
  trans.bysubj <- NULL
  # xx: sample by genes
  if (is.list(xx)){
    # subjidName should be a character
    for (id in 1:length(unique(xx[[subjidName]]))){
      cat(unique(xx[[subjidName]])[id], "\n")
      if (transposeCount){
        temp.y <- t(xx[[CountMatName]])[xx[[subjidName]] == unique(xx[[subjidName]])[id],]
      } else {
        temp.y <- xx[[CountMatName]][xx[[subjidName]] == unique(xx[[subjidName]])[id],]
      }
      if (!is.null(dim(temp.y))){
        if (length(unique(temp.y[,1]))==1){ #if the first col are all zeros, huge.npn will generate all NAs.
          temp.y2 <- temp.y[,order(-temp.y[1,])]
          temp.npn2 <- huge.npn(temp.y2)
          temp.npn <- temp.npn2[,colnames(temp.y)]
        } else {
          temp.npn <- huge.npn(temp.y)
        }
      } else {
        message("Subject ", unique(xx[[subjidName]])[id], " only has 1 cell ... \n")
        temp.npn <- rep(median(temp.y), length(temp.y))
      }
      trans.bysubj <- rbind(trans.bysubj, temp.npn)
    }
  } else if (is.matrix(xx)){
    # subjidName should be a vector showing the ids
    for (id in 1:length(unique(subjidName))){
      cat(unique(subjidName)[id], "\n")
      if (transposeCount){
        temp.y <- t(xx)[subjidName == unique(subjidName)[id],]
      } else {
        temp.y <- xx[subjidName == unique(subjidName)[id],]
      }
      if (!is.null(dim(temp.y))){
        if (length(unique(temp.y[,1]))==1){ #if the first col are all zeros, huge.npn will generate all NAs.
          temp.y2 <- temp.y[,order(-temp.y[1,])]
          temp.npn2 <- huge.npn(temp.y2)
          temp.npn <- temp.npn2[,colnames(temp.y)]
        } else {
          temp.npn <- huge.npn(temp.y)
        }
      } else {
        message("Subject ", unique(xx[[subjidName]])[id], " only has 1 cell ... \n")
        temp.npn <- rep(median(temp.y), length(temp.y))
      }
      trans.bysubj <- rbind(trans.bysubj, temp.npn)
    }
  }
  return(trans.bysubj)
}

getPartcorrViaTPSelect <- function(inputgau, l1.vec, l2.vec){
  if (is.null(l1.vec)){
    l1.vec <- seq(1,30,by=2)/100
  }
  if (is.null(l2.vec)){
    l2.vec <- seq(1,20,by=2)/100
  }
  tuningparam <- NULL
  for (lam1 in l1.vec){
    for(lam2 in l2.vec){
      cat("tuning parameter:", lam1,", ", lam2,"\n")
      # tps <- AIC_select(mat.list.t.gau = input.npn, lam1 = lam1, lam2 = lam2)
      tps <- tuning_select(inputgau, lam1 = lam1, lam2 = lam2)
      tuningparam <- rbind(tuningparam, tps[[1]])
    }
  }
  colnames(tuningparam) <- c("lam1", "lam2", "aic", "bic", "ebic", "ebic1", "ebic2") 
  JGL.aic <- JGL(inputgau, lambda1 = tuningparam[which.min(tuningparam[,3]),1],
                 lambda2 = tuningparam[which.min(tuningparam[,3]),2], return.whole.theta = T)
  partcorr.aic <- lapply(JGL.aic$theta, prec2partialcorr)
  JGL.bic <- JGL(inputgau, lambda1 = tuningparam[which.min(tuningparam[,4]),1],
                 lambda2 = tuningparam[which.min(tuningparam[,4]),2], return.whole.theta = T)
  partcorr.bic <- lapply(JGL.bic$theta, prec2partialcorr)
  JGL.ebic <- JGL(inputgau, lambda1 = tuningparam[which.min(tuningparam[,5]),1],
                  lambda2 = tuningparam[which.min(tuningparam[,5]),2], return.whole.theta = T)
  partcorr.ebic <- lapply(JGL.ebic$theta, prec2partialcorr)
  res.partcor <- list(partcorr.aic = partcorr.aic,
                      partcorr.bic = partcorr.bic,
                      partcorr.ebic = partcorr.ebic,
                      tuningparam = tuningparam)
  return(res.partcor)
}



JGNsc.cont <- function(y, stepsize = 0.5, warm = 50, a2 = 0.001, a3 = 0.001, b2 = 1e8, b3 = 1e8,
                       a1 = 2, b1 = 1, iter = 50, c0 = 0, min.cell = 2, dropThreshold = 0.65){
  # change this to Rcpp
  # c0: a constant number close to zero, for the dropout case, theta value
  # y is genes by samples
  # remove genes with zero expression (low expression level: at least 20 cells expressed this gene) across samples
  # a1 and b1 are the parameter for the non-dropout event!!!!
  
  keep.gene <- rowSums(y>0)> min.cell
  y <- y[keep.gene,]
  ng <- nrow(y)
  nsample <- ncol(y)
  z <- matrix(1, ncol = ncol(y), nrow = nrow(y))
  yz = y*z
  # by subject: subj by genes
  yzsum = apply(yz, 1, sum)
  zsum = apply(z,1, sum)
  
  # set up initial values
  logalpha <- rep(0, ng)
  alpha <- exp(logalpha)
  beta <- seq(1, ng)
  thetaj <- (alpha + yzsum)/(beta + zsum)
  z.total <- matrix(0, ncol = nsample, nrow = ng)
  
  ave <- matrix(0, ncol = ng, nrow = nsample)
  theta.total <- rep(0, ng)
  npoi.total <- matrix(0, ncol = ng, nrow = nsample )
  pi <- rowMeans(y>0)
  pi.total <- rep(0,ng)
  
  # write this in parallel mode ???
  # estimation of posterior parameters
  # for a gene, if it expressed in a subject in less than 3 cells, skip impute
  for (ii in 1:(iter+warm)){ #number of iterations, warm: warm up (burn), dump the first warm number of initial results
    cat(ii,"..\n")
    for (gg in 1:ng){ # for each gene
      # ii=1
      # gg=1
      ##### update z #####
      P0 <- ifelse(y[gg,]==0, 1,0)*(1-pi[gg])
      P1 <- exp(-rep(thetaj[gg], nsample))*pi[gg]
      z[gg,] <- sapply(P1/(P1+P0), function(x){
        if (is.na(x)){
          x = 1
        }
        rbinom(1,1, x)
      })
      z[gg,y[gg,]>0] <- 1 #make sure: y>0 -> z=1
      
      ##### update alpha #####comparing log(f), use the complete function, not propto...
      newlogalpha <- logalpha[gg] + gasdev(1)*stepsize
      alpha[gg] <- exp(logalpha[[gg]])
      newalpha <- exp(newlogalpha)
      
      newsum <- 0
      newsum <- (a2-1)*newlogalpha - lgamma(newalpha) +
        newalpha*(- b2+log(beta[gg]) + sum(log(thetaj[gg])))
      
      newsum <- newsum / b2
      sum <- 0
      sum <- (a2-1)*logalpha[gg] - lgamma(alpha[gg]) +
        alpha[gg]*(- b2+ log(beta[gg]) + sum(log(thetaj[gg])))
      sum <- sum / b2
      
      r <- newsum - sum
      if (is.na(r)){
        accept = 0} else if(r>0){
          accept = 1
        } else {
          un = runif(1)
          if (un < exp(r) & newalpha < alpha[gg]){
            accept = 1
          } else{
            accept = 0
          }}
      
      if (accept ==1){
        logalpha[gg] = newlogalpha
        alpha[gg] = newalpha}
      # accept_loc[gg] = accept_loc[gg] +1}
      # total_loc[gg] = total_loc[gg] + 1
      
      ##### update beta #####
      sum <- 0
      sum <- sum(thetaj[gg])
      beta[gg] <- rgamma(1, shape = alpha[gg]+a3, rate = sum + b3)
      
      ##### update theta*z #####
      temp.yz = y[gg,]*z[gg,]
      # by subject: subj by genes
      temp.yzsum = sum(temp.yz)
      temp.zsum = sum(z[gg,])
      temp.y0sum = sum(y[gg,]>0)
      
      if (temp.y0sum > 1){ #if only <2 cells in a subject expressed this gene, skip
        thetaj[gg] <- rgamma(1, shape = alpha[gg]+temp.yzsum , rate = beta[gg]+temp.zsum )  
      } else {
        thetaj[gg] <- 0
      } 
      
      ##### update p #####
      temp.1zy0 = sum((y[gg,]==0)*(1-z[gg,]))
      pi[gg] <- rbeta(1, shape1 = a1 + temp.zsum , shape2 = b1+temp.1zy0 )
      
    } #end for for each gene gg
    b2 <- b2 + 1/iter
    b3 <- b2
    if (ii > warm){
      theta.total = theta.total + thetaj
      npoi <- thetaj >0
      npoi.total <- npoi.total + npoi
      pi.total <- pi.total + pi
      z.total <- z.total + z
    }
  }
  
  # estimate the final theta values 
  ave <- theta.total/npoi.total
  pi.final <- pi.total/iter
  z.final <- z.total/iter
  ave[is.na(ave)] <- 0
  ave[is.infinite(ave)] <- 0
  
  # --------
  #imputed theta: y>0 parts are the same, y=0 parts are imputed using Gaussian distribution 
  z <- z.final >= dropThreshold
  ave.impute <- y
  for (gg in 1:ng){
    ave.impute[gg,] = y[gg,] + ave[gg]*(1-z[gg,])*(y[gg,]==0) 
  } 
  ave.impute[is.na(ave.impute)] <- 0
  ave.impute[is.infinite(ave.impute)] <- 0
  
  res <- list(thetaij = ave,
              pi = pi.final,
              zijc = z ,
              y.impute = ave.impute,  
              keep.gene = keep.gene )
  return(res)
}



CountMap5 <- function(sigma, ngene, n, a1 = 3,
                      b1 = 1, a20 = 2,  b20 = 3, a30 = 1, b30 = 10){
  precision1 <- solve(sigma)
  mu <- rep(0, ngene)
  adj <- abs(sign(precision1))
  diag(adj) <- 0
  x <- mvtnorm::rmvnorm(n, mu, sigma)
  y <- x 
  
  # -------------------------------------------------
  # generate count data from posterior distribution
  # one subject at a time
  # subject i
  z <- matrix(1, nrow = n, ncol = ngene)
  pij.vec <- rep(1, ngene)
  ycomplete <- x 
  
  alphavec <- rep(0, ngene)
  betavec <- rep(0,ngene)
  scmeanvec <- rep(0,ngene)
  # ----------------------------- 
  for (j in 1:ngene) {
    # i=1
    # j=1 
    dat <- x[, j]
    mu_v <- mean(dat)
    sd_v <- sd(dat)
    p_v <- pnorm(dat, mu_v, sd_v)
    
    # ---------------------
    # simulate sc from zip -- 1 gene
    # 3. alpha, beta, thetaij
    alphaj <- rgamma(1, shape = a20, rate = b20)
    betaj <- rgamma(1, shape = a30, rate = b30)
    scmeanj <- rgamma(1, shape = alphaj, rate = betaj)
    if (any(scmeanj >=1000) | any(scmeanj <1) ){
      scmeanj <- runif(1, 1,10)
    } 
    sctemp <- rpois(n, scmeanj)
    alphavec[j] <- alphaj
    betavec[j] <- betaj
    scmeanvec[j] <- scmeanj
    
    ytemp <- quantile(sctemp, p_v)
    ycomplete[,j] <- ytemp
    pij <- rbeta(1, shape1 = a1, shape2 = b1)
    zijc <- sapply(1:n, function(x){
      px = ifelse(scmeanvec[j] > 10, 1, 1- ((1-pij) + pij * exp(-scmeanvec[j])))
      rbinom(1, size = 1, prob = px)
      # rbinom(1, size = 1, prob = pij)
    })
    
    z[,j] <- zijc
    pij.vec[j] <- round(pij,3) 
  }
  
  # -------------------------------
  
  yout = ycomplete*z
  # plot(yout[,53], ycomplete[,53])
  
  colnames(yout) <- paste("gene",1:ngene, sep = "")
  rownames(yout) <-  paste("cell",1:n,sep = "")
  colnames(ycomplete) <- paste("gene",1:ngene, sep = "")
  rownames(ycomplete) <- paste("cell",1:n,sep = "")
  colnames(z) <- paste("gene",1:ngene, sep = "")
  rownames(z) <- paste("cell",1:n,sep = "")
  
  result <- list() 
  result$count <- yout 
  result$count.nodrop <- ycomplete
  result$zijc <- z
  result$thetaj <- round(scmeanvec,2)
  result$pij <- pij.vec
  result$sigma <- sigma
  result$precision <- precision1 
  return(result)
}


JGNsc.cont2 <- function(y, stepsize = 0.5, warm = 50, a2 = 0.001, a3 = 0.001, b2 = 1e8, b3 = 1e8,
                        a1 = 2, b1 = 1, iter = 50, c0 = 0, min.cell = 2, dropThreshold = 0.65){
  # change this to Rcpp
  # c0: a constant number close to zero, for the dropout case, theta value
  # y is genes by samples
  # remove genes with zero expression (low expression level: at least 20 cells expressed this gene) across samples
  # a1 and b1 are the parameter for the non-dropout event!!!!
  
  keep.gene <- rowSums(y>0)> min.cell
  y <- y[keep.gene,]
  tau <- colSums(y)/median(colSums(y))
  ng <- nrow(y)
  nsample <- ncol(y)
  z <- matrix(1, ncol = ncol(y), nrow = nrow(y))
  yz = y*z
  # by subject: subj by genes
  yzsum = apply(yz, 1, sum)
  zsum = apply(z,1, sum)
  
  # set up initial values
  logalpha <- rep(0, ng)
  alpha <- exp(logalpha)
  beta <- seq(1, ng)
  thetaj <- (alpha + yzsum)/(beta + zsum)
  z.total <- matrix(0, ncol = nsample, nrow = ng)
  
  ave <- matrix(0, ncol = ng, nrow = nsample)
  theta.total <- rep(0, ng)
  npoi.total <- matrix(0, ncol = ng, nrow = nsample )
  pi <- rowMeans(y>0)
  pi.total <- rep(0,ng)
  
  # write this in parallel mode ???
  # estimation of posterior parameters
  # for a gene, if it expressed in a subject in less than 3 cells, skip impute
  for (ii in 1:(iter+warm)){ #number of iterations, warm: warm up (burn), dump the first warm number of initial results
    cat(ii,"..\n")
    for (gg in 1:ng){ # for each gene
      # ii=1
      # gg=1
      ##### update z #####
      P0 <- ifelse(y[gg,]==0, 1,0)*(1-pi[gg])
      P1 <- exp(-thetaj[gg]*tau)*pi[gg]
      z[gg,] <- sapply(P1/(P1+P0), function(x){
        if (is.na(x)){
          x = 1
        }
        rbinom(1,1, x)
      })
      z[gg,y[gg,]>0] <- 1 #make sure: y>0 -> z=1
      
      ##### update alpha #####comparing log(f), use the complete function, not propto...
      newlogalpha <- logalpha[gg] + gasdev(1)*stepsize
      alpha[gg] <- exp(logalpha[[gg]])
      newalpha <- exp(newlogalpha)
      
      newsum <- 0
      newsum <- (a2-1)*newlogalpha - lgamma(newalpha) +
        newalpha*(- b2+log(beta[gg]) + sum(log(thetaj[gg])))
      
      newsum <- newsum / b2
      sum <- 0
      sum <- (a2-1)*logalpha[gg] - lgamma(alpha[gg]) +
        alpha[gg]*(- b2+ log(beta[gg]) + sum(log(thetaj[gg])))
      sum <- sum / b2
      
      r <- newsum - sum
      if (is.na(r)){
        accept = 0} else if(r>0){
          accept = 1
        } else {
          un = runif(1)
          if (un < exp(r) & newalpha < alpha[gg]){
            accept = 1
          } else{
            accept = 0
          }}
      
      if (accept ==1){
        logalpha[gg] = newlogalpha
        alpha[gg] = newalpha}
      # accept_loc[gg] = accept_loc[gg] +1}
      # total_loc[gg] = total_loc[gg] + 1
      
      ##### update beta #####
      sum <- 0
      sum <- sum(thetaj[gg])
      beta[gg] <- rgamma(1, shape = alpha[gg]+a3, rate = sum + b3)
      
      ##### update theta*z #####
      temp.yz = y[gg,]*z[gg,]
      # by subject: subj by genes
      temp.yzsum = sum(temp.yz)
      temp.zsum = sum(z[gg,])
      temp.zsumtau = sum(z[gg,]*tau)
      temp.y0sum = sum(y[gg,]>0)
      
      if (temp.y0sum > 1){ #if only <2 cells in a subject expressed this gene, skip
        thetaj[gg] <- rgamma(1, shape = alpha[gg]+temp.yzsum , rate = beta[gg]+temp.zsumtau )  
      } else {
        thetaj[gg] <- 0
      }
      
      ##### update p #####
      temp.1zy0 = sum((y[gg,]==0)*(1-z[gg,]))
      pi[gg] <- rbeta(1, shape1 = a1 + temp.zsum , shape2 = b1+temp.1zy0 )
      
    } #end for for each gene gg
    b2 <- b2 + 1/iter
    b3 <- b2
    if (ii > warm){
      theta.total = theta.total + thetaj
      npoi <- thetaj >0
      npoi.total <- npoi.total + npoi
      pi.total <- pi.total + pi
      z.total <- z.total + z
    }
  }
  
  # estimate the final theta values 
  ave <- theta.total/npoi.total
  pi.final <- pi.total/iter
  z.final <- z.total/iter
  ave[is.na(ave)] <- 0
  ave[is.infinite(ave)] <- 0
  avetau <- ave*tau
  # --------
  #imputed theta: y>0 parts are the same, y=0 parts are imputed using Gaussian distribution 
  z <- z.final >= dropThreshold
  ave.impute <- y
  for (gg in 1:ng){
    ave.impute[gg,] = y[gg,] + avetau[gg]*(1-z[gg,])*(y[gg,]==0) 
  } 
  ave.impute[is.na(ave.impute)] <- 0
  ave.impute[is.infinite(ave.impute)] <- 0
  
  res <- list(thetaij = ave,
              thetatau = avetau,
              pi = pi.final,
              zijc = z ,
              y.impute = ave.impute,  
              keep.gene = keep.gene )
  return(res)
}


#======================== RCPP VERSIONS ===================

RunJGNsc <- function(observed.list, warm = 1000, iter = 5000,
                     mask.rate = 0.15, nrep = 50, min.cell = 3, runNetwork = F, l1.vec = NULL,
                     l2.vec = NULL, a1 = 3, b1 = 1, dropThreshold = 0.75, AvoidIterImp = F){
  # observed.list: the list containing the matrices of K conditions. dim: genes by samples
  # mask.rate: iterative imputation procedure
  # nrep: number of iterations in the imputation procedure
  # set dropThreshold = 0 for UMI
  if (dropThreshold == 0){
    AvoidIterImp = T
    print("UMI Mode \n")
  }
  
  zip.list <- lapply(observed.list, JGNsc_cont_cpp, warm = warm, iter = iter, minCell= min.cell, dropThreshold = dropThreshold, a1 = a1, b1 = b1)
  for(kk in 1:length(zip.list)){
    colnames(zip.list[[kk]]$y.impute) = colnames(observed.list[[kk]])
    gnames = rownames(observed.list[[kk]])[zip.list[[kk]]$keep.gene > min.cell]
    rownames(zip.list[[kk]]$y.impute) = gnames
  }
  
  g.common <- Reduce(intersect, lapply(zip.list, function(x){rownames(x$y.impute)}))
  theta.star <- lapply(zip.list, function(x){
    y = x$y.impute[g.common,]
    return(y)
  })
  observed.list2 <- lapply(observed.list, function(x){
    y = x[g.common,]
  })
  
  if(AvoidIterImp){
    # UMI mode
    theta.star.npn = theta.star
  } else {
    for (k in 1:length(zip.list)){
      nr = nrow(observed.list2[[k]])
      nc = ncol(observed.list2[[k]])
      cat("cond",k,"\n")
      # current.imp <- theta.star[[k]]
      imp.sum <-0
      mask.sum <-0
      x = t(theta.star[[k]])
      for(kk in 1:nrep){
        rmat <- matrix(data = runif(nr*nc) > mask.rate,nrow = nr, ncol = nc) # keep 1-mask.rate,
        mask <- t((observed.list2[[k]]!=0) | rmat)
        x.mask = x*mask
        # x.mask.mcimpute = mcImpute.R(x.mask, preprocess = T) #samples by genes input
        x.mask.mcimpute = mcImpute_cpp(x.mask, preprocess = T)
        current.imp <- t(x.mask.mcimpute$data)
        x = t(current.imp)
        imp.sum <- imp.sum + current.imp
      }
      imp.mean <- imp.sum / nrep
      rownames(imp.mean) <- rownames(observed.list2[[k]])
      colnames(imp.mean) <- colnames(observed.list2[[k]])
      if (is.null(rownames(observed.list2[[k]]))){
        rownames(imp.mean) <- paste("gene",1:ncol(x), sep = "")
      }
      if (is.null(colnames(observed.list2[[k]]))){
        colnames(imp.mean) <- paste("sample",1:nrow(x), sep = "")
      }
      theta.star[[k]] <- imp.mean
    }
    theta.star.t <- lapply(theta.star, t)
    theta.star.npn <- lapply(theta.star.t, huge.npn)
  }
  
  # joint lasso:
  if(runNetwork){
    if (is.null(l1.vec)){
      l1.vec <- seq(1,30,by=2)/100
    }
    if (is.null(l2.vec)){
      l2.vec <- seq(1,30,by=2)/100
    }
    jgnsc.aic <- NULL
    for (lam1 in l1.vec){
      for(lam2 in l2.vec){
        cat("tuning parameter:", lam1,", ", lam2,"\n")
        tps <- AIC_select(mat.list.t.gau = theta.star.npn, lam1 = lam1, lam2 = lam2)
        jgnsc.aic <- rbind(jgnsc.aic, tps)
      }
    }
    JGL.res <- JGL(theta.star.npn, lambda1 = jgnsc.aic[which.min(jgnsc.aic[,3]),1],
                   lambda2 = jgnsc.aic[which.min(jgnsc.aic[,3]),2], return.whole.theta = T)
    partcorr <- lapply(JGL.res$theta, prec2partialcorr)
  } else {
    JGL.res <- NULL
    jgnsc.aic <- NULL
    partcorr <- NULL
  }
  return(list(theta.star.npn = theta.star.npn,
              JGL = JGL.res,
              aic.table = jgnsc.aic,
              partcorr = partcorr))
}




# -------------------
CountMap6 <- function(sigma, ngene, n, a1 = 3,
                      b1 = 1, a20 = 2,  b20 = 3, a30 = 1, b30 = 10,
                      scmeanvec = NULL, pijvec = NULL){
  precision1 <- solve(sigma)
  mu <- rep(0, ngene)
  adj <- abs(sign(precision1))
  diag(adj) <- 0
  x <- mvtnorm::rmvnorm(n, mu, sigma)
  y <- x 
  
  # -------------------------------------------------
  # generate count data from posterior distribution
  # one subject at a time
  # subject i
  z <- matrix(1, nrow = n, ncol = ngene)
  pij.vec <- rep(1, ngene)
  ycomplete <- x 
  
  alphavec <- rep(0, ngene)
  betavec <- rep(0,ngene)
  # ----------------------------- 
  if (is.null(scmeanvec)){
    scmeanvec <- rep(0,ngene)
    for (j in 1:ngene) {
      # i=1
      # j=1 
      dat <- x[, j]
      mu_v <- mean(dat)
      sd_v <- sd(dat)
      p_v <- pnorm(dat, mu_v, sd_v)
      
      # ---------------------
      # simulate sc from zip -- 1 gene
      # 3. alpha, beta, thetaij
      alphaj <- rgamma(1, shape = a20, rate = b20)
      betaj <- rgamma(1, shape = a30, rate = b30)
      scmeanj <- rgamma(1, shape = alphaj, rate = betaj)
      if (any(scmeanj >=1000) | any(scmeanj <1) ){
        scmeanj <- runif(1, 1,10)
      } 
      sctemp <- rpois(n, scmeanj)
      alphavec[j] <- alphaj
      betavec[j] <- betaj
      scmeanvec[j] <- scmeanj
      
      ytemp <- quantile(sctemp, p_v)
      ycomplete[,j] <- ytemp
      if(is.null(pijvec)){
        pij <- rbeta(1, shape1 = a1, shape2 = b1)
      } else {
        pij <- pijvec[j]
      }
      zijc <- sapply(1:n, function(x){
        px = ifelse(scmeanvec[j] > 10, 1, 1- ((1-pij) + pij * exp(-scmeanvec[j])))
        rbinom(1, size = 1, prob = px) 
      })
      
      z[,j] <- zijc
      pij.vec[j] <- round(pij,3) 
    }
  } else {
    for (j in 1:ngene) { 
      dat <- x[, j]
      mu_v <- mean(dat)
      sd_v <- sd(dat)
      p_v <- pnorm(dat, mu_v, sd_v)
      
      # ---------------------
      # simulate sc from zip -- 1 gene
      # 3. alpha, beta, thetaij 
      scmeanj <- scmeanvec[j] 
      sctemp <- rpois(n, scmeanj) 
      
      ytemp <- quantile(sctemp, p_v)
      ycomplete[,j] <- ytemp
      if(is.null(pijvec)){
        pij <- rbeta(1, shape1 = a1, shape2 = b1)
      } else {
        pij <- pijvec[j]
      }
      zijc <- sapply(1:n, function(x){
        px = ifelse(scmeanvec[j] > 10, 1, 1- ((1-pij) + pij * exp(-scmeanvec[j])))
        rbinom(1, size = 1, prob = px) 
      })
      
      z[,j] <- zijc
      pij.vec[j] <- round(pij,3) 
    }
  }
  
  
  # -------------------------------
  
  yout = ycomplete*z 
  
  colnames(yout) <- paste("gene",1:ngene, sep = "")
  rownames(yout) <-  paste("cell",1:n,sep = "")
  colnames(ycomplete) <- paste("gene",1:ngene, sep = "")
  rownames(ycomplete) <- paste("cell",1:n,sep = "")
  colnames(z) <- paste("gene",1:ngene, sep = "")
  rownames(z) <- paste("cell",1:n,sep = "")
  
  result <- list() 
  result$count <- yout 
  result$count.nodrop <- ycomplete
  result$zijc <- z
  result$thetaj <- round(scmeanvec,2)
  result$pij <- pij.vec
  result$sigma <- sigma
  result$precision <- precision1 
  return(result)
}

