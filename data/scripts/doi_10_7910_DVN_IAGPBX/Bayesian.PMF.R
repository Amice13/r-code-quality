library('MCMCpack')   # to sample from Dirichlet and Inverse-Gamma distribution


sample.posterior.eps <- function(X, A, B, k.prior = 0.001, theta.prior = 0.001) {
  # Sample from the conditional posterior of sigma^2 given its inverse-gamma prior parameters k, theta
  k.pos     <- k.prior + n*T/2 + 1
  theta.pos <- theta.prior + 0.5 * sum((X - A %*% B)^2) 
  return(sqrt(rinvgamma(1, k.pos, theta.pos)))
}


sample.posterior.B <- function (X, A, B, j, k, sd.eps, mu.V, sigma.V) {
  # Sample from the conditional posterior of B[j, k] given its Gaussian prior parameters mu and sigma
  E <- A %*% B
  mu.lik.nor <- (X[, k] %*% A[, j] - ((E[, k] %*% A[,j])[1, 1] - sum(A[, j]^2) * B[j,k]))[1, 1]
  sigma.pos  <- 1 / sqrt(1/sigma.V^2 + sum(A[, j]^2) / sd.eps^2)
  mu.pos     <- sigma.pos^2 * ( mu.lik.nor/sd.eps^2 + mu.V/sigma.V^2 )
  return(rnorm(1, mu.pos, sigma.pos))
}


log.density.posterior.A <- function (X, A, B, i, alpha.W, sd.eps) {
  # Compute the log of conditional posterior density of row ith of A given its Dirichlet prior parameter 
  E <- (X - A %*% B)^2
  return( -sum(E[i,])/(2*sd.eps^2) + log(ddirichlet(A[i, ], alpha.W)) )
}


proposal.dist.row.A <- function (row.A, step = 0.001) {
  # Uniform proposal distribution of mcmc sampler for rows of A
  # Tune step size for better sample acceptance rate but default value 0.001 usually works well with over 80%
  # acceptance
  K        <- length(row.A)
  continue <- TRUE
  samples  <- rep(0, K)
  end      <- K - 1
  while (continue) { 
    for (i in 1:end) {
      # set the bound to sample only within [0, 1]
      m.1 <- if (row.A[i] - step < 0) 0 else (row.A[i] - step)
      m.2 <- if (row.A[i] + step > 1) 1 else (row.A[i] + step) 
      samples[i] <- runif(1, min = m.1, max = m.2)
    }
    if ((sum(samples) < 1) & (sum(samples) > 0)) {
      samples[K] <- 1 - sum(samples)
      continue   <- FALSE
    }
  }
  return(samples)
}



run.mcmc.matrix.factorization <- function (no.iter, K, proposal.step = 0.001, X, W.start, alpha.W, 
                                           V.start, mu.V, sigma.V, 
                                           sd.eps.start, k.prior = 0.001, theta.prior = 0.001) {
  # Run Metropolis-Hasting-within-Gibbs to 
  # - sample from conditional posteriors of sigma^2 and B
  # - use mcmc to sample from A
  # 
  # Args:
  # - no.iter: #iterations
  # - K: #latent dimensions, usually K << n and T
  # - proposal.step: step size for uniform proposal distribution of A
  # - X: observed matrix to be factorized
  # - A.start: initial value for matrix A
  # - alpha.W: concentration parameter for Dirichlet prior of A
  # - B.start: initial value for matrix B
  # - mu.V: mean of B - Gaussian prior mean
  # - sigma.V: sd of B - Gaussian prior sd
  # - sd.eps.start: initial value for noise variance
  # - k.prior: shape parameter for Inverse-Gamma prior of noise var
  # - theta.prior: rate parameter for Inverse-Gamma prior of noise var
  #
  # Returns: a list of 
  # - A: samples of posterior of A containing a list of matrices
  # - B: samples of posterior of B containing a list of matrices
  # - eps: samples of posterior of eps 
  # - accept: acceptance rate in percentage
  
  N      <- no.iter
  step   <- proposal.step
  sd.eps <- sd.eps.start
  ptm    <- proc.time()
  acceptance.count <- 0    # acceptance count per row
  
  A <- W.start
  B <- V.start
  
  eps.samples <- c()
  A.samples   <- list()
  B.samples   <- list()
  
  for (it in 1:N) {
    A.candidate <- A
    
    # sample rows of A using MCMC
    for (i in 1:n) {
      A.candidate     <- A
      A.candidate[i,] <- proposal.dist.row.A(A[i,], step)
      acceptance  <- log.density.posterior.A(X, A.candidate, B, i, alpha.W, sd.eps) - log.density.posterior.A(X, A, B, i, alpha.W, sd.eps)
      if (log(runif(1, 0, 1)) < acceptance) {
        # accept the new position, update row i
        A[i,] <- A.candidate[i,]
        acceptance.count <- acceptance.count + 1
      } 
    }
    A.samples[[it]] <- A
    
    # sample B from conditional posterior
    for (j in 1:K) {
      for (k in 1:T) {
        B[j, k] <- sample.posterior.B(X, A, B, j, k, sd.eps, mu.V, sigma.V)
      }
    }
    B.samples[[it]] <- B
    
    # sample sd of noise from conditional posterior
    sd.eps <- sample.posterior.eps(X, A, B, k.prior, theta.prior) 
    eps.samples <- c(eps.samples, sd.eps)
  }
  
  acceptance.count <- acceptance.count / n      # acceptance count per matrix
  acceptance.percent <- acceptance.count / N    # acceptance rate
#  print(proc.time() - ptm)
  
  return( list(W = A.samples, V = B.samples, eps = eps.samples, accept = acceptance.percent) )
}


geweke.test <- function (trace, intervals=20, len=200, first=0.1*length(trace)) {
  # Geweke test for convergence 
  # Hypothesis testing to compare the means of two subsamples 
  # if the absolute value of test statistic is smaller than 2 then we can accept that 
  # two subsamples have equal mean or the chain converge
  z <- rep(0, intervals)
  for (i in 0:intervals) {
    start.a <- first + i * len
    start.b <- 0.5*length(trace) + i * len
    
    # extract two sub chains
    end.a  <- start.a + len
    end.b  <- start.b + len
    sub.trace.a <- trace[start.a:end.a]
    sub.trace.b <- trace[start.b:end.b]
    
    theta.a <- mean(sub.trace.a)
    theta.b <- mean(sub.trace.b)
    
    var.a   <- var(sub.trace.a)
    var.b   <- var(sub.trace.b)
    
    z[i+1] <- (theta.a - theta.b) / sqrt(var.a + var.b)
  }
  return(z)
}




