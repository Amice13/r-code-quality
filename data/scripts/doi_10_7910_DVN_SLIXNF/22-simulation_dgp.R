##
## Simulation Study: DGP
##

sim_dgp_Time <- function(
  n_unit, time, 
  dgp = 'PTT', e_auto = "AR1", auto1 = NULL, auto2 = NULL,
  alpha  = NULL, bias_term = NULL, ep_sigma = NULL, ep_sigma2 = NULL,
  gammas = NULL, eta1 = NULL, eta2 = NULL, tau = NULL
) {
  ## unit id
  id_unit <- rep(1:n_unit, time)

  ## generate time-intercept
  # alpha <- rnorm(3, 1, 2.5)
  # alpha <- rnorm(3, 1, 4)
  if (is.null(alpha)) {
    alpha <- seq(1:time)
  }

  ## generate Error terms AR(1) process
  ## this can be correlated with U (maybe fix later)
  ep  <- matrix(NA, nrow =  n_unit, ncol = time)

  if (e_auto == "AR1") {
    ep[, 1]  <- mvnfast::rmvn(n_unit, mu = 0, sigma = ep_sigma/(1-auto1^2))   #  sigma is variance
    for(j in seq(from = 2, to = time)){
      ep[,j] <- auto1*ep[, j-1] + mvnfast::rmvn(n_unit, mu = 0, sigma = ep_sigma)
    }
  } else if (e_auto == "AR2") {
    sigma_use <- ((1-auto2)/(1+auto2))*(ep_sigma/((1 - auto2)^2 - auto1^2))
    ep[, 1]  <- mvnfast::rmvn(n_unit, mu = 0, sigma = sigma_use)   #  sigma is variance
    ep[, 2]  <- mvnfast::rmvn(n_unit, mu = 0, sigma = sigma_use)   #  sigma is variance
    for (j in seq(from = 3, to = time)) {
      ep[,j] <- auto1*ep[, j-1] + auto2*ep[, j-2] + mvnfast::rmvn(n_unit, mu = 0, sigma = ep_sigma)
    }
  }

  ##
  ## generate treatment assignment
  ##
  ps <- rep(0.5, 1000)
  Dvec <- rbinom(n_unit, size = 1, prob = ps)

  ## generate the treatment effect
  if (is.null(tau)) {
    tau <- rep(1.5, n_unit)
  }

  ## generate E[Y(0) \mid G_i = g]
  Ylist <- list()
  for (j in 1:time) {
    if (ep_sigma2 > 0) {
      ep2 <- mvnfast::rmvn(n_unit, mu = 0, sigma = ep_sigma2)
    } else {
      ep2 <- 0
    }
    if (dgp == "PT") {
      Ytmp <- rep(alpha[j], n_unit) + Dvec*bias_term + ep[,j] + ep2
    } else if (dgp == "PTT") {
      Ytmp <- rep(alpha[j], n_unit) + Dvec*bias_term*j + ep[,j] + ep2
    }
    Ylist[[j]] <- as.vector(Ytmp)
  }

  Y0 <- do.call("c", Ylist)
  Y0mat <- do.call("cbind", Ylist)

  ## generate observed value (only one post-treatment)
  Yobs <- Y0mat[, time] + tau * Dvec

  Yfull <- c(Y0mat[,1:(time-1)], Yobs)
  Dfull <- c(matrix(0, nrow = n_unit, ncol = (time-1)), Dvec)
  Gfull <- rep(Dvec, times = time)
  id_time <- rep(1:time, each = n_unit)

  ## generate dataset
  dat <- data.frame(
    Y = Yfull,
    Y0 = Y0,
    D = Dfull,
    G = Gfull,
    time = id_time,
    id = id_unit
  )

  return(list(pdata = dat, Y0 = Y0mat, Yobs = Yobs, G = Dvec, ps = ps,
              'tau' = tau, 'tau_mean' = mean(tau)))
}