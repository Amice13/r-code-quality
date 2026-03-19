# Code from Marble, William, and Matthew Tyler. 2022. “The Structure of Political Choices: Distinguishing Between Constraint and Multidimensionality.” Political Analysis 30 (3): 328–45. 
source(here("functions", "MultiScaleSparse.R"))

adapt.emIRT.prior <- function(emIRT.prior) {

    ndim <- length(emIRT.prior$x$mu)

    prior <- list()

    if (ndim>0){
      prior$sigma.inv.gamma <-  solve(emIRT.prior$x$sigma)
      prior$mu.gamma <- emIRT.prior$x$mu
    }
    prior$sigma.inv.ab <- solve(emIRT.prior$beta$sigma)
    prior$mu.ab <- emIRT.prior$beta$mu

    return(prior)
}

make.starts <- function(data) {
    alpha <- rnorm(data$J)
    beta <- matrix(rnorm(data$J * data$D), nrow = data$J)
    gamma <- matrix(rnorm(data$N * data$D), nrow = data$N)
    return(list(alpha = alpha, beta = beta, gamma = gamma))
}



# for 0-D model
probit.log.prob <- function(params, data, priors){

  ## data:
  ## * N - number of actors
  ## * J - number of questions
  ## * D - number of dimensions
  ## * Y - 1/-1/NA observations in N x J matrix

  ## prior:
  ## * sigma.inv.ab - prior precision for alpha
  ## * mu.ab - prior mean for alpha

  alpha <- params

  lp <- 0
  for (j in 1:data$J){
    lp <- lp + dnorm(alpha[j], mean = prior$mu.ab, sd = sqrt(1 / prior$sigma.inv.ab), log=TRUE )

    ll.i <- ifelse(data$Y[, j] == 1, pnorm(alpha[j], mean = 0, sd = 1, log = TRUE),
                   ifelse(data$Y[,j] == -1, log(1 - pnorm(alpha[j])), NA))

    lp <- lp + sum(ll.i, na.rm=TRUE)
  }

  return(-1 * lp)
}

fit.intercepts <- function(data, prior = NULL, bayes = FALSE, ...) {
    ## intercept from probit, use as start value if fullbayes
    probalph = rep(NA, data$J)
    for (j in 1:data$J){
        DV <- (data$Y[, j]+1)/2
        DV <- DV[complete.cases(DV)] # get errors without this line for some reason
        mod = glm(DV ~ 1, family = binomial(link = "probit"))
        probalph[j] = coef(mod)[1]
    }

    if (bayes){
        message("Finding MAP estimate for 0-d model\n\n")

        out <- optim(par = probalph, fn = probit.log.prob,
                     data = data, priors = priors, method = "BFGS")

        if(out$convergence!=0){
            warning("Warning! MAP estimation didn't converge after",
                    out$counts, "iterations. Convergence code:", out$convergence)
        }
        probalpha <- out$par

    }

    return(list(alpha = probalph))
}

MultiScale <- function(method = "sparse", ...) {
    if (method == "sparse") {
        MultiScaleSparse(...)
    } else if (method == "intercepts") {
        fit.intercepts(...)
    } else {
        stop("Not a valid MultiScale method! Try 'sparse', or 'intercepts'.")
    }
}
